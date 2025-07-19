(**
Module implementing the LZ4 block compression format.

Ref : `Link <https://github.com/lz4/lz4/blob/dev/doc/lz4_Block_format.md>`_

This code is from the simplified implementation : `Link <https://github.com/nigeltao/sflz4/>`_

License is Apache License 2.0, Copyright 2022 Nigel Tao.
*)
MODULE DataLZ4 IN Std;

IMPORT SYSTEM;
	
TYPE
	ADDRESS = SYSTEM.ADDRESS;
	U8 = UNSIGNED8;
	U32 = UNSIGNED32;

CONST
	ERROR_INVALID_DATA* = -1;
	ERROR_SRC_TO_LARGE* = -2;
	ERROR_DST_TO_SMALL* = -3;
    MAX_SRC_LEN = 0FFFFFFH;
    HASH_TABLE_SHIFT = 12;

(**
Decode LZ4 block format data from src to dst.
Operates directly on raw memory data.
Source size is limited to 16777215 bytes in order to simplify the code.
Return decoded length if success or error code on failure (negative value).
*)
PROCEDURE BlockDecodeRaw*(dst : ADDRESS; dlen : LENGTH; src : ADDRESS; slen : LENGTH): INTEGER;
VAR
	odst, csrc : ADDRESS;
	s, token, literalLen, coff, clen : U32;
	PROCEDURE Next(): U32;
	VAR s : U8;
	BEGIN
		SYSTEM.GET(src, s);
		INC(src, SIZE(U8));
		DEC(slen);
		RETURN U32(s);
	END Next;
	PROCEDURE MemCopy(dst, src : ADDRESS; cnt : LENGTH);
	VAR
	    s : U8;
	    i: LENGTH;
	BEGIN
	    FOR i := 0 TO cnt - 1  DO
	    	SYSTEM.GET(src + i, s);
	        SYSTEM.PUT(dst + i, s);
	    END
	END MemCopy;
BEGIN
	IF slen > MAX_SRC_LEN THEN RETURN ERROR_SRC_TO_LARGE END;
	odst := dst;
	WHILE slen > 0 DO
		token := Next();
		literalLen := SYSTEM.LSH(token, -4);
		IF literalLen > 0 THEN
			IF literalLen = 15 THEN
				s := 255;
				WHILE s = 255 DO
					IF slen = 0 THEN RETURN ERROR_INVALID_DATA END;
					s := Next();
					literalLen := literalLen + s;
				END;
			END;
			IF literalLen > slen THEN RETURN ERROR_INVALID_DATA END;
			IF literalLen > dlen THEN RETURN ERROR_DST_TO_SMALL END;
			MemCopy(dst, src, LENGTH(literalLen));
			dst := dst + literalLen;
			dlen := LENGTH(dlen - literalLen);
			src := src + literalLen;
			slen := LENGTH(slen - literalLen);
      		IF slen = 0 THEN RETURN INTEGER(dst - odst) END;
		END;
		IF slen < 2 THEN RETURN ERROR_INVALID_DATA END;
		coff := Next() + SYSTEM.LSH(Next(), 8);
		IF (coff = 0) OR (coff > U32(dst - odst))  THEN RETURN ERROR_INVALID_DATA END;
		clen := U32(SET32(token) * SET32(15)) + 4;
		IF clen = 19 THEN
			s := 255;
			WHILE s = 255 DO
				IF slen = 0 THEN RETURN ERROR_INVALID_DATA END;
				s := Next();
				clen := clen + s;
			END;
		END;
		IF dlen < clen THEN RETURN ERROR_DST_TO_SMALL END;
		dlen := LENGTH(dlen - clen);
		csrc := dst - coff;
		MemCopy(dst, csrc, LENGTH(clen));
		dst := dst + clen;
	END;
	RETURN INTEGER(dst - odst)
END BlockDecodeRaw;

(** Worst case needed destination for encoded data *)
PROCEDURE MaxEncodeSize*(slen : LENGTH): LENGTH;
BEGIN RETURN slen + (slen DIV 255) + 16
END MaxEncodeSize;

(**
Encode raw memory data in src to LZ4 block format data in dst.
Return encoded length if success or error code on failure (negative value).
*)
PROCEDURE BlockEncodeRaw*(dst : ADDRESS; dlen : LENGTH; src : ADDRESS; slen : LENGTH): INTEGER;
VAR
	hashTable : POINTER TO ARRAY OF U32;
	sp, spNext, match, matchLimit, literalStart : ADDRESS;
	dp, token : ADDRESS;
	i, j, finalLiteralsLimit : LENGTH;
	step, stepCounter : LENGTH;
	copyOff, adjCopyLen : LENGTH;
	hentry, hash, nextHash : U32;
	oldOffset, newOffset : U32;

	PROCEDURE Hash(x : U32): U32;
	BEGIN
		x := SYSTEM.VAL(U32, x * 2654435761);
		RETURN SYSTEM.LSH(x, HASH_TABLE_SHIFT - 32)
	END Hash;

	PROCEDURE GetU32(src : ADDRESS): U32;
	VAR s : U32;
	BEGIN
		SYSTEM.GET(src, s);
		RETURN s
	END GetU32;

	PROCEDURE GetU8(src : ADDRESS): U8;
	VAR s : U8;
	BEGIN
		SYSTEM.GET(src, s);
		RETURN s
	END GetU8;

	PROCEDURE MemCopy(dst, src : ADDRESS; cnt : LENGTH);
	VAR
		s : U8;
		i: LENGTH;
	BEGIN
		FOR i := 0 TO cnt - 1  DO
			SYSTEM.GET(src + i, s);
			SYSTEM.PUT(dst + i, s);
		END
	END MemCopy;
		
	PROCEDURE longestCommonPrefix(p, q, pLimit : ADDRESS): LENGTH;
	VAR
		op : ADDRESS;
		n : LENGTH;
	BEGIN
		op := p;
		n := LENGTH(pLimit - p);
		WHILE (n >= 4) & (GetU32(p) = GetU32(q)) DO
			INC(p, 4);
			INC(q, 4);
			DEC(n, 4)
		END;
		WHILE (n > 0) & (GetU8(p) = GetU8(q)) DO
			INC(p);
			INC(q);
			DEC(n)
		END;
		RETURN LENGTH(p - op)
	END longestCommonPrefix;

	PROCEDURE Append(s : U8);
	BEGIN
		SYSTEM.PUT(dp, s);
		INC(dp)
	END Append;
	
	PROCEDURE AppendLen(len : LENGTH);
	VAR j : LENGTH;
	BEGIN
		IF len < 15 THEN
			Append(U8(SYSTEM.LSH(len, 4)));
		ELSE
			j := len - 15;
			Append(U8(0F0H));
			WHILE j >= 255 DO
				Append(U8(0FFH));
				j := j - 255
			END;
			Append(U8(j));
		END;
		MemCopy(dp, literalStart, len);
		dp := dp + len;
	END AppendLen;
BEGIN
	IF dlen < MaxEncodeSize(slen) THEN RETURN ERROR_DST_TO_SMALL END;
	NEW(hashTable, 4096);
	dp := dst;
	sp := src;
	literalStart := src;
	(* The last match must start at least 12 bytes before the end of block *)
	IF slen > 12 THEN
		matchLimit := src + slen - 5;
		finalLiteralsLimit := slen - 11;
		(*
		   hashTable maps from 4096 keys to 32-bit values.
           Each value is an offset o, relative to src, initialized to zero.
           Each key, when set, is a hash of 4 bytes src[o .. o+4].
        *)
		FOR i := 0 TO LEN(hashTable^) - 1 DO hashTable[i] := 0 END;
		LOOP
			(*
				Start with 1-byte steps, accelerating when not finding any matches
      			(e.g. when compressing binary data, not text data).
			*)
			step := 1;
			stepCounter := 64;
      		(* Start with a non-empty literal. *)
      		spNext := sp + 1;
      		nextHash := Hash(GetU32(spNext));
      		(* Find a match or goto final_literals. *)
      		match := 0;
          	LOOP
				sp := spNext;
				spNext := spNext + step;
				step := SYSTEM.LSH(stepCounter, -6);
				INC(stepCounter);
				IF LENGTH(spNext - src) > finalLiteralsLimit THEN
					AppendLen(slen - LENGTH(literalStart - src));
					DISPOSE(hashTable);
					RETURN INTEGER(dp - dst)
				END;
				hentry := hashTable[LENGTH(nextHash)];
				match := src + hentry;
				hashTable[LENGTH(nextHash)] := U32(sp - src);
				nextHash := Hash(GetU32(spNext));
				IF ((sp - match) > 0FFFFH) OR (GetU32(sp) = GetU32(match)) THEN EXIT END;                        
          	END;
          	
			(* Extend the match backwards. *)
			WHILE (sp > literalStart) & (match > src) & (GetU8(sp - 1) = GetU8(match - 1)) DO
				DEC(sp);
				DEC(match);
			END;
          	(*
				Emit half of the LZ4 token, encoding the literal length. We'll fix up
      			the other half later.
          	*)
      		token := dp;
      		AppendLen(LENGTH(sp - literalStart));
      		
			(*
				At this point:
				- sp    points to the start of the match's later   copy.
				- match points to the start of the match's earlier copy.
				- token points to the LZ4 token.

				Calculate the match length and update the token's other half.
			*)
      		LOOP
      			(* Calculate the match length and update the token's other half. *)
				copyOff := LENGTH(sp - match);
				Append(U8(copyOff MOD 256));
				Append(U8(SYSTEM.LSH(copyOff, -8) MOD 256));
				adjCopyLen := longestCommonPrefix(4 + sp, 4 + match, matchLimit);

				IF adjCopyLen < 15 THEN
					SYSTEM.PUT(token, SET8(GetU8(token)) + SET8(adjCopyLen))
	   			ELSE
					j := adjCopyLen - 15;
					SYSTEM.PUT(token, SET8(GetU8(token)) + SET8(0FH));
					WHILE j >= 255 DO
						Append(U8(0FFH));
						j := j - 255
					END;
					Append(U8(j));
	   			END;
   				sp := sp + 4 + adjCopyLen;
   				
				(* Update the literal_start and check the final_literals_limit. *)
				literalStart := sp;
				IF LENGTH(sp - src) >= finalLiteralsLimit THEN
					AppendLen(slen - LENGTH(literalStart - src));
					DISPOSE(hashTable);
					RETURN INTEGER(dp - dst)
				END;

				(*
					We've skipped over hashing everything within the match. Also, the
					minimum match length is 4. Update the hash table for one of those
					skipped positions.
				*)
				hashTable[LENGTH(Hash(GetU32(sp - 2)))] := U32(sp - 2 - src);
		        
				(*
					Check if this match can be followed immediately by another match.
		        	If so, continue the loop. Otherwise, break.
				*)
				hash := Hash(GetU32(sp));
				oldOffset := hashTable[LENGTH(hash)];
				newOffset := U32(sp - src);
				hashTable[LENGTH(hash)] := newOffset;
				match := src + oldOffset;
				IF ((newOffset - oldOffset) > 0FFFFH) OR (GetU32(sp) # GetU32(match)) THEN
					EXIT
				END;
				token := dp;
				INC(dp);
				SYSTEM.PUT(token, U8(00H));
      		END;
		END;
	END;
	DISPOSE(hashTable);
	RETURN ERROR_INVALID_DATA
END BlockEncodeRaw;

END DataLZ4.