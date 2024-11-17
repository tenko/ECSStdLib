MODULE TestMain;
IMPORT Testing := O2Testing IN Std;
IMPORT TestArrayOfByte;
IMPORT TestArrayOfChar;
IMPORT TestArrayOfSet;
IMPORT TestCardinal;
IMPORT TestInteger;
IMPORT TestReal;
IMPORT TestString;
IMPORT TestRegex;
IMPORT TestDateTime;
IMPORT TestOSPath;
IMPORT TestOS;
IMPORT TestADTBasicType;
IMPORT TestADTList;
IMPORT TestADTSet;
IMPORT TestADTDictionary;
IMPORT TestADTVector;
IMPORT TestADTTree;
IMPORT TestADTStream;
IMPORT TestDataConfig;

IMPORT SysMem IN Std;
(* IN Micro IMPORT Traps := ARMv7MTraps; *)

CONST
    M = "TestMain";

VAR
    test : Testing.TEST;
BEGIN
    (* Traps.Init; Traps.debug := TRUE; *)
    
    Testing.Initialize(test, M);
    TestArrayOfByte.Run(test);
    TestArrayOfChar.Run(test);
    TestArrayOfSet.Run(test);
    TestCardinal.Run(test);
    TestInteger.Run(test);
    TestReal.Run(test);
    TestString.Run(test);
    (* TestRegex.Run(test); *) (* Leaking memory *)
    (* TestDateTime.Run(test); *) (* Currenly fails due to error in code generator *)
    TestOSPath.Run(test);
    TestOS.Run(test);
    TestADTBasicType.Run(test);
    TestADTList.Run(test);
    TestADTSet.Run(test);
    TestADTDictionary.Run(test);
    TestADTVector.Run(test);
    TestADTTree.Run(test);
    TestADTStream.Run(test);
    TestDataConfig.Run(test);
    Testing.Finalize(test);

    TRACE(SysMem.AllocSize);
    TRACE(SysMem.FreeMem());
END TestMain.