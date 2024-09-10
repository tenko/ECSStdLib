; Only works on Cortex-M profile ARMv7-M
; Flash/memory origin and size must be changed to values for target device configuration.
.code vector
    .required
    .origin 0x00000000                  ; Flash start address

    .qbyte 0x20004000                   ; Stack = ram top.
    .qbyte extent (@vector) + 1         ; Initial PC. (+1 for Thumb flag)
    .qbyte @isr_nmi + 1;                ; Non maskable interrupt.
    .qbyte @isr_hardfault + 1           ; All class of fault.
    .qbyte @isr_memmanage + 1           ; Memory management, ARMv7-M only.
    .qbyte @isr_busfault + 1            ; Pre-fetch fault, memory access fault, ARMv7-M only.
    .qbyte @isr_usagefault + 1          ; Undefined instruction or illegal state, ARMv7-M only.
    #repeat 4
        .qbyte 0x00                     ; Reserved
    #endrep
    .qbyte @isr_svc + 1                 ; System service call via SWI instruction.
    .qbyte @isr_debugmonitor + 1        ; Debug Monitor.
    .qbyte 0x00                         ; Reserved.
    .qbyte @isr_pendsvc + 1             ; Pendable request for system service.
    .qbyte @isr_systick + 1             ; System tick timer.
    #repeat 112                         ; Pad to 128 word size
        .qbyte 0x00                     ; Reserved
    #endrep

#define exception_code
	.code #0
		.replaceable
        .alignment    4
        bkpt    0x00        ; try to go to debugger
loop:   b.n    loop         ; loop forever if return from bkpt
#enddef
	exception_code	isr_nmi
    exception_code	isr_hardfault
    exception_code	isr_memmanage
    exception_code	isr_busfault
    exception_code	isr_usagefault
#undef exception_code

#define isr_code
	.code #0
		.replaceable
        .alignment    4
        bx.n	 lr   ; ignore interrupt
#enddef
    isr_code    isr_svc
    isr_code    isr_debugmonitor
    isr_code    isr_pendsvc
    isr_code	isr_systick
#undef isr_code

.data ram
	.required
	.origin	0x20000000 ; ram start
    .require	_init_ram

.initdata _init_ram
    .alignment    4

    mov     r0, 0
	ldr	    r1, [pc, offset (start)]
    ldr     r2, [pc, offset (ext)]
    b       cond
start:  .qbyte	0x20000000
ext:    .qbyte  extent (@_trailer)
loop:    
    str     r0, [r1]
    add     r1, r1, 4
cond:
    cmp     r1, r2
    bcc     loop

; last section
.trailer _trailer

; standard abort function
.code abort
    .replaceable
    .alignment    4
loop:
    b.n    loop

; standard _Exit function
.code _Exit
    .alignment    4
	bl       @abort

; standard getchar function
.code getchar
    .alignment    4
    bx.n	 lr

; standard free function
.code free
    .replaceable
    .alignment    4
	bx.n	lr

; standard malloc function
.code malloc
    .replaceable
    .alignment    4

    ldr.n   r2, offset (heap) + offset (heap) % 4
	ldr.n	r0, [r2, 0]
	ldr.n	r3, [sp, 0]

    ; round up to nearest word
    mov	r1, 3
	add	r4, r3, r1
    mov	r1, 4
	rsb	r1, 0
	and	r3, r4, r1

	add.n	r3, r3, r0
	str.n	r3, [r2, 0]
	bx.n	lr

heap:	.qbyte	@_heap_start

; heap start
.data _heap_start

	.alignment	4
	.reserve	4
	.require	_init_heap

.initdata _init_heap
    .alignment    4

	ldr	    r0, [pc, offset (heap)]
    ldr     r3, [pc, offset (start)]

    ; round up to nearest word
    mov	r1, 3
	add	r4, r3, r1
    mov	r1, 4
	rsb	r1, 0
	and	r3, r4, r1
    
	str	    r3, [r0, 0]
	b	    skip
heap:   .qbyte	@_heap_start
start:  .qbyte  extent (@_trailer)
skip:

; standard putchar function
.code putchar
    .alignment    4
    bx.n	 lr
