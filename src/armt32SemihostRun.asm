; Only works on Cortex-M profile ARMv6-M and ARMv7-M
; Uses only Thumb1 16bit instructions to support Coretex-M0/M0+
; Flash/memory origin and size must be changed to values for target device.
.code vector
    .required
    .origin 0x00000000 ; flash

    .qbyte 0x20004000 ; stack = ram top
    .qbyte extent (@vector) + 1 ; +1 for Thumb flag

    #repeat 15
        .qbyte    @handle + 1 ; +1 for Thumb flag
    #endrep
    
.code handle
    .alignment    4

    loop:    b.n    loop

.data ram
	.required
	.origin	0x20000000 ; ram start

; last section
.trailer _trailer

; standard abort function
.code abort
    .alignment    4

	ldr.n    r0, offset (SYS_EXIT) + offset (SYS_EXIT) % 4
    ldr.n    r1, offset (ADP_Stopped_ApplicationExit) + offset (ADP_Stopped_ApplicationExit) % 4
    bkpt.n   0xab
loop:
    b.n    loop

    .align    4
SYS_EXIT:    .qbyte    0x18
ADP_Stopped_ApplicationExit: .qbyte    0x20026

; standard _Exit function
.code _Exit
    .alignment    4

	bl       @abort

; standard getchar function
.code getchar
    .alignment    4

    ldr.n    r0, offset (SYS_READC) + offset (SYS_READC) % 4
    mov      r1, 0x00
    bkpt.n   0xab
    bx.n	 lr

    .align    4
SYS_READC:    .qbyte    0x07

; standard free function
.code free
    .alignment    4
    push    {lr}
    ;add sp, sp, 4
    bl.w    @Std:SysMem.Dispose
    ;sub sp, sp, 4
    pop     {lr}
	bx.n	lr

; standard malloc function
.code malloc
    .alignment    4
    push {lr}
    ;add sp, sp, 4
    bl.w    @Std:SysMem.New
    ;sub sp, sp, 4
    pop {lr}
	bx.n	lr

; fetch heap start
.code get_heap_start
    .alignment    4
    
    ldr.n   r2, offset (heap) + offset (heap) % 4
	ldr.n	r0, [r2, 0]
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
    ldr     r1, [pc, offset (start)]
	str	    r1, [r0, 0]
	b	    skip
heap:   .qbyte	@_heap_start
start:  .qbyte  extent (@_trailer)
skip:

; standard putchar function
.code putchar
    .alignment    4

    ldr.n    r0, offset (SYS_WRITEC) + offset (SYS_WRITEC) % 4
    mov      r1, sp
    bkpt.n   0xab
    ldr.n	 r0, [r1]
    bx.n	 lr

    .align    4
SYS_WRITEC:    .qbyte    0x03
