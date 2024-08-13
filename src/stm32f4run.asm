; Only works on Cortex-M profile ARMv7-M and STM32F4
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

    ; Start of STM32F405xx/07xx and STM32F415xx/17xx vector. Ref. RM0090 Reference manual
    .qbyte @isr_wwdg + 1                ; Window Watchdog interrupt
    .qbyte @isr_pvd + 1                 ; PVD through EXTI line detection interrupt
    .qbyte @isr_tamp_stamp + 1          ; Tamper and TimeStamp interrupts through the EXTI line
    .qbyte @isr_rtc_wkup + 1            ; RTC Wake-up interrupt through the EXTI line
    .qbyte @isr_flash + 1               ; Flash global interrupt
    .qbyte @isr_rcc + 1                 ; RCC global interrupt
    .qbyte @isr_exti0 + 1               ; EXTI Line0 interrupt 
    .qbyte @isr_exti1 + 1               ; EXTI Line1 interrupt
    .qbyte @isr_exti2 + 1               ; EXTI Line2 interrupt
    .qbyte @isr_exti3 + 1               ; EXTI Line3 interrupt
    .qbyte @isr_exti4 + 1               ; EXTI Line4 interrupt
    .qbyte @isr_dma1_stream0 + 1        ; DMA1 Stream0 global interrupt
    .qbyte @isr_dma1_stream1 + 1        ; DMA1 Stream1 global interrupt
    .qbyte @isr_dma1_stream2 + 1        ; DMA1 Stream2 global interrupt
    .qbyte @isr_dma1_stream3 + 1        ; DMA1 Stream3 global interrupt
    .qbyte @isr_dma1_stream4 + 1        ; DMA1 Stream4 global interrupt
    .qbyte @isr_dma1_stream5 + 1        ; DMA1 Stream5 global interrupt
    .qbyte @isr_dma1_stream6 + 1        ; DMA1 Stream6 global interrupt
    .qbyte @isr_adc + 1                 ; ADC1, ADC2 and ADC3 global interrupts
    .qbyte @isr_can1_tx + 1             ; CAN1 TX interrupts
    .qbyte @isr_can1_rx0 + 1            ; CAN1 RX0 interrupts
    .qbyte @isr_can1_rx1 + 1            ; CAN1 RX1 interrupts
    .qbyte @isr_can1_sce + 1            ; CAN1 SCE interrupt
    .qbyte @isr_exti9_5 + 1             ; EXTI Line[9:5] interrupts
    .qbyte @isr_tim1_brk_tim9 + 1       ; TIM1 Break interrupt and TIM9 global interrupt
    .qbyte @isr_tim1_up_tim10 + 1       ; TIM1 Update interrupt and TIM10 global interrupt
    .qbyte @isr_tim1_trg_com_tim11  + 1 ; TIM1 Trigger and Commutation interrupts and TIM11 global interrupt
    .qbyte @isr_tim1_cc + 1             ; TIM1 Capture Compare interrupt
    .qbyte @isr_tim2 + 1                ; TIM2 global interrupt
    .qbyte @isr_tim3 + 1                ; TIM3 global interrupt
    .qbyte @isr_tim4 + 1                ; TIM4 global interrupt
    .qbyte @isr_i2c1_ev + 1             ; I2C1 event interrupt
    .qbyte @isr_i2c1_er + 1             ; I2C1 error interrupt
    .qbyte @isr_i2c2_ev + 1             ; I2C2 event interrupt
    .qbyte @isr_i2c2_er + 1             ; I2C2 error interrupt
    .qbyte @isr_spi1 + 1                ; SPI1 global interrupt
    .qbyte @isr_spi2 + 1                ; SPI1 global interrupt
    .qbyte @isr_usart1 + 1              ; USART1 global interrupt
    .qbyte @isr_usart2 + 1              ; USART2 global interrupt
    .qbyte @isr_usart3 + 1              ; USART3 global interrupt
    .qbyte @isr_exti15_10 + 1           ; EXTI Line[15:10] interrupts
    .qbyte @isr_rtc_alarm + 1           ; RTC Alarms (A and B) through EXTI line interrupt
    .qbyte @isr_otg_fs_wkup + 1         ; USB On-The-Go FS Wake-up through EXTI line interrupt
    .qbyte @isr_tim8_brk_tim12 + 1      ; TIM8 Break interrupt and TIM12 global interrupt
    .qbyte @isr_tim8_up_tim13 + 1       ; TIM8 Update interrupt and TIM13 global interrupt
    .qbyte @isr_tim8_trg_com_tim14 + 1  ; TIM8 Trigger and Commutation interrupts and TIM14 global interrupt
    .qbyte @isr_tim8_cc + 1             ; TIM8 Capture Compare interrupt
    .qbyte @isr_dma1_stream7 + 1        ; DMA1 Stream7 global interrupt
    .qbyte @isr_fsmc + 1                ; FSMC global interrupt
    .qbyte @isr_sdio + 1                ; SDIO global interrupt
    .qbyte @isr_tim5 + 1                ; TIM5 global interrupt
    .qbyte @isr_spi3 + 1                ; SPI3 global interrupt
    .qbyte @isr_uart4 + 1               ; UART4 global interrupt
    .qbyte @isr_uart5 + 1               ; UART5 global interrupt
    .qbyte @isr_tim6_dac + 1            ; TIM6 global interrupt, DAC1 and DAC2 underrun error interrupts
    .qbyte @isr_tim7 + 1                ; TIM7 global interrupt
    .qbyte @isr_dma2_stream0 + 1        ; DMA2 Stream0 global interrupt
    .qbyte @isr_dma2_stream1 + 1        ; DMA2 Stream1 global interrupt
    .qbyte @isr_dma2_stream2 + 1        ; DMA2 Stream2 global interrupt
    .qbyte @isr_dma2_stream3 + 1        ; DMA2 Stream3 global interrupt
    .qbyte @isr_dma2_stream4 + 1        ; DMA2 Stream4 global interrupt
    .qbyte @isr_eth + 1                 ; Ethernet global interrupt
    .qbyte @isr_eth_wkup + 1            ; Ethernet Wake-up through EXTI line interrupt
    .qbyte @isr_can2_tx + 1             ; CAN2 TX interrupts
    .qbyte @isr_can2_rx0 + 1            ; CAN2 RX0 interrupts
    .qbyte @isr_can2_rx1 + 1            ; CAN2 RX1 interrupt
    .qbyte @isr_can2_sce + 1            ; CAN2 SCE interrupt
    .qbyte @isr_otg_fs + 1              ; USB On The Go FS global interrupt
    .qbyte @isr_dma2_stream5 + 1        ; DMA2 Stream5 global interrupt
    .qbyte @isr_dma2_stream6 + 1        ; DMA2 Stream6 global interrupt
    .qbyte @isr_dma2_stream7 + 1        ; DMA2 Stream7 global interrupt
    .qbyte @isr_usart6 + 1              ; USART6 global interrupt
    .qbyte @isr_i2c3_ev + 1             ; I2C3 event interrupt
    .qbyte @isr_i2c3_er + 1             ; I2C3 error interrupt
    .qbyte @isr_otg_hs_ep1_out + 1      ; USB On The Go HS End Point 1 Out global interrupt 
    .qbyte @isr_otg_hs_ep1_in + 1       ; USB On The Go HS End Point 1 In global interrupt 
    .qbyte @isr_otg_hs_wkup + 1         ; USB On The Go HS Wake-up through EXTI interrupt 
    .qbyte @isr_otg_hs + 1              ; USB On The Go HS global interrupt
    .qbyte @isr_dcmi + 1                ; DCMI global interrupt
    .qbyte @isr_cryp + 1                ; CRYP crypto global interrupt
    .qbyte @isr_hash_rng + 1            ; Hash and Rng global interrupt
    .qbyte @isr_fpu + 1                 ; FPU global interrupt
    #repeat 30                          ; Pad to 128 word size
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
	isr_code	isr_wwdg
    isr_code	isr_pvd
    isr_code	isr_tamp_stamp
    isr_code	isr_rtc_wkup
    isr_code	isr_flash
    isr_code	isr_rcc
    isr_code	isr_exti0
    isr_code	isr_exti1
    isr_code	isr_exti2
    isr_code	isr_exti3
    isr_code	isr_exti4
    isr_code	isr_dma1_stream0
    isr_code	isr_dma1_stream1
    isr_code	isr_dma1_stream2
    isr_code	isr_dma1_stream3
    isr_code	isr_dma1_stream4
    isr_code	isr_dma1_stream5
    isr_code	isr_dma1_stream6
    isr_code	isr_adc
    isr_code	isr_can1_tx
    isr_code	isr_can1_rx0
    isr_code	isr_can1_rx1
    isr_code	isr_can1_sce
    isr_code	isr_exti9_5
    isr_code	isr_tim1_brk_tim9
    isr_code	isr_tim1_up_tim10
    isr_code	isr_tim1_trg_com_tim11
    isr_code	isr_tim1_cc
    isr_code	isr_tim2
    isr_code	isr_tim3
    isr_code	isr_tim4
    isr_code	isr_i2c1_ev
    isr_code	isr_i2c1_er
    isr_code	isr_i2c2_ev
    isr_code	isr_i2c2_er
    isr_code	isr_spi1
    isr_code	isr_spi2
    isr_code	isr_usart1
    isr_code	isr_usart2
    isr_code	isr_usart3
    isr_code	isr_exti15_10
    isr_code	isr_rtc_alarm
    isr_code	isr_otg_fs_wkup
    isr_code	isr_tim8_brk_tim12
    isr_code	isr_tim8_up_tim13
    isr_code	isr_tim8_trg_com_tim14
    isr_code	isr_tim8_cc
    isr_code	isr_dma1_stream7
    isr_code	isr_fsmc
    isr_code	isr_sdio
    isr_code	isr_tim5
    isr_code	isr_spi3
    isr_code	isr_uart4
    isr_code	isr_uart5
    isr_code	isr_tim6_dac
    isr_code	isr_tim7
    isr_code	isr_dma2_stream0
    isr_code	isr_dma2_stream1
    isr_code	isr_dma2_stream2
    isr_code	isr_dma2_stream3
    isr_code	isr_dma2_stream4
    isr_code	isr_eth
    isr_code	isr_eth_wkup
    isr_code	isr_can2_tx
    isr_code	isr_can2_rx0
    isr_code	isr_can2_rx1
    isr_code	isr_can2_sce
    isr_code	isr_otg_fs
    isr_code	isr_dma2_stream5
    isr_code	isr_dma2_stream6
    isr_code	isr_dma2_stream7
    isr_code	isr_usart6
    isr_code	isr_i2c3_ev
    isr_code	isr_i2c3_er
    isr_code	isr_otg_hs_ep1_out
    isr_code	isr_otg_hs_ep1_in
    isr_code	isr_otg_hs_wkup
    isr_code	isr_otg_hs
    isr_code	isr_dcmi
    isr_code	isr_cryp
    isr_code	isr_hash_rng
    isr_code	isr_fpu
#undef isr_code

.data ram
	.required
	.origin	0x20000000 ; ram start

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
    .replaceable
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
    ldr     r1, [pc, offset (start)]
	str	    r1, [r0, 0]
	b	    skip
heap:   .qbyte	@_heap_start
start:  .qbyte  extent (@_trailer)
skip:

; standard putchar function
.code putchar
    .replaceable
    .alignment    4
    bx.n	 lr
