LIST 	P=PIC16F877
			include	<P16f877.inc>
	 __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _HS_OSC & _WRT_ENABLE_ON & _LVP_OFF & _DEBUG_OFF & _CPD_OFF
	
			org		0x00
	reset:	goto	start
	
			org		0x04
			goto	psika
	
			org		0x10
	start:	bcf		STATUS, RP0
			bcf		STATUS, RP1			; Bank0 <------
			clrf	PORTA               ; Analog input that represents the vdd inserted by the user.
			clrf	PORTD               ; output register for the LCD
			clrf	PORTE		        ; his 3 LSB bits are for Digital output 
			clrf	INTCON				; disable all interrupts
	       	clrf	PIR1                ; the flags register(of timer1 and others)
			clrf 	0x50   				; hundreds
	        clrf	0x51 				; dozens
	  		clrf 	0x52				; unity
			clrf 	0x53				; counter register 0-250
			clrf    0x55 				            
	        movlw   0x30                ; the addres of '0' in the lcd
	   		movwf  	0x55                ; a register to the address of '0' in the lcd
			clrf 	0x40 	 			; used for addressH in order to not harm the saved address 
			
			clrf	0x45    			; status of counting(up/down/stay)      		
	;-------------------------------------------------
			bsf		STATUS, RP0			; Bank1 <------
			clrf	PIE1                ; peripheral interrupts Disable
			bsf		PIE1, TMR1IE		; TIMER1 interrupt Enable
			bsf		PIE1, ADIE			; Enable A to D Interrupt
			
			movlw	0x02				; E - Digital, A - Ananlog
			movwf	ADCON1
			
			movlw	0xFF				; 11111111
			movwf	TRISA	            ; set port A  input
			clrf 	TRISD				; port D = LCD output
			clrf	TRISE				; porte E = LCD output 
			
			
			bcf		STATUS, RP0			; Bank0 <------
	
	;-------------------------------------------------;
			movlw	0x81				;;;; B'10000001'
			movwf	ADCON0				; 10 = Fosc/32,000 = channel_0, 0 = Go bit,0 = dontCare   1 = A to D on bit 
			call	d_20				; Delay TACQ
			bsf		ADCON0, GO			; Start conversion
	
			bsf 	INTCON, PEIE		; Enable Peripherals interrupts
			bsf		INTCON, GIE			; Enable Interrupts 
			
			clrf	0x30				; 0x30 is the counter.
	        movlw   d'10'               
	     	movwf  	0x30                ; moving 10 to the counter 0x30.
	;---------------------------------------------------------------------------------------
	;** TIMER 1 initialization **
			movlw	0x30                ; 00110000 
			movwf	T1CON				; internal clock source with 1:8 prescaler
			movlw	0xF0                                
			movwf	TMR1L
			movlw	0x0A                
			movwf	TMR1H				;TMR1H:TMR1L = 0x0AF0 = 2800d
										;Td = 200ns*(2^16-TMR1H:TMR1L)PS = 200ns(2^16-2800)*8 ~= 100ms (mili)
			bsf		T1CON, TMR1ON		;Timer 1 starts to increment


			
	;---------------------------------------------------------------------------------------
	;   *	LCD initialization *		
			call 	init ; use lcd initialization subroutine
	;---------------------------------------------------------------------------------------
	loop:    
	        clrw              ;clears the w_reg.
	       	addwf 	0x30,0 
	        btfss 	STATUS,2  ; if(counter == 0) jump, else continue.        
		    goto  	loop
	show:
			
			movlw 	d'10'
			movwf 	0x30         	; restore the counter
			call	direction
			call 	split_number 	; spliting the number to 3 pieces
			call 	print_number	; print the pieces
			btfsc 	0x45,0			; 0x01  
			goto 	loop			; is stay
			btfsc	0x45,1			; 0x02 
			goto 	increase		; is up
			goto 	decrease		; else decrease
	increase:
			incf 	0x53,1			; increament counter
			movlw 	d'251'			
			subwf	0x53,0			
			btfsc	STATUS, Z		; if(counter==251)
			clrf	0x53			; clear counter
			goto 	loop			; else
	decrease:
			decf 	0x53,1
			movlw	d'255'
			subwf	0x53,0
			btfsc	STATUS, Z
			call 	recount			; counter= 250
			goto 	loop     		; else
	recount:
			movlw 	d'250'          
			movwf 	0x53			;counter= 250
			return	 		
	   	  
	
	;---------------------------------------------------------------;
	print_number: ; need to convert number to h,d,u
			
			
			movlw	B'10000000' 	; PLACE for the data on the LCD
			movwf	0x20			; B'10000000'
			call 	lcdc
			call	mdel
	        
	        movf  	0x55,0          ; adds 0x30 to w_reg.
	 		addwf	0x50,0          ; 0+ Hundreds		
			movwf	0x20
			call 	lcdd
			call	mdel
	
	
		    movf  	0x55,0          ; adds 0x30 to w_reg.
	 		addwf	0x51,0          ; 0+ dozens
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movf  	0x55,0          ; adds 0x30 to w_reg.
	 		addwf	0x52,0          ; 0+ unity
			movwf	0x20
			call 	lcdd
			call	mdel 
	
			movlw	B'11000000' 	; PLACE for the data on the LCD
			movwf	0x20			; B'10000000'
			call 	lcdc
			call	mdel
	
			btfsc 	0x45,0          ; if the LSB is not clear 
			goto 	steady			; goto steady
			btfsc	0x45,1			
			goto 	printUp
	
	printDown:
			movlw	0x44			; CHAR (D)
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x4F			; CHAR (0)
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x57			; CHAR (W)
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x4E			; CHAR ??
			movwf	0x20
			call 	lcdd
			call	mdel
			return
		
	printUp:
			movlw	0x55			; CHAR (U)
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x50			; CHAR (P)
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x20			; CHAR ( )
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x20			; CHAR ( )
			movwf	0x20
			call 	lcdd
			call	mdel
			
			return   	
	steady:
			movlw	0x20			; CHAR ( )
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x20			; CHAR ( )
			movwf	0x20
			call 	lcdd
			call	mdel
			
			movlw	0x20			; CHAR ( )
			movwf	0x20
			call 	lcdd
			call	mdel
	
			movlw	0x20			; CHAR ( )
			movwf	0x20
			call 	lcdd
			call	mdel
			
			return   
					
	;---------------------------------------------------------------------------------------
	; a label that split the number that we want to print to 3 registers
	split_number: 
		   clrw
	       clrf 0x60                ; copy to temp register in order to not change 
	       movf 0x53,0				; move 0x53 to W_reg
	       movwf 0x60				; move the 0x53 amount to 0x60
	       clrf 0x50 
	       clrf 0x51
	       clrf 0x52 
	       clrw
	       addlw d'100'			 	
	       							
	      
	decH:
	       subwf 0x60,1				; 0x60 - 100
	       incf 0x50,1				; Hundreds++
	       btfsc STATUS,C			; if there wasn't a carry
	       goto decH				; go to dec another 100
	       addwf 0x60,1				; else add 100 to 0x60
	       decf 0x50,1				; Hundreds--
	       clrw  
	       addlw d'10'				; and start dec 10
	decD:    
	       subwf 0x60,1				; 0x60 - 10
	       incf 0x51,1				; dozens++
	       btfsc STATUS,C			; if there wasn't a carry
	       goto decD				; go to dec another 10
	       addwf 0x60 ,1			; else add the 10 back
	       decf 0x51,1 				; dozens-- 
	       movf 0x60,0				; move 0x60 to the W_reg
	       movwf 0x52				; move the result of the unity number to 0x52
		   return
	;----------------------------------------------------------;
	;set the direction of the counter according to the VC
	;save the current direction at 0x45 
	direction: 
			movf 	0X4A,w 			; the 8 most significant bit from the 10 bit result of the A to D conversion    
			movwf	0x40				; moves the amount of voltage to 0x40
			movlw 	d'25'				; 25 to W_reg
			subwf	0x40,1              ; voltage-25
			btfss	STATUS, C           ; if there was a carry 
			goto	setStay				; go to stay
			movlw 	d'51'				; else 51 to W_reg
			subwf	0x40,1				; (voltage-25)-51
			btfss	STATUS, C			; if there was a carry
			goto	setUp				; go to up
			movlw 	d'16'				; else 16 to W_reg
			subwf	0x40,1				; (((voltage-25)-51)-16)
			btfss	STATUS, C 			; if there was a carry
			goto	setStay				; go to stay
			movlw 	d'25'				; 25 to W_reg
			subwf	0x40,1				; (((voltage-25)-51)-16)-25
			btfss	STATUS, C			; if there was a carry
			goto	setDown				; go to down
	
	setStay:	
			movlw	0x01
			movwf	0x45                ; moves 1 to 0x45
			return
	setUp:
			movlw	0x02				; moves 2 to 0x45
			movwf	0x45				
			return
	setDown:
			movlw	0x04					
			movwf	0x45				; moves 4 to 0x45
			return
	;---------------------------------------------------------------------------------------
	;
	;subroutine to initialize LCD
	;
	init	movlw	0x30 				; 00011110
			movwf	0x20				; 
			call 	lcdc   				; subroutine to write command to LCD 
			call	del_41				; 
	
			movlw	0x30
			movwf	0x20
			call 	lcdc
			call	del_01
	
			movlw	0x30
			movwf	0x20
			call 	lcdc
			call	mdel
			
			movlw	0x01		; display clear
			movwf	0x20
			call 	lcdc
			call	mdel
	
			movlw	0x06		; 3. ID=1,S=0 increment, no shift 000001 ID S  
			movwf	0x20
			call 	lcdc
			call	mdel
	
			movlw	0x0c		; 4. D=1,C=B=0 set display ,no cursor, no blinking
			movwf	0x20
			call 	lcdc
			call	mdel
	
			movlw	0x38		; dl=1 ( 8 bits interface,n=2 lines,f=5x8 dots)
			movwf	0x20
			call 	lcdc
			call	mdel
			return
	
	;
	;subroutine to write command to LCD
	;
	
	lcdc:	movlw	0x00		; E=0,RS=0 
			movwf	PORTE
			movf	0x20,w
			movwf	PORTD       ;;;;;;;
			movlw	0x01		; E=1,RS=0
			movwf	PORTE
	        call	sdel
			movlw	0x00		; E=0,RS=0
			movwf	PORTE
			return
	
	;
	;subroutine to write data to LCD
	;
	
	lcdd:	movlw		0x02		; E=0, RS=1
			movwf		PORTE
			movf		0x20,w
			movwf		PORTD
	        movlw		0x03		; E=1, rs=1  
			movwf		PORTE
			call		sdel
			movlw		0x02		; E=0, rs=1  
			movwf		PORTE
			return
	
	;----------------------------------------------------------
	;aTOd delays
	d_20:	movlw	0x20
			movwf	0x70 ; changed in order to not harm lcd init 
	lulaa1_AD:
			decfsz	0x70, f
			goto	lulaa1_AD
			return
	
	d_4:	movlw	0x06
			movwf	0x71
	lulaa2_AD:	
			decfsz	0x71, f
			goto	lulaa2_AD
			return
	;----------------------------------------------------------;
	;LCD delays
	del_41	movlw		0xcd        ; (11001101)B or (205)d
			movwf		0x23
	lulaa6	movlw		0x20		; (00010100)B or (32)d 
			movwf		0x22        
	lulaa7	decfsz		0x22,1      ; counting down from 32 to 0
			goto		lulaa7     
			decfsz		0x23,1      ; counting down from 205 to 0
			goto 		lulaa6 
			return                  ; 
	
	
	del_01	movlw		0x20
			movwf		0x22
	lulaa8	decfsz		0x22,1
			goto		lulaa8
			return
	
	
	sdel	movlw		0x19		; movlw = 1 cycle
			movwf		0x23		; movwf	= 1 cycle
	lulaa2	movlw		0xfa
			movwf		0x22
	lulaa1	decfsz		0x22,1		; decfsz= 12 cycle
			goto		lulaa1		; goto	= 2 cycles
			decfsz		0x23,1
			goto 		lulaa2 
			return
	
	
	mdel	movlw		0x0a         
			movwf		0x24
	lulaa5	movlw		0x19
			movwf		0x23
	lulaa4	movlw		0xfa 		;;;;;;;;
			movwf		0x22
	lulaa3	decfsz		0x22,1
			goto		lulaa3
			decfsz		0x23,1
			goto 		lulaa4 
			decfsz		0x24,1
			goto		lulaa5
			return
	
	;----------------------------------------------------------
	
	psika:		
			movwf	0x7A				; store W_reg --> 0x7A
			swapf	STATUS,w
			movwf	0x7B				; store STATUS --> 0x7B
			
			btfsc	PIR1, TMR1IF		; check timer1 int flag
			goto	Timer1
			btfsc	PIR1,ADIF
			goto	AtD
	ERR:	goto	ERR
	
	Timer1:
			bcf		T1CON, TMR1ON		; stop timer1
			decf    0x30,1 				; decrese the counter.
											
			movlw	0xF0                
			movwf	TMR1L
			movlw	0x0A               
			movwf	TMR1H				; reset the timer 1 to 2800d
			bcf		PIR1, TMR1IF		; puts the flag down
			bsf		T1CON, TMR1ON		; start timer1
			goto 	ret          
		
	AtD:	
			bcf 	PIR1, ADIF			; Clear A-to-D Flag
			movf	ADRESH, w
			movwf 	0X4A
			call	d_4		
			bsf 	ADCON0, GO			; Start new conversion
		
	ret:			
			swapf	0x7B, w
			movwf	STATUS				;restore STATUS <-- 0x7B
			swapf	0x7A, f
			swapf	0x7A, w				;restore W_reg <-- 0x7A
			retfie
	;----------------------------------------------------------------;		
	end
