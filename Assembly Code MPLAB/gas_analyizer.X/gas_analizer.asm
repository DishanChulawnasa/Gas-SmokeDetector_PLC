List p=16f877A                      ;List directive to define processor
Include "P1C6F877A.INC"              ;Processor specific variable definitions; Special function registers
 __CONFIG _CP_OFF& _DEBUG_OFF& _WRT_OFF& _CPD_OFF& _LVP_OFF& _BODEN_OFF& _PWRTE_ON& _WDT_OFF& _XT_OSC

     
;PINS in LCD to PIC16F877A PORTC PINS
	;DB7 (14) -----RC7(40)
	;DB6 (13) ----RC6(39)
	;DB5 (12) ----RC5(38)
	;DB4 (11) ----RC4(37)
	;DB3 (10) ----RC3(36)
	;DB2 (9)---- RC2(35)
	;DB1 (8) ----RC1(34)
	;DB0 (7) ----RC0(33)
	;E (6) ------RD5(28)
	;RW (5) -----RD6(29)
	;RS (4) -----RD4(27)
	;Vo (3) -----+5V
	;Vdd (2) ----+5V
	;Vss (1) -----GND

	
    ;define General Function Registers for control lines
    RS	    EQU 0x04 ;For RS(Register Select) pin in the LCD, Level=H/L,  H:Data Mode, L:Instruction Mode
    E	    EQU 0x05 ;For E(Enable) pin in the LCD,  Level=H;H-->L  ,Chip Enable Signal    
    RW	    EQU 0x06 ;For RW(Read/Write) pin in the LCD, Level=H/L,  H:Read Mode, L:Write Mode
    
    ;MQ2     EQU 0x01 ;
	    
    ;Counter GFRs register adresses
    CBLOCK 0x20                        
        Count
	Countx
    ENDC

	org	0x00                   ;Reset Vector
	goto    Main 
	org	0x05
    
Main:
       ; bsf     TRISA,MQ2 
        bsf     STATUS,5               ;switch to bank1
	clrf	TRISD                  ;RD0 to RD7 all are outputs
	clrf	TRISC                  ;RB0 to RB7 all are outputs    
	bcf     STATUS,5               ;switch back to bank0
	
Start:
        call    LCDinitailize
	btfss   PORTB,0                 ;(Bit Test Skip f Set) If 0V at RB0 pin, next line will be executed, If 5V at RB0 pin, next line will be discarded.
	goto    Nogas
	goto    Gasdetected
			
	
LCDinitailize:

	clrf	PORTC
	clrf	PORTD                  

	;LCD routine starts
	call	Delay
	call	Delay
	
	;give LCD module to reset automatically
	;Fundtion Set for 8-bit(L=1), 2-line display(N=1), and 5x7 dot matrix(F=0)
	movlw	0x38                    ;0x38=00111000-->001LNFXX
	call	Instwrite
	
	;Display Control for Display On(D=1), Cursor On(C=1), No blinking(B=0)
	movlw	0x0E                    ;0x0E=00001110-->00001DCB
	call	Instwrite
	
	;Entry Mode Set for DD RAM address increment by one(M=1) and cursor shift to right(S=0)
	movlw	0x06                    ;0x06=00000110-->000001MS
	call	Instwrite
	
	;Clear Display - clears all display and returns the cursor to the home position.
	movlw	0x01                    ;00000001
	call	Instwrite
	
	;Set DD RAM Address - Set the Display Data(DD) RAM address. DD RAM data is sent and recieved after this set.
	movlw	0x80                    ;10000000
	call	Instwrite
	Return


Gasdetected:
	;WRITE DATA in the 1st position of line 1
	;Characters (G, A and S)
	movlw	0x47 ;G
	call	Datawrite
	movlw	0x41 ;A
	call	Datawrite
	movlw	0x53 ;S
	call	Datawrite
	
	;Set DDRAM address for the next (D, E, T, E, C, T, E and D) in line 2
        ;Set DDRAM address for the 1st position of line 2 (40h)
	movlw	0xC0      ;11000000
	call	Instwrite ;RS=0
	
	movlw	0x44 ;D
	call	Datawrite
	movlw	0x45 ;E
	call	Datawrite
	movlw	0x54 ;T
	call	Datawrite
	movlw	0x45 ;E
	call	Datawrite
	movlw	0x43 ;C
	call	Datawrite
	movlw	0x54 ;T
	call	Datawrite
	movlw	0x45 ;E
	call    Datawrite
	movlw	0x44 ;D
	call    Datawrite
	
	
	GOTO    Start
	
Nogas:
	;WRITE DATA in the 1st position of line 1
	;Characters (N and O)
	movlw	0x4E ;N
	call	Datawrite
	movlw	0x4F ;O
	call	Datawrite
		
	;Set DDRAM address for the next characters (G, A and S) in line 2
        ;Set DDRAM address for the 1st position of line 2 (40h)
	movlw	0xC0
	call	Instwrite ;RS=0
	
	movlw	0x47 ;G
	call	Datawrite
	movlw	0x41 ;A
	call	Datawrite
	movlw	0x53 ;S
	call	Datawrite

	GOTO    Start
	
;subroutine to write instructions (Instwrite), Instruction to be written is stored in W before the call
Instwrite:	movwf   PORTC
		call    Delay      ;delay may not be needed
		bcf     PORTD,RS   ;RS=0: Instruction Write
		call    Delay
		bsf     PORTD,E
		call    Delay
		bcf     PORTD,E
		call    Delay
		return
	
;Subroutine to Write Data
Datawrite:	movwf   PORTC
		call    Delay      ;delay may not be needed
		bsf     PORTD,RS   ;RS=1: Data Write
		call    Delay
		bsf     PORTD,E
		call    Delay
		bcf     PORTD,E    ;Transitional E signal
		call    Delay
		return
	

;Delay Soubroutine
Delay: 
		movlw   D'10'                   
		movwf   Countx     
Delayloop:
		decfsz	Count,1              
		goto	Delayloop
		decfsz	Countx,1              
		goto	Delayloop 
		return
		
END
	
