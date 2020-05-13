
SEVSEG	EQU	P1
S_LSB	EQU	07H
S_MSB	EQU	06H

HRT_SNS EQU	P2.0

LEDR	EQU	P2.2
LEDY	EQU	P2.3
LEDG	EQU	P2.4

RS	EQU	P2.5
RW	EQU	P2.6
ENB	EQU	P2.7

LCD16	EQU	P3
LCD_ACT	EQU	038H
LCD_ON	EQU	00FH
LCD_CLR	EQU	001H
LCD_LN1	EQU	080H
LCD_LN2	EQU	0C0H
LCD_LN	EQU	000H

;=====! (DATA) !=====;
	ORG	300H

STR_KLM:	DB	'Blue Pill', 0
STR_TTL1:	DB	'Heartbeat', 0
STR_TTL2:	DB	'Detector', 0
STR_INP:	DB	'Please Enter', 0
STR_AGE:	DB	'Your AGE: ', 0
STR_DTT1:	DB	'Finger Sensor', 0
STR_DTT2:	DB	'Waiting...', 0
STR_KEP:	DB	'Keep Finger', 0
STR_ONS:	DB	'On The Sensor', 0
STR_HRT:	DB	'HB: ', 0
STR_BPM:	DB	' (BPM)', 0
STR_SUM:	DB	'Result: ', 0
STR_EXC:	DB	'Excellent', 0
STR_GUD:	DB	'Good', 0
STR_BAD:	DB	'Bad', 0


;=====! (SUBROUTINE) !=====;
	ORG	400H

SHOW7:
		MOV	A, R6
		MOV	B, #4
		SL:	RL	A
			DJNZ	B, SL
		ADD	A, R7
		MOV	SEVSEG, A
		RET

SSTR:
		SETB	SCON.6
		MOV	TMOD, #20H
		MOV	TH1, R0
		SETB	TR1

		SEND:	CLR	A
				MOVC	A, @A+DPTR
				JZ	S_RET
				MOV	SBUF, A
		WAIT:	JNB	TI, WAIT
				CLR	TI

		INC	DPTR
		SJMP	SEND
		S_RET:	RET

DELAY:
		MOV TMOD, #01H
		MOV 	TH0, B
		SETB 	TR0
		HERE:	JNB	TF0, HERE
		CLR 	TR0
		CLR 	TF0
		RET

FIX_DELAY:
		MOV A, #10H
		D_AGAIN:	MOV B, #LOW(-0FFH)
					ACALL DELAY
					DJNZ A, D_AGAIN
		RET

;=====! (LCD SUBROUTINE) !=====;

LCD_INIT:
		MOV	A,	#LCD_ACT
		ACALL	LCD_CMD
		MOV 	A, 	#LCD_ON
		ACALL	LCD_CMD
		MOV 	A, 	#LCD_LN1
		ACALL 	LCD_CMD
		RET

LCD_AUTOWRITE:
		MOV	A,	LCD_LN
		JNZ	LINE2
		MOV 	A,	#LCD_LN1
		ACALL	LCD_CMD
		MOV 	A,	#LCD_CLR
		ACALL	LCD_CMD
		MOV	LCD_LN, #01H
		SJMP	LCD_ONEWRITE
	LINE2:	MOV 	A,	#LCD_LN2
		ACALL	LCD_CMD
		MOV	LCD_LN, #00H
LCD_ONEWRITE:
		CLR	A
		MOVC	A,	@A+DPTR
		JZ	LCD_RET
		ACALL	LCD_TXT
		INC	DPTR
		SJMP	LCD_ONEWRITE
		LCD_RET: RET

LCD_WRITERAM:
		MOV A, @R0
		JZ LCD_RET
		ACALL LCD_TXT
		INC R0
		SJMP LCD_WRITERAM

LCD_CMD:
		MOV 	LCD16, 	A
		CLR 	RS
		CLR 	RW
		SETB 	ENB
		CLR 	ENB
		MOV 	B, 	#LOW(-10H)
		ACALL 	DELAY
		RET

LCD_TXT:
		MOV 	LCD16, 	A
		SETB 	RS
		CLR 	RW
		SETB 	ENB
		CLR 	ENB
		MOV 	B, 	#LOW(-10H)
		ACALL 	DELAY
		RET

HELLO:
		MOV DPTR, #STR_KLM
		ACALL LCD_ONEWRITE
		ACALL FIX_DELAY
		MOV A, #LCD_CLR
		ACALL LCD_CMD
		MOV DPTR, #STR_TTL1
		ACALL LCD_AUTOWRITE
		MOV DPTR, #STR_TTL2
		ACALL  LCD_AUTOWRITE
		ACALL FIX_DELAY
		RET

INPUT:
		MOV DPTR, #STR_INP
		ACALL LCD_AUTOWRITE
		MOV DPTR, #STR_AGE
		ACALL LCD_AUTOWRITE
		RET

LISTEN:		MOV A, #0
		MOV R3, #0
		MOV TH0, #0
		MOV TL0, #0
		LOWPULSE:	JNB	TF0, LOW_DETECT
				CLR 	TF0
				INC	R3
				CJNE	R3, #152, LOW_DETECT
				SJMP 	DONE_10SEC
				LOW_DETECT:	JNB	HRT_SNS, LOWPULSE
		HIGHPULSE:	JNB	TF0, HIGH_DETECT
				CLR 	TF0
				INC	R3
				CJNE	R3, #152, HIGH_DETECT
				SJMP 	DONE_10SEC
				HIGH_DETECT:	JB	HRT_SNS, HIGHPULSE
		SETB TR0
		INC A
		SJMP LOWPULSE
		DONE_10SEC:	CLR TR0
				MOV B, #6
				MUL AB
				RET

CONVERT:	MOV B, #10
		DIV AB
		MOV 32H, B
		MOV B, #10
		DIV AB
		MOV 31H, B
		MOV 30H, A

		ORL 32H, #30H
		ORL 31H, #30H
		ORL 30H, #30H

		RET

SHOWBPM:	MOV A, #LCD_CLR
		ACALL LCD_CMD
		MOV DPTR, #STR_HRT
		ACALL LCD_ONEWRITE
		MOV R0, #30H
		ACALL LCD_WRITERAM
		MOV DPTR, #STR_BPM
		ACALL LCD_ONEWRITE
		MOV A, #LCD_LN2
		ACALL LCD_CMD
		MOV DPTR, #STR_SUM
		ACALL LCD_ONEWRITE
		RET

;========! (MAIN) !========;
	ORG	00H

;Inisiasi Nyalain LCD
ACALL 	LCD_INIT

;Reset Counter
MOV 	S_MSB, #0
MOV 	S_LSB, #0
ACALL 	SHOW7
CLR 	HRT_SNS

MAIN:	ACALL HELLO
	ACALL INPUT
	ACALL LISTEN
	ACALL CONVERT
	ACALL SHOWBPM

HALT:	SJMP HALT





