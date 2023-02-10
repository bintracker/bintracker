;********************************************
; 2 voice squarewave pattern player
; for 1 bit output at $bfff on MC10
; (C) 2021 Simon Jonassen (Invisible Man)
;
; FREE FOR ALL - USE AS YOU SEE FIT, JUST
; REMEMBER WHERE IT ORGINATED AND GIVE CREDIT
;********************************************
		;; PROCESSOR 	6803
;********************************************
; MC-10 clock is .89488625Mhz or 894.88625 KHz
; We need 7.875Khz interrupts so 894.88625 / 7.875
; = 114 (113.6xxxx rounded up)
;********************************************
TVAL		.equ		114		; Timer period 128
;*************************************
;some equates for the 6803 hardware
;*************************************
TCSR		.equ		$0008		; Timer Control Status Register
TIMER		.equ		$0009		; Counter ($9/$a)
OCR		.equ		$000B		; Output Compare Register ($b/$c)
OCV		.equ		$4206		; Output Compare interrupt Vector
TCSRVAL		.equ		$08		; bit settings for the TCSR
;********************************************
;* Main
;********************************************
		;; org		$5000
start		sei				;disable irq's
;********************************************
; SETUP IRQ ROUTINE
;********************************************
		ldaa		#$7e		;load 'jmp' instruction opcode
		ldx		#note		;irq handler address after JMP instruction
		staa		OCV		;store into OCV vector
		stx		.(+ 1 (symbol-ref 'OCV))
		ldd		#TVAL		;set the timer duration
		std		<OCR
		staa		<TIMER		;reset the counter to $FFF8
		ldaa		#TCSRVAL	;Enable the timer interrupt
		staa		<TCSR

;********************************************
; ENABLE IRQ
;********************************************
		cli				;enable irq's
poop		inc		$4000		;program here spudz (is we runnin irq's)
		jmp		poop
		rts

;********************************************
; PLAYER ROUTINE
;********************************************
note		staa		<TIMER		;Reset the timer
		ldaa		<TCSR		;Reset the OCF flag
		ldd		#TVAL		;set the timer duration
		std		<OCR
		dec		frames		;
		bne		sum		;(2 ticks per row (ish))
		ldaa		#$c8		;$c0
		staa		frames
		com		.(+ 1 (symbol-ref 'frames))
		beq		sum
;********************************************
; SEQUENCER
;********************************************

oldx		ldx		#zix		;save pattern position
curnote		ldd		,x
		inx
		inx
		cpx		#endzix
		bne		plnote
                .(if (defined? 'row-play)
                     " dec len\n bne next\n sei\nhalt bra halt\nnext"
                     " ldx #zix")
		;; ldx		#zix
plnote		stx		.(+ 1 (symbol-ref 'oldx)) ; restore pattern position to start
		staa		.(+ 2 (symbol-ref 'frq1))
		stab		.(+ 2 (symbol-ref 'frq2))
frq1		ldx		#freqtab	;get the right freq
		ldx		,x
		stx		.(+ 1 (symbol-ref 'freq))		;store
frq2		ldx		#freqtab
		ldx		,x
		stx		.(+ 1 (symbol-ref 'freq2))
		rti

;********************************************
; NOTE ROUTINE
;********************************************

sum		ldd 		#$0000
freq		addd 		#$0000
		std 		.(+ 1 (symbol-ref 'sum))


sum2		ldd		#$0000
		bcs 		freq2		;tripped on overflow from above summation
		addd		.(+ 1 (symbol-ref 'freq2)) ;add the new freq (ch2)
		std		.(+ 1 (symbol-ref 'sum2))	;store it
		bcs		bit_on		;carry (overflow on above add)

bit_off		ldaa		#0		;turn off 1bit
		staa		$bfff		;set the hardware
		rti

freq2		addd		#$0000		;our 1st SUM tripped an overflow
		std		.(+ 1 (symbol-ref 'sum2))	;and we store back to sum #2
bit_on		ldaa		#128		;turn on 1bit
		staa		$bfff		;set the hardware
		rti
;******************************************************
; variables
;******************************************************
frames		.dw	$c800
.(if (defined? 'row-play)
     "len .db 4")

		.align	$100
;******************************************************
;equal tempered 12 note per octave frequency table
;
;7.875 Khz vals here
;
;val= freq / 7.875 / 8		'7.875Khz
;counter=val*256
;
;actual musical freq's - like say 32,70Hz for c1 etc...
;
;entry 0 is SILENCE (would be c1)
;******************************************************
freqtab
;c0	.dw	0,70,75,79,83,88,94,99,105,111,118,125
c1	.dw	0,133,141,149,158,167,177,188,199,211,223,237,251
c2	.dw	266,282,298,316,335,355,376,398,422,447,474,502
c3	.dw	532,563,597,632,670,710,752,796,844,894,947,1003
c4	.dw	1063,1126,1193,1264,1339,1419,1503,1593,1688,1788,1894,2007
c5	.dw	2126,2253,2387,2529,2679,2838,3007,3186,3375,3576,3789,4014
c6	.dw	4252,4505,4773,5057,5358,5676,6014,6371,6750,7152,7577,8028
c7	.dw	8505,9011,9546,10114,10716,11353,12028,12743,13501,14303,15154,16055
c8	.dw	17010,18021,19093,20228,21431,22705,24056,25486,27001,28607,30308,32110



;********************************************
;ONE LONG PATTERN TO SAVE CYCLES OF SEQ
;********************************************
zix	;; include		"pop.asm"
;; endzix	.dw	$ffff	;signal loop

;; 	end	start
