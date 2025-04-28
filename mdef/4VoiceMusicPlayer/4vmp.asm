;*******************************************************************************
;*      4-Voice Music Player - adapted by Paul Fiscarelli                      *
;*                                                                             *
;*	NUKED TO INFINTY BY Simon Jonassen (for the fun of it)		       *
;*      and also CORRECTED TO USE THE PROPER HARMONIC FREQ TABLE ETC           *
;*                                                                             *
;*      Original author credits to:                                            *
;*       Clell A. Dildy Jr. -'68' Micro Journal, March 1982                    *
;*       Garry and Linda Howard - Color Computer News, July 1982               *
;*       Larry Konecky - Rainbow Magazine, December 1983                       *
;*       Bob Ludlum - Rainbow Magazine, July 1984, Music+ 3/11/84              *
;*                                                                             *
;*      Original algorithm credits to:                                         *
;*       Hal Chamberlain - Byte Magazine, September 1977                       *
;*                                                                             *
;*******************************************************************************
		;; opt	cd
		;; opt	6809

                ;; org     $e00
Start           orcc    #$50    ; disable IRQ+FIRQ
                lda     #$3F
                sta     $ff23
		sta	$ffd9
;; ****************************************
;; * SETUP DP FOR SPEED
;; ****************************************
		lda	#.(msb current-origin)
		tfr	a,dp
		;; setdp	.(msb current-origin)
;; ****************************************
;; * RESET TUNE POINTER
;; ****************************************
		ldd	#Note			;; +1800*5 ;#note
		std	<.(+ 1 (symbol-ref 'npt))
;; ****************************************
;; *SETUP TIMER IRQ
;; ****************************************
		lda	#32
		sta	$ff91			; enable 279ns timer
		sta	$ff93			; firq source = timer
		ldd	#455			; timer value (12bit) 455 = 7875 Hz (ish)
		std	$ff94
;; ****************************************
;; * INSTALL DP JMP AT VECTOR
;; ****************************************
		lda	#$0e			; DP JMP instruction
		ldb	#.(lsb (symbol-ref 'irqnote))	; address of player
		std	$fef4			; IRQ JUMP VECTOR
;; ****************************************
;; * ENABLE GIME FIRQ + CONSTANT $FE00
;; ****************************************
		lda	#%10011100
		sta	$ff90
;; ****************************************
;; * ENABLE THE TUNE BY ACTIVATING FIRQ
;; ****************************************
		andcc	#$bf

loop		inc	$400
l2              nop             ; stuff instruction register TODO remove when
                nop             ; compiling to file
l3              nop
                nop
		jmp	<loop

;*******************************************************************************
; PLAY DA NOTEZ
;*******************************************************************************
		;; opt	cc
		;; opt	ct

irqnote		std	<.(+ 1 (symbol-ref 'oldd))	;store copy of D register

MVoice1	        ldd     #$0000			;cumulative counter #1 (fixed point)
		sta	<.(+ 2 (symbol-ref 'm1)) ;save the *integer* (pointer into our waveform)
Freq1           addd    #$0000			;add frequency
                std     <.(+ 1 (symbol-ref 'MVoice1))	;save back to counter

MVoice2         ldd     #$0000			;same comments as above
		sta	<.(+ 2 (symbol-ref 'm2))
Freq2           addd    #$0000
                std     <.(+ 1 (symbol-ref 'MVoice2))

MVoice3         ldd     #$0000
		sta	<.(+ 2 (symbol-ref 'm3))
Freq3           addd    #$0000
                std     <.(+ 1 (symbol-ref 'MVoice3))

MVoice4         ldd     #$0000
		sta	<.(+ 2 (symbol-ref 'm4))
Freq4           addd    #$0000
                std    	<.(+ 1 (symbol-ref 'MVoice4))

;; m1	        lda    	WFTableSw		;saw (come on - you work it out)
;; m2              adca    WFTableSw		;
;; m3              adca 	WFTableSq		;
;; m4		adca	WFTableSw		;
m1	        lda    	wftable1		;saw (come on - you work it out)
m2              adca    wftable2		;
m3              adca 	wftable3		;
m4		adca	wftable4		;

                sta     $ff20			;out on dac

		dec	<tempo			;slow me down
		beq	seq			;counter=0 so sequence
		lda	$ff93
oldd		ldd	#$0000			;grab old D value
		rti				;exit irq

;*******************************************************************************
; SEQUENCER
;
; WHEN WE GET TO THIS PART, WE HAVE ALREADY PROCESSED THE ABOVE CODE AND THUS
; HAVE OUR D VALUE SAVED ! - SO NO PSHS/PULS MALARKY HERE
;*******************************************************************************
		;; opt	cc
		;; opt	ct

seq		lda	#MTempo		;restore tempo
		sta	<tempo
		dec	<MDur		;is the duration done ?
		bne	out		;nope
		stu	<.(+ 1 (symbol-ref 'oldu))	;save current U value

npt		ldu	#Note		;current row in patterns
npt0
		lda	,u+		;get duration byte
		sta	<MDur		;store
		bne	oks
                .(if (defined? 'row-play)
                     ;; " clra\n sta $ff20\n sta $ff91\n sta $ff93\n sta $94"
                        " ldd #$b700\n std l2\n lda #$94\n sta l3\n rti" ;
                     " ldu #Note\n bra npt0")

oks		pulu    a,b ;; pulu	d		;get 1st 2 notes
		sta	<.(+ 2 (symbol-ref 'v1))	;store to voice #1
		stb	<.(+ 2 (symbol-ref 'v2))	;store to voice #2
		pulu    a,b ;; pulu	d		;get 2nd 2 notes
		sta	<.(+ 2 (symbol-ref 'v3))	;store to voice #3
		stb	<.(+ 2 (symbol-ref 'v4))	;store to voice #4
xpt		stu	<.(+ 1 (symbol-ref 'npt))	;keep our new 4 note pointer

v1		ldu	freqtab	;get the right freq
		stu	<.(+ 1 (symbol-ref 'Freq1))	;store
v2		ldu	freqtab	;get the right freq
		stu	<.(+ 1 (symbol-ref 'Freq2))	;store
v3		ldu	freqtab	;get the right freq
		stu	<.(+ 1 (symbol-ref 'Freq3))	;store
v4		ldu	freqtab	        ;get the right freq
		stu	<.(+ 1 (symbol-ref 'Freq4))	;store

		lda	$ff93
oldu		ldu	#$0000		;our previous X value (before entering irq)
out		ldd	<.(+ 1 (symbol-ref 'oldd))	;our previous D value
		rti


		;; opt	cc
		;; opt	ct

;; MTempo		.equ	$0a		;global tempo
MDur		.db	$a		;note duration (gets changed)
tempo		.db	$0a		;working tempo (decreases)

;*******************************************************************************
;*                                                                             *
;*      Setup Frequency Table                                                  *
;*      'A' above Middle-C (440 Hertz) located at $0DAB                        *
;*                                                                             *
;*******************************************************************************
                .align   $100

freqtab
c0		.dw	$0000,$0001,$0002,$0004,$0008,$0010,$0020,$0040,$0080,$00da,$00e7,$00f5
c1		.dw	$0104,$0113,$0123,$0135,$0147,$015b,$016f,$0185,$019c,$01b5,$01cf,$01eb
c2	        .dw     $0208,$0227,$0247,$026A,$028F,$02B6,$02DF,$030B,$0339,$036A,$039E,$03D6
c3              .dw     $0410,$044E,$048F,$04D5,$051E,$056C,$05BF,$0617,$0673,$06D5,$073D,$07AC
c4              .dw     $0821,$089C,$091F,$09AA,$0A3D,$0AD9,$0B7F,$0C2E,$0CE7,$0DAB,$0E7C,$0F58
c5              .dw     $1042,$1139,$123F,$1355,$147B,$15B3,$16FE,$185C,$19CE,$1B57,$1CF8,$1EB0
c6              .dw     $2084,$2273,$247F,$26AB,$28F7,$2B67,$2DFC,$30B8,$339D,$36AF,$39F0,$3D61
c7              .dw     $4108,$44E6,$48FF,$4D56,$51ee,$56ce,$5bf8,$6170,$673a,$6d5e,$73e0,$7ac2


;*******************************************************************************
;FUNDAMENTAL AMPLITUDE 1.0 ( REFERENCE)
;SECOND HARMONIC .5, IN PHASE WITH FUNDAMENTAL
;THIRD HARMONIC  .5, 90 DEGREES LEADING PHASE
;*******************************************************************************

                .align	$100

wavetables
