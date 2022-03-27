	    ;;     org		$e00

start		orcc		#$50


;******************************************
; SOUND ON DAC
;******************************************
		lda		$ff23
		ora		#$8
		sta		$ff23
;******************************************
; RESET TUNE POINTER
;******************************************
; the following two lines are optional *if* always starting from a 'cold' start.
		ldd		#.(- (symbol-ref 'PATTERNS) 2)
		std		.(+ (symbol-ref 'curpat) 1)
		ldx		#NOPAT 		;force $80 to load on first note load
		ldy		#freqtab	;frequency table pointer
;******************************************
;loop about playing tune
;******************************************
poll		lda		$ff03
		bpl		poll
		lda		$ff02
		jsr		play2
;******************************************
;this is for test only !!!
;
;normal operation would be do 2 hsyncs (or so)
;worth of code
;
;the call the note routine
;******************************************
ddd

		ldb		#$88		;SPEED !
count1
		pshs		b

dd		lda		$ff01
		bpl		dd
		lda		$ff00


dd2		lda		$ff01
		bpl		dd2
		lda		$ff00

		jsr		note

		puls		b
		decb
		bne		count1


		jmp		poll


;************************************************
; OK SO FAR
;************************************************
play2

curnote	ldd	,x++		;load 2 notes from pattern
	bpl	plnote		;pattern ends with $80 (-ve flag set)

curpat	ldx	#.(- (symbol-ref 'PATTERNS) 2)	;load pointer to patterns
	leax	2,x		;bump pattern
	cmpx	#.(+ (symbol-ref 'sequence-end) 2)	;end of patterns, time to loop
nlhalt
        .(if (defined? 'row-play)
             (string-intersperse
               '(" lda #6"
                 " deca"
                 " sta .(+ 1 (symbol-ref 'nlhalt))"
                 " bne start"
                 " nop"
                 " nop"
                 " nop"
                 " blo .(+ (symbol-ref 'nlhalt) 8)")
               "\n")
             " bcc nxpat")      ; TODO is blo (bcs) in the original
	;; blo	nxpat

	ldx	#PATLOOP	;loop point (use instead of pattern begin for another loop)
nxpat	stx	.(+ (symbol-ref 'curpat) 1)
	ldx	,x		;load pointer to notes
	ldd	,x++		;(replace with bra curnote if there are ever blank patterns)

plnote
	asla			;*2 for note freq lookup
ok1	ldu	a,y		;get the right freq
stor1	stu	.(+ (symbol-ref 'freq) 1)		;store
	aslb			;*2 note freq lookup chan2
ok2	ldu	b,y	;get the freq for chan2
stor2	stu	.(+ (symbol-ref 'freq2) 1)

	ldd	,x++
	asla			;*2 for note freq lookup
	ldu	a,y		;get the right freq
	stu	.(+ (symbol-ref 'freq3) 1)		;store
	aslb			;*2 note freq lookup chan2
	ldu	b,y	;get the freq for chan2
	stu	.(+ (symbol-ref 'freq4) 1)
;************************************************
out	rts


note
	lda	$ff93
sum	ldd 	#$0000
freq	addd 	#$0000
	std 	.(+ 1 (symbol-ref 'sum))
	sta	.(+ 1 (symbol-ref 'a1))
sum2	ldd 	#$0000
freq2	addd 	#$0000
	std 	.(+ 1 (symbol-ref 'sum2))
a1	adca	#0
	rora
	sta	.(+ 1 (symbol-ref 'c1_2))
sum3	ldd	#$0000
freq3	addd 	#$0000
	std	.(+ 1 (symbol-ref 'sum3))
        rora
	sta	.(+ 1 (symbol-ref 'c3))
sum4	ldd 	#$0000
freq4	addd 	#$0000
	std 	.(+ 1 (symbol-ref 'sum4))
c1_2	adca	#0
	rora
c3	adca	#0
	rora
	sta	$ff20		;dac mixedsa
	rts

frame	.db	2


;equal tempered 12 note per octave frequency table 8.4Khz

; YOU CAN INCLUDE THE 0,106,112 line aswell if you want (c-1)
; if you do, then line 2 (0,211) the ZERO needs to be 398/2 (zero is silence)

freqtab
	.dw	0,106,112,119,126,133,141,149,158,168,178,188
	.dw	199,211,224,237,251,266,282,299,316,335,355,376
	.dw	398,422,447,474,502,532,564,597,633,670,710,753
	.dw	797,845,895,948,1005,1064,1128,1195,1266,1341,1421,1505
	.dw	1595,1690,1790,1896,2009,2129,2255,2389,2531,2682,2841,3010
	.dw	3189,3379,3580,3793,4018,4257,4510,4779,5063,5364,5683,6021
	.dw	6379,6758,7160,7586,8037,8515,9021,9557,10126,10728,11366,12041
	.dw	12757,13516,14320,15171,16073,17029,18042,19114,20251,21455,22731,24083
	.dw	25514,27032,28640,30342,32146,34058,36084,38228,40502,42910,45462,48166


;; NOPAT
;;         .dl $80000000

PATTERNS
PATLOOP
NOPAT .equ .(+ 4 current-origin)
	;; include "music.asm"
	;; end	start
