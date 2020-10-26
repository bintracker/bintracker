;quattropic
;beeper engine by utz 08'2015


;HL  = add counter ch1/noise
;DE  = base freq ch1/noise
;IX  = add counter ch2
;BC  = base freq ch2
;IY  = add counter ch3
;DE' = base freq ch3
;HL' = add counter ch4
;SP  = base freq ch4
;BC' = timer

	;; di
init
	;; ei			;detect kempston
	;; halt
	;; in a,($1f)
	;; inc a
	;; jr nz,_skip
	;; ld (maskKempston),a
_skip
	di
	exx
	push hl			;preserve HL' for return to BASIC
	ld (oldSP),sp
	ld hl,musicdata
	;; ld a,(hl)
	;; ld (speed),a
	;; inc hl
	ld (seqpntr),hl

;******************************************************************
rdseq
seqpntr .equ .(+ 1 current-origin)
	ld sp,0
	xor a
	pop de			;pattern pointer to DE
	or d
	ld (seqpntr),sp
	jr nz,rdptn0

;; halt -> jp exit
        .(if (symbol-ref 'row-play)
             " halt\n"
             " ld sp,loop\n jp .(+ 3 (symbol-ref 'rdseq))\n")

;******************************************************************
rdptn0
	ld (patpntr),de
rdptn
;; 	in a,($1f)		;read joystick
;; maskKempston .equ .(+ 1 (current-origin))
;; 	and $1f
;; 	ld c,a
	in a,($fe)		;read kbd
	cpl
	;; or c
	and $1f
	jp nz,exit

;; speed .equ .(+ 2 (current-origin))
;; 	ld bc,0			;timer
;; 	exx
        ld c,0
        exx


patpntr .equ .(+ 1 current-origin)			;fetch pointer to pattern data
	ld sp,0

	pop af
	jr z,rdseq

        exx
        ld b,a
        exx

	jp c,noiseCore
	jp pe,slideCore
	jp m,noiseslideCore

;******************************************************************
regularCore
	;; ld (ch1Length),a

	ld a,$10
	ld (stopch),a

	pop hl			;duty1,2
	ld a,h
	ld (duty1),a
	ld a,l
	ld (duty2),a

	pop hl			;duty3,4
	ld a,h
	ld (duty3),a
	ld a,l
	ld (duty4),a

	pop de			;freq1
	pop bc			;freq2

	ld hl,0
	ld ix,0
	ld iy,0

	exx

	pop de			;freq3
	pop hl			;freq4

	ld (patpntr),sp		;preserve data pointer
	ld sp,hl
	ld hl,0

;******************************************************************
playRegular
	exx			;4
	add hl,de		;11
	ld a,h			;4
duty1 .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	add ix,bc		;15
	ld a,ixh		;8
duty2 .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	nop			;4
	nop			;4

	exx			;4

	add hl,sp		;11
	ld a,h			;4
duty4 .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
stopch .equ .(+ 1 current-origin)
	and $10			;7
	out ($fe),a		;11


	add iy,de		;15
	ld a,iyh		;8
duty3 .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	dec c			;4
	jr nz,playRegular	;12
				;224
;; 	ld a,b
;; ch1Length .equ .(+ 1 (current-origin))
;; 	sub $ff				;= timer - actual length
;; 	jr z,_skip
	djnz playRegular
	jp rdptn

_skip
	ld (stopch),a

	djnz playRegular
	jp rdptn


;******************************************************************
noiseCore
	;; ld (ch1Lengtha),a

	ld a,$10
	ld (stopcha),a

	pop hl			;duty1,2
	ld a,h
	ld (duty1a),a
	ld a,l
	ld (duty2a),a

	pop hl			;duty3,4
	ld a,h
	ld (duty3a),a
	ld a,l
	ld (duty4a),a

	pop de			;freq1

	pop bc			;freq2

	ld hl,0
	ld ix,0
	ld iy,0

	exx

	pop de			;freq3
	pop hl			;freq4

	ld (patpntr),sp		;preserve data pointer
	ld sp,hl
	ld hl,0

;******************************************************************
playNoise
	exx			;4
	add hl,de		;11
	ld a,h			;4
duty1a .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	add ix,bc		;15
	ld a,ixh		;8
duty2a .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	exx			;4
	rlc h

	add hl,sp		;11
	ld a,h			;4
duty4a .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
stopcha .equ .(+ 1 current-origin)
	and $10			;7
	out ($fe),a		;11

	add iy,de		;15
	ld a,iyh		;8
duty3a .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	dec c			;4
	jr nz,playNoise		;12
				;224
;; 	ld a,b

;; ch1Lengtha .equ .(+ 1 (current-origin))
;; 	sub $ff				;= timer - actual length
;; 	jr z,_skip
	djnz playNoise
	jp rdptn

_skip
	ld (stopcha),a
	djnz playNoise
	jp rdptn


;******************************************************************
slideCore
	;; ld (ch1Lengthb),a

	ld a,$10
	ld (stopchb),a

	pop hl			;duty1,2
	ld a,h
	ld (duty1b),a
	ld a,l
	ld (duty2b),a

	pop hl			;duty3,4
	ld a,h
	ld (duty3b),a
	ld a,l
	ld (duty4b),a

	pop de			;freq1
	pop bc			;freq2

	ld hl,0
	ld ix,0
	ld iy,0

	exx

	pop de			;freq3
	pop hl			;freq4

	ld (patpntr),sp		;preserve data pointer
	ld sp,hl
	ld hl,0

;******************************************************************
playSlide
	exx			;4
	add hl,de		;11
	ld a,h			;4
duty1b .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	add ix,bc		;15
	ld a,ixh		;8
duty2b .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	;nop			;4
	;nop			;4
	exx			;4

	add hl,sp		;11
	ld a,h			;4
duty4b .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
stopchb .equ .(+ 1 current-origin)
	and $10			;7
	out ($fe),a		;11

	add iy,de		;15
	ld a,iyh		;8
duty3b .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4

	and $10			;7
	out ($fe),a		;11

	nop
	nop

	dec c			;4
	jr nz,playSlide		;12
				;224
	ld a,b

;; ch1Lengthb .equ .(+ 1 (current-origin))
;; 	sub $ff				;= timer - actual length
;; 	jr z,_skip
	srl d
	djnz playSlide
	jp rdptn

_skip
	ld (stopchb),a
	djnz playSlide
	jp rdptn


;******************************************************************
noiseslideCore
	;; ld (ch1Lengthc),a

	ld a,$10
	ld (stopchc),a

	pop hl			;duty1,2
	ld a,h
	ld (duty1c),a
	ld a,l
	ld (duty2c),a

	pop hl			;duty3,4
	ld a,h
	ld (duty3c),a
	ld a,l
	ld (duty4c),a

	pop de			;freq1
	pop bc			;freq2

	ld hl,0
	ld ix,0
	ld iy,0

	exx

	pop de			;freq3
	pop hl			;freq4

	ld (patpntr),sp		;preserve data pointer
	ld sp,hl
	ld hl,0

;******************************************************************
playNoiseSlide
	exx			;4
	add hl,de		;11
	ld a,h			;4
duty1c .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	add ix,bc		;15
	ld a,ixh		;8
duty2c .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	exx			;4
	;rlc h			;8

	add hl,sp		;11
	ld a,h			;4
duty4c .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
stopchc .equ .(+ 1 current-origin)
	and $10			;7
	out ($fe),a		;11

	add iy,de		;15
	ld a,iyh		;8
duty3c .equ .(+ 1 current-origin)
	cp $80			;7
	sbc a,a			;4
	and $10			;7
	out ($fe),a		;11

	rlc h
	dec c			;4
	jr nz,playNoiseSlide	;12
				;224
	ld a,b

;; ch1Lengthc .equ .(+ 1 (current-origin))
;; 	sub $ff				;= timer - actual length
;; 	jr z,_skip
	srl d
	djnz playNoiseSlide
	jp rdptn

_skip
	ld (stopchc),a
	djnz playNoiseSlide
	jp rdptn


;******************************************************************
exit
oldSP .equ .(+ 1 current-origin)
	ld sp,0
	pop hl
	exx
	ei
	ret
;******************************************************************


musicdata
loop
;; loop .equ .(+ 1 (current-origin))
