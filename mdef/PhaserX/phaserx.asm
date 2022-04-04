;PhaserX
;by utz 09'2016 * www.irrlichtproject.de
;*******************************************************************************

;; include	"equates.h"
;; BORDER .equ $14

	;; org #8000

	di
	exx
	push hl			;preserve HL' for return to BASIC
	ld (oldSP),sp
	ld hl,musicData
	ld (seqpntr),hl
	ld iyl,0		;timer lo

;*******************************************************************************
rdseq
seqpntr .equ .(+ 1 current-origin)
	ld sp,0
rdseq0
	xor a
	pop de			;pattern pointer to DE
	or d
	ld (seqpntr),sp
	jr nz,rdptn0

        .(if (defined? 'row-play)
             " halt"
             ;; " ld sp,loop\n jr .(+ 3 (symbol-ref 'rdseq))"
             " ld sp,mdal__order_patterns_loop\n jr rdseq0")

	;; ;jp exit		;uncomment to disable looping

	;; ld sp,loop		;get loop point
	;; jr rdseq+3

;*******************************************************************************
exit
oldSP .equ .(+ 1 current-origin)
	ld sp,0
	pop hl
	exx
	ei
	ret

;*******************************************************************************
rdptn0
	ld (ptnpntr),de

readPtn
	in a,($fe)		;read kbd
	cpl
	and $1f
	jr nz,exit


ptnpntr .equ .(+ 1 current-origin)
	ld sp,0

	pop af			;speed + drums
	jr z,rdseq

	jp c,drum1
	jp m,drum2

	ex af,af'
drumret

	pop af			;flags + mix_method (xor = $ac, or = $b4, and = a4)
	ld (mixMethod),a

	jr c,noUpdateCh1

	exx

	ld a,$9f		;sbc a,a
	jp pe,setSid

	ld a,$97		;sub a,a

setSid
	ld (sid),a

	ld hl,$04cb		;rlc h
	jp m,setNoise

	ld hl,$0

setNoise
	ld (noise),hl

	pop bc			;dutymod/duty 1
	ld a,b
	ld (dutymod1),a

	pop de			;freq1
	ld hl,0			;reset ch1 accu

	exx

noUpdateCh1
	jr z,noUpdateCh2

	pop hl			;dutymod 2a/b
	ld a,h
	ld (dutymod2a),a
	ld a,l
	ld (dutymod2b),a

	pop bc			;duty 2a/b

	pop de			;freq 2a
	pop hl			;freq 2b
	ld (freq2b),hl

	pop ix			;phase 2b
	ld hl,0			;reset ch2a accu


noUpdateCh2
	ld (ptnpntr),sp
freq2b .equ .(+ 1 current-origin)
	ld sp,0


;*******************************************************************************
playNote
	exx			;4

	add hl,de		;11
sid
	sbc a,a			;4	;replace with sub a for no sid
	ld b,a			;4	;temp
	add a,c			;4	;c = duty
	ld c,a			;4

	ld a,b			;4
dutymod1 .equ .(+ 1 current-origin)
	and $0			;7
	xor c			;4
	ld c,a			;4

	cp h			;4
	sbc a,a			;4

noise
	.ds 2			;8	;replace with rlc h for noise
	exx			;4

	add hl,de		;11

	out ($fe),a		;11___104

	sbc a,a			;4
dutymod2a .equ .(+ 1 current-origin)
	and $0			;7
	xor b			;4
	ld b,a			;4
	cp h			;4
	sbc a,a			;4
	ld iyh,a		;8

	add ix,sp		;15
	sbc a,a			;4
dutymod2b .equ .(+ 1 current-origin)
	and $0			;7
	xor c			;4
	ld c,a			;4
	cp ixh			;8
	sbc a,a			;4

mixMethod .equ .(+ 1 current-origin)
	and iyh			;8

	dec iyl			;8
	jr nz,skipTimerHi	;12

	ex af,af'
	dec a
	jp z,readPtn
	ex af,af'

skipTimerHi
	out ($fe),a		;11___120

	jr playNote		;12
				;224


;*******************************************************************************
drum2
	ld (restoreHL),hl
	ld (restoreBC),bc
	ex af,af'
	ld hl,hat1
	ld b, .(- (symbol-ref 'hat1end) (symbol-ref 'hat1))
	jr drentry
drum1
	ld (restoreHL),hl
	ld (restoreBC),bc
	ex af,af'
	ld hl,kick1		;10
	ld b, .(- (symbol-ref 'kick1end) (symbol-ref 'kick1))	;7
drentry
	xor a			;4
_s2
	xor BORDER		;7
	ld c,(hl)		;7
	inc hl			;6
_s1
	out ($fe),a		;11
	dec c			;4
	jr nz,_s1		;12/7

	djnz _s2		;13/8
	ld iyl,$11		;7	;correct tempo
restoreHL .equ .(+ 1 current-origin)
	ld hl,0
restoreBC .equ .(+ 1 current-origin)
	ld bc,0
	jp drumret		;10

kick1					;27*16*4 + 27*32*4 + 27*64*4 + 27*128*4 + 27*256*4 = 53568, + 20*33 = 53568 -> -239 loops -> AF' = $11
	.ds 4,$10
	.ds 4,$20
	.ds 4,$40
	.ds 4,$80
	.ds 4,0
kick1end

hat1
	.db 16,3,12,6,9,20,4,8,2,14,9,17,5,8,12,4,7,16,13,22,5,3,16,3,12,6,9,20,4,8,2,14,9,17,5,8,12,4,7,16,13,22,5,3
	.db 12,8,1,24,6,7,4,9,18,12,8,3,11,7,5,8,3,17,9,15,22,6,5,8,11,13,4,8,12,9,2,4,7,8,12,6,7,4,19,22,1,9,6,27,4,3,11
	.db 5,8,14,2,11,13,5,9,2,17,10,3,7,19,4,3,8,2,9,11,4,17,6,4,9,14,2,22,8,4,19,2,3,5,11,1,16,20,4,7
	.db 8,9,4,12,2,8,14,3,7,7,13,9,15,1,8,4,17,3,22,4,8,11,4,21,9,6,12,4,3,8,7,17,5,9,2,11,17,4,9,3,2
	.db 22,4,7,3,8,9,4,11,8,5,9,2,6,2,8,8,3,11,5,3,9,6,7,4,8
hat1end

musicData
;; loop
	;; include "music.asm"
