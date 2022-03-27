;Squeeker beeper engine by Zilogat0r
;- original version 2000
;- size optimized version 2012
;- new data format and loader by utz 2015

	;; org #8000

init
;; 	ei			;detect kempston
;; 	halt
;; 	in a,(#1f)
;; 	inc a
;; 	jr nz,_skip
;; 	ld (maskKempston),a
;; _skip
	di
	exx
	push hl			;preserve HL' for return to BASIC
	ld (oldSP),sp
	ld hl,musicData
	;ld a,(hl)		;configure global duty setting
	;ld (duty),a
	;inc hl
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

	;jp exit		;uncomment to disable looping
        .(when (defined? 'row-play)
           " halt")

	ld sp,loop		;get loop point
	jr .(+ 3 (symbol-ref 'rdseq))

;******************************************************************
rdptn0
	ld a,(de)		;read pattern duty
	ld (duty),a

	inc de
	ld (patpntr),de
rdptn
;; 	in a,(#1f)		;read joystick
;; maskKempston equ $+1
;; 	and #1f
;; 	ld c,a
	in a,($fe)		;read kbd
	cpl
	;; or c
	and $1f
	jp nz,exit

patpntr .equ .(+ 1 current-origin)	;fetch pointer to pattern data
	ld sp,0

	pop af
	jr z,setAll		;$40
	jr c,set123		;$01
	jp pe,set12		;$04
	jp m,set1		;$80
	jp setNone		;$00

setAll
	pop hl
	ld (.(+ 12 (symbol-ref 'rowBuffer))),hl
	ld (.(+ 14 (symbol-ref 'rowBuffer))),hl

set123
	pop hl
	ld (.(+ 8 (symbol-ref 'rowBuffer))),hl
	ld (.(+ 10 (symbol-ref 'rowBuffer))),hl

set12
	pop hl
	ld (.(+ 4 (symbol-ref 'rowBuffer))),hl
	ld (.(+ 6 (symbol-ref 'rowBuffer))),hl

set1
	pop hl
	ld (rowBuffer),hl
	ld (.(+ 2 (symbol-ref 'rowBuffer))),hl

setNone
	or a
	jr z,rdseq

	ld (patpntr),sp

	ld h,a			;set speed
	ld l,0

;******************************************************************
mxb
	exx
	xor a
	ld bc,$0400
	ld sp,rowBuffer

mxa
	rl c
	pop de
	pop hl
	add hl,de
	push hl
	pop hl
duty .equ .(+ 1 current-origin)
	ld a,40			;duty
	add a,h
	djnz mxa

	ld a,15
	adc a,c
	out ($fe),a
	exx
	dec hl
	ld a,h
	or l
	jr nz,mxb

	jp rdptn

;******************************************************************
rowBuffer			;stack buffer for current row
	.ds 16

;******************************************************************
exit
oldSP .equ .(+ 1 current-origin)
	ld sp,0
	pop hl
	exx
	ei
	ret
;******************************************************************

musicData
loop
	;; include "music.asm"
