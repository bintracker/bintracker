;Huby beeper music engine by Shiru (shiru@mail.ru) 04'11
;updated 99b version 11'13
;Schemta version by utz 06'19
;Two channels of tone, no volume, global speed
;One drum, replaces note on the first channel
;The main feature of this engine is the size, under 100 bytes
;Feel free to do whatever you want with the code, it is PD


OP_INCL	.equ $2c

begin
    ld hl,musicData

    ;; .(unless (symbol-ref 'no-loop)
    ;;     " call play\n jp begin\n")

play
	ld c,(hl)				;read speed word
	inc hl
	ld b,(hl)
	inc hl					;speed is in BC
	ld e,(hl)				;read patterns offset
	inc hl
	ld d,(hl)				;it is always in DE, and HL is order list pointer now

readPos
	inc hl
	ld a,(hl)				;read first byte of the order list
	inc hl
	or a
	;; ret z					;if it is zero, it is the end of the song
    .(if (symbol-ref 'no-loop)
         " jr nz,skip\n di\n halt\n"
         " jr z,begin\n")
skip
	push hl					;store the order list pointer
	push de					;store patterns offset
	push bc					;store speed
	ld l,(hl)				;read second byte of the order list
	ld b,2					;calculate addresses of two patterns
_read
	ld h,0					;pattern number*8
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,de				;add patterns offset
	push hl					;store pattern address
	ld l,a					;now second address
	djnz _read
	exx
	pop hl					;restore pattern addresses in alternative set
	pop de

	ld b,8					;play 8 rows
readRow
	ld a,(de)				;read first note
	inc de					;increase first pattern pointer
	exx
	ld h,a
	ld d,a					;avoid pauses between notes caused by longer counter reloads
	exx
	ld a,(hl)				;read second note
	inc hl					;increase second pattern pointer
	exx
	ld l,a
	ld e,a

	cp OP_INCL				;if first note is #2c, it is drum sound
	jr z,.(+ 3 current-origin)
	xor a
	ld (soundLoop_slide),a

	pop bc					;load speed into duration counter
	push bc
	di

soundLoop
	xor a					;clear carry and set A to zero

	dec e					;counter of the first channel
	jr nz,_l1
	ld e,l					;reload if overflow
	sub l					;set carry if the note is not zero (mute)
_slide
	nop						;slide for drum
_l1

	dec d					;counter of the second channel
	jr nz,_l2
	ld d,h					;reload if overflow
	sub h					;set carry if the note is not zero (mute)
_l2

	sbc a,a					;if carry, A=255, otherwise A=0
	and 16
	out ($fe),a

	in a,($fe)				;read keyboard
	cpl
	and $1f
	jr nz,_l3				;if any key is pressed, exit loop

	dec bc
	ld a,b
	or c
	jr nz,soundLoop			;113/123t

_l3
	ld hl,$2758				;restore alternative HL to default value
	exx
	ei

	jr nz,_l4				;if any key was pressed, break loop
	djnz readRow
_l4
	pop bc
	pop de
	pop hl
	jr z,readPos			;if no key was pressed, continue

    ;; 	ret
    .(if (symbol-ref 'no-loop)
         " di\n halt\n"
         " ret")

musicData
