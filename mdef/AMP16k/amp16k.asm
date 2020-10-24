; AMP16K
; by Hikaru/Intense in 2020
; SjASMPlus syntax
; Public Domain


;In:
;HL = song address
;
;Note: Define AMP16K_LOW_ISR to place the ISR at $5C65 (I=$2B).
;      The default location is $7E5C (I=$28).
;
AMP16K
_CST_BorderLp .equ 63
_CST_ScreenLp .equ 192
_CST_IntVec .equ $28
_CST_ISRDest .equ .(if (symbol-ref 'AMP16K_LOW_ISR) #x5c65 #x7e5c)
_CST_IntVec .equ .(if (symbol-ref 'AMP16K_LOW_ISR) #x2b #x28)

        ld hl,musicdata
	push iy
	push hl
	ld hl,_CST_ISRDest	;set up IM2, test whether we are in contended memory, adjust timing parameters
	ld bc,.(+ #xC900 (symbol-ref '_CST_IntVec))
	ld e,(hl)
	ld (hl),b
	ld a,c
	ld i,a
	im 2
	ei
	halt
	in a,($1F)
	inc a
	jr nz,.(+ 5 current-origin)
	ld (_LOC_KmpSwitch),a
	ei
_mem_test
	inc bc
	ld a,i
	jp pe,_mem_test
	ld a,b
	cp $D3
	jr c,_is_contended
	ld a,.(- (symbol-ref '_b_lp) (symbol-ref '_LOC_JmpLoop) 1)
	ld (_LOC_ScrLpMode),a
	ld a,6
	ld (_LOC_DrumDelay),a
_is_contended
	ld (hl),e
	ei
	call _swap_isr
	ld de,$0780		;delay till the bottom of the screen before parsing the song
	ld a,e
	or d
	dec de
	jr nz,.(- current-origin 3)
	ld c,a
	sbc hl,hl
	push hl
	pop ix
	push hl
	pop iy
	ld (_LOC_Ch1Pw),a
	ld (_LOC_Ch1Freq),de
	ld (_LOC_Ch2Freq),hl
	ld (_LOC_Ch3Freq),hl
	ld (_LOC_Ch4Acc),hl
	ld (_LOC_Ch4Freq),hl
	ld (_LOC_ChMix),a
	exx
	ex (sp),hl
	ld (_LOC_SaveSP),sp
	ld sp,hl
	pop hl			;+0: PatternData-PatternList
	add hl,sp
	ld (_LOC_PatternData),hl
	ld b,a			;B = 0
	jr _first_pattern	;SP = PatternList
_next_pattern
_LOC_PatternListPtr .equ .(+ 1 current-origin)
	ld sp,0
	pop hl			;PTxx-PatternData
	bit 7,h
	jr z,_no_loop
        .(if (symbol-ref 'no-loop)
             " di\n halt"
             " add hl,sp\n ld sp,hl")
	;; add hl,sp		;BIT 7 = 1: PatternListLoop-PatternData
	;; ld sp,hl
_first_pattern
	pop hl
_no_loop
	ld (_LOC_PatternListPtr),sp
_LOC_PatternData .equ .(+ 1 current-origin)
	ld de,0
	add hl,de
	ex de,hl
	ld a,(de)		;pattern +0: row length, frames
	inc de
	ld (_LOC_RowLen),a
	xor a

_read_loop
	exa
	ld a,(de)		;ptn row +0: %1234 dddd
	and $0F
	jr z,_no_drum
	dec a			;$-1: next pattern
	jr z,_next_pattern
_drum				;$-2~$-F: drum
        .(if (symbol-ref 'AMP_NO_DRUMS)
             " jr _no_drum"
	     " di")
	ld c,a
	ld hl,.(- (symbol-ref '_drum_table) 1)
	add hl,bc
	ld c,(hl)
	add hl,bc
	ld c,(hl)		;drum length, frames
	inc hl
	dec b			;B=$FF - first loop is shorter
	exa
	sub c			;subtract the drum length from the row length counter
	exa
	ld a,(hl)		;rng seed
	exx
	ld e,a
	ld d,a
	exx
_drum_read_loop
	inc hl
	ld a,(hl)		;noise frequency
	exx
	ld c,a
	ld b,a
	exx
	.db $FE
_drm_lp2
	ld b,$78
_drm_lp
	exx
	ld a,e
	add a,d
	dec b
	jr z,_noise		;12/7
	ret z			;5
	ld a,d			;4 + 7
	jr _no_noise		;12 28
_noise
	ld b,c			;4 +12
	ld e,a			;4
	rlca			;4
	xor d			;4 28
_no_noise
	ld d,a
	and $10
	exx
	out ($FE),a
_LOC_DrumDelay .equ .(+ 1 current-origin)
	ld a,4
	dec a
	jr nz,.(- current-origin 1)
	djnz _drm_lp
	sub c			;loops C x 2 times
	ld c,a
	jp m,_drm_lp2
	dec c
	jr nz,_drum_read_loop
	exx
_LOC_Ch1Freq .equ .(+ 1 current-origin)
	ld de,$FFFF
_LOC_Ch2Freq .equ .(+ 1 current-origin)
	ld bc,0
	exx
        .(unless (symbol-ref 'AMP_NO_DRUMS) " ei")
_no_drum
	ld a,(de)		;ptn row +1..+N: %p nnnnnnn, - Pulse width byte follows, Note
	inc de
	add a,a
	ld c,a
	jr nc,_no_ch1_note
	ld a,(de)
	inc de
	add a,a
	ld sp,_note_table
	jr z,_ch1_rest		;ch1 rest note noise fix: set pulse width 0. Assume rest+PW doesn't occur (no meaning).
	ld l,a
	ld h,b
_LOC_Ch1Pw2 .equ .(+ 1 current-origin)
	ld a,0
	jr nc,_no_ch1_pw
	ld a,(de)
	inc de
	ld (_LOC_Ch1Pw2),a
_no_ch1_pw
	add hl,sp
	ld sp,hl
_ch1_rest
        .(unless (symbol-ref 'AMP_NO_CH1)
           " ld (_LOC_Ch1Pw),a")
	exx
	pop de
	ld a,e
	cpl
	ld e,a
	ld a,d
	cpl
	ld d,a
	ld (_LOC_Ch1Freq),de
	exx
_no_ch1_note

	sla c
	jr nc,_no_ch2_note
	ld a,(de)
	inc de
	add a,a
	ld l,a
	ld h,b
	jr nc,_no_ch2_pw
	ld a,(de)
	inc de
        .(unless (symbol-ref 'AMP_NO_CH2)
           " ld (_LOC_Ch2Pw),a")
_no_ch2_pw
	ld sp,_note_table
	add hl,sp
	ld sp,hl
	exx
	pop bc
	ld (_LOC_Ch2Freq),bc
	exx
_no_ch2_note

	sla c
_LOC_Ch3Freq .equ .(+ 1 current-origin)
	ld sp,0
	jr nc,_no_ch3_note
	ld a,(de)
	inc de
	add a,a
	ld l,a
	ld h,b
	jr nc,_no_ch3_pw
	ld a,(de)
	inc de
        .(unless (symbol-ref 'AMP_NO_CH3)
           " ld (_LOC_Ch3Pw),a")
_no_ch3_pw
	ld sp,_note_table
	add hl,sp
	ld sp,hl
	pop hl
	ld (_LOC_Ch3Freq),hl
	ld sp,hl
_no_ch3_note

	sla c
_LOC_Ch4Freq .equ .(+ 1 current-origin)
	ld hl,0
	jr nc,_no_ch4_note
	ld a,(de)
	inc de
	add a,a
	ld c,a
	jr nc,_no_ch4_pw
	ld a,(de)
	inc de
        .(unless (symbol-ref 'AMP_NO_CH4)
          " ld (_LOC_Ch4Pw),a")
_no_ch4_pw
	ld hl,_note_table
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld (_LOC_Ch4Freq),hl
_no_ch4_note

	exa
_LOC_RowLen .equ .(+ 1 current-origin)
	add a,0
	cp 1
	jp m,_read_loop
	exa
	ex de,hl
	ld (_LOC_PatternDataPtr),hl

_LOC_Ch4Acc .equ .(+ 1 current-origin)
	ld hl,0
_LOC_ChMix .equ .(+ 1 current-origin)
	ld c,0
	ld a,.(- (symbol-ref '_b_lp) (symbol-ref '_LOC_JmpLoop) 1)
	ld (_LOC_JmpLoop),a
	jr _s_lp

_b_lp
	ld a,3
	dec a
	jp nz,.(- current-origin 1)
	ld a,(de)
_s_lp
	add hl,de
	sbc a,a
_LOC_Ch4Pw .equ .(+ 1 current-origin)
	and 0
	or c
	ld c,a
	add ix,sp
	sbc a,a
_LOC_Ch3Pw .equ .(+ 1 current-origin)
	and 0
	exx
	add hl,de
	jr nc,.(+ 4 current-origin)		;JR NC (sic)
	jr .(+ 4 current-origin)
_LOC_Ch1Pw .equ .(+ 1 current-origin)
	or 0
	add iy,bc
	jr c,.(+ 4 current-origin)
	jr .(+ 4 current-origin)
_LOC_Ch2Pw .equ .(+ 1 current-origin)
	or 0
	exx
	or c
	rra
	ld c,a
	sbc a,a
	out ($FE),a
_LOC_JmpLoop .equ .(+ 1 current-origin)
	djnz _s_lp	;-5

;End of the sound loop:
;1. Top of the screen. POKE the DJNZ to use the screen time loop (delay off), and continue with B=192.
;2. Bottom of the screen. If current note has ended, leave and continue parsing the pattern.
;   If current note has not yet ended, POKE the DJNZ to use the border time loop (delay on).
;   B remains at 0, is set to the correct value in the ISR.
;
	exa
	jr nc,_s_lp_end
_b_lp_end
	or a			;+4, reset CF
	ret c			;+5
	exa
_LOC_ScrLpMode .equ .(+ 1 current-origin)
	ld a,.(- (symbol-ref '_s_lp) (symbol-ref '_LOC_JmpLoop) 1)
	ld (_LOC_JmpLoop),a
	ld b,_CST_ScreenLp
	jp _s_lp
_s_lp_end
	ei			;+4, EI
	jr z,_note_end		;ZF' = 1: Current note has ended
	exa
	ld a,.(- (symbol-ref '_b_lp) (symbol-ref '_LOC_JmpLoop) 1)
	ld (_LOC_JmpLoop),a
	jp _s_lp

_note_end
	ld (_LOC_Ch4Acc),hl
	ld hl,_LOC_ChMix
	ld (hl),c
_LOC_PatternDataPtr .equ .(+ 1 current-origin)
	ld de,0
	in a,($FE)
	cpl
	ld c,$1F
	in l,(c)
	and c
_LOC_KmpSwitch
	or l
	jp z,_read_loop		;A = 0
	im 1
_LOC_SaveSP .equ .(+ 1 current-origin)
	ld sp,0
	pop hl
	pop iy
	exx

_swap_isr
	ld hl,_isr_src
	ld de,_CST_ISRDest
	ld b,.(- (symbol-ref '_isr_end) (symbol-ref '_isr_src))
_swap_lp
	ld c,(hl)
	ld a,(de)
	ld (hl),a
	ld a,c
	ld (de),a
	inc hl
	inc de
	djnz _swap_lp
	ret

_isr_src
	;; DISP _CST_ISRDest
	pop af		;restore SP (ch3 freq)
	exa
	bit 7,d		;reset the EXX state
	jr z,.(+ 3 current-origin)
	exx
	add a,$FF		;DEC A: SCF
	exa
	ld b,_CST_BorderLp
	jp _s_lp	;+19 = 81/80
	;; ENT
_isr_end

_note_table
	.dw 0
	.dw $0022,$0024,$0027,$0029,$002B,$002E,$0031,$0033,$0037,$003A,$003D,$0041
	.dw $0045,$0049,$004D,$0052,$0057,$005C,$0061,$0067,$006D,$0074,$007A,$0082
	.dw $0089,$0092,$009A,$00A3,$00AD,$00B7,$00C2,$00CE,$00DA,$00E7,$00F5,$0103
	.dw $0113,$0123,$0134,$0147,$015A,$016F,$0185,$019C,$01B4,$01CE,$01EA,$0207
	.dw $0226,$0246,$0269,$028E,$02B4,$02DE,$0309,$0337,$0368,$039C,$03D3,$040D
	.dw $044B,$048C,$04D2,$051B,$0569,$05BB,$0612,$066F,$06D1,$0738,$07A6,$081B
	.dw $0896,$0919,$09A3,$0A36,$0AD2,$0B76,$0C25,$0CDE,$0DA1,$0E71,$0F4D,$1036
	.dw $112C,$1232,$1347,$146C,$15A3,$16EC,$1849,$19BB,$1B43,$1CE2,$1E99,$206B

_drum_table
	;; .db .(- (symbol-ref '_DR00) current-origin)
	;; .db .(- (symbol-ref '_DR01) current-origin)
	;; .db .(- (symbol-ref '_DR02) current-origin)
    	;; .db .(- (symbol-ref '_DR03) current-origin)
    	;; .db .(- (symbol-ref '_DR04) current-origin)
    	;; .db .(- (symbol-ref '_DR05) current-origin)
    	;; .db .(- (symbol-ref '_DR06) current-origin)
    	;; .db .(- (symbol-ref '_DR07) current-origin)
        .db 8
        .db 11
        .db 14
        .db 16
        .db 19
        .db 23
        .db 27
        .db 31
_DR00
	.db 2				;BD
	.db $DC
	.db 4,34
_DR01
	.db 2				;SN
	.db $DC
	.db 3,9
_DR02
	.db 1				;HHc
	.db $42
	.db 1
_DR03
	.db 2				;HHo
	.db $3F
	.db 1,1
_DR04
	.db 3				;TM1
	.db $DC
	.db 3, 15, 20
_DR05
	.db 3				;TM2
	.db $DC
	.db 3, 20, 23
_DR06
	.db 3				;CH
	.db $DC
	.db 2, 2, 1
_DR07
	.db 3				;DD
	.db $DC
	.db 25, 4, 36
musicdata
