;;; Simple test player for Exidy Sorcerer 4-voice music extension
;;; by utz 06'2020

init
    di

    ld (oldsp),sp

readseq0
    ld hl,0
    ld (ch1div),hl
    ld (ch2div),hl
    ld (ch3div),hl
    ld (ch4div),hl
    ld a,.(quotient (symbol-ref 'wavetab) 256)
    ld (wave1),a
    ld (wave2),a
    ld (wave3),a
    ld (wave4),a
    ld ix,0
    ld iy,0
    ld de,0
    ld hl,musicdata
    ld (seqptr),hl

readseq
seqptr .equ .(+ 1 current-origin)
    ld sp,0
    pop hl
    ld a,h
    or l
    jr nz,_skip
    .(if (defined? 'row-play)
         " halt"
         " jr readseq0")

_skip
    ld (seqptr),sp
    ld sp,hl
    ld hl,0

readptn
    pop af                      ; len + ctrl1
    jr z,readseq
    exx
    ld b,a
    ld c,0
    exx
    jr c,_ch2
    pop bc
    ld (ch1div),bc
    pop bc
    ld a,c
    ld (wave1),a
    ld ix,0
_ch2
    jp m,_ch3
    pop bc
    ld (ch2div),bc
    pop bc
    ld a,c
    ld (wave2),a
    ld iy,0
_ch3
    jp pe,_ch4
    pop bc
    ld (ch3div),bc
    pop bc
    ld a,c
    ld (wave3),a
    ld hl,0
_ch4
    pop af
    jr c,playnote
    ld (wave4),a
    pop bc
    ld (ch4div),bc
    ld de,0


playnote
ch1div .equ .(+ 1 current-origin)
    ld bc,0                     ;10
    add ix,bc                   ;15
ch2div .equ .(+ 1 current-origin)
    ld bc,0                     ;10
    add iy,bc                   ;15
ch3div .equ .(+ 1 current-origin)
    ld bc,0                     ;10
    add hl,bc                   ;11
ch4div .equ .(+ 1 current-origin)
    ld bc,0                     ;10
    ex de,hl                    ; 4
    add hl,bc                   ;11
    ex de,hl                    ; 4

    ld a,d                      ; 4
    exx                         ; 4
wave4 .equ .(+ 1 current-origin)
    ld h,0                      ; 7
    ld l,a                      ; 4
    ld d,(hl)                   ; 7
    exx                         ; 4
    ld a,h                      ; 4
    exx                         ; 4
wave3 .equ .(+ 1 current-origin)
    ld h,0                      ; 7
    ld l,a                      ; 4
    ld a,(hl)                   ; 7
    add a,d                     ; 4
    ld d,a                      ; 4
    ld a,iyh                    ; 8
wave2 .equ .(+ 1 current-origin)
    ld h,0                      ; 7
    ld l,a                      ; 4
    ld a,(hl)                   ; 7
    add a,d                     ; 4
    ld d,a                      ; 4
    ld a,ixh                    ; 8
wave1 .equ .(+ 1 current-origin)
    ld h,0                      ; 7
    ld l,a                      ; 4
    ld a,(hl)                   ; 7
    add a,d                     ; 4
    out ($ff),a                 ;11

    dec bc                      ; 6
    ld a,b                      ; 4
    or c                        ; 4
    exx                         ; 4
    jp nz,playnote              ;10... 263
    jp readptn

exit
oldsp .equ .(+ 1 current-origin)
    ld sp,0
    ei
    ret

    .align $100
wavetab
;; triangle
;;     .(string-append " .db "
;;                     (string-intersperse
;;                      (map (o number->string inexact->exact round)
;;                           (iota 128 0 0.5))
;;                      ", ")
;;                     "\n .db "
;;                     (string-intersperse
;;                      (map (o number->string inexact->exact round)
;;                           (iota 128 63 -0.5))
;;                      ", "))
;; rect
;;     .ds 128,0
;;     .ds 128,56

;; saw
;;     .(string-append " .db "
;;                     (string-intersperse
;;                      (map (o number->string inexact->exact round)
;;                           (iota 256 0 0.25))
;;                      ", "))

;; noise
;;     .db 0
;;     .(string-append " .db "
;;                     (string-intersperse
;;                      (map (lambda (x)
;;                            (number->string (pseudo-random-integer 57)))
;;                           (make-list 255 0))
;;                      ", "))

;; musicdata
    ;; len*256+ctrl1, freq1, wave1, freq2, wave2, freq3, wave3, wave4*256+ctrl2, freq4
