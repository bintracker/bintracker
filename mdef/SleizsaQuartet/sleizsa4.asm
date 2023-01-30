; Sleizsa Quartet v1.0 - Fairchild Channel F Music routine
;
; Copyright (c) 2019, utz/irrlicht project (irrlichtproject.de)
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.


    ;; processor f8
    ;; include "ves.h"		; standard ChannelF header
    ;; include "note_names.h"      ; note name equates

prog_size	.equ 62		; cartridge size in kb, valid sizes 4-7k, 16-62k


    ;; scratchpad register use
    ;; r0: output state
    ;; r1,r2: note length lo, hi
    ;; r3: $3f
    ;; r1-r3, K,H: used by drum
    ;; r4: noise enable
    ;; r5-r8, ch1-4 duty
    ;; r16-57: ISAR


    ;; org $800

cartridge_init			; init bla
    ;; CARTRIDGE_START
    ;; CARTRIDGE_INIT
    .db $55,$00                 ;cartridge start
    lis 0                       ;cartridge init
    outs 1
    outs 4
    outs 5
    outs 0
    lisu 4
    lisl 0
    lr s,a

;*******************************************************************************
main
    dci music_data
    xdc
    li $3f
    lr 3,a
    br _readseq

_readloop                       ; sequence-ptr <- loop-point
    .(when (defined? 'row-play)
       " clr\n outs 5\n_halt\n nop\n br _halt")
    lm
    lr qu,a
    lm
    lr ql,a
    lr dc,q
    xdc

_readseq                        ; read sequence
    xdc                         ; pattern-ptr <- (sequence-ptr)
    lis 0
    om                          ; when ptr hi-byte = 0, end of sequence reached
    bz _readloop
    lr qu,a
    lm
    lr ql,a
    xdc
    lr dc,q

_readptn
    lis 0                       ; ctrl byte, when 0, end of sequence reached
    om
    bz _readseq
    lr 9,a
    lr w,j

_readch1
    lisu 2
    bm _readch2
    lisl 0                      ; read ch1

    lm                          ; ch1-fdiv-lo
    lr i,a
    lr a,i                      ; ISAR++
    lm                          ; ch1-fdiv-hi
    lr i,a

_readch2
    bnc _readch3
    lisl 4

    lm
    lr i,a
    lr a,i
    lm
    lr i,a

_readch3
    lisu 3
    bnz _readch4
    lisl 0

    lm
    lr i,a
    lr a,i
    lm
    lr i,a

_readch4
    ;; bno _readlength             ; TODO shouldn't this be bno _readduties?
    bno _readduties
    lisl 4

    lm
    lr i,a
    lr a,i
    lm
    lr i,a
    lm
    lr 4,a                      ;       ch4_noise_enable ($ff = on, $00 = off)

_readduties
    lm                          ;       load duty control
    lr 9,a
    lr w,j
    lisu 4

    bm _readduty2
    lisl 0
    lm
    lr 5,a
    lm
    lr s,a

_readduty2
    bnc _readduty3
    lisl 1
    lm
    lr 6,a
    lm
    lr s,a

_readduty3
    bnz _readduty4
    lisl 2
    lm
    lr 7,a
    lm
    lr s,a

_readduty4
    bno _readlength
    lisl 3
    lm
    lr 8,a
    lm
    lr s,a

_readlength                     ;       read note length
    lis 0
    om
    bz _playdrum
    lr 2,a


_playNote                       ;       main sound loop
    lisl 0                      ; 1     point ISAR to r16
    lisu 2                      ; 1

    lr a,i                      ; 1     A <- ch1-fdiv lo, ISAR++
    as s                        ; 1     A += ch1-accu lo
    lr i,a                      ; 1     ch1-accu-lo <- A, ISAR++
    lr a,i                      ; 1     A <- ch1-fdiv hi, ISAR++
    lnk                         ; 1
    as s                        ; 1     A += ch1-accu-hi
    lr i,a                      ; 1     ch1-accu-hi <- A
    as 5                        ; 1     A += ch1-duty

    lr a,3                      ; 1     A = $3f
    lnk                         ; 1     when carry, A = $40
    lr 0,a                      ; 1     output_state <- A

    lr a,i                      ; 1     A <- ch2-fdiv lo, ISAR++
    as s                        ; 1     A += ch2-accu lo
    lr i,a                      ; 1     ch2-accu-lo <- A, ISAR++
    lr a,i                      ; 1     A <- ch2-fdiv hi, ISAR++
    lnk                         ; 1
    as s                        ; 1     A += ch2-accu-hi
    lr i,a                      ; 1     ch2-accu-hi <- A
    lisu 3                      ; 1     change scratchpad page
    as 6                        ; 1     A += ch2-duty

    lr a,0                      ; 1     when carry, output_state++
    lnk                         ; 1
    lr 0,a                      ; 1

    lr a,i                      ; 1     update ch3
    as s                        ; 1
    lr i,a                      ; 1
    lr a,i                      ; 1
    lnk                         ; 1
    as s                        ; 1
    lr i,a                      ; 1
    as 7                        ; 1

    lr a,0                      ; 1
    lnk                         ; 1
    lr 0,a                      ; 1

    lr a,i                      ; 1     update ch4
    as s                        ; 1
    lr i,a                      ; 1
    lr a,i                      ; 1
    lnk                         ; 1
    as s                        ; 1
    lr s,a                      ; 1

    ns 4                        ; 1     noise mode: when r4 = $ff
    as s                        ; 1                   rol w/ carry fdiv_ch4_msb
    lnk                         ; 1                 else do nothing
    lr s,a                      ; 1

    as 8                        ; 1     ch4 duty

    lr a,0                      ; 1
    lnk                         ; 1

    ;; should technically do ni $40 before writing to port, but bit 7 will never
    ;; be set and 6 bits don't seem to matter unless VRAM writes are enabled
    outs 5                      ; 4     update sound output
    ds 1                        ; 1.5   lsb(length_counter)--
    bnz _playNote               ; 3.5 - 60

    lisu 4                      ;       update duty settings
    lisl 0

    lr a,i                      ;       update duty_ch1
    as 5
    ns 3                        ;       duty_ch1 & $3f
    lr 5,a

    lr a,i                      ;       duty_ch2
    as 6
    ns 3
    lr 6,a

    lr a,i                      ;       duty_ch3
    as 7
    ns 3
    lr 7,a

    lr a,8                      ;       duty_ch4
    as i
    ns 3
    lr 8,a

    ds 2                        ;       msb(length_counter)--
    bnz _playNote

    jmp _readptn

_drumexit
    lr dc,q                     ;       restore data ptr
    lr a,Ku                     ;
    lr 3,a
    sr 1                        ;
    ai $80
    am                          ;       read note length
    lr 2,a                      ;       drum is twice as fast as tone osc, so
    lis 1                       ;       adjust note_length by -(drum_length / 2)
    ns 3
    li $3f                      ;       restore r3 to $3f
    lr 3,a
    bz _playNote
    li $8
    sl 4
    lr 1,a                      ;       adjust lsb row length
    br _playNote

_playdrum
    clr                         ;
    lr 3,a                      ;       set length counter lo-byte
    lm                          ;       read length counter hi-byte
    lr Kl,a                     ;
    lr Ku,a                     ;       backup length counter hi-byte
    lm                          ;       read drum ptr
    lr 10,a
    lm
    lr 11,a
    lr q,dc
    lr dc,h

    clr
    lr 0,a                      ;       drum state
_drumloop
    lr a,0                      ; 1
    xi $c0                      ; 2.5
    outs 5                      ; 4     toggle sound port
    lr 0,a                      ; 1
    clr                         ; 1
    om                          ; 2.5
    bz _drummute_entry          ; 2.5
    lr 1,a                      ; 1
_waitless
    lr a,3                      ; 1     could be faster: ds 3
    inc                         ; 1
    lr 3,a                      ; 1
    lr a,Ku                     ; 1
    lnk                         ; 1
    bz _drumexit                ; 2.5
    lr Ku,a                     ; 1
    ds 1                        ; 1.5
    bz _drumloop                ; 3.5 -- .drumloop-here: 29
    bnz _waitmore               ; 3.5 -- .waitmore: 29

_waitmore
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    br _next                    ; 3.5
_next
    br _waitless                ; 3.5

_drummute
    clr                         ; 1
    outs 5                      ; 4
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    nop                         ; 1
    br _drummute_entry          ; 3.5
_drummute_entry
    lr a,3                      ; 1
    inc                         ; 1
    lr 3,a                      ; 1
    lr a,4                      ; 1
    lnk                         ; 1
    bz _drumexit                ; 2.5
    lr 4,a                      ; 1
    ds 1                        ; 1.5
    br _drummute                ; 3.5 -- 29


    ;; if [prog_size > 7]
    .org $3000
    ;; endif

music_data
    ;; include "music.asm"		; song data


;; endprog
;;     org [$800 + [prog_size * $400] -$1]
;;     nop
