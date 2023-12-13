;;; solarplay
;;; 3 channel sound engine for the PTC Sol-20 Music System


    ;; org 0

init
    lxi h,0
    dad sp
    shld oldSP
    lxi sp,music_data
    mvi b,0                     ;    note length lo

read_seq
    pop h
    mov a,h
    ora l
    jnz read_step
    .(if (defined? 'row-play)
         " hlt\n"
         " pop h\n")
    ;; pop h                       ;    read loop point
    sphl
    pop h

read_step
    mov a,m
    sta len_hi
    inx h

    mov a,m
    sta note1
    inx h
    mov a,m
    sta .(+ (symbol-ref 'note1) 1)
    inx h
    mov a,m
    sta vol1
    inx h

    mov a,m
    sta note2
    inx h
    mov a,m
    sta .(+ (symbol-ref 'note2) 1)
    inx h
    mov a,m
    sta vol2
    inx h

    mov a,m
    sta note3
    inx h
    mov a,m
    sta .(+ (symbol-ref 'note3) 1)
    inx h
    mov a,m
    sta vol3
    inx h

play_note
note1 .equ .(+ 1 current-origin)
    lxi d,$400                  ; 10 calculate ch1 state
state1 .equ .(+ 1 current-origin)
    lxi h,0                     ; 10
    dad d                       ; 10
    shld state1                 ; 16
    sbb a                       ;  4
vol1 .equ .(+ 1 current-origin)
    ani 8                       ;  7
    add c                       ;  4 update total pulse length counter
    mov c,a                     ;  5

note2 .equ .(+ 1 current-origin)
    lxi d,$300                  ; 10 ch2
state2 .equ .(+ 1 current-origin)
    lxi h,0                     ; 10
    dad d                       ; 10
    shld state2                 ; 16
    sbb a                       ;  4
vol2 .equ .(+ 1 current-origin)
    ani 8                       ;  7
    add c                       ;  4
    mov c,a                     ;  5

note3 .equ .(+ 1 current-origin)
    lxi d,0                     ; 10 ch3
state3 .equ .(+ 1 current-origin)
    lxi h,0                     ; 10
    dad d                       ; 10
    shld state3                 ; 16
    sbb a                       ;  4
vol3 .equ .(+ 1 current-origin)
    ani 0                       ;  7
    add c                       ;  4

    jz out_lo                   ; 10 if total pulse length > 0
    ei                          ;  4 set audio out to hi
    mov c,a                     ;  5
    dcr c                       ;  5 decay pulse length
lret
    dcr b                       ;  5 note length lo
    jnz play_note                    ; 10 .. 232

len_hi .equ .(+ 1 current-origin)
    mvi a,0                     ;    note length hi
    dcr a
    jz read_seq
    sta len_hi
    jmp play_note

out_lo
    di                          ;  4 set audio out to lo
    jmp lret                    ; 10

exit
oldSP .equ .(+ 1 current-origin)
    lxi sp,0
    ret

music_data
