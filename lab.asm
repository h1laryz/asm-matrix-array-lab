STSEG SEGMENT PARA STACK "STACK"
    DB 64 DUP('?')
STSEG ENds

DSEG SEGMENT PARA PUBLIC "DATA"
    matr dw 51h, 51h DUP(0) 
    MatrSize dw 0
    SearchValue dw 0
    mas dw 0Bh, 0Bh DUP(0)
    InputMasSizeMsg DB 'Input array size from 1 to 9$'
    InputMatrSizeMsg DB 'Input square matrix size from 1 to 9$'
    InputValueMsg DB 'Input value -32768 to 32767 $'
    NothingInputMsg DB 'You have inputted nothing!$'
    InvalidDataMsg DB 'Incorrect input format! Please try again$'
    OverflowInputMsg DB 'Overflow in input$'
    OverflowResultMsg DB 'Result is out of range!$'
    SuccessMsg DB 'Your result is: $'
    BeforeSort DB 'Before sort: $'
    AfterSort DB 'After sort: $'
    OutputInt dw 0
    MinValue DB 'Min value: $'
    MaxValue DB 'Max value: $'
    InputNumWannaFind DB 'Input num to find: $'
    ExistMsg DB 'Matrix indexes that exist: $'
    SumValueMsg DB "Sum of array: $"
    WrongSizeMsg DB 'Error: Wrong size $'
    NoExistMsg DB 'There is no number in matrix $'
    boolExist db 0

    i dw 0
    j dw 0
    NumChar DB 7, ?, 7 DUP('?')    
    temp dw 0
    index db 0
    MasSize dw 0
    ElementToFindMatrix dw 0


    
DSEG ENDS

CSEG SEGMENT PARA PUBLIC "CODE"
    MAIN PROC FAR
        ASSUME cs:CSEG, ds:DSEG, ss:STSEG
        push ds
        xor ax, ax
        push ax
        mov ax, DSEG
        mov ds, ax

        call CLEARSCREEN
        call INPUT_ARRAY
        call NEW_LINE
        call FIND_SUM
        call FIND_MIN
        call FIND_MAX
        call SORT_ARRAY

        call INPUT_MATRIX
        call FIND_INDEXES_MATRIX
        ret
    MAIN ENDP

    FIND_SUM proc
        mov cx, MasSize
        xor ax, ax
        xor si, si
        
        START_FINDING:
        
            mov bx, mas[si]
            add ax, bx
            jo overflowinsum
            add si, 2
            loop START_FINDING

        FIND_SUM_RETURN:
        mov OutputInt, ax

        mov ah, 09h
        mov dx, offset SumValueMsg
        int 21h


        call OutInt
        call NEW_LINE
        jmp find_sum_return1

        overflowinsum:
        .exit

        find_sum_return1:
        ret
    FIND_SUM endp

    INPUT_MATRIX proc
        call NEW_LINE

        mov ah, 09h
        mov dx, offset InputMatrSizeMsg
        int 21h

        call NEW_LINE

        lea dx, NumChar
        mov ah, 0Ah
        int 21h

        call VALIDATION
        mov MatrSize, ax

        cmp ax, 1
        jl MATRIX_ERROR_SIZE
        cmp ax, 9
        jg MATRIX_ERROR_SIZE
        jmp continue_matrix_input

        MATRIX_ERROR_SIZE:
        call NEW_LINE
        mov ah, 09h
        mov dx, offset WrongSizeMsg 
        int 21h
        .exit

        continue_matrix_input:

        call NEW_LINE

        mov si, 0
        mov di, 0

        mov cx, MatrSize
        EXTERNAL:
            ; для вывода
            mov dx, MatrSize
            sub dx, cx
            inc dx
            mov i, dx
            ; ///

            push cx
            mov cx, MatrSize 
            xor bx, bx
            INTERNAL:
                mov dx, MatrSize
                sub dx, cx
                inc dx
                mov j, dx

                call OUTPUT_INDEX_MATRIX
            
                lea dx, NumChar
                mov ah, 0Ah
                int 21h

                push bx
                push cx
                push si
                push di
                call VALIDATION
                pop di
                pop si
                pop cx
                pop bx
                mov matr[bx][di], ax
                add di, 2
                call NEW_LINE
                loop INTERNAL
            pop cx
            cmp bx, 0
            jne addbx
            lea bx, matr[di]
            addbx:
                add bx, di
            loop EXTERNAL

        ret
    INPUT_MATRIX endp

    FIND_INDEXES_MATRIX proc
        call NEW_LINE
        
        mov ah, 09h
        lea dx, InputNumWannaFind
        int 21h

        call NEW_LINE

        lea dx, NumChar
        mov ah, 0Ah
        int 21h
        call NEW_LINE

        call VALIDATION
        mov SearchValue, ax

        mov ah, 09h
        lea dx, ExistMsg
        int 21h
        call NEW_LINE
        
        xor di, di
        xor si, si
        xor bx, bx

        mov cx, MatrSize 
        EXTERNALFIND:

            cmp cx, 0FFFFh
            je matrix_find_return

            ; для вывода
            mov dx, MatrSize
            sub dx, cx
            inc dx
            mov i, dx
            ; ///

            push cx
            mov cx, MatrSize 
            xor di, di
            INTERNALFIND:
                mov dx, MatrSize
                sub dx, cx
                inc dx
                mov j, dx
            
                mov ax, matr[bx][di]
                cmp ax, SearchValue
                jne dontOutput
                call OUTPUT_INDEX_MATRIX
                call NEW_LINE
                dontOutput:
                add di, 2
                loop INTERNALFIND
            pop cx
            cmp bx, 0
            jne addbxFIND
            lea bx, matr[di]
            loop EXTERNALFIND
            addbxFIND:
                add bx, di
            loop EXTERNALFIND
            
            cmp boolExist, 0
            je matrix_find_return

            mov ah, 09h
            lea dx, NoExistMsg
            int 21h

            matrix_find_return:
        ret
    FIND_INDEXES_MATRIX endp

    OUTPUT_INDEX_MATRIX proc
        mov boolExist, 1

        mov dx, '('
        mov ah, 02h
        int 21h

        mov dx, i
        add dx, '0'
        mov ah, 02h
        int 21h

        mov dx, ','
        mov ah, 02h
        int 21h

        mov dx, ' '
        mov ah, 02h
        int 21h

        mov dx, j
        add dx, '0'
        mov ah, 02h
        int 21h

        mov dx, ')'
        mov ah, 02h
        int 21h

        mov dx, ' '
        mov ah, 02h
        int 21h

        ret
    OUTPUT_INDEX_MATRIX endp

    FIND_MIN proc
        mov si, 0
        mov ax, mas[si]
        mov cx, MasSize
        findmin:
            cmp ax, mas[si]
            jg newmin
            jmp continueMin
            newmin:
                mov ax, mas[si]
        continueMin:
            add si, 2
            loop findmin
        mov OutputInt, ax

        lea dx, MinValue
        mov ah, 09h
        int 21h


        call OutInt
        call NEW_LINE
        ret
    FIND_MIN endp

    FIND_MAX proc
        mov si, 0
        mov ax, mas[si]
        mov cx, MasSize
        findmax:
            cmp ax, mas[si]
            jl newmax
            jmp continueMax
            newmax:
                mov ax, mas[si]
        continueMax:
            add si, 2
            loop findmax
        mov OutputInt, ax

        lea dx, MaxValue
        mov ah, 09h
        int 21h
        call OutInt
        call NEW_LINE
        ret
    FIND_MAX endp    

    SORT_ARRAY proc
        lea dx, BeforeSort
        mov ah, 09h
        int 21h
        call OUTMAS

        mov si, 0
        mov cx, MasSize
        
        cmp cx, 1
        je sort_return
        jmp sort_continue
        

        sort_continue:

        dec cx              

        nextscan: 
            mov bx,cx
            mov si,0 

        nextcomp:

            mov ax, mas[si]
            mov dx, mas[si+2]
            cmp ax, dx
            jng noswap 

            mov mas[si], dx
            mov mas[si+2], ax

        noswap: 
            add si, 2
            dec bx
            jnz nextcomp

            loop nextscan     

                
        sort_return:
        lea dx, AfterSort
        mov ah, 09h
        int 21h
        call OUTMAS
        ret
    SORT_ARRAY endp

    VALIDATION proc 
        xor ax, ax
        xor cx, cx
        xor dx, dx
        mov si, 10
        mov CL, [NumChar + 1]
        lea di, NumChar + 2
        cmp [NumChar + 2], '-'
        jne ProcessNum
        dec cx
        inc di
        ProcessNum:
            mov bl, [di]
            cmp bl, '0'
            jl WrongFormattedError
            cmp bl, '9'
            jg WrongFormattedError
            sub bl, '0'
            mul si
            jo ovError
            add ax, bx
            jo ovError
            inc di
            loop ProcessNum
            cmp [NumChar + 2], '-'
            je MakeNegative
            jne return
        MakeNegative:
            neg ax
            ret
        return:
            ret
        WrongFormattedError:
            lea dx, InvalidDataMsg
            mov ah, 9
            int 21h
            call NEW_LINE
        ovError:
            lea dx, OverflowInputMsg
            mov ah, 9
            int 21h
            call NEW_LINE
            .exit
    VALIDATION endp

    CLEARSCREEN proc
        mov al, 03h  
        mov ah, 00h
        int 10h
        xor ax, ax
        ret
    CLEARSCREEN endp

    INPUT_ARRAY proc
        mov ah, 09h
        mov dx, offset InputMasSizeMsg 
        int 21h

        call NEW_LINE

        lea dx, NumChar
        mov ah, 0Ah
        int 21h

        call VALIDATION
        mov MasSize, ax

        cmp ax, 1
        jl ARRAY_ERROR_SIZE
        cmp ax, 9
        jg ARRAY_ERROR_SIZE
        jmp continue_array_input

        ARRAY_ERROR_SIZE:
        call NEW_LINE
        mov ah, 09h
        mov dx, offset WrongSizeMsg 
        int 21h
        .exit

        continue_array_input:

        mov cx, ax
        
        call NEW_LINE

        mov i, 0
        inputMasElements:
            mov dx, MasSize  
            sub dx, cx 
            inc dx

            add dx, '0'
            mov ah, 02h
            int 21h

            mov dx, ')'
            mov ah, 02h
            int 21h

            mov dx, ' '
            mov ah, 02h
            int 21h

            lea dx, NumChar
            mov ah, 0Ah
            int 21h

            call NEW_LINE

            push cx
            call VALIDATION
            pop cx

            mov si, i
            mov mas[si], ax
            add si, 2
            
            mov i, si
            loop inputMasElements

        ret

    INPUT_ARRAY endp

    OUTMAS proc
        mov si, 0
        mov cx, MasSize
        
        OutMasLoop:
            mov ax, mas[si]
            mov OutputInt, ax
            add si, 2
            push cx
            call OutInt
            pop cx

            mov dx, ' '
            mov ah, 02h
            int 21h

            loop OutMasLoop
        ret
    OUTMAS endp

    OUTINT proc 
;; если число знаковое, то необходимо расскоментировать следующие строки
;; Проверяем число на знак.
       mov     ax, OutputInt
       test    ax, ax
       jns     oi1
;
;; Если оно отрицательное, выведем минус и оставим его модуль.
       mov  cx, ax
       mov     ah, 02h
       mov     dl, '-'
       int     21h
       mov  ax, cx
       neg     ax
;; Количество цифр будем держать в CX.
;; Количество цифр будем держать в CX.
oi1:  
    xor     cx, cx
    mov     bx, 10 ; основание сс. 10 для десятеричной и т.п.
oi2:
    xor     dx,dx
    div     bx
; Делим число на основание сс. В остатке получается последняя цифра.
; Сразу выводить её нельзя, поэтому сохраним её в стэке.
    push    dx
    inc     cx
; А с частным повторяем то же самое, отделяя от него очередную
; цифру справа, пока не останется ноль, что значит, что дальше
; слева только нули.
    test    ax, ax
    jnz     oi2
; Теперь приступим к выводу.
    mov     ah, 02h
oi3:
    pop     dx
; Извлекаем очередную цифру, переводим её в символ и выводим.

oi4:
    add     dl, '0'
    int     21h
; Повторим ровно столько раз, сколько цифр насчитали.
    loop    oi3

    ret
 
OUTINT endp

    SHOW_ERROR proc
        mov ah, 09h
        pop dx
        int 21h

        call NEW_LINE
        .exit
        ret
    SHOW_ERROR endp

    NEW_LINE PROC 

        mov dl, 10 
        mov ah, 02h 
        int 21h

        mov dl, 13 
        mov ah, 02h
        int 21h

        ret 
    NEW_LINE ENDP

CSEG ENDS
END MAIN