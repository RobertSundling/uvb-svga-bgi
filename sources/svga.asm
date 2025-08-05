; [Comments translated from German to English]

; ******************************************************************************
; *                                                                            *
; * BGI driver for Super-VGAs in 256-color mode.                               *
; *                                                                            *
; * Ullrich von Bassewitz on 29.04.1991                                        *
; *                                                                            *
; ******************************************************************************


IDEAL                   ; Ideal mode
JUMPS                   ; Automatic jump adjustment
LOCALS                  ; Allow local symbols
SMART


;---------------------------------------------------
;
; Include used macros
;

INCLUDE "macros.asi"


IFNDEF Ver3
Ver3  = 0
ENDIF


; ==================================================
; The following symbols can be used to control code generation.
; The symbols are usually set via the Makefile or the TASM call.
;
; P80286          = 1
; P80386          = 1

IFNDEF P80386
P80386  = 0
IFNDEF P80286
P80286  = 0
ENDIF
ENDIF


; For 80386 also use 80286 instructions
IF P80386
IFNDEF P80286
P80286  = 1
ENDIF
ENDIF

; Set processor
RESETCPU

; ----------------------------------------
; Include used constants

INCLUDE "const.asi"



; ==================================================

SEGMENT CODE PARA PUBLIC 'CODE'

IF Ver3
ASSUME  CS:CODE, DS:DATA
ELSE
ASSUME  CS:CODE, DS:CODE
ENDIF

Start:

;---------------------------------------------------
; Include definition of used structs
;

INCLUDE "structs.asi"


;---------------------------------------------------
; The dispatcher is located at address 0
; It is FAR coded, its RET also serves as a NOP vector

PROC    Dispatch Far

        push    ds
        SetToDS ds
        cld
        push    bp
        call    [Vector_Table + si]

        pop     bp
        pop     ds
FAR_NOP_Vector:
        ret

ENDP    Dispatch

;---------------------------------------------------
; Fill bytes up to 10h

IF Ver3
        db      0
ELSE
        db      "Anna"
ENDIF

;---------------------------------------------------
; Data segment

IF      Ver3
DSeg    dw      ?
ENDIF

;---------------------------------------------------
; The Emulate vector
; The final ret is also used as a NOP vector

PROC    Emulate NEAR

        ret                             ; These two lines are overwritten with
        db      00h, 00h, 00h, 00h      ; call FAR GRAPH:XXXX
NOP_Vector:
        ret                             ; Then it returns

ENDP    Emulate

; ----------------------------------------------------------------
; General subroutines


PROC    Int10 Near

        push    bp
        ;cld
        int     10h
        pop     bp
        ret

ENDP    Int10

;---------------------------------------------------------------------
; Include subroutines

INCLUDE "dpmi.asi"                      ; DPMI stuff

INCLUDE "svgaopts.asi"                  ; Environment options

INCLUDE "detect.asi"                    ; Autodetect

INCLUDE "cards.asi"                     ; Card-specific routines

INCLUDE "s3.asi"                        ; S3 specific routines


; =========================================================================
;
; Install routine
;


PROC    Install NEAR

        cmp     al,01h                          ; Mode query?
        jnz     @@L1                            ; No
        mov     cx, ModeCount                   ; Yes: Number of modes supported
        ret

@@L1:   push    ds
        pop     es
        cmp     al, 02h                         ; Mode name?
        jnz     InstallDevice                   ; No
        jcxz    @@L3                            ; Mode 0
        cmp     cx, MaxAutoMode
        ja      @@L2
        mov     bx, cx                          ; Auto mode number in bx
        shl     bx, 1                           ; * 2 for word access
        mov     bx, [WORD AutoName-2+bx]        ; Get pointer to name
        ret                                     ; done

; It is not an autodetect mode

@@L2:   sub     cx, MaxAutoMode
@@L3:   fastimul bx, cx, SIZE TMode
		;mov     ax, SIZE TMode                  ; Length of a table entry
        ;mul     cx                              ; * entry number
        ;mov     bx, ax
        mov     bx, [(TMode ModeTable + bx).ModeName]   ; Pointer to name
        ret

; Install

InstallDevice:
        push    cx                              ; Save mode
        call    GetOpts                         ; Get environment options
        pop     cx
        xor     ch, ch
        cmp     cl, ModeCount                   ; Mode allowed?
        jae     IllegalMode                     ; NO!
        jcxz    @@L5                            ; Mode 0
        cmp     cx, MaxAutoMode                 ; Is it an autodetect mode?
        ja      @@L4                            ; No

; It is an autodetect mode. Get the mode mask from the table and
; call the autodetect routine. Return is an entry of the mode table
; in di

        mov     di, cx                          ; Mode number in di
        mov     al, [BYTE AutoMode-1+di]        ; Get mode bitset
        push    es                              ; es is destroyed
        call    AutoDetectMode                  ; Search for mode
        pop     es
        jmp     short @@L6

; It is not an autodetect mode to be set

@@L4:   sub     cx, MaxAutoMode
@@L5:   mov     ax, SIZE TMode                  ; Size of a table entry
        mul     cx
        mov     di, ax                          ; Offset into di
        lea     di, [ModeTable + di]            ; Address into di
@@L6:   mov     [ModePtr], di                   ; Save address

; From the data of the mode entry in di, calculate and set the data for DST

        mov     ax, [(TMode di).XRes]           ; Set X resolution
        dec     ax
        mov     [DST.XRes], ax
        mov     [DST.XEfRes], ax
        mov     ax, [(TMode di).YRes]           ; Set Y resolution
        dec     ax
        mov     [DST.YRes], ax
        mov     [DST.YEfRes], ax

; Aspect ratio according to the formula
;
;       Y * 10000     XInch
;      ----------- *  -----
;           X         YInch
;
; calculate

        mov     ax, 10000
        mul     [(TMode di).YRes]
        div     [(TMode di).XRes]
        mul     [DST.XInch]
        div     [DST.YInch]
        mov     [DST.Aspec], ax

; Return pointer to DST in es:bx, es is already correct

        mov     bx, OFFSET DST                  ; Mark as current DST
        ret

IllegalMode:
        mov     di, OFFSET ModeTable            ; Return VGA256
        mov     [DST.Stat], grInvalidMode       ; Invalid mode
        jmp     @@L6                            ; and install

ENDP    Install

; =======================================================================
;
;      DW   offset   INIT         ; Initialize device for output
;
;   Input:
;      es:bx  --> Device Information Table
;
;   Return:
;      Nothing
;
; This vector is used to change an already INSTALLed device from text mode to
; graphics mode. This vector should also initialize any default palettes and
; drawing mode information as required. The input to this vector is a
; device information table (DIT). The format of the DIT is shown below and
; contains the background color and an initialization flag. If the device
; requires additional information at INIT time, these values can be appended
; to the DIT. There in no return value for this function. If an
; error occurs during device initialization, the STAT field of the Device
; Status Table should be loaded with the appropriate error value.
;
; ; ************** Device Information Table Definition **************
;
; struct  DIT
;         DB      0               ; Background color for initializing screen
;         DB      0               ; Init flag; 0A5h = don't init; anything
;                                 ;   else = init
;         DB      64 dup 0        ; Reserved for Borland's future use
;                                 ; additional user information here
; DIT     ends
;
;

PROC    Init NEAR

        mov     al, [es:bx]             ; Background color
        and     al, 0fh                 ; BIOS only uses 0-15
        mov     [BkColor], al           ; Save

; If there is an error entered in the status field of the DST, then end
; directly (GRAPH apparently does not take error checking too seriously,
; so a call with an error set is quite possible. In this case, mode 0 is
; enabled, but it still looks a bit silly if you switch to an unwanted
; graphics mode).

        cmp     [DST.Stat], grOk
        jne     InitEnd

; Cheat the graphics kernel: As already explained in the comment on the
; vector table, the graphics kernel from version 7.0 checks the FillPoly
; vector. If this does not point to Emulate, the kernel sets a flag that
; causes Arc and PieSlice with small radii to no longer work correctly.
; So the table contains a pointer to Emulate, which is corrected here.

        mov     [Vector_Table + FuncFillPoly], OFFSET FillPoly

; Reset X and Y resolution and the clip window

        xor     ax, ax
        mov     [Clip_X1], ax
        mov     [Clip_Y1], ax

        mov     ax, [DST.XEfRes]                ; Effective X resolution
        mov     [Clip_X2], ax
        inc     ax
        mov     [MaxX], ax
        mov     [BytesPerLine], ax
        mov     ax, [DST.YEfRes]                ; Effective Y resolution
        mov     [Clip_Y2], ax
        inc     ax
        mov     [MaxY], ax

; Set the segment switching routine suitable for the mode

        mov     di, [ModePtr]
        mov     bl, [(TMode di).CardType]
        xor     bh, bh
        shl     bx, 1
        mov     ax, [SegSwitchTable+bx]
        mov     [SegSelect], ax

; Reset the pointer to the VESA switching routine

IF P80386
        xor     eax, eax
        mov     [VESA_WinFunc], eax
ELSE
        xor     ax, ax
        mov     [WORD LOW VESA_WinFunc], ax
        mov     [WORD HIGH VESA_WinFunc], ax
ENDIF

; Set mode. Here, if option M is set, the VESA function numbers must be used

        test    [Options], OpUseVesaModes
        jnz     @@L2

; Normal switching

@@L1:   mov     ax, [(TMode di).BIOSax]
        mov     bx, [(TMode di).BIOSbx]
        jmp     @@L4

; Switching with VESA mode numbers

@@L2:   cmp     [(TMode di).Capabilities], M320x200
        mov     ax, 0013h
        jz      @@L4
        mov     al, [(TMode di).Capabilities]
        mov     bx, 100h
        cmp     al, M640x400
        jz      @@L3
        mov     bl, 01h
        cmp     al, M640x480
        jz      @@L3
        mov     bl, 03h
        cmp     al, M800x600
        jz      @@L3
        mov     bl, 05h
        cmp     al, M1024x768
        jz      @@L3
        mov     bl, 07h
        cmp     al, M1280x1024
        jnz     @@L1
@@L3:   mov     ax, 4F02h
@@L4:   push    di                              ; Save pointer
        call    Int10
        pop     di                              ; Pointer to mode entry

; Now call any special routines if present

        call    [(TMode di).GraphOn]

; Calculate and set screen paragraphs. This must only happen now, because
; the card-specific routine may change BytesPerLine.

        mov     ax, [MaxY]
        mul     [BytesPerLine]
        mov     [Word Low ScreenBytes], ax
        mov     [Word High ScreenBytes], dx

; Set background color as palette entry 0

		IF P80386        
			movzx     bx, [BkColor]
		ELSE
	        mov     bl, [BkColor]
	        xor     bh, bh
		ENDIF
        mov     ax, 0C000h              ; Code for: Set background
        call    Palette                 ; "Palette"

; that's it

InitEnd:
        ret

ENDP    Init

; ======================================================================
;
; CLEAR: Clears the graphics screen
;

PROC    Clear   NEAR

        call    [GE_Ready]              ; Wait for the GE
        mov     es, [VideoSeg]
        mov     ax, [PageOfs]
        mul     [MaxX]                  ; Calculate start address of screen
        mov     di, ax                  ; Offset in di
        mov     [Seg64], dl             ; Set segment
        call    [SegSelect]             ; Set segment
        cld

IF      P80386
        xor     eax, eax
ELSE
        xor     ax, ax
ENDIF
        mov     cx, [WORD LOW ScreenBytes]
        mov     dx, [WORD HIGH ScreenBytes]
        jcxz    @@L5
        mov     bx, di
        add     bx, cx                  ; Segment overflow?
        jnc     @@L4                    ; Jump if no
        jmp     @@L1                    ; Jump if segment overflow

IF      P80386

@@L0:   mov     bx, di                  ; Rest after segment overflow
@@L1:   mov     cx, di
        neg     cx                      ; Rest until segment overflow
        jnz     @@L2
        mov     ecx, 4000h
        rep     stosd
        jmp     @@L3

@@L2:   movzx   ecx, cx
        shr     cx, 2
        rep     stosd
        adc     cx, cx
        rep     stosw
@@L3:   inc     [Seg64]
        call    [SegSelect]
        movzx   ecx, bx
@@L4:   shr     cx, 2
        rep     stosd
        adc     cx, cx
        rep     stosw
@@L5:   dec     dx
        jns     @@L0

ELSE

@@L0:   mov     bx, di                  ; Rest after segment overflow
@@L1:   mov     cx, di
        neg     cx                      ; Rest until segment overflow
        jnz     @@L2
        mov     cx, 8000h
        jmp     @@L3

@@L2:   shr     cx, 1                   ; Count is always even
@@L3:   rep     stosw
        inc     [Seg64]
        call    [SegSelect]
        mov     cx, bx
@@L4:   shr     cx, 1                   ; Count is always even
        rep     stosw
@@L5:   dec     dx
        jns     @@L0

ENDIF

        ret

ENDP    Clear

; =========================================================================
;
; Post routine
;

PROC    Post    NEAR

; If the driver is running in protected mode and the VESA segment switching
; was also called in protected mode, then a descriptor was allocated for it.
; Free this.

        mov     bx, [WORD HIGH VESA_WinFunc]
        test    bx, bx                          ; Allocated?
        jz      @@L1                            ; Jump if no

        call    DPMI_FreeDesc                   ; Free descriptor

; Reset the Emulate vector: As already explained in the comment on the
; vector table, the graphics kernel from version 7.0 checks the FillPoly
; vector.
; If this does not point to Emulate, the kernel sets a flag that causes
; Arc and PieSlice with small radii to no longer work correctly. The vector
; in the table is therefore only set correctly at runtime (during Init).
; To ensure this also works correctly for drivers linked as OBJ files,
; it must be reset here.

@@L1:   mov     [Vector_Table + FuncFillPoly], OFFSET Emulate

; And done...

        ret

ENDP    Post



; =======================================================================
; Set color, al = character color, ah = fill color
;

PROC    Color Near

        mov     [DrawingColor], al
        mov     [FillColor], ah
        ret

ENDP    Color

; ======================================================================
; Move pixel-cursor
;

PROC    Move    Near

        mov     [CursorX], ax
        mov     [CursorY], bx
        ret

ENDP    Move

; ======================================================================
;
; Include routines Vect and Draw
;

INCLUDE "line.asi"


; ======================================================================
;
;       FillPoly
;
; Called with:
;       es:bx   Pointer to point list, where sub-polygons are terminated
;               with point pairs $8001/$8001 and the entire polygon again
;               with $8000/$8000
;               es:bx is also the pointer to the memory area reserved by
;               GRAPH (GraphBuf or similar), which is also used for
;               FloodFill. At the end of the list there is therefore space
;               for own variables, although it is not clear how much (in
;               the normal case, however, 4KB, which is more than enough
;               for the method used).
;       ax      Number of points, where the separator and end points are
;               counted as well.
;
; The passed list is closed, i.e. the first and last point of a subpolygon
; are the same.
;


PROC    FillPoly        NEAR
LOCAL   Y: WORD, YMax: WORD, Count: WORD, VertexList: DWORD, \
        LineList: DWORD, LineCount: WORD = LocalSize

        EnterProc       LocalSize

; Save or initialize variables

        and     ax, 7FFFh               ; BP 7.0 (High bit is flag for what?)
        mov     [Count], ax             ; Remember number of points
        mov     [Seg64], -1             ; No segment set

; Find the maximum and minimum Y value from the list.
; The separator and end points inserted by Graph are not needed,
; they are deleted when searching the list.
; The list is copied without these points behind the previous list. The
; pointer VertexList points to this new list. At the end of this new list,
; the line list is created.


        push    es
        pop     ds
        mov     si, bx                  ; ds:si = VertexList
        mov     di, bx                  ; es:di = VertexList
        mov     cx, ax                  ; cx = number of points
IF P80286
        shl     ax, 2
ELSE
        shl     ax, 1
        shl     ax, 1                   ; number * 4
ENDIF
        add     di, ax                  ; es:di points behind VertexList
        mov     [WORD LOW VertexList], di
        mov     [WORD HIGH VertexList], es
        cld

        mov     bx, [WORD ds:si+2]      ; first Y value as YMax...
        mov     dx, bx                  ; ...and as YMin

Even
@@L0:   lodsw                           ; Get X value
        cmp     ah, 080h                ; Separator or end point?
        jz      @@L3                    ; Jump if yes

; The point is valid, check if it is a new maximum or minimum.

        stosw                           ; Store X value
        ;movsw							; Get Y value / store Y value
        lodsw                           ; Get Y value
        stosw                           ; Store Y value
        cmp     ax, bx                  ; Y value > YMax?
        jl      @@L1                    ; Jump if not
        xchg    ax, bx                  ; Take over if yes
        LOOPCX  @@L0                    ; Value > Max can't be < Min
        jmp     @@C1

@@L1:   cmp     ax, dx                  ; Y value < YMin?
        jg      @@L2                    ; Jump if not
        xchg    ax, dx                  ; Take over if yes
@@L2:   LOOPCX  @@L0                    ; Next point
        jmp     @@C1

; The point is invalid and is skipped when copying

@@L3:   
		add2    si						; Ignore Y value
        dec     [Count]                 ; One point less
        jmp     @@L0                    ; and next point

; All separator and end points are now deleted, bx contains
; the largest and dx the smallest Y value of the polygon. The number of
; points in Count corresponds to the actual number.
; Maximum and minimum are saved and a pointer to the
; end of the polygon list is calculated, the space behind it is later used
; for storing the line list.

@@C1:   mov     [WORD LOW LineList], di
        mov     [WORD HIGH LineList], es

        SetToDS ds                      ; Reload data segment
        mov     ax, [Clip_Y2]
        cmp     bx, ax                  ; Clip YMax
        jle     @@C2
        xchg    ax, bx
@@C2:   mov     [YMax], bx

        mov     ax, [Clip_Y1]
        cmp     dx, ax                  ; Clip YMin
        jge     @@C3
        xchg    ax, dx
@@C3:

; For all Y values of the polygon, the start and end points of
; horizontal lines are now determined, with which the polygon will later
; be filled. The X values are initially stored unsorted in the line list
; and only sorted in ascending order later.

        mov     [Y], dx                 ; Initialize Y with minimum value

; Start of loop

Even
@@L4:   mov     ax, [Y]
        cmp     ax, [YMax]
        jg      @@L30                   ; End reached

        mov     [LineCount], 0          ; No X line points present
        les     di, [LineList]          ; Load pointer to list
        lds     si, [VertexList]        ; Load pointer to polygon list
        mov     cx, [Count]             ; Number of polygon points
        dec     cx                      ; One line less than points

; Determine if the addressed line intersects the current Y value

Even
@@L5:   mov     ax, [Y]                 ; Current Y value
        mov     bx, [WORD ds:si+2]      ; Y value 1
        mov     dx, [WORD ds:si+6]      ; Y value 2
        cmp     bx, dx                  ; bx < dx ?
        jle     @@L6                    ; Jump if yes

        cmp     ax, dx                  ; ax >= dx ?
        jl      @@L9                    ; No, no intersection
        cmp     ax, bx                  ; ax < bx ?
        jge     @@L9                    ; No, no intersection
        jmp     @@C10

@@L6:   cmp     ax, bx                  ; ax >= bx ?
        jl      @@L9                    ; No, no intersection
        cmp     ax, dx                  ; ax < dx ?
        jge     @@L9                    ; No, no intersection

; The line passes through the current Y coordinate. Calculate the X value
; where this happens.

Even
@@C10:  sub     bx, dx                  ; Y value1 - Y value2
        jnz     @@L7

; The slope of the polygon boundary line is 0! For the later
; line, both X coordinates must be stored. So there is
; no simple intersection here, but a whole line. Its
; end coordinates are recorded.

        mov     ax, [WORD ds:si]        ; X value 1
        stosw                           ; Remember in line list
        inc     [LineCount]             ; Increase number of points
        mov     ax, [WORD ds:si+4]      ; X value 2
        jmp     @@L8                    ; store and next line

; The line has a real slope, calculate the X value of the intersection.

@@L7:   mov     ax, [WORD ds:si]        ; X value 1
        sub     ax, [WORD ds:si+4]      ; - X value 2
        mov     dx, [Y]                 ; Current Y
        sub     dx, [WORD ds:si+2]      ; - Start Y
        imul    dx                      ;
        idiv    bx                      ;
        add     ax, [WORD ds:si]        ; X value in ax

; Store calculated X value and increase number of values

@@L8:   stosw                           ; Store X value
        inc     [LineCount]

; Address next line

@@L9:   add     si, 4                   ; Next polygon point
        LOOPCX  @@L5

; The line list now contains an unsorted sequence of
; X values. These must be sorted in ascending order.
; es and ds still point to the same segment, which is used by VertexList
; and LineList and therefore do not need to be reloaded.
; In addition, the fact that es=ds can be used.

        mov     si, [WORD LOW LineList] ; Load offset of LineList
        mov     cx, [LineCount]         ; Counter
        jcxz    @@L20                   ; There are no points!
        dec     cx                      ;

Even
@@L10:  lodsw                           ; Get first value
        mov     dx, cx                  ; Number of remaining values
        mov     di, si

Even
@@L11:  scasw                           ; ax > current value?
        jle     @@L12                   ; Jump if not
        xchg    ax, [WORD di-2]         ; Swap
@@L12:  dec     dx
        jnz     @@L11

        mov     [WORD ds:si-2], ax      ; Store smallest value back
        LOOPCX  @@L10                   ; Next X value

; The line list is now sorted in ascending order. For every two
; points from it, a horizontal line is output. For this,
; the ds register must be restored, as access
; to the data segment is necessary from here.
; The HorLine vector is used to draw the lines.
; The line must still be clipped in the X direction.


        SetToDS ds                      ; Load data segment
        mov     es, [VideoSeg]          ; and video segment to es ...
        call    [GE_Ready]              ; Wait until GE is ready
        call    [HorLineInit]           ; Perform initialization for HorLine
        mov     si, [WORD LOW LineList]
@@L15:  mov     ds, [WORD HIGH LineList]
        lodsw                           ; First X value
        xchg    ax, bx
        lodsw                           ; Second X value
        xchg    ax, cx

        SetToDS ds                      ; Reload data segment

        mov     ax, [Clip_X1]
        mov     dx, [Clip_X2]
        cmp     bx, dx                  ; Line completely to the right?
        jg      @@L18                   ; No line if yes
        cmp     bx, ax                  ; Clip X1
        jg      @@L16
        mov     bx, ax
@@L16:  cmp     cx, ax                  ; Line completely to the left?
        jl      @@L18                   ; No line if yes
        cmp     cx, dx                  ; Clip X2
        jl      @@L17
        mov     cx, dx
@@L17:
        mov     ax, [Y]
        push    si
        call    [HorLine]
        pop     si
@@L18:  sub     [LineCount], 2          ; Two points used
        ja      @@L15                   ; Continue if more are there

; Continue with the next Y value

@@L20:  inc     [Y]
        jmp     @@L4                    ; Next Y value

; End. Data segment does not need to be restored.

@@L30:  LeaveProc
        ret

ENDP    FillPoly

; ======================================================================
;
;         DW      FILLSTYLE       ; Set the filling pattern
;
;   Input:
;      al         Primary fill pattern number
;      es:bx      If the pattern number is 0FFh, this points to user define
;                 pattern mask.
;
;   Return:
;      Nothing
;
;

PROC    FillStyle Near

        cmp     al,0FFh                 ; User defined?
        jnz     @@L2                    ; No

; The user pattern is copied to the last table entry and then gets
; the number 12

        mov     cx, 8                   ; 8 bytes
        mov     di, OFFSET UserPattern  ; etc...
@@L1:   mov     al, [es:bx]
        mov     [di],al
        inc     di
        inc     bx
        loop    @@L1
        mov     al, 12                  ; User-defined

; Here it continues for all patterns

@@L2:   mov     [FillPatternNum], al

; Now copy the pattern for faster access
; cld was already set by the dispatcher

        cbw                             ; ah = 0
IF P80286
        shl     ax, 3
ELSE
		shl     ax, 1
		shl     ax, 1
        shl     ax, 1                  ; * 8
ENDIF
        xchg    si, ax                  ; xchg instead of mov
        add     si, OFFSET FillPatternTable
        mov     di, OFFSET FillPattern
        push    ds
        pop     es
IF P80386
        movsd
        movsd
ELSE
        movsw
        movsw
        movsw
        movsw
ENDIF
; And done

        ret

ENDP    FillStyle

; ==========================================================================
; Text output
;
;         DW      TEXT            ; Hardware text output at (CP)
;
;   Input:
;      es:bx      --> ASCII text of the string
;      cx         The length (in characters) of the string.
;
; This function is used to send hardware text to the output device. The text
; is output to the device beginning at the (CP). The (CP) is assumed to be
; at the upper left of the string.
;
;

;---------------------------------------------------
; Subroutine for text output
; Input
;  di = character
; RETURN
;   Pixel representation in buffer CharShape
;

PROC    GetCharShape    Near

        push    bp
        cld

IF P80286
        shl     di, 3
ELSE
        shl     di, 1
        shl     di, 1
        shl     di, 1                   ; * 8 for index in 8x8 font
ENDIF
        cmp     di, 128 * 8             ; Code >= 128 ?
        jb      ROMCh                   ; No

; Get pointer es:di from 0000h:0007Ch (RAM font for chars >= 80h)

IF      Ver3
        les     bx, [Int1F]             ; Get vector 1F
ELSE
        xor     bx, bx
        mov     es, bx
        les     bx, [DWORD es:07Ch]
ENDIF
        mov     ax, es
        or      ax, bx                  ; Vector = NIL ?
        jz      NoChar                  ; YES: Font does not exist
        lea     di, [es:di+bx-400h]     ; Offset of char to di
        jmp     CharOK                  ; es:di --> char

; Here entry if char >= 128 but no RAM font loaded

NoChar: mov     di, ' ' * 8             ; Use space from ROM

; Only a char < 128 or no RAM font exists
; The ROM font is at 0F000h:0FA6Eh

ROMCh:  lea     di, [di+ 0FA6Eh]
        mov     es, [SegF000]           ; es:di = 0F000h:0FA6Eh + char offset

; If the font must be rotated by 90° --> from here
; On entry, es:di points to the pixel representation of the char
; First, load the char into ax-dx

CharOK: mov     ax,[es:di+00h]                  ; Byte 1 and 2
        mov     bx,[es:di+02h]                  ; etc.
        mov     cx,[es:di+04h]
        mov     dx,[es:di+06h]
        SetToDS es                              ; es = data segment
        mov     di, Offset CharShape            ; That's where the char goes
        cmp     [TextOrient], 00                ; horizontal text?
        jnz     Rotate                          ; No, must be rotated

; We only need to store the char

        stosw                                   ; 1st + 2nd byte
        mov     ax, bx
        stosw                                   ; 2nd + 3rd ...
        mov     ax, cx
        stosw
        mov     ax, dx
        stosw
        jmp     GetCharEnd

; The char must be rotated

Rotate: mov     bp,0008h
RotLp:  shr     al,1
        rcl     si,1
        shr     ah,1
        rcl     si,1
        shr     bl,1
        rcl     si,1
        shr     bh,1
        rcl     si,1
        shr     cl,1
        rcl     si,1
        shr     ch,1
        rcl     si,1
        shr     dl,1
        rcl     si,1
        shr     dh,1
        rcl     si,1
        xchg    ax,si
        stosb
        xchg    ax,si
        dec     bp
        jnz     RotLp

; Done, the char is in the buffer CharShape

GetCharEnd:
        pop     bp
        ret

ENDP    GetCharShape



;---------------------------------------------------
; Character output hardware text
;
; Character is in al, bx = X, cx = Y
;

PROC    OutChar Near

Local   YMult: Word, Diff: Word = LocalSize

        EnterProc  LocalSize

; First check character position against clip window

        cmp     bx, [Clip_X1]
        jb      OutCharEnd
        mov     di, [Clip_X2]
        sub     di, [TextSizeX]
        inc     di
        cmp     bx, di
        ja      OutCharEnd

        cmp     cx, [Clip_Y1]
        jb      OutCharEnd
        mov     di, [Clip_Y2]
        sub     di, [TextSizeY]
        inc     di
        cmp     cx, di
        ja      OutCharEnd

; Calculate address of first point

        xor     ah, ah
        mov     di, ax                  ; Character in di
        mov     ax, cx                  ; ax = Y
        add     ax, [PageOfs]           ; Correction for current screen page
        mul     [MaxX]
        add     ax, bx                  ; + X
        adc     dl, 0
        mov     [Seg64], dl             ; Remember segment
        call    [SegSelect]             ; Set segment
        push    ax                      ; Remember offset

; Calculate how much must be added at the end of a line (X2) to get to
; (X1/Y+1)

        mov     ax, [MaxX]              ; Line length in pixels
        sub     ax, [TextSizeX]         ; - character size in pixels
        mov     [Diff], ax              ; and store

; Load the pixel representation of the char into the buffer (di = char)

        call    GetCharShape            ; Pixel representation --> CharShape

; Load variable

        mov     si, Offset CharShape    ; There is the char
        mov     al, [DrawingColor]
        mov     es, [VideoSeg]
        mov     bx, [TextMultX]         ; Into register for speed
        pop     di                      ; Offset

; Y loop

@@OC2:  mov     ah, [si]                ; Pixel in ah
        mov     dx, [TextMultY]
        mov     [YMult], dx

; X loop

@@OC3:  mov     dx, 8                   ; 8 pixels in bh
@@OC4:  mov     cx, bx                  ; X size of pixel
        rol     ah, 1                   ; Set pixel?
        jc      @@OC5                   ; Yes

; Do not set pixel, but still increase di

        add     di, cx
        jnc     @@OC7                   ; No segment carry
        inc     [Seg64]                 ; Carry
        call    [SegSelect]             ; set
        jmp     @@OC7                   ; Next char pixel

; End here for jump optimization

OutCharEnd:
        LeaveProc
        ret

; Handle segment overflow

@@OC9:  inc     [Seg64]                 ; Next segment
        call    [SegSelect]             ; ... set
        jmp     @@OC6

@@OC10: inc     [Seg64]                 ; Next segment
        call    [SegSelect]
        jmp     @@OC8

; Set pixel XMult times

@@OC5:  mov     [es:di], al             ; Set pixel
        inc     di                      ; Next pixel
        jz      @@OC9                   ; Handle segment overflow
@@OC6:  LOOPCX  @@OC5

; Next char pixel

@@OC7:  dec     dx
        jnz     @@OC4

; Next same line

        add     di, [Diff]              ; Next screen line
        jc      @@OC10                  ; Overflow
@@OC8:  dec     [YMult]
        jnz     @@OC3

; Next Y line

        inc     si
        cmp     si, Offset CharShape + 8
        jne     @@OC2
        jmp     OutCharEnd

ENDP    OutChar

; ------------------------------------------------------------
; Output of a text string (pixel font). Called by the dispatcher.

PROC    Text    Near

        call    [GE_Ready]              ; Wait until the GE is ready
        cmp     [TextOrient], 01h       ; Rotated 90°?
        jne     CharLoop
        add     bx, cx
        dec     bx

CharLoop:
        mov     al, [es:bx]             ; Character from string
        and     al, al                  ; 0 ?
        jz      TextEnd                 ; then end
        push    es
        push    bx
        push    cx
        mov     bx, [CursorX]
        mov     cx, [CursorY]
        call    OutChar                 ; Output character
        pop     cx
        pop     bx
        pop     es

; Dangerous! Here the X size of the font is optionally added to the X or Y
; coordinate. This only works for a hardware font with X/Y = 1/1.

        mov     dx, [TextSizeX]
        cmp     [TextOrient], 01h       ; Rotated 90°?
        jnz     @@L1
        add     [CursorY], dx
        dec     bx
        jmp     Short @@L2

@@L1:   add     [CursorX], dx
        inc     bx
@@L2:   LOOPCX  CharLoop

; Done

TextEnd:
        ret

ENDP    Text

; ========================================================================
; TextStyle
;
;         DW      TEXTSTYLE       ; Hardware text style control
;
;   Input:
;      al         Hardware font number
;      ah         Hardware font orientation
;                 0 = Normal,   1 = 90 Degree,   2 = Down
;      bx         Desired X Character (size in graphics units)
;      cx         Desired Y Character (size in graphics units)
;
;   Return:
;      bx         Closest X Character size available (in graphics units)
;      cx         Closest Y Character size available (in graphics units)
;

PROC    TextStyle Near

        mov     [TextNum], al
        mov     [TextOrient], ah

; Round X and Y down to 8 pixels each and ensure that the size is at least 1

        and     bx, 00F8h
        jnz     @@L1
        mov     bx, 8
@@L1:   mov     [TextSizeX], bx
        and     cx, 00F8h
        jnz     @@L2
        mov     cx, 8
@@L2:   mov     [TextSizeY], cx

; Set the corresponding multipliers

IF P80286
        shr     bl, 3
ELSE
        shr     bl, 1
        shr     bl, 1
        shr     bl, 1                   ; / 8
ENDIF
        mov     [Byte low TextMultX], bl

IF P80286
        shr     cl, 3
ELSE
        shr     cl, 1
        shr     cl, 1
        shr     cl, 1                    ; / 8
ENDIF
        mov     [Byte low TextMultY], cl

; Now return the actual size

        mov     bx, [TextSizeX]
        mov     cx, [TextSizeY]
        ret

ENDP    TextStyle

; ==========================================================================
;
;         DW      LINESTYLE       ; Set the line drawing pattern
;
;   Input:
;      al         Line pattern number
;      bx         User-defined line drawing pattern
;      cx         Line width for drawing
;
;   Return:
;      Nothing
;
; Sets the current line-drawing style and the width of the line. The line
; width is either one pixel or three pixels in width.
;

PROC    LineStyle Near

        cmp     al, 04h                 ; User-defined ?
        jz      @@L1                    ; yes
        xor     ah, ah
        shl     ax, 1                   ; Number * 2 because Word
        mov     bx, ax
        mov     bx, [LineStyles+bx]     ; Get bit pattern
@@L1:   mov     [LinePattern], bx       ; Store bit pattern
        mov     [LineWidth], cx         ; Store line width
        ret

ENDP    LineStyle

; ============================================================================
; Set clip window
;
;         DW      SETCLIP         ; Define a clipping rectangle
;
;   Input:
;      ax         Upper Left X coordinate of clipping rectangle
;      bx         Upper Left Y coordinate of clipping rectangle
;      cx         Lower Right X coordinate of clipping rectangle
;      dx         Lower Right Y coordinate of clipping rectangle
;
;   Return:
;      Nothing
;
; The SETCLIP vector defines a rectangular clipping region on the screen. The
; registers (ax,bx) - (cx,dx) define the clipping region.
;
;

PROC    SetClip Near

        mov     [Clip_X1], ax
        mov     [Clip_Y1], bx
        mov     [Clip_X2], cx
        mov     [Clip_Y2], dx
        ret

ENDP    SetClip

; ============================================================================
;
;         DW      SAVEBITMAP      ; Write from screen memory to system memory
;
;   Input:
;      es:bx      Points to the buffer in system memory
;                 to be written. es:[bx] contains the width of the
;                 rectangle -1. es:[bx+2] contains the heigth of
;                 the rectangle -1.
;
;      cx         The upper left X coordinate of the rectangle.
;      dx         The upper left Y coordinate of the rectangle.
;
;   Return:
;      Nothing
;
; The SAVEBITMAP routine is a block copy routine that copies screen pixels
; from a defined rectangle as specified by (si,di) - (cx,dx) to the system
; memory.
;
;

PROC    SaveBitmap      Near

Local SX1:Word, SDiff:Word, SWidth: Word = L

; Create stack frame

        mov     bp, sp
        sub     sp, L

; Wait until the GE is ready

        call    [GE_Ready]

; Store X1. Get and store width

        mov     [SX1], cx
        mov     ax, [Word es:bx]        ; Width - 1
        inc     ax
        push    ax                      ; save

; Calculate how much must be added when reaching the point X2+1 to
; reach X1 in the next line

        mov     cx, [MaxX]
        sub     cx, ax
        push    cx                      ; save difference

; Set es:di to es:bx+4 (buffer)

        lea     di, [bx+4]

; Get the number of lines into bx

        mov     bx, [es:bx+2]           ; Height - 1
        inc     bx

; Calculate the address of the pixel at X1/Y1. Afterwards, ax:si contains the
; current address, ds points to the video segment (no more variables!!!)

        mov     ax, dx                  ; Y1
        add     ax, [PageOfs]
        mul     [MaxX]                  ; dx is lost
        add     ax, [SX1]
        adc     dl, 0                   ; overflow
        xchg    si, ax                  ; Offset to si
        mov     [Seg64], dl             ; Segment
        call    [SegSelect]             ; Set segment

        mov     ds, [VideoSeg]
        pop     dx                      ; Difference (X2/Y) to (X1/Y+1)
        pop     bp                      ; Width of rectangle to bp

; Y-loop

Save1:  mov     cx, bp                  ; Width of rectangle = number of points

; Test if a segment overflow can occur in the line

        mov     ax, si                  ; Screen offset
        add     ax, cx                  ; + pixels to get
        jc      Save4                   ; Yes, overflow

; Line has no overflow
		RepMovS
; New line

Save2:  add     si, dx
        jc      Save7                   ; Carry
Save3:  dec     bx                      ; More lines?
        jnz     Save1                   ; Yes

; End

        add     sp, L
        ret

; X-loop if overflow

Even
Save4:  movsb                           ; Transfer byte
        test    si, si
        jz      Save6                   ; Carry
Save5:  LOOPCX  Save4
        jmp     Save2

; Correct segment overflow in X-loop

Save6:  SetToDS ds
        inc     [Seg64]                 ; overflow
        call    [SegSelect]             ; Set new segment
        mov     ds, [VideoSeg]
        jmp     Save5

; Correct segment overflow in Y-loop

Save7:  SetToDS ds
        inc     [Seg64]                 ; overflow
        call    [SegSelect]             ; Set new segment
        mov     ds, [VideoSeg]
        jmp     Save3

ENDP    SaveBitmap


; ============================================================================
;
;         DW      RESTOREBITMAP   ; Write screen memory to the screen.
;
;   Input:
;      es:bx      Points to the buffer in system memory
;                 to be read. es:[bx] contains the width of the
;                 rectangle -1. es:[bx+2] contains the heigth of
;                 the rectangle -1.
;
;      cx         The upper left X coordinate of the rectangle.
;      dx         The upper left Y coordinate of the rectangle.
;
;      al         The pixel operation to use when transferring
;                 the image into graphics memory. Write mode for
;                 block writing.
;                   0: Overwrite mode
;                   1: xor mode
;                   2: OR mode
;                   3: and mode
;                   4: Complement mode
;
;   Return:
;      Nothing
;
; The RESTOREBITMAP vector is used to load screen pixels from the system
; memory. The routine reads a stream of bytes from the system memory into the
; rectangle defined by (si,di) - (cx,dx). The value in the al register
; defines the mode that is used for the write. The following table defines
; the values of the available write modes:
;
;         Pixel Operation                 Code
;          Overwrite mode                  0
;          Logical xor                     1
;          Logical OR                      2
;          Logical and                     3
;          Complement                      4
;
;


;---------------------------------------------------
; First, a series of subroutines, each copying a line in the corresponding
; mode. Register usage see RestoreBitmap
;
; The following registers may be modified by the subroutines:
; AX, DI
;
; The subroutines that perform a modification are coded here as
; macros (OPC is the responsible operand):
;

MACRO   RestoreOP OP

; Test if an overflow can occur in the line

        mov     ax, di                  ;; Offset
        add     ax, cx                  ;; + pixels to set
        jc      @@L4                    ;; overflow possible

; Line can be copied without overflow

        shr     cx, 1                   ;; Odd number?
        jnc     @@L1                    ;; No
        lodsb                           ;; Yes: process byte
        OP      [BYTE es:di], al
        inc     di
@@L1:   jcxz    @@L7                    ;; Nothing more to do

IF P80386
        shr     cx, 1                   ;; Odd word count?
        jnc     @@L2                    ;; No
        lodsw                           ;; Yes: process word
        OP      [WORD es:di], ax
        Add2	di
@@L2:   jcxz    @@L7

ALIGN 4
@@L3:   lodsd
        OP      [DWORD es:di], eax      ;; XOR
        add     di, 4
        loop	@@L3
        ret
ELSE

ALIGN 4
@@L2:   lodsw                           ;; Get word
        OP      [WORD es:di], ax        ;; XOR
        Add2	di						; address next word
        loop	@@L2
        ret
ENDIF

; Copy line if segment overflow

EVEN
@@L4:   lodsb                           ;; Get byte
        OP      [BYTE es:di], al        ;; XOR
        inc     di
        jz      @@L6                    ;; No carry
@@L5:   
		loop	@@L4
@@L7:   ret

@@L6:   mov     ax, ds                  ;; save ds
        SetToDS ds                      ;; and reload
        inc     [Seg64]                 ;; carry
        call    [SegSelect]             ;; Set new segment
        mov     ds, ax                  ;; restore old ds
        jmp     @@L5

ENDM    RestoreOP




PROC    RestoreOver     Near

; Test if an overflow can occur in the line

        mov     ax, di                  ; Offset
        add     ax, cx                  ; + pixels to set
        jc      ROver1                  ; overflow possible

; No overflow possible
		RepMovS
        ret                             ; and end

; Copy line if segment overflow in the line

Even
ROver1: movsb                           ; Transfer byte
        test    di, di
        jz      ROver3                  ; No carry
ROver2: LOOPCX  ROver1
        ret

ROver3: mov     ax, ds                  ; save ds
        SetToDS ds                      ; and reload
        inc     [Seg64]                 ; carry
        call    [SegSelect]             ; Set new segment
        mov     ds, ax                  ; restore old ds
        jmp     ROver2

ENDP    RestoreOver



PROC    RestoreXOR      NEAR

        RestoreOP XOR

ENDP    RestoreXOR



PROC    RestoreOR       NEAR

        RestoreOP OR

ENDP    RestoreOR


PROC    RestoreAND      NEAR

        RestoreOP AND

ENDP    RestoreAND


PROC    RestoreNOT      NEAR

RNOT1:  lodsb                           ; Get byte
        not     al                      ; Complement
        mov     [Byte es:di], al        ; NOT
        inc     di
        jz      RNOT3                   ; carry
RNOT2:  LOOPCX  RNOT1
        ret

RNOT3:  mov     ax, ds                  ; save ds
        SetToDS ds                      ; and reload
        inc     [Seg64]                 ; carry
        call    [SegSelect]             ; Set new segment
        mov     ds, ax                  ; restore old ds
        jmp     RNOT2

ENDP    RestoreNOT





PROC    RestoreTrans    NEAR

@@L1:   ;mov     al, [si]                ; Get byte
        ;inc     si
        lodsb
        test    al, al                   ; = 0
        jz      @@L3                    ; Then do not write
        mov     [es:di], al             ; Write byte
        inc     di
        test    di, di                  ; overflow?
        jz      @@L2                    ; Jump if yes
        loop	@@L1
        jmp     @@L4                    ; Jump to end

; Handle segment overflow
; ATTENTION: This must always be done, even if the counter in cx has expired.
; The check for cx=0 comes at the end of the segment correction!!!

@@L2:   mov     ax, ds                  ; save ds
        SetToDS ds                      ; and load with data segment
        inc     [Seg64]
        call    [SegSelect]
        mov     ds, ax                  ; old ds
        jcxz    @@L4                    ; Just in case
        jmp     @@L1

; Do not write byte

EVEN
@@L3:   inc     di                      ; Skip byte
        loopne  @@L1                    ; Loop if no seg overflow
        je      @@L2                    ; Jump if overflow

; End

@@L4:   ret

ENDP    RestoreTrans


;---------------------------------------------------
; A table with the addresses of the functions

RestProcTable   dw      RestoreOver
                dw      RestoreXOR
                dw      RestoreOR
                dw      RestoreAND
                dw      RestoreNOT
                dw      RestoreTrans

;---------------------------------------------------

PROC    RestoreBitmap      NEAR

Local RX1:Word, RWidth: Word, RestProc: Word = LocalSize

; Create stack frame

        EnterProc       LocalSize
        push    ds                      ; We need this
        cld                             ; this too...

; Wait until the GE is ready

        call    [GE_Ready]

; Store X1. Assign RCopy with a vector that performs the mode-specific
; operation. Get and store width

        mov     [RX1], cx

        cbw                             ; al --> ax
        mov     di, ax                  ; ax --> di
        shl     di, 1                   ; * 2 for word
        mov     di, [RestProcTable+di]  ; Address
        mov     [RestProc], di          ; store

        mov     ax, [WORD es:bx]        ; Width - 1
        inc     ax
        mov     [RWidth], ax

; Calculate how much must be added when reaching the point X2+1 to
; reach X1 in the next line

        mov     cx, [MaxX]
        sub     cx, ax
        push    cx                      ; save value

; Set si to bx+4 (buffer)

        lea     si, [bx+4]

; Get the number of lines into bx

        mov     bx, [es:bx+2]           ; Height - 1
        inc     bx

; Calculate the address of the pixel at X1/Y1. Afterwards, ax:di contains the
; current address, es points to the video segment, ds to the buffer

        mov     ax, dx                  ; Y1
        add     ax, [PageOfs]           ; Screen page correction
        mul     [MaxX]                  ; dx is lost
        add     ax, [RX1]
        adc     dl, 0                   ; overflow
        xchg    di, ax                  ; Offset to di
        mov     [Seg64], dl             ; Store segment
        call    [SegSelect]             ; Set segment

        mov     cx, es
        mov     es, [VideoSeg]          ; VideoSeg in es:di
        mov     ds, cx                  ; Buffer in ds:si

        pop     dx                      ; RDiff ((Y/X2) --> (Y+1/X1) in dx)


; Y-loop

@@L1:   mov     cx, [RWidth]            ; Width of rectangle = number of points

; X-loop

        call    [RestProc]              ; Doit yeah !

; New line

        add     di, dx
        jc      @@L3                    ; carry
@@L2:   dec     bx                      ; More lines?
        jnz     @@L1                    ; Yes

; End

        pop     ds
        LeaveProc
        ret

; Correct segment overflow

@@L3:   mov     ax, ds
        SetToDS ds                      ; Load data segment
        inc     [Seg64]                 ; carry
        call    [SegSelect]             ; Set new segment
        mov     ds, ax
        jmp     @@L2

ENDP    RestoreBitMap


; ==========================================================================
;
;         DW      PALETTE         ; Load a color entry into the Palette
;
;   Input:
;      ax         The index number and function code for load
;      bx         The color value to load into the palette
;
;   Return:
;      Nothing
;
; The PALETTE vector is used to load single entries into the palette. The
; register ax contains the function code for the load action and the index
; of the color table entry to be loaded. The upper two bits of ax determine
; the action to be taken. The table below tabulates the actions. If the
; control bits are 00, the color table index in (ax and 03FFFh) is loaded
; with the value in bx. If the control bits are 10, the color table index in
; (ax and 03FFFh) is loaded with the RGB value in (Red=bx, Green=cx, and
; Blue=dx). If the control bits are 11, the color table entry for the
; background is loaded with the value in bx.
;
;  Control Bits           Color Value and Index
;
;       00                Register bx contains color, ax is index
;       01                not used
;       10                Red=bx  Green=cx  Blue=dx, ax is index
;       11                Register bx contains color for background
;

PROC    Palette   Near

        test    ah, 0c0h        ; Code = 00 ?
        jnz     @@L1

; Bits 00, bx = color, ax = index

        mov     bh, bl
        mov     bl, al           ; bl = Index, bh = Color
        jmp     @@L2

; Bits 10 or 11

@@L1:   test    ah, 40h
        jz      RGBPalette

; Bits 11, bx is background color

        mov     bh, bl                  ; Color in bh
        xor     bl, bl                  ; Index for background is 0
        mov     [BkColor], bh           ; Remember background color
@@L2:   mov     ax, 1000h               ; Set palette register
        int     10h
        ret

; Bits 10 --> RGB-Palette
; To match the manual, the color values must each be shifted right by 2 bits,
; the lowest 2 bits are not used according to the manual, but the DAC needs
; a range of 0..3Fh

; Change on 21.08.1991 according to c't 9/91 p. 162. Before setting the blue
; component, wait for the horizontal retrace to avoid access conflicts
; ("snow"). Interrupts are no longer disabled.

RGBPalette:
IF P80286
        shr     bx, 2
        shr     cx, 2
        shr     dx, 2
ELSE
        shr     bx, 1
        shr     bx, 1
        shr     cx, 1
        shr     cx, 1
        shr     dx, 1
        shr     dx, 1
ENDIF

        mov     si, dx
        mov     dx, 3C8h

        out     dx, al                  ; Entry number
        inc     dx
        mov     ax, bx
        out     dx, al                  ; Red
        mov     ax, cx
        out     dx, al                  ; Green

; Wait for horizontal retrace

        mov     dx, 03DAh
@@L3:   in      al, dx
        test    al, 1
        jnz     @@L3                    ; Wait for end of H-Sync
@@L4:   in      al, dx
        test    al, 1
        jz      @@L4                    ; Wait for start of H-Sync

; Now set blue component

        mov     dx, 03C9h
        xchg    ax, si
        out     dx, al                  ; Blue

; End

        ret

ENDP    Palette


; ======================================================================
;
;         DW      ALLPALETTE      ; Load the full palette
;
;   Input:
;      es:bx --> array of palette entries
;
;   Return:
;      Nothing
;
; The ALLPALETTE routine loads the entire palette in one driver
; call. The register pair es:bx points to the table of values to be loaded
; into the palette. The number of entries is determined by the color entries
; in the Driver Status Table. The background color is not explicitly loaded
; with this command.
;
;

PROC    AllPalette      Near

        mov     al, 02h
        mov     dx, bx
        mov     ah, 16                  ; 16 entries here only for compatibility
        int     10h                     ; Set palette
        ret

ENDP    AllPalette

; =========================================================================
;
;         DW      TEXTSIZ         ; Determine the height and width of text
;                                 ; strings in graphics units.
;
;   Input:
;      es:bx      --> ASCII text of the string
;      cx         The length (in characters) of the string.
;
;   Return:
;      bx         The width of the string in graphics units.
;      cx         The height of the string in graphics units.
;
; This function is used to determine the actual physical length and width of
; a text string. The current text attributes (set by TEXTSTYLE) are used to
; determine the actual dimensions of a string without displaying it. The
; application can thereby determine how a specific string will fit and reduce
; or increase the font size as required. There is NO graphics output for
; this vector. If an error occurs during length calculation, the STAT field
; of the Device Status Record should be marked with the device error code.
;
;

; This always assumes a horizontal string!

PROC    TextSize        Near

        xchg    ax, cx                  ; Length to ax
        mul     [TextSizeX]             ; * width of a character
        xchg    bx, ax                  ; Result to bx
        mov     cx, [TextSizeY]         ; cx is height of a character
        ret

ENDP    TextSize


; =========================================================================
;
; FloodFill routine include
;

INCLUDE "fill.asi"


; =========================================================================
;
; Routine to get a pixel. Expects X/Y in ax/bx.
; Result: Color in dl.
; For performance reasons, does not wait for the GE
;

PROC    GetPixel Near

        add     bx, [PageOfs]                   ; Screen page correction
        xchg    ax, bx                          ; Y in ax, X in bx
        mul     [MaxX]                          ; * line length
        add     bx, ax                          ; + offset
        adc     dl, 0                           ; Segment overflow
        mov     [Seg64], dl                     ; Remember segment
        call    [SegSelect]                     ; and set
        mov     ds, [VideoSeg]                  ; Load video segment
        mov     dl, [bx]                        ; Get byte
        ret

ENDP    GetPixel



; =========================================================================
;
; Routine to set a pixel. Expects X/Y in ax/bx, color in dl.
;
; For performance reasons, does not wait for the GE
;


PROC    PutPixel

        add     bx, [PageOfs]                   ; Screen page correction
        mov     cl, dl                          ; Save color
        xchg    ax, bx                          ; Y in ax, X in bx
        mul     [MaxX]                          ; * line length
        add     bx, ax                          ; + offset
        adc     dl, 0                           ; Segment overflow
        mov     [Seg64], dl                     ; Remember segment
        call    [SegSelect]                     ; Set segment
        mov     ds, [VideoSeg]                  ; Get video segment
        mov     [bx], cl                        ; Write byte
        ret

ENDP    PutPixel


; =====================================================================
;
;         DW      BITMAPUTIL      ; Bitmap Utilities Function Table
;
;   Input:
;      Nothing
;
;   Return:
;      es:bx      --> BitMap Utility Table.
;
;
; The BITMAPUTIL vector loads a pointer into es:bx, which is the base of a
; table defining special case-entry points used for pixel manipulation.
; These functions are currently only called by the ellipse emulation routines
; that are in the BGI Kernel. If the device driver does not use emulation
; for ellipses, this entry does not need to be implemented. This entry was
; provided because some hardware requires additional commands to enter and
; exit pixel mode, thus adding overhead to the GETPIXEL and SETPIXEL vectors.
; This overhead affected the drawing speed of the ellipse emulation routines.
; These entry points are provided so that the ellipse emulation routines can
; enter pixel mode, and remain in pixel mode for the duration of the ellipse-
; rendering process.
;
; The format of the BITMAPUTIL table is as follows:
;
;   DW    offset  GOTOGRAPHIC     ; Enter pixel mode on the graphics hardware
;   DW    offset  EXITGRAPHIC     ; Leave pixel mode on the graphics hardware
;   DW    offset  PUTPIXEL        ; Write a pixel to the graphics hardware
;   DW    offset  GETPIXEL        ; Read a pixel from the graphics hardware
;   DW    offset  GETPIXBYTE      ; Return a word containing the pixel depth
;   DW    offset  SET_DRAW_PAGE   ; Select page in which to draw primitives
;   DW    offset  SET_VISUAL_PAGE ; Set the page to be displayed
;   DW    offset  SET_WRITE_MODE  ; xor Line Drawing Control
;
; The parameters of these functions are as follows:
;
;         GOTOGRAPHIC     ; Enter pixel mode on the graphics hardware
;         This function is used to enter the special Pixel Graphics mode.
;
;         EXITGRAPHIC     ; Leave pixel mode on the graphics hardware
;         This function is used to leave the special Pixel Graphics mode.
;
;         PUTPIXEL        ; Write a pixel to the graphics hardware
;         This function has the same format as the PUTPIXEL entry described
;         above.
;
;         GETPIXEL        ; Read a pixel from the graphics hardware
;         This function has the same format as the GETPIXEL entry described
;         above.
;
;         GETPIXBYTE      ; Return a word containing the pixel depth
;         This function returns the number of bits per pixel (color depth) of
;         the graphics hardware in the ax register.
;
;         SET_DRAW_PAGE   ; Select alternate output graphics pages (if any)
;         This function take the desired page number in the al register and
;         selects alternate graphics pages for output of graphics primitives.
;
;         SET_VISUAL_PAGE ; Select the visible alternate graphics pages (if any)
;         This function take the desired page number in the al register and
;         selects alternate graphics for displaying on the screen.
;
;         SET_WRITE_MODE  ; xor Line drawing mode control. xor Mode is selected
;         if the value in ax is one, and disabled if the value in ax is zero.
;
;
;

;--------------------------------

PROC    BitMapUtil Near

        push    ds
        pop     es
        mov     bx, OFFSET BitMapUtilTable
        ret

ENDP    BitMapUtil

;---------------------------------------------------
; The various functions of the BitMapUtil table start here

PROC    GetPixByte      FAR

        mov     ax, 8
        ret

ENDP    GetPixByte



PROC    SetWriteMode    FAR

        push    ds
        SetToDS ds
        and     al, 01h
        mov     [WriteMode], al
        pop     ds
        ret

ENDP    SetWriteMode


PROC    SetDrawPage FAR

        push    ds
        SetToDS ds                              ; Load data segment correctly

; Call card-specific routine

        mov     bx, [ModePtr]                   ; Pointer to mode descriptor
        
		IF P80386        
			movzx     bx, [(TMode bx).CardType]       ; Get card type
		ELSE
	        mov     bl, [(TMode bx).CardType]       ; Get card type
    	    xor     bh, bh                           ; ... into bx
		ENDIF
      
        shl     bx, 1                           ; * 2 for word access
        call    [SetDrawPageTable+bx]           ; card-specific call

        pop     ds
        ret

ENDP    SetDrawPage


PROC    SetVisualPage FAR

        push    ds
        SetToDS ds                              ; Load data segment

; Call card-specific routine

        mov     bx, [ModePtr]                   ; Pointer to mode descriptor
        
		IF P80386        
			movzx   bx, [(TMode bx).CardType]
		ELSE
	        mov     bl, [(TMode bx).CardType]       ; Get card type
	        xor     bh, bh                           ; ... into bx
		ENDIF

        
        shl     bx, 1                           ; * 2 for word access
        call    [SetVisualPageTable+bx]         ; card-specific call

        pop     ds
        ret

ENDP    SetVisualPage


; ========================================================================
;
;         DW      offset COLOR_QUERY      ; Device Color Information Query
;
; This vector is used to inquire about the color capabilities of a given
; piece of hardware. A function code is passed into the driver in al. The
; following function codes are defined:
;
; >>> Color Table Size    al = 000h
;   Input:
;      None:
;
;   Return:
;      bx    The size of the color lookup table.
;      cx    The maximum color number allowed.
;
; The COLOR TABLE SIZE query is used to determine the maximum number of
; colors supported by the hardware. The value returned in the bx register is
; the number of color entries in the color lookup table. The value returned
; in the cx register is the highest number for a color value. This value is
; usually the value in bx minus one; however, there can be exceptions.
;
;
; >>> Default Color Table    al = 001h
;   Input:
;      Nothing
;
;   Return:
;      es:bx   --> default color table for the device
;
; The DEFAULT COLOR TABLE function is used to determine the color table
; values for the default (power-up) color table. The format of this table is
; a byte containing the number of valid entries, followed by the given number
; of bytes of color information.
;
;

PROC    ColorQuery NEAR

        cmp     al,01h
        jz      @@L1
        mov     bx, AvailColors         ; 16 colors occupied
        mov     cx, MaxColors-1         ; but 256 allowed
        ret

@@L1:   push    ds
        pop     es
        mov     bx, OFFSET ColorTable
        ret

ENDP    ColorQuery

;   ================================================================
;
;         DW      PATBAR          ; fill rectangle (X1,Y1), (X2,Y2)
;
;   Input:
;      ax         X1--the rectangle's left coordinate
;      bx         Y1--the rectangle's top coordinate
;      cx         X2--the rectangle's right coordinate
;      dx         Y2--the rectangle's bottom coordinate
;
;   Return:
;      Nothing
;
; Fill (but don't outline) the indicated rectangle with the current fill
; pattern and fill color.
;

PROC    PatBar NEAR

Local BX1:Word, BY1:Word, Diff:Word, BHe:Word, BW:Word, Lines:Word, Lines8:Word = L

; Create stack frame (bp does not need to be saved)

        mov     bp, sp
        sub     sp, L

; Wait until the GE is ready

        call    [GE_Ready]

; Correct Y values for the set screen page offset

        mov     si, [PageOfs]
        add     bx, si
        add     dx, si

; Rearrange X1/Y1 and X2/Y2 so that X1<=X2 and Y1<=Y2.
; X2 and Y2 are only needed to calculate width and height and are not
; stored.
; The swapping at the beginning is unfortunately necessary, even if not
; documented...

        cmp     ax, cx
        jb      @@L1
        xchg    ax, cx
@@L1:   cmp     bx, dx
        jb      @@L2
        xchg    bx, dx
@@L2:   mov     [BX1], ax
        mov     [BY1], bx

; Calculate how much must be added when reaching the point X2+1 to get
; to X1 in the next line
; Calculate width and height in pixels

        sub     dx, bx                  ; Y2 - Y1
        inc     dx                      ; + 1
        mov     [BHe], dx               ; = height
        sub     cx, ax                  ; X2 - X1
        inc     cx                      ; + 1
        mov     [BW], cx                ; = width
        mov     ax, [MaxX]
        sub     ax, cx
        mov     [Diff], ax              ; X2+1+Diff = X1 + MaxX

; Store eight times the line length in bytes. Every 8 lines the pattern
; repeats, and (if there is no overflow between the 8 lines) we can simply
; copy with rep movsb.
; Also, a counter for the lines must be kept, since this only works if at
; least 7 lines have been written (looks good otherwise, but does not meet
; other criteria).

        mov     ax, [MaxX]              ; Line length in bytes
IF P80286
        shl     ax, 3
ELSE
        shl     ax, 1
        shl     ax, 1
        shl     ax, 1                   ; * 8
ENDIF
        mov     [Lines8], ax            ; Bytes for 8 lines
        mov     [Lines], 0000           ; Line counter

; Address of the currently processed point is in al:di.

        mov     ax, [MaxX]
        mul     [BY1]                   ; BY1 * MaxX, dx is trashed
        add     ax, [BX1]
        adc     dx, 0                   ; Overflow
        mov     di, ax                  ; Offset
        mov     [Seg64], dl             ; Segment
        call    [SegSelect]             ; Set segment

; Set es to video segment

        mov     es, [VideoSeg]

; BX1 := BX1 mod 8

        and     [BX1], 7                ; BX1 mod 8

; Check two special cases: EmptyFill and SolidFill.

		IF P80386        
			movzx     ax, [FillColor]
		ELSE
	        xor     ah, ah                  ; Color = background color
	        mov     al, [FillColor]
		ENDIF

        cmp     [FillPatternNum], SolidFill
        je      PatBar1                 ; al = SolidFill color
        xchg    ah, al                  ; al=0, ah=FillColor, Flags=Const
        jb      PatBar1                 ; Jump if EmptyFill

; Load color into bx

        mov     bx, ax                  ; bh=FillColor, bl=0 (background)

; Y loop
; Get matching pattern for Y value

Even
Bar1:   mov     cx, [BW]                ; Width of rectangle = number of points

; Check if a segment overflow can occur within the line. If
; so, fill with overflow check and pattern by hand.

        mov     dx, di                  ; Address offset
        add     dx, cx                  ; + pixels to set
        jc      Bar2

; No overflow in this line.
; Next, check if there are already 7 lines on the screen. If
; not --> fill by hand

        cmp     [Lines], 7
        jbe     Bar2                    ; nothing

; We have more than 7 lines, i.e. if the line from 8 lines before is
; in the same segment, we can simply copy.

        mov     si, di                  ;
        sub     si, [Lines8]            ; - bytes for 8 lines
        jc      Bar2                    ; overflow, nothing

; So: fill by copying

        mov     dx, ds                  ; save ds
        mov     ds, [VideoSeg]          ; ... and also to video segment
        test    di, 1                   ; Address odd?
        jz      Bar02                   ; No
        movsb                           ; Copy 1 byte (--> address even)
        loopz   Bar03                   ; one byte less, done!
Bar02:
		RepMovS
Bar03:  mov     ds, dx

; Next line

Bar4:   inc     [BY1]                   ; Next line
        add     di, [Diff]              ; Continue address
        jc      Bar04                   ; Carry to segment
Bar0:   inc     [Lines]                 ; We have one more line
        dec     [BHe]                   ; Height - 1
        jnz     Bar1

; And here is the end. ds is not reset because the dispatcher
; pops it anyway

PatBarEnd:
        add     sp, L
        ret

; Handle segment overflow in the Y loop

Bar04:  inc     [Seg64]                 ; Carry
        call    [SegSelect]             ; Set segment
        jmp     Bar0                    ; And continue

; ----------------------------------------------------
; Line with overflow or too few lines to copy
; Get matching pattern for Y value

Bar2:   mov     si, [BY1]
        and     si, 7
        mov     ah, [FillPattern + si]  ; Desired pattern in ah, si = Y mod 8
        mov     cx, [BX1]               ; = BX1 mod 8
        rol     ah, cl                  ; Set first point correctly

; Get number of points (new because cx was destroyed)

        mov     cx, [BW]                ; Width of rectangle = number of points

; Here we go
; Test pattern (Even for optimization)

Even
Bar5:   rol     ah, 1                   ; Set point?
        jc      Bar3                    ; Yes
        mov     [Byte es:di], bl        ; BkColor
        inc     di                      ; Next address
        jz      Bar6                    ; Correct overflow
Bar7:   LOOPCX  Bar5                    ; Next point
        jmp     Bar4                    ; Next line

; Here if point is set

Even
Bar3:   mov     [es:di], bh             ; Set point in fill color
        inc     di                      ; Address next point
        jz      Bar8                    ; Correct overflow
Bar9:   LOOPCX  Bar5                    ; Next point
        jmp     Bar4                    ; Next line

; Here entry for segment overflows

Bar6:   inc     [Seg64]
        call    [SegSelect]
        jmp     Bar7

Bar8:   inc     [Seg64]
        call    [SegSelect]
        jmp     Bar9

; -------------------------------------------------------------
; Special case for fast filling: No pattern present.
; Register usage:
;   AL  = color
;   DI  = offset in video segment
;   ES  = video segment

PatBar1:
        mov     si, [BHe]               ; Height of rectangle
        mov     bx, [Diff]              ; Value for overflow from X2+1 --> X1
        mov     ah, al                  ; Color also in ah
        mov     bp, [BW]                ; No more local variables from here

PB1:    mov     cx, bp                  ; Width of rectangle

; Test if a segment overflow can occur within the line

        mov     dx, di                  ; Address offset
        add     dx, cx                  ; + pixels to set
        jc      PB2                     ; Overflow in the line

; Line is without overflow

        test    di, 1                   ; Address odd?
        jz      PB4                     ; No
        stosb                           ; Set point
        loopz	PB5						; Width was 1

PB4:    RepStoS		                    ; / 2
										; Set words until cx = 0
										; Was there still a carry?
										; Remaining byte if cx <> 0

; New line

PB5:    dec     si                      ; Another line?
        jz      PatBarEnd               ; No, end
        add     di, bx                  ; bx = [Diff]
        jnc     PB1
        inc     [Seg64]                 ; Overflow
        call    [SegSelect]             ; Set new segment
        jmp     PB1

; Segment overflow within the line

PB2:    mov     [es:di], al             ; Set pixel
        inc     di
        jnz     PB3                     ; No segment overflow
        inc     [Seg64]                 ; Overflow
        call    [SegSelect]             ; And set segment
PB3:    LOOPCX  PB2
        jmp     PB5

ENDP    PatBar

; ======================================================================
;
;         DW      ARC             ; Draw an elliptical arc
;
;   Input:
;      AX         The starting angle of the arc in degrees (0-360)
;      BX         The ending angle of the arc in degrees (0-360)
;      CX         X radius of the elliptical arc
;      DX         Y radius of the elliptical arc
;
;   Return:
;      Nothing
;
; ARC draws an elliptical arc using the (CP) as the center point of the
; arc, from the given start angle to the given end angle. To get circular
; arcs the application (not the driver) must adjust the Y radius as follows:
;
;      YRAD := XRAD * (ASPEC / 10000)
;
; where ASPEC is the aspect value stored in the DST.
;
;


; ---------------------------------------------------------------------------
;
; Plotting the points (Y/X0-X) and (Y/X0+X). The Y value is definitely in
; the window and is in ax. Due to the trick in the calling routine, X1 (di)
; is always positive, so the address calculation proceeds as normal.
;
; Register usage:
;    ax  = Y
;    di  = X1
;    si  = DeltaX   (X2 - X1)
;    cl  = Flags, Bit0 = set X1, Bit1 = set X2
;

PROC    Plot2   Near

        add     ax, [PageOfs]           ; Correction for screen page
        mul     [MaxX]
        add     di, ax                  ; Offset in video segment
        adc     dl, 0                   ; Overflow
        cmp     dl, [Seg64]             ; Is the segment already correct?
        jz      @@L1                    ; Yes
        mov     [Seg64], dl             ; No: remember...
        call    [SegSelect]             ; ...and set
@@L1:   mov     al, [DrawingColor]      ; Get color
        test    cl, 01h                 ; Plot this point?
        jz      @@L2                    ; No
        mov     [es:di], al             ; Yes: set point
@@L2:   test    cl, 02h                 ; Plot the second point?
        jz      @@L4                    ; No

; Plot the second point

        add     di, si                  ; + DeltaX
        jc      @@L3                    ; Segment overflow
        stosb                           ; Set point
        ret

; Correct segment overflow

@@L3:   inc     [Seg64]                 ; Consider segment overflow
        call    [SegSelect]             ; Set segment
        stosb                           ; Set point

; And done

@@L4:   ret

ENDP    Plot2

; -------------------------------------------------------------------
; Subroutine for ellipse to set the 4-way mirrored point with
; 1 pixel diameter.
; cx = x, bx = y, ds = DSeg, es = VideoSeg
; ds, es and bp must be preserved, everything else may be destroyed.
; [Seg64] is expected to contain the currently set segment - the variable
; must be initialized to 0ffh before start.
;

PROC    EPlot4Thin   Near
Local   X1: WORD = LocalSize

        EnterProc       LocalSize

; Calculate X values and check against the clipping window boundaries

        mov     di, [CursorX]
        sub     di, cx                  ; X1
        mov     [X1], di
        shl     cx, 1                   ; DeltaX
        mov     si, cx                  ; si = DeltaX
        mov     ax, di
        add     ax, cx                  ; X2

        xor     cx, cx                  ; Flags = 0
        cmp     di, [Clip_X1]           ; Left point inside left?
        jl      @@L1                    ; No
        cmp     di, [Clip_X2]           ; Left point inside right?
        jg      @@L1                    ; No
        or      cl, 1                   ; Set bit for left point
@@L1:   cmp     ax, [Clip_X1]           ; Right point inside left?
        jl      @@L2                    ; No
        cmp     ax, [Clip_X2]           ; Right point inside right?
        jg      @@L2                    ; No
        or      cl, 02h                 ; Set bit for right point
@@L2:   jcxz    @@L5                    ; Both bits 0 --> no points

; Here comes a somewhat "sly" trick: for negative X1, the left point is not
; drawn, but since the plot routine treats X1 as unsigned (and thus very
; large) and gets confused, the following approach is taken: X1 is set to 0
; and DeltaX is reduced by the amount X1 was increased. This way, X2 is
; plotted correctly and the address calculation is correct without a
; 10-line assembler acrobatics in the innermost loop.

        test    di, di                  ; Is di negative?
        jns     @@L3                    ; No
        add     si, di                  ; si = reduce DeltaX
        xor     di, di                  ; X1 = 0
        mov     [X1], di                ; and store

; Calculate Y value of the upper two points, check if these points are in
; the window

@@L3:   mov     ax, [CursorY]           ; Y0
        sub     ax, bx                  ; -Y
        cmp     ax, [Clip_Y2]           ; Y1 below the window?
        jg      @@L5                    ; --> None of the points will be plotted
        cmp     ax, [Clip_Y1]           ; Y1 above the window?
        jl      @@L4                    ; --> Point will not be plotted

; Call the plot function. ax = Y, cl = Flags, di = X1, si = DeltaX
; Destroys ax, di

        call    Plot2

; Calculate next point

@@L4:   mov     ax, [CursorY]
        add     ax, bx                  ; Y
        cmp     ax, [Clip_Y2]           ; Below?
        jg      @@L5                    ; Yes --> Done
        cmp     ax, [Clip_Y1]           ; Above?
        jl      @@L5                    ; Yes --> Done

; Call the plot function. ax = Y, cl = Flags, di = X1, si = DeltaX
; Destroys ax, di

        mov     di, [X1]
        call    Plot2

; Done!

@@L5:
        LeaveProc
        ret

ENDP    EPlot4Thin


; -------------------------------------------------------------------
; Subroutine for ellipse to set the 4-way mirrored point with
; 3 pixel diameter.
; cx = x, bx = y, ds = DSeg, es = VideoSeg
; ds, es and bp must be preserved, everything else may be destroyed.
; [Seg64] is expected to contain the currently set segment - the variable
; must be initialized to 0ffh before start.
;

PROC    EPlot4Thick Near
Local   X: WORD, Y: WORD = LocalSize

        EnterProc       LocalSize

        mov     [X], cx
        mov     [Y], bx

; Call EPlot4Thin 5 times

        dec     bx
        js      @@L1
        call    EPlot4Thin              ; X/Y-1
@@L1:   mov     bx, [Y]
        mov     cx, [X]
        dec     cx
        js      @@L2
        call    EPlot4Thin              ; X-1/Y
@@L2:   mov     bx, [Y]
        mov     cx, [X]
        call    EPlot4Thin              ; X/Y
        mov     bx, [Y]
        mov     cx, [X]
        inc     cx
        call    EPlot4Thin              ; X+1/Y
        mov     bx, [Y]
        mov     cx, [X]
        inc     bx
        call    EPlot4Thin              ; X/Y+1

; End

        LeaveProc
        ret

ENDP    EPlot4Thick

; ----------------------------------------------------------------
; Drawing a (full) ellipse. The midpoint algorithm from "Computer Graphics"
; p. 90 (88ff) is used, but with second order differences and
; consolidation of factors, etc.
;
; PROCEDURE Ellipse (X0, Y0, A, B);
;
; VAR
;   X, Y   : INTEGER;
;   Diff   : LONGINT;
;   QA, QB : LONGINT;
;   QA2    : LONGINT;
;   QB2    : LONGINT;
;   DeltaX : LONGINT;
;   DeltaY : LONGINT;
;   R1     : LONGINT;
;
;
; BEGIN
;   X := 0;
;   Y := B;
;   QA := LONGINT (A) * LONGINT (A);
;   QB := LONGINT (B) * LONGINT (B);
;   QA2 := 2 * QA;
;   QB2 := 2 * QB;
;
;   EPixels;
;
;   Diff := QB - (QA * LONGINT (B)) + (QA DIV 4);
;   R1 := QA * LONGINT (Y) - (QA DIV 2) - QB;
;   DeltaX := QB2 + QB;
;   DeltaY := QA2 * LONGINT (-Y + 1);
;
;   WHILE (R1 > 0) DO BEGIN
;     IF (Diff >= 0) THEN BEGIN
;       Inc (Diff, DeltaY);
;       Inc (DeltaY, QA2);
;       Dec (Y);
;       Dec (R1, QA);
;     END;
;     Inc (Diff, DeltaX);
;     Inc (DeltaX, QB2);
;     Inc (X);
;     Dec (R1, QB);
;     EPixels;
;   END;
;
;   Diff := (QB * (LONGINT (X + 1) * LONGINT (X) - QA) + (QB DIV 4)) +
;           (SQR (LONGINT (Y - 1)) * QA);
;   DeltaX := QB2 * LONGINT (X + 1);
;   DeltaY := QA * LONGINT (-2 * Y + 3);
;
;   WHILE (Y > 0) DO BEGIN
;     IF (Diff < 0) THEN BEGIN
;       Inc (Diff, DeltaX);
;       Inc (DeltaX, QB2);
;       Inc (X);
;     END;
;     Inc (Diff, DeltaY);
;     Inc (DeltaY, QA2);
;     Dec (Y);
;     EPixels;
;   END;
; END;
;

; Entry with: cx = X radius (A), dx = Y radius (B)


PROC    Ellipse   Near

Local X: WORD, Y: WORD, Diff: DWORD, QA: DWORD, QB: DWORD, QA2: DWORD, \
      QB2: DWORD, DeltaX: DWORD, DeltaY: DWORD, R1: DWORD = LocalBytes

; Check if either radius <= 0. If so, exit immediately

        test    cx, cx
        jle     @@N1
        test    dx, dx
        jg      @@N2
@@N1:   ret

; Set up stack frame

@@N2:   mov     bp, sp
        sub     sp, LocalBytes

; Initialize variables

        mov     [X], 0
        mov     [Y], dx                 ; dx = B
        mov     ax, dx                  ; ax = dx = B
        mul     dx
        mov     [WORD low QB], ax
        mov     [WORD high QB], dx      ; QB := B * B;

        shl     ax, 1
        rcl     dx, 1
        mov     [WORD low QB2], ax
        mov     [WORD high QB2], dx     ; QB2 := QB * 2;

        add     ax, [WORD low QB]
        adc     dx, [WORD high QB]
        mov     [WORD low DeltaX], ax
        mov     [WORD high DeltaX], dx  ; DeltaX := QB2 + QB

        mov     ax, cx                  ; ax = A
        mul     cx                      ;
        mov     [WORD low QA], ax
        mov     [WORD high QA], dx      ; QA := A * A;

        shl     ax, 1
        rcl     dx, 1
        mov     [WORD low QA2], ax
        mov     [WORD high QA2], dx     ; QA2 := QA * 2;

;   DeltaY := QA2 * LONGINT (-Y + 1);

        mov     bx, [Y]
        dec     bx                      ; bx = Y - 1
        mov     cx, dx                  ; save high word of QA2
        mul     bx
        mov     di, ax                  ; low word already done
        mov     si, dx                  ; save high word
        mov     ax, cx                  ; high word of QA2
        mul     bx
        add     ax, si
        not     di
        not     ax
        sub     di, 1
        sbb     ax, 0
        mov     [WORD low DeltaY], di
        mov     [WORD high DeltaY], ax

;   Diff := QB - (QA * LONGINT (B)) + (QA DIV 4);

        mov     ax, [WORD low QA]
        mov     bx, [WORD high QA]
        mov     di, ax
        mov     si, bx
        shr     si, 1
        rcr     di, 1
        shr     si, 1
        rcr     di, 1
        add     di, [WORD low QB]
        adc     si, [WORD high QB]      ; QB + (QA div 4)

        mul     [Y]                     ; Y = B
        xchg    ax, bx                  ; low word in bx
        mov     cx, dx                  ; high word in cx
        mul     [Y]
        add     ax, cx
        sub     di, bx
        sbb     si, ax                  ; - (QA * B)

        mov     [WORD low Diff], di
        mov     [WORD high Diff], si

;   R1 := QA * LONGINT (Y) - (QA DIV 2) - QB;

        mov     ax, [WORD low QA]
        mov     bx, [WORD high QA]      ; QA = ax:bx
        mov     di, ax
        mov     si, bx
        shr     si, 1
        rcr     di, 1

        mul     [Y]
        xchg    ax, bx
        mov     cx, dx
        mul     [Y]
        add     ax, cx
        sub     bx, di
        sbb     ax, si                  ; QA * Y - (QA div 2)

        sub     bx, [WORD low QB]
        sbb     ax, [WORD high QB]
        mov     [WORD low R1], bx
        mov     [WORD high R1], ax

; Other small stuff

        mov     es, [VideoSeg]
        mov     [Seg64], 0FFh           ; Flag for "no segment set"
        mov     cx, [X]
        mov     bx, [Y]
        call    [PlotVector]

; Phew! Initialization done, now comes the loop
; WHILE (R1 > 0) DO BEGIN

@@L1:   xor     ax, ax                  ; ax = 0
        cmp     [WORD high R1], ax
        jl      @@L5
        jg      @@L2
        cmp     [WORD low R1], ax
        jbe     @@L5
@@L2:

; IF (Diff >= 0) THEN BEGIN

        cmp     [WORD high Diff], ax
        jl      @@L4
        jg      @@L3
        cmp     [WORD low Diff], ax
        jb      @@L4

; Diff is >= 0

@@L3:   mov     ax, [WORD low DeltaY]
        mov     dx, [WORD high DeltaY]
        add     [WORD low Diff], ax
        adc     [WORD high Diff], dx    ; Inc (Diff, DeltaY)

        add     ax, [WORD low QA2]
        adc     dx, [WORD high QA2]
        mov     [WORD low DeltaY], ax
        mov     [WORD high DeltaY], dx  ; Inc (DeltaY, QA2)

        dec     [Y]                     ; Dec (Y);

        mov     ax, [WORD low QA]
        mov     dx, [WORD high QA]
        sub     [WORD low R1], ax
        sbb     [WORD high R1], dx      ; Dec (R1, QA);

; What else must be done in the first region

@@L4:   mov     ax, [WORD low DeltaX]
        mov     dx, [WORD high DeltaX]
        add     [WORD low Diff], ax
        adc     [WORD high Diff], dx    ; Inc (Diff, DeltaY);

        add     ax, [WORD low QB2]
        adc     dx, [WORD high QB2]
        mov     [WORD low DeltaX], ax
        mov     [WORD high DeltaX], dx  ; Inc (DeltaX, QB2);

        inc     [X]                     ; Inc (X);

        mov     ax, [WORD low QB]
        mov     dx, [WORD high QB]
        sub     [WORD low R1], ax
        sbb     [WORD high R1], dx      ; Dec (R1, QB);

; Set point and repeat

        mov     cx, [X]
        mov     bx, [Y]
        call    [PlotVector]
        jmp     @@L1

; Second region: initializations
;   Diff := (QB * (LONGINT (X + 1) * LONGINT (X) - QA) + (QB DIV 4)) +
;           (SQR (LONGINT (Y - 1)) * QA);
;

@@L5:   mov     ax, [X]
        inc     ax
        mul     [X]                     ; X is always positive, QB too
        sub     ax, [WORD low QA]
        sbb     dx, [WORD high QA]      ; - QA
        mov     di, ax
        mov     si, dx                  ; X * (X+1) - QA in di:si
        mul     [WORD high QB]
        mov     cx, ax                  ; Intermediate result in bx:cx
        mov     ax, di
        mul     [WORD low QB]
        mov     bx, ax
        add     cx, dx
        mov     ax, si
        mul     [WORD low QB]
        add     cx, ax                  ; bx:cx = ((X+1) * X - QA) * QB

        mov     ax, [WORD low QB]
        mov     dx, [WORD high QB]
        shr     dx, 1
        rcr     ax, 1
        shr     dx, 1
        rcr     ax, 1
        add     bx, ax
        adc     cx, dx                  ; + (QB DIV 4)

        mov     ax, [Y]
        dec     ax                      ; Y-1 (always positive)
        mul     ax                      ; ax * ax
        mov     di, ax
        mov     si, dx
        mul     [WORD high QA]
        add     cx, ax                  ; Intermediate result in bx:cx
        mov     ax, di
        mul     [WORD low QA]
        add     bx, ax
        adc     cx, dx
        mov     ax, si
        mul     [WORD low QA]
        add     cx, ax                  ; Diff in bx:cx

        mov     [WORD low Diff], bx
        mov     [WORD high Diff], cx

;   DeltaX := QB2 * LONGINT (X + 1);

        mov     bx, [X]
        inc     bx                      ; X+1 always positive
        mov     ax, [WORD low QB2]
        mul     bx
        mov     [WORD low DeltaX], ax
        mov     cx, dx
        mov     ax, [WORD high QB2]
        mul     bx
        add     ax, cx
        mov     [WORD high DeltaX], ax

;   DeltaY := QA * LONGINT (-2 * Y + 3);

        mov     bx, [Y]
        shl     bx, 1
        sub     bx, 3                   ; bx = 2*Y - 3
        mov     ax, [WORD low QA]
        mul     bx
        xchg    ax, bx
        mov     cx, dx
        mul     [WORD high QA]
        add     ax, cx
        not     bx
        not     ax
        add     bx, 1
        adc     ax, 0
        mov     [WORD low DeltaY], bx
        mov     [WORD high DeltaY], ax

; Gargl! But now it starts...

@@L6:   xor     ax, ax
        cmp     [Y], ax
        jle     @@L9

; IF (Diff < 0) THEN BEGIN

        cmp     [WORD high Diff], ax
        jg      @@L8
        jl      @@L7
        cmp     [WORD low Diff], ax
        jnb     @@L8
@@L7:

; Diff is < 0

        mov     ax, [WORD low DeltaX]
        mov     dx, [WORD high DeltaX]
        add     [WORD low Diff], ax
        adc     [WORD high Diff], dx            ; Inc (Diff, DeltaX);

        add     ax, [WORD low QB2]
        adc     dx, [WORD high QB2]
        mov     [WORD low DeltaX], ax
        mov     [WORD high DeltaX], dx          ; Inc (DeltaX, QB2);

        inc     [X]

; And the rest of the loop

@@L8:   mov     ax, [WORD low DeltaY]
        mov     dx, [WORD high DeltaY]
        add     [WORD low Diff], ax
        adc     [WORD high Diff], dx            ; Inc (Diff, DeltaY)

        add     ax, [WORD low QA2]
        adc     dx, [WORD high QA2]
        mov     [WORD low DeltaY], ax
        mov     [WORD high DeltaY], dx          ; Inc (DeltaY, QA2);

        dec     [Y]

; Set point and repeat

        mov     cx, [X]
        mov     bx, [Y]
        call    [PlotVector]
        jmp     @@L6

; End

@@L9:   mov     sp, bp
        ret

ENDP    Ellipse


; ---------------------------------------------------------------------------
; Main program for ellipse. Called via the dispatcher.

PROC    Arc     NEAR

; Wait until the GE is ready

        call    [GE_Ready]              ; Wait until GE is ready

; Check if it is a 360° ellipse

        test    ax, ax                  ; Start angle = 0?
        jnz     @@L2                    ; No
        cmp     bx, 360                 ; End angle = 360?
        jnz     @@L2                    ; No

; It is a 360 degree ellipse. Check line thickness and
; assign appropriate drawing vector

        mov     [PlotVector], OFFSET EPlot4Thin
        cmp     [LineWidth], 3          ; Thick lines?
        jnz     @@L1                    ; No, thin
        mov     [PlotVector], OFFSET EPlot4Thick

@@L1:   jmp     Ellipse                 ; No --> Ellipse

; It is not a 360° ellipse. Call Emulate

@@L2:   call    Emulate                 ; Emulate ellipse
        ret

ENDP    Arc


;     ==================================================================
;
;         DW      PIESLICE        ; Draw an elliptical pie slice
;
;   Input:
;      AX         The starting angle of the slice in degrees (0-360)
;      BX         The ending angle of the slice in degrees (0-360)
;      CX         X radius of the elliptical slice
;      DX         Y radius of the elliptical slice
;
;   Return:
;      Nothing
;
; PIESLICE draws a filled elliptical pie slice (or wedge) using CP as the
; center of the slice, from the given start angle to the given end angle.
; The current FILLPATTERN and FILLCOLOR is used to fill the slice and it is
; outlined in the current COLOR. To get circular pie slices, the application
; (not the driver) must adjust the Y radius as follows:
;
;     YRAD := XRAD * ASPEC / 10000
;
; where ASPEC is the aspect value stored in the driver's DST.
;
;

PROC    PieSlice Near

; Wait until the GE is ready

        call    [GE_Ready]              ; Wait until GE is ready

; Check if it is a 360° ellipse

        test    ax, ax                  ; Start angle = 0?
        jnz     @@L2                    ; No
        cmp     bx, 360                 ; End angle = 360?
        jnz     @@L2                    ; No

; Assign a suitable value to the plot routine of Ellipse

        mov     [PlotVector], OFFSET FullEllipsePlot

; Call Ellipse procedure

        jmp     Ellipse

; It is not a 360° ellipse. Call Emulate

@@L2:   call    Emulate                 ; Emulate ellipse
        ret

ENDP    PieSlice

;     ==================================================================
;
;         DW      FILLED_ELLIPSE  ; Draw a filled ellipse at (CP)
;
;   Input:
;      AX         X Radius of the ellipse
;      BX         Y Radius of the ellipse
;
;   Return:
;      Nothing
;
; This vector is used to draw a filled ellipse. The center point of the
; ellipse is assumed to be at the current pointer (CP). The AX Register
; contains the X Radius of the ellipse, and the BX Register contains the Y
; Radius of the ellipse.
;
;

; -------------------------------------------------------------------------
; Draw a horizontal line in the current fill pattern without clipping
;
; Subroutine for FullEllipsePlot and FillPoly. Called with ax = Y,
; bx = X1, cx = X2, es = VideoSegment.
;
; ax, bx, cx, dx, si, di may be used freely
; The values that must be preserved: es, ds, bp, ss, cs

PROC    Generic_HorLine Near

        cld
        mov     si, ax                  ; Y value for pattern
        and     si, 7                   ; mod 8

; Calculate address

        CalcAdr                         ; Calculate address
        xchg    di, ax                  ; Offset to di

; Get color

        mov     al, [FillColor]

; Get the appropriate pattern into ah

        mov     ah, [FillPattern + si]

; Test if the pattern is 0FFh (all colored) or 00h (all background).
; If so --> special handling with rep stosb

        cmp     ah, 0FFh                ; All colored?
        jz      EPL_Solid
        test    ah, ah                  ; All background?
        jnz     EPL_Pattern             ; No, pattern
        xor     al, al                  ; Get background color

; The line is drawn in one color. Here, rep stosb is much faster.
; Move count to cx and check if a segment overflow occurs within the line.

EPL_Solid:
        mov     ah, al                  ; Color in ah and al
        sub     cx, bx
        inc     cx
        jz      @@L9                    ; Length is 0
        mov     bx, di
        add     bx, cx
        jnc     @@L2                    ; No overflow

; An overflow occurs. Draw the line in two parts

        mov     bx, di
        neg     bx                      ; Count until overflow
        sub     cx, bx                  ; In cx the rest after
        xchg    bx, cx

; Draw first part

		RepStoS

; Set next segment

        inc     [Seg64]                 ; Next segment...
        call    [SegSelect]             ; ...set
        mov     cx, bx                  ; Remaining count

; If the remaining count is 0, nothing happens here, because rep is then
; executed 0 times (unlike below, where loop would execute 65536 times ...
; how we love the world of compatibles).

; Draw the second (or only) part of the line

@@L2:   RepStoS

; Done!

        ret

; Filled with a pattern. The pattern is in ah and must be adjusted for the
; starting coordinate.

EPL_Pattern:
        xchg    bx, cx                  ; cx = X1, bx = X2
        rol     ah, cl                  ; Initial value (rol is always mod xx)
        xchg    bx, cx                  ; cx = X2, bx = X1

; Move count to cx and check if a segment overflow occurs within the line.

        sub     cx, bx
        inc     cx
        mov     bx, di
        add     bx, cx
        jnc     @@L6                    ; No overflow

; An overflow occurs. Draw the line in two parts

        mov     bx, di
        neg     bx                      ; Count until overflow
        sub     cx, bx                  ; In cx the rest after
        xchg    bx, cx

EVEN
@@L3:   rol     ah, 1
        jnc     @@L4
        stosb                           ; Set pixel in color
        LOOPCX  @@L3
        jmp     short @@L5

EVEN
@@L4:   mov     [Byte es:di], 0         ; Fill pixel with background color
        inc     di
        LOOPCX  @@L3
@@L5:

        inc     [Seg64]                 ; Next segment...
        call    [SegSelect]             ; ...set
        mov     cx, bx                  ; Remaining count

; Draw the second (only) part of the line.

@@L6:   jcxz    @@L9                    ; Nothing to draw

EVEN
@@L7:   rol     ah, 1
        jnc     @@L8
        stosb                           ; Set pixel in color
        LOOPCX  @@L7
        jmp     short @@L9

EVEN
@@L8:   mov     [Byte es:di], 0         ; Fill pixel with background color
        inc     di
        LOOPCX  @@L7

; That's it

@@L9:   ret

ENDP    Generic_HorLine

; -------------------------------------------------------------------
; Subroutine for FilledEllipse to draw the ellipse.
;
; cx = x, bx = y, ds = cs, es = VideoSeg
; ds, es and bp must be preserved, everything else may be destroyed.
; [Seg64] is expected to contain the currently set segment - the variable
; must be initialized to 0ffh before start.
;

PROC    FullEllipsePlot NEAR
Local   Y:WORD, X1: WORD, X2: WORD = LocalSize

        EnterProc       LocalSize

; Initialize horizontal line

        call    [HorLineInit]

; Remember the passed Y value

        mov     [Y], bx

; Calculate X1 and X2 and correct as needed

        mov     ax, cx
        neg     cx
        add     cx, [CursorX]           ; cx = X1
        add     ax, [CursorX]           ; ax = X2

        mov     dx, [Clip_X1]           ; Left corner into register
        cmp     ax, dx                  ; Right corner left outside?
        jl      @@L9                    ; Yes --> draw nothing
        cmp     cx, dx                  ; Left corner left outside?
        jge     @@L1                    ; No
        mov     cx, dx                  ; Yes --> correct X1 to left corner
@@L1:   mov     dx, [Clip_X2]           ; Right corner into register
        cmp     cx, dx                  ; Left corner right outside?
        jg      @@L9                    ; Yes --> nothing to draw
        cmp     ax, dx                  ; Right corner right outside?
        jle     @@L2                    ; No
        mov     ax, dx                  ; Yes --> correct X2 to right corner
@@L2:

; Store values

        mov     [X1], cx
        mov     [X2], ax

; Calculate Y value of the upper line and check if it is in the window.

        neg     bx
        add     bx, [CursorY]           ; bx = Y0 - Y
        cmp     bx, [Clip_Y2]           ; Below the window?
        jg      @@L9                    ; Yes --> draw nothing
        cmp     bx, [Clip_Y1]           ; In the window?
        jl      @@L5                    ; No --> no line

; The upper of the two lines is in the window, draw

        xchg    bx, ax                  ; ax = Y, bx = X2
        xchg    bx, cx                  ; bx = X1, cx = X2
        call    [HorLine]

; Calculate Y value of the lower line and check if it is in the window

@@L5:   mov     ax, [Y]
        add     ax, [CursorY]
        cmp     ax, [Clip_Y2]           ; Below the window?
        jg      @@L9                    ; Yes --> no line
        cmp     ax, [Clip_Y1]           ; Above the window?
        jl      @@L9                    ; Yes --> no line

; Draw the lower line

        mov     bx, [X1]
        mov     cx, [X2]
        call    [HorLine]

; Done!

@@L9:   LeaveProc
        ret

ENDP    FullEllipsePlot


; ----------------------------------------------------------------------
;
; Subroutine to draw a filled ellipse. Ellipse is used with an appropriate
; plot vector.
;

PROC    FilledEllipse  NEAR

; Wait until the GE is ready

        call    [GE_Ready]

; Store to cx:dx, as DoEllipse expects the radii there

        xchg    cx, ax
        xchg    dx, bx

; Assign a suitable value to the plot routine of Ellipse

        mov     [PlotVector], OFFSET FullEllipsePlot

; Call Ellipse procedure

        jmp     Ellipse

ENDP    FilledEllipse


; -----------------------------------------------------------------

IF      Ver3
ENDS   Code

; -----------------------------------------------------------------

SEGMENT DATA PARA PUBLIC 'DATA'
ENDIF

;---------------------------------------------------
; Protected-Mode Variable

IF Ver3
; The following structure _must_ be at the very beginning of the data segment!

                db      4               ; Number of segments
                dw      OFFSET Segs
                db      1               ; One int vector is needed
                dw      OFFSET Ints
                db      1               ; Low-mem segments
                dw      LowSegs
                db      0               ; Data not in low-mem
                dw      0
ENDIF
ProtMode        db      0               ; Protected mode if != 0


LABEL           Segs
SegB800         dw      0B800h
VideoSeg        dw      0A000h
SegC000         dw      0C000h
SegF000         dw      0F000h

IF Ver3
LABEL           Ints
Int1F           dd      1Fh

LABEL           LowSegs
                dw      16              ; Size: 16 paras (256 bytes)
LowBufSeg       dw      0               ; Real-mode segment
LowBufSel       dw      0               ; Protected mode selector
ELSE

; Static buffer in low memory for versions before 3.0
LowBuf          db      256 dup (?)

ENDIF

;---------------------------------------------------
; Static struct for calling a simulated real-mode INT

IF Ver3
RMRegs          RealModeRegs <>
ENDIF

;---------------------------------------------------
; Vector table
; Note on the table: According to c't 11/89, 9/90 and 2/91, vector
; 14 (0Eh) contains an entry point for the scan converter of FillPoly. This
; vector is not entered, however, because the graphics kernel of BP 7.0
; checks this vector and if it no longer points to Emulate, it sets a flag,
; which causes Arc and PieSlice with small radii to no longer work
; correctly. So the vector here is set to Emulate and is later (at Init)
; set to the offset of FillPoly.
;

LABEL   Vector_Table  WORD

        dw      Install
        dw      Init
        dw      Clear
        dw      Post
        dw      Move
        dw      Draw
        dw      Vect
        dw      Emulate                 ; FillPoly, see above
        dw      Emulate                 ; Bar = Emulate
        dw      PatBar
        dw      Arc
        dw      Emulate                 ; Pieslice = Emulate
        dw      FilledEllipse
        dw      Palette
        dw      AllPalette
        dw      Color
        dw      FillStyle
        dw      LineStyle
        dw      TextStyle
        dw      Text
        dw      TextSize
        dw      NOP_Vector              ; Reserved
        dw      FloodFill
        dw      GetPixel
        dw      PutPixel
        dw      BitMapUtil
        dw      SaveBitMap
        dw      RestoreBitMap
        dw      SetClip
        dw      ColorQuery

        dw      35 dup (NOP_Vector)      ; Reserved for Borland use

; -------------------------------------------------------
; Here come vectors that are used internally.

SegSelect       dw      NOP_Vector      ; Vector for segment switching
PlotVector      dw      EPlot4Thin      ; Drawing vector for DoEllipse
HorLine         dw      Generic_HorLine ; Horizontal line in current fill pattern
HorLineInit     dw      NOP_Vector      ; Init for HorLine
GE_Ready        dw      NOP_Vector      ; Waits until the hardware engine is ready

; ------------------------------------------------------------
; BitMap utility table

BitMapUtilTable:
        dw      FAR_NOP_Vector          ; GotoGraphic not implemented
        dw      FAR_NOP_Vector          ; ExitGraphic not implemented
        dw      FAR_NOP_Vector          ; PutPixel not implemented
        dw      FAR_NOP_Vector          ; GetPixel not implemented
        dw      GetPixByte
        dw      SetDrawPage             ; SetDrawPage
        dw      SetVisualPage           ; SetVisualPage
        dw      SetWriteMode

;---------------------------------------------------
; Flags for options. See the corresponding constants in const.asi

Options         dw      0
OptText         db      9, "SVGAOPTS="

;---------------------------------------------------
; Size of the screen in bytes as LongInt (for clearing)

ScreenBytes     dd      0

;---------------------------------------------------
; X and Y resolution of the currently active mode and Y offset of the
; current page.
; BytesPerLine is the number of bytes per scanline. It normally
; corresponds to MaxX (BytesPerLine is new with version 3.51, previously
; MaxX was used), but can be different (larger) for VESA drivers.

MaxX            dw      0
MaxY            dw      0
BytesPerLine    dw      0
PageOfs         dw      0

;---------------------------------------------------
; Clipping window

Clip_X1         dw      0
Clip_Y1         dw      0
Clip_X2         dw      0
Clip_Y2         dw      0

;---------------------------------------------------
; Here are the colors: background color, drawing color, and fill color
;

BkColor         db      00h             ; Goes to palette register 0
DrawingColor    db      0Fh             ; Drawing color
FillColor       db      0Fh             ; Fill color

;---------------------------------------------------
; Variable for FloodFill.

BorderColor     db      ?               ; Border color
EVEN
StackBot        =       512             ; Lower part of the stack
StackTop        dw      ?               ; Upper end
StackPtr        dw      ?               ; Pointer in stack
PrevXR          dw      ?
CurrXR          dw      ?
FillDir         dw      ?

;---------------------------------------------------
; Byte in which the segment is written. The SegSelect routines use this byte.

Seg64           db      0FFh            ; -1 = Not set

;---------------------------------------------------
; The default color table contains 16 entries:

ColorTable      db      10h     ; 16 entries follow
                db      00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h
                db      08h, 09h, 0Ah, 0Bh, 0Ch, 0Dh, 0Eh, 0Fh

;---------------------------------------------------
; WriteMode for line drawing, 01 = XORMode, 00 = Normal

WriteMode       db      0

;---------------------------------------------------
; X and Y coordinate of the "Current Drawing Pointer"

EVEN

CursorX         dw      0
CursorY         dw      0

;---------------------------------------------------
; Various information for the hardware font

TextSizeX       dw      8               ; Pixels
TextSizeY       dw      8               ; Pixels
TextNum         db      00h             ; Number is always 0
TextOrient      db      00h             ; 0 = horiz., 1 = vert.
TextMultX       dw      01h             ; Text X-multiplier
TextMultY       dw      01h             ; Text Y-multiplier

;---------------------------------------------------
; LinePattern (pattern for set LineStyle) and line width

LinePattern     dw      0FFFFh          ; SolidLn
LineWidth       dw      1

;---------------------------------------------------
; Table of bit patterns for the linestyles.

LineStyles      dw      0FFFFh          ; SolidLn
                dw      0CCCCh          ; DottedLn
                dw      0FC78h          ; CenterLn
                dw      0F8F8h          ; DashedLn

;---------------------------------------------------
; Here is the number of the currently active fill pattern (0-12), where
; 2-11 are in the Fillpatterns table, the user pattern is coded with 12
; instead of 0FFh) and copied behind the table.

FillPatternNum  db      SolidFill

; The current fill pattern is copied here for faster access

FillPattern     db      8 dup (?)

;---------------------------------------------------
; FillPatterns come here

LABEL   FillPatternTable        BYTE
        db      000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h  ;Empty Fill
        db      0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh  ;Solid Fill
        db      0FFh, 0FFh, 000h, 000h, 0FFh, 0FFh, 000h, 000h  ;Line Fill
        db      001h, 002h, 004h, 008h, 010h, 020h, 040h, 080h  ;Lt Slash Fill
        db      0E0h, 0C1h, 083h, 007h, 00Eh, 01Ch, 038h, 070h  ;Slash Fill
        db      0F0h, 078h, 03Ch, 01Eh, 00Fh, 087h, 0C3h, 0E1h  ;Backslash Fill
        db      0A5h, 0D2h, 069h, 0B4h, 05Ah, 02Dh, 096h, 04Bh  ;Lt Bkslash Fill
        db      0FFh, 088h, 088h, 088h, 0FFh, 088h, 088h, 088h  ;Hatch Fill
        db      081h, 042h, 024h, 018h, 018h, 024h, 042h, 081h  ;XHatch Fill
        db      0CCh, 033h, 0CCh, 033h, 0CCh, 033h, 0CCh, 033h  ;Interleave Fill
        db      080h, 000h, 008h, 000h, 080h, 000h, 008h, 000h  ;Wide Dot Fill
        db      088h, 000h, 022h, 000h, 088h, 000h, 022h, 000h  ;Close Dot Fill

; The last entry is reserved for the user pattern, which is copied here.

UserPattern     db      8 dup (0)

;---------------------------------------------------
; The pixel representation of the output character (hardware text)

CharShape       db      8 dup (0)

;---------------------------------------------------
; Variables for ATI Wonder (XL):
; ATI identification string, found at C000:0031,
; a table of ATI mode numbers with the corresponding bitmasks
; for autodetect and the address of the extended register of the ATI Wonder
;

ATI_Ident       db      "761295520"
ATI_Modes       db      61h, M640x400
                db      62h, M640x480
                db      63h, M800x600
                db      64h, M1024x768
                db      00h                     ; End marker
Extended_Reg    dw      1CEh

;---------------------------------------------------
; Identification string of Paradise VGAs

Paradise_Ident  db      "VGA="

;---------------------------------------------------
; Variables for accessing VESA functions
;

VESA_Granularity        db      1       ; Granularity of the window
EVEN
VESA_Window             dw      0       ; Used window
VESA_WinFunc            dd      0       ; Pointer to segment switch routine


;---------------------------------------------------
; The names of the various modes. The names are accessed via a table
; with pointers to the strings, distinguishing between normal and
; autodetect modes.

VGA256Name      db      15, "320x200x256 VGA", 0
VGA350Name      db      16, "640x350x256 SVGA", 0
VGA400Name      db      16, "640x400x256 SVGA", 0
VGA480Name      db      16, "640x480x256 SVGA", 0
VGA540Name      db      16, "720x540x256 SVGA", 0
VGA600Name      db      16, "800x600x256 SVGA", 0
VGA768Name      db      17, "1024x768x256 SVGA", 0
VGA1024Name     db      18, "1280x1024x256 SVGA", 0
VGAXXXName      db      15, "Autodetect SVGA", 00


; -------------------------------------------------------------
; Tables for autodetect
; A table with pointers to the names

LABEL   AutoName WORD
        dw      VGAXXXName      ; Mode 1
        dw      VGA400Name      ; Mode 2
        dw      VGA480Name      ; Mode 3
        dw      VGA600Name      ; Mode 4
        dw      VGA768Name      ; Mode 5
        dw      VGA1024Name     ; Mode 6

; Table with the mode bits for the autodetect modes

LABEL   AutoMode BYTE
        db      MAll            ; Mode 1
        db      M640x400        ; Mode 2
        db      M640x480        ; Mode 3
        db      M800x600        ; Mode 4
        db      M1024x768       ; Mode 5
        db      M1280x1024      ; Mode 6

; --------------------------------------------------------------
; The segment-select routines for the various cards.
; The table is indexed directly with the card number.

LABEL SegSwitchTable    WORD
        dw      Nop_Vector              ; standard VGA
        dw      ET3000_SegSwitch        ; ET3000 chipset
        dw      ET4000_SegSwitch        ; ET4000 chipset
        dw      Trident_SegSwitch       ; Trident 8900 chipset
        dw      V7_SegSwitch            ; Video7 1024i or VEGA VGA
        dw      Par_SegSwitch           ; Paradise VGA
        dw      ATI_SegSwitch           ; ATI VGAWonder
        dw      Evx_SegSwitch           ; Everex
        dw      Oak_SegSwitch           ; Oak
        dw      S3_SegSwitch            ; S3 chipset
        dw      VESA_SegSwitch2         ; VGA supports VESA standard

; --------------------------------------------------------------
; SetDrawPage for the various cards.
; The table is indexed directly with the card number.

LABEL   SetDrawPageTable        WORD
        dw      Nop_Vector              ; standard VGA
        dw      Generic_DrawPage        ; ET3000 chipset
        dw      Generic_DrawPage        ; ET4000 chipset
        dw      Generic_DrawPage        ; Trident 8900 chipset
        dw      Generic_DrawPage        ; Video7 1024i or VEGA VGA
        dw      Generic_DrawPage        ; Paradise VGA
        dw      Nop_Vector              ; ATI VGAWonder
        dw      Nop_Vector              ; Everex
        dw      Nop_Vector              ; Oak
        dw      S3_DrawPage             ; S3 chipset
        dw      Generic_DrawPage        ; VGA supports VESA standard

; --------------------------------------------------------------
; SetVisualPage for the various cards.
; The table is indexed directly with the card number.

LABEL   SetVisualPageTable      WORD
        dw      Nop_Vector              ; standard VGA
        dw      ET3000_VisualPage       ; ET3000 chipset
        dw      ET4000_VisualPage       ; ET4000 chipset
        dw      Trident_VisualPage      ; Trident 8900 chipset
        dw      V7_VisualPage           ; Video7 1024i or VEGA VGA
        dw      Par_VisualPage          ; Paradise VGA
        dw      Nop_Vector              ; ATI VGAWonder
        dw      Nop_Vector              ; Everex
        dw      Nop_Vector              ; Oak
        dw      S3_VisualPage           ; S3 chipset
        dw      VESA_VisualPage         ; VGA supports VESA standard

; ------------------------------------------------------------
;
; Tables with the values of the modes. For space reasons and because it
; is advantageous to have exactly *one* DST, the values are not stored
; in extended DSTs as before.
;

LABEL   ModeTable       TMode

; VGA 320x200x256
TMode   <320, 200, VGA256Name, 13h, 0,          \
         GenericVGA, M320x200, NOP_Vector>

; Tseng ET3000, 640x350x256
TMode  <640, 350, VGA350Name, 2Dh, 0,           \
        ET3000VGA, M640x350, NOP_Vector>

; Tseng ET3000, 640x480x256
TMode  <640, 480, VGA480Name, 2Eh, 0,           \
        ET3000VGA, M640x480, NOP_Vector>

; Tseng ET3000, 800x600x256
TMode  <800, 600, VGA600Name, 30h, 0,           \
        ET3000VGA, M800x600, NOP_Vector>

; Tseng ET4000, 640x350x256
TMode  <640, 350, VGA350Name, 2Dh, 0,           \
        ET4000VGA, M640x350, ET4000_GraphOn>

; TSeng ET4000 640x400x256
TMode  <640, 400, VGA400Name, 2Fh, 0,           \
        ET4000VGA, M640x400, ET4000_GraphOn>

; Tseng ET4000, 640x480x256
TMode  <640, 480, VGA480Name, 2Eh, 0,           \
        ET4000VGA, M640x480, ET4000_GraphOn>

; Tseng ET4000, 800x600x256
TMode  <800, 600, VGA600Name, 30h, 0,           \
        ET4000VGA, M800x600, ET4000_GraphOn>

; Tseng ET4000, 1024x768x256
TMode  <1024, 768, VGA768Name, 38h,0,           \
        ET4000VGA, M1024x768, ET4000_GraphOn>

; Trident 640x400x256
TMode  <640, 400, VGA400Name, 5Ch, 0,           \
        TridentVGA, M640x400, NOP_Vector>

; Trident 640x480x256
TMode  <640, 480, VGA480Name, 5Dh, 0,           \
        TridentVGA, M640x480, NOP_Vector>

; Trident 800x600x256
TMode  <800, 600, VGA600Name, 5Eh, 0,           \
        TridentVGA, M800x600, NOP_Vector>

; Trident 1024x768x256
TMode  <1024, 768, VGA768Name, 62h, 0,          \
        TridentVGA, M1024x768, NOP_Vector>

; Video7 640x400x256
TMode  <640, 400, VGA400Name, 6F05h, 66h,       \
        Video7VGA, M640x400, NOP_Vector>

; Video7 640x480x256
TMode  <640, 480, VGA480Name, 6F05h, 67h,       \
        Video7VGA, M640x480, NOP_Vector>

; Video7 800x600x256
TMode  <800, 600, VGA600Name, 6F05h, 69h,       \
        Video7VGA, M800x600, NOP_Vector>

; ATI VGA-Wonder 640x400x256
TMode  <640, 400, VGA400Name, 61h, 0,           \
        ATIVGA, M640x400, ATI_GraphOn>

; ATI VGA-Wonder 640x480x256
TMode  <640, 480, VGA480Name, 62h, 0,           \
        ATIVGA, M640x480, ATI_GraphOn>

; ATI VGA-Wonder 800x600x256
TMode  <800, 600, VGA600Name, 63h, 0,           \
        ATIVGA, M800x600, ATI_GraphOn>

; ATI VGA-Wonder 1024x768x256
TMode  <1024, 768, VGA1024Name, 64h, 0,          \
        ATIVGA, M1024x768, ATI_GraphOn>

; Paradise 640x400x256
TMode  <640, 400, VGA400Name, 5Eh, 0,           \
        ParadiseVGA, M640x400, NOP_Vector>

; Paradise 640x480x256
TMode  <640, 480, VGA480Name, 5Fh, 0,           \
        ParadiseVGA, M640x480, NOP_Vector>

; Paradise 800x600x256
TMode  <800, 600, VGA600Name, 5Ch, 0,           \
        ParadiseVGA, M800x600, NOP_Vector>

; Everex 640x350x256
TMode  <640, 350, VGA350Name, 70h, 13h,         \
        EverexVGA, M640x350, NOP_Vector>

; Everex 640x400x256
TMode  <640, 400, VGA400Name, 70h, 14h,         \
        EverexVGA, M640x400, NOP_Vector>

; Everex 640x480x256
TMode  <640, 480, VGA480Name, 70h, 30h,         \
        EverexVGA, M640x480, NOP_Vector>

; Everex 800x600x256
TMode  <800, 600, VGA600Name, 70h, 31h,         \
        EverexVGA, M800x600, NOP_Vector>

; Oak 640x400x256
TMode  <640, 400, VGA400Name, 53h, 00h,         \
        OakVGA, M640x400, NOP_Vector>

; Oak 800x600x256
TMode  <800, 600, VGA600Name, 54h, 00h,         \
        OakVGA, M800x600, NOP_Vector>

; S3 640x480x256
TMode  <640, 480, VGA480Name, 69h, 0,           \
        S3VGA, M640x480, S3_GraphOn>

; S3 800x600x256
TMode  <800, 600, VGA600Name, 6Bh, 0,           \
        S3VGA, M800x600, S3_GraphOn>

; S3 1024x768x256
TMode  <1024, 768, VGA768Name, 6Dh, 0h,         \
        S3VGA, M1024x768, S3_GraphOn>

; S3 1280x1024x256
TMode  <1280, 1024, VGA1024Name, 72h, 0h,       \
        S3VGA, M1280x1024, S3_GraphOn>

; VESA 640x400x256
TMode  <640, 400, VGA400Name, 4F02h, 100h,      \
        VESAVGA, M640x400, VESA_GraphOn>

; VESA 640x480x256
TMode  <640, 480, VGA480Name, 4F02h, 101h,      \
        VESAVGA, M640x480, VESA_GraphOn>

; VESA 800x600x256
TMode  <800, 600, VGA600Name, 4F02h, 103h,      \
        VESAVGA, M800x600, VESA_GraphOn>

; VESA 1024x768x256
TMode  <1024, 768, VGA768Name, 4F02h, 105h,     \
        VESAVGA, M1024x768, VESA_GraphOn>

; VESA 1280x1024x256
TMode  <1280, 1024, VGA1024Name, 4F02h, 107h,   \
        VESAVGA, M1280x1024, VESA_GraphOn>

Label   ModeTableEnd    TMode

; Another pointer to the current entry

ModePtr         dw      ModeTable               ; VGA 320x200

; -----------------------------------------------
; The status record that is passed to TP.

DST     Status <>

IF      Ver3
ENDS    Data
ELSE
ENDS    Code
ENDIF

END Start



; ------------------------------------------------------------------------------


