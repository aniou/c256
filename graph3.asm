.cpu "65816"

; Copyright 2019 Piotr Meyer <aniou@smutek.pl>
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted, provided that the above 
; copyright notice and this permission notice appear in all copies.

; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES 
; WITH REGARD TO THIS SOFTWARE  INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR 
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN 
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


.include "macros_inc.asm"
.include "page_00_inc.asm"
.include "vicky_def.asm"

CLRSCREEN         = $10a8
IINITVKYGRPMODE   = $10d0
IINITALLLUT       = $10c8

GRAPH_LINES = 480
GRAPH_COLS  = 640
GRAPH_MEM   = $b0c000

;======================================================================
; MAIN DEMO PROGRAM
;======================================================================

                  .virtual $00e0
scr0              .block
color             .byte ?
control           .word ?
graph_ptr         .long ?
graph_ptr_free    .byte ? ; nothing, just placeholder to two words
xpos              .word ?
xpos_fr           .word ?
ypos              .word ?
ypos_fr           .word ?
xorder            .word ?
yorder            .word ?
xdir              .word ? ; totalne naduzycie, bo potrzebuje jeden bit
ydir              .word ? ; totalne jak wyzej 0 - dodatni, 1 - ujemny
                  .bend
                  .endv


; doesn't work, doesn't work!

                  * = $030000

                  clc                                             ; switch to native
                  xce                                             ; 

                  phd
                  php
                  setaxl                                          ; set 16bits
                  pha
                  phx


                  jsl IINITALLLUT
                  jsl IINITVKYGRPMODE
                  jsl LINES_INIT

                  setas

                  lda #$01
                  sta BM_CONTROL_REG

                  ;lda #$03|$08
                  lda #$04|$08
                  sta MASTER_CTRL_REG_L

                  LDA #$00
                  STA BORDER_CTRL_REG


                  lda #$00
                  sta BACKGROUND_COLOR_B
                  sta BACKGROUND_COLOR_R
                  sta BACKGROUND_COLOR_G

                  lda #2
                  sta scr0.color

                  setaxl
                  lda @w #$0001
                  sta scr0.xorder
                  lda @w #$0001
                  sta scr0.yorder
                  lda @w #100
                  sta scr0.xpos
                  lda @w #$0001
                  sta scr0.ypos
                  sta scr0.control

                  sta scr0.xpos_fr
                  sta scr0.ypos_fr


                  lda @w #$0000
                  sta scr0.xdir
                  sta scr0.ydir

                  jsr CLEAR_SCREEN

loop              lda scr0.ypos
                  asl a
                  asl a
                  tax

                  lda line_offset, x
                  sta scr0.graph_ptr
                  lda line_offset+2, x
                  sta scr0.graph_ptr+2

                  ldy scr0.xpos

                  ; now cursor is at poxytion xpos,ypos

                  setas
                  lda scr0.color
                  sta [scr0.graph_ptr], y

                  ; move in x direction
                  setal


                  ; fraction of X
move_x            lda scr0.xpos_fr
                  adc scr0.xorder
                  sta scr0.xpos_fr
                  cmp #10
                  bcc move_y
                  lda @w #0
                  sta scr0.xpos_fr

                  lda scr0.xdir
                  beq x_inc
x_dec             lda scr0.xpos
                  dec a
                  sta scr0.xpos
                  bne move_y
                  lda @w #0
                  sta scr0.xdir   ; zmiana kierunku
                  bra move_y
x_inc             lda scr0.xpos
                  inc a
                  sta scr0.xpos
                  cmp #639
                  bne move_y
                  lda @w #1
                  sta scr0.xdir   ; zmiana kierunku
                  ; XXX - test
                  lda scr0.xorder
                  inc a
                  cmp #3
                  bcs move_y
                  sta scr0.xorder
                  ; XXX - koniec testu

                  ; fraction of X
move_y            lda scr0.ypos_fr
                  adc scr0.yorder
                  sta scr0.ypos_fr
                  cmp #10
                  bcc counter
                  lda @w #0
                  sta scr0.ypos_fr

                  lda scr0.ydir
                  beq y_inc
y_dec             lda scr0.ypos
                  dec a
                  sta scr0.ypos
                  bne counter
                  lda @w #0
                  sta scr0.ydir   ; zmiana kierunku
                  bra counter
y_inc             lda scr0.ypos
                  inc a
                  sta scr0.ypos
                  cmp #479
                  bne counter
                  lda @w #1
                  sta scr0.ydir   ; zmiana kierunku


counter           ldx @w #1000
delay             dex
                  bne delay

                  jmp loop

finish            nop
                  setal
                  plx
                  pla
                  plp
                  pld
endles_loop       wdm #3
                  jmp endles_loop



;=======================================================================
; CLEAR_SCREEN
; very rudimentary, change to MVP or VDMA 

CLEAR_SCREEN      setaxl
                  ldx @w #$0000

clear0            lda line_offset, x
                  sta scr0.graph_ptr
                  lda line_offset+2, x
                  sta scr0.graph_ptr+2
                  inx
                  inx
                  inx
                  inx
                  cpx #481 * 4
                  beq clear_finish
                  lda @w #$0000
                  ldy @w #$0000
clear1            sta [scr0.graph_ptr],y
                  iny
                  iny
                  cpy #640
                  bcc clear1
                  jmp clear0

clear_finish      rts



;=======================================================================
; LINES_INIT
; calculate absolute offsets for lines in graphics memory
; we use 4 bytes because it is easier to calculate (line_no * 4) than
; three bytes

LINES_INIT  setas
                  lda #`GRAPH_MEM
                  sta scr0.graph_ptr+2
                  lda #$00
                  sta scr0.graph_ptr+3

                  setaxl
                  lda #<>GRAPH_MEM
                  sta scr0.graph_ptr

                  ldx #$0000

linit0            lda scr0.graph_ptr
                  sta line_offset, x
                  inx
                  inx
                  lda scr0.graph_ptr+2
                  sta line_offset, x
                  inx
                  inx
                  cpx #GRAPH_LINES * 4            ; 480 * 4
                  beq linit_end

                  lda scr0.graph_ptr
                  clc
                  adc #GRAPH_COLS
                  sta scr0.graph_ptr
                  bcc linit0
                  inc scr0.graph_ptr+2
                  jmp linit0


linit_end   rtl


;=======================================================================

                  .align $10                                      ; waste of memory, but debugging is easier
line_offset .fill 480 * 4, $00                    ; not possible to determine at compile time
