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


;======================================================================
; algorithm
; 1. create  sequence of colors, from lightest (f) to darkest
; 2. fill screen memory with random characters
; 3. create "matrix" effect by manipulating color attributes
;
; todo
; - better random
; - better delay loop
; - ramdom changes for characters
; - longer color sequences

.include "macros_inc.asm" ; take them from https://github.com/Trinity-11/Kernel
.include "page_00_inc.asm"
.include "vicky_def.asm"

CLRSCREEN	= $1900a8
ENDLESS_LOOP	= $1904ed ; so lame, but works for testing...

RAND_SIZE	= 803

;======================================================================
; MAIN DEMO PROGRAM
;======================================================================

		* = $030000

		clc			; switch to native
		xce			;

		phd
		php
		setaxl			; set 16bits
		pha
		phx

		jsl ICLRSCREEN2
		jsl TBOX_INIT		; init line offsets
		jsl INIT_LUT
		jsl MATRIX_PRINT

		setal
		plx
		pla
		plp
		pld
		jmp ENDLESS_LOOP


;======================================================================
; ICLRSCREEN2 (additional utility routine)

ICLRSCREEN2	PHD
		PHP
		setaxl			; set 16bits
		PHA
		PHX
		setas
		LDX #$0000		; Only Use One Pointer
		LDA #$20		; Fill the Entire Screen with Space
iclearprint_text	 STA CS_TEXT_MEM_PTR, x  ;
		INX
		CPX #$2000
		BNE iclearprint_text
		; Now Set the Colors so we can see the text
		LDX #$0000		; Only Use One Pointer
		LDA #$11		; Fill the Color Memory with Foreground: 75% Purple, Background 12.5% White
		;LDA #$ED		; Fill the Color Memory with Foreground: 75% Purple, Background 12.5% White
iclearloop1	 STA CS_COLOR_MEM_PTR, x ;
		INX
		CPX #$2000
		BNE iclearloop1
		setal
		PLX
		PLA
		PLP
		PLD
		RTL

; end of ICLRSCREEN2
;======================================================================


		.virtual $00e0
matrix		.block
counter		.word ? ;
rnd_pos		.word ?	; for displaying characters
rnd2_pos	.word ?	; for new rain chance 
row_current     .word ? ;
delay_counter	.word ? ;
		.bend
		.endv

;=======================================================================
; MATRIX_PRINT


MATRIX_PRINT	nop
		setal
		lda #RAND_SIZE-1
		sta matrix.rnd_pos
		sta matrix.rnd2_pos

		lda #2048		; how many iterations?
		sta matrix.counter

		lda LINES_VISIBLE
		dec a			; assume that LINES_VISIBLE > 0
		sta matrix.row_current


		setaxl
print_text	lda matrix.row_current
		asl a			; because we have array of words
		tax

		lda line_offset, x
		clc
		adc COLS_VISIBLE
		tax


		setas
		ldy COLS_VISIBLE
put_char	nop
		jsr GET_RAND
		sta CS_TEXT_MEM_PTR, x
		dex
		dey
		bpl put_char

next_line	nop
		setaxl
		lda matrix.row_current
		dec a
		sta matrix.row_current
		bpl print_text

; ---------------------------------------------------------------------
; first line

text_done	nop
		setas
		setxl
		ldx COLS_VISIBLE
line0		jsr GET_RAND2
		cmp #3			; 2% chance 
		bcs line1
		lda #$f1
		sta CS_COLOR_MEM_PTR, x
line1		dex
		bpl line0


; ---------------------------------------------------------------------
; exit procedure

		setal
matrix_preloop	lda LINES_VISIBLE
		dec a			; assume that LINES_VISIBLE >= 2
		dec a			; assume that LINES_VISIBLE >= 2
		sta matrix.row_current

matrix_loop	lda matrix.row_current
		asl a			; because we have array of words
		tax

		lda line_offset, x
		clc
		adc COLS_VISIBLE
		tax


		setas
		ldy COLS_VISIBLE
move0		nop
		lda CS_COLOR_MEM_PTR, x
		and #$f0
		;cmp #$10
		bne move2
		ora #$11
		sta CS_COLOR_MEM_PTR+$80, x
		jmp move1

move2		ora #$01
		sta CS_COLOR_MEM_PTR+$80, x
		lsr a
		lsr a
		lsr a
		lsr a
		dec a
		asl a
		asl a
		asl a
		asl a
		ora #$01
		sta CS_COLOR_MEM_PTR, x
move1		dex
		dey
		bpl move0

		setaxl
		lda matrix.row_current
		dec a
		sta matrix.row_current
		bpl matrix_loop

		lda #$0000
		sta matrix.delay_counter
delay_loop	inc matrix.delay_counter
		lda matrix.delay_counter
		cmp #$8000
		bcc delay_loop

;		ldx #$ffff
;delay0		dex
;		bne delay0
;
;		ldx #$ffff
;delay1		dex
;		bne delay1
;
;		ldx #$ffff
;delay2		dex
;		bne delay2
;
;		ldx #$ffff
;delay3		dex
;		bne delay3
;
;		ldx #$ffff
;delay4		dex
;		bne delay4

		dec matrix.counter
		beq finish
		jmp text_done

finish		rtl


;=======================================================================
; GET_RAND
; it will be a random function. in future. maybe.
; TODO - use DBR

GET_RAND	phx
		ldx matrix.rnd_pos
		lda rnd0,x
		dex
		bpl get_rand_out
		ldx #RAND_SIZE-1
get_rand_out	stx matrix.rnd_pos
		plx
		rts

GET_RAND2	phx
		ldx matrix.rnd2_pos
		lda rnd1,x
		dex
		bpl get_rand2_out
		ldx #RAND_SIZE-1
get_rand2_out	stx matrix.rnd2_pos
		plx
		rts


;=======================================================================
; TBOX_INIT
; calculate offset table for LINES_MAX lines (usually 64) but no more
; than 255. each offset is a word, later used as value for Y register
;
; calling syntax:
;     jsl TBOX_INIT
;
; exit registers:
;     A - garbage
;     X - garbage
;     Y - unchanged
;
; exit flags:
;     m = 0 (16)
;     x = 0 (16)
;
;
; we count from LINES_MAX*2 (because we create word index)

TBOX_INIT	setaxl
		lda LINES_MAX
		asl a			; *2 because X indexes a words
		tax

		lda LINES_MAX
		asl a
		asl a
		asl a
		asl a
		asl a
		asl a
		asl a			; in A: LINES_MAX*$80 

tinit0		sec
		sbc COLS_PER_LINE	; default 128 bytes
		dex
		dex
		sta line_offset,x
		bne tinit0
		rtl

;=======================================================================
; INIT_LUT
; init color table - from darkest to lightest

INIT_LUT	nop
		setas
		setxl
		ldx #0
init_fg_lut	lda lut_fg_tbl, x
		sta FG_CHAR_LUT_PTR, x
		sta BG_CHAR_LUT_PTR, x
		inx
		cpx #$40
		bne init_fg_lut
		rtl

; ---------------------------------------------------------------------

lut_fg_tbl	.byte $00, $1a, $00, $00
		.byte $00, $1a, $00, $00
		.byte $00, $33, $00, $00
		.byte $00, $33, $00, $00
		.byte $00, $50, $00, $00
		.byte $00, $61, $00, $00
		.byte $00, $63, $00, $00
		.byte $00, $63, $00, $00
		.byte $00, $67, $00, $00
		.byte $00, $69, $00, $00
		.byte $00, $79, $00, $00
		.byte $00, $7a, $00, $00
		.byte $00, $80, $00, $00
		.byte $00, $8f, $00, $00
		.byte $00, $e6, $00, $00
		.byte $cc, $ff, $cc, $00


		.align $10			; waste of memory, but debugging is easier
line_offset	.fill 64 * 2, $FE		; not possible to determine at compile time

		.align $10			; waste of memory, but debugging is easier

		.seed(3741)
rnd0		.byte random([256] x RAND_SIZE)
rnd1		.byte random([100] x RAND_SIZE)
