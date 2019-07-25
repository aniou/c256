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

CLRSCREEN	= $1900a8
ENDLESS_LOOP	= $1904ed ; so lame, but works for testing...



;======================================================================
; MAIN DEMO PROGRAM
;======================================================================

; if you want a TBOX routine see below

		* = $030000
		
		clc			; switch to native
		xce			; 

		phd
		php
		setaxl			; set 16bits
		pha
		phx

		jsl ICLRSCREEN2
		jsl TBOX_INIT		; TBOX must be initialized once
		jsl nw_win
		jsl sw_win
		jsl se_win
		jsl ne_win

		setal
		plx
		pla
		plp
		pld
		jmp ENDLESS_LOOP


;----------------------------------------------------------------------
; data blocks for test
; see below for struct config_data definition

data1		.dstruct config_data
;data2		.dstruct config_data


set_cfg         .macro
		lda \2		; begin set_cfg
		sta \1		; end set_cfg
		.endm

;----------------------------------------------------------------------
nw_win		setaxl
		.set_cfg data1.src_offset,  @w #00
		.set_cfg data1.col_start,   @w #02
		.set_cfg data1.col_count,   @w #55
		.set_cfg data1.row_start,   @w #02
		.set_cfg data1.row_count,   @w #28
		.set_cfg data1.src_addr,    #<>data_buffer
		setas    
		.set_cfg data1.src_addr+2,  #`data_buffer
		.set_cfg data1.control,     #tf_wrap_at_word | tf_use_cr

		ldy #<>data1		; pass config block addr
		lda #`data1
		jmp TBOX_PRINT

;----------------------------------------------------------------------
sw_win		setaxl
		.set_cfg data1.src_offset,  @w #00
		.set_cfg data1.col_start,   @w #02
		.set_cfg data1.col_count,   @w #33
		.set_cfg data1.row_start,   @w #32
		.set_cfg data1.row_count,   @w #22
		.set_cfg data1.src_addr,    #<>data_buffer
		setas    
		.set_cfg data1.src_addr+2,  #`data_buffer
		.set_cfg data1.control,     #tf_wrap_at_char | tf_use_cr
	
		ldy #<>data1		; pass config block addr
		lda #`data1
		jmp TBOX_PRINT

;----------------------------------------------------------------------
se_win		setaxl
		.set_cfg data1.src_offset,  @w #00
		.set_cfg data1.col_start,   @w #38
		.set_cfg data1.col_count,   @w #35
		.set_cfg data1.row_start,   @w #32
		.set_cfg data1.row_count,   @w #22
		.set_cfg data1.src_addr,    #<>data_buffer
		setas    
		.set_cfg data1.src_addr+2,  #`data_buffer
		.set_cfg data1.control,     #tf_nowrap | tf_use_cr

		ldy #<>data1		; pass config block addr
		lda #`data1
		jmp TBOX_PRINT


;----------------------------------------------------------------------
ne_win		setaxl
		.set_cfg data1.src_offset,  @w #00
		.set_cfg data1.col_start,   @w #60
		.set_cfg data1.col_count,   @w #14
		.set_cfg data1.row_start,   @w #02
		.set_cfg data1.row_count,   @w #28
		.set_cfg data1.src_addr,    #<>data_buffer
		setas    
		.set_cfg data1.src_addr+2,  #`data_buffer
		.set_cfg data1.control,     #tf_raw_mode

		ldy #<>data1		 ; pass config block addr
		lda #`data1
		jmp TBOX_PRINT



;======================================================================
; END of MAIN DEMO PROGRAM
;======================================================================


;======================================================================
; ICLRSCREEN2 (additional utility routine)
;
; Clear the screen and set the background and foreground colors to the
; currently selected colors.
; that version preserve original flags
;
; https://github.com/Trinity-11/Kernel/issues/2

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
		LDA #$ED		; Fill the Color Memory with Foreground: 75% Purple, Background 12.5% White
		;LDA #$7D		; Fill the Color Memory with Foreground: 75% Purple, Background 12.5% White
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



;======================================================================
; START OF TBOX ROUTINES
;======================================================================

;======================================================================
; DATA DEFINITION AND FLAGS

; config_data.control and (internally tbox.control) flags.
tf_use_cr       = %00000001	; 1: CR is newline 0: ignore
tf_use_lf       = %00000010	; 1: LF is newline 0: ignore
tf_use_markers  = %00000100	; print marks at rightmost column
tf_wrap_at_word = %00001000     ; 1: at word 0: at char
tf_wrap_at_char = %00000000     ; does nothing but supplements _at_word
tf_nowrap       = %00010000     ; wrapping disabled?
tf_raw_mode     = %10000000     ; raw mode, print all, disables 


;----------------------------------------------------------------------
; tbox - internal routine config
;
; there are too many wariables on direct page, but it is 
; a simpler solution that splitting between direct and
; "regular" memory
		.virtual $00e0
tbox		.block
control		.byte ?	; multiple lda          per char
config_addr     .long ? ; twice - lda at start, sta at end
src_addr	.long ?	;      lda [src_addr],x per char
src_offset	.word ?	; ldy/sty               per line
row_start	.word ? ; once, lda at start
row_current     .word ? ; ldy/sty/lda	   	per line
row_end		.word ?	; cpy	 		per line
col_start	.word ?	; 2 x ADC 		per line
col_count	.word ?	; 1 x ADC 		per line
col_end		.word ?	; cpx	   per char; adc offset+col_start+col_count per line
src_last_space	.word ?	; stx few times per line
dst_last_space	.word ?	; sty few times per line
		.bend
		.endv

;----------------------------------------------------------------------
; config_data - program config used by TBOX_CONFIG_SET to fill tbox.*
;
;
config_data	.struct
src_addr	.long	?
control		.byte	?
src_offset	.word	?
row_start	.word	?
row_count	.word	?
row_end		.word	?   ; TODO: value returned from tbox
col_start	.word	?
col_count	.word	?
col_end		.word	?   ; TODO: value returned from tbox
		.ends

;======================================================================
; MACROS for TBOX
; puts colored char on screen
;
; 1 - color
; 2 - char number

right_marker_as	.macro
		setas			; begin right_marker_as
		lda #\1			; color
		sta CS_COLOR_MEM_PTR, x
		lda #\2		 	; char
		sta CS_TEXT_MEM_PTR, x	; end right_marker_as
		.endm

;=======================================================================
; TBOX_PRINT
; main printing routine

TBOX_PRINT	jsr TBOX_CONFIG_SET
print_text 	setaxl
		lda tbox.row_current
		asl a			; because we have array of words
		tax

		lda line_offset, x
		clc
		adc tbox.col_count
		adc tbox.col_start
		sta tbox.col_end	; memory place for last column in current line

		; text/color pointer
		lda line_offset, x
		clc
		adc tbox.col_start
		tax

		; reset space position
		lda #$8000		; highest bit denotes lack of space in row
		sta tbox.dst_last_space

		; restore source offset
		ldy tbox.src_offset

;----------------------------------------------------------------------
; print row
; UNIX       LF	: $0a
; DOS     CR LF : $0d $0a
; C64     CR	: $0d
; AtariXE -	: $9b       TODO - not supported yet

		setas
print_row	lda tbox.control
		bpl parse_chars		; highest bit denotes "raw mode"
		lda [tbox.src_addr], y
		jmp put_char

parse_chars	lda [tbox.src_addr], y
		bne t_20		; $0 denotes EOF
		jmp text_done		

t_20		cmp #$20		; space? XXX: any whitechar
		bne t_0d
		sty tbox.src_last_space	; remember position of last word separator in source
		stx tbox.dst_last_space	; remember screen column (NOT position in dest!)

t_0d		cmp #$0d		; CR - new line
		bne t_0a
		iny
		lda tbox.control
		and #%00000001
		beq print_row
		jmp next_line

t_0a		cmp #$0a		; LF - new line?
		bne put_char
		iny
		lda tbox.control
		and #%00000010
		beq print_row
		jmp next_line

put_char	sta CS_TEXT_MEM_PTR, x
		iny
		inx
		cpx tbox.col_end
		bne print_row

;----------------------------------------------------------------------
; end-of-line handling

		lda tbox.control
		;bmi next_line		; raw mode when high bit is set
		bpl +			; workaround for too long bmi
		jmp next_line
+
		;lda tbox.control
		and #%00010000		; word wrapping disabled?
		beq line_wrapping

		; we do not wrap line, only print marker and move forward
		; X register to any known EOL
		; TODO - can it be optimized? it is something hairy

		.right_marker_as $4d, $1a	; dark yellow / arrow
line_skip	lda [tbox.src_addr], y		; search for line end
		beq text_done			; $0 denotest eof
		cmp #$0a			; LF - new line?
		beq end_of_skip
		cmp #$0d			; CR - new line?
		bne line_skip_cont		;
		iny				; testing CR LF sequence
		lda [tbox.src_addr], y
		cmp #$0a
		beq end_of_skip			; skip one more char by inx at end_of_skip
		bra next_line			; only CR sequence, X already points at next char
line_skip_cont
		iny
		bra line_skip
end_of_skip
		iny
		bra next_line

;----------------------------------------------------------------------
line_wrapping	lda tbox.control		; already in A, comment-out?
		and #%00001000
		beq break_at_char

		; word wrap?
		lda [tbox.src_addr], y
		cmp #$20			; space at newline?
		bne break_at_word

		iny
		.right_marker_as $6d, $f9	; purple / dot - for skipped spaces
		bra next_line

;----------------------------------------------------------------------
; when we are here that means there was no space at line ending
; so we need to back source index to last known space
; and erase everything on screen from now to last known space position

break_at_word	setal
		lda tbox.dst_last_space
		cmp #$8000			; foenix ide emulator is broken
		beq break_at_char
		;bmi break_at_char		; there was no space in row when == $8000

		.right_marker_as $5d, $f9	; cyan / dot for cleaning a partial word
		setas
		lda #$20			; space for clearing

fill_row
		dex
		sta CS_TEXT_MEM_PTR, x
		cpx tbox.dst_last_space
		bne fill_row

		ldy tbox.src_last_space		; todo - move at end
		iny				; the same case as space at newline, we skip the
		bra next_line


break_at_char	.right_marker_as $7d, $f9	; white / dot

; ---------------------------------------------------------------------
; next line
next_line	sty tbox.src_offset
		ldy tbox.row_current
		iny
		cpy tbox.row_end
		beq text_done
		sty tbox.row_current
		
		; reset space position
		ldy #$8000		; $8000: "no whitechar in this row"
		sty tbox.dst_last_space

		jmp print_text

; ---------------------------------------------------------------------
; exit procedure

		; last row+1 in X
		; XXX fill program config block by current tbox.* values
text_done	rtl


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
		asl			; *2 because X indexes a words
		tax

		lda LINES_MAX
		asl
		asl
		asl
		asl
		asl
		asl
		asl			; in A: LINES_MAX*$80 

tinit0		sec
		sbc COLS_PER_LINE	; default 128 bytes
		dex
		dex
		sta line_offset,x
		bne tinit0
		rtl

;=======================================================================
; TBOX_CONFIG_SET
; read data from config block and fill tbox internal config
; structures there is possibility to use macros here, but all
; my attempts were ugly
;
; calling syntax:
;    jsr TBOX_CONFIG_SET
;
;    flag x=0 (long)
;    flag m=1 (short)
;    reg  Y=<> of config_block
;    reg  A=`  of config_block 
;

lc              .macro
		ldy \1				; begin lc
		lda [tbox.config_addr], y	; end lc
		.endm

TBOX_CONFIG_SET nop
		sty tbox.config_addr
		sta tbox.config_addr+2

		.lc #config_data.src_addr+2
		sta tbox.src_addr+2
		setal
		.lc #config_data.src_addr
		sta tbox.src_addr

		.lc #config_data.src_offset
		sta tbox.src_offset
		sta tbox.src_last_space

                lda #$8000			; highest bit: "no space at this row"
		sta tbox.dst_last_space

		.lc #config_data.row_start
		sta tbox.row_start
		sta tbox.row_current

		.lc #config_data.row_count
		clc
		adc tbox.row_start
		sta tbox.row_end

		.lc #config_data.col_start
		sta tbox.col_start

		.lc #config_data.col_count
		sta tbox.col_count

		.lc #config_data.control
		sta tbox.control

		rts

; ---------------------------------------------------------------------
; offset table for calculating row position

		.align $10			; waste of memory, but debugging is easier
line_offset	.fill 64 * 2, $FE		; not possible to determine at compile time

;======================================================================
; END OF TBOX ROUTINES
;======================================================================


;======================================================================
; sample data area for demo program

		* = $040000
;data_buffer	 .binary "lorem-ipsum.txt"
data_buffer	 .binary "moby.txt", $0 ; $0 denotes end-of-text

