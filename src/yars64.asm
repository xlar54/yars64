        .cpu "6502"

; ----------------------------
; BASIC stub: 10 SYS2061
; ----------------------------
        * = $0801
        .word bas_next
        .word 10
        .byte $9e
        .text "2061"
        .byte 0
bas_next:
        .word 0

        * = $080d

; ----------------------------
; Constants
; ----------------------------
VIC             = $d000
CIA2            = $dd00

SCREEN          = $0400
COLORRAM        = $d800

CHAR_DST        = $2000
SPR_DST         = $3000

SPR_PTRS        = SCREEN + $03f8

JOY2            = $dc00

RASTER_FRAME    = 0
RASTER_ZONE_ON  = 70
RASTER_ZONE_OFF = 170

ZONE_ROW_TOP    = 0
ZONE_ROW_BOTTOM = 25    ; loop ends when X == 25

ZONE_COL_L      = 10
ZONE_COL_R      = 21

WALL_COL_L      = 23
WALL_COL_R      = 25

CH_SPACE        = 0
CH_ZONE_A       = 1
CH_ZONE_B       = 2
CH_ZONE_C       = 3
CH_WALL_SOLID   = 10
CH_WALL_DMG1    = 11
CH_WALL_DMG2    = 12
CH_WALL_DMG3    = 13

MISSILE_SLOW_MASK = %00000111    ; update once every 8 frames (7 = slower, 3 = medium)

SHOT_START_X     = 24
SHOT_SPEED       = 4          ; pixels per update (try 2 or 4)
SHOT_OFFSCREEN_X = 252
SHOT_OFFSCREEN_LO = $68    ; 360 = $0168
SHOT_OFFSCREEN_HI = 1

BARRIER_ROW   = 5          ; top row of the barrier (tweak if needed)
BARRIER_COL   = 29         ; left edge of the barrier (tweak if needed)
BARRIER_COLOR = 2          ; red

BARRIER_H      = 12
BARRIER_W      = 10              ; overall width in chars
BARRIER_THICK  = 3               ; thickness of border

PLAYER_MIN_X_LO = $18   ; 24
PLAYER_MIN_X_HI = 0
PLAYER_MAX_X_LO = $57   ; 343-256 = 87
PLAYER_MAX_X_HI = 1

ZONE_BLANK_MASK = 2   ; 0=toggle every frame, 1=every 2 frames, 3=every 4, 7=every 8...

; sprite pointer values (bank 0, sprite data at $3000)
SPR0_PTR_RIGHT  = $c0   ; $3000
SPR0_PTR_UP     = $c1   ; $3040
SPR0_PTR_DOWN   = $c2   ; $3080
SPR0_PTR_LEFT   = $c3   ; $30c0
SPR1_PTR_ALIEN  = $c4   ; $3100
SPR2_PTR_MISS   = $c5   ; $3140
SPR3_PTR_SHOT   = $c6   ; $3180

; ----------------------------
; Game variables (zero page)
; ----------------------------
yar_x   = $f0
yar_y   = $f1
miss_x  = $f2
miss_y  = $f3

shot_x   = $f7
shot_y   = $f8

shot_state  = $f9     ; 0=ready, 1=flying
fire_prev   = $fa     ; previous fire bit (0 pressed, 1 released)

joy_state   = $ef     ; temp joystick cache (picked a free ZP byte here)

; pointers must be ZP for (zp),y
src_lo      = $fb
src_hi      = $fc
dst_lo      = $fd
dst_hi      = $fe
pages_left  = $ff

irq_state     = $f4
frame_counter = $f5
zone_phase = $f6

; ------------------------------------------------------------
start:
        sei

        lda #$2f        ; CPU port DDR: bits 0-5 outputs (standard reset value)
        sta $00

        lda #$35        ; RAM under BASIC+KERNAL, I/O still visible
        sta $01

        lda #80
        sta yar_x
        lda #120
        sta yar_y

        lda #0
        sta yar_x_hi
        sta miss_x_hi
        sta shot_x_hi

        lda #220
        sta miss_x
        lda #120
        sta miss_y

        lda #0
        sta zone_phase

        lda #0
        sta irq_state
        sta frame_counter

        ; VIC bank 0 ($0000-$3fff). CIA2 bits 0-1 are inverted: bank0 => %11
        lda CIA2
        and #%11111100
        ora #%00000011
        sta CIA2

        ; screen=$0400, charset=$2000 -> D018 = $18 (screen index 1, charset index 4)
        lda #$18
        sta $d018

        ; bitmap off
        lda $d011
        and #%10111111
        sta $d011

        ; enable multicolor text mode
        lda $d016
        ora #%00010000
        sta $d016

        ; pick base colors
        lda #0
        sta $d021          ; background (black)
        sta $d020
        lda #12
        sta $d022          ; zone shared color 1
        lda #11
        sta $d023          ; zone shared color 2

        ; copy assets
        jsr copy_charset
        jsr copy_sprites

        lda #SHOT_START_X
        sta shot_x
        lda yar_y
        sta shot_y

        lda #0
        sta shot_state

        lda #$10          ; fire bit released (bit4=1) initial
        sta fire_prev

        jsr clear_screen
        jsr draw_zone_and_wall
        jsr draw_barrier
        jsr init_sprites
        jsr init_irq

        cli
mainloop:
        jmp mainloop

; ------------------------------------------------------------
; Copy charset data (2048 bytes) to $2000
; ------------------------------------------------------------
copy_charset:
        lda #<charset_data
        sta src_lo
        lda #>charset_data
        sta src_hi

        lda #<CHAR_DST
        sta dst_lo
        lda #>CHAR_DST
        sta dst_hi

        lda #8
        sta pages_left

cc_pages:
        ldy #0
cc_loop:
        lda (src_lo),y
        sta (dst_lo),y
        iny
        bne cc_loop
        inc src_hi
        inc dst_hi
        dec pages_left
        bne cc_pages
        rts

; ------------------------------------------------------------
; Copy sprite data to $3000
; NOW copies 2 pages (512 bytes) so we can store 4 frames + sprites.
; ------------------------------------------------------------
copy_sprites:
        lda #<sprite_data
        sta src_lo
        lda #>sprite_data
        sta src_hi

        lda #<SPR_DST
        sta dst_lo
        lda #>SPR_DST
        sta dst_hi

        lda #2
        sta pages_left

cs_pages:
        ldy #0
cs_loop:
        lda (src_lo),y
        sta (dst_lo),y
        iny
        bne cs_loop
        inc src_hi
        inc dst_hi
        dec pages_left
        bne cs_pages
        rts

; ------------------------------------------------------------
clear_screen:
        lda #0
        ldx #0
cs1:
        sta SCREEN + 0*256, x
        sta SCREEN + 1*256, x
        sta SCREEN + 2*256, x
        sta SCREEN + 3*256, x
        inx
        bne cs1

        lda #0
        ldx #0
cs2:
        sta COLORRAM + 0*256, x
        sta COLORRAM + 1*256, x
        sta COLORRAM + 2*256, x
        sta COLORRAM + 3*256, x
        inx
        bne cs2
        rts

; ------------------------------------------------------------
; Thick "<" barrier (hollow wedge), open to the LEFT side,
; flat nose (not pointy), 2-char thick border.
; ------------------------------------------------------------
draw_barrier:
        ldx #0
db_row:
        cpx #BARRIER_H
        beq db_done

        ; row pointer -> screen
        txa
        clc
        adc #BARRIER_ROW
        jsr row_to_ptr

        ; compute left edge = BARRIER_COL + barrier_left[x]
        lda barrier_left, x
        clc
        adc #BARRIER_COL
        sta bar_left

        ; right edge = BARRIER_COL + (BARRIER_W-1)
        lda #BARRIER_COL + (BARRIER_W-1)
        sta bar_right

        ; Decide if this row is part of top/bottom thickness band
        cpx #BARRIER_THICK
        bcc db_horiz               ; top band (rows 0..THICK-1)

        lda #BARRIER_H - BARRIER_THICK
        cmp #0
        cpx #(BARRIER_H - BARRIER_THICK)
        bcs db_horiz

; ---- middle rows: draw only the LEFT/front wall (open on right) ----
db_sides:
        ; left border (2 chars): left and left+1
        ldy bar_left
        lda #CH_WALL_SOLID
        sta (dst_lo),y
        iny
        sta (dst_lo),y

        ; colors (same positions)
        txa
        clc
        adc #BARRIER_ROW
        jsr row_to_color_ptr

        ldy bar_left
        lda #BARRIER_COLOR
        sta (dst_lo),y
        iny
        sta (dst_lo),y

        inx
        jmp db_row

; ---- top/bottom bands: draw horizontal run (thick cap) ----
db_horiz:
        ldy bar_left
        lda #CH_WALL_SOLID
db_hloop:
        sta (dst_lo),y
        iny
        cpy #bar_right_plus1
        bne db_hloop

        txa
        clc
        adc #BARRIER_ROW
        jsr row_to_color_ptr

        ldy bar_left
        lda #BARRIER_COLOR
db_cloop:
        sta (dst_lo),y
        iny
        cpy #bar_right_plus1
        bne db_cloop

        inx
        jmp db_row

db_done:
        rts

bar_left:        .byte 0
bar_right:       .byte 0
bar_right_plus1  = (BARRIER_COL + BARRIER_W)

barrier_left:
        .byte 7,6,5,4,3,3,3,3,4,5,6,7,8

; ------------------------------------------------------------
draw_zone_and_wall:
        ldx #ZONE_ROW_TOP
dz_row:
        cpx #ZONE_ROW_BOTTOM
        beq dz_done

        jsr fill_row_zone
        inx
        jmp dz_row
dz_done:
        rts

fill_row_zone:
        txa
        pha
        jsr row_to_ptr

        ldy #ZONE_COL_L
        lda #0
fz_loop:
        tax
        lda zone_chars,x
        sta (dst_lo),y

        iny
        cpy #(ZONE_COL_R+1)
        beq fz_done

        txa
        clc
        adc #1
        and #3
        jmp fz_loop

fz_done:
        pla
        tax
        rts

zone_chars: .byte CH_ZONE_A, CH_ZONE_B, CH_ZONE_C, CH_ZONE_B

fill_row_wall:
        pha
        txa
        jsr row_to_ptr
        pla
        ldy #WALL_COL_L
frw:
        sta (dst_lo),y
        iny
        cpy #(WALL_COL_R+1)
        bne frw
        rts

; ------------------------------------------------------------
row_to_ptr:
        tay

        lda #<SCREEN
        sta dst_lo
        lda #>SCREEN
        sta dst_hi

        cpy #0
        beq rtp_done

rtp_add40:
        clc
        lda dst_lo
        adc #40
        sta dst_lo
        bcc rtp_no_c
        inc dst_hi
rtp_no_c:
        dey
        bne rtp_add40

rtp_done:
        rts

; ------------------------------------------------------------
init_sprites:
        ; pointers (note sprite0 is dynamic; default RIGHT)
        lda #SPR0_PTR_RIGHT
        sta SPR_PTRS+0
        lda #SPR1_PTR_ALIEN
        sta SPR_PTRS+1
        lda #SPR2_PTR_MISS
        sta SPR_PTRS+2
        lda #SPR3_PTR_SHOT
        sta SPR_PTRS+3

        lda #%00001111
        sta $d015

        ; sprite 1 behind background (alien behind barrier)
        lda #%00000010
        sta $d01b

        ; multicolor mode for sprites 0-3
        lda #%00001111
        sta $d01c

        ; hazy shared multicolor for sprites (grays)
        lda #11          ; dark gray
        sta $d025
        lda #12          ; gray
        sta $d026

        ; per-sprite colors
        lda #1           ; white
        sta $d027        ; sprite0 player

        lda #13          ; light green
        sta $d028        ; sprite1 alien
        sta $d029        ; sprite2 missile (alien bullet)
        lda #1
        sta $d02a        ; sprite3 player shot (white)

        lda yar_x
        sta $d000
        lda yar_y
        sta $d001

        lda miss_x
        sta $d004
        lda miss_y
        sta $d005

        lda #0
        sta $d006
        sta $d007

        ; enemy sprite (sprite 1) inside the barrier opening
        ENEMY_X = (BARRIER_COL+10)*8
        ENEMY_Y = (BARRIER_ROW+11)*8

        lda #<ENEMY_X
        sta $d002

        lda $d010
        and #%11111101
        ora #(((ENEMY_X >> 8) & 1) << 1)
        sta $d010

        lda #ENEMY_Y
        sta $d003

        rts

; ------------------------------------------------------------
init_irq:
        sei

        lda #$7f
        sta $dc0d
        sta $dd0d
        lda $dc0d
        lda $dd0d

        lda #<irq
        sta $fffe
        lda #>irq
        sta $ffff

        lda #<nmi
        sta $fffa
        lda #>nmi
        sta $fffb

        lda #$1b
        sta $d011

        lda #$f8
        sta $d012

        lda #$01
        sta $d019

        lda #$01
        sta $d01a

        cli
        rts

irq:
        pha
        txa
        pha
        tya
        pha

        lda #$2f
        sta $00
        lda #$35
        sta $01

        lda $dc0d
        lda $dd0d

        lda #$01
        sta $d019

        lda irq_state
        beq irq_frame
        cmp #1
        beq irq_zone_on
        jmp irq_zone_off

irq_frame:
        lda frame_counter
        and #ZONE_BLANK_MASK
        bne +
        lda zone_blank
        eor #$01
        sta zone_blank
        jsr zone_update_chars
+

        jsr zone_shimmer
        jsr game_update
        lda #1
        sta irq_state
        lda #RASTER_ZONE_ON
        sta $d012
        jmp irq_exit

irq_zone_on:
        inc frame_counter
        lda frame_counter
        and #$0f
        tax
        lda shimmer_table, x
        sta $d022
        lda shimmer_table2, x
        sta $d023

        lda #2
        sta irq_state
        lda #RASTER_ZONE_OFF
        sta $d012
        jmp irq_exit

irq_zone_off:
        lda #0
        sta irq_state
        lda #RASTER_FRAME
        sta $d012

irq_exit:
        pla
        tay
        pla
        tax
        pla
        rti

nmi:
        rti

shimmer_table:   .byte 0,0,0,12,0,12,0,12, 12,11,0,0,0,11,12,12
shimmer_table2:  .byte 0,0,12,11,0,11,0,15, 11,0,00,12,0,0,0,12

; ------------------------------------------------------------
zone_update_chars:
        txa
        pha

        ldx #ZONE_ROW_TOP
zuc_row:
        cpx #ZONE_ROW_BOTTOM
        beq zuc_done

        txa
        jsr row_to_ptr

        lda zone_blank
        beq zuc_pattern

        ldy #ZONE_COL_L
        lda #CH_SPACE
zuc_bloop:
        sta (dst_lo),y
        iny
        cpy #(ZONE_COL_R+1)
        bne zuc_bloop

        inx
        jmp zuc_row

zuc_pattern:
        stx zone_rowtmp

        ldy #ZONE_COL_L
        ldx #0
zuc_ploop:
        lda zone_chars,x
        sta (dst_lo),y
        iny
        cpy #(ZONE_COL_R+1)
        beq zuc_pdone

        inx
        cpx #4
        bne zuc_ploop
        ldx #0
        jmp zuc_ploop

zuc_pdone:
        ldx zone_rowtmp
        inx
        jmp zuc_row

zuc_done:
        pla
        tax
        rts

; ------------------------------------------------------------
game_update:
        jsr read_joy_move_yar
        jsr update_shot

        lda frame_counter
        and #MISSILE_SLOW_MASK
        bne skip_missile

        jsr update_missile
        jsr write_positions
        rts

skip_missile:
        jsr write_positions
        rts

; ------------------------------------------------------------
update_shot:
        lda JOY2
        and #$10
        tax

        lda fire_prev
        beq us_prev_pressed
        cpx #$00
        bne us_no_new_press

        lda shot_state
        bne us_no_new_press
        lda #1
        sta shot_state
        lda yar_y
        sta shot_y
us_no_new_press:

us_prev_pressed:
        txa
        sta fire_prev

        lda shot_state
        bne us_flying

        lda #SHOT_START_X
        sta shot_x
        lda #0
        sta shot_x_hi
        lda yar_y
        sta shot_y
        rts

us_flying:
        lda shot_x
        clc
        adc #SHOT_SPEED
        sta shot_x
        bcc +
        inc shot_x_hi
+

        lda shot_x_hi
        cmp #SHOT_OFFSCREEN_HI
        bcc us_done
        bne us_reset

        lda shot_x
        cmp #SHOT_OFFSCREEN_LO
        bcc us_done

us_reset:
        lda #0
        sta shot_state
        lda #SHOT_START_X
        sta shot_x
        lda #0
        sta shot_x_hi
        lda yar_y
        sta shot_y

us_done:
        rts

; ------------------------------------------------------------
; Joystick movement + sprite0 direction swap (RIGHT/UP/DOWN/LEFT)
; ------------------------------------------------------------
read_joy_move_yar:
        lda JOY2
        sta joy_state

; left (bit2 low)
        lda joy_state
        and #%00000100
        bne no_left

        lda yar_x
        sec
        sbc #2
        sta yar_x
        bcs +
        dec yar_x_hi
+
        lda yar_x_hi
        bpl +
        lda #PLAYER_MIN_X_HI
        sta yar_x_hi
        lda #PLAYER_MIN_X_LO
        sta yar_x
+
        lda yar_x_hi
        bne no_left
        lda yar_x
        cmp #PLAYER_MIN_X_LO
        bcs no_left
        lda #PLAYER_MIN_X_LO
        sta yar_x
no_left:

; right (bit3 low)
        lda joy_state
        and #%00001000
        bne no_right

        lda yar_x
        clc
        adc #2
        sta yar_x
        bcc +
        inc yar_x_hi
+
        lda yar_x_hi
        cmp #PLAYER_MAX_X_HI
        bcc no_right
        bne clamp_max
        lda yar_x
        cmp #PLAYER_MAX_X_LO
        bcc no_right
clamp_max:
        lda #PLAYER_MAX_X_HI
        sta yar_x_hi
        lda #PLAYER_MAX_X_LO
        sta yar_x
no_right:

; up (bit0 low)
        lda joy_state
        and #%00000001
        bne no_up
        lda yar_y
        sec
        sbc #2
        sta yar_y
no_up:

; down (bit1 low)
        lda joy_state
        and #%00000010
        bne no_down
        lda yar_y
        clc
        adc #2
        sta yar_y
no_down:

; ---- sprite0 direction frame selection ----
; priority: left > right > up > down > default right
        lda joy_state
        and #%00000100
        beq spr_left

        lda joy_state
        and #%00001000
        beq spr_right

        lda joy_state
        and #%00000001
        beq spr_up

        lda joy_state
        and #%00000010
        beq spr_down

        lda #SPR0_PTR_RIGHT
        bne spr_set

spr_left:
        lda #SPR0_PTR_LEFT
        bne spr_set
spr_right:
        lda #SPR0_PTR_RIGHT
        bne spr_set
spr_up:
        lda #SPR0_PTR_UP
        bne spr_set
spr_down:
        lda #SPR0_PTR_DOWN

spr_set:
        sta SPR_PTRS+0
        rts

; ------------------------------------------------------------
update_missile:
        lda miss_x_hi
        cmp yar_x_hi
        bne mx_hi_diff

        lda miss_x
        cmp yar_x
        beq umx_done
        bcc umx_inc
umx_dec:
        lda miss_x
        sec
        sbc #1
        sta miss_x
        bcs umx_done
        dec miss_x_hi
        jmp umx_done

umx_inc:
        inc miss_x
        bne umx_done
        inc miss_x_hi
        jmp umx_done

mx_hi_diff:
        bcc umx_inc
        jmp umx_dec

umx_done:
        lda miss_y
        cmp yar_y
        beq umy_done
        bcc umy_inc
        dec miss_y
        jmp umy_done
umy_inc:
        inc miss_y
umy_done:
        rts

; ------------------------------------------------------------
write_positions:
        lda yar_x
        sta $d000
        lda yar_y
        sta $d001

        lda miss_x
        sta $d004
        lda miss_y
        sta $d005

        lda shot_x
        sta $d006
        lda shot_y
        sta $d007

        lda #0

        ldx yar_x_hi
        beq +
        ora #%00000001
+
        ora #%00000010

        ldx miss_x_hi
        beq +
        ora #%00000100
+
        ldx shot_x_hi
        beq +
        ora #%00001000
+
        sta $d010
        rts

; ------------------------------------------------------------
zone_shimmer:
        inc zone_phase
        lda zone_phase
        and #$0f
        sta tmp_phase

        ldx #ZONE_ROW_TOP
zs_row:
        cpx #ZONE_ROW_BOTTOM
        beq zs_done

        txa
        jsr row_to_color_ptr

        lda tmp_phase
        eor row_xor, x

        ldy #ZONE_COL_L
zs_col:
        ora #$08
        sta (dst_lo),y
        iny
        cpy #(ZONE_COL_R+1)
        bne zs_col

        inx
        jmp zs_row
zs_done:
        rts

tmp_phase: .byte 0

row_xor:
        .byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
        .byte 0,1,2,3,4,5,6,7,8

; ------------------------------------------------------------
row_to_color_ptr:
        pha
        lda #<COLORRAM
        sta dst_lo
        lda #>COLORRAM
        sta dst_hi
        pla

        tay
        beq rtcp_done

rtcp_add40:
        clc
        lda dst_lo
        adc #40
        sta dst_lo
        bcc rtcp_no_c
        inc dst_hi
rtcp_no_c:
        dey
        bne rtcp_add40

rtcp_done:
        rts

; ------------------------------------------------------------
; Assets
; ------------------------------------------------------------
charset_data:
        .rept 8
        .byte 0
        .endrept

        .byte $1b,$e4,$b1,$4e,$d8,$27,$72,$8d
        .byte $4e,$1b,$8d,$72,$27,$d8,$e4,$b1
        .byte $55,$aa,$1b,$e4,$55,$aa,$4e,$b1

        .rept (6*8)
        .byte 0
        .endrept

        .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
        .byte $ff,$ff,$ef,$ef,$ef,$ff,$ff,$ff
        .byte $ff,$ef,$c7,$c7,$c7,$ef,$ff,$ff
        .byte $ef,$c7,$83,$83,$83,$c7,$ef,$ff

        .rept (2048 - (14*8))
        .byte 0
        .endrept

yar_x_hi:   .byte 0
miss_x_hi:  .byte 0
shot_x_hi:  .byte 0

zone_blank:  .byte 0
zone_rowtmp: .byte 0

; ------------------------------------------------------------
; sprite_data: 512 bytes copied to $3000
; Layout:
;   $3000 sprite0 RIGHT
;   $3040 sprite0 UP
;   $3080 sprite0 DOWN
;   $30C0 sprite0 LEFT
;   $3100 sprite1 alien
;   $3140 sprite2 missile
;   $3180 sprite3 shot
; ------------------------------------------------------------
sprite_data:

; sprite0 RIGHT (data 0)
sprite_0_right:
        .byte $00,$00,$00,$02,$00,$00,$02,$00
        .byte $00,$02,$00,$00,$02,$a0,$00,$02
        .byte $a0,$20,$08,$28,$20,$08,$28,$20
        .byte $0a,$82,$80,$0a,$82,$80,$0a,$82
        .byte $80,$0a,$82,$80,$08,$28,$20,$08
        .byte $28,$20,$02,$a0,$20,$02,$a0,$00
        .byte $02,$00,$00,$02,$00,$00,$02,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite0 UP (data 1)
sprite_0_up:
        .byte $00,$00,$00,$00,$00,$00,$00,$82
        .byte $00,$00,$82,$00,$00,$28,$00,$00
        .byte $28,$00,$00,$28,$00,$00,$28,$00
        .byte $00,$82,$00,$00,$82,$00,$00,$82
        .byte $00,$02,$00,$80,$02,$28,$80,$02
        .byte $28,$80,$0a,$28,$a0,$0a,$aa,$a0
        .byte $00,$aa,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite0 DOWN (data 2)
sprite_0_down:
        .byte $00,$00,$00,$00,$00,$00,$00,$aa
        .byte $00,$00,$aa,$00,$0a,$28,$a0,$0a
        .byte $28,$a0,$02,$28,$80,$02,$00,$80
        .byte $00,$82,$00,$00,$82,$00,$00,$82
        .byte $00,$00,$28,$00,$00,$28,$00,$00
        .byte $28,$00,$00,$28,$00,$00,$82,$00
        .byte $00,$82,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite0 LEFT (data 3)
sprite_0_left:
        .byte $00,$00,$00,$00,$00,$80,$00,$00
        .byte $80,$00,$00,$80,$00,$0a,$80,$08
        .byte $0a,$80,$08,$28,$20,$08,$28,$20
        .byte $02,$82,$a0,$02,$82,$a0,$02,$82
        .byte $a0,$02,$82,$a0,$08,$28,$20,$08
        .byte $28,$20,$08,$0a,$80,$00,$0a,$80
        .byte $00,$00,$80,$00,$00,$80,$00,$00
        .byte $80,$00,$00,$00,$00,$00,$00,$83

; sprite1 (alien / qotile)
.byte $00,$00,$00,$00,$28,$00,$00,$a8
.byte $00,$02,$a8,$00,$0a,$28,$00,$a8
.byte $28,$00,$a0,$28,$00,$a0,$28,$00
.byte $aa,$a8,$00,$a0,$28,$00,$a0,$28
.byte $00,$a8,$28,$00,$0a,$28,$00,$02
.byte $a8,$00,$00,$a8,$00,$00,$28,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite2 (missile / alien bullet)
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$aa,$80,$00
.byte $aa,$80,$00,$aa,$80,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite3 (shot)
        
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$aa
.byte $a0,$00,$55,$50,$00,$55,$50,$00
.byte $ff,$f0,$00,$ff,$f0,$00,$55,$50
.byte $00,$55,$50,$00,$aa,$a0,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$83

; pad sprite_data to 512 bytes so our 2-page copy is safe
        .rept (512 - (7*64))
        .byte 0
        .endrept
