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

BARRIER_ROW   = 7          ; top row of the barrier (tweak if needed)
BARRIER_COL   = 27         ; left edge of the barrier (tweak if needed)
BARRIER_COLOR = 2          ; red

BARRIER_H      = 13
BARRIER_W      = 13              ; overall width in chars
BARRIER_THICK  = 2               ; thickness of border

PLAYER_MIN_X_LO = $18   ; 24
PLAYER_MIN_X_HI = 0
PLAYER_MAX_X_LO = $57   ; 343-256 = 87
PLAYER_MAX_X_HI = 1

ZONE_BLANK_MASK = 2   ; 0=toggle every frame, 1=every 2 frames, 3=every 4, 7=every 8...


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

        lda #24        ; near left side
        sta shot_x
        lda yar_y
        sta shot_y

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
; Copy sprite data (256 bytes = 4 sprites) to $3000
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

        ; 1 page
        ldy #0
cs_loop:
        lda (src_lo),y
        sta (dst_lo),y
        iny
        bne cs_loop
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
        cmp #0                     ; (harmless, keeps assembler happy)
        ; if X >= (H-THICK) => bottom band
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
        ; draw from left..right
        ldy bar_left
        lda #CH_WALL_SOLID
db_hloop:
        sta (dst_lo),y
        iny
        cpy #bar_right_plus1
        bne db_hloop

        ; colors for left..right
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

; --- temp bytes (can live anywhere in RAM; ZP not required) ---
bar_left:        .byte 0
bar_right:       .byte 0

; right+1 helper (computed constant-ish each row)
; we’ll store bar_right+1 into this by using Y compare target
; easiest: use a fixed byte we update each row, but since bar_right is constant,
; we can precompute it once as an assembled constant:
bar_right_plus1  = (BARRIER_COL + BARRIER_W)

; ------------------------------------------------------------
; Left-edge shape table (offsets from BARRIER_COL).
; This makes a wedge "<" with a FLAT nose (rows 5..7).
; Smaller number = further left.
; ------------------------------------------------------------
barrier_left:
        .byte 7,6,5,4,3,3,3,3,4,5,6,7,8

; ------------------------------------------------------------
; Draw zone and wall using screen RAM (rows 3..21)
; ------------------------------------------------------------
draw_zone_and_wall:
        ldx #ZONE_ROW_TOP
dz_row:
        cpx #ZONE_ROW_BOTTOM
        beq dz_done

        jsr fill_row_zone

        ;lda #CH_WALL_SOLID
        ;jsr fill_row_wall

        inx
        jmp dz_row
dz_done:
        rts



; ------------------------------------------------------------
; Fill zone columns on row X with repeating chars 1,2,3,2
; Preserves X for caller (critical!)
; ------------------------------------------------------------
fill_row_zone:
        txa
        pha                     ; save row counter X on stack

        ; A still has row value here
        jsr row_to_ptr          ; dst_lo/dst_hi = SCREEN + row*40

        ldy #ZONE_COL_L
        lda #0                  ; pattern index 0..3
fz_loop:
        tax                     ; X = pattern index (temporary)
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
        tax                     ; restore row counter X
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
; Row->pointer: dst = SCREEN + row*40
; Input: A=row (0..24). Output: dst_lo/dst_hi set.
; Clobbers: A,Y  (does NOT clobber X)
; ------------------------------------------------------------
row_to_ptr:
        tay                     ; Y = row (save it BEFORE clobbering A)

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
        ; sprite pointers: $3000/64 = $c0
        lda #$c0
        sta SPR_PTRS+0
        lda #$c1
        sta SPR_PTRS+1
        lda #$c2
        sta SPR_PTRS+2
        lda #$c3
        sta SPR_PTRS+3

        lda #%00001111
        sta $d015

        ;lda $d01b
        ;ora #%11111101    ; sprite 1 behind background (bit1)
        lda #$00
        sta $d01b


        lda #14
        sta $d027
        lda #7
        sta $d028
        lda #2
        sta $d029
        lda #15
        sta $d02a

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

        ;lda #0
        ;sta $d010

        ; enemy sprite (sprite 1) inside the barrier opening
        ENEMY_X = (BARRIER_COL+6)*8     ; adjust +4..+6 to taste
        ENEMY_Y = (BARRIER_ROW+9)*8     ; middle-ish

        lda #<ENEMY_X
        sta $d002

        lda $d010
        and #%11111101          ; clear sprite1 MSB
        ora #(((ENEMY_X >> 8) & 1) << 1)   ; set if needed
        sta $d010

        lda #ENEMY_Y
        sta $d003


        rts

init_irq:
        sei

        ; disable all CIA IRQ sources (optional but good)
        lda #$7f
        sta $dc0d
        sta $dd0d
        lda $dc0d
        lda $dd0d

        ; install IRQ/BRK vector at $FFFE (KERNAL is banked out with $01=$35)
        lda #<irq
        sta $fffe
        lda #>irq
        sta $ffff

        ; NMI (RESTORE) -> just RTI
        lda #<nmi
        sta $fffa
        lda #>nmi
        sta $fffb

        ; make sure raster high-bit is 0 and display is enabled (DEN=1)
        lda #$1b
        sta $d011

        ; choose a stable raster line (avoid 0 while debugging)
        lda #$f8
        sta $d012

        ; clear any pending raster IRQ (write-1-to-clear)
        lda #$01
        sta $d019

        ; enable raster IRQ
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

; keep banking correct even if something toggles it
        lda #$2f
        sta $00
        lda #$35
        sta $01

        lda $dc0d       ; ack CIA1 if it caused the IRQ
        lda $dd0d       ; ack CIA2 if it caused the IRQ

        lda #$01
        sta $d019

        lda irq_state
        beq irq_frame
        cmp #1
        beq irq_zone_on
        jmp irq_zone_off

irq_frame:
        ; periodically toggle blanking and rewrite the zone chars
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
        ;lda #6
        ;sta $d021

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

;shimmer_table:   .byte 14,3,13,5,12,7,15,2,10,9,11,4,8,1,6,0
;shimmer_table2:  .byte 3,13,5,12,7,15,2,10,9,11,4,8,1,6,0,14

shimmer_table:   .byte 0,0,0,12,0,12,0,12, 12,11,0,0,0,11,12,12
shimmer_table2:  .byte 0,0,12,11,0,11,0,15, 11,0,00,12,0,0,0,12


; ------------------------------------------------------------
; zone_update_chars
; If zone_blank=1, fill zone area with CH_SPACE.
; If zone_blank=0, restore repeating zone pattern (1,2,3,2...).
; Preserves X (caller).
; ------------------------------------------------------------
zone_update_chars:
        txa
        pha

        ldx #ZONE_ROW_TOP
zuc_row:
        cpx #ZONE_ROW_BOTTOM
        beq zuc_done

        txa
        jsr row_to_ptr          ; dst_lo/dst_hi = SCREEN row (X preserved by row_to_ptr)

        lda zone_blank
        beq zuc_pattern

; --- blank fill ---
        ldy #ZONE_COL_L
        lda #CH_SPACE
zuc_bloop:
        sta (dst_lo),y
        iny
        cpy #(ZONE_COL_R+1)
        bne zuc_bloop

        inx
        jmp zuc_row

; --- pattern fill (1,2,3,2 repeating) ---
zuc_pattern:
        stx zone_rowtmp         ; save current row

        ldy #ZONE_COL_L
        ldx #0                  ; X = pattern index 0..3
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
        ldx zone_rowtmp         ; restore row
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
; update_shot
; shot_state=0: sit at left, follow yar_y, wait for fire edge
; shot_state=1: move right until offscreen, then reset
; ------------------------------------------------------------
update_shot:
        ; read joystick and isolate fire bit (bit4)
        lda JOY2
        and #$10              ; $10 = fire bit (1=released, 0=pressed)
        tax                   ; save current fire bit in X

        ; detect "new press": prev was released (nonzero), now pressed (zero)
        lda fire_prev
        beq us_prev_pressed   ; prev was 0 (pressed)
        cpx #$00
        bne us_no_new_press   ; still released -> no press
        ; NEW PRESS!
        lda shot_state
        bne us_no_new_press   ; already flying, ignore
        lda #1
        sta shot_state
        ; lock Y at the moment of fire
        lda yar_y
        sta shot_y
us_no_new_press:

us_prev_pressed:
        ; store current fire bit for next frame
        txa
        sta fire_prev

        lda shot_state
        bne us_flying

; --- READY: follow Yar ---
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

        ; if (shot >= SHOT_OFFSCREEN) reset
        lda shot_x_hi
        cmp #SHOT_OFFSCREEN_HI
        bcc us_done            ; hi < offscreen_hi => still on
        bne us_reset           ; hi > offscreen_hi => offscreen

        lda shot_x
        cmp #SHOT_OFFSCREEN_LO
        bcc us_done            ; lo < offscreen_lo => still on

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



read_joy_move_yar:
; left (bit2 low)
        lda JOY2
        and #%00000100
        bne no_left

        lda yar_x
        sec
        sbc #2
        sta yar_x
        bcs +
        dec yar_x_hi          ; borrow into MSB
+
; clamp min (24)
        lda yar_x_hi
        bpl +                ; avoid $FF going weird
        lda #PLAYER_MIN_X_HI
        sta yar_x_hi
        lda #PLAYER_MIN_X_LO
        sta yar_x
+
        lda yar_x_hi
        bne no_left           ; if hi=1, we're definitely > min
        lda yar_x
        cmp #PLAYER_MIN_X_LO
        bcs no_left
        lda #PLAYER_MIN_X_LO
        sta yar_x
no_left:

; right (bit3 low)
        lda JOY2
        and #%00001000
        bne no_right

        lda yar_x
        clc
        adc #2
        sta yar_x
        bcc +
        inc yar_x_hi          ; carry into MSB
+
; clamp max (343 = $0157)
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

        lda JOY2
        ; up (bit0 low)
        and #%00000001
        bne no_up
        lda yar_y
        sec
        sbc #2
        sta yar_y
no_up:

        lda JOY2
        ; down (bit1 low)
        and #%00000010
        bne no_down
        lda yar_y
        clc
        adc #2
        sta yar_y
no_down:
        rts

update_missile:
; --- X: move miss toward yar using 9-bit compare ---
        lda miss_x_hi
        cmp yar_x_hi
        bne mx_hi_diff

; hi equal, compare low
        lda miss_x
        cmp yar_x
        beq umx_done
        bcc umx_inc
        ; miss > yar
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
        bcc umx_inc      ; miss_hi < yar_hi => increase
        jmp umx_dec      ; miss_hi > yar_hi => decrease

umx_done:

; --- Y is unchanged ---
        lda miss_y
        cmp yar_y
        beq umy_done
        bcc umy_inc
        dec miss_y
        jmp umy_done
umy_inc: inc miss_y
umy_done:
        rts


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

         ; ---- build D010 MSB bits fresh ----
        lda #0

        ; sprite 0 (player)
        ldx yar_x_hi
        beq +
        ora #%00000001
+

        ; sprite 1 (enemy) - ENEMY_X is 264, so MSB=1
        ora #%00000010    ; bit1 set (since 264>255)

        ; sprite 2 (missile)
        ldx miss_x_hi
        beq +
        ora #%00000100
+

        ; sprite 3 (shot)
        ldx shot_x_hi
        beq +
        ora #%00001000         ; bit3
+

        sta $d010

        rts

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

; optional: tiny 0..15 offsets for rows; table just needs to exist
row_xor:
        .byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
        .byte 0,1,2,3,4,5,6,7,8
        ; (only first ~22 used)

; ------------------------------------------------------------
; Row->pointer for COLOR RAM: dst = COLORRAM + row*40
; Input: A=row (0..24)
; Output: dst_lo/dst_hi
; Clobbers: A,Y (preserves X)
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
        ; char 0
        .rept 8
        .byte 0
        .endrept

        ; char 1 (zone A) - multicolor noise
        .byte $1b,$e4,$b1,$4e,$d8,$27,$72,$8d

        ; char 2 (zone B) - different noise
        .byte $4e,$1b,$8d,$72,$27,$d8,$e4,$b1

        ; char 3 (zone C) - stripes/noise mix
        .byte $55,$aa,$1b,$e4,$55,$aa,$4e,$b1

        ; chars 4..9 empty (6 chars)
        .rept (6*8)
        .byte 0
        .endrept

        ; char 10 wall solid
        .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
        ; char 11
        .byte $ff,$ff,$ef,$ef,$ef,$ff,$ff,$ff
        ; char 12
        .byte $ff,$ef,$c7,$c7,$c7,$ef,$ff,$ff
        ; char 13
        .byte $ef,$c7,$83,$83,$83,$c7,$ef,$ff
        

        ; pad to 2048 bytes total (we've defined 14 chars = 112 bytes)
        .rept (2048 - (14*8))
        .byte 0
        .endrept

yar_x_hi:   .byte 0     ; 0 or 1
miss_x_hi:  .byte 0     ; 0 or 1
shot_x_hi:  .byte 0

zone_blank: .byte 0   ; 0=normal zone chars, 1=blank zone chars
zone_rowtmp: .byte 0

sprite_data:
        ; sprite 0 (yar)
        ; sprite 0 (player) - arrow/ship (hi-res)
        .byte $00,$00,$00
        .byte $00,$00,$00
        .byte $00,$00,$00

        .byte $03,$F0,$00
        .byte $07,$F8,$00
        .byte $0F,$FC,$00
        .byte $1F,$FE,$00
        .byte $3F,$FF,$00
        .byte $7F,$FF,$80
        .byte $FF,$FF,$C0
        .byte $FF,$FF,$E0
        .byte $FF,$FF,$F0
        .byte $FF,$FF,$E0
        .byte $FF,$FF,$C0
        .byte $7F,$FF,$80
        .byte $3F,$FF,$00
        .byte $1F,$FE,$00
        .byte $0F,$FC,$00
        .byte $07,$F8,$00
        .byte $03,$F0,$00

        .byte $00,$00,$00
        .byte $00,$00,$00
        .byte $00,$00,$00
        .byte 0

        ; sprite 1 (qotile)
        .rept 21
        .byte %00100100, %00011000, %00100100
        .endrept
        .byte 0

        ; sprite 2 (missile)
        .rept 21
        .byte %00011000, %00011000, %00011000
        .endrept
        .byte 0

        ; sprite 3 (shot)
        .rept 21
        .byte %00000000, %00011000, %00000000
        .endrept
        .byte 0
