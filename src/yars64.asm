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

CHAR_DST        = $2800
SPR_DST         = $3000

SPR_PTRS        = SCREEN + $03f8

JOY2            = $dc00

RASTER_FRAME    = 0
RASTER_ZONE_ON  = 70
RASTER_ZONE_OFF = 170

ZONE_ROW_TOP    = 0
ZONE_ROW_BOTTOM = 25    ; loop ends when X == 25

ZONE_COL_L      = 12
ZONE_COL_R      = 19

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

MISSILE_SLOW_MASK = 2

SHOT_START_X     = 24
SHOT_SPEED       = 4
SHOT_OFFSCREEN_LO = $68    ; 360 = $0168
SHOT_OFFSCREEN_HI = 1

BARRIER_ROW   = 5
BARRIER_COL   = 29
BARRIER_COLOR = 2

BARRIER_H      = 12
BARRIER_W      = 10
BARRIER_THICK  = 3

PLAYER_MIN_X_LO = $18   ; 24
PLAYER_MIN_X_HI = 0
PLAYER_MAX_X_LO = $57   ; 343-256 = 87
PLAYER_MAX_X_HI = 1

ZONE_BLANK_MASK = 2

; ---- Game modes ----
GAME_PLAY    = 0
GAME_EXPLODE = 1     ; big screen explosion
GAME_SPIN    = 2     ; missile-hit spin + freeze
GAME_TITLE   = 3


; ---- Spin death tuning ----
SPIN_STEP_MASK    = 3      ; rotate every 4 frames (0..3)
SPIN_WAIT_FRAMES  = 60     ; ~1 second at 60Hz


TEXT_X0 = 24
TEXT_Y0 = 50

; ----- Player vs barrier collision sampling offsets -----
PLY_COL_L_XOFF   = 2
PLY_COL_R_XOFF   = 23      ; lets you get 1-2 px closer before blocking
PLY_COL_MID_XOFF = 12

PLY_COL_TOP_YOFF = 3
PLY_COL_MID_YOFF = 10
PLY_COL_BOT_YOFF = 18

; ----- Unlock sample (touch point on player sprite) -----
; Sample a little ahead of the player's right edge so it can "touch" while still blocked from overlapping.
PLAYER_UNLOCK_X_OFF = (PLY_COL_R_XOFF + 2)   ; 23+2 = 25
PLAYER_UNLOCK_Y_OFF = 10

BULLET_HIT_X_OFF    = 12
BULLET_HIT_Y_OFF    = 10



; sprite pointer values (bank 0, sprite data at $3000)
SPR0_PTR_RIGHT  = $c0
SPR0_PTR_UP     = $c1
SPR0_PTR_DOWN   = $c2
SPR0_PTR_LEFT   = $c3
SPR1_PTR_ALIEN  = $c4
SPR2_PTR_MISS   = $c5
SPR3_PTR_SHOT   = $c6
SPR4_PTR_BULLET = $c8

SPR1_PTR_ALIEN_DASH = $c7     ; new dash frame (9th sprite at $3000 + 8*64)


; --- Lose/Explosion constants ---
EXPLODE_MID_ROW      = 12
EXPLODE_MAX_RADIUS   = 12
EXPLODE_WAIT_FRAMES  = 45
COLLIDE_X_THRESH     = 24
COLLIDE_Y_THRESH     = 21

MISS_HIT_X_OFF      = 12
MISS_HIT_Y_OFF      = 10
PLAYER_HIT_X_OFF    = 12
PLAYER_HIT_Y_OFF    = 10

COLLIDE_MISS_X_THR  = 8
COLLIDE_MISS_Y_THR  = 8

PMISS_HIT_X_OFF    = 12     ; tip of the big missile sprite
PMISS_HIT_Y_OFF    = 10     ; center-ish

ENEMY_HIT_X_OFF      = 12
ENEMY_HIT_Y_OFF      = 10

COLLIDE_PMISS_X_THR  = 10
COLLIDE_PMISS_Y_THR  = 8

; ---- Enemy dash attack ----
ENEMY_DASH_SPEED_X  = 6
ENEMY_DASH_SPEED_Y  = 3


; ---- Directions ----
DIR_RIGHT = 0
DIR_UP    = 1
DIR_DOWN  = 2
DIR_LEFT  = 3

BULLET_SPEED     = 5
BULLET_OFF_Y     = 250
BULLET_OFF_X     = 0

; ---- Player missile (left-side, post-unlock) ----
PMISS_START_X_LO = PLAYER_MIN_X_LO
PMISS_START_X_HI = PLAYER_MIN_X_HI
PMISS_SPEED      = 5
PMISS_OFFSCREEN_LO = $68
PMISS_OFFSCREEN_HI = 1

BARRIER_BOUNCES_TO_NIBBLE = 6  

SID = $d400

; Voice 1 regs ($d400..$d406)
V1_FREQ_LO = SID+$00
V1_FREQ_HI = SID+$01
V1_PW_LO   = SID+$02
V1_PW_HI   = SID+$03
V1_CTRL    = SID+$04
V1_AD      = SID+$05
V1_SR      = SID+$06


; Voice 2 regs ($d407..$d40d)
V2_FREQ_LO = SID+$07
V2_FREQ_HI = SID+$08
V2_PW_LO   = SID+$09
V2_PW_HI   = SID+$0a
V2_CTRL    = SID+$0b
V2_AD      = SID+$0c
V2_SR      = SID+$0d

V3_FREQ_LO = SID+$0e
V3_FREQ_HI = SID+$0f
V3_PW_LO   = SID+$10
V3_PW_HI   = SID+$11
V3_CTRL    = SID+$12
V3_AD      = SID+$13
V3_SR      = SID+$14

SID_FC_LO  = SID+$15
SID_FC_HI  = SID+$16
SID_RESFLT = SID+$17
SID_MODEVOL= SID+$18

GATE  = $01
TRI   = $10
NOISE = $80
PULSE = $40



; ----------------------------
; Game variables (zero page)
; ----------------------------
yar_x   = $f0
yar_y   = $f1
miss_x  = $f2
miss_y  = $f3

shot_x   = $f7
shot_y   = $f8

shot_state  = $f9
fire_prev   = $fa

joy_state   = $ef

; pointers must be ZP for (zp),y
src_lo      = $fb
src_hi      = $fc
dst_lo      = $fd
dst_hi      = $fe
pages_left  = $ff

irq_state     = $f4
frame_counter = $f5
zone_phase    = $f6

; ------------------------------------------------------------
start:
        sei

        lda #$2f
        sta $00

        lda #$35
        sta $01

        jsr reset_level_vars

        ; VIC bank 0 ($0000-$3fff). CIA2 bits 0-1 are inverted: bank0 => %11
        lda CIA2
        and #%11111100
        ora #%00000011
        sta CIA2

        ; screen=$0400, charset=$2000 -> D018 = $18
        lda #$1a
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
        sta $d021
        sta $d020
        lda #12
        sta $d022
        lda #11
        sta $d023

        ; copy assets
        jsr copy_charset
        jsr copy_sprites

        jsr sound_init

        ; install IRQ (it enables interrupts inside; we'll re-SEI briefly)
        jsr init_irq
        sei

        ; show title screen (sprites off, ROM charset)
        jsr draw_title_screen

        cli

mainloop:
        lda game_mode
        cmp #GAME_TITLE
        bne mainloop
        jsr title_poll_fire
        jmp mainloop

draw_title_screen:
        lda #GAME_TITLE
        sta game_mode

        ; use ROM charset at $1000 (uppercase/graphics)
        ; screen at $0400 => $10, charset $1000 => $04, total $14
        lda #$14
        sta $d018

        ; sprites OFF
        lda #0
        sta $d015

        jsr clear_screen

        ; --- "YARS REVENGE" (white) ---
        lda #1
        sta title_color

        lda #<str_title
        sta src_lo
        lda #>str_title
        sta src_hi
        lda #11                 ; row
        ldx #13                 ; col ((40-12)/2)
        jsr put_line

        ; --- "BY SCOTT HUTTER AND CHATGPT" (white) ---
        lda #<str_byline
        sta src_lo
        lda #>str_byline
        sta src_hi
        lda #13                 ; row (skip one line)
        ldx #6                  ; col ((40-27)/2)
        jsr put_line

        ; --- "JOYSTICK 2 - FIRE TO BEGIN" (red) ---
        lda #2                  ; red
        sta title_color

        lda #<str_press
        sta src_lo
        lda #>str_press
        sta src_hi
        lda #16                 ; row (two lines below byline)
        ldx #6
        jsr put_line

        ; ensure title fire edge starts "not pressed"
        lda #$10
        sta fire_prev

        rts

title_poll_fire:
        lda JOY2
        and #$10
        tax                     ; X = $10 not pressed, 0 pressed

        lda fire_prev
        beq tp_prev_pressed     ; prev was pressed

        cpx #0
        bne tp_store            ; still not pressed

        ; NEW press -> start game
        jsr title_start_game
        jmp tp_store

tp_prev_pressed:
        ; if still pressed, do nothing

tp_store:
        txa
        sta fire_prev
        rts

title_start_game:
        ; switch back to your game charset at $2800 ($d018=$1a)
        lda #$1a
        sta $d018

        jsr restart_level       ; resets vars, draws playfield, inits sprites

        ; restart_level already sets game_mode to play via reset_level_vars
        rts


put_line:
        sta tmp_row          ; (already exists in your program)
        stx title_col

        ; --- write characters to SCREEN ---
        lda tmp_row
        jsr row_to_ptr

        clc
        lda dst_lo
        adc title_col
        sta dst_lo
        bcc +
        inc dst_hi
+

        ldy #0
pl_char_loop:
        lda (src_lo),y
        beq pl_chars_done

        ; convert 'A'..'Z' to screen codes 1..26
        cmp #$41
        bcc pl_store
        cmp #$5B
        bcs pl_store
        and #$1f

pl_store:
        sta (dst_lo),y
        iny
        bne pl_char_loop

pl_chars_done:
        tya
        tax                     ; X = length

        ; --- write colors to COLORRAM ---
        lda tmp_row
        jsr row_to_color_ptr

        clc
        lda dst_lo
        adc title_col
        sta dst_lo
        bcc +
        inc dst_hi
+

        ldy #0
        lda title_color
        cpx #0
        beq pl_done

pl_col_loop:
        sta (dst_lo),y
        iny
        dex
        bne pl_col_loop

pl_done:
        rts

BUZZ_LEN = 3        ; how long the bzzzt lasts (0..15). try 2..4

sound_update:
        lda game_mode
        cmp #GAME_PLAY
        beq su_play

        cmp #GAME_EXPLODE
        beq su_explode

        ; other modes (spin/title): silence everything
        lda #0
        sta V1_CTRL
        sta V2_CTRL
        sta V3_CTRL
        lda #0
        sta buzz_on
        sta pew_timer
        sta expl_timer
        sta pmiss_snd_timer
        rts

su_explode:
        ; keep the boom alive during explosion frames
        jsr explosion_sound_update

        ; optional: keep hum/buzz off during boom
        lda #0
        sta V2_CTRL
        sta V3_CTRL
        lda #0
        sta buzz_on
        sta pew_timer
        sta pmiss_snd_timer
        rts


su_play:
        jsr explosion_sound_update

        ; keep hum alive
        lda V3_CTRL
        and #GATE
        bne hum_ok
        lda #(TRI|GATE)
        sta V3_CTRL
hum_ok:

        ; ---- run rocket sound on V2 (and suppress buzz while active) ----
        jsr pmiss_sound_update
        lda pmiss_snd_timer
        bne su_done            ; <-- NEW: if rocket playing, don't run buzz logic

        ; ---- existing buzz logic below stays as-is ----
        lda frame_counter
        and #$0f
        cmp #BUZZ_LEN
        bcc want_buzz

want_no_buzz:
        lda buzz_on
        beq su_done
        lda #0
        sta V2_CTRL          ; gate off
        lda #0
        sta buzz_on
        jmp su_done

want_buzz:
        lda buzz_on
        bne buzz_live

        ; --- start buzz (Voice 2 pulse) ---
        lda #0
        sta V2_CTRL

        lda #$00
        sta V2_PW_LO
        lda #$08
        sta V2_PW_HI          ; narrow-ish pulse

        lda #$00
        sta V2_FREQ_LO
        lda #$0c
        sta V2_FREQ_HI        ; LOWER buzz pitch (try $09..$0f)

        lda #$c0
        sta V2_AD             ; fast attack
        lda #$01
        sta V2_SR             ; loud sustain

        lda #(PULSE|GATE)
        sta V2_CTRL

        lda #1
        sta buzz_on
        jmp su_done

buzz_live:
        ; tiny wobble so it feels “electrical” without being high
        lda frame_counter
        and #1
        beq buzz_a
buzz_b:
        lda #$0d
        sta V2_FREQ_HI
        jmp su_done
buzz_a:
        lda #$0b
        sta V2_FREQ_HI
        jmp su_done

su_done:

        ; ---- PEW update (Voice 1) ----
        lda pew_timer
        beq pew_done

        ; index = pew_timer-1 (0..PEW_LEN-1)
        sec
        sbc #1
        tax

        lda #$00
        sta V1_FREQ_LO
        lda pew_freq_hi_tbl, x
        sta V1_FREQ_HI

        dec pew_timer
        bne pew_done

        ; timer hit 0 -> gate off
        lda #0
        sta V1_CTRL

pew_done:
        rts


pew_start:
        lda expl_timer
        bne +       ; explosion active -> don’t start other V1 sounds

        ; restart the pew every time you fire
        lda #PEW_LEN
        sta pew_timer

        ; pulse width (about 50%)
        lda #$00
        sta V1_PW_LO
        lda #$08
        sta V1_PW_HI

        ; fast envelope (short blip)
        lda #$02        ; A=0, D=2
        sta V1_AD
        lda #$02        ; S=0, R=2
        sta V1_SR

        ; start at first pitch (table[0])
        lda #$00
        sta V1_FREQ_LO
        lda pew_freq_hi_tbl
        sta V1_FREQ_HI

        lda #(PULSE|GATE)
        sta V1_CTRL
+
        rts

explosion_sound_start:

        lda #120               ; ~2 seconds at 60Hz
        sta expl_timer

        lda #$0f               ; volume max (no filter bits)
        sta SID_MODEVOL

        lda #0
        sta SID_RESFLT         ; no filter routing
        sta SID_FC_LO
        lda #$ff
        sta SID_FC_HI

        lda #0
        sta V1_CTRL            ; reset voice 1

        lda #$0f               ; A=0, D=F (slow decay)
        sta V1_AD
        lda #$f8               ; S=F, R=8 (tail)
        sta V1_SR

        lda #$00
        sta V1_FREQ_LO
        lda #$40               ; IMPORTANT: higher noise clock => audible
        sta V1_FREQ_HI

        lda #(NOISE|GATE)
        sta V1_CTRL
        rts


explosion_sound_update:
        lda expl_timer
        beq esu_done

        ; optional falling noise-rate for “boom”
        lda expl_timer
        lsr
        lsr
        ora #$10               ; don’t let it go too low
        sta V1_FREQ_HI

        dec expl_timer
        bne esu_done

        lda #0
        sta V1_CTRL
esu_done:
        rts


explosion_sound_stop:
        lda #0
        sta expl_timer
        lda #0
        sta V1_CTRL

        ; reset filter too
        lda #$00
        sta SID_RESFLT
        sta SID_FC_LO
        lda #$ff
        sta SID_FC_HI

        lda #12
        sta SID_MODEVOL
        rts

pmiss_sound_start:
    lda expl_timer
    bne +                   ; explosion active -> don’t start

    lda #PMISS_SND_LEN
    sta pmiss_snd_timer

    ; While rocket is playing, kill buzz on Voice 2
    lda #0
    sta V2_CTRL
    sta buzz_on

    ; Voice 2: pulse “rocket”
    lda #0
    sta V2_CTRL

    ; ~50% duty pulse
    lda #$00
    sta V2_PW_LO
    lda #$08
    sta V2_PW_HI

    ; ADSR: fast attack, max sustain, moderate release
    lda #$02        ; A=0, D=2
    sta V2_AD
    lda #$F6        ; S=15, R=6
    sta V2_SR

    ; base pitch ~370Hz  (word $17B6)
    lda #$B6
    sta V2_FREQ_LO
    lda #$17
    sta V2_FREQ_HI

    lda #(PULSE|GATE)
    sta V2_CTRL
+
    rts


pmiss_sound_update:
    lda pmiss_snd_timer
    beq psu_done

    ; tiny vibrato (4-step)
    lda frame_counter
    and #3
    tax
    lda pmiss_vib_lo, x
    sta V2_FREQ_LO
    lda pmiss_vib_hi, x
    sta V2_FREQ_HI

    dec pmiss_snd_timer
    bne psu_done

    ; finished -> gate off voice 2
    lda #0
    sta V2_CTRL
    lda #0
    sta buzz_on

psu_done:
    rts


pmiss_sound_stop:
    lda #0
    sta pmiss_snd_timer
    lda #0
    sta V2_CTRL
    sta buzz_on
    rts



; ------------------------------------------------------------
; Reset gameplay vars
; ------------------------------------------------------------
reset_level_vars:
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

        lda #SHOT_START_X
        sta shot_x
        lda yar_y
        sta shot_y
        lda #0
        sta shot_state

        lda #$10
        sta fire_prev

        lda #0
        sta zone_phase
        sta irq_state
        sta frame_counter
        sta zone_blank

        lda #0
        sta game_mode
        sta explode_radius
        sta explode_wait

        lda #DIR_RIGHT
        sta player_dir

        lda #0
        sta bullet_state
        sta bullet_dir
        sta bullet_x_hi
        lda #BULLET_OFF_X
        sta bullet_x
        lda #BULLET_OFF_Y
        sta bullet_y

        lda #0
        sta pmiss_unlocked
        sta pmiss_state
        lda #PMISS_START_X_LO
        sta pmiss_x
        lda #PMISS_START_X_HI
        sta pmiss_x_hi
        lda yar_y
        sta pmiss_y

        lda #0
        sta enemy_state
        jsr enemy_set_delay


        rts

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
; Copy sprite data to $3000 (2 pages = 512 bytes)
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

        lda #3
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

sound_init:

        lda #12
        sta SID_MODEVOL

        ; --- RESET FILTER (so pew isn't muffled/silenced after explosion) ---
        lda #$00
        sta SID_RESFLT      ; $d417: no voices routed through filter, res=0
        sta SID_FC_LO       ; $d415
        lda #$ff
        sta SID_FC_HI       ; $d416: wide open cutoff

        lda SID_MODEVOL
        sta sid_mode_save


        ; ---- Voice 1: PEW off initially ----
        lda #0
        sta V1_CTRL
        lda #0
        sta pew_timer
        sta pmiss_snd_timer


        ; ---- Voice 3: constant HUM (triangle) ----
        lda #0
        sta V3_CTRL

        lda #$00
        sta V3_FREQ_LO
        lda #$06          ; hum pitch (raise to $07 if you want)
        sta V3_FREQ_HI

        lda #$22
        sta V3_AD
        lda #$56          ; a little stronger sustain than before
        sta V3_SR

        lda #(TRI|GATE)
        sta V3_CTRL

        ; ---- Voice 2: BUZZ off initially ----
        lda #0
        sta V2_CTRL
        lda #0
        sta buzz_on

        rts




hum_start:
        lda hum_state
        bne hs_done

        ; Voice 3: triangle + gate
        ; Frequency word: tweak to taste (lower = deeper)
        lda #$00
        sta V3_FREQ_LO
        lda #$07
        sta V3_FREQ_HI

        ; Pulse width not used for triangle, but set anyway
        lda #$00
        sta V3_PW_LO
        lda #$08
        sta V3_PW_HI

        ; ADSR: slow-ish attack, no decay, low sustain, gentle release
        lda #$24          ; A=2, D=4
        sta V3_AD
        lda #$68          ; S=6, R=8
        sta V3_SR

        lda #$11          ; %00010001 = TRIANGLE($10) + GATE($01)
        sta V3_CTRL

        lda #1
        sta hum_state
hs_done:
        rts


hum_stop:
        lda hum_state
        beq hst_done

        ; clear GATE (leave waveform bit alone or clear all)
        lda #$10          ; triangle, gate off
        sta V3_CTRL

        lda #0
        sta hum_state
hst_done:
        rts


; ------------------------------------------------------------
; Thick "<" barrier (hollow wedge)
; ------------------------------------------------------------
draw_barrier:
        ldx #0
db_row:
        cpx #BARRIER_H
        beq db_done

        txa
        clc
        adc #BARRIER_ROW
        jsr row_to_ptr

        lda barrier_left, x
        clc
        adc #BARRIER_COL
        sta bar_left

        lda #BARRIER_COL + (BARRIER_W-1)
        sta bar_right

        cpx #BARRIER_THICK
        bcc db_horiz

        lda #BARRIER_H - BARRIER_THICK
        cpx #(BARRIER_H - BARRIER_THICK)
        bcs db_horiz

db_sides:
        ldy bar_left
        lda #CH_WALL_SOLID
        sta (dst_lo),y
        iny
        sta (dst_lo),y

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

; ------------------------------------------------------------
; Fill entire row (0..39) with repeating zone chars 1,2,3,2
; Input: A=row (0..24)
; ------------------------------------------------------------
fill_row_zone_full:
        pha
        jsr row_to_ptr
        pla

        ldy #0
        ldx #0
fzz_loop:
        lda zone_chars,x
        sta (dst_lo),y
        iny
        cpy #40
        beq fzz_done
        inx
        cpx #4
        bne fzz_loop
        ldx #0
        jmp fzz_loop
fzz_done:
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
        ; sprite pointers
        lda #SPR0_PTR_RIGHT
        sta SPR_PTRS+0
        lda #SPR1_PTR_ALIEN
        sta SPR_PTRS+1

        ; sprite2 = enemy missile (SMALL)
        lda #SPR2_PTR_MISS
        sta SPR_PTRS+2

        ; sprite3 = player bullet
        lda #SPR4_PTR_BULLET
        sta SPR_PTRS+3

        ; sprite4 = player missile (LARGE)
        lda #SPR3_PTR_SHOT
        sta SPR_PTRS+4

        ; enable some sprites initially (write_positions manages dynamically too)
        lda #%00001111
        sta $d015

        ; sprite 1 behind background
        lda #%00000010
        sta $d01b

        ; ALL sprites multicolor (0..4)
        lda #%00011111
        sta $d01c

        ; shared multicolor
        lda #11
        sta $d025
        lda #12
        sta $d026

        ; per-sprite colors
        lda #1
        sta $d027
        lda #13
        sta $d028
        lda #13
        sta $d029
        lda #1
        sta $d02a
        lda #1
        sta $d02b

        ; positions
        lda yar_x
        sta $d000
        lda yar_y
        sta $d001

        lda #0
        sta $d006
        sta $d007

        lda pmiss_x
        sta $d008
        lda pmiss_y
        sta $d009

        lda miss_x
        sta $d004
        lda miss_y
        sta $d005

        ; enemy sprite (sprite 1) inside the barrier opening
        ENEMY_X = (BARRIER_COL+10)*8
        ENEMY_Y = (BARRIER_ROW+11)*8

        lda #<ENEMY_X
        sta $d002
        sta enemy_x_lo

        lda #>ENEMY_X
        and #$01
        sta enemy_x_hi

        ; set VIC high-bit for sprite1 (bit 1 in $d010)
        lda $d010
        and #%11111101          ; clear bit 1
        ora #(((ENEMY_X >> 8) & 1) << 1)
        sta $d010

        lda #ENEMY_Y
        sta $d003
        sta enemy_y_val

        ; after setting enemy_x_lo/enemy_x_hi/enemy_y_val...
        lda enemy_x_lo
        sta enemy_home_x_lo
        lda enemy_x_hi
        sta enemy_home_x_hi
        lda enemy_y_val
        sta enemy_home_y_val

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
        ; normal zone blank toggle only during play
        lda game_mode
        bne +

        lda frame_counter
        and #ZONE_BLANK_MASK
        bne +
        lda zone_blank
        eor #$01
        sta zone_blank
        jsr zone_update_chars
+

        ; shimmer selection
        lda game_mode
        beq do_zone_shim          ; play -> zone shimmer
        cmp #GAME_EXPLODE
        beq do_explode_shim       ; explode -> explosion shimmer
        jmp do_update             ; spin -> no shimmer (frozen)

do_zone_shim:
        jsr zone_shimmer
        jmp do_update

do_explode_shim:
        jsr explosion_shimmer

do_update:

        lda game_mode
        beq do_game               ; play
        cmp #GAME_EXPLODE
        beq do_explode_update     ; big explosion
        cmp #GAME_SPIN
        beq do_spin_update        ; spin death
        ; else GAME_TITLE (or anything else)
        jmp after_update

do_spin_update:
        jsr death_spin_update
        jmp after_update

do_explode_update:
        jsr explosion_update
        jmp after_update

do_game:
        jsr game_update

after_update:
        jsr sound_update

                ; --------------------------------------------------------
        ; IRQ scheduling:
        ; - In GAME_PLAY: keep the 3-stage raster scheme (zone on/off)
        ; - In other modes (EXPLODE/SPIN/TITLE): single IRQ per frame
        ;   to prevent IRQ "pileups" that speed up expl_timer.
        ; --------------------------------------------------------
        lda game_mode
        cmp #GAME_PLAY
        beq schedule_zone_irqs

        ; single IRQ per frame at raster 0
        lda #0
        sta irq_state
        lda #RASTER_FRAME
        sta $d012
        jmp irq_exit

schedule_zone_irqs:
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

game_update:
        jsr read_joy_move_yar

        jsr update_enemy_attack 

        jsr check_unlock_player_missile
        jsr update_player_fire
        jsr check_bullet_barrier_hit
        jsr check_pmiss_enemy_hit

        ; collision: enemy hits player => lose
        jsr check_player_enemy_collision

        ; collision: enemy missile hits player => spin death
        jsr check_player_missile_collision

        ; NEW: collision: player missile hits player => spin death
        jsr check_player_pmiss_collision

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
; Unlock player missile when player touches barrier wall char (10..13)
; Robust: samples 3 points vertically (top/mid/bot) along right edge.
; FIXED: preserves X sample-index across wall_at_point (which clobbers X)
; ------------------------------------------------------------
check_unlock_player_missile:
        lda pmiss_unlocked
        bne cul_done

        ldx #0
cul_try:
        txa
        pha                     ; save sample index (0..2)

        ; X sample point (a bit past right edge)
        lda yar_x
        clc
        adc #PLAYER_UNLOCK_X_OFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi

        ; Y sample point = yar_y + unlock_yoffs[index]
        pla
        tay                     ; Y = index
        tya
        pha                     ; keep index on stack again

        lda yar_y
        clc
        adc unlock_yoffs, y
        sta tmp2_lo

        jsr wall_at_point       ; C=1 if wall, X=row, tmp1_lo=col
        stx tmp_row             ; save row (since we must restore index)

        pla
        tax                     ; restore sample index into X

        bcc cul_next            ; not wall => next sample

        ; constrain to barrier rectangle using tmp_row + tmp1_lo
        lda tmp_row
        cmp #BARRIER_ROW
        bcc cul_next
        cmp #(BARRIER_ROW + BARRIER_H)
        bcs cul_next

        lda tmp1_lo
        cmp #BARRIER_COL
        bcc cul_next
        cmp #(BARRIER_COL + BARRIER_W)
        bcs cul_next

        ; -------- UNLOCK! --------
        lda #1
        sta pmiss_unlocked
        lda #0
        sta pmiss_state

        lda #PMISS_START_X_LO
        sta pmiss_x
        lda #PMISS_START_X_HI
        sta pmiss_x_hi
        lda yar_y
        sta pmiss_y

cul_done:
        rts

cul_next:
        inx
        cpx #3
        bne cul_try
        rts

unlock_yoffs:
        .byte (PLAYER_UNLOCK_Y_OFF-6), (PLAYER_UNLOCK_Y_OFF), (PLAYER_UNLOCK_Y_OFF+6)


; ------------------------------------------------------------
update_player_fire:
        ; edge-detect fire
        lda JOY2
        and #$10
        tax

        lda fire_prev
        beq upf_prev_pressed
        cpx #$00
        bne upf_no_new_press

        ; NEW press detected here -----------------

        ; NEW: no firing allowed while inside shimmer zone
        jsr player_in_zone
        bcc fire_ok
        jmp upf_no_new_press
fire_ok:

        lda pmiss_unlocked
        bne upf_fire_pmiss


        ; ---- fire BULLET (sprite3) if inactive ----
        lda bullet_state
        bne upf_no_new_press

        lda #1
        sta bullet_state
        lda player_dir
        sta bullet_dir

        jsr pew_start

        lda yar_x
        sta bullet_x
        lda yar_x_hi
        sta bullet_x_hi
        lda yar_y
        sta bullet_y
        jmp upf_no_new_press


upf_fire_pmiss:
        ; ---- fire LEFT MISSILE (sprite4) if parked ----
        lda pmiss_state
        bne upf_no_new_press

        lda #1
        sta pmiss_state
        lda #PMISS_START_X_LO
        sta pmiss_x
        lda #PMISS_START_X_HI
        sta pmiss_x_hi
        lda yar_y
        sta pmiss_y

        jsr pmiss_sound_start

        jmp upf_no_new_press


upf_no_new_press:
upf_prev_pressed:
        txa
        sta fire_prev

        ; ---- update bullet motion ----
        lda bullet_state
        beq upf_bullet_done

        lda bullet_dir
        cmp #DIR_RIGHT
        beq bdir_right
        cmp #DIR_LEFT
        beq bdir_left
        cmp #DIR_UP
        beq bdir_up
        ; else down
bdir_down:
        lda bullet_y
        clc
        adc #BULLET_SPEED
        sta bullet_y
        cmp #240
        bcc upf_bullet_done
        jmp bullet_reset

bdir_up:
        lda bullet_y
        sec
        sbc #BULLET_SPEED
        sta bullet_y
        bcs upf_bullet_done
        jmp bullet_reset

bdir_left:
        lda bullet_x
        sec
        sbc #BULLET_SPEED
        sta bullet_x
        bcs upf_bullet_done
        dec bullet_x_hi
        lda bullet_x_hi
        bmi bullet_reset
        jmp upf_bullet_done

bdir_right:
        lda bullet_x
        clc
        adc #BULLET_SPEED
        sta bullet_x
        bcc +
        inc bullet_x_hi
+
        lda bullet_x_hi
        cmp #SHOT_OFFSCREEN_HI
        bcc upf_bullet_done
        bne bullet_reset
        lda bullet_x
        cmp #SHOT_OFFSCREEN_LO
        bcc upf_bullet_done

bullet_reset:
        lda #0
        sta bullet_state
        lda #BULLET_OFF_X
        sta bullet_x
        lda #0
        sta bullet_x_hi
        lda #BULLET_OFF_Y
        sta bullet_y

upf_bullet_done:

                ; ---- update player missile motion ----
        lda pmiss_state
        beq upf_done

        ; missile flies RIGHT
        lda pmiss_x
        clc
        adc #PMISS_SPEED
        sta pmiss_x
        bcc +
        inc pmiss_x_hi
+
        ; NEW: if missile hits barrier, make it disappear (same as offscreen)
        jsr check_pmiss_barrier_hit
        lda pmiss_state
        beq upf_done

        ; existing offscreen check
        lda pmiss_x_hi
        cmp #PMISS_OFFSCREEN_HI
        bcc upf_done
        bne pmiss_reset
        lda pmiss_x
        cmp #PMISS_OFFSCREEN_LO
        bcc upf_done


pmiss_reset:
        lda #0
        sta pmiss_state

        ; disable / re-lock the player missile so we go back to bullets
        sta pmiss_unlocked

        jsr pmiss_sound_stop

        lda #PMISS_START_X_LO
        sta pmiss_x
        lda #PMISS_START_X_HI
        sta pmiss_x_hi
        lda yar_y
        sta pmiss_y

upf_done:
        rts

; ------------------------------------------------------------
; player_overlap_wall
;   checks 8 sample points around player sprite bbox
;   Output: C=1 if overlapping wall (10..13), else C=0
;   Uses: tmp1_lo/tmp1_hi = X (16-bit), tmp2_lo = Y (8-bit)
;   NEW: records hit tile in wall_hit_row / wall_hit_col on first hit
; ------------------------------------------------------------
player_overlap_wall:

        ; TL
        lda yar_x
        clc
        adc #PLY_COL_L_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_TOP_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +                  ; short branch
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next1

pow_next1:
        ; TM
        lda yar_x
        clc
        adc #PLY_COL_MID_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_TOP_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next2

pow_next2:
        ; TR
        lda yar_x
        clc
        adc #PLY_COL_R_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_TOP_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next3

pow_next3:
        ; ML
        lda yar_x
        clc
        adc #PLY_COL_L_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_MID_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next4

pow_next4:
        ; MR
        lda yar_x
        clc
        adc #PLY_COL_R_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_MID_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next5

pow_next5:
        ; BL
        lda yar_x
        clc
        adc #PLY_COL_L_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_BOT_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next6

pow_next6:
        ; BM
        lda yar_x
        clc
        adc #PLY_COL_MID_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_BOT_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc +
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts
+       jmp pow_next7

pow_next7:
        ; BR
        lda yar_x
        clc
        adc #PLY_COL_R_XOFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi
        lda yar_y
        clc
        adc #PLY_COL_BOT_YOFF
        sta tmp2_lo
        jsr wall_at_point
        bcc pow_clear
        stx wall_hit_row
        lda tmp1_lo
        sta wall_hit_col
        sec
        rts

pow_clear:
        clc
        rts


; ------------------------------------------------------------
; wall_at_point
;   Input:
;     tmp1_lo/tmp1_hi = X pixel (16-bit)
;     tmp2_lo         = Y pixel
;   Output:
;     C=1 if wall char (10..13), else C=0
;   Leaves:
;     X = row, tmp1_lo = col (0..39)
; ------------------------------------------------------------
wall_at_point:
        ; ---- convert Y pixel -> row ----
        lda tmp2_lo
        sec
        sbc #TEXT_Y0
        bcc wap_not
        lsr
        lsr
        lsr
        tax
        cpx #25
        bcs wap_not

        ; ---- convert X pixel -> col ----
        sec
        lda tmp1_lo
        sbc #TEXT_X0
        sta tmp1_lo
        lda tmp1_hi
        sbc #0
        sta tmp1_hi
        bmi wap_not

        ldy #3
wap_div8:
        lsr tmp1_hi
        ror tmp1_lo
        dey
        bne wap_div8

        ; IMPORTANT: row_to_ptr clobbers Y, so do it BEFORE loading Y
        txa
        jsr row_to_ptr          ; sets dst_lo/dst_hi to start of row

        ldy tmp1_lo             ; column
        cpy #40
        bcs wap_not

        lda (dst_lo),y

        cmp #CH_WALL_SOLID
        bcc wap_not
        cmp #CH_WALL_DMG3+1
        bcs wap_not

        sec
        rts
wap_not:
        clc
        rts

; ------------------------------------------------------------
; player_in_zone
;   Uses player hit point to see if player is inside shimmer zone
;   Output: C=1 if in zone, C=0 if not
; ------------------------------------------------------------
player_in_zone:
        ; --- row from (yar_y + PLAYER_HIT_Y_OFF) ---
        lda yar_y
        clc
        adc #PLAYER_HIT_Y_OFF
        sec
        sbc #TEXT_Y0
        bcc piz_not

        lsr
        lsr
        lsr
        tax                         ; X = row (0..)

        cpx #ZONE_ROW_TOP
        bcc piz_not
        cpx #ZONE_ROW_BOTTOM        ; 25
        bcs piz_not

        ; --- col from 16-bit (yar_x + PLAYER_HIT_X_OFF) ---
        lda yar_x
        clc
        adc #PLAYER_HIT_X_OFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi

        sec
        lda tmp1_lo
        sbc #TEXT_X0
        sta tmp1_lo
        lda tmp1_hi
        sbc #0
        sta tmp1_hi
        bmi piz_not

        ldy #3
piz_div8:
        lsr tmp1_hi
        ror tmp1_lo
        dey
        bne piz_div8

        lda tmp1_lo                 ; A = col (0..39)
        cmp #ZONE_COL_L
        bcc piz_not
        cmp #(ZONE_COL_R+1)
        bcs piz_not

        sec
        rts

piz_not:
        clc
        rts


; ------------------------------------------------------------
; Joystick movement + sprite0 direction swap (RIGHT/UP/DOWN/LEFT)
; HARD BLOCK: cannot overlap barrier/wall chars 10..13
; ------------------------------------------------------------
read_joy_move_yar:
        lda JOY2
        sta joy_state

        ; save old position (for collision revert)
        lda yar_x
        sta old_yar_x
        lda yar_x_hi
        sta old_yar_x_hi
        lda yar_y
        sta old_yar_y

; left (bit2 low)
        lda joy_state
        and #%00000100
        bne no_left

        lda yar_x
        sec
        sbc #4
        sta yar_x
        bcs +
        dec yar_x_hi
+
no_left:

; right (bit3 low)
        lda joy_state
        and #%00001000
        bne no_right

        lda yar_x
        clc
        adc #4
        sta yar_x
        bcc +
        inc yar_x_hi
+
no_right:

; up (bit0 low)
        lda joy_state
        and #%00000001
        bne no_up
        lda yar_y
        sec
        sbc #4
        sta yar_y
no_up:

; down (bit1 low)
        lda joy_state
        and #%00000010
        bne no_down
        lda yar_y
        clc
        adc #4
        sta yar_y
no_down:

        ; clamp X min
        lda yar_x_hi
        bpl +
        lda #PLAYER_MIN_X_HI
        sta yar_x_hi
        lda #PLAYER_MIN_X_LO
        sta yar_x
+
        lda yar_x_hi
        bne +
        lda yar_x
        cmp #PLAYER_MIN_X_LO
        bcs +
        lda #PLAYER_MIN_X_LO
        sta yar_x
+

        ; clamp X max
        lda yar_x_hi
        cmp #PLAYER_MAX_X_HI
        bcc +
        bne clamp_max
        lda yar_x
        cmp #PLAYER_MAX_X_LO
        bcc +
clamp_max:
        lda #PLAYER_MAX_X_HI
        sta yar_x_hi
        lda #PLAYER_MAX_X_LO
        sta yar_x
+

        ; ---- HARD collision check AFTER movement ----
        jsr player_overlap_wall
        bcc +                       ; ok, not overlapping

        ; while we are still in the "attempted" (into-wall) position,
        ; allow unlock to trigger on that contact frame
        jsr check_unlock_player_missile

        ; revert to old safe position...
        lda old_yar_x
        sta yar_x
        lda old_yar_x_hi
        sta yar_x_hi
        lda old_yar_y
        sta yar_y

        ; ...then bounce away from the wall a couple pixels
        jsr bounce_from_wall

        jsr barrier_nibble_on_bounce

        ; re-clamp X after bounce
        jsr clamp_player_x

+


; --- keep parked left-missile glued to player Y ---
        lda pmiss_unlocked
        beq +
        lda pmiss_state          ; 0=parked/ready
        bne +
        lda yar_y
        sta pmiss_y
+

; ---- sprite0 direction frame selection ----
; If joystick centered (bits 0..3 all 1), keep last facing.
        lda joy_state
        and #%00001111
        cmp #%00001111
        beq dir_done

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

dir_done:
        rts

spr_left:
        lda #SPR0_PTR_LEFT
        sta SPR_PTRS+0
        lda #DIR_LEFT
        sta player_dir
        rts

spr_right:
        lda #SPR0_PTR_RIGHT
        sta SPR_PTRS+0
        lda #DIR_RIGHT
        sta player_dir
        rts

spr_up:
        lda #SPR0_PTR_UP
        sta SPR_PTRS+0
        lda #DIR_UP
        sta player_dir
        rts

spr_down:
        lda #SPR0_PTR_DOWN
        sta SPR_PTRS+0
        lda #DIR_DOWN
        sta player_dir
        rts

; ------------------------------------------------------------
; barrier_nibble_on_bounce
; If you bounce on the same barrier tile 3 times, erase it.
; Uses wall_hit_row / wall_hit_col set by player_overlap_wall.
; ------------------------------------------------------------
barrier_nibble_on_bounce:
        ; only nibble if hit is inside barrier rectangle
        lda wall_hit_row
        cmp #BARRIER_ROW
        bcc bnb_done
        cmp #(BARRIER_ROW + BARRIER_H)
        bcs bnb_done

        lda wall_hit_col
        cmp #BARRIER_COL
        bcc bnb_done
        cmp #(BARRIER_COL + BARRIER_W)
        bcs bnb_done

        ; same tile as last time?
        lda wall_hit_row
        cmp nib_last_row
        bne bnb_new_tile
        lda wall_hit_col
        cmp nib_last_col
        bne bnb_new_tile

        ; same tile -> count++
        inc nib_count
        lda nib_count
        cmp #BARRIER_BOUNCES_TO_NIBBLE
        bcc bnb_done
        jmp bnb_erase

bnb_new_tile:
        lda wall_hit_row
        sta nib_last_row
        lda wall_hit_col
        sta nib_last_col
        lda #1
        sta nib_count
        rts

bnb_erase:
        ; clear the character at (row,col)
        lda wall_hit_row
        jsr row_to_ptr
        ldy wall_hit_col
        lda #CH_SPACE
        sta (dst_lo),y

        ; clear its color too
        lda wall_hit_row
        jsr row_to_color_ptr
        ldy wall_hit_col
        lda #0
        sta (dst_lo),y

        ; reset so it doesn't keep nuking adjacent bounces
        lda #0
        sta nib_count
        lda #$ff
        sta nib_last_row
        sta nib_last_col

bnb_done:
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
; bounce_from_wall
; Bounce opposite the direction the player was trying to move.
; Priority: right, left, up, down (good enough for diagonals).
; ------------------------------------------------------------
bounce_from_wall:
        ; if pressing RIGHT (bit3=0), bounce LEFT
        lda joy_state
        and #%00001000
        bne bf_not_right

        lda yar_x
        sec
        sbc #2
        sta yar_x
        bcs bf_done
        dec yar_x_hi
        jmp bf_done

bf_not_right:
        ; if pressing LEFT (bit2=0), bounce RIGHT
        lda joy_state
        and #%00000100
        bne bf_not_left

        lda yar_x
        clc
        adc #2
        sta yar_x
        bcc bf_done
        inc yar_x_hi
        jmp bf_done

bf_not_left:
        ; if pressing UP (bit0=0), bounce DOWN
        lda joy_state
        and #%00000001
        bne bf_not_up

        lda yar_y
        clc
        adc #2
        sta yar_y
        jmp bf_done

bf_not_up:
        ; if pressing DOWN (bit1=0), bounce UP
        lda joy_state
        and #%00000010
        bne bf_done

        lda yar_y
        sec
        sbc #2
        sta yar_y

bf_done:
        rts

; ------------------------------------------------------------
; clamp_player_x (reusable)
; ------------------------------------------------------------
clamp_player_x:
        ; clamp X min
        lda yar_x_hi
        bpl +
        lda #PLAYER_MIN_X_HI
        sta yar_x_hi
        lda #PLAYER_MIN_X_LO
        sta yar_x
+
        lda yar_x_hi
        bne +
        lda yar_x
        cmp #PLAYER_MIN_X_LO
        bcs +
        lda #PLAYER_MIN_X_LO
        sta yar_x
+

        ; clamp X max
        lda yar_x_hi
        cmp #PLAYER_MAX_X_HI
        bcc +
        bne clamp_max2
        lda yar_x
        cmp #PLAYER_MAX_X_LO
        bcc +
clamp_max2:
        lda #PLAYER_MAX_X_HI
        sta yar_x_hi
        lda #PLAYER_MAX_X_LO
        sta yar_x
+
        rts


; ------------------------------------------------------------
write_positions:
        ; sprite0 player
        lda yar_x
        sta $d000
        lda yar_y
        sta $d001

        ; sprite1 enemy alien  <--- ADD THIS
        lda enemy_x_lo
        sta $d002
        lda enemy_y_val
        sta $d003

        ; sprite2 enemy missile
        lda miss_x
        sta $d004
        lda miss_y
        sta $d005

        ; sprite3 bullet
        lda bullet_x
        sta $d006
        lda bullet_y
        sta $d007

        ; sprite4 player missile
        lda pmiss_x
        sta $d008
        lda pmiss_y
        sta $d009

        ; ----- $d015 enable mask -----
        lda #%00000111          ; sprites 0,1,2 always on

        ldx bullet_state
        beq +
        ora #%00001000          ; sprite3 when bullet flying
+
        ldx pmiss_unlocked
        beq +
        ora #%00010000          ; sprite4 visible once unlocked
+
        sta $d015

        ; ----- $d010 high X bits -----
        lda #0

        ldx yar_x_hi
        beq +
        ora #%00000001
+
        ldx enemy_x_hi
        beq +
        ora #%00000010
+
        ldx miss_x_hi
        beq +
        ora #%00000100
+
        ldx bullet_x_hi
        beq +
        ora #%00001000
+
        ldx pmiss_x_hi
        beq +
        ora #%00010000
+
        sta $d010
        rts

; ------------------------------------------------------------
; Bullet vs Barrier (sprite->screen mapping fixed with TEXT_X0/Y0)
; ------------------------------------------------------------
check_bullet_barrier_hit:
        lda bullet_state
        bne +
        jmp cbb_done
+

        lda #BULLET_HIT_X_OFF
        sta hit_x_off
        lda #BULLET_HIT_Y_OFF
        sta hit_y_off

        lda bullet_dir
        cmp #DIR_RIGHT
        bne +
        lda #18
        sta hit_x_off
        jmp cbb_calc
+
        cmp #DIR_LEFT
        bne +
        lda #6
        sta hit_x_off
        jmp cbb_calc
+
        cmp #DIR_UP
        bne +
        lda #6
        sta hit_y_off
        jmp cbb_calc
+
        lda #14
        sta hit_y_off

cbb_calc:
        lda bullet_y
        clc
        adc hit_y_off
        sec
        sbc #TEXT_Y0
        bcs +
        jmp cbb_done
+
        lsr
        lsr
        lsr
        tax

        cpx #BARRIER_ROW
        bcs +
        jmp cbb_done
+
        cpx #(BARRIER_ROW + BARRIER_H)
        bcc +
        jmp cbb_done
+

        lda bullet_x
        clc
        adc hit_x_off
        sta tmp1_lo
        lda bullet_x_hi
        adc #0
        sta tmp1_hi

        sec
        lda tmp1_lo
        sbc #TEXT_X0
        sta tmp1_lo
        lda tmp1_hi
        sbc #0
        sta tmp1_hi
        bpl +
        jmp cbb_done
+

        ldy #3
cbb_div8:
        lsr tmp1_hi
        ror tmp1_lo
        dey
        bne cbb_div8

        lda tmp1_lo
        sta tmp2_lo

        lda tmp2_lo
        cmp #BARRIER_COL
        bcs +
        jmp cbb_done
+
        cmp #(BARRIER_COL + BARRIER_W)
        bcc +
        jmp cbb_done
+

        txa
        jsr row_to_ptr
        ldy tmp2_lo
        lda (dst_lo),y

        cmp #CH_WALL_SOLID
        bcs +
        jmp cbb_done
+
        cmp #CH_WALL_DMG3+1
        bcc +
        jmp cbb_done
+

        lda #CH_SPACE
        sta (dst_lo),y

        txa
        jsr row_to_color_ptr
        ldy tmp2_lo
        lda #0
        sta (dst_lo),y

        lda #0
        sta bullet_state
        lda #BULLET_OFF_X
        sta bullet_x
        lda #0
        sta bullet_x_hi
        lda #BULLET_OFF_Y
        sta bullet_y

cbb_done:
        rts

hit_x_off: .byte 0
hit_y_off: .byte 0


; ------------------------------------------------------------
; Collision: player (sprite0) vs player missile (pmiss / sprite4)
; Same behavior as enemy missile hit: spin + freeze + restart
; ------------------------------------------------------------
check_player_pmiss_collision:
        lda game_mode
        beq cppc_continue
        rts
cppc_continue:

        lda pmiss_state
        bne +
        rts                     ; missile not flying
+

        ; same immunity rule as enemy missile: ignore while in zone
        jsr player_in_zone
        bcc +                   ; not in zone -> check collision
        rts                     ; in zone -> ignore missile damage
+

        ; ---- X collision (16-bit), require same high byte ----
        ; player hitpoint X
        lda yar_x
        clc
        adc #PLAYER_HIT_X_OFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi

        ; pmiss hitpoint X
        lda pmiss_x
        clc
        adc #PMISS_HIT_X_OFF
        sta tmp2_lo
        lda pmiss_x_hi
        adc #0
        sta tmp2_hi

        lda tmp1_hi
        cmp tmp2_hi
        bne cppc_done           ; if hi differs, too far apart

        ; abs(tmp1_lo - tmp2_lo) < COLLIDE_PMISS_X_THR ?
        lda tmp1_lo
        cmp tmp2_lo
        beq cppc_y
        bcc cppc_x_lt

cppc_x_gt:
        sec
        sbc tmp2_lo
        cmp #COLLIDE_PMISS_X_THR
        bcc cppc_y
        rts

cppc_x_lt:
        lda tmp2_lo
        sec
        sbc tmp1_lo
        cmp #COLLIDE_PMISS_X_THR
        bcc cppc_y
        rts

cppc_y:
        ; ---- Y collision (8-bit) ----
        lda yar_y
        clc
        adc #PLAYER_HIT_Y_OFF
        sta tmp1_lo

        lda pmiss_y
        clc
        adc #PMISS_HIT_Y_OFF
        sta tmp2_lo

        lda tmp1_lo
        cmp tmp2_lo
        beq cppc_hit
        bcc cppc_y_lt

cppc_y_gt:
        sec
        sbc tmp2_lo
        cmp #COLLIDE_PMISS_Y_THR
        bcc cppc_hit
        rts

cppc_y_lt:
        lda tmp2_lo
        sec
        sbc tmp1_lo
        cmp #COLLIDE_PMISS_Y_THR
        bcc cppc_hit
        rts

cppc_hit:
        jsr player_missile_hit
cppc_done:
        rts


; ------------------------------------------------------------
; Collision: player missile (pmiss / sprite4) vs enemy (sprite1)
; Triggers the big explosion (same as player_lose)
; ------------------------------------------------------------
check_pmiss_enemy_hit:
        lda game_mode
        beq +
        rts
+
        lda pmiss_state
        bne +
        rts                       ; missile not flying
+

        ; pmiss hitpoint (16-bit X)
        lda pmiss_x
        clc
        adc #PMISS_HIT_X_OFF
        sta tmp1_lo
        lda pmiss_x_hi
        adc #0
        sta tmp1_hi

        ; enemy hitpoint (16-bit X)
        lda enemy_x_lo
        clc
        adc #ENEMY_HIT_X_OFF
        sta tmp2_lo
        lda enemy_x_hi
        adc #0
        sta tmp2_hi

        ; X hi must match (enemy is fixed, so this is fine)
        lda tmp1_hi
        cmp tmp2_hi
        beq +
        rts
+

        ; abs(tmp1_lo - tmp2_lo) < COLLIDE_PMISS_X_THR ?
        lda tmp1_lo
        cmp tmp2_lo
        beq cpeh_x_ok
        bcc cpeh_x_lt

cpeh_x_gt:
        sec
        sbc tmp2_lo
        cmp #COLLIDE_PMISS_X_THR
        bcc cpeh_y
        rts

cpeh_x_lt:
        lda tmp2_lo
        sec
        sbc tmp1_lo
        cmp #COLLIDE_PMISS_X_THR
        bcc cpeh_y
        rts

cpeh_x_ok:
cpeh_y:
        ; abs((pmiss_y+off) - (enemy_y+off)) < COLLIDE_PMISS_Y_THR ?
        lda pmiss_y
        clc
        adc #PMISS_HIT_Y_OFF
        sta tmp1_lo

        lda enemy_y_val
        clc
        adc #ENEMY_HIT_Y_OFF
        sta tmp2_lo

        lda tmp1_lo
        cmp tmp2_lo
        beq cpeh_hit
        bcc cpeh_y_lt

cpeh_y_gt:
        sec
        sbc tmp2_lo
        cmp #COLLIDE_PMISS_Y_THR
        bcc cpeh_hit
        rts

cpeh_y_lt:
        lda tmp2_lo
        sec
        sbc tmp1_lo
        cmp #COLLIDE_PMISS_Y_THR
        bcc cpeh_hit
        rts

cpeh_hit:
        lda #0
        sta pmiss_state
        sta pmiss_unlocked

        jsr pmiss_sound_stop    
        jsr player_lose
        rts



; ------------------------------------------------------------
; Player missile vs Barrier
; If pmiss sprite enters barrier wall chars (10..13), reset it
; like the offscreen behavior (also re-locks to bullets).
; ------------------------------------------------------------
check_pmiss_barrier_hit:
        lda pmiss_state
        beq cph_done

        ; row = (pmiss_y + PMISS_HIT_Y_OFF - TEXT_Y0) / 8
        lda pmiss_y
        clc
        adc #PMISS_HIT_Y_OFF
        sec
        sbc #TEXT_Y0
        bcc cph_done
        lsr
        lsr
        lsr
        tax

        cpx #BARRIER_ROW
        bcc cph_done
        cpx #(BARRIER_ROW + BARRIER_H)
        bcs cph_done

        ; col = ((pmiss_x:hi + PMISS_HIT_X_OFF - TEXT_X0) / 8)
        lda pmiss_x
        clc
        adc #PMISS_HIT_X_OFF
        sta tmp1_lo
        lda pmiss_x_hi
        adc #0
        sta tmp1_hi

        sec
        lda tmp1_lo
        sbc #TEXT_X0
        sta tmp1_lo
        lda tmp1_hi
        sbc #0
        sta tmp1_hi
        bmi cph_done

        ldy #3
cph_div8:
        lsr tmp1_hi
        ror tmp1_lo
        dey
        bne cph_div8

        lda tmp1_lo                 ; A = col
        cmp #BARRIER_COL
        bcc cph_done
        cmp #(BARRIER_COL + BARRIER_W)
        bcs cph_done

        sta tmp2_lo                 ; save col for (dst),y

        txa
        jsr row_to_ptr
        ldy tmp2_lo
        lda (dst_lo),y

        ; wall chars are 10..13
        cmp #CH_WALL_SOLID
        bcc cph_done
        cmp #CH_WALL_DMG3+1
        bcs cph_done

        ; HIT: reset exactly like pmiss_reset
        lda #0
        sta pmiss_state
        sta pmiss_unlocked

        jsr pmiss_sound_stop

        lda #PMISS_START_X_LO
        sta pmiss_x
        lda #PMISS_START_X_HI
        sta pmiss_x_hi
        lda yar_y
        sta pmiss_y

cph_done:
        rts


; ------------------------------------------------------------
; Collision: sprite0 (player) vs sprite1 (enemy)
; ------------------------------------------------------------
check_player_enemy_collision:
        lda game_mode
        bne cpec_done

        lda yar_x_hi
        cmp enemy_x_hi
        bne dx_hi_diff

        lda yar_x
        cmp enemy_x_lo
        beq dx_zero
        bcc dx_player_lt

dx_player_gt:
        lda yar_x
        sec
        sbc enemy_x_lo
        sta dx_lo
        lda #0
        sbc #0
        sta dx_hi
        jmp dx_check

dx_player_lt:
        lda enemy_x_lo
        sec
        sbc yar_x
        sta dx_lo
        lda #0
        sbc #0
        sta dx_hi
        jmp dx_check

dx_hi_diff:
        lda #1
        sta dx_hi

dx_zero:
dx_check:
        lda dx_hi
        bne cpec_done
        lda dx_lo
        cmp #COLLIDE_X_THRESH
        bcs cpec_done

        lda yar_y
        cmp enemy_y_val
        beq dy_ok
        bcc dy_player_lt
        sec
        sbc enemy_y_val
        jmp dy_cmp
dy_player_lt:
        lda enemy_y_val
        sec
        sbc yar_y
dy_cmp:
        cmp #COLLIDE_Y_THRESH
        bcs cpec_done
dy_ok:
        jsr player_missile_hit


cpec_done:
        rts

; ------------------------------------------------------------
; Collision: player vs enemy missile using CENTER hit points
; ------------------------------------------------------------
check_player_missile_collision:
        lda game_mode
        beq cpmc_continue
        jmp cpmc_done
cpmc_continue:

        jsr player_in_zone
        bcc +               ; not in zone -> do normal collision
        jmp cpmc_done        ; in zone -> ignore missile damage
+
        lda yar_x
        clc
        adc #PLAYER_HIT_X_OFF
        sta tmp1_lo
        lda yar_x_hi
        adc #0
        sta tmp1_hi

        lda miss_x
        clc
        adc #MISS_HIT_X_OFF
        sta tmp2_lo
        lda miss_x_hi
        adc #0
        sta tmp2_hi

        lda tmp1_hi
        cmp tmp2_hi
        bne mdx_hi_diff

        lda tmp1_lo
        cmp tmp2_lo
        beq mdx_ok
        bcc mdx_player_lt

mdx_player_gt:
        lda tmp1_lo
        sec
        sbc tmp2_lo
        cmp #COLLIDE_MISS_X_THR
        bcs cpmc_done
        jmp mdy_calc

mdx_player_lt:
        lda tmp2_lo
        sec
        sbc tmp1_lo
        cmp #COLLIDE_MISS_X_THR
        bcs cpmc_done
        jmp mdy_calc

mdx_hi_diff:
        jmp cpmc_done

mdx_ok:
mdy_calc:
        lda yar_y
        clc
        adc #PLAYER_HIT_Y_OFF
        sta tmp1_lo

        lda miss_y
        clc
        adc #MISS_HIT_Y_OFF
        sta tmp2_lo

        lda tmp1_lo
        cmp tmp2_lo
        beq hit
        bcc mdy_player_lt

        sec
        sbc tmp2_lo
        cmp #COLLIDE_MISS_Y_THR
        bcs cpmc_done
        jmp hit

mdy_player_lt:
        lda tmp2_lo
        sec
        sbc tmp1_lo
        cmp #COLLIDE_MISS_Y_THR
        bcs cpmc_done

hit:
        jsr player_missile_hit


cpmc_done:
        rts

; ------------------------------------------------------------
player_lose:
        lda #GAME_EXPLODE
        sta game_mode

        lda #0
        sta explode_radius
        sta explode_wait

        ; start boom if it's not already running
        lda expl_timer
        bne +
        jsr explosion_sound_start
+
        rts


; ------------------------------------------------------------
; enemy_set_delay
; Random-ish delay (128..255 frames) before next dash
; ------------------------------------------------------------
enemy_set_delay:
        lda $dc04        ; CIA1 Timer A low (changes constantly)
        eor $dc05        ; mix with high
        and #$7f
        ora #$80         ; 128..255
        sta enemy_timer
        rts


; ------------------------------------------------------------
; update_enemy_attack
; Enemy (sprite1) occasionally dashes in a straight line aimed
; at the player's position at dash start. When offscreen, snaps home.
; ------------------------------------------------------------
update_enemy_attack:
        lda game_mode
        beq +
        rts
+
        lda enemy_state
        beq uea_idle
        jmp uea_dash

uea_idle:
        lda enemy_timer
        beq uea_start
        dec enemy_timer
        rts

uea_start:
        ; ---- choose X direction toward player ----
        lda yar_x_hi
        cmp enemy_x_hi
        bne uea_x_hi_diff
        lda yar_x
        cmp enemy_x_lo
        bcc uea_x_left
        jmp uea_x_right

uea_x_hi_diff:
        bcc uea_x_left
        jmp uea_x_right

uea_x_right:
        lda #0
        sta enemy_dx_neg
        lda #ENEMY_DASH_SPEED_X
        sta enemy_dx_mag
        jmp uea_y_dir

uea_x_left:
        lda #1
        sta enemy_dx_neg
        lda #ENEMY_DASH_SPEED_X
        sta enemy_dx_mag

uea_y_dir:
        ; ---- choose Y direction toward player ----
        lda yar_y
        cmp enemy_y_val
        bcc uea_y_up
        beq uea_y_none

uea_y_down:
        lda #0
        sta enemy_dy_neg
        lda #ENEMY_DASH_SPEED_Y
        sta enemy_dy_mag
        jmp uea_go

uea_y_up:
        lda #1
        sta enemy_dy_neg
        lda #ENEMY_DASH_SPEED_Y
        sta enemy_dy_mag
        jmp uea_go

uea_y_none:
        lda #0
        sta enemy_dy_mag
        sta enemy_dy_neg

uea_go:
        lda #1
        sta enemy_state
        lda #SPR1_PTR_ALIEN_DASH
        sta SPR_PTRS+1

        rts


uea_dash:
        ; ---- move X ----
        lda enemy_dx_mag
        beq uea_dash_y
        lda enemy_dx_neg
        beq uea_x_add

uea_x_sub:
        lda enemy_x_lo
        sec
        sbc enemy_dx_mag
        sta enemy_x_lo
        lda enemy_x_hi
        sbc #0
        sta enemy_x_hi
        jmp uea_dash_y

uea_x_add:
        lda enemy_x_lo
        clc
        adc enemy_dx_mag
        sta enemy_x_lo
        lda enemy_x_hi
        adc #0
        sta enemy_x_hi

uea_dash_y:
        ; ---- move Y ----
        lda enemy_dy_mag
        beq uea_offcheck
        lda enemy_dy_neg
        beq uea_y_add

uea_y_sub:
        lda enemy_y_val
        sec
        sbc enemy_dy_mag
        sta enemy_y_val
        jmp uea_offcheck

uea_y_add:
        lda enemy_y_val
        clc
        adc enemy_dy_mag
        sta enemy_y_val

uea_offcheck:
        ; ---- RIGHT offscreen (9-bit sprite X) ----
        ; if (enemy_x_hi == 1) and (enemy_x_lo >= SHOT_OFFSCREEN_LO) => reset
        lda enemy_x_hi
        and #$01
        cmp #SHOT_OFFSCREEN_HI      ; should be 1
        bne uea_left_check
        lda enemy_x_lo
        cmp #SHOT_OFFSCREEN_LO      ; $68 (360)
        bcs uea_reset_home

uea_left_check:
        ; ---- LEFT offscreen: if hi underflowed (negative) ----
        lda enemy_x_hi
        bmi uea_reset_home

        ; (optional) Y bounds reset too (keeps it from disappearing forever)
        lda enemy_y_val
        cmp #10
        bcc uea_reset_home
        cmp #245
        bcs uea_reset_home

        rts


uea_reset_home:
        lda #0
        sta enemy_state

        lda enemy_home_x_lo
        sta enemy_x_lo
        lda enemy_home_x_hi
        sta enemy_x_hi
        lda enemy_home_y_val
        sta enemy_y_val

        lda #SPR1_PTR_ALIEN
        sta SPR_PTRS+1

        jsr enemy_set_delay
        rts


; ------------------------------------------------------------
; Called when enemy missile hits player:
; spin clockwise until UP, then freeze 1 second, then restart
; ------------------------------------------------------------
player_missile_hit:
        lda #GAME_SPIN
        sta game_mode

        lda #0
        sta spin_wait

        lda player_dir
        cmp #DIR_UP
        beq pmh_already_up

        lda #0
        sta spin_phase
        lda #SPIN_STEP_MASK       ; 3 -> first update rotates immediately
        sta spin_counter
        rts

pmh_already_up:
        ; already facing up: just freeze 1 sec
        lda #1
        sta spin_phase
        lda #SPIN_WAIT_FRAMES
        sta spin_wait

        lda #SPR0_PTR_UP
        sta SPR_PTRS+0
        lda #DIR_UP
        sta player_dir
        rts


; ------------------------------------------------------------
; GAME_SPIN update (runs from IRQ, freezes everything else)
; ------------------------------------------------------------
death_spin_update:
        lda spin_phase
        beq dsu_spin

        ; waiting
        lda spin_wait
        beq dsu_restart
        dec spin_wait
        jsr write_positions
        rts

dsu_restart:
        jsr restart_level
        rts

dsu_spin:
        inc spin_counter
        lda spin_counter
        and #SPIN_STEP_MASK
        bne dsu_draw_only

        ; clockwise rotate: RIGHT->DOWN->LEFT->UP->RIGHT...
        ldx player_dir
        lda cw_next, x
        sta player_dir
        tax
        lda dir_ptr_tbl, x
        sta SPR_PTRS+0

        lda player_dir
        cmp #DIR_UP
        bne dsu_draw_only

        ; reached UP: freeze 1 sec
        lda #1
        sta spin_phase
        lda #SPIN_WAIT_FRAMES
        sta spin_wait

dsu_draw_only:
        jsr write_positions
        rts

; DIR_RIGHT=0 DIR_UP=1 DIR_DOWN=2 DIR_LEFT=3
cw_next:
        .byte DIR_DOWN, DIR_RIGHT, DIR_LEFT, DIR_UP   ; 0->2, 1->0, 2->3, 3->1

dir_ptr_tbl:
        .byte SPR0_PTR_RIGHT, SPR0_PTR_UP, SPR0_PTR_DOWN, SPR0_PTR_LEFT


; ------------------------------------------------------------
explosion_update:
        lda explode_wait
        beq eu_not_waiting
        dec explode_wait
        bne eu_done
        jsr restart_level
        rts

eu_not_waiting:
        lda explode_radius
        cmp #EXPLODE_MAX_RADIUS+1
        bcs eu_start_wait

        lda #EXPLODE_MID_ROW
        sec
        sbc explode_radius
        bcc eu_skip_top
        jsr fill_row_zone_full
eu_skip_top:

        lda explode_radius
        beq eu_inc

        lda #EXPLODE_MID_ROW
        clc
        adc explode_radius
        cmp #25
        bcs eu_inc
        jsr fill_row_zone_full

eu_inc:
        inc explode_radius
        rts

eu_start_wait:
        lda #EXPLODE_WAIT_FRAMES
        sta explode_wait
eu_done:
        rts

; ------------------------------------------------------------
restart_level:
        sei
        jsr reset_level_vars
        jsr clear_screen
        jsr draw_zone_and_wall
        jsr draw_barrier
        jsr init_sprites

        jsr explosion_sound_stop   ; make sure Voice1 & volume are sane
        jsr sound_init             ; restart hum/buzz baseline every level

        cli
        rts


; ------------------------------------------------------------
explosion_shimmer:
        inc zone_phase
        lda zone_phase
        and #$0f
        sta tmp_phase

        ldx #0
es_row:
        cpx #25
        beq es_done

        txa
        jsr row_to_color_ptr

        lda tmp_phase
        eor row_xor, x

        ldy #0
es_col:
        ora #$08
        sta (dst_lo),y
        iny
        cpy #40
        bne es_col

        inx
        jmp es_row

es_done:
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
; State / temps (regular RAM)
; ------------------------------------------------------------
game_mode:       .byte 0
explode_radius:  .byte 0
explode_wait:    .byte 0

enemy_x_lo:      .byte 0
enemy_x_hi:      .byte 0
enemy_y_val:     .byte 0

dx_lo:           .byte 0
dx_hi:           .byte 0

tmp1_lo: .byte 0
tmp1_hi: .byte 0
tmp2_lo: .byte 0
tmp2_hi: .byte 0

; --- barrier nibble tracking ---
wall_hit_row:   .byte 0      ; set by player_overlap_wall (row of tile hit)
wall_hit_col:   .byte 0      ; set by player_overlap_wall (col of tile hit)

nib_last_row:   .byte $ff
nib_last_col:   .byte $ff
nib_count:      .byte 0


; ---- enemy dash state ----
enemy_state:       .byte 0     ; 0=home/idle, 1=dashing
enemy_timer:       .byte 0     ; idle countdown
enemy_dx_mag:      .byte 0
enemy_dx_neg:      .byte 0     ; 0=+, 1=-
enemy_dy_mag:      .byte 0
enemy_dy_neg:      .byte 0     ; 0=+, 1=-

enemy_home_x_lo:   .byte 0
enemy_home_x_hi:   .byte 0
enemy_home_y_val:  .byte 0


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

player_dir:      .byte DIR_RIGHT

bullet_state:    .byte 0
bullet_dir:      .byte 0
bullet_x:        .byte 0
bullet_x_hi:     .byte 0
bullet_y:        .byte 0

pmiss_unlocked:  .byte 0
pmiss_state:     .byte 0
pmiss_x:         .byte 0
pmiss_x_hi:      .byte 0
pmiss_y:         .byte 0

old_yar_x:     .byte 0
old_yar_x_hi:  .byte 0
old_yar_y:     .byte 0

tmp_row: .byte 0

spin_phase:    .byte 0   ; 0=spinning, 1=waiting
spin_counter:  .byte 0
spin_wait:     .byte 0

tmp_col:    .byte 0
tmp_color:  .byte 0

title_col:   .byte 0
title_color: .byte 0

str_title:  .text "yars revenge"
            .byte 0

str_byline: .text "by scott hutter and chatgpt"
            .byte 0

str_press:  .text "joystick 2 - fire to begin"
            .byte 0

hum_state: .byte 0     ; 0=off, 1=on
sound_mode: .byte $ff    ; $ff=unknown, 0=hum, 1=buzz
buzz_on: .byte 0    ; 0=off, 1=on

pew_timer: .byte 0     ; counts down frames remaining in pew

expl_timer: .byte 0       ; >0 while explosion sound active
sid_mode_save: .byte 0    ; saves $d418 so we can restore volume/mode


; simple downward sweep (hi byte only). tweak values to taste.
PEW_LEN = 6
pew_freq_hi_tbl:
        .byte $28,$24,$20,$1c,$18,$14

; boom lasts through full explosion: expand (0..MAX) + wait frames (+ a little tail)
EXP_SND_LEN = (EXPLODE_MAX_RADIUS+1 + EXPLODE_WAIT_FRAMES + 30)


sid_resflt_save: .byte 0
sid_fc_lo_save:  .byte 0
sid_fc_hi_save:  .byte 0

exp_elapsed: .byte 0
exp_mul:     .byte 0

pmiss_snd_timer: .byte 0     ; >0 while left-missile fire sound active

; Left-missile fire sound length (~1.05s @ 60Hz)
PMISS_SND_LEN = 64

; Base pitch from WAV ≈ 370 Hz (NTSC):
; SID word ≈ $17B6  (lo=$B6 hi=$17)
pmiss_vib_lo:
    .byte $B0, $B6, $BC, $B6
pmiss_vib_hi:
    .byte $17, $17, $17, $17


; ------------------------------------------------------------
; sprite_data: 512 bytes copied to $3000
; ------------------------------------------------------------
sprite_data:

; sprite0 RIGHT
        .byte $00,$00,$00,$02,$00,$00,$02,$00
        .byte $00,$02,$00,$00,$02,$a0,$00,$02
        .byte $a0,$20,$08,$28,$20,$08,$28,$20
        .byte $0a,$82,$80,$0a,$82,$80,$0a,$82
        .byte $80,$0a,$82,$80,$08,$28,$20,$08
        .byte $28,$20,$02,$a0,$20,$02,$a0,$00
        .byte $02,$00,$00,$02,$00,$00,$02,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite0 UP
        .byte $00,$00,$00,$00,$00,$00,$00,$82
        .byte $00,$00,$82,$00,$00,$28,$00,$00
        .byte $28,$00,$00,$28,$00,$00,$28,$00
        .byte $00,$82,$00,$00,$82,$00,$00,$82
        .byte $00,$02,$00,$80,$02,$28,$80,$02
        .byte $28,$80,$0a,$28,$a0,$0a,$aa,$a0
        .byte $00,$aa,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite0 DOWN
        .byte $00,$00,$00,$00,$00,$00,$00,$aa
        .byte $00,$00,$aa,$00,$0a,$28,$a0,$0a
        .byte $28,$a0,$02,$28,$80,$02,$00,$80
        .byte $00,$82,$00,$00,$82,$00,$00,$82
        .byte $00,$00,$28,$00,$00,$28,$00,$00
        .byte $28,$00,$00,$28,$00,$00,$82,$00
        .byte $00,$82,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite0 LEFT
        .byte $00,$00,$00,$00,$00,$80,$00,$00
        .byte $80,$00,$00,$80,$00,$0a,$80,$08
        .byte $0a,$80,$08,$28,$20,$08,$28,$20
        .byte $02,$82,$a0,$02,$82,$a0,$02,$82
        .byte $a0,$02,$82,$a0,$08,$28,$20,$08
        .byte $28,$20,$08,$0a,$80,$00,$0a,$80
        .byte $00,$00,$80,$00,$00,$80,$00,$00
        .byte $80,$00,$00,$00,$00,$00,$00,$83

; sprite1 alien
        .byte $00,$00,$00,$00,$28,$00,$00,$a8
        .byte $00,$02,$a8,$00,$0a,$28,$00,$a8
        .byte $28,$00,$a0,$28,$00,$a0,$28,$00
        .byte $aa,$a8,$00,$a0,$28,$00,$a0,$28
        .byte $00,$a8,$28,$00,$0a,$28,$00,$02
        .byte $a8,$00,$00,$a8,$00,$00,$28,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite2 SMALL enemy missile
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$aa,$80,$00
        .byte $aa,$80,$00,$aa,$80,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite3 LARGE missile/shot
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$aa
        .byte $a0,$00,$55,$50,$00,$55,$50,$00
        .byte $ff,$f0,$00,$ff,$f0,$00,$55,$50
        .byte $00,$55,$50,$00,$aa,$a0,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite1 DASH (new frame)
        .byte $0a,$00,$00,$20,$a0,$00,$00,$20
        .byte $00,$28,$20,$00,$22,$a0,$00,$82
        .byte $82,$00,$82,$82,$00,$82,$82,$00
        .byte $0a,$88,$00,$08,$28,$00,$08,$00
        .byte $00,$0a,$08,$00,$00,$a0,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$83

; sprite4 player bullet (21 rows * 3 bytes = 63) + 1 pad byte
        .rept 9
        .byte $00,$00,$00
        .endrept

        .byte $00,$18,$00
        .byte $00,$18,$00

        .rept 10
        .byte $00,$00,$00
        .endrept

        .byte $00        ; 64th byte (unused/pad)

        .rept (768 - (9*64))
        .byte 0
        .endrept


