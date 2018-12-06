;
; SUPER HYPERZAP
;

; Code and graphics by Jason "T.M.R" Kelk
; Music by Sean "Odie" Connolly


; A slightly less simple gallery shoot 'em up coded for
; C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "super_hyperzap.prg",cbm


; Pull in the binary data
		* = $2000
		!binary "binary\characters.chr"

		* = $2800
		!binary "binary\sprites.spr"

		* = $4000
music		!binary "binary\outer_space.prg",,2


; Raster split positions
raster_1_pos	= $00
raster_2_pos	= $56
raster_3_pos	= $a9
raster_4_pos	= $f1

; Label assignments
raster_num	= $50		; raster split counter
sync		= $51		; raster sync for runtime code
rt_store_1	= $52		; temporary store for runtime code
random_count	= $53		; position in the "random" table

d01e_mirror	= $54		; VIC-II register mirrors
d01f_mirror	= $55
d021_mirror	= $56

joystick_temp	= $57		; joystick register store
bullet_x	= $58		; player bullet X
bullet_y	= $59		; player bullet Y
bullet_x_nudge	= $5a		; player bullet X position nudge
bullet_flag	= $5b		; used to mark the bullet for removal
bullet_colour	= $5c
anim_timer	= $5d		; sprite animation timer

screen_read	= $5e		; read location for pre-calc screen - $02 bytes used
screen_write	= $60		; read/write location on screen - $02 bytes used

star_col_timer	= $62		; colour pulse effect timer for starfield
status_count	= $63		; colour pulse counter for the status bar
status_timer	= $64		; colour pulse timer for the status bar

wave_read	= $65		; attack wave read position - $02 bytes used
wave_count	= $67		; counter - says which enemy to change speed
wave_timer	= $68		; time to next speed change

t_mode		= $69		; is title mode enabled?
t_scrl_x	= $6a		; title scroller screen position
t_scrl_pos	= $6b		; titles scroller position - $02 bytes used
t_scrl_col	= $6d		; current text colour
t_scrl_c_count	= $6e		; counter for the text colour

death_count	= $6f		; counter used whilst killing the player

screen_build	= $1c00		; copy of the generated starfield screen to save
				; having to repeatedly rebuild it!


; Add a BASIC startline
		* = $0801
		!word code_start-2
		!byte $40,$00,$9e
		!text "2066"
		!byte $00,$00,$00


; Entry point for the code
		* = $0812

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Set up the starfield screen using "random" data from the music
		ldx #$00
screen_init_1	lda music+$100,x
		and #$1f
		ora #$80
		sta screen_build+$000,x
		inx
		inx
		cpx #$28
		bne screen_init_1

		ldx #$00
screen_init_2a	lda screen_build+$000,x
		clc
		adc #$01
		and #$1f
		ora #$80
		sta screen_build+$028,x
		inx
		inx
		cpx #$f0
		bne screen_init_2a

		ldx #$00
screen_init_2b	lda screen_build+$0f0,x
		clc
		adc #$01
		and #$1f
		ora #$80
		sta screen_build+$118,x
		inx
		inx
		cpx #$f0
		bne screen_init_2b

		ldx #$00
screen_init_2c	lda screen_build+$1e0,x
		clc
		adc #$01
		and #$1f
		ora #$80
		sta screen_build+$208,x
		inx
		inx
		cpx #$f0
		bne screen_init_2c

		ldx #$00
screen_init_2d	lda screen_build+$2d0,x
		clc
		adc #$01
		and #$1f
		ora #$80
		sta screen_build+$2f8,x
		inx
		inx
		cpx #$a0
		bne screen_init_2d

; Set up status bar
		ldx #$0
status_set	lda status_text,x
		sta $07c0,x
		lda #$00
		sta $dbc0,x
		inx
		cpx #$28
		bne status_set

; Initialise some of our own labels
		lda #$01
		sta raster_num

; Set up the music driver
		lda #$00
		jsr music+$00

; Restart the interrupts
		cli


; Initialise the titles page
t_init		jsr screen_clear

; Move the sprites for the title logo
		ldx #$00
t_sprite_set	lda t_spr_x_dflt,x
		sta sprite_x,x
		lda t_spr_y_dflt,x
		sta sprite_y,x
		lda t_spr_c_dflt,x
		sta sprite_col,x
		lda t_spr_dp_dflt,x
		sta sprite_dp,x
		inx
		cpx #$08
		bne t_sprite_set

; Set up the text (space characters are skipped)
		ldx #$00
t_text_loop	lda t_text_1,x
		cmp #$20
		beq t_text_skip_1
		sta $0404,x
		lda #$01
		sta $d804,x

t_text_skip_1	lda t_text_2,x
		cmp #$20
		beq t_text_skip_2
		sta $05bc,x
		lda #$03
		sta $d9bc,x

t_text_skip_2	lda t_text_3,x
		cmp #$20
		beq t_text_skip_3
		sta $060c,x
		lda #$05
		sta $da0c,x

t_text_skip_3	lda t_text_4,x
		cmp #$20
		beq t_text_skip_4
		sta $0774,x
		lda #$04
		sta $db74,x

t_text_skip_4	inx
		cpx #$20
		bne t_text_loop

; Reset the scroller
		jsr t_scrl_reset

		lda #$05
		sta t_scrl_c_count
		lda #$50
		sta t_scrl_x

; Turn on the second row of title logo sprites
		lda #$01
		sta t_mode

; Titles page main loop -
t_loop		jsr sync_wait

		ldx t_scrl_x
		cpx #$28
		bcs t_scrl_cnt

; If we're at character position $00, fetch a text colour
		cpx #$00
		bne t_scrl_nocc

		ldy t_scrl_c_count
		lda t_scrl_cols,y
		sta t_scrl_col
		iny
		cpy #$0a
		bne *+$04
		ldy #$00
		sty t_scrl_c_count

; Restore the background for this character in case it's a space
t_scrl_nocc	lda screen_build+$320,x
		sta $0720,x
		lda #$08
		sta $db20,x

; Fetch the character
t_scrl_mread	ldy #$00
		lda (t_scrl_pos),y
		bne t_scrl_okay
		jsr t_scrl_reset
		jmp t_scrl_mread

; Draw in with the current character colour if it's not a space
t_scrl_okay	cmp #$20
		beq t_scrl_nowrite
		sta $0720,x
		lda t_scrl_col
		sta $db20,x

; Update the text position
t_scrl_nowrite	inc t_scrl_pos+$00
		bne *+$04
		inc t_scrl_pos+$01

t_scrl_cnt	inx
		stx t_scrl_x

; Check to see if fire has been pressed
		lda $dc00
		and #$10
		bne t_loop

; Turn off the second row of title logo sprites
		lda #$00
		sta t_mode


; Reset the score and lives counters
		ldx #$00
		txa
score_reset	sta player_score,x
		inx
		cpx #$07
		bne score_reset

		lda #$03
		sta player_lives

; Reset the attack wave engine
		jsr wave_reset

		ldx #$00
		ldy #$00
wave_init	lda (wave_read),y
		sta enemy_x_speeds,x
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

		lda (wave_read),y
		sta enemy_y_speeds,x
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

		inx
		cpx #$06
		bne wave_init

		lda #$00
		sta wave_count

; Initialise the main game
main_init	jsr screen_clear

; Reset the sprites
		ldx #$00
sprite_reset	lda sprite_x_dflt,x
		sta sprite_x,x
		lda sprite_y_dflt,x
		sta sprite_y,x
		lda sprite_col_dflt,x
		sta sprite_col,x

		lda sprite_dp_dflt,x
		sta sprite_dp,x
		lda anim_start_dflt,x
		sta anim_start,x
		lda anim_end_dflt,x
		sta anim_end,x

		inx
		cpx #$08
		bne sprite_reset

; Reset the enemy state bytes
		ldx #$00
		lda #$01
state_reset	sta enemy_state,x
		inx
		cpx #$06
		bne state_reset

; Select the player craft's colours
		ldx player_lives
		dex
		lda player_mid_col,x
		sta sprite_col+$00
		lda player_dark_col,x
		sta sprite_col+$01

; Reset the player bullet
		jsr bullet_clear
		lda #$00
		sta bullet_y

; Reset the wave counter
		lda #$00
		sta wave_timer

; Wait for the hardware registers to clear
spr_coll_wait	jsr sync_wait
		lda $d01e
		and #$03
		bne spr_coll_wait


; Main game loop
main_loop	jsr sync_wait

; Clear the bullet
		jsr bullet_clear

; Update player and test for collisions
		jsr player_update
		lda d01e_mirror
		and #$03
		bne death_init

; Update the enemies
		jsr enemy_update

; Update the player bullet
		jsr bullet_update

; Update the sprite animations
		jsr anim_update

		jmp main_loop


; Player death initialisation
death_init	lda #$00
		sta death_count

; Make the player into an explosion
		lda #$a9
		sta sprite_dp+$00
		lda #$af
		sta anim_start+$00
		lda #$b0
		sta anim_end+$00

		lda #$07
		sta sprite_col+$00

		lda #$af
		sta sprite_dp+$01
		lda #$af
		sta anim_start+$01
		lda #$b0
		sta anim_end+$01

death_loop	jsr sync_wait

		lda death_count
		cmp #$32
		bcs dl_no_strobe
		and #$03
		bne dl_no_strobe

		lda death_count
		lsr
		lsr
		tax
		lda random_x_table,x
		sta d021_mirror

; Clear the bullet
dl_no_strobe	jsr bullet_clear

; Update the enemies
		jsr enemy_update

; Update the player bullet
		jsr bullet_update

; Update the sprite animations
		jsr anim_update

		inc death_count
		lda death_count
		cmp #$96
		bne death_loop

; Decrease the lives counter and call the game over if zero
		dec player_lives
		lda player_lives
		beq gover_init

; Quick strobe before the player respawns
		lda #$07
		sta d021_mirror

		jmp main_init


; Game over
gover_init	ldx #$00
gi_text_loop	lda game_over_text,x
		sta $05c0,x
		lda #$01
		sta $d9c0,x
		inx
		cpx #$18
		bne gi_text_loop

		ldy #$96
		jsr sync_wait_long

		jmp t_init


; Read the joystick and update the player
player_update	lda $dc00
		sta joystick_temp
pu_up		and #$01
		bne pu_down

		lda sprite_y+$00
		sec
		sbc #$04
		cmp #$32
		bcs *+$04
		lda #$32
		sta sprite_y+$00
		sta sprite_y+$01

pu_down		lda joystick_temp
		and #$02
		bne pu_left

		lda sprite_y+$00
		clc
		adc #$04
		cmp #$dc
		bcc *+$04
		lda #$db
		sta sprite_y+$00
		sta sprite_y+$01

pu_left		lda joystick_temp
		and #$04
		bne pu_right

		lda sprite_x+$00
		sec
		sbc #$02
		cmp #$0c
		bcs *+$04
		lda #$0c
		sta sprite_x+$00
		sta sprite_x+$01

pu_right	lda joystick_temp
		and #$08
		bne pu_fire

		lda sprite_x+$00
		clc
		adc #$02
		cmp #$a1
		bcc *+$04
		lda #$a0
		sta sprite_x+$00
		sta sprite_x+$01

pu_fire		lda joystick_temp
		and #$10
		bne pu_joy_out

		lda bullet_y
		bne pu_joy_out

		lda sprite_x
		sec
		sbc #$0c
		lsr
		lsr
		sta bullet_x

		ldx #$00
		lda sprite_x
		and #$02
		beq *+$04
		ldx #$0c
		stx bullet_x_nudge

		lda sprite_y
		sec
		sbc #$22
		lsr
		lsr
		lsr
		sta bullet_y

		lda bullet_colour
		eor #$01
		sta bullet_colour

pu_joy_out	rts


; Clear the bullet (has to happen early otherwise removing
; it when blasting a nasty won't work!)
bullet_clear	ldx bullet_y
		lda screen_low,x
		sta screen_write+$00
		sta screen_read+$00
		lda screen_high,x
		sta screen_write+$01
		clc
		adc #(>screen_build)-$04
		sta screen_read+$01

		ldy bullet_x
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y

		tya
		clc
		adc #$25
		tay

		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y

		tya
		clc
		adc #$25
		tay

		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		iny
		lda (screen_read),y
		sta (screen_write),y
		rts

; Update the player bullet position
bullet_update	lda bullet_y
		sec
		sbc #$02
		cmp #$28
		bcc *+$04
		lda #$00
		sta bullet_y

; Draw the bullet to screen memory if Y isn't zero
		tax
		cpx #$00
		bne *+$03
		rts

		lda screen_low,x
		sta screen_write+$00
		lda screen_high,x
		sta screen_write+$01

; Select which of the two bullets to draw
		ldx bullet_x_nudge

		ldy bullet_x
		lda bullet_data+$00,x
		sta (screen_write),y
		iny
		lda bullet_data+$01,x
		sta (screen_write),y
		iny
		lda bullet_data+$02,x
		sta (screen_write),y
		iny
		lda bullet_data+$03,x
		sta (screen_write),y

		tya
		clc
		adc #$25
		tay

		lda bullet_data+$04,x
		sta (screen_write),y
		iny
		lda bullet_data+$05,x
		sta (screen_write),y
		iny
		lda bullet_data+$06,x
		sta (screen_write),y
		iny
		lda bullet_data+$07,x
		sta (screen_write),y

		tya
		clc
		adc #$25
		tay

		lda bullet_data+$08,x
		sta (screen_write),y
		iny
		lda bullet_data+$09,x
		sta (screen_write),y
		iny
		lda bullet_data+$0a,x
		sta (screen_write),y
		iny
		lda bullet_data+$0b,x
		sta (screen_write),y

; Draw the bullet's colour
		lda screen_write+$01
		clc
		adc #$d4
		sta screen_write+$01

		ldx #$00
		lda bullet_colour
		beq *+$04
		ldx #$0c

		ldy bullet_x
		lda bullet_col_data+$00,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$01,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$02,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$03,x
		sta (screen_write),y

		tya
		clc
		adc #$25
		tay

		lda bullet_col_data+$04,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$05,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$06,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$07,x
		sta (screen_write),y

		tya
		clc
		adc #$25
		tay

		lda bullet_col_data+$08,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$09,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$0a,x
		sta (screen_write),y
		iny
		lda bullet_col_data+$0b,x
		sta (screen_write),y

		rts


; Update the enemies
enemy_update	ldx #$00

; Check the state, skip moving if it's $00
eu_loop		lda enemy_state,x
		beq eu_no_wrap

; Update sprite X position
		lda sprite_x+$02,x
		clc
		adc enemy_x_speeds,x
		sta sprite_x+$02,x

; Update sprite Y position
		lda sprite_y+$02,x
		clc
		adc enemy_y_speeds,x
		sta sprite_y+$02,x
		bcc eu_no_wrap

; If the sprite has wrapped around, "randomise" it's X
		ldy random_count
		lda random_x_table,y
		sta sprite_x+$02,x
		iny
		sty random_count

eu_no_wrap	inx
		cpx #$06
		bne eu_loop

; Check to see if the enemies need destroying
		ldx #$00
		stx bullet_flag
		lsr d01f_mirror
		lsr d01f_mirror
eu_coll_loop	lsr d01f_mirror
		bcc eu_cl_skip

		lda enemy_state,x
		beq eu_cl_skip

; Collision detected, so make the enemy explode
		lda #$a9
		sta sprite_dp+$02,x
		lda #$af
		sta anim_start+$02,x
		lda #$b0
		sta anim_end+$02,x

		lda #$07
		sta sprite_col+$02,x

		lda #$00
		sta enemy_state,x

; Colour strobe
		lda #$0a
		sta d021_mirror

; Give the player 125 points
		stx rt_store_1
		ldx #$04
		jsr score_update

		ldx #$05
		jsr score_update
		ldx #$05
		jsr score_update

		ldx #$06
		jsr score_update
		ldx #$06
		jsr score_update
		ldx #$06
		jsr score_update
		ldx #$06
		jsr score_update
		ldx #$06
		jsr score_update

		ldx rt_store_1

; Mark the bullet for removal
		inc bullet_flag

eu_cl_skip	inx
		cpx #$06
		bne eu_coll_loop

; Check to see if the bullet needs resetting
		lda bullet_flag
		beq eu_respawn

		lda #$00
		sta bullet_y

; Do any enemies need to be respawned?
eu_respawn	ldx #$00
eu_rs_loop	lda sprite_dp+$02,x
		cmp #$af
		bne eu_rs_skip

		ldy random_count
		lda random_x_table,y
		sta sprite_x+$02,x
		iny
		sty random_count
		lda #$00
		sta sprite_y+$02,x

		lda sprite_col_dflt+$02,x
		sta sprite_col+$02,x

		lda sprite_dp_dflt+$02,x
		sta sprite_dp+$02,x

		lda anim_start_dflt+$02,x
		sta anim_start+$02,x
		lda anim_end_dflt+$02,x
		sta anim_end+$02,x

		lda #$01
		sta enemy_state,x

eu_rs_skip	inx
		cpx #$06
		bne eu_rs_loop

; Find out if an enemy speed change is due
eu_wave_update	ldx wave_timer
		inx
		bne eu_wt_xb

; It's time, so fetch two enemy speeds
		lda #$02
		sta rt_store_1

eu_wave_fetch	ldx wave_count
		ldy #$00
eu_wf_loop	lda (wave_read),y
		cmp #$80
		bne eu_wf_okay
		jsr wave_reset_2
		jmp eu_wf_loop

eu_wf_okay	sta enemy_x_speeds,x
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

		lda (wave_read),y
		sta enemy_y_speeds,x
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

		inx
		cpx #$06
		bne *+$04
		ldx #$00
		stx wave_count

		dec rt_store_1
		bne eu_wave_fetch

		ldx #$00
eu_wt_xb	stx wave_timer
		rts

; Reset the attack wave reader
wave_reset	lda #<wave_data
		sta wave_read+$00
		lda #>wave_data
		sta wave_read+$01
		rts

wave_reset_2	lda #<wave_data_loop
		sta wave_read+$00
		lda #>wave_data_loop
		sta wave_read+$01
		rts


; Animate the sprites
anim_update	ldx anim_timer
		dex
		cpx #$ff
		bne au_exit

		ldx #$00
au_loop		lda sprite_dp,x
		clc
		adc #$01
		cmp anim_end,x
		bne au_skip
		lda anim_start,x
au_skip		sta sprite_dp,x
		inx
		cpx #$08
		bne au_loop

		ldx #$03
au_exit		stx anim_timer
		rts


; Add to the score (X says which column to start from)
score_update	lda player_score,x
		clc
		adc #$01
		cmp #$0a
		beq su_skip
		sta player_score,x
		rts

su_skip		lda #$00
		sta player_score,x
		dex
		cpx #$ff
		bne score_update
		rts


; Titles subroutine to reset the scrolling message
t_scrl_reset	lda #<t_scrl_text
		sta t_scrl_pos+$00
		lda #>t_scrl_text
		sta t_scrl_pos+$01
		rts


; Wait for the end of the screen
sync_wait	lda #$00
		sta sync

sw_loop		cmp sync
		beq sw_loop
		rts

; Call sync_wait for Y frames
sync_wait_long	jsr sync_wait
		dey
		bne sync_wait_long
		rts


; Clear the screen and set up colour RAM
screen_clear	ldx #$00
		lda screen_build+$000,x
		sta $0400,x
		lda screen_build+$100,x
		sta $0500,x
		lda screen_build+$200,x
		sta $0600,x
		lda screen_build+$2c0,x
		sta $06c0,x

		lda #$08
		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dac0,x
		inx
		bne screen_clear+$02

		rts


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num

		cmp #$02
		bne *+$05
		jmp irq_rout2

		cmp #$03
		bne *+$05
		jmp irq_rout3

		cmp #$04
		bne *+$05
		jmp irq_rout4


; Raster split 1
irq_rout1	lda #$00
		sta $d020
		lda d021_mirror
		sta $d021

		inc star_col_timer
		lda star_col_timer
		lsr
		lsr
		and #$07
		tax
		lda star_colours,x
		sta $d022
		ldx bullet_colour
		lda bullet_mcol_dta,x
		sta $d023

		lda #$18
		sta $d016
		sta $d018

; Set up and position the hardware sprites
		lda #$ff
		sta $d015
		sta $d01b
		sta $d01c

		lda #$00
		sta $d01d

		ldx #$00
		ldy #$00
xploder_1	lda sprite_x,x
		asl
		ror $d010
		sta $d000,y
		lda sprite_y,x
		sta $d001,y
		iny
		iny
		inx
		cpx #$08
		bne xploder_1

		ldx #$00
xploder_2	lda sprite_col,x
		sta $d027,x
		lda sprite_dp,x
		sta $07f8,x
		inx
		cpx #$08
		bne xploder_2

		lda #$0b
		sta $d025
		lda #$01
		sta $d026

; Fetch the hardware collision registers to clear them
		lda $d01e
		lda $d01f

; Erase the old stars...
		ldx #$00
		txa
star_clear	ldy star_y,x
		sta $2400,y
		inx
		cpx #$03
		bne star_clear

; ...update their positions...
		ldx #$00
star_update	lda star_y,x
		clc
		adc star_speed,x
		sta star_y,x
		inx
		cpx #$03
		bne star_update

; ...and draw them back in
		ldx #$00
		txa
star_draw	ldy star_y,x
		lda $2400,y
		ora star_byte,x
		sta $2400,y
		inx
		cpx #$03
		bne star_draw

; Copy the player score into place
		ldx #$00
score_copy	lda player_score,x
		ora #$30
		sta $07c0,x
		inx
		cpx #$07
		bne score_copy

; And the lives counter
		lda player_lives
		ora #$30
		sta $07e7

; Colour effect for the status bar
		ldx #$00
status_col_upd	lda status_col_off,x
		clc
		adc status_count
		and #$0f
		tay
		lda status_pulse,y
		sta $dbc0,x
		inx
		cpx #$28
		bne status_col_upd

		ldx status_timer
		inx
		cpx #$03
		bcc stc_xb
		inc status_count
		ldx #$00
stc_xb		stx status_timer

; Play the music
		jsr music+$03

; Reset the $d021 mirror
		lda #$00
		sta d021_mirror

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
irq_rout2

; Are we in titles mode?
		lda t_mode
		beq t_mode_off_1

; Yes, so set up the second row of title logo sprites
		ldx #$00
		lda t_spr_y_row2
t_sprite_row_2a	sta $d001,x
		inx
		inx
		cpx #$10
		bne t_sprite_row_2a

		ldx #$00
		ldy #$00
t_sprite_row_2b	lda t_spr_x_row2,x
		asl
		ror $d010
		sta $d000,y

		lda t_spr_c_row2,x
		sta $d027,x
		lda t_spr_dp_row2,x
		sta $07f8,x

		iny
		iny
		inx
		cpx #$08
		bne t_sprite_row_2b


; Set interrupt handler for split 3
t_mode_off_1	lda #$03
		sta raster_num
		lda #raster_3_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 3
irq_rout3

; Are we in titles mode?
		lda t_mode
		beq t_mode_off_r3

; Yes, so set up the C64CD logo sprites
		ldx #$00
		lda t_spr_y_row3
t_sprite_row_3a	sta $d001,x
		inx
		inx
		cpx #$10
		bne t_sprite_row_3a

		ldx #$00
		ldy #$00
t_sprite_row_3b	lda t_spr_x_row3,x
		asl
		ror $d010
		sta $d000,y

		lda t_spr_c_row3,x
		sta $d027,x
		lda t_spr_dp_row3,x
		sta $07f8,x

		iny
		iny
		inx
		cpx #$08
		bne t_sprite_row_3b

; Specific sprite registers for the C64CD logo
		lda #$31
		sta $d01c

		lda #$0e
		sta $d01d

		lda #$06
		sta $d025


; Set interrupt handler for split 4
t_mode_off_r3	lda #$04
		sta raster_num
		lda #raster_4_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit



; Raster split 4
irq_rout4

; Hide the enemy sprites by changing their data pointers
		lda #$af
		sta $07fa
		sta $07fb
		sta $07fc
		sta $07fd
		sta $07fe
		sta $07ff

; Fetch the hardware collision registers for checking
		lda $d01e
		sta d01e_mirror
		lda $d01f
		sta d01f_mirror

; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012

; Tell the runtime code to execute
		lda #$01
		sta sync

; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Titles screen data
t_text_1	!scr "     c64cd presents in 2018     "

t_text_2	!scr "code and graphics     jason kelk"
t_text_3	!scr "music              sean connolly"

t_text_4	!scr "      press fire to start!      "

; "Scrolling" message text for the titles page
t_scrl_text	!scr "welcome to    s u p e r  h y p e r z a p"
		!scr "       a c64cd production in 2018       "
		!scr "  code and graphics by      jason kelk  "
		!scr "  music composed by      sean connolly  "

		!scr "                                        "

		!scr "  dedicated to the people on facebook,  "
		!scr "   twitter and the csdb who said that   "
		!scr "  hyperzap 2018 needed some x movement  "
		!scr "   and copious amounts of explosions!   "

		!scr "                                        "

		!scr "c64cd thanks-for-the-ideas greetings to:"
		!scr "  1001 crew - ash and dave - black bag  "
		!scr "         copy service stuttgart         "
		!scr "  borderzone dezign team - dynamic duo  "
		!scr "    four horsemen  of the apocalypse    "
		!scr "            happy  demomaker            "
		!scr "harlow cracking service - high-tech team"
		!scr "    ikari - jewels - kernal - laxity    "
		!scr "   mean team - paul, shandor and matt   "
		!scr "      pulse productions - reset 86      "
		!scr "    rob hubbard - scoop - slipstream    "
		!scr "   stoat and tim - tangent - thalamus   "
		!scr " the commandos - the gps - the six pack "
		!scr " we music - xess - yak - yeti factories "
		!scr "and the usual anti-greeting to c64hater!"

		!scr "                                        "

		!scr "   enjoy the blasting and remember to   "
		!scr "    visit the c64cd website for more    "
		!scr " 8-bit goodness, hyperbole and sarcasm! "
		!scr "the url is   c64crapdebunk.wordpress.com"

		!scr "                                        "

		!scr "disconnecting from server     2018-12-06"

		!scr "                                        "
		!byte $00		; end of text marker

; Colours for the message
t_scrl_cols	!byte $06,$02,$04,$05,$03,$07,$03,$05
		!byte $04,$02

; Titles page sprite positions, colours and definitions
; First row
t_spr_x_dflt	!byte $38,$47,$56,$65,$74,$00,$00,$00
t_spr_y_dflt	!byte $44,$44,$44,$44,$44,$00,$00,$00

t_spr_c_dflt	!byte $0c,$0c,$0c,$0c,$0c,$00,$00,$00
t_spr_dp_dflt	!byte $a0,$a1,$a2,$a3,$a4,$a4,$a4,$a4


; Titles page sprite positions, colours and definitions
; Second row
t_spr_x_row2	!byte $21,$30,$3f,$4e,$5d,$6c,$7b,$8a
t_spr_y_row2	!byte $60,$60,$60,$60,$60,$60,$60,$60

t_spr_c_row2	!byte $0e,$0e,$0e,$0e,$0e,$0a,$0a,$0a
t_spr_dp_row2	!byte $a5,$a6,$a2,$a3,$a4,$a7,$a8,$a2


; Titles page sprite positions, colours and definitions
; Third row - C64CD logo
t_spr_x_row3	!byte $3e,$4a,$4a,$4a,$62,$6e,$00,$ac
t_spr_y_row3	!byte $b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2

t_spr_c_row3	!byte $0e,$02,$0a,$0f,$0e,$0e,$01,$01
t_spr_dp_row3	!byte $db,$dc,$dd,$de,$df,$e0,$e1,$e1


; Sprite positions, colours and definitions
sprite_x	!byte $00,$00,$00,$00,$00,$00,$00,$00
sprite_y	!byte $00,$00,$00,$00,$00,$00,$00,$00

sprite_col	!byte $00,$00,$00,$00,$00,$00,$00,$00
sprite_dp	!byte $00,$00,$00,$00,$00,$00,$00,$00

; Sprite animation start and end positions
anim_start	!byte $00,$00,$00,$00,$00,$00,$00,$00
anim_end	!byte $00,$00,$00,$00,$00,$00,$00,$00

; Enemy state counters - $00 means exploding, $01 is active
enemy_state	!byte $00,$00,$00,$00,$00,$00

; Enemy sprite X and Y movement speeds
enemy_x_speeds	!byte $00,$00,$00,$00,$00,$00
enemy_y_speeds	!byte $00,$00,$00,$00,$00,$00


; Player colours
player_dark_col	!byte $06,$09,$02
player_mid_col	!byte $0e,$05,$0a

; Attack wave data (first two patterns aren't repeated)
wave_data	!byte $00,$01
		!byte $00,$02
		!byte $00,$01
		!byte $00,$01
		!byte $00,$02
		!byte $00,$03

		!byte $00,$02
		!byte $00,$03
		!byte $01,$01
		!byte $01,$01
		!byte $00,$03
		!byte $00,$01

wave_data_loop	!byte $ff,$04
		!byte $ff,$02
		!byte $00,$03
		!byte $00,$02
		!byte $01,$03
		!byte $ff,$02

		!byte $00,$03
		!byte $01,$03
		!byte $01,$04
		!byte $ff,$06
		!byte $00,$05
		!byte $ff,$02

		!byte $ff,$04
		!byte $ff,$05
		!byte $01,$03
		!byte $01,$03
		!byte $00,$02
		!byte $01,$04

		!byte $ff,$03
		!byte $01,$04
		!byte $ff,$06
		!byte $00,$05
		!byte $ff,$04
		!byte $00,$03

		!byte $00,$06
		!byte $ff,$05
		!byte $ff,$06
		!byte $00,$05
		!byte $00,$06
		!byte $01,$06

		!byte $01,$04
		!byte $ff,$07
		!byte $01,$05
		!byte $ff,$04
		!byte $01,$08
		!byte $ff,$07

		!byte $00,$04
		!byte $01,$05
		!byte $02,$02
		!byte $00,$04
		!byte $ff,$05
		!byte $ff,$03

		!byte $80		; end of data marker


; Sprite default positions, colours, definitions, animations
sprite_x_dflt	!byte $56,$56,$ae,$be,$ce,$de,$ee,$fe
sprite_y_dflt	!byte $c0,$c0,$a5,$cc,$49,$64,$27,$87

sprite_col_dflt	!byte $0e,$06,$0d,$05,$0a,$07,$03,$0e
sprite_dp_dflt	!byte $b0,$b9,$c2,$c5,$ca,$cd,$d2,$d5

anim_start_dflt	!byte $b0,$b9,$c2,$c2,$ca,$ca,$d2,$d2
anim_end_dflt	!byte $b9,$c2,$ca,$ca,$d2,$d2,$db,$db


; Bullet graphics block
bullet_data	!byte $40,$00,$43,$00
		!byte $41,$00,$44,$00
		!byte $42,$00,$45,$00

		!byte $46,$00,$00,$49
		!byte $47,$00,$00,$4a
		!byte $48,$00,$00,$4b

bullet_col_data	!byte $0f,$0f,$0f,$0f
		!byte $0a,$0a,$0a,$0a
		!byte $0a,$0a,$0a,$0a

		!byte $0b,$0b,$0b,$0b
		!byte $0e,$0e,$0e,$0e
		!byte $0e,$0e,$0e,$0e

bullet_mcol_dta	!byte $0a,$0e

; Multiples of $28 for working out where each line of the screen
; starts (low byte of each value in the first table, high byte
; in the second) for the bullet
screen_low	!byte $00
		!byte $00,$28,$50,$78,$a0,$c8,$f0,$18
		!byte $40,$68,$90,$b8,$e0,$08,$30,$58
		!byte $80,$a8,$d0,$f8,$20,$48,$00,$00

screen_high	!byte $04
		!byte $04,$04,$04,$04,$04,$04,$04,$05
		!byte $05,$05,$05,$05,$05,$06,$06,$06
		!byte $06,$06,$06,$06,$07,$07,$04,$04

; "Random" X positions for enemies
random_x_table	!byte $3e,$98,$5e,$21,$91,$2c,$3c,$50
		!byte $2f,$40,$10,$7e,$62,$1d,$0d,$81
		!byte $10,$72,$3e,$32,$7c,$1d,$80,$a0
		!byte $21,$50,$0c,$5c,$3e,$71,$0c,$50
		!byte $2e,$7f,$4d,$6d,$5f,$3d,$71,$51
		!byte $4c,$21,$6e,$51,$41,$2e,$a0,$50
		!byte $0d,$1c,$41,$6c,$92,$6f,$9e,$70
		!byte $8f,$5c,$1d,$2e,$7c,$3c,$80,$4e

		!byte $5f,$8c,$7d,$3f,$52,$7c,$72,$8f
		!byte $81,$6d,$9e,$31,$10,$9c,$5d,$91
		!byte $2d,$80,$6c,$9f,$7f,$0e,$1e,$8f
		!byte $42,$7d,$9f,$90,$5f,$8d,$3f,$51
		!byte $12,$71,$62,$70,$4c,$8c,$12,$4f
		!byte $9e,$70,$9e,$2f,$21,$9d,$0e,$5f
		!byte $72,$5c,$4e,$1d,$0c,$9e,$81,$8f
		!byte $22,$6e,$1c,$0d,$92,$3f,$82,$20

		!byte $a0,$62,$9d,$80,$1d,$2c,$9c,$72
		!byte $2e,$9c,$6d,$9e,$2e,$70,$9c,$51
		!byte $70,$a0,$6f,$9f,$1d,$61,$3d,$9c
		!byte $52,$21,$8c,$9e,$2e,$7d,$4f,$0e
		!byte $1d,$9f,$80,$40,$8c,$22,$41,$0c
		!byte $8e,$10,$4e,$0d,$61,$37,$81,$62
		!byte $3c,$52,$0d,$7d,$31,$1c,$62,$9d
		!byte $60,$21,$62,$4e,$0c,$5e,$6f,$4d

		!byte $8e,$7f,$21,$3c,$9e,$80,$9f,$7d
		!byte $30,$10,$60,$1e,$7f,$5c,$82,$20
		!byte $4f,$1f,$4f,$2d,$92,$0e,$4d,$90
		!byte $51,$92,$10,$8d,$61,$41,$80,$91
		!byte $0e,$5f,$0e,$42,$11,$9f,$90,$9d
		!byte $62,$99,$70,$2d,$9f,$71,$61,$12
		!byte $2c,$62,$3f,$6f,$4c,$0e,$4c,$82
		!byte $1d,$4e,$3d,$30,$7f,$0c,$62,$9c


; Starfield position, speed and colour data
star_y		!byte $17,$69,$47
star_speed	!byte $01,$02,$03
star_byte	!byte $40,$04,$10

star_colours	!byte $06,$0b,$04,$0e,$0f,$0a,$08,$02


; Status bar text and colour tables
status_text	!scr "             super hyperzap      lives  "
status_col_off	!byte $00,$01,$02,$01,$00,$01,$02,$01
		!byte $08,$07,$06,$05,$04,$03,$02,$04
		!byte $06,$08,$0a,$0c,$0e,$00,$0f,$0e
		!byte $0d,$0c,$0b,$0a,$09,$08,$07,$06
		!byte $0c,$0c,$0b,$0a,$0b,$0c,$0c,$0e

status_pulse	!byte $06,$02,$04,$05,$03,$07,$01,$01
		!byte $01,$01,$01,$07,$03,$05,$04,$06

; Status work spaces
player_score	!byte $00,$00,$00,$00,$00,$00,$00,$00
player_lives	!byte $00


; Game over message
game_over_text	!scr "game over man, game over"
