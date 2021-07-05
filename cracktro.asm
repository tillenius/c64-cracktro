.var music = LoadSid("Enlightenment_Druid_II.sid")

BasicUpstart2(start)

.function sinus(i, amplitude, center, noOfSteps) {
	.return round(center+amplitude*cos(toRadians(i*360/noOfSteps)))	
}

//////////////////////////////////////////////
// SETUP
//////////////////////////////////////////////

* = $0810 "cracktro"

start:
	sei
	lda #$7f
	sta $dc0d		// disable cia-1 interrupts
	sta $dd0d	

	lda #$0 		// black
	sta $d020 		// screenframe
	sta $d021 		// background

	// init music

	lda #music.startSong-1
	jsr music.init

	lda #%11101010  // screen memory at $3800, char memory at $2800
	sta $d018		// memory setup register

	lda #$2 		// dark red
	sta $d022 		// multicolor %01
	lda #$a 		// light red
	sta $d023 		// multicolor %10

	// fill $d820-$d920 with multi-color white
	lda #%1001 		// multi-color white
 	ldx #$00
!loop:
 	sta $d820,x 	// color-ram
 	inx
 	bne !loop-

	// fill $d940-$da40 with high-res white
	lda #%0001 		// multi-color white
 	ldx #$00
!loop:
 	sta $d940,x 	// color-ram
 	inx
 	bne !loop-

	// fill $da40-$db40 with high-res black
	lda #%0000 		// high-res black
 	ldx #$00
!loop:
 	sta $da40,x 	// color-ram
 	inx
 	bne !loop-

	// fill $dae8-$dbe8 with high-res white
	lda #%0001 		// multi-color white
 	ldx #$00
!loop:
 	sta $dae8,x 	// color-ram
 	inx
 	bne !loop-

	lda #%00011000  // horizontal raster scroll = 0, screen width = 40 cols, multicolor mode
	sta $d016 		// screen control register 2

	// setup sprites

	lda #$d
	sta	$d025 // multi-color color for %01
	lda #$1
	sta	$d026 // multi-color color for %11

	lda #$c0  // $c0*$40 = $3000
	sta $3bf8 // sprite pointer index
	sta $3bf9 // sprite pointer index
	sta $3bfa // sprite pointer index
	sta $3bfb // sprite pointer index
	sta $3bfc // sprite pointer index
	sta $3bfd // sprite pointer index
	sta $3bfe // sprite pointer index

	lda #$7c
	sta $d001 // sprite y
	sta $d003 // sprite y
	sta $d005 // sprite y
	sta $d007 // sprite y
	sta $d009 // sprite y
	sta $d00b // sprite y
	sta $d00d // sprite y

	lda #$18
	sta $d000 // sprite x
	lda #$48
	sta $d002 // sprite x
	lda #$78
	sta $d004 // sprite x
	lda #$a8
	sta $d006 // sprite x
	lda #$d8
	sta $d008 // sprite x
	lda #$08
	sta $d00a // sprite x
	lda #$38
	sta $d00c // sprite x

	lda #%01100000
	sta $d010 // sprite x high bits

	lda #$5
	sta $d027 // sprite 0 color
	sta $d028 // sprite 1 color
	sta $d029 // sprite 2 color
	sta $d02a // sprite 3 color
	sta $d02b // sprite 4 color
	sta $d02c // sprite 5 color
	sta $d02d // sprite 6 color

	lda #$7f
	sta $d01d // sprite width stretch
	lda #$ff
	sta $d01c // high-color mode
	lda #$7f
	sta $d015 // enable sprites

	// raster line interrupt on line 20

	lda #%00011011
	sta $d011
	lda #0 			// raster line interrupt on line 0
	sta $d012

	lda #<irq_logo
	sta $0314
	lda #>irq_logo
	sta $0315

	lda #%00000001	// enable raster interrupt
	sta $d01a

	lda $dc0d
	lda $dd0d
	asl $d019
	cli

end:
	jmp	end




//////////////////////////////////////////////
// setup for logo
//////////////////////////////////////////////

irq_logo:
	lda #%00011000  // horizontal raster scroll = 0, screen width = 40 cols, multicolor mode
	sta $d016 		// screen control register 2

	// update color bar

	ldx colorbarpos
	lda sinus, x
	sta $d001 // sprite y
	sta $d003 // sprite y
	sta $d005 // sprite y
	sta $d007 // sprite y
	sta $d009 // sprite y
	sta $d00b // sprite y
	sta $d00d // sprite y
	inx
	txa
	and #$7f
	sta colorbarpos
	beq !zero+
	cmp #$40
	bne !cont+
!set:
	lda #0
	sta $d01b
	jmp !cont+
!zero:
	lda #%01111111
	sta $d01b
!cont:

	// update color memory

	ldx #0
	ldy colorpos
!loop:
	lda colors, y
	sta $d800+21*40, x
	iny
	inx
	cpx #40
	bne !loop-

	lda colorpos
	cmp #20
	bne !nocolorposreset+
	lda	#0
	sta colorpos

!nocolorposreset:

	inc colorpos

	// next interrupt

	lda #$a9
	sta $d012

	lda #<irq_colorbar1
	sta $0314
	lda #>irq_colorbar1
	sta $0315

	lda #1
	sta $d019	// interrupt status register
	jmp $ea31	// default interrupt


//////////////////////////////////////////////
// colorbar 1
//////////////////////////////////////////////

skipline:
	ldx #8
!loop:
	dex
	bne !loop-
	nop
	nop
	rts

skipbadline:
	nop
	rts

irq_colorbar1:
	nop;nop;nop;nop;nop;nop;nop

	lda #2
	sta $d021
	jsr skipline

	lda #10
	sta $d021
	jsr skipbadline

	lda #10
	sta $d021
	jsr skipline

	lda #1
	sta $d021
	jsr skipline

	lda #1
	sta $d021
	jsr skipline

	lda #1
	sta $d021
	jsr skipline

	lda #10
	sta $d021
	jsr skipline

	lda #10
	sta $d021
	jsr skipline

	lda #2
	sta $d021
	jsr skipline

	lda #0
	sta $d021

	// next interrupt

	lda #$b9
	sta $d012

	lda #<irq_colorbar2
	sta $0314
	lda #>irq_colorbar2
	sta $0315

	lda #1
	sta $d019	// interrupt status register
	jmp $ea31	// default interrupt

//////////////////////////////////////////////
// colorbar 2
//////////////////////////////////////////////

irq_colorbar2:
	nop;nop;nop;nop;nop;nop;nop

	lda #6
	sta $d021
	jsr skipline

	lda #14
	sta $d021
	jsr skipbadline

	lda #14
	sta $d021
	jsr skipline

	lda #1
	sta $d021
	jsr skipline

	lda #1
	sta $d021
	jsr skipline

	lda #1
	sta $d021
	jsr skipline

	lda #14
	sta $d021
	jsr skipline

	lda #14
	sta $d021
	jsr skipline

	lda #6
	sta $d021
	jsr skipline

	lda #0
	sta $d021

	// next interrupt

	lda #$c8
	sta $d012

	lda #<irq_scroll
	sta $0314
	lda #>irq_scroll
	sta $0315

	lda #1
	sta $d019	// interrupt status register
	jmp $ea31	// default interrupt

//////////////////////////////////////////////
// setup for scroller
//////////////////////////////////////////////

irq_scroll:
	dec offset
	lda offset
	and #%00000111
	ora #%00001000
	sta $d016
	cmp #%00001111
	bne skip

	// scroll text

	ldx #0
!loop:
	lda scrollline+1,x
	sta scrollline,x
	inx
	cpx #39
	bne !loop-

	// add new character

 	inc scrollpos

 	ldx scrollpos
 	lda scrolltext,x
 	cmp	#0
 	bne !noreset+

 	sta scrollpos
 	lda scrolltext

!noreset:
 	sta scrollline+39

skip:
	// next interrupt

	lda #$f0
	sta $d012

	lda #<irq_endscroll
	sta $0314
	lda #>irq_endscroll
	sta $0315

	lda #1
	sta $d019	// interrupt status register
	jmp $ea81

irq_endscroll:
	lda #%00011000  // horizontal raster scroll = 0, screen width = 40 cols, multicolor mode
	sta $d016 		// screen control register 2

	jsr $0fc5 	// music update

	// next interrupt

	lda #10
	sta $d012

	lda #<irq_logo
	sta $0314
	lda #>irq_logo
	sta $0315

	lda #1
	sta $d019	// interrupt status register
	jmp $ea81

* = $2800 "font"

	.fill	8*44,0 	  // 0-43

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00110000
	.byte	%01100000

	.fill	8,0 	  // 46

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00110000
	.byte	%00000000

	.fill	8,0 	  // 46

	.byte	%01111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%00011000
	.byte	%00111000
	.byte	%00011000
	.byte	%00011000
	.byte	%00011000
	.byte	%00011000
	.byte	%00111100
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%00011100
	.byte	%01110000
	.byte	%11000000
	.byte	%11000000
	.byte	%11111110
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%00011100
	.byte	%00000110
	.byte	%00000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%00000110
	.byte	%00011110
	.byte	%11100110
	.byte	%11111110
	.byte	%00000110
	.byte	%00000110
	.byte	%00000110
	.byte	%00000000

	.byte	%11111110
	.byte	%11000000
	.byte	%11111100
	.byte	%00000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%01111110
	.byte	%11000000
	.byte	%11111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11111110
	.byte	%00001100
	.byte	%00011000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%01111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%01111110
	.byte	%00000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.fill	8*7,0 	  // 58-64

	.byte	%01111100 // A
	.byte	%11000110
	.byte	%11111110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%00000000

	.byte	%11111100
	.byte	%11000110
	.byte	%11111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11111100
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11111000
	.byte	%11001100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11111100
	.byte	%00000000

	.byte	%11111110
	.byte	%11000000
	.byte	%11111000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11111110
	.byte	%00000000

	.byte	%11111110
	.byte	%11000000
	.byte	%11111000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%00000000

	.byte	%01111100
	.byte	%11000000
	.byte	%11011110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11000110
	.byte	%11000110
	.byte	%11111110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%00000000

	.byte	%01111000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%01111000
	.byte	%00000000

	.byte	%00011110
	.byte	%00000110
	.byte	%00000110
	.byte	%00000110
	.byte	%00000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11000110
	.byte	%11000110
	.byte	%11111000
	.byte	%11001100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%00000000

	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11111110
	.byte	%00000000

	.byte	%10000010
	.byte	%11000110
	.byte	%11101110
	.byte	%11111110
	.byte	%11010110
	.byte	%11000110
	.byte	%11000110
	.byte	%00000000

	.byte	%10000110
	.byte	%11000110
	.byte	%11100110
	.byte	%11110110
	.byte	%11011110
	.byte	%11001110
	.byte	%11000110
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11111100
	.byte	%11000110
	.byte	%11111100
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%00000000

	.byte	%11111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11001100
	.byte	%01110110
	.byte	%00000000

	.byte	%11111100
	.byte	%11000110
	.byte	%11111100
	.byte	%11001100
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%00000000

	.byte	%01111100
	.byte	%11000110
	.byte	%01110000
	.byte	%00011100
	.byte	%00000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11111100
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00000000

	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01111100
	.byte	%00000000

	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%01101100
	.byte	%01101100
	.byte	%00111000
	.byte	%00010000
	.byte	%00000000

	.byte	%11000110
	.byte	%11000110
	.byte	%11010110
	.byte	%11111110
	.byte	%11101110
	.byte	%11000110
	.byte	%10000010
	.byte	%00000000

	.byte	%11000110
	.byte	%01101100
	.byte	%00111000
	.byte	%01101100
	.byte	%01101100
	.byte	%11000110
	.byte	%11000110
	.byte	%00000000

	.byte	%11000110
	.byte	%11001100
	.byte	%01111000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00110000
	.byte	%00000000

	.byte	%11111110
	.byte	%00000110
	.byte	%00001100
	.byte	%00011000
	.byte	%00110000
	.byte	%01100000
	.byte	%11111110
	.byte	%00000000

	.fill	8*6,0 	  // 91-96

	.byte	%01010101 // a
	.byte	%10101010
	.byte	%10101010
	.byte	%11111111
	.byte	%11111111
	.byte	%10101010
	.byte	%10101010
	.byte	%01010101

	.byte	%10111001 // b
	.byte	%10111001
	.byte	%10111001
	.byte	%10111001
	.byte	%10111001
	.byte	%10111001
	.byte	%10111001
	.byte	%10111001

	.byte	%10111001 // c
	.byte	%10111001
	.byte	%11100100
	.byte	%11100100
	.byte	%10010000
	.byte	%10010000
	.byte	%01000000
	.byte	%01000000

	.byte	%01000000 // d
	.byte	%01000000
	.byte	%10010000
	.byte	%10010000
	.byte	%11100100
	.byte	%11100100
	.byte	%10111001
	.byte	%10111001

	.byte	%01101110 // e
	.byte	%01101110
	.byte	%00011011
	.byte	%00011011
	.byte	%00000110
	.byte	%00000110
	.byte	%00000001
	.byte	%00000001

	.byte	%01101110 // f
	.byte	%10101110
	.byte	%10011011
	.byte	%10011011
	.byte	%10110110
	.byte	%10110110
	.byte	%10111001
	.byte	%10111001

	.byte	%00000001 // g
	.byte	%00000001
	.byte	%00000110
	.byte	%00000110
	.byte	%00011011
	.byte	%00011011
	.byte	%01101110
	.byte	%01101110

	.byte	%10111001 // h
	.byte	%10111001
	.byte	%11100101
	.byte	%11100101
	.byte	%10011001
	.byte	%10011001
	.byte	%01111001
	.byte	%01111001

	.byte	%01010101 // i
	.byte	%10101010
	.byte	%10101010
	.byte	%10111111
	.byte	%10111111
	.byte	%10111010
	.byte	%10111010
	.byte	%10111001

	.byte	%10111001 // j
	.byte	%10111010
	.byte	%11101001
	.byte	%11100100
	.byte	%11100100
	.byte	%11101001
	.byte	%10111010
	.byte	%10111001

* = $3000 "spritedata"

	.byte	%10101010,%10101010,%10101010 // 5
	.byte	%10101010,%10101010,%10101010 // 5
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%10101010,%10101010,%10101010 // 5
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%11111111,%11111111,%11111111 // 1
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%11111111,%11111111,%11111111 // 1
	.byte	%11111111,%11111111,%11111111 // 1
	.byte	%11111111,%11111111,%11111111 // 1
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%11111111,%11111111,%11111111 // 1
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%10101010,%10101010,%10101010 // 5
	.byte	%01010101,%01010101,%01010101 // d
	.byte	%10101010,%10101010,%10101010 // 5
	.byte	%10101010,%10101010,%10101010 // 5

	* = music.location "Music"
	.fill music.size, music.getData(i)

//////////////////////////////////////////////
// data
//////////////////////////////////////////////

* = $3100 "data"

offset:
	.byte 0

scrollpos:
	.byte 0

colors:
	.byte 1, 1, 1, 1, 1
	.byte 15, 15, 15, 15, 15
	.byte 12, 12, 12, 12, 12
	.byte 11, 11, 11, 11, 11
	.byte 1, 1, 1, 1, 1
	.byte 15, 15, 15, 15, 15
	.byte 12, 12, 12, 12, 12
	.byte 11, 11, 11, 11, 11
	.byte 1, 1, 1, 1, 1
	.byte 15, 15, 15, 15, 15
	.byte 12, 12, 12, 12, 12
	.byte 11, 11, 11, 11, 11

colorpos:
	.byte 0

scrolltext:
	.text " A TRIBUTE TO THE LEGENDARY FAIRLIGHT INTRO BY WOODO.  "
	.byte 0

colorbarpos:
	.byte 74

colorbarindex:
	.byte 0

sinus:
	.fill $80, sinus(i, 37, 87, $80)


* = $3800 "screen"
screen:
	.encoding "ascii"
	.text "                                        "
	.text "                                        "
	.text "                                        "
	.text "aaaaaaaaaaad  gaaaaadaaabd iaaaaaaaaaaaa"
	.text "           fdghb bb b b bfdb            "
	.text "           becbbabbaj b bbeb            "
	.text "           b  bb bb b b bb b            "
	.text "                                        "
	.text "            C R A C K T R O             "
	.text "                                        "
	.text "                                        "
	.text "                                        "
	.text "                                        "
	.text "                PRESENTS                "
	.text "                                        "
	.text "           A TRIBUTE CRACKTRO           "
	.text "                                        "
	.text "        CODED 06.07.21 BY MARTIN        "
	.text "                                        "
	.text "                                        "
	.text "                                        "
scrollline:
	.text "                                        "
	.text "                                        "
	.text "                                        "
	.text "           ORIGINAL BY WOODO            "
