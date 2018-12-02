	device zxspectrum128
	org	#6000
SnaStart:
	di
	ld hl,#9400
	ld de,#9401
	ld bc,257
	ld (hl),#96
	ldir

	ld a,#94
	ld i,a
	im 2

	ld sp,#bfff
	ld a,2
	out (#fe),a
	ei
	ld hl,#4000
	ld de,#4001
	ld bc,#1800
	ld (hl),l
	ldir
;	ld hl,#5800
;	ld de,#5801
	ld b,3
	ld (hl),%00010111
	ldir

	ld b,64
.l0	push bc
	call menyaem
	pop bc
	halt
	djnz .l0

	call pluer
	call genesprite
	call genescrtable
	call genesprtable

kabradabra
	call print
	call coordcount
	call countaddr
	call pluer+5
	ei
;	xor a
;	out (#fe),a
	halt
;	inc a
;	out (#fe),a
	di
	ld (savesp+1),sp
gavgav2:
	ld sp,scrtable

	dup 32
	pop de
	pop hl
	 dup 32
	 ldi
	 edup
	edup
savesp ld sp,#face
	jp kabradabra
pzzzaza equ 2*50
print:
	ld a,0
	or a
	jp z,printtekst
pzauza:
	ld hl,pzzzaza
	dec hl
	ld (pzauza+1),hl
	ld a,h
	or l
	ret nz
	ld hl,pzzzaza
	ld (pzauza+1),hl
	xor a
	ld (print+1),a
	ret
printtekst
	ld hl,tekst
prnttt:
	ld a,(hl)
	inc hl
	ld (printtekst+1),hl
	or a
	jp z,newtekst
notnewtekst:
	ld e,a
	ld d,high font
smsch:
	ld a,0
	or %00010000
	ld l,a
	ld h,#48
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld a,(de):ld(hl),a:inc h:inc d
	ld h,#59
	ld (hl),%01111010
	ld a,(smsch+1)
	inc a
	and 15
	ld (smsch+1),a
	or a
	ret nz
	ld a,1
	ld (print+1),a
	ret
newtekst:
	ld hl,THEloop
	jp prnttt
tekst:
	db "HELLO ALL ON MM "
	db "    HELLO MM2018"
	db "     Q-BONE TEAM"
	db "  PRESENT TO YOU"
	db "       INVITE TO"
	DB "     DIHALT 2018"
	db "GREETS FLYING TO"
	DB "CONSCIOUSNESS ZS"
	DB  " 4D HOOY-PROGRAM"
	DB  "KAKOS NONOS NYUK"
	DB  " ARTX TRIEBKRAFT"
	DB  " MMCM NEDOPC VBI"
	DB  " GEMBABOYS SKRJU"
	DB  "  CPU KABARDCOMP"
	DB  "GOBLINISH KPACKU"
	DB  " SHURAN THESUPER"
	DB  " DEMARCHE TSLABS"
	DB  "  DEBRIS SIBCREW"
	DB  " OUTSIDERS QUITE"
	DB  " MOROZ SPECCY.PL"
	DB  "VINNNY OUTSIDERS"
	DB  "ZEROTEAM B-STATE"
	DB  " TECHNO LAB NUTS"
	DB  "  NOTSOFT EXCESS"
	DB  " HYPE HYDRA BFOX"
	DB  " PERSONAL INVITE"
	DB  "       SERZHSOFT"
	DB  "        ANDY FER"
	DB  "         KOTSOFT"
	DB  "          G0BLIN"
	DB  "           N1K-0"
	DB  "           BUYAN"
	DB  "           QUIET"
	DB  "            MMCM"
	DB  "            BFOX"
	DB  "         AND YOU"
THEloop:
	db "    MORE INFO AT"
	db "   DIHALT.ORG.RU"
	db "   CODE - RASMER"
	db "      MUSIC - EA"
	db 0
genesprtable:
	ld hl,sprtable

	xor a
	ld b,64
.l0	ld d,0
	ld e,a
	ex de,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	inc a
	djnz .l0
	dec a
	ld b,64
.l1	ld d,0
	ld e,a
	ex de,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	dec a
	djnz .l1
	ret

genescrtable:
	ld hl,scrtable
	ld de,#4000
	call .l0
	ld de,#5000
	call .l0
	ld de,#4200
	call .l0
	ld de,#5200
.l0:
	ld b,16
.l1:
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	inc hl
	inc hl

	ld c,4
.l2:
	INC D
	LD A,D
	AND 7
	jr NZ,.lx ;CY=0
	LD A,E
	ADD A,32
	LD E,A
	jr C,.lx  ;CY=1
	LD A,D
	ADD A,-8
	LD D,A     ;CY=1
.lx
	dec c
	jr nz,.l2
	djnz .l1
	ret

genesprite:
	ld hl,krugosprite
	ld de,krugosprite+4096
	ld a,3
.l0	ld bc,4096
	ldir
	ld xh,2
.l1	push hl
	ld b,64
.l2	scf
	ccf
	srl (hl)
	inc hl
	ld c,63
.l3	rr (hl)
	inc hl
	dec c
	jr nz,.l3
	djnz .l2
	pop hl
	dec xh
	jr nz,.l1
	dec a
	jr nz,.l0
	ret

menyaem:
chislo:
	ld a,0

	bit 0,a
	jr z,.l1
	xor #20
.l1
	bit 6,a
	jr z,.l2
	xor #20
.l2


	ld c,a

	ld b,4
.l0:
	ld l,c
	ld e,c
	ld h,high sprite
	ld d,#48

	dup 7
		ld a,(hl)
		ld (de),a
		inc h
		inc d
	edup

	ld a,(hl) ;8
	ld (de),a
	inc h
	ld d,#59
	ld a,(hl)
	ld (de),a

	ld a,c
	add #40
	ld c,a

	djnz .l0

	ld a,(chislo+1)
	inc a
	and #3f
	ld (chislo+1),a
	ret

countaddr:
	ld a,(xcor)
	ld c,a
	rrca
	rrca
	and 31
	ld (sm_2+1),a

	ld a,c ;xcor
	and %00000011
	xor %00000011
	rlca
	rlca
	rlca
	rlca
	or %11000000
	and #f0
	ld l,0
	ld h,a
sm_2
	ld de,0
	add hl,de

	ld (add111+1),hl

	ld a,(ycor)
	add a
	ld yl,a
	ld yh,high sprtable
gavgav:
	ld ix,scrtable+2

	call lpp1
	ld a,yl
	add 30 ;!!!
	ld yl,a
lpp1:
	ld b,16
lpp0:
	ld l,(iy+0)
	inc iy
	ld h,(iy+0)
	inc iy
add111
	ld de,#dead
	add hl,de
	ld (ix+0),l
	inc ix
	ld (ix+0),h
	inc ix
	inc ix
	inc ix
	djnz lpp0
	ret

coordcount:
	ld hl,(x1)
	ld de,200
	add hl,de
	ld (x1),hl

	ld hl,(y1)
	ld de,160
	add hl,de
	ld (y1),hl

	ld hl,(x2)
	ld de,230
	add hl,de
	ld (x2),hl

	ld hl,(y2)
	ld de,80
	add hl,de
	ld (y2),hl


faZZZa
	ld a,0
	inc a
	and 1
	ld (faZZZa+1),a

	ld a,(faZZZa+1)
	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
	ld de,x1+1
	add hl,de
	ex de,hl

	ld a,(de)
	ld h,high sintabx
	ld l,a
	ld a,(hl)
	ld (xcor),a
	inc de
	inc de
	ld a,(de)
	ld h,high sintaby
	ld l,a
	ld a,(hl)
	ld (ycor),a

	ld a,(faZZZa+1)
	rrca
	ld h,0
	ld l,a
	ld de,scrtable+2
	add hl,de
	ld (gavgav+2),hl
	dec hl
	dec hl
	ld (gavgav2+1),hl
	ret

ycor db 0
xcor db 0





x1	dw 0
y1	dw 64
x2	dw 128
y2	dw 192


	align #1000
sprite:
;	ds 2048+256
	incbin "dhLOGO.bin"

	align #100
sintabx:
	db 64,62,60,59,57,56,54,53,51,49,48,46,45,43,42,40
	db 39,37,36,35,33,32,30,29,28,27,25,24,23,22,20,19
	db 18,17,16,15,14,13,12,11,10,9,8,8,7,6,6,5
	db 4,4,3,3,2,2,1,1,1,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,1,1,1,2,2,2,3,3,4
	db 5,5,6,7,7,8,9,10,11,11,12,13,14,15,16,18
	db 19,20,21,22,23,25,26,27,28,30,31,33,34,35,37,38
	db 40,41,43,44,46,47,49,50,52,53,55,56,58,60,61,63
	db 64,66,67,69,71,72,74,75,77,78,80,81,83,84,86,87
	db 89,90,92,93,94,96,97,99,100,101,102,104,105,106,107,108
	db 109,111,112,113,114,115,116,116,117,118,119,120,120,121,122,122
	db 123,124,124,125,125,125,126,126,126,127,127,127,127,127,127,127
	db 127,127,127,127,127,127,127,126,126,126,125,125,124,124,123,123
	db 122,121,121,120,119,119,118,117,116,115,114,113,112,111,110,109
	db 108,107,105,104,103,102,100,99,98,97,95,94,92,91,90,88
	db 87,85,84,82,81,79,78,76,74,73,71,70,68,67,65,64

sintaby:
	db 0,0,0,0,0,0,0,0,0,0,1,1,1,2,2,2
	db 3,3,3,4,4,5,5,6,6,7,7,8,9,9,10,11
	db 11,12,13,13,14,15,16,17,17,18,19,20,21,22,23,23
	db 24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39
	db 40,41,42,43,44,45,46,47,48,49,49,50,51,52,53,54
	db 55,56,57,58,59,59,60,61,62,63

	db	64,64,65,66,67,67
	db 68,69,69,70,71,71,72,72,73,74,74,75,75,75,76,76
	db 77,77,77,78,78,78,78,79,79,79,79,79,79,79,79,79
	db 79,79,79,79,79,79,79,79,79,78,78,78,78,77,77,77
	db 76,76,75,75,75,74,74,73,72,72,71,71,70,69,69,68
	db 67,67,66,65,64,64
	db	63,62,61,60,60,59,58,57,56,55
	db 54,53,52,51,50,49,49,48,47,46,45,44,43,42,41,40
	db 39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24
	db 23,23,22,21,20,19,18,17,17,16,15,14,13,13,12,11
	db 11,10,9,9,8,7,7,6,6,5,5,4,4,3,3,3
	db 2,2,2,1,1,1,0,0,0,0,0,0,0,0,0,0

	align #100
scrtable:
	ds 64*4
	align #100
sprtable:
	ds 128*2
pluer
	include "pt3xplayer.asm
muza
	incbin "lom.pt3"

align #1000
	org #b000
font:
	incbin "font.bin"

	org #c000
krugosprite:
	incbin "spr.bin"

	org pluer
	ld hl,muza

	org #9696
	di
	push iy
	push ix
	pop ix
	pop iy
	ei
	ret
	savesna "mm18intro.sna",SnaStart