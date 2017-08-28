; Obliterate v1.0

;===Standard Includes and Equates===
.nolist
;#define TASM
#include "ti83plus.inc"
#include "keyval.inc"
#include "dcs7.inc"

;===Build Flags for CALCnet2===
#define enableCn22
#define enablecn2eis
#define cn2fakeserial
#define cn2debug

MouseY		.equ	$989E	; [header.asm:141]
MouseX		.equ	$989F	; [header.asm:142]
MAXHISTLENGTH .equ	10
PROGRESSBAR_INNERWIDTH .equ 92
PAGESCROLL_LINES .equ	9

MySafeRAM2		.equ	AppBackupScreen+129			;[cf. GUI Memory Areas]	;166 bytes total
GUIScratchSpace	.equ	MySafeRAM2					;up to 166 bytes, obv

calctextsize	.equ	saferam2					;2 bytes
calcrefssize	.equ	calctextsize+2				;2 bytes
calctextloc		.equ	calcrefssize+2				;2 bytes
calcrefsloc		.equ 	calctextloc+2				;2 bytes
havecontents	.equ	calcrefsloc+2				;1 byte
acceptcontents	.equ	havecontents+1				;1 byte
calctextrecvd	.equ	acceptcontents+1			;2 bytes
calcrefsrecvd	.equ	calctextrecvd+2				;2 bytes
scrolloffset	.equ	calcrefsrecvd+2				;2 bytes
pusheditems		.equ	scrolloffset+2				;1 byte
renderingline	.equ	pusheditems+1				;1 byte
atbottom		.equ	renderingline+1				;1 byte
linklocx		.equ	atbottom+1					;1 byte
linkwidthx		.equ	linklocx+1					;1 byte
ccscratch		.equ	linkwidthx+1				;20 bytes
cchotspotscratch .equ	ccscratch
totallines		.equ	ccscratch+20				;2 bytes
lineat			.equ	totallines+2				;2 bytes

.list
	.org progstart
#IFDEF TI83P
	.db $BB,$6D
#ENDIF
INIT:
	xor d
#IFDEF TI83P
	.db $C9
#ENDIF
	jr Start
	.dw Description
	.db $07,$00
	.dw Icon
	.dw $0000
	.dw $0000			;the routine to open files.  DCS will start you here instead of at $9D95
						;if a file is pending
Description:
	.db "Gossamer Browser v1.0",0
Icon:				;a 16x16 icon
	.db %00000010,%01000000
	.db %00000100,%00100000
	.db %01000100,%00100010
	.db %01001000,%00010010
	.db %00101000,%00010100
	.db %00100100,%00100100
	.db %00010101,%10101000
	.db %00001011,%11010000
	.db %00000111,%11100000
	.db %00000011,%11000000
	.db %00111111,%11111100
	.db %01000111,%11100010
	.db %00001111,%11110000
	.db %00010011,%11001000
	.db %00100001,%10000100
	.db %00100000,%00000100

START:                          	;main routines
	set textwrite,(iy+sgrflags)

Start_DBLoop:						;debounce...
	ld b,16
Start_DBInnerLoop:
	call Cn2_GetK
	or a
	jr nz,Start_DBLoop
	djnz Start_DBInnerLoop
	call Cn2_Setup					;set up calcnet 2.2

Start_Reset:
	ld a,1
	ld ($890F),a					;DO show debug CALCnet2.2 info

Start_Splash:
	OpenGUIStack()
	PushGUIStacks(AboutWindow)
	RenderGUI()
	CloseGUIStack()

	ld hl,0
Splash_Pause_Loop:
	push hl
		pop hl
	dec hl
	push hl	
		pop hl
	ld a,h
	or l
	jr nz,Splash_Pause_Loop
	
	call DeleteHistory

	;create history appvar
	ld hl,StartPage
	call AppendHistory		;takes from hl
	
	;load it
	
Gossamer_Main:
	call DeletePageAppvars
	CloseGUIStack()
	OpenGUIStack()
	PushGUIStacks(MainWindow)
	RenderGUI()
	CloseGUIStack()
	
Gossamer_Loading_Page:
	ld hl,(12*256)+2
	ld (pencol),hl
	ld hl,LoadingTxt
	bcall(_vputs)
	call iFastCopy
	ld hl,$87FA					;Cn2_Send_Buf
	ld b,5
Gossamer_Loading_Page_DestLoop:
	ld (hl),$AA
	inc hl
	djnz Gossamer_Loading_Page_DestLoop
	push hl
		inc hl
		inc hl
		ld (hl),46				;type = URL request
		inc hl
		push hl
			ld hl,AV_HIST
			rst 20h
			bcall(_chkfindsym)
			ex de,hl
			inc hl
			inc hl
			push hl
				call LengthToNewlineWithNewline
				dec bc
				pop hl
			pop de
		push bc
			ldir
			pop bc
		inc bc
		pop hl
	ld (hl),c
	inc hl
	ld a,$80
	or b
	ld (hl),a
Gossamer_Loading_Page_WaitSend:
	call Cn2GetK_Sub
	cp skClear
	jr z,Gossamer_Loading_Page_WaitSend
Gossamer_Loading_Page_WaitSendLoop:
	call Cn2GetK_Sub
	cp skClear
	jp z,Gossamer_Loading_Page_WaitSendEscape
	ld a,(Cn2_Int_SendBuf+6)
	or a
	jp nz,Gossamer_Loading_Page_WaitSendLoop
	
	xor a
	ld h,a
	ld l,a
	ld (calctextsize),hl
	ld (calcrefssize),hl
	ld (calctextrecvd),hl
	ld (calcrefsrecvd),hl
	ld (totallines),hl
	ld (lineat),hl
	ld (havecontents),a
	ld (acceptcontents),a

Gossamer_Fetching_DataLoop:
	call Cn2GetK_Sub
	cp skClear
	jp z,Gossamer_Loading_Page_WaitSendEscape

	ld a,(havecontents)
	or a
	jp nz,Gossamer_MainRenderStart

	ld a,(Cn2_Int_RecBuf+6)
	or a
	jr z,Gossamer_Fetching_DataLoop
	
	;process incoming msg!
	ld hl,Cn2_Int_RecBuf+7
	ld a,(hl)					;msg type
	inc hl						;now at seqno
	cp 48
	jr z,Gossamer_Recv_Text
	cp 49
	jp z,Gossamer_Recv_Refs
	cp 47
	jp nz,Gossamer_Recv_ClrBuf
Gossamer_Recv_Header:
	;this contains sizes.
	inc hl						;skip seqno
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (calctextsize),de
	push de
		inc hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld (calcrefssize),de
		pop hl
	add hl,de
	ld de,32
	add hl,de			;total size needed
	bcall(_EnoughMem)
	jr nc,Gossamer_Recv_Header_EnoughMem

	ld hl,(18*256)+2
	ld (pencol),hl
	ld hl,DocTooBigTxt
	bcall(_vputs)
	call iFastCopy
	xor a
	ld (acceptcontents),a
	jp Gossamer_Recv_ClrBuf
Gossamer_Recv_Header_EnoughMem:
	ld a,1
	ld (acceptcontents),a

	;create page appvar
	ld hl,AV_PAGE
	rst 20h
	ld hl,(calctextsize)
	bcall(_CreateAppVar)
	inc de
	inc de
	ld (calctextloc),de

	;create references appvar
	ld hl,AV_REFS
	rst 20h
	ld hl,(calcrefssize)
	bcall(_CreateAppVar)
	inc de
	inc de
	ld (calcrefsloc),de

	ld hl,(2*256)+55
	ld de,(93*256)+58
	ld a,1					;black
	call fastrectangle
	
	ld hl,(2*256)+58
	ld de,(93*256)+61
	ld a,1					;black
	call fastrectangle
	
	call iFastCopy
	
	jp Gossamer_Recv_ClrBuf
Gossamer_Recv_Text:
	ld a,(acceptcontents)
	or a
	jp z,Gossamer_Recv_ClrBuf
	;save it now
	push hl
		ld e,(hl)
		ld h,250						;chunk offset
		call multhe
		ld de,(calctextloc)
		add hl,de
		push hl
			ld hl,(Cn2_Int_Recbuf+5)
			ld a,$7F
			and h
			ld h,a
			dec hl						;type
			dec hl						;seqno
			push hl
				pop bc
			pop de
		pop hl							;hl=dest,de=source
	inc hl								;skip seqno
	push hl
		ld hl,(calctextrecvd)
		add hl,bc
		ld (calctextrecvd),hl
		pop hl
	ld ix,(totallines)
Gossamer_Recv_Text_LoadLoop:
	ld a,(hl)
	cp $D6
	jp z,Gossamer_Recv_Text_LoadLoop_CRLF
Gossamer_Recv_Text_LoadLoop_CRLF_Ret:
	ldi
	jp pe,Gossamer_Recv_Text_LoadLoop			;even parity means bc != 0
	ld (totallines),ix
	;ldir						;de is byte after contents copied

	;this code is to update the progress bar
	ld hl,(calctextloc)
	or a
	ex de,hl
	sbc hl,de					;now hl is total bytes present so far
	push hl
		ex de,hl
		ld a,PROGRESSBAR_INNERWIDTH
		call multade				;returns ahl
		push de
			pop bc
		ld de,(calctextsize)
		call divabcde				;returns abc.  c should be real width
		ld hl,(2*256)+55
		ld e,57
		ld a,2
		add a,c
		ld d,a
		ld a,1					;black fill
		call filledrectangle
		pop hl
	ld de,(30*256)+2
	ld (pencol),de
	call vdisphl
	ld a,'/'
	bcall(_vputmap)
	ld hl,(calctextsize)
	call vdisphl
	call iFastCopy

	call UpdateHaveContents
	jr Gossamer_Recv_ClrBuf
Gossamer_Recv_Refs:
	ld a,(acceptcontents)
	or a
	jr z,Gossamer_Recv_ClrBuf
	;save it now
	push hl
		ld e,(hl)
		ld h,250						;chunk offset
		call multhe
		ld de,(calcrefsloc)
		add hl,de
		push hl
			ld hl,(Cn2_Int_Recbuf+5)
			ld a,$7F
			and h
			ld h,a
			dec hl						;type
			dec hl						;seqno
			push hl
				pop bc
			pop de
		pop hl							;hl=dest,de=source
	inc hl								;skip seqno
	push hl
		ld hl,(calcrefsrecvd)
		add hl,bc
		ld (calcrefsrecvd),hl
		pop hl
	ldir
	
	;this code is to update the progress bar
	ld hl,(calcrefsloc)
	or a
	ex de,hl
	sbc hl,de					;now hl is total bytes present so far
	push hl
		ex de,hl
		ld a,PROGRESSBAR_INNERWIDTH
		call multade				;returns ahl
		push de
			pop bc
		ld de,(calcrefssize)
		call divabcde				;returns abc.  c should be real width
		ld hl,(2*256)+58
		ld e,60
		ld a,2
		add a,c
		ld d,a
		ld a,1					;black fill
		call filledrectangle
		pop hl
	ld de,(36*256)+2
	ld (pencol),de
	call vdisphl
	ld a,'/'
	bcall(_vputmap)
	ld hl,(calcrefssize)
	call vdisphl
	call iFastCopy

	call UpdateHaveContents
;	jr Gossamer_Recv_ClrBuf	
Gossamer_Recv_ClrBuf:
	ld hl,0
	ld (Cn2_Int_Recbuf+5),hl
	jp Gossamer_Fetching_DataLoop

Gossamer_Recv_Text_LoadLoop_CRLF:
	inc ix
	jp Gossamer_Recv_Text_LoadLoop_CRLF_Ret

UpdateHaveContents:
	ld hl,(calcrefsrecvd)
	ld de,(calcrefssize)
	or a
	sbc hl,de
	push hl
		ld hl,(calctextrecvd)
		ld de,(calctextsize)
		or a
		sbc hl,de
		pop de
	add hl,de
	ld a,h
	or l
	ret nz
	ld a,1
	ld (havecontents),a
	ret

Gossamer_MainRenderStart:
	ld hl,0
	ld (scrolloffset),hl
	OpenGUIStack()
	PushGUIStacks(MainWindow)
	PushGUIStacks(MainScrollAreas)
Gossamer_MainRender:
	ld b,0						;number of pushed items
	ld a,(acceptcontents)
	or a
	jp z,Gossamer_MainRenderFinish
	xor a
	ld (atbottom),a
	ld (renderingline),a
	ld (pusheditems),a
	ld hl,(scrolloffset)
	ld de,(calctextloc)
	add hl,de
Gossamer_MainRenderLoop:
	ld a,(renderingline)
	cp 54
	jp z,Gossamer_MainRenderFinish
	ld de,GUIScratchSpace
	xor a
	ld (de),a
	inc de
	ld a,(renderingline)
	ld (de),a
	inc de
	inc de
	ld bc,3
Gossamer_MainRenderInnerLoop:
	ld a,(hl)
	cp $D6
	jr nz,Gossamer_MainRenderInnerLoop1
	xor a
Gossamer_MainRenderInnerLoop1:
	push af
		cp $C1			;'['
		jp nz,Gossamer_MainRenderInnerLoop_NoHotspot
		push hl
			push bc
				ld b,3
Gossamer_MainRenderInnerLoop1Inner:
				inc hl
				ld a,(hl)
				cp ']'
				jr z,Gossamer_MainRenderInnerLoop1InnerComplete
				cp '0'
				jp c,Gossamer_MainRenderInnerLoop_NoHotspotPop
				cp '9'+1
				jp nc,Gossamer_MainRenderInnerLoop_NoHotspotPop
				inc b \ inc b \ inc b \ inc b
				jr Gossamer_MainRenderInnerLoop1Inner
Gossamer_MainRenderInnerLoop1InnerComplete:
				inc b \ inc b \ inc b
				ld a,b
				ld (linkwidthx),a
				ld a,(pusheditems)
				inc a
				inc a
				ld (pusheditems),a
				push de
					ld hl,GUIScratchSpace+3
					ex de,hl
					or a
					sbc hl,de
					ld a,l
					dec de
					ld (de),a
					or a
					jr z,Gossamer_MainRenderInnerLoop1InnerComplete1
					ex de,hl
					bcall(_SStringLength)
Gossamer_MainRenderInnerLoop1InnerComplete1:
					;string length is in accumulator
					ld (linklocx),a
					ld hl,ccscratch
					push hl
						ld a,(renderingline)
						add a,10
						ld (hl),a
						ld b,a
						inc hl
						ld a,(linklocx)
						ld (hl),a
						ld c,a
						inc hl
						ld a,5
						add a,b
						ld (hl),a
						inc hl
						ld a,(linkwidthx)
						dec a
						add a,c
						ld (hl),a
						inc hl
						ld de,CursorHand
						ld bc,16
						ex de,hl
						ldir
						pop hl
					ld a,GUIRMouseCursor
					ld de,20
					call PushGUIStack
					
					ld hl,ccscratch
					push hl
						ld a,(linklocx)
						ld (hl),a
						inc hl
						ld a,(renderingline)
						;add a,10
						ld (hl),a
						inc hl
						ld a,(linkwidthx)
						ld (hl),a
						inc hl
						ld a,6
						ld (hl),a
						inc hl
						ld de,Gossamer_Link_Clicked
						ld (hl),e
						inc hl
						ld (hl),d
						pop hl
					ld a,GUIRHotspot
					ld de,6
					call PushGUIStack

					pop de
Gossamer_MainRenderInnerLoop_NoHotspotPop:
				pop bc
			pop hl
Gossamer_MainRenderInnerLoop_NoHotspot:
		pop af
	ld (de),a
	inc bc
	inc hl
	inc de
	or a
	jp nz,Gossamer_MainRenderInnerLoop
	push bc
		pop de
	push hl
		xor a
		ld (GUIScratchSpace+2),a			;font number
		ld hl,GUIScratchSpace
		ld a,GUIRText
		call PushGUIStack
		pop hl
	ld a,(pusheditems)
	inc a
	ld (pusheditems),a
	ld a,(renderingline)
	add a,6
	ld c,a
	dec hl
	ld a,(hl)
	inc hl
	or a
	jr nz,Gossamer_MainRenderInnerLoop2
	ld c,54
	ld a,1
	ld (atbottom),a
Gossamer_MainRenderInnerLoop2:
	ld a,c
	ld (renderingline),a	
	jp Gossamer_MainRenderLoop
	
Gossamer_MainRenderFinish:

Gossamer_MainMouse:
	GUIMouse(MainMouseHook)
	
Gossamer_ScrollPageUp:
	call ResetAppPage
	ld a,(acceptcontents)
	or a
	jr z,Gossamer_MainMouse
	ld hl,(lineat)
	ld a,h
	or l
	jr z,Gossamer_MainMouse
	ld a,(pusheditems)
	ld b,a
	call PopGUIStacks
	
	ld b,PAGESCROLL_LINES
Gossamer_ScrollPageUpLoop:
	ld hl,(lineat)
	ld a,h
	or l
	jp z,Gossamer_MainRender
	
	push bc
		call ScrollUpSub
		pop bc

	djnz Gossamer_ScrollPageUpLoop
	jp Gossamer_MainRender
	
Gossamer_ScrollPageDown:
	call ResetAppPage
	ld a,(acceptcontents)
	or a
	jr z,Gossamer_MainMouse
	ld a,(atbottom)
	or a
	jr nz,Gossamer_MainMouse
	ld a,(pusheditems)
	ld b,a
	call PopGUIStacks
	
	ld b,PAGESCROLL_LINES
Gossamer_ScrollPageDownLoop:
	ld hl,(totallines)
	ld de,(lineat)
	or a
	sbc hl,de
	;eg, 10 total lines, at line 1 (of 0-9), meaning we shouldn't go further, if 9 onscreen lines
	or a
	ld de,PAGESCROLL_LINES
	sbc hl,de
	dec hl
	ld a,h
	and %10000000
	jp nz,Gossamer_MainRender

	push bc
		call ScrollDownSub
		pop bc
	djnz Gossamer_ScrollPageDownLoop

	jp Gossamer_MainRender
	
Gossamer_ScrollUp:
	call ResetAppPage
	ld a,(acceptcontents)
	or a
	jr z,Gossamer_MainMouse
	ld hl,(scrolloffset)
	ld a,h
	or l
	jr z,Gossamer_MainMouse
	ld a,(pusheditems)
	ld b,a
	call PopGUIStacks
	call ScrollUpSub
	jp Gossamer_MainRender

Gossamer_ScrollDown:
	call ResetAppPage
	ld a,(acceptcontents)
	or a
	jp z,Gossamer_MainMouse
	ld a,(atbottom)
	or a
	jp nz,Gossamer_MainMouse
	ld a,(pusheditems)
	ld b,a
	call PopGUIStacks
	call ScrollDownSub
	jp Gossamer_MainRender
	
ScrollUpSub:
	ld hl,(calctextloc)
	ld bc,(scrolloffset)
	dec bc						;start at last $D6
	add hl,bc
Gossamer_ScrollUpLoop:
	dec hl
	dec bc
	ld a,c
	or b
	jr z,Gossamer_ScrollUpLoopFound
	ld a,(hl)
	cp $D6
	jr nz,Gossamer_ScrollUpLoop
	inc bc
Gossamer_ScrollUpLoopFound:
	ld (scrolloffset),bc
	ld hl,(lineat)
	dec hl
	ld (lineat),hl
	ret

ScrollDownSub:
	ld hl,(calctextloc)
	ld de,(scrolloffset)
	push de
		add hl,de
		call LengthToNewLineWithNewLine
		;returns bc
		pop hl
	add hl,bc
	ld (scrolloffset),hl
	ld hl,(lineat)
	inc hl
	ld (lineat),hl
	ret

OpenArbitraryURL:
	call ResetAppPage
	
	;Set up GUI
	ld hl,OAU_GUI
	call PushGUIStacks

	;do GUIMouse
	GUIMouse(0)

OAU_Cancel:
	call ResetAppPage
	ld b,6					;OAU_GUI_NUMITEMS
	call PopGUIStacks
	jp Gossamer_MainMouse

OAU_Go:
	call ResetAppPage
	
	;copy URL to temporary space
	ld a,1
	call GUIFindThis
	push hl
		ld hl,-10
		add hl,bc
		push hl
			pop bc			;bc = size of text
		pop hl
	ld de,10
	add hl,de				;hl = start of text
	ld de,GUIScratchSpace
	ldir
	xor a
	ld (de),a
	
	CloseGUIStack()
	
	ld hl,GUIScratchSpace
	call AppendHistory
	jp Gossamer_Main

CloseGossamer:
	call ResetAppPage
Gossamer_Quit:
	CloseGUIStack()
	call Cn2_Setdown
	call DeletePageAppvars
	call DeleteHistory
	ret
	
Gossamer_Link_Clicked:
	call ResetAppPage
	ld hl,(calctextloc)
	ld de,(scrolloffset)
	add hl,de
	xor a
	ld (renderingline),a
Gossamer_Link_Clicked_FindLine:
	ld a,(MouseY)
	sub 9					;was 10
	ld b,a
	ld a,(renderingline)
	add a,6
	cp b
	jr nc,Gossamer_Link_Clicked_FoundLine
	push hl
		call LengthToNewLineWithNewLine
		pop hl
	add hl,bc
	ld a,(renderingline)
	add a,6
	ld (renderingline),a
	jr Gossamer_Link_Clicked_FindLine
Gossamer_Link_Clicked_FoundLine:
	ld de,GUIScratchSpace+1
Gossamer_Link_Clicked_FindLinkInLine:
	ld a,(hl)
	cp $D6
	jr nz,Gossamer_Link_Clicked_FindLinkInLine1
	xor a
Gossamer_Link_Clicked_FindLinkInLine1:
	push af
		cp $C1			;'['
		jp nz,Gossamer_Link_Clicked_FindLinkInLine_NoHotspot
		push hl
			ld b,3
Gossamer_Link_Clicked_FindLinkInLine1Inner:
			inc hl
			ld a,(hl)
			cp ']'
			jr z,Gossamer_Link_Clicked_FindLinkInLine1InnerComplete
			cp '0'
			jp c,Gossamer_Link_Clicked_FindLinkInLine_NoHotspotPop
			cp '9'+1
			jp nc,Gossamer_Link_Clicked_FindLinkInLine_NoHotspotPop
			inc b \ inc b \ inc b \ inc b
			jr Gossamer_Link_Clicked_FindLinkInLine1Inner
Gossamer_Link_Clicked_FindLinkInLine1InnerComplete:
			inc b \ inc b \ inc b
			ld a,b
			ld (linkwidthx),a
			push de
				ld hl,GUIScratchSpace+1
				ex de,hl
				or a
				sbc hl,de
				ld a,l
				dec de
				ld (de),a
				or a
				jr z,Gossamer_Link_Clicked_FindLinkInLine1InnerComplete1
				ex de,hl
				bcall(_SStringLength)
Gossamer_Link_Clicked_FindLinkInLine1InnerComplete1:
				;string length is in accumulator
				ld (linklocx),a
				ld b,a
				ld a,(MouseX)
				dec a
				cp b
				jp c,Gossamer_Link_Clicked_FindLinkInLine_WrongLink
				ld c,a
				ld a,(linkwidthx)
				add a,b
				cp c
				jp c,Gossamer_Link_Clicked_FindLinkInLine_WrongLink
Gossamer_Link_Clicked_FindLinkInLine_RightLink:
				pop de
			pop hl
		pop af
	inc hl
	ld de,0
Gossamer_Link_Clicked_FindLinkNumber:
	ld a,(hl)
	cp ']'
	jr z,Gossamer_Link_Clicked_FoundLinkNumber
	push hl
		ld a,10
		call multade
		pop de
	ex de,hl
	push hl
		ld a,(hl)
		sub '0'
		ld l,a
		ld h,0
		add hl,de
		pop de
	ex de,hl
	inc hl
	jr Gossamer_Link_Clicked_FindLinkNumber
Gossamer_Link_Clicked_FoundLinkNumber:
	ld a,e
	or d
	jp z,Gossamer_MainMouse
	dec de

	;de is link offset
	ld hl,(calcrefsloc)
Gossamer_Link_Clicked_NextLink:
	ld a,e
	or d
	jr z,Gossamer_Link_Clicked_LinkLocated
Gossamer_Link_Clicked_NextLinkInner:
	ld a,(hl)
	or a
	jp z,Gossamer_MainMouse
	inc hl
	cp $D6
	jr nz,Gossamer_Link_Clicked_NextLinkInner
	dec de
	jr Gossamer_Link_Clicked_NextLink
Gossamer_Link_Clicked_LinkLocated:
	push hl
		pop de
Gossamer_Link_Clicked_LinkTerminate:
	inc hl
	ld a,(hl)
	or a
	jr z,Gossamer_Link_Clicked_LinkTermination
	cp $D6
	jr nz,Gossamer_Link_Clicked_LinkTerminate
Gossamer_Link_Clicked_LinkTermination:
	or a
	sbc hl,de
	push hl
		pop bc
	push de
		pop hl
	ld de,GUIScratchSpace
	push de
		ldir
		xor a
		ld (de),a
		pop hl
	call AppendHistory
	jp Gossamer_Main
Gossamer_Link_Clicked_FindLinkInLine_WrongLink:
				pop de
Gossamer_Link_Clicked_FindLinkInLine_NoHotspotPop:
			pop hl
Gossamer_Link_Clicked_FindLinkInLine_NoHotspot:
		pop af
	ld (de),a
	inc hl
	inc de
	or a
	jp nz,Gossamer_Link_Clicked_FindLinkInLine
	;FAILURE.  Drop out instead of crashing. Debugging: off-by-N errors in link boundary checking?
	jp Gossamer_Quit

Gossamer_Loading_Page_WaitSendEscape:
	jp Gossamer_Quit
	
MainMouseHook:
	im 2				;because GUIMouse APD sets IM 1
	ei
	ld a,$ff \ out (1),a
    ld 	a,$fd	;group for ENTER
    out (1), a
    nop \ nop	;let the keyboard settle
    in 	a,(1)
;    bit 0,a
;    jr	z, ClickSend
	cp 	dkClear
	jr z,clearpressed
	cp dkMinus
	jr z,hookscrollup
	cp dkPlus
	jr z,hookscrolldown
	cp dkDiv
	jr z,hookscrollpageup
	cp dkMul
	jr z,hookscrollpagedown
	ret
clearpressed:
	ld hl, MouseY
	ld (hl), 2
	inc hl
	ld (hl), 91
	pop hl
	ret
hookscrollup:
	ld hl, MouseY
	ld (hl), 3
	inc hl
	ld (hl), 102
	pop hl
	ret
hookscrolldown:
	ld hl, MouseY
	ld (hl), 11
	inc hl
	ld (hl), 102
	pop hl
	ret
hookscrollpageup:
	ld hl, MouseY
	ld (hl), 19
	inc hl
	ld (hl), 102
	pop hl
	ret
hookscrollpagedown:
	ld hl, MouseY
	ld (hl), 27
	inc hl
	ld (hl), 102
	pop hl
	ret
Gossamer_GoBack:
	call ResetAppPage
	call GetHistDepth				;returns a
	cp 2
	jp c,Gossamer_MainMouse
RemoveHistoryLatest:
	CloseGUIStack()
	;DOOOO ITTTT
	ld hl,AV_HIST
	rst 20h
	bcall(_chkfindsym)
	jp c,Gossamer_Main
	push de
		ex de,hl
		ld c,(hl)
		inc hl
		ld b,(hl)
		push bc
			inc hl
			push hl
				call LengthToNewlineWithNewline
				;returns bc
				push bc
					pop de
				pop hl
			push bc
				bcall(_DelMem)		;DE bytes at HL
				pop de
			pop hl
		or a
		sbc hl,de
		pop de
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
	jp Gossamer_Main

AppendHistory:
	push hl
		ld hl,AV_HIST
		rst 20h
		bcall(_chkfindsym)
		jp nc,AppendHistoryExists
		pop hl
	push hl
		call LengthToZeroExcludingZero
		push bc
			push bc
				pop hl
			inc hl						;for the $D6
			bcall(_CreateAppVar)
			inc de
			inc de
			pop bc
		pop hl
	ldir
	ld a,$D6
	ld (de),a
	ret	
AppendHistoryExists:
		;start at this level - hl is pushed
		pop hl
	push hl
		push de
			call LengthToZeroExcludingZero
			inc bc			;add the $D6
			pop hl
		push hl
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			add hl,bc
			pop de
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		push bc
			push bc
				pop de
			ex de,hl
			bcall(_InsertMem)
			pop bc
		pop hl
	dec bc
	ldir
	ld a,$D6
	ld (de),a
	
	call GetHistDepth
	cp MAXHISTLENGTH+1
	ret c
	;Delete one item from history!
	ld hl,AV_HIST
	rst 20h
	bcall(_chkfindsym)
	push de
		ld b,MAXHISTLENGTH-1
DeleteOldestHistory_FindStartLoopOuter:
DeleteOldestHistory_FindStartLoopInner:
		ld a,(de)
		inc de
		cp $D6
		jr nz,DeleteOldestHistory_FindStartLoopInner
		djnz DeleteOldestHistory_FindStartLoopOuter
		
		push hl
			call LengthToNewLineWithNewline
			push bc
				pop de
			pop hl
		push bc
			bcall(_DelMem)		;DE bytes at HL
			pop bc
		pop hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	push hl
		push bc
			pop hl
		ex de,hl
		or a
		sbc hl,de
		pop de
	ex de,hl
	ld (hl),d
	dec hl
	ld (hl),e
	ret
	
GetHistDepth:
	ld hl,AV_HIST
	rst 20h
	bcall(_chkfindsym)
	xor a
	ret c
	ld b,a
	ex de,hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
GetHistDepthLoop:
	ld a,(hl)
	cp $D6
	jr nz,GetHistDepthLoop1
	inc b
GetHistDepthLoop1:
	inc hl
	dec de
	ld a,d
	or e
	jr nz,GetHistDepthLoop
	ld a,b
	ret
	
LengthToNewLineWithNewline:
	ld bc,0
LengthToNewLineWithNewlineLoop:
	inc bc
	ld a,(hl)
	inc hl
	cp $D6
	jr nz,LengthToNewLineWithNewlineLoop
	ret

LengthToZeroExcludingZero:
	ld bc,-1
LengthToZeroExcludingZeroLoop:
	inc bc
	ld a,(hl)
	inc hl
	or a
	jr nz,LengthToZeroExcludingZeroLoop
	ret
	
Gossamer_About:
	call ResetAppPage
	PushGUIStacks(AboutWindow)
	GUIMouse(0)
	
CloseAboutWindow:
	call ResetAppPage
	PopGUIStacks(3)
	jp Gossamer_Main

Gossamer_Help:
	call ResetAppPage
	PushGUIStacks(HelpWindow)
	GUIMouse(0)
	
CloseHelpWindow:
	call ResetAppPage
	PopGUIStacks(3)
	jp Gossamer_Main

QuickLCDClear:
	ld hl,gbuf
	push hl
		pop de
	inc de
	ld (hl),0
	ld bc,767
	ldir							;clear the gbuf
	ret
	
DeletePageAppvars
	ld hl,AV_PAGE
	rst 20h
	bcall(_chkfindsym)
	jr c,DeleteRefAppvar
	bcall(_DelVar)
DeleteRefAppvar:
	ld hl,AV_REFS
	rst 20h
	bcall(_chkfindsym)
	ret c
	bcall(_DelVar)
	ret

DeleteHistory:
	ld hl,AV_HIST
	rst 20h
	bcall(_chkfindsym)
	ret c
	bcall(_DelVar)
	ret

Cn2GetK_Sub:
	push hl
		push bc

			ld a,$ff		;
			out (1),a		;reset keyport
			ld e,$fe		;frist group
			ld c,$01		;key port
			ld l,0		;l holds key pressed
cscloop:
			out (c),e		;set keygroup
			ld b,8		;loop, Delay needed when work with key driver
			nop \ nop
			in a,(c)		;read key
cscbit:
			inc l			;inc to get key pressed
			rra 			; if key pressed done
			jr nc,donecsc
			djnz cscbit 	;loop 8
			rlc e			;next key group
			jp m,cscloop	;if bit 7 set loop
			ld l,0		;if no key pressed 0
donecsc:
			ld a,l		;
			or a
			pop bc
		pop hl
	ret

multade:
	ld hl,0
	ld c,l
	add	a,a		; optimised 1st iteration
	jr	nc,$+4
	ld	h,d
	ld	l,e
	ld b,7
multade_loop:
	add	hl,hl		; unroll 7 times
	rla			; ...
	jr	nc,$+4		; ...
	add	hl,de		; ...
	adc	a,c		; ...
	djnz multade_loop
	ret

;Input: A:BC = Dividend, DE = Divisor, HL = 0
;Output: A:BC = Quotient, HL = Remainder
divabcde:
	ld hl,0
	ld ix,24
	push af
		push hl
divabcde_loop:
			pop hl
		pop af
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr	nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	push af
		push hl
			dec ix
			push ix
				pop hl
			ld a,l
			or a
			jr nz,divabcde_loop
			pop hl
		pop af
	ret


;--------------------------------------------------------------------------------
; Main Window
;--------------------------------------------------------------------------------
MainWindow:
	.dw MainWindowE-MainWindow
	.db GUIRLargeWin
	.db %01111000
	.db %10000000
	.db %10011000
	.db %10001000
	.db %01110000
	.db " Gossamer 1.0",0
MainWindowE:
MainWinButtons:
	; The close button
	.dw MainWinButtonsE - MainWinButtons
	.db GUIRWinButtons
	.db %01100000
	.dw 0
	.dw OpenArbitraryURL
	.dw CloseGossamer
MainWinButtonsE:
MainWinAbout:
	.dw MainWinAboutE - MainWinAbout
	.db GUIRSprite
	.db 75,-8
	.db 7
	.db %01111100
	.db %10000000
	.db %10010000
	.db %10111000
	.db %10101000
	.db %10000000
	.db %01111100
MainWinAboutE:
MainWinAboutHS:
	.dw MainWinAboutHSE - MainWinAboutHS
	.db GUIRHotspot
	.db 75,-8
	.db 6,7
	.dw Gossamer_About
MainWinAboutHSE:
MainWinBack:
	.dw MainWinBackE - MainWinBack
	.db GUIRSprite
	.db 69,-8
	.db 7
	.db %01111100
	.db %10001000
	.db %10011000
	.db %10111000
	.db %10011000
	.db %10001000
	.db %01111100
MainWinBackE:
MainWinBackHS:
	.dw MainWinBackHSE - MainWinBackHS
	.db GUIRHotspot
	.db 69,-8
	.db 6,7
	.dw Gossamer_GoBack
MainWinBackHSE:
MainWinHelp:
	.dw MainWinAboutE - MainWinAbout
	.db GUIRSprite
	.db 63,-8
	.db 7
	.db %01111100
	.db %10111000
	.db %10001000
	.db %10010000
	.db %10000000
	.db %10010000
	.db %01111100
MainWinHelpE:
MainWinHelpHS:
	.dw MainWinHelpHSE - MainWinHelpHS
	.db GUIRHotspot
	.db 63,-8
	.db 6,7
	.dw Gossamer_Help
MainWinHelpHSE:
	.db $ff,$ff
	
MainScrollAreas:
MainWinScrollUpHS:
	.dw MainWinScrollUpHSE - MainWinScrollUpHS
	.db GUIRHotspot
	.db 100,-8
	.db 8,8
	.dw Gossamer_ScrollUp
MainWinScrollUpHSE:
MainWinScrollDownHS:
	.dw MainWinScrollDownHSE - MainWinScrollDownHS
	.db GUIRHotspot
	.db 100,0
	.db 8,8
	.dw Gossamer_ScrollDown
MainWinScrollDownHSE:
MainWinScrollPageUpHS:
	.dw MainWinScrollPageUpHSE - MainWinScrollPageUpHS
	.db GUIRHotspot
	.db 100,8
	.db 8,8
	.dw Gossamer_ScrollPageUp
MainWinScrollPageUpHSE:
MainWinScrollPageDownHS:
	.dw MainWinScrollPageDownHSE - MainWinScrollPageDownHS
	.db GUIRHotspot
	.db 100,16
	.db 8,8
	.dw Gossamer_ScrollPageDown
MainWinScrollPageDownHSE:
	.db $ff,$ff
;--------------------------------------------------------------------------------
; About Window
;--------------------------------------------------------------------------------
AboutWindow:
	; The small window
	.dw AboutPicData - AboutWindow
	.db 2
	.db 7,12
	.db %01100000
	.db %10010000
	.db %00100000
	.db %00000000
	.db %00100000	
	.db "About Gossamer",0
AboutPicData:
	; The about text
	.dw AboutWindowButtons - AboutPicData
	.db GUIRLargeSprite
	.db 0,0
	.db 10,39
	.db %11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11111111,%11111100,%00001111,%11111111,%11100001,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11111111,%11100011,%00110000,%01111111,%11101111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11111111,%10010110,%11000110,%10111111,%11001111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11111110,%01000001,%00110111,%01001111,%11001111,%11000111,%00011000,%11000110,%00101110,%00100100
	.db %11111101,%01101010,%01100000,%00000111,%11001111,%11010110,%01110011,%11110110,%01010100,%10100010
	.db %11111011,%01000011,%00011011,%10110011,%11001100,%10010010,%01110011,%11000010,%01010100,%10100010
	.db %11110110,%00000001,%10111001,%11011001,%11001100,%10010011,%00111001,%10010010,%01010100,%00100110
	.db %11100000,%00000000,%00000100,%00001010,%11001100,%10010011,%10011100,%10010010,%01010100,%11100110
	.db %11101110,%00000011,%10111101,%11010001,%01101101,%11010111,%10011100,%10010010,%01010100,%11100110
	.db %11000000,%00000011,%10000101,%11011100,%01100001,%11000110,%00110001,%11000110,%01010110,%00100110
	.db %11010000,%00000000,%10111000,%01011101,%01111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11010100,%00000011,%00111011,%10001101,%10111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %10010010,%00010100,%11001011,%11000101,%10110011,%00111011,%01110100,%10001001,%11111110,%11111010
	.db %10110100,%10010110,%11110011,%11011001,%10110011,%01010101,%01110101,%10011010,%11010100,%11110100
	.db %00101010,%11011001,%01110100,%10111100,%00110101,%00110101,%01010110,%10111001,%11010110,%11110100
	.db %01011000,%11100011,%10101111,%00111101,%10110011,%01011011,%10101100,%10001010,%11101100,%01011010
	.db %01010101,%00001101,%11001111,%10011011,%10011111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %00101101,%01101101,%10001111,%01101011,%01011111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %01101101,%01101110,%01110110,%11110011,%01011111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %01100011,%01110100,%01111000,%11110001,%01011111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %00101000,%00000011,%01111101,%11101110,%01011111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %01011011,%00110111,%10110010,%11101110,%01011111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %01011011,%01110011,%10001111,%01011110,%00111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %10010011,%10111011,%10001111,%10011101,%10111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %10011001,%10111010,%01101111,%01011011,%01111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11011011,%00000001,%11101110,%11101011,%01111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11101011,%10111101,%11110101,%11110110,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11110011,%10111101,%11110011,%11101001,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11111000,%11011110,%10001001,%10011001,%11110101,%00010011,%01011010,%11011001,%10001011,%01100110
	.db %11111101,%00000000,%01111101,%01110011,%11110011,%00110101,%00011000,%10101010,%11011010,%10101010
	.db %11111101,%11011111,%01111100,%11101111,%11110101,%01110011,%01011010,%10001001,%11011010,%00101010
	.db %11111110,%01101111,%01110011,%01011111,%11110101,%00010101,%01011010,%10101010,%11011010,%10101010
	.db %11111111,%10000000,%00001111,%00111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110
	.db %11111111,%11000111,%11011000,%11111111,%11111001,%00010101,%00010001,%00011110,%01010111,%11111110
	.db %11111111,%11111000,%00000111,%11111111,%11110111,%00110001,%00111011,%00111101,%11000111,%11111110
	.db %11111111,%11111111,%11111111,%11111111,%11110111,%01110101,%01111011,%01111101,%11000111,%11111110
	.db %11111111,%11111111,%11111111,%11111111,%11111001,%00010101,%00011011,%00010110,%01010111,%11111110
	.db %11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110
AboutWindowButtons:
	; The close button
	.dw AboutWindowE - AboutWindowButtons
	.db 5
	.db %00100000
	.dw 0, 0
	.dw CloseAboutWindow
AboutWindowE:
	; The end
	.db	$FF,$FF
	
OAU_GUI:
	.dw OAU_GUI_SME-OAU_GUI
	.db GUIRSmallWin
	.db 7,12
	.db %11111000
	.db %11111000
	.db %10001000
	.db %10001000
	.db %11111000	
	.db "Open URL",0
OAU_GUI_SME:
OAU_GUI_Text:
	.dw OAU_GUI_TextE-OAU_GUI_Text
	.db GUIRText
	.db 1,1
	.db 0
	.db "Enter URL:",0
OAU_GUI_TextE:
OAU_GUI_TextIn:
	.dw OAU_GUI_TextInE-OAU_GUI_TextIn
	.db GUIRTextLineIn
	.db 1,8
	.db 75
	.dw 160
	.dw 0
	.db "http://",0
OAU_GUI_TextInE:
OAU_GUI_OK:
	.dw OAU_GUI_OKE-OAU_GUI_OK
	.db GUIRButtonText
	.db 8,20
	.dw OAU_Go
	.db "OK",0
OAU_GUI_OKE:
OAU_GUI_Cancel:
	.dw OAU_GUI_CancelE-OAU_GUI_Cancel
	.db GUIRButtonText
	.db 22,20
	.dw OAU_Cancel
	.db "Cancel",0
OAU_GUI_CancelE:
OAU_GUI_WinButtons:
	.dw OAU_GUI_WinButtonsE-OAU_GUI_WinButtons
	.db GUIRWinButtons
	.db %00100000
	.dw 0,0
	.dw OAU_Cancel
OAU_GUI_WinButtonsE:
	.db $ff,$ff

;--------------------------------------------------------------------------------
; About Window
;--------------------------------------------------------------------------------
HelpWindow:
	; The small window
	.dw HelpText - HelpWindow
	.db 2
	.db 7,12
	.db %01100000
	.db %10010000
	.db %00100000
	.db %00000000
	.db %00100000	
	.db "Gossamer Help",0
HelpText:
	.dw HelpWindowButtons - HelpText
	.db GUIRWrappedText
	.db 1,1,75,0
	.db "Gossamer Web Browser",$D6
	.db "(c) 2011 Kerm Martian",$D6
	.db "Scroll: ",$C1,"+]",$C1,"-]",$D6
	.db "Page Scroll: ",$C1,"*]",$C1,"/]",$D6
	.db "http://www.cemetech.net",0
HelpWindowButtons:
	; The close button
	.dw HelpWindowE - HelpWindowButtons
	.db 5
	.db %00100000
	.dw 0, 0
	.dw CloseHelpWindow
HelpWindowE:
	; The end
	.db	$FF,$FF

AV_PAGE:
	.db $15,"GPage",0
AV_REFS:
	.db $15,"GRefs",0
AV_HIST:
	.db $15,"GHist",0
	
StartPage:
	.db "https://www.cemetech.net/gcn/portal.html",0
	;.db "http://www.cemetech.net",0
	
LoadingTxt:
	.db "Loading page...",0
	
DocTooBigTxt:
	.db $C1,"Doc too large",$21,"]",0
	
CursorHand:
	.db %11001111		;8 bytes of mask
	.db %11000111
	.db %11000011
	.db %10000001
	.db %00000000
	.db %10000000
	.db %11000000
	.db %11100001
	.db %00110000		;8 bytes of cursor
	.db %00101000
	.db %00101100
	.db %01100010
	.db %10100001
	.db %01000001
	.db %00100001
	.db %00010010

.end
END
