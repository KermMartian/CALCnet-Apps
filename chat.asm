; Program Name: Chat
; Author: 		Shaun McFall
; Version: 		.1
; Date: 		Fall '10
; Written for Doors CS 7.2 and higher (http://dcs.cemetech.net)

//#define NO_APD		;for Maker Faire 2012

.nolist
#include "ti83plus.inc"
#include "dcs7.inc"
#include "keyval.inc"
MouseY		.equ	$989E	; [header.asm:141]
MouseX		.equ	$989F	; [header.asm:142]
MySafeRAM	.equ	AppBackupScreen+129	;[cf. GUI Memory Areas]
	;CALCnet starts putting stuff at $9999. That leaves 295 bytes between AppBackupScreen
	;and the start of the Cn2.2 stuff. Since the GUI stuff is reserving 129 bytes, that
	;leaves us with 166 bytes
MySafeRAM2				.equ	$8A3A
InputBackupRAM			.equ	textShadow	;128 bytes / textshadow
UserSIDBuf				.equ	MySafeRAM2	;14 bytes per entry: [5 bytes SID, 8 bytes username, 1 byte timeout]
KnownCalcCount 			.equ	MySafeRAM					;1  byte
OutWindowDataLocation	.equ	KnownCalcCount+1			;2  bytes
OutWindowE_Len			.equ	OutWindowDataLocation+2		;2	bytes
NamesWindowLocation		.equ	OutWindowE_Len+2			;2  bytes
MsgLenBuf				.equ	NamesWindowLocation+2		;2	bytes
PingSendTimer			.equ	MsgLenBuf+2					;2	bytes
SendActiveFlag			.equ	PingSendTimer+2				;1 byte
JoinPartMsgStage		.equ	SendActiveFlag+1			;27 bytes max (see below)
															;**12345678 joined0
															;**12345678 parted0
															;**12345678 is now ABCDEFGH0
															;---------------------------
															;123456789012345678901234567

NameEntryLength			.equ	5+8+1	; calc_id + name_length + timeout_byte
PingResetVal			.equ	$4000
PingTimeoutVal			.equ	15
OutWindowAppVarSize		.equ	512

.list
	.org progstart
	.db $BB,$6D
Init:
	xor d
	ret
	jr Start
	
	.dw Description			;or .dw $0000 if you don't have a description
	.db $07,$00				;always this string
	.dw Icon				;or .dw $0000 if you don't have an icon
	.dw 0000				;usually .dw $0000 if you don't have or know what an ALE is
Description:
	.db "Chat for CalcNet2.2",0
Icon:
	.db %00000000,%00000000
	.db %00000000,%00000000
	.db %00000111,%11111100
	.db %00000100,%00000100
	.db %00000100,%00000100
	.db %01111110,%01111110
	.db %01000010,%01000010
	.db %01011010,%01011010
	.db %01000010,%01000010
	.db %01111110,%01111110
	.db %01101010,%01101010
	.db %01010110,%01010110
	.db %01101010,%01101010
	.db %01111110,%01111110
	.db %00000000,%00000000
	.db %00000000,%00000000

Start:
	; Set up the appvars
	call Cn2_Setup
	ld a,1
	ld ($890F),a
	
	ld b,5
	ld hl,MySID
	ld de,$86EE
StartCheckSIDLoop:
	ld a,(de)
	cp (hl)
	jr nz,StartAskUsername
	inc hl
	inc de
	djnz StartCheckSIDLoop
	
	ld a,(MyUsername)
	or a
	jr nz,StartNoAskUsername

StartAskUsername:
	CloseGUIStack()
	OpenGUIStack()
	PushGUIStack(GUIRSmallWin,UsernameWinData,UsernameWinDataE-UsernameWinData)
	PushGUIStack(GUIRWinButtons, UsernameWinButtons, UsernameWinButtonsE-UsernameWinButtons)
	PushGUIStack(GUIRTextLineIn,UsernameIn,UsernameInE-UsernameIn)
	PushGUIStack(GUIRButtonText,UsernameSetButton,UsernameSetButtonE-UsernameSetButton)
	GUIFindThis(1)
	ld de,10
	add	hl,de
	ld de,MyUsername
	ld bc,8
	ex de,hl
	ldir
	GUIMouse(0)
StartAskUsernameProcess:
	call ResetAppPage
	GUIFindThis(1)
	ld de,10
	add	hl,de
	ld a,(hl)
	or a
	jr z,StartAskUsername
	ld de,MyUsername
	ld bc,8
	ldir
	
	ld bc,5
	ld hl,$86EE
	ld de,MySID
	ldir
	
StartNoAskUsername:
	CloseGUIStack()

	call SetUpAppVars
	xor a
	ld (KnownCalcCount),a
	ld hl,7
	ld (OutWindowE_Len),hl
	ld hl,PingResetVal
	ld (PingSendTimer),hl
	ld hl,InputBackupRAM
	ld (hl),1						;character count
	inc hl
	ld (hl),0						;first character
	xor a
	ld (SendActiveFlag),a
	
	ld hl,(8*256)+28
	ld (pencol),hl
	ld hl,NotifyText
	bcall(_vputs)
	ei
	call iFastCopy
	ld b,2
StartSetupBroadcastOuter:
	push bc
		call SetBroadcastPing
StartSetupBroadcastWait:
		call Cn2_GetK
		cp skClear
		jp z,ExitSetupBroadcast
		ld a,($8800)
		or a
		jr nz,StartSetupBroadcastWait
		ld hl,$6000
StartSetupBroadcastWait2:
		dec hl
		inc hl
		dec hl
		ld a,h
		or l
		jr nz,StartSetupBroadcastWait2
		pop bc
	djnz StartSetupBroadcastOuter
	
	OpenGUIStack()
	; Put the data from the window into saferam
	ld	de, (OutWindowDataLocation)
	ld	hl, OutWindow
	ld	bc, OutWindowAppvarSize		;(OutWindowE_Len)		;(OutWindowE)
	ldir
RenderMain:
	; Set up the main window
	PushGUIStack(GUIRLargeWin,LargeWinData,LargeWinDataE-LargeWinData)
	PushGUIStack(GUIRTextMultiline, (OutWindowDataLocation), OutWindowAppvarSize)	;(OutWindowE))	;changed 1/6/2011 CRM, 4th arg
	ld de,InWindowE-InWindow
	ld a,(InputBackupRAM)
	ld l,a
	ld h,0
	add hl,de
	ld de,InWindow
	ex de,hl
	ld a,GUIRTextLineIn
	call PushGUIStack
	ld a,1
	call GUIFindThis
	ld de,10
	add hl,de
	ld de,InputBackupRAM
	ld a,(de)
	ld c,a
	ld b,0
	inc de
	ex de,hl
	ldir

	ld hl,InputBackupRAM
	ld (hl),1						;character count
	inc hl
	ld (hl),0						;first character
	
	PushGUIStack(GUIRButtonText,SendButton,SendButtonE-SendButton)
	PushGUIStack(GUIRHotspot,NamesWindowHotSpot,NamesWindowHotSpotE-NamesWindowHotSpot)
	PushGUIStack(GUIRHotspot,LargeWinAboutButtonHotSpot,LargeWinAboutButtonHotSpotE-LargeWinAboutButtonHotSpot)
	PushGUIStack(GUIRHotspot,ReceiveHotSpot,ReceiveHotSpotE-ReceiveHotSpot)
	PushGUIStack(GUIRWinButtons, LargeWinButtons, LargeWinButtonsE - LargeWinButtons)
	PushGUIStack(GUIRLargeSprite,LargeWinUsersButton,LargeWinUsersButtonE-LargeWinUsersButton)
	PushGUIStack(GUIRLargeSprite,LargeWinAboutButton,LargeWinAboutButtonE-LargeWinAboutButton)
	PushGUIStack(GUIRHotspot,DiscoHotSpot,DiscoHotSpotE-DiscoHotSpot)
	GUIMouse(MouseHook)

ExitSetupBroadcast:
		pop hl
	jr ExitNoStackNoReset

Exit:
	call ResetAppPage
	CloseGUIStack()
ExitNoStackNoReset:
	call DeleteAppVars
	
	ld b,2
	xor a
	ld ($8800),a
ExitSendDiscoLoop:
	push bc
		ld hl,$87FA				;Send buffer
		ld b,5
ExitSendDiscoLoopBroadcastID:
		ld (hl),0
		inc hl
		djnz ExitSendDiscoLoopBroadcastID
		ld (hl),9
		inc hl
		inc hl
		ld (hl),172
		dec hl
		ld (hl),$80
ExitSendDiscoLoopBroadcastIDWait:
		call Cn2_GetK
		cp skClear
		jp z,ExitQuick
		ld a,($8800)
		or a
		jr nz,ExitSendDiscoLoopBroadcastIDWait
		ld hl,$6000
ExitSendDiscoLoopWait2:
		dec hl
		inc hl
		dec hl
		ld a,h
		or l
		jr nz,ExitSendDiscoLoopWait2
		pop bc
	djnz ExitSendDiscoLoop	
	push bc
ExitQuick:
		pop bc
	call Cn2_Setdown
	ret
	
OpenAboutWindow:
	call ResetAppPage
	PushGUIStacks(AboutWindow)
	GUIMouse(0)
	
CloseAboutWindow:
	call ResetAppPage
	PopGUIStacks(3)
	jp FinishClick
	
OpenNamesWindow:
	call ResetAppPage
	PushGUIStack(GUIRSmallWin, NamesWindow, NamesWindowE-NamesWindow)
	PushGUIStack(GUIRWinButtons, NamesWindowButtons, 7)
	PushGUIStack(GUIRLargeSprite,SmallWinMeButton,SmallWinMeButtonE-SmallWinMeButton)
	PushGUIStack(GUIRHotspot,MeWindowHotSpot,MeWindowHotSpotE-MeWindowHotSpot)

	;Push an empty element for the user list
	ld a,(KnownCalcCount)
	inc a
	ld l,a
	ld h,0
	add hl,hl			;x2
	push hl
		add hl,hl		;x4
		add hl,hl		;x8
		pop de
	add hl,de			;x10
	ld de,7
	add hl,de			;(KnownCalcCount+1)*10+7
	ld de,UserListStub
	ex de,hl
	ld a,GUIRTextMultiline
	call PushGUIStack

	;Now fill it in
	ld a,3
	call GUIFindThis
	ld de,9
	add hl,de
	ld de,UserSIDBuf
	ld a,(KnownCalcCount)
	ex de,hl
NamesListFillLoop:
	or a
	jr z,NamesListFillFinish
	push af
		ld bc,5
		add hl,bc			;past the SID
		ld b,8
NamesListFillLoopInner:
			ld a,(hl)
			or a
			jr z,NamesListFillLoopInner2
			ld (de),a
			inc hl
			inc de
			djnz NamesListFillLoopInner
			jr NamesListFillFinishInner2
NamesListFillLoopInner2:
			inc hl
			djnz NamesListFillLoopInner2
NamesListFillFinishInner2:
			inc hl				;ping thing
			ld a,$D6
			ld (de),a
			inc de
			ld a,$01
			ld (de),a
			inc de
		pop af
	dec a
	jr NamesListFillLoop
NamesListFillFinish:
	ld hl,MyUsername
	ld bc,8
	ldir
	xor a
	ld (de),a

	GUIMouse(0)
	
CloseNamesWindow:
	call ResetAppPage
	PopGUIStacks(5)
	jp FinishClick

ChangeName:
	call ResetAppPage
	call DeleteAppVars
	jp StartAskUsername

;--------------------------------------------------------------------------------
; App var stuff
;--------------------------------------------------------------------------------
SetUpAppVars:
	call SetUpOutWindowAppVar
	ld (OutWindowDataLocation), de
;	call SetUpUsersDataAppVar
;	ld (UsersDataLocation), de
;	call SetUpNamesAppVar
;	ld (NamesWindowLocation), de
	ret
	
SetUpOutWindowAppVar:
	ld	hl, OutWindowAppVarName
	rst 20h
	bcall(_chkfindsym)
	ret	nc   	;return if it exists
	ld	hl,OutWindowAppVarSize ;we'll make it be 512 bytes
	bcall(_CreateAppVar)
	inc de \ inc de
	ret
	
;SetUpUsersDataAppVar:
;	ld	hl, UsersDataAppVarName
;	rst 20h
;	bcall(_chkfindsym)
;	ret	nc   	;return if it exists
;	ld	hl, 512 ;we'll make it be 512 bytes
;	bcall(_CreateAppVar)
;	inc de \ inc de
;	ret
	
;SetUpNamesAppVar:
;	ld	hl, NamesWindowAppVarName
;	rst 20h
;	bcall(_chkfindsym)
;	ret	nc   	;return if it exists
;	ld	hl, 512 ;we'll make it be 512 bytes
;	bcall(_CreateAppVar)
;	inc de \ inc de
;	ret
	
DeleteAppVars:
	ld	hl, OutWindowAppVarName
	rst 20h
	bcall(_chkfindsym)
	bcallnc(_delvar)
;	ld	hl, UsersDataAppVarName
;	rst 20h
;	bcall(_chkfindsym)
;	bcall(_delvar)
;	ld	hl, NamesWindowAppVarName
;	rst 20h
;	bcall(_chkfindsym)
;	bcallnc(_delvar)
	ret

;--------------------------------------------------------------------------------
; Mouse stuff
;--------------------------------------------------------------------------------
MouseHook:
	im 2				;because GUIMouse APD sets IM 1
	ei
	ld a,$ff \ out (1),a
    ld 	a,$fd	;group for ENTER
    out (1), a
#ifdef NO_APD
	xor a
	ld ($98A7),a		;dAPDtimer
#else
    nop \ nop	;let the keyboard settle
#endif
    in 	a,(1)
;    bit 0,a
;    jr	z, ClickSend
	cp 	dkClear
	jr	z, ClickClose
	ld 	a,KeyRow_Top	; group for the function row
	out (1),a
	nop \ nop
	in 	a,(1)
	cp 	dkY
	jr 	z,ClickOpenNames
	; Check if there's data on the wire
	ld	a, ($86F9)
	or a
	jr	nz, ClickReceive
MouseHookReturn:
	ld a,($8800)
	or a
	ret nz
	ld hl,(PingSendTimer)
	dec hl
	ld (PingSendTimer),hl
	ld a,h
	or l
	ret nz
	ld hl,PingResetVal
	ld (PingSendTimer),hl
;	in a,(6)
;	push af
;		inc a
;		out (6),a
;		ld b,254
;		call iRandom
;		ld c,a
;		ld b,15
;		call iRandom
;		or c
;		ld b,a
;		pop af
;	out (6),a
;	ld a,b
;	or a
;	ret nz
	jp SetBroadcastPing
	
ClickClose:
	ld hl, MouseY
	ld (hl), 3
	inc hl
	ld (hl), 92
	pop hl
	ret

ClickSend:
	ld hl, MouseY
	ld (hl), 54
	inc hl
	ld (hl), 76
	pop hl
	ret
	
ClickReceive:
	;Modded by Kerm to try to remove some slowdowns and annoyances
	ld a,($86FA)
	cp 171
	jr z,ClickReceive_Ping
	cp 172
	jr z,ClickReceive_Disco	
DoClickReceiveMsg:
	ld hl, MouseY
	ld (hl), 10
	inc hl
	ld (hl), 105
	pop hl
	ret
ClickReceive_Disco:
	call IsSenderKnown
	jr z,DoClickReceiveMsg
	jr MouseHookResetBuffer
ClickReceive_Ping:
	call IsSenderKnown
	jr nz,DoClickReceiveMsg
	inc de										;if we get here, known; check if name changed
	inc de
	inc de
	ld b,8
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	push de
		push hl
ClickReceive_Ping_ChkNameLoop:
			ld a,(de)
			cp (hl)
			jr nz,DoClickReceiveMsg_Pop
			or a
			jr z,ClickReceive_Ping_ChkNameLoopFinish
			inc hl
			inc de
			djnz ClickReceive_Ping_ChkNameLoop
ClickReceive_Ping_ChkNameLoopFinish:
			pop hl
		ld de,8
		add hl,de
		ld (hl),PingTimeoutVal
		pop de
MouseHookResetBuffer:
	xor a
	ld ($86F9),a
	ret
DoClickReceiveMsg_Pop:
			pop hl
		pop de
	jr DoClickReceiveMsg

ClickOpenNames:
	ld hl, MouseY
	ld (hl), 1
	inc hl
	ld (hl), 70
	pop hl
	ret
	
SetBroadcastPing:
	ld hl,$87FA				;Send buffer
	ld b,5
StartSetupBroadcastID:
	ld (hl),0
	inc hl
	djnz StartSetupBroadcastID
	ld (hl),9
	inc hl
	push hl
		inc hl
		ld (hl),171
		inc hl
		ld de,MyUsername
		ld bc,8
		ex de,hl
		ldir
		pop hl
	ld (hl),$80

	ld a,(KnownCalcCount)
	ld hl,UserSIDBuf+13
	ld c,0
PingOutCheckLoop:
	or a
	jr z,PingOutFinish
	push af
		ld a,(hl)
		or a
		jr z,PingOutCheckLoopOut
		dec a
		ld (hl),a
		or a
		jr nz,PingOutCheckLoopNotOut
PingOutCheckLoopOut:
		ld c,1
PingOutCheckLoopNotOut:
		ld de,14
		add hl,de
		pop af
	dec a
	jr PingOutCheckLoop
PingOutFinish:
	ld a,c
	or a
	ret z
PingOutDo:
	ld hl, MouseY
	ld (hl), 20
	inc hl
	ld (hl), 105
	pop hl
	ret

FinishClick:
	PopGUIStacks(11)
	jp RenderMain

;--------------------------------------------------------------------------------
; Actual program stuff
;--------------------------------------------------------------------------------
CalcPingedOut:
	call ResetAppPage

	ld a,(KnownCalcCount)
	ld hl,UserSIDBuf+13
	ld c,0
CalcPingOutCheckLoop:
	or a
	jr z,CalcPingOutFinish
	push af
		ld a,(hl)
		or a
		jr nz,CalcPingOutCheckLoopNotOut
CalcPingOutCheckLoopOut:
		ld de,-13
		add hl,de
		ex de,hl
		ld hl,$86F9
		ld (hl),$80
		ld hl,$86F3
		ex de,hl
		ld bc,5
		ldir
		inc de
		inc de
		inc de
		ld bc,8
		ldir
		pop af
	jp Receive_Disco
		
CalcPingOutCheckLoopNotOut:
		ld de,14
		add hl,de
		pop af
	dec a
	jr CalcPingOutCheckLoop
CalcPingOutFinish:
	jp FinishReceiveClearbufNoReset

Receive:
	call ResetAppPage
	ld a,1
	call GUIFindThis
	push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,-10
		add hl,de
		ld b,h
		ld c,l
		pop hl
	ld de,10
	add hl,de
	ld de,InputBackupRAM
	ld a,c
	ld (de),a
	inc de
	ldir

ReceiveReentrant:
	; de needs to be the tail end of the OutWindowData section
	ld	hl, $86FA
	ld a,(hl)
	cp 171
	jp z,Receive_Ping
	cp 172
	jp z,Receive_Disco
	push hl
		call IsSenderKnown
		pop hl
	jr nz,FinishReceiveClearbuf
;	cp 173
;	jp nz,FAILFAILFAILFAILFAIL
	ld	bc, ($86F8)
	ld a,$7f
	and b
	ld b,a
	inc hl
	dec bc
SetLineToBufferHLBC:
	;Prepending:
	;(1) Move OutWindowAppvarSize-bc bytes from OutWindowDataLocation to OutWindowDataLocation+bc
	;(2) write a null to OutWindowDataLocation+OutWindowAppvarSize-1
	;(3) copy from hl to OutWindowAppvarSize
	push hl
		push bc
			inc bc \ inc bc		;for the newline
			ld hl,OutWindowAppvarSize
			push hl
				ld de,(OutWindowDataLocation)
				add hl,de
				dec hl
				pop de
			ex de,hl		;hl is OutWindowAppvarSize, de is where to copy to, bc is size of new
			or a
			sbc hl,bc		;hl contains size to copy
			push bc
				ld bc,6
				or a
				sbc hl,bc		;remove the seven bytes of the header
				pop bc
			ex de,hl		;de contains size to copy, hl is where to copy to, bc is size of new
			push hl
				push de
					or a
					sbc hl,bc	;hl contains where to copy from
					pop bc	;retrieve copy length from de
				pop de		;retrieve copy destination from hl
			ld (hl),0		;null as last char of appvar, just in case, for when it overflows
			lddr			;push old stuff forward
			pop bc
		ld de,(OutWindowDataLocation)
		ld hl,6	;header size
		add hl,de
		pop de
	ex de,hl
	ldir
	ld hl,endl
	ld bc,2
	ldir
			
#ifdef false
	push hl
		ld	hl, (OutWindowDataLocation)
		ld	de, (OutWindowE_Len)
		add	hl, de
		dec	hl
		pop de
	ex	de, hl
	; hl has the start of the receive buffer
	; de is the very end of the OutWindowData section
	; bc needs to be set to size of the recieve buffer, which means mask out the top byte

	; Now update the length of the string
	push hl
		ld	hl, (OutWindowE_Len)
		add	hl, bc
		ld	(OutWindowE_Len), hl
		pop	hl
	; Copy it all over!
	ldir
	; Now append the newline character (this can probably be combined with the above somehow, but it works, so that's cool.)
	; Update the length of the string
	ld	hl, (OutWindowE_Len)
	inc	hl \ inc hl
	ld	(OutWindowE_Len), hl
	; set hl (note: de is already in the right place)
	ld	hl, endl
	ld	bc, 3
	ldir
#endif
	; Clear the receieve bit
FinishReceiveClearbuf:
	xor a
	ld ($86f9),a
	ld hl,SendActiveFlag
	ld a,(hl)
	ld (hl),0
	or a
	jp nz,SendMsgLoopAllCalcsWait_NoRecv
	jp FinishClick

FinishReceiveClearbufNoReset:
	xor a
	ld ($86f9),a
	
FinishReceiveCheckSendReentrant:
	ld hl,SendActiveFlag
	ld a,(hl)
	ld (hl),0
	or a
	jp nz,SendMsgLoopAllCalcsWait_NoRecv
	GUIMouse(MouseHook)

Receive_Ping:
	call IsSenderKnown
	jr nz,AddSenderToList
	inc de										;if we get here, known; check if name changed
	inc de
	inc de
	ld b,8
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	push de
		push hl
Receive_Ping_ChkNameLoop:
			ld a,(de)
			cp (hl)
			jr nz,Receive_Ping_NewName
			or a
			jr z,Receive_Ping_ChkNameLoopFinish
			inc hl
			inc de
			djnz Receive_Ping_ChkNameLoop
Receive_Ping_ChkNameLoopFinish:
			pop hl
		ld de,8
		add hl,de
		ld (hl),PingTimeoutVal
		pop de
	jr FinishReceiveClearbufNoReset				;name is the same, continue looping
Receive_Ping_NewName:							;Store a new name
			pop de								;copy to the user/SID storage...
		pop	ix
	push ix
		push de
			ld hl,JoinPartMsgStage
			ld (hl),'*'
			inc hl
			ld (hl),'*'
			inc hl
			ld b,8
			ld c,10
NewNameJoinedMsgLoop:
			ld a,(de)
			or a
			jr z,NewNameJoinedMsgFinish
			ld (hl),a
			inc hl
			inc de
			inc c
			djnz NewNameJoinedMsgLoop
NewNameJoinedMsgFinish:
			push bc
				ld de,IsNowInfix
				ld bc,8
				ex de,hl
				ldir
				ex de,hl
				pop bc
			ld b,8
NewNameJoinedMsgLoop2:
			ld a,(ix)
			or a
			jr z,NewNameJoinedMsgFinish2
			ld (hl),a
			inc hl
			inc ix
			inc c
			djnz NewNameJoinedMsgLoop2
NewNameJoinedMsgFinish2:
			pop de
		pop hl									;...from the receive buffer
	push bc
		ld bc,8
		ldir
		ld a,PingTimeoutVal
		ld (de),a
		pop bc
	ld b,0
	ld hl,JoinPartMsgStage
	jp SetLineToBufferHLBC

AddSenderToList:
	ld a,(KnownCalcCount)
	ld l,a
	ld h,0
	call MultHLBy14
	ld de,UserSIDBuf
	add hl,de
	ld de,$86F3						;receive buffer sender ID, 5 bytes
	ex de,hl
	ld bc,5
	ldir							;copy SID in
	inc hl
	inc hl
	inc hl							;past size byte and "171" to contents (name)
	ld bc,8
	ldir
	ld a,PingTimeoutVal
	ld (de),a
	ld a,255
	ld (de),a						;lag byte
	ld hl,KnownCalcCount
	inc (hl)						;one more calc known now
	ld hl,JoinPartMsgStage
	push hl
		ld (hl),'*'
		inc hl
		ld (hl),'*'
		inc hl
		ld b,8
		ld c,9
		ld de,$86FB
AddSenderJoinedMsgLoop:
		ld a,(de)
		or a
		jr z,AddSenderJoinedMsgFinish
		ld (hl),a
		inc hl
		inc de
		inc c
		djnz AddSenderJoinedMsgLoop
AddSenderJoinedMsgFinish:
		push bc
			ld de,JoinedSuffix
			ex de,hl
			ld bc,8
			ldir
			pop bc
		ld b,0
		pop hl
	jp SetLineToBufferHLBC

Receive_Disco:
	call IsSenderKnown
	jp nz,FinishReceiveClearbufNoReset
	call RemoveSenderFromList
Receive_Disco_Msg:
	ld hl,JoinPartMsgStage
	push hl
		ld (hl),'*'
		inc hl
		ld (hl),'*'
		inc hl
		ld b,8
		ld c,9
		ld de,$86FB
RemSenderJoinedMsgLoop:
		ld a,(de)
		or a
		jr z,RemSenderJoinedMsgFinish
		ld (hl),a
		inc hl
		inc de
		inc c
		djnz RemSenderJoinedMsgLoop
RemSenderJoinedMsgFinish:
		push bc
			ld de,PartedSuffix
			ex de,hl
			ld bc,8
			ldir
			pop bc
		ld b,0
		pop hl
	jp SetLineToBufferHLBC
	
MultHLBy14:
	push de
		add hl,hl			;x2
		push hl
			add hl,hl			;x4
			push hl
				add hl,hl		;x8
				pop de
			add hl,de			;x12
			pop de
		add hl,de				;x14
		pop de
	ret

Send:
	call ResetAppPage
	; set broadcast address
;	ld	bc, 0
;	ld	($87FA), bc
	; get hl to be the start of the data segment structured as (low byte of size), (high byte of size), (type byte), contents
	GUIFindThis(1)
	; make de the send buffer
	ld	de, $8801
	; bc refers to the whole data size, so make it be just the string size
	dec bc \ dec bc \ dec bc \ dec bc \ dec bc \ dec bc \ dec bc \ dec bc \ dec bc \ dec bc \ dec bc
	; If bc is zero, we can just skip this whole process
	ld	a, b
	or	c
	jp	z, FinishClick
	ld a,173					;frame type
	ld (de),a
	inc de
	push bc
		push hl
			inc bc				;frame type byte
			ld hl,MyUsername
SendPrependUnameLoop:
			ld a,(hl)
			or a
			jr z,SendPrependUnameLoopFinish
			inc bc
			ld (de),a
			inc hl
			inc de
			jr SendPrependUnameLoop
SendPrependUnameLoopFinish:
			ld a,':'
			ld (de),a
			inc de
			inc bc
			pop hl
	; Now update the length of buffer
		;ld	($87FF), bc
		ld (MsgLenBuf),bc
		push bc
			pop ix
	; Get to the data
		ld 	bc, 10
		add	hl, bc
		pop bc
; Now that we're at the actual data part, and de holds where we want to copy to, let's copy
	ldir
	push ix
	;DO THE SENDING
		ld hl,UserSIDBuf
		ld a,(KnownCalcCount)
		ld b,a
		or a
		jr z,SendMsgLoopAllCalcsFinish
SendMsgLoopAllCalcs:
		push bc
			push hl
				ld de,$87FA
				ld bc,5
				ldir
				; Set the send buffer ready bit
				ld hl,(MsgLenBuf)
				ld h,$80
				ld ($87FF),hl
				;ld a,$80
				;ld ($8800),a
SendMsgLoopAllCalcsWait:
				call Cn2_GetK
				cp skClear
				jp z,ExitSendAll
				ld a,($86F9)
				or a
				jr z,SendMsgLoopAllCalcsWait_NoRecv
SendMsgLoopAllCalcsWait_Recv:
				ld a,1
				ld (SendActiveFlag),a
				jp ReceiveReentrant
				
SendMsgLoopAllCalcsWait_NoRecv:
				ld a,($8800)
				or a
				jr nz,SendMsgLoopAllCalcsWait
				pop hl
			ld de,14
			add hl,de
			pop bc
		djnz SendMsgLoopAllCalcs
SendMsgLoopAllCalcsFinish:
		pop bc
	dec bc				;frame type byte
	ld hl,$8802			;skip frame type byte
	jp SetLineToBufferHLBC

ExitSendAll:
				pop hl
			pop hl
		pop hl
	jp Exit+3
	
IsSenderKnown:
	ld hl,UserSIDBuf
	ld a,(KnownCalcCount)
IsSenderKnownLoop:
	or a
	jr z,IsSenderKnown_No
	push af
		push hl
			ld de,$86F3 				;receive buffer ID, first byte of 5
			ld b,5
IsSenderKnownLoop_Inner:
			ld a,(de)
			cp (hl)
			jr nz,IsSenderKnownLoop_InnerFinish
			inc hl
			inc de
			djnz IsSenderKnownLoop_Inner
			pop hl
		pop bc
	;ZERO FLAG is set because of the djnz!
	ret
IsSenderKnownLoop_InnerFinish:
			pop hl
		ld de,NameEntryLength		;14
		add hl,de
		pop af
	dec a
	jr IsSenderKnownLoop
IsSenderKnown_No:
	cp 1		;a is zero, resets z flag
	ret

RemoveSenderFromList:
	ld hl,KnownCalcCount
	ld a,(hl)
	dec (hl)
	ld hl,UserSIDBuf
RemoveSenderFromListLoop:
	or a
	ret z
	push af
		push hl
			ld de,$86F3 				;receive buffer ID, first byte of 5
			ld b,5
RemoveSenderFromListLoop_Inner:
			ld a,(de)
			cp (hl)
			jr nz,RemoveSenderFromListLoop_InnerFinish
			inc hl
			inc de
			djnz RemoveSenderFromListLoop_Inner
			pop de
		pop af			;a==number remain
	dec a
	ret z
	ld l,a
	ld h,0
	call MultHLBy14
	push hl
		pop bc
	ld hl,NameEntryLength
	add hl,de
	ldir
	ret
RemoveSenderFromListLoop_InnerFinish:
			pop hl
		ld de,NameEntryLength		;14
		add hl,de
		pop af
	dec a
	jr RemoveSenderFromListLoop

;--------------------------------------------------------------------------------
; Data
;--------------------------------------------------------------------------------
NotifyText:
	.db " Broadcasting join...",0
JoinedSuffix:
	.db " joined",0
PartedSuffix:
	.db " parted",0
IsNowInfix:
	.db " is now ",0
	
;--------------------------------------------------------------------------------
; AppVar stuff
;--------------------------------------------------------------------------------
OutWindowAppVarName:
	.db	AppVarObj, "OUTWIN", 0

;OutWindowDataLocation:
;	.dw 0
	
;UsersDataAppVarName:
;	.db	AppVarObj, "USERS", 0
	
;UsersDataLocation:
;	.dw 0
	
;NamesWindowAppVarName:
;	.db AppVarObj, "NAMES", 0
	
;NamesWindowLocation:
;	.dw 0

;--------------------------------------------------------------------------------
; Username input window stuff
;--------------------------------------------------------------------------------
UsernameWinData:
	.db 5,5
	.db %01100000
	.db %10010000
	.db %11110000
	.db %10010000
	.db %11110000
	.db "Enter Username:",0
UsernameWinDataE:

UsernameWinButtons:
	.db %00100000
	.dw 0, 0
	.dw StartAskUsernameProcess
UsernameWinButtonsE:

UsernameIn:
	.db 3,3
	.db 60
	.dw 8
	.dw 0
	.db 0,0,0,0,0,0,0,0,0
UsernameInE:

UsernameSetButton:
	.db 24,30
	.dw StartAskUsernameProcess
	.db "Set Username",0
UsernameSetButtonE:
;--------------------------------------------------------------------------------
; Large window stuff
;--------------------------------------------------------------------------------
LargeWinData:
	.db %01100000
	.db %10010000
	.db %11110000
	.db %10010000
	.db %11110000
	.db "Chat!",0
LargeWinDataE:

LargeWinButtons:
	.db %00100000
	.dw 0,0
	.dw Exit
LargeWinButtonsE:

OutWindow:
	.db	1, 1
	.db 7
	.db 91
	.db 0, 0
	.db 0
OutWindowE:
	.dw 7

InWindow:
	.db 1, 45
	.db 71
	.dw 1024
	.db 0,0
;	.db 0
InWindowE:

SendButton:
	.db 75, 45
	.dw Send
	.db " Snd",0
SendButtonE:

NamesWindowHotSpot:
	.db 69, -8
	.db 18, 7
	.dw OpenNamesWindow
NamesWindowHotSpotE:

ReceiveHotSpot:
	.db 103, 0
	.db 5, 5
	.dw Receive
ReceiveHotSpotE:

LargeWinUsersButton:
	.db 69,-8
	.db 3,7
	.db %01111111,%11111111,%11000000
	.db %00000000,%00000000,%00000000
	.db %00101001,%10110001,%10000000
	.db %00101001,%00110001,%00000000
	.db %00111011,%00101011,%00000000
	.db %00000000,%00000000,%00000000
	.db %01111111,%11111111,%11000000
LargeWinUsersButtonE:

LargeWinAboutButton:
	.db 47,-8
	.db 3,7
	.db $7F,$FF,$FC
	.db $80,$00,$02
	.db $BB,$3A,$BA
	.db $BB,$AA,$92
	.db $AB,$BB,$92
	.db $80,$00,$02
	.db $7F,$FF,$FC
LargeWinAboutButtonE:

LargeWinAboutButtonHotSpot:
	.db 47,-8
	.db 23, 7
	.dw OpenAboutWindow
LargeWinAboutButtonHotSpotE:
	
DiscoHotSpot:
	.db 103, 10
	.db 5, 5
	.dw CalcPingedOut
DiscoHotSpotE:

;--------------------------------------------------------------------------------
; Names window data
;--------------------------------------------------------------------------------
NamesWindow:
	.db 7,12
	.db %01100000
	.db %11110000
	.db %10010000
	.db %11110000
	.db %01100000	
	.db "Names",0
NamesWindowE:
NamesWindowButtons:
	.db %00100000
	.dw 0, 0
	.dw CloseNamesWindow

MyUsername:
	.db 0,0,0,0,0,0,0,0
	.db 0

MySID:
	.db $DE,$AD,$BE,$EF,$42

SmallWinMeButton:
	.db 56,-8
	.db 2,7
	.db %01111111,%11111111
	.db %10000000,%00000000
	.db %10111011,%10000000
	.db %10111011,%00000000
	.db %10101011,%10101010
	.db %10000000,%00000000
	.db %01111111,%11111111
SmallWinMeButtonE:
	
MeWindowHotSpot:
	.db 56, -8
	.db 16, 7
	.dw ChangeName
MeWindowHotSpotE:

UserListStub:
	.db	1, 1
	.db 6
	.db 76
	.db 0, 0
	.db 0
UserListStubE:

;--------------------------------------------------------------------------------
; About Window
;--------------------------------------------------------------------------------
AboutWindow:
	; The small window
	.dw AboutTextData - AboutWindow
	.db 2
	.db 7,12
	.db %01100000
	.db %10010000
	.db %00100000
	.db %00000000
	.db %00100000	
	.db "About Chat",0
AboutTextData:
	; The about text
	.dw AboutWindowButtons - AboutTextData
	.db 6
	.db 1, 1
	.db 77
	.db 0
	.db "CALCnet Chat! v1.0",$D6
	.db "By Merthsoft and",$D6,"Kerm Martian",$D6
	.db "January 2011",$D6
	.db "http://cemetech.net",0
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

;--------------------------------------------------------------------------------
; User name prompt data
;--------------------------------------------------------------------------------
UserNameWindow:
	.db 7,12
	.db %01100000
	.db %11110000
	.db %10010000
	.db %11110000
	.db %01100000	
	.db "Name",0
UserNameWindowE:

endl:
	.db $D6,$01,0