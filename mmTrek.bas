'Option Explicit
Randomize Timer

'Color Computer Magazine : May 1983,pg40-46
'https://archive.org/details/color-computer-magazine-1983-05/page/n37/mode/2up

'Direct Commands
'1 Set Course
'2 Local Scanner
'3 Remote Scanner
'4 Fire Phasers
'5 Photon Torpedo
'6 Shield Control
'7 Damage Report
'8 Onboard Computer

'Computer Commands
'1 Computer Guidance
'2 Status Report
'3 Torpedo Data
'4 Computer Course
'5 Guided Torpedo
'6 Mission Record

'Course Wheel
'      1
'    8   2
'  7   .   3
'    6   4
'      5
Const dirN=1
Const dirNE=2
Const dirE=3
Const dirSE=4
Const dirS=5
Const dirSW=6
Const dirW=7
Const dirNW=8
Const dirN2=9

Const IntroMusic=0
Const KlingShields=200

Const FCEr$="?FC ERROR"
Const CoCoFont=4 '32x20
Const PicoLF=10 '[Enter]
Const PicoDown=129 'Down arrow

Const blk=RGB(black)
Const grn=RGB(green)

Const chrTorpedo=159
Const chrSpace=128
Const GreenHalf=131
Const GreenCorner=135
Const GreenOther=139
Const WhiteHalf=195
Const WhiteCorner=199
Const WhiteOther=203
Const YellowHalf=147
Const YellowCorner=151
Const YellowOther=155

Const snaSpace=0
Const snaKling=1
Const snaCloaked=2
Const snaShip=3
Const snaStar=4
Const snaBase=5

Const stDocked=0
Const stGreen=1
Const stYellow=2
Const stRed=3
Const stINIT=4

Const syCMD=1
Const syOPT=2

Const sysGuidance=1
Const sysLocalScan=2
Const sysRemoteScan=3
Const sysPhasers=4
Const sysTorpedoes=5
Const sysDamageRpt=6
Const sysComputer=7

'Docked$=C$(0)
Const Docked$="DOCKED "
Const VS$="ENTERPRISE"
Const US$="FEDERATION"
Const XS$=String$(7," ")
Const YS$=" .............. "
Const ZS$=String$(16," ")

' SNA() = Sector, GNA() = Galaxy, ZNA() = ?
' CNA() = CourseDeltaTrig, DNA() = System Damage
' KNA() = Klingon statuses in sector
Dim SNA(8,8), GNA(8,8), ZNA(8,8)
Dim CNA(9,2), KNA(3,3), DNA(8)
' DSA() = System strings
' BSA() = Command strings
' CSA() = Alert Condition bars
' Replaced CSA$() with Docked$ const
Dim DSA$(8), BSA$(30)', CSA$(3)

' ST = Condition State
' E = Energy, G = Shields, P = Torpedos
' T0 = Init Stardate, T = Stardate, TT = End date
' L,M = Ship galaxy coords
' U,V = Ship sector coords
' K = Kling count, KT = total Kling, K7 = Init
Dim ST,E,G,P,T0,T,TT,L,M,U,V,K,KT,K7
' B = Base count, BT = Total base count
' S = Sector star count
' CL = Klingons are cloaking
' SC = Viewbar version
Dim B,BT,S,CL,SC

' *************************************
' FrameBuffer Routines
' *************************************

Sub MakeCharBox(x,y,c)
  'Draw CoCo color characters
  Box x+15,y+9,10,9,1,c,c
  Box x+30,y+9,10,9,1,c,c
  Box x+45,y+0,5,9,1,c,c
  Box x+55,y+0,5,18,1,c,c
  Box x+60,y+9,5,9,1,c,c
  Box x+65,y+0,5,9,1,c,c
  Box x+70,y+0,10,18,1,c,c
  Box x+70,y+0,5,9,1,blk,blk
  Box x+80,y+0,5,9,1,c,c
  Box x+90,y+0,5,9,1,c,c
  Box x+95,y+9,5,9,1,c,c
  Box x+100,y+0,5,18,1,c,c
  Box x+110,y+0,50,18,1,c,c
  Box x+115,y+0,5,9,1,blk,blk
  Box x+120,y+9,15,9,1,blk,blk
  Box x+145,y+9,5,9,1,blk,blk
End Sub

Sub MakeChars
  'Draw CoCo color characters
  MakeCharBox 0,0,RGB(green)
  MakeCharBox 0,18,RGB(yellow)
  MakeCharBox 0,36,RGB(blue)
  MakeCharBox 0,54,RGB(red)
  MakeCharBox 0,72,RGB(white)
  MakeCharBox 0,90,RGB(cyan)
  MakeCharBox 0,108,RGB(pink)
  MakeCharBox 0,126,RGB(orange)
End Sub

Sub BlitChar(n,c,f)
  '=Print@n,CHR$(c) c=128-256
  Local x=Int((n Mod 32)*10)
  Local y=(n\32)*18
  Local cx=Int((c-128) Mod 16)*10)
  Local cy=((c-128)\16)*18

  If f=0 Then
    Blit FrameBuffer F,N,cx,cy,x,y,10,18
  Else
    Blit cx,cy,x,y,10,18
  End If
  n=n+1
End Sub

Sub BlitSpace(n)
  '110 A$(0),SNA()=0
  BlitChar(n,128)
  BlitChar(n,128)
End Sub

Sub BlitKlingon(n,c)
  '110 A$(1-2),SNA()=1,2
  If c=0 Then
    BlitChar(n,237)
    BlitChar(n,232)
  Else
    'Cloaked
    BlitSpace n
  End If
End Sub

Sub BlitShip(n)
  '110 A$(3),SNA()=3
  BlitChar(n,220)
  BlitChar(n,214)
End Sub

Sub BlitStar(n)
  '110 A$(4),SNA()=4
  BlitChar(n,192)
  BlitChar(n,146)
End Sub

Sub BlitBase(n)
  '110 A$(5),SNA()=5
  BlitChar(n,166)
  BlitChar(n,169)
End Sub

Sub MakeViewBar(n,h1,h2,h3,c1,c2,c3,c4)
  '40-60 L$()
  Local x Integer
  For x=1 To 2
    BlitChar n,h1,1
    BlitChar n,h2,1
    BlitChar n,h3,1
  Next

  BlitChar n,h1,1
  BlitChar n,c2,1
  BlitChar n,c3,1

  BlitChar n,h1,1
  BlitChar n,c2,1
  BlitChar n,h3,1

  BlitChar n,c1,1
  BlitChar n,h2,1
  BlitChar n,c3,1

  BlitChar n,h1,1
  BlitChar n,c2,1
  BlitChar n,h3,1

  BlitChar n,c1,1
  BlitChar n,h2,1
  BlitChar n,c3,1

  BlitChar n,h1,1
  BlitChar n,c2,1
  BlitChar n,h3,1

  BlitChar n,c4,1
  BlitChar n,h2,1
  BlitChar n,h3,1

  BlitChar n,h1,1
  BlitChar n,h2,1
  BlitChar n,h3,1

  BlitChar n,h1,1
  BlitChar n,h2,1
End Sub

Sub BlitViewBar(n)
  '40-60 L$()
  Const w=320
  Const h=18
  Const cx=0
  Const x=0
  Const y=288\32*18
  Local cy=288\32*18+(n-1)*18
  Blit FrameBuffer F,N,cx,cy,x,y,w,h
End Sub

Sub MakeViewBars
  '40-60 L$()
  Local h1,h2,h3,c1,c2,c3,c4 Integer
  h1=GreenHalf
  h2=YellowHalf
  h3=WhiteHalf
  c1=GreenCorner
  c2=YellowCorner
  c3=WhiteCorner
  c4=GreenOther
  MakeViewBar 288,h1,h2,h3,c1,c2,c3,c4

  h1=YellowHalf
  h2=WhiteHalf
  h3=GreenHalf
  c1=YellowCorner
  c2=WhiteCorner
  c3=GreenCorner
  c4=YellowOther
  MakeViewBar 320,h1,h2,h3,c1,c2,c3,c4

  h1=WhiteHalf
  h2=GreenHalf
  h3=YellowHalf
  c1=WhiteCorner
  c2=GreenCorner
  c3=YellowCorner
  c4=WhiteOther
  MakeViewBar 352,h1,h2,h3,c1,c2,c3,c4
End Sub

Sub MakeAlert(n,c,f)
  '100 C$()
  BlitChar n,c-8,f
  For x=1 To 5
    BlitChar n,c,f
  Next
  BlitChar n,c-4,f
End Sub

Sub MakeFrameAlerts
  'DIDN'T USE BUT...
  'Condition: Green
  MakeAlert 57,140,1
  'Condition: Yellow
  MakeAlert 89,156,1
  'Condition: Red
  MakeAlert 121,188,1
End Sub

Sub MakeFrameBuffer
  FRAMEBUFFER Create
  FRAMEBUFFER Write F

  Font CoCoFont
  CLS blk

  MakeChars
  MakeViewBars
  'MakeAlerts

  FRAMEBUFFER Write N
End Sub

' *************************************
' CoCo BASIC Routines
' *************************************

Function Rand(n) As integer
  Rand=Int(Rnd*n)+1
End Function

Sub PrintAt(n,ts$,f)
  If n<0 Then Error FCEr$
  Local x=Int((n Mod 32)*10)
  Local y=Int(n\32*18)
  Print @(x,y)ts$;
  If f<>1 Then n=n+Len(ts$)
End Sub

Sub Sound(p,d)
  If p<1 Or d<1 Then Error FCEr$
  If p>255 Or d>255 Then Error FCEr$
  Const ax=8.445e+8
  Const bx=253.913
  Const cx=809.0832
  Const dx=10.26776
  Local f=ax+(bx-ax)/(1+(p/cx)^dx)
  'Play TONE f,f,d*60
  Play Sound 1,B,Q,f,25
  Pause d*60
  Play Stop
End Sub

Sub PlayNote(o,n,d)
  If o<1 Or o>5 Then Error FCEr$
  If n<1 Or n>12 Then Error FCEr$
  If d<0 Then Error FCEr$
  Local x=(o-1)*12+n
  Local f=60.925*Exp(0.0579*x)
  'Play TONE f,f,d
  Play Sound 1,B,Q,f
  Pause d
  Play Stop
End Sub

' *************************************
' UI Routines
' *************************************

Sub ViewerBackground(c)
  Box 80,18,160,144,1,c,c
End Sub

Sub DrawMainViewer
  '20-30 A$,B$
  Local x, n=0
  For x=1 To 7
    BlitChar n,204
  Next
  BlitChar n,205
  For x=1 To 8
    BlitChar n,205
    BlitChar n,204
  Next
  BlitChar n,206
  For x=1 To 7
    BlitChar n,204
  Next
  For x=0 To 7
    BlitChar 39+x*32,206
    BlitChar 56+x*32,205
  Next
  ViewerBackground blk
End Sub

Sub BuildDisplay
  '1370
  DrawMainViewer
  BlitViewBar 1
  PrintAt 33,"DATE"
  PrintAt 97,"QUAD"
  PrintAt 90,"FUEL"
  PrintAt 161,"SECT"
  PrintAt 154,"SHLD"
  PrintAt 225,"TORP"
  PointToMessageArea
End Sub

Sub PlayIntroNote
  '1700
  Local o1,n1,x
  o1=Rand(5):n1=Rand(12)
  For x=33 To 100 Step 33
    Play volume x,x
    PlayNote(o1,n1,40)
    Pause 1
  Next x
End Sub

Sub ShowField(n,v,l)
  PrintAt n,Str$(v,l,0)
  'Local s$=Format$(v,"%-4g")
  'PrintAt n,s$
  PointToMessageArea
End Sub

Sub ShowFuel
  '1400
  ShowField 122,E,4
  PointToMessageArea
End Sub

Sub ShowShields
  '1410
  ShowField 186,G,4
  PointToMessageArea
End Sub

Sub ShowCoord(n,x,y)
  Local q$=Str$(x,2,0)+" ,"
  q$=q$+Str$(y,2,0)
  PrintAt n,q$
End Sub

Sub UpdateCoords
  '300
  ShowCoord 128,L,M
  ShowCoord 192,U,V
  PointToMessageArea
End Sub

Sub SoundAllClear
  SOUND 240,9:SOUND 236,7
End Sub

Sub PointToMessageArea
  '1500
  PrintAt 320,""
End Sub

Sub ClearMessageArea
  '1640
  Box 0,180,320,102,1,grn,grn
End Sub

Sub ShowMessage(m$, n)
  If n=1 Then ClearMessageArea
  PrintAt 320, m$
End Sub

Sub ShowAuthor
  '90
  PrintAt 522,"CC SPACE TREK"
  PrintAt 552,"BY JAKE COMMANDER"
End Sub

Sub ShowGoalMsg
  '200
  Local msg$
  msg$="YOU HAVE 32 STARDATES TO DESTROY"
  msg$=msg$+" FLEET OF "+Str$(KT)
  msg$=msg$+" KLINGONS: THERE "
  msg$=msg$+"ARE "+Str$(BT)+" STARBASES IN "
  msg$=msg$+"THIS GALAXY"

  ShowMessage msg$, 1
End Sub

Sub ClearCMD(sys)
  If sys=syCMD Then PrintAt 217,"CMD?  "
  If sys=syOPT Then PrintAt 217,"OPT?  "
  PrintAt 249,XS$
  PrintAt 281,XS$
End Sub

Sub CombatAreaMsg
  '250
  Local msg$
  msg$="COMBAT AREA .. CONDITION RED -  "
  msg$=msg$+"SHIELDS ARE DANGEROUSLY LOW."
  Print msg$
End Sub

Sub CombatAreaMsg2
  '240
  Local msg$
  msg$="YOU HAVE ENTERED A COMBAT AREA  WITH"
  msg$=msg$+" YOUR SHIELDS LOW. THIS TIME"
  msg$=msg$+"THEY DIDN'T SHOOT. BE WARNED!"
  ShowMessage msg$,1
End Sub

Sub NoKlingonsMsg
  '1620
  Print "SENSORS DETECT NO KLINGONS";
  Print " IN   THIS QUADRANT"
End Sub

Sub ShowCloakMsg
  '300
  msg$="SENSORS DETECT KLINGONS "
  msg$=msg$+"CLOAKING - PHASERS "
  msg$=msg$+"NECESSARY"
  ShowMessage(msg$, 1)
End Sub

Sub TorpedoOfNoUse
  '1630
  Print DSA$(5);" WILL BE OF NO USE -USE ";BSA$(8)
End Sub

Sub OutputCourse(CC,CD)
  '1260
  PrintAt 352,"COURSE = "
  Print Format$(CC,"%1.3f");

  Print ", DISTANCE = ";
  Print Format$(CD,"%1.3f")
End Sub

Function GetAlertColor(n)
  Local c=0
  If n=stGreen Then c=140
  If n=stYellow Then c=156
  If n=stRed Then c=188
  GetAlertColor=c
End Function

Sub ShowAlert(n)
  '1470
  Local c
  If n=stDocked Then
    PrintAt 57,Docked$
  Else
    c=GetAlertColor(n)
    MakeAlert 57,c,0
  End If
End Sub

Sub BlitLocalSanner(n, v)
  Select Case v
    Case 0
      BlitSpace n
    Case 1
      BlitKlingon n,0
    Case 2
      BlitKlingon n,1
    Case 3
      BlitShip n
    Case 4
      BlitStar n
    Case 5
      BlitBase n
  End Select
End Sub

Sub BeepGreen
  '1660
  'Play "V=SO;L32T2O5G"
  Play Volume SD
  PlayNote 5,7,37
  Pause 75
End Sub

Sub BeepRed
  '1680
  'Play "L32T2V8O1A"
  Play Volume 8
  PlayNote 1,9,37
  Pause 75
End Sub

Sub CycleViewBar
  '1650-1680
  ' Cycle between 3 versions
  SC=SC+1
  If SC>3 Then SC=1
  BlitViewBar SC

  If ST<=stGreen Then

    If SO=0 Then
      If Rand(20)>19 Then
        SO=16
      Else
        Return
      End If
    End If

    SO=SO-2
    If SO<2 Then
      SO=0
      Return
    Else
      BeepGreen
      Return
    End If

  End If

  BeepRed

End Sub

Sub FlashPhaserDamage
  '1360
  ' Flash for phaser hit
  ' Can I make another framebuffer
  ' that's just a yellow blank screen
  ' then flash between normal and yellow?
  Local ZZ,ylw=RGB(Yellow)
  'Reduced to.125 from .25 since BOX command slows
  For ZZ=H*.125+1 To 1 Step -1
    'SCREEN 0,1 'CAN'T EMULATE
    ViewerBackground ylw
    'PLAY"T255O1;12" ' Play too fast to be heard.
     'Just do the SOUND
     SOUND 255,1
    'SCREEN 0,0 'CAN'T EMULATE
    ViewerBackground blk
  Next
End Sub

Sub TorpedoSound
  '1690
  Local ZO,ZN
  Play volume 27,27

  For ZO=5 To 3 Step -1
    For ZN=12 To 1 Step -1
   'PLAY "L255O=ZO;=ZN;"
      PlayNote(ZO,ZN,10)
   Pause 1
  Next ZN,ZO
End Sub

Function GetPrintPos(X,Y)
  '1600
  GetPrintPos=Int(7.5+X*2+32*Y)
End Function

Sub HideLastTorpedo
 '900
 BlitChar Q,chrSpace
End Sub

Sub FixShipAfterTorpedo
 '890
 BlitChar Q,214
End Sub

Sub Explosion
  ' 1580 - 1590
  Local Z,ZZ,n
  If O<>1 Then
    Return
  Else
    Q=GetPrintPos(X,Y)
    For Z=1 To 40
      ZZ=Rand(10)
      'Play "T255O1=ZZ;"
      PlayNote 1,ZZ,35
      ClearMessageArea
      n=Q+Rand(2)-2
      BlitChar n, 128+Rand(62)
    Next
  End If

  BlitSpace(Q-1)
  PointToMessageArea
End Sub

' *************************************
' Game Routines
' *************************************

Sub InitShip
  '1570
  Local Z
  ST=stDocked
  For Z=1 To 8:DNA(Z)=0:Next
  E=5000
  ShowField 122,E,4
  UpdateTorpedo
  G=0 'Shields
  UpdateShields
  SC=0 'ViewBar
End Sub

Sub BuildSpaceMap
  '170-200
  Local R, S, I, J

  Do

  For J=1 To 8:For I=1 To 8

  If IntroMusic=1 Then PlayIntroNote

  ' Generate Klingons
  R=Rand(100)

  ' Klingons in Sector
  If R>96 Then
    K=3
  Else If R>90 Then
    K=2
  Else If R>75 Then
    K=1
  Else
    K=0
  End If

  ' Add to total Klingons
  KT=KT+K

  ' Generate starbases
  R=Rand(100)

  ' How many starbases this sector (0-1)
  If R<98 Then
    B=0
  Else
    B=1:BT=BT+1
  End If

  ' Generate stars
  S=Rand(8)

  ' Galaxy sector code Klingons + Bases + Stars
  GNA(I,J)=K*100+B*10+S

  Next I,J

  ' Set initial Klingon count
  K7=KT

  ' IF no star bases generated then add 1
  If BT=0 Then
    I=Rand(8):J=Rand(8)
    GNA(I,J)=GNA(I,J)+10:BT=1
  End If

  ' If no Klingons generated...
  ' build space all over again?
  Loop Until KT>0
End Sub

Sub LostGame
  '1080
  Print
  Print "THERE ARE STILL ";
  Print Str$(KT)+" KLINGON "
  Print "BATTLE-CRUISERS SURVIVING."
  End
End Sub

Sub OutOfTime
  '1060
  Local msg$="IT IS STARDATE "+Str$(T)
  msg$=msg$+". "
  ShowMessage msg$
  Print
  LostGame
End Sub

Sub WinGame
  '1090
  Print
  Print "LAST BATTLE-CRUISER DESTROYED !"
  Print "THE ";US$;" IS SAVED !!"
  Print "YOUR EFFICIENCY RATED =";
  Print Str$(K7/(T-T0)*1000)
  End
End Sub

Sub OutOfGas
  '1040
  Print "THE ";VS$;" IS DEAD IN SPACE.";
  Print "YOU ARE AT THE KLINGONS' MERCY"
  Pause 2000*6
  Do
    If K=0 Then
      LostGame
    Else
        StarBaseDefenese
        Exit Do
    End If
  Loop
  OutOfTime
End Sub

Sub PickUnusedSector
  '1310
  Do
    X=Rand(8):Y=Rand(8)
  Loop Until SNA(X,Y)=snaSpace
End Sub

Sub ClearQuadrant
  Local X,Y
  For X=1 To 8:For Y=1 To 8
    SNA(X,Y)=snaSpace
  Next Y,X
End Sub

Sub UpdateStarDate
  '1520
  T=T+1
  PrintAt 65,Str$(T)
  PointToMessageArea
  If T>T0+TT Then OutOfTime
End Sub

Sub SetRedAlert
  '1440
  If ST<>stRed Then
    ST=stRed
  End If
  SignalAlert
End Sub

Sub SetYellowAlert
  If ST<>stYellow Then
    ST=stYellow
  End If
  SignalAlert
End Sub

Sub CheckIfDocked
  '1420
  For Z=U-1 To U+1

  If Z>=1 And Z<=8 Then
    If SNA(Z,V)=snaBase Then
      Z=U+1
      DockAtBase
      Return
    End If
  End If

  Next
End Sub

Sub SetAlertStatus
  CheckIfDocked

  '1430
  If K=0 Then
    Do1450
  Else
    SetRedAlert
    Return
  End If
End Sub

Sub Do1450
  '1450
  If E>300 Then
    ST=stGreen
    'SignalAlert
      ShowAlert ST
    Return
  Else
    If E>5*(G/100+1) Then
      SetYellowAlert
      Return
    End If
  End If

  SetRedAlert

  ShowAlert ST
  'SignalAlert
  PointToMessageArea
End Sub

Sub SignalAlert
  '1470-1490
  Local FL,ZZ

  For FL=1 To 5

  ShowAlert ST
  Pause 1000
  ShowAlert stGreen

  For ZZ=175 To 125 Step -4
    SOUND ZZ,1
  Next ZZ

  If ST=stDocked Then
    SOUND 200,1
  Else
    SOUND 1,1
  End If

  Next FL

  ShowAlert ST
  PointToMessageArea
End Sub

Sub UpdateTorpedo
  '1530
  If ST=stDocked Then
    P=10
  Else
    P=P-1
  End If
  PrintAt 258,Str$(P,2)
  PointToMessageArea
End Sub

Sub UpdateShields
  '1400
  ShowShields
  If G<0 Then
    Local msg$="THE "+US$
    msg$=msg$+" WILL BE CONQUERED"
    PrintAt 320,msg$
    LostGame
  EndIf
End Sub

Sub ShowInoperative
  '370
  ClearMessageArea
  PrintAt 320,DSA$(A)
  Print " - INOPERATIVE"
  If A=4 And CL=1 Then
    Print "ILLOGICAL TO REMAIN HERE,";
    Print "KLINGONS CLOAKING";
  End If
End Sub

Sub ShowSystem
  '1510
  ClearMessageArea
  'Local s$=String$(32,14)
  PrintAt 320,DSA$(R)
  PrintAt 336,Str$(DNA(R))
  PointToMessageArea
End Sub

Sub UpgradeSystem
  '490
  If ST=stDocked Then Return
  R=0
  For I=1 To 8
    If DNA(I)<0 Then
      DNA(I)=DNA(I)+1
      If DNA(I)=0 Then
        R=I:I=8
      End If
    End If
  Next

  If DNA(7)<0 Then Return

  PointToMessageArea

  If R<>0 Then
    SoundAllClear
    ClearMessageArea
    Print DSA$(R);"- NOW FUNCTIONAL.";
    PointToMessageArea
    Return
  End If

  If Rand(10)<9 Then Return
  R=Rand(8)
  If Rand(2)=1 Then
    SystemOut
  Else
    SystemUpgraded
  End If
End Sub

Sub SystemOut
  '530
  DNA(R)=DNA(R)-Rand(5)
  If DNA(R)<0 Then
    SoundAllClear
    ClearMessageArea
    Print DSA$(7);" REPORT:"
    Print DSA$(R);" OUT";
    Pause 2000
    PointToMessageArea
End Sub

Sub SystemUpgraded
  '540
  DNA(R)=DNA(R)+Rand(5)
  SoundAllClear
  ClearMessageArea
  Print DSA$(7);" REPORT:"
  Print DSA$(R);" - UPGRADED"
  Pause 2000
  PointToMessageArea
End Sub

Sub EngageWarp
  If DNA(1)<0 And W>.2 Then
    Print
    Print DSA$(1);
    Print " DAMAGED, MAX. SPEED IS ";
    Print "WARP 0.2"
    W=.25
  Else
    If W<.9 Then W=W*1.25
  End If

  UpgradeSystem
  MoveShip
End Sub

Sub MoveShip
  '560-590
  KlingonAttack
  N=W*8:F=E/(5*(G/100+1))
  If N<=F Then
    F=0
  Else
    N=F:F=1
  End If
  X=U:Y=V:SNA(X,Y)=snaSpace
  GetCourseDelta

  For H=1 To N
    GetCourseResult
    'ON Z GOTO 590,600,680
    Select Case Z
      'Case 1
        'Next
      Case 2
        ShipCrashed
        Return
      Case 3
        CalculateJump
        Return
    End Select
  Next
  If F=0 Then SetShipInQuadrant:Return
  H=H-1
  Arriving
End Sub

Sub ShipCrashed
  '600
  If M2<>0 Then
    Print "SHIP HAS SUSTAINED DAMAGE"
    For I=1 To 8
      DNA(I)=DNA(I)-Rand(M2+1)+1
    Next
    If M2=5 Then ShipCrashBounce:Return
  End If
  Print "NAVIGATIONAL ERROR: ";
  ShipCrashBounce
End Sub

Sub CalculateJump
  '680
  ClearQuadrant

  U=L*8+U+X1*N:V=M*8+V+Y1*N
  L=Int(U/8):M=Int(V/8)
  U=Int(U-L*8+.5):V=Int(V-M*8+.5)

  If U=0 Then L=L-1:U=8
  If V=0 Then M=M-1:V=8

  UpdateStarDate
  UpdateEnergy

  EnterQuadrant
End Sub

Sub Arriving
  '620
  CheckEnergy
  ClearMessageArea
  Print "ENERGY DEPLETED  - ";
  ShipCrashBounce
End Sub

Sub AddBases2Sector
  '270
  If B<>0 Then
    For I=1 To B
      PickUnusedSector
      SNA(X,Y)=snaBase
    Next
  End If
End Sub

Sub AddStars2Sector
  '280
  If S<>0 Then
    For I=1 To S
      PickUnusedSector
      SNA(X,Y)=snaStar
    Next
  End If
End Sub

Sub AddKlingons2Sect
  '1330
  If K<>3 Then If Rand(10)>7 Then CL=1
  For I=1 To 3:KNA(I,3)=-1:Next
  For I=1 To K
    PickUnusedSector
    SNA(X,Y)=CL+1'appearance
    KNA(I,1)=X
    KNA(I,2)=Y
    KNA(I,3)=KlingShields
  Next
End Sub

Sub DockAtBase
  '1610
  If ST=stDocked Then Return
  ClearMessageArea
  PrintAt 320,"SHIELDS DROPPED FOR DOCKING"
  InitShip
  SignalAlert
End Sub

Sub ReadySector
  '220
  X=GNA(L,M)
  K=Int(X/100):X=X-K*100
  B=Int(X/10)
  S=X-B*10

  If K<>0 Then

    If G<=200 Then
      If Z9=1 Or Z7=1 Then
        CombatAreaMsg
      Else
        CombatAreaMsg2
        Z9=1
      End If
    End If

    '260
    AddKlingons2Sect
  End If

  AddBases2Sector
  AddStars2Sector

  '290
  ST=stINIT

End Sub

Sub EnterQuadrant
  '210
  'This quadrant vars:
  ' CL=Cloaking flag, K=Klingon count
  ' B=base count, S=star count
  CL=0:K=0:B=0:S=0
  'ClearQuadrant
  SNA(U,V)=snaShip

  'If within galaxy...
  If L>=1 And L<=8 And M>=1 And M<=8 Then
    ReadySector
  End If

  SetAlertStatus
  UpdateCoords

  If CL=1 Then
    ShowCloakMsg
  End If

  If DNA(2)<0 Then
    O=0:A=2
    ShowInoperative
  End If

  O=1
  LocalScanner

  If K<>0 Then
    If Z7=1 Then
      If Rand(2)>1 Then
        KlingonAttack
      End If
    Else
      Z7=1
    End If
  End If

End Sub

Sub ScanQuandrant
  '300
  UpdateCoords
  If CL=1 Then
    ClearMessageArea
    ShowCloakMsg
  End If
  If DNA(2)<0 Then
    O=0:A=2
    ShowInoperative
    Return
  End If
  LocalScanner
  If K<>0 Then
    If Z7=1 Then
      If Rand(2)>1 Then
        KlingonAttack
      End If
    Else
      Z7=1
    End If
  End If
End Sub

Sub ShipCrashBounce
  '630
  X=X-X1:Y=Y-Y1:N=H-1
  Print DSA$(1)+"SHUT DOWN AT SECTOR";
  Print Format$(X,"%2g");", ";
  Print Format$(Y,"%2g")
  Pause 2000
End Sub

Sub SetShipInQuadrant
  '640
  U=Int(X+.5):V=Int(Y+.5)
  SNA(U,V)=snaShip
  UpdateEnergy
  If W>=1 Then UpdateStardate
  ScanQuandrant
End Sub

Sub DetectDocking
  '1420
  For Z=U-1 To U+1
    If Z>=1 And Z<=8 Then
      If S(Z,V)=stBase Then
      Z=U+1
      DockAtBase
    End If
  Next

  If K=0 Then Do1450
  If ST<>stShip Then
    ST=stShip
    SignalAlert
  End If

  ShowAlert ST
End Sub

Sub UpdateEnergy
  '1390
  E=E-((N*5)*(G/100+1))
  ShowFuel
  CheckIfDocked
  UpdateShields
End Sub

Sub CheckEnergy
  '1460
  PointToMessageArea
  'If G=0 Then OutOfGas
  Print "YOU HAVE ";Str$(E);" UNITS ";
  Print "OF ENERGY. SHIELDS ARE TAKING ";
  Print Str$(G);"UNITS"
  Pause 2000

  If DNA(6)<0 Then
    Print "BUT, ";DSA$(6);" IS OUT.";
    Print "SITUATION IRRECOVERABLE."
    OutOfGas
  End If
End Sub

Function TorpComp(dx,dy)
  TorpComp=Abs(dx)/(Abs(dx)+Abs(dy))
End Function

Function GetTorpCourse(x1, y1, x2, y2)
  Local dx, dy, tc
  dx=x2-x1
  dy=y2-y1

  ' Handle special cases
  If dx=0 And dy=0 Then
    GetTorpCourse=0
    Exit Function
  End If

  If dy=0 Then
    If dx>0 Then
      GetTorpCourse=dirE
    Else
      GetTorpCourse=dirW
    End If
    Exit Function
  End If

  If dx=0 Then
    If dy>0 Then
      GetTorpCourse=dirS
    Else
      GetTorpCourse=dirN
    End If
    Exit Function
  End If

  ' General cases
  If dy<0 Then
    If dx>0 Then
      'NE quadrant:between 1(N) & 3(E)
      tc=dirN+2*TorpComp(dx,dy)
    Else
      'NW quadrant:between 9&7 (wrap)
      tc=dirN2-2*TorpComp(dx,dy)
    End If
  Else
    If dx>0 Then
      'SE quadrant:between 5(S) & 3(E)
      tc=dirS-2*TorpComp(dx,dy)
    Else
      'SW quadrant:between 5(S) & 7(W)
      tc=dirS+2*TorpComp(dx,dy)
    End If
  End If

  GetTorpCourse=tc
End Function

Sub FindCourse(x1,y1,x2,y2)
  '1180
  CC=GetTorpCourse(x1,y1,x2,y2)
  CD=Sqr((x2-x1)^2+(y2-y1)^2)
  OutputCourse CC,CD
End Sub

Sub GetCourseDelta
  '650
  D=Int(C)
  X1=CNA(D,1)+(CNA(D+1,1)-CNA(D,1))*(C-D)
  Y1=CNA(D,2)+(CNA(D+1,2)-CNA(D,2))*(C-D)
End Sub

Sub GetCourseResult
  '660
  X=X+X1:Y=Y+Y1
  C=Int(X+.5):D=Int(Y+.5)
  If C<1 Or C>8 Or D<1 Or D>8 Then
    Z=3
  Else
    If SNA(C,D)<>0 Then
      Z=2
      M2=Rand(2)-1
    Else
      Z=1
    End If
  End If
End Sub

Sub TorpedoHitKlingon
 '920
 For I=1 To 3
  If C=KNA(I,1) And D=KNA(I,2) Then
   KNA(I,3)=-1
   R=320
   X=C:Y=D
   DestroyKlingon
  End If
 Next
 KlingonAttack
End Sub

Sub ExplodeSector
  '1550
  Explosion
  SNA(X,Y)=snaSpace
  GNA(L,M)=K*100+B*10+S
End Sub

Sub DestroyKlingon
  '1560
  K=K-1
  ExplodeSector
  SetAlertStatus
  ClearMessageArea
  PrintAt R, "KLINGON DESTROYED"
  R=R+32
  KT=KT-1
  If KT=0 Then WinGame
  Pause 2000
  If K=0 Then CL=0
End Sub

Sub TorpedoHitStar
 '940
 PrintAt 352, BSA$(10)
 Print " CAPTURED BY STAR GRAVITYAT ";
 Print "SECTOR ";
 Print Str$(X,1)+","+Str$(Y,1)
 KlingonAttack
End Sub

Sub TorpedoHitBase
 '950
 Explosion
 PrintAt 320,"WELL DONE! STAR BASE "
 Print "DESTROYED!"
 B=B-1:BT=BT-1
 M2=4
 X=C:Y=D
 DestroySector
 PrintAt 352,"DAMAGE HAS THROWN YOU "
 Print "OFF COURSE"
 C=Rand(8):W=Rand(8)/2
 MoveShip
End Sub

Sub TorpedoMissed
 '960
 PrintAt 352,BSA$(10)
 Print " MISSED."
 KlingonAttack
 ScanQuandrant
End Sub

Sub DestroySector
  '1550
  Explosion
  SNA(X,Y)=snaSpace
  GNA(L,M)=K*100+B*10+S
End Sub

Sub MoveTorpedo
  '910
  Local inMotion=1

  Do

  GetCourseResult

  If Z=3 Then
    TorpedoMissed
    Return
  End If

  If Y=Int(Y) And O=1 Then
    Q=GetPrintPos(X,Y)
    BlitChar Q,chrTorpedo
    Q=Q-1
    Pause 100'Let it be seen

 Select Case SNA(C,D)+1
  Case 1
   HideLastTorpedo '900
  Case 2
   inMotion=0
   TorpedoHitKlingon '920
  Case 3
   HideLastTorpedo '900
  Case 4
   FixShipAfterTorpedo '890
  Case 5
   inMotion=0
   TorpedoHitStar '940
  Case 6
   inMotion=0
   TorpedoHitBase '950
 End Select
  Else
 Select Case SNA(C,D)+1
  Case 1
   'MoveTorpedo '910 ? Recursive!
  Case 2
   inMotion=0
   TorpedoHitKlingon '920
  Case 3
   MoveTorpedo '910 ? Recursive!
  Case 4
   MoveTorpedo '910 ? Recursive!
  Case 5
   inMotion=0
   TorpedoHitStar '940
  Case 6
   inMotion=0
   TorpedoHitBase '950
 End Select
  End If

  Loop Until inMotion=0
End Sub

Sub LaunchTorpedo
  '880
  Z7=1 '???
  UpdateTorpedo
  GetCourseDelta

  X=U-XL:Y=V-YL
  TorpedoSound
  MoveTorpedo
End Sub

Sub KlingonPhaserHit(I)
  '1320
  Local ZZ
  'ClearMessageArea
  H=(H*2/Sqr((KNA(I,1)-U)^2+(KNA(I,2)-V)^2))
  H=H*(J*Rnd+.2)
  Local hs$ = Str$(H,3,0)
  ZZ=R
  PrintAt ZZ, hs$+" UNIT HIT ON "
  R=R+32
End Sub

Sub KlingonAttack
  '1020
  Local msg$

  If K=0 Then Return
  If ST=stDocked Then
    Z7=0
    msg$="STAR-BASE SHIELDS PROTECT "
    msg$=msg$+"THE "+VS$
    PrintAt 384,msg$
    Return
  End If

  ClearMessageArea
  PointToMessageArea
  R=320
  For I=1 To 3
    H=KNA(I,3)
    If H>=1 Then
      J=1
      FlashPhaserDamage
      'ClearMessageArea
      KlingonPhaserHit(I)
      Print VS$;
      msg$=" FROM SECTOR"
      msg$=msg$+Str$(KNA(I,1),2)+","
      msg$=msg$+Str$(KNA(I,2),2)
      Print msg$;
      G=G-H
    End If
  Next

  UpdateShields
End Sub

Function PromptForCommand(sys)
  PromptForCommand=-1
  'Either can't be local
  'or need build bigger loop/sub
  Local AX$
  AX$=Inkey$
  If AX$="" Then Exit Function
  If AX$=Chr$(PicoLF) Then
    ClearMessageArea
    PointToMessageArea
    PromptForCommand=A
    Exit Function
  End If
  A=Val(AX$)
  ClearCMD sys
  PrintAt 222,Str$(A)
  If sys=syCMD And A>0 And A<9 Then
    PrintAt 249,BSA$(A*2-1)
    PrintAt 281,BSA$(A*2)
  End If
  If sys=syOPT And A>=0 And A<7 Then
    PrintAt 249,BSA$(A*2+17)
    PrintAt 281,BSA$(A*2+18)
  End If
  PromptForCommand=-1
End Function

Sub KlingonsEnterQuadrant
  If L>0 And L<9 And M>0 And M<9 Then
    For N=1 To 8
      For Q=1 To 8
        X=Int(G(N,Q)/100)
        If X<>0 Then
          G(N,Q)=G(N,Q)-X*100
          G(L,M)=G(L,M)+X*100
          K=X
          ClearMessageArea
          Print "KLINGONS HAVE JUST ENTERED THIS QUADRANT"
          AddKlingons2Sect
          SetAlertStatus
        End If
      Next Q
    Next N
  End If
End Sub

Sub IdleEvent
  '430
  H=0
  UpdateStarDate
  PointToMessageArea
  UpgradeSystem

  If K<>0 Then
    If Rnd(2)>1 Then
      KlingonAttack
      Return
    End If
  Else
    If Rnd(10)>8 Then
      UpdateStarDate
      KlingonsEnterQuadrant
    End If
  End If
End Sub

' *************************************
' CMD Routines
' *************************************

Sub SetCourse
  '450
  Local cs$,I

  Do
    ClearMessageArea
    PointToMessageArea
    Input "COURSE(1-9) ";C
  Loop Until C>=0 And C<=9

  If C=0 Then Return

  cs$=Format$(c,"%1.1f")
  PrintAt 253, cs$

  If C=9 Then C=1

  Do
    ClearMessageArea
    PointToMessageArea
    Input "WARP FACTOR (0-8)";W
  Loop Until W>=0 And W<=9

  If W=0 Then Return

  EngageWarp
End Sub

Sub LocalScanner
  '310
  IF DNA(2)<0 THEN 
    O=0:A=2
    ShowInoperative
  ELSE
    Local Z=38
    For Y=1 To 8:For X=1 To 8
      BlitLocalSanner x*2+Z, SNA(X,Y)
    Next X:Z=Z+32:Next Y
  End if
End Sub

Sub RemoteScanner
  '710
  ViewerBackground grn
  Local NNA(3),Z,I,J,A
  Local msg$,s1$,s2$,s3$
  O=0
  A=40
  PrintAt A,YS$,1
  For J=M-1 To M+1
  For Z=1 To 3:NNA(Z)=0:Next
  For I=L-1 To L+1
  If I>0 And I<9 And J>0 And J<9 Then
    NNA(I-L+2)=GNA(I,J)
    If DNA(8)>=0 Then
      ZNA(I,J)=GNA(I,J)
    End If
  End If
  Next I'750
  A=A+32
  s1$=Format$(NNA(1),"%3g")
  s2$=Format$(NNA(2),"%3g")
  s3$=Format$(NNA(3),"%3g")
  msg$=" :"+s1$+":"+s2$+":"+s3$+":  "
  PrintAt A,msg$,1
  A=A+32
  PrintAt A,YS$,1
  Next J
  A=A+32
  PrintAt A,ZS$,1
End Sub

Sub FirePhasers
  '770
  Local I
  If ST=stDocked Then
    Print Docked$;" - ";
    ShowInoperative
    Return
  End If
  If K=0 Then
    NoKlingonsMsg
    Return
  End If
  Do
    PointToMessageArea
    Z7=1
    If DNA(8)<0 Then
      Print DSA$(8);" FAILURE-MANUALLY ";
    End If
    Print "LOCKED ON. ENERGY=";E
    Input "UNITS TO FIRE";F
    If F<=0 Then Return
  Loop Until F<=E
  PointToMessageArea
  E=E-F
  ShowFuel
  If DNA(8)<0 Then F=Rand(F)
  R=320
  ClearMessageArea
  For I=1 To 3
    If KNA(I,3)>0 Then
      H=F/K:J=.98
      KlingonPhaserHit(I)
      KNA(I,3)=KNA(I,3)-H
      Print " KLINGON - "
      If KNA(I,3)<0 Then
        X=KNA(I,1):Y=KNA(I,2)
        DestroyKlingon
      Else
        Print "AT SECTOR ";
        Print Format$(KNA(I,1),"%2.0f");
        Print ", ";
        Print Format$(KNA(I,2),"%2.0f");
        Print " (";
        Print Format$(KNA(I,3),"%3.0f");
        Print " LEFT)";
        Pause 2000
      End If
    End If
  Next
  KlingonAttack
End Sub

Sub PhotonTorpedo
  '850
  If P<1 Then
    Print "ALL PHOTON TORPEDOES EXPENDED."
    Return
  End If

  If CL<>0 Then
    TorpedoOfNoUse
    Return
  End If

  Do
    PrintAt 320,BSA$(10)
    Input " COURSE (1-9)";C
  Loop Until C>=1 And C<=9 Or C=0

  If C=0 Then Return
  If C=9 Then C=1

  LaunchTorpedo
End Sub

Sub SetShields
  '980
  Local X
  ClearMessageArea
  If ST=stDocked Then
    Print Docked$;" - ";
    ShowInoperative
    Return
  End If

  Do
    ClearMessageArea
    PointToMessageArea
    Input "TOTAL UNITS ON SHIELDS";X
  Loop Until X>=0 And X<=E+G

  E=E+G-X:G=X

  ShowFuel
  UpdateShields

  'ClearMessageArea
  PrintAt 352,"ENERGY CONSUMPTION IS NOW"
  Print " ";Format$(5*(G/100+1),"%3.1f")
  Print " UNITS PER SECTOR"
End Sub

Sub DamageReport
  '1000
  R=0
  ClearMessageArea
  PrintAt 320, "NEXT...HIT [Down] "
  Local A,AX$
  Do

  Do
    AX$=Inkey$
    A=Asc(AX$)
  Loop Until A=PicoDown Or A=PicoLF

  If A=PicoLF Then Return

  R=R+1
  If R>8 Then Return

  ShowSystem

  Loop
End Sub

' *************************************
' OPT Routines
' *************************************

Sub ComputerGuidance
  '1290
  If CD<>0 Then
    C=CC:W=CD
    EngageWarp
    Return
  End If

  Print "NO DATA AVAILABLE FOR GUIDANCE  SYSTEM"
End Sub

Sub StatusReport
  '1150
  ViewerBackground grn
  Local A,n
  O=0
  For A=40 To 266 Step 32
    n=A
    PrintAt n,YS$
  Next
  PrintAt 41,"STATUS REPORT:"
  PrintAt 105,"# "
  Print "KLINGONS =";
  Print Format$(KT,"%2g");
  PrintAt 137,"# "
  Print "STARDATES=";
  Print Format$(T0+TT-T,"%2g");
  PrintAt 169,"# "
  Print "STARBASES=";
  Print Format$(BT,"%2g");
End Sub

Sub TorpedoData
  '1160
  Local msg$
  If K=0 Then
    NoKlingonsMsg
    Return
  Else
    CC=U
    A=V
    For F=1 To 3
      If KNA(F,3)>=0 Then
        W=KNA(F,1)
        X=KNA(F,2)
        FindCourse CC,A,W,X
        Exit For
      End If
    Next F

    If F<=3 Then
      If KNA(F,3)<0 Then Return
    End If
  End If
End Sub

Sub ComputeCourse
  '1170
  ClearMessageArea

  msg$="ENTER START AND END CO-ORDINATES"
  PrintAt 384,msg$

  PointToMessageArea
  Input "(X,Y,X,Y)";CC,A,W,X

  FindCourse CC,A,W,X
End Sub

Sub GuidedTorpedo
  '1280
  If DNA(5)<0 Then
    ShowInoperative
    Return
  Else
    If P<1 Or CL=1 Then
      PhotonTorpedo
      Return
    Else
      If CD<>0 Then
        C=CC
        LaunchTorpedo
        Return
     Else
        Print "NO DATA AVAILABLE FOR GUIDANCE  SYSTEM"
        Return
      End If
    End If
  End If
End Sub

Sub MissionRecord
  '1140
  'Like RemoteScanner except 2 digit
  '1=klingon count
  '2=star count (inverse color if base)
  'ie. A green 2 on black background
  'if 2 stars and a base
  'Black 2 on green background no base

  ViewerBackground grn

  Local A=0,CC,O,ccs$,n

  For J=1 To 8:For I=1 To 8

  CC=ZNA(I,J)\100
  ccs$=Left$(Str$(cc),1)
  Color blk,grn
  PrintAt 40+A+(I-1)*2,ccs$

  CC=ZNA(I,J)-100*CC
  n=cc\10
  O=cc Mod 10
  ccs$=Left$(Str$(O),1)
  If n>0 Then
    Color grn,blk
  Else
    Color blk,grn
  End If
  PrintAt 41+A+(I-1)*2,ccs$
  Next
  A=A+32
  Next
  'O=0
  Color blk,grn
End Sub

' *************************************
' Initialization
' WARNING! Data is read in order it
' appears in code. DO NOT CHANGE ORDER!
' *************************************

Sub LoadSystems
  '120,150
  Local X Integer
  For X=1 To 8:Read DSA$(X):Next
  Data "WARP ENGINES","LOCAL SCANNER"
  Data "REMOTE SCANNER","PHASER CONTROL"
  Data "PHOTON TUBES","SHIELD CONTROL"
  Data "DAMAGE CONTROL","COMPUTER"
End Sub

Sub LoadCommands
  '120,160
  Local X Integer
  For X=1 To 30:Read BSA$(X):Next
  Data "SET","COURSE","LOCAL","SCANNER"
  Data "REMOTE","SCANNER","FIRE","PHASERS"
  Data "PHOTON","TORPEDO","SHIELD","CONTROL"
  Data "DAMAGE","REPORT","ONBOARD","CMPUTER"
  Data "COMMAND","MODE","CMPUTER","GUIDNCE"
  Data "STATUS","REPORT","TORPEDO","DATA"
  Data "COMPUTE","COURSE","GUIDED","TORPEDO"
  Data "MISSION","RECORD"
End Sub

Sub LoadCourseDeltas
  '120,140
  'Course Deltas
  Local X,Y Integer
  For Y=1 To 2:For X=1 To 9
    Read CNA(X,Y)
  Next X,Y
  ' Delta X for course 1-9
  Data  0, 1, 1
  Data  1, 0,-1
  Data -1,-1, 0
  ' Delta Y for course 1-9
  Data -1,-1, 0
  Data  1, 1, 1
  Data  0,-1,-1
End Sub

Sub InitStarDate
  '130
  T=(Rand(20)+19)*100+Rand(100)
  T0=T+1:TT=32
  UpdateStarDate
End Sub

Sub InitGame
  MakeFrameBuffer

  LoadSystems
  LoadCommands
  LoadCourseDeltas

  CLS grn
  Color blk,grn

  ShowAuthor
  BuildDisplay
  InitShip
  InitStarDate

  'Init Galactic coords
  L=Rand(8):M=Rand(8)
  'Init Sector coords
  U=Rand(8):V=Rand(8)

  ShowMessage "AWAIT FURTHER ORDERS."
  BuildSpaceMap

  ' Display initial messages and start game play
  SoundAllClear
  ShowGoalMsg

  Pause 2000

  EnterQuadrant

End Sub

' *************************************
' MAIN / START
' *************************************

Sub MainLoop
  Local CMD,Sys

  Sys=syCMD
  ClearCMD Sys

  Do
    A=0
    M2=0
    H=0

    Do

      CMD=PromptForCommand(Sys)

      If Sys=syCMD Then
        Select Case CMD
          Case 1
            SetCourse
            ClearCMD Sys
          Case 2
            LocalScanner
            ClearCMD Sys
          Case 3
            RemoteScanner
            ClearCMD Sys
          Case 4
            FirePhasers
            LocalScanner
            ClearCMD Sys
          Case 5
            PhotonTorpedo
            UpdateCoords
            LocalScanner
            ClearCMD Sys
          Case 6
            SetShields
            ClearCMD Sys
          Case 7
            DamageReport
            ClearCMD Sys
          Case 8
            Sys=syOPT
            ClearCMD Sys
        End Select
      End If

      If Sys=syOPT Then
        Select Case CMD
          Case 0
            Sys=syCMD
            ClearCMD Sys
          Case 1
            ComputerGuidance
            ClearCMD Sys
          Case 2
            StatusReport
            ClearCMD Sys
          Case 3
            TorpedoData
            ClearCMD Sys
          Case 4
            ComputeCourse
            ClearCMD Sys
          Case 5
            GuidedTorpedo
            Sys=syCMD
            ClearCMD Sys
          Case 6
            MissionRecord
            ClearCMD Sys
        End Select
      End If

      CycleviewBar

      H=H+1
      If H>280 Then IdleEvent

      Pause 45 'Slow down!
    Loop

  Loop

End Sub

InitGame
MainLoop
