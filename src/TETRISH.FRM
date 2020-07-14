VERSION 5.00
Begin VB.Form tet 
   Caption         =   "Form1"
   ClientHeight    =   9675
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   9270
   LinkTopic       =   "Form1"
   ScaleHeight     =   645
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   618
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Picture2 
      AutoRedraw      =   -1  'True
      Height          =   2175
      Left            =   4920
      ScaleHeight     =   141
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   173
      TabIndex        =   10
      Top             =   6720
      Width           =   2655
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   300
      Left            =   2640
      Top             =   8520
   End
   Begin VB.PictureBox spic 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   1230
      Left            =   5640
      ScaleHeight     =   24.849
      ScaleMode       =   0  'User
      ScaleWidth      =   51.79
      TabIndex        =   1
      Top             =   4920
      Width           =   1230
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      DrawMode        =   7  'Invert
      ForeColor       =   &H80000008&
      Height          =   7500
      Left            =   120
      ScaleHeight     =   500
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   300
      TabIndex        =   0
      Top             =   120
      Width           =   4500
   End
   Begin VB.Label Label6 
      Caption         =   "время"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   375
      Left            =   5040
      TabIndex        =   9
      Top             =   1560
      Width           =   1095
   End
   Begin VB.Label Label2 
      Caption         =   "уровень"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   375
      Left            =   5040
      TabIndex        =   8
      Top             =   1080
      Width           =   1335
   End
   Begin VB.Label Label1 
      Caption         =   "линий"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   375
      Left            =   5040
      TabIndex        =   7
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label lblTIme 
      ForeColor       =   &H00008000&
      Height          =   375
      Left            =   6120
      TabIndex        =   6
      Top             =   1560
      Width           =   1095
   End
   Begin VB.Label lblLevel 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF8080&
      Height          =   375
      Left            =   6480
      TabIndex        =   5
      Top             =   1080
      Width           =   735
   End
   Begin VB.Label lblLine 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   375
      Left            =   6360
      TabIndex        =   4
      Top             =   600
      Width           =   855
   End
   Begin VB.Label Label5 
      Caption         =   "Label5"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   735
      Left            =   4800
      TabIndex        =   3
      Top             =   2280
      Width           =   2415
   End
   Begin VB.Label Label3 
      Height          =   375
      Left            =   4920
      TabIndex        =   2
      Top             =   4320
      Width           =   3255
   End
   Begin VB.Menu file 
      Caption         =   "игра"
      Begin VB.Menu newgame 
         Caption         =   "новая"
         Shortcut        =   {F2}
      End
      Begin VB.Menu puz 
         Caption         =   "пауза"
         Shortcut        =   {F3}
      End
   End
   Begin VB.Menu ur 
      Caption         =   "уровни"
      Begin VB.Menu u 
         Caption         =   "уровень 1"
         Checked         =   -1  'True
         Index           =   1
      End
      Begin VB.Menu u 
         Caption         =   "уровень 2"
         Index           =   2
      End
      Begin VB.Menu u 
         Caption         =   "уровень 3"
         Index           =   3
      End
      Begin VB.Menu u 
         Caption         =   "уровень 4"
         Index           =   4
      End
      Begin VB.Menu u 
         Caption         =   "уровень 5"
         Index           =   5
      End
      Begin VB.Menu u 
         Caption         =   "уровень 6"
         Index           =   6
      End
      Begin VB.Menu u 
         Caption         =   "уровень 7"
         Index           =   7
      End
      Begin VB.Menu u 
         Caption         =   "уровень 8"
         Index           =   8
      End
      Begin VB.Menu u 
         Caption         =   "уровень 9"
         Index           =   9
      End
      Begin VB.Menu u 
         Caption         =   "уровень 10"
         Index           =   10
      End
   End
   Begin VB.Menu options 
      Caption         =   "опции"
   End
   Begin VB.Menu help 
      Caption         =   "помощь"
      Begin VB.Menu about 
         Caption         =   "о программе"
      End
   End
End
Attribute VB_Name = "tet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Private Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Const SRCCOPY = &HCC0020
Dim fig(7, 5) As Long
Dim fig_b(2, 11) As Integer
Dim fig_ogr(2) As Integer
Dim figur As Integer
Dim poloj As Integer
Dim nextfigur As Integer
Dim nextpoloj As Integer
Dim t As Integer
Dim sost As Integer
Dim col As Single
Dim oldfigur As Integer
Dim oldpoloj As Integer
Dim x As Integer
Dim y As Integer
Dim oldx As Integer
Dim oldy As Integer
Dim ti As Integer
Dim otkl As Integer
Dim switch As Integer
Dim ps As Boolean
Dim tin As Integer
Dim urg  As Integer
Private Sub Form_Load()
figur = 0
poloj = 0
oldfigur = figur
oldpoloj = poloj
Randomize (10000)
nextfigur = Int(Rnd(7) * 7 + 1)
Randomize (7000)
nextpoloj = Int(Rnd(fig(nextfigur, 1)) * fig(nextfigur, 1) + 1)
tin = 300
urg = 1

'кол-во положений буквы г
fig(1, 1) = 4
'кол-во точек
fig(1, 2) = 4
'цвет
fig(1, 3) = vbGreen

'кол-во положений палочки
fig(2, 1) = 2
'кол-во точек
fig(2, 2) = 4
'цвет
fig(2, 3) = vbYellow

'кол-во положений трёшки
fig(3, 1) = 4
'кол-во точек
fig(3, 2) = 4
'цвет
fig(3, 3) = vbRed

'кол-во положений змейки
fig(4, 1) = 2
'кол-во точек
fig(4, 2) = 4
'цвет
fig(4, 3) = vbMagenta

'кол-во положений кубик
fig(5, 1) = 1
'кол-во точек
fig(5, 2) = 4
'цвет
fig(5, 3) = vbBlue

'кол-во положений обратная буква г
fig(6, 1) = 4
'кол-во точек
fig(6, 2) = 4
'цвет
fig(6, 3) = vbWhite

'кол-во положений обратная змейка
fig(7, 1) = 2
'кол-во точек
fig(7, 2) = 4
'цвет
fig(7, 3) = vbCyan


End Sub
Sub koord(f As Integer, p As Integer)

Select Case f

Case 1
    
    Select Case p
    Case 1
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
       
    fig_b(1, 3) = 40
    fig_b(2, 3) = 0
          
    fig_b(1, 4) = 0
    fig_b(2, 4) = 20
       
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
            
    Case 2
    
    'box
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
          
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
         
    fig_b(1, 3) = 20
    fig_b(2, 3) = 20
        
    fig_b(1, 4) = 20
    fig_b(2, 4) = 40
        
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
        
    Case 3
        
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 20
       
    fig_b(1, 2) = 20
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 40
    fig_b(2, 3) = 20
            
    fig_b(1, 4) = 40
    fig_b(2, 4) = 0
    
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
    
    Case 4
    
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
        
    fig_b(1, 2) = 0
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 0
    fig_b(2, 3) = 40
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 40
       
    
    
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
    
    End Select
    
'2 - фигура
Case 2
    
    Select Case p
        
    Case 1
    
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
        
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
        
    fig_b(1, 3) = 40
    fig_b(2, 3) = 0
    
    fig_b(1, 4) = 60
    fig_b(2, 4) = 0
    
    'ограничения
    fig_ogr(1) = 80
    fig_ogr(2) = 20
        
    Case 2
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
           
    fig_b(1, 2) = 0
    fig_b(2, 2) = 20
        
    fig_b(1, 3) = 0
    fig_b(2, 3) = 40
    
    fig_b(1, 4) = 0
    fig_b(2, 4) = 60
    
    'ограничения
    fig_ogr(1) = 20
    fig_ogr(2) = 80
   
    End Select


'3 - фигура
Case 3
    
    Select Case p
    
    Case 1
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
         
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
    
    fig_b(1, 3) = 40
    fig_b(2, 3) = 0
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 20
    
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
    
    Case 2
    
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 20
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 40
    fig_b(2, 3) = 20
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 0
    
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
    
    Case 3
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 0
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 0
    fig_b(2, 3) = 40
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 20
    
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
    
    Case 4
     
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 20
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
    
    fig_b(1, 3) = 20
    fig_b(2, 3) = 20
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 40
    
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
   
   End Select
   
'4 - фигура
Case 4
    Select Case p
        
    Case 1
    
    'box
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 0
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 20
    fig_b(2, 3) = 20
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 40
    
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
       
    Case 2
    
    'box
    fig_b(1, 1) = 0
    fig_b(2, 1) = 20
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 20
    fig_b(2, 3) = 0
    
    fig_b(1, 4) = 40
    fig_b(2, 4) = 0
    
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
    
    End Select
    
'5 - фигура
Case 5
    If p = 1 Then
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
    
    fig_b(1, 3) = 0
    fig_b(2, 3) = 20
    
    fig_b(1, 4) = 20
    fig_b(2, 4) = 20
    
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 40
        
    End If
'6-фигура
Case 6
    
    Select Case p
    Case 1
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
       
    fig_b(1, 3) = 40
    fig_b(2, 3) = 0
          
    fig_b(1, 4) = 40
    fig_b(2, 4) = 20
       
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
            
    Case 2
    
    'box
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
          
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
         
    fig_b(1, 3) = 0
    fig_b(2, 3) = 20
        
    fig_b(1, 4) = 0
    fig_b(2, 4) = 40
        
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
        
    Case 3
        
    'box
    
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
       
    fig_b(1, 2) = 0
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 20
    fig_b(2, 3) = 20
            
    fig_b(1, 4) = 40
    fig_b(2, 4) = 20
    
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
    
    Case 4
    
    'box
    
    fig_b(1, 1) = 20
    fig_b(2, 1) = 0
        
    fig_b(1, 2) = 20
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 20
    fig_b(2, 3) = 40
    
    fig_b(1, 4) = 0
    fig_b(2, 4) = 40
       
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
    
    End Select
    
'7 - фигура
Case 7
    Select Case p
        
    Case 1
    
    'box
    fig_b(1, 1) = 20
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 20
    
    fig_b(1, 3) = 0
    fig_b(2, 3) = 20
    
    fig_b(1, 4) = 0
    fig_b(2, 4) = 40
    
    'ограничения
    fig_ogr(1) = 40
    fig_ogr(2) = 60
       
    Case 2
    
    'box
    fig_b(1, 1) = 0
    fig_b(2, 1) = 0
    
    fig_b(1, 2) = 20
    fig_b(2, 2) = 0
    
    fig_b(1, 3) = 20
    fig_b(2, 3) = 20
    
    fig_b(1, 4) = 40
    fig_b(2, 4) = 20
    
    'ограничения
    fig_ogr(1) = 60
    fig_ogr(2) = 40
    
    End Select
        


End Select


End Sub
Private Sub newgame_Click()
Picture1.Cls

lblLevel.Caption = urg
Call zapusk

End Sub

Private Sub Picture1_KeyDown(KeyCode As Integer, Shift As Integer)

If KeyCode = vbKeySpace Then
While otkl <> 1
Call dtet
Wend

End If

If KeyCode = vbKeyUp Then
    sost = 4
    'BitBlt Picture1.hdc, oldx, oldy, spic.Width, spic.Height, spic.hdc, 0, 0, SRCCOPY
    Call resz
    If poloj < fig(figur, 1) Then
        poloj = poloj + 1
        Call koord(figur, poloj)
        If Not (stolk) Then
            'BitBlt spic.hdc, 0, 0, spic.Width, spic.Height, Picture1.hdc, x, y, SRCCOPY
            Call resz
        Else
            poloj = poloj - 1
            Call koord(figur, poloj)
            Call resz
        End If
        
     Else
         poloj = 1
         Call koord(figur, poloj)
         If Not (stolk) Then
            'BitBlt Picture1.hdc, oldx, oldy, spic.Width, spic.Height, spic.hdc, 0, 0, SRCCOPY
            'BitBlt spic.hdc, 0, 0, spic.Width, spic.Height, Picture1.hdc, x, y, SRCCOPY
            Call resz
        Else
            poloj = fig(figur, 1)
            Call koord(figur, poloj)
            Call resz
        End If
        
        
    End If
  
  'Text2.Text = Str(poloj)
End If

If KeyCode = vbKeyDown Then 'And y < 500 - fig_ogr(2) Then
     Call dtet
     'Else
     'If KeyCode = vbKeyDown Then
        'Timer1.Enabled = False
        'Call zapusk
    'End If
       
End If


If KeyCode = vbKeyRight Then ' And x < 300 - fig_ogr(1) Then
      sost = 2
      'BitBlt Picture1.hdc, oldx, oldy, spic.Width, spic.Height, spic.hdc, 0, 0, SRCCOPY
      Call resz
          x = x + 20
           If Not (stolk) Then
            'BitBlt spic.hdc, 0, 0, spic.Width, spic.Height, Picture1.hdc, x, y, SRCCOPY
            Call resz
        Else
            x = x - 20
            Call resz
        End If
    
End If


If KeyCode = vbKeyLeft And x > 0 Then
    sost = 1
    'BitBlt Picture1.hdc, oldx, oldy, spic.Width, spic.Height, spic.hdc, 0, 0, SRCCOPY
    Call resz
        x = x - 20
        If Not (stolk) Then
            BitBlt spic.hdc, 0, 0, spic.Width, spic.Height, Picture1.hdc, x, y, SRCCOPY
            Call resz
        Else
            x = x + 20
            Call resz
        End If
    
End If
'Label5.Caption = Str(x) + " : " + Str(y)

oldx = x
oldy = y

End Sub
Sub resz()
For t = 1 To fig(figur, 2)
Picture1.Line (fig_b(1, t) + x + 1, fig_b(2, t) + y + 1)-(fig_b(1, t) + 19 + x, fig_b(2, t) + 19 + y), fig(figur, 3), BF
Picture1.Line (fig_b(1, t) + x, fig_b(2, t) + y)-(fig_b(1, t) + 20 + x, fig_b(2, t) + 20 + y), &H0&, B
Next

End Sub
Function stolk() As Boolean

For t = 1 To fig(figur, 2)
    
   If Picture1.Point((fig_b(1, t) + 10) + x, (fig_b(2, t) + 10) + y) <> &H0& Then
    stolk = True
    Label3.Caption = "столкновение"
    Exit For
    Else
    stolk = False
    Label3.Caption = ""
    End If
Next


End Function
Sub ochistka()
Dim xt, yt As Integer
Dim nx, ny, nl As Integer
Dim nach, kon As Integer
Dim hBMP As Long
Dim hDestDC As Long

ny = 0
nl = 0
'Do
nach = 0
kon = 0


For yt = 10 To Picture1.Height - 10 Step 10
        nx = 0
    For xt = 10 To Picture1.Width - 10 Step 10

        If Picture1.Point(xt, yt) <> &H0& Then nx = nx + 1

    Next
        If nx = 15 Then
    hBMP = CreateCompatibleBitmap(Picture1.hdc, 300, yt - 10)
    hDestDC = CreateCompatibleDC(Picture1.hdc)
    SelectObject hDestDC, hBMP
    BitBlt hDestDC, 0, 0, 300, yt + 10, Picture1.hdc, 0, 0, SRCCOPY
    BitBlt Picture1.hdc, 0, 20, 300, yt - 10, hDestDC, 0, 0, SRCCOPY
    switch = switch + 1
    lblLine.Caption = Str$(switch)
    If switch / 10 = Int(switch / 10) Then
        urg = urg + 1
        lblLevel.Caption = urg
    End If
    
End If

Next
DeleteObject (hBMP)
DeleteObject (hDestDC)

End Sub

Sub dtet()
'BitBlt Picture1.hdc, oldx, oldy, spic.Width, spic.Height, spic.hdc, 0, 0, SRCCOPY
Call resz
        y = y + 20
        If Not (stolk) Then
'            Label4.Caption = Str(y) + Str(fig_ogr(2))
         '   BitBlt spic.hdc, 0, 0, spic.Width, spic.Height, Picture1.hdc, x, y, SRCCOPY
            Call resz
        Else
            
            y = y - 20
            Call resz
            
            otkl = otkl + 1
            
            If y = 0 Then
               Call zapusk
             Exit Sub
            End If
            
          
            sost = 3
                              
            ti = ti + 1
            
            If otkl = 2 Or ti = 25 Then
            Timer1.Enabled = False
            Call zapusk
            End If
        
        End If


oldy = y
End Sub

Private Sub puz_Click()


If ps = True Then
    ps = False
    Picture1.Enabled = True
    Timer1.Enabled = True
    Picture1.SetFocus
Else
    ps = True
    Call puzpas
End If

End Sub

Private Sub Timer1_Timer()
ti = ti + 1
Label5.Caption = ti
Call dtet
End Sub
Sub zapusk()

Dim i As Integer
    
    figur = nextfigur
    poloj = nextpoloj

    Randomize (100000)
    nextfigur = Int(Rnd(7) * 7 + 1)
    Randomize (5000)
    nextpoloj = Int(Rnd(fig(nextfigur, 1)) * fig(nextfigur, 1) + 1)

Call koord(nextfigur, nextpoloj)
Picture2.Cls
For t = 1 To fig(nextfigur, 2)
Picture2.Line (fig_b(1, t) + 1, fig_b(2, t))-(fig_b(1, t) + 19, fig_b(2, t) + 19), fig(nextfigur, 3), BF
Picture2.Line (fig_b(1, t), fig_b(2, t))-(fig_b(1, t) + 20, fig_b(2, t) + 20), &H0&, B
Next


    

x = 100
y = 0
oldx = x
oldy = y
Call koord(figur, poloj)
If stolk = True Then
MsgBox "игра окончена"
Call puzpas
Exit Sub
End If

'BitBlt spic.hdc, 0, 0, spic.Width, spic.Height, Picture1.hdc, x, y, SRCCOPY
Call ochistka
Call resz

Picture1.Enabled = True
Picture1.SetFocus
Timer1.Enabled = True

ti = 0
otkl = 0
End Sub


Private Sub u1_Click()
u1.Checked = False

End Sub

Private Sub u_Click(Index As Integer)
Call ucl(Index)
Call puzpas
Picture1.Cls
End Sub
Sub puzpas()
Picture1.Enabled = False
Timer1.Enabled = False
End Sub
Sub ucl(indx As Integer)
'смена галочек на меню
Dim tn As Integer
For tn = 1 To 10
If tin <> indx Then u(tn).Checked = False
Next
urg = indx
Select Case indx
Case 1
tin = 300
Case 2
tin = 280
Case 3
tin = 260
Case 4
tin = 240
Case 5
tin = 220
Case 6
tin = 200
Case 7
tin = 180
Case 8
tin = 160
Case 9
tin = 140
Case 10
tin = 120
End Select

Timer1.Interval = tin
u(indx).Checked = True

End Sub
