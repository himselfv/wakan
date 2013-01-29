object fKanjiDetails: TfKanjiDetails
  Left = 686
  Top = 140
  BorderStyle = bsSizeToolWin
  Caption = '#00159^eCharacter details'
  ClientHeight = 418
  ClientWidth = 321
  Color = clBtnFace
  Constraints.MaxWidth = 337
  Constraints.MinHeight = 254
  Constraints.MinWidth = 337
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    321
    418)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 6
    Top = 220
    Width = 307
    Height = 162
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object ShapeKanji: TShape
    Left = 6
    Top = 8
    Width = 153
    Height = 153
    Brush.Color = clWindow
  end
  object Label1: TLabel
    Left = 166
    Top = 94
    Width = 151
    Height = 60
    AutoSize = False
    Caption = '-'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object RxLabel21: TRxLabel
    Left = 6
    Top = 8
    Width = 46
    Height = 18
    Caption = 'Kanji'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object ShapeRadical: TShape
    Left = 166
    Top = 24
    Width = 65
    Height = 65
    Brush.Color = clWindow
  end
  object pbKanji: TPaintBox
    Left = 14
    Top = 16
    Width = 137
    Height = 137
    Color = clBtnFace
    ParentColor = False
    OnPaint = pbKanjiPaint
  end
  object pbRadical: TPaintBox
    Left = 174
    Top = 32
    Width = 49
    Height = 49
    Color = clBtnFace
    ParentColor = False
    OnDblClick = pbRadicalDblClick
    OnMouseDown = pbRadicalMouseDown
    OnMouseMove = pbRadicalMouseMove
    OnMouseUp = pbRadicalMouseUp
    OnPaint = pbRadicalPaint
  end
  object ShapeSimplified: TShape
    Left = 254
    Top = 24
    Width = 65
    Height = 65
    Brush.Color = clWindow
  end
  object pbSimplified: TPaintBox
    Left = 262
    Top = 32
    Width = 49
    Height = 49
    OnMouseDown = pbSimplifiedMouseDown
    OnMouseMove = pbSimplifiedMouseMove
    OnMouseUp = pbSimplifiedMouseUp
    OnPaint = pbSimplifiedPaint
  end
  object RxLabel10: TRxLabel
    Left = 166
    Top = 5
    Width = 107
    Height = 13
    Caption = '#00160^eRadical:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object RxLabel35: TRxLabel
    Left = 246
    Top = 5
    Width = 37
    Height = 13
    Caption = 'Simpl:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object RxLabel38: TRxLabel
    Left = 206
    Top = 8
    Width = 46
    Height = 18
    Alignment = taRightJustify
    Caption = 'Kanji'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Label9: TLabel
    Left = 111
    Top = 165
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object SpeedButton21: TSpeedButton
    Left = 295
    Top = 161
    Width = 24
    Height = 21
    Caption = '+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = SpeedButton21Click
  end
  object RxLabel39: TRxLabel
    Left = 5
    Top = 165
    Width = 137
    Height = 13
    Caption = '#00162^eStroke count:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object ProUrlLabel1: TUrlLabel
    Left = 8
    Top = 198
    Width = 54
    Height = 13
    Cursor = crHandPoint
    Hint = 
      '#00163^ewww.zhongwen.com - Etymological information about the ch' +
      'aracter'
    Caption = 'ZhongWen'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object ProUrlLabel2: TUrlLabel
    Left = 79
    Top = 198
    Width = 56
    Height = 13
    Cursor = crHandPoint
    Hint = 
      '#00164^ewww.csse.monash.edu.au/~jwb/wwwjdic - Jim Breen'#39's WWWJDI' +
      'C dictionary server'
    Caption = 'WWWJDIC'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object ProUrlLabel3: TUrlLabel
    Left = 154
    Top = 198
    Width = 36
    Height = 13
    Cursor = crHandPoint
    Hint = 
      '#00165^echarts.unicode.org/unihan - UniHan entry for this charac' +
      'ter'
    Caption = 'UniHan'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object ProUrlLabel4: TUrlLabel
    Left = 210
    Top = 198
    Width = 26
    Height = 13
    Cursor = crHandPoint
    Hint = '#00166^ewww.ocrat.com - Animated stroke order'
    Caption = 'Ocrat'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object ProUrlLabel5: TUrlLabel
    Left = 257
    Top = 198
    Width = 56
    Height = 13
    Cursor = crHandPoint
    Hint = '#00167^eweb.mit.edu/jpnet/ji - KanjiProject Data Page'
    Caption = 'KanjiProject'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label2: TLabel
    Left = 225
    Top = 75
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Caption = ' '
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object SpeedButton1: TSpeedButton
    Left = 121
    Top = 163
    Width = 38
    Height = 17
    Hint = '#00168^eShow/hide stroke order (only for Japanese Jouyou-kanji)'
    AllowAllUp = True
    GroupIndex = 99
    Caption = '#00169^eOrder'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object RxLabel1: TRxLabel
    Left = 5
    Top = 182
    Width = 124
    Height = 13
    Caption = '#00879^eCategories:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label3: TLabel
    Left = 93
    Top = 181
    Width = 19
    Height = 13
    Caption = 'cat'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object ScrollBox1: TScrollBox
    Left = 7
    Top = 221
    Width = 304
    Height = 160
    VertScrollBar.Position = 258
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 0
    object pbKanjiInfo: TPaintBox
      Left = 0
      Top = -250
      Width = 295
      Height = 800
      OnMouseDown = pbKanjiInfoMouseDown
      OnMouseMove = pbKanjiInfoMouseMove
      OnMouseUp = pbKanjiInfoMouseUp
      OnPaint = pbKanjiInfoPaint
    end
  end
  object btnClose: TButton
    Left = 6
    Top = 390
    Width = 225
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = '#00170^eClose'
    Default = True
    TabOrder = 1
    OnClick = btnCloseClick
    OnKeyPress = btnCloseKeyPress
  end
  object btnDock: TButton
    Left = 237
    Top = 390
    Width = 75
    Height = 17
    Hint = '#00171^eDocks / undocks this window into main window'
    Anchors = [akRight, akBottom]
    Caption = 'DOCK'
    TabOrder = 2
    OnClick = btnDockClick
  end
  object ComboBox1: TComboBox
    Left = 168
    Top = 161
    Width = 127
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBox1Change
  end
  object FormPlacement1: TFormPlacement
    Active = False
    IniSection = '\Software\Labyrinth\Wakan\DetailPos'
    UseRegistry = False
    Left = 96
    Top = 112
  end
end
