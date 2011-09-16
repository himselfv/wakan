object fKanjiDetails: TfKanjiDetails
  Left = 686
  Top = 140
  Width = 337
  Height = 452
  BorderStyle = bsSizeToolWin
  Caption = '#00159^eCharacter details^cDetaily znaku'
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
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 6
    Top = 220
    Width = 315
    Height = 162
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Shape2: TShape
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
  object Shape8: TShape
    Left = 166
    Top = 24
    Width = 65
    Height = 65
    Brush.Color = clWindow
  end
  object PaintBox1: TPaintBox
    Left = 14
    Top = 16
    Width = 137
    Height = 137
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox1Paint
  end
  object PaintBox2: TPaintBox
    Left = 174
    Top = 32
    Width = 49
    Height = 49
    Color = clBtnFace
    ParentColor = False
    OnDblClick = PaintBox2DblClick
    OnMouseDown = PaintBox2MouseDown
    OnMouseMove = PaintBox2MouseMove
    OnMouseUp = PaintBox2MouseUp
    OnPaint = PaintBox2Paint
  end
  object Shape10: TShape
    Left = 254
    Top = 24
    Width = 65
    Height = 65
    Brush.Color = clWindow
  end
  object PaintBox4: TPaintBox
    Left = 262
    Top = 32
    Width = 49
    Height = 49
    OnMouseDown = PaintBox4MouseDown
    OnMouseMove = PaintBox4MouseMove
    OnMouseUp = PaintBox4MouseUp
    OnPaint = PaintBox4Paint
  end
  object RxLabel10: TRxLabel
    Left = 166
    Top = 5
    Width = 168
    Height = 13
    Caption = '#00160^eRadical:^cRadikál:'
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
    Width = 217
    Height = 13
    Caption = '#00162^eStroke count:^cPoèet tahù:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object ProUrlLabel1: TProUrlLabel
    Left = 8
    Top = 198
    Width = 54
    Height = 13
    Cursor = 15000
    Hint = 
      '#00163^ewww.zhongwen.com - Etymological information about the ch' +
      'aracter^cwww.zhongwen.com - Etymologická informace o znaku'
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
  object ProUrlLabel2: TProUrlLabel
    Left = 83
    Top = 198
    Width = 56
    Height = 13
    Cursor = 15000
    Hint = 
      '#00164^ewww.csse.monash.edu.au/~jwb/wwwjdic - Jim Breen'#39's WWWJDI' +
      'C dictionary server^cwww.csse.monash.edu.au/~jwb/wwwjdic - Infor' +
      'mace o znaku ve slovníku'
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
  object ProUrlLabel3: TProUrlLabel
    Left = 161
    Top = 198
    Width = 36
    Height = 13
    Cursor = 15000
    Hint = 
      '#00165^echarts.unicode.org/unihan - UniHan entry for this charac' +
      'ter^ccharts.unicode.org/unihan - UniHan záznam pro tento znak'
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
  object ProUrlLabel4: TProUrlLabel
    Left = 218
    Top = 198
    Width = 26
    Height = 13
    Cursor = 15000
    Hint = 
      '#00166^ewww.ocrat.com - Animated stroke order^cwww.ocrat.com - A' +
      'nimované poøadí tahù'
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
  object ProUrlLabel5: TProUrlLabel
    Left = 263
    Top = 198
    Width = 56
    Height = 13
    Cursor = 15000
    Hint = 
      '#00167^eweb.mit.edu/jpnet/ji - KanjiProject Data Page^cweb.mit.e' +
      'du/jpnet/ji - Datová stránka Kanji Projectu'
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
    Hint = 
      '#00168^eShow/hide stroke order (only for Japanese Jouyou-kanji)^' +
      'cZobrazit/schovat poøadí tahù (pouze pro japonské Jouyou-kanji)'
    AllowAllUp = True
    GroupIndex = 99
    Caption = '#00169^eOrder^cPoøadí'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object RxLabel1: TRxLabel
    Left = 5
    Top = 182
    Width = 196
    Height = 13
    Caption = '#00879^eCategories:^cKategorie:'
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
    Width = 313
    Height = 160
    VertScrollBar.Position = 258
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 0
    object PaintBox3: TPaintBox
      Left = 0
      Top = -250
      Width = 295
      Height = 800
      OnMouseDown = PaintBox3MouseDown
      OnMouseMove = PaintBox3MouseMove
      OnMouseUp = PaintBox3MouseUp
      OnPaint = PaintBox3Paint
    end
  end
  object Button1: TButton
    Left = 6
    Top = 390
    Width = 235
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = '#00170^eClose^cZavøít'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
    OnKeyPress = Button1KeyPress
  end
  object Button2: TButton
    Left = 248
    Top = 391
    Width = 75
    Height = 17
    Hint = 
      '#00171^eDocks / undocks this window into main window^cZaøadí neb' +
      'o vyøadí okno z hlavního okna'
    Anchors = [akRight, akBottom]
    Caption = 'DOCK'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 168
    Top = 161
    Width = 127
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = ComboBox1Change
  end
  object FormPlacement1: TFormPlacement
    Active = False
    IniSection = '\Software\Labyrinth\Wakan\DetailPos'
    UseRegistry = True
    Left = 96
    Top = 112
  end
end
