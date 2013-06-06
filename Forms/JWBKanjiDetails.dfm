object fKanjiDetails: TfKanjiDetails
  Left = 686
  Top = 140
  BorderStyle = bsSizeToolWin
  Caption = '#00159^eCharacter details'
  ClientHeight = 370
  ClientWidth = 321
  Color = clBtnFace
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
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    321
    370)
  PixelsPerInch = 96
  TextHeight = 13
  object ShapeKanji: TShape
    Left = 6
    Top = 8
    Width = 153
    Height = 153
    Brush.Color = clWindow
  end
  object lblMeaning: TLabel
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
  object RxLabel21: TLabel
    Left = 6
    Top = 8
    Width = 44
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
  object RxLabel10: TLabel
    Left = 166
    Top = 5
    Width = 105
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
  object RxLabel35: TLabel
    Left = 246
    Top = 5
    Width = 35
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
  object RxLabel38: TLabel
    Left = 208
    Top = 8
    Width = 44
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
  object lblStrokeCount: TLabel
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
  object btnAddToCategory: TSpeedButton
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
    OnClick = btnAddToCategoryClick
  end
  object RxLabel39: TLabel
    Left = 5
    Top = 165
    Width = 135
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
  object lblRadicalNo: TLabel
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
  object btnStrokeOrder: TSpeedButton
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
    OnClick = btnStrokeOrderClick
  end
  object RxLabel1: TLabel
    Left = 5
    Top = 182
    Width = 122
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
  object lblCategories: TLabel
    Left = 93
    Top = 182
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
  object pnlSecondHalf: TPanel
    Left = 7
    Top = 198
    Width = 305
    Height = 164
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      305
      164)
    object ProUrlLabel1: TUrlLabel
      Left = 1
      Top = 0
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
      Transparent = True
    end
    object ProUrlLabel2: TUrlLabel
      Left = 72
      Top = 0
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
      Transparent = True
    end
    object ProUrlLabel3: TUrlLabel
      Left = 147
      Top = 0
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
      Transparent = True
    end
    object ProUrlLabel4: TUrlLabel
      Left = 203
      Top = 0
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
      Transparent = True
    end
    object ProUrlLabel5: TUrlLabel
      Left = 250
      Top = 0
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
      Transparent = True
    end
    object btnClose: TButton
      Left = -1
      Top = 144
      Width = 225
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '#00170^eClose'
      Default = True
      TabOrder = 0
      OnClick = btnCloseClick
      OnKeyPress = btnCloseKeyPress
    end
    object btnDock: TButton
      Left = 230
      Top = 144
      Width = 75
      Height = 17
      Hint = '#00171^eDocks / undocks this window into main window'
      Anchors = [akRight, akBottom]
      Caption = 'DOCK'
      TabOrder = 1
      OnClick = btnDockClick
    end
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 23
      Width = 304
      Height = 115
      VertScrollBar.Position = 258
      VertScrollBar.Tracking = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelKind = bkFlat
      BorderStyle = bsNone
      TabOrder = 2
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
  end
  object cbCategories: TComboBox
    Left = 168
    Top = 161
    Width = 127
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbCategoriesChange
  end
  object FormPlacement1: TFormPlacement
    UseRegistry = False
    IniSection = '\Software\Labyrinth\Wakan\DetailPos'
    Left = 96
    Top = 112
  end
end
