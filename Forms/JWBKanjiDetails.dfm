object fKanjiDetails: TfKanjiDetails
  Left = 686
  Top = 140
  BorderStyle = bsSizeToolWin
  Caption = '#00159^eCharacter details'
  ClientHeight = 547
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSecondHalf: TPanel
    Left = 0
    Top = 200
    Width = 321
    Height = 347
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object pnlFooter: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 326
      Width = 315
      Height = 18
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 340
      object btnClose: TButton
        Left = 0
        Top = 0
        Width = 240
        Height = 18
        Align = alClient
        Caption = '#00170^eClose'
        Default = True
        TabOrder = 0
        OnClick = btnCloseClick
        OnKeyPress = btnCloseKeyPress
      end
      object btnDock: TButton
        Left = 240
        Top = 0
        Width = 75
        Height = 18
        Hint = '#00171^eDocks / undocks this window into main window'
        Align = alRight
        Caption = 'DOCK'
        TabOrder = 1
        OnClick = btnDockClick
      end
    end
    object Scrollbox: TScrollBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 315
      Height = 317
      VertScrollBar.Tracking = True
      Align = alClient
      BorderStyle = bsNone
      Padding.Right = 5
      TabOrder = 1
      OnClick = ScrollboxClick
      ExplicitTop = 16
      ExplicitHeight = 315
      object pbKanjiInfo: TPaintBox
        Left = 0
        Top = 40
        Width = 310
        Height = 200
        Align = alTop
        OnMouseDown = pbKanjiInfoMouseDown
        OnMouseMove = pbKanjiInfoMouseMove
        OnMouseUp = pbKanjiInfoMouseUp
        OnPaint = pbKanjiInfoPaint
        ExplicitLeft = 2
        ExplicitTop = 208
        ExplicitWidth = 295
      end
      object FlowPanel1: TFlowPanel
        Left = 0
        Top = 0
        Width = 310
        Height = 21
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 0
        object RxLabel1: TLabel
          Left = 0
          Top = 0
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
          Left = 122
          Top = 0
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
        object cbCategories: TComboBox
          Left = 141
          Top = 0
          Width = 127
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbCategoriesChange
        end
        object btnAddToCategory: TSpeedButton
          Left = 268
          Top = 0
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
      end
      object FlowPanel2: TFlowPanel
        Left = 0
        Top = 21
        Width = 310
        Height = 19
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 1
        object ProUrlLabel1: TUrlLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
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
          AlignWithMargins = True
          Left = 63
          Top = 3
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
          AlignWithMargins = True
          Left = 125
          Top = 3
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
          AlignWithMargins = True
          Left = 167
          Top = 3
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
          AlignWithMargins = True
          Left = 199
          Top = 3
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
      end
    end
  end
  object pnlFirst: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 315
    Height = 174
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ShapeKanji: TShape
      Left = 0
      Top = 0
      Width = 153
      Height = 153
      Brush.Color = clWindow
    end
    object pbKanji: TPaintBox
      Left = 8
      Top = 8
      Width = 137
      Height = 137
      Color = clBtnFace
      ParentColor = False
      OnPaint = pbKanjiPaint
    end
    object lblMeaning: TLabel
      Left = 160
      Top = 88
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
    object ShapeRadical: TShape
      Left = 160
      Top = 16
      Width = 65
      Height = 65
      Brush.Color = clWindow
    end
    object pbRadical: TPaintBox
      Left = 168
      Top = 24
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
      Left = 240
      Top = 16
      Width = 65
      Height = 65
      Brush.Color = clWindow
    end
    object pbSimplified: TPaintBox
      Left = 248
      Top = 24
      Width = 49
      Height = 49
      OnMouseDown = pbSimplifiedMouseDown
      OnMouseMove = pbSimplifiedMouseMove
      OnMouseUp = pbSimplifiedMouseUp
      OnPaint = pbSimplifiedPaint
    end
    object RxLabel10: TLabel
      Left = 160
      Top = 0
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
      Left = 240
      Top = 0
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
      Left = 159
      Top = 130
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
      Left = 108
      Top = 156
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
    object RxLabel39: TLabel
      Left = 3
      Top = 156
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
      Left = 219
      Top = 67
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
      Left = 115
      Top = 153
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
  end
  object FormPlacement1: TFormPlacement
    UseRegistry = False
    IniSection = '\Software\Labyrinth\Wakan\DetailPos'
    Left = 96
    Top = 112
  end
end
