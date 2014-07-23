object fKanji: TfKanji
  Left = 591
  Top = 473
  BorderStyle = bsSizeToolWin
  Caption = '#00117^eCharacter list'
  ClientHeight = 661
  ClientWidth = 579
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 223
    Width = 579
    Height = 438
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    ExplicitWidth = 560
    ExplicitHeight = 231
    object splDockCompounds: TSplitter
      Left = 0
      Top = 436
      Width = 579
      Height = 2
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Beveled = True
      Visible = False
      ExplicitLeft = 3
      ExplicitTop = 552
      ExplicitWidth = 535
    end
    object BlankPanel1: TBlankPanel
      Left = 0
      Top = 148
      Width = 579
      Height = 261
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TextLeft = 8
      TextTop = 8
      Text = '#00118^eNo characters were found.'
      ExplicitTop = 27
      ExplicitWidth = 560
      ExplicitHeight = 175
    end
    object DrawGrid1: TDrawGrid
      AlignWithMargins = True
      Left = 3
      Top = 151
      Width = 573
      Height = 255
      Align = alClient
      BorderStyle = bsNone
      ColCount = 10
      DefaultColWidth = 59
      DefaultRowHeight = 59
      DefaultDrawing = False
      FixedCols = 0
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goThumbTracking]
      PopupMenu = PopupMenu
      ScrollBars = ssVertical
      TabOrder = 0
      OnClick = DrawGrid1Click
      OnDblClick = DrawGrid1DblClick
      OnDrawCell = DrawGrid1DrawCell
      OnKeyDown = DrawGrid1KeyDown
      OnKeyPress = DrawGrid1KeyPress
      OnKeyUp = DrawGrid1KeyUp
      OnMouseDown = DrawGrid1MouseDown
      OnMouseMove = DrawGrid1MouseMove
      OnMouseUp = DrawGrid1MouseUp
      OnSelectCell = DrawGrid1SelectCell
      ExplicitTop = 30
      ExplicitWidth = 554
      ExplicitHeight = 169
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 124
      Width = 573
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 3
      ExplicitWidth = 554
      object lblFoundChars: TLabel
        Left = 0
        Top = 0
        Width = 363
        Height = 21
        Align = alClient
        Caption = 'Found characters:'
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
        ExplicitTop = 3
      end
      object Label2: TLabel
        AlignWithMargins = True
        Left = 363
        Top = 0
        Width = 76
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 5
        Margins.Bottom = 0
        Align = alRight
        Caption = '#00197^Sort by'
        Layout = tlCenter
        ExplicitLeft = 352
        ExplicitHeight = 13
      end
      object ComboBox3: TComboBox
        Left = 444
        Top = 0
        Width = 129
        Height = 21
        Margins.Left = 0
        Margins.Right = 0
        Align = alRight
        Style = csDropDownList
        ItemIndex = 4
        TabOrder = 0
        Text = 'Gakken Kanji'
        OnChange = SearchFilterChanged
        Items.Strings = (
          '#00146^eRadical'
          '#00147^eStroke count'
          '#00148^eFrequency'
          '#00198^eLearner index'
          'Gakken Kanji'
          'Remembering Kanji'
          '#00149^eRandom')
        ExplicitTop = 3
      end
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 412
      Width = 573
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitTop = 205
      ExplicitWidth = 554
      object btnCompounds: TSpeedButton
        Left = 0
        Top = 0
        Width = 112
        Height = 21
        Hint = '#00125^eCompounds'
        Align = alLeft
        AllowAllUp = True
        GroupIndex = 5
        Caption = '#00125^eCompounds'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnCompoundsClick
        ExplicitLeft = 137
      end
      object btnKanjiDetails: TSpeedButton
        Left = 444
        Top = 0
        Width = 129
        Height = 21
        Hint = '#00123^eDetails (Ctrl-D)'
        Align = alRight
        AllowAllUp = True
        GroupIndex = 4
        Caption = '#00124^eDetails'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnKanjiDetailsClick
        ExplicitLeft = 499
        ExplicitHeight = 17
      end
      object btnPrintCards: TButton
        Left = 315
        Top = 0
        Width = 129
        Height = 21
        Action = aPrint
        Align = alRight
        Caption = '#00128^ePrint cards'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = False
        ExplicitLeft = 296
      end
    end
    object pnlDockCompounds: TPanel
      Left = 0
      Top = 436
      Width = 579
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      UseDockManager = False
      DockSite = True
      TabOrder = 4
      ExplicitTop = 229
      ExplicitWidth = 560
    end
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 579
      Height = 121
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkSoft
      BevelOuter = bvNone
      UseDockManager = False
      DockSite = True
      Padding.Bottom = 2
      ParentBackground = False
      TabOrder = 5
      object Panel8: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 573
        Height = 21
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          573
          21)
        object SpeedButton1: TSpeedButton
          Left = 444
          Top = 0
          Width = 125
          Height = 21
          Hint = '#00125^eCompounds'
          AllowAllUp = True
          GroupIndex = 5
          Caption = 'More'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = SpeedButton1Click
        end
        object Edit1: TEdit
          Left = 0
          Top = 0
          Width = 112
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = edtPinYinChange
        end
        object ComboBox1: TComboBox
          Left = 118
          Top = 0
          Width = 119
          Height = 21
          Style = csDropDownList
          Anchors = [akTop, akRight]
          TabOrder = 1
          OnChange = SearchFilterChanged
          Items.Strings = (
            'Any'
            '-'
            'Character'
            'Definition'
            'On reading'
            'Kun reading'
            'Pinyin'
            'SKIP code'
            '-'
            '*rest*')
        end
      end
      object Panel10: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 30
        Width = 573
        Height = 84
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        ExplicitTop = 2
        ExplicitHeight = 79
        object SpeedButton4: TSpeedButton
          Left = 0
          Top = -1
          Width = 57
          Height = 22
          Hint = 
            '#00190^eFilter by stroke count (you can search by range, ex. 1-6' +
            ')'
          AllowAllUp = True
          GroupIndex = 14
          Caption = '#00191^eStroke #'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          OnClick = SearchFilterChanged
        end
        object SpeedButton8: TSpeedButton
          Left = 0
          Top = 26
          Width = 57
          Height = 22
          Hint = '#00177^eFilter by radical (Ctrl-R)'
          AllowAllUp = True
          GroupIndex = 15
          Caption = '#00178^eRadical'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          OnClick = SearchFilterChanged
        end
        object SpeedButton6: TSpeedButton
          Left = 90
          Top = 27
          Width = 63
          Height = 22
          Hint = '#00177^eFilter by radical (Ctrl-R)'
          AllowAllUp = True
          Caption = '#00195^eList...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          OnClick = pbRadicalsClick
        end
        object SpeedButton5: TSpeedButton
          Left = 0
          Top = 50
          Width = 57
          Height = 22
          Hint = '#00194^eFilter by Jouyou grade (Japanese school grade)'
          AllowAllUp = True
          GroupIndex = 18
          Caption = '#00963^eJouyou'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          OnClick = SearchFilterChanged
        end
        object RangeSpinEdit1: TRangeSpinEdit
          Left = 56
          Top = 0
          Width = 97
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 0
          LowValue = 0
          HighValue = 0
          OnChange = edtStrokeCountChange
        end
        object WakanPaintbox1: TWakanPaintbox
          Left = 57
          Top = 27
          Width = 31
          Height = 22
          Cursor = crHandPoint
          Color = clBtnFace
          DoubleBuffered = True
          OnPaint = pbRadicalsPaint
          OnClick = pbRadicalsClick
        end
        object RangeSpinEdit2: TRangeSpinEdit
          Left = 56
          Top = 50
          Width = 97
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 2
          LowValue = 0
          HighValue = 0
          OnChange = edtJouyouChange
        end
        object JwbCheckbox1: TJwbCheckbox
          Left = 159
          Top = 1
          Width = 131
          Height = 21
          Margins.Top = 0
          Margins.Bottom = 0
          Action = aCommon
          TabOrder = 3
        end
        object JwbCheckbox2: TJwbCheckbox
          Left = 159
          Top = 22
          Width = 145
          Height = 12
          Margins.Top = 0
          Margins.Bottom = 0
          Action = aClipboard
          TabOrder = 4
        end
        object CheckListBox1: TCheckListBox
          AlignWithMargins = True
          Left = 388
          Top = 3
          Width = 185
          Height = 81
          Margins.Left = 0
          Margins.Right = 0
          Margins.Bottom = 0
          OnClickCheck = lbCategoriesClickCheck
          Align = alRight
          ItemHeight = 13
          PopupMenu = pmCategories
          TabOrder = 5
          OnClick = lbCategoriesClick
          OnDblClick = lbCategoriesDblClick
          OnDrawItem = lbCategoriesDrawItem
          ExplicitHeight = 73
        end
      end
    end
  end
  object pnlDockSearch: TPanel
    Left = 0
    Top = 0
    Width = 579
    Height = 223
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    BevelOuter = bvNone
    UseDockManager = False
    DockSite = True
    Padding.Bottom = 2
    ParentBackground = False
    TabOrder = 1
    Visible = False
    ExplicitWidth = 560
    object Panel7: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 209
      Height = 213
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 190
      DesignSize = (
        209
        213)
      object sbPinYin: TSpeedButton
        Left = 0
        Top = 0
        Width = 57
        Height = 22
        Hint = '#00175^eFilter by PinYin (chinese reading) (Ctrl-I)'
        AllowAllUp = True
        GroupIndex = 10
        Caption = '#00964^e&PinYin'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbYomi: TSpeedButton
        Left = 0
        Top = 24
        Width = 57
        Height = 22
        Hint = '#00176^eFilter by japanese reading (Ctrl-Y)'
        AllowAllUp = True
        GroupIndex = 11
        Caption = '#00965^eYomi'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbDefinition: TSpeedButton
        Left = 0
        Top = 48
        Width = 57
        Height = 22
        Hint = '#00179^eFilter by definition (meaning) (Ctrl-M)'
        AllowAllUp = True
        GroupIndex = 12
        Caption = '#00180^eDefinition'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbStrokeCount: TSpeedButton
        Left = 0
        Top = 72
        Width = 57
        Height = 22
        Hint = 
          '#00190^eFilter by stroke count (you can search by range, ex. 1-6' +
          ')'
        AllowAllUp = True
        GroupIndex = 14
        Caption = '#00191^eStroke #'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbJouyou: TSpeedButton
        Left = 0
        Top = 143
        Width = 57
        Height = 22
        Hint = '#00194^eFilter by Jouyou grade (Japanese school grade)'
        AllowAllUp = True
        GroupIndex = 18
        Caption = '#00963^eJouyou'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbListRadicals: TSpeedButton
        Left = 146
        Top = 96
        Width = 63
        Height = 22
        Hint = '#00177^eFilter by radical (Ctrl-R)'
        AllowAllUp = True
        Anchors = [akTop, akRight]
        Caption = '#00195^eList...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = pbRadicalsClick
        ExplicitLeft = 116
      end
      object sbOther: TSpeedButton
        Left = 0
        Top = 167
        Width = 57
        Height = 22
        Hint = '#00192^eFilter by Unicode / Nelson index / Halpern index'
        AllowAllUp = True
        GroupIndex = 13
        Caption = '#00193^eOther'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbRadicals: TSpeedButton
        Left = 0
        Top = 95
        Width = 57
        Height = 22
        Hint = '#00177^eFilter by radical (Ctrl-R)'
        AllowAllUp = True
        GroupIndex = 15
        Caption = '#00178^eRadical'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object sbSKIP: TSpeedButton
        Left = 0
        Top = 119
        Width = 57
        Height = 22
        Hint = '#00181^eFilter by SKIP code (see KANJIDIC for explanation)'
        AllowAllUp = True
        GroupIndex = 16
        Caption = '#00962^eSKIP'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
      end
      object edtPinYin: TEdit
        Left = 56
        Top = 0
        Width = 153
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edtPinYinChange
        ExplicitWidth = 134
      end
      object edtYomi: TEdit
        Left = 56
        Top = 24
        Width = 153
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = edtYomiChange
        ExplicitWidth = 134
      end
      object edtDefinition: TEdit
        Left = 56
        Top = 48
        Width = 153
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = edtDefinitionChange
        ExplicitWidth = 134
      end
      object cbOtherType: TComboBox
        Left = 56
        Top = 191
        Width = 153
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = SearchFilterChanged
        ExplicitWidth = 134
      end
      object edtOther: TEdit
        Left = 56
        Top = 169
        Width = 153
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = edtOtherChange
        ExplicitWidth = 134
      end
      object edtSkip: TEdit
        Left = 56
        Top = 120
        Width = 153
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnChange = edtSkipChange
        ExplicitWidth = 134
      end
      object pbRadicals: TWakanPaintbox
        Left = 57
        Top = 96
        Width = 89
        Height = 22
        Cursor = crHandPoint
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        DoubleBuffered = True
        OnPaint = pbRadicalsPaint
        OnClick = pbRadicalsClick
        ExplicitWidth = 70
      end
      object edtStrokeCount: TRangeSpinEdit
        Left = 56
        Top = 73
        Width = 153
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        MaxValue = 100
        MinValue = 0
        TabOrder = 7
        LowValue = 0
        HighValue = 0
        OnChange = edtStrokeCountChange
        ExplicitWidth = 134
      end
      object edtJouyou: TRangeSpinEdit
        Left = 56
        Top = 143
        Width = 153
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        MaxValue = 100
        MinValue = 0
        TabOrder = 8
        LowValue = 0
        HighValue = 0
        OnChange = edtJouyouChange
        ExplicitWidth = 134
      end
    end
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 218
      Top = 3
      Width = 167
      Height = 213
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 199
      object lbCategories: TCheckListBox
        AlignWithMargins = True
        Left = 0
        Top = 27
        Width = 167
        Height = 186
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        OnClickCheck = lbCategoriesClickCheck
        Align = alClient
        ItemHeight = 13
        PopupMenu = pmCategories
        TabOrder = 0
        OnClick = lbCategoriesClick
        OnDblClick = lbCategoriesDblClick
        OnDrawItem = lbCategoriesDrawItem
      end
      object cbOrAnd: TComboBox
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 167
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 1
        Text = 'In all of:'
        Items.Strings = (
          'In any of:'
          'In all of:'
          'Not in any of:')
      end
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 391
      Top = 3
      Width = 185
      Height = 213
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 372
      object sbClearFilters: TSpeedButton
        Left = 0
        Top = 192
        Width = 185
        Height = 21
        Margins.Top = 5
        Action = aResetFilters
        Align = alBottom
        AllowAllUp = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ExplicitTop = 186
      end
      object Label1: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 42
        Width = 185
        Height = 13
        Margins.Left = 0
        Margins.Top = 9
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = '#00197^Sort by'
        Layout = tlCenter
        ExplicitWidth = 76
      end
      object cbOnlyCommon: TJwbCheckbox
        Left = 0
        Top = 0
        Width = 185
        Height = 21
        Margins.Top = 0
        Margins.Bottom = 0
        Action = aCommon
        Align = alTop
        TabOrder = 0
      end
      object cbInClipboard: TJwbCheckbox
        Left = 0
        Top = 21
        Width = 185
        Height = 12
        Margins.Top = 0
        Margins.Bottom = 0
        Action = aClipboard
        Align = alTop
        TabOrder = 1
      end
      object rgSortBy: TComboBox
        AlignWithMargins = True
        Left = 0
        Top = 58
        Width = 185
        Height = 21
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Style = csDropDownList
        ItemIndex = 4
        TabOrder = 2
        Text = 'Gakken Kanji'
        OnChange = SearchFilterChanged
        Items.Strings = (
          '#00146^eRadical'
          '#00147^eStroke count'
          '#00148^eFrequency'
          '#00198^eLearner index'
          'Gakken Kanji'
          'Remembering Kanji'
          '#00149^eRandom')
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text file (*.txt)|*.txt'
    Left = 272
    Top = 528
  end
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 350
    Top = 528
  end
  object Actions: TActionList
    Left = 272
    Top = 584
    object aSearch: TCheckAction
      AutoCheck = True
      Caption = '#00230^e&Search'
      Hint = '#00119^eSearch & change sort order'
      OnExecute = aSearchExecute
      OnChecked = aSearchChecked
    end
    object aResetFilters: TAction
      Caption = '#00278^eDisplay all'
      Hint = '#00182^eDisplay all characters (Ctrl-N)'
      ShortCut = 16469
      OnExecute = aResetFiltersExecute
    end
    object aClipboard: TAction
      AutoCheck = True
      Caption = '#00281^eIn clipboard only'
      Hint = '#00188^eDisplay only characters in clipboard'
      OnExecute = SearchFilterChanged
    end
    object aCommon: TAction
      AutoCheck = True
      Caption = '#00280^eCommon only'
      Hint = '#00186^eDisplay only common characters'
      OnExecute = SearchFilterChanged
    end
    object aPinYin: TAction
      Caption = '#00282^eSearch by PinYin'
      ShortCut = 16457
      OnExecute = aPinYinExecute
    end
    object aYomi: TAction
      Caption = '#00283^eSearch by Yomi'
      ShortCut = 16473
      OnExecute = aYomiExecute
    end
    object aRadical: TAction
      Caption = '#00284^eSearch by radical...'
      ShortCut = 16466
      OnExecute = aRadicalExecute
    end
    object aMeaning: TAction
      Caption = '#00295^eSearch by meaning'
      ShortCut = 16461
      OnExecute = aMeaningExecute
    end
    object aPrint: TAction
      Caption = '#00234^e&Print cards...'
      Hint = '#00127^ePrint Kanji cards (for memorizing) (Ctrl-F6)'
      ShortCut = 16501
      OnExecute = aPrintExecute
    end
    object aSaveToFile: TAction
      Caption = '#00944^eSave characters to file...'
      OnExecute = aSaveToFileExecute
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 352
    Top = 592
    object miCopyAs: TMenuItem
      Caption = '#01102^Copy As'
      object N1: TMenuItem
      end
    end
  end
  object ilCategoryActions: TImageList
    Left = 96
    Top = 504
    Bitmap = {
      494C010104000800D00010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      00007F7F7F007F7F7F007F7F7F0000FFFF0000FFFF007F7F7F007F7F7F007F7F
      7F007F7F7F0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF0000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000FF000000
      FF0000000000000000007F7F7F00000000007F7F7F00000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF0000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000FFFFFF00000000000000000000000000FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF0000000000000000000000FF00000000000000
      FF000000FF000000FF007F7F7F00000000007F7F7F0000000000000000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF0000000000FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000000000FFFF0000FFFF0000FFFF000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF000000000000FFFF0000FFFF000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF00000000000000000000FFFF0000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000FFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      000000000000000000007F7F7F00000000007F7F7F0000000000000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00000000000000000000FFFF0000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000FF000000FF00000000000000
      000000000000000000000000800000000000000080000000FF00000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000FFFFFF0000000000BFBFBF00FFFFFF0000000000FFFFFF000000000000FF
      FF000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF0000FFFF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF00FFFFFF0000000000000000000000FF000000FF000000
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00000000000000000000000000000000000000FF000000
      FF000000FF00000000007F7F7F00000000007F7F7F0000000000000000000000
      FF000000FF000000000000000000000000000000000000FFFF0000FFFF000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      00000000000000FFFF0000FFFF000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000000000000000000000000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF7EFC00FC00FFFF9001F000FC00F83F
      C003C0002000E00FE00300000000CC47E003000000008463E00300000000A073
      E0030000000031F900010000000038F98000000000003C79E007000000003C39
      E00F0001E0003C19E00F0003F8009C0BE0270007F0008C43C073001FE001C467
      9E79007FC403E00F7EFE01FFEC07F83F00000000000000000000000000000000
      000000000000}
  end
  object pmCategories: TPopupMenu
    Images = ilCategoryActions
    Left = 40
    Top = 504
    object miAddCategory: TMenuItem
      Caption = '#00881^eAdd category'
      ImageIndex = 0
      OnClick = miAddCategoryClick
    end
    object miUncheckAllCategories: TMenuItem
      Caption = '#00880^eUncheck all categories'
      ImageIndex = 1
      OnClick = miUncheckAllCategoriesClick
    end
    object miEditCategory: TMenuItem
      Caption = '#00030^eEdit category'
      ImageIndex = 2
      OnClick = miEditCategoryClick
    end
    object miDeleteCategory: TMenuItem
      Caption = '#00031^eDelete category'
      ImageIndex = 3
      OnClick = miDeleteCategoryClick
    end
  end
end
