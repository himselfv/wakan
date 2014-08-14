object fKanji: TfKanji
  Left = 591
  Top = 473
  BorderStyle = bsSizeToolWin
  Caption = '#00117^eCharacter list'
  ClientHeight = 660
  ClientWidth = 680
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
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 78
    Width = 680
    Height = 582
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object splDockCompounds: TSplitter
      Left = 0
      Top = 580
      Width = 680
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
      Top = 54
      Width = 680
      Height = 499
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
    end
    object DrawGrid1: TDrawGrid
      AlignWithMargins = True
      Left = 3
      Top = 57
      Width = 674
      Height = 493
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
      TabOrder = 1
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
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 30
      Width = 674
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblFoundChars: TLabel
        Left = 0
        Top = 0
        Width = 447
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
        ExplicitWidth = 132
        ExplicitHeight = 16
      end
      object Label2: TLabel
        AlignWithMargins = True
        Left = 447
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
        ExplicitHeight = 13
      end
      object rgSortBy: TComboBox
        Left = 528
        Top = 0
        Width = 146
        Height = 21
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
      end
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 556
      Width = 674
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
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
        Left = 545
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
        Left = 416
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
      end
    end
    object pnlDockCompounds: TPanel
      Left = 0
      Top = 580
      Width = 680
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      UseDockManager = False
      DockSite = True
      TabOrder = 4
    end
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 680
      Height = 27
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkSoft
      BevelOuter = bvNone
      UseDockManager = False
      DockSite = True
      Padding.Bottom = 3
      ParentBackground = False
      TabOrder = 5
      object sbInClipboard: TSpeedButton
        AlignWithMargins = True
        Left = 368
        Top = 0
        Width = 27
        Height = 22
        Hint = '#00188^eDisplay only characters in clipboard'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        AllowAllUp = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFDFDFDB2B2B2A4A4A4A3A3A3A1A1A19F9F9F9E9E9E9D9D
          9D9B9B9B9A9A9AB4B4B4BED1E14980AB206398206398206398246395587388F7
          F7F7F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F3F3F39F9F9F5588B062A5D7
          65A8DA64A6D962A4D8629FD1758EA4EFEFEFE7E7E7E7E7E7E7E7E7E7E7E7E6E6
          E6E6E6E6ECECECA0A0A020639868ABDC488ECF468BCE4387CD4484C66885A1F0
          F0F0B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B3B3B3EDEDEDA2A2A220639869AEDC
          4A93D1488FD0468BCE4788C76C88A3F0F0F0E8E8E8E8E8E8E7E7E7E7E7E7E7E7
          E7E7E7E7EDEDEDA3A3A32063986BB1DE4D97D34B93D2488FD04A8CC96F8BA5F1
          F1F1B6B6B6B5B5B5B5B5B5B4B4B4B4B4B4B4B4B4EDEDEDA5A5A52063986DB3DF
          509CD54E98D34B94D14C91CB708EA7F1F1F1E9E9E9E9E9E9E8E8E8E8E8E8E8E8
          E8E7E7E7EDEDEDA7A7A720639870B5E0529FD7509CD64E98D44F95CD7391AAF1
          F1F1B7B7B7B6B6B6B6B6B6B6B6B6B5B5B5B5B5B5EEEEEEA9A9A920639873B7E1
          57A3D753A0D7509DD55299CF7594ACF8F8F8F2F2F2F2F2F2F2F2F2F2F2F2F2F2
          F2F1F1F1F4F4F4ABABAB20639876B9E25CA7D958A4D853A0D7539ED5618BA964
          88A16487A16386A069879F4A6881AEAEAEADADADABABABC2C2C22063987ABBE3
          61AADB5AA5D953A0D7529FD7529FD7529FD7529FD7529FD762A3D8206398FFFF
          FFFFFFFFFFFFFFFFFFFF2063987CBDE465AEDD62ABDC5EA8DA5CA7D95CA7D95C
          A7D95CA7D9529FD762A3D8206398FFFFFFFFFFFFFFFFFFFFFFFF2063987FBFE4
          69B2DE4A9BDA4497DC4396DC4296DC4295DC4195DB519ED66CB2DE206398FFFF
          FFFFFFFFFFFFFFFFFFFF3B75A471B3DB7EBFE44E9DDFB5EEFD75D4F075D4F0B5
          EEFD4B9BDE6EB4E070B4DF2A6A9CFFFFFFFFFFFFFFFFFFFFFFFFD3E0EA6392B7
          2063983775A4B6EFFE80DBF380DBF3B6EFFE2E6EA12063986E9ABCB6CCDDFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDAE5EE20639820639820639820639820
          63982D6C9EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentFont = False
        OnClick = sbInClipboardClick
        ExplicitTop = 5
      end
      object sbOnlyCommon: TSpeedButton
        AlignWithMargins = True
        Left = 337
        Top = 0
        Width = 28
        Height = 22
        Hint = '#00186^eDisplay only common characters'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        AllowAllUp = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFF898989474747FFFFFFFFFFFFFFFFFF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8989894747
          47FFFFFFFFFFFFFFFFFF000000696969000000696969FFFFFFFFFFFFA7A7A711
          11110000007A7A7AFFFFFF898989343434343434111111989898000000B5B5B5
          ECECEC232323989898DDDDDD000000B5B5B5ECECEC2323239898988989891111
          11DDDDDDD0D0D0000000232323FFFFFFFFFFFF898989474747C3C3C3232323FF
          FFFFFFFFFF7A7A7A575757898989696969FFFFFFFFFFFF474747232323FFFFFF
          FFFFFF898989474747C3C3C3232323FFFFFFFFFFFF8989895757578989896969
          69FFFFFFFFFFFF474747000000A7A7A7ECECEC232323989898DDDDDD000000B5
          B5B5ECECEC232323989898898989111111D0D0D0D0D0D0000000000000696969
          000000474747ECECECFFFFFFA7A7A71111110000007A7A7AFFFFFF8989893434
          343434340000007A7A7AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentFont = False
        OnClick = sbOnlyCommonClick
        ExplicitTop = -4
      end
      object edtLookup: TEdit
        Left = 0
        Top = 0
        Width = 58
        Height = 22
        Align = alClient
        TabOrder = 0
        OnChange = SearchFilterChanged
        ExplicitHeight = 21
      end
      object btnStrokes: TButton
        AlignWithMargins = True
        Left = 398
        Top = 0
        Width = 92
        Height = 22
        Hint = 
          '#01133^Search by reading, writing or meaning, depending on what ' +
          'you type'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Caption = 'Strokes: 15-17'
        Style = bsSplitButton
        TabOrder = 2
        OnDropDownClick = btnStrokesDropDownClick
      end
      object btnRadicals: TButton
        AlignWithMargins = True
        Left = 493
        Top = 0
        Width = 92
        Height = 22
        Hint = 
          '#01133^Search by reading, writing or meaning, depending on what ' +
          'you type'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Caption = 'Radicals: A B'
        Style = bsSplitButton
        TabOrder = 3
        OnDropDownClick = pbRadicalsClick
      end
      object btnGroups: TButton
        AlignWithMargins = True
        Left = 588
        Top = 0
        Width = 92
        Height = 22
        Hint = 
          '#01133^Search by reading, writing or meaning, depending on what ' +
          'you type'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Caption = 'Groups: A B'
        Style = bsSplitButton
        TabOrder = 4
        TabStop = False
        OnDropDownClick = btnGroupsDropDownClick
      end
      object btnLookupMode: TButton
        AlignWithMargins = True
        Left = 199
        Top = 0
        Width = 135
        Height = 22
        Hint = 
          '#01133^Search by reading, writing or meaning, depending on what ' +
          'you type'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Caption = '#01132^Any matches'
        DropDownMenu = pmLookupMode
        Style = bsSplitButton
        TabOrder = 1
        TabStop = False
      end
      object cbOtherType: TComboBox
        AlignWithMargins = True
        Left = 61
        Top = 0
        Width = 135
        Height = 21
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = '#01132^Any matches'
        OnChange = SearchFilterChanged
        Items.Strings = (
          '#01132^Any matches'
          'Characters'
          'Definition'
          'On'
          'Kun'
          'PinYin'
          'SKIP'
          '-')
      end
    end
    object PopupPanel1: TPopupPanel
      Left = 238
      Top = 31
      Width = 92
      Height = 29
      TabOrder = 6
      Visible = False
      object RangeSpinEdit1: TRangeSpinEdit
        Left = 3
        Top = 3
        Width = 85
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 0
        LowValue = 0
        HighValue = 0
        OnChange = edtStrokeCountChange
      end
    end
    object Panel4: TPanel
      Left = 11
      Top = 80
      Width = 366
      Height = 177
      Caption = 'Panel4'
      TabOrder = 7
      object SpeedButton2: TSpeedButton
        Left = 240
        Top = 32
        Width = 23
        Height = 22
      end
      object WinSpeedButton1: TWinSpeedButton
        Left = 3
        Top = 26
        Width = 170
        Height = 37
        Hint = '#00188^eDisplay only characters in clipboard'
        AllowAllUp = True
        Caption = 'Test'
        DropButtonSettings.Size = 32
        DropButtonSettings.ImageIndex = 1
        DropDownMenu = PopupMenu
        ImageIndex = 1
        Images = ilCategoryActions
        PressedImageIndex = 2
        SelectedImageIndex = 3
        Style = bsSplitButton
        TabOrder = 0
        Transparent = False
        OnClick = aInClipboardExecute
      end
      object Button1: TButton
        Left = 32
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 1
      end
      object Button2: TButton
        Left = 113
        Top = 88
        Width = 75
        Height = 73
        Caption = 'Button1'
        ImageIndex = 0
        Images = ilCategoryActions
        TabOrder = 2
      end
      object Button3: TButton
        Left = 194
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Button1'
        Default = True
        TabOrder = 3
      end
      object BitBtn1: TBitBtn
        Left = 275
        Top = 104
        Width = 75
        Height = 25
        Caption = 'BitBtn1'
        Default = True
        TabOrder = 4
      end
      object BitBtn2: TBitBtn
        Left = 275
        Top = 135
        Width = 75
        Height = 25
        Caption = 'BitBtn1'
        TabOrder = 5
      end
      object Button4: TButton
        AlignWithMargins = True
        Left = 175
        Top = 0
        Width = 175
        Height = 85
        Hint = 
          '#01133^Search by reading, writing or meaning, depending on what ' +
          'you type'
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Caption = '#01132^Any matches'
        DropDownMenu = PopupMenu
        Style = bsSplitButton
        TabOrder = 6
        TabStop = False
      end
    end
  end
  object pnlDockSearch: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 674
    Height = 72
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
    DesignSize = (
      674
      70)
    object sbStrokeCount: TSpeedButton
      Left = 0
      Top = 0
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
    object sbRadicals: TSpeedButton
      Left = 0
      Top = 22
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
    object sbListRadicals: TSpeedButton
      Left = 605
      Top = 23
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
      ExplicitLeft = 611
    end
    object sbJouyou: TSpeedButton
      Left = 0
      Top = 46
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
    object edtStrokeCount: TRangeSpinEdit
      Left = 56
      Top = 0
      Width = 612
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      MaxValue = 100
      MinValue = 0
      TabOrder = 0
      LowValue = 0
      HighValue = 0
      OnChange = edtStrokeCountChange
    end
    object pbRadicals: TWakanPaintbox
      Left = 57
      Top = 23
      Width = 548
      Height = 22
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      DoubleBuffered = True
      OnPaint = pbRadicalsPaint
      OnClick = pbRadicalsClick
    end
    object edtJouyou: TRangeSpinEdit
      Left = 56
      Top = 46
      Width = 612
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      LowValue = 0
      HighValue = 0
      OnChange = edtJouyouChange
    end
  end
  object pnlGroups: TPopupPanel
    Left = 489
    Top = 300
    Width = 185
    Height = 238
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    OnExit = pnlGroupsExit
    object Panel9: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 181
      Height = 22
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object SpeedButton1: TSpeedButton
        Left = 0
        Top = 0
        Width = 57
        Height = 22
        Hint = '#00194^eFilter by Jouyou grade (Japanese school grade)'
        Align = alLeft
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
        ExplicitTop = 4
      end
      object RangeSpinEdit3: TRangeSpinEdit
        AlignWithMargins = True
        Left = 57
        Top = 0
        Width = 124
        Height = 22
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        MaxValue = 100
        MinValue = 0
        TabOrder = 0
        LowValue = 0
        HighValue = 0
        OnChange = edtJouyouChange
      end
    end
    object Panel11: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 25
      Width = 181
      Height = 22
      Margins.Left = 0
      Margins.Right = 0
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SpeedButton5: TSpeedButton
        Left = 0
        Top = 0
        Width = 57
        Height = 22
        Hint = '#00194^eFilter by Jouyou grade (Japanese school grade)'
        Align = alLeft
        AllowAllUp = True
        GroupIndex = 18
        Caption = 'JLPT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        OnClick = SearchFilterChanged
        ExplicitLeft = -3
        ExplicitTop = 3
      end
      object RangeSpinEdit2: TRangeSpinEdit
        AlignWithMargins = True
        Left = 57
        Top = 0
        Width = 124
        Height = 22
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        MaxValue = 100
        MinValue = 0
        TabOrder = 0
        LowValue = 0
        HighValue = 0
        OnChange = edtJouyouChange
      end
    end
    object lbCategories: TCheckListBox
      AlignWithMargins = True
      Left = 0
      Top = 77
      Width = 181
      Height = 157
      Margins.Left = 0
      Margins.Right = 0
      Margins.Bottom = 0
      OnClickCheck = lbCategoriesClickCheck
      Align = alClient
      ItemHeight = 13
      PopupMenu = pmCategories
      TabOrder = 2
      OnClick = lbCategoriesClick
      OnDblClick = lbCategoriesDblClick
      OnDrawItem = lbCategoriesDrawItem
    end
    object cbOrAnd: TComboBox
      AlignWithMargins = True
      Left = 0
      Top = 53
      Width = 181
      Height = 21
      Margins.Left = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 3
      Text = 'In all of:'
      Items.Strings = (
        'In any of:'
        'In all of:'
        'Not in any of:')
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text file (*.txt)|*.txt'
    Left = 392
    Top = 472
  end
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 326
    Top = 472
  end
  object Actions: TActionList
    Left = 272
    Top = 472
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
    object aInClipboard: TAction
      AutoCheck = True
      Caption = '#00281^eIn clipboard only'
      Hint = '#00188^eDisplay only characters in clipboard'
      OnExecute = aInClipboardExecute
    end
    object aOnlyCommon: TAction
      AutoCheck = True
      Caption = '#00280^eCommon only'
      Hint = '#00186^eDisplay only common characters'
      OnExecute = aOnlyCommonExecute
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
      GroupIndex = 1
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
    Images = PopupImages
    OnPopup = PopupMenuPopup
    Left = 272
    Top = 528
    object miCopy: TMenuItem
      Caption = '#00266^Copy'
      OnClick = miCopyClick
    end
    object miCopyAs: TMenuItem
      Caption = '#01102^Copy As'
      object N1: TMenuItem
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miCharDetails: TMenuItem
      Caption = '#00124^Details'
      OnClick = miCharDetailsClick
    end
    object miCharWords: TMenuItem
      Caption = '#01118^Words'
      Hint = '#01119^Show words with this kanji in the dictionary'
      OnClick = miCharWordsClick
    end
    object miCategories: TMenuItem
      Caption = '#00634^Categories'
      object TMenuItem
      end
    end
    object miBeforeLookupIn: TMenuItem
      Caption = '-'
    end
    object miLookUpIn: TMenuItem
      Caption = '#01124^Look Up In'
      object TMenuItem
      end
    end
    object miAfterLookupIn: TMenuItem
      Caption = '-'
    end
  end
  object ilCategoryActions: TImageList
    Left = 96
    Top = 504
    Bitmap = {
      494C010104001401940110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
  object PopupImages: TImageList
    Left = 336
    Top = 528
  end
  object pmLookupMode: TPopupMenu
    Left = 216
    Top = 320
    object miLookupAuto: TMenuItem
      Tag = 1
      AutoCheck = True
      Caption = '#01132^Auto/all'
      Checked = True
      GroupIndex = 1
      Hint = 
        '#01133^Search by reading, writing or meaning, depending on what ' +
        'you type'
      RadioItem = True
      ShortCut = 113
      OnClick = miLookupSKIPClick
    end
    object N3: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object miLookupCharacters: TMenuItem
      AutoCheck = True
      Caption = 'Characters'
      GroupIndex = 1
      OnClick = miLookupSKIPClick
    end
    object miLookupDefinition: TMenuItem
      AutoCheck = True
      Caption = 'Definition'
      GroupIndex = 1
      OnClick = miLookupSKIPClick
    end
    object miLookupOn: TMenuItem
      AutoCheck = True
      Caption = 'On'
      GroupIndex = 1
      OnClick = miLookupSKIPClick
    end
    object miLookupKun: TMenuItem
      AutoCheck = True
      Caption = 'Kun'
      GroupIndex = 1
      OnClick = miLookupSKIPClick
    end
    object miLookupPinYin: TMenuItem
      AutoCheck = True
      Caption = 'PinYin'
      GroupIndex = 1
      OnClick = miLookupSKIPClick
    end
    object miLookupSKIP: TMenuItem
      AutoCheck = True
      Caption = 'SKIP'
      GroupIndex = 1
      OnClick = miLookupSKIPClick
    end
    object N4: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
  end
end
