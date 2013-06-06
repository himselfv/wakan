object fWords: TfWords
  Left = 374
  Top = 508
  BorderStyle = bsSizeToolWin
  Caption = '#00215^eVocabulary'
  ClientHeight = 455
  ClientWidth = 776
  Color = clBtnFace
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splDockFilters: TSplitter
    Left = 773
    Top = 0
    Height = 455
    Align = alRight
    AutoSnap = False
    ExplicitLeft = 768
    ExplicitTop = -6
  end
  object pnlDockFilters: TPanel
    Left = 776
    Top = 0
    Width = 0
    Height = 455
    Align = alRight
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 773
    Height = 455
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object splDockDetails: TSplitter
      Left = 0
      Top = 452
      Width = 773
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      ExplicitLeft = 770
      ExplicitTop = 0
      ExplicitWidth = 455
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 773
      Height = 452
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        773
        452)
      object RxLabel1: TLabel
        Left = 8
        Top = 5
        Width = 183
        Height = 16
        Caption = '#00828^eVocabulary list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object SpeedButton1: TSpeedButton
        Left = 10
        Top = 429
        Width = 138
        Height = 17
        Hint = '#00829^eAdd word'
        AllowAllUp = True
        Anchors = [akLeft, akBottom]
        GroupIndex = 1
        Caption = '#00315^eExamples'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton1Click
        ExplicitTop = 432
      end
      object SpeedButton2: TSpeedButton
        Left = 154
        Top = 429
        Width = 138
        Height = 17
        Hint = '#00830^eList settings'
        AllowAllUp = True
        Anchors = [akLeft, akBottom]
        GroupIndex = 2
        Caption = '#00830^eList settings'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton2Click
        ExplicitTop = 432
      end
      object SpeedButton4: TSpeedButton
        Left = 298
        Top = 429
        Width = 159
        Height = 17
        Hint = '#00832^eWord details'
        AllowAllUp = True
        Anchors = [akLeft, akBottom]
        GroupIndex = 4
        Caption = '#00832^eWord details'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton4Click
        ExplicitTop = 432
      end
      object Button15: TButton
        Left = 688
        Top = 5
        Width = 75
        Height = 21
        Hint = '#00835^ePrint vocabulary list (Ctrl-F8)'
        Anchors = [akTop, akRight]
        Caption = '#00382^ePrint'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = Button15Click
      end
      object Button18: TButton
        Left = 568
        Top = 5
        Width = 119
        Height = 21
        Hint = 
          '#00836^eRecommend characters for learning based on current vocab' +
          'ulary'
        Anchors = [akTop, akRight]
        Caption = '#00837^eRecommend kanji'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = Button18Click
      end
      object Button19: TButton
        Left = 457
        Top = 5
        Width = 111
        Height = 21
        Hint = '#00838^eAutomatically generate learning list'
        Anchors = [akTop, akRight]
        Caption = '#00839^eLearning list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = Button19Click
      end
      object Button2: TButton
        Left = 386
        Top = 5
        Width = 71
        Height = 21
        Hint = '#00936^eAdd to vocabulary'
        Anchors = [akTop, akRight]
        Caption = '#00534^eAdd...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = Button2Click
      end
      object BlankPanel1: TBlankPanel
        Left = 10
        Top = 26
        Width = 753
        Height = 399
        Anchors = [akLeft, akTop, akRight, akBottom]
        TextLeft = 14
        TextTop = 16
        Text = '#00155^eNo words were found.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
      end
      object StringGrid1: TWakanGrid
        Left = 11
        Top = 27
        Width = 751
        Height = 397
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        ColCount = 4
        DefaultRowHeight = 16
        DefaultDrawing = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
        PopupMenu = PopupMenu1
        TabOrder = 5
        OnClick = StringGrid1Click
        OnDrawCell = StringGrid1DrawCell
        OnKeyPress = StringGrid1KeyPress
        OnMouseDown = StringGrid1MouseDown
        OnMouseMove = StringGrid1MouseMove
        OnMouseUp = StringGrid1MouseUp
        OnSelectCell = StringGrid1SelectCell
        OnControlWidthResize = StringGrid1ControlResize
        ColWidths = (
          110
          138
          306
          159)
      end
    end
    object pnlDockDetails: TPanel
      Left = 0
      Top = 455
      Width = 773
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      DoubleBuffered = False
      FullRepaint = False
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object pnlDockExamples: TPanel
      Left = 0
      Top = 455
      Width = 773
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 2
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.wkl'
    Filter = 
      'Word List in CSV (*.csv)|*.csv|WaKan Word List (*.wkl)|*.wkl|Any' +
      ' file (import as CSV) (*.*)|*.*'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 632
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wkl'
    Filter = 'Word List in CSV (*.csv)|*.csv|WaKan Word List (*.wkl)|*.wkl'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 664
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 32
    Top = 48
    object miResetColumns: TMenuItem
      Caption = '#01002^eReset columns'
      OnClick = miResetColumnsClick
    end
  end
end
