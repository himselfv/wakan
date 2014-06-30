object fVocab: TfVocab
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
    Visible = False
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
      Visible = False
      ExplicitLeft = 770
      ExplicitTop = 0
      ExplicitWidth = 455
    end
    object TopPanel: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 0
      Width = 767
      Height = 22
      Margins.Top = 0
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object RxLabel1: TLabel
        Left = 0
        Top = 0
        Width = 183
        Height = 16
        Align = alLeft
        Caption = '#00828^eVocabulary list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object btnPrintVocabList: TButton
        Left = 621
        Top = 0
        Width = 75
        Height = 22
        Hint = '#00835^ePrint vocabulary list (Ctrl-F8)'
        Align = alRight
        Caption = '#00382^ePrint'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = btnPrintVocabListClick
      end
      object btnRecommendKanji: TButton
        Left = 391
        Top = 0
        Width = 119
        Height = 22
        Hint = 
          '#00836^eRecommend characters for learning based on current vocab' +
          'ulary'
        Align = alRight
        Caption = '#00837^eRecommend kanji'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = btnRecommendKanjiClick
      end
      object btnLearningList: TButton
        Left = 510
        Top = 0
        Width = 111
        Height = 22
        Hint = '#00838^eAutomatically generate learning list'
        Align = alRight
        Caption = '#00839^eLearning list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = btnLearningListClick
      end
      object btnAddWord: TButton
        Left = 696
        Top = 0
        Width = 71
        Height = 22
        Hint = '#00936^eAdd to vocabulary'
        Align = alRight
        Caption = '#00534^eAdd...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = btnAddWordClick
      end
    end
    object pnlDockDetails: TPanel
      Left = 0
      Top = 455
      Width = 773
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      FullRepaint = False
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
    object BlankPanel: TBlankPanel
      Left = 0
      Top = 25
      Width = 773
      Height = 399
      Align = alClient
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
      AlignWithMargins = True
      Left = 1
      Top = 26
      Width = 771
      Height = 397
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BorderStyle = bsNone
      ColCount = 4
      DefaultRowHeight = 16
      DefaultDrawing = False
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
      PopupMenu = PopupMenu1
      TabOrder = 4
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
    object BottomPanel: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 427
      Width = 767
      Height = 22
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      object btnExamples: TSpeedButton
        Left = 0
        Top = 0
        Width = 138
        Height = 22
        Align = alLeft
        AllowAllUp = True
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
        OnClick = btnExamplesClick
        ExplicitLeft = 8
        ExplicitTop = 8
        ExplicitHeight = 17
      end
      object btnListSettings: TSpeedButton
        Left = 629
        Top = 0
        Width = 138
        Height = 22
        Hint = '#00830^eList settings'
        Align = alRight
        AllowAllUp = True
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
        OnClick = btnListSettingsClick
        ExplicitLeft = 152
        ExplicitTop = 8
        ExplicitHeight = 17
      end
      object btnWordDetails: TSpeedButton
        Left = 138
        Top = 0
        Width = 159
        Height = 22
        Hint = '#00832^eWord details'
        Align = alLeft
        AllowAllUp = True
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
        OnClick = btnWordDetailsClick
        ExplicitLeft = 296
        ExplicitTop = 8
        ExplicitHeight = 17
      end
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
