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
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 776
    Top = 0
    Width = 0
    Height = 455
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 776
    Height = 455
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 776
      Height = 455
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        776
        455)
      object Shape7: TShape
        Left = 10
        Top = 26
        Width = 756
        Height = 402
        Anchors = [akLeft, akTop, akRight, akBottom]
        Brush.Color = clWindow
      end
      object RxLabel1: TRxLabel
        Left = 8
        Top = 5
        Width = 185
        Height = 16
        Caption = '#00828^eVocabulary list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label24: TLabel
        Left = 24
        Top = 42
        Width = 240
        Height = 19
        Caption = '#00155^eNo words were found.'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object SpeedButton1: TSpeedButton
        Left = 10
        Top = 432
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
      end
      object SpeedButton2: TSpeedButton
        Left = 154
        Top = 432
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
      end
      object SpeedButton4: TSpeedButton
        Left = 298
        Top = 432
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
      end
      object StringGrid1: TWakanGrid
        Left = 11
        Top = 27
        Width = 754
        Height = 400
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        ColCount = 4
        DefaultRowHeight = 16
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
        PopupMenu = PopupMenu1
        TabOrder = 0
        OnClick = StringGrid1Click
        OnDrawCell = StringGrid1DrawCell
        OnKeyPress = StringGrid1KeyPress
        OnMouseDown = StringGrid1MouseDown
        OnMouseMove = StringGrid1MouseMove
        OnMouseUp = StringGrid1MouseUp
        OnSelectCell = StringGrid1SelectCell
        OnControlResize = StringGrid1ControlResize
        ColWidths = (
          110
          138
          306
          159)
      end
      object Button9: TButton
        Left = 546
        Top = 5
        Width = 71
        Height = 21
        Hint = '#00833^eExport vocabulary'
        Anchors = [akTop, akRight]
        Caption = '#00934^eExport'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 618
        Top = 5
        Width = 72
        Height = 21
        Hint = '#00834^eImport vocabulary'
        Anchors = [akTop, akRight]
        Caption = '#00935^eImport'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = Button10Click
      end
      object Button15: TButton
        Left = 691
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
        TabOrder = 3
        OnClick = Button15Click
      end
      object Button18: TButton
        Left = 426
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
        TabOrder = 4
        OnClick = Button18Click
      end
      object Button19: TButton
        Left = 315
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
        TabOrder = 5
        OnClick = Button19Click
      end
      object Button2: TButton
        Left = 244
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
        TabOrder = 6
        OnClick = Button2Click
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 455
      Width = 776
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel5: TPanel
      Left = 0
      Top = 455
      Width = 776
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
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
