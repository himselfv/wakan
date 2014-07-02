object fKanji: TfKanji
  Left = 591
  Top = 473
  BorderStyle = bsSizeToolWin
  Caption = '#00117^eCharacter list'
  ClientHeight = 348
  ClientWidth = 630
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splDockCompounds: TSplitter
    Left = 0
    Top = 345
    Width = 630
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Beveled = True
    Visible = False
    ExplicitTop = 330
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 630
    Height = 345
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object BlankPanel1: TBlankPanel
      Left = 0
      Top = 27
      Width = 630
      Height = 291
      Align = alClient
      TextLeft = 8
      TextTop = 8
      Text = '#00118^eNo characters were found.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object DrawGrid1: TDrawGrid
      AlignWithMargins = True
      Left = 3
      Top = 30
      Width = 624
      Height = 285
      Align = alClient
      BorderStyle = bsNone
      ColCount = 10
      DefaultColWidth = 59
      DefaultRowHeight = 59
      DefaultDrawing = False
      FixedCols = 0
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goThumbTracking]
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
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 624
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object RxLabel15: TLabel
        Left = 0
        Top = 0
        Width = 75
        Height = 16
        Align = alClient
        Caption = 'RxLabel15'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object btnSearchSort: TSpeedButton
        Left = 487
        Top = 0
        Width = 137
        Height = 21
        Hint = '#00119^eSearch & change sort order'
        Action = aSearch
        Align = alRight
        AllowAllUp = True
        Caption = '#00120^eSearch && sort'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        ExplicitLeft = 8
      end
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 321
      Width = 624
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
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
        Left = 495
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
        Left = 366
        Top = 0
        Width = 129
        Height = 21
        Hint = '#00127^ePrint Kanji cards (for memorizing) (Ctrl-F6)'
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
        OnClick = btnPrintCardsClick
      end
    end
  end
  object pnlDockCompounds: TPanel
    Left = 0
    Top = 348
    Width = 630
    Height = 0
    Align = alBottom
    BevelOuter = bvNone
    UseDockManager = False
    DockSite = True
    TabOrder = 1
  end
  object pnlDockSearch: TPanel
    Left = 0
    Top = 0
    Width = 630
    Height = 0
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    UseDockManager = False
    DockSite = True
    TabOrder = 2
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text file (*.txt)|*.txt'
    Left = 152
    Top = 40
  end
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 222
    Top = 40
  end
  object Actions: TActionList
    Left = 152
    Top = 104
    object aSearch: TCheckAction
      AutoCheck = True
      Caption = '#00230^e&Search'
      OnExecute = aSearchExecute
      OnChecked = aSearchChecked
    end
    object aAll: TAction
      Caption = '#00278^eDisplay all'
      ShortCut = 16469
      OnExecute = aAllExecute
    end
    object aClipboard: TAction
      Caption = '#00281^eIn clipboard only'
      OnExecute = aClipboardExecute
    end
    object aLearned: TAction
      Caption = '#00279^eLearned only'
      OnExecute = aLearnedExecute
    end
    object aCommon: TAction
      Caption = '#00280^eCommon only'
      OnExecute = aCommonExecute
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
      ShortCut = 16501
      OnExecute = aPrintExecute
    end
    object aSaveToFile: TAction
      Caption = '#00944^eSave characters to file...'
      OnExecute = aSaveToFileExecute
    end
  end
end
