object fKanji: TfKanji
  Left = 591
  Top = 473
  Width = 640
  Height = 376
  BorderStyle = bsSizeToolWin
  Caption = '#00117^cSeznam znakù^eCharacter list'
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
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 340
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object RxLabel15: TRxLabel
      Left = 8
      Top = 8
      Width = 77
      Height = 16
      Caption = 'RxLabel15'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Shape6: TShape
      Left = 7
      Top = 32
      Width = 618
      Height = 282
      Anchors = [akLeft, akTop, akRight, akBottom]
    end
    object Label18: TLabel
      Left = 413
      Top = 173
      Width = 5
      Height = 13
      Alignment = taRightJustify
      Anchors = []
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Label24: TLabel
      Left = 15
      Top = 40
      Width = 520
      Height = 19
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = '#00118^eNo characters were found.^cŽádné znaky nebyly nalezeny.'
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
    object SpeedButton5: TSpeedButton
      Left = 8
      Top = 321
      Width = 113
      Height = 17
      Hint = 
        '#00119^eSearch & change sort order^cHledat a zmìnit poøadí tøídì' +
        'ní'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 2
      Caption = '#00120^eSearch && sort^cHledat && tøídit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton5Click
    end
    object SpeedButton1: TSpeedButton
      Left = 216
      Top = 321
      Width = 65
      Height = 17
      Hint = '#00121^eSort (Ctrl-Alt-O)^cTøídit (Ctrl-Alt-O)'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 3
      Caption = '#00122^eSort^cTøídit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Visible = False
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 559
      Top = 321
      Width = 65
      Height = 17
      Hint = '#00123^eDetails (Ctrl-D)^cDetaily (Ctrl-D)'
      AllowAllUp = True
      Anchors = [akRight, akBottom]
      GroupIndex = 4
      Caption = '#00124^eDetails^cDetaily'
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
    object SpeedButton3: TSpeedButton
      Left = 128
      Top = 321
      Width = 81
      Height = 17
      Hint = '#00125^eCompounds^cSloženiny'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 5
      Caption = '#00125^eCompounds^cSloženiny'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton3Click
    end
    object SpeedButton4: TSpeedButton
      Left = 461
      Top = 7
      Width = 25
      Height = 17
      Hint = '#00126^eShow stroke order^cZobrazit poøadí tahù'
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 6
      Caption = 'SO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton4Click
    end
    object DrawGrid1: TDrawGrid
      Left = 8
      Top = 33
      Width = 616
      Height = 280
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      ColCount = 10
      DefaultColWidth = 59
      DefaultRowHeight = 59
      DefaultDrawing = False
      FixedCols = 0
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goThumbTracking]
      TabOrder = 0
      Visible = False
      OnDblClick = DrawGrid1DblClick
      OnDrawCell = DrawGrid1DrawCell
      OnKeyPress = DrawGrid1KeyPress
      OnMouseDown = DrawGrid1MouseDown
      OnMouseMove = DrawGrid1MouseMove
      OnSelectCell = DrawGrid1SelectCell
    end
    object Button1: TButton
      Left = 495
      Top = 4
      Width = 128
      Height = 21
      Hint = 
        '#00127^ePrint Kanji cards (for memorizing) (Ctrl-F6)^cTisknout k' +
        'arty znakù (pro opakování) (Ctrl-F6)'
      Anchors = [akTop, akRight]
      Caption = '#00128^ePrint cards^cTisk karet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      TabStop = False
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 344
      Top = 8
      Width = 115
      Height = 17
      Caption = 'ReadingChart'
      TabOrder = 2
      Visible = False
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 340
    Width = 624
    Height = 0
    Align = alBottom
    BevelOuter = bvNone
    UseDockManager = False
    DockSite = True
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 0
    Align = alTop
    BevelOuter = bvNone
    UseDockManager = False
    DockSite = True
    TabOrder = 2
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 352
    Top = 65528
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text file (*.txt)|*.txt'
    Left = 288
    Top = 8
  end
end
