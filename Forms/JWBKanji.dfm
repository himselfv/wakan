object fKanji: TfKanji
  Left = 591
  Top = 473
  BorderStyle = bsSizeToolWin
  Caption = '#00117^cSeznam znak'#1097'^eCharacter list'
  ClientHeight = 342
  ClientWidth = 624
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
    Height = 342
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      624
      342)
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
      Height = 284
      Anchors = [akLeft, akTop, akRight, akBottom]
      ExplicitHeight = 282
    end
    object Label18: TLabel
      Left = 413
      Top = 174
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
      ExplicitTop = 173
    end
    object Label24: TLabel
      Left = 15
      Top = 40
      Width = 525
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = '#00118^eNo characters were found.^c'#1035#1073'dn'#1081' znaky nebyly nalezeny.'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = True
      ExplicitHeight = 19
    end
    object SpeedButton5: TSpeedButton
      Left = 8
      Top = 323
      Width = 113
      Height = 17
      Hint = 
        '#00119^eSearch & change sort order^cHledat a zm'#1084'nit po'#1096'ad'#1085' t'#1096#1085'd'#1084 +
        'n'#1085
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 2
      Caption = '#00120^eSearch && sort^cHledat && t'#1096#1085'dit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton5Click
      ExplicitTop = 321
    end
    object SpeedButton1: TSpeedButton
      Left = 216
      Top = 323
      Width = 65
      Height = 17
      Hint = '#00121^eSort (Ctrl-Alt-O)^cT'#1096#1085'dit (Ctrl-Alt-O)'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 3
      Caption = '#00122^eSort^cT'#1096#1085'dit'
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
      ExplicitTop = 321
    end
    object SpeedButton2: TSpeedButton
      Left = 559
      Top = 323
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
      ExplicitTop = 321
    end
    object SpeedButton3: TSpeedButton
      Left = 128
      Top = 323
      Width = 81
      Height = 17
      Hint = '#00125^eCompounds^cSlo'#1115'eniny'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 5
      Caption = '#00125^eCompounds^cSlo'#1115'eniny'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton3Click
      ExplicitTop = 321
    end
    object SpeedButton4: TSpeedButton
      Left = 461
      Top = 7
      Width = 25
      Height = 17
      Hint = '#00126^eShow stroke order^cZobrazit po'#1096'ad'#1085' tah'#1097
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
      Height = 282
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
        'arty znak'#1097' (pro opakov'#1073'n'#1085') (Ctrl-F6)'
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
    Top = 342
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
