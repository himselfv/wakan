object fUserDetails: TfUserDetails
  Left = 237
  Top = 305
  BorderStyle = bsNone
  Caption = '#00692^eVocabulary word details'
  ClientHeight = 219
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  DesignSize = (
    752
    219)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 752
    Height = 219
    Align = alClient
    Shape = bsFrame
  end
  object Label5: TLabel
    Left = 8
    Top = 9
    Width = 138
    Height = 13
    Caption = '#00060^ePhonetic:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Shape2: TShape
    Left = 8
    Top = 25
    Width = 281
    Height = 29
    Brush.Color = clWindow
  end
  object PaintBox1: TPaintBox
    Left = 9
    Top = 26
    Width = 279
    Height = 27
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox1Paint
  end
  object Shape5: TShape
    Left = 298
    Top = 25
    Width = 248
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object Label6: TLabel
    Left = 298
    Top = 9
    Width = 127
    Height = 13
    Caption = '#00061^eWritten:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object PaintBox6: TPaintBox
    Left = 299
    Top = 26
    Width = 246
    Height = 27
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnMouseMove = PaintBox6MouseMove
    OnPaint = PaintBox6Paint
  end
  object RxLabel3: TRxLabel
    Left = 554
    Top = 25
    Width = 75
    Height = 18
    Caption = 'Learned'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Anchors = [akTop, akRight]
    ParentFont = False
    Transparent = True
  end
  object Label8: TLabel
    Left = 554
    Top = 9
    Width = 119
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '#00693^eStatus:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label9: TLabel
    Left = 8
    Top = 65
    Width = 146
    Height = 13
    Caption = '#00691^eMeaning:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label11: TLabel
    Left = 8
    Top = 113
    Width = 157
    Height = 13
    Caption = '#00694^eCreated:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label12: TLabel
    Left = 336
    Top = 113
    Width = 200
    Height = 13
    Caption = '#00695^eLearning list:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label13: TLabel
    Left = 112
    Top = 113
    Width = 150
    Height = 13
    Caption = '#00696^eLearned:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label14: TLabel
    Left = 200
    Top = 113
    Width = 190
    Height = 13
    Caption = '#00697^eMastered:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label15: TLabel
    Left = 8
    Top = 137
    Width = 48
    Height = 16
    Caption = 'Label15'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label16: TLabel
    Left = 112
    Top = 137
    Width = 48
    Height = 16
    Caption = 'Label15'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label17: TLabel
    Left = 200
    Top = 137
    Width = 48
    Height = 16
    Caption = 'Label15'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label18: TLabel
    Left = 336
    Top = 137
    Width = 48
    Height = 16
    Caption = 'Label15'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object SpeedButton4: TSpeedButton
    Left = 697
    Top = 26
    Width = 23
    Height = 23
    Hint = '#00698^eMove up in the category'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
      3333333333777F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
      3333333777737777F333333099999990333333373F3333373333333309999903
      333333337F33337F33333333099999033333333373F333733333333330999033
      3333333337F337F3333333333099903333333333373F37333333333333090333
      33333333337F7F33333333333309033333333333337373333333333333303333
      333333333337F333333333333330333333333333333733333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton4Click
  end
  object SpeedButton5: TSpeedButton
    Left = 721
    Top = 26
    Width = 23
    Height = 23
    Hint = '#00699^eMove down in the category'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
      333333333337F33333333333333033333333333333373F333333333333090333
      33333333337F7F33333333333309033333333333337373F33333333330999033
      3333333337F337F33333333330999033333333333733373F3333333309999903
      333333337F33337F33333333099999033333333373333373F333333099999990
      33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333300033333333333337773333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton5Click
  end
  object Button2: TButton
    Left = 669
    Top = 7
    Width = 75
    Height = 17
    Hint = '#00700^eDelete this word'
    Anchors = [akTop, akRight]
    Caption = '#00701^eDelete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button2Click
  end
  object Edit4: TEdit
    Left = 8
    Top = 81
    Width = 393
    Height = 21
    TabOrder = 1
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object Button3: TButton
    Left = 401
    Top = 80
    Width = 75
    Height = 23
    Caption = '#00553^eChange'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button3Click
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 161
    Width = 473
    Height = 50
    Caption = '#00702^eSet as'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    object Button5: TButton
      Left = 8
      Top = 16
      Width = 113
      Height = 25
      Caption = '#00638^eProblematic'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 128
      Top = 16
      Width = 106
      Height = 25
      Caption = '#00639^eUnlearned'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 240
      Top = 16
      Width = 107
      Height = 25
      Caption = '#00640^eLearned'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 352
      Top = 16
      Width = 113
      Height = 25
      Caption = '#00703^eMastered'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = Button8Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 488
    Top = 65
    Width = 256
    Height = 146
    Anchors = [akLeft, akTop, akRight]
    Caption = '#00634^eCategories'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 4
    DesignSize = (
      256
      146)
    object ComboBox2: TComboBox
      Left = 9
      Top = 20
      Width = 182
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox2Change
    end
    object Button4: TButton
      Left = 191
      Top = 20
      Width = 57
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = '#00078^eAdd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = Button4Click
    end
    object ListBox2: TListBox
      Left = 8
      Top = 48
      Width = 240
      Height = 75
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 2
    end
    object Button13: TButton
      Left = 191
      Top = 122
      Width = 57
      Height = 18
      Anchors = [akRight, akBottom]
      Caption = '#00079^eRemove'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = Button13Click
    end
  end
end
