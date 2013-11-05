object fVocabAdd: TfVocabAdd
  Left = 328
  Top = 259
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = '#00057^eAdd into vocabulary'
  ClientHeight = 265
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnClose = FormClose
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    661
    265)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPhonetic: TLabel
    Left = 8
    Top = 14
    Width = 94
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
  object lblWritten: TLabel
    Left = 8
    Top = 71
    Width = 161
    Height = 13
    Caption = '#00690^eWritten (from clipboard):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblCategory: TLabel
    Left = 8
    Top = 220
    Width = 94
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '#00059^eCategory:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 237
  end
  object lblMeaning: TLabel
    Left = 8
    Top = 125
    Width = 93
    Height = 13
    Caption = '#00691^eMeaning:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object pbPhonetic: TWakanPaintbox
    Left = 8
    Top = 33
    Width = 645
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Color = clWindow
    DoubleBuffered = True
    OnPaint = pbPhoneticPaint
  end
  object edtPhonetic: TEdit
    Left = 115
    Top = 8
    Width = 538
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = edtPhoneticChange
  end
  object cbCategories: TComboBox
    Left = 8
    Top = 236
    Width = 361
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object btnOk: TBitBtn
    Left = 390
    Top = 234
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00062^eAdd to vocabulary'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
    ExplicitTop = 251
  end
  object btnCancel: TBitBtn
    Left = 524
    Top = 234
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00050^eCancel'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
  object pbWritten: TWakanPaintbox
    Left = 8
    Top = 90
    Width = 645
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Color = clWindow
    DoubleBuffered = True
    OnPaint = pbWrittenPaint
  end
  object edtMeaning: TMemo
    Left = 8
    Top = 144
    Width = 645
    Height = 70
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'Three lines'
      'should be enough'
      'for everybody')
    ParentFont = False
    TabOrder = 1
    WantReturns = False
    OnKeyPress = edtMeaningKeyPress
  end
end
