object fVocabAdd: TfVocabAdd
  Left = 328
  Top = 259
  BorderStyle = bsSizeToolWin
  Caption = '#00057^eAdd into vocabulary'
  ClientHeight = 281
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
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPhonetic: TLabel
    Left = 8
    Top = 8
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
    Top = 80
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
    Left = 6
    Top = 237
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
    ExplicitTop = 181
  end
  object lblMeaning: TLabel
    Left = 8
    Top = 136
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
    Left = 7
    Top = 22
    Width = 649
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Color = clWhite
    DoubleBuffered = True
    OnPaint = pbPhoneticPaint
    ExplicitWidth = 659
  end
  object edtPhonetic: TEdit
    Left = 8
    Top = 24
    Width = 648
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
    Left = 6
    Top = 253
    Width = 361
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    ExplicitTop = 283
    ExplicitWidth = 371
  end
  object btnOk: TBitBtn
    Left = 390
    Top = 251
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00062^eAdd to vocabulary'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
    ExplicitLeft = 400
    ExplicitTop = 281
  end
  object btnCancel: TBitBtn
    Left = 527
    Top = 251
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
    Top = 96
    Width = 645
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Color = clWhite
    DoubleBuffered = True
    OnPaint = pbWrittenPaint
    ExplicitWidth = 655
  end
  object pbPhoneticConv: TWakanPaintbox
    Left = 8
    Top = 48
    Width = 648
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    DoubleBuffered = True
    OnPaint = pbPhoneticConvPaint
    ExplicitWidth = 658
  end
  object edtMeaning: TMemo
    Left = 8
    Top = 155
    Width = 648
    Height = 68
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
    ExplicitWidth = 658
    ExplicitHeight = 98
  end
end
