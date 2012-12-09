object fUserAdd: TfUserAdd
  Left = 328
  Top = 259
  BorderStyle = bsDialog
  Caption = '#00057^eAdd into vocabulary'
  ClientHeight = 225
  ClientWidth = 671
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  DesignSize = (
    671
    225)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
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
  object Shape9: TShape
    Left = 8
    Top = 48
    Width = 658
    Height = 20
    Brush.Color = clBtnFace
  end
  object PaintBox3: TPaintBox
    Left = 9
    Top = 49
    Width = 656
    Height = 18
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox3Paint
  end
  object Label7: TLabel
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
  object Shape3: TShape
    Left = 8
    Top = 96
    Width = 655
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox2: TPaintBox
    Left = 9
    Top = 97
    Width = 653
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox2Paint
  end
  object Label2: TLabel
    Left = 6
    Top = 181
    Width = 94
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '#00059^eCategory:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
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
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 657
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit3: TEdit
    Left = 8
    Top = 152
    Width = 655
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object ComboBox1: TComboBox
    Left = 6
    Top = 197
    Width = 371
    Height = 21
    Anchors = [akTop, akRight]
    ItemHeight = 0
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 400
    Top = 195
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '#00062^eAdd to vocabulary'
    TabOrder = 3
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 537
    Top = 194
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '#00050^eCancel'
    TabOrder = 4
    Kind = bkCancel
  end
end
