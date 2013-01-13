object fDicAdd: TfDicAdd
  Left = 365
  Top = 568
  Caption = '#00057^eAdd into vocabulary'
  ClientHeight = 237
  ClientWidth = 672
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    672
    237)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape2: TShape
    Left = 7
    Top = 22
    Width = 659
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
    ExplicitWidth = 669
  end
  object PaintBox1: TPaintBox
    Left = 8
    Top = 23
    Width = 657
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox1Paint
    ExplicitWidth = 667
  end
  object Shape3: TShape
    Left = 8
    Top = 71
    Width = 658
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
    ExplicitWidth = 668
  end
  object PaintBox2: TPaintBox
    Left = 9
    Top = 72
    Width = 656
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox2Paint
    ExplicitWidth = 666
  end
  object Label6: TLabel
    Left = 9
    Top = 108
    Width = 139
    Height = 13
    Caption = '#00058^eMeaning (editable):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label4: TLabel
    Left = 9
    Top = 191
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
    Transparent = True
    ExplicitTop = 148
  end
  object Label1: TLabel
    Left = 9
    Top = 5
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
  object Label2: TLabel
    Left = 9
    Top = 54
    Width = 86
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
  object ComboBox1: TComboBox
    Left = 9
    Top = 207
    Width = 366
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
    ExplicitTop = 164
    ExplicitWidth = 376
  end
  object BitBtn1: TBitBtn
    Left = 401
    Top = 206
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00062^eAdd to vocabulary'
    Kind = bkOK
    TabOrder = 1
    ExplicitLeft = 411
    ExplicitTop = 163
  end
  object BitBtn2: TBitBtn
    Left = 538
    Top = 205
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00050^eCancel'
    Kind = bkCancel
    TabOrder = 2
    ExplicitLeft = 548
    ExplicitTop = 162
  end
  object edtMeaning: TMemo
    Left = 8
    Top = 126
    Width = 658
    Height = 55
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    WantReturns = False
  end
end
