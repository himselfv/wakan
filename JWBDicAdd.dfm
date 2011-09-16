object fDicAdd: TfDicAdd
  Left = 365
  Top = 568
  BorderStyle = bsDialog
  Caption = '#00057^eAdd into vocabulary^cPøidat do slovíèek'
  ClientHeight = 194
  ClientWidth = 682
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Shape2: TShape
    Left = 7
    Top = 22
    Width = 669
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox1: TPaintBox
    Left = 8
    Top = 23
    Width = 667
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox1Paint
  end
  object Shape3: TShape
    Left = 8
    Top = 71
    Width = 668
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox2: TPaintBox
    Left = 9
    Top = 72
    Width = 666
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox2Paint
  end
  object Label6: TLabel
    Left = 9
    Top = 103
    Width = 251
    Height = 13
    Caption = '#00058^eMeaning (editable):^cVýznam (upravitelný):'
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
    Top = 148
    Width = 154
    Height = 13
    Caption = '#00059^eCategory:^cKategorie:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 9
    Top = 5
    Width = 134
    Height = 13
    Caption = '#00060^ePhonetic:^cÈtení:'
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
    Width = 127
    Height = 13
    Caption = '#00061^eWritten:^cZápis:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Edit3: TEdit
    Left = 9
    Top = 117
    Width = 668
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object ComboBox1: TComboBox
    Left = 9
    Top = 164
    Width = 376
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 411
    Top = 163
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '#00062^eAdd to vocabulary^cPøidat do slovíèek'
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 548
    Top = 162
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '#00050^eCancel^cStorno'
    TabOrder = 3
    Kind = bkCancel
  end
end
