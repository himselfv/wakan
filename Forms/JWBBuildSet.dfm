object Form2: TForm2
  Left = 151
  Top = 380
  BorderStyle = bsDialog
  Caption = 'JALET.DIC settings'
  ClientHeight = 275
  ClientWidth = 346
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'EDICT version'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 85
    Height = 13
    Caption = 'KANJIDIC version'
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 79
    Height = 13
    Caption = 'UNIHAN version'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 136
    Width = 321
    Height = 17
    Caption = 'Build with chinese characters'
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 8
    Top = 104
    Width = 329
    Height = 21
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 8
    Top = 64
    Width = 329
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 136
    Top = 240
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkOK
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Encrypt files'
    TabOrder = 5
  end
  object CheckBox3: TCheckBox
    Left = 120
    Top = 168
    Width = 217
    Height = 17
    Caption = 'Protect package with CRC'
    TabOrder = 6
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 192
    Width = 329
    Height = 41
    Caption = 'Packing method'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Huffman'
      'LZW')
    TabOrder = 7
  end
end
