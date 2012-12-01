object Form1: TForm1
  Left = 392
  Top = 256
  Width = 359
  Height = 198
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'EN output:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 50
    Height = 13
    Caption = 'CZ output:'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 38
    Height = 13
    Caption = 'Version:'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 40
    Height = 13
    Caption = 'Number:'
  end
  object Edit1: TEdit
    Left = 72
    Top = 8
    Width = 273
    Height = 21
    TabOrder = 0
    Text = 'en.lng'
  end
  object Button1: TButton
    Left = 8
    Top = 136
    Width = 337
    Height = 25
    Caption = 'Create Language files'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 72
    Top = 40
    Width = 273
    Height = 21
    TabOrder = 2
    Text = 'cz.lng'
  end
  object Edit3: TEdit
    Left = 72
    Top = 72
    Width = 273
    Height = 21
    TabOrder = 3
    Text = '1.50'
  end
  object Edit4: TEdit
    Left = 72
    Top = 104
    Width = 273
    Height = 21
    TabOrder = 4
    Text = '1'
  end
end
