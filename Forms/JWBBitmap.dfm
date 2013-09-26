object fBitmap: TfBitmap
  Left = 261
  Top = 231
  BorderStyle = bsDialog
  Caption = '#00002^ePrint to BMP'
  ClientHeight = 154
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 126
    Height = 13
    Caption = '#00003^eWidth (in pixels):'
  end
  object Label2: TLabel
    Left = 176
    Top = 8
    Width = 193
    Height = 13
    AutoSize = False
    Caption = '#00004^eHeight (in pixels):'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 122
    Height = 13
    Caption = '#00005^eFilename prefix:'
  end
  object Label4: TLabel
    Left = 8
    Top = 96
    Width = 323
    Height = 14
    Caption = '#00006^e%d.BMP will be added after prefix (%d is page number).'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 120
    Width = 97
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 264
    Top = 120
    Width = 91
    Height = 25
    Caption = '#00007^eCancel'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 169
    Height = 21
    TabOrder = 2
    Text = '1024'
  end
  object Edit2: TEdit
    Left = 192
    Top = 24
    Width = 161
    Height = 21
    TabOrder = 3
    Text = '768'
  end
  object Edit3: TEdit
    Left = 8
    Top = 72
    Width = 345
    Height = 21
    TabOrder = 4
    Text = 'BITMAP_'
  end
end
