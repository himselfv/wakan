object fNewCategory: TfNewCategory
  Left = 232
  Top = 145
  BorderStyle = bsDialog
  Caption = '#00370^eNew category'
  ClientHeight = 146
  ClientWidth = 388
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
    Width = 80
    Height = 13
    Caption = '#00371^eName:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 369
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 56
    Width = 369
    Height = 41
    Caption = '#00372^eType'
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      '#00373^eLesson'
      '#00374^eGroup'
      '#00375^eTemporary')
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 112
    Width = 97
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 280
    Top = 112
    Width = 99
    Height = 25
    Caption = '#00007^eCancel'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
