object fNewCategory: TfNewCategory
  Left = 232
  Top = 145
  BorderStyle = bsDialog
  Caption = '#00370^eNew category^cNová kategorie'
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
    Width = 89
    Height = 13
    Caption = '#00371^eName:^cNázev:'
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
    Caption = '#00372^eType^cTyp'
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      '#00373^eLesson^cLekce'
      '#00374^eGroup^cSkupina'
      '#00375^eTemporary^cPøechodná')
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 112
    Width = 97
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 280
    Top = 112
    Width = 99
    Height = 25
    Caption = '#00007^eCancel^cZrušit'
    TabOrder = 3
    Kind = bkCancel
  end
end
