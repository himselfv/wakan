object fWordsExpChoose: TfWordsExpChoose
  Left = 374
  Top = 278
  BorderStyle = bsDialog
  Caption = '#00908^eVocabulary export'
  ClientHeight = 119
  ClientWidth = 355
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
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 345
    Height = 73
    Caption = '#00909^eChoose which information include'
    ItemIndex = 0
    Items.Strings = (
      '#00910^eCategory & learned state'
      '#00911^eCategory only'
      '#00912^eNeither category nor learned state')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 32
    Top = 88
    Width = 75
    Height = 25
    Kind = bkOK
    TabOrder = 1
  end
  object BitBtn2: TBitBtn
    Left = 240
    Top = 88
    Width = 91
    Height = 25
    Caption = '#00913^eCancel'
    Kind = bkCancel
    TabOrder = 2
  end
end
