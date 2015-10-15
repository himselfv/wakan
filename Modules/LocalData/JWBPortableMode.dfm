object fPortableMode: TfPortableMode
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '#01009^eSelect mode'
  ClientHeight = 260
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    423
    260)
  PixelsPerInch = 96
  TextHeight = 13
  object lblQuestion: TLabel
    Left = 8
    Top = 8
    Width = 391
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '#01010^eHow do you want Wakan to run?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblStandaloneDescription: TLabel
    Left = 8
    Top = 95
    Width = 391
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      '#01012^eStore settings in registry and user data in roaming AppD' +
      'ata folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblPortableDescription: TLabel
    Left = 8
    Top = 191
    Width = 391
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '#01016^eStore all settings and user data in Wakan folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object btnStandalone: TButton
    Left = 8
    Top = 56
    Width = 193
    Height = 33
    Caption = '#01011^eStandalone'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnStandaloneClick
  end
  object btnPortable: TButton
    Left = 8
    Top = 152
    Width = 193
    Height = 33
    Caption = '#01015^ePortable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnPortableClick
  end
end
