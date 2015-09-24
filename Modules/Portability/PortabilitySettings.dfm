object PortabilitySettingsPage: TPortabilitySettingsPage
  Left = 0
  Top = 0
  Caption = '#01060^Portability'
  ClientHeight = 414
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblWakanMode: TLabel
    Left = 3
    Top = 3
    Width = 198
    Height = 13
    Caption = 'Wakan is running in portable mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label49: TLabel
    Left = 3
    Top = 30
    Width = 90
    Height = 13
    Caption = '#01029^eSettings:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label55: TLabel
    Left = 3
    Top = 49
    Width = 107
    Height = 13
    Caption = '#01030^eDictionaries:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label56: TLabel
    Left = 3
    Top = 68
    Width = 98
    Height = 13
    Caption = '#01031^eUser data:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblSettingsPath: TUrlLabel
    Left = 107
    Top = 30
    Width = 27
    Height = 13
    Cursor = crHandPoint
    Caption = '[path]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblDictionariesPath: TUrlLabel
    Left = 107
    Top = 49
    Width = 27
    Height = 13
    Cursor = crHandPoint
    Caption = '[path]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblUserDataPath: TUrlLabel
    Left = 107
    Top = 68
    Width = 27
    Height = 13
    Cursor = crHandPoint
    Caption = '[path]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblUpgradeToStandalone: TLabel
    Left = -4
    Top = 118
    Width = 453
    Height = 26
    Caption = 
      '#01032^eYou can move all your data to Application Data folder an' +
      'd make this copy standalone.'#13#10'This process is not reversible.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblBackupPath: TUrlLabel
    Left = 107
    Top = 87
    Width = 27
    Height = 13
    Cursor = crHandPoint
    Caption = '[path]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label59: TLabel
    Left = 3
    Top = 87
    Width = 94
    Height = 13
    Caption = '#01087^eBackups:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object btnUpgradeToStandalone: TButton
    Left = 123
    Top = 158
    Width = 177
    Height = 25
    Caption = '#01033^eUpgrade to standalone'
    TabOrder = 0
  end
end
