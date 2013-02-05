object fPortableMode: TfPortableMode
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select mode'
  ClientHeight = 280
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    371
    280)
  PixelsPerInch = 96
  TextHeight = 13
  object lblQuestion: TLabel
    Left = 16
    Top = 16
    Width = 339
    Height = 42
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Do you want this copy of Wakan to be standalone?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 329
  end
  object lblPortableDescription: TLabel
    Left = 16
    Top = 215
    Width = 339
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Move Wakan from a computer to a computer.'#13#10'All settings are stor' +
      'ed in the application folder.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblStandaloneDescription: TLabel
    Left = 16
    Top = 119
    Width = 339
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Share Wakan with all users of this computer.'#13#10'Each user keeps th' +
      'eir own settings.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object btnPortable: TButton
    Left = 16
    Top = 176
    Width = 193
    Height = 33
    Caption = 'Portable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1001
    ParentFont = False
    TabOrder = 0
  end
  object btnStandalone: TButton
    Left = 16
    Top = 80
    Width = 193
    Height = 33
    Caption = 'Standalone'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1000
    ParentFont = False
    TabOrder = 1
  end
end
