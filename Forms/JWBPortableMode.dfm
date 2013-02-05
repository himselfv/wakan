object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 325
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
    325)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 339
    Height = 42
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Do you want this copy of Wakan to be portable?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 329
  end
  object Label2: TLabel
    Left = 16
    Top = 119
    Width = 339
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Move Wakan from a computer to a computer - '#13#10'all settings are st' +
      'ored in the application folder.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 329
  end
  object Label3: TLabel
    Left = 16
    Top = 215
    Width = 339
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Install Wakan for all users of this computer.'#13#10'Each user keeps s' +
      'eparate settings.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 329
  end
  object Button1: TButton
    Left = 16
    Top = 80
    Width = 193
    Height = 33
    Caption = 'Portable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Button2: TButton
    Left = 16
    Top = 176
    Width = 193
    Height = 33
    Caption = 'Standalone'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Button3: TButton
    Left = 16
    Top = 272
    Width = 105
    Height = 25
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
end
