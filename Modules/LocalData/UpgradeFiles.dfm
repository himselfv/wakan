object fUpgradeFiles: TfUpgradeFiles
  Left = 0
  Top = 0
  Caption = '#01017^eMove files'
  ClientHeight = 323
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    407
    323)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMoveQuestion: TLabel
    Left = 8
    Top = 8
    Width = 391
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '#01017^eMove files'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 39
    Width = 391
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '#01018^eYour Wakan folder already contains some user data.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 247
    Width = 391
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      '#01019^eDo you want to move these files to the AppData directory' +
      '?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lbFiles: TListBox
    Left = 8
    Top = 63
    Width = 353
    Height = 178
    ItemHeight = 13
    TabOrder = 0
  end
  object btnMoveFiles: TButton
    Left = 64
    Top = 278
    Width = 129
    Height = 33
    Caption = '#01020^eMove'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
  end
  object btnIgnoreFiles: TButton
    Left = 208
    Top = 278
    Width = 129
    Height = 33
    Caption = '#01021^eIgnore'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
  end
end
