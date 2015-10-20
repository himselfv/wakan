object fUpgradeFiles: TfUpgradeFiles
  Left = 0
  Top = 0
  Caption = '#01017^eMove files'
  ClientHeight = 346
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    408
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMoveQuestion: TLabel
    Left = 8
    Top = 8
    Width = 392
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '#01230^Upgrade data'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 391
  end
  object Label1: TLabel
    Left = 8
    Top = 39
    Width = 392
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      '#01231^Your Wakan folder has some data which needs to be upgrade' +
      'd.'
    WordWrap = True
    ExplicitWidth = 389
  end
  object Label2: TLabel
    Left = 8
    Top = 247
    Width = 392
    Height = 42
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      '#01233^If you decline, old data will not be accessible. You can ' +
      'perform the upgrade manually from Settings.'
    WordWrap = True
  end
  object lbFiles: TListBox
    Left = 8
    Top = 63
    Width = 353
    Height = 178
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnMoveFiles: TButton
    Left = 64
    Top = 301
    Width = 129
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = '#01234^Upgrade'
    ModalResult = 1
    TabOrder = 1
  end
  object btnIgnoreFiles: TButton
    Left = 208
    Top = 301
    Width = 129
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = '#01235^Skip'
    ModalResult = 2
    TabOrder = 2
  end
end
