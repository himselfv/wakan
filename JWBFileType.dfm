object fFileType: TfFileType
  Left = 192
  Top = 111
  BorderStyle = bsDialog
  Caption = '#00888^eSelect file type^cZvolte typ souboru'
  ClientHeight = 192
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object rgType: TRadioGroup
    Left = 8
    Top = 8
    Width = 265
    Height = 145
    Caption = '#00889^eFile type^cTyp souboru'
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 160
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 200
    Top = 160
    Width = 75
    Height = 25
    Caption = '#00890^eCancel^cStorno'
    TabOrder = 2
    Kind = bkCancel
  end
end
