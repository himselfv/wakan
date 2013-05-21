object fCharDataBrowser: TfCharDataBrowser
  Left = 0
  Top = 0
  Caption = 'Character DB Browser'
  ClientHeight = 337
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 576
    Height = 304
    Align = alClient
    ColCount = 7
    DefaultColWidth = 80
    DefaultRowHeight = 20
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 304
    Width = 576
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      576
      33)
    object btnClose: TButton
      Left = 467
      Top = 4
      Width = 101
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ModalResult = 11
      TabOrder = 0
    end
  end
end
