object fTextTableBrowser: TfTextTableBrowser
  Left = 0
  Top = 0
  Caption = 'Text table browser'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 304
    Width = 576
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
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
    TabOrder = 1
    ExplicitLeft = 4
    ExplicitTop = 24
    ExplicitWidth = 568
    ExplicitHeight = 276
  end
end
