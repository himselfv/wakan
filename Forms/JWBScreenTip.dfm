object fScreenTip: TfScreenTip
  Left = 192
  Top = 111
  BorderStyle = bsNone
  Caption = 'fScreenTip'
  ClientHeight = 449
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object pb: TPaintBox
    Left = 0
    Top = 0
    Width = 677
    Height = 449
    Align = alClient
    OnMouseMove = pbMouseMove
    OnMouseUp = pbMouseUp
    OnPaint = pbPaint
  end
end
