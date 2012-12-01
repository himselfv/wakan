object fSelectFont: TfSelectFont
  Left = 241
  Top = 233
  BorderStyle = bsDialog
  Caption = '#00867^eSelect font^cVýbìr fontu'
  ClientHeight = 359
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape9: TShape
    Left = 8
    Top = 253
    Width = 511
    Height = 60
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox3: TPaintBox
    Left = 9
    Top = 254
    Width = 509
    Height = 58
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox3Paint
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 8
    Width = 513
    Height = 233
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect, goThumbTracking]
    TabOrder = 0
    OnSelectCell = StringGrid1SelectCell
    ColWidths = (
      343
      140)
  end
  object BitBtn1: TBitBtn
    Left = 40
    Top = 328
    Width = 113
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 368
    Top = 328
    Width = 121
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
