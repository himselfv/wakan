object fWordDetails: TfWordDetails
  Left = 404
  Top = 453
  Width = 464
  Height = 139
  BorderStyle = bsSizeToolWin
  Caption = '#00649^eSelected word^cVybran� slovo'
  Color = clBtnFace
  Constraints.MaxHeight = 139
  Constraints.MinHeight = 139
  Constraints.MinWidth = 211
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 456
    Height = 108
    Align = alClient
    Style = bsRaised
  end
  object Shape2: TShape
    Left = 7
    Top = 6
    Width = 444
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox1: TPaintBox
    Left = 8
    Top = 7
    Width = 442
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox1Paint
  end
  object Shape3: TShape
    Left = 7
    Top = 38
    Width = 418
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox2: TPaintBox
    Left = 8
    Top = 39
    Width = 416
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox2Paint
  end
  object SpeedButton23: TSpeedButton
    Left = 425
    Top = 38
    Width = 24
    Height = 27
    Hint = '#00161^eInsert character into clipboard^cVlo�it znak do schr�nky'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333FF33333333FF333993333333300033377F3333333777333993333333
      300033F77FFF3333377739999993333333333777777F3333333F399999933333
      33003777777333333377333993333333330033377F3333333377333993333333
      3333333773333333333F333333333333330033333333F33333773333333C3333
      330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
      333333333337733333FF3333333C333330003333333733333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton23Click
  end
  object Shape5: TShape
    Left = 7
    Top = 70
    Width = 443
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clWindow
  end
  object PaintBox5: TPaintBox
    Left = 8
    Top = 71
    Width = 441
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = PaintBox5Paint
  end
end