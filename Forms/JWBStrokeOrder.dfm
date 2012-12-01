object fStrokeOrder: TfStrokeOrder
  Left = 304
  Top = 185
  BorderStyle = bsToolWindow
  Caption = '#00608^eStroke order^cPoøadí tahù'
  ClientHeight = 303
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 136
    Width = 291
    Height = 18
    Caption = '#00609^eNot available^cNení k dispozici'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 6
    Top = 35
    Width = 261
    Height = 261
    Anchors = []
    Shape = bsFrame
  end
  object RxGIFAnimator1: TRxGIFAnimator
    Left = 8
    Top = 37
    Width = 256
    Height = 256
    Anchors = []
  end
  object SpeedButton3: TSpeedButton
    Left = 240
    Top = 8
    Width = 23
    Height = 22
    AllowAllUp = True
    Anchors = []
    GroupIndex = 1
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      0000377777777777777703030303030303037F7F7F7F7F7F7F7F000000000000
      00007777777777777777933393303933337073F37F37F73F3377393393303393
      379037FF7F37F37FF777379793303379793037777337F3777737339933303339
      93303377F3F7F3F77F3733993930393993303377F737F7377FF7399993303399
      999037777337F377777793993330333393307377FF37F3337FF7333993303333
      993033377F37F33377F7333993303333993033377337F3337737333333303333
      33303FFFFFF7FFFFFFF700000000000000007777777777777777030303030303
      03037F7F7F7F7F7F7F7F00000000000000007777777777777777}
    NumGlyphs = 2
    OnClick = SpeedButton3Click
  end
  object TrackBar1: TTrackBar
    Left = 6
    Top = 4
    Width = 229
    Height = 26
    Anchors = []
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 0
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 88
    Top = 16
  end
end
