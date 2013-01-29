object fWordKanji: TfWordKanji
  Left = 757
  Top = 301
  BorderStyle = bsNone
  Caption = '#00650^eCharacters in word'
  ClientHeight = 605
  ClientWidth = 220
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 220
    Height = 605
    Align = alClient
    Shape = bsFrame
  end
  object BoxShape1: TShape
    Left = 8
    Top = 26
    Width = 202
    Height = 43
  end
  object Label2: TLabel
    Left = 6
    Top = 6
    Width = 122
    Height = 13
    Caption = '#00734^eKanji / Radical:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label3: TLabel
    Left = 117
    Top = 7
    Width = 93
    Height = 13
    Caption = '#00691^eMeaning:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object PaintBoxK1: TPaintBox
    Tag = 1
    Left = 9
    Top = 27
    Width = 89
    Height = 41
    OnClick = PaintBoxK1Click
    OnMouseMove = PaintBoxK1MouseMove
    OnMouseUp = PaintBoxK1MouseUp
    OnPaint = PaintBoxK1Paint
  end
  object Label8: TLabel
    Left = 121
    Top = 28
    Width = 89
    Height = 41
    AutoSize = False
    Caption = 'Label8'
    Transparent = True
    WordWrap = True
  end
  object Shape1: TShape
    Left = 8
    Top = 74
    Width = 202
    Height = 43
  end
end
