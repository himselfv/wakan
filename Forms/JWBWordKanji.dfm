object fWordKanji: TfWordKanji
  Left = 757
  Top = 301
  BorderStyle = bsNone
  Caption = '#00650^eCharacters in word'
  ClientHeight = 79
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 220
    Height = 79
    Align = alClient
    Shape = bsFrame
    Visible = False
    ExplicitHeight = 605
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
    Left = 85
    Top = 6
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
end
