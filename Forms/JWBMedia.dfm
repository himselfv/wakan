object fMedia: TfMedia
  Left = 192
  Top = 114
  BorderStyle = bsSizeToolWin
  Caption = '^eAnnotations Media Window'
  ClientHeight = 606
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 854
    Height = 585
    Align = alClient
    Center = True
    ExplicitWidth = 862
  end
  object TabSet1: TTabSet
    Left = 0
    Top = 585
    Width = 854
    Height = 21
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    OnChange = TabSet1Change
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 0
    Width = 854
    Height = 585
    Align = alClient
    TabOrder = 1
    OnDocumentComplete = WebBrowser1DocumentComplete
    ExplicitWidth = 862
    ControlData = {
      4C00000043580000763C00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126202000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
