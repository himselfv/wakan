object fTest: TfTest
  Left = 52
  Top = 82
  BorderStyle = bsDialog
  Caption = 'Kana Test'
  ClientHeight = 163
  ClientWidth = 883
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
  object UnicodeEdit1: TUnicodeEdit
    Left = 16
    Top = 16
    Width = 857
    Height = 28
    AutoSize = False
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'MS Mincho'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    OnKeyPress = UnicodeEdit1KeyPress
  end
  object Button1: TButton
    Left = 400
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 96
    Width = 857
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 2
  end
  object UnicodeEdit2: TUnicodeEdit
    Left = 16
    Top = 56
    Width = 857
    Height = 28
    AutoSize = False
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'MS Mincho'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
    OnKeyPress = UnicodeEdit1KeyPress
  end
end
