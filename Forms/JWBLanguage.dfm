object fLanguage: TfLanguage
  Left = 270
  Top = 167
  BorderStyle = bsDialog
  Caption = 'WaKan - Language Selection'
  ClientHeight = 417
  ClientWidth = 242
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 80
    Width = 172
    Height = 13
    Caption = 'Please choose your language:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 336
    Width = 19
    Height = 13
    Caption = 'File:'
  end
  object Label3: TLabel
    Left = 8
    Top = 360
    Width = 34
    Height = 13
    Caption = 'Author:'
  end
  object lbLanguageFile: TLabel
    Left = 187
    Top = 336
    Width = 45
    Height = 13
    Alignment = taRightJustify
    Caption = '[filename]'
  end
  object lbLanguageAuthor: TLabel
    Left = 196
    Top = 360
    Width = 36
    Height = 13
    Alignment = taRightJustify
    Caption = '[author]'
  end
  object RxLabel1: TLabel
    Left = 72
    Top = 8
    Width = 89
    Height = 37
    Caption = 'wakan'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object RxLabel2: TLabel
    Left = 32
    Top = 48
    Width = 175
    Height = 13
    Caption = 'Tool for learning Japanese && Chinese'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 72
    Width = 225
    Height = 25
    Shape = bsTopLine
  end
  object lbLanguages: TListBox
    Left = 8
    Top = 104
    Width = 225
    Height = 225
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbLanguagesClick
  end
  object bbOk: TBitBtn
    Left = 8
    Top = 384
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnShowInfo: TButton
    Left = 160
    Top = 384
    Width = 75
    Height = 25
    Caption = 'More info...'
    TabOrder = 2
    OnClick = btnShowInfoClick
  end
end
