object FontSettingsPage: TFontSettingsPage
  Left = 0
  Top = 0
  Caption = '#00450^eFonts'
  ClientHeight = 452
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblFontSmall: TLabel
    Left = 3
    Top = 336
    Width = 144
    Height = 13
    Caption = '#00456^eFont for small text:'
  end
  object lblFontEnglish: TLabel
    Left = 3
    Top = 362
    Width = 112
    Height = 13
    Caption = '#00923^eEnglish font:'
  end
  object lblFontPinYin: TLabel
    Left = 3
    Top = 390
    Width = 184
    Height = 13
    Caption = '#00924^eRomanization && PinYin font:'
  end
  object btnFontSmall: TSpeedButton
    Left = 371
    Top = 336
    Width = 73
    Height = 23
    Caption = '#00452^eChoose'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = btnFontSmallClick
  end
  object btnFontEnglish: TSpeedButton
    Left = 371
    Top = 362
    Width = 73
    Height = 23
    Caption = '#00452^eChoose'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = btnFontEnglishClick
  end
  object btnFontPinYin: TSpeedButton
    Left = 371
    Top = 388
    Width = 73
    Height = 23
    Caption = '#00452^eChoose'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = btnFontPinYinClick
  end
  object btnAutodetectFonts: TButton
    Left = 3
    Top = 3
    Width = 457
    Height = 25
    Caption = '#00464^eSelect recommended fonts'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnAutodetectFontsClick
  end
  object pnlJapaneseFonts: TGroupBox
    Left = 3
    Top = 35
    Width = 457
    Height = 108
    Caption = '#00453^eJapanese fonts'
    TabOrder = 1
    object lblFontJapaneseGrid: TLabel
      Left = 8
      Top = 24
      Width = 181
      Height = 13
      Caption = '#00454^eFont for characters in grid:'
    end
    object btnFontJapaneseGrid: TSpeedButton
      Left = 368
      Top = 24
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontJapaneseGridClick
    end
    object lblFontJapanese: TLabel
      Left = 8
      Top = 51
      Width = 166
      Height = 13
      Caption = '#00455^eFont for big characters:'
    end
    object btnFontJapanese: TSpeedButton
      Left = 368
      Top = 51
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontJapaneseClick
    end
    object lblFontStrokeOrder: TLabel
      Left = 8
      Top = 78
      Width = 175
      Height = 13
      Caption = '#00875^eStroke order display font:'
    end
    object btnFontStrokeOrder: TSpeedButton
      Left = 368
      Top = 78
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontStrokeOrderClick
    end
    object edtFontJapaneseGrid: TEdit
      Left = 240
      Top = 24
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'MS Mincho'
    end
    object edtFontJapanese: TEdit
      Left = 240
      Top = 51
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'MS Mincho'
    end
    object edtFontStrokeOrder: TEdit
      Left = 240
      Top = 78
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'MS Gothic'
    end
  end
  object pnlChineseFonts: TGroupBox
    Left = 3
    Top = 148
    Width = 457
    Height = 178
    Caption = '#00457^eChinese fonts:'
    TabOrder = 2
    object lblFontChineseGrid: TLabel
      Left = 8
      Top = 24
      Width = 253
      Height = 13
      Caption = '#00458^eBig5 font for traditional characters in grid:'
    end
    object btnFontChineseGrid: TSpeedButton
      Left = 368
      Top = 24
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontChineseGridClick
    end
    object lblFontChinese: TLabel
      Left = 8
      Top = 76
      Width = 238
      Height = 13
      Caption = '#00459^eBig5 font for big traditional characters:'
    end
    object btnFontChinese: TSpeedButton
      Left = 368
      Top = 76
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontChineseClick
    end
    object lblFontRadical: TLabel
      Left = 8
      Top = 145
      Width = 164
      Height = 13
      Caption = '#00460^eComplete unicode font:'
    end
    object btnFontRadical: TSpeedButton
      Left = 368
      Top = 145
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontRadicalClick
    end
    object lblFontChineseGridGB: TLabel
      Left = 8
      Top = 50
      Width = 265
      Height = 13
      Caption = '#00461^eGB2312 font for simplified characters in grid:'
    end
    object btnFontChineseGridGB: TSpeedButton
      Left = 368
      Top = 50
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontChineseGridGBClick
    end
    object lblFontChineseGB: TLabel
      Left = 8
      Top = 102
      Width = 250
      Height = 13
      Caption = '#00462^eGB2312 font for big simplified characters:'
    end
    object btnFontChineseGB: TSpeedButton
      Left = 368
      Top = 102
      Width = 73
      Height = 23
      Caption = '#00452^eChoose'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontChineseGBClick
    end
    object lblFontChineseGBWarning: TLabel
      Left = 8
      Top = 126
      Width = 479
      Height = 14
      Caption = 
        '#00463^eMake sure that the Big5 and GB2312 fonts are either the ' +
        'same or at least look the same.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object edtFontChineseGrid: TEdit
      Left = 240
      Top = 24
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'MingLiU'
    end
    object edtFontChinese: TEdit
      Left = 240
      Top = 76
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'MingLiU'
    end
    object edtFontRadical: TEdit
      Left = 240
      Top = 145
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'MingLiU'
    end
    object edtFontChineseGridGB: TEdit
      Left = 240
      Top = 50
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'SimSun'
    end
    object edtFontChineseGB: TEdit
      Left = 240
      Top = 102
      Width = 129
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'SimSun'
    end
  end
  object edtFontPinYin: TEdit
    Left = 243
    Top = 388
    Width = 129
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = 'Arial'
  end
  object edtFontEnglish: TEdit
    Left = 243
    Top = 362
    Width = 129
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    Text = 'Arial'
  end
  object edtFontSmall: TEdit
    Left = 243
    Top = 336
    Width = 129
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'MS Gothic'
  end
end
