object fStatistics: TfStatistics
  Left = 248
  Top = 209
  BorderStyle = bsDialog
  Caption = '#00585^eStatistics'
  ClientHeight = 305
  ClientWidth = 599
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
  object RxLabel16: TLabel
    Left = 208
    Top = 8
    Width = 207
    Height = 18
    Alignment = taCenter
    AutoSize = False
    Caption = '#00585^eStatistics'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 8
    Top = 120
    Width = 585
    Height = 9
    Shape = bsTopLine
  end
  object RxLabel1: TLabel
    Left = 64
    Top = 40
    Width = 395
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '#00586^eProgram dictionary (WAKAN.CHR)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object RxLabel2: TLabel
    Left = 64
    Top = 128
    Width = 393
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '#00587^eUser database (WAKAN.USR)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 8
    Top = 32
    Width = 585
    Height = 9
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 196
    Height = 13
    Caption = '#00588^eDictionary file build date'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 163
    Height = 13
    Caption = '#00589^eKANJIDIC version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 96
    Width = 155
    Height = 13
    Caption = '#00590^eUNIHAN version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 280
    Top = 64
    Width = 221
    Height = 13
    Caption = '#00591^eChinese character database:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 280
    Top = 80
    Width = 180
    Height = 13
    Caption = '#00592^eJapanese characters:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 280
    Top = 96
    Width = 140
    Height = 13
    Caption = '#00593^eAll characters:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label10: TLabel
    Left = 547
    Top = 64
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label10'
  end
  object Label11: TLabel
    Left = 548
    Top = 80
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label11'
  end
  object Label12: TLabel
    Left = 548
    Top = 96
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label12'
  end
  object lblDicBuildDate: TLabel
    Left = 198
    Top = 64
    Width = 72
    Height = 13
    Alignment = taRightJustify
    Caption = 'lblDicBuildDate'
  end
  object lblKanjidicVersion: TLabel
    Left = 188
    Top = 80
    Width = 82
    Height = 13
    Alignment = taRightJustify
    Caption = 'lblKanjidicVersion'
  end
  object lblUnihanVersion: TLabel
    Left = 191
    Top = 96
    Width = 79
    Height = 13
    Alignment = taRightJustify
    Caption = 'lblUnihanVersion'
  end
  object Label17: TLabel
    Left = 8
    Top = 152
    Width = 167
    Height = 13
    Caption = '#00594^eVocabulary entries:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label18: TLabel
    Left = 8
    Top = 168
    Width = 145
    Height = 13
    Caption = '#00595^eLearned words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label19: TLabel
    Left = 8
    Top = 184
    Width = 151
    Height = 13
    Caption = '#00596^eMastered words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label20: TLabel
    Left = 8
    Top = 200
    Width = 165
    Height = 13
    Caption = '#00597^eProblematic words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label21: TLabel
    Left = 8
    Top = 232
    Width = 214
    Height = 13
    Caption = '#00598^eNumber of katakana words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label22: TLabel
    Left = 280
    Top = 152
    Width = 202
    Height = 13
    Caption = '#00599^eNumber of kanji in words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label23: TLabel
    Left = 280
    Top = 184
    Width = 139
    Height = 13
    Caption = '#00600^eLearned kanji:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label24: TLabel
    Left = 280
    Top = 200
    Width = 185
    Height = 13
    Caption = '#00601^eUnlearned basic kanji:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label25: TLabel
    Left = 232
    Top = 152
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label25'
  end
  object Label26: TLabel
    Left = 232
    Top = 168
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label26'
  end
  object Label27: TLabel
    Left = 232
    Top = 184
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label27'
  end
  object Label28: TLabel
    Left = 232
    Top = 200
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label28'
  end
  object Label29: TLabel
    Left = 232
    Top = 232
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label29'
  end
  object Label30: TLabel
    Left = 547
    Top = 152
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label30'
  end
  object Label31: TLabel
    Left = 547
    Top = 184
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label31'
  end
  object Label32: TLabel
    Left = 547
    Top = 200
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label32'
  end
  object Label33: TLabel
    Left = 8
    Top = 216
    Width = 168
    Height = 13
    Caption = '#00602^eNon-popular words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label34: TLabel
    Left = 280
    Top = 216
    Width = 198
    Height = 13
    Caption = '#00603^eLearned non-basic kanji:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label35: TLabel
    Left = 232
    Top = 216
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label35'
  end
  object Label36: TLabel
    Left = 547
    Top = 216
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label36'
  end
  object Label37: TLabel
    Left = 264
    Top = 232
    Width = 15
    Height = 13
    Caption = '^e'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label38: TLabel
    Left = 280
    Top = 168
    Width = 201
    Height = 13
    Caption = '#00604^eNon-basic kanji in words:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label39: TLabel
    Left = 547
    Top = 168
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label39'
  end
  object Label40: TLabel
    Left = 280
    Top = 232
    Width = 247
    Height = 13
    Caption = '#00605^eLearned chinese-only characters:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label41: TLabel
    Left = 547
    Top = 232
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label36'
  end
  object Label42: TLabel
    Left = 8
    Top = 248
    Width = 207
    Height = 13
    Caption = '#00606^eWords with known writing:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label43: TLabel
    Left = 280
    Top = 248
    Width = 156
    Height = 13
    Caption = '#00607^eLearned radicals:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label44: TLabel
    Left = 232
    Top = 248
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label29'
  end
  object Label45: TLabel
    Left = 547
    Top = 248
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = 'Label36'
  end
  object BitBtn1: TBitBtn
    Left = 264
    Top = 272
    Width = 89
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    DoubleBuffered = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 1
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
  end
end
