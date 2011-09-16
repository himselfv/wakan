object Form1: TForm1
  Left = 258
  Top = 204
  BorderStyle = bsDialog
  Caption = 'Wakan Dictionary Builder 1.1'
  ClientHeight = 203
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ArtLabel1: TArtLabel
    Left = 5
    Top = 8
    Width = 369
    Height = 41
    ArtStyle = [as3DRaise, asShadow]
    Caption = 'WaKan Dictionary Builder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -31
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Transparent = False
    Color = clBtnFace
    ShadowColor = clGray
    OutlineColor = clWhite
    AutoSize = False
    ShadowLength = 3
    Gradient.GradientStyle = gsLine
    Gradient.LineSize = 2
    Gradient.Color1 = clSilver
    Gradient.Color2 = clGray
    Gradient.Color3 = clBlack
    Gradient.Color4 = clWhite
    Gradient.Color5 = clBlack
    Gradient.ColorCount = 3
    Gradient.LineDirection = diHorizonal
    TextAlign = taLeftJustify
  end
  object Label1: TLabel
    Left = 8
    Top = 184
    Width = 145
    Height = 13
    Caption = 'Copyright (C) Filip Kabrt 2002-3'
  end
  object Label2: TLabel
    Left = 272
    Top = 184
    Width = 100
    Height = 13
    Caption = 'This utility is freeware'
  end
  object Label3: TLabel
    Left = 160
    Top = 184
    Width = 102
    Height = 13
    Caption = 'dreamfly@centrum.cz'
  end
  object Button5: TButton
    Left = 8
    Top = 56
    Width = 361
    Height = 25
    Caption = 'Step 1 : Import KANJIDIC.UNI and UNIHAN.TXT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = Button5Click
  end
  object Button1: TButton
    Left = 8
    Top = 88
    Width = 361
    Height = 25
    Caption = 'Step 2 : Generate datafiles from Paradox tables'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 120
    Width = 361
    Height = 25
    Caption = 'Step 3: Build WAKAN.CHR from datafiles'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 248
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 3
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 168
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Memo1: TMemo
    Left = 32
    Top = 24
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 5
  end
  object Button6: TButton
    Left = 8
    Top = 152
    Width = 361
    Height = 25
    Caption = 'Step 1A: Put JLPT info into tables'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = Button6Click
  end
  object TEdict: TTable
    DatabaseName = 'Data\'
    TableName = 'Dict.db'
    Left = 48
    Top = 64
    object TEdictKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TEdictIndex: TIntegerField
      FieldName = 'Index'
    end
    object TEdictEnglish: TStringField
      FieldName = 'English'
      Size = 150
    end
    object TEdictPhonetic: TStringField
      FieldName = 'Phonetic'
      Size = 80
    end
    object TEdictKanji: TStringField
      FieldName = 'Kanji'
      Size = 80
    end
    object TEdictSort: TStringField
      FieldName = 'Sort'
      Size = 40
    end
  end
  object TKanjiDic: TTable
    DatabaseName = 'Data\'
    TableName = 'Char.db'
    Left = 80
    Top = 64
    object TKanjiDicKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TKanjiDicIndex: TIntegerField
      FieldName = 'Index'
    end
    object TKanjiDicChinese: TSmallintField
      FieldName = 'Chinese'
    end
    object TKanjiDicType: TStringField
      FieldName = 'Type'
      Size = 1
    end
    object TKanjiDicUnicode: TStringField
      FieldName = 'Unicode'
      Size = 4
    end
    object TKanjiDicStrokeCount: TIntegerField
      FieldName = 'StrokeCount'
    end
    object TKanjiDicJpFrequency: TIntegerField
      FieldName = 'JpFrequency'
    end
    object TKanjiDicChFrequency: TIntegerField
      FieldName = 'ChFrequency'
    end
    object TKanjiDicJouyouGrade: TIntegerField
      FieldName = 'JouyouGrade'
    end
    object TKanjiDicJpStrokeCount: TIntegerField
      FieldName = 'JpStrokeCount'
    end
  end
  object TKanjiRead: TTable
    DatabaseName = 'Data\'
    IndexFieldNames = 'Kanji'
    MasterFields = 'Index'
    TableName = 'CharRead.DB'
    Left = 112
    Top = 64
    object TKanjiReadKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TKanjiReadIndex: TIntegerField
      FieldName = 'Index'
    end
    object TKanjiReadKanji: TIntegerField
      FieldName = 'Kanji'
    end
    object TKanjiReadType: TSmallintField
      FieldName = 'Type'
    end
    object TKanjiReadReading: TStringField
      FieldName = 'Reading'
      Size = 200
    end
    object TKanjiReadReadDot: TIntegerField
      FieldName = 'ReadDot'
    end
    object TKanjiReadPosition: TSmallintField
      FieldName = 'Position'
    end
  end
  object dsKanjiDic: TDataSource
    DataSet = TKanjiDic
    Left = 144
    Top = 64
  end
  object TUser: TTable
    DatabaseName = 'Data\'
    TableName = 'User.db'
    Left = 176
    Top = 64
    object TUserKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TUserIndex: TIntegerField
      FieldName = 'Index'
    end
    object TUserEnglish: TStringField
      FieldName = 'English'
      Size = 80
    end
    object TUserPhonetic: TStringField
      FieldName = 'Phonetic'
      Size = 80
    end
    object TUserKanji: TStringField
      FieldName = 'Kanji'
      Size = 80
    end
    object TUserAdded: TDateField
      FieldName = 'Added'
    end
    object TUserPrinted: TDateField
      FieldName = 'Printed'
    end
    object TUserLearned: TDateField
      FieldName = 'Learned'
    end
    object TUserMastered: TDateField
      FieldName = 'Mastered'
    end
    object TUserLevelAdded: TSmallintField
      FieldName = 'LevelAdded'
    end
    object TUserLevelLearned: TSmallintField
      FieldName = 'LevelLearned'
    end
    object TUserLevelMastered: TSmallintField
      FieldName = 'LevelMastered'
    end
    object TUserNoPrinted: TIntegerField
      FieldName = 'NoPrinted'
    end
    object TUserSumScore: TIntegerField
      FieldName = 'SumScore'
    end
    object TUserScore: TSmallintField
      FieldName = 'Score'
    end
    object TUserNoDescend: TIntegerField
      FieldName = 'NoDescend'
    end
    object TUserMaxScore: TSmallintField
      FieldName = 'MaxScore'
    end
    object TUserSheetNo: TIntegerField
      FieldName = 'SheetNo'
    end
    object TUserSheetOrder: TIntegerField
      FieldName = 'SheetOrder'
    end
  end
  object TEdictIdx: TTable
    DatabaseName = 'Data\'
    IndexFieldNames = 'Kanji'
    MasterFields = 'Index'
    TableName = 'DictIdx.db'
    Left = 208
    Top = 64
    object TEdictIdxKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TEdictIdxWord: TIntegerField
      FieldName = 'Word'
    end
    object TEdictIdxKanji: TIntegerField
      FieldName = 'Kanji'
    end
    object TEdictIdxBegin: TBooleanField
      FieldName = 'Begin'
    end
    object TEdictIdxIndex: TIntegerField
      FieldName = 'Index'
    end
  end
  object TUserIdx: TTable
    DatabaseName = 'Data\'
    IndexFieldNames = 'Kanji'
    MasterFields = 'Index'
    TableName = 'UserIdx.db'
    Left = 240
    Top = 64
    object TUserIdxKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TUserIdxWord: TIntegerField
      FieldName = 'Word'
    end
    object TUserIdxKanji: TIntegerField
      FieldName = 'Kanji'
    end
    object TUserIdxBegin: TBooleanField
      FieldName = 'Begin'
    end
  end
  object TKanjiReadRef: TTable
    DatabaseName = 'Data\'
    IndexName = 'Reading_Ind'
    TableName = 'CharRead.DB'
    Left = 272
    Top = 64
    object TKanjiReadRefKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TKanjiReadRefKanji: TIntegerField
      FieldName = 'Kanji'
    end
    object TKanjiReadRefType: TSmallintField
      FieldName = 'Type'
    end
    object TKanjiReadRefReading: TStringField
      FieldName = 'Reading'
      Size = 50
    end
  end
  object TUserIdxRef: TTable
    DatabaseName = 'Data\'
    IndexFieldNames = 'Kanji;Word'
    TableName = 'UserIdx.db'
    Left = 304
    Top = 64
    object TUserIdxRefKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TUserIdxRefWord: TIntegerField
      FieldName = 'Word'
    end
    object TUserIdxRefKanji: TIntegerField
      FieldName = 'Kanji'
    end
    object TUserIdxRefBegin: TBooleanField
      FieldName = 'Begin'
    end
  end
  object TEdictWord: TTable
    DatabaseName = 'Data\'
    TableName = 'DictWord.db'
    Left = 80
    Top = 96
    object TEdictWordKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TEdictWordWord: TStringField
      FieldName = 'Word'
    end
    object TEdictWordEntry: TIntegerField
      FieldName = 'Entry'
    end
    object TEdictWordBegin: TBooleanField
      FieldName = 'Begin'
    end
    object TEdictWordIndex: TIntegerField
      FieldName = 'Index'
    end
  end
  object TRomaji: TTable
    DatabaseName = 'Data\'
    IndexName = 'Index_Ind'
    TableName = 'Romaji.DB'
    Left = 112
    Top = 96
    object TRomajiKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TRomajiIndex: TIntegerField
      FieldName = 'Index'
    end
    object TRomajiHiragana: TStringField
      FieldName = 'Hiragana'
      Size = 12
    end
    object TRomajiKatakana: TStringField
      FieldName = 'Katakana'
      Size = 12
    end
    object TRomajiJapanese: TStringField
      FieldName = 'Japanese'
      Size = 40
    end
    object TRomajiEnglish: TStringField
      FieldName = 'English'
      Size = 40
    end
    object TRomajiCzech: TStringField
      FieldName = 'Czech'
      Size = 40
    end
    object TRomajiInput: TBooleanField
      FieldName = 'Input'
    end
    object TRomajiSort: TStringField
      FieldName = 'Sort'
      Size = 4
    end
  end
  object TRadical: TTable
    DatabaseName = 'Data\'
    IndexName = 'Number_Ind'
    TableName = 'Radical.db'
    Left = 48
    Top = 96
    object TRadicalKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TRadicalNumber: TIntegerField
      FieldName = 'Number'
    end
    object TRadicalVariant: TIntegerField
      FieldName = 'Variant'
    end
    object TRadicalUnicode: TStringField
      FieldName = 'Unicode'
      Size = 4
    end
    object TRadicalStrokeCount: TIntegerField
      FieldName = 'StrokeCount'
    end
    object TRadicalBushuCount: TIntegerField
      FieldName = 'BushuCount'
    end
    object TRadicalUnicodeCount: TIntegerField
      FieldName = 'UnicodeCount'
    end
    object TRadicalJapaneseCount: TIntegerField
      FieldName = 'JapaneseCount'
    end
    object TRadicalKangXiCount: TIntegerField
      FieldName = 'KangXiCount'
    end
  end
  object TBopomofo: TTable
    DatabaseName = 'Data\'
    IndexFieldNames = 'Key'
    TableName = 'Bopomofo.db'
    Left = 144
    Top = 96
    object TBopomofoKey: TAutoIncField
      FieldName = 'Key'
      ReadOnly = True
    end
    object TBopomofoLetter: TStringField
      FieldName = 'Letter'
      Size = 5
    end
    object TBopomofoPosition: TSmallintField
      FieldName = 'Position'
    end
    object TBopomofoGlyph: TStringField
      FieldName = 'Glyph'
      Size = 8
    end
  end
end
