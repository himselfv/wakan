object fKanjiSearch: TfKanjiSearch
  Left = 359
  Top = 186
  BorderStyle = bsNone
  Caption = '#00174^eSearch characters'
  ClientHeight = 209
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    618
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 618
    Height = 209
    Align = alClient
    Anchors = [akTop, akRight]
    Shape = bsFrame
    ExplicitLeft = 1
  end
  object Shape1: TShape
    Left = 66
    Top = 103
    Width = 175
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clBtnFace
  end
  object sbPinYin: TSpeedButton
    Left = 8
    Top = 8
    Width = 57
    Height = 22
    Hint = '#00175^eFilter by PinYin (chinese reading) (Ctrl-I)'
    AllowAllUp = True
    GroupIndex = 10
    Caption = '&PinYin'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbYomi: TSpeedButton
    Left = 8
    Top = 32
    Width = 57
    Height = 22
    Hint = '#00176^eFilter by japanese reading (Ctrl-Y)'
    AllowAllUp = True
    GroupIndex = 11
    Caption = 'Yomi'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbRadicals: TSpeedButton
    Left = 8
    Top = 104
    Width = 58
    Height = 22
    Hint = '#00177^eFilter by radical (Ctrl-R)'
    AllowAllUp = True
    GroupIndex = 15
    Caption = '#00178^eRadical'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbDefinition: TSpeedButton
    Left = 8
    Top = 56
    Width = 57
    Height = 22
    Hint = '#00179^eFilter by definition (meaning) (Ctrl-M)'
    AllowAllUp = True
    GroupIndex = 12
    Caption = '#00180^eDefinition'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object Bevel2: TBevel
    Left = 310
    Top = 8
    Width = 17
    Height = 193
    Anchors = [akTop, akRight]
    Shape = bsLeftLine
  end
  object sbSKIP: TSpeedButton
    Left = 8
    Top = 128
    Width = 57
    Height = 22
    Hint = '#00181^eFilter by SKIP code (see KANJIDIC for explanation)'
    AllowAllUp = True
    GroupIndex = 16
    Caption = 'SKIP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbClearFilters: TSpeedButton
    Left = 480
    Top = 178
    Width = 131
    Height = 22
    Hint = '#00182^eDisplay all characters (Ctrl-N)'
    AllowAllUp = True
    Anchors = [akTop, akRight]
    Caption = '#00183^eAll filters off'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbClearFiltersClick
  end
  object SpeedButton2: TSpeedButton
    Left = 480
    Top = 132
    Width = 131
    Height = 22
    Hint = '#00186^eDisplay only common characters'
    AllowAllUp = True
    Anchors = [akTop, akRight]
    GroupIndex = 20
    Caption = '#00187^eCommon'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object SpeedButton3: TSpeedButton
    Left = 480
    Top = 155
    Width = 131
    Height = 22
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Anchors = [akTop, akRight]
    GroupIndex = 21
    Caption = '#00189^eIn clipboard'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbStrokeCount: TSpeedButton
    Left = 8
    Top = 80
    Width = 57
    Height = 22
    Hint = 
      '#00190^eFilter by stroke count (you can search by range, ex. 1-6' +
      ')'
    AllowAllUp = True
    GroupIndex = 14
    Caption = '#00191^eStroke #'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbOther: TSpeedButton
    Left = 8
    Top = 176
    Width = 57
    Height = 22
    Hint = '#00192^eFilter by Unicode / Nelson index / Halpern index'
    AllowAllUp = True
    GroupIndex = 13
    Caption = '#00193^eOther'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbJouyou: TSpeedButton
    Left = 8
    Top = 152
    Width = 57
    Height = 22
    Hint = '#00194^eFilter by Jouyou grade (Japanese school grade)'
    AllowAllUp = True
    GroupIndex = 18
    Caption = 'Jouyou'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbPinYinClick
  end
  object sbListRadicals: TSpeedButton
    Left = 241
    Top = 104
    Width = 63
    Height = 22
    Hint = '#00177^eFilter by radical (Ctrl-R)'
    AllowAllUp = True
    Anchors = [akTop, akRight]
    Caption = '#00195^eList...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = sbListRadicalsClick
  end
  object SpeedButton9: TSpeedButton
    Left = 243
    Top = 80
    Width = 17
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '-'
    OnClick = SpeedButton9Click
  end
  object SpeedButton10: TSpeedButton
    Left = 226
    Top = 80
    Width = 17
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '+'
    OnClick = SpeedButton10Click
  end
  object SpeedButton11: TSpeedButton
    Left = 260
    Top = 80
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '-/+'
    OnClick = SpeedButton11Click
  end
  object SpeedButton13: TSpeedButton
    Left = 282
    Top = 80
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '+/-'
    OnClick = SpeedButton13Click
  end
  object SpeedButton15: TSpeedButton
    Left = 243
    Top = 152
    Width = 17
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '-'
    OnClick = SpeedButton15Click
  end
  object SpeedButton17: TSpeedButton
    Left = 226
    Top = 152
    Width = 17
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '+'
    OnClick = SpeedButton17Click
  end
  object SpeedButton21: TSpeedButton
    Left = 260
    Top = 152
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '-/+'
    OnClick = SpeedButton21Click
  end
  object SpeedButton23: TSpeedButton
    Left = 282
    Top = 152
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '+/-'
    OnClick = SpeedButton23Click
  end
  object Bevel3: TBevel
    Left = 473
    Top = 8
    Width = 17
    Height = 193
    Anchors = [akTop, akRight]
    Shape = bsLeftLine
  end
  object pbRadicals: TPaintBox
    Left = 67
    Top = 104
    Width = 173
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ParentColor = False
    OnPaint = pbRadicalsPaint
  end
  object SpeedButton19: TSpeedButton
    Left = 397
    Top = 153
    Width = 23
    Height = 22
    Hint = '#00880^eUncheck all categories'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555000000
      000055555F77777777775555000FFFFFFFF0555F777F5FFFF55755000F0F0000
      FFF05F777F7F77775557000F0F0FFFFFFFF0777F7F7F5FFFFFF70F0F0F0F0000
      00F07F7F7F7F777777570F0F0F0FFFFFFFF07F7F7F7F5FFFFFF70F0F0F0F0000
      00F07F7F7F7F777777570F0F0F0FFFFFFFF07F7F7F7F5FFF55570F0F0F0F000F
      FFF07F7F7F7F77755FF70F0F0F0FFFFF00007F7F7F7F5FF577770F0F0F0F00FF
      0F057F7F7F7F77557F750F0F0F0FFFFF00557F7F7F7FFFFF77550F0F0F000000
      05557F7F7F77777775550F0F0000000555557F7F7777777555550F0000000555
      55557F7777777555555500000005555555557777777555555555}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton19Click
  end
  object SpeedButton20: TSpeedButton
    Left = 420
    Top = 153
    Width = 23
    Height = 22
    Hint = '#00030^eEdit category'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
      000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
      00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
      F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
      0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
      FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
      FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
      0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
      00333377737FFFFF773333303300000003333337337777777333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton20Click
  end
  object SpeedButton25: TSpeedButton
    Left = 443
    Top = 153
    Width = 23
    Height = 22
    Hint = '#00031^eDelete category'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
      33333337777FF377FF3333993370739993333377FF373F377FF3399993000339
      993337777F777F3377F3393999707333993337F77737333337FF993399933333
      399377F3777FF333377F993339903333399377F33737FF33377F993333707333
      399377F333377FF3377F993333101933399377F333777FFF377F993333000993
      399377FF3377737FF7733993330009993933373FF3777377F7F3399933000399
      99333773FF777F777733339993707339933333773FF7FFF77333333999999999
      3333333777333777333333333999993333333333377777333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton25Click
  end
  object SpeedButton1: TSpeedButton
    Left = 374
    Top = 153
    Width = 23
    Height = 22
    Hint = '#00881^eAdd category'
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
      333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
      0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
      07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
      07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
      0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
      33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
      B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
      3BB33773333773333773B333333B3333333B7333333733333337}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object edtPinYin: TEdit
    Left = 64
    Top = 8
    Width = 240
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edtPinYinChange
  end
  object edtRadicals: TEdit
    Left = 64
    Top = 103
    Width = 54
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 4
    Visible = False
    OnChange = edtRadicalsChange
  end
  object edtDefinition: TEdit
    Left = 64
    Top = 56
    Width = 240
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = edtDefinitionChange
  end
  object edtSkip: TEdit
    Left = 65
    Top = 128
    Width = 239
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = edtSkipChange
  end
  object edtYomi: TEdit
    Left = 64
    Top = 32
    Width = 240
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edtYomiChange
  end
  object edtStrokeCount: TEdit
    Left = 64
    Top = 80
    Width = 160
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edtStrokeCountChange
  end
  object edtOther: TEdit
    Left = 64
    Top = 176
    Width = 91
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = edtOtherChange
  end
  object edtJouyou: TEdit
    Left = 65
    Top = 152
    Width = 160
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = edtJouyouChange
  end
  object rgSortBy: TRadioGroup
    Left = 481
    Top = 8
    Width = 129
    Height = 119
    Anchors = [akTop, akRight]
    Caption = '#00197^eSort by'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      '#00146^eRadical'
      '#00147^eStroke count'
      '#00148^eFrequency'
      '#00198^eLearner index'
      'Gakken Kanji'
      'Remembering Kanji'
      '#00149^eRandom')
    ParentFont = False
    TabOrder = 9
    OnClick = rgSortByClick
  end
  object cbOtherType: TComboBox
    Left = 155
    Top = 176
    Width = 149
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 8
    OnChange = cbOtherTypeChange
  end
  object lbCategories: TCheckListBox
    Left = 318
    Top = 9
    Width = 148
    Height = 144
    OnClickCheck = lbCategoriesClickCheck
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 10
    OnClick = lbCategoriesClick
    OnDblClick = lbCategoriesDblClick
    OnDrawItem = lbCategoriesDrawItem
  end
  object rgOrAnd: TRadioGroup
    Left = 318
    Top = 157
    Width = 53
    Height = 44
    Anchors = [akTop, akRight]
    ItemIndex = 0
    Items.Strings = (
      'OR'
      'AND')
    TabOrder = 11
    OnClick = rgOrAndClick
  end
  object cbNot: TCheckBox
    Left = 376
    Top = 183
    Width = 81
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'NOT'
    TabOrder = 12
    OnClick = rgOrAndClick
  end
end
