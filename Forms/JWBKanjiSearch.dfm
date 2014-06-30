object fKanjiSearch: TfKanjiSearch
  Left = 359
  Top = 186
  BorderStyle = bsNone
  Caption = '#00174^eSearch characters'
  ClientHeight = 206
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 584
    Top = 0
    Width = 144
    Height = 206
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      144
      206)
    object btnOnlyCommon: TSpeedButton
      Left = 4
      Top = 129
      Width = 131
      Height = 22
      Hint = '#00186^eDisplay only common characters'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 20
      Caption = '#00187^eCommon'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = sbPinYinClick
      ExplicitTop = 131
    end
    object btnInClipboard: TSpeedButton
      Left = 4
      Top = 152
      Width = 131
      Height = 22
      Hint = '#00188^eDisplay only characters in clipboard'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 21
      Caption = '#00189^eIn clipboard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = sbPinYinClick
      ExplicitTop = 154
    end
    object sbClearFilters: TSpeedButton
      Left = 4
      Top = 175
      Width = 131
      Height = 22
      Hint = '#00182^eDisplay all characters (Ctrl-N)'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      Caption = '#00183^eAll filters off'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = sbClearFiltersClick
      ExplicitTop = 177
    end
    object rgSortBy: TRadioGroup
      Left = 6
      Top = 7
      Width = 129
      Height = 117
      Anchors = [akLeft, akTop, akRight, akBottom]
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
      TabOrder = 0
      OnClick = rgSortByClick
    end
  end
  object Panel2: TPanel
    Left = 432
    Top = 0
    Width = 152
    Height = 206
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      152
      206)
    object SpeedButton1: TSpeedButton
      Left = 58
      Top = 148
      Width = 23
      Height = 22
      Hint = '#00881^eAdd category'
      Anchors = [akLeft, akBottom]
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
      ExplicitTop = 150
    end
    object SpeedButton19: TSpeedButton
      Left = 81
      Top = 148
      Width = 23
      Height = 22
      Hint = '#00880^eUncheck all categories'
      Anchors = [akLeft, akBottom]
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
      ExplicitTop = 150
    end
    object SpeedButton20: TSpeedButton
      Left = 104
      Top = 148
      Width = 23
      Height = 22
      Hint = '#00030^eEdit category'
      Anchors = [akLeft, akBottom]
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
      ExplicitTop = 150
    end
    object SpeedButton25: TSpeedButton
      Left = 127
      Top = 148
      Width = 23
      Height = 22
      Hint = '#00031^eDelete category'
      Anchors = [akLeft, akBottom]
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
      ExplicitTop = 150
    end
    object rgOrAnd: TRadioGroup
      Left = 2
      Top = 152
      Width = 53
      Height = 44
      Anchors = [akLeft, akBottom]
      ItemIndex = 0
      Items.Strings = (
        'OR'
        'AND')
      TabOrder = 0
      OnClick = rgOrAndClick
    end
    object cbNot: TCheckBox
      Left = 60
      Top = 178
      Width = 81
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'NOT'
      TabOrder = 1
      OnClick = rgOrAndClick
    end
    object lbCategories: TCheckListBox
      Left = 2
      Top = 6
      Width = 148
      Height = 142
      OnClickCheck = lbCategoriesClickCheck
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
      OnClick = lbCategoriesClick
      OnDblClick = lbCategoriesDblClick
      OnDrawItem = lbCategoriesDrawItem
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 432
    Height = 206
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 2
    DesignSize = (
      432
      206)
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
    object sbJouyou: TSpeedButton
      Left = 8
      Top = 152
      Width = 57
      Height = 22
      Hint = '#00194^eFilter by Jouyou grade (Japanese school grade)'
      AllowAllUp = True
      GroupIndex = 18
      Caption = '#00963^eJouyou'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = sbPinYinClick
    end
    object sbJouyouExpand: TSpeedButton
      Left = 379
      Top = 152
      Width = 22
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '-/+'
      OnClick = sbJouyouExpandClick
      ExplicitLeft = 260
    end
    object sbJouyouMinus: TSpeedButton
      Left = 362
      Top = 152
      Width = 17
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '-'
      OnClick = sbJouyouMinusClick
      ExplicitLeft = 243
    end
    object sbJouyouPlus: TSpeedButton
      Left = 345
      Top = 152
      Width = 17
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = sbJouyouPlusClick
      ExplicitLeft = 226
    end
    object sbJouyouShrink: TSpeedButton
      Left = 401
      Top = 152
      Width = 22
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '+/-'
      OnClick = sbJouyouShrinkClick
      ExplicitLeft = 282
    end
    object sbListRadicals: TSpeedButton
      Left = 360
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
      ExplicitLeft = 241
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
    object sbPinYin: TSpeedButton
      Left = 8
      Top = 8
      Width = 57
      Height = 22
      Hint = '#00175^eFilter by PinYin (chinese reading) (Ctrl-I)'
      AllowAllUp = True
      GroupIndex = 10
      Caption = '#00964^e&PinYin'
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
    object sbSKIP: TSpeedButton
      Left = 8
      Top = 128
      Width = 57
      Height = 22
      Hint = '#00181^eFilter by SKIP code (see KANJIDIC for explanation)'
      AllowAllUp = True
      GroupIndex = 16
      Caption = '#00962^eSKIP'
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
    object sbStrokeCountExpand: TSpeedButton
      Left = 379
      Top = 80
      Width = 22
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '-/+'
      OnClick = sbStrokeCountExpandClick
      ExplicitLeft = 260
    end
    object sbStrokeCountMinus: TSpeedButton
      Left = 362
      Top = 80
      Width = 17
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '-'
      OnClick = sbStrokeCountMinusClick
      ExplicitLeft = 243
    end
    object sbStrokeCountPlus: TSpeedButton
      Left = 345
      Top = 80
      Width = 17
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = sbStrokeCountPlusClick
      ExplicitLeft = 226
    end
    object sbStrokeCountShrink: TSpeedButton
      Left = 401
      Top = 80
      Width = 22
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '+/-'
      OnClick = sbStrokeCountShrinkClick
      ExplicitLeft = 282
    end
    object sbYomi: TSpeedButton
      Left = 8
      Top = 32
      Width = 57
      Height = 22
      Hint = '#00176^eFilter by japanese reading (Ctrl-Y)'
      AllowAllUp = True
      GroupIndex = 11
      Caption = '#00965^eYomi'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = sbPinYinClick
    end
    object cbOtherType: TComboBox
      Left = 310
      Top = 176
      Width = 113
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnChange = cbOtherTypeChange
    end
    object edtDefinition: TEdit
      Left = 64
      Top = 56
      Width = 359
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edtDefinitionChange
    end
    object edtJouyou: TEdit
      Left = 65
      Top = 152
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edtJouyouChange
    end
    object edtOther: TEdit
      Left = 64
      Top = 176
      Width = 244
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edtOtherChange
    end
    object edtPinYin: TEdit
      Left = 64
      Top = 8
      Width = 359
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edtPinYinChange
    end
    object edtSkip: TEdit
      Left = 65
      Top = 128
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = edtSkipChange
    end
    object edtStrokeCount: TEdit
      Left = 64
      Top = 80
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = edtStrokeCountChange
    end
    object edtYomi: TEdit
      Left = 64
      Top = 32
      Width = 359
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
      OnChange = edtYomiChange
    end
    object pbRadicals: TWakanPaintbox
      Left = 66
      Top = 103
      Width = 294
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      DoubleBuffered = True
      OnPaint = pbRadicalsPaint
    end
  end
end
