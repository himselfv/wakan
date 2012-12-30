object fSettings: TfSettings
  Left = 451
  Top = 131
  BorderStyle = bsDialog
  Caption = '#00397^eSettings'
  ClientHeight = 492
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcPages: TPageControl
    Left = 177
    Top = 0
    Width = 484
    Height = 451
    ActivePage = tsEditor
    Align = alClient
    MultiLine = True
    Style = tsButtons
    TabOrder = 0
    OnChange = pcPagesChange
    object tsGeneral: TTabSheet
      Caption = '#00398^eGeneral'
      ImageIndex = 7
      TabVisible = False
      object Label41: TLabel
        Left = 16
        Top = 224
        Width = 206
        Height = 13
        Caption = '#00399^eDictionary entries grid font height:'
      end
      object Label47: TLabel
        Left = 288
        Top = 336
        Width = 85
        Height = 13
        Caption = '#00400^eminutes'
      end
      object CheckBox26: TCheckBox
        Left = 16
        Top = 128
        Width = 401
        Height = 17
        Hint = 
          '#00401^eSaves statistical information to disk, filename is gener' +
          'ated by date so you can check your progress'
        Caption = '#00402^eSave statistics to disk (folder STAT)'
        TabOrder = 0
      end
      object CheckBox10: TCheckBox
        Left = 16
        Top = 152
        Width = 433
        Height = 17
        Hint = '#00403^eShows words with non-learned kanji in different color'
        Caption = '#00404^eDifferentiate words with non-learned characters'
        TabOrder = 1
      end
      object CheckBox11: TCheckBox
        Left = 16
        Top = 200
        Width = 433
        Height = 17
        Hint = 
          '#00405^eColors: green - mastered, yellow - learned, cyan - unlea' +
          'rned, red - problematic'
        Caption = '#00406^eShow word learned-status by different background color'
        TabOrder = 2
      end
      object CheckBox46: TCheckBox
        Left = 16
        Top = 312
        Width = 433
        Height = 17
        Hint = 
          '#00405^eColors: green - mastered, yellow - learned, cyan - unlea' +
          'rned, red - problematic'
        Caption = '#00407^eAuto-save user data on exit'
        TabOrder = 3
      end
      object Edit25: TEdit
        Left = 232
        Top = 224
        Width = 121
        Height = 21
        TabOrder = 4
        Text = 'Edit25'
      end
      object CheckBox49: TCheckBox
        Left = 16
        Top = 248
        Width = 417
        Height = 17
        Caption = '#00408^eLoad dictionaries on demand'
        TabOrder = 5
      end
      object CheckBox53: TCheckBox
        Left = 16
        Top = 272
        Width = 353
        Height = 17
        Caption = '#00409^eMulti-line word grids'
        TabOrder = 6
      end
      object CheckBox54: TCheckBox
        Left = 16
        Top = 336
        Width = 193
        Height = 17
        Caption = '#00410^eAuto-save user database every '
        TabOrder = 7
      end
      object Edit29: TEdit
        Left = 216
        Top = 336
        Width = 65
        Height = 21
        TabOrder = 8
        Text = 'Edit29'
      end
      object CheckBox55: TCheckBox
        Left = 16
        Top = 360
        Width = 417
        Height = 17
        Caption = '#00411^eMake backup of user file into BACKUP folder every day'
        TabOrder = 9
      end
      object RadioGroup5: TRadioGroup
        Left = 16
        Top = 12
        Width = 353
        Height = 85
        Hint = '#00432^eSelects what chinese characters are displayed'
        Caption = '#00883^eChinese character mode && character conversion'
        ItemIndex = 0
        Items.Strings = (
          '#00434^eTraditional (Big5) only'
          '#00435^eSimplified (GB2312) only'
          '#00436^eAll characters (Unicode)')
        TabOrder = 10
      end
      object CheckBox70: TCheckBox
        Left = 16
        Top = 104
        Width = 417
        Height = 17
        Caption = '^eDisplay message when word is added to vocabulary'
        TabOrder = 11
      end
    end
    object tsRomanization: TTabSheet
      Caption = '#00412^eRomanization'
      TabVisible = False
      object GroupBox6: TGroupBox
        Left = 8
        Top = 8
        Width = 457
        Height = 177
        Caption = '#00413^eRomanization of japanese'
        TabOrder = 0
        DesignSize = (
          457
          177)
        object Shape1: TShape
          Left = 16
          Top = 145
          Width = 226
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          Brush.Color = clBtnFace
        end
        object Label16: TLabel
          Left = 16
          Top = 88
          Width = 163
          Height = 13
          Caption = '#00414^eRomanization test:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Shape9: TShape
          Left = 16
          Top = 126
          Width = 226
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          Brush.Color = clBtnFace
        end
        object PaintBox3: TPaintBox
          Left = 17
          Top = 127
          Width = 224
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ParentColor = False
          OnPaint = PaintBox3Paint
        end
        object Label17: TLabel
          Left = 248
          Top = 120
          Width = 98
          Height = 13
          Caption = '#00415^eJapanese:'
        end
        object Label18: TLabel
          Left = 248
          Top = 136
          Width = 86
          Height = 13
          Caption = '#00416^eEnglish:'
        end
        object Label19: TLabel
          Left = 248
          Top = 152
          Width = 82
          Height = 13
          Caption = '#00417^eCzech:'
        end
        object Label20: TLabel
          Left = 344
          Top = 120
          Width = 46
          Height = 13
          Caption = 'Label20'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label21: TLabel
          Left = 344
          Top = 136
          Width = 46
          Height = 13
          Caption = 'Label21'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label22: TLabel
          Left = 344
          Top = 152
          Width = 46
          Height = 13
          Caption = 'Label22'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object PaintBox1: TPaintBox
          Left = 17
          Top = 146
          Width = 224
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ParentColor = False
          OnPaint = PaintBox1Paint
        end
        object RadioGroup1: TRadioGroup
          Left = 16
          Top = 20
          Width = 185
          Height = 61
          Hint = 
            '#00418^eSelect romanization system used in both input and displa' +
            'y'
          Caption = '#00419^eRomanization system'
          ItemIndex = 0
          Items.Strings = (
            '#00420^eKunreishiki romaji (japanese)'
            '#00421^eHepburn (english)'
            '#00422^eCzech')
          TabOrder = 0
          OnClick = RadioGroup1Click
        end
        object RadioGroup2: TRadioGroup
          Left = 216
          Top = 20
          Width = 225
          Height = 61
          Caption = '#00423^eShow phonetic in'
          ItemIndex = 0
          Items.Strings = (
            '#00424^eHiragana / Katakana'
            '#00425^eRomaji')
          TabOrder = 1
          OnClick = RadioGroup1Click
        end
        object Edit15: TEdit
          Left = 16
          Top = 104
          Width = 225
          Height = 21
          TabOrder = 2
          OnChange = Edit15Change
        end
      end
      object GroupBox7: TGroupBox
        Left = 8
        Top = 192
        Width = 457
        Height = 161
        Caption = '#00426^eRomanization of chinese'
        TabOrder = 1
        DesignSize = (
          457
          161)
        object Label27: TLabel
          Left = 16
          Top = 88
          Width = 163
          Height = 13
          Caption = '#00414^eRomanization test:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Shape3: TShape
          Left = 16
          Top = 126
          Width = 226
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          Brush.Color = clBtnFace
        end
        object PaintBox2: TPaintBox
          Left = 17
          Top = 127
          Width = 224
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ParentColor = False
          OnPaint = PaintBox2Paint
        end
        object Label28: TLabel
          Left = 248
          Top = 104
          Width = 33
          Height = 13
          Caption = 'PinYin:'
        end
        object Label29: TLabel
          Left = 248
          Top = 120
          Width = 58
          Height = 13
          Caption = 'Wade-Giles:'
        end
        object Label30: TLabel
          Left = 248
          Top = 136
          Width = 24
          Height = 13
          Caption = 'Yale:'
        end
        object Label31: TLabel
          Left = 344
          Top = 104
          Width = 46
          Height = 13
          Caption = 'Label20'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label32: TLabel
          Left = 344
          Top = 120
          Width = 46
          Height = 13
          Caption = 'Label21'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label33: TLabel
          Left = 344
          Top = 136
          Width = 46
          Height = 13
          Caption = 'Label22'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RadioGroup6: TRadioGroup
          Left = 16
          Top = 20
          Width = 185
          Height = 61
          Hint = 
            '#00418^eSelect romanization system used in both input and displa' +
            'y'
          Caption = '#00419^eRomanization system'
          ItemIndex = 0
          Items.Strings = (
            'PinYin'
            'Wade-Giles'
            'Yale')
          TabOrder = 0
          OnClick = RadioGroup1Click
        end
        object RadioGroup7: TRadioGroup
          Left = 216
          Top = 20
          Width = 225
          Height = 61
          Caption = '#00423^eShow phonetic in'
          ItemIndex = 0
          Items.Strings = (
            'BoPoMoFo'
            'PinYin / Wade-Giles / Yale')
          TabOrder = 1
          OnClick = RadioGroup1Click
        end
        object Edit20: TEdit
          Left = 16
          Top = 104
          Width = 225
          Height = 21
          TabOrder = 2
          OnChange = Edit20Change
        end
      end
    end
    object tsCharacterList: TTabSheet
      Caption = '#00298^eCharacter list'
      ImageIndex = 1
      TabVisible = False
      object Label35: TLabel
        Left = 16
        Top = 96
        Width = 132
        Height = 13
        Caption = '#00427^eDisplayed radical:'
      end
      object Label52: TLabel
        Left = 13
        Top = 192
        Width = 348
        Height = 13
        Caption = 
          '#00920^eHow many most frequent compounds to display in "Freq" mo' +
          'de:'
      end
      object RadioGroup3: TRadioGroup
        Left = 16
        Top = 12
        Width = 401
        Height = 77
        Caption = '#00428^eCharacter size in grid'
        ItemIndex = 0
        Items.Strings = (
          '#00429^eSmall'
          '#00430^eMedium'
          '#00431^eLarge')
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 144
        Width = 305
        Height = 17
        Caption = '#00437^eShow stroke count in grid'
        TabOrder = 1
      end
      object ComboBox1: TComboBox
        Left = 16
        Top = 112
        Width = 233
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Bushu'
          'Unicode'
          '#00438^eJapanese'
          'KanWa'
          'KangXi'
          '#00439^eKorean')
      end
      object CheckBox51: TCheckBox
        Left = 8
        Top = 384
        Width = 433
        Height = 17
        Caption = '#00440^eUse grid font for displaying stroke order'
        TabOrder = 3
        Visible = False
      end
      object CheckBox57: TCheckBox
        Left = 16
        Top = 168
        Width = 433
        Height = 17
        Caption = '#00891^eSearch by yomi can ignore okurigana'
        TabOrder = 4
      end
      object Edit34: TEdit
        Left = 336
        Top = 192
        Width = 121
        Height = 21
        TabOrder = 5
        Text = '0'
      end
    end
    object tsDictionary: TTabSheet
      Caption = '#00217^eDictionary'
      ImageIndex = 3
      TabVisible = False
      object Label26: TLabel
        Left = 16
        Top = 280
        Width = 161
        Height = 13
        Caption = '#00441^eNot used dictionary files'
        Visible = False
      end
      object GroupBox3: TGroupBox
        Left = 16
        Top = 16
        Width = 441
        Height = 137
        Caption = '#00442^eSearch results order'
        TabOrder = 0
        object CheckBox4: TCheckBox
          Left = 8
          Top = 16
          Width = 401
          Height = 17
          Caption = '#00443^ePrefer words in user dictionary'
          TabOrder = 0
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 40
          Width = 401
          Height = 17
          Caption = '#00444^ePrefer nouns and verbs'
          TabOrder = 1
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 64
          Width = 385
          Height = 17
          Caption = '#00445^ePrefer polite words'
          TabOrder = 2
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 88
          Width = 377
          Height = 17
          Caption = '#00446^ePrefer popular words (marked by "pop")'
          TabOrder = 3
        end
        object CheckBox59: TCheckBox
          Left = 8
          Top = 112
          Width = 417
          Height = 17
          Caption = '#00921^eOrder by word frequency (where available)'
          TabOrder = 4
        end
      end
      object CheckBox8: TCheckBox
        Left = 16
        Top = 160
        Width = 433
        Height = 17
        Caption = '#00447^eReplace kanji with kana for words marked "kana"'
        TabOrder = 1
      end
      object Edit19: TEdit
        Left = 16
        Top = 296
        Width = 441
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 2
        Text = 'Edit19'
        Visible = False
      end
      object CheckBox12: TCheckBox
        Left = 16
        Top = 184
        Width = 433
        Height = 17
        Caption = '#00448^eDisplay only results that fit one page in "Auto" mode'
        TabOrder = 3
      end
      object CheckBox50: TCheckBox
        Left = 16
        Top = 208
        Width = 433
        Height = 17
        Caption = '#00449^eAuto-switch to Examples panel'
        TabOrder = 4
      end
      object CheckBox58: TCheckBox
        Left = 16
        Top = 232
        Width = 441
        Height = 17
        Caption = '#00922^eDisplay word count for each word'
        TabOrder = 5
      end
    end
    object tsFonts: TTabSheet
      Caption = '#00450^eFonts'
      ImageIndex = 2
      TabVisible = False
      object Label4: TLabel
        Left = 8
        Top = 367
        Width = 107
        Height = 13
        Caption = '#00923^eEnglish font:'
      end
      object SpeedButton4: TSpeedButton
        Left = 376
        Top = 367
        Width = 73
        Height = 23
        Caption = '#00452^eChoose'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButton4Click
      end
      object Label5: TLabel
        Left = 8
        Top = 341
        Width = 134
        Height = 13
        Caption = '#00456^eFont for small text:'
      end
      object SpeedButton5: TSpeedButton
        Left = 376
        Top = 341
        Width = 73
        Height = 23
        Caption = '#00452^eChoose'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButton5Click
      end
      object Label51: TLabel
        Left = 8
        Top = 395
        Width = 179
        Height = 13
        Caption = '#00924^eRomanization && PinYin font:'
      end
      object SpeedButton14: TSpeedButton
        Left = 376
        Top = 393
        Width = 73
        Height = 23
        Caption = '#00452^eChoose'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButton14Click
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 40
        Width = 457
        Height = 108
        Caption = '#00453^eJapanese fonts'
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 172
          Height = 13
          Caption = '#00454^eFont for characters in grid:'
        end
        object SpeedButton1: TSpeedButton
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
          OnClick = SpeedButton1Click
        end
        object Label2: TLabel
          Left = 8
          Top = 51
          Width = 158
          Height = 13
          Caption = '#00455^eFont for big characters:'
        end
        object SpeedButton2: TSpeedButton
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
          OnClick = SpeedButton2Click
        end
        object Label50: TLabel
          Left = 8
          Top = 78
          Width = 166
          Height = 13
          Caption = '#00875^eStroke order display font:'
        end
        object SpeedButton13: TSpeedButton
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
          OnClick = SpeedButton13Click
        end
        object Edit1: TEdit
          Left = 240
          Top = 24
          Width = 129
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = 'MS Mincho'
        end
        object Edit2: TEdit
          Left = 240
          Top = 51
          Width = 129
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
          Text = 'MS Mincho'
        end
        object Edit32: TEdit
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
      object Edit4: TEdit
        Left = 248
        Top = 367
        Width = 129
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
        Text = 'Arial'
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 153
        Width = 457
        Height = 178
        Caption = '#00457^eChinese fonts:'
        TabOrder = 2
        object Label6: TLabel
          Left = 8
          Top = 24
          Width = 241
          Height = 13
          Caption = '#00458^eBig5 font for traditional characters in grid:'
        end
        object SpeedButton6: TSpeedButton
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
          OnClick = SpeedButton6Click
        end
        object Label7: TLabel
          Left = 8
          Top = 76
          Width = 227
          Height = 13
          Caption = '#00459^eBig5 font for big traditional characters:'
        end
        object SpeedButton7: TSpeedButton
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
          OnClick = SpeedButton7Click
        end
        object Label8: TLabel
          Left = 8
          Top = 145
          Width = 158
          Height = 13
          Caption = '#00460^eComplete unicode font:'
        end
        object SpeedButton8: TSpeedButton
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
          OnClick = SpeedButton8Click
        end
        object Label3: TLabel
          Left = 8
          Top = 50
          Width = 256
          Height = 13
          Caption = '#00461^eGB2312 font for simplified characters in grid:'
        end
        object SpeedButton3: TSpeedButton
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
          OnClick = SpeedButton3Click
        end
        object Label9: TLabel
          Left = 8
          Top = 102
          Width = 242
          Height = 13
          Caption = '#00462^eGB2312 font for big simplified characters:'
        end
        object SpeedButton9: TSpeedButton
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
          OnClick = SpeedButton9Click
        end
        object Label10: TLabel
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
        object Edit6: TEdit
          Left = 240
          Top = 24
          Width = 129
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = 'MingLiU'
        end
        object Edit7: TEdit
          Left = 240
          Top = 76
          Width = 129
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
          Text = 'MingLiU'
        end
        object Edit8: TEdit
          Left = 240
          Top = 145
          Width = 129
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
          Text = 'MingLiU'
        end
        object Edit3: TEdit
          Left = 240
          Top = 50
          Width = 129
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
          Text = 'SimSun'
        end
        object Edit9: TEdit
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
      object Button5: TButton
        Left = 8
        Top = 8
        Width = 457
        Height = 25
        Caption = '#00464^eSelect recommended fonts'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = Button5Click
      end
      object Edit5: TEdit
        Left = 248
        Top = 341
        Width = 129
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 4
        Text = 'MS Gothic'
      end
      object Edit33: TEdit
        Left = 248
        Top = 393
        Width = 129
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 5
        Text = 'Arial'
      end
    end
    object tsWordListPrinting: TTabSheet
      Caption = '#00465^eWord list printing'
      ImageIndex = 4
      TabVisible = False
      object Label11: TLabel
        Left = 16
        Top = 344
        Width = 202
        Height = 13
        Caption = '#00466^eNumber of lines on square page:'
      end
      object CheckBox14: TCheckBox
        Left = 16
        Top = 296
        Width = 241
        Height = 17
        Caption = '#00467^ePrint horizontal lines'
        TabOrder = 0
      end
      object CheckBox15: TCheckBox
        Left = 224
        Top = 296
        Width = 241
        Height = 17
        Caption = '#00468^ePrint vertical lines'
        TabOrder = 1
      end
      object CheckBox16: TCheckBox
        Left = 16
        Top = 320
        Width = 241
        Height = 17
        Caption = '#00469^eVary colors'
        TabOrder = 2
      end
      object CheckBox17: TCheckBox
        Left = 224
        Top = 320
        Width = 313
        Height = 17
        Caption = '#00470^eDo not print unlearned kanji'
        TabOrder = 3
      end
      object GroupBox4: TGroupBox
        Left = 16
        Top = 8
        Width = 433
        Height = 281
        Caption = '#00471^eColumns'
        TabOrder = 4
        object Label23: TLabel
          Left = 16
          Top = 248
          Width = 113
          Height = 13
          Caption = '#00472^eUser settings:'
        end
        object ListBox1: TListBox
          Left = 16
          Top = 24
          Width = 401
          Height = 217
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 14
          Items.Strings = (
            '#00473^ePhonetic            |Meaning'
            '#00474^eWritten             |Meaning'
            '#00475^ePhonetic |Written   |Meaning'
            '#00476^eWritten  |Phonetic  |Meaning'
            '#00477^eRomaji   |Kana      |Meaning'
            '#00478^ePhonetic1           |Phonetic2'
            '#00479^eWritten1            |Written2'
            '#00480^eMeaning1            |Meaning2'
            '#00481^ePhonetic1|Phonetic2 |Phonetic3|Phonetic4'
            '#00482^eWritten1 |Written2  |Written3 |Written4'
            '#00483^eMeaning1 |Meaning2  |Meaning3 |Meaning4'
            '#00484^ePhonetic1|Written1  |Phonetic2|Written2'
            '#00485^ePhonetic1|Meaning1  |Phonetic2|Meaning2'
            '#00486^eWritten1 |Meaning1  |Written2 |Meaning2'
            '#00487^eUser settings')
          ParentFont = False
          TabOrder = 0
          OnClick = ListBox1Click
        end
        object Edit16: TEdit
          Left = 144
          Top = 248
          Width = 273
          Height = 21
          TabOrder = 1
          Text = 'Edit16'
        end
      end
      object Edit10: TEdit
        Left = 16
        Top = 360
        Width = 161
        Height = 21
        TabOrder = 5
        Text = 'Edit10'
      end
    end
    object tsCharacterCardPrinting: TTabSheet
      Caption = '#00488^eCharacter cards printing'
      ImageIndex = 5
      TabVisible = False
      object Label12: TLabel
        Left = 8
        Top = 8
        Width = 267
        Height = 13
        Caption = '#00489^eVertical number of characters on square page:'
      end
      object Label13: TLabel
        Left = 8
        Top = 56
        Width = 281
        Height = 13
        Caption = '#00952^eSpace for vocabulary compounds (in characters):'
      end
      object Label14: TLabel
        Left = 8
        Top = 104
        Width = 250
        Height = 13
        Caption = '#00953^eVertical number of vocabulary compounds:'
      end
      object Label15: TLabel
        Left = 8
        Top = 152
        Width = 124
        Height = 13
        Caption = '#00492^eCalligraphy font:'
      end
      object SpeedButton10: TSpeedButton
        Left = 136
        Top = 168
        Width = 73
        Height = 23
        Caption = '#00452^eChoose'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButton10Click
      end
      object Label53: TLabel
        Left = 208
        Top = 104
        Width = 175
        Height = 13
        Caption = '#00951^eNumber of full compounds:'
      end
      object Edit11: TEdit
        Left = 8
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Edit11'
      end
      object Edit12: TEdit
        Left = 8
        Top = 72
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'Edit12'
      end
      object Edit13: TEdit
        Left = 8
        Top = 120
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'Edit13'
      end
      object CheckBox18: TCheckBox
        Left = 8
        Top = 200
        Width = 217
        Height = 17
        Caption = '#00493^ePrint vocabulary compounds'
        TabOrder = 3
      end
      object CheckBox19: TCheckBox
        Left = 232
        Top = 224
        Width = 209
        Height = 17
        Caption = '#00494^ePrint radical'
        TabOrder = 4
      end
      object CheckBox20: TCheckBox
        Left = 232
        Top = 248
        Width = 225
        Height = 17
        Caption = '#00495^ePrint alternate form'
        TabOrder = 5
      end
      object CheckBox21: TCheckBox
        Left = 8
        Top = 272
        Width = 257
        Height = 17
        Caption = '#00496^ePrint outer lines'
        TabOrder = 6
      end
      object CheckBox22: TCheckBox
        Left = 8
        Top = 224
        Width = 209
        Height = 17
        Caption = '#00497^ePrint readings'
        TabOrder = 7
      end
      object CheckBox23: TCheckBox
        Left = 8
        Top = 296
        Width = 225
        Height = 17
        Caption = '#00498^ePrint inner lines'
        TabOrder = 8
      end
      object Edit14: TEdit
        Left = 8
        Top = 168
        Width = 129
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 9
        Text = 'MingLiU'
      end
      object CheckBox24: TCheckBox
        Left = 8
        Top = 320
        Width = 241
        Height = 17
        Caption = '#00499^ePrint vertically (right to left)'
        TabOrder = 10
      end
      object CheckBox25: TCheckBox
        Left = 8
        Top = 344
        Width = 241
        Height = 17
        Caption = '#00500^eLeave space between columns'
        TabOrder = 11
      end
      object CheckBox44: TCheckBox
        Left = 8
        Top = 248
        Width = 217
        Height = 17
        Caption = '#00501^ePrint definition'
        TabOrder = 12
      end
      object CheckBox45: TCheckBox
        Left = 232
        Top = 272
        Width = 217
        Height = 17
        Caption = '#00502^ePrint stroke count'
        TabOrder = 13
      end
      object CheckBox52: TCheckBox
        Left = 232
        Top = 296
        Width = 225
        Height = 17
        Caption = '#00503^ePrint stroke order'
        TabOrder = 14
      end
      object CheckBox62: TCheckBox
        Left = 232
        Top = 200
        Width = 217
        Height = 17
        Caption = '#00954^ePrint full compounds'
        TabOrder = 15
      end
      object CheckBox63: TCheckBox
        Left = 232
        Top = 320
        Width = 217
        Height = 17
        Caption = '#00956^eSort compounds by frequency'
        TabOrder = 16
      end
      object Edit35: TEdit
        Left = 208
        Top = 120
        Width = 121
        Height = 21
        TabOrder = 17
        Text = 'Edit35'
      end
    end
    object tsDatabaseMaintenance: TTabSheet
      Caption = '#00504^eDatabase maintenance'
      ImageIndex = 6
      TabVisible = False
      object Label48: TLabel
        Left = 224
        Top = 176
        Width = 105
        Height = 13
        Caption = 'Filename (without ext):'
      end
      object Label49: TLabel
        Left = 336
        Top = 176
        Width = 114
        Height = 13
        Caption = 'Category (with group id):'
      end
      object Button2: TButton
        Left = 16
        Top = 376
        Width = 441
        Height = 25
        Caption = '#00505^eCheck dictionary database indexes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        Visible = False
        OnClick = Button2Click
      end
      object Button1: TButton
        Left = 16
        Top = 8
        Width = 441
        Height = 25
        Caption = '#00506^eExport user database into delimited text file'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button3: TButton
        Left = 16
        Top = 40
        Width = 441
        Height = 25
        Caption = '#00507^eImport user database from delimited text file'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 16
        Top = 88
        Width = 441
        Height = 25
        Caption = '#00508^eCheck user dictionary categories'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button11: TButton
        Left = 16
        Top = 136
        Width = 441
        Height = 25
        Caption = '#00509^eShow memory allocation stats'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = Button11Click
      end
      object Button13: TButton
        Left = 16
        Top = 176
        Width = 201
        Height = 25
        Caption = 'Import .UNI (Unicode 16-bit) as .WKL'
        TabOrder = 5
        OnClick = Button13Click
      end
      object Edit30: TEdit
        Left = 336
        Top = 192
        Width = 121
        Height = 21
        TabOrder = 6
        Text = 'LJLPT 4'
      end
      object Edit31: TEdit
        Left = 224
        Top = 192
        Width = 105
        Height = 21
        TabOrder = 7
        Text = 'JLPT4'
      end
    end
    object tsTextTranslator: TTabSheet
      Caption = '#00300^eText translator'
      ImageIndex = 8
      TabVisible = False
      object sbTextTranslator: TScrollBox
        Left = 0
        Top = 0
        Width = 476
        Height = 441
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object Label25: TLabel
          Left = 12
          Top = 250
          Width = 224
          Height = 13
          Caption = '#00510^eNumber of lines reserved to meaning:'
        end
        object GroupBox5: TGroupBox
          Left = 12
          Top = 274
          Width = 449
          Height = 105
          Caption = '#00511^ePrinting'
          TabOrder = 0
          object Label24: TLabel
            Left = 16
            Top = 72
            Width = 202
            Height = 13
            Caption = '#00512^eNumber of lines on square page:'
          end
          object cbPrintReading: TCheckBox
            Left = 16
            Top = 24
            Width = 193
            Height = 17
            Caption = '#00272^eDisplay reading'
            TabOrder = 0
          end
          object cbPrintMeaning: TCheckBox
            Left = 232
            Top = 24
            Width = 209
            Height = 17
            Caption = '#00513^eDisplay meaning'
            TabOrder = 1
          end
          object cbNoPrintColors: TCheckBox
            Left = 16
            Top = 48
            Width = 265
            Height = 17
            Caption = '#00514^eDo not use colors'
            TabOrder = 2
          end
          object cbVerticalPrint: TCheckBox
            Left = 232
            Top = 48
            Width = 377
            Height = 17
            Caption = '#00515^ePrint vertically in columns'
            TabOrder = 3
          end
          object Edit18: TEdit
            Left = 216
            Top = 72
            Width = 121
            Height = 21
            TabOrder = 4
            Text = '20'
          end
        end
        object cbDisplayLines: TCheckBox
          Left = 12
          Top = 34
          Width = 401
          Height = 17
          Caption = '#00516^eDisplay lines'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbNoMeaningLearned: TCheckBox
          Left = 12
          Top = 58
          Width = 353
          Height = 17
          Caption = '#00517^eDo not display meaning in learned words'
          TabOrder = 2
        end
        object cbNoSearchParticles: TCheckBox
          Left = 12
          Top = 130
          Width = 353
          Height = 17
          Caption = '#00518^eDo not search for particles'
          TabOrder = 3
        end
        object cbNoReadingLearned: TCheckBox
          Left = 12
          Top = 82
          Width = 377
          Height = 17
          Caption = '#00519^eDo not display reading of learned kanji'
          TabOrder = 4
        end
        object CheckBox36: TCheckBox
          Left = 12
          Top = 106
          Width = 393
          Height = 17
          Caption = '#00884^eDisplay transcript above kana'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object cbNoTranslateHiragana: TCheckBox
          Left = 12
          Top = 154
          Width = 377
          Height = 17
          Caption = '#00521^eDo not translate hiragana-only words'
          TabOrder = 6
        end
        object cbUserBold: TCheckBox
          Left = 12
          Top = 202
          Width = 401
          Height = 17
          Caption = '#00522^eDisplay words with user-defined translation in bold'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object Edit17: TEdit
          Left = 228
          Top = 250
          Width = 121
          Height = 21
          TabOrder = 8
          Text = '2'
        end
        object CheckBox42: TCheckBox
          Left = 12
          Top = 178
          Width = 225
          Height = 17
          Caption = '#00523^eLeave space between lines'
          Checked = True
          State = cbChecked
          TabOrder = 9
        end
        object CheckBox43: TCheckBox
          Left = 12
          Top = 10
          Width = 433
          Height = 17
          Caption = '#00524^eBreak lines only at word boundaries'
          Checked = True
          State = cbChecked
          TabOrder = 10
        end
        object CheckBox27: TCheckBox
          Left = 356
          Top = 250
          Width = 97
          Height = 17
          Caption = '#00525^eDouble size'
          TabOrder = 11
        end
        object CheckBox41: TCheckBox
          Left = 170
          Top = 35
          Width = 401
          Height = 17
          Caption = '#00526^eDisplay non-japanese chars in grey'
          Checked = True
          State = cbChecked
          TabOrder = 12
          Visible = False
        end
        object CheckBox2: TCheckBox
          Left = 12
          Top = 226
          Width = 401
          Height = 17
          Caption = '#00527^eShow editor hint'
          Checked = True
          State = cbChecked
          TabOrder = 13
        end
        object CheckBox13: TCheckBox
          Left = 228
          Top = 226
          Width = 225
          Height = 17
          Caption = '#00528^eDisplay meaning on hint'
          Checked = True
          State = cbChecked
          TabOrder = 14
        end
        object CheckBox56: TCheckBox
          Left = 244
          Top = 178
          Width = 217
          Height = 17
          Caption = '#00529^eLeave space even when reading is off'
          TabOrder = 15
        end
        object cbTranslateNoLongTextWarning: TCheckBox
          Left = 10
          Top = 388
          Width = 457
          Height = 17
          Caption = '^eDo not warn when translating large text blocks'
          TabOrder = 16
        end
        object cbMultithreadedTranslation: TCheckBox
          Left = 10
          Top = 410
          Width = 457
          Height = 17
          Caption = '^eMultithreaded translation'
          TabOrder = 17
        end
      end
    end
    object tsCharacterDetails: TTabSheet
      Caption = '#00530^eCharacter details'
      ImageIndex = 9
      TabVisible = False
      DesignSize = (
        476
        441)
      object Label34: TLabel
        Left = 8
        Top = 8
        Width = 125
        Height = 13
        Caption = '#00531^eDisplayed items:'
      end
      object SpeedButton11: TSpeedButton
        Left = 453
        Top = 98
        Width = 23
        Height = 79
        Hint = '#00532^eMove up'
        Anchors = [akTop, akRight]
        Enabled = False
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
          3333333333777F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
          3333333777737777F333333099999990333333373F3333373333333309999903
          333333337F33337F33333333099999033333333373F333733333333330999033
          3333333337F337F3333333333099903333333333373F37333333333333090333
          33333333337F7F33333333333309033333333333337373333333333333303333
          333333333337F333333333333330333333333333333733333333}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton11Click
        ExplicitLeft = 448
      end
      object SpeedButton12: TSpeedButton
        Left = 453
        Top = 234
        Width = 23
        Height = 79
        Hint = '#00533^eMove down'
        Anchors = [akTop, akRight]
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
          333333333337F33333333333333033333333333333373F333333333333090333
          33333333337F7F33333333333309033333333333337373F33333333330999033
          3333333337F337F33333333330999033333333333733373F3333333309999903
          333333337F33337F33333333099999033333333373333373F333333099999990
          33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333309033333333333337F7F333333333333090333
          33333333337F7F33333333333300033333333333337773333333}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton12Click
        ExplicitLeft = 448
      end
      object ListBox2: TListBox
        Left = 8
        Top = 24
        Width = 443
        Height = 323
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox2Click
      end
      object Button7: TButton
        Left = 8
        Top = 354
        Width = 113
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00534^eAdd...'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 180
        Top = 354
        Width = 113
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00535^eChange...'
        TabOrder = 2
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 350
        Top = 354
        Width = 97
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00536^eDelete'
        TabOrder = 3
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 296
        Top = 1
        Width = 150
        Height = 22
        Caption = '#00537^eReset to default'
        TabOrder = 4
        OnClick = Button10Click
      end
    end
    object tsPopupTool: TTabSheet
      Caption = '#00538^ePopup tool'
      ImageIndex = 10
      TabVisible = False
      object Label36: TLabel
        Left = 8
        Top = 80
        Width = 159
        Height = 13
        Caption = '#00539^ePopup delay (x100 ms):'
      end
      object Label37: TLabel
        Left = 8
        Top = 112
        Width = 164
        Height = 13
        Caption = '#00540^eText scan range (pixels):'
      end
      object Label38: TLabel
        Left = 160
        Top = 112
        Width = 67
        Height = 13
        Caption = '#00541^eLeft'
      end
      object Label39: TLabel
        Left = 312
        Top = 112
        Width = 74
        Height = 13
        Caption = '#00542^eRight'
      end
      object Label40: TLabel
        Left = 8
        Top = 144
        Width = 207
        Height = 13
        Caption = '#00543^eMax. number of dictionary entries:'
      end
      object Label43: TLabel
        Left = 8
        Top = 176
        Width = 173
        Height = 13
        Caption = '#00544^eCharacter card size factor:'
      end
      object Label44: TLabel
        Left = 8
        Top = 208
        Width = 455
        Height = 13
        Caption = 
          '#00545^eNumber of characters reserved for compounds on a card (a' +
          'ffects overall popup width):'
      end
      object Label45: TLabel
        Left = 104
        Top = 224
        Width = 93
        Height = 13
        Caption = '#00546^eMinimum:'
      end
      object Label46: TLabel
        Left = 264
        Top = 224
        Width = 96
        Height = 13
        Caption = '#00547^eMaximum:'
      end
      object CheckBox28: TCheckBox
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = '#00548^eShow translation for japanese/chinese text'
        TabOrder = 0
      end
      object CheckBox47: TCheckBox
        Left = 8
        Top = 32
        Width = 353
        Height = 17
        Caption = '#00549^eShow translation for english text (only for screen)'
        TabOrder = 1
      end
      object CheckBox48: TCheckBox
        Left = 8
        Top = 56
        Width = 361
        Height = 17
        Caption = '#00550^eShow character details'
        TabOrder = 2
      end
      object Edit21: TEdit
        Left = 160
        Top = 80
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'Edit21'
      end
      object Edit22: TEdit
        Left = 224
        Top = 112
        Width = 81
        Height = 21
        TabOrder = 4
        Text = 'Edit22'
      end
      object Edit23: TEdit
        Left = 376
        Top = 112
        Width = 81
        Height = 21
        TabOrder = 5
        Text = 'Edit23'
      end
      object Edit24: TEdit
        Left = 224
        Top = 144
        Width = 121
        Height = 21
        TabOrder = 6
        Text = 'Edit24'
      end
      object Edit26: TEdit
        Left = 224
        Top = 176
        Width = 121
        Height = 21
        TabOrder = 7
        Text = 'Edit26'
      end
      object Edit27: TEdit
        Left = 192
        Top = 224
        Width = 65
        Height = 21
        TabOrder = 8
        Text = 'Edit27'
      end
      object Edit28: TEdit
        Left = 344
        Top = 224
        Width = 65
        Height = 21
        TabOrder = 9
        Text = 'Edit28'
      end
    end
    object tsColors: TTabSheet
      Caption = '#00551^eColors'
      ImageIndex = 11
      TabVisible = False
      DesignSize = (
        476
        441)
      object Shape2: TShape
        Left = 8
        Top = 347
        Width = 105
        Height = 25
        Anchors = [akLeft, akBottom]
        ExplicitTop = 332
      end
      object Label42: TLabel
        Left = 8
        Top = 8
        Width = 91
        Height = 13
        Caption = '#00552^eCategory'
      end
      object ListBox3: TListBox
        Left = 8
        Top = 72
        Width = 457
        Height = 272
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox3Click
      end
      object Button12: TButton
        Left = 120
        Top = 347
        Width = 217
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00553^eChange'
        TabOrder = 1
        OnClick = Button12Click
      end
      object Button14: TButton
        Left = 344
        Top = 347
        Width = 121
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00554^eRevert to default'
        TabOrder = 2
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 336
        Top = 8
        Width = 129
        Height = 25
        Caption = '#00555^eSet all to default'
        TabOrder = 3
        OnClick = Button15Click
      end
      object ComboBox2: TComboBox
        Left = 8
        Top = 24
        Width = 313
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = ComboBox2Change
        Items.Strings = (
          '#00298^eCharacter list'
          '#00556^eWord grids'
          '#00557^eWord markers'
          '#00558^eEditor & translator'
          '#00559^ePopup tool')
      end
      object CheckBox3: TCheckBox
        Left = 8
        Top = 49
        Width = 457
        Height = 17
        Caption = 
          '#00560^eDo not use colors in character list, use Windows default' +
          ' colors instead'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckBox3Click
      end
      object cbNoColors: TCheckBox
        Left = 8
        Top = 49
        Width = 457
        Height = 17
        Caption = 
          '#00561^eDo not use colors in editor, use Windows default colors ' +
          'instead'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = cbNoColorsClick
      end
      object CheckBox9: TCheckBox
        Left = 8
        Top = 50
        Width = 433
        Height = 17
        Hint = 
          '#00562^eColors: black - common, grey - uncommon, blue - learned,' +
          ' green - only in names'
        Caption = 
          '#00563^eDo not use colors in word grids, use Windows default col' +
          'ors instead'
        TabOrder = 7
        OnClick = CheckBox9Click
      end
    end
    object tsEditor: TTabSheet
      Caption = '#00966^eEditor'
      ImageIndex = 12
      TabVisible = False
      object lblAozoraRuby: TLabel
        Left = 8
        Top = 109
        Width = 134
        Height = 13
        Caption = '#00972^eAozora Ruby:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSavingAndLoading: TLabel
        Left = 8
        Top = 13
        Width = 171
        Height = 13
        Caption = '#00968^eSaving and loading:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSaveAnnotationsToRubyDesc: TLabel
        Left = 16
        Top = 218
        Width = 352
        Height = 13
        Caption = 
          '#00977^eIf not, you'#39'll have the option of choosing that when you' +
          ' save file.'
      end
      object lblAozoraTagsInColor: TLabel
        Left = 16
        Top = 174
        Width = 211
        Height = 13
        Caption = '#00975^eYou can set colors on Colors page'
      end
      object Label54: TLabel
        Left = 8
        Top = 245
        Width = 88
        Height = 13
        Caption = '#00978^eMisc:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object CheckBox60: TCheckBox
        Left = 8
        Top = 32
        Width = 457
        Height = 17
        Caption = '#00969^eAuto-save file on exit'
        TabOrder = 0
      end
      object CheckBox61: TCheckBox
        Left = 8
        Top = 56
        Width = 457
        Height = 17
        Caption = '#00970^eAuto-load last opened file on start'
        TabOrder = 1
      end
      object cbNoSaveChangesWarning: TCheckBox
        Left = 8
        Top = 80
        Width = 457
        Height = 17
        Caption = 
          '#00971^eDo not ask to save changes on exit (WARNING: Use at your' +
          ' own risk)'
        TabOrder = 2
      end
      object cbLoadAozoraRuby: TCheckBox
        Left = 8
        Top = 128
        Width = 457
        Height = 17
        Caption = '#00973^eShow Aozora-Ruby as annotations (requires text reload)'
        TabOrder = 3
      end
      object cbAozoraTagsInColor: TCheckBox
        Left = 8
        Top = 151
        Width = 457
        Height = 17
        Caption = '#00974^eShow Aozora-Ruby tags in color'
        TabOrder = 4
      end
      object cbSaveAnnotationsToRuby: TCheckBox
        Left = 8
        Top = 195
        Width = 457
        Height = 17
        Caption = '#00976^eSave annotations as Aozora-Ruby tags by default'
        TabOrder = 5
      end
      object cbAdjustCharPriorities: TCheckBox
        Left = 8
        Top = 264
        Width = 457
        Height = 17
        Caption = '#00979^eAdjust character priorities based on editor usage'
        TabOrder = 6
      end
    end
    object tsAnnotations: TTabSheet
      Caption = '#00967^eAnnotations'
      ImageIndex = 13
      TabVisible = False
      object Bevel1: TBevel
        Left = 8
        Top = 40
        Width = 457
        Height = 17
        Shape = bsTopLine
      end
      object CheckBox64: TCheckBox
        Left = 11
        Top = 16
        Width = 302
        Height = 17
        Caption = '^eEnable annotations (ANNOTATE.PKG)'
        TabOrder = 0
      end
      object CheckBox65: TCheckBox
        Left = 11
        Top = 54
        Width = 294
        Height = 17
        Caption = 
          '^eRebuild ANNOTATE.PKG from .ANO files if needed whenever WaKan ' +
          'starts'
        TabOrder = 1
      end
      object CheckBox66: TCheckBox
        Left = 11
        Top = 150
        Width = 326
        Height = 17
        Caption = '^eAllow annotations to play sound files'
        TabOrder = 2
        Visible = False
      end
      object CheckBox67: TCheckBox
        Left = 11
        Top = 78
        Width = 326
        Height = 17
        Caption = '^eAllow annotations to display pictures'
        TabOrder = 3
      end
      object CheckBox68: TCheckBox
        Left = 11
        Top = 102
        Width = 326
        Height = 17
        Caption = '^eAllow annotations to display web pages'
        TabOrder = 4
      end
      object CheckBox69: TCheckBox
        Left = 11
        Top = 126
        Width = 326
        Height = 17
        Caption = '^eAllow annotations to change foreground color'
        TabOrder = 5
      end
      object Button16: TButton
        Left = 72
        Top = 352
        Width = 337
        Height = 25
        Caption = '^eHelp for the annotation feature'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
        OnClick = Button16Click
      end
    end
  end
  object lbContents: TListBox
    Left = 0
    Top = 0
    Width = 177
    Height = 451
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbContentsClick
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 451
    Width = 661
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      661
      41)
    object Button6: TButton
      Left = 12
      Top = 12
      Width = 177
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '#00929^eChange language'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button6Click
    end
    object btnOk: TBitBtn
      Left = 520
      Top = 8
      Width = 130
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOkClick
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
      NumGlyphs = 2
    end
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '.wbk'
    Filter = 'WaKan User Data Backup|*.wbk'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 528
    Top = 304
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = '.wbk'
    Filter = 'WaKan User Data Bakup|*.wbk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 600
    Top = 304
  end
  object ColorDialog1: TColorDialog
    Left = 148
    Top = 352
  end
end
