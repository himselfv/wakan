object fSettings: TfSettings
  Left = 451
  Top = 131
  BorderStyle = bsDialog
  Caption = '#00397^eSettings^cNastaven'#1085
  ClientHeight = 558
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnShow = FormShow
  DesignSize = (
    497
    558)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 479
    Height = 508
    ActivePage = TabSheet13
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 0
    object TabSheet8: TTabSheet
      Caption = '#00398^eGeneral^cObecn'#1081
      ImageIndex = 7
      object Label41: TLabel
        Left = 16
        Top = 224
        Width = 390
        Height = 13
        Caption = 
          '#00399^eDictionary entries grid font height:^cVelikost p'#1085'sma v m' +
          #1096#1085#1115'ce slovn'#1085'ku:'
      end
      object Label47: TLabel
        Left = 288
        Top = 336
        Width = 122
        Height = 13
        Caption = '#00400^eminutes^cminut'
      end
      object CheckBox26: TCheckBox
        Left = 16
        Top = 128
        Width = 401
        Height = 17
        Hint = 
          '#00401^eSaves statistical information to disk, filename is gener' +
          'ated by date so you can check your progress^cUlo'#1115#1085' na disk '#1098'daje' +
          ' o statistice podle aktu'#1073'ln'#1085'ho data tak'#1115'e m'#1097#1115'ete sledovat v'#1101'voj ' +
          'statistiky'
        Caption = 
          '#00402^eSave statistics to disk (folder STAT)^cUkl'#1073'dat statistik' +
          'u na disk (slo'#1115'ka STAT)'
        TabOrder = 0
      end
      object CheckBox10: TCheckBox
        Left = 16
        Top = 152
        Width = 433
        Height = 17
        Hint = 
          '#00403^eShows words with non-learned kanji in different color^cZ' +
          'obrazuje slova s nenau'#1080'en'#1101'mi znaky odli'#1113'nou barvou'
        Caption = 
          '#00404^eDifferentiate words with non-learned characters^cOdli'#1113'ov' +
          'at slova s nenau'#1080'en'#1101'mi znaky'
        TabOrder = 1
      end
      object CheckBox11: TCheckBox
        Left = 16
        Top = 200
        Width = 433
        Height = 17
        Hint = 
          '#00405^eColors: green - mastered, yellow - learned, cyan - unlea' +
          'rned, red - problematic^cBarvy: zelen'#1073' - dob'#1096'e nau'#1080'eno, '#1115'lut'#1073' - ' +
          'nau'#1080'eno, modr'#1073' - nenau'#1080'eno, '#1080'erven'#1073' - problematick'#1081
        Caption = 
          '#00406^eShow word learned-status by different background color^c' +
          'Zobrazit nau'#1080'enost slov pomoc'#1085' r'#1097'zn'#1101'ch barev pozad'#1085
        TabOrder = 2
      end
      object CheckBox46: TCheckBox
        Left = 16
        Top = 312
        Width = 433
        Height = 17
        Hint = 
          '#00405^eColors: green - mastered, yellow - learned, cyan - unlea' +
          'rned, red - problematic^cBarvy: zelen'#1073' - dob'#1096'e nau'#1080'eno, '#1115'lut'#1073' - ' +
          'nau'#1080'eno, modr'#1073' - nenau'#1080'eno, '#1080'erven'#1073' - problematick'#1081
        Caption = 
          '#00407^eAuto-save user data on exit^cP'#1096'i ukon'#1080'en'#1085' automaticky ul' +
          'o'#1115'it u'#1115'ivatelsk'#1073' data'
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
        Caption = 
          '#00408^eLoad dictionaries on demand^cNahr'#1073'vat slovn'#1085'ky pouze p'#1096'i' +
          ' '#1115#1073'dosti'
        TabOrder = 5
      end
      object CheckBox53: TCheckBox
        Left = 16
        Top = 272
        Width = 353
        Height = 17
        Caption = '#00409^eMulti-line word grids^cV'#1085'ce '#1096#1073'dkov'#1081' m'#1096#1085#1115'ky slov'
        TabOrder = 6
      end
      object CheckBox54: TCheckBox
        Left = 16
        Top = 336
        Width = 193
        Height = 17
        Caption = 
          '#00410^eAuto-save user database every ^cAutomaticky ulo'#1115'it u'#1115'iv.' +
          ' datab'#1073'zi ka'#1115'd'#1101'ch'
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
        Caption = 
          '#00411^eMake backup of user file into BACKUP folder every day^cU' +
          'd'#1084'lat z'#1073'lohu u'#1115'ivatelsk'#1081'ho souboru do BACKUP adres'#1073#1096'e ka'#1115'd'#1101' den'
        TabOrder = 9
      end
      object RadioGroup5: TRadioGroup
        Left = 16
        Top = 12
        Width = 353
        Height = 85
        Hint = 
          '#00432^eSelects what chinese characters are displayed^cV'#1101'b'#1084'r typ' +
          'u '#1080#1085'nsk'#1101'ch znak'#1097', kter'#1081' jsou zobrazeny'
        Caption = 
          '#00883^eChinese character mode && character conversion^cRe'#1115'im '#1080#1085 +
          'nsk'#1101'ch znak'#1097' a konverze'
        ItemIndex = 0
        Items.Strings = (
          '#00434^eTraditional (Big5) only^cPouze tradi'#1080'n'#1085' (Big5)'
          '#00435^eSimplified (GB2312) only^cPouze zjednodu'#1113'en'#1081' (GB2312)'
          '#00436^eAll characters (Unicode)^cV'#1113'echny znaky (Unicode)')
        TabOrder = 10
      end
      object CheckBox70: TCheckBox
        Left = 16
        Top = 104
        Width = 417
        Height = 17
        Caption = 
          '^eDisplay message when word is added to vocabulary^cZobrazit zpr' +
          #1073'vu, kdy'#1115' je slov'#1085#1080'ko p'#1096'id'#1073'no do slovn'#1085'ku'
        TabOrder = 11
      end
    end
    object TabSheet1: TTabSheet
      Caption = '#00412^eRomanization^cRomanizace'
      object GroupBox6: TGroupBox
        Left = 8
        Top = 8
        Width = 457
        Height = 177
        Caption = '#00413^eRomanization of japanese^cRomanizace japon'#1113'tiny'
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
          Width = 274
          Height = 13
          Caption = '#00414^cTest romanizace:^eRomanization test:'
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
          Width = 159
          Height = 13
          Caption = '#00415^eJapanese:^cJaponsk'#1081':'
        end
        object Label18: TLabel
          Left = 248
          Top = 136
          Width = 142
          Height = 13
          Caption = '#00416^eEnglish:^cAnglick'#1081':'
        end
        object Label19: TLabel
          Left = 248
          Top = 152
          Width = 128
          Height = 13
          Caption = '#00417^eCzech:^c'#1048'esk'#1081':'
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
            'y^cV'#1101'b'#1084'r romaniza'#1080'n'#1085'ho syst'#1081'mu pro vstup i pro zobrazen'#1085
          Caption = '#00419^eRomanization system^cRomaniza'#1080'n'#1085' syst'#1081'm'
          ItemIndex = 0
          Items.Strings = (
            
              '#00420^eKunreishiki romaji (japanese)^cKunrei'#1113'iki r'#1091'mad'#1115'i (japon' +
              'sk'#1101')'
            '#00421^eHepburn (english)^cHepburn'#1097'v (anglick'#1101')'
            '#00422^eCzech^c'#1048'esk'#1101)
          TabOrder = 0
          OnClick = RadioGroup1Click
        end
        object RadioGroup2: TRadioGroup
          Left = 216
          Top = 20
          Width = 225
          Height = 61
          Caption = '#00423^eShow phonetic in^cZobraz fonetick'#1081' z'#1073'pisy v'
          ItemIndex = 0
          Items.Strings = (
            '#00424^eHiragana / Katakana^cHiragan'#1084' / Katakan'#1084
            '#00425^eRomaji^cR'#1091'mad'#1115'i')
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
        Caption = '#00426^eRomanization of chinese^cRomanizace '#1080#1085'n'#1113'tiny'
        TabOrder = 1
        DesignSize = (
          457
          161)
        object Label27: TLabel
          Left = 16
          Top = 88
          Width = 274
          Height = 13
          Caption = '#00414^cTest romanizace:^eRomanization test:'
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
            'y^cV'#1101'b'#1084'r romaniza'#1080'n'#1085'ho syst'#1081'mu pro vstup i pro zobrazen'#1085
          Caption = '#00419^eRomanization system^cRomaniza'#1080'n'#1085' syst'#1081'm'
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
          Caption = '#00423^eShow phonetic in^cZobraz fonetick'#1081' z'#1073'pisy v'
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
    object TabSheet2: TTabSheet
      Caption = '#00298^eCharacter list^cSeznam znak'#1097
      ImageIndex = 1
      object Label35: TLabel
        Left = 16
        Top = 96
        Width = 232
        Height = 13
        Caption = '#00427^eDisplayed radical:^cZobrazen'#1101' radik'#1073'l:'
      end
      object Label52: TLabel
        Left = 13
        Top = 192
        Width = 659
        Height = 13
        Caption = 
          '#00920^eHow many most frequent compounds to display in "Freq" mo' +
          'de:^cKolik nejfrekventovan'#1084'j'#1113#1085'ch slo'#1115'enin zobrazit v re'#1115'imu "Fre' +
          'q"'
      end
      object RadioGroup3: TRadioGroup
        Left = 16
        Top = 12
        Width = 401
        Height = 77
        Caption = '#00428^cVelikost znaku v m'#1096#1085#1115'ce^eCharacter size in grid'
        ItemIndex = 0
        Items.Strings = (
          '#00429^cMal'#1073'^eSmall'
          '#00430^cSt'#1096'edn'#1085'^eMedium'
          '#00431^cVelk'#1073'^eLarge')
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 144
        Width = 305
        Height = 17
        Caption = 
          '#00437^eShow stroke count in grid^cZobrazovat po'#1080'et tah'#1097' v m'#1096#1085#1115'c' +
          'e'
        TabOrder = 1
      end
      object ComboBox1: TComboBox
        Left = 16
        Top = 112
        Width = 233
        Height = 21
        Style = csDropDownList
        TabOrder = 2
        Items.Strings = (
          'Bushu'
          'Unicode'
          '#00438^eJapanese^cJaponsk'#1101
          'KanWa'
          'KangXi'
          '#00439^eKorean^cKorejsk'#1101)
      end
      object CheckBox51: TCheckBox
        Left = 8
        Top = 384
        Width = 433
        Height = 17
        Caption = 
          '#00440^eUse grid font for displaying stroke order^cPou'#1115#1085't font m' +
          #1096#1085#1115'ky pro zobrazen'#1085' po'#1096'ad'#1085' tah'#1097
        TabOrder = 3
        Visible = False
      end
      object CheckBox57: TCheckBox
        Left = 16
        Top = 168
        Width = 433
        Height = 17
        Caption = 
          '#00891^eSearch by yomi can ignore okurigana^cHled'#1073'n'#1085' podle yomi ' +
          'm'#1097#1115'e ignorovat okuriganu'
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
    object TabSheet4: TTabSheet
      Caption = '#00217^eDictionary^cSlovn'#1085'k'
      ImageIndex = 3
      object Label26: TLabel
        Left = 16
        Top = 280
        Width = 343
        Height = 13
        Caption = 
          '#00441^eNot used dictionary files^cNepou'#1115#1085'van'#1081' soubory se slovn'#1085 +
          'ky:'
        Visible = False
      end
      object GroupBox3: TGroupBox
        Left = 16
        Top = 16
        Width = 441
        Height = 137
        Caption = '#00442^eSearch results order^cPo'#1096'ad'#1085' v'#1101'sledk'#1097' hled'#1073'n'#1085
        TabOrder = 0
        object CheckBox4: TCheckBox
          Left = 8
          Top = 16
          Width = 401
          Height = 17
          Caption = 
            '#00443^ePrefer words in user dictionary^cPreferovat slova v u'#1115'iv' +
            'atelsk'#1081'm slovn'#1085'ku'
          TabOrder = 0
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 40
          Width = 401
          Height = 17
          Caption = 
            '#00444^ePrefer nouns and verbs^cPreferovat podstatn'#1073' jm'#1081'na a slo' +
            'vesa'
          TabOrder = 1
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 64
          Width = 385
          Height = 17
          Caption = '#00445^ePrefer polite words^cPreferovat zdvo'#1096'il'#1073' slova'
          TabOrder = 2
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 88
          Width = 377
          Height = 17
          Caption = 
            '#00446^ePrefer popular words (marked by "pop")^cPreferovat popul' +
            #1073'rn'#1085' slova (ozna'#1080'ena "pop")'
          TabOrder = 3
        end
        object CheckBox59: TCheckBox
          Left = 8
          Top = 112
          Width = 417
          Height = 17
          Caption = 
            '#00921^eOrder by word frequency (where available)^cT'#1096#1085'dit podle ' +
            'frekvence slov (kdy'#1115' je k dispozici)'
          TabOrder = 4
        end
      end
      object CheckBox8: TCheckBox
        Left = 16
        Top = 160
        Width = 433
        Height = 17
        Caption = 
          '#00447^eReplace kanji with kana for words marked "kana"^cNahradi' +
          't kanji pomoc'#1085' kany u slov ozna'#1080'en'#1101'ch "kana"'
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
        Caption = 
          '#00448^eDisplay only results that fit one page in "Auto" mode^cZ' +
          'obrazit jen v'#1101'sledky, kter'#1081' se vejdou na str'#1073'nku v "Auto" re'#1115'imu'
        TabOrder = 3
      end
      object CheckBox50: TCheckBox
        Left = 16
        Top = 208
        Width = 433
        Height = 17
        Caption = 
          '#00449^eAuto-switch to Examples panel^cAutomaticky p'#1096'epnout na p' +
          'anel P'#1096#1085'klady'
        TabOrder = 4
      end
      object CheckBox58: TCheckBox
        Left = 16
        Top = 232
        Width = 441
        Height = 17
        Caption = 
          '#00922^eDisplay word count for each word^cZobrazovat frekven'#1080'n'#1085' ' +
          'po'#1080'et u ka'#1115'd'#1081'ho slova'
        TabOrder = 5
      end
    end
    object TabSheet3: TTabSheet
      Caption = '#00450^eFonts^cFonty'
      ImageIndex = 2
      object Label4: TLabel
        Left = 8
        Top = 367
        Width = 229
        Height = 13
        Caption = '#00923^eEnglish font:^cFont pro anglick'#1081' texty:'
      end
      object SpeedButton4: TSpeedButton
        Left = 376
        Top = 367
        Width = 73
        Height = 23
        Caption = '#00452^eChoose^cVybrat'
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
        Width = 257
        Height = 13
        Caption = '#00456^eFont for small text:^cFont pro drobn'#1081' n'#1073'pisy:'
      end
      object SpeedButton5: TSpeedButton
        Left = 376
        Top = 341
        Width = 73
        Height = 23
        Caption = '#00452^eChoose^cVybrat'
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
        Width = 322
        Height = 13
        Caption = '#00924^eRomanization && PinYin font:^cRomaniza'#1080'n'#1085' a PinYin font:'
      end
      object SpeedButton14: TSpeedButton
        Left = 376
        Top = 393
        Width = 73
        Height = 23
        Caption = '#00452^eChoose^cVybrat'
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
        Caption = '#00453^eJapanese fonts^cJaponsk'#1081' fonty'
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 309
          Height = 13
          Caption = '#00454^eFont for characters in grid:^cFont pro znaky v m'#1096#1085#1115'ce:'
        end
        object SpeedButton1: TSpeedButton
          Left = 368
          Top = 24
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 272
          Height = 13
          Caption = '#00455^eFont for big characters:^cFont pro velk'#1081' znaky:'
        end
        object SpeedButton2: TSpeedButton
          Left = 368
          Top = 51
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 340
          Height = 13
          Caption = 
            '#00875^eStroke order display font:^cFont pro zobrazen'#1085' po'#1096'ad'#1085' ta' +
            'h'#1097':'
        end
        object SpeedButton13: TSpeedButton
          Left = 368
          Top = 78
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
        Caption = '#00457^eChinese fonts:^c'#1048#1085'nsk'#1081' fonty:'
        TabOrder = 2
        object Label6: TLabel
          Left = 8
          Top = 24
          Width = 440
          Height = 13
          Caption = 
            '#00458^eBig5 font for traditional characters in grid:^cBig5 font' +
            ' pro tradi'#1080'n'#1085' znaky v m'#1096#1085#1115'ce:'
        end
        object SpeedButton6: TSpeedButton
          Left = 368
          Top = 24
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 403
          Height = 13
          Caption = 
            '#00459^eBig5 font for big traditional characters:^cBig5 font pro' +
            ' velk'#1081' tradi'#1080'n'#1085' znaky:'
        end
        object SpeedButton7: TSpeedButton
          Left = 368
          Top = 76
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 285
          Height = 13
          Caption = '#00460^eComplete unicode font:^cKompletn'#1085' unicode font:'
        end
        object SpeedButton8: TSpeedButton
          Left = 368
          Top = 145
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 506
          Height = 13
          Caption = 
            '#00461^eGB2312 font for simplified characters in grid:^cGB2312 f' +
            'ont pro zjednodu'#1113'en'#1081' znaky v m'#1096#1085#1115'ce:'
        end
        object SpeedButton3: TSpeedButton
          Left = 368
          Top = 50
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 469
          Height = 13
          Caption = 
            '#00462^eGB2312 font for big simplified characters:^cGB2312 font ' +
            'pro velk'#1081' zjednodu'#1113'en'#1081' znaky:'
        end
        object SpeedButton9: TSpeedButton
          Left = 368
          Top = 102
          Width = 73
          Height = 23
          Caption = '#00452^eChoose^cVybrat'
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
          Width = 878
          Height = 14
          Caption = 
            '#00463^eMake sure that the Big5 and GB2312 fonts are either the ' +
            'same or at least look the same.^cUjist'#1084'te se, '#1115'e oba Big5 a GB23' +
            '12 fonty jsou bu'#1087' stejn'#1081' nebo stejn'#1084' vypadaj'#1085'.'
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
        Caption = '#00464^eSelect recommended fonts^cNastavit doporu'#1080'en'#1081' fonty'
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
    object TabSheet5: TTabSheet
      Caption = '#00465^eWord list printing^cTisk seznamu slov'
      ImageIndex = 4
      object Label11: TLabel
        Left = 16
        Top = 344
        Width = 380
        Height = 13
        Caption = 
          '#00466^cPo'#1080'et '#1096#1073'dek na '#1080'tvercov'#1081' stran'#1084':^eNumber of lines on squ' +
          'are page:'
      end
      object CheckBox14: TCheckBox
        Left = 16
        Top = 296
        Width = 241
        Height = 17
        Caption = '#00467^ePrint horizontal lines^cTisknout horizont'#1073'ln'#1085' '#1080#1073'ry'
        TabOrder = 0
      end
      object CheckBox15: TCheckBox
        Left = 224
        Top = 296
        Width = 241
        Height = 17
        Caption = '#00468^ePrint vertical lines^cTisknout vertik'#1073'ln'#1085' '#1080#1073'ry'
        TabOrder = 1
      end
      object CheckBox16: TCheckBox
        Left = 16
        Top = 320
        Width = 241
        Height = 17
        Caption = '#00469^eVary colors^cSt'#1096#1085'dat barvy'
        TabOrder = 2
      end
      object CheckBox17: TCheckBox
        Left = 224
        Top = 320
        Width = 313
        Height = 17
        Caption = '#00470^eDo not print unlearned kanji^cNetisknout nenau'#1080'en'#1081' znaky'
        TabOrder = 3
      end
      object GroupBox4: TGroupBox
        Left = 16
        Top = 8
        Width = 433
        Height = 281
        Caption = '#00471^eColumns^cSloupce'
        TabOrder = 4
        object Label23: TLabel
          Left = 16
          Top = 248
          Width = 237
          Height = 13
          Caption = '#00472^eUser settings:^cU'#1115'ivatelsk'#1081' nastaven'#1085':'
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
            '#00473^ePhonetic            |Meaning^c'#1048'ten'#1085'            |V'#1101'znam'
            '#00474^eWritten             |Meaning^cZ'#1073'pis            |V'#1101'znam'
            '#00475^ePhonetic |Written   |Meaning^c'#1048'ten'#1085'   |Z'#1073'pis   |V'#1101'znam'
            '#00476^eWritten  |Phonetic  |Meaning^cZ'#1073'pis   |'#1048'ten'#1085'   |V'#1101'znam'
            '#00477^eRomaji   |Kana      |Meaning^cRomaji  |Kana    |V'#1101'znam'
            '#00478^ePhonetic1           |Phonetic2^c'#1048'ten'#1085'1           |'#1048'ten'#1085'2'
            '#00479^eWritten1            |Written2^cZ'#1073'pis1           |Z'#1073'pis2'
            '#00480^eMeaning1            |Meaning2^cV'#1101'znam1          |V'#1101'znam2'
            
              '#00481^ePhonetic1|Phonetic2 |Phonetic3|Phonetic4^c'#1048'ten'#1085'1  |'#1048'ten'#1085 +
              '2  |'#1048'ten'#1085'3  |'#1048'ten'#1085'4'
            
              '#00482^eWritten1 |Written2  |Written3 |Written4^cZ'#1073'pis1  |Z'#1073'pis2' +
              '  |Z'#1073'pis3  |Z'#1073'pis4'
            
              '#00483^eMeaning1 |Meaning2  |Meaning3 |Meaning4^cV'#1101'znam1 |V'#1101'znam' +
              '2 |V'#1101'znam3 |V'#1101'znam4'
            
              '#00484^ePhonetic1|Written1  |Phonetic2|Written2^c'#1048'ten'#1085'1  |Z'#1073'pis1' +
              '  |'#1048'ten'#1085'2  |Z'#1073'pis2'
            
              '#00485^ePhonetic1|Meaning1  |Phonetic2|Meaning2^c'#1048'ten'#1085'1  |V'#1101'znam' +
              '1 |'#1048'ten'#1085'2  |V'#1101'znam2'
            
              '#00486^eWritten1 |Meaning1  |Written2 |Meaning2^cZ'#1073'pis1  |V'#1101'znam' +
              '1 |Z'#1073'pis2  |V'#1101'znam2'
            '#00487^eUser settings^cU'#1115'ivatelsk'#1081' nastaven'#1085)
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
    object TabSheet6: TTabSheet
      Caption = '#00488^eCharacter cards printing^cTisk karet znak'#1097
      ImageIndex = 5
      object Label12: TLabel
        Left = 8
        Top = 8
        Width = 497
        Height = 13
        Caption = 
          '#00489^cPo'#1080'et znak'#1097' na v'#1101#1113'ku na '#1080'tvercov'#1081' stran'#1084':^eVertical numb' +
          'er of characters on square page:'
      end
      object Label13: TLabel
        Left = 8
        Top = 56
        Width = 519
        Height = 13
        Caption = 
          '#00952^cProstor pro slo'#1115'eniny ze slov'#1085#1080'ek (ve znac'#1085'ch):^eSpace f' +
          'or vocabulary compounds (in characters):'
      end
      object Label14: TLabel
        Left = 8
        Top = 104
        Width = 447
        Height = 13
        Caption = 
          '#00953^cPo'#1080'et slo'#1115'enin ze slov'#1085#1080'ek na v'#1101#1113'ku:^eVertical number of' +
          ' vocabulary compounds:'
      end
      object Label15: TLabel
        Left = 8
        Top = 152
        Width = 215
        Height = 13
        Caption = '#00492^eCalligraphy font:^cKaligrafick'#1101' font:'
      end
      object SpeedButton10: TSpeedButton
        Left = 136
        Top = 168
        Width = 73
        Height = 23
        Caption = '#00452^eChoose^cVybrat'
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
        Width = 295
        Height = 13
        Caption = '#00951^cPo'#1080'et pln'#1101'ch slo'#1115'enin:^eNumber of full compounds:'
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
        Caption = 
          '#00493^ePrint vocabulary compounds^cTisknout slo'#1115'eniny ze slov'#1085#1080 +
          'ek'
        TabOrder = 3
      end
      object CheckBox19: TCheckBox
        Left = 232
        Top = 224
        Width = 209
        Height = 17
        Caption = '#00494^ePrint radical^cTisknout radik'#1073'l'
        TabOrder = 4
      end
      object CheckBox20: TCheckBox
        Left = 232
        Top = 248
        Width = 225
        Height = 17
        Caption = '#00495^ePrint alternate form^cTisknout alternativn'#1085' formu'
        TabOrder = 5
      end
      object CheckBox21: TCheckBox
        Left = 8
        Top = 272
        Width = 257
        Height = 17
        Caption = '#00496^ePrint outer lines^cTisknout vn'#1084'j'#1113#1085' '#1080#1073'ry'
        TabOrder = 6
      end
      object CheckBox22: TCheckBox
        Left = 8
        Top = 224
        Width = 209
        Height = 17
        Caption = '#00497^ePrint readings^cTisknout '#1080'ten'#1085
        TabOrder = 7
      end
      object CheckBox23: TCheckBox
        Left = 8
        Top = 296
        Width = 225
        Height = 17
        Caption = '#00498^ePrint inner lines^cTisknout vnit'#1096'n'#1085' '#1080#1073'ry'
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
        Caption = 
          '#00499^ePrint vertically (right to left)^cTisknout vertik'#1073'ln'#1084' (z' +
          'prava doleva)'
        TabOrder = 10
      end
      object CheckBox25: TCheckBox
        Left = 8
        Top = 344
        Width = 241
        Height = 17
        Caption = '#00500^eLeave space between columns^cVynech m'#1085'sto mezi sloupci'
        TabOrder = 11
      end
      object CheckBox44: TCheckBox
        Left = 8
        Top = 248
        Width = 217
        Height = 17
        Caption = '#00501^ePrint definition^cTisknout definici'
        TabOrder = 12
      end
      object CheckBox45: TCheckBox
        Left = 232
        Top = 272
        Width = 217
        Height = 17
        Caption = '#00502^ePrint stroke count^cTisknout po'#1080'et tah'#1097
        TabOrder = 13
      end
      object CheckBox52: TCheckBox
        Left = 232
        Top = 296
        Width = 225
        Height = 17
        Caption = '#00503^ePrint stroke order^cTisknout po'#1096'ad'#1085' tah'#1097
        TabOrder = 14
      end
      object CheckBox62: TCheckBox
        Left = 232
        Top = 200
        Width = 217
        Height = 17
        Caption = '#00954^ePrint full compounds^cTisknout pln'#1081' slo'#1115'eniny'
        TabOrder = 15
      end
      object CheckBox63: TCheckBox
        Left = 232
        Top = 320
        Width = 217
        Height = 17
        Caption = 
          '#00956^eSort compounds by frequency^cT'#1096#1085'dit slo'#1115'eniny podle frek' +
          'vence'
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
    object TabSheet7: TTabSheet
      Caption = '#00504^eDatabase maintenance^c'#1066'dr'#1115'ba datab'#1073'ze'
      ImageIndex = 6
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
        Caption = 
          '#00505^eCheck dictionary database indexes^cOtestuj indexy datab'#1073 +
          'ze slovn'#1085'ku'
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
        Caption = 
          '#00506^eExport user database into delimited text file^cExportova' +
          't u'#1115'ivatelskou datab'#1073'zi do textov'#1081'ho souboru s odd'#1084'lova'#1080'i'
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
        Caption = 
          '#00507^eImport user database from delimited text file^cImportova' +
          't u'#1115'ivatelskou datab'#1073'zi z textov'#1081'ho souboru s odd'#1084'lova'#1080'i'
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
        Caption = 
          '#00508^eCheck user dictionary categories^cOtestovat kategorie sl' +
          'ov'#1085#1080'ek'
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
        Caption = 
          '#00509^eShow memory allocation stats^cZobrazit statistiky alokac' +
          'e pam'#1084'ti'
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
    object TabSheet9: TTabSheet
      Caption = '#00300^eText translator^cP'#1096'eklad textu'
      ImageIndex = 8
      object Label25: TLabel
        Left = 16
        Top = 248
        Width = 434
        Height = 13
        Caption = 
          '#00510^eNumber of lines reserved to meaning:^cPo'#1080'et '#1096#1073'dek rezerv' +
          'ovan'#1101'ch pro v'#1101'znam:'
      end
      object GroupBox5: TGroupBox
        Left = 16
        Top = 272
        Width = 449
        Height = 105
        Caption = '#00511^ePrinting^cTisk'
        TabOrder = 0
        object Label24: TLabel
          Left = 16
          Top = 72
          Width = 380
          Height = 13
          Caption = 
            '#00512^eNumber of lines on square page:^cPo'#1080'et '#1096#1073'dek na '#1080'tvercov' +
            #1081' stran'#1084':'
        end
        object CheckBox29: TCheckBox
          Left = 16
          Top = 24
          Width = 193
          Height = 17
          Caption = '#00272^eDisplay reading^cZobrazit '#1080'ten'#1085
          TabOrder = 0
        end
        object CheckBox30: TCheckBox
          Left = 232
          Top = 24
          Width = 209
          Height = 17
          Caption = '#00513^eDisplay meaning^cZobrazit v'#1101'znam'
          TabOrder = 1
        end
        object CheckBox31: TCheckBox
          Left = 16
          Top = 48
          Width = 265
          Height = 17
          Caption = '#00514^eDo not use colors^cNepou'#1115#1085'vat barvy'
          TabOrder = 2
        end
        object CheckBox37: TCheckBox
          Left = 232
          Top = 48
          Width = 377
          Height = 17
          Caption = 
            '#00515^ePrint vertically in columns^cTisknout vertik'#1073'ln'#1084' do slou' +
            'pc'#1097
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
      object CheckBox32: TCheckBox
        Left = 16
        Top = 32
        Width = 401
        Height = 17
        Caption = '#00516^eDisplay lines^cZobrazovat '#1080#1073'ry'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object CheckBox33: TCheckBox
        Left = 16
        Top = 56
        Width = 353
        Height = 17
        Caption = 
          '#00517^eDo not display meaning in learned words^cNezobrazovat v'#1101 +
          'znam nau'#1080'en'#1101'ch slova'
        TabOrder = 2
      end
      object CheckBox34: TCheckBox
        Left = 16
        Top = 128
        Width = 353
        Height = 17
        Caption = '#00518^eDo not search for particles^cNevyhled'#1073'vat partikule'
        TabOrder = 3
      end
      object CheckBox35: TCheckBox
        Left = 16
        Top = 80
        Width = 377
        Height = 17
        Caption = 
          '#00519^eDo not display reading of learned kanji^cNezobrazovat '#1080't' +
          'en'#1085' nau'#1080'en'#1101'ch znak'#1097
        TabOrder = 4
      end
      object CheckBox36: TCheckBox
        Left = 16
        Top = 104
        Width = 393
        Height = 17
        Caption = 
          '#00884^eDisplay transcript above kana^cZobrazovat p'#1096'epis nad kan' +
          'ou'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object CheckBox38: TCheckBox
        Left = 16
        Top = 152
        Width = 377
        Height = 17
        Caption = 
          '#00521^eDo not translate hiragana-only words^cNep'#1096'ekl'#1073'dat slova ' +
          'pouze v hiragan'#1084
        TabOrder = 6
      end
      object CheckBox40: TCheckBox
        Left = 16
        Top = 200
        Width = 401
        Height = 17
        Caption = 
          '#00522^eDisplay words with user-defined translation in bold^cZob' +
          'razovat slova s u'#1115'ivatelsky definovan'#1101'm p'#1096'ekladem tu'#1080'n'#1084
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object Edit17: TEdit
        Left = 232
        Top = 248
        Width = 121
        Height = 21
        TabOrder = 8
        Text = '2'
      end
      object CheckBox42: TCheckBox
        Left = 16
        Top = 176
        Width = 225
        Height = 17
        Caption = '#00523^eLeave space between lines^cVynechat m'#1085'sto mezi '#1096#1073'dky'
        Checked = True
        State = cbChecked
        TabOrder = 9
      end
      object CheckBox43: TCheckBox
        Left = 16
        Top = 8
        Width = 433
        Height = 17
        Caption = 
          '#00524^eBreak lines only at word boundaries^cRozd'#1084'lovat '#1096#1073'dky je' +
          'n na rozhran'#1085' slov'
        Checked = True
        State = cbChecked
        TabOrder = 10
      end
      object CheckBox27: TCheckBox
        Left = 360
        Top = 248
        Width = 97
        Height = 17
        Caption = '#00525^eDouble size^cDvojn'#1073'sobn'#1073' velikost'
        TabOrder = 11
      end
      object CheckBox41: TCheckBox
        Left = 248
        Top = 32
        Width = 401
        Height = 17
        Caption = 
          '#00526^eDisplay non-japanese chars in grey^cZobrazovat nejaponsk' +
          #1081' znaky '#1113'ed'#1084
        Checked = True
        State = cbChecked
        TabOrder = 12
        Visible = False
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 224
        Width = 401
        Height = 17
        Caption = '#00527^eShow editor hint^cZobrazit n'#1073'pov'#1084'du p'#1096'i psan'#1085
        Checked = True
        State = cbChecked
        TabOrder = 13
      end
      object CheckBox13: TCheckBox
        Left = 232
        Top = 224
        Width = 225
        Height = 17
        Caption = '#00528^eDisplay meaning on hint^cZobrazit v'#1101'znam v n'#1073'pov'#1084'd'#1084
        Checked = True
        State = cbChecked
        TabOrder = 14
      end
      object CheckBox56: TCheckBox
        Left = 248
        Top = 176
        Width = 217
        Height = 17
        Caption = 
          '#00529^eLeave space even when reading is off^cVynechat m'#1085'sto i k' +
          'dy'#1115' je '#1080'ten'#1085' vypnuto'
        TabOrder = 15
      end
      object cbTranslateNoLongTextWarning: TCheckBox
        Left = 16
        Top = 386
        Width = 457
        Height = 17
        Caption = '^eDo not warn when translating large text chunks'
        TabOrder = 16
      end
    end
    object TabSheet10: TTabSheet
      Caption = '#00530^eCharacter details^cDetaily znak'#1097
      ImageIndex = 9
      DesignSize = (
        471
        372)
      object Label34: TLabel
        Left = 8
        Top = 8
        Width = 231
        Height = 13
        Caption = '#00531^eDisplayed items:^cZobrazen'#1081' polo'#1115'ky:'
      end
      object SpeedButton11: TSpeedButton
        Left = 448
        Top = 98
        Width = 23
        Height = 79
        Hint = '#00532^eMove up^cP'#1096'esu'#1090' nahoru'
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
      end
      object SpeedButton12: TSpeedButton
        Left = 448
        Top = 234
        Width = 23
        Height = 79
        Hint = '#00533^eMove down^cP'#1096'esu'#1090' dol'#1097
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
      end
      object ListBox2: TListBox
        Left = 8
        Top = 24
        Width = 438
        Height = 308
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox2Click
      end
      object Button7: TButton
        Left = 8
        Top = 339
        Width = 113
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00534^eAdd...^cP'#1096'idat...'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 180
        Top = 339
        Width = 113
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00535^eChange...^cZm'#1084'nit...'
        TabOrder = 2
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 350
        Top = 339
        Width = 97
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00536^eDelete^cZru'#1113'it'
        TabOrder = 3
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 296
        Top = 1
        Width = 150
        Height = 22
        Caption = '#00537^eReset to default^cNastavit implicitn'#1085
        TabOrder = 4
        OnClick = Button10Click
      end
    end
    object TabSheet11: TTabSheet
      Caption = '#00538^ePopup tool^cPopup okno'
      ImageIndex = 10
      object Label36: TLabel
        Left = 8
        Top = 80
        Width = 362
        Height = 13
        Caption = 
          '#00539^ePopup delay (x100 ms):^cZpo'#1115'd'#1084'n'#1085' p'#1096'ed zobrazen'#1085'm (x100 m' +
          's):'
      end
      object Label37: TLabel
        Left = 8
        Top = 112
        Width = 344
        Height = 13
        Caption = 
          '#00540^eText scan range (pixels):^cRozsah prohled'#1073'v'#1073'n'#1085' textu (pi' +
          'xely):'
      end
      object Label38: TLabel
        Left = 160
        Top = 112
        Width = 113
        Height = 13
        Caption = '#00541^eLeft^cDoleva'
      end
      object Label39: TLabel
        Left = 312
        Top = 112
        Width = 127
        Height = 13
        Caption = '#00542^eRight^cDoprava'
      end
      object Label40: TLabel
        Left = 8
        Top = 144
        Width = 403
        Height = 13
        Caption = 
          '#00543^eMax. number of dictionary entries:^cMaxim'#1073'ln'#1085' po'#1080'et polo' +
          #1115'ek ze slovn'#1085'ku:'
      end
      object Label43: TLabel
        Left = 8
        Top = 176
        Width = 317
        Height = 13
        Caption = 
          '#00544^eCharacter card size factor:^cFaktor velikosti karty znak' +
          'u:'
      end
      object Label44: TLabel
        Left = 8
        Top = 208
        Width = 872
        Height = 13
        Caption = 
          '#00545^eNumber of characters reserved for compounds on a card (a' +
          'ffects overall popup width):^cPo'#1080'et znak'#1097' rezervovan'#1101'ch pro slo'#1115 +
          'eniny na kart'#1084' (ovlivn'#1085' celkovou '#1113#1085#1096'ku okna):'
      end
      object Label45: TLabel
        Left = 104
        Top = 224
        Width = 157
        Height = 13
        Caption = '#00546^eMinimum:^cMinim'#1073'ln'#1084':'
      end
      object Label46: TLabel
        Left = 264
        Top = 224
        Width = 163
        Height = 13
        Caption = '#00547^eMaximum:^cMaxim'#1073'ln'#1084':'
      end
      object CheckBox28: TCheckBox
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = 
          '#00548^eShow translation for japanese/chinese text^cZobrazit p'#1096'e' +
          'klad pro japonsk'#1101'/'#1080#1085'nsk'#1101' text'
        TabOrder = 0
      end
      object CheckBox47: TCheckBox
        Left = 8
        Top = 32
        Width = 353
        Height = 17
        Caption = 
          '#00549^eShow translation for english text (only for screen)^cZob' +
          'razit p'#1096'eklad pro anglick'#1101' text (pouze v re'#1115'imu obrazovky)'
        TabOrder = 1
      end
      object CheckBox48: TCheckBox
        Left = 8
        Top = 56
        Width = 361
        Height = 17
        Caption = '#00550^eShow character details^cZobrazit detaily znaku'
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
    object TabSheet12: TTabSheet
      Caption = '#00551^eColors^cBarvy'
      ImageIndex = 11
      DesignSize = (
        471
        372)
      object Shape2: TShape
        Left = 8
        Top = 332
        Width = 105
        Height = 25
        Anchors = [akLeft, akBottom]
      end
      object Label42: TLabel
        Left = 8
        Top = 8
        Width = 148
        Height = 13
        Caption = '#00552^eCategory^cKategorie'
      end
      object ListBox3: TListBox
        Left = 8
        Top = 72
        Width = 457
        Height = 257
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox3Click
      end
      object Button12: TButton
        Left = 120
        Top = 332
        Width = 217
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00553^eChange^cZm'#1084'nit'
        TabOrder = 1
        OnClick = Button12Click
      end
      object Button14: TButton
        Left = 344
        Top = 332
        Width = 121
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00554^eRevert to default^cVr'#1073'tit na v'#1101'choz'#1085
        TabOrder = 2
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 336
        Top = 8
        Width = 129
        Height = 25
        Caption = '#00555^eSet all to default^cNastavit v'#1113'e na v'#1101'choz'#1085
        TabOrder = 3
        OnClick = Button15Click
      end
      object ComboBox2: TComboBox
        Left = 8
        Top = 24
        Width = 313
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = ComboBox2Change
        Items.Strings = (
          '#00298^eCharacter list^cSeznam znak'#1097
          '#00556^eWord grids^cM'#1096#1085#1115'ky slov'
          '#00557^eWord markers^cP'#1096#1085'znaky slov'
          '#00558^eEditor & translator^cEditor / P'#1096'ekl'#1073'd'#1073'n'#1085
          '#00559^ePopup tool^cPopup n'#1073'stroj')
      end
      object CheckBox3: TCheckBox
        Left = 8
        Top = 49
        Width = 457
        Height = 17
        Caption = 
          '#00560^eDo not use colors in character list, use Windows default' +
          ' colors instead^cNepou'#1115#1085'vat barvy v seznamu znak'#1097', pou'#1115#1085't nastav' +
          'en'#1085' Windows'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckBox3Click
      end
      object CheckBox39: TCheckBox
        Left = 8
        Top = 49
        Width = 457
        Height = 17
        Caption = 
          '#00561^eDo not use colors in editor, use Windows default colors ' +
          'instead^cNepou'#1115#1085'vat barvy v editoru, pou'#1115#1085't m'#1085'sto toho barvy Win' +
          'dows'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = CheckBox39Click
      end
      object CheckBox9: TCheckBox
        Left = 8
        Top = 50
        Width = 433
        Height = 17
        Hint = 
          '#00562^eColors: black - common, grey - uncommon, blue - learned,' +
          ' green - only in names^cBarvy: '#1080'ern'#1073' - b'#1084#1115'n'#1101' znak, '#1113'ed'#1073' - vz'#1073'cn'#1101 +
          ', modr'#1073' - nau'#1080'en'#1101', zelen'#1073' - pouze ve jm'#1081'nech'
        Caption = 
          '#00563^eDo not use colors in word grids, use Windows default col' +
          'ors instead^cNepou'#1115#1085'vat barvy v m'#1096#1085#1115'ce slov, pou'#1115#1085't barvy Window' +
          's'
        TabOrder = 7
        OnClick = CheckBox9Click
      end
    end
    object TabSheet13: TTabSheet
      Caption = '^eEditor^cEditor'
      ImageIndex = 12
      object lblAozoraRuby: TLabel
        Left = 8
        Top = 109
        Width = 77
        Height = 13
        Caption = 'Aozora Ruby:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSaving: TLabel
        Left = 8
        Top = 13
        Width = 114
        Height = 13
        Caption = 'Saving and loading:'
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
        Width = 315
        Height = 13
        Caption = 
          '^eIf not, you'#39'll have the option of choosing that when you save ' +
          'file.'
      end
      object lblAozoraTagsInColor: TLabel
        Left = 16
        Top = 174
        Width = 174
        Height = 13
        Caption = '^eYou can set colors on Colors page'
      end
      object CheckBox60: TCheckBox
        Left = 8
        Top = 32
        Width = 457
        Height = 17
        Caption = '^eAuto-save file on exit'
        TabOrder = 0
      end
      object CheckBox61: TCheckBox
        Left = 8
        Top = 56
        Width = 457
        Height = 17
        Caption = '^eAuto-load last opened file on start'
        TabOrder = 1
      end
      object cbNoSaveChangesWarning: TCheckBox
        Left = 8
        Top = 80
        Width = 457
        Height = 17
        Caption = 
          '^eDo not ask to save changes on exit (WARNING: Use at your own r' +
          'isk)'
        TabOrder = 2
      end
      object cbLoadAozoraRuby: TCheckBox
        Left = 8
        Top = 128
        Width = 457
        Height = 17
        Caption = '^eShow Aozora-Ruby as annotations (requires text reload)'
        TabOrder = 3
      end
      object cbAozoraTagsInColor: TCheckBox
        Left = 8
        Top = 151
        Width = 457
        Height = 17
        Caption = '^eShow Aozora-Ruby tags in color'
        TabOrder = 4
      end
      object cbSaveAnnotationsToRuby: TCheckBox
        Left = 8
        Top = 195
        Width = 457
        Height = 17
        Caption = '^eSave annotations as Aozora-Ruby tags by default'
        TabOrder = 5
      end
    end
    object TabSheet14: TTabSheet
      Caption = '^eAnnotations^cAnotace'
      ImageIndex = 13
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
        Caption = 
          '^eEnable annotations (ANNOTATE.PKG)^cPovolit anotace (ANNOTATE.P' +
          'KG)'
        TabOrder = 0
      end
      object CheckBox65: TCheckBox
        Left = 11
        Top = 54
        Width = 294
        Height = 17
        Caption = 
          '^eRebuild ANNOTATE.PKG from .ANO files if needed whenever WaKan ' +
          'starts^cVytvo'#1096'it ANNOTATE.PKG ze soubor'#1097' .ANO p'#1096'i ka'#1115'd'#1081'm startu ' +
          'WaKanu kdy'#1115' je pot'#1096'eba'
        TabOrder = 1
      end
      object CheckBox66: TCheckBox
        Left = 11
        Top = 150
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to play sound files^cPovolit anotac'#1085'm p'#1096'ehr'#1073 +
          't zvukov'#1081' soubory '
        TabOrder = 2
        Visible = False
      end
      object CheckBox67: TCheckBox
        Left = 11
        Top = 78
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to display pictures^cPovolit anotac'#1085'm zobraz' +
          'it obr'#1073'zky'
        TabOrder = 3
      end
      object CheckBox68: TCheckBox
        Left = 11
        Top = 102
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to display web pages^cPovolit anotac'#1085'm zobra' +
          'zit webov'#1081' str'#1073'nky'
        TabOrder = 4
      end
      object CheckBox69: TCheckBox
        Left = 11
        Top = 126
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to change foreground color^cPovolit anotac'#1085'm' +
          ' m'#1084'nit barvu pop'#1096'ed'#1085
        TabOrder = 5
      end
      object Button16: TButton
        Left = 72
        Top = 352
        Width = 337
        Height = 25
        Caption = 
          '^eHelp for the annotation feature^cN'#1073'pov'#1084'da pro funkci anotace (' +
          'anglicky)'
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
  object BitBtn1: TBitBtn
    Left = 200
    Top = 523
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object Button6: TButton
    Left = 8
    Top = 527
    Width = 177
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '#00929^eChange language^cZm'#1084'na jazyka'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button6Click
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '.wbk'
    Filter = 'WaKan User Data Backup|*.wbk'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 344
    Top = 24
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = '.wbk'
    Filter = 'WaKan User Data Bakup|*.wbk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 392
    Top = 16
  end
  object ColorDialog1: TColorDialog
    Left = 148
    Top = 352
  end
end
