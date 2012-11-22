object fSettings: TfSettings
  Left = 451
  Top = 131
  BorderStyle = bsDialog
  Caption = '#00397^eSettings^cNastavení'
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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 479
    Height = 508
    ActivePage = TabSheet10
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 0
    object TabSheet8: TTabSheet
      Caption = '#00398^eGeneral^cObecné'
      ImageIndex = 7
      object Label41: TLabel
        Left = 16
        Top = 224
        Width = 376
        Height = 13
        Caption = 
          '#00399^eDictionary entries grid font height:^cVelikost písma v m' +
          'øížce slovníku:'
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
          'ated by date so you can check your progress^cUloží na disk údaje' +
          ' o statistice podle aktuálního data takže mùžete sledovat vývoj ' +
          'statistiky'
        Caption = 
          '#00402^eSave statistics to disk (folder STAT)^cUkládat statistik' +
          'u na disk (složka STAT)'
        TabOrder = 0
      end
      object CheckBox10: TCheckBox
        Left = 16
        Top = 152
        Width = 433
        Height = 17
        Hint = 
          '#00403^eShows words with non-learned kanji in different color^cZ' +
          'obrazuje slova s nenauèenými znaky odlišnou barvou'
        Caption = 
          '#00404^eDifferentiate words with non-learned characters^cOdlišov' +
          'at slova s nenauèenými znaky'
        TabOrder = 1
      end
      object CheckBox11: TCheckBox
        Left = 16
        Top = 200
        Width = 433
        Height = 17
        Hint = 
          '#00405^eColors: green - mastered, yellow - learned, cyan - unlea' +
          'rned, red - problematic^cBarvy: zelená - dobøe nauèeno, žlutá - ' +
          'nauèeno, modrá - nenauèeno, èervená - problematické'
        Caption = 
          '#00406^eShow word learned-status by different background color^c' +
          'Zobrazit nauèenost slov pomocí rùzných barev pozadí'
        TabOrder = 2
      end
      object CheckBox46: TCheckBox
        Left = 16
        Top = 312
        Width = 433
        Height = 17
        Hint = 
          '#00405^eColors: green - mastered, yellow - learned, cyan - unlea' +
          'rned, red - problematic^cBarvy: zelená - dobøe nauèeno, žlutá - ' +
          'nauèeno, modrá - nenauèeno, èervená - problematické'
        Caption = 
          '#00407^eAuto-save user data on exit^cPøi ukonèení automaticky ul' +
          'ožit uživatelská data'
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
          '#00408^eLoad dictionaries on demand^cNahrávat slovníky pouze pøi' +
          ' žádosti'
        TabOrder = 5
      end
      object CheckBox53: TCheckBox
        Left = 16
        Top = 272
        Width = 353
        Height = 17
        Caption = '#00409^eMulti-line word grids^cVíce øádkové møížky slov'
        TabOrder = 6
      end
      object CheckBox54: TCheckBox
        Left = 16
        Top = 336
        Width = 193
        Height = 17
        Caption = 
          '#00410^eAuto-save user database every ^cAutomaticky uložit uživ.' +
          ' databázi každých'
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
          'dìlat zálohu uživatelského souboru do BACKUP adresáøe každý den'
        TabOrder = 9
      end
      object RadioGroup5: TRadioGroup
        Left = 16
        Top = 12
        Width = 353
        Height = 85
        Hint = 
          '#00432^eSelects what chinese characters are displayed^cVýbìr typ' +
          'u èínských znakù, které jsou zobrazeny'
        Caption = 
          '#00883^eChinese character mode && character conversion^cRežim èí' +
          'nských znakù a konverze'
        ItemIndex = 0
        Items.Strings = (
          '#00434^eTraditional (Big5) only^cPouze tradièní (Big5)'
          '#00435^eSimplified (GB2312) only^cPouze zjednodušené (GB2312)'
          '#00436^eAll characters (Unicode)^cVšechny znaky (Unicode)')
        TabOrder = 10
      end
      object CheckBox70: TCheckBox
        Left = 16
        Top = 104
        Width = 417
        Height = 17
        Caption = 
          '^eDisplay message when word is added to vocabulary^cZobrazit zpr' +
          'ávu, když je slovíèko pøidáno do slovníku'
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
        Caption = '#00413^eRomanization of japanese^cRomanizace japonštiny'
        TabOrder = 0
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
          Caption = '#00415^eJapanese:^cJaponské:'
        end
        object Label18: TLabel
          Left = 248
          Top = 136
          Width = 142
          Height = 13
          Caption = '#00416^eEnglish:^cAnglické:'
        end
        object Label19: TLabel
          Left = 248
          Top = 152
          Width = 127
          Height = 13
          Caption = '#00417^eCzech:^cÈeské:'
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
            'y^cVýbìr romanizaèního systému pro vstup i pro zobrazení'
          Caption = '#00419^eRomanization system^cRomanizaèní systém'
          ItemIndex = 0
          Items.Strings = (
            
              '#00420^eKunreishiki romaji (japanese)^cKunreišiki rómadži (japon' +
              'ský)'
            '#00421^eHepburn (english)^cHepburnùv (anglický)'
            '#00422^eCzech^cÈeský')
          TabOrder = 0
          OnClick = RadioGroup1Click
        end
        object RadioGroup2: TRadioGroup
          Left = 216
          Top = 20
          Width = 225
          Height = 61
          Caption = '#00423^eShow phonetic in^cZobraz fonetické zápisy v'
          ItemIndex = 0
          Items.Strings = (
            '#00424^eHiragana / Katakana^cHiraganì / Katakanì'
            '#00425^eRomaji^cRómadži')
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
        Caption = '#00426^eRomanization of chinese^cRomanizace èínštiny'
        TabOrder = 1
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
            'y^cVýbìr romanizaèního systému pro vstup i pro zobrazení'
          Caption = '#00419^eRomanization system^cRomanizaèní systém'
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
          Caption = '#00423^eShow phonetic in^cZobraz fonetické zápisy v'
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
      Caption = '#00298^eCharacter list^cSeznam znakù'
      ImageIndex = 1
      object Label35: TLabel
        Left = 16
        Top = 96
        Width = 231
        Height = 13
        Caption = '#00427^eDisplayed radical:^cZobrazený radikál:'
      end
      object Label52: TLabel
        Left = 13
        Top = 192
        Width = 647
        Height = 13
        Caption = 
          '#00920^eHow many most frequent compounds to display in "Freq" mo' +
          'de:^cKolik nejfrekventovanìjších složenin zobrazit v režimu "Fre' +
          'q"'
      end
      object RadioGroup3: TRadioGroup
        Left = 16
        Top = 12
        Width = 401
        Height = 77
        Caption = '#00428^cVelikost znaku v møížce^eCharacter size in grid'
        ItemIndex = 0
        Items.Strings = (
          '#00429^cMalá^eSmall'
          '#00430^cStøední^eMedium'
          '#00431^cVelká^eLarge')
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 144
        Width = 305
        Height = 17
        Caption = 
          '#00437^eShow stroke count in grid^cZobrazovat poèet tahù v møížc' +
          'e'
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
          '#00438^eJapanese^cJaponský'
          'KanWa'
          'KangXi'
          '#00439^eKorean^cKorejský')
      end
      object CheckBox51: TCheckBox
        Left = 8
        Top = 384
        Width = 433
        Height = 17
        Caption = 
          '#00440^eUse grid font for displaying stroke order^cPoužít font m' +
          'øížky pro zobrazení poøadí tahù'
        TabOrder = 3
        Visible = False
      end
      object CheckBox57: TCheckBox
        Left = 16
        Top = 168
        Width = 433
        Height = 17
        Caption = 
          '#00891^eSearch by yomi can ignore okurigana^cHledání podle yomi ' +
          'mùže ignorovat okuriganu'
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
      Caption = '#00217^eDictionary^cSlovník'
      ImageIndex = 3
      object Label26: TLabel
        Left = 16
        Top = 280
        Width = 336
        Height = 13
        Caption = 
          '#00441^eNot used dictionary files^cNepoužívané soubory se slovní' +
          'ky:'
        Visible = False
      end
      object GroupBox3: TGroupBox
        Left = 16
        Top = 16
        Width = 441
        Height = 137
        Caption = '#00442^eSearch results order^cPoøadí výsledkù hledání'
        TabOrder = 0
        object CheckBox4: TCheckBox
          Left = 8
          Top = 16
          Width = 401
          Height = 17
          Caption = 
            '#00443^ePrefer words in user dictionary^cPreferovat slova v uživ' +
            'atelském slovníku'
          TabOrder = 0
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 40
          Width = 401
          Height = 17
          Caption = 
            '#00444^ePrefer nouns and verbs^cPreferovat podstatná jména a slo' +
            'vesa'
          TabOrder = 1
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 64
          Width = 385
          Height = 17
          Caption = '#00445^ePrefer polite words^cPreferovat zdvoøilá slova'
          TabOrder = 2
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 88
          Width = 377
          Height = 17
          Caption = 
            '#00446^ePrefer popular words (marked by "pop")^cPreferovat popul' +
            'ární slova (oznaèena "pop")'
          TabOrder = 3
        end
        object CheckBox59: TCheckBox
          Left = 8
          Top = 112
          Width = 417
          Height = 17
          Caption = 
            '#00921^eOrder by word frequency (where available)^cTøídit podle ' +
            'frekvence slov (když je k dispozici)'
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
          't kanji pomocí kany u slov oznaèených "kana"'
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
          'obrazit jen výsledky, které se vejdou na stránku v "Auto" režimu'
        TabOrder = 3
      end
      object CheckBox50: TCheckBox
        Left = 16
        Top = 208
        Width = 433
        Height = 17
        Caption = 
          '#00449^eAuto-switch to Examples panel^cAutomaticky pøepnout na p' +
          'anel Pøíklady'
        TabOrder = 4
      end
      object CheckBox58: TCheckBox
        Left = 16
        Top = 232
        Width = 441
        Height = 17
        Caption = 
          '#00922^eDisplay word count for each word^cZobrazovat frekvenèní ' +
          'poèet u každého slova'
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
        Caption = '#00923^eEnglish font:^cFont pro anglické texty:'
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
        Caption = '#00456^eFont for small text:^cFont pro drobné nápisy:'
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
        Width = 319
        Height = 13
        Caption = '#00924^eRomanization && PinYin font:^cRomanizaèní a PinYin font:'
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
        Caption = '#00453^eJapanese fonts^cJaponské fonty'
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 301
          Height = 13
          Caption = '#00454^eFont for characters in grid:^cFont pro znaky v møížce:'
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
          Caption = '#00455^eFont for big characters:^cFont pro velké znaky:'
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
          Width = 327
          Height = 13
          Caption = 
            '#00875^eStroke order display font:^cFont pro zobrazení poøadí ta' +
            'hù:'
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
        Caption = '#00457^eChinese fonts:^cÈínské fonty:'
        TabOrder = 2
        object Label6: TLabel
          Left = 8
          Top = 24
          Width = 429
          Height = 13
          Caption = 
            '#00458^eBig5 font for traditional characters in grid:^cBig5 font' +
            ' pro tradièní znaky v møížce:'
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
          Width = 400
          Height = 13
          Caption = 
            '#00459^eBig5 font for big traditional characters:^cBig5 font pro' +
            ' velké tradièní znaky:'
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
          Width = 282
          Height = 13
          Caption = '#00460^eComplete unicode font:^cKompletní unicode font:'
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
          Width = 493
          Height = 13
          Caption = 
            '#00461^eGB2312 font for simplified characters in grid:^cGB2312 f' +
            'ont pro zjednodušené znaky v møížce:'
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
          Width = 464
          Height = 13
          Caption = 
            '#00462^eGB2312 font for big simplified characters:^cGB2312 font ' +
            'pro velké zjednodušené znaky:'
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
          Width = 872
          Height = 14
          Caption = 
            '#00463^eMake sure that the Big5 and GB2312 fonts are either the ' +
            'same or at least look the same.^cUjistìte se, že oba Big5 a GB23' +
            '12 fonty jsou buï stejné nebo stejnì vypadají.'
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
        Caption = '#00464^eSelect recommended fonts^cNastavit doporuèené fonty'
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
        Width = 374
        Height = 13
        Caption = 
          '#00466^cPoèet øádek na ètvercové stranì:^eNumber of lines on squ' +
          'are page:'
      end
      object CheckBox14: TCheckBox
        Left = 16
        Top = 296
        Width = 241
        Height = 17
        Caption = '#00467^ePrint horizontal lines^cTisknout horizontální èáry'
        TabOrder = 0
      end
      object CheckBox15: TCheckBox
        Left = 224
        Top = 296
        Width = 241
        Height = 17
        Caption = '#00468^ePrint vertical lines^cTisknout vertikální èáry'
        TabOrder = 1
      end
      object CheckBox16: TCheckBox
        Left = 16
        Top = 320
        Width = 241
        Height = 17
        Caption = '#00469^eVary colors^cStøídat barvy'
        TabOrder = 2
      end
      object CheckBox17: TCheckBox
        Left = 224
        Top = 320
        Width = 313
        Height = 17
        Caption = '#00470^eDo not print unlearned kanji^cNetisknout nenauèené znaky'
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
          Width = 233
          Height = 13
          Caption = '#00472^eUser settings:^cUživatelské nastavení:'
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
            '#00473^ePhonetic            |Meaning^cÈtení            |Význam'
            '#00474^eWritten             |Meaning^cZápis            |Význam'
            '#00475^ePhonetic |Written   |Meaning^cÈtení   |Zápis   |Význam'
            '#00476^eWritten  |Phonetic  |Meaning^cZápis   |Ètení   |Význam'
            '#00477^eRomaji   |Kana      |Meaning^cRomaji  |Kana    |Význam'
            '#00478^ePhonetic1           |Phonetic2^cÈtení1           |Ètení2'
            '#00479^eWritten1            |Written2^cZápis1           |Zápis2'
            '#00480^eMeaning1            |Meaning2^cVýznam1          |Význam2'
            
              '#00481^ePhonetic1|Phonetic2 |Phonetic3|Phonetic4^cÈtení1  |Ètení' +
              '2  |Ètení3  |Ètení4'
            
              '#00482^eWritten1 |Written2  |Written3 |Written4^cZápis1  |Zápis2' +
              '  |Zápis3  |Zápis4'
            
              '#00483^eMeaning1 |Meaning2  |Meaning3 |Meaning4^cVýznam1 |Význam' +
              '2 |Význam3 |Význam4'
            
              '#00484^ePhonetic1|Written1  |Phonetic2|Written2^cÈtení1  |Zápis1' +
              '  |Ètení2  |Zápis2'
            
              '#00485^ePhonetic1|Meaning1  |Phonetic2|Meaning2^cÈtení1  |Význam' +
              '1 |Ètení2  |Význam2'
            
              '#00486^eWritten1 |Meaning1  |Written2 |Meaning2^cZápis1  |Význam' +
              '1 |Zápis2  |Význam2'
            '#00487^eUser settings^cUživatelské nastavení')
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
      Caption = '#00488^eCharacter cards printing^cTisk karet znakù'
      ImageIndex = 5
      object Label12: TLabel
        Left = 8
        Top = 8
        Width = 486
        Height = 13
        Caption = 
          '#00489^cPoèet znakù na výšku na ètvercové stranì:^eVertical numb' +
          'er of characters on square page:'
      end
      object Label13: TLabel
        Left = 8
        Top = 56
        Width = 512
        Height = 13
        Caption = 
          '#00952^cProstor pro složeniny ze slovíèek (ve znacích):^eSpace f' +
          'or vocabulary compounds (in characters):'
      end
      object Label14: TLabel
        Left = 8
        Top = 104
        Width = 437
        Height = 13
        Caption = 
          '#00953^cPoèet složenin ze slovíèek na výšku:^eVertical number of' +
          ' vocabulary compounds:'
      end
      object Label15: TLabel
        Left = 8
        Top = 152
        Width = 214
        Height = 13
        Caption = '#00492^eCalligraphy font:^cKaligrafický font:'
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
        Width = 293
        Height = 13
        Caption = '#00951^cPoèet plných složenin:^eNumber of full compounds:'
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
          '#00493^ePrint vocabulary compounds^cTisknout složeniny ze slovíè' +
          'ek'
        TabOrder = 3
      end
      object CheckBox19: TCheckBox
        Left = 232
        Top = 224
        Width = 209
        Height = 17
        Caption = '#00494^ePrint radical^cTisknout radikál'
        TabOrder = 4
      end
      object CheckBox20: TCheckBox
        Left = 232
        Top = 248
        Width = 225
        Height = 17
        Caption = '#00495^ePrint alternate form^cTisknout alternativní formu'
        TabOrder = 5
      end
      object CheckBox21: TCheckBox
        Left = 8
        Top = 272
        Width = 257
        Height = 17
        Caption = '#00496^ePrint outer lines^cTisknout vnìjší èáry'
        TabOrder = 6
      end
      object CheckBox22: TCheckBox
        Left = 8
        Top = 224
        Width = 209
        Height = 17
        Caption = '#00497^ePrint readings^cTisknout ètení'
        TabOrder = 7
      end
      object CheckBox23: TCheckBox
        Left = 8
        Top = 296
        Width = 225
        Height = 17
        Caption = '#00498^ePrint inner lines^cTisknout vnitøní èáry'
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
          '#00499^ePrint vertically (right to left)^cTisknout vertikálnì (z' +
          'prava doleva)'
        TabOrder = 10
      end
      object CheckBox25: TCheckBox
        Left = 8
        Top = 344
        Width = 241
        Height = 17
        Caption = '#00500^eLeave space between columns^cVynech místo mezi sloupci'
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
        Caption = '#00502^ePrint stroke count^cTisknout poèet tahù'
        TabOrder = 13
      end
      object CheckBox52: TCheckBox
        Left = 232
        Top = 296
        Width = 225
        Height = 17
        Caption = '#00503^ePrint stroke order^cTisknout poøadí tahù'
        TabOrder = 14
      end
      object CheckBox62: TCheckBox
        Left = 232
        Top = 200
        Width = 217
        Height = 17
        Caption = '#00954^ePrint full compounds^cTisknout plné složeniny'
        TabOrder = 15
      end
      object CheckBox63: TCheckBox
        Left = 232
        Top = 320
        Width = 217
        Height = 17
        Caption = 
          '#00956^eSort compounds by frequency^cTøídit složeniny podle frek' +
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
      Caption = '#00504^eDatabase maintenance^cÚdržba databáze'
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
          '#00505^eCheck dictionary database indexes^cOtestuj indexy databá' +
          'ze slovníku'
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
          't uživatelskou databázi do textového souboru s oddìlovaèi'
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
          't uživatelskou databázi z textového souboru s oddìlovaèi'
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
          'ovíèek'
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
          'e pamìti'
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
      Caption = '#00300^eText translator^cPøeklad textu'
      ImageIndex = 8
      object Label25: TLabel
        Left = 16
        Top = 248
        Width = 428
        Height = 13
        Caption = 
          '#00510^eNumber of lines reserved to meaning:^cPoèet øádek rezerv' +
          'ovaných pro význam:'
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
          Width = 374
          Height = 13
          Caption = 
            '#00512^eNumber of lines on square page:^cPoèet øádek na ètvercov' +
            'é stranì:'
        end
        object CheckBox29: TCheckBox
          Left = 16
          Top = 24
          Width = 193
          Height = 17
          Caption = '#00272^eDisplay reading^cZobrazit ètení'
          TabOrder = 0
        end
        object CheckBox30: TCheckBox
          Left = 232
          Top = 24
          Width = 209
          Height = 17
          Caption = '#00513^eDisplay meaning^cZobrazit význam'
          TabOrder = 1
        end
        object CheckBox31: TCheckBox
          Left = 16
          Top = 48
          Width = 265
          Height = 17
          Caption = '#00514^eDo not use colors^cNepoužívat barvy'
          TabOrder = 2
        end
        object CheckBox37: TCheckBox
          Left = 232
          Top = 48
          Width = 377
          Height = 17
          Caption = 
            '#00515^ePrint vertically in columns^cTisknout vertikálnì do slou' +
            'pcù'
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
        Caption = '#00516^eDisplay lines^cZobrazovat èáry'
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
          '#00517^eDo not display meaning in learned words^cNezobrazovat vý' +
          'znam nauèených slova'
        TabOrder = 2
      end
      object CheckBox34: TCheckBox
        Left = 16
        Top = 128
        Width = 353
        Height = 17
        Caption = '#00518^eDo not search for particles^cNevyhledávat partikule'
        TabOrder = 3
      end
      object CheckBox35: TCheckBox
        Left = 16
        Top = 80
        Width = 377
        Height = 17
        Caption = 
          '#00519^eDo not display reading of learned kanji^cNezobrazovat èt' +
          'ení nauèených znakù'
        TabOrder = 4
      end
      object CheckBox36: TCheckBox
        Left = 16
        Top = 104
        Width = 393
        Height = 17
        Caption = 
          '#00884^eDisplay transcript above kana^cZobrazovat pøepis nad kan' +
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
          '#00521^eDo not translate hiragana-only words^cNepøekládat slova ' +
          'pouze v hiraganì'
        TabOrder = 6
      end
      object CheckBox40: TCheckBox
        Left = 16
        Top = 200
        Width = 401
        Height = 17
        Caption = 
          '#00522^eDisplay words with user-defined translation in bold^cZob' +
          'razovat slova s uživatelsky definovaným pøekladem tuènì'
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
        Caption = '#00523^eLeave space between lines^cVynechat místo mezi øádky'
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
          '#00524^eBreak lines only at word boundaries^cRozdìlovat øádky je' +
          'n na rozhraní slov'
        Checked = True
        State = cbChecked
        TabOrder = 10
      end
      object CheckBox27: TCheckBox
        Left = 360
        Top = 248
        Width = 97
        Height = 17
        Caption = '#00525^eDouble size^cDvojnásobná velikost'
        TabOrder = 11
      end
      object CheckBox41: TCheckBox
        Left = 248
        Top = 32
        Width = 401
        Height = 17
        Caption = 
          '#00526^eDisplay non-japanese chars in grey^cZobrazovat nejaponsk' +
          'é znaky šedì'
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
        Caption = '#00527^eShow editor hint^cZobrazit nápovìdu pøi psaní'
        Checked = True
        State = cbChecked
        TabOrder = 13
      end
      object CheckBox13: TCheckBox
        Left = 232
        Top = 224
        Width = 225
        Height = 17
        Caption = '#00528^eDisplay meaning on hint^cZobrazit význam v nápovìdì'
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
          '#00529^eLeave space even when reading is off^cVynechat místo i k' +
          'dyž je ètení vypnuto'
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
      Caption = '#00530^eCharacter details^cDetaily znakù'
      ImageIndex = 9
      object Label34: TLabel
        Left = 8
        Top = 8
        Width = 230
        Height = 13
        Caption = '#00531^eDisplayed items:^cZobrazené položky:'
      end
      object SpeedButton11: TSpeedButton
        Left = 448
        Top = 98
        Width = 23
        Height = 79
        Hint = '#00532^eMove up^cPøesuò nahoru'
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
        Hint = '#00533^eMove down^cPøesuò dolù'
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
        Caption = '#00534^eAdd...^cPøidat...'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 180
        Top = 339
        Width = 113
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00535^eChange...^cZmìnit...'
        TabOrder = 2
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 350
        Top = 339
        Width = 97
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00536^eDelete^cZrušit'
        TabOrder = 3
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 296
        Top = 1
        Width = 150
        Height = 22
        Caption = '#00537^eReset to default^cNastavit implicitní'
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
        Width = 349
        Height = 13
        Caption = 
          '#00539^ePopup delay (x100 ms):^cZpoždìní pøed zobrazením (x100 m' +
          's):'
      end
      object Label37: TLabel
        Left = 8
        Top = 112
        Width = 341
        Height = 13
        Caption = 
          '#00540^eText scan range (pixels):^cRozsah prohledávání textu (pi' +
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
        Width = 396
        Height = 13
        Caption = 
          '#00543^eMax. number of dictionary entries:^cMaximální poèet polo' +
          'žek ze slovníku:'
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
        Width = 850
        Height = 13
        Caption = 
          '#00545^eNumber of characters reserved for compounds on a card (a' +
          'ffects overall popup width):^cPoèet znakù rezervovaných pro slož' +
          'eniny na kartì (ovlivní celkovou šíøku okna):'
      end
      object Label45: TLabel
        Left = 104
        Top = 224
        Width = 155
        Height = 13
        Caption = '#00546^eMinimum:^cMinimálnì:'
      end
      object Label46: TLabel
        Left = 264
        Top = 224
        Width = 161
        Height = 13
        Caption = '#00547^eMaximum:^cMaximálnì:'
      end
      object CheckBox28: TCheckBox
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = 
          '#00548^eShow translation for japanese/chinese text^cZobrazit pøe' +
          'klad pro japonský/èínský text'
        TabOrder = 0
      end
      object CheckBox47: TCheckBox
        Left = 8
        Top = 32
        Width = 353
        Height = 17
        Caption = 
          '#00549^eShow translation for english text (only for screen)^cZob' +
          'razit pøeklad pro anglický text (pouze v režimu obrazovky)'
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
        Caption = '#00553^eChange^cZmìnit'
        TabOrder = 1
        OnClick = Button12Click
      end
      object Button14: TButton
        Left = 344
        Top = 332
        Width = 121
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '#00554^eRevert to default^cVrátit na výchozí'
        TabOrder = 2
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 336
        Top = 8
        Width = 129
        Height = 25
        Caption = '#00555^eSet all to default^cNastavit vše na výchozí'
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
          '#00298^eCharacter list^cSeznam znakù'
          '#00556^eWord grids^cMøížky slov'
          '#00557^eWord markers^cPøíznaky slov'
          '#00558^eEditor & translator^cEditor / Pøekládání'
          '#00559^ePopup tool^cPopup nástroj')
      end
      object CheckBox3: TCheckBox
        Left = 8
        Top = 49
        Width = 457
        Height = 17
        Caption = 
          '#00560^eDo not use colors in character list, use Windows default' +
          ' colors instead^cNepoužívat barvy v seznamu znakù, použít nastav' +
          'ení Windows'
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
          'instead^cNepoužívat barvy v editoru, použít místo toho barvy Win' +
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
          ' green - only in names^cBarvy: èerná - bìžný znak, šedá - vzácný' +
          ', modrá - nauèený, zelená - pouze ve jménech'
        Caption = 
          '#00563^eDo not use colors in word grids, use Windows default col' +
          'ors instead^cNepoužívat barvy v møížce slov, použít barvy Window' +
          's'
        TabOrder = 7
        OnClick = CheckBox9Click
      end
    end
    object TabSheet13: TTabSheet
      Caption = '^eEditor^cEditor'
      ImageIndex = 12
      object CheckBox60: TCheckBox
        Left = 8
        Top = 8
        Width = 457
        Height = 17
        Caption = '^eAuto-save file on exit^cAutomaticky uložit soubor pøi ukonèení'
        TabOrder = 0
      end
      object CheckBox61: TCheckBox
        Left = 8
        Top = 32
        Width = 457
        Height = 17
        Caption = 
          '^eAuto-load last opened file on start^cAutomaticky nahrát naposl' +
          'ed otevøený soubor pøi startu'
        TabOrder = 1
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
          'starts^cVytvoøit ANNOTATE.PKG ze souborù .ANO pøi každém startu ' +
          'WaKanu když je potøeba'
        TabOrder = 1
      end
      object CheckBox66: TCheckBox
        Left = 11
        Top = 150
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to play sound files^cPovolit anotacím pøehrá' +
          't zvukové soubory '
        TabOrder = 2
        Visible = False
      end
      object CheckBox67: TCheckBox
        Left = 11
        Top = 78
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to display pictures^cPovolit anotacím zobraz' +
          'it obrázky'
        TabOrder = 3
      end
      object CheckBox68: TCheckBox
        Left = 11
        Top = 102
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to display web pages^cPovolit anotacím zobra' +
          'zit webové stránky'
        TabOrder = 4
      end
      object CheckBox69: TCheckBox
        Left = 11
        Top = 126
        Width = 326
        Height = 17
        Caption = 
          '^eAllow annotations to change foreground color^cPovolit anotacím' +
          ' mìnit barvu popøedí'
        TabOrder = 5
      end
      object Button16: TButton
        Left = 72
        Top = 352
        Width = 337
        Height = 25
        Caption = 
          '^eHelp for the annotation feature^cNápovìda pro funkci anotace (' +
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
    TabOrder = 1
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object Button6: TButton
    Left = 8
    Top = 527
    Width = 177
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '#00929^eChange language^cZmìna jazyka'
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
    Ctl3D = True
    Left = 148
    Top = 352
  end
end
