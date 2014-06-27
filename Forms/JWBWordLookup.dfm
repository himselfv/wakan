inherited fWordLookup: TfWordLookup
  Left = 57
  Top = 137
  Caption = '#00642^eDictionary lookup'
  ClientHeight = 248
  ClientWidth = 687
  Font.Name = 'MS Sans Serif'
  OldCreateOrder = True
  Scaled = False
  ShowHint = True
  OnShow = FormShow
  ExplicitWidth = 687
  ExplicitHeight = 248
  PixelsPerInch = 96
  TextHeight = 13
  inherited BottomPanel: TPanel
    Top = 223
    Width = 681
    TabOrder = 1
    ExplicitTop = 223
    ExplicitWidth = 681
    inherited btnGoToVocab: TSpeedButton
      Left = 290
      ExplicitLeft = 413
      ExplicitTop = 26
    end
    inherited btnAddToVocab: TSpeedButton
      Left = 384
      ExplicitLeft = 507
      ExplicitTop = 26
    end
    inherited btnCopyToClipboard: TSpeedButton
      Left = 478
      ExplicitLeft = 601
      ExplicitTop = 26
    end
    object btnWordKanji: TSpeedButton
      Left = 558
      Top = 0
      Width = 123
      Height = 22
      Hint = '#00650^eCharacters in word'
      Align = alRight
      AllowAllUp = True
      GroupIndex = 6
      Caption = '#00651^eChar. in word'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnWordKanjiClick
      ExplicitLeft = 607
    end
    object btnExamples: TSpeedButton
      Left = 0
      Top = 0
      Width = 130
      Height = 22
      Hint = '#00062^eAdd to vocabulary'
      Align = alLeft
      AllowAllUp = True
      GroupIndex = 4
      Caption = '#00315^eExamples'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnExamplesClick
      ExplicitLeft = 144
      ExplicitTop = 8
      ExplicitHeight = 17
    end
  end
  object pnlDockExamples: TPanel [1]
    Left = 0
    Top = 248
    Width = 687
    Height = 0
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
  end
  inherited BlankPanel: TBlankPanel
    Top = 26
    Width = 687
    Height = 194
    ExplicitTop = 26
    ExplicitWidth = 687
    ExplicitHeight = 194
  end
  inherited StringGrid: TWakanWordGrid
    Top = 27
    Width = 685
    Height = 192
    TabOrder = 3
    ExplicitTop = 27
    ExplicitWidth = 685
    ExplicitHeight = 192
    ColWidths = (
      110
      138
      413)
  end
  object TopPanel: TPanel [4]
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 687
    Height = 23
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object btnLookupClip: TSpeedButton
      Left = 377
      Top = 0
      Width = 73
      Height = 23
      Hint = '#00647^eSearch by Kanji stored in clipboard (F4)'
      Align = alRight
      AllowAllUp = True
      GroupIndex = 1
      Caption = '#00289^eBy clipboard'
      PopupMenu = pmLookupMode
      OnClick = btnLookupClipClick
      ExplicitLeft = 413
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnMatchExact: TSpeedButton
      Left = 454
      Top = 0
      Width = 23
      Height = 23
      Hint = '#00656^eSearch exact word (F5)'
      Align = alRight
      GroupIndex = 7
      Down = True
      Caption = 'A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnMatchExactClick
      ExplicitLeft = 535
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnMatchLeft: TSpeedButton
      Left = 477
      Top = 0
      Width = 23
      Height = 23
      Hint = '#00657^eSearch beginning (F6)'
      Align = alRight
      GroupIndex = 7
      Caption = 'A+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnMatchExactClick
      ExplicitLeft = 559
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnMatchRight: TSpeedButton
      Left = 500
      Top = 0
      Width = 23
      Height = 23
      Hint = '#00658^eSearch end (F7)'
      Align = alRight
      GroupIndex = 7
      Caption = '+A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnMatchExactClick
      ExplicitLeft = 583
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnMatchAnywhere: TSpeedButton
      Left = 523
      Top = 0
      Width = 25
      Height = 23
      Hint = '#00930^eSearch middle'
      Align = alRight
      GroupIndex = 7
      Caption = '+A+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnMatchExactClick
      ExplicitLeft = 607
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnInflect: TSpeedButton
      Left = 552
      Top = 0
      Width = 23
      Height = 23
      Hint = '#00661^eSearch for inflected words / conjugated verbs'
      Align = alRight
      AllowAllUp = True
      GroupIndex = 8
      Caption = 'Inf'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 742
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object sbAutoPreview: TSpeedButton
      Left = 575
      Top = 0
      Width = 33
      Height = 23
      Hint = 
        '#00662^eAuto-preview while typing (full search with arrow button' +
        ')'
      Align = alRight
      AllowAllUp = True
      GroupIndex = 9
      Caption = 'Auto'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 766
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnDictGroup1: TSpeedButton
      Left = 612
      Top = 0
      Width = 25
      Height = 23
      Hint = '#00663^eUse dictionaries in group 1 (Ctrl-1)'
      Align = alRight
      GroupIndex = 10
      Down = True
      Caption = 'D1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 801
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnDictGroup2: TSpeedButton
      Left = 637
      Top = 0
      Width = 25
      Height = 23
      Hint = '#00664^eUse dictionaries in group 2 (Ctrl-2)'
      Align = alRight
      GroupIndex = 10
      Caption = 'D2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 832
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object btnDictGroup3: TSpeedButton
      Left = 662
      Top = 0
      Width = 25
      Height = 23
      Hint = '#00665^eUse dictionaries in group 3 (Ctrl-3)'
      Align = alRight
      GroupIndex = 10
      Caption = 'D3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 858
      ExplicitTop = 4
      ExplicitHeight = 22
    end
    object Bevel1: TBevel
      Left = 450
      Top = 0
      Width = 4
      Height = 23
      Align = alRight
      Shape = bsSpacer
      ExplicitLeft = 437
    end
    object Bevel2: TBevel
      Left = 548
      Top = 0
      Width = 4
      Height = 23
      Align = alRight
      Shape = bsSpacer
      ExplicitLeft = 535
    end
    object Bevel3: TBevel
      Left = 608
      Top = 0
      Width = 4
      Height = 23
      Align = alRight
      Shape = bsSpacer
      ExplicitLeft = 595
    end
    object Bevel4: TBevel
      Left = 238
      Top = 0
      Width = 4
      Height = 23
      Align = alRight
      Shape = bsSpacer
      ExplicitLeft = 225
    end
    object btnSearch: TBitBtn
      Left = 206
      Top = 0
      Width = 32
      Height = 23
      Hint = 
        '#00668^eSearch results did not fit one page, click to display ev' +
        'erything'
      Align = alRight
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333033333
        33333333373F33333333333330B03333333333337F7F33333333333330F03333
        333333337F7FF3333333333330B00333333333337F773FF33333333330F0F003
        333333337F7F773F3333333330B0B0B0333333337F7F7F7F3333333300F0F0F0
        333333377F73737F33333330B0BFBFB03333337F7F33337F33333330F0FBFBF0
        3333337F7333337F33333330BFBFBFB033333373F3333373333333330BFBFB03
        33333337FFFFF7FF3333333300000000333333377777777F333333330EEEEEE0
        33333337FFFFFF7FF3333333000000000333333777777777F33333330000000B
        03333337777777F7F33333330000000003333337777777773333}
      NumGlyphs = 2
      ParentFont = False
      TabOrder = 2
      OnClick = btnSearchClick
    end
    object Edit1: TEdit
      Left = 0
      Top = 0
      Width = 206
      Height = 23
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = Edit1Change
      OnClick = Edit1Click
      OnKeyPress = Edit1KeyPress
      ExplicitHeight = 24
    end
    object btnLookupMode: TButton
      Left = 242
      Top = 0
      Width = 135
      Height = 23
      Hint = 
        '#01133^Search by reading, writing or meaning, depending on what ' +
        'you type'
      Align = alRight
      Caption = '#01132^Any matches'
      DropDownMenu = pmLookupMode
      PopupMenu = pmLookupMode
      Style = bsSplitButton
      TabOrder = 1
      TabStop = False
      OnClick = btnLookupModeClick
    end
  end
  object CharInWordDock: TPanel [5]
    Left = 687
    Top = 26
    Width = 0
    Height = 194
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 5
  end
  inherited pmPopup: TPopupMenu
    Left = 32
    Top = 40
  end
  inherited ilImages: TImageList
    Left = 96
    Top = 40
    Bitmap = {
      494C010103000800C00010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00000000000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF00000000000000FFFFFF0000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF0000000000000000000000FFFFFF0000000000FFFFFF0000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000000000000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF0000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF0000000000000000000000FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      00000000000000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF00FFFFFF0000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000FF000000FF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFE7F8FE3F0000
      1FFFE7F8F81F0000041F81FFF40F0000000F81FCE0070000000FE7FC80030000
      0007E7FF400100000001FFFC000000000000FEFC000000000001FE7F80010000
      003F8013C0030000FC7F8013E00F0000FFFFFE7FF07F0000FFFFFEF8F8FF0000
      FFFFFFF8FFFF0000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object pmLookupMode: TPopupMenu
    Images = ilImages
    OnPopup = pmPopupPopup
    Left = 32
    Top = 96
    object miLookupAuto: TMenuItem
      AutoCheck = True
      Caption = '#01132^Auto/all'
      Checked = True
      GroupIndex = 1
      Hint = 
        '#01133^Search by reading, writing or meaning, depending on what ' +
        'you type'
      RadioItem = True
      OnClick = miLookupAutoClick
    end
    object miLookupJtoE: TMenuItem
      AutoCheck = True
      Caption = '#00644^eJ -> E'
      GroupIndex = 1
      Hint = '#00643^Search by japanese reading (F2)'
      RadioItem = True
      OnClick = miLookupJtoEClick
    end
    object miLookupEtoJ: TMenuItem
      AutoCheck = True
      Caption = '#00646^e&E -> J'
      GroupIndex = 1
      Hint = '#00645^Search by english meaning (F3)'
      RadioItem = True
      OnClick = miLookupEtoJClick
    end
  end
  object Actions: TActionList
    Left = 160
    Top = 40
    object aLookupAuto: TAction
      Caption = '#01132^Auto/all'
      Hint = 
        '#01133^Search by reading, writing or meaning, depending on what ' +
        'you type'
      ShortCut = 113
      OnExecute = aLookupAutoExecute
    end
    object aKanji: TAction
      Category = 'NotYetUsed'
      Caption = '#00237^e&Characters in word'
    end
    object aExamples: TAction
      Category = 'NotYetUsed'
      Caption = '#00315^eExamples'
    end
    object aJapanese: TAction
      Caption = '#00287^Japanese/Chinese -> English'
      Hint = '#00643^Search by japanese reading'
      OnExecute = aJapaneseExecute
    end
    object aEnglish: TAction
      Caption = '#00288^English -> Japanese/Chinese'
      Hint = '#00645^Search by english meaning'
      ShortCut = 114
      OnExecute = aEnglishExecute
    end
    object aClipboard: TAction
      Caption = '#00289^Search by clipboard'
      Hint = '#00647^Search by Kanji stored in clipboard'
      ShortCut = 115
      OnExecute = aClipboardExecute
    end
    object aAddClipboard: TAction
      Caption = '#00285^Add to clipboard'
      OnExecute = aAddClipboardExecute
    end
    object aExact: TAction
      Caption = '#00290^Search exact word'
      ShortCut = 116
      OnExecute = aExactExecute
    end
    object aBeginning: TAction
      Caption = '#00291^Search beginning'
      ShortCut = 117
      OnExecute = aBeginningExecute
    end
    object aEnd: TAction
      Caption = '#00292^Search ending'
      ShortCut = 118
      OnExecute = aEndExecute
    end
    object aInflect: TAction
      Caption = '#00301^Search inflected words'
      OnExecute = aInflectExecute
    end
    object aAuto: TAction
      Caption = '#00302^Auto-search while typing'
      OnExecute = aAutoExecute
    end
    object aGroup1: TAction
      Caption = '#00303^Use dictionaries in group 1'
      ShortCut = 16433
      OnExecute = aGroup1Execute
    end
    object aGroup2: TAction
      Caption = '#00304^Use dictionaries in group 2'
      ShortCut = 16434
      OnExecute = aGroup2Execute
    end
    object aGroup3: TAction
      Caption = '#00305^Use dictionaries in group 3'
      ShortCut = 16435
      OnExecute = aGroup3Execute
    end
    object aMiddle: TAction
      Caption = '#00919^Search substring'
      OnExecute = aMiddleExecute
    end
  end
end
