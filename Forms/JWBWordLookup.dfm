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
  inherited Bevel: TPanel
    Width = 687
    Height = 248
    TabOrder = 2
    ExplicitWidth = 704
    ExplicitHeight = 214
    inherited btnGoToVocab: TSpeedButton
      Left = 408
      Top = 219
      ExplicitTop = 185
    end
    inherited btnAddToVocab: TSpeedButton
      Left = 504
      Top = 219
      ExplicitTop = 185
    end
    inherited btnCopyToClipboard: TSpeedButton
      Left = 600
      Top = 219
      ExplicitTop = 185
    end
    object Panel2: TPanel [3]
      AlignWithMargins = True
      Left = 3
      Top = 224
      Width = 681
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitWidth = 757
      object SpeedButton6: TSpeedButton
        Left = 0
        Top = 0
        Width = 123
        Height = 21
        Hint = '#00650^eCharacters in word'
        Align = alLeft
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
        OnClick = SpeedButton6Click
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitHeight = 17
      end
      object SpeedButton9: TSpeedButton
        Left = 123
        Top = 0
        Width = 130
        Height = 21
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
        OnClick = SpeedButton9Click
        ExplicitLeft = 144
        ExplicitTop = 8
        ExplicitHeight = 17
      end
    end
    inherited BlankPanel: TBlankPanel
      Left = 0
      Top = 26
      Width = 687
      Height = 195
      Align = alClient
      ExplicitTop = 62
      ExplicitWidth = 687
      ExplicitHeight = 123
    end
    inherited StringGrid: TWakanWordGrid
      AlignWithMargins = True
      Left = 3
      Top = 29
      Width = 681
      Height = 189
      Align = alClient
      ExplicitTop = 63
      ExplicitWidth = 685
      ExplicitHeight = 121
      ColWidths = (
        110
        138
        409)
    end
    object Panel1: TPanel
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
      TabOrder = 2
      ExplicitLeft = 3
      ExplicitTop = 3
      ExplicitWidth = 815
      object btnLookupClip: TSpeedButton
        Left = 364
        Top = 0
        Width = 73
        Height = 23
        Hint = '#00647^eSearch by Kanji stored in clipboard (F4)'
        Align = alLeft
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
        Left = 441
        Top = 0
        Width = 23
        Height = 23
        Hint = '#00656^eSearch exact word (F5)'
        Align = alLeft
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
        Left = 464
        Top = 0
        Width = 23
        Height = 23
        Hint = '#00657^eSearch beginning (F6)'
        Align = alLeft
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
        Left = 487
        Top = 0
        Width = 23
        Height = 23
        Hint = '#00658^eSearch end (F7)'
        Align = alLeft
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
        Left = 510
        Top = 0
        Width = 25
        Height = 23
        Hint = '#00930^eSearch middle'
        Align = alLeft
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
        Left = 539
        Top = 0
        Width = 23
        Height = 23
        Hint = '#00661^eSearch for inflected words / conjugated verbs'
        Align = alLeft
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
        Left = 562
        Top = 0
        Width = 33
        Height = 23
        Hint = 
          '#00662^eAuto-preview while typing (full search with arrow button' +
          ')'
        Align = alLeft
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
        Left = 599
        Top = 0
        Width = 25
        Height = 23
        Hint = '#00663^eUse dictionaries in group 1 (Ctrl-1)'
        Align = alLeft
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
        Left = 624
        Top = 0
        Width = 25
        Height = 23
        Hint = '#00664^eUse dictionaries in group 2 (Ctrl-2)'
        Align = alLeft
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
        Left = 649
        Top = 0
        Width = 25
        Height = 23
        Hint = '#00665^eUse dictionaries in group 3 (Ctrl-3)'
        Align = alLeft
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
        Left = 437
        Top = 0
        Width = 4
        Height = 23
        Align = alLeft
        Shape = bsSpacer
      end
      object Bevel2: TBevel
        Left = 535
        Top = 0
        Width = 4
        Height = 23
        Align = alLeft
        Shape = bsSpacer
      end
      object Bevel3: TBevel
        Left = 595
        Top = 0
        Width = 4
        Height = 23
        Align = alLeft
        Shape = bsSpacer
      end
      object Bevel4: TBevel
        Left = 225
        Top = 0
        Width = 4
        Height = 23
        Align = alLeft
        Shape = bsSpacer
      end
      object Edit1: TEdit
        Left = 0
        Top = 0
        Width = 225
        Height = 23
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = Edit1Change
        OnClick = Edit1Click
      end
      object btnLookupMode: TButton
        Left = 229
        Top = 0
        Width = 135
        Height = 23
        Hint = 
          '#01133^Search by reading, writing or meaning, depending on what ' +
          'you type'
        Align = alLeft
        Caption = '#01132^Any matches'
        DropDownMenu = pmLookupMode
        PopupMenu = pmLookupMode
        Style = bsSplitButton
        TabOrder = 1
        OnClick = btnLookupModeClick
        ExplicitLeft = 272
        ExplicitTop = 4
        ExplicitHeight = 22
      end
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
    ExplicitTop = 214
    ExplicitWidth = 704
  end
  object Panel3: TPanel [2]
    Left = 687
    Top = 0
    Width = 0
    Height = 248
    Align = alRight
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    ExplicitLeft = 704
    ExplicitHeight = 214
  end
  inherited pmPopup: TPopupMenu
    Left = 32
    Top = 40
  end
  inherited ilImages: TImageList
    Left = 96
    Top = 40
    Bitmap = {
      494C0101030008008C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
end
