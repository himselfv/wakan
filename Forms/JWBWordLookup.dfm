object fWordLookup: TfWordLookup
  Left = 57
  Top = 137
  BorderStyle = bsSizeToolWin
  Caption = '#00642^eDictionary lookup'
  ClientHeight = 214
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 704
    Height = 214
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    DesignSize = (
      704
      214)
    object btnLookupJtoE: TSpeedButton
      Left = 11
      Top = 4
      Width = 110
      Height = 22
      Hint = '#00643^eSearch by japanese reading (F2)'
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = '#00644^eJ -> E'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object btnLookupEtoJ: TSpeedButton
      Left = 123
      Top = 4
      Width = 107
      Height = 22
      Hint = '#00645^eSearch by english meaning (F3)'
      AllowAllUp = True
      GroupIndex = 1
      Caption = '#00646^e&E -> J'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object btnLookupClip: TSpeedButton
      Left = 232
      Top = 4
      Width = 73
      Height = 22
      Hint = '#00647^eSearch by Kanji stored in clipboard (F4)'
      AllowAllUp = True
      GroupIndex = 1
      Caption = '#00289^eBy clipboard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object SpeedButton5: TSpeedButton
      Left = 290
      Top = 193
      Width = 128
      Height = 17
      Hint = '#00648^eSelected word (Ctrl-Alt-F)'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 2
      Caption = '#00649^eSelected word'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Visible = False
      OnClick = SpeedButton5Click
      ExplicitTop = 195
    end
    object SpeedButton6: TSpeedButton
      Left = 7
      Top = 193
      Width = 123
      Height = 17
      Hint = '#00650^eCharacters in word'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
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
      ExplicitTop = 195
    end
    object SpeedButton7: TSpeedButton
      Left = 271
      Top = 193
      Width = 123
      Height = 17
      Hint = '#00652^eInformation (Ctrl-Alt-I)'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 3
      Caption = '#00653^eWord information'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Visible = False
      OnClick = SpeedButton7Click
      ExplicitTop = 195
    end
    object SpeedButton9: TSpeedButton
      Left = 135
      Top = 193
      Width = 130
      Height = 17
      Hint = '#00062^eAdd to vocabulary'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
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
      ExplicitTop = 195
    end
    object SpeedButton10: TSpeedButton
      Left = 311
      Top = 4
      Width = 23
      Height = 22
      Hint = '#00656^eSearch exact word (F5)'
      GroupIndex = 7
      Down = True
      Caption = 'A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton10Click
    end
    object SpeedButton11: TSpeedButton
      Left = 335
      Top = 4
      Width = 23
      Height = 22
      Hint = '#00657^eSearch beginning (F6)'
      GroupIndex = 7
      Caption = 'A+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton10Click
    end
    object SpeedButton12: TSpeedButton
      Left = 359
      Top = 4
      Width = 23
      Height = 22
      Hint = '#00658^eSearch end (F7)'
      GroupIndex = 7
      Caption = '+A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton10Click
    end
    object btnCopyToClipboard: TSpeedButton
      Left = 617
      Top = 185
      Width = 80
      Height = 22
      Hint = '#00659^eInsert word into clipboard'
      Anchors = [akRight, akBottom]
      Caption = '#00660^eClipboard'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333FF33333333FF333993333333300033377F3333333777333993333333
        300033F77FFF3333377739999993333333333777777F3333333F399999933333
        33003777777333333377333993333333330033377F3333333377333993333333
        3333333773333333333F333333333333330033333333F33333773333333C3333
        330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
        993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
        333333333337733333FF3333333C333330003333333733333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = btnCopyToClipboardClick
      ExplicitTop = 187
    end
    object SpeedButton4: TSpeedButton
      Left = 414
      Top = 4
      Width = 23
      Height = 22
      Hint = '#00661^eSearch for inflected words / conjugated verbs'
      AllowAllUp = True
      GroupIndex = 8
      Caption = 'Inf'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object SpeedButton13: TSpeedButton
      Left = 438
      Top = 4
      Width = 33
      Height = 22
      Hint = 
        '#00662^eAuto-preview while typing (full search with arrow button' +
        ')'
      AllowAllUp = True
      GroupIndex = 9
      Caption = 'Auto'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object SpeedButton14: TSpeedButton
      Left = 478
      Top = 4
      Width = 25
      Height = 22
      Hint = '#00663^eUse dictionaries in group 1 (Ctrl-1)'
      GroupIndex = 10
      Down = True
      Caption = 'D1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object SpeedButton15: TSpeedButton
      Left = 504
      Top = 4
      Width = 25
      Height = 22
      Hint = '#00664^eUse dictionaries in group 2 (Ctrl-2)'
      GroupIndex = 10
      Caption = 'D2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object SpeedButton16: TSpeedButton
      Left = 530
      Top = 4
      Width = 25
      Height = 22
      Hint = '#00665^eUse dictionaries in group 3 (Ctrl-3)'
      GroupIndex = 10
      Caption = 'D3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = btnLookupJtoEClick
    end
    object Label2: TLabel
      Left = 604
      Top = 37
      Width = 92
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = '#00666^eAll visible'
      Enabled = False
    end
    object SpeedButton17: TSpeedButton
      Left = 521
      Top = 185
      Width = 94
      Height = 22
      Hint = '#00667^eInsert word into vocabulary'
      Anchors = [akRight, akBottom]
      Caption = '#00215^eVocabulary'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333FF33333333FF333993333333300033377F3333333777333993333333
        300033F77FFF3333377739999993333333333777777F3333333F399999933333
        33003777777333333377333993333333330033377F3333333377333993333333
        3333333773333333333F333333333333330033333333F33333773333333C3333
        330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
        993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
        333333333337733333FF3333333C333330003333333733333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton17Click
      ExplicitTop = 187
    end
    object Label3: TLabel
      Left = 688
      Top = 17
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = '0'
    end
    object SpeedButton18: TSpeedButton
      Left = 383
      Top = 4
      Width = 25
      Height = 22
      Hint = '#00930^eSearch middle'
      GroupIndex = 7
      Caption = '+A+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton10Click
    end
    object SpeedButton19: TSpeedButton
      Left = 425
      Top = 185
      Width = 94
      Height = 22
      Hint = '#00931^eGo to this word in vocabulary'
      Anchors = [akRight, akBottom]
      Caption = '#00215^eVocabulary'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333FFF333333333333000333333333
        3333777FFF3FFFFF33330B000300000333337F777F777773F333000E00BFBFB0
        3333777F773333F7F333000E0BFBF0003333777F7F3337773F33000E0FBFBFBF
        0333777F7F3333FF7FFF000E0BFBF0000003777F7F3337777773000E0FBFBFBF
        BFB0777F7F33FFFFFFF7000E0BF000000003777F7FF777777773000000BFB033
        33337777773FF733333333333300033333333333337773333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton19Click
      ExplicitTop = 187
    end
    object Edit1: TEdit
      Left = 8
      Top = 32
      Width = 616
      Height = 24
      Anchors = [akLeft, akTop, akRight]
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
    object BitBtn1: TBitBtn
      Left = 626
      Top = 32
      Width = 70
      Height = 25
      Hint = 
        '#00668^eSearch results did not fit one page, click to display ev' +
        'erything'
      Anchors = [akTop, akRight]
      Caption = '#00669^eSearch'
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
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object BlankPanel: TBlankPanel
      Left = 10
      Top = 63
      Width = 687
      Height = 122
      Anchors = [akLeft, akTop, akRight, akBottom]
      TextLeft = 20
      TextTop = 27
      Text = '#00155^eNo words were found.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object StringGrid1: TWakanWordGrid
      Left = 11
      Top = 64
      Width = 685
      Height = 119
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      ColCount = 3
      DefaultRowHeight = 16
      DefaultDrawing = False
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
      PopupMenu = PopupMenu1
      TabOrder = 3
      OnDblClick = StringGrid1DblClick
      OnDrawCell = StringGrid1DrawCell
      OnMouseDown = StringGrid1MouseDown
      OnMouseMove = StringGrid1MouseMove
      OnMouseUp = StringGrid1MouseUp
      OnSelectCell = StringGrid1SelectCell
      ColWidths = (
        131
        128
        402)
    end
  end
  object pnlDockExamples: TPanel
    Left = 0
    Top = 214
    Width = 704
    Height = 0
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 704
    Top = 0
    Width = 0
    Height = 214
    Align = alRight
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 32
    Top = 48
    object miResetColumns: TMenuItem
      Caption = '#01002^eReset columns'
      OnClick = miResetColumnsClick
    end
  end
end
