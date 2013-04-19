object fKanjiCompounds: TfKanjiCompounds
  Left = 227
  Top = 187
  BorderStyle = bsNone
  Caption = '#00125^eCompounds'
  ClientHeight = 178
  ClientWidth = 468
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 120
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 178
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    DesignSize = (
      468
      178)
    object sbShowVocab: TSpeedButton
      Left = 208
      Top = 153
      Width = 60
      Height = 22
      Hint = '#00151^eShow user vocabulary compounds'
      Anchors = [akLeft, akBottom]
      GroupIndex = 4
      Down = True
      Caption = '#00152^eUserDict'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton11Click
    end
    object sbShowDict: TSpeedButton
      Left = 152
      Top = 153
      Width = 54
      Height = 22
      Hint = '#00153^eShow dictionary compounds'
      Anchors = [akLeft, akBottom]
      GroupIndex = 4
      Caption = '#00154^eDict.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton11Click
    end
    object sbCopyToClipboard: TSpeedButton
      Left = 380
      Top = 149
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
      OnClick = sbCopyToClipboardClick
    end
    object sbInsertIntoVocab: TSpeedButton
      Left = 282
      Top = 149
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
      OnClick = sbInsertIntoVocabClick
    end
    object BlankPanel1: TBlankPanel
      Left = 8
      Top = 9
      Width = 451
      Height = 140
      Anchors = [akLeft, akTop, akRight, akBottom]
      TextLeft = 119
      TextTop = 11
      Text = '#00155^eNo words were found.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object cbLeftMatchOnly: TCheckBox
      Left = 9
      Top = 154
      Width = 89
      Height = 17
      Hint = '#00156^eShow only compounds with beginning character'
      Anchors = [akLeft, akBottom]
      Caption = '#00878^eBeg.'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = SpeedButton11Click
    end
    object StringGrid1: TWakanWordGrid
      Left = 9
      Top = 10
      Width = 449
      Height = 138
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      ColCount = 3
      DefaultRowHeight = 16
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
      PopupMenu = PopupMenu1
      TabOrder = 1
      OnDblClick = StringGrid1DblClick
      OnDrawCell = StringGrid1DrawCell
      OnMouseDown = StringGrid1MouseDown
      OnMouseMove = StringGrid1MouseMove
      OnMouseUp = StringGrid1MouseUp
      OnSelectCell = StringGrid1SelectCell
      ColWidths = (
        110
        138
        177)
    end
    object cbPopularOnly: TCheckBox
      Left = 62
      Top = 154
      Width = 39
      Height = 17
      Hint = '#00158^eShow only compounds marked as popular'
      Anchors = [akLeft, akBottom]
      Caption = '^ePop'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = SpeedButton11Click
    end
    object cbSortByFrequency: TCheckBox
      Left = 105
      Top = 154
      Width = 39
      Height = 17
      Hint = '^eShow compounds sorted by frequency'
      Anchors = [akLeft, akBottom]
      Caption = '^eFreq'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = SpeedButton11Click
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 24
    Top = 24
    object miResetColumns: TMenuItem
      Caption = '#01002^eReset columns'
      OnClick = miResetColumnsClick
    end
  end
end
