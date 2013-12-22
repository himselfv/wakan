object fWordLookupBase: TfWordLookupBase
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 178
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    object btnGoToVocab: TSpeedButton
      Left = 189
      Top = 149
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
      OnClick = btnGoToVocabClick
      ExplicitLeft = 425
      ExplicitTop = 187
    end
    object btnAddToVocab: TSpeedButton
      Left = 285
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
      OnClick = btnAddToVocabClick
      ExplicitLeft = 521
      ExplicitTop = 187
    end
    object btnCopyToClipboard: TSpeedButton
      Left = 381
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
      OnClick = btnCopyToClipboardClick
      ExplicitLeft = 617
      ExplicitTop = 187
    end
    object BlankPanel: TBlankPanel
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
    object StringGrid: TWakanWordGrid
      Left = 9
      Top = 10
      Width = 449
      Height = 138
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      ColCount = 3
      DefaultRowHeight = 16
      DefaultDrawing = False
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
      PopupMenu = pmHeader
      TabOrder = 0
      OnDblClick = StringGridDblClick
      OnDrawCell = StringGridDrawCell
      OnKeyPress = StringGridKeyPress
      OnMouseDown = StringGridMouseDown
      OnMouseMove = StringGridMouseMove
      OnMouseUp = StringGridMouseUp
      OnSelectCell = StringGridSelectCell
      ColWidths = (
        110
        138
        177)
    end
  end
  object pmHeader: TPopupMenu
    OnPopup = pmHeaderPopup
    Left = 24
    Top = 16
    object miResetColumns: TMenuItem
      Caption = '#01002^eReset columns'
      OnClick = miResetColumnsClick
    end
  end
end
