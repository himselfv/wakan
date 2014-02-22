object fDownloader: TfDownloader
  Left = 0
  Top = 0
  Caption = 'File Downloader'
  ClientHeight = 430
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    609
    430)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPageTitle: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 25
    Caption = 'Page title'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPageDescription: TLabel
    Left = 8
    Top = 39
    Width = 255
    Height = 13
    Caption = 'Page description which is taken from active page Hint'
  end
  object btnCancel: TBitBtn
    Left = 506
    Top = 393
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
    ExplicitLeft = 424
    ExplicitTop = 334
  end
  object btnNext: TBitBtn
    Left = 405
    Top = 393
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    Default = True
    TabOrder = 1
    ExplicitLeft = 323
    ExplicitTop = 334
  end
  object btnPrev: TBitBtn
    Left = 304
    Top = 393
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Back'
    TabOrder = 2
    ExplicitLeft = 222
    ExplicitTop = 334
  end
  object PageControl: TPageControl
    Left = 8
    Top = 58
    Width = 593
    Height = 329
    ActivePage = tsSelectFiles
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsButtons
    TabOrder = 3
    OnChange = PageControlChange
    ExplicitWidth = 511
    ExplicitHeight = 270
    object tsSelectFiles: TTabSheet
      Hint = 'Please select which files you want to download:'
      Caption = 'Select Files'
      TabVisible = False
      ExplicitWidth = 503
      ExplicitHeight = 260
      object vtKnownFiles: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 585
        Height = 230
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        TabOrder = 0
        TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toFullRowDrag, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnFocusChanged = vtKnownFilesFocusChanged
        OnFreeNode = vtKnownFilesFreeNode
        OnGetText = vtKnownFilesGetText
        OnPaintText = vtKnownFilesPaintText
        OnGetNodeDataSize = vtKnownFilesGetNodeDataSize
        OnInitNode = vtKnownFilesInitNode
        ExplicitWidth = 503
        ExplicitHeight = 171
        Columns = <
          item
            Position = 0
            Width = 350
            WideText = 'Name'
            WideHint = 'Name'
          end
          item
            Position = 1
            Width = 100
            WideText = 'Language'
          end>
      end
      object mmFileDetails: TMemo
        Left = 0
        Top = 230
        Width = 585
        Height = 89
        Align = alBottom
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
        ExplicitTop = 171
        ExplicitWidth = 503
      end
    end
  end
end
