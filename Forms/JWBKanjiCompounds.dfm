inherited fKanjiCompounds: TfKanjiCompounds
  Left = 227
  Top = 187
  Caption = '#00125^eCompounds'
  ClientWidth = 556
  Constraints.MinHeight = 100
  Constraints.MinWidth = 120
  Font.Name = 'MS Sans Serif'
  Scaled = False
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  ExplicitWidth = 556
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel: TPanel
    Width = 556
    inherited btnGoToVocab: TSpeedButton
      Left = 277
    end
    inherited btnAddToVocab: TSpeedButton
      Left = 373
    end
    inherited btnCopyToClipboard: TSpeedButton
      Left = 469
    end
    object sbShowDict: TSpeedButton [3]
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
      OnClick = OptionChanged
    end
    object sbShowVocab: TSpeedButton [4]
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
      OnClick = OptionChanged
    end
    inherited BlankPanel: TBlankPanel
      Width = 539
    end
    inherited StringGrid: TWakanWordGrid
      Width = 537
      ColWidths = (
        110
        138
        265)
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
      TabOrder = 2
      OnClick = OptionChanged
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
      TabOrder = 3
      OnClick = OptionChanged
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
      TabOrder = 4
      OnClick = OptionChanged
    end
  end
end
