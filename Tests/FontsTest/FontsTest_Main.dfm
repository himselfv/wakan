object FontTestForm: TFontTestForm
  Left = 0
  Top = 0
  Caption = 'FontTestForm'
  ClientHeight = 407
  ClientWidth = 1009
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    1009
    407)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 320
    Top = 8
    Width = 676
    Height = 256
    Anchors = [akLeft, akTop, akRight]
    OnPaint = PaintBoxPaint
    ExplicitWidth = 256
  end
  object lblFont: TLabel
    Left = 8
    Top = 11
    Width = 26
    Height = 13
    Caption = 'Font:'
  end
  object lblText: TLabel
    Left = 8
    Top = 58
    Width = 27
    Height = 13
    Caption = 'Char:'
  end
  object lblSize: TLabel
    Left = 8
    Top = 85
    Width = 23
    Height = 13
    Caption = 'Size:'
  end
  object lblBoxSize: TLabel
    Left = 320
    Top = 273
    Width = 43
    Height = 13
    Caption = 'Box size:'
  end
  object Label1: TLabel
    Left = 320
    Top = 300
    Width = 30
    Height = 13
    Caption = 'Mode:'
  end
  object Label2: TLabel
    Left = 320
    Top = 350
    Width = 45
    Height = 13
    Caption = 'Shift top:'
  end
  object cbFonts: TComboBox
    Left = 72
    Top = 8
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbFontsChange
  end
  object edtText: TEdit
    Left = 72
    Top = 55
    Width = 121
    Height = 21
    TabOrder = 1
    Text = #20001#24248#24037#23560#23574#21451#21452
    OnChange = edtTextChange
  end
  object cbSize: TComboBox
    Left = 72
    Top = 82
    Width = 145
    Height = 21
    ItemIndex = 4
    TabOrder = 2
    Text = '16'
    OnChange = cbSizeChange
    Items.Strings = (
      '8'
      '10'
      '12'
      '14'
      '16'
      '18'
      '20'
      '24'
      '28'
      '32'
      '48'
      '72')
  end
  object mmFontInfo: TMemo
    Left = 8
    Top = 109
    Width = 297
    Height = 289
    ReadOnly = True
    TabOrder = 3
  end
  object cbBoxSize: TComboBox
    Left = 384
    Top = 270
    Width = 145
    Height = 21
    ItemIndex = 6
    TabOrder = 4
    Text = '100'
    OnChange = cbBoxSizeChange
    Items.Strings = (
      '40'
      '50'
      '60'
      '70'
      '80'
      '90'
      '100'
      '120'
      '140')
  end
  object cbLayoutMode: TComboBox
    Left = 384
    Top = 297
    Width = 192
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 5
    Text = 'By line height'
    OnChange = cbBoxSizeChange
    Items.Strings = (
      'By line height'
      'By cell height'
      'By cell height + centered'
      'By cell height + raise by leading'
      'By cell height + raise + autoadjust'
      'By baseline')
  end
  object cbDrawMetrics: TCheckBox
    Left = 320
    Top = 324
    Width = 256
    Height = 17
    Caption = 'Draw font metrics'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = cbDrawMetricsClick
  end
  object cbJISFontsOnly: TCheckBox
    Left = 72
    Top = 32
    Width = 233
    Height = 17
    Caption = 'JIS fonts only'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = cbJISFontsOnlyClick
  end
  object cbShiftTop: TComboBox
    Left = 384
    Top = 347
    Width = 145
    Height = 21
    ItemIndex = 4
    TabOrder = 8
    Text = '0'
    OnChange = cbShiftTopChange
    Items.Strings = (
      '-20'
      '-15'
      '-10'
      '-5'
      '0'
      '5'
      '10'
      '15'
      '20')
  end
end
