object fCharDataImport: TfCharDataImport
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Import character data'
  ClientHeight = 167
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    386
    167)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 12
    Width = 158
    Height = 13
    Caption = 'Specify the KANJIDIC file to use:'
  end
  object Label2: TLabel
    Left = 14
    Top = 72
    Width = 364
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'When you press UPDATE, your current kanji data will be replaced ' +
      'by the one read from the file.'
    WordWrap = True
  end
  object edtKanjidicFile: TEdit
    Left = 14
    Top = 31
    Width = 275
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'KANJIDIC'
  end
  object btnKanjidicBrowse: TButton
    Left = 295
    Top = 29
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
    OnClick = btnKanjidicBrowseClick
  end
  object btnUpdate: TButton
    Left = 143
    Top = 125
    Width = 98
    Height = 25
    Caption = 'Update'
    TabOrder = 2
    OnClick = btnUpdateClick
  end
  object OpenKanjidicDialog: TOpenDialog
    FileName = 'KANJIDIC'
    Left = 344
    Top = 8
  end
end
