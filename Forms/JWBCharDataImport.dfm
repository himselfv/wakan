object fCharDataImport: TfCharDataImport
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '#01079^eImport character data'
  ClientHeight = 252
  ClientWidth = 386
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
    386
    252)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 12
    Width = 210
    Height = 13
    Caption = '#01080^eSpecify the KANJIDIC file to use:'
  end
  object Label2: TLabel
    Left = 14
    Top = 72
    Width = 364
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      '#01082^eWhen you press UPDATE, your current kanji data will be r' +
      'eplaced by the one read from the file.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 14
    Top = 135
    Width = 364
    Height = 31
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      '#01085^eA backup of your current WAKAN.CHR will also be made and' +
      ' placed in the following folder:'
    WordWrap = True
  end
  object lblBackupPath: TUrlLabel
    Left = 14
    Top = 172
    Width = 27
    Height = 13
    Cursor = crHandPoint
    Caption = '[path]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 14
    Top = 103
    Width = 364
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      '#01084^eYou can download original WAKAN.CHR from Wakan distribut' +
      'ion to restore kanji data at any time.'
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
    Caption = '#01081^eBrowse'
    TabOrder = 1
    OnClick = btnKanjidicBrowseClick
  end
  object btnUpdate: TButton
    Left = 87
    Top = 210
    Width = 98
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '#01083^eUpdate'
    TabOrder = 2
    OnClick = btnUpdateClick
  end
  object btnCancel: TButton
    Left = 207
    Top = 210
    Width = 98
    Height = 25
    Cancel = True
    Caption = '#01086^eCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object OpenKanjidicDialog: TOpenDialog
    FileName = 'KANJIDIC'
    Left = 344
    Top = 8
  end
end
