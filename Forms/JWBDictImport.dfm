object fDictImport: TfDictImport
  Left = 184
  Top = 231
  BorderStyle = bsDialog
  Caption = '#00071^eDictionary import'
  ClientHeight = 497
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    535
    497)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 11
    Width = 153
    Height = 13
    Caption = '#00073^eDictionary name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 192
    Width = 218
    Height = 13
    Caption = '#00074^eIncluded EDICT format files:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 35
    Width = 126
    Height = 13
    Caption = '#00035^eDescription:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edtDictName: TEdit
    Left = 128
    Top = 8
    Width = 289
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object lbFiles: TListBox
    Left = 8
    Top = 208
    Width = 409
    Height = 206
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object btnBuild: TBitBtn
    Left = 88
    Top = 461
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '#00077^eBuild'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
    ParentFont = False
    TabOrder = 7
    OnClick = btnBuildClick
    ExplicitTop = 512
  end
  object btnCancel: TBitBtn
    Left = 344
    Top = 461
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = '#00007^eCancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
    ParentFont = False
    TabOrder = 8
    OnClick = btnCancelClick
    ExplicitTop = 512
  end
  object btnAddFile: TButton
    Left = 424
    Top = 208
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '#00078^eAdd'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = btnAddFileClick
  end
  object btnRemoveFile: TButton
    Left = 424
    Top = 239
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '#00079^eRemove'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = btnRemoveFileClick
  end
  object rgLanguage: TRadioGroup
    Left = 8
    Top = 128
    Width = 409
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = '#00080^eLanguage'
    Columns = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemIndex = 0
    Items.Strings = (
      '#00081^eJapanese'
      '#00082^eMandarin chinese')
    ParentFont = False
    TabOrder = 2
  end
  object cbAddFrequencyInfo: TCheckBox
    Left = 8
    Top = 420
    Width = 513
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '#00914^eBuild with frequency information (requires WORDFREQ_CK)'
    Checked = True
    State = cbChecked
    TabOrder = 6
    ExplicitTop = 423
  end
  object mmDescription: TMemo
    Left = 8
    Top = 54
    Width = 409
    Height = 60
    TabOrder = 1
  end
  object AddFileDialog: TOpenDialog
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 456
    Top = 144
  end
end
