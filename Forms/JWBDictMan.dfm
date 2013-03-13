object fDictMan: TfDictMan
  Left = 304
  Top = 258
  BorderStyle = bsDialog
  Caption = '#00095^eDictionary manager'
  ClientHeight = 439
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    649
    439)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 107
    Height = 13
    Caption = '#00096^eDictionaries:'
  end
  object Panel1: TPanel
    Left = 290
    Top = 24
    Width = 352
    Height = 373
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvLowered
    TabOrder = 5
    DesignSize = (
      352
      373)
    object Label2: TLabel
      Left = 8
      Top = 8
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
      Top = 32
      Width = 112
      Height = 13
      Caption = '#00097^eFilename:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 56
      Width = 104
      Height = 13
      Caption = '#00075^eVersion:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Top = 80
      Width = 134
      Height = 13
      Caption = '#00098^eWord entries:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 152
      Width = 119
      Height = 13
      Caption = '#00099^eBuild date:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 8
      Top = 200
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
    object Label9: TLabel
      Left = 8
      Top = 176
      Width = 101
      Height = 13
      Caption = '#00100^ePriority:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 176
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label10: TLabel
      Left = 176
      Top = 32
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label11: TLabel
      Left = 176
      Top = 56
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label12: TLabel
      Left = 176
      Top = 80
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label13: TLabel
      Left = 176
      Top = 152
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label14: TLabel
      Left = 176
      Top = 176
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label15: TLabel
      Left = 8
      Top = 216
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label16: TLabel
      Left = 8
      Top = 238
      Width = 115
      Height = 13
      Caption = '#00985^eCopyright:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label17: TLabel
      Left = 8
      Top = 254
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label18: TLabel
      Left = 8
      Top = 104
      Width = 126
      Height = 13
      Caption = '#00101^eWord index:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label19: TLabel
      Left = 176
      Top = 104
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label20: TLabel
      Left = 8
      Top = 128
      Width = 151
      Height = 13
      Caption = '#00102^eCharacter index:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label21: TLabel
      Left = 176
      Top = 128
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object SpeedButton1: TSpeedButton
      Left = 8
      Top = 316
      Width = 108
      Height = 22
      Hint = '#00103^eSelect the dictionary for use with group 1'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 1
      Caption = '#00104^eGroup 1'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
      ExplicitTop = 308
    end
    object SpeedButton2: TSpeedButton
      Left = 122
      Top = 316
      Width = 108
      Height = 22
      Hint = '#00105^eSelect the dictionary for use with group 2'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 2
      Caption = '#00106^eGroup 2'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
      ExplicitTop = 308
    end
    object SpeedButton3: TSpeedButton
      Left = 236
      Top = 316
      Width = 108
      Height = 22
      Hint = '#00107^eSelect the dictionary for use with group 3'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 3
      Caption = '#00108^eGroup 3'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
      ExplicitTop = 308
    end
    object SpeedButton4: TSpeedButton
      Left = 8
      Top = 344
      Width = 165
      Height = 22
      Hint = '#00109^eUse the dictionary for compounds'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 4
      Caption = '#00110^eUse for compounds'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
      ExplicitTop = 336
    end
    object SpeedButton5: TSpeedButton
      Left = 179
      Top = 344
      Width = 165
      Height = 22
      Hint = '#00111^eUse the dictionary for popup tool & editor'
      AllowAllUp = True
      Anchors = [akLeft, akBottom]
      GroupIndex = 5
      Caption = '#00112^eUse for popup/editor'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
      ExplicitTop = 336
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 293
      Width = 313
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = '#00918^eLoad entire dictionary into memory (much faster)'
      TabOrder = 0
      OnClick = SpeedButton1Click
    end
  end
  object cbDicts: TCheckListBox
    Left = 8
    Top = 24
    Width = 225
    Height = 373
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = cbDictsClick
  end
  object Button1: TButton
    Left = 8
    Top = 403
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '#00113^eRefresh && rescan'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 143
    Top = 403
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '#00114^eImport from EDICT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object btnOk: TBitBtn
    Left = 377
    Top = 403
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00980^eApply'
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
    ModalResult = 1
    NumGlyphs = 2
    ParentFont = False
    TabOrder = 3
    OnClick = btnOkClick
  end
  object btnCancel: TBitBtn
    Left = 512
    Top = 403
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '#00981^eCancel'
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
    ModalResult = 2
    NumGlyphs = 2
    ParentFont = False
    TabOrder = 4
  end
  object btnMoveUp: TBitBtn
    Left = 239
    Top = 27
    Width = 45
    Height = 44
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
      3333333333777F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
      3333333777737777F333333099999990333333373F3333373333333309999903
      333333337F33337F33333333099999033333333373F333733333333330999033
      3333333337F337F3333333333099903333333333373F37333333333333090333
      33333333337F7F33333333333309033333333333337373333333333333303333
      333333333337F333333333333330333333333333333733333333}
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TBitBtn
    Left = 239
    Top = 77
    Width = 45
    Height = 44
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
      333333333337F33333333333333033333333333333373F333333333333090333
      33333333337F7F33333333333309033333333333337373F33333333330999033
      3333333337F337F33333333330999033333333333733373F3333333309999903
      333333337F33337F33333333099999033333333373333373F333333099999990
      33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333300033333333333337773333333}
    NumGlyphs = 2
    TabOrder = 7
    OnClick = btnMoveDownClick
  end
end
