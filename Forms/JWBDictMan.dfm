object fDictMan: TfDictMan
  Left = 304
  Top = 258
  BorderStyle = bsDialog
  Caption = '#00095^eDictionary manager'
  ClientHeight = 435
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 240
    Top = 24
    Width = 329
    Height = 369
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 107
    Height = 13
    Caption = '#00096^eDictionaries:'
  end
  object Label2: TLabel
    Left = 248
    Top = 32
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
    Left = 248
    Top = 56
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
    Left = 248
    Top = 80
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
    Left = 248
    Top = 104
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
    Left = 248
    Top = 176
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
    Left = 248
    Top = 224
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
    Left = 248
    Top = 200
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
    Left = 416
    Top = 32
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label10: TLabel
    Left = 416
    Top = 56
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label11: TLabel
    Left = 416
    Top = 80
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label12: TLabel
    Left = 416
    Top = 104
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label13: TLabel
    Left = 416
    Top = 176
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label14: TLabel
    Left = 416
    Top = 200
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label15: TLabel
    Left = 248
    Top = 240
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label16: TLabel
    Left = 248
    Top = 256
    Width = 58
    Height = 13
    Caption = 'Copyright:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label17: TLabel
    Left = 248
    Top = 272
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label18: TLabel
    Left = 248
    Top = 128
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
    Left = 416
    Top = 128
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label20: TLabel
    Left = 248
    Top = 152
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
    Left = 416
    Top = 152
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object SpeedButton1: TSpeedButton
    Left = 248
    Top = 331
    Width = 105
    Height = 22
    Hint = '#00103^eSelect the dictionary for use with group 1'
    AllowAllUp = True
    GroupIndex = 1
    Caption = '#00104^eGroup 1'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 360
    Top = 331
    Width = 97
    Height = 22
    Hint = '#00105^eSelect the dictionary for use with group 2'
    AllowAllUp = True
    GroupIndex = 2
    Caption = '#00106^eGroup 2'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object SpeedButton3: TSpeedButton
    Left = 464
    Top = 331
    Width = 97
    Height = 22
    Hint = '#00107^eSelect the dictionary for use with group 3'
    AllowAllUp = True
    GroupIndex = 3
    Caption = '#00108^eGroup 3'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object SpeedButton4: TSpeedButton
    Left = 248
    Top = 360
    Width = 153
    Height = 22
    Hint = '#00109^eUse the dictionary for compounds'
    AllowAllUp = True
    GroupIndex = 4
    Caption = '#00110^eUse for compounds'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object SpeedButton5: TSpeedButton
    Left = 408
    Top = 360
    Width = 153
    Height = 22
    Hint = '#00111^eUse the dictionary for popup tool & editor'
    AllowAllUp = True
    GroupIndex = 5
    Caption = '#00112^eUse for popup/editor'
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object CheckListBox1: TCheckListBox
    Left = 8
    Top = 24
    Width = 225
    Height = 369
    ItemHeight = 13
    TabOrder = 0
    OnClick = CheckListBox1Click
  end
  object Button1: TButton
    Left = 8
    Top = 400
    Width = 145
    Height = 25
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
    Left = 168
    Top = 400
    Width = 145
    Height = 25
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
  object BitBtn1: TBitBtn
    Left = 432
    Top = 400
    Width = 129
    Height = 25
    Caption = '^eApply'
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Kind = bkOK
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object CheckBox1: TCheckBox
    Left = 248
    Top = 304
    Width = 313
    Height = 17
    Caption = '#00918^eLoad entire dictionary into memory (much faster)'
    TabOrder = 4
    OnClick = SpeedButton1Click
  end
end
