object fCharItem: TfCharItem
  Left = 292
  Top = 274
  BorderStyle = bsDialog
  Caption = '#00033^eCharacter details item^cPoložka detailù znaku'
  ClientHeight = 300
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 125
    Height = 13
    Caption = '#00034^eItem:^cPoložka:'
  end
  object Label2: TLabel
    Left = 104
    Top = 32
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 8
    Top = 32
    Width = 146
    Height = 13
    Caption = '#00035^eDescription:^cPopis:'
  end
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 125
    Height = 13
    Caption = '#00036^eSource:^cZdroj:'
  end
  object Label5: TLabel
    Left = 104
    Top = 48
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object ComboBox1: TComboBox
    Left = 104
    Top = 8
    Width = 473
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 72
    Width = 297
    Height = 73
    Caption = '#00037^eDisplay type^cTyp zobrazení'
    ItemIndex = 0
    Items.Strings = (
      '#00038^eWhole line^cCelá øádka'
      '#00039^eOne column^cJeden sloupec'
      '#00040^eMore lines (word wrap)^cVíce øádek (lámat slova)')
    TabOrder = 1
  end
  object RadioGroup2: TRadioGroup
    Left = 312
    Top = 72
    Width = 265
    Height = 73
    Caption = '#00041^eDisplay label^cZobrazování titulku'
    ItemIndex = 0
    Items.Strings = (
      '#00042^eDisplay label^cZobrazit titulek'
      
        '#00043^eDisplay label on whole line^cZobrazit titulek na celé øá'+
        'dce'
      '#00044^eDo not display label^cNezobrazovat titulek')
    TabOrder = 2
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 152
    Width = 297
    Height = 73
    Caption = '#00045^eMode display^cZobrazit režim'
    ItemIndex = 0
    Items.Strings = (
      '#00046^eDisplay in both modes^cZobrazit v obou režimech'
      
        '#00047^eDisplay in japanese mode only^cZobrazit jen v režimu jap'+
        'onštiny'
      
        '#00048^eDisplay in chinese mode only^cZobrazit jen v režimu èínš'+
        'tiny')
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 312
    Top = 152
    Width = 265
    Height = 17
    Caption = 
      '#00049^eDisplay even if information is missing^cZobrazit i když '+
      'chybí informace'
    TabOrder = 4
  end
  object Edit1: TEdit
    Left = 176
    Top = 232
    Width = 401
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
  end
  object BitBtn1: TBitBtn
    Left = 64
    Top = 264
    Width = 113
    Height = 25
    TabOrder = 6
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 424
    Top = 264
    Width = 107
    Height = 25
    Caption = '#00050^eCancel^cStorno'
    TabOrder = 7
    Kind = bkCancel
  end
  object RadioGroup4: TRadioGroup
    Left = 312
    Top = 176
    Width = 265
    Height = 49
    Caption = '#00051^eFont size^cVelikost písma'
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      '#00052^eSmall^cMalá'
      '#00053^eMedium^cStøední'
      '#00054^eBig^cVelká')
    TabOrder = 8
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 232
    Width = 161
    Height = 17
    Caption = '#00055^eCustom title:^cVlastní titulek:'
    TabOrder = 9
    OnClick = CheckBox2Click
  end
end
