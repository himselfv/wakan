object fUserFilters: TfUserFilters
  Left = 562
  Top = 172
  BorderStyle = bsNone
  Caption = '#00704^eVocabulary settings^cNastavení slovíèek'
  ClientHeight = 490
  ClientWidth = 192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 192
    Height = 490
    Align = alClient
    Shape = bsFrame
  end
  object SpeedButton1: TSpeedButton
    Left = 115
    Top = 463
    Width = 23
    Height = 22
    Hint = 
      '#00957^eCheck / uncheck all categories^cOznaè / odznaè všechny k' +
      'ategorie'
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555000000
      000055555F77777777775555000FFFFFFFF0555F777F5FFFF55755000F0F0000
      FFF05F777F7F77775557000F0F0FFFFFFFF0777F7F7F5FFFFFF70F0F0F0F0000
      00F07F7F7F7F777777570F0F0F0FFFFFFFF07F7F7F7F5FFFFFF70F0F0F0F0000
      00F07F7F7F7F777777570F0F0F0FFFFFFFF07F7F7F7F5FFF55570F0F0F0F000F
      FFF07F7F7F7F77755FF70F0F0F0FFFFF00007F7F7F7F5FF577770F0F0F0F00FF
      0F057F7F7F7F77557F750F0F0F0FFFFF00557F7F7F7FFFFF77550F0F0F000000
      05557F7F7F77777775550F0F0000000555557F7F7777777555550F0000000555
      55557F7777777555555500000005555555557777777555555555}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 138
    Top = 463
    Width = 23
    Height = 22
    Hint = '#00030^eEdit category^cUpravit kategorii'
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
      000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
      00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
      F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
      0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
      FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
      FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
      0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
      00333377737FFFFF773333303300000003333337337777777333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton2Click
  end
  object SpeedButton3: TSpeedButton
    Left = 161
    Top = 463
    Width = 23
    Height = 22
    Hint = '#00031^eDelete category^cSmazat kategorii'
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
      33333337777FF377FF3333993370739993333377FF373F377FF3399993000339
      993337777F777F3377F3393999707333993337F77737333337FF993399933333
      399377F3777FF333377F993339903333399377F33737FF33377F993333707333
      399377F333377FF3377F993333101933399377F333777FFF377F993333000993
      399377FF3377737FF7733993330009993933373FF3777377F7F3399933000399
      99333773FF777F777733339993707339933333773FF7FFF77333333999999999
      3333333777333777333333333999993333333333377777333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton3Click
  end
  object Label1: TLabel
    Left = 8
    Top = 214
    Width = 162
    Height = 13
    Caption = '#00705^eCategories:^cKategorie:'
  end
  object RadioGroup2: TRadioGroup
    Left = 7
    Top = 104
    Width = 177
    Height = 105
    Caption = '#00197^eSort by^cTøídit podle'
    ItemIndex = 0
    Items.Strings = (
      '#00706^eCategory order^cPoøadí v kategorii'
      '#00707^ePhonetic^cÈtení'
      '#00708^eWritten^cZápisu'
      '#00709^eMeaning^cVýznamu'
      '#00710^eAdded date^cDatumu pøidání'
      '#00711^eStatus^cStavu')
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 8
    Width = 177
    Height = 89
    Caption = '#00712^eFilter^cFiltr'
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Hint = '#00713^eDisplay unlearned words^cZobrazit nenauèená slovíèka'
      Caption = '#00639^eUnlearned^cNenauèené'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox8: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Hint = '#00714^eDisplay learned words^cZobrazit nauèená slovíèka'
      Caption = '#00640^eLearned^cNauèené'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox9: TCheckBox
      Left = 8
      Top = 48
      Width = 145
      Height = 17
      Hint = '#00715^eDisplay mastered words^cZobrazit dobøe nauèená slovíèka'
      Caption = '#00641^eMastered^cDobøe nauèené'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox11: TCheckBox
      Left = 8
      Top = 64
      Width = 113
      Height = 17
      Hint = 
        '#00716^eDisplay problematic words^cZobrazit problematická slovíè' +
        'ka'
      Caption = '#00638^eProblematic^cProblematické'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object TabSet1: TTabSet
    Left = 7
    Top = 463
    Width = 108
    Height = 21
    Hint = 
      '#00032^eSelect list of categories (lessons / groups / temporary ' +
      '/ word lists)^cVýbìr seznamu kategorií (lekce / skupiny / pøecho' +
      'dné / seznamy slov)'
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Tabs.Strings = (
      'L'
      'G'
      'T'
      'W')
    TabIndex = 1
    OnChange = TabSet1Change
  end
  object ListBox1: TCheckListBox
    Left = 7
    Top = 231
    Width = 176
    Height = 232
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
end
