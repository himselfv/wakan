object fDictCoding: TfDictCoding
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = '#00063^eEDICT character coding^cKódování znakù EDICT'
  ClientHeight = 273
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 305
    Height = 13
    AutoSize = False
    Caption = 
      '#00064^ePlease select language of the input EDICT file:^cZvolte '+
      'prosím jazyk vsupního EDICT souboru:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Label2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 152
    Width = 305
    Height = 73
    AutoSize = False
    Caption = 
      '#00065^ePlease note:'#13#10'For encodings different than "Unicode 16-b'+
      'it" you need the UNICONV utility in the current directory.'#13#10'This'+
      ' utility can be downloaded from ftp.cc.monash.edu.au/pub/nihongo'+
      '/uniconv.zip.'#13#10'^cUpozornìní:'#13#10'Pro kódování jiná než "Unicode 16-'+
      'bit" potøebujete mít utilitu UNICONV v aktuálním adresáøi.'#13#10'Tato'+
      ' utilita se dá stáhnout z ftp.cc.monash.edu.au/pub/nihongo/unico'+
      'nv.zip.'
    WordWrap = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 56
    Width = 305
    Height = 89
    Caption = '#00066^eLanguage / Character coding^cJazyk / kódování znakù'
    ItemIndex = 1
    Items.Strings = (
      'Unicode 16-bit'
      '#00067^eJapanese (autodetect)^cJaponština (autodetekce)'
      '#00068^eChinese (autodetect)^cÈínština (autodetekce)'
      '#00069^eKorean (autodetect)^cKorejština (autodetekce)')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 48
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = BitBtn1Click
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
  end
  object BitBtn2: TBitBtn
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = '#00007^eCancel^cZrušit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = BitBtn2Click
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
  end
end
