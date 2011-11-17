object fExamplesPanel: TfExamplesPanel
  Left = 410
  Top = 129
  BorderStyle = bsNone
  Caption = '#00306^eExamples'
  ClientHeight = 54
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 120
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 592
    Height = 54
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Notebook1: TNotebook
      Left = 2
      Top = 2
      Width = 588
      Height = 50
      Align = alClient
      PageIndex = 1
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Add'
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Examples'
        DesignSize = (
          588
          50)
        object Bevel3: TBevel
          Left = 484
          Top = 2
          Width = 5
          Height = 44
          Anchors = [akTop, akRight]
          Shape = bsRightLine
        end
        object Shape9: TShape
          Left = 1
          Top = 1
          Width = 400
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          Brush.Color = clWindow
        end
        object PaintBox3: TPaintBox
          Left = 2
          Top = 2
          Width = 399
          Height = 43
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ParentColor = False
          OnMouseDown = PaintBox3MouseDown
          OnMouseMove = PaintBox3MouseMove
          OnMouseUp = PaintBox3MouseUp
          OnPaint = PaintBox3Paint
        end
        object Label2: TLabel
          Left = 445
          Top = 15
          Width = 32
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Label2'
        end
        object btnExamplesShowTranslation: TSpeedButton
          Left = 495
          Top = 1
          Width = 42
          Height = 15
          Hint = '#00718^eDisplay translation^cZobrazit p'#248'eklad'
          Anchors = [akTop, akRight]
          GroupIndex = 2
          Down = True
          Caption = '#00719^eTrans^cP'#248'eklad'
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesShowTranslationClick
        end
        object btnExamplesFontSmall: TSpeedButton
          Left = 495
          Top = 16
          Width = 42
          Height = 16
          Hint = 
            '#00720^eDisplay example in small font^cZobrazit p'#248#237'klad v mal'#233'm ' +
            'fontu'
          Anchors = [akTop, akRight]
          GroupIndex = 2
          Caption = '#00721^eSmall^cMal'#233
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesShowTranslationClick
        end
        object btnExamplesFontBig: TSpeedButton
          Left = 495
          Top = 32
          Width = 42
          Height = 16
          Hint = 
            '#00722^eDisplay example in big font^cZobrazit p'#248#237'klad ve velk'#233'm ' +
            'fontu'
          Anchors = [akTop, akRight]
          GroupIndex = 2
          Caption = '#00723^eBig^cVelk'#233
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesShowTranslationClick
        end
        object btnExamplesPrevious: TSpeedButton
          Left = 445
          Top = 29
          Width = 18
          Height = 18
          Hint = '#00724^ePrevious example^cP'#248'edchoz'#237' p'#248#237'klad'
          Anchors = [akTop, akRight]
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333FF3333333333333003333333333333F77F33333333333009033
            333333333F7737F333333333009990333333333F773337FFFFFF330099999000
            00003F773333377777770099999999999990773FF33333FFFFF7330099999000
            000033773FF33777777733330099903333333333773FF7F33333333333009033
            33333333337737F3333333333333003333333333333377333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesPreviousClick
        end
        object btnExamplesNext: TSpeedButton
          Left = 464
          Top = 30
          Width = 18
          Height = 18
          Hint = '#00725^eNext example^cDal'#353#237' p'#248#237'klad'
          Anchors = [akTop, akRight]
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333FF3333333333333003333
            3333333333773FF3333333333309003333333333337F773FF333333333099900
            33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
            99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
            33333333337F3F77333333333309003333333333337F77333333333333003333
            3333333333773333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesNextClick
        end
        object Bevel2: TBevel
          Left = 541
          Top = 1
          Width = 4
          Height = 44
          Anchors = [akTop, akRight]
          Shape = bsRightLine
        end
        object Label3: TLabel
          Left = 551
          Top = 2
          Width = 123
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '#00726^eClip^cSchr.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object btnExampleCopyToClipboard: TSpeedButton
          Left = 552
          Top = 32
          Width = 33
          Height = 16
          Hint = 
            '#00727^eCopy example to clipboard^cZkop'#237'rovat p'#248#237'klad do schr'#225'nk' +
            'y'
          Anchors = [akTop, akRight]
          Caption = '#00728^eThis^cToto'
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExampleCopyToClipboardClick
        end
        object btnExamplesCopyAllToClipboard: TSpeedButton
          Left = 552
          Top = 16
          Width = 27
          Height = 16
          Hint = 
            '#00729^eCopy all examples to clipboard^cZkop'#237'rovat v'#353'echny p'#248#237'kl' +
            'ady do schr'#225'nky'
          Anchors = [akTop, akRight]
          Caption = '#00670^eAll^cV'#353'e'
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesCopyAllToClipboardClick
        end
        object btnExamplesRandomOrder: TSpeedButton
          Left = 407
          Top = 29
          Width = 42
          Height = 16
          Hint = '#00730^eRandom order of examples^cN'#225'hodn'#233' po'#248'ad'#237' p'#248#237'klad'#249
          AllowAllUp = True
          Anchors = [akTop, akRight]
          GroupIndex = 4
          Caption = '^eRND^cRND'
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -10
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesRandomOrderClick
        end
        object Label4: TLabel
          Left = 408
          Top = 15
          Width = 66
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '^ePos:^cPos:'
        end
        object Label5: TLabel
          Left = 408
          Top = 1
          Width = 62
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '^eEx#^cEx#'
        end
        object Label6: TLabel
          Left = 443
          Top = 1
          Width = 32
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Label6'
        end
        object btnExamplesNum: TSpeedButton
          Left = 463
          Top = 0
          Width = 18
          Height = 17
          Hint = '#00968^eSelect example by number'
          Anchors = [akTop, akRight]
          Caption = '#'
          Flat = True
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = btnExamplesNumClick
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Filters'
        object Label1: TLabel
          Left = 11
          Top = 14
          Width = 932
          Height = 18
          Caption = 
            '#00731^eUnder construction. Will be implemented in next WaKan ve' +
            'rsion.^cVe v'#253'stavb'#236'. Bude hotovo v p'#248#237#353't'#237' verzi WaKanu.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsItalic]
          ParentFont = False
        end
      end
    end
  end
end
