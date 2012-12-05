object fExamples: TfExamples
  Left = 342
  Top = 413
  BorderStyle = bsNone
  Caption = '#00306^eExamples'
  ClientHeight = 49
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 469
    Height = 49
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      469
      49)
    object Shape9: TShape
      Left = 1
      Top = 1
      Width = 313
      Height = 45
      Anchors = [akLeft, akTop, akRight]
      Brush.Color = clWindow
    end
    object Bevel2: TBevel
      Left = 426
      Top = 1
      Width = 9
      Height = 44
      Anchors = [akTop, akRight]
      Shape = bsRightLine
    end
    object Bevel3: TBevel
      Left = 385
      Top = 2
      Width = 9
      Height = 44
      Anchors = [akTop, akRight]
      Shape = bsRightLine
    end
    object Label2: TLabel
      Left = 354
      Top = 15
      Width = 32
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 346
      Top = 2
      Width = 79
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '#00726^eClip'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 317
      Top = 15
      Width = 33
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '^ePos:'
    end
    object Label5: TLabel
      Left = 317
      Top = 1
      Width = 31
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '^eEx#'
    end
    object Label6: TLabel
      Left = 344
      Top = 1
      Width = 32
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Label6'
    end
    object PaintBox3: TPaintBox
      Left = 2
      Top = 2
      Width = 311
      Height = 43
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ParentColor = False
      OnMouseDown = PaintBox3MouseDown
      OnMouseMove = PaintBox3MouseMove
      OnMouseUp = PaintBox3MouseUp
      OnPaint = PaintBox3Paint
    end
    object SpeedButton1: TSpeedButton
      Left = 372
      Top = 0
      Width = 18
      Height = 17
      Hint = '#00725^eNext example'
      Anchors = [akTop, akRight]
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333FFF333333333333000333333333
        3333777FFF3FFFFF33330B000300000333337F777F777773F333000E00BFBFB0
        3333777F773333F7F333000E0BFBF0003333777F7F3337773F33000E0FBFBFBF
        0333777F7F3333FF7FFF000E0BFBF0000003777F7F3337777773000E0FBFBFBF
        BFB0777F7F33FFFFFFF7000E0BF000000003777F7FF777777773000000BFB033
        33337777773FF733333333333300033333333333337773333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton10: TSpeedButton
      Left = 437
      Top = 32
      Width = 27
      Height = 16
      Hint = '#00729^eCopy all examples to clipboard'
      Anchors = [akTop, akRight]
      Caption = '#00670^eAll'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton10Click
    end
    object SpeedButton11: TSpeedButton
      Left = 316
      Top = 29
      Width = 37
      Height = 16
      Hint = '#00730^eRandom order of examples'
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 4
      Caption = '^eRND'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton11Click
    end
    object SpeedButton4: TSpeedButton
      Left = 396
      Top = 1
      Width = 35
      Height = 15
      Hint = '#00718^eDisplay translation'
      Anchors = [akTop, akRight]
      GroupIndex = 2
      Down = True
      Caption = '#00719^eTrans'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton4Click
    end
    object SpeedButton5: TSpeedButton
      Left = 396
      Top = 16
      Width = 35
      Height = 16
      Hint = '#00720^eDisplay example in small font'
      Anchors = [akTop, akRight]
      GroupIndex = 2
      Caption = '#00721^eSmall'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton4Click
    end
    object SpeedButton6: TSpeedButton
      Left = 396
      Top = 32
      Width = 35
      Height = 16
      Hint = '#00722^eDisplay example in big font'
      Anchors = [akTop, akRight]
      GroupIndex = 2
      Caption = '#00723^eBig'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton4Click
    end
    object SpeedButton7: TSpeedButton
      Left = 354
      Top = 29
      Width = 18
      Height = 18
      Hint = '#00724^ePrevious example'
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
      OnClick = SpeedButton7Click
    end
    object SpeedButton8: TSpeedButton
      Left = 373
      Top = 30
      Width = 18
      Height = 18
      Hint = '#00725^eNext example'
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
      OnClick = SpeedButton8Click
    end
    object SpeedButton9: TSpeedButton
      Left = 437
      Top = 16
      Width = 26
      Height = 16
      Hint = '#00727^eCopy example to clipboard'
      Anchors = [akTop, akRight]
      Caption = '#00728^eThis'
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton9Click
    end
  end
end
