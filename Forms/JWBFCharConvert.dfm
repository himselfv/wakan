object FCharConvert: TFCharConvert
  Left = 0
  Top = 0
  Caption = 'FChar convert'
  ClientHeight = 271
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 98
    Width = 527
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 98
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      527
      98)
    object mmRaw: TMemo
      Left = 8
      Top = 8
      Width = 511
      Height = 89
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 102
    Width = 527
    Height = 169
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 184
    ExplicitTop = 144
    ExplicitWidth = 185
    ExplicitHeight = 41
    DesignSize = (
      527
      169)
    object cbCJKOnly: TCheckBox
      Left = 8
      Top = 6
      Width = 349
      Height = 17
      Caption = 'CJK symbols only'
      TabOrder = 0
    end
    object btnToFChar: TButton
      Left = 363
      Top = 2
      Width = 75
      Height = 25
      Caption = 'To FChar'
      TabOrder = 1
      OnClick = btnToFCharClick
    end
    object btnToRaw: TButton
      Left = 444
      Top = 2
      Width = 75
      Height = 25
      Caption = 'To raw'
      TabOrder = 2
      OnClick = btnToRawClick
    end
    object mmFChar: TMemo
      Left = 8
      Top = 33
      Width = 511
      Height = 90
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object btnClose: TButton
      Left = 444
      Top = 129
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      TabOrder = 4
      OnClick = btnCloseClick
    end
  end
end
