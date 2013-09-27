object fPortableMode: TfPortableMode
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '#01009^eSelect mode'
  ClientHeight = 349
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pcPages: TPageControl
    Left = 0
    Top = 0
    Width = 423
    Height = 349
    ActivePage = tsMoveFiles
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    object tsSelectMode: TTabSheet
      Caption = 'tsSelectMode'
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        415
        339)
      object lblQuestion: TLabel
        Left = 8
        Top = 8
        Width = 391
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '#01010^eHow do you want Wakan to run?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblPortableDescription: TLabel
        Left = 8
        Top = 287
        Width = 391
        Height = 34
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '#01016^eStore all settings and user data in Wakan folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblStandaloneDescription: TLabel
        Left = 8
        Top = 95
        Width = 391
        Height = 34
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          '#01012^eStore settings in registry and user data in roaming AppD' +
          'ata folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblCompatibleDescription: TLabel
        Left = 8
        Top = 191
        Width = 391
        Height = 34
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          '#01014^eStore settings in registry and all data in Wakan folder,' +
          ' like older versions did'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object btnPortable: TButton
        Left = 8
        Top = 248
        Width = 193
        Height = 33
        Caption = '#01015^ePortable'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = btnPortableClick
      end
      object btnStandalone: TButton
        Left = 8
        Top = 56
        Width = 193
        Height = 33
        Caption = '#01011^eStandalone'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = btnStandaloneClick
      end
      object btnCompatible: TButton
        Left = 8
        Top = 152
        Width = 193
        Height = 33
        Caption = '#01013^eCompatible'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = btnCompatibleClick
      end
    end
    object tsMoveFiles: TTabSheet
      Caption = 'tsMoveFiles'
      ImageIndex = 1
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        415
        339)
      object lblMoveQuestion: TLabel
        Left = 8
        Top = 8
        Width = 391
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '#01017^eMove files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label1: TLabel
        Left = 8
        Top = 39
        Width = 391
        Height = 18
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '#01018^eYour Wakan folder already contains some user data.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label2: TLabel
        Left = 8
        Top = 247
        Width = 391
        Height = 18
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          '#01019^eDo you want to move these files to the AppData directory' +
          '?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lbFiles: TListBox
        Left = 8
        Top = 63
        Width = 353
        Height = 178
        ItemHeight = 13
        TabOrder = 0
      end
      object btnMoveFiles: TButton
        Left = 64
        Top = 288
        Width = 129
        Height = 33
        Caption = '#01020^eMove'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ModalResult = 1001
        ParentFont = False
        TabOrder = 1
        OnClick = btnMoveFilesClick
      end
      object btnIgnoreFiles: TButton
        Left = 208
        Top = 288
        Width = 129
        Height = 33
        Caption = '#01021^eIgnore'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ModalResult = 1001
        ParentFont = False
        TabOrder = 2
        OnClick = btnIgnoreFilesClick
      end
    end
  end
end
