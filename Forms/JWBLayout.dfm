object fLayout: TfLayout
  Left = 318
  Top = 357
  Caption = '#00200^eLayout manager'
  ClientHeight = 300
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 251
    Height = 13
    Caption = '#00201^eStandard layouts:'
  end
  object Label2: TLabel
    Left = 8
    Top = 160
    Width = 377
    Height = 13
    Caption = 
      '#00202^ePercentage screen occupation:'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 24
    Width = 385
    Height = 129
    ItemHeight = 13
    Items.Strings = (
      '#00203^eWindowed'
      '#00204^eComplete'
      '#00205^eKanji search'
      '#00206^eLearning'
      '#00207^eTranslation'
      'Editor')
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 208
    Top = 160
    Width = 185
    Height = 33
    Max = 100
    PageSize = 20
    Frequency = 10
    Position = 100
    TabOrder = 1
  end
  object Button1: TButton
    Left = 16
    Top = 200
    Width = 369
    Height = 25
    Caption = '#00208^eSet standard layout'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 232
    Width = 369
    Height = 25
    Caption = '#00209^eSave layout'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 264
    Width = 369
    Height = 25
    Caption = '#00210^eLoad layout'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WaKan Layout Files (*.lay)|*.lay'
    Left = 200
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.lay'
    Filter = 'WaKan Layout Files (*.lay)|*.lay'
    Left = 240
    Top = 48
  end
end
