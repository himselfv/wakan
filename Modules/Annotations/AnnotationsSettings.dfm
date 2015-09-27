object AnnotationsSettingsPage: TAnnotationsSettingsPage
  Left = 0
  Top = 0
  Caption = '#00967^eAnnotations'
  ClientHeight = 414
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    449
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = -8
    Top = 27
    Width = 457
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object cbEnableAnnotations: TCheckBox
    Left = 3
    Top = 3
    Width = 446
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '^eEnable annotations (ANNOTATE.PKG)'
    TabOrder = 0
  end
  object cbRebuildAnnotations: TCheckBox
    Left = 3
    Top = 41
    Width = 446
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      '^eRebuild ANNOTATE.PKG from .ANO files if needed whenever WaKan ' +
      'starts'
    TabOrder = 1
  end
  object cbAnnotateWithPictures: TCheckBox
    Left = 3
    Top = 65
    Width = 446
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '^eAllow annotations to display pictures'
    TabOrder = 2
  end
  object cbAnnotateWithWebPages: TCheckBox
    Left = 3
    Top = 89
    Width = 446
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '^eAllow annotations to display web pages'
    TabOrder = 3
  end
  object cbAnnotateWithColors: TCheckBox
    Left = 3
    Top = 113
    Width = 446
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '^eAllow annotations to change foreground color'
    TabOrder = 4
  end
  object cbAnnotateWithSound: TCheckBox
    Left = 3
    Top = 137
    Width = 446
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '^eAllow annotations to play sound files'
    TabOrder = 5
    Visible = False
  end
  object btnShowHelp: TButton
    Left = 64
    Top = 339
    Width = 337
    Height = 25
    Caption = '^eHelp for the annotation feature'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnShowHelpClick
  end
end
