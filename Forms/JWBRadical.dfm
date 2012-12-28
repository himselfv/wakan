object fRadical: TfRadical
  Left = 454
  Top = 263
  BorderStyle = bsDialog
  Caption = '#00387^eRadical list'
  ClientHeight = 536
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    770
    536)
  PixelsPerInch = 96
  TextHeight = 13
  object RxLabel17: TRxLabel
    Left = 24
    Top = 24
    Width = 186
    Height = 18
    Caption = '#00387^eRadical list'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Shape6: TShape
    Left = 7
    Top = 68
    Width = 755
    Height = 419
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Label1: TLabel
    Left = 360
    Top = 48
    Width = 412
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 
      '#00388^eMulti-radical filter method: Select any suspected radica' +
      'l by clicking the mouse'#13#10
  end
  object Label2: TLabel
    Left = 360
    Top = 48
    Width = 383
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 
      '#00389^eParts of characters filter method: Select all parts that' +
      ' occur in character'#13#10
  end
  object DrawGrid1: TDrawGrid
    Left = 8
    Top = 69
    Width = 753
    Height = 417
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    ColCount = 21
    DefaultColWidth = 36
    DefaultRowHeight = 50
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 11
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goThumbTracking]
    TabOrder = 0
    OnDblClick = DrawGrid1DblClick
    OnDrawCell = DrawGrid1DrawCell
    OnKeyPress = DrawGrid1KeyPress
    OnMouseMove = DrawGrid1MouseMove
    OnMouseUp = DrawGrid1MouseUp
    OnSelectCell = DrawGrid1SelectCell
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 502
    Width = 225
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '#00390^eSet filter'
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
    Left = 600
    Top = 502
    Width = 161
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '#00170^eClose'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = BitBtn2Click
    Kind = bkCancel
  end
  object CheckBox1: TCheckBox
    Left = 256
    Top = 494
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '#00391^eDisplay variants'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object CheckBox2: TCheckBox
    Left = 400
    Top = 494
    Width = 193
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '#00392^eDisplay learned radicals in blue'
    TabOrder = 4
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 400
    Top = 510
    Width = 185
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '#00393^eDisplay uncommon radicals gray'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 360
    Top = 8
    Width = 401
    Height = 35
    Caption = '#00394^eSearch method'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '#00395^eParts of characters'
      '#00396^eClassical radicals')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
end
