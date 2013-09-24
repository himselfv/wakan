object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'WindowPlacement Test'
  ClientHeight = 531
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 108
    Height = 13
    Caption = 'Form as Delphi sees it:'
  end
  object lblStatus: TLabel
    Left = 136
    Top = 8
    Width = 10
    Height = 13
    Caption = 'nil'
  end
  object Label2: TLabel
    Left = 8
    Top = 136
    Width = 122
    Height = 13
    Caption = 'Form as Windows sees it:'
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Destroy'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 176
    Top = 32
    Width = 75
    Height = 25
    Hint = 'Sets Visible to True and brings to the front'
    Caption = 'Show'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 338
    Top = 32
    Width = 75
    Height = 25
    Hint = 'Equivalent to Visible:=false'
    Caption = 'Hide'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 338
    Top = 6
    Width = 75
    Height = 20
    Caption = 'Update'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 155
    Width = 153
    Height = 25
    Caption = 'GetWindowPlacement'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 8
    Top = 186
    Width = 153
    Height = 25
    Caption = 'SW_HIDE'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Tag = 3
    Left = 8
    Top = 217
    Width = 153
    Height = 25
    Caption = 'SW_MAXIMIZE'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button9: TButton
    Tag = 6
    Left = 8
    Top = 248
    Width = 153
    Height = 25
    Caption = 'SW_MINIMIZE'
    TabOrder = 8
    OnClick = Button7Click
  end
  object Button10: TButton
    Tag = 9
    Left = 8
    Top = 279
    Width = 153
    Height = 25
    Caption = 'SW_RESTORE'
    TabOrder = 9
    OnClick = Button7Click
  end
  object Button11: TButton
    Tag = 5
    Left = 8
    Top = 310
    Width = 153
    Height = 25
    Caption = 'SW_SHOW'
    TabOrder = 10
    OnClick = Button7Click
  end
  object Button12: TButton
    Tag = 3
    Left = 8
    Top = 341
    Width = 153
    Height = 25
    Caption = 'SW_SHOWMAXIMIZED'
    TabOrder = 11
    OnClick = Button7Click
  end
  object Button13: TButton
    Tag = 2
    Left = 8
    Top = 372
    Width = 153
    Height = 25
    Caption = 'SW_SHOWMINIMIZED'
    TabOrder = 12
    OnClick = Button7Click
  end
  object Button14: TButton
    Tag = 7
    Left = 8
    Top = 403
    Width = 153
    Height = 25
    Caption = 'SW_SHOWMINNOACTIVE'
    TabOrder = 13
    OnClick = Button7Click
  end
  object Button15: TButton
    Tag = 8
    Left = 8
    Top = 434
    Width = 153
    Height = 25
    Caption = 'SW_SHOWNA'
    TabOrder = 14
    OnClick = Button7Click
  end
  object Button16: TButton
    Tag = 4
    Left = 8
    Top = 465
    Width = 153
    Height = 25
    Caption = 'SW_SHOWNOACTIVATE'
    TabOrder = 15
    OnClick = Button7Click
  end
  object Button17: TButton
    Tag = 1
    Left = 8
    Top = 496
    Width = 153
    Height = 25
    Caption = 'SW_SHOWNORMAL'
    TabOrder = 16
    OnClick = Button7Click
  end
  object mmPla: TMemo
    Left = 167
    Top = 157
    Width = 185
    Height = 166
    Lines.Strings = (
      'mmPla')
    TabOrder = 17
  end
  object Button18: TButton
    Left = 257
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Visible:=True'
    TabOrder = 18
    OnClick = Button18Click
  end
  object Button19: TButton
    Left = 257
    Top = 63
    Width = 75
    Height = 25
    Hint = 
      'Explicitly apply BoundsRect taken from the latest WindowPlacemen' +
      't'
    Caption = 'SetBounds'
    TabOrder = 19
    OnClick = Button19Click
  end
  object Button20: TButton
    Left = 8
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Maximize'
    TabOrder = 20
    OnClick = Button20Click
  end
  object Button21: TButton
    Left = 89
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Minimize'
    TabOrder = 21
    OnClick = Button21Click
  end
  object Button22: TButton
    Left = 176
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Restore'
    TabOrder = 22
    OnClick = Button22Click
  end
end
