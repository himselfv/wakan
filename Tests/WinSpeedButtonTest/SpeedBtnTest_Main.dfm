object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 715
  ClientWidth = 952
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton2: TSpeedButton
    Left = 40
    Top = 666
    Width = 23
    Height = 22
  end
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Native'
  end
  object Label2: TLabel
    Left = 8
    Top = 53
    Width = 37
    Height = 13
    Caption = 'Buttons'
  end
  object Label3: TLabel
    Left = 8
    Top = 90
    Width = 32
    Height = 13
    Caption = 'Classic'
  end
  object Label4: TLabel
    Left = 8
    Top = 127
    Width = 33
    Height = 13
    Caption = 'Hidden'
  end
  object Label5: TLabel
    Left = 8
    Top = 164
    Width = 23
    Height = 13
    Caption = 'Vista'
  end
  object Label6: TLabel
    Left = 8
    Top = 201
    Width = 35
    Height = 13
    Caption = 'Default'
  end
  object Label7: TLabel
    Left = 8
    Top = 238
    Width = 69
    Height = 13
    Caption = 'Non-focusable'
  end
  object Label8: TLabel
    Left = 8
    Top = 275
    Width = 42
    Height = 13
    Caption = 'No glyph'
  end
  object Label9: TLabel
    Left = 8
    Top = 395
    Width = 52
    Height = 13
    Caption = 'WordWrap'
  end
  object Label10: TLabel
    Left = 8
    Top = 349
    Width = 66
    Height = 13
    Caption = 'Disabled vista'
  end
  object Label11: TLabel
    Left = 8
    Top = 481
    Width = 68
    Height = 13
    Caption = 'Image layouts'
  end
  object Label12: TLabel
    Left = 8
    Top = 312
    Width = 40
    Height = 13
    Caption = 'Disabled'
  end
  object WinSpeedButton1: TWinSpeedButton
    Left = 104
    Top = 82
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssClassic
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 0
    Transparent = False
  end
  object Button4: TButton
    AlignWithMargins = True
    Left = 464
    Top = 584
    Width = 175
    Height = 85
    Hint = 
      '#01133^Search by reading, writing or meaning, depending on what ' +
      'you type'
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Caption = '#01132^Any matches blah blah blah blah'
    ImageIndex = 1
    Images = ilCategoryActions
    Style = bsSplitButton
    TabOrder = 1
    TabStop = False
    WordWrap = True
  end
  object Button1: TButton
    Left = 90
    Top = 601
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 169
    Top = 590
    Width = 75
    Height = 73
    Caption = 'Button1'
    ImageIndex = 0
    TabOrder = 3
  end
  object Button3: TButton
    Left = 252
    Top = 601
    Width = 75
    Height = 25
    Caption = 'Button1'
    Default = True
    TabOrder = 4
  end
  object BitBtn1: TBitBtn
    Left = 333
    Top = 601
    Width = 75
    Height = 25
    Caption = 'BitBtn1'
    Default = True
    TabOrder = 5
  end
  object BitBtn2: TBitBtn
    Left = 333
    Top = 632
    Width = 75
    Height = 25
    Caption = 'BitBtn1'
    TabOrder = 6
  end
  object WinSpeedButton2: TWinSpeedButton
    Left = 104
    Top = 8
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 7
    Transparent = False
  end
  object WinSpeedButton3: TWinSpeedButton
    Left = 104
    Top = 45
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssButtons
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 8
    Transparent = False
  end
  object WinSpeedButton4: TWinSpeedButton
    Left = 104
    Top = 119
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssHidden
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 9
    Transparent = False
  end
  object WinSpeedButton5: TWinSpeedButton
    Left = 104
    Top = 156
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssVista
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 10
    Transparent = False
  end
  object WinSpeedButton6: TWinSpeedButton
    Left = 288
    Top = 8
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 11
    Transparent = False
  end
  object WinSpeedButton7: TWinSpeedButton
    Left = 288
    Top = 45
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssButtons
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 12
    Transparent = False
  end
  object WinSpeedButton8: TWinSpeedButton
    Left = 288
    Top = 82
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssClassic
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 13
    Transparent = False
  end
  object WinSpeedButton9: TWinSpeedButton
    Left = 288
    Top = 119
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssHidden
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 14
    Transparent = False
  end
  object WinSpeedButton10: TWinSpeedButton
    Left = 288
    Top = 156
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssVista
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 15
    Transparent = False
  end
  object WinSpeedButton11: TWinSpeedButton
    Left = 480
    Top = 8
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 16
    Transparent = False
  end
  object WinSpeedButton12: TWinSpeedButton
    Left = 480
    Top = 45
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssButtons
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 17
    Transparent = False
  end
  object WinSpeedButton13: TWinSpeedButton
    Left = 480
    Top = 82
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssClassic
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 18
    Transparent = False
  end
  object WinSpeedButton14: TWinSpeedButton
    Left = 480
    Top = 119
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssHidden
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 19
    Transparent = False
  end
  object WinSpeedButton15: TWinSpeedButton
    Left = 480
    Top = 156
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssVista
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 20
    Transparent = False
  end
  object WinSpeedButton16: TWinSpeedButton
    Left = 672
    Top = 8
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 21
    Transparent = False
  end
  object WinSpeedButton17: TWinSpeedButton
    Left = 672
    Top = 45
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssButtons
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 22
    Transparent = False
  end
  object WinSpeedButton18: TWinSpeedButton
    Left = 672
    Top = 82
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssClassic
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 23
    Transparent = False
  end
  object WinSpeedButton19: TWinSpeedButton
    Left = 672
    Top = 119
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssHidden
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 24
    Transparent = False
  end
  object WinSpeedButton20: TWinSpeedButton
    Left = 672
    Top = 156
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.SplitterStyle = ssVista
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 25
    Transparent = False
  end
  object WinSpeedButton21: TWinSpeedButton
    Left = 104
    Top = 193
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    Default = True
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 26
    Transparent = False
  end
  object WinSpeedButton22: TWinSpeedButton
    Left = 288
    Top = 193
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    Default = True
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 27
    Transparent = False
  end
  object WinSpeedButton23: TWinSpeedButton
    Left = 672
    Top = 193
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    Default = True
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 28
    Transparent = False
  end
  object WinSpeedButton24: TWinSpeedButton
    Left = 480
    Top = 193
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    Default = True
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 29
    Transparent = False
  end
  object WinSpeedButton25: TWinSpeedButton
    Left = 104
    Top = 230
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Focusable = False
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 30
    Transparent = False
  end
  object WinSpeedButton26: TWinSpeedButton
    Left = 288
    Top = 230
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    Focusable = False
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 31
    Transparent = False
  end
  object WinSpeedButton27: TWinSpeedButton
    Left = 480
    Top = 230
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Focusable = False
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 32
    Transparent = False
  end
  object WinSpeedButton28: TWinSpeedButton
    Left = 672
    Top = 230
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    Focusable = False
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 33
    Transparent = False
  end
  object WinSpeedButton29: TWinSpeedButton
    Left = 104
    Top = 267
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 34
    Transparent = False
  end
  object WinSpeedButton30: TWinSpeedButton
    Left = 288
    Top = 267
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 35
    Transparent = False
  end
  object WinSpeedButton31: TWinSpeedButton
    Left = 480
    Top = 267
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 36
    Transparent = False
  end
  object WinSpeedButton32: TWinSpeedButton
    Left = 672
    Top = 267
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 37
    Transparent = False
  end
  object WinSpeedButton33: TWinSpeedButton
    Left = 104
    Top = 378
    Width = 170
    Height = 49
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 38
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton34: TWinSpeedButton
    Left = 288
    Top = 378
    Width = 170
    Height = 49
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 39
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton35: TWinSpeedButton
    Left = 480
    Top = 378
    Width = 170
    Height = 49
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 40
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton36: TWinSpeedButton
    Left = 672
    Top = 378
    Width = 170
    Height = 49
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 41
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton37: TWinSpeedButton
    Left = 104
    Top = 433
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    GroupIndex = 1
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 42
    Transparent = False
  end
  object WinSpeedButton38: TWinSpeedButton
    Left = 288
    Top = 433
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    GroupIndex = 2
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 43
    Transparent = False
  end
  object WinSpeedButton39: TWinSpeedButton
    Left = 480
    Top = 433
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    ForceClassicLook = True
    GroupIndex = 3
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 44
    Transparent = False
  end
  object WinSpeedButton40: TWinSpeedButton
    Left = 672
    Top = 433
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.Size = 32
    DropButtonSettings.ImageIndex = 1
    DropDownMenu = PopupMenu1
    Flat = True
    ForceClassicLook = True
    GroupIndex = 4
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 45
    Transparent = False
  end
  object WinSpeedButton41: TWinSpeedButton
    Left = 104
    Top = 470
    Width = 170
    Height = 89
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    Layout = blGlyphTop
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 46
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton42: TWinSpeedButton
    Left = 288
    Top = 470
    Width = 170
    Height = 89
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    Layout = blGlyphBottom
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 47
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton43: TWinSpeedButton
    Left = 480
    Top = 470
    Width = 170
    Height = 89
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    ImageIndex = 1
    Images = ilCategoryActions
    Layout = blGlyphRight
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 48
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton44: TWinSpeedButton
    Left = 672
    Top = 470
    Width = 170
    Height = 89
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test blah blah blah blah blah blah'
    DropButtonSettings.Size = 32
    DropDownMenu = PopupMenu1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 49
    Transparent = False
    WordWrap = True
  end
  object WinSpeedButton45: TWinSpeedButton
    Left = 104
    Top = 304
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    Enabled = False
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 50
    Transparent = False
  end
  object WinSpeedButton46: TWinSpeedButton
    Left = 288
    Top = 304
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    Enabled = False
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 51
    Transparent = False
  end
  object WinSpeedButton47: TWinSpeedButton
    Left = 480
    Top = 304
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    Enabled = False
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 52
    Transparent = False
  end
  object WinSpeedButton48: TWinSpeedButton
    Left = 672
    Top = 304
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropDownMenu = PopupMenu1
    Enabled = False
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 53
    Transparent = False
  end
  object Button5: TButton
    AlignWithMargins = True
    Left = 648
    Top = 584
    Width = 175
    Height = 85
    Hint = 
      '#01133^Search by reading, writing or meaning, depending on what ' +
      'you type'
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Caption = '#01132^Any matches blah blah blah blah'
    Enabled = False
    ImageIndex = 1
    Images = ilCategoryActions
    Style = bsSplitButton
    TabOrder = 54
    TabStop = False
    WordWrap = True
  end
  object WinSpeedButton49: TWinSpeedButton
    Left = 104
    Top = 341
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.SplitterStyle = ssClassic
    DropDownMenu = PopupMenu1
    Enabled = False
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 55
    Transparent = False
  end
  object WinSpeedButton50: TWinSpeedButton
    Left = 288
    Top = 341
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.SplitterStyle = ssClassic
    DropDownMenu = PopupMenu1
    Enabled = False
    Flat = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 56
    Transparent = False
  end
  object WinSpeedButton51: TWinSpeedButton
    Left = 480
    Top = 341
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.SplitterStyle = ssVista
    DropDownMenu = PopupMenu1
    Enabled = False
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 57
    Transparent = False
  end
  object WinSpeedButton52: TWinSpeedButton
    Left = 672
    Top = 341
    Width = 170
    Height = 31
    Hint = '#00188^eDisplay only characters in clipboard'
    AllowAllUp = True
    Caption = 'Test'
    DropButtonSettings.SplitterStyle = ssVista
    DropDownMenu = PopupMenu1
    Enabled = False
    Flat = True
    ForceClassicLook = True
    ImageIndex = 1
    Images = ilCategoryActions
    SelectedImageIndex = 3
    Style = bsSplitButton
    TabOrder = 58
    Transparent = False
  end
  object ilCategoryActions: TImageList
    Left = 696
    Top = 670
    Bitmap = {
      494C010104001401F80110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      00007F7F7F007F7F7F007F7F7F0000FFFF0000FFFF007F7F7F007F7F7F007F7F
      7F007F7F7F0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF0000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000FF000000
      FF0000000000000000007F7F7F00000000007F7F7F00000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF0000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000FFFFFF00000000000000000000000000FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF0000000000000000000000FF00000000000000
      FF000000FF000000FF007F7F7F00000000007F7F7F0000000000000000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF0000000000FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000000000FFFF0000FFFF0000FFFF000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF000000000000FFFF0000FFFF000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF00000000000000000000FFFF0000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000FFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      000000000000000000007F7F7F00000000007F7F7F0000000000000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00000000000000000000FFFF0000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000FF000000FF00000000000000
      000000000000000000000000800000000000000080000000FF00000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000FFFFFF0000000000BFBFBF00FFFFFF0000000000FFFFFF000000000000FF
      FF000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000FF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF0000FFFF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF00FFFFFF0000000000000000000000FF000000FF000000
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00000000000000000000000000000000000000FF000000
      FF000000FF00000000007F7F7F00000000007F7F7F0000000000000000000000
      FF000000FF000000000000000000000000000000000000FFFF0000FFFF000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      00000000000000FFFF0000FFFF000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000000000000000000000000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF7EFC00FC00FFFF9001F000FC00F83F
      C003C0002000E00FE00300000000CC47E003000000008463E00300000000A073
      E0030000000031F900010000000038F98000000000003C79E007000000003C39
      E00F0001E0003C19E00F0003F8009C0BE0270007F0008C43C073001FE001C467
      9E79007FC403E00F7EFE01FFEC07F83F00000000000000000000000000000000
      000000000000}
  end
  object PopupMenu1: TPopupMenu
    Left = 80
    Top = 666
    object Asd1: TMenuItem
      Caption = 'Asd'
    end
    object Bsd1: TMenuItem
      Caption = 'Bsd'
    end
    object N1231: TMenuItem
      Caption = '123'
    end
  end
end
