unit RangeSpinEdit;
{ Similar to TSpinEdit, but supports range expressions such as "3-5" }

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows, Vcl.Samples.Spin;

type
 //Simply TEdit for expressions of type "3-5", supports mouse wheel
  TRangeEdit = class(TCustomEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FEditorEnabled: Boolean;
    function GetMinHeight: Integer;
    function GetLowValue: LongInt;
    function GetHighValue: LongInt;
    procedure GetValue(out ALowValue, AHighValue: LongInt);
    function CheckValue(NewValue: LongInt): LongInt;
    procedure SetLowValue(NewValue: LongInt);
    procedure SetHighValue(NewValue: LongInt);
    procedure SetValue(ALowValue, AHighValue: LongInt);
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
  protected
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure ExpandClick(Sender: TObject); virtual;
    procedure ShrinkClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxLength;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property LowValue: LongInt read GetLowValue write SetLowValue;
    property HighValue: LongInt read GetHighValue write SetHighValue;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  //TSpinEdit for TRangeEdit, with additional buttons
  TRangeSpinEdit = class(TRangeEdit)
  protected
    FPlusButton: TSpinButton;
    FExpandButton: TSpinButton;
    procedure CreateWnd; override;
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PlusButton: TSpinButton read FPlusButton;
    property ExpandButton: TSpinButton read FExpandButton;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Wakan', [TRangeSpinEdit]);
end;

{$R 'RangeSpin'}

constructor TRangeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
  ParentBackground := False;
end;

destructor TRangeEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TRangeEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TRangeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then
    if ssCtrl in Shift then
      ExpandClick(Self)
    else
      UpClick(Self)
  else
  if Key = VK_DOWN then
    if ssCtrl in Shift then
      ShrinkClick(Self)
    else
      DownClick (Self);
  inherited KeyDown(Key, Shift);
end;

procedure TRangeEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TRangeEdit.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if ssCtrl in Shift then
    ShrinkClick(Self)
  else
    DownClick(Self);
  Result := true;
end;

function TRangeEdit.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if ssCtrl in Shift then
    ExpandClick(Self)
  else
    UpClick(Self);
  Result := true;
end;

function TRangeEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (CharInSet(Key, [FormatSettings.DecimalSeparator, '+', '-', '0'..'9'])) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TRangeEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

function TRangeEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TRangeEdit.UpClick(Sender: TObject);
var lo, hi: integer;
begin
  if ReadOnly then MessageBeep(0)
  else begin
    GetValue(lo, hi);
    SetValue(lo + FIncrement, hi + FIncrement);
  end;
end;

procedure TRangeEdit.DownClick(Sender: TObject);
var lo, hi: integer;
begin
  if ReadOnly then MessageBeep(0)
  else begin
    GetValue(lo, hi);
    SetValue(lo - FIncrement, hi - FIncrement);
  end;
end;

procedure TRangeEdit.ExpandClick(Sender: TObject);
var lo, hi: integer;
begin
  if ReadOnly then MessageBeep(0)
  else begin
    GetValue(lo, hi);
    SetValue(lo - FIncrement, hi + FIncrement);
  end;
end;

procedure TRangeEdit.ShrinkClick(Sender: TObject);
var lo, hi: integer;
begin
  if ReadOnly then MessageBeep(0)
  else begin
    GetValue(lo, hi);
    SetValue(lo + FIncrement, hi - FIncrement);
  end;
end;

procedure TRangeEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TRangeEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TRangeEdit.CMExit(var Message: TCMExit);
var lo, hi: integer;
begin
  inherited;
  lo := LowValue;
  hi := HighValue;
  if (CheckValue(lo) <> lo) or (CheckValue(hi) <> hi) or (lo > hi) then
    SetValue(lo, hi);
end;

function TRangeEdit.GetLowValue: LongInt;
var AHighValue: integer;
begin
  GetValue(Result, AHighValue);
end;

procedure TRangeEdit.SetLowValue(NewValue: LongInt);
var AHighValue: integer;
begin
  AHighValue := GetHighValue;
  NewValue := CheckValue(NewValue);
  if NewValue > AHighValue then
    SetValue(NewValue, NewValue)
  else
    SetValue(NewValue, AHighValue);
end;

function TRangeEdit.GetHighValue: LongInt;
var ALowValue: integer;
begin
  GetValue(ALowValue, Result);
end;

procedure TRangeEdit.SetHighValue(NewValue: LongInt);
var ALowValue: integer;
begin
  ALowValue := GetLowValue;
  NewValue := CheckValue(NewValue);
  if ALowValue > NewValue then
    SetValue(NewValue, NewValue)
  else
    SetValue(ALowValue, NewValue);
end;

procedure TRangeEdit.GetValue(out ALowValue, AHighValue: LongInt);
var i: integer;
  AText: string;
begin
  AText := Trim(Self.Text);
  i := pos('-', AText);
  if i=1 then
    //allow at most one minus before the first element
    i := pos('-', AText, 2);
  if i>0 then begin
    ALowValue := StrToIntDef(copy(Text, 1, i-1), FMinValue);
    AHighValue := StrToIntDef(copy(Text, i+1, MaxInt), FMinValue);
  end else begin
    ALowValue := StrToIntDef(Text, FMinValue);
    AHighValue := ALowValue;
  end;
end;

procedure TRangeEdit.SetValue(ALowValue, AHighValue: LongInt);
begin
  ALowValue := CheckValue(ALowValue);
  AHighValue := CheckValue(AHighValue);
  if ALowValue > AHighValue then
    AHighValue := ALowValue;
  if ALowValue=AHighValue then
    Text := IntToStr(ALowValue)
  else
    Text := IntToStr(ALowValue) + '-' + IntToStr(AHighValue);
end;

function TRangeEdit.CheckValue(NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if FMaxValue <> FMinValue then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TRangeEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;


constructor TRangeSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlusButton := TSpinButton.Create(Self);
  FPlusButton.Width := 15;
  FPlusButton.Height := 17;
  FPlusButton.Visible := True;
  FPlusButton.Parent := Self;
  FPlusButton.FocusControl := Self;
  FPlusButton.OnUpClick := UpClick;
  FPlusButton.OnDownClick := DownClick;
  FExpandButton := TSpinButton.Create(Self);
  FExpandButton.Width := 15;
  FExpandButton.Height := 17;
  FExpandButton.Visible := True;
  FExpandButton.Parent := Self;
  FExpandButton.FocusControl := Self;
  FExpandButton.OnUpClick := ExpandClick;
  FExpandButton.OnDownClick := ShrinkClick;
  FExpandButton.UpGlyph.Handle := LoadBitmap(HInstance, 'SpinExpand');
  FExpandButton.DownGlyph.Handle := LoadBitmap(HInstance, 'SpinShrink');
end;

destructor TRangeSpinEdit.Destroy;
begin
  FExpandButton := nil;
  FPlusButton := nil;
  inherited;
end;

procedure TRangeSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TRangeSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FPlusButton.Width - FExpandButton.Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

procedure TRangeSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  ExpandBtnRect: TRect;
begin
  inherited;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else if (FPlusButton <> nil) and (FExpandButton <> nil) then
  begin
    if NewStyleControls and Ctl3D then
      ExpandBtnRect := Rect(Width - FExpandButton.Width - 5, 0, FExpandButton.Width, Height - 5)
    else
      ExpandBtnRect := Rect(Width - FExpandButton.Width, 1, FExpandButton.Width, Height - 3);
    FExpandButton.SetBounds(ExpandBtnRect.Left, ExpandBtnRect.Top,
      ExpandBtnRect.Width, ExpandBtnRect.Height);
    if NewStyleControls and Ctl3D then
      FPlusButton.SetBounds(ExpandBtnRect.Left - FPlusButton.Width, 0, FPlusButton.Width, Height - 5)
    else
      FPlusButton.SetBounds(ExpandBtnRect.Left - FPlusButton.Width, 1, FPlusButton.Width, Height - 3);
    SetEditRect;
  end;
end;

end.
