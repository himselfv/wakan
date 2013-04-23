unit WakanWordGrid;

interface
uses
  SysUtils, Classes, Controls, StdCtrls, Grids, Windows;

type
  TWakanGrid = class(TStringGrid)
  protected //Resizing and Auto-sizing
    FLastWidth: integer;
    FLastHeight: integer;
    FLastControlWidth: integer;
    FLastControlHeight: integer;
    FOnWidthResize: TNotifyEvent;
    FOnHeightResize: TNotifyEvent;
    FOnControlWidthResize: TNotifyEvent;
    FOnControlHeightResize: TNotifyEvent;
    function ScrollBarVisible(Code: Word): Boolean;
    function GetControlWidth: integer; //including scrollbars, if visible
    function GetControlHeight: integer;
    procedure Resize; override;
    procedure WidthResize; virtual;
    procedure HeightResize; virtual;
    procedure ControlWidthResize; virtual;
    procedure ControlHeightResize; virtual;
  published
    property OnResize;
    property OnWidthResize: TNotifyEvent read FOnWidthResize write FOnWidthResize;
    property OnHeightResize: TNotifyEvent read FOnHeightResize write FOnHeightResize;
    property OnControlWidthResize: TNotifyEvent read FOnControlWidthResize write FOnControlWidthResize;
    property OnControlHeightResize: TNotifyEvent read FOnControlHeightResize write FOnControlHeightResize;

  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

  end;

  TWakanWordGrid = class(TWakanGrid)
  protected
    procedure ControlWidthResize; override;
  public
    procedure AutoSizeColumns;
  end;

procedure Register;

implementation
uses Graphics;

constructor TWakanGrid.Create(AOwner: TComponent);
begin
  inherited;
 //Default values
  DefaultDrawing := false; //We do all drawing in custom
end;

{ Delphi, stop sucking. Why did you need to make these important functions
 private?
 Why the painting mechanics for TCustomGrid is so unextensible? }
type
  TCustomGridHack = class helper for TCustomGrid
  public
    procedure __GridRectToScreenRect(GridRect: TGridRect;
      var ScreenRect: TRect; IncludeLine: Boolean); inline;
    function __GetSelection: TGridRect; inline;
  end;

procedure TCustomGridHack.__GridRectToScreenRect(GridRect: TGridRect;
  var ScreenRect: TRect; IncludeLine: Boolean);
begin
  Self.GridRectToScreenRect(GridRect, ScreenRect, IncludeLine);
end;

function TCustomGridHack.__GetSelection: TGridRect;
begin
  Result := Self.GetSelection;
end;

procedure TWakanGrid.Paint;
var FocRect, AFocRect: TRect;
begin
  inherited Paint;

 { Disabling DefaultDrawing also disables focus painting.
  We can't do it cell-by-cell if RowSelect is enabled, so we do it here. }
  if not (csDesigning in ComponentState) and
    (goRowSelect in Options) and (not DefaultDrawing) and Focused then
  begin
    __GridRectToScreenRect(__GetSelection, FocRect, False);
    Canvas.Brush.Style := bsSolid;
    if (FInternalDrawingStyle = gdsThemed) and (Win32MajorVersion >= 6) then
      InflateRect(FocRect, -1, -1);
    AFocRect := FocRect;
    if not UseRightToLeftAlignment then
      Canvas.DrawFocusRect(AFocRect)
    else
    begin
      AFocRect := FocRect;
      AFocRect.Left := FocRect.Right;
      AFocRect.Right := FocRect.Left;
      DrawFocusRect(Canvas.Handle, AFocRect);
    end;
  end;

end;


function TWakanGrid.ScrollBarVisible(Code: Word): Boolean;
var Min, Max: Integer;
  flag: integer;
begin
  Result := False;
{
  if (ScrollBars = ssBoth) or
    ((Code = SB_HORZ) and (ScrollBars = ssHorizontal)) or
    ((Code = SB_VERT) and (ScrollBars = ssVertical)) then
}
  case Code of
    SB_HORZ: flag := WS_HSCROLL;
    SB_VERT: flag := WS_VSCROLL;
  else flag := 0;
  end;

  if (GetWindowLong(Handle, GWL_STYLE) and flag) <> 0 then
  begin
    GetScrollRange(Handle, Code, Min, Max);
    Result := Min <> Max;
  end;
end;

function TWakanGrid.GetControlWidth: integer;
begin
  Result := ClientWidth;
  if ScrollBarVisible(SB_VERT) then
    Inc(Result, GetSystemMetrics(SM_CXVSCROLL));
end;

function TWakanGrid.GetControlHeight: integer;
begin
  Result := ClientHeight;
  if ScrollBarVisible(SB_HORZ) then
    Inc(Result, GetSystemMetrics(SM_CYHSCROLL));
end;

procedure TWakanGrid.Resize;
var NewWidth, NewHeight: integer;
begin
  inherited;
  NewWidth := ClientWidth;
  if NewWidth<>FLastWidth then begin
    WidthResize;
    FLastWidth := NewWidth;
  end;
  NewHeight := ClientHeight;
  if NewHeight<>FLastHeight then begin
    HeightResize;
    FLastHeight := NewHeight;
  end;

  NewWidth := GetControlWidth;
  if NewWidth<>FLastControlWidth then begin
    ControlWidthResize;
    FLastControlWidth := NewWidth;
  end;
  NewHeight := GetControlHeight;
  if NewHeight<>FLastControlHeight then begin
    ControlHeightResize;
    FLastControlHeight := NewHeight;
  end;
end;

procedure TWakanGrid.WidthResize;
begin
  if Assigned(OnWidthResize) then OnWidthResize(Self);
end;

procedure TWakanGrid.HeightResize;
begin
  if Assigned(OnHeightResize) then OnHeightResize(Self);
end;

procedure TWakanGrid.ControlWidthResize;
begin
  if Assigned(OnControlWidthResize) then OnControlWidthResize(Self);
end;

procedure TWakanGrid.ControlHeightResize;
begin
  if Assigned(OnControlHeightResize) then OnControlHeightResize(Self);
end;

procedure TWakanWordGrid.ControlWidthResize;
begin
  inherited;
  AutoSizeColumns;
end;

procedure TWakanWordGrid.AutoSizeColumns;
begin
  ColWidths[2]:=Self.Width-ColWidths[1]-ColWidths[0]-24;
end;

procedure Register;
begin
  RegisterComponents('Wakan', [TWakanWordGrid]);
end;

end.
