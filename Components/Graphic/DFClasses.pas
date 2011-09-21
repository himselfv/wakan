
{ --------------------------------------------------------------------------
	Name: DFClasses    written by William Yang
	Email: yang@btinternet.com		URL: http://www.btinternet.com/~yang

  Classes
  	-TObjectsList - Auto Free up the object instance when delete or destroy
    -TMatrix
    -TObjMatrix
    -TTimerAlarmer - Alarm service
    -TDFGraphicControl - a far much quick graphic control
  -------------------------------------------------------------------------- }

unit DFClasses;

interface

uses Windows, Classes, SysUtils, ExtCtrls, Controls, Graphics;

type

  { ------------------------------------------------------------------------
  	The advantages of TObjectsList is it can free objects automatically
    while you delete an object.
    ------------------------------------------------------------------------ }

	TObjectsList = class(TList)
  private
  	function GetObjs(Index: Integer) : TObject;
    procedure SetObjs(Index: Integer; Val: TObject);
  public
  	destructor Destroy; override;
  	property Objects[Index: Integer] : TObject read GetObjs write SetObjs;
    procedure FreeItem(Index: Integer);
  end;

  { --------------------------------------------------------------------------
    Store dynamic data in matrix format
    -------------------------------------------------------------------------- }
  TOnCellChange = procedure (Sender: TObject; R, C: Word) of Object;
  TMatrix = class(TList)
  private
    FRows, FCols : Integer;
    fChanged: TOnCellChange;
    function GetCells(R, C : Integer) : Pointer;
    procedure SetCells(R, C : Integer; Val : Pointer);
  public
  	constructor Create(R, C : Integer);
    procedure Recreate(R, C : Integer);
    property Rows : Integer read FRows;
    property Columns : Integer read FCols;
    property Cells[R, C : Integer] : Pointer read GetCells write SetCells;
    property OnChange : TOnCellChange read fChanged write fChanged;
  end;

  { --------------------------------------------------------------------------
    Store dynamic objects data in matrix format
    -------------------------------------------------------------------------- }

  TObjMatrix = class(TMatrix)
	private
    function GetObjs(R, C : Integer) : TObject;
    procedure SetObjs(R, C : Integer; Val : TObject);
  public
    property Rows;
    property Columns;
    property Objects[R, C : Integer] : TObject read GetObjs write SetObjs;
  end;

	TTimeAlarmer = class(TObject)
  private
  	fTimer: TTimer;
    fCmpList: TObjectsList;

{    fOnStop: TNotifyEvent;
    fOnStart: TNotifyEvent; }

    procedure SetInterval( Val: Integer);
    function GetInterval: Integer;

    procedure OnNotify(Sender: TObject);
  public
  	constructor Create;
    destructor Destroy; override;
    procedure StartTimer;
    procedure StopTimer;
  	procedure Add(Comp: TControl);
  	procedure Remove(Comp: TControl);

    property Interval: Integer read GetInterval write SetInterval;
    property Controls: TObjectsList read fCmpList write fCmpList;

{    property OnStop: TNotifyEvent read fOnStop write fOnStop;
    property OnStart: TNotifyEvent read fOnStart write fOnStart;}

  end;

  TDFStream = class(TMemoryStream)
  private
  public

  	procedure WriteInteger(Val: LongInt);
    procedure WriteFloat(Val: Extended);
  	procedure WriteChar(Val: Char);
    procedure WriteBool(Val: Boolean);
  	procedure WriteString(Val: String);

  	function ReadInteger: LongInt;
    function ReadFloat: Extended;
  	function ReadChar: Char;
    function ReadBool: Boolean;
  	function ReadString: String;

    procedure WriteStrings(Val: TStringList);
    procedure ReadStrings(Val: TStringList);

  end;

  TDFGraphicControl = class(TGraphicControl)
  private
  	fBackup: TBitmap;
  protected
  	property Backup: TBitmap read fBackup;
		procedure Paint; override;
    procedure RepaintAll; dynamic;
    procedure PaintBackup; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


var
	Alarmer: TTimeAlarmer;

implementation

{ TObjectsList }
destructor TObjectsList.Destroy;
var
	i : Integer;
begin
	for i := 0 to Count - 1 do
    Objects[i].Free;
  Pack;
  inherited Destroy;
end;

function TObjectsList.GetObjs(Index: Integer): TObject;
begin
	Result := TObject(Items[Index]);
end;

procedure TObjectsList.SetObjs(Index: Integer; Val : TObject);
begin
  Items[Index] := @Val;
end;

procedure TObjectsList.FreeItem(Index: Integer);
begin
	Objects[Index].Free;
  inherited Delete(Index);
end;


{ TMatrix }
constructor TMatrix.Create(R, C : Integer);
var
	I: Integer;
begin
	inherited Create;
  FRows := R;
  FCols := C;
  Capacity := (FRows * FCols + 1);
	for i := 1 to FRows * FCols + 1 do
  	Add(nil);
end;

procedure TMatrix.Recreate(R, C : Integer);
var
	i: Integer;
begin
  FRows := R;
  FCols := C;
  Capacity := (FRows * FCols + 1);
	Clear;
  for i := 1 to FRows * FCols + 1 do Add(nil)
end;

function TMatrix.GetCells(R, C : Integer) : Pointer;
begin
	Result := Items[(fRows*C)+R];
end;

procedure TMatrix.SetCells(R, C : Integer; Val : Pointer);
begin
	Items[(fRows*C)+R] := Val;
  if Assigned(fChanged) then fChanged(Self, R, C);
end;

{ TObjMatrix }
function TObjMatrix.GetObjs(R, C : Integer) : TObject;
begin
	Result := TObject(Cells[R, C]);
end;

procedure TObjMatrix.SetObjs(R, C : Integer; Val : TObject);
begin
	Cells[R, C] := Val;
end;

{ TEventNotifier}
constructor TTimeAlarmer.Create;
begin
	inherited Create;
  fCmpList := TObjectsList.Create;
end;

destructor TTimeAlarmer.Destroy;
begin
  fCmpList.Free;
  if fTimer <> nil then FTimer.Free;
	inherited Destroy;
end;

procedure TTimeAlarmer.StartTimer;
begin
  if fTimer = nil then
  begin
  	fTimer := TTimer.Create(nil);
    fTimer.Interval := 1000*2;
    fTImer.Enabled := True;
    fTimer.OnTimer := OnNotify;
  end;
end;

procedure TTimeAlarmer.Add(Comp: TControl);
begin
	fCmpList.Add(Comp);
end;

procedure TTimeAlarmer.Remove(Comp: TControl);
begin
	fCmpList.Remove(Comp);
end;

procedure TTimeAlarmer.OnNotify(Sender: TObject);
var
	i: Integer;
begin
	if fCmpList.Count = 0 then Exit;
  for i := 0 to fCmpList.Count - 1 do
  begin
  	TControl(fCmpList[i]).Repaint;
  end;
end;

procedure TTimeAlarmer.SetInterval( Val: Integer);
begin
	fTimer.Interval := Val;
end;

function TTimeAlarmer.GetInterval: Integer;
begin
	Result := fTimer.Interval;
end;

procedure TTimeAlarmer.StopTimer;
begin
	fTimer.Enabled := False;
end;

{ TDFStream }
procedure TDFStream.WriteInteger(Val: LongInt);
begin
	Write(Val, SizeOf(Val));
end;

procedure TDFStream.WriteFloat(Val: Extended);
begin
	Write(Val, SizeOf(Val));
end;

procedure TDFStream.WriteChar(Val: Char);
begin
	Write(Val, SizeOf(Val));
end;

procedure TDFStream.WriteBool(Val: Boolean);
begin
	Write(Val, SizeOf(Val));
end;

procedure TDFStream.WriteString(Val: String);
begin
	WriteInteger(SizeOf(Val));
	Write(Val, SizeOf(Val));
end;

function TDFStream.ReadInteger: LongInt;
begin
	Read(Result, SizeOf(Result));
end;

function TDFStream.ReadFloat: Extended;
begin
	Read(Result, SizeOf(Result));
end;

function TDFStream.ReadChar: Char;
begin
	Read(Result, SizeOf(Result));
end;

function TDFStream.ReadBool: Boolean;
begin
	Read(Result, SizeOf(Result));
end;

function TDFStream.ReadString: String;
var
	Len: LongInt;
begin
	Len := ReadInteger;
	Read(Result, Len);
end;

procedure TDFStream.WriteStrings(Val: TStringList);
var
	j: Integer;
begin
	WriteInteger(Val.Count);
  for j := 0 to Val.Count - 1 do
  begin
  	WriteString(Val[j]);
  end;
end;

procedure TDFStream.ReadStrings(Val: TStringList);
var
	j, jc: Integer;
begin
	if Val = nil then Exit; 
	jc := ReadInteger;
  for j := 1 to JC do
  begin
  	Val.Add(ReadString);
  end;
end;

{ TDFGraphicControl }
constructor TDFGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBackup := TBitmap.Create;
//  fBackup.PixelFormat := pf24Bit;
end;

destructor TDFGraphicControl.Destroy;
begin
  fBackup.Free;
  inherited Destroy;
end;

procedure TDFGraphicControl.PaintBackup;
begin
	if fBackup = nil then Exit;
	if (fBackup.Width <> Width) or (fBackup.Height <> Height) then
  begin
		fBackup.Free;
    fBackup := TBitmap.Create;
		fBackup.Width := Width;
  	fBackup.Height := Height;
//    fBackup.FillRect
  end;
end;

procedure TDFGraphicControl.Paint;
var
	x, y, x2, y2: Integer;
begin
	if (fBackup.Width <> Width) or (fBackup.Height <> Height) then PaintBackup;
  with Canvas.ClipRect do
  begin
  	x := Left;
    y := Top;
    x2 := Right-Left;
    y2 := Bottom-Top;
  end;
 	BitBlt(Canvas.Handle, x, y, x2, y2, fBackup.Canvas.Handle, x, y, SRCCOPY);
end;

procedure TDFGraphicControl.RepaintAll;
begin
	PaintBackup;
  Canvas.Draw(0, 0, fBackup);
end;

procedure Finish; far;
begin
	Alarmer.Free;
end;

initialization
	Alarmer := TTimeAlarmer.Create;
  AddExitProc(Finish);

end.

