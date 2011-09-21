
{
	Copyright by Dream Factory
  Author William Yang
  Email: yang@btinternet.com
  URL: http://www.btinternet.com/~yang
}

unit DFCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, DFClasses, Backgnd,
  Colorman, DrawMan, NumMan;

type
	TDFControl = class;

	TBevelSize = 0..5;
	TBevelType = (btNone, btWindows95, btFlat3D, btIE4Interactive, bt2DRect);
	TDFBevel = class(TPersistent)
  private
  	fType: TBevelType;
    fSize: TBevelSize;
    fColor: TColor;
    fOnChange: TNotifyEvent;

    procedure SetType(Val: TBevelType);
    procedure SetSize(Val: TBevelSize);
    procedure SetColor(Val: TColor);

  public

  	procedure Assign( Value: TDFBevel );
  	procedure Changed;

  published

  	property BevelType: TBevelType read fType write SetType;
    property Size: TBevelSize read fSize write SetSize;
    property Color: TColor read fColor write SetColor;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

  end;

  TDFControl = class(TDFGraphicControl)
  private
    { Private declarations }
    fBackground: TBackgndObj;
    fBevel: TDFBevel;
		fFocused: Boolean;
    fPaintRect :TRect;
    fMouseEnter: TNotifyEvent;
    fMouseLeave: TNotifyEvent;
    fMouseOver: Boolean;

    procedure SetBackground(Val: TBackgndObj);
    procedure SetBevel(Val: TDFBevel);
    procedure SetFocused(Val: Boolean);

    procedure BackChanged(Sender: TObject);
    procedure BevelChanged(Sender: TObject);

    procedure MsgMouseEnter(var Message: TMessage); message CM_MouseEnter;
    procedure MsgMouseLeave(var Message: TMessage); message CM_MouseLeave;

  protected
    { Protected declarations }

		procedure PaintBackup; override;

    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;

		property Background: TBackgndObj read fBackground write SetBackground;
    property Bevel: TDFBevel read fBevel write SetBevel;
		property PaintRect: Trect read fPaintRect write fPaintRect;
    property Focused: Boolean read fFocused write SetFocused;
    property MouseOver: Boolean read fMouseOver;

    property OnMouseEnter: TNotifyEvent read fMouseEnter write fMouseEnter;
    property OnMouseLeave: TNotifyEvent read fMouseLeave write fMouseLeave;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
  end;

  TApples = class(TDFControl)
  private
    { Private Declarations }
    FApple: TBitmap;
    FLineWrap: Boolean;
    FTransparentColor: TColor;
    FValue: Extended;
    procedure SetApple(Val: TBitmap);
    procedure SetLineWrap(Val: Boolean);
    procedure SetTransparentColor(Val: TColor);
    procedure SetValue(Val: Extended);
  protected
    { Protected Declarations }
    procedure PaintBackup; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Apple: TBitmap read FApple write SetApple;
    property LineWrap: Boolean read FLineWrap write SetLineWrap;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property Value: Extended read FValue write SetValue;

		property Background: TBackgndObj read fBackground write SetBackground;
    property Bevel: TDFBevel read fBevel write SetBevel;

    property OnMouseEnter: TNotifyEvent read fMouseEnter write fMouseEnter;
    property OnMouseLeave: TNotifyEvent read fMouseLeave write fMouseLeave;

    property Align;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


  TDFScrollEvent = (seNone, seIncClick, seDecClick, sePageUp,
  	sePageDown, seDragThumb);
  TDFScrollOrientation = (soHorizontal, soVertical);
	TDFScroll = class(TDFControl)
  private
  	fUp, fDown, fLeft, fRight: TBitmap;
  	fThumb: TBitmap;
    fMax, fMin, fPosition: Integer;
    fLargeChange: Integer;
    fSmallChange: Integer;
    fOnChange: TNotifyEvent;
    fOrientation: TDFScrollOrientation;
    fMouseDown: Boolean;
    fThumbDrag: TPoint;
    fGlyphs: Byte;
    fCurrEvent: TDFScrollEvent;
    fThumbTrans: Boolean;
    procedure SetUp(Val: TBitmap);
    procedure SetDown(Val: TBitmap);
    procedure SetLeft(Val: TBitmap);
    procedure SetRight(Val: TBitmap);
    procedure SetThumb(Val: TBitmap);

    procedure SetMax(Val: Integer);
    procedure SetMin(Val: Integer);
    procedure SetPosition(Val: Integer);
    procedure SetGlyphs(Val: Byte);
		procedure SetThumbTrans(Val: Boolean);

		procedure SetOrientation(Val: TDFScrollOrientation);

    procedure OnTimer(Sender: TObject);

  protected

  	procedure PaintBackup; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

//    procedure MsgEraseBack(var Message: TWMERASEBKGND); message WM_ERASEBKGND;

    function DecBox: TRect; virtual;
    function IncBox: TRect; virtual;
    function ThumbBox: TRect; virtual;

    function PosToPoint(Pos: Integer): TPoint; virtual;
    function PointToPos(Point: TPoint): Integer; virtual;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PageUp;
    procedure PageDown;
    procedure Increment;
    procedure Decrement;
    procedure ScrollBy(X, Y: Integer);

  published

  	property Pic_Up: TBitmap read fUp write SetUp;
  	property Pic_Down: TBitmap read fDown write SetDown;
  	property Pic_Left: TBitmap read fLeft write SetLeft;
  	property Pic_Right: TBitmap read fRight write SetRight;
  	property Pic_Thumb: TBitmap read fThumb write SetThumb;

		property Max: Integer read fMax write SetMax default 100;
    property Min: Integer read fMin write SetMin default 0;
    property Position: Integer read fPosition write SetPosition default 0;
    property LargeChange: Integer read fLargeChange write fLargeChange default 1;
    property SmallChange: Integer read fSmallChange write fSmallChange default 1;    
    property NumGlyph: Byte read fGlyphs write SetGlyphs default 1;
    property TransparentThumb: Boolean read fThumbTrans write SetThumbTrans default False;
		property Orientation: TDFScrollOrientation read fOrientation write SetOrientation default soHorizontal;

		property Background: TBackgndObj read fBackground write SetBackground;
    property Bevel: TDFBevel read fBevel write SetBevel;

    property OnMouseEnter: TNotifyEvent read fMouseEnter write fMouseEnter;
    property OnMouseLeave: TNotifyEvent read fMouseLeave write fMouseLeave;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    property Align;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

  end;

	TDrawStyle = (dsNone, dsClient, dsCentered);
  TColorOption = (coNone, coTransparent, coSmoothed);
  TBCBitmap = class(TDFControl)
  private
    {Private Declarations}
    {Private Variables}
    FAutoSize: Boolean;
    FPicture: TBitmap;
    FStyle : TDrawStyle;
    FColorOption : TColorOption;
    FInvert : Boolean;
    FMouseDown: Boolean;
    FSmoothed: TBitmap;
    procedure MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); override;
    procedure SetPicture(Val: TBitmap);
    procedure SetStyle(Val : TDrawStyle);
    procedure SetColorOption (Val : TColorOption);
    procedure SetAutoSize(Val: Boolean);
  protected
    {Protected Declarations}
    procedure PaintBackup; override;
  public
   {Public Method}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
  {Properties}
    property Picture: TBitmap read FPicture write SetPicture;
    property DrawStyle : TDrawStyle read FStyle write SetStyle;
    property ColorOption : TColorOption read FColorOption write SetColorOption;
    property InvertImageOnMouseDown : Boolean read FInvert write FInvert;
    property AutoSize: Boolean read FAutosize write SetAutosize default False;
		property Background: TBackgndObj read fBackground write SetBackground;
    property Bevel: TDFBevel read fBevel write SetBevel;
    
  {Events}

    property OnMouseEnter: TNotifyEvent read fMouseEnter write fMouseEnter;
    property OnMouseLeave: TNotifyEvent read fMouseLeave write fMouseLeave;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

  end;


//Rotate font between 0..3600
{$IFDEF DELPHI}
procedure RotateFont(AFont: TFont; Anglo: Integer);
{$ENDIF}
procedure PaintBevel(Canvas: TCanvas; var Rect: Trect; Bevel: TDFBevel; Focused: Boolean);

implementation

var
	fTimer: TTimer;
	Tmp: TBitmap;

function IsBitmapEmpty(Bmp: TBitmap): Boolean;
begin
	Result := False;
	{$IFDEF DF_D3}    {-- Delphi 3 --}

  Result := Bmp.Empty;

	{$ELSE}    {-- Delphi 2 or C++ Builder 1     }

	if Bmp = nil then
  begin
  	Result := True;
    Exit;
  end;
	if Bmp.Handle = 0 then
  begin
  	Result := True;
    Exit;
  end;

  {$ENDIF}
end;

function GetGlyph(Source: TBitmap; Num: Integer): TBitmap;
begin
  with Tmp do
  begin
  	Width := Source.Width div 2;
  	Height := Source.Height;
  	Canvas.CopyRect(Rect(0,0,Width,Height),Source.Canvas, Bounds(Width*Num, 0, Width, Height));
	end;
  Result := Tmp;
end;

procedure PaintBevel(Canvas: TCanvas; var Rect: Trect; Bevel: TDFBevel; Focused: Boolean);
var
	clDarkShadow, clLightShade, clBtnShadow, clLight: TColor;
  l, t, r, b: Integer;

	procedure DrawWin95;
  begin
  	Canvas.Pen.Color := clBtnShadow;
    Canvas.Polyline([Point(r, t), Point(l, t), Point(l, b)]);

  	Canvas.Pen.Color := clDarkShadow;
    Canvas.Polyline([Point(r-1, t+1), Point(l+1, t+1), Point(l+1, b-1)]);

  	Canvas.Pen.Color := clLightShade;
    Canvas.Polyline([Point(l+1, b-1), Point(r-1, b-1), Point(r-1, t+1)]);

  	Canvas.Pen.Color := clLight;
    Canvas.Polyline([Point(l, b), Point(r, b), Point(r, t)]);

    with Rect do
    begin
    	Left := Left + 2;
    	Top := Top + 2;
    	Right := Right - 2;
    	Bottom := Bottom - 2;
    end;

  end;

  procedure Draw2DRect;
  begin
  	Canvas.Pen.Color := Bevel.Color;
    Canvas.Rectangle(l, t, r, b);

    with Rect do
    begin
    	Left := Left + 1;
    	Top := Top + 1;
    	Right := Right - 1;
    	Bottom := Bottom - 1;
    end;

  end;

	procedure DrawFlat;
  begin
  	Canvas.Pen.Color := clBtnShadow;
    Canvas.Polyline([Point(r, t), Point(l, t), Point(l, b)]);

  	Canvas.Pen.Color := clLight;
    Canvas.Polyline([Point(l, b), Point(r, b), Point(r, t)]);

    with Rect do
    begin
    	Left := Left + 1;
    	Top := Top + 1;
    	Right := Right - 1;
    	Bottom := Bottom - 1;
    end;

  end;

	procedure DrawIE4;
  begin

  	if Focused then
    begin

  		Canvas.Pen.Color := clBtnShadow;
    	Canvas.Polyline([Point(r, t), Point(l, t), Point(l, b)]);

  		Canvas.Pen.Color := clLight;
    	Canvas.Polyline([Point(l, b), Point(r, b), Point(r, t)]);

    end;

    with Rect do
    begin
    	Left := Left + 1;
    	Top := Top + 1;
    	Right := Right - 1;
    	Bottom := Bottom - 1;
    end;

  end;

begin
	clDarkShadow := DarkenColor(Bevel.Color, 120);
	clLightShade := Bevel.Color;
	clBtnShadow := DarkenColor(Bevel.Color, 50);
	clLight := BrightenColor(Bevel.Color, 64);
	with Rect do
  begin
  	l := Left;
    t := Top;
    r := Right;
		b := Bottom;
  end;
	case Bevel.BevelType of
  btWindows95: DrawWin95;
  btFlat3D: DrawFlat;
	btIE4Interactive: DrawIE4;
  bt2DRect: Draw2DRect;
  end;
end;

{$IFDEF DELPHI}
procedure RotateFont(AFont: TFont; Anglo: Integer);
var
	HFont: THandle;
  Weight: Integer;
begin
	if fsBold in AFont.Style then Weight := FW_BOLD	else Weight := FW_NORMAL;
	Hfont := CreateFont(AFont.Height, 0, Anglo, 0, Weight, Integer(fsItalic in AFont.Style),
  	Integer(fsUnderline in AFont.Style), Integer(fsStrikeout in AFont.Style), AFont.Charset,
    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH,
    PChar(AFont.Name));
	AFont.Handle := HFont;
end;
{$ENDIF}

procedure TDFBevel.SetType(Val: TBevelType);
begin
	if fType <> Val then
  begin
		fType := Val;
    Changed;
  end;
end;

procedure TDFBevel.SetSize(Val: TBevelSize);
begin
	if fSize <> Val then
  begin
		fSize := Val;
    Changed;
  end;
end;

procedure TDFBevel.SetColor(Val: TColor);
begin
	if fColor <> Val then
  begin
		fColor := Val;
    Changed;
  end;
end;

procedure TDFBevel.Assign( Value: TDFBevel );
begin
	Self.Color := Value.Color;
	Self.Size := Value.Size;
  Self.BevelType := Value.BevelType;
end;

procedure TDFBevel.Changed;
begin
	if Assigned(fOnChange) then fOnChange(Self);
end;

{ TDFControl }
constructor TDFControl.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fBackground := TBackgndObj.Create;
  fBackground.OnChange := BackChanged;
  fBevel := TDFBevel.Create;
  fBevel.Size := 2;
  fBevel.BevelType := btWindows95;
  fBevel.Color := clBtnFace;
  fBevel.OnChange := BevelChanged;
end;

destructor TDFControl.Destroy;
begin
  fBackground.Free;
  fBevel.Free;
	inherited Destroy;
end;

procedure TDFControl.SetBackground(Val: TBackgndObj);
begin
	if fBackground <> Val then
  begin
  	fBackground.Assign(Val);
    RepaintAll;
  end;
end;

procedure TDFControl.SetBevel(Val: TDFBevel);
begin
  if fBevel <> Val then
  begin
  	fBevel.Assign(Val);
  	RepaintAll;
  end;
end;

procedure TDFControl.BackChanged(Sender: TObject);
begin
	RepaintAll;
end;

procedure TDFControl.BevelChanged(Sender: TObject);
begin
	RepaintAll;
end;

procedure TDFControl.SetFocused(Val: Boolean);
begin
	if fFocused <> Val then
  begin
  	fFocused := Val;
    RepaintAll;
  end;
end;

procedure TDFControl.MsgMouseEnter(var Message: TMessage);
begin
  MouseEnter;
end;

procedure TDFControl.MsgMouseLeave(var Message: TMessage);
begin
	MouseLeave;
end;

procedure TDFControl.MouseEnter;
begin
	fMouseOver := True;
	if Assigned(fMouseEnter) then fMouseEnter(Self);
end;

procedure TDFControl.MouseLeave;
begin
	fMouseOver := False;
  if Assigned(fMouseLeave) then fMouseLeave(Self);
end;

procedure TDFControl.PaintBackup;
begin
	inherited PaintBackup;
	fPaintRect := ClientRect;
	PaintBevel(Backup.Canvas, fPaintRect, fBevel, fFocused);
  PaintBackObj(Backup.Canvas, fPaintRect, fBackground);
end;

{ DFScroll }
constructor TDFScroll.Create(AOwner: TComponent);
begin
	inherited Create(Aowner);
  fUp := TBitmap.Create;
  fDown := TBitmap.Create;
  fLeft := TBitmap.Create;
  fRight := TBitmap.Create;
  fThumb := TBitmap.Create;
  fMax := 100;
  fMin := 0;
  fPosition := 0;
  fLargeChange := 1;
  fSmallChange := 1;
  fGlyphs := 1;
  fCurrEvent := seNone;
  fThumbTrans := False;
  SetBounds(Left, Top, 160, 20);
end;

destructor TDFScroll.Destroy;
begin
	fUp.Free;
  fDown.Free;
  fLeft.Free;
  fRight.Free;
  fThumb.Free;
	inherited Destroy;
end;

procedure TDFScroll.SetUp(Val: TBitmap);
begin
	if fUp <> Val then
  begin
  	fUp.Assign(Val);
    if fOrientation = soVertical then RepaintAll;
  end
end;

procedure TDFScroll.SetDown(Val: TBitmap);
begin
	if fDown <> Val then
  begin
  	fDown.Assign(Val);
    if fOrientation = soVertical then RepaintAll;
  end
end;

procedure TDFScroll.SetLeft(Val: TBitmap);
begin
	if fLeft <> Val then
  begin
  	fLeft.Assign(Val);
    if fOrientation = soHorizontal then RepaintAll;
  end
end;

procedure TDFScroll.SetRight(Val: TBitmap);
begin
	if fRight <> Val then
  begin
  	fRight.Assign(Val);
    if fOrientation = soHorizontal then RepaintAll;
  end
end;

procedure TDFScroll.SetThumb(Val: TBitmap);
begin
	if fThumb <> Val then
  begin
  	fThumb.Assign(Val);
    RepaintAll;
  end
end;

procedure TDFScroll.SetMax(Val: Integer);
begin
	if fMax <> Val then
  begin
  	fMax := Val;
    if fPosition > fMax then fPosition := fMax;
    RepaintAll;
  end;
end;

procedure TDFScroll.SetMin(Val: Integer);
begin
	if fMin <> Val then
  begin
  	fMin := Val;
    if fPosition < fMin then fPosition := fMin;
    RepaintAll;
  end;
end;

procedure TDFScroll.SetPosition(Val: Integer);
begin
 	if fPosition <> Val then
  begin
  	fPosition := Val div fSmallChange * fSmallChange;
		if fPosition < fMin then fPosition := fMin;
		if fPosition > fMax then fPosition := fMax;
    RepaintAll;
  end;
  if Assigned(fOnChange) then fOnChange(Self);
end;

procedure TDFScroll.SetOrientation(Val: TDFScrollOrientation);
begin
	if fOrientation <> Val then
  begin
  	fOrientation := Val;
    RepaintAll;
  end;
end;

function TDFScroll.IncBox: TRect;
begin
	if fOrientation = soHorizontal then
  	Result := Rect(PaintRect.Right-fRight.Width div fGlyphs, PaintRect.Top, PaintRect.Right, PaintRect.Bottom)
  else
  	Result := Rect(PaintRect.Left, PaintRect.Bottom-fDown.Height, PaintRect.Right, PaintRect.Bottom);
end;

function TDFScroll.DecBox: TRect;
begin
	if fOrientation = soHorizontal then
  	Result := Rect(PaintRect.Left, PaintRect.Top, PaintRect.Left + fLeft.Width div fGlyphs, PaintRect.Bottom)
  else
  	Result := Rect(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Top + fUp.Height);
end;

function TDFScroll.ThumbBox: TRect;
var
	X, Y: Integer;
begin
	X := PosToPoint(fPosition).X;
	Y := PosToPoint(fPosition).Y;
	if fOrientation = soHorizontal then
  	Result := Rect(X, PaintRect.Top, X+fThumb.Width div fGlyphs, PaintRect.Bottom)
  else
  	Result := Rect(PaintRect.Left, Y, PaintRect.Right, Y+fThumb.Height);
end;

function TDFScroll.PosToPoint(Pos: Integer): TPoint;
var
	x, y : Integer;
begin
	x := PaintRect.Left; y := PaintRect.Top;
	if fOrientation = soHorizontal then
		X := Round((Pos-fMin)*((IncBox.Left-DecBox.Right-fThumb.Width div fGlyphs)/(fMax-fMin)))+DecBox.Right
  else
		Y := Round((Pos-fMin)*((IncBox.Top-DecBox.Bottom-fThumb.Height)/(fMax-fMin)))+DecBox.Bottom;
  Result := Point(X, Y);
end;

function TDFScroll.PointToPos(Point: TPoint): Integer;
var
	x, y : Integer;
begin
	x := Point.x;
  y := Point.y;
	if fOrientation = soHorizontal then
		Result := Trunc((X - DecBox.Right)/((IncBox.Left-DecBox.Right-fThumb.Width div fGlyphs)/(fMax-fMin)))
  else
		Result := Trunc((Y - DecBox.Bottom)/((IncBox.Top-DecBox.Bottom-fThumb.Height)/(fMax-fMin)));
end;

procedure TDFScroll.PaintBackup;
begin
  inherited PaintBackup;
	if fOrientation = soHorizontal then
	begin

		if not IsBitmapEmpty(fLeft) then
  	if (fCurrEvent = seDecClick) and (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(DecBox, GetGlyph(fLeft, 1))
    else if (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(DecBox, GetGlyph(fLeft, 0))
    else if fGlyphs < 2 then
	  	Backup.Canvas.StretchDraw(DecBox, fLeft);

		if not IsBitmapEmpty(fRight) then
   	if (fCurrEvent = seIncClick) and (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(IncBox, GetGlyph(fRight, 1))
    else if (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(IncBox, GetGlyph(fRight, 0))
    else  if fGlyphs < 2 then
	  	Backup.Canvas.StretchDraw(IncBox, fRight);

  end
  else
  begin

		if not IsBitmapEmpty(fUp) then
  	if (fCurrEvent = seDecClick) and (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(DecBox, GetGlyph(fUp, 1))
    else if (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(DecBox, GetGlyph(fUp, 0))
    else if fGlyphs < 2 then
	  	Backup.Canvas.StretchDraw(DecBox, fUp);

		if not IsBitmapEmpty(fDown) then
  	if (fCurrEvent = seIncClick) and (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(IncBox, GetGlyph(fDown, 1))
    else if (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(IncBox, GetGlyph(fDown, 0))
    else if fGlyphs < 2 then
	  	Backup.Canvas.StretchDraw(IncBox, fDown);

  end;

	if not IsBitmapEmpty(fThumb) then
  if fThumbTrans then
  begin
  	if (fCurrEvent = seDragThumb) and (fGlyphs >= 2) then
	  	TransBlt(Backup.Canvas, GetGlyph(fThumb, 1), ThumbBox.Left, ThumbBox.Top)
    else if (fGlyphs >= 2) then
	  	TransBlt(Backup.Canvas, GetGlyph(fThumb, 0), ThumbBox.Left, ThumbBox.Top)
    else if fGlyphs < 2 then
	  	TransBlt(Backup.Canvas, fThumb, ThumbBox.Left, ThumbBox.Top);
  end
  else
  begin
  	if (fCurrEvent = seDragThumb) and (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(ThumbBox, GetGlyph(fThumb, 1))
    else if (fGlyphs >= 2) then
	  	Backup.Canvas.StretchDraw(ThumbBox, GetGlyph(fThumb, 0))
    else if fGlyphs < 2 then
	  	Backup.Canvas.StretchDraw(ThumbBox, fThumb);
  end;

end;

procedure TDFScroll.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
	fCurrEvent := seNone;
	if Button = mbLeft then
  begin
  	if PtInRect(ThumbBox, Point(X, Y)) then
		begin
    	fMouseDown := True;
      fThumbDrag := Point(X-ThumbBox.Left, Y-ThumbBox.Top);
      fCurrEvent := seDragThumb;
    end
    else if PtInRect(DecBox, Point(X, Y)) then
		begin
      fCurrEvent := seDecClick;
  		Decrement;
    end
    else if PtInRect(IncBox, Point(X, Y)) then
		begin
      fCurrEvent := seIncClick;
  		Increment;
    end
    else if PtInRect(PaintRect, Point(X, Y)) then
    begin
    	if Orientation = soHorizontal then
      begin
      	if X < ThumbBox.Left then
				begin
		      fCurrEvent := sePageUp;
  				PageUp;
    		end
    		else if X > ThumbBox.Right then
				begin
		      fCurrEvent := sePageDown;
  				PageDown;
    		end;
      end
      else
      begin
      	if Y < ThumbBox.Top then
				begin
		      fCurrEvent := sePageUp;
  				PageUp;
    		end
    		else if Y > ThumbBox.Bottom then
				begin
		      fCurrEvent := sePageDown;
  				PageDown;          
    		end;
      end;
    end;
  end;
  if fCurrEvent <> seNone then
  begin
	  fTimer.OnTimer := OnTimer;
  	fTimer.Enabled := True;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TDFScroll.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	if fMouseDown then
  begin
  	Position := PointToPos(Point(X-fThumbDrag.X, Y-fThumbDrag.Y));
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TDFScroll.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
	if Button = mbLeft then
	begin
  	fMouseDown := False;
    fCurrEvent := seNone;
    RepaintAll;
  end;
  fTimer.Enabled := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TDFScroll.PageUp;
begin
	Position := Position - fLargeChange;
end;

procedure TDFScroll.PageDown;
begin
	Position := Position + fLargeChange;
end;

procedure TDFScroll.Increment;
begin
	Position := Position + fSmallChange;
end;

procedure TDFScroll.Decrement;
begin
	Position := Position - fSmallChange;
end;

procedure TDFScroll.ScrollBy(X, Y: Integer);
begin
	Position := PointToPos(Point(X, Y));
end;

procedure TDFScroll.OnTimer(Sender: TObject);
var
	P: TPoint;
begin
	GetCursorPos(P);
  P := Self.ScreenToClient(P);
  MouseDown(mbLeft, [], P.X, P.Y);
end;

procedure TDFScroll.SetGlyphs(Val: Byte);
begin
	if fGlyphs <> Val then
  begin
  	fGlyphs := Val;
    RepaintAll;
  end;
end;

procedure TDFScroll.SetThumbTrans(Val: Boolean);
begin
	if fThumbTrans <> Val then
  begin
  	fThumbTrans := Val;
    RepaintAll;
  end;
end;


{$R Apples.RES}
{--------------------------------------}
{ TApples.Create                       }
{--------------------------------------}
constructor TApples.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApple := TBitmap.Create;
  FApple.LoadFromResourceName(HINSTANCE, 'APPLE');
  FTransparentColor := FApple.Canvas.Pixels[0, 0];
  SetBounds(Left, Top, 80, 25);
end;

{--------------------------------------}
{ TApples.Destroy                      }
{--------------------------------------}
destructor TApples.Destroy;
begin
  FApple.Free;
  inherited Destroy;
end;

{--------------------------------------}
{ TApples.Paint                        }
{--------------------------------------}
procedure TApples.PaintBackup;
var
	CX, CY : Integer;
  i : Real;
  Offset : Integer;
  R : TRect;
  B : TBitmap;
begin
	inherited PaintBackup;
  Offset := 2;
	CX := PaintRect.Left+Offset; CY := PaintRect.Top+Offset;
  if FApple = nil then Exit;
  i := FValue;
	while i >= 1 do
  begin
    TransparentBlt(Backup.Canvas, FApple, Cx, Cy, fTransparentColor);
   	CX := CX + FApple.Width + Offset;
    if CX + FApple.Width >= PaintRect.Right then
    begin
    	if FLineWrap then
      begin
      	CY := CY + FApple.Height + Offset;
        CX := PaintRect.Left+Offset;
      end
      else
      	Exit;
    end;
    i := i - 1;
  end;

  if (i > 0) then
  begin
    B := TBitmap.Create;
    B.Width := FApple.Width; B.Height := FApple.Height;
    B.Canvas.CopyRect(Rect(0, 0, Round(B.Width * i), B.Height), FApple.Canvas,
			Rect(0, 0, Round(B.Width * i), B.Height));
    TransparentBlt(Backup.Canvas, B, Cx, Cy, fTransparentColor);
    B.Free;
  end;

end;

{--------------------------------------}
{ TApples.SetApple                     }
{--------------------------------------}
procedure TApples.SetApple(Val: TBitmap);
begin
	if FApple <> Val then
  begin
  	FApple.Assign(Val);
    if Val <> nil then
			FTransparentColor := FApple.Canvas.Pixels[0,0];
    RepaintAll;
  end;
end;

{--------------------------------------}
{ TApples.SetLineWrap                  }
{--------------------------------------}
procedure TApples.SetLineWrap(Val: Boolean);
begin
	if FLineWrap <> Val then
  begin
  	FLineWrap := Val;
    RepaintAll;
  end;
end;


{--------------------------------------}
{ TApples.SetTransparentColor          }
{--------------------------------------}
procedure TApples.SetTransparentColor(Val: TColor);
begin
	if FTransparentColor <> Val then
  begin
  	FTransparentColor := Val;
    RepaintAll;
  end;
end;

{--------------------------------------}
{ TApples.SetValue                     }
{--------------------------------------}
procedure TApples.SetValue(Val: Extended);
begin
	if FValue <> Val then
  begin
  	FValue := Val;
    RepaintAll;
  end;
end;


//-------------------------------------
{ TBCBITMAP }
//-------------------------------------
constructor TBCBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TBitmap.Create;
  FSmoothed := TBitmap.Create;
	FAutosize := False;
  Width := 64;
  Height := 64;
  FColorOption := coNone;
  FStyle := dsNone;
  FMouseDown := False;
end;

destructor TBCBitmap.Destroy;
begin
  FPicture.Free;
  FSmoothed.Free;
  inherited Destroy;
end;

{--------------------------------------}
{ TBCBitmap.Paint                      }
{--------------------------------------}
procedure TBCBitmap.PaintBackup;
var
	R : TRect;
  FTrans: TColor;
  HF : Integer;
  Tmp: TBItmap;
begin

inherited PaintBackup;

with Backup.Canvas do
begin
//  Backup.Canvas.CopyRect(PaintRect, Canvas, PaintRect);
  tmp := TBitmap.Create;
  tmp.Width := fPicture.Width;
  tmp.Height := fPicture.Height;
  tmp.Canvas.Draw(0, 0, fPicture);
  if FInvert and FMouseDown then
  begin
  	fTrans := fPicture.TransparentColor;
    tmp.Canvas.Pen.Mode := pmNot;
    tmp.Canvas.FillRect(Rect(0, 0, tmp.Width, tmp.Height));
    tmp.Canvas.Pen.Mode := pmCopy;
  end
  else
  	FTrans := fPicture.TransparentColor;;
		case FStyle of
  		dsNone : 			R := Rect(PaintRect.Left, PaintRect.Top,
      	Min(PaintRect.Left+tmp.Width, PaintRect.Right), Min(tmp.Height+PaintRect.Top, PaintRect.Bottom));
			dsClient :	 	R := PaintRect;
			dsCentered : 	R := Bounds(Round((Width - tmp.Width) / 2) ,
  			Round((Height - tmp.Height) / 2), tmp.Width, tmp.Height);
  	end;
		case FColorOption of
  		coTransparent : TransBlt(Backup.Canvas, Tmp, 0, 0);
    	coSmoothed: SmoothBlt(Backup.Canvas, R.Left, R.Top, fPicture, FTrans);
    else
  		Backup.Canvas.CopyRect(R , tmp.Canvas, Rect(0, 0, tmp.Width, tmp.Height));
  	end;
  	tmp.Free;
  end;

end;

procedure TBCBitmap.SetAutosize;
begin
	if FAutosize <> Val then
  begin
  	FAutosize := Val;
    if (FPicture <> nil) and FAutosize then
    	SetBounds(Left, Top, PaintRect.Left + FPicture.Width + (Width-PaintRect.Right),
      	PaintRect.Top + FPicture.Height + (Height-PaintRect.Bottom));
    RepaintAll;
  end;
end;

procedure TBCBitmap.SetPicture;
begin
	if fPicture <> Val then
  begin
		FPicture.Assign(Val);
   	if (FPicture <> nil) and FAutosize then
    	SetBounds(Left, Top, PaintRect.Left + FPicture.Width + (Width-PaintRect.Right),
      	PaintRect.Top + FPicture.Height + (Height-PaintRect.Bottom));

		RepaintAll;
  end;
end;

procedure TBCBitmap.SetStyle;
begin
	if FStyle <> Val then
  begin
  	FStyle := Val;
    RepaintAll;
  end;
end;

procedure TBCBitmap.SetColorOption (Val : TColorOption);
begin
	if Val <> FColorOption then
  begin
  	FColorOption := Val;
		RepaintAll;
  end;
end;

procedure TBCBitmap.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FInvert then
		if Button = mbLeft then
		begin
    	FMouseDown := True;
      RepaintAll;
  	end;
end;

procedure TBCBitmap.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FInvert then
		if Button = mbLeft then
		begin
    	FMouseDown := False;
      RepaintAll;
  	end;
end;

initialization
	fTimer := TTimer.Create(nil);
  fTimer.Interval := 100;
	Tmp := TBitmap.Create;

finalization
	fTimer.Free;
  Tmp.Free;
end.
