
{  ---------------------------------------------------------
   GradientFill Component, written by William Yang.
   yang@btinternet.com    http://www.btinternet.com/~yang/
   ---------------------------------------------------------  }

unit Gradient;

interface

uses Windows, SysUtils, Classes, Controls, Messages, Graphics, ColorMan, DrawMan;

type

	TGradientStyle=(gsLine, gsEllipse, gsRect, gsCustomRect);
	TColorCount=1..4;
	TDirection=(diHorizonal, diVertical);
	TDrawRectEvent = procedure (Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;

	TGradient=class(TPersistent)
	private
		{ Private declarations }
		fGradientStyle: TGradientStyle;
		fLineSize: Integer;
		fColor1: TColor;
		fColor2: TColor;
		fColor3: TColor;
		fColor4: TColor;
		fColor5: TColor;
		fColorCount: TColorCount;
    fOnChange: TNotifyEvent;
    fLineDirection: TDirection;
    fOnDrawRect: TDrawRectEvent;

		procedure SetGradientStyle( Value: TGradientStyle );
		procedure SetLineSize( Value: Integer );
		procedure SetColor1( Value: TColor );
		procedure SetColor2( Value: TColor );
		procedure SetColor3( Value: TColor );
		procedure SetColor4( Value: TColor );
		procedure SetColor5( Value: TColor );
		procedure SetColorCount( Value: TColorCount );
    procedure SetLineDir(Value: TDirection);
	protected
		{ Protected declarations }
	public
		{ Public declarations }
    constructor Create;

    procedure Assign ( Value: TGradient );
    procedure Changed;
	published
		{ Published declarations }
		property GradientStyle: TGradientStyle read fGradientStyle write SetGradientStyle;
		property LineSize: Integer read fLineSize write SetLineSize;
		property Color1: TColor read fColor1 write SetColor1;
		property Color2: TColor read fColor2 write SetColor2;
		property Color3: TColor read fColor3 write SetColor3;
		property Color4: TColor read fColor4 write SetColor4;
		property Color5: TColor read fColor5 write SetColor5;
		property ColorCount: TColorCount read fColorCount write SetColorCount;
    property LineDirection: TDirection read fLineDirection write SetLineDir;

    property OnDrawRect: TDrawRectEvent read fOnDrawRect write fOnDrawRect;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;


	TDfGradient =class(TGraphicControl)
	private
		{ Private declarations }
		fGradient: TGradient;
    fTmp: TBitmap;
		procedure SetGradient(Value: TGradient);
    procedure CreateOnTmp;
    procedure GradChanged(Sender: TOBject);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
	protected
		{ Protected declarations }
		procedure Paint ; override;

	public
		{ Public declarations }
		constructor Create ( AOwner: TComponent ); override;
		destructor Destroy ; override;

	published
		{ Published declarations }
		property Gradient: TGradient read fGradient write SetGradient;

    property Color;
    property Align;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
 	end;


procedure LineGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; Direction: TDirection; LineSize: Integer);
procedure EllipseGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; LineSize: Integer);
procedure RectGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; LineSize: Integer);
procedure PaintGradient(Canvas: TCanvas; Rect: TRect; Gradient : TGradient);
procedure CustomGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; LineSize: Integer; OnDraw: TDrawRectEvent);
procedure FastLineGrad(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; Direction: TDirection; LineSize: Integer);


implementation

{ TDfGradient }
procedure TDfGradient.Paint ;
var
	g: Integer;
begin
	g := Gradient.LineSize;
	if (fTmp.Width <> Width) or (fTmp.Height <> Height) then
  begin
    CreateOnTmp;
  end;
  with Canvas do
  BitBlt(Canvas.Handle, ClipRect.Left, ClipRect.Top, ClipRect.Right-ClipRect.Left, ClipRect.Bottom-ClipRect.Top,
  	fTmp.Canvas.Handle, ClipRect.Left, ClipRect.Top, SRCCOPY);
end;

constructor TDfGradient.Create ( AOwner: TComponent );
begin
  fTmp := TBitmap.Create;
	fGradient := TGradient.Create;
  with fGradient do
  begin
  	Linesize := 2;
  	Color1 := clBlack;
  	Color2 := clWhite;
  	Color3 := clBlack;
  	Color4 := clWhite;
  	Color5 := clBlack;
  	ColorCount := 4;
  	GradientStyle := gsLine;
  	LineDirection := diHorizonal;
    OnChange := GradChanged;
  end;
	inherited Create( AOwner );
  Width := 144; Height := 144;
end;

destructor TDfGradient.Destroy ;
begin
	fGradient.Free;
  if fTmp <> nil then fTmp.Free;
	inherited Destroy;
end;

procedure TDfGradient.SetGradient( Value: TGradient );
begin
	if fGradient <> Value then
	begin
		fGradient.Assign(Value);
	end;
end;

procedure TDfGradient.GradChanged(Sender: TOBject);
begin
	CreateOnTmp;
  Invalidate;
end;

procedure TDfGradient.CreateOnTmp;
begin
 	fTmp.Width := Width;
	fTmp.Height := Height;
	fTmp.Canvas.Brush.Color := Color;
  fTmp.Canvas.FillRect(Rect(0,0,fTmp.Width, fTmp.Height));
  PaintGradient(fTmp.Canvas, ClientRect, fGradient);
end;

procedure TDfGradient.CMColorChanged(var Message: TMessage);
begin
	inherited;
  CreateOnTmp
end;

procedure PaintGradient(Canvas: TCanvas; Rect: TRect; Gradient : TGradient);
var
	Colors: array[1..5] of TColor;
begin
	with Gradient do
  begin

	if (Rect.Right = 0) or (Rect.Bottom = 0) then Exit;
	Colors[1] := Color1;
	Colors[2] := Color2;
	Colors[3] := Color3;
	Colors[4] := Color4;
	Colors[5] := Color5;
  case GradientStyle of
  	gsLine: FastLineGrad(Canvas, Rect, Colors, ColorCount, LineDirection, LineSize);
  	gsEllipse: EllipseGradient(Canvas, Rect, Colors, ColorCount, LineSize);
  	gsRect: RectGradient(Canvas, Rect, Colors, ColorCount, LineSize);
    gsCustomRect:
    begin
    	if Assigned(OnDrawRect) then
    		CustomGradient(Canvas, Rect, Colors, ColorCount, LineSize, OnDrawRect)
    	else
        RectGradient(Canvas, Rect, Colors, ColorCount, LineSize);
    end;
  end;

  end;
end;

procedure EllipseGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; LineSize: Integer);
var R,G,B : Real;
  	R1,G1,B1,
  	R2,G2,B2 : Integer;
  	Color1, Color2: TColor;
  	i, X1, X2, Y1, Y2, Wide, CWide, Finish, Cx, Cy : Integer;
begin
	if (Rect.Right = 0) or (Rect.Bottom = 0) then Exit;
	i := Low(Colors);
	X1 := Rect.Left;
  Y1 := Rect.Top;
  X2 := Rect.Right;
  Y2 := Rect.Bottom;
  if (Y2-Y1)>(X2-X1) then
  begin
  	Finish := Y2;
  	Wide := (Y2 - Y1) div ColorCount div 2;
  	CWide := Wide;
  end
  else
  begin
  	Finish := X2;
  	Wide := (X2 - X1) div ColorCount div 2;
  	CWide := Wide;
  end;

  if ColorCount <= 1 then
  begin
  	Color1 := Colors[i];
    Color2 := Color1;
  end
  else
  begin
  	Color1 := Colors[i];
    Color2 := Colors[i+1];
  end;
	CWide := Wide;
  Inc(i);
  DeColor(Color1, R1, G1, B1);
  DeColor(Color2, R2, G2, B2);
  R := R1; G := G1; B := B1;
  Cx := X1; Cy := Y1;
  with Canvas do
  begin
    Pen.Width := 0;
    Pen.Style := psClear;
    while (Cx < X2 div 2) and (Cy < Y2 div 2) do
    begin
      Brush.Color := ERGB(R, G, B);
      Ellipse(Cx, Cy, X2-Cx, Y2-Cy);

      Inc(Cx, LineSize); Inc(Cy, LineSize);
    	if Cy >= CWide then
    	begin
      	Inc(CWide, Wide);
      	Inc(i);
      	if i > Low(Colors) + ColorCount then Dec(i);
      	Color1 := Color2;
				Color2 := Colors[i];
  			DeColor(Color1, R1, G1, B1);
  			DeColor(Color2, R2, G2, B2);
    	end;
   		R := R + (R2 - R1) / Wide * LineSize;
   		G := G + (G2 - G1) / Wide * LineSize;
  		B := B + (B2 - B1) / Wide * LineSize;
    end;
  end;
end;

procedure CustomGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; LineSize: Integer; OnDraw: TDrawRectEvent);
var R,G,B : Real;
  	R1,G1,B1,
  	R2,G2,B2 : Integer;
  	Color1, Color2: TColor;
  	i, X1, X2, Y1, Y2, Wide, CWide, Finish, Cx, Cy : Integer;
begin
	if not Assigned(OnDraw) then Exit;
	if (Rect.Right = 0) or (Rect.Bottom = 0) then Exit;
	i := Low(Colors);
	X1 := Rect.Left;
  Y1 := Rect.Top;
  X2 := Rect.Right;
  Y2 := Rect.Bottom;
  if (Y2-Y1)>(X2-X1) then
  begin
  	Finish := Y2;
  	Wide := (Y2 - Y1) div ColorCount div 2;
  	CWide := Wide;
  end
  else
  begin
  	Finish := X2;
  	Wide := (X2 - X1) div ColorCount div 2;
  	CWide := Wide;
  end;

  if ColorCount <= 1 then
  begin
  	Color1 := Colors[i];
    Color2 := Color1;
  end
  else
  begin
  	Color1 := Colors[i];
    Color2 := Colors[i+1];
  end;
	CWide := Wide;
  Inc(i);
  DeColor(Color1, R1, G1, B1);
  DeColor(Color2, R2, G2, B2);
  R := R1; G := G1; B := B1;
  Cx := X1; Cy := Y1;
  with Canvas do
  begin
    Pen.Width := 0;
    Pen.Style := psClear;
    while (Cx < X2 div 2) and (Cy < Y2 div 2) do
    begin
      Brush.Color := ERGB(R, G, B);
      OnDraw(nil, Canvas, Classes.Bounds(Cx, Cy, X2-Cx, Y2-Cy));
      Inc(Cx, LineSize); Inc(Cy, LineSize);
    	if Cy >= CWide then
    	begin
      	Inc(CWide, Wide);
      	Inc(i);
      	if i > Low(Colors) + ColorCount then Dec(i);
      	Color1 := Color2;
				Color2 := Colors[i];
  			DeColor(Color1, R1, G1, B1);
  			DeColor(Color2, R2, G2, B2);
    	end;
   		R := R + (R2 - R1) / Wide * LineSize;
   		G := G + (G2 - G1) / Wide * LineSize;
  		B := B + (B2 - B1) / Wide * LineSize;
    end;
  end;
end;

procedure RectGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; LineSize: Integer);
var R,G,B : Real;
  	R1,G1,B1,
  	R2,G2,B2 : Integer;
  	Color1, Color2: TColor;
  	i, X1, X2, Y1, Y2, Wide, CWide, Finish, Cx, Cy : Integer;
begin
	if (Rect.Right = 0) or (Rect.Bottom = 0) then Exit;
	i := Low(Colors);
	X1 := Rect.Left;
  Y1 := Rect.Top;
  X2 := Rect.Right;
  Y2 := Rect.Bottom;

  if (Y2-Y1)>(X2-X1) then
  begin
  	Finish := Y2;
  	Wide := (Y2 - Y1) div ColorCount div 2;
  	CWide := Wide;
  end
  else
  begin
  	Finish := X2;
  	Wide := (X2 - X1) div ColorCount div 2;
  	CWide := Wide;
  end;

  if ColorCount <= 1 then
  begin
  	Color1 := Colors[i];
    Color2 := Color1;
  end
  else
  begin
  	Color1 := Colors[i];
    Color2 := Colors[i+1];
  end;
	CWide := Wide;
  Inc(i);
  DeColor(Color1, R1, G1, B1);
  DeColor(Color2, R2, G2, B2);
  R := R1; G := G1; B := B1;
  Cx := X1; Cy := Y1;
  with Canvas do
  begin
    while (Cx < (X2 div 2)) and (Cy < (Y2 div 2)) do
    begin
      Brush.Color := ERGB(R, G, B);
      FillRect(Classes.Bounds(Cx, Cy, X2-Cx, Y2-Cy));

      Inc(Cx, LineSize); Inc(Cy, LineSize);
    	if Cy >= CWide then
    	begin
      	Inc(CWide, Wide);
      	Inc(i);
      	if i > Low(Colors) + ColorCount then Dec(i);
      	Color1 := Color2;
				Color2 := Colors[i];
  			DeColor(Color1, R1, G1, B1);
  			DeColor(Color2, R2, G2, B2);
    	end;
   		R := R + (R2 - R1) / Wide * LineSize;
   		G := G + (G2 - G1) / Wide * LineSize;
  		B := B + (B2 - B1) / Wide * LineSize;
    end;
  end;
end;

procedure FastLineGrad(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; Direction: TDirection; LineSize: Integer);
var
	tmp: TBitmap;
begin
	//Paint the gradient fill as a bitmap pattern
	tmp := TBitmap.Create;
  if Direction = diVertical then
  begin
  	tmp.Width := Rect.Right-Rect.Left;
  	tmp.Height := 8;
  end
  else if Direction = diHorizonal then
  begin
  	tmp.Width := 8;
  	tmp.Height := Rect.Bottom-Rect.Top;
  end;
  LineGradient(tmp.Canvas, Classes.Rect(0,0,tmp.Width,Tmp.Height), Colors,
  	ColorCount, Direction, LineSize);
	MultiPaint(Canvas, tmp, Rect);
end;

procedure LineGradient(Canvas: TCanvas; Rect: TRect; Colors: array of TColor;
	ColorCount: Integer; Direction: TDirection; LineSize: Integer);
var R,G,B : Real;
  	R1,G1,B1,
  	R2,G2,B2 : Integer;
  	Color1, Color2: TColor;
  	i, X1, X2, Y1, Y2, Wide, CWide, Finish, Cp : Integer;
  	CR: TRect;
begin
	if (Rect.Right = 0) or (Rect.Bottom = 0) then Exit;
  if (LineSize <= 0) then LineSize := 1;
  if (ColorCount <= 0) then ColorCount := 3;

	i := Low(Colors);
	X1 := Rect.Left;
  Y1 := Rect.Top;
  X2 := Rect.Right;
  Y2 := Rect.Bottom;

  if Direction = diHorizonal then
  begin
	  Finish := Y2;
  	Wide := (Y2 - Y1) div ColorCount;
    Cp := Y1;
  end
  else if Direction = diVertical then
  begin
  	Wide := (X2 - X1) div ColorCount;
    Finish := X2;
    Cp := X1;
  end;

  if ColorCount <= 1 then
  begin
  	Color1 := Colors[i];
    Color2 := Color1;
  end
  else
  begin
  	Color1 := Colors[i];
    Color2 := Colors[i+1];
  end;
	CWide := Wide;
  Inc(i);
  DeColor(Color1, R1, G1, B1);
  DeColor(Color2, R2, G2, B2);

	R := R1;
 	G := G1;
 	B := B1;

	while CP <= Finish do
  begin

  	case Direction of
    	diHorizonal:
    	begin
      	CR := Classes.Rect(X1, Cp, X2, Cp+LineSize);
    	end;
  		diVertical:
    	begin
      	CR := Classes.Rect(Cp, Y1, Cp+LineSize, Y2);
    	end
    end;

	  Canvas.Brush.Color := ERGB(R, G, B);
		Canvas.FillRect(CR);

  	Inc(Cp, LineSize);
    if Cp >= CWide then
    begin
      Inc(CWide, Wide);
      Inc(i);
      if i > Low(Colors) + ColorCount then	Dec(i);
      Color1 := Color2;
			Color2 := Colors[i];
  		DeColor(Color1, R1, G1, B1);
  		DeColor(Color2, R2, G2, B2);

    end;

   	R := R + (R2 - R1) / Wide * LineSize;
   	G := G + (G2 - G1) / Wide * LineSize;
  	B := B + (B2 - B1) / Wide * LineSize;

  end;
end;


{ TGradient }
constructor TGradient.Create;
begin
	inherited Create;
  fGradientStyle := gsLine;
  fLineSize := 3;
  fColor1 := clGray;
  fColor2 := clWhite;
  fColor3 := clSilver;
  fColor4 := clWhite;
  fColor5 := clBlack;
  fColorCount := 3;
  fLineDirection := diHorizonal;
end;

procedure TGradient.Assign ( Value: TGradient );
begin
  fGradientStyle := Value.GradientStyle;
  fLineSize := Value.LineSize;
  fColor1 := Value.Color1;
  fColor2 := Value.Color2;
  fColor3 := Value.Color3;
  fColor4 := Value.Color4;
  fColor5 := Value.Color5;
  fColorCount := Value.ColorCount;
  fLineDirection := Value.LineDirection;
  Changed;
end;

procedure TGradient.Changed ;
begin
	if Assigned(fOnChange) then fOnChange(Self);
end;

procedure TGradient.SetGradientStyle( Value: TGradientStyle );
begin
	if fGradientStyle <> Value then
	begin
		fGradientStyle := Value;
		Changed;
	end;
end;

procedure TGradient.SetLineDir(Value: TDirection);
begin
  if fLineDirection <> Value then
  begin
  	fLineDirection := Value;
    Changed;
  end;
end;

procedure TGradient.SetLineSize( Value: Integer );
begin
	if fLineSize <> Value then
	begin
		fLineSize := Value;
		Changed;
	end;
end;

procedure TGradient.SetColor1( Value: TColor );
begin
	if fColor1 <> Value then
	begin
		fColor1 := Value;
		Changed;
	end;
end;

procedure TGradient.SetColor2( Value: TColor );
begin
	if fColor2 <> Value then
	begin
		fColor2 := Value;
		Changed;
	end;
end;

procedure TGradient.SetColor3( Value: TColor );
begin
	if fColor3 <> Value then
	begin
		fColor3 := Value;
		Changed;
	end;
end;

procedure TGradient.SetColor4( Value: TColor );
begin
	if fColor4 <> Value then
	begin
		fColor4 := Value;
		Changed;
	end;
end;

procedure TGradient.SetColor5( Value: TColor );
begin
	if fColor5 <> Value then
	begin
		fColor5 := Value;
		Changed;
	end;
end;

procedure TGradient.SetColorCount( Value: TColorCount );
begin
	if fColorCount <> Value then
	begin
		fColorCount := Value;
		Changed;
	end;
end;


end.
