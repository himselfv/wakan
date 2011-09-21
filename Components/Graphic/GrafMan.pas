
{ ---------------------------------------------------
		Graphics Manager 	Copyright (r) by DreamFactory
    Version : 1.75   	Author : William Yang
		Last Update 			09 - Sep - 97
  --------------------------------------------------- }

unit GrafMan;

interface

uses Windows, SysUtils, Graphics, Classes, ColorMan, ExtCtrls, Forms,
	Drawman, NumMan, VBitmap;

{ *************************************************************************
	There is a small bug in this unit.
	Becase I used a internal variable for each function that returns
	a TBitmap object, which means if you are going to add another function
	you must not cross use this variable.
	I mean you cannot use in one of the function and not yet wait until it is finished
	call another function in this unit.
  _________________________________________________________________________ }

function ColoriseImage(Bmp : TBitmap; BaseColor : TColor) : TBitmap;
function MergeBMP(Pic1, Pic2 : TBitmap) : TBitmap;
function SoftenBMP(Src : TBitmap; Deep : Single) : TBitmap;
function GreyBMP(Src : TBitmap) : TBitmap;
function BWOnlyBMP(Src : TBitmap) : TBitmap;
function DarkenBMP(Bmp : TBitmap; Grade : Integer) : TBitmap;
function BrightenBMP(Bmp : TBitmap; Grade : Integer) : TBitmap;
function MakeButton(Src : TBitmap; Border : Integer; Soft : Boolean) : TBitmap;
function MergeBMPExt(Pic1, Pic2 : TBitmap; Grade: Byte) : TBitmap;
function BMPFromRes(ResName: String) : TBitmap;
function ResizeBMP(Src: TBitmap; Width, Height: Integer): TBitmap;
function LoadBMPFile(Filename: String): TBitmap;
function DisabledBmp(Src: TBitmap; DisabledColor, FaceColor: TColor): TBitmap;
function CreatePattern(Color1, Color2: TColor ): TBitmap;
function BmpToLCD(Src: TBitmap; ForeColor, Backcolor: TColor): TBitmap;

implementation

var
	TmpMem : TBitmap;

function CreatePattern(Color1, Color2: TColor ): TBitmap;
var
  X, Y: Integer;
begin
  TmpMem.Width := 8;
  TmpMem.Height := 8;
  with TmpMem.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color1;
    FillRect(Rect(0, 0, TmpMem.Width, TmpMem.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
          Pixels[X, Y] := Color2;    { on even/odd rows }
  end;
  Result := TmpMem;
end;

function BmpToLCD(Src: TBitmap; ForeColor, Backcolor: TColor): TBitmap;
var
	x, y: Integer;
begin
  TmpMem.Width := Src.Width*4;
  TmpMem.Height := Src.Height*4;
  with TmpMem.Canvas do
  begin
  	Brush.Color := BackColor;
    Brush.Style := bsSolid;
  	FillRect(Rect(0, 0, TmpMem.Width, TmpMem.Height));
    Brush.Color := ForeColor;
    Pen.Color := MergeColor(ForeColor, BackColor);
		for x := 0 to Src.Width do
			for y := 0 to Src.Height do
      begin
      	if not IsLightColor(Src.Canvas.Pixels[x, y]) then
        begin
					Rectangle(x*3+1, y*3+1, x*3+3, y*3+3);  
        end
      end;
  end;
  Result := TmpMem;
end;

function LoadBMPFile(Filename: String): TBitmap;
begin
	TmpMem.LoadFromFile(Filename);
	Result := TmpMem;
end;

function DisabledBmp(Src: TBitmap; DisabledColor, FaceColor: TColor): TBitmap;
var
	i, j : Integer;
  Pattern: TBitmap;
	x, y : Integer;
begin
	Pattern := TBitmap.Create;
  Pattern := CreatePattern(DisabledColor, FaceColor);
	TmpMem.Assign(Src);

  if (Src.Width = 0) or (Src.Height=0) then Exit;
	x := 0; y := 0;
	while (y < Src.Height) and (x < Src.Width) do
  begin
    TransparentBlt(TmpMem.Canvas, Pattern, x, y, FaceColor);
    Inc(x, 8);
    if x >= Src.Width then
    begin
      x := 0;
      Inc(y, 8);
    end;
  end;

  Pattern.Free;
end;

function ResizeBMP(Src: TBitmap; Width, Height: Integer): TBitmap;
var
	DR, SR: TRect;
begin
	DR := Rect(0, 0, Width, Height);
  SR := Rect(0, 0, Src.Width, Src.Height);
  TmpMem.Width := Width;
  TmpMem.Height := Height;
  TmpMem.Canvas.CopyRect(DR, Src.Canvas, SR);
  Result := TmpMem;
end;

function BMPFromRes(ResName: String) : TBitmap;
begin
	TmpMem.LoadFromResourceName(HINSTANCE, PChar(UpperCase(ResName)));
	Result := TmpMem;
end;

function BrightenBMP(Bmp : TBitmap; Grade : Integer) : TBitmap;
var
	VB: TVirtualBitmap;
  i, j : Integer;
begin
	VB := TVirtualBitmap.CreateFromBitmap(Bmp);
	if VB.Bits <= 8 then
  begin
    for i := 0 to VB.Colors - 1 do
    begin
    	VB.ColorTable[i] := BrightenColor(VB.ColorTable[i], Grade);
    end;
  end
  else
  begin
  for i := 0 to VB.Width-1 do
  	for j := 0 to VB.Height-1 do
    begin
      VB.SetPixel(i, j, BrightenColor(VB.GetPixel(i, j), Grade));
    end;
  end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
end;

function DarkenBMP(Bmp : TBitmap; Grade : Integer) : TBitmap;
var
	VB: TVirtualBitmap;
  i, j : Integer;
begin
	VB := TVirtualBitmap.CreateFromBitmap(Bmp);
	if VB.Bits <= 8 then
  begin
    for i := 0 to VB.Colors - 1 do
    begin
    	VB.ColorTable[i] := DarkenColor(VB.ColorTable[i], Grade);
    end;
  end
  else
  begin
  for i := 0 to VB.Width-1 do
  	for j := 0 to VB.Height-1 do
    begin
      VB.SetPixel(i, j, DarkenColor(VB.GetPixel(i, j), Grade));
    end;
  end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
end;

function MakeButton(Src : TBitmap; Border : Integer; Soft : Boolean) : TBitmap;
var
	VB: TVirtualBitmap;
  i, j : Integer;
  Edge: Integer;
begin
	VB := TVirtualBitmap.CreateFromBitmap(Src);
  if VB.Bits <= 8 then
  begin
  	VB.Free;  
  	raise Exception.Create('Required 16bit or more colors for this operation.');
  end;
  Edge := 96;
  for j := 1 to Border do
    for i := j - 1 to Src.Width - (j - 1) do
    begin
      VB.SetPixel(i, j - 1, BrightenColor(VB.GetPixel(i, j -1), Edge));
      VB.SetPixel(i, Src.Height - (j - 1), DarkenColor(VB.GetPixel(i,
      	VB.Height - (j - 1)), Edge));
      if Soft then Edge := Round(96 - (j-1) / Border * 96);
    end;
  Edge := 96;
  for j := 1 to Border do
    for i := j - 1 to Src.Height - (j - 1)  do
    begin
      VB.SetPixel(j - 1, i , BrightenColor(VB.GetPixel(j - 1, i), Edge));
      VB.SetPixel(Src.Width - (j - 1), i, DarkenColor(VB.GetPixel(VB.Width - (j - 1), i), Edge));
      if Soft then Edge := Round(96 - (j-1) / Border * 96);
    end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
end;

function BWOnlyBMP(Src : TBitmap) : TBitmap;
begin
  TmpMem.Assign(Src);
  TmpMem.Monochrome := True;
  TmpMem.Monochrome := False;
  Result := TmpMem;
end;

function GreyBMP(Src : TBitmap) : TBitmap;
var
	VB: TVirtualBitmap;
  i, j : Integer;
begin
	VB := TVirtualBitmap.CreateFromBitmap(Src);
	if VB.Bits <= 8 then
  begin
    for i := 0 to VB.Colors - 1 do
    begin
    	VB.ColorTable[i] := ColorToGrey(VB.ColorTable[i]);
    end;
  end
  else
  begin
  for i := 0 to VB.Width-1 do
  	for j := 0 to VB.Height-1 do
    begin
      VB.SetPixel(i, j, ColorToGrey(VB.GetPixel(i, j)));
    end;
  end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
end;

function MergeBMP(Pic1, Pic2 : TBitmap) : TBitmap;
begin
	Result := MergeBMPExt(Pic1, Pic2, 50);
end;

function MergeBMPExt(Pic1, Pic2 : TBitmap; Grade: Byte) : TBitmap;
var
	VB1, VB2, VB: TVirtualBitmap;
  i, j : Integer;
begin
	VB1 := TVirtualBitmap.CreateFromBitmap(Pic1);
	VB2 := TVirtualBitmap.CreateFromBitmap(Pic2);
	VB := TVirtualBitmap.CreateFromBitmap(Pic1);

	if VB.Bits <= 8 then
  begin
    for i := 0 to VB.Colors - 1 do
    begin
    	VB.ColorTable[i] := MergeColorExt(VB1.ColorTable[i], VB2.ColorTable[i], Grade)
    end;
  end
  else
  begin
  for i := 0 to VB.Width-1 do
  	for j := 0 to VB.Height-1 do
    begin
      VB.SetPixel(i, j, MergeColorExt(VB1.GetPixel(i, j), VB2.GetPixel(i, j), Grade));
    end;
  end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
  VB1.Free;
  VB2.Free;
end;

function SoftenBMP(Src : TBitmap; Deep : Single) : TBitmap;
var
	VB: TVirtualBitmap;
  i, j : Integer;
  SR: TRect;
begin
	VB := TVirtualBitmap.CreateFromBitmap(Src);
  for i := 0 to VB.Width-1 do
  	for j := 0 to VB.Height-1 do
    begin
      SR := Bounds(Round(i - Deep), Round(j - Deep), Round(Deep * 2), Round(Deep * 2));
      VB.SetPixel(i, j, MiscVBToColor(VB, SR));
    end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
end;

function ColoriseImage(Bmp : TBitmap; BaseColor : TColor) : TBitmap;
var
	VB: TVirtualBitmap;
  i, j : Integer;
begin
	VB := TVirtualBitmap.CreateFromBitmap(Bmp);
	if VB.Bits <= 8 then
  begin
    for i := 0 to VB.Colors - 1 do
    begin
    	VB.ColorTable[i] := Colorise(VB.ColorTable[i], BaseColor);
    end;
  end
  else
  begin
  for i := 0 to VB.Width-1 do
  	for j := 0 to VB.Height-1 do
    begin
      VB.SetPixel(i, j, Colorise(VB.GetPixel(i, j), BaseColor));
    end;
  end;
  VB.ProduceBitmap(TmpMem);
  Result := TmpMem;
  VB.Free;
end;

initialization
	TmpMem := TBitmap.Create;

finalization
	TmpMem.Free;


end.


