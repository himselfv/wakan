
{ ---------------------------------------------------
		Drawing Manager  Copyright (r) by DreamFactory
    Version : 1.75   Author : William Yang
		Last Update 10 - Sep - 97
  --------------------------------------------------- }

unit DrawMan;

interface

{ ------------------------------------------------------------------------

  HOW TO Draw a transparent image
	1. Create a mask first.
    * Draw a Black & White image with black as the non-transparent area.
  2. Create a Storage bitmap.
  	 For store the background image.
     1 Copy the background with the size of the transparent image.
     2 Paint the Transparent Image on to the Storage Bitmap.
       Use CopyMode of cmSrcInvert.
       All the colors will be Inverted.
     3 Paint the mask on to the storage bitmap.
       Use CopyMode of cmSrcAnd.
       This will only paint the black area on to the bitmap.
     4 Agian paint the Transparent Image on to the Storage Bitmap.
       Use CopyMode of cmSrcInvert.
       The black area will be painted with the Image.
       and the other parts will restore to orignal color.
  3. Paint the Storage bitmap on to the Destination Canvas.

  See PaintOnMask, PaintOnText.
 -------------------------------------------------------------------------}

uses Windows, Classes, Controls ,Graphics, ColorMan, ExtCtrls,
  NumMan, VBitmap, SysUtils;

{$I DFDefin.inc}

{Draw a rounded button}
procedure RoundButton(Canvas : TCanvas; Size : TRect;
	ButtonColor, SeatColor : TColor; SWidth : Integer );

{Draw a 3D rectangle which the colours are fit the given "Color"}
{Style: 1:Raise, 2:Sunk, 3:Frame}
procedure Draw3D(Dest : TCanvas; Area : TRect; Color : TColor;
	Style, Width : Integer);
{Like Frame3D but this is for text}
procedure Draw3DText(Dest : TCanvas; Text: String; X, Y : Integer; HighEdge, LowEdge: TColor);

{Paint Tiled "Src" in "Dest"
 Area indicates the painting rectangle }
procedure MultiPaint(Dest : TCanvas; Src : TBitmap; Area : TRect);

{Paint transparent "Bmp" on the "Dest"}
procedure TransparentBlt (Dest : TCanvas; Bmp : TBitmap;
	destX, destY : Integer; TransColor : TColor);
{One parameter ignored, TransColor is the first pixel of your bitmap}
procedure TransBlt(Dest : TCanvas; Bmp : TBitmap;
	destX, destY : Integer);

{Draw text with shadow}
procedure DrawShadowText(Dest: TCanvas; X, Y : Integer; Text: String);

{Draw the given text with shadow and more options}
{SC : Outline Colour}
procedure DrawShadowTextExt(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor;
	SX, SY: Integer);
//Create a mask for the source bitmap.
function CreateMask(Src: TBitmap; TransColor: TColor): HBitmap;
{Draw the given text and the outlines}
{SC : Outline Colour}
procedure DrawOutlinedText(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor);
{Draw only the outlines of the given text}
procedure DrawTextOutline(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor);

{Like multipaint but this can work out the Clipping area
 so you dont need to refresh the whole area when repaint,
 simply by send Canvas.ClipRect. }
procedure MultiClipPaint(Dest : TCanvas; Src : TBitmap; Area, Clip : TRect);

{This one for paint big picture, it will chop it into small pieces
 and paint them on to the dest. (if you have few ram )}
procedure BytesPaint(Dest : TCanvas; Area: TRect; Src : TBitmap);

{This function will give the rect measure in screen mode}
{ClientToScreen only give Point of its parent control }
function GetCtrlRect(const Ctrl: TControl): TRect;
function GetCtrlPoint(const Ctrl: TControl): TPoint;
{Merge rectangles to one piece}
function MergeRect(R : array of TRect): TRect;


{Paint bitmaps on the mask.
 the mask must be monochrome.}
 
procedure StretchPaintOnMask(Dest: TCanvas; X,Y : Integer; XMask, Bmp: TBitmap);
procedure PaintOnMask(Dest: TCanvas; X,Y : Integer; XMask, Bmp: TBitmap);

{ Some functions that will make you a nice text }
{ Paint bitmaps on text
  Font can be modified before send the Canvas.
}
procedure PaintOnText(Dest: TCanvas; X,Y : Integer; Text: String; Bmp: TBitmap);
// Stretch the bitmap to fit on the text.
procedure StretchPaintOnText(Dest: TCanvas; X,Y : Integer; Text: String; Bmp: TBitmap);

//Set the size of bitmap rather than Width :=, Height :=.
procedure SetSize(Bitmap: TBItmap; W, H: Integer);
//Check if bitmap is Empty.
function BmpIsNil(Bitmap: TBitmap): Boolean;

procedure TextSmooth(Canvas: TCanvas; X, Y: Integer; Text: String);
procedure SmoothBlt(Canvas: TCanvas; X, Y: Integer; Bmp: TBitmap; Trans: TColor);
function TransColor(Bmp: TBitmap): TColor;
procedure DrawTextAnglo(Canvas: TCanvas; X, Y: Integer; Text: String; Anglo: Integer);


//Read Text Pixels
type
	TReadPixelProcs = procedure (Color: TColor);

procedure ReadPixels(Src: TBitmap; ReadProcs: TReadPixelProcs);
//Use canvas so you send the font together.
procedure ReadText(Canvas: TCanvas; Text: String; ReadProcs: TReadPixelProcs);

implementation

function TransColor(Bmp: TBitmap): TColor;
begin
	Result := Bmp.Canvas.Pixels[0, 0];
end;

procedure SSetPixel(Canvas: TCanvas; X, Y: Integer; Color: TColor);
begin
	Canvas.Pixels[X, Y] := MergeColorExt(Color,	Canvas.Pixels[X, Y], 30);
end;

procedure SmoothBlt(Canvas: TCanvas; X, Y: Integer; Bmp: TBitmap; Trans: TColor);
var
  Back: TBitmap;
  i,j : Integer;

  procedure SmoothPixel(ix, iy, Color: Integer);
  begin
  	Back.Canvas.Pixels[ix, iy] := MergeColorExt(Back.Canvas.Pixels[ix, iy],
    	Color, 20);
  end;

begin
	if Trans < 0 then Trans := TransColor(Bmp); 
	Back := TBitmap.Create;
  //the background picture will be slidely larger than the smooth image.
  Back.Width := Bmp.Width + 2;   //**
  Back.Height := Bmp.Height + 2; //**
  //Create a background picture make it as small as possible.
  Back.Canvas.CopyRect(Rect(0, 0, Back.Width, Back.Width), Canvas,
  	Bounds(x-1, y-1, Back.Width, Back.Width));
  //Paint the image to back picture
  TransparentBlt(Back.Canvas, Bmp, 1, 1, Trans);
  //Only process the image area.  (see **)
	for i := 1 to Back.Width-2 do
  	for j := 1 to Back.Height-2 do
    begin
    	//Not in Image area, becase the size
    	if Bmp.Canvas.Pixels[i-1, j-1] <> Trans then
      begin
      	if (Bmp.Canvas.Pixels[i-2, j-1] = Trans) then SmoothPixel(i-1, j, Bmp.Canvas.Pixels[i-1, j-1]);
      	if (Bmp.Canvas.Pixels[i-1, j-2] = Trans) then SmoothPixel(i, j-1, Bmp.Canvas.Pixels[i-1, j-1]);
      	if (Bmp.Canvas.Pixels[i, j-1] = Trans) then SmoothPixel(i+1, j, Bmp.Canvas.Pixels[i-1, j-1]);
      	if (Bmp.Canvas.Pixels[i-1, j] = Trans) then SmoothPixel(i, j+1, Bmp.Canvas.Pixels[i-1, j-1]);
      end;
    end;
	Canvas.Draw(x-1, y-1, Back);
  Back.Free;
end;

procedure TextSmooth(Canvas: TCanvas; X, Y: Integer; Text: String);
var
	Tb, Tmp: TBitmap;
  i, j: Integer;
begin
	//Paint text to BW bitmap first.
	Tb := TBitmap.Create;
	Tb.Width := Canvas.TextWidth(Text);
	Tb.Height := Canvas.TextHeight(Text);
  Tb.Monochrome := True;
  Tb.Canvas.Brush.Color := clWhite;
  Tb.Canvas.Font.Assign(Canvas.Font);
  Tb.Canvas.Font.Color := clBlack;
  Tb.Canvas.Textout(0, 0, Text);
  Tmp := TBitmap.Create;
  Tmp.Width := Tb.Width + 2;
  Tmp.Height := Tb.Height + 2;
  Tmp.Canvas.CopyRect(Rect(0, 0, Tmp.Width, Tmp.Height), Canvas,
  	Bounds(x-1, y-1, Tmp.Width, Tmp.Height));
  for i := 0 to Tb.Width do
  	for j := 0 to Tb.Height do
    begin
    	if Tb.Canvas.Pixels[i, j] = clBlack then
      begin
      	Tmp.Canvas.Pixels[i, j] := Canvas.Font.Color;
     		if Tb.Canvas.Pixels[i+1, j] <> clBlack then
        	SSetPixel(Tmp.Canvas, i+1, J, Canvas.Font.Color);
//     		if Tb.Canvas.Pixels[i-1, j] <> clBlack then
//        	SSetPixel(Tmp.Canvas, i-1, J, Canvas.Font.Color);
     		if Tb.Canvas.Pixels[i, j+1] <> clBlack then
        	SSetPixel(Tmp.Canvas, i, J+1, Canvas.Font.Color);
//     		if Tb.Canvas.Pixels[i, j-1] <> clBlack then
//        	SSetPixel(Tmp.Canvas, i, J-1, Canvas.Font.Color);
      end;
    end;
  Canvas.Draw(x, y, Tmp);
  Tmp.Free;
  Tb.Free;
end;

procedure DrawTextAnglo(Canvas: TCanvas; X, Y: Integer; Text: String; Anglo: Integer);
var
	hFont: Integer;
  Weight: Integer;
  Italic: Integer;
  Under: Integer;
  Strike: Integer;
  Fontname: array[1..32] of Char;
begin
	if fsBold in Canvas.Font.Style then Weight := 700 else Weight := 400;
  if fsItalic in Canvas.Font.Style then Italic := 1  else Italic := 0;
  if fsUnderline in Canvas.Font.Style then Under := 1 else Under := 0;
  if fsStrikeout in Canvas.Font.Style then Strike := 1 else Strike := 0;
  StrPCopy(@Fontname, Canvas.Font.Name);
	hFont := CreateFont(Abs(Canvas.Font.Height), 0, Anglo, Anglo, Weight,
  	Italic, Under, Strike, 0, 0, PROOF_QUALITY, DEFAULT_PITCH, 0, @Fontname);

  Canvas.Textout(x, y, Text);
  DeleteObject(hfont);
end;

procedure ReadPixels(Src: TBitmap; ReadProcs: TReadPixelProcs);
var
	i, j : Integer;
begin
  for i := 0 to Src.Width do
  	for j := 0 to Src.Height do
    begin
    	ReadProcs(GetPixel(Src.Canvas.Handle, i, j));
    end;
end;

function BmpIsNil(Bitmap: TBitmap): Boolean;
begin
  Result := False;
	if Bitmap = nil then
  begin
  	Result := True;
    Exit;
  end;
	if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
  begin
  	Result := True;
    Exit;
  end;
end;

procedure SetSize(Bitmap: TBItmap; W, H: Integer);
begin
	Bitmap.Width := W;
  Bitmap.Height := H;
end;

procedure ReadText(Canvas: TCanvas; Text: String; ReadProcs: TReadPixelProcs);
var
	BWText: TBitmap;
begin
	BWText := TBitmap.Create;
  BWText.Monochrome := True;
  BWText.Width := Canvas.TextWidth(Text);
  BWText.Height := Canvas.TextHeight(Text);
  //ensure that font.color is Black.
  Canvas.Font.Color := clBlack;
  BWText.Canvas.Textout(0, 0, Text);
  ReadPixels(BWText, ReadProcs);
  BWText.Free;
end;

function MergeRect(R: array of TRect): TRect;
var
	i: Integer;
begin
	//Get the first one, so we compare it to the others
	Result := R[Low(R)];
	for i := Low(R) + 1 to High(R) do
  begin
  	//Compare the Top/Left point keep the small ones.
  	Result.Left := Min(Result.Left, R[i].Left);
  	Result.Top := Min(Result.Top, R[i].Top);
    //Compare the Right/Bottom kepp the Big ones
  	Result.Right := Max(Result.Right, R[i].Right);
  	Result.Bottom := Max(Result.Bottom, R[i].Bottom);
  end;
end;

function GetCtrlRect(const Ctrl: TControl): TRect;
var
	FParent : TControl;
begin
	FParent := Ctrl;
  Result := FParent.ClientRect;
	while FParent.HasParent do
  begin
  	if not FParent.Parent.HasParent then Exit;
		FParent := FParent.Parent;
    OffsetRect(Result, FParent.Left, FParent.Top);
  end;
end;

function GetCtrlPoint(const Ctrl: TControl): TPoint;
var
	FParent : TControl;
begin
	FParent := Ctrl;
  with Result do
  begin
  	Result.X := 0; Result.Y := 0;
		Result.X := Result.X + Ctrl.Left;
		Result.Y := Result.Y + Ctrl.Top;
		while FParent.HasParent do
  	begin
			FParent := FParent.Parent;
			Result.X := Result.X + FParent.Left;
			Result.Y := Result.Y + FParent.Top;
  	end;
  end;
end;


procedure DrawOutlinedText(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor);
var
	Backup: TColor;
begin
  with Dest do
  begin
  	Backup := Font.Color;
    Font.Color := SC;
    Brush.Style := bsClear;
  	TextOut(X + 1, Y + 1, Text);
  	TextOut(X - 1, Y + 1, Text);
  	TextOut(X - 1, Y - 1, Text);
  	TextOut(X + 1, Y - 1, Text);
    Font.Color := Backup;
  	TextOut(X, Y, Text);
  end;
end;

procedure Draw3DText(Dest : TCanvas; Text: String; X, Y : Integer; HighEdge, LowEdge: TColor);
var
	Backup: TColor;
begin
  with Dest do
  begin
  	Backup := Font.Color;
    Brush.Style := bsClear;
    Font.Color := LowEdge;
  	TextOut(X + 1, Y + 1, Text);
    Font.Color := HighEdge;
  	TextOut(X - 1, Y - 1, Text);
    Font.Color := Backup;
  	TextOut(X, Y, Text);
  end;
end;

procedure DrawTextOutline(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor);
begin
  with Dest do
  begin
    Font.Color := SC;
    Brush.Style := bsClear;
  	TextOut(X + 1, Y + 1, Text);
  	TextOut(X - 1, Y + 1, Text);
  	TextOut(X - 1, Y - 1, Text);
  	TextOut(X + 1, Y - 1, Text);
  end;
end;

procedure DrawShadowText(Dest: TCanvas; X, Y : Integer; Text: String);
begin
	DrawShadowTextExt(Dest, X, Y, Text, clGray, 2, 2);
end;

procedure DrawShadowTextExt(Dest: TCanvas; X, Y : Integer; Text: String; SC: TColor;
	SX, SY: Integer);
var
	Backup: TColor;
begin
  with Dest do
  begin
  	Backup := Font.Color;
    Brush.Style := bsClear;
    Font.Color := SC;
  	TextOut(X + SX, Y + SY, Text);
    Font.Color := Backup;
  	TextOut(X, Y, Text);
  end;
end;

procedure TransBlt(Dest : TCanvas; Bmp : TBitmap;
	destX, destY : Integer);
begin
	TransparentBlt(Dest, Bmp, DestX, DestY, Bmp.TransparentColor);
end;

procedure StretchPaintOnMask(Dest: TCanvas; X,Y : Integer; XMask, Bmp: TBitmap);
var
  fStore: TBitmap;
  R: TRect;
begin
	fStore := TBitmap.Create;
  with fStore do
  begin
  	Width := XMask.Width;
  	Height := XMask.Height;
  	R := Rect(0,0,Width,Height);
  	with Canvas do
    begin
    	CopyRect(R, Dest, Bounds(X,Y,Width,Height));

    	CopyMode := cmSrcInvert;
    	StretchDraw(R, Bmp);

    	CopyMode := cmSrcAnd;
    	Draw(0, 0, XMask);

    	CopyMode := cmSrcInvert;
    	StretchDraw(R, Bmp);
    end;
  end;
  Dest.Draw(x, y, fStore);
  fStore.Free;
end;

procedure PaintOnMask(Dest: TCanvas; X,Y : Integer; XMask, Bmp: TBitmap);
var
  fStore: TBitmap;
  R: TRect;
begin
	fStore := TBitmap.Create;
  with fStore do
  begin
  	Width := Bmp.Width;
  	Height := Bmp.Height;
  	R := Rect(0, 0, Width,Height);
  	with Canvas do
    begin
    	CopyRect(R, Dest, Bounds(X,Y,Width,Height));

    	CopyMode := cmSrcInvert;
    	Draw(0, 0, Bmp);

    	CopyMode := cmSrcAnd;
    	Draw(0, 0, XMask);

    	CopyMode := cmSrcInvert;
    	Draw(0, 0, Bmp);
    end;
  end;
  Dest.Draw(x, y, fStore);
  fStore.Free;
end;

procedure StretchPaintOnText(Dest: TCanvas; X,Y : Integer; Text: String; Bmp: TBitmap);
var
	fMask: TBitmap;
  fStore: TBitmap;
  R: TRect;
begin
	fMask := TBitmap.Create;
  // Make the b&w XMask first.
  with fMask, fMask.Canvas do
  begin
  	Monochrome := True;
    Font.Assign(Dest.Font);
    //Must be black;
    Font.Color := clBlack;
  	Width := TextWidth(Text);
  	Height := TextHeight(Text);
  	Textout(0, 0, Text);
  end;
	fStore := TBitmap.Create;
  with fStore do
  begin
  	Width := fMask.Width;
  	Height := fMask.Height;
  	R := Rect(0,0,Width,Height);
  	with Canvas do 	//Always use with...do, it will also save
    begin 					//the size of your program.
    	CopyRect(R, Dest, Bounds(X,Y,Width,Height));

    	CopyMode := cmSrcInvert;
    	StretchDraw(R, Bmp);

    	CopyMode := cmSrcAnd;
    	Draw(0, 0, fMask);

    	CopyMode := cmSrcInvert;
    	StretchDraw(R, Bmp);
    end;
  end;
  Dest.Draw(x, y, fStore);
  fStore.Free;
	fMask.Free;
end;

procedure PaintOnText(Dest: TCanvas; X,Y : Integer; Text: String; Bmp: TBitmap);
var
	fMask: TBitmap;
  fStore: TBitmap;
begin
	fMask := TBitmap.Create;
  // Make the b&w XMask first.
  with fMask, fMask.Canvas do
  begin
  	Monochrome := True;
    Font.Assign(Dest.Font);
    //Must be black;
    Font.Color := clBlack;
  	Width := TextWidth(Text);
  	Height := TextHeight(Text);
  	Textout(0, 0, Text);
  end;
	fStore := TBitmap.Create;
  with fStore, fStore.Canvas do
  begin
  	Width := fMask.Width;
  	Height := fMask.Height;
  	CopyRect(Rect(0,0,Width,Height), Dest, Bounds(X,Y,Width,Height));

    CopyMode := cmSrcInvert;
    Draw(0, 0, Bmp);

    CopyMode := cmSrcAnd;
    CopyRect(Rect(0,0,Width,Height), fMask.Canvas, Rect(0,0,Width,Height));

    CopyMode := cmSrcInvert;
    Draw(0, 0, Bmp);
  end;
  Dest.Draw(X, Y, fStore);
  fStore.Free;
	fMask.Free;
end;

function CreateMask(Src: TBitmap; TransColor: TColor): HBitmap;
var
  XMask: TBitmap;
  OrigColor : TColor;
  SDC: Integer;
begin
	if Src.Handle = 0 then Exit;
	SDC := Src.Canvas.Handle;
  XMask := TBitmap.Create;
  XMask.Monochrome := True;
  XMask.Width := Src.Width;
  XMask.Height := Src.Height;
  TransColor := ColorToRGB(TransColor);
  OrigColor := SetBkColor(SDC, TransColor);
  // The transparent area will White and the non-transparent area
  // will be Black.
  BitBlt(XMask.Canvas.Handle, 0, 0, Src.Width, Src.Height,
  	SDC, 0, 0, SRCCOPY);
	TransColor := SetBkColor(SDC, OrigColor);
  Result := XMask.Handle;
end;

//Old paint method for D2, D1 only
procedure D2TransparentBlt (Dest : TCanvas; Bmp : TBitmap;
	destX, destY : Integer; TransColor : TColor);
var
srcDC, saveDC, maskDC, invDC, resultDC, hResultBmp, hSaveBmp, hMaskBmp,
hInvBmp, hPrevBmp, hSrcPrevBmp, hSavePrevBmp, hDestPrevBmp, hMaskPrevBmp,
hInvPrevBmp, Success, HDC, BH, BW, hB : Integer;
OrigColor : TColor;
begin
//	Dest.Brush.Style := bsClear;
//	Dest.FillRect(Dest.ClipRect);
	if TransColor < 0 then TransColor := Bmp.TransparentColor;
	HDC := Dest.Handle;
  BH := BMP.Height; BW := BMP.Width; hB := BMP.Handle;
  srcDC := CreateCompatibleDC(hDC);    //Create DC to hold stage
  saveDC := CreateCompatibleDC(hDC);   //Create DC to hold stage
  maskDC := CreateCompatibleDC(hDC);   //Create DC to hold stage
  invDC := CreateCompatibleDC(hDC);    //Create DC to hold stage
  resultDC := CreateCompatibleDC(hDC); //Create DC to hold stage
  //Create monochrome bitmaps for the XMask-related bitmaps:
  hMaskBmp := CreateBitmap(BW, BH, 1, 1, PChar(0));
  hInvBmp := CreateBitmap(BW, BH, 1, 1, PChar(0));
  //Create color bitmaps for final result & stored copy of source
  hResultBmp := CreateCompatibleBitmap(HDC, BW,
  	BH);
  hSaveBmp := CreateCompatibleBitmap(HDC, BW, BH);
  hSrcPrevBmp := SelectObject(srcDC, hB);     //Select bitmap in DC
  hSavePrevBmp := SelectObject(saveDC, hSaveBmp); //Select bitmap in DC
  hMaskPrevBmp := SelectObject(maskDC, hMaskBmp); //Select bitmap in DC
  hInvPrevBmp := SelectObject(invDC, hInvBmp);    //Select bitmap in DC
  hDestPrevBmp := SelectObject(resultDC, hResultBmp); //Select bitmap
  BitBlt(saveDC, 0, 0, BW, BH, srcDC,
		0, 0, SRCCOPY);

  OrigColor := SetBkColor(srcDC, TransColor);
  BitBlt(maskDC, 0, 0, BW, BH, srcDC, 0, 0, SRCCOPY);
	TransColor := SetBkColor(srcDC, OrigColor);

	BitBlt(invDC, 0, 0, BW, BH, maskDC, 0, 0, NOTSRCCOPY);
  //Copy background bitmap to result & create final transparent bitmap
  BitBlt(resultDC, 0, 0, BW, BH, HDC, destX, destY, SRCCOPY);
  //AND XMask bitmap w/ result DC to punch hole in the background by
  //painting black area for non-transparent portion of source bitmap.
  BitBlt(resultDC, 0, 0, BW, BH, maskDC, 0, 0, SRCAND);
  //AND inverse XMask w/ source bitmap to turn off bits associated

  //with transparent area of source bitmap by making it black.
  BitBlt(srcDC, 0, 0, BW, BH, invDC,
           0, 0, SRCAND);
  //XOR result w/ source bitmap to make background show through.
  BitBlt(resultDC, 0, 0, BW, BH, srcDC, 0, 0, SRCPAINT);
  BitBlt(HDC, destX, destY, BW, BH, resultDC, 0, 0, SRCCOPY);
  //Display transparent bitmap on backgrnd

  BitBlt(srcDC, 0, 0, BW, BH, saveDC,0, 0, SRCCOPY);
  //Restore backup of bitmap.
  hPrevBmp := SelectObject(srcDC, hSrcPrevBmp); //Select orig object
  hPrevBmp := SelectObject(saveDC, hSavePrevBmp); //Select orig object
  hPrevBmp := SelectObject(resultDC, hDestPrevBmp); //Select orig object
  hPrevBmp := SelectObject(maskDC, hMaskPrevBmp); //Select orig object
  hPrevBmp := SelectObject(invDC, hInvPrevBmp); //Select orig object

  DeleteObject(hSaveBmp);   //Deallocate system resources.
  DeleteObject(hMaskBmp);   //Deallocate system resources.
  DeleteObject(hInvBmp);    //Deallocate system resources.
  DeleteObject(hResultBmp); //Deallocate system resources.
  DeleteDC(srcDC);          //Deallocate system resources.
  DeleteDC(saveDC);         //Deallocate system resources.
  DeleteDC(invDC);          //Deallocate system resources.

  DeleteDC(maskDC);         //Deallocate system resources.
  DeleteDC(resultDC);       //Deallocate system resources.

end;

procedure TransparentBlt (Dest : TCanvas; Bmp : TBitmap;
	destX, destY : Integer; TransColor : TColor);
begin
 {$IFDEF DF_D3} 	//Use Delphi3's own method
		Bmp.Transparent := True;
		Bmp.TransparentColor := TransColor;
  	Dest.Draw(DestX, DestY, Bmp);
 {$ELSE} {-- Delphi 2.0  or C++ Builder   }
	D2TransparentBlt(Dest, Bmp, destX, destY, TransColor);
 {$ENDIF}
end;

procedure MultiPaint(Dest : TCanvas; Src : TBitmap; Area : TRect);
var
	x, y : Integer;
  xl, yl : Integer;
  Dr, Sr : TRect;
begin
  if (Src.Width = 0) or (Src.Height=0) then Exit;
	x := Area.Left; y := Area.Top;
	while (y < Area.Bottom) and (x < Area.Right) do
  begin
  	yl := Min(Area.Bottom - y, Src.Height);
  	xl := Min(Area.Right - x, Src.Width);
    DR := Bounds(x, y, xl, yl);
    SR := Bounds(0, 0, xl, yl);
    Dest.CopyRect(DR, Src.Canvas, SR);
    Inc(x, xl);
    if x >= Area.Right then
    begin
      x := 0;
      Inc(y, yl);
    end;
  end;
end;

procedure MultiClipPaint(Dest : TCanvas; Src : TBitmap; Area, Clip : TRect);
var
	x, y : Integer;
  xl, yl : Integer;
  Dr, Sr : TRect;
  Sx, sy: Integer;
begin
  if (Src.Width = 0) or (Src.Height=0) then Exit;
	x := Clip.Left; y := Clip.Top;
	while (y < Clip.Bottom) and (x < Clip.Right) do
  begin
  	sx := x mod Src.Width; SY := y mod Src.Height;
  	yl := Min(Clip.Bottom - y, Src.Height - sy);
  	xl := Min(Clip.Right - x, Src.Width - sx);
    DR := Bounds(x, y, xl, yl);
    SR := Bounds(sx, sy, xl, yl);
    Dest.CopyRect(DR, Src.Canvas, SR);
    Inc(x, xl);
    if x >= Clip.Right then
    begin
      x := Clip.Left;
      Inc(y, yl);
    end;
  end;
end;

procedure BytesPaint(Dest : TCanvas; Area: TRect; Src : TBitmap);
const
	ByteSize = 48;

var
	px, py : Integer;
  xl, yl : Integer;
  Dr, Sr : TRect;
begin
  if (Src.Width = 0) or (Src.Height=0) then Exit;
  py := 0; px := 0;
	while (py < Area.Bottom-Area.Left) and (px < Area.Right-Area.Top) do
  begin
  	yl := MinMost([ByteSize, Src.Height, Area.Bottom - py]);
  	xl := MinMost([ByteSize, Src.Width, Area.Right - px]);
    DR := Bounds(Area.Left + px, Area.Top + py, xl, yl);
    SR := Bounds(px, py, xl, yl);
    Dest.CopyRect(DR, Src.Canvas, SR);
    Inc(px, xl);
    if px >= Area.Right then
    begin
      px := 0;
      Inc(py, yl);
    end;
  end;
end;

procedure Draw3D(Dest : TCanvas; Area : TRect; Color : TColor;
	Style, Width : Integer);
var
BrightColor, DarkColor : TColor;

	procedure DrawSunken;
  begin
//  	Dest.Brush.Style := bsClear;
  	Frame3D(Dest, Area, DarkColor, BrightColor, Width);
  end;

	procedure DrawRaise;
  begin
//  	Dest.Brush.Style := bsClear;
  	Frame3D(Dest, Area, BrightColor, DarkColor, Width);
  end;

	procedure DrawFrame;
  begin
		with Dest do
    begin
    Brush.Style := bsClear;
    Pen.Width := Width;
    Pen.Color := BrightColor;
    Rectangle(Area.Left + 1, Area.Top + 1 ,
    	Area.Right + 1, Area.Bottom + 1);
    Brush.Color := DarkColor;
    Rectangle(Area.Left, Area.Top ,
    	Area.Right, Area.Bottom);
  	end;
  end;
begin
  {Clear the area with backcolor}
  BrightColor := BrightenColor(Color, 40);
  DarkColor := DarkenColor(Color, 64);
  case Style of
  1 : DrawRaise;
  2 : DrawSunken;
  3 : DrawFrame;
  end;
end;

procedure RoundButton(Canvas : TCanvas; Size : TRect;
	ButtonColor, SeatColor : TColor; SWidth : Integer );
var
	l, t, r, b, h, w  : Integer;
  i, j : Integer;
  bl, bt, br, bb : Integer;
begin

  with Size do
  begin
  	l := Left;
    t := Top;
    r := Right;
    b := Bottom;
    h := b - t;
    w := r - l;
  end;
	{ Draw Seat Set }
  with Canvas do
  begin
  	{Fill with orginal color}
    Brush.Style := bsClear;
    Pen.Style := psClear;
    FillRect(Size);
    {pen and brush Back to normal}
    Pen.Style := psSolid;
    Brush.Style := bsClear;
  	{Draw the seat}
    Pen.Width := SWidth;
    Pen.Color := SeatColor;
  	Ellipse(l + SWidth div 2, t + SWidth div 2, r - SWidth div 2, b - SWidth  div 2);
    {Draw out line}
    Pen.Width := 1;
		Pen.Color:= DarkenColor(SeatColor, 32);
    Brush.Style := bsClear;
  	Arc(l, t, r, b, w div 5 * 4, h div 5, w div 5,
    	h div 5 * 4);
  	Pen.Color:= DarkenColor(SeatColor, 64);

  	Arc(l, t, r, b, w div 5, h div 5 * 4, w div 5 * 4,
    	h div 5);


    Brush.Style := bsSolid;
    {Draw Button's shadow with gradient color}
    for i := 0 to SWidth - 2 do
    begin
    	Pen.Color := DarkenColor(ButtonColor, Round(80 - i * 80 / (SWidth - 3)));
      Pen.Width := 2;
      Brush.Color := ButtonColor;
      bl := l + SWidth - i; bt := t + SWidth - i;
      br := r - SWidth - i; bb := b - SWidth - i;
    	Ellipse(bl, bt, br, bb);
    end;

    Dec(bl); Dec(bt);
   Dec(br); Dec(bb);

    {draw the button's outline}
    Pen.Width := 1;
    Pen.Color := DarkenColor(ButtonColor, 16);
    Arc(bl, bt, br, bb, (br - bl) div 5 * 4, (bb - bt) div 5 , (br - bl) div 5,
    	(bb - bt) div 5 * 4);
    Arc(bl, bt, br, bb, (br - bl) div 5 * 4, (bb - bt) div 5 , (br - bl) div 5,
    	(bb - bt) div 5 * 4);

    Pen.Color := DarkenColor(ButtonColor, 32);
    Arc(bl, bt, br, bb, (br - bl) div 5, (bb - bt) div 5 * 4, (br - bl) div 5 * 4,
    	(bb - bt) div 5);

  end; {with do}

end;

end.
 
