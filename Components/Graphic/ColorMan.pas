
{ ---------------------------------------------------
		Colour Manager   Copyright (r) by DreamFactory
    Version : 1.75   Author : William Yang
		Last Update 09 - Sep - 97
  --------------------------------------------------- }

unit ColorMan;

interface

uses Windows, SysUtils, Graphics, NumMan, VBitmap, Variants;

//Common Used in Quick Basic
function QBColor (n:Integer) : TColor;
function ToQBColor (C: TColor) : Integer;

function HexToColor (Hex : String) : TColor; //Transfer Hexidecimal to Color
function HexToInt (Hex : String) : Integer;   //Transfer Hexidecimal to Integer
function BrightenColor(BaseColor: TColor; Adjust : Integer): TColor;
  //Convert the BaseColor to the Grade of Adjust Brighten Color
function DarkenColor(BaseColor: TColor; Adjust : Integer): TColor;
  //Convert the BaseColor to the Grade of Adjust Darken Color
function ColorToGrey(SC : TColor) : TColor; //Convert the SC to Grey Color
function Colorise(SC, MC : TColor) : TColor; //Convert the SC to MC Color
function ERGB(R,G,B : Single) : TColor; //Error RGB Color
function CorrectColor(C : Single) : Integer; //Correct the Wrong Color Byte
function MergeColor(C1, C2 : TColor) : TColor; //Merge Two Color to One
function MiscColor(var C : Variant) : TColor; //Misc A Number of Colors To One
function MiscBmpToColor(SDC : Integer; W, H : Integer; A : TRect) : TColor;
//Misc A Whole BMP to One Color
function IsGreyColor(C : TColor) : Boolean; //Check if the Color is grey color
function IsLightColor(C : TColor) : Boolean; //Check if the Color is Light Color

{Added 19-March-1997}
function RGBAvg(C : TColor) : Integer;
function SumRGB(C : TColor) : Integer;
function CompareR(C1, C2 : TColor) : Integer;
function CompareG(C1, C2 : TColor) : Integer;
function CompareB(C1, C2 : TColor) : Integer;
function CompareColor(C1, C2 : TColor) : Integer;

function ColorAdd(C1, C2 : TColor) : TColor;
function ColorMinus(C1, C2 : TColor) : TColor;

function InvertColor(C: TColor) : TColor;
function CentreRGB(C : TColor; Percent : Integer) : TColor;

// 10 July 1997
function MergeColorExt(C1, C2 : TColor; Grade: Byte) : TColor;

function AutoBW(BGColor: TColor): TColor;

procedure DeColor(Color: TColor; var R, G, B: Integer);

// 16 Oct 1997
procedure DeColorB(Color: TColor; var R, G, B: Byte);
function MiscVBToColor(VB : TVirtualBitmap; A : TRect) : TColor;

implementation

procedure DeColor(Color: TColor; var R, G, B: Integer);
begin
	R := GetRValue(Color);
	G := GetGValue(Color);
	B := GetBValue(Color);
end;

procedure DeColorB(Color: TColor; var R, G, B: Byte);
begin
	R := GetRValue(Color);
	G := GetGValue(Color);
	B := GetBValue(Color);
end;


function ToQBColor (C: TColor) : Integer;
var
	i : Byte;
begin
	Result := 0;
	for  i := 0 to 15 do
  begin
  	if C = QBColor(i) then
    begin
    	Result := i;
      Exit;
    end;
  end;
end;


function AutoBW(BGColor: TColor): TColor;
begin
	if IsLightColor(BGColor) then
  	Result := clBlack
  else
  	Result := clWhite;
end;

function CentreRGB(C : TColor; Percent : Integer) : TColor;
var
  Avg : Integer;
  R, G, B : Integer;
begin
  Avg := RGBAvg(C);
  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);
  R := R - (R - Avg) * Percent;
  G := G - (G - Avg) * Percent;
  B := B - (B - Avg) * Percent;
  Result := RGB(R,G,B);
end;

function InvertColor(C: TColor) : TColor;
begin
  Result := RGB(255 - GetRValue(C), 255 - GetGValue(C), 255 - GetBValue(C));
end;

function ColorAdd(C1, C2 : TColor) : TColor;
begin
  Result := ERGB(GetRValue(C1) + GetRValue(C2), GetGValue(C1) + GetGValue(C2),
    GetBValue(C1) + GetBValue(C2));
end;

function ColorMinus(C1, C2 : TColor) : TColor;
begin
  Result := ERGB(GetRValue(C1) - GetRValue(C2), GetGValue(C1) - GetGValue(C2),
    GetBValue(C1) - GetBValue(C2));
end;

function CompareR(C1, C2 : TColor) : Integer;
begin
  Result := GetRValue(C1) - GetRValue(C2);
end;

function CompareG(C1, C2 : TColor) : Integer;
begin
  Result := GetGValue(C1) - GetGValue(C2);
end;

function CompareB(C1, C2 : TColor) : Integer;
begin
  Result := GetBValue(C1) - GetBValue(C2);
end;

function CompareColor(C1, C2 : TColor) : Integer; 
begin
  Result := RGBAvg(C1) - RGBAvg(C2);
end;

function SumRGB(C : TColor) : Integer;
begin
  Result := GetRValue(C) + GetGValue(C) + GetBValue(C);
end;

function RGBAvg(C : TColor) : Integer; 
begin
  Result := SumRGB(C) div 3;
end;

function IsLightColor(C : TColor) : Boolean;
var
	Count : Integer;
begin
  Count := 0;
{ Check if any of these value is more than a half of 255 }
  C := ColorToGrey(C); 
	if GetRValue(C) < 128 then Count := Count + 1;
	if GetGValue(C) < 128 then Count := Count + 1;
	if GetBValue(C) < 128 then Count := Count + 1;

  {-------------------------------------------
  |   0      |---|---|---|---|     255       |
  |   Black                        White     |
  -------------------------------------------}

  if Count > 1 then
    Result := False
  else
    Result := True;
end;


function IsGreyColor(C : TColor) : Boolean; 
begin
  {A Grey Color is combined with eque R,G,B Value}
	{Check if Red = Green, Blue = Green }
	if (GetRValue(C) = GetGValue(C)) and (GetBValue(C) = GetGValue(C)) then
  	Result := True
  else
  	Result := False;
end;

{
This function is written after the MiscColor, because I thought the Variant
Parameter Make the tranfering too slow.
}

function MiscBmpToColor(SDC : Integer; W, H : Integer; A : TRect) : TColor; 
var
	C, k, i, j : Integer;
  pR, pG, pB : LongInt;
begin
  K := 0; pR := 0; pG := 0; pB := 0;
 	for i := A.Left to A.Right do
 		for j := A.Top to A.Bottom do
    begin
     	if not ((i < 0) or (j <= 0) or (i > W) or (j > H)) then
      begin
      	C := GetPixel(SDC, i, j);
  			Inc(pR, GetRValue(C));
  			Inc(pG, GetGValue(C));
  			inc(pB, GetBValue(C));
				k := k + 1;
      end;
    end;
  pR := Round( pR / k );
  pB := Round( pB / k );
  pG := Round( pG / k );
  Result := RGB(pR, pG, pB);
end;

function MiscVBToColor(VB : TVirtualBitmap; A : TRect) : TColor;
var
	C, k, i, j : Integer;
  pR, pG, pB : LongInt;
begin
  K := 0; pR := 0; pG := 0; pB := 0;
 	for i := A.Left to A.Right do
 		for j := A.Top to A.Bottom do
    begin
     	if not ((i < 0) or (j <= 0) or (i > VB.Width) or (j > VB.Height)) then
      begin
      	C := VB.GetPixel(i, j);
  			Inc(pR, GetRValue(C));
  			Inc(pG, GetGValue(C));
  			inc(pB, GetBValue(C));
				Inc(k);
      end;
    end;
  pR := Round( pR / k );
  pB := Round( pB / k );
  pG := Round( pG / k );
  Result := RGB(pR, pG, pB);
end;

function MergeColor(C1, C2 : TColor) : TColor;
begin
	MergeColorExt(C1, C2, 50);
end;

function MergeColorExt(C1, C2 : TColor; Grade: Byte) : TColor; 
var
	pR, pG, pB : Single;
begin
  pR := (GetRValue(C1) * Grade / 100 + GetRValue(C2) * (100-Grade) / 100);
  pG := (GetGValue(C1) * Grade / 100 + GetGValue(C2) * (100-Grade) / 100);
  pB := (GetBValue(C1) * Grade / 100 + GetBValue(C2) * (100-Grade) / 100);
  Result := ERGB(pR, pG, pB);
end;

function MiscColor(var C : Variant) : TColor; 
var
	pR, pG, pB : LongInt;
  Count, i, l, h : Integer;
begin
	l := VarArrayLowBound	(C, 1);
  h := VarArrayHighBound (C, 1);
  Count := h - l + 1;
  pR := 0; pG := 0; pB := 0;
  for i := l to h do
  begin
  	pR := pR + GetRValue(C[i]);
  	pG := pG + GetGValue(C[i]);
  	pB := pB + GetBValue(C[i]);
  end;
  pR := Round( pR / Count );
  pB := Round( pB / Count );
  pG := Round( pG / Count );
  Result := RGB( pR, pG, pB);
end;

function CorrectColor(C : Single) : Integer; 
begin
  Result := Round(C);
  if Result > 255 then Result := 255;
  if Result < 0 then Result := 0;
end;

function ERGB(R,G,B : Single) : TColor;
begin
	Result := RGB(CorrectColor(R), CorrectColor(G), CorrectColor(B));
end;

function Colorise(SC, MC : TColor) : TColor; 
var

pR, pG, pB : Single;
begin
  // take the each percentage of r, g, b in the given color
  pR := GetRValue(MC) / 255 + 1;
  pG := GetGValue(MC) / 255 + 1;
  pB := GetBValue(MC) / 255 + 1;

  Result := ColorToGrey(SC);
  Result := ERGB(pR * GetRValue(Result), pG * GetGValue(Result), pB * GetBValue(Result));

end;

function ColorToGrey(SC : TColor) : TColor; 
var
	avg : Integer;
begin
	avg := Round((GetRValue(SC) * 20 + GetGValue(SC) * 50 + GetBValue(SC)*30)/100);
  Result := RGB(avg, avg, avg);
end;

function QBColor (n:Integer):TColor;
var
   C:TColor;
begin
     case n of
          0: C := 0;
          1: C := clNavy;
          2: C := 32768;
          3: C := 8421376;
          4: C := 128;

          5: C := 8388736;
          6: C := 32896;
          7: C := 12632256;

          8: C := 8421504;
          9: C := 16711680;
          10: C := 65280;
          11: C := 16776960;
          12: C := 255;
          13: C := 16711935;
          14: C := clYellow;
          15: C := clWhite;
     end;
     Result := C;
end;

function HexToColor(Hex:String) : TColor; 
var
	RHex : string;
begin
	RHex := '$' + Hex;
  Result := StrToInt(RHex);
end;

function HexToInt(Hex:String) : Integer; 
var
	RHex : string;
begin
	RHex := '$' + Hex;
  Result := StrToInt(RHex);
end;

function BrightenColor(BaseColor: TColor; Adjust : Integer): TColor;
begin
	BaseColor := ColorToRGB(BaseColor);
	Result := ERGB(GetRValue(BaseColor) + Adjust, GetGValue(BaseColor) + Adjust,
  	GetBValue(BaseColor) + Adjust);
end;

function DarkenColor(BaseColor: TColor; Adjust : Integer): TColor;
begin
  Result := BrightenColor(BaseColor, -Adjust);
end;

end.




