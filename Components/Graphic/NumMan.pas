
{ ---------------------------------------------------
		Numbers Manager  Copyright (r) by DreamFactory
    Version : 1.75   Author : William Yang
		Last Update 24 - Aug - 97
  --------------------------------------------------- }

unit NumMan;

interface

uses Classes, SysUtils, Windows;

function MakeBetween(S, nFrom, nTo : Integer) : Integer;
function Between(S, N1, N2 : Integer) : Boolean;
function Min(X, Y: Integer): Integer;
function Max(X, Y: Integer): Integer;
function RectWidth(Rect: TRect) : Integer;
function RectHeight(Rect: TRect) : Integer;
function MinMost(Nums: array of Integer): Integer;
function MaxMost(Nums: array of Integer): Integer;
function AllEqual(Nums: array of Integer): Boolean;
//Check if these numbers in the range
function AllBetween(Nums: array of Integer; nFrom, nTo : Integer): Boolean;
{Check if the numbers are like (1, 2, 3, 4, 5),
you can set InOrder to false if you want check(4,2,3,5,1) }
function IsIncreasement(Nums: array of Integer; InOrder: Boolean): Boolean;
{more customisable with amount that increase }
function IsIncreasementExt(Nums: array of Integer; InOrder: Boolean;
	Incs: Integer): Boolean;
//Find a number an array of numbers, returns the index of the first catch.
function FindNum(Num: Integer; Nums: array of Integer): Integer;
//Find pairs, returns the total amount of pairs.
function FindPairs(Nums: array of Integer): Integer;
//Find the how many times the number appears.
function NumAppears(Num: Integer; Nums: array of Integer): Integer;

function ReadBits(Num, Start, Count: Integer): Integer;
function MaxBits(Num: Integer): Integer;
function IntToBin(Num: Integer): String;
function WriteBits(Num, Start, Val: Integer): Integer;
procedure ISwap(var n1, n2: Integer);
procedure BSwap(var n1, n2: Byte);
procedure FSwap(var n1, n2: Double);

implementation

procedure ISwap(var n1, n2: Integer);
var
	t: Integer;
begin
	t := n1;
  n1 := n2;
  n2 := t;
end;

procedure BSwap(var n1, n2: Byte);
var
	t: Byte;
begin
	t := n1;
  n1 := n2;
  n2 := t;
end;

procedure FSwap(var n1, n2: Double);
var
	t: Double;
begin
	t := n1;
  n1 := n2;
  n2 := t;
end;

function WriteBits(Num, Start, Val: Integer): Integer;
var
	BitMask: Integer;
  i, Max: Integer;
begin
	Max := MaxBits(Num);
  BitMask := 0;
  Val := Val shl (Start - 1);
  Result := Num or Val;
end;

function MaxBits(Num: Integer): Integer;
begin
  Result := 0;
	repeat
  	Num := Num shr 1;
    Inc(Result);
	until Num <= 0;
end;

function IntToBin(Num: Integer): String;
var
	Mask: Integer;
  i, Bits: Integer;
begin
  Result := ''; Mask := 1;
  Bits := MaxBits(Num);
	for i := 1 to bits do
  begin
    if (Num and Mask) = Mask then
    	Result := Result + '1'
    else
    	Result := Result + '0';
		Mask := Mask shl 1;
  end;
end;

function ReadBits(Num, Start, Count: Integer): Integer;
var
	BitMask: Integer;
  i, Max: Integer;
begin
	Max := MaxBits(Num);
  {
       0000 1111
  and  1011 0111
  ---- ---- ----
			 0000 0111
  }
  //Initialize Bitmask with 0.
  BitMask := 0;
	for i := Max downto 1 do
  begin
    if (i >= Start) and (i <= Start + Count - 1) then
    begin
    	Bitmask := Bitmask or 1;
    end;
		if i > 1 then
    begin
    	BitMask := BitMask shl 1;
    end;
  end;
	Result := BitMask and Num;
  Result := Result shr (Start - 1)
end;

function FindPairs(Nums: array of Integer): Integer;
var
	i: Integer;
begin
	Result := 0;
	for i := Low(Nums) to High(Nums) do
  begin
		if NumAppears(Nums[i], Nums) = 2 then
    	Inc(Result);
  end;
  Result := Result div 2;
end;

function FindNum(Num: Integer; Nums: array of Integer): Integer;
var
	i:Integer;
begin
	Result := -1;
	for i := Low(Nums) to High(Nums) do
  begin
  	if Nums[i] = Num then
    begin
    	Result := i;
      Exit;
    end;
  end;
end;

function NumAppears(Num: Integer; Nums: array of Integer): Integer;
var
	i:Integer;
begin
	Result := 0;
	for i := Low(Nums) to High(Nums) do
  begin
  	if Nums[i] = Num then
    begin
    	Inc(Result);
    end;
  end;
end;

function IsIncreasementExt(Nums: array of Integer; InOrder: Boolean;
	Incs: Integer): Boolean;
var
	i,j, k : Integer;
begin
	Result := True;
  if InOrder then
  begin
  	j := Nums[Low(Nums)] + Incs;
  	for i := Low(Nums) + 1 to High(Nums) do
    begin
    	if Nums[i] <> J then
      begin
      	Result := False;
      	Exit;
      end;
    	Inc(j, Incs);
    end;
  end
  else
  begin
  	k := MinMost(Nums);
    //Get the smallest number to start with.
    j := k + Incs;
  	while (FindNum(j, Nums) <> - 1) do
    begin
	    Inc(j, Incs);
    end;
    //if j is equal to the total increasement + minmost value.
    if j = k + (High(Nums) - Low(Nums)) * Incs then
    	Result := True
    else
    	Result := False;
  end;
end;

function IsIncreasement(Nums: array of Integer; InOrder: Boolean): Boolean;
begin
	Result := IsIncreasementExt(Nums, InOrder, 1);
end;

function AllBetween(Nums: array of Integer; nFrom, nTo : Integer): Boolean;
var
	i:Integer;
begin
	Result := True;
	for i := Low(Nums) to High(Nums) do
  begin
  	if not Between(Nums[i], nFrom, nTo) then
    begin
			Result := False;
      Exit;
    end;
  end;

end;

function AllEqual(Nums: array of Integer): Boolean;
var
	i : Integer;
begin
	Result := True;
	for i := Low(Nums) + 1 to High(Nums) do
  begin
  	if Nums[Low(Nums)] <> Nums[i] then
    begin
    	Result := False;
      Exit;
    end;
  end;
end;

function MinMost(Nums: array of Integer): Integer;
var
	i,j, k : Integer;
begin
	//Go through each numbers.
	for i := Low(Nums) to High(Nums) do
  begin
		k := 0;
    //check if this number is smaller than others
    for j := Low(Nums) to High(Nums) do
  	begin
  		if (Nums[i] <= Nums[j]) and (i <> j) then
      	Inc(k);
  	end;
		{If there is 5 numbers, if a number smaller than other 4
    then it is the smallest}
	  if k = High(Nums) - Low(Nums) then
    	Result := Nums[i];
  end;
end;

function MaxMost(Nums: array of Integer): Integer;
var
	i,j, k : Integer;
begin
	for i := Low(Nums) to High(Nums) do
  begin
		k := 0;
    for j := Low(Nums) to High(Nums) do
  	begin
  		if (Nums[i] >= Nums[j]) and (i <> j) then
      	Inc(k);
  	end;
	  if k = High(Nums) - Low(Nums) then
    	Result := Nums[i];
  end;
end;

function RectWidth(Rect: TRect) : Integer;
begin
	Result := Rect.Right - Rect.Left;
end;

function RectHeight(Rect: TRect) : Integer;
begin
	Result := Rect.Bottom - Rect.Top;
end;

function Min(X, Y: Integer): Integer;
begin
	if X < Y then Result := X else Result := Y;
end;

function Max(X, Y: Integer): Integer;
begin
	if X > Y then Result := X else Result := Y;
end;


function Between(S, N1, N2 : Integer) : Boolean;
begin
  if (S >= N1) and (S <= N2) then
    Result := True
  else
    Result := False;
end;

function MakeBetween(S, nFrom, nTo : Integer) : Integer;
begin
  Result := S;
  while Result < nFrom do
  begin
    Result := Result + (nTo - nFrom);
  end;
  while Result > nTo do
  begin
    Result := Result - (nTo - nFrom);
  end;
end;


end.
