unit VBitmap;

interface

uses SysUtils, Windows, Numman, StrMan, Classes, Graphics;

type

	TColorTable = array[0..MaxInt div SizeOf(TRGBQUAD) - 1] of TRGBQUAD;
  PColorTable = ^TColorTable;

	TVirtualBitmap=class(TObject)
	private
		{ Private declarations }
    fBrush: TBrush;
    fPen: TPen;
		fWidth: Integer;  {Width of the bitmap}
		fHeight: Integer; {Height of the bitmap}
		fColors: Integer; {Colors count}
    fColorTable: PColorTable; {Only < 8bit bitmap can use color table}
    ClrTableSize: Integer;
    ColorSize: Single;
    fSkip: Integer;
    fAntialiased: Boolean;
    { in another words they uses palette }
		fBits: Integer; {Indicates the size of each color}
		fContent: TMemoryStream; {Contain the whole bitmap}
		procedure SetWidth( Value: Integer );
		procedure SetHeight( Value: Integer );
		procedure SetColor(i: Byte; Value: TColor );
		function  GetColor(i: Byte): TColor;
		procedure SetBits( Value: Integer );
		function CheckTable: Boolean;
		procedure SetPen(Val: TPen);
		procedure SetBrush(Val: TBrush);
    procedure GotoXY(x, y: Integer);
    procedure ValidXY(var x1, y1, x2, y2: Integer);
	protected
		{ Protected declarations }
    procedure RecreateContent; virtual;
	public
		{ Public declarations }
		constructor Create ( AWidth, AHeight, ABits: Integer );
		constructor CreateFromBitmap ( Bitmap: TBitmap );
		procedure LoadFromStream ( Stream: TStream );
		destructor Destroy;
		function GetPixel (x, y :Integer ): Integer;
		procedure SetPixel (x, y :Integer; Color: Integer );
    procedure ProduceBitmap(Bitmap: TBitmap);
		function GetNearestColor(Color: Integer): Integer;
    procedure Draw(x, y: Integer; Src: TVirtualBitmap);
    procedure Line(x1, y1, x2, y2: Integer);
    procedure Rectangle(x1, y1, x2, y2: Integer);

    property Content: TMemoryStream read fContent;
    property Pen: TPen read fPen write SetPen;
    property Brush: TBrush read fBrush write SetBrush;
		property Width: Integer read fWidth write SetWidth;
		property Height: Integer read fHeight write SetHeight;
		property Colors: Integer read fColors;
    property AntiAliase: Boolean read fAntialiased write fAntialiased;
		property Bits: Integer read fBits write SetBits;
    property ColorTable[i: Byte]: TColor read GetColor write SetColor;
	published
		{ Published declarations }
	end;

implementation

{ TVirtualBitmap }
constructor TVirtualBitmap.Create ( AWidth, AHeight, ABits: Integer );
begin
	inherited Create;
  fContent := TMemoryStream.Create;
	fWidth := AWidth;
  fHeight := AHeight;
  fBits := ABits;
 	fColors := 1 shl Bits;
  RecreateContent;
//  FillChar(fContent.Memory, 0);
  fBrush := TBrush.Create;
  fPen := TPen.Create;
end;

constructor TVirtualBitmap.CreateFromBitmap ( Bitmap: TBitmap );
var
	Stream: TMemoryStream;
begin
	inherited Create;
  fContent := TMemoryStream.Create;
  Stream := TMemoryStream.Create;
  Bitmap.SaveToStream(Stream);
  {ensure the stream is started from beginning}
  Stream.Seek(0, 0);
	LoadFromStream(Stream);
  Stream.Free;
  fBrush := TBrush.Create;
  fPen := TPen.Create;
end;

procedure TVirtualBitmap.ValidXY(var x1, y1, x2, y2: Integer);
begin
  if x1 > x2 then ISwap (x1, x2);
  if y1 > y2 then ISwap (y1, y2);
	x1 := Max(x1, 0); x2 := Min(x2, fWidth);
	y1 := Max(y1, 0); y2 := Min(y2, fHeight);
end;

procedure TVirtualBitmap.LoadFromStream ( Stream: TStream );
var
  Bmh: TBitmapInfoHeader;
  Bmf: TBitmapFileHeader;
  Bmc: TBITMAPCOREHEADER;
  Size: Integer;
begin
	{read bitmap file information header}
	Stream.Read(Bmf, SizeOf(Bmf));
  {check size}
  Stream.Read(Size, SizeOf(Size));
  {Go back}
  Stream.Seek(-SizeOf(Size), 1);
  if Size = SizeOf(Bmh) then
  begin
  	Stream.Read(Bmh, SizeOf(Bmh));
    fWidth := Bmh.biWidth;
    fHeight := Bmh.biHeight;
    fBits := Bmh.biBitCount;
  end
  else if Size = SizeOf(Bmc) then
  begin
  	Stream.Read(Bmc, SizeOf(Bmc));
    fWidth := Bmc.bcWidth;
    fHeight := Bmc.bcHeight;
    fBits := Bmc.bcBitCount;
  end
  else raise Exception.Create('VirtualBitmap: Invalid bitmap.');
 	fColors := 1 shl fBits;
	RecreateContent;
  if fBits <= 8 then
  begin
  	if Assigned(fColorTable) then FreeMem(fColorTable);
  	ClrTableSize := fColors * SizeOf(TRGBQUAD);
		GetMem(fColorTable, ClrTableSize);
    Stream.Read(fColorTable^, ClrTableSize);
  end;
  {Recreate/resize the content stream}
	fContent.SetSize(Bmh.biSizeImage);
  Stream.Read(fContent.Memory^, fContent.Size);
  fSkip := (Bmh.biSizeImage div (fBits div 8))  - (fWidth*fHeight)
end;

destructor TVirtualBitmap.Destroy ;
begin
	fContent.Free;
  fBrush.Free;
  fPen.Free;
	if Assigned(fColorTable) then FreeMem(fColorTable);
  inherited Destroy;
end;

procedure TVirtualBitmap.Draw(x, y: Integer; Src: TVirtualBitmap);
var
	i, j: Integer;
  x1, y1, x2, y2: Integer;
begin
	x1 := x; x2 := x + fWidth;
  y1 := y;  y2 := y + fHeight;
	ValidXY(x1, y1, x2, y2);
  for i := x1 to x2 do
  	for j := y1 to y2 do
    begin
    	SetPixel(i, j, Src.GetPixel(i-x1 , j-y1));
    end;
end;

procedure TVirtualBitmap.GotoXY(x, y: Integer);
var
	Pos: Integer;
begin
	Pos:= (y*(fWidth+1)+x)  * SizeOf(TRGBTRIPLE);
	fContent.Seek(Pos, 0);
end;

procedure TVirtualBitmap.Line(x1, y1, x2, y2: Integer);
var
	k, i, j : Integer;
  d, z: Single;
  x, y: Single;
begin
	ValidXY(x1, y1, x2, y2);
  if x1 = x2 then
  	for i := y1 to y2 do SetPixel(x1, i, Pen.Color)
  else if y1 = y2 then
  	for i := x1 to x2 do SetPixel(i, y1, Pen.Color)
  else
  begin
  	z := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));
    d := (X2 - X1) / (Y2 - Y1);
    j := Trunc(z);
  	for i := 1 to j do
  	begin
			x := Sqrt(Sqr(z)/(Sqr(d)+1));
      y := d * x;
      SetPixel(Trunc(X1+X), Trunc(Y1+Y), Pen.Color);
  	end;
  end;
end;

procedure TVirtualBitmap.Rectangle(x1, y1, x2, y2: Integer);
begin
	ValidXY(x1, y1, x2, y2);
	Line(x1, y1, x1, y2);
	Line(x1, y1, x2, y1);
	Line(x2, y1, x2, y2);
	Line(x1, y2, x2, y2);
end;

procedure TVirtualBitmap.RecreateContent;
begin
		if fBits <= 8 then
  	begin
      ColorSize := fBits/8;
  		if Assigned(fColorTable) then FreeMem(fColorTable);
      GetMem(fColorTable, fColors * SizeOf(TRGBQUAD));
	 		case fBits of
			1: fContent.SetSize(fWidth * fHeight div 8);
			4: fContent.SetSize(fWidth * fHeight div 2);
			8: fContent.SetSize(fWidth * fHeight);
    	end;
  	end
  	else
  	begin
			ColorSize := SizeOf(TRGBTRIPLE);
  		if Assigned(fColorTable) then FreeMem(fColorTable);
			fContent.SetSize((fWidth+1)*(fHeight+1)*3);
  	end;
end;

function TVirtualBitmap.GetNearestColor(Color: Integer): Integer;
var
	j: Integer;
  Gap: Integer;
  Cur: Integer;
  Si: Integer;
begin
	if fBits > 8 then Exit;
  Gap := 1 shl SizeOf(Gap) * 8;
	for j := 0 to fColors do
  begin
  	Cur := Abs(Color - GetColor(j));
    if Cur = 0 then
    begin
    	Result := GetColor(j);
      Exit;
    end;
    if Cur < Gap then
    begin
    	Gap := Cur;
      Si := j;
    end;
  end;
  Result := GetColor(Si);
end;

function TVirtualBitmap.GetPixel ( x,y :Integer ):Integer;
var
	Pos: Integer;
  SPos: Single;
  RGB3: TRGBTriple;
  ClrIndex: Byte;
begin
	if fBits <= 8 then
  begin
  	SPos := (X*fWidth + y*fHeight) * fBits / 8;
    {while the bits is less 8}
    fContent.Seek(Trunc(SPos), 0);
    {I read the a byte }
		fContent.Read(ClrIndex, 1);
		{and I use bitwise workout the data}
    if fBits < 8 then
    begin
    	if Frac(SPos)=0 then
    		ClrIndex := ReadBits(ClrIndex, 1, fBits)
    	else
    		ClrIndex := ReadBits(ClrIndex, Round(8/(1/Frac(SPos))), fBits);
    end;
		Result := GetColor(ClrIndex);
  end
  else
  begin
		Pos:= ((fWidth+1)*(fHeight+1)  - (y*(fWidth+1)) + x) * SizeOf(TRGBTRIPLE);
		fContent.Seek(Pos, 0);
  	fContent.Read(RGB3, SizeOf(RGB3));
    Result := RGB(RGB3.rgbtRed, RGB3.rgbtGreen, RGB3.rgbtBlue);
  end;
end;

function MC(C1, C2 : TColor; Grade: Byte) : TColor;
var
	pR, pG, pB : Real;
begin
  pR := (GetRValue(C1) * Grade / 100 + GetRValue(C2) * (100-Grade) / 100);
  pG := (GetGValue(C1) * Grade / 100 + GetGValue(C2) * (100-Grade) / 100);
  pB := (GetBValue(C1) * Grade / 100 + GetBValue(C2) * (100-Grade) / 100);
  Result := RGB(Trunc(pR), Trunc(pG), Trunc(pB));
end;

procedure TVirtualBitmap.SetPixel (x,y :Integer; Color: Integer );
var
	Pos: Integer;
  SPos: Single;
  ClrIndex: Byte;

	procedure Write16BitColor(i, j, C: Integer);
  var
    RGB3 : TRGBTRIPLE;
  begin
		Pos:= ((fWidth+1)*(fHeight+1)  - (y*(fWidth+1)) + x) * SizeOf(TRGBTRIPLE) + fSkip;
		fContent.Seek(Pos, 0);
    RGB3.rgbtRed := GetRValue(C);
    RGB3.rgbtBlue := GetBValue(C);
    RGB3.rgbtGreen := GetGValue(C);
    fContent.Write(RGB3, 3);
{  	fContent.Write(RGB3.rgbtBlue, 1);
  	fContent.Write(RGB3.rgbtGreen, 1);
  	fContent.Write(RGB3.rgbtRed, 1);}
  end;

begin
	if fBits <= 8 then
  begin
  	Color := GetNearestColor(Color);
  	SPos := (X*fWidth + y*fHeight) * fBits / 8;
    {while the bits is less 8}
    fContent.Seek(Trunc(SPos), 0);
    {I read the a byte }
		fContent.Read(ClrIndex, 1);
		{and I use bitwise workout the data}
    if fBits < 8 then
    begin
    	if Frac(SPos)=0 then
				ClrIndex := WriteBits(ClrIndex, 1, Color)
    	else
    		ClrIndex := WriteBits(ClrIndex, Round(8/(1/Frac(SPos))), Color);
    end;
    fContent.Seek(-1, 1);
    fContent.Write(ClrIndex, 1);
  end
  else
  begin
  	Write16BitColor(x, y, Color);
    if fAntialiased then
    begin
    	// Set the pixels around with 50%
    	// Top Pixel
  		Write16BitColor(x, y-1, MC(GetPixel(x, y-1), Color, 50));
     	// Bottom Pixel
  		Write16BitColor(x, y+1, MC(GetPixel(x, y+1), Color, 50));
     	// Left Pixel
  		Write16BitColor(x-1, y, MC(GetPixel(x-1, y), Color, 50));
     	// Right Pixel
  		Write16BitColor(x+1, y, MC(GetPixel(x+1, y), Color, 50));

      // Set the pixels at the corners with 33%
      // Top Left Pixel
  		Write16BitColor(x-1, y-1, MC(GetPixel(x-1, y-1), Color, 30));
      // Top Right Pixel
  		Write16BitColor(x+1, y-1, MC(GetPixel(x+1, y-1), Color, 30));
      // Bottom Left Pixel
      Write16BitColor(x-1, y+1, MC(GetPixel(x-1, y+1), Color, 30));
      // Bottom Right Pixel
  		Write16BitColor(x+1, y+1, MC(GetPixel(x+1, y+1), Color, 30));
    end;
  end;
end;

procedure TVirtualBitmap.SetWidth( Value: Integer );
begin
	if fWidth <> Value then
	begin
		fWidth := Value;
		ReCreateContent;
	end;
end;

procedure TVirtualBitmap.SetPen(Val: TPen);
begin
	if fPen <> Val then
	begin
		fPen.Assign(Val);
	end;
end;

procedure TVirtualBitmap.SetBrush(Val: TBrush);
begin
	if fBrush <> Val then
	begin
		fBrush.Assign(Val);
	end;
end;

procedure TVirtualBitmap.SetHeight( Value: Integer );
begin
	if fHeight <> Value then
	begin
		fHeight := Value;
		ReCreateContent;
	end;
end;

procedure TVirtualBitmap.SetBits( Value: Integer );
begin
	if fBits <> Value then
	begin
		{256 colors or less bitmap they use RGBQuad }
		{16bit colors or more bitmap they use RGBTriple }
		fBits := Value;
    RecreateContent;
	end;
end;

function TVirtualBitmap.CheckTable: Boolean;
begin
	Result := False;
  if not Assigned(fColorTable) then raise Exception.Create('VirtualBitmap: Color table is' +
  	'not avialable for 16bit or 24bit bitmaps.')
  else
  	Result := True;
end;

procedure TVirtualBitmap.SetColor(i: Byte; Value: TColor );
begin
	if CheckTable and Between(i, 0, fColors-1) then
  begin
   	fColorTable^[i].rgbRed := GetRValue(Value);
   	fColorTable^[i].rgbBlue := GetBValue(Value);
   	fColorTable^[i].rgbGreen := GetGValue(Value);
   	fColorTable^[i].rgbReserved := 0;
  end;
end;

function TVirtualBitmap.GetColor(i: Byte): TColor;
begin
	if CheckTable and Between(i, 0, fColors-1) then
  begin
   	Result := RGB(fColorTable^[i].rgbRed,
    		fColorTable^[i].rgbGreen, fColorTable^[i].rgbBlue);
  end;
end;

procedure TVirtualBitmap.ProduceBitmap(Bitmap: TBitmap);
var
	Stream: TMemoryStream;
  BmpSize: Integer;
  Bmh: TBitmapInfoHeader;
  Bmf: TBitmapFileHeader;
begin
	Stream := TMemoryStream.Create;
  BmpSize := SizeOf(Bmh) + SizeOf(Bmf) + fContent.Size;
	if fBits <= 8 then Inc(BmpSize, fColors * SizeOf(TRGBQUAD));
	Stream.SetSize(BmpSize);
  FillChar(BMF, sizeof(BMF), 0);
  FillChar(BMH, sizeof(BMH), 0);
  BMF.bfType := $4D42;
  BMF.bfSize := SizeOf(BMF);
  BMF.bfOffBits := fContent.Size;
  BMH.biWidth := fWidth;
  BMH.biHeight := fHeight;
  BMH.biBitCount := fBits;
  BMH.biSize := SizeOf(BMH);
	BMH.biPlanes := 1;
  BMH.biSizeImage := fContent.Size;
  Stream.Write(bmf, Sizeof(BMF));
	Stream.Write(Bmh, SizeOf(Bmh));
  if fBits <= 8 then Stream.WriteBuffer(fColorTable^, 4 * fColors);
  Stream.WriteBuffer(fContent.Memory^, fContent.Size);
  Stream.Seek(0, 0);
  Bitmap.LoadFromStream(Stream);
  Stream.Free;
end;

end.
