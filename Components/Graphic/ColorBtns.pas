{----------------------------------------------------------}
{   TColor95Button - Fully Multimedia Button               }
{   Programmed by William Yang, yang@btinternet.com        }
{   Some Parts are From Delphi Visual Component Library    }
{----------------------------------------------------------}
{   Version : 1.90.                                        }
{----------------------------------------------------------}
unit ColorBtns;

{$S-,W-,R-}
{$C PRELOAD}

interface

uses SysUtils, Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl, DrawMan, APIMan, VBitmap, ClrPanel, Gradient;

type
  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);
  TClr95ButtonStyle = (bsWin31, bsNew, bsEllipse, bsTab, bsIE4,
  	bsSideDownTab, bsGlass, bsFlat, bsOwnerDraw, bsMicroposeGame, bsGradient);
  TButtonOptions = set of (boShowMouseFocus, boRepeatable, boDitherPattern,
  	boAutoBlackWhiteText);

  TNumGlyphs = 1..4;
  TClrBtnOwnerDrawEvent = procedure (Sender: TObject; ACanvas: TCanvas; var BtnRect: TRect) of object;
  TColor95Button = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FColor: TColor;
    FPattern: TBitmap;
    FGlyph: Pointer;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseOver : Boolean;
    FOwnerDraw: TClrBtnOwnerDrawEvent;
    FStyle: TClr95ButtonStyle;
    FSndClick: TFilename;
    FSndMouseOver : TFilename;
		fOptions: TButtonOptions;

    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Message: TMessage); message CM_MouseLeave;
    procedure SetStyle(Value: TClr95ButtonStyle);

    procedure SetOptions(Value: TButtonOptions);
  protected
    FState: TButtonState;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property State: TButtonState read FState;
    procedure OnButtonRepeat(Sender: TObject);
  published

    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property SoundClick: TFilename read FSndClick write FSndClick;
    property SoundMouseOver : TFilename read FSndMouseOver write FSndMouseOver;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property Color: TColor read FColor write SetColor;
    property Style: TClr95ButtonStyle read FStyle write SetStyle;
    property Options: TButtonOptions read fOptions write SetOptions;

    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
    property OnOwnerDraw: TClrBtnOwnerDrawEvent read FOwnerDraw write FOwnerDraw;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
  Style: TClr95ButtonStyle; IsDesign, IsDown,
  IsFocused: Boolean; FaceColor: TColor): TRect;

implementation

uses Consts, Colorman;

{$R ClrBtns.Res}

//Register procedure
procedure Register;
begin
	RegisterComponents('Grafix', [TColor95Button]);
end;

{ DrawButtonFace - returns the remaining usable area inside the Client rect.}
function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
  Style: TClr95ButtonStyle; IsDesign, IsDown,
  IsFocused: Boolean; FaceColor: TColor): TRect;
const
	Corner = 3;
var
  R: TRect;
  clHigh, clLow, clSHBlack, clFace: TColor;
  Angle : Double;
  BlackEdge, GreyEdge, LightEdge, WhiteEdge: TColor;
  X1, X2, Y1, Y2: Integer;
  Offset: Integer;
begin
//  NewStyle := ((Style = bsAutoDetect) and NewStyleControls) or (Style = bsNew);
  FaceColor := ColorToRGB(FaceColor);
  R := Client;
  X1 := R.Left; X2 := R.Right;
  Y1 := R.Top; Y2 := R.Bottom;
 	Offset := 2;
	if IsDown then
  begin
   	clHigh := DarkenColor(FaceColor, 48);
    clLow := BrightenColor(FaceColor, 48);
    clSHBlack := BrightenColor(FaceColor, 64);
    clFace := DarkenColor(FaceColor, 96);
  end
  else
  begin
 		clHigh := BrightenColor(FaceColor, 60);
    clLow := DarkenColor(FaceColor, 48);
    clSHBlack := DarkenColor(FaceColor, 100);
    clFace := BrightenColor(FaceColor, 12);
	end;
  with Canvas do
  begin
    if Style=bsNew then
    begin
  		Brush.Color := FaceColor;
  		Brush.Style := bsSolid;
  		FillRect(R);
      Brush.Color := FaceColor;
      Brush.Style := bsSolid;
   		Frame3D(Canvas, R, clFace, clSHBlack, 1);
   		Frame3D(Canvas, R, clHigh, clLow, 1);
    end
    else if Style=bsWin31 then
    begin
  		Brush.Color := FaceColor;
    	Brush.Style := bsClear;
    	FillRect(R);
			Offset := 3;
      Pen.Color := DarkenColor(FaceColor, 126);
      Brush.Color := FaceColor;
      Brush.Style := bsSolid;
      Rectangle(X1, Y1, X2, Y2);
      { round the corners - only applies to Win 3.1 style buttons }
      Pixels[X1, Y1] := FaceColor;
      Pixels[X1, Y2 - 1] := FaceColor;
      Pixels[X2 - 1, Y1] := FaceColor;
      Pixels[X2 - 1, Y2 - 1] := FaceColor;
      InflateRect(R, -1, -1);
      if not IsDown then
        Frame3D(Canvas, R, BrightenColor(FaceColor, 48),
        	DarkenColor(FaceColor, 48), 2)
      else
      begin
        Pen.Color := DarkenColor(FaceColor, 48);
        PolyLine([Point(X1, Y2 - 1), Point(X1, Y1),
          Point(X2, Y1)]);
      end;
    end
    else if Style=bsEllipse then
    begin
    	Brush.Style := bsClear;
    	FillRect(R);
      Brush.Color := FaceColor;
    	Brush.Style := bsSolid;
      Ellipse(X1+2, Y1+2, X2-2, Y2-2);
    	Brush.Style := bsClear;
    	Pen.Width := 2;
    	Pen.Color := clHigh;
    	Arc(X1+2,Y1+2,X2-2,Y2-2,X2 div 5 * 4,Y2 div 5,X2 div 5,Y2 div 5 * 4);
      if IsDown then Pen.Color := clSHBlack else Pen.Color := clLow;
     	Arc(X1+2,Y1+2,X2-2,Y2-2,X2 div 5,Y2 div 5 * 4,X2 div 5 * 4,Y2 div 5);

    	Pen.Width := 1;
    	Pen.Color := clFace;
    	Arc(X1+2,Y1+2,X2-2,Y2-2,X2 div 5 * 4,Y2 div 5,X2 div 5,Y2 div 5 * 4);
      if IsDown then Pen.Color := clLow else Pen.Color := clSHBlack;
     	Arc(X1+2,Y1+2,X2-2,Y2-2,X2 div 5,Y2 div 5 * 4,X2 div 5 * 4,Y2 div 5);
    end
    else if Style=bsTab then
    begin
   		Brush.Color := FaceColor;
  		Brush.Style := bsSolid;
  		FillRect(R);

  		Pen.Color := BrightenColor(FaceColor, 24);
  		Polyline([Point(X1,Y2-2), Point(X1,Y1+2), Point(X1 + 2,Y1),
      		Point(X2-2, Y1), Point(X2,Y1+2)]);

      Pen.Style := psSolid;
  		Pen.Color := BrightenColor(FaceColor, 64);
  		Polyline([Point(X1+1,Y2-3), Point(X1+1,Y1+3), Point(X1 + 3,Y1+1),
      		Point(X2-3, Y1+1), Point(X2-1,Y1+3)]);
      //Add two shadow lines.
			Pen.Color := DarkenColor(FaceColor, 48);
      Polyline([Point(X2-2, Y2-4), Point(X2-2,Y1+4)]);

			Pen.Color := DarkenColor(FaceColor, 128);
      Polyline([Point(X2-1, Y2-3), Point(X2-1,Y1+3)]);
      //If is not pressed down then draw then lower edge.
  		if not IsDown then
      begin
      	Pen.Color := BrightenColor(FaceColor, 64);
        MoveTo(X1, Y2-2);
        LineTo(X2, Y2-2);
      end;
    end
    else if Style = bsSideDownTab then
    begin
   		Brush.Color := FaceColor;
  		Brush.Style := bsSolid;
  		FillRect(R);
      //Polyline is faster than Moveto and LineTo
  		Pen.Color := BrightenColor(FaceColor, 24);
  		Polyline([Point(X1,Y1+2), Point(X1,Y2-2),
      	Point(X1 + 2,Y2)]);
      Pen.Style := psSolid;
  		Pen.Color := BrightenColor(FaceColor, 64);
  		Polyline([Point(X1+1,Y1+3), Point(X1+1,Y2-3),
      	Point(X1 + 3,Y2-1)]);
      //Add two shadow lines.
			Pen.Color := DarkenColor(FaceColor, 48);
      Polyline([Point(X1+2,Y2-4), Point(X1 + 4,Y2-2),
      		Point(X2-4, Y2-2), Point(X2-2, Y2-4),
          Point(X2-2,Y1+3)]);

			Pen.Color := DarkenColor(FaceColor, 128);
      Polyline([Point(X1+1,Y2-3), Point(X1 + 3,Y2-1),
      		Point(X2-3, Y2-1), Point(X2-1, Y2-3),
          Point(X2-1,Y1+2)]);
      //If is not pressed down then draw then lower edge.
  		if not IsDown then
      begin
      	Pen.Color := DarkenColor(FaceColor, 64);
        MoveTo(X1, Y1+2);
        LineTo(X2, Y1+2);
      end;
    end else if Style = bsGlass then
    begin
   		BlackEdge := DarkenColor(FaceColor, 128);
			GreyEdge := DarkenColor(FaceColor, 64);
  		LightEdge := FaceColor;
  		WhiteEdge := clWhite;
    	Offset := 3;
			with Canvas do
  		begin
  			Brush.Color := WhiteEdge;
    		FillRect(R);
    		Brush.Style := bsClear;
    		if not IsDown then
    		begin
        {Draw upper button}
      		Pen.Color := BlackEdge;
      		Polyline([Point(X1, Y2-1), Point(X2-1, Y2-1), Point(X2-1, Y1 - 1)]);
      		Pen.Color := GreyEdge;
      		Rectangle(X1, Y1, X2 - 1, Y2 - 1);
      		Pen.Color := LightEdge;
          Rectangle(X1+1, Y1+1, X2 - 2, Y2 - 2);
    		end
    		else
    		begin
    			{Draw Down button}
      		Pen.Color := GreyEdge;
      		Rectangle(X1+1, Y1+1, X2 - 1, Y2 - 1);
      		Pen.Color := LightEdge;
      		Rectangle(X1, Y1, X2, Y2);
      		Pen.Color := BlackEdge;
      		PolyLine([Point(X2 - 2, Y1 + 2), Point(X1+2 , Y1+2), Point(X1+2, Y2- 2)]);
    		end;
    	end;
    end else if Style = bsFlat then
    begin
	   	Offset := 1;
    	Brush.Color := FaceColor;
   		Frame3D(Canvas, R, clHigh, clLow, 1);
     	FillRect(R);
    end else if Style = bsIE4 then
    begin
    	Offset := 1;
    	Brush.Style := bsClear;
   		if IsDown or IsFocused or (IsDesign) then
	   		Frame3D(Canvas, R, clHigh, clSHBlack, 1);
      InflateRect(R, -1, -1);
//   		FillRect(R);
    end else if Style = bsMicroposeGame then
    begin
    	Brush.Style := bsClear;
    	Offset := 1;
   		FillRect(R);
      Brush.Style := bsSolid;
      Brush.Color := FaceColor;
      Pen.Style := psClear;
      Inc(X1); Inc(Y1); Dec(Y2, 2); Dec(X2, 2);
			RoundRect(X1, Y1, X2, Y2, Corner, Corner);
      Pen.Style := psSolid;
      Pen.Width := 2;
			MoveTo(X2, Y1+Corner);
      if IsDown then
      begin
   			Pen.Color := BrightenColor(FaceColor, 24);
      	LineTo(X2-Corner, Y1);
   			Pen.Color := DarkenColor(FaceColor, 48);
      	LineTo(X1+Corner, Y1);
   			Pen.Color := DarkenColor(FaceColor, 64);
      	LineTo(X1, Y1+Corner);
   			Pen.Color := DarkenColor(FaceColor, 48);
      	LineTo(X1, Y2-Corner);
   			Pen.Color := BrightenColor(FaceColor, 24);
      	LineTo(X1+Corner, Y2);
   			Pen.Color := BrightenColor(FaceColor, 48);
      	LineTo(X2-Corner, Y2);
   			Pen.Color := BrightenColor(FaceColor, 64);
      	LineTo(X2, Y2-Corner);
   			Pen.Color := BrightenColor(FaceColor, 48);
      	LineTo(X2, Y1+Corner);
      end
      else
      begin
   			Pen.Color := DarkenColor(FaceColor, 24);
      	LineTo(X2-Corner, Y1);
   			Pen.Color := BrightenColor(FaceColor, 48);
      	LineTo(X1+Corner, Y1);
   			Pen.Color := BrightenColor(FaceColor, 64);
      	LineTo(X1, Y1+Corner);
   			Pen.Color := BrightenColor(FaceColor, 48);
      	LineTo(X1, Y2-Corner);
   			Pen.Color := DarkenColor(FaceColor, 24);
      	LineTo(X1+Corner, Y2);
   			Pen.Color := DarkenColor(FaceColor, 48);
      	LineTo(X2-Corner, Y2);
   			Pen.Color := DarkenColor(FaceColor, 64);
      	LineTo(X2, Y2-Corner);
   			Pen.Color := DarkenColor(FaceColor, 48);
      	LineTo(X2, Y1+Corner);
      end;
      Offset := Corner;
    end else if Style = bsGradient then
    begin
    	R.Right := R.Right - 1;
      R.Bottom := R.Bottom - 1;
      if IsDown then
 				Frame3D(Canvas, R,  DarkenColor(FaceColor, 96), BrightenColor(FaceColor, 24), 1)
      else
 				Frame3D(Canvas, R,  BrightenColor(FaceColor, 24), DarkenColor(FaceColor, 96), 1);
    	R.Right := R.Right - 1;
      R.Bottom := R.Bottom - 1;
    	Brush.Style := bsSolid;
    	if IsDown then
				LineGradient(Canvas, R, [FaceColor, DarkenColor(FaceColor, 24),
      		DarkenColor(FaceColor, 64)], 2, diHorizonal, 2)
      else
				LineGradient(Canvas, R, [BrightenColor(FaceColor, 24), FaceColor, DarkenColor(FaceColor, 24),
      		DarkenColor(FaceColor, 48)], 3, diHorizonal, 2);

			Offset := 2;
    end;
  end;
	Result := Rect(X1 + Offset, Y1 + Offset, X2 - Offset, Y2 - Offset);
  if IsDown then InflateRect(Result, -1, -1);
end;

type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function Add(Image, Mask: TBitmap): Integer;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FColor: TColor;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState; Color: TColor): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TButtonState);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; IsAutoBW: Boolean);

    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      var GlyphPos: TPoint; var TextBounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; IsAutoBW: Boolean): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Color : TColor read FColor write FColor;
  end;

{ TGlyphList }

constructor TGlyphList.Create(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.Add(Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.Create(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;
  ButtonCount: Integer = 0;
  Pattern: TBitmap;

function CreateBrushPattern(Color: TColor): TBitmap;
var
  X, Y: Integer;
begin
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
          Pixels[X, Y] := BrightenColor(Color, 128);     { on even/odd rows }
  end;
  Result := Pattern;
end;
{ TButtonGlyph }

constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState; Color: TColor): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, MonoBmp, CopyBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
  a,b: Integer;
  clBtnFace, cl3DLight, cl3DDarkShadow,
  clBtnShadow, clBtnHighlight : TColor;
begin
  FColor := ColorToRGB(Color);
	clBtnFace := ColorToRGB(Color);
	cl3DDarkShadow := DarkenColor(clBtnFace, 126);
  clBtnShadow := DarkenColor(clBtnFace, 48);
  cl3DLight := BrightenColor(clBtnFace, 48);
  clBtnHighlight := BrightenColor(clBtnFace, 96);
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
    	bsUp, bsDown, bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := FGlyphList.Add(TmpImage, nil);
        end;
      bsDisabled:
        begin
          MonoBmp := TBitmap.Create;
          try
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, FOriginal.Canvas, ORect);
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;
              MonoBmp.Monochrome := True;

              { Convert white to clBtnHighlight }
              FOriginal.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              FOriginal.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              FOriginal.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
            FIndexs[State] := FGlyphList.Add(TmpImage, nil);
          finally
            MonoBmp.Free;
          end;
       end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
  State: TButtonState);
var
  Index: Integer;
  Tmp: TBitmap;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State, FColor);
 	Tmp := TBitmap.Create;
  TImageList(FGlyphList).GetBitmap(Index, Tmp);
  TransparentBlt(Canvas, Tmp, X, Y, FTransparentColor);
  Tmp.Free;
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; IsAutoBW: Boolean);
begin
  with Canvas do
  begin

    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := BrightenColor(FColor, 96);
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := DarkenColor(FColor, 96);
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
    end
    else
    begin
    	if IsAutoBW then
      	if IsLightColor(FColor) then
      		Canvas.Font.Color := 0
      	else
      		Canvas.Font.Color := $FFFFFF;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  State: TButtonState; IsAutoBW: Boolean): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Caption, Layout, Margin, Spacing,
    GlyphPos, Result);
  DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State);
  DrawButtonText(Canvas, Caption, Result, State, IsAutoBW);
end;

var
	RepeatTimer: TTimer;

{ TColor95Button }
constructor TColor95Button.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 25, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  FGlyph := TButtonGlyph.Create;
  FSpacing := 4;
  if Aowner is TForm then
  	fColor := TForm(AOwner).Color
  else if AOwner is TColorPanel then
  	fColor := TColorPanel(AOwner).Color;
  ParentFont := True;
	FMouseOver := False;
  FMargin := -1;
  TButtonGlyph(FGlyph).Color := FColor;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FPattern := TBitmap.Create;
  FPattern.Assign(CreateBrushPattern(FColor));
  FLayout := blGlyphLeft;
  FStyle := bsNew;
	fOptions := [boShowMouseFocus, boAutoBlackWhiteText, boDitherPattern];
  Inc(ButtonCount);
end;

destructor TColor95Button.Destroy;
begin
	FPattern.Free;
  FPattern := nil;
  TButtonGlyph(FGlyph).Free;
  Dec(ButtonCount);
  inherited Destroy;
end;

procedure TColor95Button.Paint;
var
  PaintRect: TRect;
begin
	Canvas.Font.Assign(Self.Font);
  if not Enabled and not (csDesigning in ComponentState) then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then FState := bsUp;
  PaintRect := Rect(0, 0, Width, Height);
  if (fStyle = bsOwnerDraw) and Assigned(fOwnerDraw) then
  	fOwnerDraw(Self, Canvas, PaintRect)
  else
  begin
  	//Default drawing for Onwer Draw
  	if fStyle = bsOwnerDraw then
  	PaintRect := DrawButtonFace(Canvas, Rect(0, 0, Width, Height), bsNew,
    	(csDesigning in ComponentState), FState in [bsDown, bsExclusive], FMouseOver, FColor)
		else
  	PaintRect := DrawButtonFace(Self.Canvas, Rect(0, 0, Width, Height), FStyle,
    	(csDesigning in ComponentState), FState in [bsDown, bsExclusive], FMouseOver, FColor);
  end;

  if (FState = bsExclusive) and (not (fStyle = bsOwnerDraw)) and (boDitherPattern in fOptions) then
  begin
    Canvas.Brush.Bitmap := CreateBrushPattern(Self.Color);
    Dec(PaintRect.Right);
    Dec(PaintRect.Bottom);
    if FStyle = bsEllipse then
			Canvas.Ellipse(PaintRect.Left+1, PaintRect.Top+1, PaintRect.Right-1, PaintRect.Bottom-1)
    else
    	Canvas.FillRect(PaintRect);
    Canvas.Brush.Bitmap := nil;
  end;

  if FMouseOver and Enabled and (not (FState = bsDown)) and (not (fStyle = bsIE4))  then
  begin
  	if ((FState = bsUp) or (FState = bsExclusive)) then
    	Canvas.Brush.Color := BrightenColor(FColor, 24)
    else
    	Canvas.Brush.Color := FColor;
    Canvas.Pen.Style := psClear;
    if FStyle = bsEllipse then
			Canvas.Ellipse(PaintRect.Left+1, PaintRect.Top+1, PaintRect.Right-1, PaintRect.Bottom-1)
    else
    	Canvas.FillRect(PaintRect);
    Canvas.Pen.Style := psSolid;
  end;

  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Caption, FLayout, FMargin, FSpacing,
    FState, (boAutoBlackWhiteText in fOptions));

end;

procedure TColor95Button.Loaded;
var
  State: TButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State, FColor);
end;

const
  InitRepeatPause = 500;
  RepeatPause     = 100;

procedure TColor95Button.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Repaint;
    end;
    FDragging := True;
  end;
  if (boRepeatable in fOptions) then
  begin
    RepeatTimer.OnTimer := OnButtonRepeat;
    RepeatTimer.Interval := InitRepeatPause;
    RepeatTimer.Enabled  := True;
  end;
end;

procedure TColor95Button.OnButtonRepeat(Sender: TObject);
begin
  RepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  	try
    	Click;
  	except
    	RepeatTimer.Enabled := False;
    	raise;
  	end;
end;

procedure TColor95Button.SetStyle(Value: TClr95ButtonStyle);
begin
	if FStyle <> Value then
  begin
  	FStyle := Value;
    Repaint;
  end;
end;

procedure TColor95Button.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end;

procedure TColor95Button.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    FState := bsUp;
    if FGroupIndex = 0 then
      Repaint
    else
      if DoClick then SetDown(not FDown)
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click;
  end;
  RepeatTimer.Enabled  := False;
end;

procedure TColor95Button.Click;
begin
  PlayWave(FSndClick);
  inherited Click;
end;

function TColor95Button.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TColor95Button.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TColor95Button.CMMouseEnter(var Message: TMessage);
begin
	if (not FMouseOver) and (boShowMouseFocus in fOptions) and Enabled then
  begin
		FMouseOver := True;
    PlayWave(FSndMouseOver);
	  Invalidate;
  end;
end;

procedure TColor95Button.SetOptions(Value: TButtonOptions);
begin
	if Value <> fOptions then
  begin
  	fOptions := Value;
    Invalidate;
  end;
end;

procedure TColor95Button.CMMouseLeave(var Message: TMessage);
begin
	if FMouseOver then
  begin
  	FMouseOver := False;
	  Invalidate;
  end;
end;

procedure TColor95Button.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

function TColor95Button.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TColor95Button.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TColor95Button.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TColor95Button.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TColor95Button.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then FState := bsExclusive
    else FState := bsUp;
    Invalidate;
    if Value then UpdateExclusive;
  end;
end;

procedure TColor95Button.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TColor95Button.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TColor95Button.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TColor95Button.SetColor(Value: TColor);
begin
	if Value <> FColor then
  begin
  	FColor := Value;
    FPattern.Assign(CreateBrushPattern(FColor));
	  TButtonGlyph(FGlyph).CreateButtonGlyph(FState, FColor);
    Invalidate;
  end;
end;

procedure TColor95Button.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TColor95Button.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TColor95Button.WMLButtonDblClk(var Message: TWMMouse);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TColor95Button.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
	FState := NewState[Enabled];
  TButtonGlyph(FGlyph).CreateButtonGlyph(FState, FColor);
  Invalidate;
end;

procedure TColor95Button.CMButtonPressed(var Message: TMessage);
var
  Sender: TColor95Button;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TColor95Button(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TColor95Button.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TColor95Button.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TColor95Button.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

initialization
	RepeatTimer := TTimer.Create(nil);
  RepeatTimer.Enabled := False;
	Pattern := TBitmap.Create;
  Pattern.LoadFromResourceName(HInstance, 'CLRBTNPATTERN');

finalization
	RepeatTimer.Free;
	Pattern.Free;
  
end.
