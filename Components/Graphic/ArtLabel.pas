unit ArtLabel;

{ ----------------------------------------------------------------------
		Art Label Component Designed By William Yang.
    Copyright(r) By Dream Factory 1997.
    Email : yang@btinternet.com   http://www.btinternet.com/~yang/
    Version : 1.05    First Program : 1997 - Sep - 10 (Y2000 Date Format)
  ---------------------------------------------------------------------- }
  
interface

uses Windows, SysUtils, Classes, Controls, Messages, Graphics,
	Gradient, DFClasses;

type

TArtStyle = set of (as3DSunk, as3DRaise, asShadow, asBitmap, asOutlined,
	asBitmapFit, asGradient);

//TDirection = (diNormal, diMirrored, diChinese, diStartMenu, diNotStartMenu);

	TArtLabel=class(TGraphicControl)
	private
		{ Private declarations }
    Tmp: TBitmap;
    fGradBmp: TBitmap;
		fArtStyle: TArtStyle;
    fTextAlign: TAlignment;
		fCaption: TCaption;
		fAutoSize: Boolean;
//		fFont: TFont;
		fTransparent: Boolean;
		fColor: TColor;
    fBitmap: TBitmap;
    fShadowColor: TColor;
    fOutlineColor: TColor;
    fShadowLength: Integer;
		fGradient: TGradient;
    fGradChanged: Boolean;
//		fDirection: TDirection;
		procedure SetArtStyle( Value: TArtStyle  );
		procedure SetCaption( Value: TCaption );
		procedure SetFont( Value: TFont );
		procedure SetTransparent( Value: Boolean );
		procedure SetColor( Value: TColor );
    procedure SetAutosize(Value: Boolean);
    procedure SetBitmap( Value: TBitmap);
    procedure SetShadowColor( Value: TColor );
    procedure SetOutlineColor(Value: TColor);
    procedure SetShadowLength(Value: Integer);
    procedure SetGrad(Value: TGradient);
    procedure SetTextAlign(Value: TAlignment);
    
    function GetFont: TFont;
//		procedure SetDirection( Value: TDirection );
	protected
		{ Protected declarations }
		procedure Paint; override;
    procedure OnFontChange(Sender: TObject);
    procedure OnGradChange(Sender: TObject);
	public
		{ Public declarations }
		constructor Create ( AOwner: TComponent ); override;
		destructor Destroy ; override;
	published
		{ Published declarations }
		property ArtStyle: TArtStyle  read fArtStyle write SetArtStyle;
		property Caption: TCaption read fCaption write SetCaption;
		property Font: TFont read GetFont write SetFont;
		property Transparent: Boolean read fTransparent write SetTransparent;
		property Color: TColor read fColor write SetColor;
    property Bitmap: TBitmap read fBitmap write SetBitmap;
    property ShadowColor: TColor read fShadowColor write SetShadowColor;
    property OutlineColor: TColor read fOutlineColor write SetOutlineColor;
    property AutoSize: Boolean read fAutoSize write SetAutosize;
		property ShadowLength: Integer read fShadowLength write SetShadowLength;
    property Gradient: TGradient read FGradient write SetGrad;
    property TextAlign: TAlignment read fTextAlign write SetTextAlign;

//		property Direction: TDirection read fDirection write SetDirection;
    {Pre-exists properties}
    property Align;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
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

implementation

uses ColorMan, Drawman, NumMan;

{ TArtLabel }
procedure TArtLabel.Paint;
var
	xMask : TBitmap;
  R, TextR : TRect;
  X, Y : Integer;
  LowClr, HighClr : TColor;
begin
  R := ClientRect;
  with Tmp.Canvas do
  begin
  	Font.Assign(Self.Canvas.Font);
 		Brush.Color := fColor;
  	if fAutoSize then
  	begin
  		Self.Width := TextWidth(fCaption)+4;
			Self.Height := TextHeight('Wg')+4;
  	end;
  	Tmp.Width := R.Right - R.Left;
  	Tmp.Height := R.Bottom - R.Top;
  	FillRect(Rect(0, 0, Tmp.Width, Tmp.Height));
  	//Keep the text in VCenter and HCenter.
    case fTextAlign of
    	taCenter:
    	begin
  			X := Max((Width - TextWidth(fCaption)) div 2, 0);
  			Y := Max((Height - TextHeight(fCaption)) div 2, 0);
  			//X and Y should be move to UpLeft.
  			X := X - R.Left;
  			Y := Y - R.Top;
      end;
      taLeftJustify:
      begin
  			X := 2;
  			Y := Max((Height - TextHeight(fCaption)) div 2, 2);
  			Y := Y - R.Top;
      end;
      taRightJustify:
      begin
  			X := R.Right-TextWidth(fCaption)-2;
  			Y := Max((Height - TextHeight(fCaption)) div 2, 2);
  			Y := Y - R.Top;
      end;
		end;
    //Get the 3D Colors.
		LowClr := DarkenColor(fColor, 64);
		HighClr := BrightenColor(fColor, 64);
 		if asShadow in fArtStyle then DrawShadowTextExt(Tmp.Canvas, x, y, fCaption, fShadowColor, fShadowLength, fShadowLength);
 		if asOutlined in fArtStyle then DrawOutlinedText(Tmp.Canvas, x, y, fCaption, fOutlineColor)
    else if as3DSunk in fArtStyle then Draw3DText(Tmp.Canvas, fCaption, x, y, LowClr, HighClr)
    else if as3DRaise in fArtStyle then Draw3DText(Tmp.Canvas, fCaption, x, y, HighClr, LowClr);

  	if asGradient in fArtStyle then
  	begin
  		if fGradBmp = nil then fGradBmp := TBitmap.Create;
			if fGradChanged or (fGradBmp.Width <> Self.Width) or (fGradBmp.Height <> Self.Height) then
    	begin
    		fGradBmp.Width := Self.Width;
      	fGradBmp.Height := Self.Height;
      	fGradBmp.Canvas.Brush.Color := Tmp.Canvas.Font.Color;
      	fGradBMp.Canvas.FillRect(ClientRect);
    		PaintGradient(fGradBmp.Canvas, ClientRect, fGradient);
      	fGradChanged := False;
    	end;

		StretchPaintOnText(Tmp.Canvas, x, y, fCaption, fGradBmp);

  	end
  	else if asBitmapFit in fArtStyle then
  	begin
			StretchPaintOnText(Tmp.Canvas, x, y, fCaption, fBitmap);
  	end
  	else if asBitmap in fArtStyle then
 		begin
			PaintOnText(Tmp.Canvas, x, y, fCaption, fBitmap);
  	end;
  end;

  if fTransparent then
  begin
    with xMask do
    begin
    	//Create the mask, and keep it small as possible.
  		xMask := TBitmap.Create;
    	//Set monochrome first make sure the best performance.
    	Monochrome := True;
    	//use cliprect, only refresh the dirty part.
    	Width := Tmp.Width;
    	Height := Tmp.Height;
    	//X and Y are already been set above.
    	//Paint as usual, just try to create the mask.
    	Canvas.Font.Assign(Self.Canvas.Font);
    	Canvas.Font.Color := clBlack;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(Rect(0, 0, Tmp.Width, Tmp.Height));

 			if asShadow in fArtStyle then DrawShadowTextExt(Canvas, x, y, fCaption, clBlack, fShadowLength, fShadowLength);
			if (asOutlined in fArtStyle) then DrawOutlinedText(Canvas, x, y, fCaption, clBlack);
 			if (as3DSunk in fArtStyle) or (as3DRaise in fArtStyle)
      	then Draw3DText(Canvas, fCaption, x, y, clBlack, clBlack);
    end;

    //Because the Tmp is already been comtracted. there is no need to make another
    //object.
  	Self.Canvas.Brush.Style := bsClear;
  	Self.Canvas.FillRect(R);
    PaintOnMask(Self.Canvas, R.Left, R.Top, xMask, Tmp);
    xMask.Free;
 	end
  else
  	Canvas.Draw(R.Left, R.Top, Tmp);
  Self.Canvas.Brush.Style := bsSolid;
end;

procedure TArtLabel.SetGrad(Value: TGradient);
begin
	if fGradient <> Value then
  begin
  	fGradient.Assign(Value);
  end;
end;
{ TArtLabel }

constructor TArtLabel.Create ( AOwner: TComponent );
begin
	inherited Create( AOwner );
  fBitmap := TBitmap.Create;
  Tmp := TBitmap.Create;
  fColor := clBtnFace;
  fCaption := 'Art Label';
  fArtStyle := [asShadow, as3DRaise];
  fShadowColor := clGray;
  fOutlineColor := clWhite;
  fShadowLength := 3;
  fTransparent := False;
  Width := 64;
  Height := 25;
  fGradient := TGradient.Create;
  with fGradient do
  begin
  	OnChange := OnGradChange;
  	Color1 := clSilver;
  	Color2 := clGray;
  	Color3 := clBlack;
  	ColorCount := 3;
  	LineSize := 2;
  	GradientStyle := gsLine;
  end;
	fGradChanged := True;
  Canvas.Font.OnChange := OnFontChange;
  Font.Name := 'Arial';
  Font.Size := 24;
  Font.Style := [fsBold];
end;

destructor TArtLabel.Destroy ;
begin
  fBitmap.Free;
  Tmp.Free;
  fGradient.Free;
  if fGradBmp <> nil then fGradBmp.Free;
	inherited Destroy;  
end;

procedure TArtLabel.SetArtStyle( Value: TArtStyle  );
begin
	if fArtStyle <> Value then
	begin
		fArtStyle := Value;
		Repaint;
	end;
end;

procedure TArtLabel.SetCaption( Value: TCaption );
begin
	if fCaption <> Value then
	begin
		fCaption := Value;
		Repaint;
	end;
end;

procedure TArtLabel.SetFont( Value: TFont );
begin
	if Font <> Value then
	begin
		Canvas.Font.Assign(Value);
//		Repaint;
	end;
end;

function TArtLabel.GetFont: TFont;
begin
	Result := Canvas.Font;
end;

procedure TArtLabel.SetTransparent( Value: Boolean );
begin
	if fTransparent <> Value then
	begin
		fTransparent := Value;
		Repaint;
	end;
end;

procedure TArtLabel.SetShadowColor( Value: TColor );
begin
	if fShadowColor <> Value then
	begin
		fShadowColor := Value;
 		if (asShadow in fArtStyle) then
    begin
			Repaint;
    end;
	end;
end;

procedure TArtLabel.SetOutlineColor(Value: TColor);
begin
	if fOutlineColor <> Value then
	begin
		fOutlineColor := Value;
 		if (asOutlined in fArtStyle) then
    begin
			Repaint;
    end;
	end;
end;

procedure TArtLabel.SetColor( Value: TColor );
begin
	if fColor <> Value then
	begin
		fColor := Value;
		Repaint;
	end;
end;

procedure TArtLabel.SetBitmap(Value: TBitmap);
begin
	if fBitmap <> Value then
  begin
  	fBitmap.Assign(Value);
 		if (asBitmap in fArtStyle) or (asBitmapFit in fArtStyle) then
    begin
			Repaint;
    end;
  end;
end;

procedure TArtLabel.OnFontChange(Sender: TObject);
begin
	Repaint;
end;

procedure TArtLabel.OnGradChange(Sender: TObject);
begin
	fGradChanged := True;
  if asGradient in fArtStyle then
  	Repaint;
end;

procedure TArtLabel.SetShadowLength(Value: Integer);
begin
	if fShadowLength <> Value then
  begin
		fShadowLength := Value;
 		if (asShadow in fArtStyle) then
    begin
			Repaint;
    end;
  end;
end;

procedure TArtLabel.SetTextAlign(Value: TAlignment);
begin
	if fTextAlign <> Value then
  begin
		fTextAlign := Value;
		Repaint;
  end;
end;

procedure TArtLabel.SetAutosize(Value: Boolean);
begin
	if fAutosize <> Value then
  begin
  	fAutosize := (Value);
		Repaint;
  end;
end;

{procedure TArtLabel.SetDirection( Value: TDirection );
begin
	if fDirection <> Value then
	begin
		fDirection := Value;
		Invialidate;
	end;
end;}

end.


