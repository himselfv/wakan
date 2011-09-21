Unit SImageBtn;

{Version 1.30}


{ First Program on 1997 - May - 19
 1997 June 13

Added Yellow mouse focusing outline to the button image.(Got a problem)
Replace FCaption with a override caption property.
Added Message events handlers. (Fonts, Caption, Enabled)

}

//----------------------------------------------------------------------//
//	Author : William Yang, Email : yang@btinternet.com      	          //
//	URL : http://www.btinternet.com/~yang/                              //
//----------------------------------------------------------------------//

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ColorMan, DrawMan, StdCtrls, GrafMan, Buttons;
type

	TDeepth = 0..12;
  TSImageBtn = class(TGraphicControl)
  private
    { Private Declarations }
//    FGroupIndex : Integer;
//    FLayout : TButtonLayout;
    FShadow: Boolean;
    FImage : TBitmap;
    FDeepth : TDeepth;
    FMouseDown : Boolean;
    FDeepth2 : TDeepth;
    FDisabled : TBitmap;
    FTrans: TColor;
    FSImage: TBitmap;
    FSpace: Integer;
    FDarkness: Integer;
    FBriImage : TBitmap;
 		FShowMouseFocus : Boolean;
//    FAllowAllUp : Boolean;
    FMouseFocused : Boolean;
    TMPBMP : TBitmap;
    FCaptionChanged : Boolean;

    procedure SetShadow(Val: Boolean);
    procedure SetImage(Val: TBitmap);
    procedure SetDeepth(Val: TDeepth);
    procedure SetTrans(Val : TColor);
    procedure SetSpace(Val: Integer);
    procedure SetDarkness(Val: Integer);
    procedure SetBriImage(Val : TBitmap);
    {Private Methods}
  	procedure ImageChanged(Sender: TObject);
//    procedure MeasureXY(var IX, IY, TX, TY : Integer);
    procedure UpdateTracking;
    procedure UpdateShadow;
    {Message Events}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
//    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  protected
    { Protected Declarations }
    procedure Paint; override;

//    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
//  Y: Integer);
		procedure MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
		procedure MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Shadow: Boolean read FShadow write SetShadow;
    property Deepth: TDeepth read FDeepth write SetDeepth;
    property Image: TBitmap read FImage write SetImage;
    property TransparentColor: TColor read FTrans write SetTrans;
  	property Space: Integer read FSpace write SetSpace;
    property ShadowsDarkness: Integer read FDarkness write SetDarkness;
    property ShowMouseFocus : Boolean read FShowMouseFocus write FShowMouseFocus;
    property FocusedImage: TBitmap read FBriImage write SetBriImage;
//    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
//    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
//    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;

    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnEndDrag;
    property OnDragDrop;
    property OnDragOVer;
    property OnStartDrag;

    property Caption;
    property Cursor;
    property Enabled;
    property Font;
    property Hint;
    property ParentShowHint;
    property ParentFont;
    property ShowHint;
    property PopupMenu;

  end;

procedure Register;

implementation

{--------------------------------------}
{ TSImageBtn.Create                    }
{--------------------------------------}
constructor TSImageBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShadow := True;
  TMPBMP := TBitmap.Create;
  FBriImage := TBitmap.Create;
  FDeepth := 2;
  FMouseDown := False;
  FImage := TBitmap.Create;
  FDisabled := TBitmap.Create;
  FSImage := TBitmap.Create;
  FSpace := 2;
  FDarkness := 40;
  FShowMouseFocus := True;
	FMouseFocused := False;
  Caption := '';
  Width := 25;
  Height := 26;
  FImage.OnChange := ImageChanged;
  OnMouseDown := MouseDown;
//  OnMouseMove := MouseMove;
  OnMouseUp := MouseUp;
  FCaptionChanged := False;
end;

destructor TSImageBtn.Destroy;
begin
  FImage.Free;
  FDisabled.Free;
  FSImage.Free;
  TMPBMP.Free;
	FBriImage.Free;
  inherited Destroy;
end;

procedure TSImageBtn.Paint;
var
	Tx, Ty : Integer;
  SW, SH : Integer;
  IX, IY : Integer;
begin
  TMPBMP.Width := Width;
  TMPBMP.Height := Height;
	with TMPBMP.Canvas do
  begin
   	Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Width, Height));
  	Font.Assign(Self.Font);
    if FImage <> nil then
  	begin
    	iX := (Width - FImage.Width) div 2;
   		iY := (Height - TextHeight(Caption) - FImage.Height) div 2;
   		Tx := (Width - TextWidth(Caption)) div 2;
    	Ty := iy + FSpace + FImage.Height;
    end
    else
    begin
   		Tx := (Width - TextWidth(Caption)) div 2;
    	Ty := (Height - TextHeight(Caption)) div 2;
    end;

  	if FImage <> nil then
    begin
    	if FMouseDown then
 				if Enabled then
					TransparentBlt(TMPBMP.Canvas, FImage, iX + FDeepth, iY + FDeepth, FTrans)
        else
					TransparentBlt(TMPBMP.Canvas, FDisabled, iX + FDeepth, iY + FDeepth, ColorToGrey(FTrans))
      else
      begin
       	if FShadow then
        	TransparentBlt(TMPBMP.Canvas, FSImage, iX + FDeepth,
        		iY + FDeepth, clWhite);
   			if Enabled then
        begin
         	if FShowMouseFocus and FMouseFocused then
          	if FBriImage.Handle <> 0 then
							TransparentBlt(TMPBMP.Canvas, FBriImage, iX, iY, FTrans)
            else
            	TransparentBlt(TMPBMP.Canvas, FImage, iX, iY, FTrans)
          else
          	TransparentBlt(TMPBMP.Canvas, FImage, iX, iY, FTrans)
        end
    		else
  				TransparentBlt(TMPBMP.Canvas, FDisabled, iX, iY, ColorToGrey(FTrans))
      end;
    end;
    Brush.Style := bsClear;

    if (Caption <> '') and FCaptionChanged then
    begin
    	Font.Assign(Self.Font);
    	if not Enabled then	Font.Color := ColorToGrey(Font.Color);
      if FShadow then
      begin
      	Font.Color := DarkenColor(clBtnFace, FDarkness);
      	TextOut(Tx + FDeepth, Ty + FDeepth, Caption);
        Font.Color := Self.Font.Color;
      end;
    	if FMouseDown then
      	TextOut(Tx + FDeepth, Ty + FDeepth, Caption)
      else
      	TextOut(Tx, Ty, Caption);
    end;
    if csDesigning in ComponentState then
    begin
    	Pen.Style := psDot;
    	Rectangle(0, 0, Width, Height);
    end;
  end;
	Canvas.CopyRect(Canvas.ClipRect, TMPBMP.Canvas, Canvas.ClipRect);
end;

procedure TSImageBtn.SetShadow(Val: Boolean);
begin
	if FShadow <> Val then
  begin
  	FShadow := Val;
  	Repaint;
  end;
end;


procedure TSImageBtn.SetTrans(Val: TColor);
begin
	if FTrans <> Val then
  begin
  	FTrans := Val;
  	Repaint;
  end;
end;

procedure TSImageBtn.SetImage(Val: TBitmap);
begin
	if FImage <> Val then
  begin
  	FImage.Assign(Val);
  end;
end;

procedure TSImageBtn.SetSpace(Val : Integer);
begin
	if FSpace <> Val then
  begin
  	FSpace := Val;
  	Repaint;
  end;
end;

procedure TSImageBtn.SetDeepth(Val: TDeepth);
begin
	if FDeepth <> Val then
  begin
  	FDeepth := Val;
  	Repaint;
  end;
end;

procedure TSImageBtn.SetDarkness(Val: Integer);
begin
	if FDarkness <> Val then
  begin
  	FDarkness := Val;
    UpdateShadow;
  	Repaint;
  end;
end;

procedure TSImageBtn.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
  begin
		FMouseDown := True;
  	Repaint;
  end;
end;

procedure TSImageBtn.UpdateShadow;
var
	i, j : Integer;
begin
  with FSImage do
	begin
  	Width := FImage.Width;
  	Height := FImage.Height;
  	with Canvas do
			for i := 0 to FSImage.Width do
  			for j := 0 to FSImage.Height do
        	if FImage.Canvas.Pixels[i, j] <> FTrans then
          	FSImage.Canvas.Pixels[i, j] := DarkenColor(clBtnFace, FDarkness)
          else
          	FSImage.Canvas.Pixels[i, j] := clWhite;
  end;

end;

procedure TSImageBtn.SetBriImage(Val : TBitmap);
begin
	if FBriImage <> Val then
  begin
  	FBriImage.Assign(Val);
    Repaint;
  end;
end;

procedure TSImageBtn.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
  begin
  	FMouseDown := False;
  	Repaint;
  end;
end;

procedure TSImageBtn.ImageChanged(Sender: TObject);
begin
	if FImage.Handle<>0 then
	begin
  	FTrans := FImage.Canvas.Pixels[0,0];
    FDisabled.Assign(DisabledBmp(FImage, clBtnFace, clWhite));
    UpdateShadow;
    Repaint;
  end;
end;

procedure TSImageBtn.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    if (FindDragTarget(P, True) = Self) then
      Perform(CM_MOUSEENTER, 0, 0)
    else
      Perform(CM_MOUSELEAVE, 0, 0);
  end;
end;

{procedure TSImageBtn.SetDown(Value: Boolean);
var
  NeedInvalidate: Boolean;
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      NeedInvalidate := FState <> bsExclusive;
      FState := bsExclusive;
      if NeedInvalidate then Repaint;
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
  end;
end;

procedure TSImageBtn.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TSImageBtn.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Repaint;
  end;
end;   }

procedure TSImageBtn.CMEnabledChanged(var Message: TMessage);
begin
  UpdateTracking;
  Repaint;
end;

{procedure TSImageBtn.CMButtonPressed(var Message: TMessage);
var
  Sender: TSImageBtn;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TSImageBtn(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Repaint;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;      }

procedure TSImageBtn.CMFontChanged(var Message: TMessage);
begin
  Repaint;
end;

procedure TSImageBtn.CMTextChanged(var Message: TMessage);
begin
  FCaptionChanged := True;
  Repaint;
end;

procedure TSImageBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseFocused and Enabled and FShowMouseFocus and (FBriImage.Handle = 0) then
  begin
    FMouseFocused := True;
    Repaint;
  end;
end;

procedure TSImageBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseFocused and Enabled and FShowMouseFocus then
  begin
    FMouseFocused := False;
    Repaint;
  end;
end;

{--------------------------------------}
{ Register                             }
{--------------------------------------}
procedure Register;
begin
  RegisterComponents('Grafix', [TSImageBtn]);
end;

end.

