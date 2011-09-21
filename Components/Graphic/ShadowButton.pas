Unit ShadowButton;

{ First Program In 1997 - May - 19}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ColorMan, DrawMan, StdCtrls;

type
  TShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle);
	TDeepth = 0..12;
  TShadowButton = class(TCustomControl)
  private
    { Private Declarations }
    FCaption: TCaption;
    FColor: TColor;
    FShadow: Boolean;
    FShape: TShapeType;
    FBorder : Boolean;
    FBorderColor : TColor;
    FDeepth : TDeepth;
    FMouseDown : Boolean;
    FDeepth2 : TDeepth;
    procedure SetCaption(Val: TCaption);
    procedure SetColor(Val: TColor);
    procedure SetShadow(Val: Boolean);
    procedure SetShape(Val: TShapeType);
    procedure SetBorderColor(Val: TColor);
    procedure SetBorder(Val : Boolean);
    procedure SetDeepth(Val: TDeepth);
    procedure SetFont(var Message: TMessage); message CM_FONTCHANGED;
  protected
    { Protected Declarations }
    procedure Paint; override;

    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
		procedure MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
		procedure MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Shadow: Boolean read FShadow write SetShadow;
    property Shape: TShapeType read FShape write SetShape;
    property Border: Boolean read FBorder write SetBorder;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Deepth : TDeepth read FDeepth write SetDeepth;

    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnEndDrag;
    property OnDragDrop;
    property OnDragOVer;
    property OnStartDrag;

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
{ TShadowButton.Create                 }
{--------------------------------------}
constructor TShadowButton.Create(AOwner: TComponent);
begin

  FShape := stRoundRect;
  FColor := clLime;
  FBorder := True;
  FShadow := True;
  FBorderColor := clBlack;
  FDeepth := 3;
  FCaption := 'Shadow Button';
  FMouseDown := False;

  inherited Create(AOwner);
  Width := 100;
  Height := 33;
  
  OnMouseDown := MouseDown;
  OnMouseMove := MouseMove;
  OnMouseUp := MouseUp;
end;

{--------------------------------------}
{ TShadowButton.Destroy                }
{--------------------------------------}
destructor TShadowButton.Destroy;
begin
  inherited Destroy;
end;

{--------------------------------------}
{ TShadowButton.Paint                  }
{--------------------------------------}
procedure TShadowButton.Paint;
	procedure DrawShape(C: TCanvas; Shape: TShapeType; X, Y, W, H : Integer);
  var
  	S : Integer;
  begin
    with C do
    begin
    if W < H then S := W else S := H;
    case Shape of
      stSquare, stRoundSquare, stCircle:
        begin
          X := (W - S) div 2 + X;
          Y := (H - S) div 2 + Y;
          W := S;
          H := S;
        end;
    end;
    case Shape of
      stRectangle, stSquare:
        Rectangle(X, Y, X + W, Y + H);
      stRoundRect, stRoundSquare:
        RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
      stCircle, stEllipse:
        Ellipse(X, Y, X + W, Y + H);
    end;
 	    case FShape of
      stRectangle, stSquare:
        Rectangle(X, Y, X + W, Y + H);
      stRoundRect, stRoundSquare:
        RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
      stCircle, stEllipse:
        Ellipse(X, Y, X + W, Y + H);
  		end;
    end;
	end;

var
	Tx, Ty : Integer;
  SW, SH : Integer;

begin
	with Canvas do
  begin
  	Tx := 0;
    Ty := 0;

  	SW := Width - FDeepth;
    SH := Height - FDeepth;

  	Brush.Style := bsClear;
    FillRect(Rect(0,0,Width,Height));

    Brush.Style := bsSolid;
    Pen.Style := psClear;
    Brush.Color := clBtnShadow;

    if FShadow then
      if not FMouseDown then
      	DrawShape(Canvas, FShape, FDeepth, FDeepth, SW, SH);

    if FBorder then Pen.Style := psSolid else	Pen.Style := psClear;
    if Enabled then	Brush.Color := FColor else Brush.Color := ColorToGrey(FColor);
    if Enabled then	Pen.Color := FBorderColor else Pen.Color := ColorToGrey(FBorderColor);

    if FMouseDown then
    	DrawShape(Canvas, FShape, FDeepth, FDeepth, SW, SH)
    else
    	DrawShape(Canvas, FShape, 0, 0, SW, SH);
    Brush.Style := bsClear;
    if FCaption <> '' then
    begin
    	if Enabled then	Font.Color := FBorderColor else Font.Color := ColorToGrey(Font.Color);
			Font.Assign(Self.Font);
    	Tx := (SW - TextWidth(FCaption)) div 2;
      Ty := (SH - TextHeight(FCaption)) div 2;
    	if FMouseDown then
      	TextOut(Tx + FDeepth, Ty + FDeepth, FCaption)
      else
      	TextOut(Tx, Ty, FCaption);
    end;
  end;
end;

procedure TShadowButton.SetCaption(Val: TCaption);
begin
	if FCaption <> Val then
  begin
  	FCaption := Val;
  	Repaint;
  end;
end;

procedure TShadowButton.SetColor(Val: TColor);
begin
	if FColor <> Val then
  begin
  	FColor := Val;
  	Repaint;
  end;
end;

{--------------------------------------}
{ TShadowButton.SetShadow              }
{--------------------------------------}
procedure TShadowButton.SetShadow(Val: Boolean);
begin
	if FShadow <> Val then
  begin
  	FShadow := Val;
  	Repaint;
  end;
end;

{--------------------------------------}
{ TShadowButton.SetShape               }
{--------------------------------------}
procedure TShadowButton.SetShape(Val: TShapeType);
begin
	if FShape <> Val then
  begin
  	FShape := Val;
  	Repaint;
  end;
end;

procedure TShadowButton.SetBorderColor(Val: TColor);
begin
	if FBorderColor <> Val then
  begin
  	FBorderColor := Val;
  	Repaint;
  end;
end;

procedure TShadowButton.SetFont(var Message: TMessage);
begin
	inherited;
	Repaint;
end;

procedure TShadowButton.SetBorder(Val : Boolean);
begin
	if FBorder <> Val then
  begin
  	FBorder := Val;
  	Repaint;
  end;
end;

procedure TShadowButton.SetDeepth(Val: TDeepth);
begin
	if FDeepth <> Val then
  begin
  	FDeepth := Val;
  	Repaint;
  end;
end;

procedure TShadowButton.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TShadowButton.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
  begin
		FMouseDown := True;
  	Repaint;
  end;
end;

procedure TShadowButton.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
  begin
  	FMouseDown := False;
  	Repaint;
  end;
end;

{--------------------------------------}
{ Register                             }
{--------------------------------------}
procedure Register;
begin
  RegisterComponents('Grafix', [TShadowButton]);
end;

end.

