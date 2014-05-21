unit WakanPaintbox;

interface

uses
  SysUtils, Classes, Types, Messages, Graphics, Controls;

type
  TEdgeBorder = (ebLeft, ebTop, ebRight, ebBottom);
  TEdgeBorders = set of TEdgeBorder;

 { Basic custom painted panel, supports simple Wakan-style border, double-buffering }
  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas) of object;
  TWakanPaintbox = class(TCustomControl)
  protected
    FEdgeBorders: TEdgeBorders;
    FOnPaint: TPaintEvent;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
    procedure SetEdgeBorders(const Value: TEdgeBorders);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Canvas;
    property Color default clNone;
    property DoubleBuffered;
    property EdgeBorders: TEdgeBorders read FEdgeBorders write SetEdgeBorders
      stored true default [ebLeft, ebTop, ebRight, ebBottom];
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint stored true;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;

  {
  White panel with borders and a message somehwere around the top left corner.
  Used by Wakan to show instead of controls when there are no items avaiable.
  }
  TBlankPanel = class(TWakanPaintbox)
  protected
    FTextLeft: integer;
    FTextTop: integer;
    FTextVisible: boolean;
    procedure Paint; override;
    procedure SetTextLeft(const Value: integer);
    procedure SetTextTop(const Value: integer);
    procedure SetTextVisible(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TextLeft: integer read FTextLeft write SetTextLeft stored true;
    property TextTop: integer read FTextTop write SetTextTop stored true;
    property Text stored true;
    property TextVisible: boolean read FTextVisible write SetTextVisible stored true default true;
    property Font stored true;
    property Color stored true default clWhite;
  end;

procedure Register;

implementation

{ TWakanPaintbox }

constructor TWakanPaintbox.Create(AOwner: TComponent);
begin
  inherited;
 //Initialize to default values
  Color := clWhite;
  BorderWidth := 0;
  EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
end;

procedure TWakanPaintbox.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0; //we do erasing on paint
end;

procedure TWakanPaintbox.Paint;
var r: TRect;
begin
  if Color<>clNone then begin
   //Draw default background box
    r := GetClientRect;
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    if EdgeBorders=[ebLeft, ebTop, ebRight, ebBottom] then begin
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psSolid; //draw now
      Canvas.Pen.Width := 1;
    end else begin
      Canvas.Pen.Style := psClear; //later
      Canvas.Pen.Width := 0;
      Inc(r.Right); //fill border too
      Inc(r.Bottom);
    end;
    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
    if EdgeBorders<>[ebLeft, ebTop, ebRight, ebBottom] then begin
      Dec(r.Right, 1); //decrease back
      Dec(r.Bottom, 1);
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      if ebLeft in EdgeBorders then begin
        Canvas.MoveTo(r.Left, r.Top);
        Canvas.LineTo(r.Left, r.Bottom-1);
      end;
      if ebTop in EdgeBorders then begin
        Canvas.MoveTo(r.Left, r.Top);
        Canvas.LineTo(r.Right-1, r.Top);
      end;
      if ebRight in EdgeBorders then begin
        Canvas.MoveTo(r.Right-1, r.Top);
        Canvas.LineTo(r.Right-1, r.Bottom-1);
      end;
      if ebBottom in EdgeBorders then begin
        Canvas.MoveTo(r.Left, r.Bottom-1);
        Canvas.LineTo(r.Right-1, r.Bottom-1);
      end;
    end;
  end;
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

procedure TWakanPaintbox.SetEdgeBorders(const Value: TEdgeBorders);
begin
  FEdgeBorders := Value;
  Invalidate;
end;


{ TBlankPanel }

constructor TBlankPanel.Create(AOwner: TComponent);
begin
  inherited;
 //Initialize to default values
  Color := clWhite;
  BorderWidth := 0;
  Font.Name := 'Arial';
  Font.Style := [fsBold];
  Font.Size := 12;
  FTextLeft := 20;
  FTextTop := 27;
  FTextVisible := true;
end;

procedure TBlankPanel.Paint;
begin
  inherited Paint; //draws a border
  if FTextVisible then begin
    Canvas.Font.Assign(Self.Font);
    Canvas.TextOut(FTextLeft, FTextTop, Self.Text);
  end;
end;

procedure TBlankPanel.SetTextLeft(const Value: integer);
begin
  FTextLeft := Value;
  Invalidate;
end;

procedure TBlankPanel.SetTextTop(const Value: integer);
begin
  FTextTop := Value;
  Invalidate;
end;

procedure TBlankPanel.SetTextVisible(const Value: boolean);
begin
  FTextVisible := Value;
  Invalidate;
end;


procedure Register;
begin
  RegisterComponents('Wakan', [TWakanPaintbox]);
  RegisterComponents('Wakan', [TBlankPanel]);
end;

end.
