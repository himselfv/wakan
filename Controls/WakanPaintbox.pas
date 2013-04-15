unit WakanPaintbox;

interface

uses
  SysUtils, Classes, Types, Messages, Graphics, Controls;

type
  TWakanPaintbox = class(TCustomControl)
  protected
    FOnPaint: TNotifyEvent;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color default clNone;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint stored true;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;

procedure Register;

implementation

constructor TWakanPaintbox.Create(AOwner: TComponent);
begin
  inherited;
 //Initialize to default values
  Color := clNone;
  BorderWidth := 0;
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
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
  end;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;


procedure Register;
begin
  RegisterComponents('Wakan', [TWakanPaintbox]);
end;

end.
