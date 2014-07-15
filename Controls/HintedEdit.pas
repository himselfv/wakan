unit HintedEdit;

interface
uses
  SysUtils, Classes, Messages, Controls, StdCtrls, ExtCtrls;

type
  THintedEdit = class(TEdit)
  protected
    FCanvas: TControlCanvas;
    FTitle: string;
    procedure SetTitle(const Value: string);
    procedure UpdateEditMargins;
    procedure WndProc(var Message: TMessage); override;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read FTitle write SetTitle;
  end;

procedure Register;

implementation
uses Windows;

procedure Register;
begin
  RegisterComponents('Wakan', [THintedEdit]);
end;

constructor THintedEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
end;

destructor THintedEdit.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited;
end;

procedure THintedEdit.SetTitle(const Value: string);
begin
  FTitle := Value;
  Self.Invalidate;
end;

procedure THintedEdit.UpdateEditMargins;
var LMargin: Integer;
begin
  if HandleAllocated then begin
    FCanvas.Font := Font;
    LMargin := FCanvas.TextWidth(FTitle);
    SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN,  MakeLong(LMargin, 0));
    Invalidate;
  end;
end;

procedure THintedEdit.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_FONTCHANGED:
      if not (csLoading in ComponentState) then
        UpdateEditMargins;
  end;
end;

procedure THintedEdit.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;


end.
