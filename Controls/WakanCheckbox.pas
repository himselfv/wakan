unit WakanCheckbox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages;

type
 {
  Additions:
  - Autosize
 }
  TWakanCheckbox = class(TCheckBox)
  protected
    FAutoSize: Boolean;
    procedure AdjustBounds; dynamic;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
  end;

procedure Register;

implementation
uses Windows;

procedure Register;
begin
  RegisterComponents('Wakan', [TWakanCheckbox]);
end;

constructor TWakanCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
end;

//See TCustomLabel.SetAutoSize
procedure TWakanCheckbox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TWakanCheckbox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

//See TCustomLabel.AdjustBounds
procedure TWakanCheckbox.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  X, W: Integer;
  Canvas: TControlCanvas;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    X := Left;
    Canvas := TControlCanvas.Create;
    try
      Canvas.Control := Self;
      W := GetSystemMetrics(SM_CXMENUCHECK) + 4 + Canvas.TextWidth(Self.Caption);
      SetBounds(X, Top, X+W, Height);
    finally
      FreeAndNil(Canvas);
    end;
  end;
end;

end.
