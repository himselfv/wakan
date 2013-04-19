unit WinSpeedButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows;

type
  TWinSpeedButton = class(TButton)
  protected
    FFlat: boolean;
    procedure SetFlat(Value: boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Flat: boolean read FFlat write SetFlat stored true default false;
    property TabStop default false;
  end;

procedure Register;

implementation

constructor TWinSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
 //Initialize to default values
  TabStop := false;
end;

procedure TWinSpeedButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FFlat then
    Params.Style := Params.Style or BS_FLAT
  else
    Params.Style := Params.Style and not BS_FLAT;
end;

procedure TWinSpeedButton.SetFlat(Value: boolean);
begin
  FFlat := Value;
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or BS_FLAT);
  Invalidate;
end;

procedure TWinSpeedButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Windows.SetFocus(Message.FocusedWnd);
end;


procedure Register;
begin
  RegisterComponents('Samples', [TWinSpeedButton]);
end;

end.
