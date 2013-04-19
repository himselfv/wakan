unit WinSpeedButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows, Buttons;

type
 { TSpeedButton replacement which descends from TWinControl.
  Not everything can be reimplemented; for some use cases you're better off
  with single-button TToolBar.

  What TWinSpeedButton has:
  - No focus
  - Default (3D) look

  What it hasn't:
  - Flat look
  - Down property.

  Problems:
  - BS_FLAT doesn't work since XP.
  - Cannot make a focus-less control out of TButton descendant.
   There's a way to disable Delphi auto-focus on LBUTTONDOWN by overriding Focused(),
   but Windows internal handling is both critical for handling clicks and does
   auto-focus too. }
  TWinSpeedButton = class(TBitBtn)
  protected
    FLastFocusSource: HWND;
//    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
//    procedure WMMouseUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    function Focused: Boolean; override;
  published
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

{
procedure TWinSpeedButton.WMMouseActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := MA_NOACTIVATE;
end;
}


procedure TWinSpeedButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FLastFocusSource := Message.FocusedWnd;
end;

{
procedure TWinSpeedButton.WMMouseUp(var Message: TMessage);
begin
  inherited;
  Windows.SetFocus(FLastFocusSource);
end;
}

function TWinSpeedButton.Focused: Boolean;
begin
  Result := true; //always focused, ya ha-ha ha ha!
end;

procedure TWinSpeedButton.WndProc(var Message: TMessage);
var tmp: pointer;
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
    begin
     //TButtonControl does some nasty focus stealing in WndProc so we skip that stage
     //...or we would like to. But there's no way.
     //But it checks for Focused before continuing so we'll just hack Focused.
     tmp := Self.DefWndProc;
     Self.DefWndProc := nil;
     inherited WndProc(Message);
     Self.DefWndProc := tmp;
    end
  else
    inherited WndProc(Message);
  end;
end;


procedure Register;
begin
//  RegisterComponents('Samples', [TWinSpeedButton]);
end;

end.
