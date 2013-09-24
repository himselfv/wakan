program WindowPlacementTest;
{ An app to test the effects of GetWindowPlacement/SetWindowPlacement and how it
 coordinates with Delphi internal visibility etc bookkeeping.
 
 In short:
 1. No changes done to Visiblity by SetWindowPlacement are reflected in Delphi.
  You MUST call appropriate Delphi routines after doing SetWindowPlacement, e.g.:
  	SetWindowPlacement(...SW_SHOW);
  	Window.Visible:=true;
 2. Changes to WindowState are coordinated with Delphi:
 	SetWindowPlacement(...SW_MAXIMIZE);
 	> Window.WindowState = wsMaximized
 3. Changes to window position and size are mostly coordinated with Delphi.
 4. When SetWindowPlacement puts the window onto another monitor, after doing
  Window.Visible:=true it's moved to the same position but on the monitor where
  it was hidden.
}

uses
  Vcl.Forms,
  WindowPlacementTest_Control in 'WindowPlacementTest_Control.pas' {Form1},
  WindowPlacementTest_Target in 'WindowPlacementTest_Target.pas' {TargetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
