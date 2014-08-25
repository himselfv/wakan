program SpeedBtnTest;

uses
  Vcl.Forms,
  SpeedBtnTest_Main in 'SpeedBtnTest_Main.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
