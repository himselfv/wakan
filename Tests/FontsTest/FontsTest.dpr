program FontsTest;

uses
  Vcl.Forms,
  FontsTest_Main in 'FontsTest_Main.pas' {FontTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFontTestForm, FontTestForm);
  Application.Run;
end.
