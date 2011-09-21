program PKGTest;

uses
  Forms,
  PKGTestMain in 'PKGTestMain.pas' {Form1},
  MemSource in 'MemSource.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
