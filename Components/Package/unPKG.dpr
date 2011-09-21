program unPKG;

uses
  Forms,
  unPKGmain in 'unPKGmain.pas' {Form1},
  MemSource in 'MemSource.pas',
  unPKGwait in 'unPKGwait.pas' {UnPKGWaitForm},
  unPKGview in 'unPKGview.pas' {ViewForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TUnPKGWaitForm, UnPKGWaitForm);
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.
