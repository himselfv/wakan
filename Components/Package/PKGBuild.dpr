program PKGBuild;

uses
  Forms,
  Dialogs,
  PKGWrite in 'PKGWrite.pas' {PKGWriteForm};

{$R *.RES}

var t:textfile;
    s:string;

begin
  Application.Initialize;
  Application.CreateForm(TPKGWriteForm, PKGWriteForm);
  if paramcount<1 then
  begin
    showmessage('Package Builder (C) LABYRINTH 1999'+#13+
    'Usage: PKGBUILD <command file>');
    Application.Terminate;
    exit;
  end;
  assignfile(t,paramstr(1));
  reset(t);
  while not eof(t) do
  begin
    readln(t,s);
    if not PKGWriteForm.PKGWriteCmd(s) then
    begin
      showmessage('Package Builder was terminated.');
      Application.Terminate;
      exit;
    end;
  end;
  closefile(t);
end.
