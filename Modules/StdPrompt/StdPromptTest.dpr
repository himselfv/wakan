program StdPromptTest;

uses
  Forms,
  Dialogs,
  Windows,
  SysUtils,
  NicePrompt in 'NicePrompt.pas' {SMPromptForm};

{$R *.RES}

var a:TSMPromptForm;
    i:integer;

begin
  Application.Initialize;
  writesm('Ahoy');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Begone to faerie land.');
  writesm('Show me the hero.');
  if smsask(warningstyle,[mbOK,mbAll,mbNoToAll,mbYesToAll],'Warning to all the citizems and all places')=mbYesToAll then
  smprompt(errorstyle,'Warning','YES TO ALL!');
  a:=SMProgressDlg('Ahoy','MESSAGE',10);
  for i:=1 to 10 do
  begin
    sleep(100);
    a.SetProgress(i);
  end;
  a.Free;
end.