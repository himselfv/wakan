program JaletBuild;

uses
  Forms,
  JWBBuild in 'JWBBuild.pas' {Form1},
  MultiLang in '..\MultiLang\MultiLang.pas' {MultiLangForm},
  StdPrompt in '..\StdPrompt\StdPrompt.pas' {SMPromptForm},
  PKGWrite in '..\Package\PKGWrite.pas' {PKGWriteForm},
  JWBBuildSet in 'JWBBuildSet.pas' {Form2},
  TextTable in '..\TextTable\TextTable.pas',
  MemSource in '..\Package\MemSource.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TMultiLangForm, MultiLangForm);
  Application.CreateForm(TPKGWriteForm, PKGWriteForm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
