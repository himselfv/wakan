program FCharConv;

uses
  Forms,
  JWBFCharConvert in 'Forms\JWBFCharConvert.pas' {FCharConvert};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFCharConvert, FCharConvert);
  Application.Run;
end.
