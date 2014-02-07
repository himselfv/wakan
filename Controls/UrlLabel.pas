unit UrlLabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls;

type
  TUrlLabel = class(TLabel)
  protected
    FURL: string;
    procedure Click; override;
  published
    property URL: string read FURL write FURL;
  end;

procedure Register;

implementation
uses JwbUtils;

procedure TUrlLabel.Click;
begin
  if URL<>'' then
    ShellOpen(URL);
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TUrlLabel]);
end;

end.
