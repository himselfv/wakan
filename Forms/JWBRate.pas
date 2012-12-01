unit JWBRate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfRate = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    sc:integer;
    { Public declarations }
  end;

var
  fRate: TfRate;

implementation

{$R *.DFM}

procedure TfRate.Button1Click(Sender: TObject);
begin
  sc:=0;
  close;
end;

procedure TfRate.Button2Click(Sender: TObject);
begin
  sc:=1;
  close;
end;

procedure TfRate.Button3Click(Sender: TObject);
begin
  sc:=2;
  close;
end;

procedure TfRate.Button4Click(Sender: TObject);
begin
  sc:=3;
  close;
end;

end.
