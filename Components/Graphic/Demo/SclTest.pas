unit SclTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DFClasses, DFCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    DFScroll1: TDFScroll;
    Label1: TLabel;
    Label2: TLabel;
    procedure DFScroll1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DFScroll1Change(Sender: TObject);
begin
	Application.ProcessMessages;
	Label1.Caption := 'Position : ' + IntToStr(DFScroll1.Position);
end;

end.
