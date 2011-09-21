unit ActivListForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ColorBtns, DFClasses, Backgnd, ExtCtrls, ClrPanel, StdCtrls;

type
  TForm1 = class(TForm)
    ColorPanel1: TColorPanel;
    Background1: TBackground;
    Color95Button1: TColor95Button;
    Color95Button2: TColor95Button;
    Color95Button3: TColor95Button;
    Color95Button4: TColor95Button;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    procedure Color95Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Color95Button1Click(Sender: TObject);
begin
	ShowMessage(TColor95Button(Sender).Caption);
end;





end.
