unit JWBLayout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfLayout = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Label2: TLabel;
    TrackBar1: TTrackBar;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fLayout: TfLayout;

implementation

uses JWBMenu;

{$R *.DFM}

procedure TfLayout.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    fMenu.WriteLayout(SaveDialog1.FileName);
  Close;
end;

procedure TfLayout.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    fMenu.ReadLayout(OpenDialog1.FileName);
  Close;
end;

procedure TfLayout.FormShow(Sender: TObject);
begin
  ListBox1.ItemIndex:=0;
end;

procedure TfLayout.Button1Click(Sender: TObject);
begin
  fMenu.StandardLayout(ListBox1.ItemIndex,TrackBar1.Position);
  Close;
end;

end.
