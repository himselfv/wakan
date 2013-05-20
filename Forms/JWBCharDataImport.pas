unit JWBCharDataImport;
{
Updates or re-imports character database from sources.
Currently only KANJIDIC is supported (and as a consequence no full reimport is
possible).
Only full replacement of all relevant properties is supported (impossible to
add translations while keeping existing ones at this point).
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfCharDataImport = class(TForm)
    Label1: TLabel;
    edtKanjidicFile: TEdit;
    btnKanjidicBrowse: TButton;
    Label2: TLabel;
    btnUpdate: TButton;
    OpenKanjidicDialog: TOpenDialog;
    procedure btnKanjidicBrowseClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  end;

var
  fCharDataImport: TfCharDataImport;

procedure UpdateKanjidicData(const KanjidicFilename: string);

implementation
uses JWBCharData;

{$R *.dfm}

procedure TfCharDataImport.btnKanjidicBrowseClick(Sender: TObject);
begin
  with OpenKanjidicDialog do
    if Execute then
      edtKanjidicFile.Text := Filename;
end;

procedure TfCharDataImport.btnUpdateClick(Sender: TObject);
begin
 //Not really needed but let's check beforehand
  if not FileExists(edtKanjidicFile.Text) then
    raise Exception.Create('File '+edtKanjidicFile.Text+' does not exist!');
  UpdateKanjidicData(edtKanjidicFile.Text);
end;

procedure UpdateKanjidicData(const KanjidicFilename: string);
begin

end;

end.
