unit JWBBitmap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, JwbForms;

type
  TfBitmap = class(TJwbForm)
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label4: TLabel;
    procedure BitBtn1Click(Sender: TObject);
  protected
    FIntWidth: integer;
    FIntHeight: integer;
  end;

  TBitmapParams = record
    Filename: string;
    Width: integer;
    Height: integer;
  end;

function QuerySaveToBitmap(out AParams: TBitmapParams): boolean;

implementation

{$R *.DFM}

procedure TfBitmap.BitBtn1Click(Sender: TObject);
begin
  FIntWidth := StrToInt(Edit1.Text);
  FIntHeight := StrToInt(Edit2.Text);
end;

function QuerySaveToBitmap(out AParams: TBitmapParams): boolean;
var fBitmap: TfBitmap;
begin
  fBitmap := TfBitmap.Create(Application);
  try
    Result := (fBitmap.ShowModal=mrOK);
    if not Result then exit;
    AParams.Width := fBitmap.FIntWidth;
    AParams.Height := fBitmap.FIntHeight;
    AParams.Filename := fBitmap.Edit3.Text;
  finally
    FreeAndNil(fBitmap);
  end;
end;


end.
