unit JWBDictCoding;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JwbForms;

type
  TfDictCoding = class(TJwbForm)
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  public
    succeeded:boolean;
  end;

implementation
uses JWBLanguage;

{$R *.DFM}

procedure TfDictCoding.BitBtn1Click(Sender: TObject);
begin
  if (RadioGroup1.ItemIndex>0) and (not FileExists('UniConv.exe')) then
  begin
    Application.MessageBox(
      pchar(_l('#00070^eUNICONV.EXE was not found. It is required for encoding '
        +'conversion.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
    exit;
  end;
  succeeded:=true;
  close;
end;

procedure TfDictCoding.BitBtn2Click(Sender: TObject);
begin
  succeeded:=false;
  close;
end;

end.
