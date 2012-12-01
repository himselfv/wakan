unit JWBReport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QuickRpt, Qrctrls, ExtCtrls;

type
  TfReport = class(TForm)
    QuickRep1: TQuickRep;
    TitleBand1: TQRBand;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    DetailBand1: TQRBand;
    QRLabel4: TQRLabel;
    QRDBText1: TQRDBText;
    QRRichText1: TQRRichText;
    procedure DetailBand1BeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fReport: TfReport;

implementation

uses JWBMain;

{$R *.DFM}

function HexToUnicode(s:string):widestring;
var s2:widestring;
    d:word;
    c:widechar;
    i:integer;
begin
  s2:='';
  for i:=1 to length(s) div 4 do
  begin
    d:=StrToInt('0x'+copy(s,(i-1)*4+1,4));
    c:=widechar(d);
    s2:=s2+c;
  end;
  result:=s2;
end;

procedure TfReport.DetailBand1BeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  QRRichText1.Lines.Clear;
  QRRichText1.Lines.Add('{\rtf1\ansi\ansicpg1250\deff0\deflang1029\''82\''d0}'{+HexToUnicode(+fMain.tblKanji.AsString)});
end;

end.
