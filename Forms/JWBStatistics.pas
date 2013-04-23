unit JWBStatistics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RXCtrls, Buttons;

type
  TfStatistics = class(TForm)
    BitBtn1: TBitBtn;
    RxLabel16: TRxLabel;
    Bevel1: TBevel;
    RxLabel1: TRxLabel;
    RxLabel2: TRxLabel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lblDicBuildDate: TLabel;
    lblKanjidicVersion: TLabel;
    lblUnihanVersion: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    procedure FormShow(Sender: TObject);
  end;

var
  fStatistics: TfStatistics;

implementation
uses JWBCharData;

{$R *.DFM}

procedure TfStatistics.FormShow(Sender: TObject);
begin
  lblDicBuildDate.Caption := CharDataProps.DicBuildDate;
  lblKanjidicVersion.Caption := CharDataProps.KanjidicVersion;
  lblUnihanVersion.Caption := CharDataProps.UnihanVersion;
end;

end.
