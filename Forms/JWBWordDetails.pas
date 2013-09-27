unit JWBWordDetails;
{ Displays word, reading and meaning for the currently selected word in
 dictionary lookup results.
 Has no use at this time -- dictionary results provide more info as it stands. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, WakanPaintbox;

type
  TfWordDetails = class(TForm)
    Bevel: TPanel;
    Paintbox5: TWakanPaintbox;
    Paintbox2: TWakanPaintbox;
    btnAddToClipboard: TSpeedButton;
    Paintbox1: TWakanPaintbox;
    procedure btnAddToClipboardClick(Sender: TObject);
    procedure Paintbox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Paintbox2Paint(Sender: TObject; Canvas: TCanvas);
    procedure Paintbox5Paint(Sender: TObject; Canvas: TCanvas);
  end;

var
  fWordDetails: TfWordDetails;

implementation
uses JWBWordLookup, JWBMenu, JWBStrings, JWBUnit;

{$R *.DFM}

procedure TfWordDetails.btnAddToClipboardClick(Sender: TObject);
begin
  fWordLookup.btnCopyToClipboardClick(Sender);
end;

procedure TfWordDetails.Paintbox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  if showroma then
    DrawUnicode(Canvas,2,2,22,KanaToRomaji(fWordLookup.curphonetic,romasys,curlang),FontEnglish)
  else
    DrawUnicode(Canvas,2,2,22,fWordLookup.curphonetic,FontJapanese);
end;

procedure TfWordDetails.Paintbox2Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  DrawUnicode(Canvas,2,2,22,fWordLookup.curkanji,FontJapanese);
end;

procedure TfWordDetails.Paintbox5Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  DrawUnicode(Canvas,2,2,22,fstr(remmark(fWordLookup.curmeaning)),FontEnglish);
end;

end.
