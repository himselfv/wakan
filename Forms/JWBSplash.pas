unit JWBSplash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, UrlLabel, JwbForms;

type
  TfSplash = class(TJwbForm)
    Panel1: TPanel;
    lblVersion: TLabel;
    Image1: TImage;
    Bevel1: TBevel;
    lblURL: TUrlLabel;
    BitBtn1: TBitBtn;
    Memo1: TMemo;
    Shape1: TShape;
    lblCopyright: TLabel;
    lblTargetOS: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

implementation
uses JWBCore;

{$R *.DFM}

procedure TfSplash.FormCreate(Sender: TObject);
begin
  lblVersion.Caption:=WakanVer;
  lblCopyright.Caption:=WakanCopyright;
  lblUrl.URL := AppUrl;
end;

end.
