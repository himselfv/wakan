unit JWBMedia;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw, Tabs, ExtCtrls;

type
  TfMedia = class(TForm)
    TabSet1: TTabSet;
    WebBrowser1: TWebBrowser;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure WebBrowser1DocumentComplete(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
  private
    { Private declarations }
  public
    media:TStringList;
    { Public declarations }
  end;

var
  fMedia: TfMedia;

implementation

uses JWBMenu;

{$R *.DFM}

procedure TfMedia.FormCreate(Sender: TObject);
begin
  media:=TStringList.Create;
end;

procedure TfMedia.FormDestroy(Sender: TObject);
begin
  media.Free;
end;

procedure TfMedia.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  if pos('://',media[NewTab])>0 then
  begin
    WebBrowser1.Navigate(media[NewTab]);
    WebBrowser1.Show;
    Image1.Hide;
  end else
  begin
    Image1.Show;
    try
      Image1.Picture.LoadFromFile(media[NewTab]);
    except Image1.Hide; end;
    WebBrowser1.Hide;
  end;
end;

procedure TfMedia.WebBrowser1DocumentComplete(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  fMenu.SetFocus;
end;

end.
