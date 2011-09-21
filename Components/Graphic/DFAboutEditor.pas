unit DFAboutEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ShellAPI;

type
  TDFCmpAbout = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    Label3: TLabel;
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowCmpAbout;

implementation

{$R *.dfm}
{$R MyFace2.Res}

procedure TDFCmpAbout.Label1Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open', PChar(Label1.Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TDFCmpAbout.Label2Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open', PChar(Label2.Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure ShowCmpAbout;
begin
  with TDFCmpAbout.Create(Application) do
  begin
  	try
    	ShowModal;
    finally
    	Free;
    end;
  end;
end;

procedure TDFCmpAbout.FormCreate(Sender: TObject);
begin
   Image1.Picture.Bitmap.LoadFromResourceName(HINSTANCE, 'MYFACE');
end;

end.
