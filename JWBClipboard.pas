unit JWBClipboard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type
  TfClipboard = class(TForm)
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fClipboard: TfClipboard;

implementation

uses JWBMenu;

{$R *.DFM}

procedure TfClipboard.Timer1Timer(Sender: TObject);
begin
  fMenu.Clipboard_Timer1Timer(sender);
end;

procedure TfClipboard.PaintBox3Paint(Sender: TObject);
begin
  fMenu.Clipboard_PaintBox3Paint(sender);
end;

procedure TfClipboard.SpeedButton7Click(Sender: TObject);
begin
  fMenu.Clipboard_SpeedButton7Click(sender);
end;

procedure TfClipboard.FormCreate(Sender: TObject);
begin
  fMenu.ResFixVal:=76-ClientHeight;
end;

end.
