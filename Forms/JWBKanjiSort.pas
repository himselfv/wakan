unit JWBKanjiSort;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfKanjiSort = class(TForm)
    Bevel1: TBevel;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fKanjiSort: TfKanjiSort;

implementation

uses JWBKanji, JWBMenu;

{$R *.DFM}

procedure TfKanjiSort.RadioGroup1Click(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSort.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fKanji.SpeedButton1.Down:=false;
  fMenu.aKanjiSort.Checked:=false;
end;

end.
