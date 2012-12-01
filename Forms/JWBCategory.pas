unit JWBCategory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Tabs, Buttons;

type
  TfUserCategory = class(TForm)
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TabSet1: TTabSet;
    ListBox1: TCheckListBox;
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserCategory: TfUserCategory;

implementation

uses JWBWords;

{$R *.DFM}

procedure TfUserCategory.ListBox1Click(Sender: TObject);
begin
  fWords.ShowIt;
end;

procedure TfUserCategory.ListBox1DblClick(Sender: TObject);
var i:integer;
begin
  if ListBox1.ItemIndex<>-1 then
  begin
    for i:=0 to ListBox1.Items.Count-1 do
      ListBox1.Checked[i]:=i=ListBox1.ItemIndex;
    ShowIt;
  end;
end;

end.
