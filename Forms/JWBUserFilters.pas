unit JWBUserFilters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Tabs, Buttons;

type
  TfUserFilters = class(TForm)
    RadioGroup2: TRadioGroup;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox11: TCheckBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TabSet1: TTabSet;
    ListBox1: TCheckListBox;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserFilters: TfUserFilters;

implementation

uses JWBWords, JWBMenu, JWBUnit;

{$R *.DFM}

procedure TfUserFilters.CheckBox1Click(Sender: TObject);
begin
  fWords.ShowIt(false);
end;

procedure TfUserFilters.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fWords.SpeedButton2.Down:=false;
  fMenu.aUserSettings.Checked:=false;
end;

procedure TfUserFilters.ListBox1Click(Sender: TObject);
begin
  fWords.ShowIt(false);
end;

procedure TfUserFilters.ListBox1DblClick(Sender: TObject);
var i:integer;
begin
  if ListBox1.ItemIndex<>-1 then ListBox1.Checked[ListBox1.ItemIndex]:=not
    ListBox1.Checked[ListBox1.ItemIndex];
  exit;
  if ListBox1.ItemIndex<>-1 then
  begin
    for i:=0 to ListBox1.Items.Count-1 do
      ListBox1.Checked[i]:=i=ListBox1.ItemIndex;
    fWords.ShowIt(false);
  end;
end;

procedure TfUserFilters.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var i:integer;
    lc:char;
    s:string;
begin
  ListBox1.Items.Clear;
  TUserCat.SetOrder('Name_Ind');
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    if chr(TUserCat.Int(TUserCatType))=TabSet1.Tabs[NewTab] then
    begin
      lc:='j';
      s:=TUserCat.Str(TUserCatName);
      if (length(s)>1) and (s[2]='~') then lc:=s[1];
      if lc=curlang then
        ListBox1.Items.Add(StripCatName(TUserCat.Str(TUserCatName)));
    end;
    TUserCat.Next;
  end;
  for i:=0 to ListBox1.Items.Count-1 do ListBox1.Checked[i]:=true;
  ListBox1.ItemIndex:=0;
  fWords.ShowIt(false);
end;

procedure TfUserFilters.SpeedButton1Click(Sender: TObject);
var i:integer;
    allchecked:boolean;
begin
  allchecked:=true;
  for i:=0 to ListBox1.Items.Count-1 do if not ListBox1.Checked[i] then allchecked:=false;
  for i:=0 to ListBox1.Items.Count-1 do ListBox1.Checked[i]:=not allchecked;
  fWords.ShowIt(false);
end;

procedure TfUserFilters.SpeedButton2Click(Sender: TObject);
begin
  fWords.UserCategory_SpeedButton2Click(sender);
end;

procedure TfUserFilters.SpeedButton3Click(Sender: TObject);
begin
  fWords.UserCategory_SpeedButton3Click(sender);
end;

end.
