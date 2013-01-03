unit JWBDictMan;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, Buttons;

type
  TfDictMan = class(TForm)
    Label1: TLabel;
    cbDicts: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label4: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    CheckBox1: TCheckBox;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    procedure cbDictsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);

  protected
    procedure UpdateUpDownButtons;
  public
    procedure ReloadDicts;
    procedure CarefulRefreshDicts;


  end;

var
  fDictMan: TfDictMan;

implementation

uses JWBMenu, JWBDictImport, JWBDic, JWBDicSearch;

{$R *.DFM}

procedure TfDictMan.FormShow(Sender: TObject);
begin
  ReloadDicts();
  cbDictsClick(self);
  ModalResult := mrNone;
end;

procedure TfDictMan.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
    dic:TJaletDic;
begin
  if not IsPositiveResult(ModalResult) then exit; //don't save on cancel

  dicts.Priority.Clear;
  for i:=0 to cbDicts.Items.Count-1 do
  begin
    dicts.Priority.Add(cbDicts.Items[i]);
    dic:=dicts.Find(cbDicts.Items[i]);
    if dic.loaded<>cbDicts.Checked[i] then
      if cbDicts.Checked[i] then dic.Load else dic.Unload;
  end;
end;

procedure TfDictMan.btnOkClick(Sender: TObject);
var i:integer;
begin
  for i:=0 to cbDicts.Items.Count-1 do
    dicts.PutInGroup(cbDicts.Items[i],GROUP_NOTUSED,not cbDicts.Checked[i]);
end;

procedure TfDictMan.ReloadDicts;
var i: integer;
  dic: TJaletDic;
begin
  cbDicts.Clear;
 //First we load priority dicts
  for i := 0 to dicts.Priority.Count - 1 do begin
    dic := dicts.Find(dicts.Priority[i]);
    if dic=nil then continue;
    cbDicts.Items.AddObject(dic.name, dic);
  end;
 //Then the rest of them
  for i := 0 to dicts.Count - 1 do begin
    dic := dicts[i];
    if cbDicts.Items.IndexOfObject(dic)>=0 then continue; //already added
    cbDicts.Items.AddObject(dic.name, dic);
  end;
 //Finally, checkbox states
  for i := 0 to cbDicts.Count - 1 do
    cbDicts.Checked[i]:= not dicts.IsInGroup(cbDicts.Items[i],GROUP_NOTUSED);
  cbDicts.ItemIndex:=0
end;

{
Reloads dictionary list while preserving user changes to it.
}

type
  TDicCheckState = record
    name: string;
    state: boolean;
  end;

procedure TfDictMan.CarefulRefreshDicts;
var dicStates: array of TDicCheckState;
  dic: TJaletDic;
  i,j: integer;
begin
 //Save states
  SetLength(dicStates, cbDicts.Items.Count);
  for i:=0 to cbDicts.Items.Count-1 do
  begin
    dic:=dicts[i];
    dicStates[i].name := dic.name;
    dicStates[i].state := cbDicts.Checked[i];
  end;

  fMenu.RescanDicts();
  ReloadDicts(); //might introduce new dictionaries

 //Restore states for known dictionaries
  for i := 0 to Length(dicStates) - 1 do
    for j :=0 to cbDicts.Items.Count-1 do
      if cbDicts.Items[j]=dicStates[i].name then begin
        cbDicts.Checked[j] := dicStates[i].state;
        break; //of j-loop
      end;
end;

procedure TfDictMan.cbDictsClick(Sender: TObject);
var dic:TJaletDic;
begin
  UpdateUpDownButtons;
  if cbDicts.ItemIndex=-1 then exit;
  dic:=dicts.Find(cbDicts.Items[cbDicts.ItemIndex]);
  label4.Caption:=dic.name;
  label10.caption:=dic.pname;
  label11.caption:=dic.version;
  if dic.entries=-1 then label12.caption:='N/A' else label12.caption:=inttostr(dic.entries);
  label13.caption:=datetostr(dic.builddate);
  label14.caption:=inttostr(dic.priority);
  label15.caption:=dic.description;
  label17.caption:=dic.copyright;
  if dic.hasWordIndex then label19.Caption:=_l('#00115^ePresent') else label19.Caption:=_l('#00116^eAbsent');
  if dic.hasCharIndex then label21.Caption:=_l('#00115^ePresent') else label21.Caption:=_l('#00116^eAbsent');
  SpeedButton1.Down:=dicts.IsInGroup(dic, 1);
  SpeedButton2.Down:=dicts.IsInGroup(dic, 2);
  SpeedButton3.Down:=dicts.IsInGroup(dic, 3);
  SpeedButton4.Down:=dicts.IsInGroup(dic, 4);
  SpeedButton5.Down:=dicts.IsInGroup(dic, 5);
  CheckBox1.Checked:=not dicts.IsInGroup(dic, GROUP_OFFLINE);
end;

procedure TfDictMan.Button1Click(Sender: TObject);
begin
  CarefulRefreshDicts;
end;

procedure TfDictMan.Button2Click(Sender: TObject);
begin
  if IsPositiveResult(fDictImport.ShowModal) then
    CarefulRefreshDicts;
end;

procedure TfDictMan.SpeedButton1Click(Sender: TObject);
var s:string;
begin
  s:=cbDicts.Items[cbDicts.ItemIndex];
  dicts.PutInGroup(s,1,SpeedButton1.Down);
  dicts.PutInGroup(s,2,SpeedButton2.Down);
  dicts.PutInGroup(s,3,SpeedButton3.Down);
  dicts.PutInGroup(s,4,SpeedButton4.Down);
  dicts.PutInGroup(s,5,SpeedButton5.Down);
  dicts.PutInGroup(s,GROUP_OFFLINE,CheckBox1.Checked);
end;

procedure TfDictMan.UpdateUpDownButtons;
begin
  btnMoveUp.Enabled := (cbDicts.ItemIndex>0);
  btnMoveDown.Enabled := (cbDicts.ItemIndex>=0) and (cbDicts.ItemIndex<cbDicts.Count);
end;

procedure TfDictMan.btnMoveUpClick(Sender: TObject);
begin
  cbDicts.Items.Exchange(cbDicts.ItemIndex, cbDicts.ItemIndex-1);
  UpdateUpDownButtons;
end;

procedure TfDictMan.btnMoveDownClick(Sender: TObject);
begin
  cbDicts.Items.Exchange(cbDicts.ItemIndex, cbDicts.ItemIndex+1);
  UpdateUpDownButtons;
end;

end.
