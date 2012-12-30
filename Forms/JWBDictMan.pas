unit JWBDictMan;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, Buttons;

type
  TfDictMan = class(TForm)
    Label1: TLabel;
    CheckListBox1: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
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
    BitBtn1: TBitBtn;
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
    BitBtn2: TBitBtn;
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  public
    procedure ReloadCheckStates;
    procedure CarefulRefreshDicts;
  end;

var
  fDictMan: TfDictMan;

implementation

uses JWBMenu, JWBDictImport, JWBDicSearch;

{$R *.DFM}

procedure TfDictMan.FormShow(Sender: TObject);
begin
  ReloadCheckStates();
  CheckListBox1Click(self);
  ModalResult := mrNone;
end;

procedure TfDictMan.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
    dic:TJaletDic;
begin
  if not IsPositiveResult(ModalResult) then exit; //don't save on cancel
  for i:=0 to CheckListBox1.Items.Count-1 do
  begin
    dic:=dicts.Objects[i] as TJaletDic;
    if dic.loaded<>CheckListBox1.Checked[i] then
      if CheckListBox1.Checked[i] then dic.Load else dic.Unload;
  end;
end;

procedure TfDictMan.BitBtn1Click(Sender: TObject);
var i:integer;
    s:string;
begin
  for i:=0 to CheckListBox1.Items.Count-1 do
  begin
    s:=CheckListBox1.Items[i];
    if (CheckListBox1.Checked[i]) and (pos(','+s,NotUsedDicts)>0) then
      delete(NotUsedDicts,pos(','+s,NotUsedDicts),length(s)+1);
    if (not CheckListBox1.Checked[i]) and (pos(','+s,NotUsedDicts)=0) then
      NotUsedDicts:=NotUsedDicts+','+s;
  end;
end;

type
  TDicCheckState = record
    name: string;
    state: boolean;
  end;

{
Reloads dictionary list while preserving user changes to it.
}
procedure TfDictMan.CarefulRefreshDicts;
var dicStates: array of TDicCheckState;
  dic: TJaletDic;
  i,j: integer;
begin
 //Save states
  SetLength(dicStates, CheckListBox1.Items.Count);
  for i:=0 to CheckListBox1.Items.Count-1 do
  begin
    dic:=dicts.Objects[i] as TJaletDic;
    dicStates[i].name := dic.name;
    dicStates[i].state := CheckListBox1.Checked[i];
  end;

  fMenu.RescanDicts();
  ReloadCheckStates(); //for newly loaded dictionaries for which we have no saved state

 //Restore states
  for i := 0 to Length(dicStates) - 1 do
    for j :=0 to CheckListBox1.Items.Count-1 do
      if CheckListBox1.Items[j]=dicStates[i].name then begin
        CheckListBox1.Checked[j] := dicStates[i].state;
        break; //of j-loop
      end;
end;

{
Updates dictionary check states according to current (permanent) user preferences
}
procedure TfDictMan.ReloadCheckStates;
var dic:TJaletDic;
    i:integer;
begin
  for i:=0 to CheckListBox1.Items.Count-1 do
  begin
    dic:=dicts.Objects[i] as TJaletDic;
    CheckListBox1.Checked[i]:=pos(','+dic.name,NotUsedDicts)=0;
  end;
end;

procedure TfDictMan.CheckListBox1Click(Sender: TObject);
var dic:TJaletDic;
begin
  if CheckListBox1.ItemIndex=-1 then exit;
  dic:=dicts.Objects[CheckListBox1.ItemIndex] as TJaletDic;
  label4.Caption:=dic.name;
  label10.caption:=dic.pname;
  label11.caption:=dic.version;
  if dic.entries=-1 then label12.caption:='N/A'else label12.caption:=inttostr(dic.entries);
  label13.caption:=datetostr(dic.builddate);
  label14.caption:=inttostr(dic.priority);
  label15.caption:=dic.description;
  label17.caption:=dic.copyright;
  if dic.wordidx<>nil then label19.Caption:=_l('#00115^ePresent') else label19.Caption:=_l('#00116^eAbsent');
  if dic.charidx<>nil then label21.Caption:=_l('#00115^ePresent') else label21.Caption:=_l('#00116^eAbsent');
  SpeedButton1.Down:=pos(','+dic.name,NotGroupDicts[1])=0;
  SpeedButton2.Down:=pos(','+dic.name,NotGroupDicts[2])=0;
  SpeedButton3.Down:=pos(','+dic.name,NotGroupDicts[3])=0;
  SpeedButton4.Down:=pos(','+dic.name,NotGroupDicts[4])=0;
  SpeedButton5.Down:=pos(','+dic.name,NotGroupDicts[5])=0;
  CheckBox1.Checked:=pos(','+dic.name,OfflineDicts)=0;
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
  s:=CheckListBox1.Items[CheckListBox1.ItemIndex];
  if (SpeedButton1.Down) and (pos(','+s,NotGroupDicts[1])>0) then
    delete(NotGroupDicts[1],pos(','+s,NotGroupDicts[1]),length(s)+1);
  if (not SpeedButton1.Down) and (pos(','+s,NotGroupDicts[1])=0) then
    NotGroupDicts[1]:=NotGroupDicts[1]+','+s;
  if (SpeedButton2.Down) and (pos(','+s,NotGroupDicts[2])>0) then
    delete(NotGroupDicts[2],pos(','+s,NotGroupDicts[2]),length(s)+1);
  if (not SpeedButton2.Down) and (pos(','+s,NotGroupDicts[2])=0) then
    NotGroupDicts[2]:=NotGroupDicts[2]+','+s;
  if (SpeedButton3.Down) and (pos(','+s,NotGroupDicts[3])>0) then
    delete(NotGroupDicts[3],pos(','+s,NotGroupDicts[3]),length(s)+1);
  if (not SpeedButton3.Down) and (pos(','+s,NotGroupDicts[3])=0) then
    NotGroupDicts[3]:=NotGroupDicts[3]+','+s;
  if (SpeedButton4.Down) and (pos(','+s,NotGroupDicts[4])>0) then
    delete(NotGroupDicts[4],pos(','+s,NotGroupDicts[4]),length(s)+1);
  if (not SpeedButton4.Down) and (pos(','+s,NotGroupDicts[4])=0) then
    NotGroupDicts[4]:=NotGroupDicts[4]+','+s;
  if (SpeedButton5.Down) and (pos(','+s,NotGroupDicts[5])>0) then
    delete(NotGroupDicts[5],pos(','+s,NotGroupDicts[5]),length(s)+1);
  if (not SpeedButton5.Down) and (pos(','+s,NotGroupDicts[5])=0) then
    NotGroupDicts[5]:=NotGroupDicts[5]+','+s;
  if (CheckBox1.Checked) and (pos(','+s,OfflineDicts)>0) then
    delete(OfflineDicts,pos(','+s,OfflineDicts),length(s)+1);
  if (not CheckBox1.Checked) and (pos(','+s,OfflineDicts)=0) then
    OfflineDicts:=OfflineDicts+','+s;
end;

end.
