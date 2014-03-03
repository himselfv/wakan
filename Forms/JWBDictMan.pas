unit JWBDictMan;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, Buttons, JwbForms;

type
  TfDictMan = class(TJwbForm)
    Label1: TLabel;
    cbDicts: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblDicName: TLabel;
    lblWordEntries: TLabel;
    lblBuildDate: TLabel;
    lblDescription: TLabel;
    Label18: TLabel;
    lblWordIndex: TLabel;
    Label20: TLabel;
    lblCharIndex: TLabel;
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
    FLoadingData: boolean; //set when we are loading dict data -- do not handle checkbox changes etc
    procedure UpdateUpDownButtons;

  public
    procedure ReloadDicts;
    procedure CarefulRefreshDicts;

  end;

implementation
uses UITypes, JWBMenu, JWBLanguage, JWBDictImport, JWBDic, JWBDicSearch;

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
    if cbDicts.Items.IndexOfObject(dic)>=0 then continue; //safety for duplicates in priorities
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
  FLoadingData := true;
  try
    lblDicName.Caption := ChangeFileExt(ExtractFilename(dic.pname), ''); //We also have dic.name, but it's deprecated

    lblDescription.Caption := dic.description;
    if dic.version<>'' then
      lblDescription.Caption := lblDescription.Caption + #13
        +_l('#00075^Version:')+' '+dic.version;
    if dic.copyright<>'' then
      lblDescription.Caption := lblDescription.Caption + #13
        +dic.copyright;

    if dic.entries=-1 then
      lblWordEntries.Caption:='N/A'
    else
      lblWordEntries.Caption:=IntToStr(dic.entries);
    lblBuildDate.Caption:=DateToStr(dic.builddate);
    if dic.hasWordIndex then
      lblWordIndex.Caption:=_l('#00115^ePresent')
    else
      lblWordIndex.Caption:=_l('#00116^eAbsent');
    if dic.hasCharIndex then
      lblCharIndex.Caption:=_l('#00115^ePresent')
    else
      lblCharIndex.Caption:=_l('#00116^eAbsent');

    SpeedButton1.Down:=dicts.IsInGroup(dic, 1);
    SpeedButton2.Down:=dicts.IsInGroup(dic, 2);
    SpeedButton3.Down:=dicts.IsInGroup(dic, 3);
    SpeedButton4.Down:=dicts.IsInGroup(dic, 4);
    SpeedButton5.Down:=dicts.IsInGroup(dic, 5);
    CheckBox1.Checked:=not dicts.IsInGroup(dic, GROUP_OFFLINE);
  finally
    FLoadingData := false;
  end;
end;

procedure TfDictMan.Button1Click(Sender: TObject);
begin
  CarefulRefreshDicts;
end;

procedure TfDictMan.Button2Click(Sender: TObject);
var fDictImport: TfDictImport;
begin
  fDictImport := TfDictImport.Create(Self);
  try
    if IsPositiveResult(fDictImport.ShowModal) then
      CarefulRefreshDicts;
  finally
    FreeAndNil(fDictImport);
  end;
end;

procedure TfDictMan.SpeedButton1Click(Sender: TObject);
var s:string;
begin
  if FLoadingData then exit; //loading dict data, not a user action
  s:=cbDicts.Items[cbDicts.ItemIndex];
  dicts.PutInGroup(s,1,SpeedButton1.Down);
  dicts.PutInGroup(s,2,SpeedButton2.Down);
  dicts.PutInGroup(s,3,SpeedButton3.Down);
  dicts.PutInGroup(s,4,SpeedButton4.Down);
  dicts.PutInGroup(s,5,SpeedButton5.Down);
  dicts.PutInGroup(s,GROUP_OFFLINE,not CheckBox1.Checked);
end;

procedure TfDictMan.UpdateUpDownButtons;
begin
  btnMoveUp.Enabled := (cbDicts.ItemIndex>0);
  btnMoveDown.Enabled := (cbDicts.ItemIndex>=0) and (cbDicts.ItemIndex<cbDicts.Count-1);
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
