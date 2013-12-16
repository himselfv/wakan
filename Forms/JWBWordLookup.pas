unit JWBWordLookup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons,
  JWBStrings, JWBDic, JWBDicSearch, Menus, WakanWordGrid,
  WakanPaintbox, JWBWordLookupBase;

type
  TfWordLookup = class(TfWordLookupBase)
    pnlDockExamples: TPanel;
    Panel3: TPanel;
    btnLookupJtoE: TSpeedButton;
    btnLookupEtoJ: TSpeedButton;
    btnLookupClip: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton4: TSpeedButton;
    sbAutoPreview: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton6: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnLookupJtoEClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure btnAddToVocabClick(Sender: TObject);

  protected
    procedure WordSelectionChanged; override;
  public
    procedure SetDefaultColumnWidths; override;
    procedure UpdateLookMode;

  protected
    donotsetbegset:boolean;
    procedure Look_Run(req: TDicSearchRequest);
  public
   { Search with current settings, populate the results }
    procedure Look();
   { Initialize a TDicSearchRequest to search like Look() would.
    You can then do a lot of searches without affecting the UI. }
    procedure SetupSearchRequest(a: TSearchType; out req: TDicSearchRequest);

  end;

var
  fWordLookup: TfWordLookup;

implementation

uses TextTable, JWBUnit, JWBMenu, JWBVocab, JWBSettings,
  JWBPrint, JWBEditor, JWBWordKanji, JWBExamples,
  JWBHint, JWBKanjiDetails, JWBKanji, StdPrompt, JWBVocabAdd, Math,
  JWBCategories, JWBAnnotations, JWBUserData, JWBCharData;

{$R *.DFM}

procedure TfWordLookup.FormShow(Sender: TObject);
begin
  if Edit1.Enabled then Edit1.SetFocus;
  Look();
end;

procedure TfWordLookup.SetDefaultColumnWidths;
begin
  StringGrid.ColWidths[0]:=131;
  StringGrid.ColWidths[1]:=128;
  StringGrid.ColWidths[2]:=575;
  StringGrid.AutoSizeColumns;
end;

//Called when any of the configuration buttons are pressed
procedure TfWordLookup.UpdateLookMode;
var a: integer;
begin
  if btnLookupJtoE.Down then a:=1 else
    if btnLookupEtoJ.Down then a:=2 else
    if btnLookupClip.Down then a:=3 else a:=4;

  if btnLookupJtoE.Down then dictmodeset:=0;
  if btnLookupEtoJ.Down then dictmodeset:=1;
  if btnLookupClip.Down then dictmodeset:=2;
  if not((btnLookupJtoE.Down) or (btnLookupEtoJ.Down)) then
  begin
    Edit1.enabled:=false;
    Edit1.Color:=clMenu;
  end else begin
    Edit1.enabled:=true;
    Edit1.Color:=clWindow;
  end;
  fMenu.aDictExact.Checked:=SpeedButton10.Down;
  fMenu.aDictBeginning.Checked:=SpeedButton11.Down;
  fMenu.aDictEnd.Checked:=SpeedButton12.Down;
  fMenu.aDictMiddle.Checked:=SpeedButton18.Down;
  fMenu.aDictBeginning.Enabled:=SpeedButton11.Enabled;
  fMenu.aDictEnd.Enabled:=SpeedButton12.Enabled;
  fMenu.aDictMiddle.Enabled:=SpeedButton18.Enabled;
  fMenu.aDictInflect.Checked:=SpeedButton4.Down;
  fMenu.aDictAuto.Checked:=sbAutoPreview.Down;
  fMenu.aDictGroup1.Checked:=SpeedButton14.Down;
  fMenu.aDictGroup2.Checked:=SpeedButton15.Down;
  fMenu.aDictGroup3.Checked:=SpeedButton16.Down;

  donotsetbegset:=true;

  SpeedButton11.Enabled:=true;
  SpeedButton12.Enabled:=true;
  SpeedButton18.Enabled:=true;
  case dictbeginset of
    0:fWordLookup.SpeedButton10.Down:=true;
    1:fWordLookup.SpeedButton11.Down:=true;
    2:fWordLookup.SpeedButton12.Down:=true;
    3:fWordLookup.SpeedButton18.Down:=true;
  end;

  if (not sbAutoPreview.Down) or (SpeedButton18.Down) then
    BitBtn1.Caption:=_l('#00669^eSearch')
  else
    BitBtn1.Caption:=_l('#00670^eAll');

  if ((a=2) or (a=4)) then
  begin
    if SpeedButton12.Down then SpeedButton10.Down:=true;
    if SpeedButton18.Down then SpeedButton10.Down:=true;
    SpeedButton12.Enabled:=false;
    SpeedButton18.Enabled:=false;
  end;
  if a=4 then
  begin
    if SpeedButton11.Down then SpeedButton10.Down:=true;
    SpeedButton11.Enabled:=false;
  end;
  donotsetbegset:=false;

  StringGrid.RowCount:=200;
end;

{
SetupSearchParams()
Creates a TDicSearchRequest, configured according to user settings and the type of search request:
  1: jp->en
  2: en->jp
  3: clipboard translation
  4: word insert/translate text
This is called automatically when doing Look(), or manually on auto-translation.
}

procedure TfWordLookup.SetupSearchRequest(a: TSearchType; out req: TDicSearchRequest);
begin
  req := TDicSearchRequest.Create;
  req.a := a;

 //Dictionary group
  if SpeedButton14.Down then req.dictgroup:=1 else
  if SpeedButton15.Down then req.dictgroup:=2 else
  if SpeedButton16.Down then req.dictgroup:=3 else
    req.dictgroup := 1; //we must have some group chosen

  req.full:=not BitBtn1.Enabled;
//  if SpeedButton10.Down then req.full:=true;

  req.maxwords:=StringGrid.VisibleRowCount;

  if SpeedButton11.Down then req.MatchType := mtMatchLeft else
  if SpeedButton12.Down then req.MatchType := mtMatchRight else
  if SpeedButton18.Down then req.MatchType := mtMatchAnywhere else
    req.MatchType := mtExactMatch;
  if (a=stEn) and not (req.MatchType in [mtExactMatch, mtMatchLeft]) then
    req.MatchType := mtExactMatch;

  if (not fSettings.CheckBox12.Checked) or (SpeedButton10.Down) then req.full:=true;

  if a in [stEditorInsert,stEditorAuto] then begin //ignore some UI settings in these modes
    req.dictgroup := 5;
    req.MatchType := mtExactMatch;
   { If we used mtMatchLeft, queries like "sama" would get results like "samazama"
    which is obviously not what we want. }
  end;

  req.AutoDeflex := SpeedButton4.Down;
  req.dic_ignorekana := false; //by default, but this can be overriden
end;

{
Look_Run()
Called with a TDicSearchRequest initialized by SetupSearchRequest() to do a search
and populate the grid with the results
Don't call directly.
}
procedure TfWordLookup.Look_Run(req: TDicSearchRequest);
var wt:TEvalCharType;
  i:integer;
  wasfull:boolean;
  s:string;
  b:boolean;
begin
  FResults.Clear;

  case req.a of
    stJp: begin s := Edit1.Text; wt := EC_UNKNOWN; end;
    stEn: begin s := Edit1.Text; wt := EC_UNKNOWN; end;
    stClipboard: begin
      s:='';
      for i:=1 to flength(clip) do
       {$IFDEF UNICODE}
        if copy(fgetch(clip,i),1,2)='00' then break
       {$ELSE}
        if fgetch(clip,i)<=#$00FF then break
       {$ENDIF}
        else s:=s+fgetch(clip,i);
      wt := EC_UNKNOWN;
    end;
    stEditorInsert: begin //In "word insert" mode
     //First try real word insert buffer
      s := fEditor.GetInsertKana(false);
      wt := EC_UNKNOWN;
     //If that is empty, show whatever the caret is at
      if s='' then
        s:=fEditor.GetWordAtCaret(wt);
    end;
    stEditorAuto: //In "translate text" mode
      s:=fEditor.GetDocWord(fEditor.rcur.x,fEditor.rcur.y,wt,{stopuser=}true);
  end;

  req.Search(s, wt, FResults);
  wasfull := req.WasFull;

  if not (req.a in [stEditorInsert, stEditorAuto]) then
    if FResults.Count=0 then
      Label3.Caption:='-'
    else
      if not wasfull then
        Label3.Caption:=inttostr(FResults.Count)+'+'
      else
        Label3.Caption:=inttostr(FResults.Count);
  
  if req.a <> stEditorAuto then //update result list
  begin
    ResultsChanged;
    if not wasfull then
      s:=_l('#00671^eSearch results (partial)')
    else
      s:=_l('#00672^eSearch results');
    BitBtn1.Visible:=not wasfull or (req.full and not BitBtn1.Enabled);
    Label2.Visible:=not BitBtn1.Visible;
    s:=s+' ';
    case req.a of
      stJp: s:=s+_l('#00673^eby phonetic');
      stEn: s:=s+_l('#00674^eby meaning');
      stClipboard: s:=s+_l('#00675^eby written (clipboard)');
      stEditorInsert: s:=s+_l('#00676^eby written (text)');
    end;
    s:=s+' ('+inttostr(FResults.Count)+')';
    curword:=0;
    if StringGrid.Visible then StringGridSelectCell(self,0,1,b);
    if StringGrid.Visible then StringGrid.Row:=1;
    if StringGrid.Visible then curword:=1;
    WordSelectionChanged;
  end;
end;

{
Look()
Searches for currently entered word and populates the grid with results.
Do not use for stEditorAuto-Translation, there's SetupSearchRequest()+Look_Run() for that.
}
procedure TfWordLookup.Look();
var a: TSearchType;
  req: TDicSearchRequest;
begin
  if btnLookupJtoE.Down then
    a:=stJp
  else
  if btnLookupEtoJ.Down then
    a:=stEn
  else
  if btnLookupClip.Down then
    a:=stClipboard
  else
    a:=stEditorInsert;

  UpdateLookMode;

  //We don't auto-search when in MatchAnywhere or when Autosearch is disabled
  if (a<>stEditorInsert) and (BitBtn1.Enabled) and ((not sbAutoPreview.Down) or (SpeedButton18.Down)) then
  begin
    BitBtn1.Visible:=true;
    Label2.Visible:=false;
    StringGrid.Visible:=false;
    BlankPanel.TextVisible:=(edit1.text<>'') or (a=stEditorInsert);
    curword:=0;
    WordSelectionChanged;
    exit;
  end;

  SetupSearchRequest(a, req);
  try
   //If full search was not requested and autopreview off / too costly
    if not req.full
    and (not Self.sbAutoPreview.Down or (req.MatchType=mtMatchAnywhere)) then
      exit; //do not search
    req.Prepare;
    Look_Run(req);
  finally
    FreeAndNil(req);
  end;
end;

procedure TfWordLookup.Edit1Change(Sender: TObject);
begin
  BitBtn1.Enabled:=true;
  Look();
end;

procedure TfWordLookup.Edit1Click(Sender: TObject);
begin
  Look();
end;

procedure TfWordLookup.WordSelectionChanged;
begin
  inherited;
  if fWordKanji<>nil then fWordKanji.Clear;
  if curword<>0 then begin
    if fExamples<>nil then fExamples.SetExamples(curkanji);
    if fWordKanji<>nil then fWordKanji.ShowKanjiFromString(remexcl(curkanji));
  end else begin
    if fExamples<>nil then fExamples.SetExamples('');
  end;
  if fWordKanji<>nil then fWordKanji.InvalidateBoxes;
  AnnotShowMedia(curkanji,curphonetic);
end;

procedure TfWordLookup.btnAddToVocabClick(Sender: TObject);
begin
  inherited;
  Look();
  if Edit1.Enabled then Edit1.SetFocus;
end;

procedure TfWordLookup.btnLookupJtoEClick(Sender: TObject);
begin
  UpdateLookMode;
  Look();
  if Edit1.Enabled then Edit1.SetFocus;
end;

procedure TfWordLookup.BitBtn1Click(Sender: TObject);
begin
  BitBtn1.Enabled:=false;
  Look();
end;

procedure TfWordLookup.SpeedButton6Click(Sender: TObject);
begin
  fMenu.aDictKanji.Execute;
end;

procedure TfWordLookup.SpeedButton9Click(Sender: TObject);
begin
  fMenu.aDictExamples.Execute;
end;

procedure TfWordLookup.SpeedButton10Click(Sender: TObject);
begin
  if not donotsetbegset then
  begin
    if fWordLookup.SpeedButton10.Down then dictbeginset:=0;
    if fWordLookup.SpeedButton11.Down then dictbeginset:=1;
    if fWordLookup.SpeedButton12.Down then dictbeginset:=2;
    if fWordLookup.SpeedButton18.Down then dictbeginset:=3;
  end;
  Look();
  if Edit1.Enabled then Edit1.SetFocus;
end;

end.
