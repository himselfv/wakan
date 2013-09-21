unit JWBWordLookup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons,
  JWBStrings, JWBDic, JWBDicSearch, Menus, WakanWordGrid,
  WakanPaintbox;

type
  TKanjiEntry = record
    tp: char; //J, K, C, N, U -- see below in ShowWord()
    char: FChar; //character itself
    rad: FChar; //its radical
  end;

  TfWordLookup = class(TForm)
    Panel1: TPanel;
    btnLookupJtoE: TSpeedButton;
    btnLookupEtoJ: TSpeedButton;
    btnLookupClip: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    btnCopyToClipboard: TSpeedButton;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    pnlDockExamples: TPanel;
    Panel3: TPanel;
    SpeedButton4: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    Label2: TLabel;
    SpeedButton17: TSpeedButton;
    Label3: TLabel;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    PopupMenu1: TPopupMenu;
    miResetColumns: TMenuItem;
    BlankPanel: TBlankPanel;
    StringGrid1: TWakanWordGrid;
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure WordDetails_PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure WordDetails_PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
    procedure FormShow(Sender: TObject);
    procedure btnLookupJtoEClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton17Click(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miResetColumnsClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);

  public
    procedure SetDefaultColumnWidths;

  public
    curkanji,curphonetic,curmeaning:string;
    curkanjid: array of TKanjiEntry; //kanji entries for current word
    procedure ShowWord;
    procedure DetailsForKanji(n:integer);

  public
    procedure UpdateLookMode;

  protected
    donotsetbegset:boolean;
    procedure Look_Run(req: TDicSearchRequest);
  public
   { Some may reference "ul". That was a redundant string list before.
    As a rule,
      ul[i]==copy(dicl[i],6,25).
      dicsl[i]=copy(dicl[i],31,length(dicsl[i])-30). }
    dicrl:TSearchResults;
   { Currently selected word. Needed to synchronize other clients such as JWBHint }
    curword:integer;
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
  JWBPrint, JWBTranslate, JWBWordDetails, JWBWordKanji, JWBExamples,
  JWBWordCategory, JWBHint, JWBKanjiDetails, JWBKanji, StdPrompt, JWBVocabAdd, Math,
  JWBCategories, JWBAnnotations, JWBUserData, JWBCharData;

{$R *.DFM}

procedure TfWordLookup.FormCreate(Sender: TObject);
begin
  dicrl:=TSearchResults.Create;
end;

procedure TfWordLookup.FormDestroy(Sender: TObject);
begin
  FreeAndNil(dicrl);
end;

procedure TfWordLookup.FormActivate(Sender: TObject);
begin
//  if SpeedButton4.Down then SpeedButton1.Down:=true;
//  SpeedButton1Click(sender);
end;

procedure TfWordLookup.FormShow(Sender: TObject);
begin
  if Edit1.Enabled then Edit1.SetFocus;
  Look();
end;

procedure TfWordLookup.SetDefaultColumnWidths;
begin
  StringGrid1.ColWidths[0]:=131;
  StringGrid1.ColWidths[1]:=128;
  StringGrid1.ColWidths[2]:=575;
  StringGrid1.AutoSizeColumns;
end;

procedure TfWordLookup.PopupMenu1Popup(Sender: TObject);
var p: TPoint;
  ACol, ARow: integer;
begin
  p := StringGrid1.ScreenToClient(Mouse.CursorPos);
  StringGrid1.MouseToCell(p.X, p.Y, ACol, ARow);
  miResetColumns.Visible := (ARow=0); //click on header
end;

procedure TfWordLookup.miResetColumnsClick(Sender: TObject);
begin
  SetDefaultColumnWidths;
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
  fMenu.aDictAuto.Checked:=SpeedButton13.Down;
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

  if (not SpeedButton13.Down) or (SpeedButton18.Down) then
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

  StringGrid1.RowCount:=200;
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

  req.maxwords:=StringGrid1.VisibleRowCount;

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
var wt:integer;
  i:integer;
  wasfull:boolean;
  s:string;
  b:boolean;
  tmp: TStringList;
begin
  dicrl.Clear;

  case req.a of
    stJp: begin s := Edit1.Text; wt := -1; end;
    stEn: begin s := Edit1.Text; wt := -1; end;
    stClipboard: begin
      s:='';
      for i:=1 to flength(clip) do
       {$IFDEF UNICODE}
        if copy(fgetch(clip,i),1,2)='00' then break
       {$ELSE}
        if fgetch(clip,i)<=#$00FF then break
       {$ENDIF}
        else s:=s+fgetch(clip,i);
      wt := -1;
    end;
    stEditorInsert: begin //In "word insert" mode
      s := fTranslate.GetInsertKana(false);
      if fTranslate.buffertype='H'then
        wt := -1
      else
        wt := -2;
    end;
    stEditorAuto: //In "translate text" mode
      s:=fTranslate.GetDocWord(fTranslate.rcur.x,fTranslate.rcur.y,wt,{stopuser=}true);
  end;

  req.Search(s, wt, dicrl);
  wasfull := req.WasFull;

  if not (req.a in [stEditorInsert, stEditorAuto]) then
    if dicrl.Count=0 then
      Label3.Caption:='-'
    else
      if not wasfull then
        Label3.Caption:=inttostr(dicrl.Count)+'+'
      else
        Label3.Caption:=inttostr(dicrl.Count);
  
  if req.a <> stEditorAuto then //update result list
  begin
    tmp := TStringList.Create;
    try
      for i:=0 to dicrl.Count - 1 do
        if req.full or (i<StringGrid1.VisibleRowCount) then
          tmp.Add(dicrl[i].ArticlesToString);
      FillWordGrid(StringGrid1,tmp,false,false);
    finally
      FreeAndNil(tmp);
    end;

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
    s:=s+' ('+inttostr(dicrl.Count)+')';
    curword:=0;
    if StringGrid1.Visible then StringGrid1SelectCell(self,0,1,b);
    if StringGrid1.Visible then StringGrid1.Row:=1;
    if StringGrid1.Visible then curword:=1;
    ShowWord;
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
  if (a<>stEditorInsert) and (BitBtn1.Enabled) and ((not SpeedButton13.Down) or (SpeedButton18.Down)) then
  begin
    BitBtn1.Visible:=true;
    Label2.Visible:=false;
    StringGrid1.Visible:=false;
    BlankPanel.TextVisible:=(edit1.text<>'') or (a=stEditorInsert);
    curword:=0;
    ShowWord;
    exit;
  end;

  SetupSearchRequest(a, req);
  try
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

procedure TfWordLookup.Edit2Change(Sender: TObject);
begin
  Look();
end;

procedure TfWordLookup.Edit2Click(Sender: TObject);
begin
  Look();
end;

procedure TfWordLookup.Edit1Click(Sender: TObject);
begin
  Look();
end;

procedure TfWordLookup.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
 { Careful not to enter an endless loop: ShowWord changes something in Grid
  which triggers SelectCell which triggers ShowWord }
 { SelectCell also gets called endlessly while you hold mouse down
  due to a bug in TStringGrid if RowSelect is true }
  if curword=ARow then exit;
  curword:=ARow;
  if curword<=dicrl.Count then ShowWord;
end;

procedure TfWordLookup.WordDetails_PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  if showroma then
    DrawUnicode(Canvas,2,2,22,KanaToRomaji(curphonetic,romasys,curlang),FontEnglish)
  else
    DrawUnicode(Canvas,2,2,22,curphonetic,FontJapanese);
end;

procedure TfWordLookup.WordDetails_PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  DrawUnicode(Canvas,2,2,22,curkanji,FontJapanese);
end;

procedure TfWordLookup.ShowWord;
var s,s2:string;
  meaning: FString;
    radf:integer;
    sl:TStringList;
    i:integer;
  rad:FString;
  CCharProp: TCharPropertyCursor;
begin
  SetLength(curkanjid,0);
  curphonetic:='';
  curkanji:='';
  curmeaning:='';
  SpeedButton17.Enabled:=false;
  btnCopyToClipboard.Enabled:=false;
  SpeedButton19.Enabled:=false;
  fWordCategory.RxLabel9.Caption:='-';
  fWordCategory.Label55.Caption:='-';
  fWordCategory.Label11.Caption:='-';
  fWordCategory.Label12.Caption:='-';
  fWordCategory.Label13.Caption:='-';
  fWordCategory.Label14.Caption:='-';
  fWordKanji.Clear;
  if curword<>0 then
  begin
    curphonetic:=remexcl(copy(StringGrid1.Cells[0,curword],2,length(StringGrid1.Cells[0,curword])-1));
    curkanji:=remexcl(copy(StringGrid1.Cells[1,curword],2,length(StringGrid1.Cells[1,curword])-1));
    curmeaning:=remexcl(StringGrid1.Cells[2,curword]);
    fExamples.SetExamples(curkanji);
    SpeedButton17.Enabled:=true;
    btnCopyToClipboard.Enabled:=true;
    fWordCategory.RxLabel9.Caption:=_l('#00677^eNot in vocabulary');
    s:=remexcl(curkanji);
    SetLength(curkanjid,0);
    while flength(s)>0 do
    begin
      s2:=fcopy(s,1,1);
      fdelete(s,1,1);
      if TChar.Locate('Unicode',s2) then
      begin
        radf:=fSettings.GetPreferredRadicalType();
        if TRadicals.Locate('Number',GetCharValueRad(TChar.Int(TCharIndex),radf)) then
        begin
          rad := TRadicals.Str(TRadicalsUnicode);
          SetLength(curkanjid, Length(curkanjid)+1);
          if flength(s2)>0 then
            curkanjid[Length(curkanjid)-1].char := fgetch(s2, 1)
          else
            curkanjid[Length(curkanjid)-1].char := UH_NOCHAR;
          if flength(rad)>0 then
            curkanjid[Length(curkanjid)-1].rad := fgetch(rad, 1)
          else
            curkanjid[Length(curkanjid)-1].rad := UH_NOCHAR;
          if TChar.Bool(TCharChinese) then
            curkanjid[Length(curkanjid)-1].tp := 'J'
          else
          if IsKnown(KnownLearned,TChar.Fch(TCharUnicode)) then
            curkanjid[Length(curkanjid)-1].tp := 'K'
          else
          if TChar.Int(TCharJouyouGrade)<9 then
            curkanjid[Length(curkanjid)-1].tp := 'C'
          else
          if TChar.Int(TCharJouyouGrade)<10 then
            curkanjid[Length(curkanjid)-1].tp := 'N'
          else
            curkanjid[Length(curkanjid)-1].tp := 'U';
          CCharProp := TCharPropertyCursor.Create(TCharProp);
          try
            if curlang='j' then
              meaning := CCharProp.GetJapaneseDefinitions(TChar.TrueInt(TCharIndex))
            else
            if curlang='c' then
              meaning := CCharProp.GetChineseDefinitions(TChar.TrueInt(TCharIndex))
            else
              meaning := '';
          finally
            FreeAndNil(CCharProp);
          end;
          fWordKanji.AddBox(meaning);
        end;
      end;
    end;
    if dicrl[curword-1].userIndex<>0 then
    begin
      SpeedButton19.Enabled:=true;
      TUser.Locate('Index',dicrl[curword-1].userIndex);
      fWordCategory.Label11.Caption:=DateForm(TUser.Str(TUserAdded));
      fWordCategory.Label12.Caption:=DateForm(TUser.Str(TUserLearned));
      fWordCategory.Label13.Caption:=DateForm(TUser.Str(TUserMastered));
      fWordCategory.Label14.Caption:=DateForm(TUser.Str(TUserPrinted));
      if fWordCategory.Label13.Caption<>'-'then fWordCategory.Label13.Caption:=fWordCategory.Label13.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
      fWordCategory.RxLabel9.Caption:=StateStr(TUser.Int(TUserScore));
      sl:=TStringList.Create;
      sl.Clear;
      ListWordCategories(dicrl[curword-1].UserIndex,sl);
      s:='';
      for i:=0 to sl.Count-1 do if s='' then s:=sl[i] else s:=s+', '+sl[i];
      fWordCategory.Label55.Caption:=s;
    end;
  end else fExamples.SetExamples('');
  fWordDetails.PaintBox1.Invalidate;
  fWordDetails.PaintBox2.Invalidate;
  fWordDetails.PaintBox5.Invalidate;
  fWordKanji.InvalidateBoxes;
  AnnotShowMedia(curkanji,curphonetic);
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

procedure TfWordLookup.SpeedButton5Click(Sender: TObject);
begin
  fMenu.aDictDetails.Execute;
end;

procedure TfWordLookup.SpeedButton6Click(Sender: TObject);
begin
  fMenu.aDictKanji.Execute;
end;

procedure TfWordLookup.SpeedButton7Click(Sender: TObject);
begin
  fMenu.aDictCategories.Execute;
end;

procedure TfWordLookup.SpeedButton9Click(Sender: TObject);
begin
  fMenu.aDictExamples.Execute;
end;

procedure TfWordLookup.btnCopyToClipboardClick(Sender: TObject);
begin
  clip:=clip+curkanji;
  fMenu.SetClipboard;
end;

procedure TfWordLookup.StringGrid1DblClick(Sender: TObject);
begin
  if SpeedButton17.Enabled then SpeedButton17Click(sender);
end;

function GridStateToStr(const state: TGridDrawState): string;
begin
  Result := '';
  if gdSelected in state then
    Result := Result + ', gdSelected';
  if gdFocused in state then
    Result := Result + ', gdFocused';
  if gdFixed in state then
    Result := Result + ', gdFixed';
  if gdRowSelected in state then
    Result := Result + ', gdRowSelected';
  if gdHotTrack in state then
    Result := Result + ', gdHotTrack';
  if gdPressed in state then
    Result := Result + ', gdPressed';
  if Length(Result)>0 then //cut forward ", "
    Result := copy(Result, 3);
end;

procedure TfWordLookup.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(TStringGrid(Sender),ACol,ARow,Rect,State);
end;

procedure TfWordLookup.CheckBox1Click(Sender: TObject);
begin
//  Look(false);
end;

procedure TfWordLookup.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipMouseMove(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfWordLookup.DetailsForKanji(n:integer);
begin
  if fMenu.CharDetDocked then exit;
  if n<=Length(curkanjid) then
    fKanjiDetails.SetCharDetails(curkanjid[n-1].char);
  if not fKanjiDetails.Visible then fMenu.aKanjiDetails.Execute else fKanjiDetails.SetFocus;
end;

procedure TfWordLookup.SpeedButton17Click(Sender: TObject);
var tmp: string;
begin
  tmp := curmeaning;
  if pos(' >> ',tmp)>0 then delete(tmp,1,pos(' >> ',tmp)+3);
  tmp:=UnfixVocabEntry(tmp); //replace markup symbols with user readable
  if IsPositiveResult(fVocabAdd.ModalAddFixed(curkanji,curphonetic,fstr(tmp))) then begin
    Look();
    if Edit1.Enabled then Edit1.SetFocus;
  end;
end;

procedure TfWordLookup.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfWordLookup.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.IntTipMouseUp;
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

procedure TfWordLookup.SpeedButton19Click(Sender: TObject);
begin
  fMenu.aModeWordsExecute(sender);
  if dicrl[curword-1].userIndex<>0 then
    fVocab.SearchWord(dicrl[curword-1].userIndex);
end;


end.
