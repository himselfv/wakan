unit JWBWordLookup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons,
  JWBStrings, JWBDic, JWBDicSearch, Menus, WakanWordGrid,
  WakanPaintbox, JWBWordLookupBase, Vcl.ImgList, Vcl.ToolWin, JvExControls,
  JvArrowButton;

type
 //Supported lookup modes for this window.
 //Integer values are important, stored in registry.
  TLookupMode = (
    lmAuto = 0,
    lmJp = 1,
    lmEn = 2,
    lmClipboard = 3,
    lmEditorInsert = 4
  );

  TfWordLookup = class(TfWordLookupBase)
    pnlDockExamples: TPanel;
    Panel3: TPanel;
    btnMatchExact: TSpeedButton;
    btnMatchLeft: TSpeedButton;
    btnMatchRight: TSpeedButton;
    btnMatchAnywhere: TSpeedButton;
    btnInflect: TSpeedButton;
    sbAutoPreview: TSpeedButton;
    btnDictGroup1: TSpeedButton;
    btnDictGroup2: TSpeedButton;
    btnDictGroup3: TSpeedButton;
    Edit1: TEdit;
    btnSearch: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton6: TSpeedButton;
    SpeedButton9: TSpeedButton;
    pmLookupMode: TPopupMenu;
    miLookupAuto: TMenuItem;
    miLookupJtoE: TMenuItem;
    miLookupEtoJ: TMenuItem;
    SpeedButton1: TSpeedButton;
    btnLookupClip: TSpeedButton;
    btnLookupMode: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure btnMatchExactClick(Sender: TObject);
    procedure btnAddToVocabClick(Sender: TObject);
    procedure miLookupAutoClick(Sender: TObject);
    procedure btnLookupModeClick(Sender: TObject);
    procedure btnLookupClipClick(Sender: TObject);
    procedure miLookupJtoEClick(Sender: TObject);
    procedure miLookupEtoJClick(Sender: TObject);

  public
    procedure LanguageChanged;

  protected
    procedure WordSelectionChanged; override;
    function GetLookupMode: TLookupMode;
    procedure SetLookupMode(const Value: TLookupMode);
    procedure LookupModeChanged; virtual;
    procedure UpdateLookupModeButtonText;
  public
    dictBeginSet: integer;
    dictModeSet: TLookupMode;
    procedure SetDefaultColumnWidths; override;
    procedure Refresh; override;
    procedure RestoreLookupMode;
    property LookupMode: TLookupMode read GetLookupMode write SetLookupMode;


  protected
    donotsetbegset:boolean;
  public
   { Search with current settings, populate the results }
    procedure Look();

  end;

var
  fWordLookup: TfWordLookup;

implementation

uses TextTable, JWBUnit, JWBMenu, JWBVocab, JWBSettings,
  JWBPrint, JWBEditor, JWBWordKanji, JWBExamples,
  JWBHint, JWBKanjiDetails, JWBKanji, StdPrompt, JWBVocabAdd, Math,
  JWBCategories, JWBAnnotations, JWBUserData, JWBCharData, JWBLegacyMarkup;

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

procedure TfWordLookup.Refresh;
begin
  Look();
end;

procedure TfWordLookup.LanguageChanged;
begin
  if curLang='j' then begin
    fWordLookup.miLookupJtoE.Caption:=_l('#00329^eJapanese ->English');
    fWordLookup.miLookupJtoE.Hint := _l('#00643^Search by japanese reading');
    fWordLookup.miLookupEtoJ.Caption:=_l('#00330^eEnglish -> Japanese');
  end else begin
    fWordLookup.miLookupJtoE.Caption:=_l('#00331^eChinese ->English');
    fWordLookup.miLookupJtoE.Hint := _l('#01134^Search by chinese reading');
    fWordLookup.miLookupEtoJ.Caption:=_l('#00332^eEnglish -> Chinese');
  end;
  UpdateLookupModeButtonText;

  if (not btnLookupClip.Enabled) and btnLookupClip.Down then
    Self.SetLookupMode(lmJp);
end;


{ Restores lookup mode which was last selected by user when this form is open
 by itself (as a dictionary lookup form).
 It's overriden when it's used by Editor for word suggestions. }
procedure TfWordLookup.RestoreLookupMode;
begin
  SetLookupMode(dictModeSet);
end;

{ Gets or sets LookupMode as a variable. Lookup mode is internally stored as
 "which button is pressed/checked" }
function TfWordLookup.GetLookupMode: TLookupMode;
begin
  if btnLookupClip.Down then Result := lmClipboard else
  if miLookupAuto.Checked then Result := lmAuto else
  if miLookupJtoE.Checked then Result := lmJp else
  if miLookupEtoJ.Checked then Result := lmEn else
   //All buttons are off -- we're being called from Editor's auto suggestions.
    Result := lmEditorInsert;
end;

procedure TfWordLookup.SetLookupMode(const Value: TLookupMode);
begin
  if GetLookupMode=Value then exit; //sometimes we just set it to be sure
  btnLookupClip.Down := Value in [lmClipboard];
  case Value of
    lmAuto: Self.miLookupAuto.Checked:=true;
    lmJp: Self.miLookupJtoE.Checked:=true;
    lmEn: Self.miLookupEtoJ.Checked:=true;
    lmClipboard: Self.miLookupAuto.Checked := true;
  else
    miLookupAuto.Checked := false;
    miLookupJtoE.Checked := false;
    miLookupEtoJ.Checked := false;
  end;
  LookupModeChanged;
end;

{ Called when lookup mode changes due to any of the buttons being pressed/unpressed,
 checked/unchecked etc.
 Updates all dependent controls such as fMenu actions. }
procedure TfWordLookup.LookupModeChanged;
var ANewMode: TLookupMode;
begin
  ANewMode := GetLookupMode;

 //Do not store EditorInsert as a permanent state.
  if ANewMode<>lmEditorInsert then begin
    dictModeSet := ANewMode;
    fMenu.aDictLookupAuto.Checked := (ANewMode=lmAuto);
    fMenu.aDictJapanese.Checked := (ANewMode=lmJp);
    fMenu.aDictEnglish.Checked := (ANewMode=lmEn);
    fMenu.aDictClipboard.Checked := (ANewMode=lmClipboard);
  end;

  UpdateLookupModeButtonText;

  btnLookupMode.Enabled := (ANewMode<>lmEditorInsert);
  btnLookupClip.Enabled := (ANewMode<>lmEditorInsert);

  if not (ANewMode in [lmAuto, lmJp, lmEn]) then
  begin
    Edit1.Enabled:=false;
    Edit1.Color:=clMenu;
  end else begin
    Edit1.Enabled:=true;
    Edit1.Color:=clWindow;
  end;

  fMenu.aDictExact.Checked:=btnMatchExact.Down;
  fMenu.aDictBeginning.Checked:=btnMatchLeft.Down;
  fMenu.aDictEnd.Checked:=btnMatchRight.Down;
  fMenu.aDictMiddle.Checked:=btnMatchAnywhere.Down;

  fMenu.aDictBeginning.Enabled:=btnMatchLeft.Enabled;
  fMenu.aDictEnd.Enabled:=btnMatchRight.Enabled;
  fMenu.aDictMiddle.Enabled:=btnMatchAnywhere.Enabled;

  fMenu.aDictInflect.Checked:=btnInflect.Down;
  fMenu.aDictAuto.Checked:=sbAutoPreview.Down;
  fMenu.aDictGroup1.Checked:=btnDictGroup1.Down;
  fMenu.aDictGroup2.Checked:=btnDictGroup2.Down;
  fMenu.aDictGroup3.Checked:=btnDictGroup3.Down;

  donotsetbegset:=true;

  btnMatchLeft.Enabled:=true;
  btnMatchRight.Enabled:=true;
  btnMatchAnywhere.Enabled:=true;
  case dictbeginset of
    0:fWordLookup.btnMatchExact.Down:=true;
    1:fWordLookup.btnMatchLeft.Down:=true;
    2:fWordLookup.btnMatchRight.Down:=true;
    3:fWordLookup.btnMatchAnywhere.Down:=true;
  end;

  if (not sbAutoPreview.Down) or btnMatchAnywhere.Down then
    btnSearch.Caption:=_l('#00669^eSearch')
  else
    btnSearch.Caption:=_l('#00670^eAll');

  if ANewMode in [lmEn, lmEditorInsert] then
  begin
    if btnMatchRight.Down then btnMatchExact.Down:=true;
    if btnMatchAnywhere.Down then btnMatchExact.Down:=true;
    btnMatchRight.Enabled:=false;
    btnMatchAnywhere.Enabled:=false;
  end;

  if ANewMode=lmEditorInsert then
  begin
    if btnMatchLeft.Down then btnMatchExact.Down:=true;
    btnMatchLeft.Enabled:=false;
  end;
  donotsetbegset:=false;

  if Edit1.Enabled and Edit1.Visible and Edit1.HandleAllocated and Self.Visible then
    Edit1.SetFocus;
  Look(); //update results in new mode
 //^ with lmClipboard Edit1.Text can be empty, so don't check for that
end;

{ Updates text on lookup mode button according to currently selected mode.
 We wouldn't need this as a separate function since it's mostly part of
 LookupModeChanged, but unfortunately text changes depending on Japanese/Chinese
 being selected, so we need to do this separately sometimes. }
procedure TfWordLookup.UpdateLookupModeButtonText;
begin
  if miLookupAuto.Checked then begin
    btnLookupMode.Caption := miLookupAuto.Caption;
    btnLookupMode.Hint := miLookupAuto.Hint;
  end else
  if miLookupJtoE.Checked then begin
    btnLookupMode.Caption := miLookupJtoE.Caption;
    btnLookupMode.Hint := miLookupJtoE.Hint;
  end else
  if miLookupEtoJ.Checked then begin
    btnLookupMode.Caption := miLookupEtoJ.Caption;
    btnLookupMode.Hint := miLookupEtoJ.Hint;
  end;
end;


{
Look()
Searches for currently entered word and populates the grid with results.
Do not use for stEditorAuto-Translation, Editor uses TDicSearchRequest directly.
}
procedure TfWordLookup.Look();
var lm: TLookupMode;
  req: TDicSearchRequest;
  wt:TEvalCharType;
  i:integer;
  wasfull:boolean;
  text:string;
  b:boolean;
begin
  //Retrieve current lookup mode
  lm := GetLookupMode();

  //Don't exit if the query is empty, we need to reset results/update presentation/etc

  //We don't auto-search when in MatchAnywhere or when Autosearch is disabled
  if (lm<>lmEditorInsert) and btnSearch.Enabled and ((not sbAutoPreview.Down) or (btnMatchAnywhere.Down)) then
  begin
    btnSearch.Visible:=true;
    Label2.Visible:=false;
    StringGrid.Visible:=false;
    BlankPanel.TextVisible:=(edit1.text<>'') or (lm=lmEditorInsert);
    curword:=0;
    WordSelectionChanged;
    exit;
  end;

  req := TDicSearchRequest.Create;
  try
   //Dictionary group
    if btnDictGroup1.Down then req.dictgroup:=1 else
    if btnDictGroup2.Down then req.dictgroup:=2 else
    if btnDictGroup3.Down then req.dictgroup:=3 else
      req.dictgroup := 1; //we must have some group chosen

    req.Full:=not btnSearch.Enabled;

    StringGrid.RowCount:=200;
    req.MaxWords:=StringGrid.VisibleRowCount;

   //Match type (left/right/exact)
    if btnMatchLeft.Down then req.MatchType := mtMatchLeft else
    if btnMatchRight.Down then req.MatchType := mtMatchRight else
    if btnMatchAnywhere.Down then req.MatchType := mtMatchAnywhere else
      req.MatchType := mtExactMatch;
    if (lm=lmEn) and not (req.MatchType in [mtExactMatch, mtMatchLeft]) then
      req.MatchType := mtExactMatch;

    if (not fSettings.cbDictLimitAutoResults.Checked) or btnMatchExact.Down then req.Full:=true;

    if lm in [lmEditorInsert] then begin //ignore some UI settings in these modes
      req.dictgroup := 5;
      req.MatchType := mtExactMatch;
    end;

    req.AutoDeflex := btnInflect.Down;
    req.dic_ignorekana := false;
    req.MindUserPrior := (lm=lmEditorInsert); //only mind kanji usage priorities in Editor suggestions

   //If full search was not requested and autopreview off / too costly
    if not req.full
    and (not Self.sbAutoPreview.Down or (req.MatchType=mtMatchAnywhere)) then
      exit; //do not search

    FResults.Clear;
    wasfull := false;
    case lm of
      lmAuto: begin
        req.st := stRomaji;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;

        req.st := stEnglish;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := wasfull or req.WasFull;

        req.st := stJapanese;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := wasfull or req.WasFull;
      end;
      lmJp: begin
        req.st := stRomaji;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;
      end;
      lmEn: begin
        req.st := stEnglish;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;
      end;
      lmClipboard: begin
        text:='';
        for i:=1 to flength(clip) do
         {$IFDEF UNICODE}
          if copy(fgetch(clip,i),1,2)='00' then break
         {$ELSE}
          if fgetch(clip,i)<=#$00FF then break
         {$ENDIF}
          else text:=text+fgetch(clip,i);
        req.st := stJapanese;
        req.Prepare;
        req.Search(text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;
      end;
      lmEditorInsert: begin
       //First try real word insert buffer
        text := fEditor.GetInsertKana(false);
        wt := EC_UNKNOWN;
       //If that is empty, show whatever the caret is at
        if text='' then
          text:=fEditor.GetWordAtCaret(wt);
        req.st := stJapanese;
        req.Prepare;
        req.Search(text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;
      end;
    end;

   //Finalize and output
    if not (lm in [lmEditorInsert]) then
      if FResults.Count=0 then
        Label3.Caption:='-'
      else
        if not wasfull then
          Label3.Caption:=inttostr(FResults.Count)+'+'
        else
          Label3.Caption:=inttostr(FResults.Count);

  //update result list
    ResultsChanged;
    if not wasfull then
      text:=_l('#00671^eSearch results (partial)')
    else
      text:=_l('#00672^eSearch results');
    btnSearch.Visible:=not wasfull or (req.full and not btnSearch.Enabled);
    Label2.Visible:=not btnSearch.Visible;
    case lm of
      lmJp: text:=text+' '+_l('#00673^eby phonetic');
      lmEn: text:=text+' '+_l('#00674^eby meaning');
      lmClipboard: text:=text+' '+_l('#00675^eby written (clipboard)');
      lmEditorInsert: text:=text+' '+_l('#00676^eby written (text)');
    end;
    text:=text+' ('+inttostr(FResults.Count)+')';
    curword:=0;
    if StringGrid.Visible then StringGridSelectCell(self,0,1,b);
    if StringGrid.Visible then StringGrid.Row:=1;
    if StringGrid.Visible then curword:=1;
    WordSelectionChanged;

  finally
    FreeAndNil(req);
  end;
end;

procedure TfWordLookup.Edit1Change(Sender: TObject);
begin
  btnSearch.Enabled:=true;
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

procedure TfWordLookup.btnLookupModeClick(Sender: TObject);
begin
  if miLookupJtoE.Checked then
    LookupMode := lmJp
  else
  if miLookupEtoJ.Checked then
    LookupMode := lmEn
  else
    LookupMode := lmAuto;
end;

procedure TfWordLookup.miLookupAutoClick(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.miLookupJtoEClick(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.miLookupEtoJClick(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.btnLookupClipClick(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled:=false;
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

procedure TfWordLookup.btnMatchExactClick(Sender: TObject);
begin
  if not donotsetbegset then
  begin
    if fWordLookup.btnMatchExact.Down then dictbeginset:=0;
    if fWordLookup.btnMatchLeft.Down then dictbeginset:=1;
    if fWordLookup.btnMatchRight.Down then dictbeginset:=2;
    if fWordLookup.btnMatchAnywhere.Down then dictbeginset:=3;
  end;
  Look();
  if Edit1.Enabled then Edit1.SetFocus;
end;

end.
