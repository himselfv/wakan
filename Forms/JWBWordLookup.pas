unit JWBWordLookup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons, Menus, ImgList, ToolWin, JWBDic,
  JWBDicSearch, JWBStrings, WakanWordGrid, WakanPaintbox, JWBWordLookupBase,
  System.Actions, Vcl.ActnList;

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
    pmLookupMode: TPopupMenu;
    miLookupAuto: TMenuItem;
    miLookupJtoE: TMenuItem;
    miLookupEtoJ: TMenuItem;
    TopPanel: TPanel;
    btnLookupClip: TSpeedButton;
    btnMatchExact: TSpeedButton;
    btnMatchLeft: TSpeedButton;
    btnMatchRight: TSpeedButton;
    btnMatchAnywhere: TSpeedButton;
    btnInflect: TSpeedButton;
    sbAutoPreview: TSpeedButton;
    btnDictGroup1: TSpeedButton;
    btnDictGroup2: TSpeedButton;
    btnDictGroup3: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Edit1: TEdit;
    btnLookupMode: TButton;
    btnWordKanji: TSpeedButton;
    btnExamples: TSpeedButton;
    CharInWordDock: TPanel;
    btnSearch: TBitBtn;
    Actions: TActionList;
    aLookupAuto: TAction;
    aKanji: TAction;
    aExamples: TAction;
    aLookupJtoE: TAction;
    aLookupEtoJ: TAction;
    aLookupClip: TAction;
    aAddToClipboard: TAction;
    aMatchExact: TAction;
    aMatchLeft: TAction;
    aMatchRight: TAction;
    aInflect: TAction;
    aAutoPreview: TAction;
    aDictGroup1: TAction;
    aDictGroup2: TAction;
    aDictGroup3: TAction;
    aMatchAnywhere: TAction;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnWordKanjiClick(Sender: TObject);
    procedure btnExamplesClick(Sender: TObject);
    procedure btnAddToVocabClick(Sender: TObject);
    procedure miLookupAutoClick(Sender: TObject);
    procedure btnLookupModeClick(Sender: TObject);
    procedure btnLookupClipClick(Sender: TObject);
    procedure miLookupJtoEClick(Sender: TObject);
    procedure miLookupEtoJClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure aLookupAutoExecute(Sender: TObject);
    procedure aLookupJtoEExecute(Sender: TObject);
    procedure aLookupEtoJExecute(Sender: TObject);
    procedure aLookupClipExecute(Sender: TObject);
    procedure aMatchExactExecute(Sender: TObject);
    procedure aAddToClipboardExecute(Sender: TObject);

  protected
    procedure ClipboardChanged(Sender: TObject);
  public
    procedure LanguageChanged;

  protected
    FDictBeginSet: integer;
    function GetDictBeginSet: integer;
    procedure SetDictBeginSet(const Value: integer);
    procedure ApplyDictBeginSet;
    procedure WordSelectionChanged; override;
    function GetLookupMode: TLookupMode;
    procedure SetLookupMode(const Value: TLookupMode);
    procedure LookupModeChanged; virtual;
    procedure UpdateLookupModeButtonText;
  public
    dictModeSet: TLookupMode;
    procedure SetDefaultColumnWidths; override;
    procedure Refresh; override;
    procedure RestoreLookupMode;
    property DictBeginSet: integer read GetDictBeginSet write SetDictBeginSet;
    property LookupMode: TLookupMode read GetLookupMode write SetLookupMode;

  public
   { Search with current settings, populate the results }
    procedure Look();

  end;

var
  fWordLookup: TfWordLookup;

implementation

uses Math, JWBLanguage, JWBUnit, JWBClipboard, JWBMenu, JWBSettings, JWBEditor,
  JWBWordKanji, JWBExamples, JWBAnnotations, JWBLegacyMarkup;

{$R *.DFM}

procedure TfWordLookup.FormShow(Sender: TObject);
begin
  if GetDictBeginSet<>FDictBeginSet then
    ApplyDictBeginSet;
  if Edit1.Enabled then Edit1.SetFocus;
  Look();
  Clipboard.Watchers.Add(Self.ClipboardChanged);
end;

procedure TfWordLookup.SetDefaultColumnWidths;
begin
  Clipboard.Watchers.Remove(Self.ClipboardChanged);
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
    Self.miLookupJtoE.Caption:=_l('#00329^eJapanese ->English');
    Self.miLookupJtoE.Hint := _l('#00643^Search by japanese reading');
    Self.miLookupEtoJ.Caption:=_l('#00330^eEnglish -> Japanese');
  end else begin
    Self.miLookupJtoE.Caption:=_l('#00331^eChinese ->English');
    Self.miLookupJtoE.Hint := _l('#01134^Search by chinese reading');
    Self.miLookupEtoJ.Caption:=_l('#00332^eEnglish -> Chinese');
  end;
  UpdateLookupModeButtonText;

  if (not aLookupClip.Enabled) and aLookupClip.Checked then
    Self.SetLookupMode(lmJp);
end;

procedure TfWordLookup.ClipboardChanged(Sender: TObject);
begin
  if Self.Visible and (Self.LookupMode=lmClipboard) then
    Self.Look();
end;


{ A way to save beginning/middle/end/exact search config as an integer }
function TfWordLookup.GetDictBeginSet: integer;
begin
  if Self.aMatchLeft.Checked then
    Result := 1
  else
  if Self.aMatchRight.Checked then
    Result := 2
  else
  if Self.aMatchAnywhere.Checked then
    Result := 3
  else
    Result := 0;
end;

{ This can be called before the form is fully initialized, so we store the value
 and apply it when convenient }
procedure TfWordLookup.SetDictBeginSet(const Value: integer);
begin
  FDictBeginSet := Value;
  if Self.Visible then
    ApplyDictBeginSet;
end;

procedure TfWordLookup.ApplyDictBeginSet;
begin
  case FDictBeginSet of
    1: Self.aMatchLeft.Checked := true;
    2: Self.aMatchRight.Checked := true;
    3: Self.aMatchAnywhere.Checked := true;
  else Self.aMatchExact.Checked := true;
  end;
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
    Self.aLookupAuto.Checked := (ANewMode=lmAuto);
    Self.aLookupJtoE.Checked := (ANewMode=lmJp);
    Self.aLookupEtoJ.Checked := (ANewMode=lmEn);
    Self.aLookupClip.Checked := (ANewMode=lmClipboard);
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

  aMatchLeft.Enabled:=true;
  aMatchRight.Enabled:=true;
  aMatchAnywhere.Enabled:=true;

 {$IFDEF SEARCH_BUTTON_CAPTION}
  if (not aAutoPreview.Checked) or aMatchAnywhere.Checked then
    btnSearch.Caption:=_l('#00669^eSearch')
  else
    btnSearch.Caption:=_l('#00670^eAll');
 {$ENDIF}

  if ANewMode in [lmEn, lmEditorInsert] then begin
    if aMatchRight.Checked or aMatchAnywhere.Checked then
      aMatchExact.Execute;
    aMatchRight.Enabled:=false;
    aMatchAnywhere.Enabled:=false;
  end;

  if ANewMode=lmEditorInsert then begin
    if aMatchLeft.Checked then
      aMatchExact.Execute;
    aMatchLeft.Enabled:=false;
  end;

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

 //Some search modes do several requests and we can't reuse request object
 //with different settings after it has been Prepare()d.
  function NewSearchRequest: TDicSearchRequest;
  begin
    Result := TDicSearchRequest.Create;

   //Dictionary group
    if aDictGroup1.Checked then Result.dictgroup:=1 else
    if aDictGroup2.Checked then Result.dictgroup:=2 else
    if aDictGroup3.Checked then Result.dictgroup:=3 else
      Result.dictgroup := 1; //we must have some group chosen

    Result.Full := not btnSearch.Enabled; //if "More matches" is yet enabled, then partial
    Result.MaxWords:=StringGrid.VisibleRowCount;

   //Match type (left/right/exact)
    if aMatchLeft.Checked then Result.MatchType := mtMatchLeft else
    if aMatchRight.Checked then Result.MatchType := mtMatchRight else
    if aMatchAnywhere.Checked then Result.MatchType := mtMatchAnywhere else
      Result.MatchType := mtExactMatch;
    if (lm=lmEn) and not (Result.MatchType in [mtExactMatch, mtMatchLeft]) then
      Result.MatchType := mtExactMatch;

    if (not fSettings.cbDictLimitAutoResults.Checked) or aMatchExact.Checked then Result.Full:=true;

    if lm in [lmEditorInsert] then begin //ignore some UI settings in these modes
      Result.dictgroup := 5;
      Result.MatchType := mtExactMatch;
    end;

    Result.AutoDeflex := aInflect.Checked;
    Result.dic_ignorekana := false;
    Result.MindUserPrior := (lm=lmEditorInsert); //only mind kanji usage priorities in Editor suggestions

   //If full search was not requested and autopreview off / too costly
    if not Result.full
    and (not Self.aAutoPreview.Checked or (Result.MatchType=mtMatchAnywhere)) then
      exit; //do not search
  end;

begin
  //Retrieve current lookup mode
  lm := GetLookupMode();

  //Don't exit if the query is empty, we need to reset results/update presentation/etc

  //We don't auto-search when in MatchAnywhere or when Autosearch is disabled
  if (lm<>lmEditorInsert) and btnSearch.Enabled and (
    (not aAutoPreview.Checked) or aMatchAnywhere.Checked) then
  begin
    btnSearch.Visible:=true;
    StringGrid.Visible:=false;
    BlankPanel.TextVisible:=(edit1.text<>'') or (lm=lmEditorInsert);
    curword:=0;
    WordSelectionChanged;
    exit;
  end;

  StringGrid.RowCount:=200;

  req := NewSearchRequest;
  try
    FResults.Clear;
    wasfull := false;
    if ((lm in [lmAuto, lmJp, lmEn]) and (Edit1.Text=''))
    or ((lm=lmClipboard) and (Clipboard.Text='')) then begin
     //Don't touch dictionaries
      wasfull := true;
    end else
    case lm of
      lmAuto: begin
        req.st := stRomaji;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;
        FreeAndNil(req);

        req := NewSearchRequest;
        req.st := stEnglish;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := wasfull and req.WasFull;
        FreeAndNil(req);

        req := NewSearchRequest;
        req.st := stJapanese;
        req.Prepare;
        req.Search(Edit1.Text, EC_UNKNOWN, FResults);
        wasfull := wasfull and req.WasFull;
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
        for i:=1 to flength(Clipboard.Text) do
         {$IFDEF UNICODE}
          if copy(fgetch(Clipboard.Text,i),1,2)='00' then break
         {$ELSE}
          if fgetch(clip,i)<=#$00FF then break
         {$ENDIF}
          else text:=text+fgetch(Clipboard.Text,i);
        req.st := stJapanese;
        req.Prepare;
        req.Search(text, EC_UNKNOWN, FResults);
        wasfull := req.WasFull;
      end;
      lmEditorInsert: begin
       //First try real word insert buffer
        text := fEditor.GetInsertKana(ikFinal);
        wt := EC_UNKNOWN;
       //If that is empty, show whatever the caret is at
        if text='' then
          text:=fEditor.GetWordAtCaret(wt);
        if text<>'' then begin
          req.st := stJapanese;
          req.Prepare;
          req.Search(text, EC_UNKNOWN, FResults);
        end;
        wasfull := req.WasFull;
      end;
    end;

  //update result list
    ResultsChanged;
    if not wasfull then
      text:=_l('#00671^eSearch results (partial)')
    else
      text:=_l('#00672^eSearch results');
    btnSearch.Visible := not wasfull  //have more results
       or (req.full and not btnSearch.Enabled); //just clicked on "Show all" - keep the disabled button
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
  btnSearch.Enabled:=true; //enable "More matches" => do partial search
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


procedure TfWordLookup.aLookupAutoExecute(Sender: TObject);
begin
  Self.LookupMode := lmAuto;
end;

procedure TfWordLookup.aLookupJtoEExecute(Sender: TObject);
begin
  Self.LookupMode := lmJp;
end;

procedure TfWordLookup.aLookupEtoJExecute(Sender: TObject);
begin
  Self.LookupMode := lmEn;
end;

procedure TfWordLookup.aLookupClipExecute(Sender: TObject);
begin
  Self.LookupMode := lmClipboard;
end;

//Called when any of a lot of lookup switches change
procedure TfWordLookup.aMatchExactExecute(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.aAddToClipboardExecute(Sender: TObject);
begin
  Self.btnCopyToClipboardClick(Sender);
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

procedure TfWordLookup.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=Chr(VK_RETURN) then
    btnSearchClick(nil);
end;

procedure TfWordLookup.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled:=false; //disable "More matches" => do full search
  Look();
end;

procedure TfWordLookup.btnWordKanjiClick(Sender: TObject);
begin
  fMenu.aDictKanji.Execute;
end;

procedure TfWordLookup.btnExamplesClick(Sender: TObject);
begin
  fMenu.aDictExamples.Execute;
end;

end.
