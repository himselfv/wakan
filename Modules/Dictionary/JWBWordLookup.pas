unit JWBWordLookup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Buttons, Menus, ImgList, ToolWin,
  Actions, ActnList, WakanWordGrid, WakanPaintbox, JWBWordLookupBase, SpeedBtn;

type
 //Supported lookup modes for this window.
 //Integer values are important, stored in registry.
  TLookupMode = (
    lmAuto = 0,
    lmJp = 1,
    lmEn = 2,
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
    btnAutoPreview: TSpeedButton;
    btnDictGroup1: TSpeedButton;
    btnDictGroup2: TSpeedButton;
    btnDictGroup3: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    edtSearchText: TEdit;
    btnLookupMode: TWinSpeedButton;
    btnWordKanji: TSpeedButton;
    btnExamples: TSpeedButton;
    CharInWordDock: TPanel;
    btnSearch: TBitBtn;
    Actions: TActionList;
    aLookupAuto: TAction;
    aLookupJtoE: TAction;
    aLookupEtoJ: TAction;
    aLookupClip: TAction;
    aMatchExact: TAction;
    aMatchLeft: TAction;
    aMatchRight: TAction;
    aInflect: TAction;
    aAutoPreview: TAction;
    aDictGroup1: TAction;
    aDictGroup2: TAction;
    aDictGroup3: TAction;
    aMatchAnywhere: TAction;
    aEditorInsert: TAction;
    btnManualMode: TSpeedButton;
    procedure edtSearchTextChange(Sender: TObject);
    procedure edtSearchTextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnWordKanjiClick(Sender: TObject);
    procedure btnExamplesClick(Sender: TObject);
    procedure btnAddToVocabClick(Sender: TObject);
    procedure btnLookupModeClick(Sender: TObject);
    procedure edtSearchTextKeyPress(Sender: TObject; var Key: Char);
    procedure aLookupAutoExecute(Sender: TObject);
    procedure aMatchExactExecute(Sender: TObject);
    procedure aEditorInsertExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miLookupEtoJClick(Sender: TObject);
    procedure aLookupClipExecute(Sender: TObject);
    procedure btnManualModeClick(Sender: TObject);

  protected
    procedure ClipboardChanged(Sender: TObject);
  public
    procedure LanguageChanged;

  protected
    procedure WordSelectionChanged; override;
    function GetLookupMode: TLookupMode;
    procedure SetLookupMode(const Value: TLookupMode);
    procedure LookupModeChanged; virtual;
    procedure UpdateLookupModeButtonText;
  public
    procedure SetDefaultColumnWidths; override;
    procedure Refresh; override;
    procedure RestoreLookupMode;
    property LookupMode: TLookupMode read GetLookupMode write SetLookupMode;

  public
   { Search with current settings, populate the results }
    procedure Look();

  end;

var
  fWordLookup: TfWordLookup;

implementation

uses Math, JWBStrings, JWBLanguage, JWBUnit, JWBClipboard, JWBMenu, JWBSettings,
  JWBEditor, JWBWordKanji, JWBExamples, JWBAnnotations, JWBLegacyMarkup, JWBDic,
  JWBDicSearch;

{$R *.DFM}

procedure TfWordLookup.FormCreate(Sender: TObject);
begin
  inherited;
 { SpeedButtons are linked to Actions, so their GroupIndex is replaced at load,
  but if we set TAction's GroupIndex to non-zero, AutoCheck is not going to work,
  and if we leave TSpeedButton's GroupIndex as 0, it's Down property not going
  to be updated (see TSpeedButtonActionLink.IsCheckedLinked) }
  btnInflect.GroupIndex := 8;
  btnAutoPreview.GroupIndex := 9;
  btnLookupClip.GroupIndex := 2;
end;

procedure TfWordLookup.FormShow(Sender: TObject);
begin
  if edtSearchText.Enabled then edtSearchText.SetFocus;
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
  if Self.Visible and (LookupMode <> lmEditorInsert) and aLookupClip.Checked then
    Self.Look();
end;


{ Restores lookup mode which was last selected by user when this form is open
 by itself (as a dictionary lookup form).
 It's overriden when it's used by Editor for word suggestions. }
procedure TfWordLookup.RestoreLookupMode;
begin
  if aEditorInsert.Checked then begin
    aEditorInsert.Checked := false;
    LookupModeChanged;
  end;
end;

{ Gets or sets LookupMode as a variable. Lookup mode is internally stored as
 "which button is pressed/checked" }
function TfWordLookup.GetLookupMode: TLookupMode;
begin
  if aEditorInsert.Checked then Result := lmEditorInsert else
  if aLookupAuto.Checked then Result := lmAuto else
  if aLookupJtoE.Checked then Result := lmJp else
  if aLookupEtoJ.Checked then Result := lmEn else
    Result := lmEditorInsert;
      //nothing is Checked usually on init, so return this to ensure re-applying
      //the correct static value
end;

procedure TfWordLookup.SetLookupMode(const Value: TLookupMode);
begin
  if GetLookupMode=Value then exit; //sometimes we set it just to be sure
  aEditorInsert.Checked := (Value=lmEditorInsert);
  if Value<>lmEditorInsert then //else leave static config as is
    case Value of
      lmAuto: aLookupAuto.Checked := true;
      lmJp: aLookupJtoE.Checked := true;
      lmEn: aLookupEtoJ.Checked := true;
    else aLookupAuto.Checked := true;
    end;
  LookupModeChanged; //apply restrictions and buttons
end;

{ Called when lookup mode changes due to any of the buttons being pressed/unpressed,
 checked/unchecked etc.
 Updates all dependent controls such as fMenu actions. }
procedure TfWordLookup.LookupModeChanged;
var ANewMode: TLookupMode;
begin
  ANewMode := GetLookupMode;

  UpdateLookupModeButtonText;

 //Checked submenu item stays checked; auto-applied if you click parent button
  case ANewMode of
    lmAuto: miLookupAuto.Checked := true;
    lmJp: miLookupJtoE.Checked := true;
    lmEn: miLookupEtoJ.Checked := true
  //else preserve checked submenu item
  end;

  //Disable and uncheck some visual clues in EditorInsert override
  if aNewMode in [lmEditorInsert] then begin
    aLookupAuto.Enabled := false;
    aLookupJtoE.Enabled := false;
    aLookupEtoJ.Enabled := false;
    aLookupClip.Enabled := false;
    btnLookupMode.Enabled := false;
    btnLookupClip.Down := false;

    btnLookupMode.Visible := false;
    //When aligning things in alRight sequence, you have to align with their right
    //borders, against the right neighbour
    btnManualMode.Left := btnLookupClip.Left - btnManualMode.Width + 1;
    btnManualMode.Visible := true;
  end else begin
    aLookupAuto.Enabled := true;
    aLookupJtoE.Enabled := true;
    aLookupEtoJ.Enabled := true;
    aLookupClip.Enabled := true;
    btnLookupMode.Enabled := true;
    btnLookupClip.Down := aLookupClip.Checked;

    btnManualMode.Visible := false;
    btnLookupMode.Left := btnLookupClip.Left - btnLookupMode.Width + 1; //put to the left of clipboard button
    btnLookupMode.Visible := true;
  end;

  if (ANewMode in [lmAuto, lmJp, lmEn]) and not aLookupClip.Checked then
  begin
    edtSearchText.Enabled:=true;
    edtSearchText.Color:=clWindow;  
  end else begin
    edtSearchText.Enabled:=false;
    edtSearchText.Color:=clMenu;
  end;

  aMatchExact.Enabled:=true;
  aMatchLeft.Enabled:=true;
  aMatchRight.Enabled:=true;
  aMatchAnywhere.Enabled:=true;

  aInflect.Enabled := not (ANewMode in [lmEditorInsert]); //Editor mode always uses deflexion, so disable the choice
  aAutoPreview.Enabled := not (ANewMode in [lmEditorInsert]);

 {$IFDEF SEARCH_BUTTON_CAPTION}
  if (not aAutoPreview.Checked) or aMatchAnywhere.Checked then
    btnSearch.Caption:=_l('#00669^eSearch')
  else
    btnSearch.Caption:=_l('#00670^eAll');
 {$ENDIF}

   btnSearch.Enabled := true; //Reenable the search button because this is a new request

  if ANewMode = lmEn then begin
    if aMatchRight.Checked or aMatchAnywhere.Checked then
      aMatchExact.Execute;
    aMatchRight.Enabled:=false;
    aMatchAnywhere.Enabled:=false;
  end;

  if ANewMode = lmEditorInsert then begin
    aMatchExact.Enabled := false;
    aMatchLeft.Enabled := false;
    aMatchRight.Enabled := false;
    aMatchAnywhere.Enabled := false;
    //but leave the value as is
  end;

  if edtSearchText.Enabled and edtSearchText.Visible and edtSearchText.HandleAllocated and Self.Visible then
    edtSearchText.SetFocus;
  Look(); //update results in new mode
 //^ with lmClipboard edtSearchText.Text can be empty, so don't check for that
end;

{ Updates text on lookup mode button according to currently selected mode.
 We wouldn't need this as a separate function since it's mostly part of
 LookupModeChanged, but unfortunately text changes depending on Japanese/Chinese
 being selected, so we need to do this separately sometimes. }
procedure TfWordLookup.UpdateLookupModeButtonText;
begin
  if aEditorInsert.Checked
  or aLookupAuto.Checked then begin
    btnLookupMode.Caption := miLookupAuto.Caption;
    btnLookupMode.Hint := miLookupAuto.Hint;
  end else
  if aLookupJtoE.Checked then begin
    btnLookupMode.Caption := miLookupJtoE.Caption;
    btnLookupMode.Hint := miLookupJtoE.Hint;
  end else
  if aLookupEtoJ.Checked then begin
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
  wasfull:boolean;
  text:string;
  roma: string;
  CanSelect: boolean;

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

    if btnSearch.Enabled then
      //if "More matches" is yet enabled, then partial
      Result.MaxWords := StringGrid.VisibleRowCount
    else
      Result.MaxWords := -1;

   //Match type (left/right/exact)
    if aMatchLeft.Checked then Result.MatchType := mtMatchLeft else
    if aMatchRight.Checked then Result.MatchType := mtMatchRight else
    if aMatchAnywhere.Checked then Result.MatchType := mtMatchAnywhere else
      Result.MatchType := mtExactMatch;
    if (lm=lmEn) and not (Result.MatchType in [mtExactMatch, mtMatchLeft]) then
      Result.MatchType := mtExactMatch;

    if (not fSettings.cbDictLimitAutoResults.Checked) or aMatchExact.Checked then
      Result.MaxWords := -1;

    if lm in [lmEditorInsert] then begin //ignore some UI settings in these modes
      Result.dictgroup := 5;
      Result.MatchType := mtBestGuessLeft;
      //In EditorInsert mode we want the best left guess to what's being typed
    end;

    Result.AutoDeflex :=
      (lm in [lmEditorInsert])    //always auto-deflex in editor mode
      or aInflect.Checked;
    Result.dic_ignorekana := false;
    Result.MindUserPrior := (lm=lmEditorInsert); //only mind kanji usage priorities in Editor suggestions

   //If full search was not requested and autopreview off / too costly
    if (Result.MaxWords > 0)
    and (not Self.aAutoPreview.Checked or (Result.MatchType=mtMatchAnywhere)) then
      exit; //do not search
  end;

  function GetSearchText: string;
  var i: integer;
  begin
    if aLookupClip.Checked then begin
      Result := '';
      for i:=1 to flength(Clipboard.Text) do
       {$IFDEF UNICODE}
        if copy(fgetch(Clipboard.Text,i),1,2)='00' then break
       {$ELSE}
        if fgetch(clip,i)<=#$00FF then break
       {$ENDIF}
        else Result :=Result + fgetch(Clipboard.Text, i);
    end else
      Result := edtSearchText.Text;
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
    BlankPanel.TextVisible:=(edtSearchText.text<>'') or (lm=lmEditorInsert);
    curword:=0;
    WordSelectionChanged;
    exit;
  end;

  StringGrid.RowCount:=200;

  req := NewSearchRequest;
  try
    FResults.Clear;
    wasfull := false;
    if (lm in [lmAuto, lmJp, lmEn]) and (
      (not aLookupClip.Checked and (edtSearchText.Text=''))
      or (aLookupClip.Checked and (Clipboard.Text=''))
    ) then begin
     //Don't touch dictionaries
      wasfull := true;
    end else
    case lm of
      lmAuto: begin
        text := GetSearchText;
        req.st := stRomaji;
        req.Prepare;
        req.Search(text, FResults, EC_UNKNOWN);
        wasfull := req.WasFull;
        FreeAndNil(req);

        req := NewSearchRequest;
        req.st := stEnglish;
        req.Prepare;
        req.Search(text, FResults, EC_UNKNOWN);
        wasfull := wasfull and req.WasFull;
        FreeAndNil(req);

        req := NewSearchRequest;
        req.st := stJapanese;
        req.Prepare;
        req.Search(text, FResults, EC_UNKNOWN);
        wasfull := wasfull and req.WasFull;
      end;
      lmJp: begin
        text := GetSearchText;

        req.st := stRomaji;
        req.Prepare;
        req.Search(text, FResults, EC_UNKNOWN);
        wasfull := req.WasFull;
        FreeAndNil(req);

        req := NewSearchRequest;
        req.st := stJapanese;
        req.Prepare;
        req.Search(text, FResults, EC_UNKNOWN);
        wasfull := wasfull and req.WasFull;
      end;
      lmEn: begin
        req.st := stEnglish;
        req.Prepare;
        req.Search(GetSearchText, FResults, EC_UNKNOWN);
        wasfull := req.WasFull;
      end;

      lmEditorInsert: begin
        //First try the insert buffer
        text := fEditor.GetInsertKana(ikFinal);
        if text <> '' then begin
         {
         We want to search for the stuff the user types, which is internally romaji.
         If we convert it and search by kana then stJapanese matches it against
         KANJI fields in the DB.

         This is correct since "stJapanese" really means "by expression".
         It has some corner handling for also looking at readings if the query
         is kana-only, but we shouldn't rely on that.

         What we really want is to search by reading (stRomaji)
         }
          roma := KanaToRomaji(text, curlang, []); //keep all chars
          req.st := stRomaji;

         {
         LOOKAHEAD/LOOKBEHIND:
         There are two ways we can approach typing suggestions: lookahead and lookbehind.

         Lookahead (mtMatchLeft):
           "souzoury" -> suggest "souzouryoku"
           Nice, but it'll need some changes to handle accepting suggestion when
           the word is not fully typed:
             "souzoury" [Enter] -> "想像力"

         Lookbehind (mtBestGuessLeft/mtExactMatch):
           "souzouryu" -> suggest "souzou"
           Accepting only accepts the guessed part:
             "souzoury" [Enter] -> "想像ry" (currently)

         Wakan has always been lookbehind.

         CHOOSING SUGGESTIONS:
         As the user types, there are 3 major stages:
         1. Fully-formed word (そうぞう = 創造)
         2. Word + yet unparsable roma (そうぞうry)
         3. Word + yet incomplete tail (そうぞうりょ)
         4. Extended word (そうぞうりょく = 創造力)

         We want to always suggest the best lookbehind, e.g.
         1. そうぞう = 創造
         2. そうぞうry = 創造ry  (so that the suggestion window does not flicker on/off)
         3. そうぞうりょ = 創造りょ

         #3 is even more important as there are word endings which the user
         might consider part of the word:
            あらわれていた [Enter] - we should detect 現れて

         We therefore want the BestGuessLeft, but it gives us too much:
            そうぞうりょ  gives matches even for そ
         If we limit by MaxWords we can miss important matches (the user must be
         able to choose from all of them!)
         ExactMatch, on the other hand, fails on 2 and 3.

         What we'd really like is to try shorter and shorter guesses until one
         returned some matches, then say "A-ha! That was the longest fit"
         and show all those matches and only them.

         The logic is too specific to put into Search() so we're just going
         to do it here.
         }

          req.MatchType := mtExactMatch;
          req.Prepare;
          req.Search(roma, FResults);

          while (text <> '') and (FResults.Count <= 0) do begin
            text := fcopy(text, 1, flength(text)-1);
            roma := DbKanaToRomaji(text, curlang, []);
            req.Search(roma, FResults);
          end;

        end

        else begin
         //Show whatever the caret is at
         //Here we're looking at a parsed text so it's usually kanji; stJapanese is our best chance
          text := fEditor.GetWordAtCaret(wt);
          req.st := stJapanese;
          req.MatchType := mtBestGuessLeft; //This is a completed text, just find the best guess
          req.Prepare;
          req.Search(text, FResults, wt);
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
       or ((req.MaxWords<=0) and not btnSearch.Enabled //HAD more results but clicked on "Show all" - keep the disabled button
           and (lm <> lmEditorInsert));                //in editor-insert mode hide the disabled button anyway
    case lm of
      lmJp: text:=text+' '+_l('#00673^eby phonetic');
      lmEn: text:=text+' '+_l('#00674^eby meaning');
      lmEditorInsert: text:=text+' '+_l('#00676^eby written (text)');
    end;
    text:=text+' ('+inttostr(FResults.Count)+')';
    curword:=0;
    if StringGrid.Visible then StringGridSelectCell(self,0,1,CanSelect);
    if StringGrid.Visible then StringGrid.Row:=1;
    if StringGrid.Visible then curword:=1;
    WordSelectionChanged;

  finally
    FreeAndNil(req);
  end;
end;

procedure TfWordLookup.edtSearchTextChange(Sender: TObject);
begin
  btnSearch.Enabled:=true; //enable "More matches" => do partial search
  Look();
end;

procedure TfWordLookup.edtSearchTextClick(Sender: TObject);
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
  if aEditorInsert.Checked then
    aEditorInsert.Checked := false;
  LookupModeChanged; //apply restrictions and buttons
end;

procedure TfWordLookup.aLookupClipExecute(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.miLookupEtoJClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
   1: aLookupAuto.Execute;
   2: aLookupJtoE.Execute;
   3: aLookupEtoJ.Execute;
  end;
end;

procedure TfWordLookup.aEditorInsertExecute(Sender: TObject);
begin
  LookupModeChanged; //apply restrictions and buttons
end;

//Called when any of a lot of lookup switches change
procedure TfWordLookup.aMatchExactExecute(Sender: TObject);
begin
  LookupModeChanged;
end;

procedure TfWordLookup.btnAddToVocabClick(Sender: TObject);
begin
  inherited;
  Look();
  if edtSearchText.Enabled then edtSearchText.SetFocus;
end;

procedure TfWordLookup.btnLookupModeClick(Sender: TObject);
begin
  btnLookupMode.DropDownClick;
end;

procedure TfWordLookup.edtSearchTextKeyPress(Sender: TObject; var Key: Char);
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

{
Switches back to user input when in editor-insert mode. This allows the user to type
manual searches in the editor dictionary panel.
Do not reactivate on a simple form-click because the user might want to browse
or copy the automatic results.
}
procedure TfWordLookup.btnManualModeClick(Sender: TObject);
begin
  inherited;
  Self.RestoreLookupMode;
  //Forcefully steal focus because even after switching to manual, we might
  //be in Clipboard mode where the editbox won't steal it automatically.
  //And we need it stolen, so the Editor knows it's not in charge anymore.
  if not edtSearchText.Enabled or not edtSearchText.Focused then
    Self.SetFocus();
end;

end.
