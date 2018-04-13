unit JWBKanjiList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Actions, ActnList, CheckAction,
  JWBStrings, IniFiles, Grids, DB, ShellAPI, WakanPaintbox, Menus, CheckLst,
  ImgList, JWBRadicalList, SimpleControls, RangeSpinEdit, JvExControls,
  JvArrowButton, SpeedBtn, Vcl.ToolWin;

//{$DEFINE INVALIDATE_WITH_DELAY}
// If set, InvalidateList() will use timer and not just update instanteneously.

{$DEFINE AUTODEFOCUS}
//  If the previously selected character is not available under the new filters,
//  automatically set focus to one of the available characters.

type
  TFilterType = (
    ftExact,        //exactly what's written (case insensitive)
    ftStart,        //starts with it
    ftExactOrBase,  //kana: match either full KUN or it's base without okurigana
    ftAnywhere,     //any part of expression
    ftAnyWord,      //any part between spaces/separators
    ftNumber        //exact match, but allow filters like "5-7"
  );

  TLanguageSet = set of AnsiChar;

  TfKanji = class(TForm)
    Panel1: TPanel;
    DrawGrid1: TDrawGrid;
    SaveDialog1: TSaveDialog;
    UpdateTimer: TTimer;
    BlankPanel1: TBlankPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    btnCompounds: TSpeedButton;
    btnKanjiDetails: TSpeedButton;
    btnPrintCards: TButton;
    Actions: TActionList;
    aPrint: TAction;
    aResetFilters: TAction;
    aOnlyCommon: TCheckAction;
    aInClipboard: TCheckAction;
    aPinYin: TAction;
    aYomi: TAction;
    aRadical: TAction;
    aMeaning: TAction;
    aSaveToFile: TAction;
    aSearch: TCheckAction;
    PopupMenu: TPopupMenu;
    miCopyAs: TMenuItem;
    N1: TMenuItem;
    ilCategoryActions: TImageList;
    pmCategories: TPopupMenu;
    miAddCategory: TMenuItem;
    miUncheckAllCategories: TMenuItem;
    miEditCategory: TMenuItem;
    miDeleteCategory: TMenuItem;
    pnlDockCompounds: TPanel;
    splDockCompounds: TSplitter;
    lblFoundChars: TLabel;
    Label2: TLabel;
    miCharWords: TMenuItem;
    miCharDetails: TMenuItem;
    N2: TMenuItem;
    miCategories: TMenuItem;
    miCopy: TMenuItem;
    miLookUpIn: TMenuItem;
    miBeforeLookupIn: TMenuItem;
    miAfterLookupIn: TMenuItem;
    PopupImages: TImageList;
    rgSortBy: TComboBox;
    pnlSearch: TPanel;
    sbInClipboard: TSpeedButton;
    sbOnlyCommon: TSpeedButton;
    edtLookup: TEdit;
    sbStrokeCount: TWinSpeedButton;
    sbRadicals: TWinSpeedButton;
    btnGroups: TWinSpeedButton;
    cbLookupType: TComboBox;
    pnlGroups: TPopupPanel;
    Panel9: TPanel;
    sbJouyou: TSpeedButton;
    edtJouyou: TRangeSpinEdit;
    Panel11: TPanel;
    sbJlpt: TSpeedButton;
    edtJlpt: TRangeSpinEdit;
    lbCategories: TCheckListBox;
    cbOrAnd: TComboBox;
    pnlStrokeCount: TPopupPanel;
    edtStrokeCount: TRangeSpinEdit;
    aSearchByText: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SearchFilterChanged(Sender: TObject);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure btnKanjiDetailsClick(Sender: TObject);
    procedure btnCompoundsClick(Sender: TObject);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1DblClick(Sender: TObject);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGrid1Click(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aResetFiltersExecute(Sender: TObject);
    procedure aPinYinExecute(Sender: TObject);
    procedure aYomiExecute(Sender: TObject);
    procedure aRadicalExecute(Sender: TObject);
    procedure aMeaningExecute(Sender: TObject);
    procedure aSaveToFileExecute(Sender: TObject);
    procedure aSearchExecute(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure miUncheckAllCategoriesClick(Sender: TObject);
    procedure miAddCategoryClick(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesClickCheck(Sender: TObject);
    procedure miDeleteCategoryClick(Sender: TObject);
    procedure miEditCategoryClick(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbCategoriesDblClick(Sender: TObject);
    procedure edtStrokeCountChange(Sender: TObject);
    procedure edtJouyouChange(Sender: TObject);
    procedure miCharWordsClick(Sender: TObject);
    procedure miCharDetailsClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbStrokeCountDropDownClick(Sender: TObject);
    procedure aInClipboardExecute(Sender: TObject);
    procedure aOnlyCommonExecute(Sender: TObject);
    procedure aOnlyCommonChecked(Sender: TObject);
    procedure sbOnlyCommonClick(Sender: TObject);
    procedure aInClipboardChecked(Sender: TObject);
    procedure sbInClipboardClick(Sender: TObject);
    procedure sbStrokeCountClick(Sender: TObject);
    procedure pnlStrokeCountExit(Sender: TObject);
    procedure edtJlptChange(Sender: TObject);
    procedure sbRadicalsClick(Sender: TObject);
    procedure sbRadicalsDropDownClick(Sender: TObject);
    procedure btnGroupsClick(Sender: TObject);
    procedure aSearchByTextExecute(Sender: TObject);

  protected
    FCellSize: integer;
    FCellFontSize: integer;
    procedure ActualizeCellSize;
    function GetCellSize: integer;
    function GetCellFontName: string;
    function GetCellFontSize: integer;
    procedure SetCellSize(Value: integer);
    function GetExpectedColCount: integer;

  protected
    FFocusedChars: FString;
    procedure KanjiGridSelectionChanged;
    function FindCharacterListIndex(const ch: FChar): integer;
    function KanjiGridGetSelection: FString;
    function KanjiGridSetSelection(const chars: FString): boolean;
    procedure SetFocusedCharsLow(const Value: FString);
    procedure CategoryListChanged(Sender: TObject);
    procedure KanjiCategoryEntriesChanged(Sender: TObject);
    procedure ClipboardChanged(Sender: TObject);
    procedure ReadFilter(flt: TStringList; const tx: string; typ: integer;
      ftype: TFilterType);
    procedure ReadAnyTextFilter(flt: TStringList; const tx: string; typ: integer;
      ftype: TFilterType; lang: TLanguageSet);
    procedure ReadRaineFilter(fltradical:TStringList;const ARadicals:string);
  public
    procedure SaveSettings(reg: TCustomIniFile);
    procedure LoadSettings(reg: TCustomIniFile);
    procedure LanguageChanged;
    procedure InvalidateList;
    procedure Reload;
    procedure SaveChars;
    procedure ResetFilters;
    procedure SetCategoryFilter(const ACategories: array of integer;
      AOr, ANot: boolean);
    procedure FilterByRadical(const radno: integer);
    function GetKanji(cx,cy:integer): string;
    function IsCharacterVisible(const ch: FChar): boolean;
    procedure SetFocusedChars(const Value: FString);
    function MaybeSetFocusedChars(const Value: FString): boolean;
    property FocusedChars: FString read FFocusedChars write SetFocusedChars;

  protected //Search filters
    FSetSortBy: integer;
    FSetLookupTypeIndex: integer; //set when reading settings, applied on next ReloadLookupTypes()
    function GetLookupTypeIndex: integer;
    procedure SetLookupTypeIndex(const AItemIndex: integer);
    procedure ClearLookupTypes;
    procedure ReloadLookupTypes;
    procedure ReloadSortBy;

  protected
    FCurRadChars: string; //C urrently selected radical characters
    procedure SetCurRadChars(const Value: string);
    procedure RadicalSelectionChanged(Sender: TObject);
  public
    CurRadSearchType: TRadSearchType;
    property CurRadChars: string read FCurRadChars write SetCurRadChars;

  protected //Right-click menu
    procedure ReloadCopyFormats;
    procedure CopyInFormatClick(Sender: TObject);
    procedure CopyAsXMLClick(Sender: TObject);
    procedure ConfigureClick(Sender: TObject);
    procedure ReloadPopupCategories;
    procedure CategoryItemClick(Sender: TObject);
    procedure CategoryNewItemClick(Sender: TObject);
    procedure ClearReferenceLinks;
    procedure ReloadReferenceLinks;
  public
    procedure CopyAsText(const AReplace: boolean);
    procedure CopyToClipboard(const AXsltFilename: string; const AReplace: boolean);

  end;

var
  fKanji: TfKanji;
  testkanji:string;

function GetKanjiCopyFormatsDir: string;
function GetKanjiCopyFormats: TArray<string>;
function KanjiInfoToXml(const AChar: string): string;

implementation
uses Types, UITypes, JWBIO, JWBUnit, JWBClipboard, JWBMenu, JWBSettings, JWBPrint,
  JWBKanjiCompounds, JWBKanjiDetails, JWBFileType, JWBLanguage, JWBKanjiCard,
  KanaConv, JWBCategories, JWBAnnotations, TextTable, JWBCharData, JWBForms,
  JWBIntTip, JWBScreenTip, AppData, JWBWordLookupBase, JWBRefLinks,
  AnnotationsSettings;

var ki:TStringList;

{$R *.DFM}

procedure TfKanji.FormCreate(Sender: TObject);
begin
  CurRadSearchType:=stRaine;
  FCurRadChars:='';
  OnCategoryListChanged.Add(Self.CategoryListChanged);
  OnKanjiCategoryEntriesChanged.Add(Self.KanjiCategoryEntriesChanged);
end;

procedure TfKanji.FormDestroy(Sender: TObject);
begin
  OnKanjiCategoryEntriesChanged.Add(Self.KanjiCategoryEntriesChanged);
  OnCategoryListChanged.Remove(Self.CategoryListChanged);
end;

procedure TfKanji.FormShow(Sender: TObject);
begin
  ReloadLookupTypes;
  cbOrAnd.ItemIndex := 0;
  Reload;
  Self.btnKanjiDetails.Down := fKanjiDetails.Visible;
  Clipboard.Watchers.Add(Self.ClipboardChanged);
  sbOnlyCommon.GroupIndex := 14;
  sbInClipboard.GroupIndex := 15;
  sbOnlyCommon.Down := aOnlyCommon.Checked; //property loading may break this at start
  sbInClipboard.Down := aInClipboard.Checked;
  if edtLookup.Enabled then edtLookup.SetFocus;
end;

procedure TfKanji.FormHide(Sender: TObject);
begin
  Clipboard.Watchers.Remove(Self.ClipboardChanged);
end;

procedure TfKanji.FormResize(Sender: TObject);
begin
  if DrawGrid1.ColCount<>GetExpectedColCount then Reload;
end;

//Called by MainForm when selected language (Japanese/Chinese) changes
procedure TfKanji.LanguageChanged;
begin
  ReloadSortBy;
  Self.InvalidateList;
end;

procedure TfKanji.CategoryListChanged;
begin
 //Reload filter categories
  PasteKanjiCategoriesTo(Self.lbCategories.Items);
  Self.lbCategories.ItemIndex:=0;
  Self.lbCategoriesClick(Self); //react to changes
 //Do not update popup categories here, instead do it in OnPopup
end;

procedure TfKanji.KanjiCategoryEntriesChanged(Sender: TObject);
begin
 //We may need to update the list of kanji if Category filters are in place.
 //We may also need to repaint LEARNED kanji in new color and the way it's done now, this also
 //requires Reload.
  Self.Reload;
end;

procedure TfKanji.ClipboardChanged(Sender: TObject);
begin
  if Self.Visible and Self.aInClipboard.Checked then
    Self.InvalidateList;
end;

//Saves form settings to registry
procedure TfKanji.SaveSettings(reg: TCustomIniFile);
begin
  reg.WriteBool('KanjiSearch','OnlyCommon',Self.aOnlyCommon.Checked);
//  reg.WriteBool('KanjiSearch','InClipboard',Self.cbInClipboard.Checked); //do not save-restore this for now (by design)

  reg.WriteInteger('KanjiSearch','LookupTypeIndex', Self.GetLookupTypeIndex);
  reg.WriteString('KanjiSearch','LookupQuery', Self.edtLookup.Text);

  if Self.sbStrokeCount.Down and (Self.edtStrokeCount.Text<>'') and (Self.edtStrokeCount.Text<>'0') then
    reg.WriteString('KanjiSearch','Strokes',Self.edtStrokeCount.Text)
  else
    reg.DeleteKey('KanjiSearch','Strokes');
  if Self.sbRadicals.Down and (Self.curRadChars<>'') then begin
    reg.WriteInteger('KanjiSearch','RadSearchType',integer(Self.curRadSearchType));
    reg.WriteString('KanjiSearch','RadSearch',Self.curRadChars);
  end else begin
    reg.DeleteKey('KanjiSearch','RadSearchType');
    reg.DeleteKey('KanjiSearch','RadSearch');
    reg.DeleteKey('KanjiSearch','RadIndexes');
  end;
  if Self.sbJouyou.Down and (Self.edtJouyou.Text<>'') and (Self.edtJouyou.Text<>'0') then
    reg.WriteString('KanjiSearch','Jouyou',Self.edtJouyou.Text)
  else
    reg.DeleteKey('KanjiSearch','Jouyou');
  if Self.sbJlpt.Down and (Self.edtJlpt.Text<>'') and (Self.edtJlpt.Text<>'0') then
    reg.WriteString('KanjiSearch','Jlpt',Self.edtJlpt.Text)
  else
    reg.DeleteKey('KanjiSearch','Jlpt');
  reg.WriteInteger('Characters','Sort',Self.rgSortBy.ItemIndex);
end;

//Loads form settings from registry
procedure TfKanji.LoadSettings(reg: TCustomIniFile);
begin
  Self.aOnlyCommon.Checked := reg.ReadBool('KanjiSearch','OnlyCommon',false);
//  Self.cbInClipboard.Checked := reg.ReadBool('KanjiSearch','InClipboard',false); //do not save-restore this for now (by design)

  SetLookupTypeIndex(reg.ReadInteger('KanjiSearch','LookupTypeIndex',0));
  Self.edtLookup.Text := reg.ReadString('KanjiSearch','LookupQuery','');
  Self.edtStrokeCount.Text := reg.ReadString('KanjiSearch','Strokes','');
  Self.curRadSearchType := TRadSearchType(reg.ReadInteger('KanjiSearch','RadSearchType',0));
  Self.curRadChars := reg.ReadString('KanjiSearch','RadSearch','');
  Self.edtJouyou.Text := reg.ReadString('KanjiSearch','Jouyou','');
  Self.edtJlpt.Text := reg.ReadString('KanjiSearch','Jlpt','');
  Self.FSetSortBy := reg.ReadInteger('Characters','Sort',0);
end;


function TfKanji.GetCellFontName: string;
begin
  if curLang='j' then
    Result := FontJapaneseGrid
  else
  case fSettings.RadioGroup5.ItemIndex of
    0: Result := FontChineseGrid;
    1: Result := FontChineseGridGB;
    2: Result := FontRadical;
  else Result := FontChineseGrid;
  end;
end;

procedure TfKanji.ActualizeCellSize;
var NewCellSize: integer;
begin
  //Currently the cell size is dictated by fSettings which does not notify us
  //on changes, so we have to track it the weird way
  case fSettings.rgKanjiGridSize.ItemIndex of
    0: NewCellSize:=30;
    1: NewCellSize:=45;
    2: NewCellSize:=60;
  else NewCellSize:=60;
  end;

  if FCellSize <> NewCellSize then
    SetCellSize(NewCellSize); //triggering the font size recalculation
end;

{ Returns cell width/height under current settings }
function TfKanji.GetCellSize: integer;
begin
  ActualizeCellSize;
  Result := FCellSize;
end;

function TfKanji.GetCellFontSize: integer;
begin
  ActualizeCellSize;
  Result := FCellFontSize;
end;

procedure TfKanji.SetCellSize(Value: integer);
begin
  if Value <= 10 then Value := 10; //too small, also rules out zero
  if FCellSize = Value then exit;
  FCellSize := Value;
  FCellFontSize := FCellSize - 8; //default rule
end;


{ Returns expected column count according to current cell size / width }
function TfKanji.GetExpectedColCount: integer;
var CellSize, FreeSpace: integer;
begin
  CellSize := GetCellSize;
  FreeSpace := DrawGrid1.ClientWidth - 8;
  //To keep things simple, always leave space for scroll bar (even if it's hidden)
  if GetWindowLong(DrawGrid1.Handle, GWL_STYLE) and WS_VSCROLL = 0 then
    Dec(FreeSpace, GetSystemMetrics(SM_CXVSCROLL));
  Result := FreeSpace div CellSize;
end;

//Resets filters but does not apply it, so that you can chain it with something.
procedure TfKanji.ResetFilters;
begin
  SetLookupTypeIndex(0);
  edtLookup.Text := '';
  aOnlyCommon.Checked := false;
  aInClipboard.Checked := false;
  edtStrokeCount.Text := '';
  FCurRadChars := '';
  edtJouyou.Text := '';
  edtJlpt.Text := '';
  SetCategoryFilter([],true,false);
end;

procedure TfKanji.aResetFiltersExecute(Sender: TObject);
begin
  ResetFilters;
  Self.InvalidateList;
end;

{ Sets category filter as specified but does not apply it, allowing you to chain
 it with some other changes.
 ACategories is an array of category indexes }
procedure TfKanji.SetCategoryFilter(const ACategories: array of integer;
  AOr, ANot: boolean);
var i, j, idx: integer;
  found: boolean;
begin
  if AOr then
    if not ANot then
      cbOrAnd.ItemIndex := 0
    else
      cbOrAnd.ItemIndex := 2
  else
    cbOrAnd.ItemIndex := 1;

  for i:=0 to lbCategories.Items.Count-1 do begin
    idx := GetCatIdx(lbCategories, i);
    found := false;
    for j := Low(ACategories) to High(ACategories) do
      if ACategories[j]=idx then begin
        found := true;
        break;
      end;
    lbCategories.Checked[i]:=found;
  end;
end;


const
  LOOKUP_ANY          = 0;
  LOOKUP_CHARS        = 1;
  LOOKUP_DEFINITION   = 2;
  LOOKUP_ON           = 3;
  LOOKUP_KUN          = 4;
  LOOKUP_PINYIN       = 5;
  LOOKUP_SKIP         = 6;
  FIXED_LOOKUP_TYPES  = 6;

//Reloads a list of textual search types
procedure TfKanji.ClearLookupTypes;
begin
  cbLookupType.Clear;
  cbLookupType.Items.Add(_l('#01132^Any matches'));
  cbLookupType.Items.Add(_l('#01213^Characters'));
  cbLookupType.Items.Add(_l('#01214^Definition'));
  cbLookupType.Items.Add(_l('#01215^On')); //4
  cbLookupType.Items.Add(_l('#01216^Kun')); //5
  cbLookupType.Items.Add(_l('#01217^PinYin')); //2 mandarin
  cbLookupType.Items.Add(_l('#01218^SKIP')); //22
end;

procedure TfKanji.ReloadLookupTypes;
var i: integer;
  AOldLookupTypeIndex: integer;
begin
  if cbLookupType.ItemIndex=-1 then begin //this is the first reload
    AOldLookupTypeIndex := FSetLookupTypeIndex;
    FSetLookupTypeIndex := -MAXINT; //to make it obvious if we mistakengly reuse it
  end else
    AOldLookupTypeIndex := GetLookupTypeIndex;

  ClearLookupTypes;
  for i:=0 to Length(CharPropTypes)-1 do
   //Exclude non-textual types and types handled in manual mode
    if not (CharPropTypes[i].id in [
      0, //-
      ptMandarinReading,
      ptJapaneseDefinition,
      ptOnReading,
      ptKunReading,
      ptChineseDefinition,
      ptRadicals,
      ptTotalStrokes,
      ptBushuRadical,
      ptRSUnicode,
      ptRSJapanese,
      ptRSKanWa,
      ptRSKangXi,
      ptRSKorean,
      ptSKIP
    ]) then
      cbLookupType.Items.AddObject(_l('^e'+CharPropTypes[i].englishName),
        TObject(CharPropTypes[i].id));
  SetLookupTypeIndex(AOldLookupTypeIndex);
end;

//Returns integer uniquely identifying the selected lookup type (either
//predefined one or from the full list)
function TfKanji.GetLookupTypeIndex: integer;
begin
  if cbLookupType.ItemIndex < 0 then
    Result := 0
  else
  if cbLookupType.ItemIndex <= FIXED_LOOKUP_TYPES then
    Result := -cbLookupType.ItemIndex
  else
   //Else return positive integer for the property # selected from the full list
    Result := integer(cbLookupType.Items.Objects[cbLookupType.ItemIndex]);
end;

//Selects the lookup type given it's id
procedure TfKanji.SetLookupTypeIndex(const AItemIndex: integer);
var i, idx: integer;
begin
 //Sometimes this is called when "other search types" are not yet reloaded,
 //so we save the value until then.
  if cbLookupType.Items.Count<=0 then
    FSetLookupTypeIndex := AItemIndex
  else
  if AItemIndex < -FIXED_LOOKUP_TYPES then
    cbLookupType.ItemIndex := 0 //unsupported fixed type
  else
  if AItemIndex < 0 then
    cbLookupType.ItemIndex := -AItemIndex //fixed type
  else begin
    idx := 0; //fallback
    for i := FIXED_LOOKUP_TYPES+1 to cbLookupType.Items.Count-1 do
      if integer(cbLookupType.Items.Objects[i]) = AItemIndex then begin
        idx := i;
        break;
      end;
    cbLookupType.ItemIndex := idx;
  end;
end;

{ Called when a radical filter changes }
procedure TfKanji.SetCurRadChars(const Value: string);
begin
  FCurRadChars := Value;
  if FCurRadChars = '' then begin
    sbRadicals.Caption := _l('#00178^Radical');
    sbRadicals.Down := false;
  end else begin
    sbRadicals.Caption := _l('#01207^Radicals: %s', [FCurRadChars]);
    sbRadicals.Down := true;
  end;
  Self.InvalidateList;
end;

procedure TfKanji.RadicalSelectionChanged(Sender: TObject);
begin
  curRadSearchType := fRadical.SearchType;
  curRadChars := fRadical.SelectedRadicals;
 //SetCurRadChars() will trigger filter update
end;

//Called from LanguageChanged because can be different for different target languages
procedure TfKanji.ReloadSortBy;
begin
  rgSortBy.Items.Clear;
  rgSortBy.Items.Add(_l('#00146^eRadical'));
  rgSortBy.Items.Add(_l('#00147^eStroke count'));
  rgSortBy.Items.Add(_l('#00148^eFrequency'));
  rgSortBy.Items.Add(_l('#00149^eRandom'));
  rgSortBy.Items.Add(_l('#00877^eUnsorted'));
 { There could be additional sorting orders for Japanese, but they're somehow
  disabled in Wakan 1.67+ }
  rgSortBy.ItemIndex := FSetSortBy; //0 by default
  FSetSortBy := 0; //no reuse
end;

{ Changes the set of characters selected in the Kanji grid and in all related
 tool windows. }
procedure TfKanji.SetFocusedCharsLow(const Value: FString);
begin
  if FFocusedChars=Value then exit;
  FFocusedChars := Value;

  fKanjiDetails.SetCharDetails(Value);

 { Pass selected chars to all tool windows }
  if flength(Value)=1 then begin //single char
    fKanjiCompounds.SetCharCompounds(fgetch(Value, 1));
    AnnotShowMedia(Value,'');
  end else begin //multiple or none
    fKanjiCompounds.Clear;
  end;
end;

//Places focus on the given characters, if any are visible, or resets it
procedure TfKanji.SetFocusedChars(const Value: FString);
begin
  if FFocusedChars=Value then exit;
  SetFocusedCharsLow(Value);
 //TODO: exit if the grid is not yet filled (we'll do the last part later)
  if not KanjiGridSetSelection(Value) then begin
 {$IFDEF AUTODEFOCUS}
    Self.KanjiGridSelectionChanged; //as if the user did that
 {$ENDIF}
  end;
end;

//Places focus on the given characters, if any are visible, or does not change it.
//While SetFocusedChars MIGHT be implemented to later focus more of these chars
//as filters are relaxed, MaybeSetFocusedChars makes the decision NOW.
//If it decides not to change focus, it's over.
function TfKanji.MaybeSetFocusedChars(const Value: FString): boolean;
var i: integer;
begin
  if FFocusedChars=Value then begin
    Result := true;
    exit;
  end;
  Result := false;
  for i := 1 to flength(Value) do
    if Self.FindCharacterListIndex(fgetch(Value, i)) >= 0 then begin
      Result := true;
      break;
    end;
  if Result then
    SetFocusedChars(Value);
end;



{ Reloading }

//split value string into string list. values are separated by comma or ;
//if numeric, values can have format n1-n2 (edtPinYin.g. 3-5 is expanded into 3,4,5)
procedure MakeList(tx:string;number:boolean;sl:TStringList);
var fnd:boolean;
    curtx,txleft:string;
    min,max:integer;
    i:integer;
begin
  if sl.Count>0 then exit;
  txleft:=tx;
  sl.Sorted:=true;
  fnd:=false;
  while (not fnd) and (txleft<>'') do
  begin
    curtx:='';
    while (length(txleft)>0) and (txleft[1]<>';') and (txleft[1]<>',') do
    begin
      curtx:=curtx+txleft[1];
      delete(txleft,1,1);
    end;
    if (length(txleft)>0) and ((txleft[1]=';') or (txleft[1]=',')) then delete(txleft,1,1);
    if not number then
      sl.Add(curtx) {Previously this added uppercase(curtx), but I removed that
        because that breaks searching for Definitions (they're not uppercase).
        I suppose uppercase was added when everything passed here was already
        in hex, which is not the case now.
        When the contents is in hex, it has to be uppercased just before
        searching for it. }
    else
      if pos('-',curtx)=0 then
        sl.Add(curtx)
      else
      begin
        if not TryStrToInt(copy(curtx,1,pos('-',curtx)-1), min) then
          min := 1;
        delete(curtx,1,pos('-',curtx));
        if not TryStrToInt(curtx, max) then
          max := min;
        for i:=min to max do sl.Add(inttostr(i));
      end;
  end;
  if sl.Count=0 then sl.Add('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$');
end;

function IsWordSeparator(const ch: char): boolean; inline;
begin
  Result := ch=' ';
end;

//Similar to pos(), but only finds word matches (space or beginstr before,
//space or endstr after the match)
function wordPos(const substr, str: string; offset: integer = 1): integer;
begin
  Result := offset-1;
  repeat
    Inc(Result);
    if Length(str)-Result+1 < Length(substr) then begin
      Result := 0;
      exit;
    end;

    Result := pos(substr, str, Result);
    if Result <= 0 then exit;
  until ((Result=1) or IsWordSeparator(str[Result-1]))
    and ((Result+Length(substr)>=Length(str)) or IsWordSeparator(str[Result+Length(substr)]));
end;

//Takes a property type Typ, a text Tx and a match type Ftype and populates /
//updates Flt with characters that have at least one matching property.
procedure TfKanji.ReadFilter(flt: TStringList; const tx: string; typ: integer;
  ftype: TFilterType);
var CCharProp: TCharPropertyCursor;
  sl:TStringList;
  s_fltval:string;
  s_val:string;
  i:integer;
  dot:integer;
  propType: PCharPropType;
  match: boolean;
  s_sp: string;
  s_dot: string;
begin
  sl:=TStringList.Create;
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
   //Convert filter value into a list of exact values to match
    MakeList(tx, {IsNumber=}ftype=ftNumber, sl);

    propType := FindCharPropType(typ);
    s_sp := ' ';
    s_dot := '.';

    for i := 0 to sl.Count - 1 do
    begin
      s_fltval := uppercase(sl[i]); //Locate is case-insensitive anyway

      if ftype in [ftAnywhere, ftAnyword] then
        CCharProp.First //have to iterate through all records
      else
        CCharProp.LocateRawValue(s_fltval);

      while not CCharProp.EOF do
      begin
        s_val := uppercase(CCharProp.RawValue);

        case ftype of
          ftExact,
          ftNumber:      if (s_fltval <> s_val) then break;
          ftStart,
          ftExactOrBase: if pos(s_fltval, s_val)<>1 then break;
          ftAnywhere,
          ftAnyWord:     begin end; //never break
        end;

        match := (CCharProp.Int(TCharProp.fTypeId)=typ);
        case propType.dataType of
          'R': begin //Radical format: radical['][.stroke_count]
            match := match and (
                 (pos(s_fltval+s_dot, s_val)=1)
              or (pos(s_fltval+s_dot, s_val)=1)
              or (s_fltval=s_val)
            );
          end;
        else
          case ftype of
            ftExact,
            ftNumber:  match := match and (s_fltval = s_val);
            ftStart:   match := match and (pos(s_fltval, s_val) = 1);
            ftExactOrBase: begin
              dot := CCharProp.Int(TCharProp.fReadDot);
              if dot <= 0 then
                match := match and (s_fltval = s_val)
              else
                match := match and (s_fltval = copy(s_val, 1, dot-1));
            end;
            ftAnywhere:
              match := match and (pos(s_fltval, s_val) > 0);
            ftAnyWord:
              match := match and (wordPos(s_fltval, s_val) > 0);
          end;
        end;

        if match and (flt.IndexOf(CCharProp.Kanji)<0) then
          flt.Add(CCharProp.Kanji);
        CCharProp.Next;
      end;
    end;

  finally
    FreeAndNil(CCharProp);
    FreeAndNil(sl);
  end;
end;

//Text properties can be stored as katakana, hiragana and romaji; user can type
//either, so try all conversions and merge results.
//Don't use for text fields where there's no ambiguity in input / data format.
procedure TfKanji.ReadAnyTextFilter(flt: TStringList; const tx: string;
  typ: integer; ftype: TFilterType; lang: TLanguageSet);
begin
  ReadFilter(flt, tx, typ, ftype); //search as is

  if AnsiChar('j') in lang then begin
    //also converts raw katakana to hiragana (since RtoK keeps invalid chars):
    ReadFilter(flt, ToHiragana(RomajiToKana('H'+tx,'j',[])), typ, ftype); //maybe that was romaji?
    ReadFilter(flt, ToKatakana(RomajiToKana('K'+tx,'j',[])), typ, ftype);
  end;

  if AnsiChar('c') in lang then begin
    ReadFilter(flt, RomajiToKana(tx,'c',[]), typ, ftype);
  end;
end;


{ Converts RadicalIndexes to the standard filter string acceptable by ReadFilter }
function RadicalIndexesToFilter(const AIndexes: TRadicalIndexes): string;
var i: integer;
begin
  if Length(AIndexes)<1 then begin
    Result := '';
    exit;
  end;

  Result := IntToStr(AIndexes[0]);
  for i := 1 to Length(AIndexes)-1 do
    Result := Result + ';' + IntToStr(AIndexes[i]);
end;

procedure TfKanji.ReadRaineFilter(fltradical:TStringList;const ARadicals:string);
var ARadIndexes: TRadicalIndexes;
  sltemp:TStringList;
  s2:string;
  rrind:integer;
  rrus:boolean;
  p:PWideChar;
  i,j:integer;
  rchars:FString;
begin
  ARadIndexes := RadicalsToIndexes(stRaine, ARadicals);
  sltemp:=TStringList.Create;
  sltemp.Sorted:=true;
  fltradical.Sorted:=true;
  rrus:=false;
  for j := 0 to Length(ARadIndexes)-1 do
  begin
    rrind:=ARadIndexes[j];
    rchars := RaineRadicals.GetContainingChars(rrind);
    p:=PWideChar(rchars);
    for i:=1 to Length(rchars) do begin
      s2:=fstr(p^);
      p:=p+1;
      if not rrus or (sltemp.IndexOf(s2)<>-1) then fltradical.Add(s2);
    end;
    if j<Length(ARadIndexes)-1 then
    begin
      sltemp.Assign(fltradical);
      fltradical.Clear;
      rrus:=true;
    end;
  end;
  sltemp.Free;
end;

//Extracts all kanji characters from the string and creates a filter set as used
//below.
procedure ExtractKanjiToFilter(const AText: string; AFilter: TStringList);
var i: integer;
begin
  for i := 1 to Length(AText) do
    if (EvalChar(AText[i])=EC_IDG_CHAR) and (AFilter.IndexOf(AText[i])<0) then
      AFilter.Add(AText[i]);
end;

//filter kanji and show them in the grid
procedure TfKanji.Reload;
var
  fltLookup,
  fltClip,
  fltRadical,
  fltStrokeCount,
  fltJlpt,
  fltJouyou: TStringList;

    accept:boolean;
    i,j,grs:integer;
    s1,s2,s3:string;
    x:integer;

    clipsort:boolean;
    clipind:integer;

  categories: array of integer; //of checked category indexes
  ftype: TFilterType;
  kclass: char;
  prop: PCharPropType;

  procedure CopyCategories; //they're UNIMAGINABLY slow if used as is
  var i: integer;
  begin
    SetLength(categories, 0);
    for i:=0 to Self.lbCategories.Items.Count-1 do
      if Self.lbCategories.Checked[i] then
    begin
      SetLength(categories, Length(categories)+1);
      categories[Length(categories)-1] := GetCatIdx(Self.lbCategories, i);
    end;
  end;

  function CheckCategories: boolean;
  var i: integer;
  begin
    if Length(categories)<=0 then begin
      Result := true;
      exit;
    end;

   { Filter mode: 0=OR, 1=AND }
    Result := (Self.cbOrAnd.ItemIndex=1); //default result: true if ANDs, false if ORs
    for i := 0 to Length(categories) - 1 do
      if IsKnown(categories[i], TChar.FCh(TChar.fUnicode)) then begin
        if Self.cbOrAnd.ItemIndex in [0, 2] then begin Result:=true; break; end;
      end else begin
        if Self.cbOrAnd.ItemIndex = 1 then Result:=false;
      end;
    if Self.cbOrAnd.ItemIndex=2 then Result:=not Result;
  end;

begin
  if not Visible then exit;
  if Self=nil then exit;

  fltLookup := nil;
  fltClip := nil;
  fltRadical := nil;
  fltStrokeCount := nil;
  fltJlpt := nil;
  fltJouyou := nil;

  Screen.Cursor:=crHourGlass;

 { Prepare lists of all possible characters matching each criteria / all possible
  values for each criteria }

  CopyCategories;

  if Self.aInClipboard.Checked then begin
    fltClip := TStringList.Create;
    ExtractKanjiToFilter(Clipboard.Text, fltClip);
  end;

  if edtLookup.Text<>'' then begin
    fltLookup := TStringList.Create;
    fltLookup.Sorted:=true;

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_CHARS] then
      ExtractKanjiToFilter(edtLookup.Text, fltLookup);

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_DEFINITION] then
      if curLang='c' then
        ReadFilter(fltLookup, edtLookup.text, ptChineseDefinition, ftAnyWord) //Chinese definition
      else
        ReadFilter(fltLookup, edtLookup.text, ptJapaneseDefinition, ftAnyWord); //Japanese definition

   //ON and KUN
    if fSettings.cbYomiIgnoreOkurigana.Checked then
      ftype := ftExactOrBase
    else
      ftype := ftExact;

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_ON]  then
      ReadAnyTextFilter(fltLookup, edtLookup.Text, ptOnReading, ftype, ['j']);

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_KUN] then
      ReadAnyTextFilter(fltLookup, edtLookup.Text, ptKunReading, ftype, ['j']);

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_PINYIN] then begin
      ReadAnyTextFilter(fltLookup, edtLookup.Text, ptMandarinReading, ftStart, ['c']); //Mandarin
      ReadAnyTextFilter(fltLookup, edtLookup.Text, ptCantoneseReading, ftStart, ['c']); //Canton
    end;

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_SKIP] then
      ReadFilter(fltLookup, edtLookup.Text, ptSKIP, ftStart);

   //Generic types
    if GetLookupTypeIndex > 0 then
      prop := FindCharPropType(GetLookupTypeIndex())
    else
      prop := nil;
    if prop <> nil then begin
      if prop.dataType = 'N' then
        ReadFilter(fltLookup, edtLookup.Text, prop.id, ftNumber)
      else
        ReadAnyTextFilter(fltLookup, edtLookup.Text, prop.id, ftStart, ['c', 'j'])
    end;

  end;

 { Raine filters multi-selection with AND (only the characters with all the chosen parts are shown),
  Classical with OR (characters which match at least one radical are shown).
  This is because a character has only one Classical Radical so AND is pointless. }
  if sbRadicals.Down and (Self.curRadChars<>'') then begin
    fltRadical := TStringList.Create;
    fltRadical.Sorted := true;
    case Self.curRadSearchType of
      stClassic: ReadFilter(fltRadical,
        RadicalIndexesToFilter(RadicalsToIndexes(stClassic,Self.CurRadChars)),
        fSettings.GetPreferredRadicalType, ftNumber);
      stRaine: ReadRaineFilter(fltRadical,Self.CurRadChars);
    end;
  end;

  if sbJlpt.Down and (Self.edtJlpt.Text<>'') and (Self.edtJlpt.Text<>'0') then begin
    fltJlpt := TStringList.Create;
    fltJlpt.Sorted := true;
    ReadFilter(fltJlpt, Self.edtJlpt.Text, ptJLPTLevel, ftNumber);
  end;

  if sbJouyou.Down and (Self.edtJouyou.Text<>'') and (Self.edtJouyou.Text<>'0') then begin
    fltJouyou := TStringList.Create;
    MakeList(Self.edtJouyou.Text, true, fltJouyou);
  end;

  if sbStrokeCount.Down and (Self.edtStrokeCount.Text<>'') then begin
    fltStrokeCount := TStringList.Create;
    MakeList(Self.edtStrokeCount.Text, true, fltStrokeCount);
  end;


 { Go through characters (either all characters or characters in clipboard),
  leaving only those that are in all lists }

  DrawGrid1.Perform(WM_SETREDRAW, 0, 0); //disable redraw
  try
    grs := GetCellSize();
    if curlang='j' then
      case Self.rgSortBy.ItemIndex of
        0,3,4:TChar.SetOrder('JpUnicode_Ind');
        1:TChar.SetOrder('JpStrokeCount_Ind');
        2:TChar.SetOrder('JpFrequency_Ind');
      end
    else
      case Self.rgSortBy.ItemIndex of
        0,3,4:TChar.SetOrder('ChUnicode_Ind');
        1:TChar.SetOrder('ChStrokeCount_Ind');
        2:TChar.SetOrder('ChFrequency_Ind');
      end;
    ki.Clear;

    clipsort:=(Self.aInClipboard.Checked) and (Self.rgSortBy.ItemIndex=4);
    clipind:=0;
    while ((not clipsort) and ((not TChar.EOF) and ((curlang='c') or (TChar.Int(TChar.fChinese)=0)))) or
          ((clipsort) and (clipind<fltclip.Count)) do
    begin
      accept := true;
      if clipsort then accept := TChar.Locate('Unicode', fltClip[clipind]);

      //Ignore traditional or simplified variants if configured
      if accept and (curlang='c') and (fSettings.RadioGroup5.ItemIndex=0) and (TChar.Str(TChar.fType)='S') then accept:=false;
      if accept and (curlang='c') and (fSettings.RadioGroup5.ItemIndex=1) and (TChar.Str(TChar.fType)='T') then accept:=false;

      //Only common characters
      if accept and Self.aOnlyCommon.Checked then begin
        if (curlang='c') and (TChar.Int(TChar.fChFrequency)>=255) then accept:=false;
        if (curlang<>'c') and (TChar.Int(TChar.fJouyouGrade)>=10) then accept:=false;
      end;

      if accept and (not clipsort) and Self.aInClipboard.Checked and (fltClip.IndexOf(TChar.Str(TChar.fUnicode))<0) then accept:=false;

      //Lookup text
      if accept and (fltLookup <> nil) and (fltLookup.IndexOf(TChar.Str(TChar.fUnicode))<0) then accept := false;

      //Radicals
      if accept and (fltradical <> nil) then
        case Self.curRadSearchType of
          stClassic: if fltRadical.IndexOf(TChar.Str(TChar.fUnicode))=-1 then accept:=false;
          stRaine: if fltRadical.IndexOf(TChar.Str(TChar.fUnicode))=-1 then accept:=false;
        end;

      //Stroke count
      if accept and (fltStrokeCount <> nil) then begin
        if (curlang='c') and (fltStrokeCount.IndexOf(TChar.Str(TChar.fChStrokeCount))<0) then accept := false;
        if (curlang<>'c') and (fltStrokeCount.IndexOf(TChar.Str(TChar.fJpStrokeCount))<0) then accept := false;
      end;

      if accept then accept := CheckCategories;
      if accept and (fltJouyou <> nil) and (fltJouyou.IndexOf(TChar.Str(TChar.fJouyouGrade))<0) then accept := false;
      if accept and (fltJlpt <> nil) and (fltJlpt.IndexOf(TChar.Str(TChar.fUnicode))<0) then accept:=false;

      if accept then begin
        kclass := GetCharClass(TChar.Str(TChar.fUnicode));
        if ((curlang<>'c') and (Self.rgSortBy.ItemIndex=3))
        or ((curlang='c') and (Self.rgSortBy.ItemIndex=3)) then
          ki.Insert(random(ki.Count),kclass+TChar.Str(TChar.fUnicode))
        else
          ki.Add(kclass+TChar.Str(TChar.fUnicode));
      end;

      if clipsort then inc(clipind) else TChar.Next;
    end;

    FreeAndNil(fltRadical);
    FreeAndNil(fltJlpt);
    FreeAndNil(fltJouyou);
    FreeAndNil(fltStrokeCount);
    FreeAndNil(fltClip);
    FreeAndNil(fltLookup);

    DrawGrid1.ColCount:=GetExpectedColCount();
    x:=DrawGrid1.ColCount;
    if ki.Count=0 then DrawGrid1.RowCount:=1 else
      DrawGrid1.RowCount:=((ki.Count-1) div x)+1;
    DrawGrid1.DefaultRowHeight:=grs;
    DrawGrid1.DefaultColWidth:=grs;
    testkanji:='';
    for i:=0 to 14 do if i<ki.Count then testkanji:=testkanji+copy(ki[i],2,4);
    TChar.SetOrder('ChUnicode_Ind');
    if not KanjiGridSetSelection(FocusedChars) then begin //previous kanji not in list
   {$IFDEF AUTODEFOCUS}
      Self.KanjiGridSelectionChanged; //as if the user did that
   {$ENDIF}
    end;
  finally
    DrawGrid1.Perform(WM_SETREDRAW, 1, 0); //enable redraw
   { WM_SETREDRAW(1) implicitly makes DrawGrid visible without telling Delphi,
    so we'd have to do Show(); Hide(); to re-hide it.
    But we'd still have problems because sometimes window is implicitly shown
    again on load.
    This approach works: }
    if ki.Count>0 then
      ShowWindow(DrawGrid1.Handle,1)
    else
      ShowWindow(DrawGrid1.Handle,0);
  end;

  DrawGrid1.Invalidate;

  if curlang='c' then begin
    case fSettings.RadioGroup5.ItemIndex of
      0: Self.Caption := _l('#01204^%d traditional characters', [ki.Count]);
      1: Self.Caption := _l('#01205^%d simplified characters', [ki.Count]);
    else Self.Caption := _l('#01203^%d characters', [ki.Count]);
    end;
  end else
    Self.Caption := _l('#01203^%d characters', [ki.Count]);

  if curlang='c' then begin
    case fSettings.RadioGroup5.ItemIndex of
      0: lblFoundChars.Caption := _l('#00958^Found traditional characters (%d):', [ki.Count]);
      1: lblFoundChars.Caption := _l('#00959^Found simplified characters (%d):', [ki.Count]);
    else lblFoundChars.Caption := _l('#00960^Found characters (%d):', [ki.Count]);
    end;
  end else
    lblFoundChars.Caption := _l('#00961^Found kanji (%d):', [ki.Count]);

  Screen.Cursor:=crDefault;
end;

procedure TfKanji.InvalidateList;
begin
{$IFDEF INVALIDATE_WITH_DELAY}
  UpdateTimer.Interval:=1000;
  UpdateTimer.Enabled:=true;
{$ELSE}
  Reload;
{$ENDIF}
end;

procedure TfKanji.UpdateTimerTimer(Sender: TObject);
begin
  UpdateTimer.Enabled:=false;
  Reload;
end;

//Returns the index of the given character in a currently filtered character list, or -1
function TfKanji.FindCharacterListIndex(const ch: FChar): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to ki.Count-1 do
    if ch = copy(ki[i],2,4) then begin
      Result := i;
      break;
    end;
end;

//True if a given character has passed all filters and is present in the grid
function TfKanji.IsCharacterVisible(const ch: FChar): boolean;
begin
  Result := (FindCharacterListIndex(ch)>=0);
end;



{ Saving to a file }

//Saves active set of characters to a file
procedure TfKanji.SaveChars;
var i:integer;
  conv: TStreamEncoder;
begin
  if not SaveDialog1.Execute then
    exit;

  conv := CreateTextFile(SaveDialog1.FileName,Conv_ChooseType(false,TUnicodeEncoding));
  try
    for i:=0 to ki.Count-1 do
      conv.Write(copy(ki[i],2,4));
  finally
    FreeAndNil(conv);
  end;
end;

procedure TfKanji.aSaveToFileExecute(Sender: TObject);
begin
  Self.SaveChars;
end;



{ Printing }

type
  TKanjiCardPainter = class(TPrintPainter)
  const
    BaseFontSize = 12; //initial measurement made in this size
  protected
    FRowsOnPage: integer;
    FAdditionalWidth: integer;
    FCharSize: integer;
    FFullCompounds: integer;
    FBaseCardWidth: integer;
    FBaseCardHeight: integer;
    procedure Reinit;
    function NewKanjiCard(const AChar: string; AFontSize: integer): TKanjiCard;
    procedure GetCardSizes(Width, Height: integer; out AFontSize,
      ACardWidth, ACardHeight: integer);
  public
    constructor Create;
    function GetPageNum(Canvas: TCanvas; Width, Height: integer): integer;
      override;
    procedure DrawPage(Canvas: TCanvas; PageNum: integer; Width, Height: integer;
      OrigWidth, OrigHeight: integer); override;
    procedure Configure; override;
  end;

constructor TKanjiCardPainter.Create;
begin
  inherited Create;
  Reinit;
end;

//Reads paint setup from settings
procedure TKanjiCardPainter.Reinit;
var card: TKanjiCard;
begin
  FRowsOnPage := StrToInt(fSettings.edtKanjiCardRowsOnPage.Text);
  FAdditionalWidth := StrToInt(fSettings.edtKanjiCardAdditionalWidth.text);
  FCharSize := StrToInt(fSettings.edtKanjiCardCharSize.Text);
  FFullCompounds := StrToInt(fSettings.edtKanjiCardFullCompounds.Text);
 //Try and measure the card
  card := NewKanjiCard('u', BaseFontSize);
  card.Measure(FBaseCardWidth, FBaseCardHeight, {abstract=}true);
end;

function TKanjiCardPainter.NewKanjiCard(const AChar: string; AFontSize: integer): TKanjiCard;
begin
  Result := TKanjiCard.Create(AChar);
  Result.Flags := [];
  if fSettings.cbKanjiCardPrintStrokeCount.Checked then Result.Flags := Result.Flags + [koPrintStrokeCount];
  if fSettings.cbKanjiCardPrintReadings.Checked then Result.Flags := Result.Flags + [koPrintOuterLines];
  if fSettings.cbKanjiCardPrintRadical.Checked then Result.Flags := Result.Flags + [koPrintRadical];
  if fSettings.cbKanjiCardPrintAlternate.Checked then Result.Flags := Result.Flags + [koPrintAlternateForm];
  if fSettings.cbKanjiCardPrintInnerLines.Checked then Result.Flags := Result.Flags + [koPrintInnerLines];
  if fSettings.cbKanjiCardPrintCompounds.Checked then Result.Flags := Result.Flags + [koPrintVocabularyCompounds];
  if fSettings.cbKanjiCardPrintOuterLines.Checked then Result.Flags := Result.Flags + [koPrintReadings];
  if fSettings.cbKanjiCardPrintDefinition.Checked then Result.Flags := Result.Flags + [koPrintDefinition];
  if fSettings.cbKanjiCardPrintStrokeOrder.Checked then Result.Flags := Result.Flags + [koPrintStrokeOrder];
  if fSettings.cbKanjiCardPrintFullComp.Checked then Result.Flags := Result.Flags + [koPrintFullCompounds];
  if fSettings.cbKanjiCardSortFrequency.Checked then Result.Flags := Result.Flags + [koSortCompoundsByFrequency];
  Result.FontSize := AFontSize;
  Result.MarginSize := AFontSize div 2;
  Result.MainCharSize := AFontSize * (FCharSize-1);
  Result.MaxFullComp := FFullCompounds;
  Result.SuggestedAdditionalWidth := FAdditionalWidth;
  Result.CalFont := fSettings.edtKanjiCardFont.Text;
end;

//Has to be already Reinit()ialized
procedure TKanjiCardPainter.GetCardSizes(Width, Height: integer; out AFontSize,
  ACardWidth, ACardHeight: integer);
var LineHeight, LineCount:integer;
  HScale: double;
begin
  GetPrintLine(Width, Height, Width, Height, FRowsOnPage, LineHeight, LineCount);
  HScale := LineHeight/FBaseCardHeight;
  AFontSize := Trunc(BaseFontSize*HScale);
  ACardWidth := Trunc(FBaseCardWidth*HScale);
  ACardHeight := Trunc(FBaseCardHeight*HScale);
end;

function TKanjiCardPainter.GetPageNum(Canvas: TCanvas; Width, Height:integer): integer;
var AFontSize, ACardWidth, ACardHeight: integer;
  HCount, VCount: integer;
begin
  GetCardSizes(Width, Height, AFontSize, ACardWidth, ACardHeight);
  HCount := Width div ACardWidth;
  VCount := Height div ACardHeight;
  Result := (ki.Count-1) div (HCount*VCount) + 1;
end;

procedure TKanjiCardPainter.DrawPage(canvas:TCanvas; pagenum:integer;
  width,height,origwidth,origheight:integer);
var AFontSize, ACardWidth, ACardHeight: integer;
  HCount, VCount: integer;
  i:integer;
  u:string;
  x,xp,y,yp:integer;
  card: TKanjiCard;
begin
  GetCardSizes(Width, Height, AFontSize, ACardWidth, ACardHeight);
  HCount := Width div ACardWidth;
  VCount := Height div ACardHeight;

  for i:=0 to HCount*VCount-1 do
    if (pagenum-1)*VCount*HCount+i<ki.Count then begin
      u:=ki[(pagenum-1)*VCount*HCount+i];
      delete(u,1,1);
      if fSettings.cbKanjiCardPrintVertical.Checked then begin
        yp:=i mod HCount;
        xp:=VCount-(i div HCount)-1;
      end
      else begin
        xp:=i mod VCount;
        yp:=i div VCount;
      end;
      y := yp*ACardHeight;
      x := xp*ACardWidth;
      card := NewKanjiCard(u, AFontSize);
      try
        card.Paint(Canvas, TRect.Create(x, y, x+ACardWidth, y+ACardHeight));
      finally
        FreeAndNil(card);
      end;
    end;
end;

procedure TKanjiCardPainter.Configure();
begin
  fSettings.pcPages.ActivePage:=fSettings.tsCharacterCardPrinting;
  fSettings.ShowModal;
  Reinit;
end;

procedure TfKanji.aPrintExecute(Sender: TObject);
var painter: TKanjiCardPainter;
begin
  painter := TKanjiCardPainter.Create;
  try
    PrintPreview(painter,_l('#00134^eKanji cards'));
  finally
    FreeAndNil(painter);
  end;
end;



{ Painting the list }

{ Called when a kanji selection changes }
procedure TfKanji.KanjiGridSelectionChanged;
begin
 { Store selected chars and pass to all child windows, but do not apply selection
  again }
  SetFocusedCharsLow(KanjiGridGetSelection);
end;

function TfKanji.KanjiGridGetSelection: FString;
var sel: TGridRect;
  i, j: integer;
  char:FString;
begin
  Result := '';
  sel := DrawGrid1.Selection;
  for i := sel.Top to sel.Bottom do
    for j := sel.Left to sel.Right do begin
      if DrawGrid1.ColCount*i+j>=ki.Count then //selection can cover unused cells
        continue;
      char := ki[DrawGrid1.ColCount*i+j];
      Result := Result + copy(char,2,Length(char)-1); //delete first char
    end;
end;

{ Highlights the specified characters in the grid. There are limitations on what
 we can highlight at this time, so returns false if the highlight differs from
 what was requested.
 Basic function. Does not call any additional handlers. }
function TfKanji.KanjiGridSetSelection(const chars: FString): boolean;
var i, j, cols: integer;
  mr: TGridRect;
begin
  if flength(chars)<>1 then begin
    Result := false;
    exit;
  end;

 //Find first matching char
  Result := false;
  mr.Left := 0;
  mr.Top := 0;
  mr.Right := 0;
  mr.Bottom := 0;
  cols := DrawGrid1.ColCount;
  for j := 1 to flength(chars) do begin
    i := Self.FindCharacterListIndex(fgetch(chars, j));
    if i>=0 then begin
        mr.Left:=i mod cols;
        mr.Top:=i div cols;
        mr.Right:=i mod cols;
        mr.Bottom:=i div cols;
        Result:=true;
        break;
    end;
  end;

 { If nothing was highlighted, we must highlight something so highlight first
  element }
  if Result=false then begin
    mr.Left := 0;
    mr.Top := 0;
    mr.Right := 0;
    mr.Bottom := 0;
  end;

 { At this time we cannot highlight more than one char programmatically }
  if flength(chars)>1 then
    Result := false;

 //Apply whatever highlight we generated
  DrawGrid1.Selection:=mr;
  if (mr.Top>1) and (DrawGrid1.RowCount>DrawGrid1.VisibleRowCount) then
    DrawGrid1.TopRow:=mr.Top-1
  else
    DrawGrid1.TopRow:=0;
end;

procedure TfKanji.DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
 //Some cells are off limits
  CanSelect := not (DrawGrid1.ColCount*ARow+ACol>=ki.Count);
end;

procedure TfKanji.DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  KanjiGridSelectionChanged;
end;

procedure TfKanji.DrawGrid1Click(Sender: TObject);
begin
 { We need to react to kanji selection change on mouse wheel,
  but MouseWheelDown/Up happen before the selection is updated.
  Click() seems to happen at any time focus changes so we do it here,
  but if this fails us we'll switch to reacting from SelectCell() I guess and
  manually constructing actual selection (because it's not saved yet there too) }
  KanjiGridSelectionChanged;
end;

procedure TfKanji.DrawGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KanjiGridSelectionChanged;
end;

procedure TfKanji.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var w:widechar;
  kix:FString;
  kig:string;
  jouyou_val:string;
  r_copy: TRect;
  fontface: string;
  fontsize: integer;
  foreCol, backCol: TColor;
begin
  if (ARow*DrawGrid1.ColCount+ACol>=ki.Count) then
  begin
    DrawGrid1.Canvas.Pen.Color:=clWindow;
    DrawGrid1.Canvas.Brush.Color:=clWindow;
    DrawGrid1.Canvas.FillRect(Rect);
    exit;
  end;
  kix:=ki[DrawGrid1.ColCount*ARow+Acol];
  delete(kix,1,1);
 {$IFDEF UNICODE}
  w := kix[1];
 {$ELSE}
  w:=HexToUnicode(kix)[1];
 {$ENDIF}
  if gdSelected in State then begin
    DrawGrid1.Canvas.Brush.Color:=clHighlight;
    DrawGrid1.Canvas.Font.Color:=clHighlightText;
  end else begin
    DrawGrid1.Canvas.Brush.Color:=clWindow;
    DrawGrid1.Canvas.Font.Color:=clWindowText;
  end;

  if (not fSettings.cbNoCharColors.Checked) then
  begin
    backCol:=Col('Kanji_Back');
    TChar.Locate('Unicode',kix);
    kig:=ki[DrawGrid1.ColCount*ARow+Acol];
    case kig[1] of
      'K':foreCol:=Col('Kanji_Learned');
      'C':foreCol:=Col('Kanji_Common');
      'U':foreCol:=Col('Kanji_Rare');
      'N':foreCol:=Col('Kanji_Names');
    else foreCol := clBlack;
    end;

    if gdSelected in State then begin
      DrawGrid1.Canvas.Brush.Color := foreCol;
      DrawGrid1.Canvas.Font.Color := backCol;
    end else begin
      DrawGrid1.Canvas.Brush.Color := backCol;
      DrawGrid1.Canvas.Font.Color := foreCol;
    end;
  end;

  if AnnotationsSettingsPage.cbAnnotateWithColors.Checked and HaveAnnotations then
  begin
    Annot.SeekK(kix,'');
    jouyou_val:=Annot.GetOne('c');
    if jouyou_val<>'' then try
      DrawGrid1.Canvas.Font.Color:=strtoint('0x'+copy(jouyou_val,5,2)+copy(jouyou_val,3,2)+copy(jouyou_val,1,2));
    except end;
  end;
  DrawGrid1.Canvas.FillRect(Rect);
  fontface := GetCellFontName();
  fontsize := GetCellFontSize();
  DrawGrid1.Canvas.Font.Style:=[];

  r_copy := rect;
  r_copy.Left := r_copy.Left + 1;
  DrawUnicodeChar(DrawGrid1.Canvas, r_copy, fontsize, w, fontface);

  if fSettings.CheckBox1.Checked then
  begin
    TChar.Locate('Unicode',kix);
    DrawGrid1.Canvas.Font.Name:=FontEnglish;
    DrawGrid1.Canvas.Font.Height:=8+4*fSettings.rgKanjiGridSize.ItemIndex;
    DrawGrid1.Canvas.Font.Color:=clWindowText;
    if curlang='c' then
      DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TChar.fChStrokeCount))
    else
      DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TChar.fJpStrokeCount));
  end;
  fKanjiDetails.pbKanji.Invalidate;
  fKanjiDetails.pbRadical.Invalidate;
  fKanjiDetails.pbSimplified.Invalidate;
end;

procedure TfKanji.DrawGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var sel: TGridRect;
begin
  if (Key=Ord('A')) and (ssCtrl in Shift) then begin
    sel.Left := 0;
    sel.Top := 0;
    sel.Right := DrawGrid1.ColCount-1;
    sel.Bottom := DrawGrid1.RowCount-1;
    DrawGrid1.Selection := sel;
  end;
end;

procedure TfKanji.DrawGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=' ' then
    Clipboard.Text := Clipboard.Text+curkanji;
  if key=Chr(VK_RETURN) then
    if not fMenu.aKanjiDetails.Checked then
      fMenu.aKanjiDetails.Execute
    else
      if fKanjiDetails.Visible then
        fKanjiDetails.SetFocus;
  if key=Chr(VK_BACK) then begin
    if length(Clipboard.Text)>0 then
      Clipboard.Text := copy(Clipboard.Text, 1, Length(Clipboard.Text)-4);
  end;
 //Copy to clipboard on Ctrl-C
  if (Key=^C) and DrawGrid1.Visible then begin
    if fSettings.DefaultKanjiCopyFormatName='' then
      CopyAsText(
        GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
      )
    else
      CopyToClipboard(
        GetKanjiCopyFormatsDir+'\'+fSettings.DefaultKanjiCopyFormatName+'.xslt',
        GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
      );
    Key := #00;
  end;
end;

procedure TfKanji.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var p: TPoint;
  ACol, ARow: integer;
  r: TGridRect;
  CanSelect: boolean;
begin
//  if mbRight=Button then
//    ScreenTip.PopupImmediate(false);
// instead show popup menu

 //Right-click-select
  p := TDrawGrid(Sender).ScreenToClient(Mouse.CursorPos);
  TDrawGrid(Sender).MouseToCell(p.X, p.Y, ACol, ARow);
  if (ARow>=0) and (ACol>=0) then
    if (ARow<TDrawGrid(Sender).Selection.Top)
    or (ARow>TDrawGrid(Sender).Selection.Bottom)
    or (ACol<TDrawGrid(Sender).Selection.Left)
    or (ACol>TDrawGrid(Sender).Selection.Right) then begin
      r := TDrawGrid(Sender).Selection;
      CanSelect := true;
      if Assigned(TDrawGrid(Sender).OnSelectCell) then
        TDrawGrid(Sender).OnSelectCell(Sender, ACol, ARow, CanSelect);
      if not CanSelect then exit;
      r.Top := ARow;
      r.Bottom := ARow;
      r.Left := ACol;
      r.Right := ACol;
      TDrawGrid(Sender).Selection := r;
      KanjiGridSelectionChanged;
    end;
end;

procedure TfKanji.DrawGrid1DblClick(Sender: TObject);
begin
  if not fMenu.aKanjiDetails.Checked then fMenu.aKanjiDetails.Execute else if fKanjiDetails.Visible then fKanjiDetails.SetFocus;
end;

procedure TfKanji.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  IntTip.MouseMove(DrawGrid1,x,y,false);
end;


{ Search filters }

//Common event handler for various search filters' OnChange
procedure TfKanji.SearchFilterChanged(Sender: TObject);
begin
  Self.InvalidateList;
end;

procedure TfKanji.lbCategoriesClick(Sender: TObject);
var IsKnownLearned: boolean;
begin
  IsKnownLearned := GetSelCatIdx(lbCategories)=KnownLearned;
  miEditCategory.Enabled:=not IsKnownLearned;
  miDeleteCategory.Enabled:=not IsKnownLearned;
end;

procedure TfKanji.lbCategoriesClickCheck(Sender: TObject);
begin
  Self.InvalidateList;
end;

procedure TfKanji.lbCategoriesDblClick(Sender: TObject);
var i:integer;
begin
  if lbCategories.ItemIndex<>-1 then
  begin
    for i:=0 to lbCategories.Items.Count-1 do
      lbCategories.Checked[i]:=i=lbCategories.ItemIndex;
    Self.Reload;
  end;
end;

procedure TfKanji.lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var IsKnownLearned: boolean;
begin
  IsKnownLearned := GetCatIdx(lbCategories,index)=KnownLearned;
  if IsKnownLearned then
    lbCategories.Canvas.Font.Style:=[fsBold]
  else
    lbCategories.Canvas.Font.Style:=[];
  lbCategories.Canvas.TextOut(Rect.Left,Rect.Top,lbCategories.Items[index]);
end;

procedure TfKanji.miAddCategoryClick(Sender: TObject);
begin
  NewKanjiCategoryUI();
end;

procedure TfKanji.miDeleteCategoryClick(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  DeleteCategoryUI(GetSelCatIdx(lbCategories));
end;

procedure TfKanji.miEditCategoryClick(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  EditCategoryUI(GetSelCatIdx(lbCategories));
end;

procedure TfKanji.miUncheckAllCategoriesClick(Sender: TObject);
var i:integer;
begin
  for i:=0 to lbCategories.Items.Count-1 do lbCategories.Checked[i]:=false;
  Self.InvalidateList;
end;

procedure TfKanji.aOnlyCommonChecked(Sender: TObject);
begin
  sbOnlyCommon.Down := aOnlyCommon.Checked;
end;

procedure TfKanji.aOnlyCommonExecute(Sender: TObject);
begin
  SearchFilterChanged(Sender);
end;

procedure TfKanji.sbOnlyCommonClick(Sender: TObject);
begin
  aOnlyCommon.Checked := sbOnlyCommon.Down;
  SearchFilterChanged(Sender);
end;

procedure TfKanji.aInClipboardChecked(Sender: TObject);
begin
  sbInClipboard.Down := aInClipboard.Checked;
end;

procedure TfKanji.aInClipboardExecute(Sender: TObject);
begin
  SearchFilterChanged(Sender);
end;

procedure TfKanji.sbInClipboardClick(Sender: TObject);
begin
  aInClipboard.Checked := sbInClipboard.Down;
  SearchFilterChanged(Sender);
end;

procedure TfKanji.edtJouyouChange(Sender: TObject);
begin
  sbJouyou.Down := (edtJouyou.Text <> '0') and (edtJouyou.Text <> '');
  SearchFilterChanged(Sender);
end;

procedure TfKanji.edtJlptChange(Sender: TObject);
begin
  sbJlpt.Down := (edtJlpt.Text <> '0') and (edtJlpt.Text <> '');
  SearchFilterChanged(Sender);
end;

procedure TfKanji.sbRadicalsClick(Sender: TObject);
begin
  if sbRadicals.Down and (FCurRadChars = '') then begin
    sbRadicalsDropDownClick(Sender);
    if FCurRadChars = '' then
      sbRadicals.Down := false;
  end else
    SearchFilterChanged(Sender);
end;

procedure TfKanji.sbRadicalsDropDownClick(Sender: TObject);
var
  _radSearchType: TRadSearchType;
  _radChars: string;
  _radDown: boolean;
begin
 //save current search, it'll be broken by RadicalSelectionChanged
  _radSearchType := curRadSearchType;
  _radChars := FCurRadChars;
  _radDown := sbRadicals.Down;
 //bring up selection window
  fRadical.SetSelectedRadicals(curRadSearchType, FCurRadChars);
  fRadical.OnSelectionChanged := Self.RadicalSelectionChanged;
  if IsPositiveResult(fRadical.ShowModal) then begin
    _radSearchType := fRadical.SearchType;
    _radChars := fRadical.SelectedRadicals;
    _radDown := true;
  end;
  fRadical.OnSelectionChanged := nil;
 //apply new search, or re-apply old search
  curRadSearchType := _radSearchType;
  curRadChars := _radChars;
  sbRadicals.Down := _radDown;
 //SetCurRadicals will trigger filter update
  SearchFilterChanged(Sender);
end;

procedure PopupUnder(APopupControl, AAnchorControl: TWinControl);
var pt: TPoint;
begin
  pt := AAnchorControl.ClientToScreen(Point(0,0));
  pt.Y := pt.Y + AAnchorControl.Height;
  pt := APopupControl.Parent.ScreenToClient(pt);
  if pt.X + APopupControl.Width > APopupControl.Parent.Width then
    pt.X := APopupControl.Parent.Width - APopupControl.Width;
  if pt.X < 0 then
    pt.X := 0;
  APopupControl.Left := pt.X;
  APopupControl.Top := pt.Y;
  if APopupControl is TPopupPanel then
    TPopupPanel(APopupControl).Popup
  else begin
    APopupControl.Visible := true;
    APopupControl.SetFocus;
  end;
end;

procedure TfKanji.btnGroupsClick(Sender: TObject);
begin
  pnlGroups.Visible := btnGroups.Down;
  Self.Reload; //width changed, reflow
end;

procedure TfKanji.sbStrokeCountClick(Sender: TObject);
begin
  if sbStrokeCount.Down then begin
    if (edtStrokeCount.Text<>'') and (edtStrokeCount.Text<>'0') then
      SearchFilterChanged(Sender) //apply immediately
    else
      sbStrokeCountDropDownClick(Sender);
  end;
  SearchFilterChanged(Sender);
end;

procedure TfKanji.sbStrokeCountDropDownClick(Sender: TObject);
begin
  PopupUnder(pnlStrokeCount, sbStrokeCount);
  pnlStrokeCount.Width := sbStrokeCount.Width;
end;

procedure TfKanji.edtStrokeCountChange(Sender: TObject);
begin
  sbStrokeCount.Down := (edtStrokeCount.Text <> '0') and (edtStrokeCount.Text <> '');
  if (edtStrokeCount.Text='0') or (edtStrokeCount.Text='') then
    sbStrokeCount.Caption := _l('#00191^Stroke #')
  else
    sbStrokeCount.Caption := _l('#01206^Stroke #: %s', [edtStrokeCount.Text]);
  SearchFilterChanged(Sender);
end;

procedure TfKanji.pnlStrokeCountExit(Sender: TObject);
begin
  if (edtStrokeCount.Text='0') or (edtStrokeCount.Text='') then
    sbStrokeCount.Down := false;
end;


procedure TfKanji.btnKanjiDetailsClick(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfKanji.btnCompoundsClick(Sender: TObject);
var CanSelect:boolean;
begin
  fMenu.aKanjiCompounds.Execute;
  DrawGrid1SelectCell(Sender, DrawGrid1.Col, DrawGrid1.Row, CanSelect);
end;

procedure TfKanji.FilterByRadical(const radno: integer);
begin
  if radno=NoRadical then exit;
  Self.CurRadSearchType:=stClassic;
  Self.CurRadChars:=RadicalUnicode(radno);
  Self.InvalidateList;
end;

procedure TfKanji.aSearchExecute(Sender: TObject);
begin
 //Has to be non-empty or AutoCheck wont work
end;

procedure TfKanji.aRadicalExecute(Sender: TObject);
begin
  sbRadicals.Down := true;
  Self.sbRadicalsClick(Sender);
end;

procedure TfKanji.aSearchByTextExecute(Sender: TObject);
begin
  cbLookupType.ItemIndex := LOOKUP_ANY;
  Self.edtLookup.SetFocus;
end;

procedure TfKanji.aPinYinExecute(Sender: TObject);
begin
  cbLookupType.ItemIndex := LOOKUP_PINYIN;
  Self.edtLookup.SetFocus;
end;

procedure TfKanji.aYomiExecute(Sender: TObject);
begin
  cbLookupType.ItemIndex := LOOKUP_ON;
  Self.edtLookup.SetFocus;
end;

procedure TfKanji.aMeaningExecute(Sender: TObject);
begin
  cbLookupType.ItemIndex := LOOKUP_DEFINITION;
  Self.edtLookup.SetFocus;
end;


{ CopyFormats }

function GetKanjiCopyFormatsDir: string;
begin
  Result := ProgramDataDir+'\KanjiCopyFormats';
end;

//Retrieves a list of all avaialable KanjiCopyFormat filenames
function GetKanjiCopyFormats: TArray<string>;
var sr: TSearchRec;
  res: integer;
  fdir: string;
begin
  SetLength(Result, 0);
  fdir := GetKanjiCopyFormatsDir;
  res := FindFirst(fdir+'\*.xslt', faAnyFile and not faDirectory, sr);
  try
    while res=0 do begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := fdir + '\' + sr.Name;
      res := FindNext(sr);
    end;
  finally
    SysUtils.FindClose(sr);
  end;
end;

function KanjiInfoToXml(const AChar: string): string;
var props: TCharPropertyCursor;
begin
  Result := '<char>'+HtmlEscape(AChar)+'</char>';
  props := TCharPropertyCursor.Create(TCharProp);
  try
    props.Locate(AChar);
    while (not props.EOF) and (props.Kanji=AChar) do begin
      Result := Result + '<prop type="'+IntToStr(props.PropType.id)+'">'
        + HtmlEscape(props.AsString) + '</prop>';
      props.Next;
    end;
  finally
    FreeAndNil(props);
  end;
  Result := '<kanji>'+Result+'</kanji>';
end;

type
  TCopyFormatMenuItem = class(TMenuItem)
  public
    Filename: string;
  end;

procedure TfKanji.ReloadCopyFormats;
var item: TMenuItem;
  fname: string;
begin
  miCopyAs.Clear;

 //Rescan every time because the user could be adding files there
 //and expecting results
  for fname in GetKanjiCopyFormats() do begin
    item := TCopyFormatMenuItem.Create(Self);
    TCopyFormatMenuItem(item).Filename := ExtractFilename(fname);
    item.Caption := ChangeFileExt(TCopyFormatMenuItem(item).Filename, '');
    item.OnClick := CopyInFormatClick;
    if item.Caption=fSettings.DefaultKanjiCopyFormatName then
      item.Default := true;
    miCopyAs.Add(item);
  end;

  {$IFDEF DEBUG}
  item := TMenuItem.Create(Self);
  item.Caption := _l('XML...');
  item.OnClick := CopyAsXMLClick;
  miCopyAs.Add(item);
 {$ENDIF}

  item := TMenuItem.Create(Self);
  item.Caption := '-';
  miCopyAs.Add(item);

  item := TMenuItem.Create(Self);
  item.Caption := _l('#01103^eConfigure...');
  item.OnClick := ConfigureClick;
  miCopyAs.Add(item);
end;

procedure TfKanji.CopyInFormatClick(Sender: TObject);
begin
  CopyToClipboard(
    GetKanjiCopyFormatsDir+'\'+TCopyFormatMenuItem(Sender).Filename,
    GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
  );
end;

procedure TfKanji.CopyAsXMLClick(Sender: TObject);
begin
  CopyToClipboard(
    '',
    GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
  );
end;

procedure TfKanji.ConfigureClick(Sender: TObject);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsKanjiCopyFormats;
  fSettings.ShowModal;
end;

procedure TfKanji.CopyAsText(const AReplace: boolean);
begin
  if AReplace or (FocusedChars='') then
    Clipboard.Text := FocusedChars
  else
    Clipboard.Text := Clipboard.Text + FocusedChars;
end;

procedure TfKanji.CopyToClipboard(const AXsltFilename: string; const AReplace: boolean);
var chars, text: string;
  i: integer;
begin
  chars := FocusedChars;
  text := '';
  for i := 1 to Length(chars) do
    text := text + KanjiInfoToXml(chars[i]);
  if Length(chars)>1 then
    text := '<chars>' + text + '</chars>';
  text := XsltTransform(text, AXsltFilename);
  if AReplace or (FocusedChars='') then
    Clipboard.Text := text
  else
    Clipboard.Text := Clipboard.Text + text;
end;


{ Right-click menu }

procedure TfKanji.PopupMenuPopup(Sender: TObject);
var p: TPoint;
  ACol, ARow: integer;
  AClickOnData: boolean;
begin
  p := DrawGrid1.ScreenToClient(Mouse.CursorPos);
  DrawGrid1.MouseToCell(p.X, p.Y, ACol, ARow);
  AClickOnData := (ARow>=0) and (ACol>=0);

  miCopyAs.Visible := AClickOnData;
  miCharDetails.Visible := AClickOnData;
  miCharWords.Visible := AClickOnData;
  miCategories.Visible := AClickOnData;
  miLookupIn.Visible := AClickOnData;

  if miCopyAs.Visible then begin
    ReloadCopyFormats;
    if miCopyAs.Count <= 0 then
      miCopyAs.Visible := false;
  end;
  if miCategories.Visible then
    ReloadPopupCategories;
  if miLookupIn.Visible then begin
    ReloadReferenceLinks;
    if miLookupIn.Count <= 0 then
      miLookupIn.Visible := false;
  end;
end;

procedure TfKanji.miCopyClick(Sender: TObject);
begin
  CopyAsText(
    GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
  );
end;

procedure TfKanji.miCharWordsClick(Sender: TObject);
begin
  if Length(FocusedChars)<1 then exit;
  fMenu.DisplayMode := 6;
  fKanjiCompounds.SetCharCompounds(Self.FocusedChars[1]);
end;

procedure TfKanji.miCharDetailsClick(Sender: TObject);
begin
  if not fMenu.aKanjiDetails.Checked then
    fMenu.aKanjiDetails.Execute;
end;

procedure TfKanji.ReloadPopupCategories;
var item: TMenuItem;
  i: integer;
begin
  miCategories.Clear;

  for i := 0 to Length(KanjiCats)-1 do begin
    item := TMenuItem.Create(Self);
    item.Caption := KanjiCats[i].name;
    item.Checked := IsAllKnown(KanjiCats[i].idx, FocusedChars);
    item.AutoCheck := true;
    item.Tag := KanjiCats[i].idx;
   //There's no "partially checked" for menu items, so check only when all are checked
    item.OnClick := CategoryItemClick;
    miCategories.Add(item);
  end;

  item := TMenuItem.Create(Self);
  item.Caption := '-';
  miCategories.Add(item);

  item := TMenuItem.Create(Self);
  item.Caption := _l('#01112^New...');
  item.OnClick := CategoryNewItemClick;
  miCategories.Add(item);
end;

procedure TfKanji.CategoryItemClick(Sender: TObject);
var ch: char;
begin
  for ch in FocusedChars do
    SetKnown(TMenuItem(Sender).Tag, ch, TMenuItem(Sender).Checked);
  fMenu.ChangeUserData;
 //Do not reload the list here even if it's filtered by category, or it'll confuse the user
 //But maybe repaint (recolors learned/unlearned if it's not cached)
  DrawGrid1.Invalidate;
end;

procedure TfKanji.CategoryNewItemClick(Sender: TObject);
var catIndex: integer;
  ch: char;
begin
  if Length(FocusedChars)<1 then exit;
  catIndex := NewKanjiCategoryUI();
  if catIndex<0 then exit;
  for ch in FocusedChars do
    SetKnown(catIndex, ch, true);
  fMenu.ChangeUserData;
  DrawGrid1.Invalidate;
end;

procedure TfKanji.ClearReferenceLinks;
var i: integer;
begin
 //Submenu
  miLookUpIn.Clear;
 //Direct items
  for i := PopupMenu.Items.Count-1 downto 0 do
    if PopupMenu.Items[i] is TRefMenuItem then
      PopupMenu.Items[i].Destroy;
end;

procedure TfKanji.ReloadReferenceLinks;
var fname: string;
  ref: TRefLink;
  mi: TRefMenuItem;
  idx: integer;
begin
  ClearReferenceLinks;
  if Length(FocusedChars)<1 then exit;

  if fSettings.cbDictRefLinksInSubmenu.Checked then begin
    miBeforeLookupIn.Visible := false;
    miAfterLookupIn.Visible := false;
    miBeforeLookupIn.Caption := ''; //or they'd still be visible, being separators
    miAfterLookupIn.Caption := '';
    miLookUpIn.Visible := true;
    idx := -1; //unused
  end else begin
    miBeforeLookupIn.Caption := '-';
    miAfterLookupIn.Caption := '-';
    miBeforeLookupIn.Visible := true;
    miAfterLookupIn.Visible := true;
    miLookUpIn.Visible := false;
    idx := PopupMenu.Items.IndexOf(miAfterLookupIn);
  end;

  for fname in GetCharacterLinks() do begin
    ref := LoadLink(fname);
    if ref<>nil then
    try
      if ref.MatchesLang(curLang) then begin
        mi := TRefMenuItem.Create(Self, ref, FocusedChars[1]);
        if fSettings.cbDictRefLinksInSubmenu.Checked then
          miLookUpIn.Add(mi)
        else begin
          PopupMenu.Items.Insert(idx, mi);
          Inc(idx);
        end;
      end;
    finally
      FreeAndNil(ref);
    end;
  end;
end;


{ Content higlight }

function TfKanji.GetKanji(cx,cy:integer): string;
begin
  if (cy*DrawGrid1.ColCount+cx>=ki.Count) or (cx<0) or (cy<0) then
  begin
    result:='';
    exit;
  end;
  result:=ki[DrawGrid1.ColCount*cy+cx];
  delete(result,1,1);
end;

function KanjiGridHighlightContent(Control: TControl; DragStart, MousePos: TPoint): string;
var gc: TGridCoord;
begin
  if (fKanji=nil) or (Control<>fKanji.DrawGrid1) then begin
    Result := '';
    exit;
  end;
  gc := TCustomDrawGrid(Control).MouseCoord(MousePos.x,MousePos.y);
  Result := fKanji.GetKanji(gc.x,gc.y);
  SetSelectionHighlight(0,0,0,0,nil);
end;

initialization
  ki:=TStringList.Create;
  curkanji:=UH_NOCHAR;
  IntTip.RegisterHighlightHandler(TCustomDrawGrid, KanjiGridHighlightContent);

finalization
  ki.Free;

end.
