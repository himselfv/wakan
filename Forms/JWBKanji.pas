unit JWBKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Actions, ActnList, CheckAction,
  JWBStrings, IniFiles, Grids, DB, ShellAPI, WakanPaintbox, Menus, CheckLst,
  ImgList, JWBRadical, SimpleControls, RangeSpinEdit, JvExControls,
  JvArrowButton, SpeedBtn, Vcl.ToolWin;

//{$DEFINE INVALIDATE_WITH_DELAY}
// If set, InvalidateList() will use timer and not just update instanteneously.

//{$DEFINE DRAW_UNSUPPORTED_CHAR_CODES}
//  For chars which are not found in the specified font, draw char codes instead.
//  Neat but confusing.

{$DEFINE AUTODEFOCUS}
//  If the previously selected character is not available under the new filters,
//  automatically set focus to one of the available characters.

type
  TReadFilterFlag = (rfPartial, rfSpace, rfNumber, rfTakedot);
  TReadFilterFlags = set of TReadFilterFlag;

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
    aOnlyCommon: TAction;
    aInClipboard: TAction;
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
    procedure pbRadicalsPaint(Sender: TObject; Canvas: TCanvas);
    procedure miAddCategoryClick(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesClickCheck(Sender: TObject);
    procedure miDeleteCategoryClick(Sender: TObject);
    procedure miEditCategoryClick(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbCategoriesDblClick(Sender: TObject);
    procedure pnlGroupsExit(Sender: TObject);
    procedure edtStrokeCountChange(Sender: TObject);
    procedure edtJouyouChange(Sender: TObject);
    procedure miCharWordsClick(Sender: TObject);
    procedure miCharDetailsClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbStrokeCountDropDownClick(Sender: TObject);
    procedure aInClipboardExecute(Sender: TObject);
    procedure aOnlyCommonExecute(Sender: TObject);
    procedure sbOnlyCommonClick(Sender: TObject);
    procedure sbInClipboardClick(Sender: TObject);
    procedure sbStrokeCountClick(Sender: TObject);
    procedure pnlStrokeCountExit(Sender: TObject);
    procedure edtJlptChange(Sender: TObject);
    procedure sbRadicalsClick(Sender: TObject);
    procedure sbRadicalsDropDownClick(Sender: TObject);
    procedure btnGroupsClick(Sender: TObject);

  protected
    FFocusedChars: FString;
    function GetCellSize: integer;
    function GetCellFontSize: integer;
    function GetExpectedColCount: integer;
    procedure KanjiGridSelectionChanged;
    function KanjiGridGetSelection: FString;
    function KanjiGridSetSelection(const chars: FString): boolean;
    procedure SetFocusedCharsLow(const Value: FString);
    procedure SetFocusedChars(const Value: FString);
    procedure CategoryListChanged(Sender: TObject);
    procedure ClipboardChanged(Sender: TObject);
    procedure ReadFilter(flt:TStringList;const tx:string;typ:integer;flags:TReadFilterFlags);
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
    function GetKanji(cx,cy:integer):string;
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
  JWBKanaConv, JWBCategories, JWBAnnotations, TextTable, JWBCharData, JWBForms,
  JWBIntTip, JWBScreenTip, JWBCore, JWBWordLookupBase, JWBRefLinks;

var ki:TStringList;
    calfonts:TStringList;
    caltype:integer;

{$R *.DFM}

procedure TfKanji.FormCreate(Sender: TObject);
begin
  CurRadSearchType:=stRaine;
  FCurRadChars:='';
  OnCategoryListChanged.Add(Self.CategoryListChanged);
end;

procedure TfKanji.FormDestroy(Sender: TObject);
begin
  OnCategoryListChanged.Remove(Self.CategoryListChanged);
end;

procedure TfKanji.FormShow(Sender: TObject);
begin
  ReloadLookupTypes;
  cbOrAnd.ItemIndex := 0;
  Reload;
  caltype:=0;
  Self.btnKanjiDetails.Down := fKanjiDetails.Visible;
  Clipboard.Watchers.Add(Self.ClipboardChanged);
  sbOnlyCommon.GroupIndex := 14;
  sbInClipboard.GroupIndex := 15;
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

{ Returns cell width/height under current settings }
function TfKanji.GetCellSize: integer;
begin
  case fSettings.rgKanjiGridSize.ItemIndex of
    0: Result:=30;
    1: Result:=45;
    2: Result:=60;
  else Result:=60;
  end;
end;

function TfKanji.GetCellFontSize: integer;
begin
  case fSettings.rgKanjiGridSize.ItemIndex of
    0:Result:=22;
    1:Result:=37;
    2:Result:=52;
  else Result:=52;
  end;
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
  FIXED_LOOKUP_TYPES  = 6;
  LOOKUP_ANY          = 0;
  LOOKUP_CHARS        = 1;
  LOOKUP_DEFINITION   = 2;
  LOOKUP_ON           = 3;
  LOOKUP_KUN          = 4;
  LOOKUP_PINYIN       = 5;
  LOOKUP_SKIP         = 6;

//Reloads a list of textual search types
procedure TfKanji.ClearLookupTypes;
begin
  cbLookupType.Clear;
  cbLookupType.Items.Add(_l('#01132^Any matches'));
  cbLookupType.Items.Add('Characters');
  cbLookupType.Items.Add('Definition');
  cbLookupType.Items.Add('On'); //4
  cbLookupType.Items.Add('Kun'); //5
  cbLookupType.Items.Add('PinYin'); //2 mandarin
  cbLookupType.Items.Add('SKIP'); //22
  //6 nanori
  //8 cantonese
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
    if CharPropTypes[i].id>20 then
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
    sbRadicals.Caption := _l('#00178^eRadical');
    sbRadicals.Down := false;
  end else begin
    sbRadicals.Caption := _l('#00178^eRadical') + ' ' + FCurRadChars;
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

procedure TfKanji.SetFocusedChars(const Value: FString);
begin
  if FFocusedChars=Value then exit;
  SetFocusedChars(Value);
 //TODO: exit if the grid is not yet filled (we'll do the last part later)
  if not KanjiGridSetSelection(Value) then begin
 {$IFDEF AUTODEFOCUS}
    Self.KanjiGridSelectionChanged; //as if the user did that
 {$ENDIF}
  end;
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

procedure TfKanji.ReadFilter(flt:TStringList;const tx:string;typ:integer;flags:TReadFilterFlags);
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
    MakeList(tx,rfNumber in flags,sl);

    propType := FindCharPropType(typ);
    s_sp := ' ';
    s_dot := '.';

    for i:=0 to sl.Count-1 do
    begin
      s_fltval:=uppercase(sl[i]); //Locate is case-insensitive anyway

      CCharProp.LocateRawValue(s_fltval);
      while not CCharProp.EOF do
      begin
        s_val:=uppercase(CCharProp.RawValue);
        if s_val<>s_fltval then
          if not (rfPartial in flags) then
            break
          else
          if (rfSpace in flags) and (pos(s_fltval+s_sp, s_val)<>1) then
            break
          else
          if pos(s_fltval,s_val)<>1 then
            break;

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
          if rfTakedot in flags then
            dot:=CCharProp.Int(TCharProp.fReadDot)
          else
            dot:=0;
          match := match and (
              not (rfTakedot in flags)
              or (s_val=s_fltval)
              or ((dot>0) and (s_fltval=copy(s_val,dot-1)))
            );
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

function InRange(tx,fld:string;number:boolean;sl:TStringList):boolean;
begin
  MakeList(tx,number,sl);
  result:=sl.IndexOf(uppercase(fld))>-1;
end;

var
  fltLookup, fltClip: TStringList;
    accept:boolean;
    i,j,grs:integer;
    s1,s2,s3:string;
    x:integer;
    sl4,sl10:TStringList;
    fltradical:TStringList;
    clipsort:boolean;
    clipind:integer;

  categories: array of integer; //of checked category indexes
  flags: TReadFilterFlags;
  kclass: char;

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

  Screen.Cursor:=crHourGlass;

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
        ReadFilter(fltLookup, edtLookup.text, ptChineseDefinition, [rfPartial,rfSpace]) //Chinese definition
      else
        ReadFilter(fltLookup, edtLookup.text, ptJapaneseDefinition, [rfPartial,rfSpace]); //Japanese definition

   //ON and KUN
    if fSettings.cbYomiIgnoreOkurigana.Checked then
      flags := [rfPartial, rfTakedot]
    else
      flags := [];

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_ON]  then begin
      ReadFilter(fltLookup, RomajiToKana(edtLookup.Text,'j',[rfDeleteInvalidChars]), ptOnReading, flags); //as is (maybe that was kana?)
      ReadFilter(fltLookup, RomajiToKana('H'+edtLookup.Text,'j',[rfDeleteInvalidChars]), ptOnReading, flags); //maybe that was romaji?
      ReadFilter(fltLookup, RomajiToKana('K'+edtLookup.Text,'j',[rfDeleteInvalidChars]), ptOnReading, flags);
    end;

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_KUN] then begin
      ReadFilter(fltLookup, RomajiToKana(edtLookup.Text,'j',[rfDeleteInvalidChars]), ptKunReading, flags); //as is (maybe that was kana?)
      ReadFilter(fltLookup, RomajiToKana('H'+edtLookup.Text,'j',[rfDeleteInvalidChars]), ptKunReading, flags); //maybe that was romaji?
      ReadFilter(fltLookup, RomajiToKana('K'+edtLookup.Text,'j',[rfDeleteInvalidChars]), ptKunReading, flags);
    end;

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_PINYIN] then begin
      ReadFilter(fltLookup, edtLookup.Text, ptMandarinReading, [rfPartial]); //Mandarin
      ReadFilter(fltLookup, edtLookup.Text, ptCantoneseReading, [rfPartial]); //Canton
    end;

    if cbLookupType.ItemIndex in [LOOKUP_ANY, LOOKUP_SKIP] then
      ReadFilter(fltLookup, edtLookup.Text, ptSKIP, [rfPartial]);

   {
    Some special handling for SKIP which weren't actually used:

      if accept and sbSKIP.Down and (Self.edtSKIP.Text<>'') then
      begin
        s1:=Self.edtSKIP.Text;
        s2:='0';
        s3:='0';
        if pos('-',s1)>0 then
        begin
          s2:=s1;
          s1:=copy(s2,1,pos('-',s2)-1);
          delete(s2,1,pos('-',s2));
        end;
        if pos('-',s2)>0 then
        begin
          s3:=s2;
          s2:=copy(s3,1,pos('-',s3)-1);
          delete(s3,1,pos('-',s3));
        end;
      end;

    Perhaps delete?
   }

    //TODO: if Other is selected, look by that field
    {
    if Self.cbOtherType.ItemIndex=0 then
      fltother.Add(Self.edtOther.Text)
    else
      j:=0;
    for i:=0 to Length(CharPropTypes)-1 do
      if CharPropTypes[i].id>20 then
      begin
        inc(j);
        if j=Self.cbOtherType.ItemIndex then begin
          if CharPropTypes[i].dataType='N' then
            flags := [rfNumber]
          else
            flags := [];
          ReadFilter(fltother,Self.edtOther.Text,CharPropTypes[i].id,flags);
        end;
      end;
    }

  end;

 { Raine filters multi-selection with AND (only the characters with all the chosen parts are shown),
  Classical with OR (characters which match at least one radical are shown).
  This is because a character has only one Classical Radical so AND is pointless. }
  if sbRadicals.Down and (Self.curRadChars<>'') then begin
    fltradical:=TStringList.Create;
    case Self.curRadSearchType of
      stClassic: ReadFilter(fltRadical,
        RadicalIndexesToFilter(RadicalsToIndexes(stClassic,Self.CurRadChars)),
        fSettings.GetPreferredRadicalType,[rfNumber]); //Radicals
      stRaine: ReadRaineFilter(fltRadical,Self.CurRadChars);
    end;
  end;


  DrawGrid1.Perform(WM_SETREDRAW, 0, 0); //disable redraw
  try
    sl4:=TStringList.Create;
    sl10:=TStringList.Create;

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
  //  if not clipsort then fltclip.Sort;
    while ((not clipsort) and ((not TChar.EOF) and ((curlang='c') or (TChar.Int(TChar.fChinese)=0)))) or
          ((clipsort) and (clipind<fltclip.Count)) do
    begin
      accept:=true;
      if clipsort then accept:=TChar.Locate('Unicode',fltclip[clipind]);
      if accept and (curlang='c') and (fSettings.RadioGroup5.ItemIndex=0) and (TChar.Str(TChar.fType)='S') then accept:=false;
      if accept and (curlang='c') and (fSettings.RadioGroup5.ItemIndex=1) and (TChar.Str(TChar.fType)='T') then accept:=false;
      if accept and (Self.aOnlyCommon.Checked) and (curlang='c') and (TChar.Int(TChar.fChFrequency)>=255) then accept:=false;
      if accept and (Self.aOnlyCommon.Checked) and (curlang<>'c') and (TChar.Int(TChar.fJouyouGrade)>=10) then accept:=false;
      if accept and (not clipsort) and Self.aInClipboard.Checked and (fltclip.IndexOf(uppercase(TChar.Str(TChar.fUnicode)))=-1) then accept:=false;
      if accept and (fltLookup<>nil) and (fltLookup.IndexOf(TChar.Str(TChar.fUnicode))<0) then accept := false;

      if accept and sbRadicals.Down and (Self.curRadChars<>'') then
        case Self.curRadSearchType of
          stClassic: if fltradical.IndexOf(TChar.Str(TChar.fUnicode))=-1 then accept:=false;
          stRaine: if fltradical.IndexOf(TChar.Str(TChar.fUnicode))=-1 then accept:=false;
        end;
      if (curlang='c') and accept and sbStrokeCount.Down and (Self.edtStrokeCount.Text<>'') and not InRange(Self.edtStrokeCount.Text,TChar.Str(TChar.fChStrokeCount),true,sl4) then accept:=false;
      if (curlang<>'c') and accept and sbStrokeCount.Down and (Self.edtStrokeCount.Text<>'') and not InRange(Self.edtStrokeCount.Text,TChar.Str(TChar.fJpStrokeCount),true,sl4) then accept:=false;

      if accept then
        accept := CheckCategories;

  {    if accept and (Self.SpeedButton16.Down) then
      begin
        s1:=Self.Edit5.Text;
        if pos('.',s1)>0 then delete(s1,pos('.',s1),1);
        if accept then accept:=InRange(s1,TChar.Str(TCharFourCornerCode),false,sl9);
      end; }
      if accept and sbJouyou.Down and (Self.edtJouyou.Text<>'') and not InRange(Self.edtJouyou.Text,TChar.Str(TChar.fJouyouGrade),true,sl10) then accept:=false;
      if accept then
      begin
        kclass := GetCharClass(TChar.Str(TChar.fUnicode));
        if ((curlang<>'c') and (Self.rgSortBy.ItemIndex=3))
        or ((curlang='c') and (Self.rgSortBy.ItemIndex=3)) then
          ki.Insert(random(ki.Count),kclass+TChar.Str(TChar.fUnicode))
        else
          ki.Add(kclass+TChar.Str(TChar.fUnicode));
      end;
      if clipsort then inc(clipind) else TChar.Next;
    end;
    fltclip.Free;
    fltradical.Free;
    FreeAndNil(fltClip);
    FreeAndNil(fltLookup);
    sl4.Free;
    sl10.Free;

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
  for i:=0 to ki.Count-1 do
    for j := 1 to flength(chars) do
      if fgetch(chars, j)=copy(ki[i],2,4) then begin
        mr.Left:=i mod cols;
        mr.Top:=i div cols;
        mr.Right:=i mod cols;
        mr.Bottom:=i div cols;
        Result:=true;
        break;
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
  if gdSelected in State then DrawGrid1.Canvas.Brush.Color:=clHighlight else
  DrawGrid1.Canvas.Brush.Color:=clWindow;
  if gdSelected in state then DrawGrid1.Canvas.Font.Color:=clHighlightText else
  DrawGrid1.Canvas.Font.Color:=clWindowText;
  if (not fSettings.CheckBox3.Checked) and not (gdSelected in State) then
  begin
    DrawGrid1.Canvas.Brush.Color:=Col('Kanji_Back');
    TChar.Locate('Unicode',kix);
    kig:=ki[DrawGrid1.ColCount*ARow+Acol];
    case kig[1] of
      'K':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Learned');
      'C':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Common');
      'U':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Rare');
      'N':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Names');
    end;
  end;
  if fSettings.CheckBox69.Checked and HaveAnnotations then
  begin
    Annot.SeekK(kix,'');
    jouyou_val:=Annot.GetOne('c');
    if jouyou_val<>'' then try
      DrawGrid1.Canvas.Font.Color:=strtoint('0x'+copy(jouyou_val,5,2)+copy(jouyou_val,3,2)+copy(jouyou_val,1,2));
    except end;
  end;
  DrawGrid1.Canvas.FillRect(Rect);
  if curLang='j' then
    fontface:=FontJapaneseGrid
  else
  case fSettings.RadioGroup5.ItemIndex of
    0:fontface:=FontChineseGrid;
    1:fontface:=FontChineseGridGB;
    2:fontface:=FontRadical;
  else fontface:=FontChineseGrid;
  end;
  fontsize := GetCellFontSize();
  DrawGrid1.Canvas.Font.Style:=[];

 { Some glyphs may be outright impossible to draw -- no suitable fonts, even with substitution }
  r_copy := rect;
  r_copy.Left := r_copy.Left + 5;
  r_copy.Top := r_copy.Top + 4;
  DrawUnicodeChar(DrawGrid1.Canvas, r_copy, fontsize, w, fontface);

 {$IFDEF DRAW_UNSUPPORTED_CHAR_CODES}
  if GetGlyphIndices(DrawGrid1.Canvas.Handle,@w,1,@w_ind, GGI_MARK_NONEXISTING_GLYPHS)=GDI_ERROR then
    RaiseLastOsError();
  if w_ind<>$FFFF then
    TextOutW(DrawGrid1.Canvas.Handle,Rect.Left+5,Rect.Top+4,@w,1)
  else begin
   //Draw unicode index instaed
    ws := IntToHex(Utf16ToUnicodeIndex(w),4);
    DrawGrid1.Canvas.Font.Name:=FontEnglish;
    case fSettings.rgKanjiGridSize.ItemIndex of
      0:DrawGrid1.Canvas.Font.Height:=10;
      1:DrawGrid1.Canvas.Font.Height:=14;
      2:DrawGrid1.Canvas.Font.Height:=22;
    end;
    r_copy:=Rect;
    DrawText(DrawGrid1.Canvas.Handle,PChar(ws),Length(ws),r_copy,DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end;
 {$ENDIF}

  if fSettings.CheckBox1.Checked then
  begin
    TChar.Locate('Unicode',kix);
    DrawGrid1.Canvas.Font.Name:=FontEnglish;
    DrawGrid1.Canvas.Font.Height:=8+4*fSettings.rgKanjiGridSize.ItemIndex;
    DrawGrid1.Canvas.Font.Color:=clWindowText;
    if curlang='c' then DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TChar.fChStrokeCount));
    if curlang<>'c' then DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TChar.fJpStrokeCount));
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

procedure TfKanji.sbOnlyCommonClick(Sender: TObject);
begin
  aOnlyCommon.Checked := sbOnlyCommon.Down;
  SearchFilterChanged(Sender);
end;

procedure TfKanji.sbInClipboardClick(Sender: TObject);
begin
  aInClipboard.Checked := sbInClipboard.Down;
  SearchFilterChanged(Sender);
end;

procedure TfKanji.aOnlyCommonExecute(Sender: TObject);
begin
  sbOnlyCommon.Down := aOnlyCommon.Checked;
  SearchFilterChanged(Sender);
end;

procedure TfKanji.aInClipboardExecute(Sender: TObject);
begin
  sbInClipboard.Down := aInClipboard.Checked;
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

procedure TfKanji.pbRadicalsPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.Font := TWakanPaintbox(Sender).Font;
  DrawUnicode(Canvas,5,2,16,FCurRadChars,FontRadical);
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
end;

procedure TfKanji.pnlGroupsExit(Sender: TObject);
begin
  (Sender as TControl).Hide;
end;

procedure TfKanji.sbStrokeCountClick(Sender: TObject);
begin
  if sbStrokeCount.Down then
    sbStrokeCountDropDownClick(Sender);
  SearchFilterChanged(Sender);
end;

procedure TfKanji.sbStrokeCountDropDownClick(Sender: TObject);
begin
  PopupUnder(pnlStrokeCount, sbStrokeCount);
  pnlStrokeCount.Width := sbStrokeCount.Width;
//  PopupUnder(RangeSpinEdit2, btnStrokes);
end;

procedure TfKanji.edtStrokeCountChange(Sender: TObject);
begin
  sbStrokeCount.Down := (edtStrokeCount.Text <> '0') and (edtStrokeCount.Text <> '');
  if (edtStrokeCount.Text='0') or (edtStrokeCount.Text='') then
    sbStrokeCount.Caption := _l('#00191^eStroke #')
  else
    sbStrokeCount.Caption := _l('#00191^eStroke #')+' '+edtStrokeCount.Text;
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

procedure TfKanji.aRadicalExecute(Sender: TObject);
begin
  Self.sbRadicalsClick(Sender);
end;

procedure TfKanji.aMeaningExecute(Sender: TObject);
begin
  cbLookupType.ItemIndex := LOOKUP_DEFINITION;
  Self.edtLookup.SetFocus;
end;



{ CopyFormats }

function GetKanjiCopyFormatsDir: string;
begin
  Result := UserDataDir+'\KanjiCopyFormats';
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

  if miCopyAs.Visible then
    ReloadCopyFormats;
  if miCategories.Visible then
    ReloadPopupCategories;
  if miLookupIn.Visible then
    ReloadReferenceLinks;
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

function TfKanji.GetKanji(cx,cy:integer):string;
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
  calfonts:=TStringList.Create;
  ki:=TStringList.Create;
  curkanji:=UH_NOCHAR;
  IntTip.RegisterHighlightHandler(TCustomDrawGrid, KanjiGridHighlightContent);

finalization
  ki.Free;
  calfonts.Free;

end.
