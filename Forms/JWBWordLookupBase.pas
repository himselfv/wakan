unit JWBWordLookupBase;
{ Basis for forms which list word lookup results: fWordLookup, fKanjiCompounds }

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, WakanWordGrid, StdCtrls, WakanPaintbox, Buttons, ExtCtrls,
  Menus, ImgList, XmlDoc, XmlIntf, JWBDicSearch;

type
  TfWordLookupBase = class(TForm)
    Bevel: TPanel;
    BlankPanel: TBlankPanel;
    StringGrid: TWakanWordGrid;
    btnGoToVocab: TSpeedButton;
    btnAddToVocab: TSpeedButton;
    btnCopyToClipboard: TSpeedButton;
    pmPopup: TPopupMenu;
    miResetColumns: TMenuItem;
    miCopyAs: TMenuItem;
    miGoToVocab: TMenuItem;
    miAddToVocab: TMenuItem;
    ilImages: TImageList;
    miLookUpIn: TMenuItem;
    miBeforeLookupIn: TMenuItem;
    miAfterLookupIn: TMenuItem;
    procedure pmPopupPopup(Sender: TObject);
    procedure miResetColumnsClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnGoToVocabClick(Sender: TObject);
    procedure btnAddToVocabClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure StringGridKeyPress(Sender: TObject; var Key: Char);
    procedure StringGridDblClick(Sender: TObject);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure miGoToVocabClick(Sender: TObject);
    procedure miAddToVocabClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  protected
   { You don't have to retrieve the results from the dictionary, you may fill
    it manually if you prefer }
    FResults: TSearchResults;
    procedure ResultsChanged; virtual;
    procedure WordSelectionChanged; virtual;
    procedure ReloadCopyFormats;
    procedure CopyInFormatClick(Sender: TObject);
    procedure CopyAsXMLClick(Sender: TObject);
    procedure ConfigureClick(Sender: TObject);
    procedure ClearReferenceLinks;
    procedure ReloadReferenceLinks;
  public
   { Currently selected word, cached for simplicity. Some rely on it outside
    of the form, such as JWBHint on JWBWordLookup.curword }
    curword: integer; //line number in string grid
    curkanji: string;
    curphonetic: string;
    curmeaning: string;
    procedure SetDefaultColumnWidths; virtual;
    procedure Clear; virtual;
    procedure Refresh; virtual;
    procedure CopyAsText(const AReplace: boolean);
    procedure CopyToClipboard(const AXsltFilename: string; const AReplace: boolean);
    function IsEmpty: boolean;
    property Results: TSearchResults read FResults;

  end;

var
  fWordLookupBase: TfWordLookupBase;

//CopyFormats
function GetCopyFormatsDir: string;
function GetCopyFormats: TArray<string>;
function XsltTransform(const s: UnicodeString; const AXslt: IXMLDocument): WideString; overload;
function XsltTransform(const s: UnicodeString; const AXsltFilename: string): WideString; overload;

implementation
uses UITypes, JWBStrings, JWBCore, JWBUnit, JWBMenu, JWBCategories, JWBVocab,
  JWBVocabAdd, JWBSettings, JWBLegacyMarkup, JWBRefLinks, JWBLanguage,
  JWBWordGrid, ActiveX;

{$R *.dfm}

constructor TfWordLookupBase.Create(AOwner: TComponent);
begin
  inherited;
  CoInitialize(nil); //for XSLT
  FResults:=TSearchResults.Create; //it is sometimes used even before FormCreate, somehow
end;

destructor TfWordLookupBase.Destroy;
begin
  FreeAndNil(FResults);
  CoUninitialize;
  inherited;
end;

procedure TfWordLookupBase.SetDefaultColumnWidths;
begin
  StringGrid.ColWidths[0]:=110;
  StringGrid.ColWidths[1]:=138;
  StringGrid.ColWidths[2]:=353;
  StringGrid.AutoSizeColumns;
end;

procedure TfWordLookupBase.Clear;
var sl: TStringList;
begin
 //Clear StringGrid the way wakan handles it.
 //We can simply hide it (thats what FillWordGrid will do), but let's play it nice
  sl := TStringList.Create;
  try
    FillWordGrid(StringGrid,sl,false,false);
  finally
    FreeAndNil(sl);
  end;

  FResults.Clear;

  curword := 0;
  WordSelectionChanged;
end;

procedure TfWordLookupBase.Refresh;
begin
 //Called when something thinks the results are not valid anymore and need to be
 //repopulated.
 //Implemented by descendants.
end;

procedure TfWordLookupBase.pmPopupPopup(Sender: TObject);
var p: TPoint;
  ACol, ARow: integer;
begin
  p := StringGrid.ScreenToClient(Mouse.CursorPos);
  StringGrid.MouseToCell(p.X, p.Y, ACol, ARow);
 //ARow may be <0 while curword is always last valid selected entry
  miResetColumns.Visible := (ARow=0); //click on header
  miCopyAs.Visible := (ARow>0); //click on data
  if miCopyAs.Visible then
    ReloadCopyFormats;
  miLookUpIn.Visible := (ARow>0);
  if miLookUpIn.Visible then
    ReloadReferenceLinks
  else
    ClearReferenceLinks; //some could have been in this menu
  miGoToVocab.Visible := (ARow>0) and (FResults[ARow-1].userIndex > 0);
  miAddToVocab.Visible := (ARow>0);
end;


{ CopyFormats }

//Returns the full path to the folder where CopyFormats are stored in this
//configuration
function GetCopyFormatsDir: string;
begin
  Result := UserDataDir+'\CopyFormats';
end;

//Retrieves a list of all avaialable CopyFormat filenames
function GetCopyFormats: TArray<string>;
var sr: TSearchRec;
  res: integer;
  fdir: string;
begin
  SetLength(Result, 0);
  fdir := GetCopyFormatsDir;
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

//Transforms a JMDICT-XML-formatted record using the specified CopyFormat
function XsltTransform(const s: UnicodeString; const AXslt: IXMLDocument): WideString; overload;
var AInp: IXMLDocument;
begin
  AInp := LoadXMLData(s);
  AInp.Node.TransformNode(AXslt.Node, Result);
end;

function XsltTransform(const s: UnicodeString; const AXsltFilename: string): WideString; overload;
var AXslt: IXMLDocument;
begin
  if AXsltFilename<>'' then begin
    AXslt := LoadXMLDocument(AXsltFilename);
    Result := XsltTransform(s, AXslt);
  end else
    Result := s;
end;

type
  TCopyFormatMenuItem = class(TMenuItem)
  public
    Filename: string;
  end;

procedure TfWordLookupBase.ReloadCopyFormats;
var item: TMenuItem;
  fname: string;
begin
  miCopyAs.Clear;

 //Rescan every time because the user could be adding files there
 //and expecting results
  for fname in GetCopyFormats() do begin
    item := TCopyFormatMenuItem.Create(Self);
    TCopyFormatMenuItem(item).Filename := ExtractFilename(fname);
    item.Caption := ChangeFileExt(TCopyFormatMenuItem(item).Filename, '');
    item.OnClick := CopyInFormatClick;
    if item.Caption=fSettings.DefaultCopyFormatName then
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

procedure TfWordLookupBase.CopyInFormatClick(Sender: TObject);
begin
  CopyToClipboard(
    GetCopyFormatsDir+'\'+TCopyFormatMenuItem(Sender).Filename,
    GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
  );
end;

procedure TfWordLookupBase.CopyAsXMLClick(Sender: TObject);
begin
  CopyToClipboard(
    '',
    GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
  );
end;

procedure TfWordLookupBase.ConfigureClick(Sender: TObject);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsDictCopyFormats;
  fSettings.ShowModal;
end;

procedure TfWordLookupBase.ClearReferenceLinks;
var i: integer;
begin
 //Submenu
  miLookUpIn.Clear;
 //Direct items
  for i := pmPopup.Items.Count-1 downto 0 do
    if pmPopup.Items[i] is TRefMenuItem then
      pmPopup.Items[i].Destroy;
end;

procedure TfWordLookupBase.ReloadReferenceLinks;
var i, ARow, idx: integer;
  mi: TMenuItem;
  fname: string;
  ref: TRefLink;
begin
  ClearReferenceLinks;

  if StringGrid.Selection.Top <> StringGrid.Selection.Bottom then begin
    miBeforeLookupIn.Visible := false;
    miAfterLookupIn.Visible := false;
    miLookUpIn.Visible := false;
    exit;
  end;
  ARow := StringGrid.Selection.Top;

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
    idx := pmPopup.Items.IndexOf(miAfterLookupIn);
  end;

  for fname in GetExpressionLinks() do begin
    ref := LoadLink(fname);
    try
      if ref.MatchesLang(curLang) then begin
        mi := TRefMenuItem.Create(Self, ref, FResults[ARow-1].kanji);
        if fSettings.cbDictRefLinksInSubmenu.Checked then
          miLookUpIn.Add(mi)
        else begin
          pmPopup.Items.Insert(idx, mi);
          Inc(idx);
        end;
      end;
    finally
      FreeAndNil(ref);
    end;
  end;
end;

procedure TfWordLookupBase.miResetColumnsClick(Sender: TObject);
begin
  SetDefaultColumnWidths;
end;

procedure TfWordLookupBase.StringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(TStringGrid(Sender),ACol,ARow,Rect,State);
end;

procedure TfWordLookupBase.StringGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipMouseMove(TStringGrid(Sender),x,y,ssLeft in Shift);
end;

procedure TfWordLookupBase.StringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
  ACol, ARow: integer;
  r: TGridRect;
  CanSelect: boolean;
begin
 //Right-click-select
  p := StringGrid.ScreenToClient(Mouse.CursorPos);
  StringGrid.MouseToCell(p.X, p.Y, ACol, ARow);
  if ARow>0 then
    if (ARow<StringGrid.Selection.Top) or (ARow>StringGrid.Selection.Bottom) then begin
      r := StringGrid.Selection;
      CanSelect := true;
      StringGridSelectCell(Sender, ACol, ARow, CanSelect);
      if not CanSelect then exit;
      r.Top := ARow;
      r.Bottom := ARow;
      StringGrid.Selection := r;
    end;
end;

procedure TfWordLookupBase.StringGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.IntTipMouseUp;
end;

procedure TfWordLookupBase.StringGridKeyPress(Sender: TObject; var Key: Char);
begin
 //Copy the article to clipboard on Ctrl-C
  if (Key=^C) and StringGrid.Visible then begin
    if fSettings.DefaultCopyFormatName='' then
      CopyAsText(
        GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
      )
    else
      CopyToClipboard(
        GetCopyFormatsDir+'\'+fSettings.DefaultCopyFormatName+'.xslt',
        GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
      );
    Key := #00;
  end;
end;

procedure TfWordLookupBase.StringGridDblClick(Sender: TObject);
begin
  if btnAddToVocab.Enabled then btnAddToVocabClick(sender);
end;

procedure TfWordLookupBase.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
 { Careful not to trigger an endless loop: WordSelectionChanged changes the grid
  which triggers SelectCell which triggers WordSelectionChanged.
  SelectCell also gets called endlessly while you hold mouse down due to a bug
  in TStringGrid if RowSelect is true }
  if curword=ARow then exit;
  curword:=ARow;
  WordSelectionChanged;
end;

{ Called when an entry has been selected in the result list }
procedure TfWordLookupBase.WordSelectionChanged;
begin
 //Simple implementation. Descendants are free to make this more complicated:
  if (curword>=1) and (curword<=FResults.Count) then begin
    curphonetic:=remexcl(copy(StringGrid.Cells[0,curword],2,length(StringGrid.Cells[0,curword])-1));
    curkanji:=remexcl(copy(StringGrid.Cells[1,curword],2,length(StringGrid.Cells[1,curword])-1));
    curmeaning:=remexcl(StringGrid.Cells[2,curword]);
    btnCopyToClipboard.Enabled:=true;
    btnAddToVocab.Enabled:=true;
    btnGoToVocab.Enabled:= (FResults[curword-1].userIndex<>0);
  end else begin
    curphonetic:='';
    curkanji:='';
    curmeaning:='';
    btnGoToVocab.Enabled:=false;
    btnCopyToClipboard.Enabled:=false;
    btnAddToVocab.Enabled:=false;
  end;
end;

{ Called when a Results list has been changed and the interface needs to be
 updated }
procedure TfWordLookupBase.ResultsChanged;
var tmp: TStringList;
  i: integer;
begin
 //Repopulate StringGrid
  tmp := TStringList.Create;
  try
    for i:=0 to FResults.Count - 1 do
      tmp.Add(FResults[i].ToLegacyString);
    FillWordGrid(StringGrid,tmp,false,false);
  finally
    FreeAndNil(tmp);
  end;
end;

//Very simple default copying
procedure TfWordLookupBase.CopyAsText(const AReplace: boolean);
var i: integer;
  AText, tmp: string;
begin
  AText := '';
  for i := StringGrid.Selection.Top to StringGrid.Selection.Bottom do begin
    tmp := FResults[i-1].kanji; //very simple default copying
    if AText<>'' then
      AText := AText+#13+tmp
    else
      AText := tmp;
  end;

  if AReplace or (Clip='') then
    clip := AText
  else
    clip := clip + #13 + AText; //add newline
  fMenu.SetClipboard;
end;

//AXsltFilename must be full path+filename or empty (default xml)
procedure TfWordLookupBase.CopyToClipboard(const AXsltFilename: string;
  const AReplace: boolean);
var i: integer;
   AText: string;
begin
  AText := '';
  for i := StringGrid.Selection.Top to StringGrid.Selection.Bottom do
    if AText<>'' then
      AText := AText+#13+FResults[i-1].ToEdictXml
    else
      AText := FResults[i-1].ToEdictXml;
  AText := XsltTransform(AText, AXsltFilename);

  if AReplace or (Clip='') then
    clip := AText
  else
    clip := clip + #13 + AText; //add newline
  fMenu.SetClipboard;
end;


//True if no results in the table
function TfWordLookupBase.IsEmpty: boolean;
begin
  Result := FResults.Count<=0;
end;

procedure TfWordLookupBase.btnGoToVocabClick(Sender: TObject);
begin
  fMenu.aModeVocabExecute(sender);
  if FResults[curword-1].userIndex<>0 then
    fVocab.SearchWord(FResults[curword-1].userIndex);
end;

procedure TfWordLookupBase.btnAddToVocabClick(Sender: TObject);
var tmp: string;
begin
  tmp := curmeaning;
  if pos(' >> ',tmp)>0 then delete(tmp,1,pos(' >> ',tmp)+3);
  tmp:=UnfixVocabEntry(tmp); //replace markup symbols with user readable
  if not IsPositiveResult(fVocabAdd.ModalAddFixed(curkanji,curphonetic,fstr(tmp))) then
    exit;
  Self.Refresh;
end;

procedure TfWordLookupBase.btnCopyToClipboardClick(Sender: TObject);
begin
 //Emulate older behavior
  CopyAsText({replace=}false);
end;

procedure TfWordLookupBase.miGoToVocabClick(Sender: TObject);
begin
  btnGoToVocabClick(Sender);
end;

procedure TfWordLookupBase.miAddToVocabClick(Sender: TObject);
begin
  btnAddToVocabClick(Sender);
end;

end.
