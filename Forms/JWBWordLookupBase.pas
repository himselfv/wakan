unit JWBWordLookupBase;
{ Basis for forms which list word lookup results: fWordLookup, fKanjiCompounds }

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, WakanWordGrid, StdCtrls, WakanPaintbox, Buttons, ExtCtrls,
  Menus, JWBDicSearch;

type
  TfWordLookupBase = class(TForm)
    Bevel: TPanel;
    BlankPanel: TBlankPanel;
    StringGrid: TWakanWordGrid;
    btnGoToVocab: TSpeedButton;
    btnAddToVocab: TSpeedButton;
    btnCopyToClipboard: TSpeedButton;
    pmHeader: TPopupMenu;
    miResetColumns: TMenuItem;
    procedure pmHeaderPopup(Sender: TObject);
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  protected
   { You don't have to retrieve the results from the dictionary, you may fill
    it manually if you prefer }
    FResults: TSearchResults;
    procedure ResultsChanged; virtual;
    procedure WordSelectionChanged; virtual;
  public
   { Currently selected word, cached for simplicity. Some rely on it outside
    of the form, such as JWBHint on JWBWordLookup.curword }
    curword: integer; //line number in string grid
    curkanji: string;
    curphonetic: string;
    curmeaning: string;
    procedure SetDefaultColumnWidths; virtual;
    procedure Clear; virtual;
    procedure CopyToClipboard(const AFullArticle, AReplace: boolean);
    property Results: TSearchResults read FResults;


  end;

var
  fWordLookupBase: TfWordLookupBase;


implementation
uses UITypes, JWBStrings, JWBUnit, JWBMenu, JWBCategories, JWBVocab, JWBVocabAdd;

{$R *.dfm}

constructor TfWordLookupBase.Create(AOwner: TComponent);
begin
  inherited;
  FResults:=TSearchResults.Create; //it is sometimes used even before FormCreate, somehow
end;

destructor TfWordLookupBase.Destroy;
begin
  FreeAndNil(FResults);
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

procedure TfWordLookupBase.pmHeaderPopup(Sender: TObject);
var p: TPoint;
  ACol, ARow: integer;
begin
  p := StringGrid.ScreenToClient(Mouse.CursorPos);
  StringGrid.MouseToCell(p.X, p.Y, ACol, ARow);
  miResetColumns.Visible := (ARow=0); //click on header
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
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
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
    CopyToClipboard(
      {full=}true, //I'd like to copy non-full on ALT, but it's not being detected!
      {replace=}GetKeyState(VK_SHIFT) and $F0 = 0 //append when Shift is pressed
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
      tmp.Add(FResults[i].ArticlesToString);
    FillWordGrid(StringGrid,tmp,false,false);
  finally
    FreeAndNil(tmp);
  end;
end;

{ Reformats combined article text a bit: moves all flags into a single clause,
 adds linebreaks etc }
function GlorifyCopypaste(const s: string): string;
var ps, pc: PChar;
  flags: string;

  procedure CommitText;
  begin
    if pc<=ps then exit;
    Result := Result + spancopy(ps,pc);
  end;

  procedure CommitTrimSpace;
  begin
   //We're going to cut something from this point on,
   //so maybe trim a space at the end of the previous block
    Dec(pc);
    if pc^=' ' then begin
      CommitText;
      Inc(pc);
    end else begin
      Inc(pc);
      CommitText;
    end;
  end;

  procedure CommitFlags;
  begin
    if flags<>'' then
      Result := Result + ' <'+flags+'>';
    flags := '';
  end;

  procedure IncTrimSpace(var pc: PChar);
  begin
   //We have cut something before this point in the string,
   //so maybe trim a space at the start of the next block
    Inc(pc);
    if pc^=' ' then begin
      Inc(pc);
      if pc^<>' ' then
        Dec(pc);
    end;
  end;

begin
  Result := '';
  if s='' then exit;

  flags := '';

  pc := PChar(s);
  ps := pc;
  while pc^<>#00 do

    if pc^='<' then begin
      CommitTrimSpace;
      Inc(pc);
      ps := pc;
      while (pc^<>#00) and (pc^<>'>') do
        Inc(pc);
      if pc>ps then
       //First char of the flag is always its type; we don't care
        Inc(ps);
      if pc>ps then begin
        if flags<>'' then
          flags := flags + ', ';
        flags := flags + spancopy(ps,pc);
      end;
      IncTrimSpace(pc);
      ps := pc;
    end else

    if pc^='/' then begin
      CommitTrimSpace;
      CommitFlags;
      if (Result<>'') and (Result[Length(Result)]<>#13) then
        Result := Result + #13;
      IncTrimSpace(pc);
      ps := pc;
    end else

    if (pc^='(') and IsDigit(pc[1]) and (
      (pc[2]=')') or ( IsDigit(pc[2]) and (pc[3]=')')  )
    ) then begin
      CommitTrimSpace;
      CommitFlags;
      if (Result<>'') and (Result[Length(Result)]<>#13) then
        Result := Result + #13;
      ps := pc; //include brackets too
      Inc(pc);
    end else

      Inc(pc); //any other char

  CommitText;
  CommitFlags;
end;

//Copies currently selected article to the clipboard
procedure TfWordLookupBase.CopyToClipboard(const AFullArticle, AReplace: boolean);
var i: integer;
   AText, tmp: string;
begin
  AText := '';
  for i := StringGrid.Selection.Top to StringGrid.Selection.Bottom do begin
    if AFullArticle then begin
      tmp := remexcl(FResults[i-1].entry);
      if pos(' >> ',tmp)>0 then delete(tmp,1,pos(' >> ',tmp)+3);
      tmp:=UnfixVocabEntry(tmp); //replace markup symbols with user readable
      tmp:=GlorifyCopypaste(tmp); //reformat a bit to make it more pleasant
      if AText<>'' then AText := AText+#13;
      AText:=AText+FResults[i-1].kanji+' ['+FResults[i-1].kana+'] '+tmp;
    end else
      AText:=AText+FResults[i-1].kanji;
  end;

  if AReplace then
    clip := AText
  else
  if AFullArticle then begin
    if clip<>'' then
      clip := clip + #13 + AText //add newline
    else
      clip := AText;
  end else
    clip := clip + AText;
  fMenu.SetClipboard;
end;

procedure TfWordLookupBase.btnGoToVocabClick(Sender: TObject);
begin
  fMenu.aModeWordsExecute(sender);
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
end;

procedure TfWordLookupBase.btnCopyToClipboardClick(Sender: TObject);
begin
 //Emulate older behavior
  CopyToClipboard({full=}false,{replace=}false);
end;

end.
