unit JWBRadical;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, RXCtrls, Buttons, StdCtrls, JWBStrings;

type
  TRadSearchType = (
    stClassic,
    stRaine
  );
  TRadRecordType = (
    radRadical,
    radStrokeGroup
  );

 // TMetaStringList = type TStringList;
  TDiabolicStringList = class(TStringList);
 { Contains FStrings for radicals and normal strings with number for radical group starts:
     1
     04FE
     04FD
     2
     etc }

  TfRadical = class(TForm)
    DrawGrid: TDrawGrid;
    RxLabel17: TRxLabel;
    Shape6: TShape;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    cbDisplayVariants: TCheckBox;
    cbLearnedInBlue: TCheckBox;
    cbUncommonInGray: TCheckBox;
    rgSearchMethod: TRadioGroup;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DrawGridKeyPress(Sender: TObject; var Key: Char);
    procedure DrawGridDblClick(Sender: TObject);
    procedure cbLearnedInBlueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure rgSearchMethodClick(Sender: TObject);


  private
    sel: array[1..300] of boolean;
    rl: TDiabolicStringList;    //radicals
    rlc: TStringList;           //radical colors
    rli: TStringList;           //TRadicals.Number
    selradical: FString;

  protected
    procedure ShowIt;
    procedure FillRadicals;
    procedure FillNormalRadicals;
    procedure FillRaineRadicals;
    function GetRadRecordType(Index: integer): TRadRecordType;
    procedure InvalidateCellsByRadNo(ARadNo: integer);

  private
    FChangingFocus: integer; //if >=0, SelectCell calls are programmatic
    procedure StartChangingFocus;
    procedure EndChangingFocus;
  protected
    FSelectedRadicals: FString; //each char = one radical
    FSelectedIndexes: string;   //index;index;index  --- TRadicals.Number for normal, or Index for raine
    FOnSelectionChanged: TNotifyEvent;
    procedure SelectionChanged;
    procedure FocusCell(idx: integer);
    function FocusRadical(rad: FChar): boolean;
    function FocusAnyRadical(rads: FString): boolean;
    function GetSearchType: TRadSearchType;
  public
    function GetKanji(x,y:integer):string;
    procedure SetSelectedRadicals(ASearchType: TRadSearchType; AValue: string);
    property SearchType: TRadSearchType read GetSearchType;
    property SelectedRadicals: FString read FSelectedRadicals;
    property SelectedIndexes: string read FSelectedIndexes;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;

  public
    procedure BuildRadicalPackage(sourceFiles: array of string);

  end;

 { Casting this to access protected members }
  TFriendlyDrawGrid = class(TDrawGrid);

 { Builds radical index from multiple files }
  TRadicalIndexBuilder = class
  protected
    radicals: array of record
      rad_char: FChar;
      strokeCount: integer; //stroke count
      chars: FString;
    end;
    function AddRadical(rad_char: WideChar): integer;
    function FindRadical(rad_char: WideChar): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseRadKFile(filename: string);
    procedure Save(path: string);
  end;

var
  fRadical: TfRadical;

implementation

uses JWBMenu, JWBUnit, JWBSettings, PKGWrite, JWBCategories, JWBConvert, JWBIO;

{$R *.DFM}

procedure TfRadical.FormCreate(Sender: TObject);
begin
  FChangingFocus := 0;
  rl:=TDiabolicStringList.Create;
  rli:=TStringList.Create;
  rlc:=TStringList.Create;
end;

procedure TfRadical.FormDestroy(Sender: TObject);
begin
  rl.Free;
  rlc.Free;
  rli.Free;
end;

procedure TfRadical.FormShow(Sender: TObject);
begin
  ShowIt;
end;

procedure TfRadical.FormResize(Sender: TObject);
{var cwx,cwy:integer;}
begin
{  cwx:=(Width-211-20) div 21;
  cwy:=round(50/36*cwx);
  DrawGrid.Width:=cwx*21+20;
  DrawGrid.Height:=cwy*11+10;
  DrawGrid.DefaultColWidth:=cwx;
  DrawGrid.DefaultRowHeight:=cwy;
  Shape6.Width:=cwx*21+22;
  Shape6.Height:=cwy*11+12;}
  ShowIt;
end;

procedure TfRadical.ShowIt;
var i:integer;
begin
  if (curlang='c') or (rainesearch=nil) then
  begin
    rgSearchMethod.ItemIndex:=1;
    rgSearchMethod.Enabled:=false;
  end else
  if rgSearchMethod.Enabled=false then
  begin
    rgSearchMethod.ItemIndex:=0;
    rgSearchMethod.Enabled:=true;
  end;
  Label1.Visible:=rgSearchMethod.ItemIndex=1;
  Label2.Visible:=rgSearchMethod.ItemIndex=0;
  cbDisplayVariants.Enabled:=rgSearchMethod.ItemIndex=1;

  FillRadicals;

  StartChangingFocus(); //do not update selection while trying out sizes
  try
   //Try to choose the biggest possible cell size so that everything fits on one screen
    for i:=40 downto 20 do
    begin
      DrawGrid.DefaultColWidth:=i;
      DrawGrid.DefaultRowHeight:=DrawGrid.DefaultColWidth+14;
      DrawGrid.ColCount:=DrawGrid.ClientWidth div (DrawGrid.DefaultColWidth+1);
      DrawGrid.RowCount:=((rl.Count-1) div DrawGrid.ColCount)+1;
      if DrawGrid.RowCount*DrawGrid.DefaultRowHeight<DrawGrid.ClientHeight then break;
    end;
    DrawGrid.DefaultRowHeight:=(DrawGrid.ClientHeight div DrawGrid.RowCount)-1;
  finally
    EndChangingFocus();
  end;

 //Focus on the first available radical, or on "nothing" (first char but no highlight)
  if (FSelectedRadicals='') or not FocusAnyRadical(FSelectedRadicals) then begin
    FocusCell(1);
    selradical := '';
  end;

  DrawGrid.Invalidate;
end;

procedure TfRadical.FillRadicals;
begin
  rl.Clear;
  rli.Clear;
  rlc.Clear;
  if rgSearchMethod.ItemIndex=1 then
    FillNormalRadicals()
  else
    FillRaineRadicals();
end;

procedure TfRadical.FillNormalRadicals;
var j: integer;
  knw:boolean;
  jap:integer;
begin
  j:=0;
  knw:=false;
  jap:=0;
  TRadicals.First;
  while not TRadicals.EOF do
  begin
    if (TRadicals.Int(TRadicalsStrokeCount)>j) and (TRadicals.Int(TRadicalsVariant)=1) then
    begin
      j:=TRadicals.Int(TRadicalsStrokeCount);
      rl.Add(inttostr(j));
      rlc.Add('0');
      rli.Add('0');
    end;
    if (cbDisplayVariants.Checked) or (TRadicals.Int(TRadicalsVariant)=1) then
    begin
      rl.Add(TRadicals.Str(TRadicalsUnicode));
      if TRadicals.Int(TRadicalsVariant)=1 then
      begin
        knw:=IsKnown(KnownLearned,TRadicals.Fch(TRadicalsUnicode));
        jap:=TRadicals.Int(TRadicalsJapaneseCount);
      end; //else keep knw and jap from before -- variants go after the base radical
      if (cbLearnedInBlue.Checked) and (knw) then
        rlc.Add('0999')
      else
        if cbUncommonInGray.Checked then
          case jap of
            0:rlc.Add('0000');
            1:rlc.Add('0005');
            2:rlc.Add('0020');
          end
        else
          rlc.Add('0020');
      rli.Add(TRadicals.Str(TRadicalsNumber));
    end;
    TRadicals.Next;
  end;
end;

procedure TfRadical.FillRaineRadicals;
var i,j: integer;
  rs:string;
  rad:FString;
begin
  j:=0;
  for i:=0 to raineradicals.Count-1 do
  begin
    rs:=raineradicals[i];
    if strtoint(copy(rs,6,2))<>j then
    begin
      j:=strtoint(copy(rs,6,2));
      rl.Add(inttostr(j));
      rlc.Add('0');
      rli.Add('0');
    end;
    rad := hextofstr(copy(rs,1,4));
    rl.Add(rad);
    if cbLearnedInBlue.Checked and IsKnown(KnownLearned,rad) then
      rlc.Add('0999')
    else
      if cbUncommonInGray.Checked then
      begin
        if strtoint(copy(rs,9,4))>100 then
          rlc.Add('0020')
        else
          if strtoint(copy(rs,9,4))>20 then
            rlc.Add('0005')
          else
            rlc.Add('0000');
      end
      else
        rlc.Add('0020');
    rli.Add(inttostr(i+1));
  end;
end;

{ Returns radical record type, either "radical" or "radical group" }
function TfRadical.GetRadRecordType(Index: integer): TRadRecordType;
var rad: string;
 {$IFDEF UNICODE}
  c: integer;
 {$ENDIF}
begin
  rad := rl[Index];
 {$IFNDEF UNICODE}
  if Length(rad)<4 then
 {$ELSE}
  if TryStrToInt(rad,c) then
 {$ENDIF}
    Result := radStrokeGroup
  else
    Result := radRadical;
end;

{ Invalidates all cells which link to the specified RadNo. There can be multiple ones if we include variants }
procedure TfRadical.InvalidateCellsByRadNo(ARadNo: integer);
var i: integer;
  sRadNo: string;
begin
  sRadNo := IntToStr(ARadNo);
  for i := 0 to rl.Count - 1 do
    if rli[i]=sRadNo then
      TFriendlyDrawGrid(DrawGrid).InvalidateCell(i mod DrawGrid.ColCount, i div DrawGrid.ColCount);
end;

procedure TfRadical.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfRadical.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfRadical.DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var p,c:integer;
  rad:string;
  radw:UnicodeString;
  i:integer;
  selc:boolean;
begin
  p:=ARow*DrawGrid.ColCount+ACol;
  if p>=rl.Count then
  begin
    DrawGrid.Canvas.Pen.Color:=clWindow;
    DrawGrid.Canvas.Brush.Color:=clWindow;
    DrawGrid.Canvas.FillRect(Rect);
    exit;
  end;
  i:=0;
  if rli[p]<>'' then i:=strtoint(rli[p]);
  if i>0 then selc:=sel[i] else selc:=false;
  rad:=rl[p];
  case GetRadRecordType(p) of
    radStrokeGroup: begin
     //Radical stroke #
      DrawGrid.Canvas.Brush.Color:=clHighlight;
      DrawGrid.Canvas.Font.Color:=clHighlightText;
      DrawGrid.Canvas.Font.Name:=FontEnglish;
      DrawGrid.Canvas.Font.Style:=[fsBold];
      DrawGrid.Canvas.Font.Height:=DrawGrid.DefaultColWidth-6;
      DrawGrid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,rad);
    end;
  else
   //Radical
    c:=StrToInt(rlc[p]);
    if selc then DrawGrid.Canvas.Brush.Color:=clHighlight else
    DrawGrid.Canvas.Brush.Color:=clWindow;
    if selc then DrawGrid.Canvas.Font.Color:=clHighlightText else
    DrawGrid.Canvas.Font.Color:=clWindowText;
    DrawGrid.Canvas.Font.Style:=[];
    if not selc then
    begin
      if c<10 then
        DrawGrid.Canvas.Font.Color:=Col('Kanji_RadRare')
      else
        DrawGrid.Canvas.Font.Color:=Col('Kanji_RadCommon');
      if c=999 then
        DrawGrid.Canvas.Font.Color:=Col('Kanji_RadLearned');
      if (not cbLearnedInBlue.Checked) and (not cbUncommonInGray.Checked) then
        DrawGrid.Canvas.Font.Color:=clWindowText
      else
        DrawGrid.Canvas.Brush.Color:=Col('Kanji_Back');
    end;
    DrawGrid.Canvas.FillRect(Rect);
    DrawGrid.Canvas.Font.Name:=FontRadical;
    DrawGrid.Canvas.Font.Height:=DrawGrid.DefaultColWidth-5;
    radw:=fstrtouni(rad);
    TextOutW(DrawGrid.Canvas.Handle,Rect.Left+2,Rect.Top+2,PWideChar(radw),1);
    DrawGrid.Canvas.Font.Name:=FontEnglish;
    DrawGrid.Canvas.Font.Height:=10;
    DrawGrid.Canvas.TextOut(Rect.Left+(Rect.Right-Rect.Left-DrawGrid.Canvas.TextExtent(rli[p]).cx) div 2,Rect.Bottom-10,rli[p]);
  end;
end;

procedure TfRadical.StartChangingFocus;
begin
  Inc(FChangingFocus);
end;

procedure TfRadical.EndChangingFocus;
begin
  Dec(FChangingFocus);
end;

procedure TfRadical.DrawGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var SelIndex,OldIndex:integer;
  SelRadNo,OldRadNo: integer;
  i:integer;
begin
  if FChangingFocus>0 then begin
   //This is a programmatic change, we don't have to alter selection because of it
    CanSelect := true; //assume they know what they're doing
    exit;
  end;

  if (ACol<0) or (ARow<0) then exit;
  SelIndex:=ARow*DrawGrid.ColCount+ACol;
  if (SelIndex<0) or (SelIndex>=rl.Count) then
  begin
    CanSelect:=false;
    exit;
  end;
  if GetRadRecordType(SelIndex) = radStrokeGroup then begin
    CanSelect:=false;
    exit;
  end;
  SelRadical:=rl[SelIndex];

 { Try to avoid flicker by invalidating only those cells which were affected }
  OldIndex := DrawGrid.Row*DrawGrid.ColCount+DrawGrid.Col;
  SelRadNo := StrToInt(rli[SelIndex]);
  OldRadNo := StrToInt(rli[OldIndex]);
  if not ((GetKeyState(VK_SHIFT) and $F000<>0) or (GetKeyState(VK_CONTROL) and $F000<>0)) then
   //Reset selection
    for i:=1 to 300 do begin
      if sel[i]=true then
        InvalidateCellsByRadNo(i);
      sel[i]:=false;
    end;
  if SelIndex>0 then sel[SelRadNo]:=not sel[SelRadNo];
  InvalidateCellsByRadNo(SelRadNo);
  InvalidateCellsByRadNo(OldRadNo);
  SelectionChanged();
end;

{ Focus cell by rl index, zero-based, with no side effects (no selection change) }
procedure TfRadical.FocusCell(idx: integer);
begin
  StartChangingFocus();
  try
    DrawGrid.Row := idx div DrawGrid.ColCount;
    DrawGrid.Col := idx mod DrawGrid.ColCount;
    if (idx>=0) and (idx<=rl.Count) then
      selradical := rl[idx]
    else
      selradical := '';
  finally
    EndChangingFocus();
  end;
end;

{ Focus cell with specified radical by FocusCell(), or return false }
function TfRadical.FocusRadical(rad: FChar): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to rl.Count - 1 do
    if rl[i]=rad then begin
      FocusCell(i);
      Result := true;
      break;
    end;
end;

{ Focus cell with any of the specified radicals by FocusCell(), or return false }
function TfRadical.FocusAnyRadical(rads: FString): boolean;
var i: integer;
begin
  Result := false;
  for i := 1 to flength(rads) do
    if FocusRadical(fgetch(rads, i)) then begin
      Result := true;
      exit;
    end;
end;

function TfRadical.GetSearchType: TRadSearchType;
begin
  case rgSearchMethod.ItemIndex of
    0: Result := stRaine;
  else Result := stClassic;
  end;
end;

{ Call to set current radical selection before presenting the form to user }
procedure TfRadical.SetSelectedRadicals(ASearchType: TRadSearchType; AValue: string);
var i, j: integer;
  fch: FChar;
begin
  case ASearchType of
    stRaine: rgSearchMethod.ItemIndex := 0;
  else rgSearchMethod.ItemIndex := 1;
  end;
  FSelectedRadicals := AValue;
  FillRadicals; //we need radicals to build selection

 //Fill in sel[] and FSelectedIndexes.
  FSelectedIndexes := '';
  for i := Low(sel) to High(sel) do
    sel[i] := false;
  for i := 1 to flength(FSelectedRadicals) do begin
    fch := fgetch(FSelectedRadicals, i);
    for j := 0 to rl.Count - 1 do
      if (GetRadRecordType(j)=radRadical) and (rl[j]=fch) then begin
        sel[StrToInt(rli[j])] := true;
        FSelectedIndexes := FSelectedIndexes + ';' + rli[j];
       //don't break
      end;
  end;
  delete(FSelectedIndexes,1,1); //delete first ';'

  if Visible then ShowIt;
end;

{ Called when radical selection is changed as a result of user actions.
 Regenerates FSelectedRadicals, FSelectedIndexes and calls handlers. }
procedure TfRadical.SelectionChanged;
var i:integer;
begin
  FSelectedRadicals:='';
  FSelectedIndexes:='';
  for i:=1 to 300 do if sel[i] then
  begin
    if rgSearchMethod.ItemIndex=1 then
    begin
      TRadicals.First;
      TRadicals.Locate('Number',i);
      FSelectedRadicals:=FSelectedRadicals+TRadicals.Str(TRadicalsUnicode);
    end else
      FSelectedRadicals:=FSelectedRadicals+hextofstr(copy(raineradicals[i-1],1,4));
    FSelectedIndexes:=FSelectedIndexes+';'+IntToStr(i);
  end;
  delete(FSelectedIndexes,1,1); //delete first ';'
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TfRadical.DrawGridKeyPress(Sender: TObject; var Key: Char);
var c: FChar;
begin
  if (key='l') and (selradical<>'') then
  begin
    TChar.Locate('Unicode',selradical);
    c:=TChar.Fch(TCharUnicode);
    SetKnown(KnownLearned,c,not IsKnown(KnownLearned,c));
    if not cbLearnedInBlue.Checked then begin
     //enable coloring if it was disabled
      cbLearnedInBlue.Checked := true;
      cbLearnedInBlueClick(cbLearnedInBlue);
    end else
     //just refresh the list -- have to refill colors
      ShowIt;
  end;
end;

procedure TfRadical.DrawGridDblClick(Sender: TObject);
var Col, Row: integer;
  SelIndex: integer;
begin
  DrawGrid.MouseToCell(Mouse.CursorPos.X, Mouse.CursorPos.Y, Col, Row);
  if (Col<0) or (Row<0) then exit;
  SelIndex := Row*DrawGrid.ColCount+Col;
  if (SelIndex<0) or (SelIndex>=rl.Count) then exit;
  if GetRadRecordType(SelIndex)<>radRadical then exit;
  BitBtn1Click(sender);
end;

procedure TfRadical.cbLearnedInBlueClick(Sender: TObject);
begin
  ShowIt;
end;

procedure TfRadical.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipGridOver(DrawGrid,x,y,false);
end;

function TfRadical.GetKanji(x,y:integer):string;
var p:integer;
begin
  p:=y*DrawGrid.ColCount+x;
  result:='';
  if p>=rli.Count then exit;
  if p<0 then exit;
  result:=rl[p];
end;

procedure TfRadical.rgSearchMethodClick(Sender: TObject);
var i:integer;
begin
  for i:=1 to 300 do sel[i]:=false;
  SelectionChanged();
  ShowIt;
end;


{ Radical index builder }

constructor TRadicalIndexBuilder.Create;
begin
  inherited Create;
end;

destructor TRadicalIndexBuilder.Destroy;
begin
  inherited Destroy;
end;

function TRadicalIndexBuilder.AddRadical(rad_char: WideChar): integer;
begin
  Result := Length(radicals);
  SetLength(radicals, Result+1);
  radicals[Result].rad_char := rad_char;
  radicals[Result].strokeCount := 0;
  radicals[Result].chars := '';
end;

function TRadicalIndexBuilder.FindRadical(rad_char: WideChar): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(radicals) - 1 do
    if radicals[i].rad_char=rad_char then begin
      Result := i;
      break;
    end;
end;

procedure TRadicalIndexBuilder.ParseRadKFile(filename: string);
const
  UH_RK_COMMENT: FChar = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
  UH_RK_RAD: FChar = {$IFDEF UNICODE}'$'{$ELSE}'0024'{$ENDIF};
var
  ln: FString;
  lineno: integer;
  ch: FChar;
  ach: AnsiChar;
  i: integer;

  rad_idx: integer; //index to active radical record
  rad_char: WideChar;
  rad_scnt: integer; //stroke count

begin
 { radicals.txt format:
    4E00-01-0762-00000
    FF5C-01-0632-00762
    char-type-len-from }
 { search.bin format:
     simply UTF16-LE characters one after another }

  rad_idx := -1;

  lineno := 0;
  Conv_Open(filename, FILETYPE_EUC); //RADKFILEs are in EUC
  while not Conv_EOF() do begin
    ln := Conv_ReadLn();
    Inc(lineno);
    if flength(ln)<=0 then continue;

    ch := fgetch(ln,1);
    if ch=UH_RK_COMMENT then
      continue;
    if ch=UH_RK_RAD then begin
      i := 2;
     //Skip spaces
      while (i<=flength(ln)) and (fgetch(ln,i)=UH_SPACE) do
        Inc(i);
      if i>flength(ln) then
        raise Exception.Create('Bad RADKFILE: missing glyph char @ line '+IntToStr(lineno));
     //Read char
      rad_char := fstrtouni(fgetch(ln, i))[1];
      Inc(i);
     //Skip spaces
      while (i<=flength(ln)) and (fgetch(ln,i)=UH_SPACE) do
        Inc(i);
      if i>flength(ln) then
        raise Exception.Create('Bad RADKFILE: missing stroke count @ line '+IntToStr(lineno));
     //Read int
      rad_scnt := 0;
      while (i<=flength(ln)) and (fgetch(ln,i)<>UH_SPACE) do begin
        if not ftoansi(fgetch(ln, i), ach)
        or not (ord(ach)>=ord('0'))
        or not (ord(ach)<=ord('9')) then
          raise Exception.Create('Bad RADKFILE: invalid stroke count @ line '+IntToStr(lineno));
        rad_scnt := rad_scnt * 10 + (Ord(ach)-Ord('0'));
        Inc(i);
      end;
     //There could be jis code/image file name after this, but we're...
     //Done with this line
      rad_idx := FindRadical(rad_char);
      if rad_idx<0 then begin
        rad_idx := AddRadical(rad_char);
        radicals[rad_idx].strokeCount := rad_scnt;
      end;
      continue;
    end;

   //Character line
    if rad_idx<0 then //we must have a rad_idx by this point
      raise Exception.Create('Bad RADKFILE: character data before any radical identification @ line '+IntToStr(lineno));
    radicals[rad_idx].chars := radicals[rad_idx].chars + ln;

   //we make no attempt to find duplicate kanjis since it's not in our use case.
   //RADKFILE/RADKFILE2 cover non-overlapping sets of kanjis

  end;
  Conv_Close();
end;

{ Saves search.bin and radicals.txt to the target dir }
procedure TRadicalIndexBuilder.Save(path: string);
var
  sl_radicals: TStringList;
  f_searchbin: TUnicodeFileWriter;
  pos: integer;
  i, j: integer;
  chars_w: UnicodeString;
begin
  sl_radicals := TStringList.Create;
  f_searchbin := TUnicodeFileWriter.Rewrite(path+'\search.bin');
  try
    pos := 0;
    for i := 0 to Length(radicals) - 1 do begin
      sl_radicals.Add(
        fstrtohex(radicals[i].rad_char)+'-'+
        Format('%2.2d-%4.4d-%5.5d', [radicals[i].strokeCount, flength(radicals[i].chars), pos])
      );
      chars_w := fstrtouni(radicals[i].chars);
      for j := 1 to length(chars_w) do
        f_searchbin.WriteWideChar(chars_w[j]);
      Inc(pos, length(chars_w));
    end;
    sl_radicals.SaveToFile(path+'\radicals.txt');
    f_searchbin.Flush();
  finally
    f_searchbin.Free;
    sl_radicals.Free;
  end;
end;

{ Rebuilds wakan.rad from RADKFILE }
procedure TfRadical.BuildRadicalPackage(sourceFiles: array of string);
var tempDir: string;
  radIndex: TRadicalIndexBuilder;
  i: integer;
begin
  radIndex := TRadicalIndexBuilder.Create;
  for i := 0 to Length(sourceFiles) - 1 do
  try
    radIndex.ParseRadKFile(sourceFiles[i])
  except
    on E: Exception do begin
      E.Message := 'While importing "'+sourceFiles[i]+'": '+E.Message;
      raise;
    end;
  end;

  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  try
    radIndex.Save(tempDir);
    FreeAndNil(radIndex);

    PKGWriteForm.PKGWriteCmd('NotShow');
    PKGWriteForm.PKGWriteCmd('PKGFileName wakan.rad');
    PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
    PKGWriteForm.PKGWriteCmd('Name Japanese Radicals');
    PKGWriteForm.PKGWriteCmd('TitleName Japanese advanced radical search');
    PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
    PKGWriteForm.PKGWriteCmd('CopyrightName (C) Michael Raine, Jim Breen');
    PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
    PKGWriteForm.PKGWriteCmd('CommentName File is used by WaKan - Japanese & Chinese Learning Tool');
    PKGWriteForm.PKGWriteCmd('VersionName 1.0');
    PKGWriteForm.PKGWriteCmd('HeaderCode 791564');
    PKGWriteForm.PKGWriteCmd('FileSysCode 978132');
    PKGWriteForm.PKGWriteCmd('WriteHeader');
    PKGWriteForm.PKGWriteCmd('TemporaryLoad');
    PKGWriteForm.PKGWriteCmd('CryptMode 0');
    PKGWriteForm.PKGWriteCmd('CRCMode 0');
    PKGWriteForm.PKGWriteCmd('PackMode 0');
    PKGWriteForm.PKGWriteCmd('CryptCode 978123');
    PKGWriteForm.PKGWriteCmd('Include '+tempDir);
    PKGWriteForm.PKGWriteCmd('Finish');

  finally
    radIndex.Free;
    DeleteDirectory(tempDir);
  end;
end;

end.
