unit JWBTranslate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXCtrls, ExtCtrls, Buttons, JWBUtils, JWBStrings;

{
Various notes go here.
}
{
Document contents is stored in "doc", line by line (==text lines).
"linl" contains graphical line descriptions:
  xs = first character of this graphical line in logical line
  ys = logical line index in "doc"
  len = length
Selection is kept in logical coordinates - (logical line, start character) pairs.
Latin characters are considered "half-width" and require only half the slot
kanji and kana uses.
}

type
  TTextAnnotMode = (
    amDefault,
      //do not load ruby
      //save only those annotations which were loaded from ruby
    amKana,
      //do not load ruby
      //save as text with generated kana instead of kanji
    amRuby
      //load ruby
      //save generated kana as aozora-ruby
  );

  TfTranslate = class(TForm)
    Shape10: TShape;
    EditorPaintBox: TPaintBox;
    ListBox1: TListBox;
    EditorScrollBar: TScrollBar;
    Bevel1: TBevel;
    sbFileOpen: TSpeedButton;
    sbFileSave: TSpeedButton;
    sbFileNew: TSpeedButton;
    sbClipCut: TSpeedButton;
    sbAsciiMode: TSpeedButton;
    sbKanjiMode: TSpeedButton;
    sbKanaMode: TSpeedButton;
    sbDisplayReading: TSpeedButton;
    sbDisplayMeaning: TSpeedButton;
    sbClearTranslation: TSpeedButton;
    sbAutoTranslate: TSpeedButton;
    sbSetTranslation: TSpeedButton;
    sbPrint: TSpeedButton;
    Bevel2: TBevel;
    sbClipCopy: TSpeedButton;
    sbClipPaste: TSpeedButton;
    lblFilename: TLabel;
    Bevel3: TBevel;
    sbSmallFont: TSpeedButton;
    sbLargeFont: TSpeedButton;
    sbMiddleFont: TSpeedButton;
    lblControlsHint: TLabel;
    sbDockDictionary: TSpeedButton;
    sbDockKanjiDetails: TSpeedButton;
    sbUseTlColors: TSpeedButton;
    BlinkCursorTimer: TTimer;
    OpenTextDialog: TOpenDialog;
    SaveTextDialog: TSaveDialog;
    SaveAnnotationDialog: TSaveDialog;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorPaintBoxClick(Sender: TObject);
    procedure EditorPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorPaintBoxPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
    procedure sbDisplayReadingClick(Sender: TObject);
    procedure sbDisplayMeaningClick(Sender: TObject);
    procedure sbClearTranslationClick(Sender: TObject);
    procedure sbAutoTranslateClick(Sender: TObject);
    procedure sbSetTranslationClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbFileOpenClick(Sender: TObject);
    procedure sbFileSaveClick(Sender: TObject);
    procedure sbFileNewClick(Sender: TObject);
    procedure sbClipCutClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbClipCopyClick(Sender: TObject);
    procedure sbClipPasteClick(Sender: TObject);
    procedure sbKanjiModeClick(Sender: TObject);
    procedure sbKanaModeClick(Sender: TObject);
    procedure sbAsciiModeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure sbSmallFontClick(Sender: TObject);
    procedure sbLargeFontClick(Sender: TObject);
    procedure sbMiddleFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure EditorScrollBarChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure EditorPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditorPaintBoxDblClick(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sbDockKanjiDetailsClick(Sender: TObject);
    procedure sbDockDictionaryClick(Sender: TObject);
    procedure ListBox1Enter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Exit(Sender: TObject);
    procedure sbUseTlColorsClick(Sender: TObject);
    procedure BlinkCursorTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  public
    CopyShort, CutShort, PasteShort, AllShort:TShortCut;

  public
    linl: TGraphicalLineList; //lines as they show on screen
    docfilename:string;
    doctp:byte;

  protected //Mostly repainting
    EditorBitmap:TBitmap;
    procedure MakeEditorBitmap;
    procedure PasteEditorBitmap;
    procedure RenderText(x,y:integer;canvas:TCanvas;l,t,w,h:integer;
      ll:TGraphicalLineList;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
  public
    mustrepaint:boolean;
    procedure RepaintText;
    procedure ShowText(dolook:boolean);

  protected //Unsorted
    procedure HandleWheel(down:boolean);
    procedure CalcBlockFromTo(backtrack:boolean);

    function SetWordTrans(x,y:integer;scanparticle:boolean;gridfirst:boolean;user:boolean):integer;
    procedure DisplayInsert(convins:string;transins:TCharacterPropArray;leaveinserted:boolean);
    procedure ResolveInsert(final:boolean);
    procedure InsertCharacter(c:char);
    procedure DrawCursor(blink:boolean);
    procedure DrawBlock(Canvas: TCanvas);
    procedure CheckTransCont(x,y:integer);
    procedure SplitLine(x,y:integer);
    procedure JoinLine(y:integer);
    procedure DeleteCharacter(x,y:integer);
    procedure RefreshLines;
    procedure ClearInsBlock;
    procedure BlockOp(docopy,dodelete:boolean);
    procedure PasteOp;
    function PosToWidth(x,y:integer):integer;
    function WidthToPos(x,y:integer):integer;
  public
    procedure CalcMouseCoords(x,y:integer;var rx,ry:integer);


  public //Document
    doc: TStringList; //document lines
    doctr: TCharacterPropList; //character property list (translation, wordstate, etc)
    docdic: TStringList;
    function GetDoc(ax,ay:integer):FChar;
    function IsHalfWidth(x,y:integer):boolean;
    function GetDocWord(x,y:integer;var wordtype:integer;stopuser:boolean):string;
    procedure GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:FString);

  protected
    oldcurx,oldcury: integer; //last cursor position, where it was drawn.
    curx,cury: integer; //cursor position (maybe not drawn yet, may differ from where cursor is drawn -- see CursorScreenX/Y)
    procedure SetCurPos(x,y:integer);
  public
    view:integer; //index of a first visible graphical line
    rviewx,rviewy:integer; //logical coordinates of a start of a first visible graphical line
    rcurx,rcury: integer; //cursor position in logical coordinates
      //also translation position when translating
    cursorposcache:integer; //cursor X in pixels, from last DrawCursor. -1 means recalculate
    lastxsiz,lastycnt,printl:integer;
    //Actual cursor position --- differs from (curx,cury) if we're at the end of the text
    function CursorScreenX:integer;
    function CursorScreenY:integer;
    procedure SelectAll;

  protected //Insert buffer
    insertbuffer:string;
    resolvebuffer:boolean;
  public
   //Unfortunately some stuff is used from elswehere
    insx,insy,inslen: integer; //editor aftertouch --- after we have inserted the word, it's highlighted
    buffertype:char;
    function GetInsertKana(display:boolean):string;

  protected //Ruby stuff
    procedure CollapseRuby(var s: FString; var sp: TCharacterLineProps);

  protected //File opening/saving
    FFileChanged: boolean;
    SaveAnnotMode: TTextAnnotMode; //if we have saved the file once, we remember the choice
    procedure SetFileChanged(Value: boolean);
    procedure LoadText(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
    procedure SaveText(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
  public //File open/save
    procedure OpenFile(filename:string;tp:byte);
    procedure SaveToFile(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
    function SaveAs: boolean;
    function CommitFile:boolean;
    property FileChanged: boolean read FFileChanged write SetFileChanged;

  end;

var
  fTranslate: TfTranslate;

implementation

uses JWBUser, JWBMenu, JWBHint, JWBKanjiDetails, JWBKanji, JWBStatistics,
  JWBSettings, JWBPrint, JWBConvert, JWBDicSearch, StdPrompt, JWBUnit;

{$R *.DFM}

var
  blockfromx,blockfromy,blocktox,blocktoy:integer;
  oldblockfromx,oldblockfromy,oldblocktox,oldblocktoy:integer;
  blockx,blocky:integer;
  plinl:TGraphicalLineList;
  printpl:integer;
  insconfirmed:boolean;
  leaveline:boolean;
  shiftpressed:boolean;
  cursorend:boolean;
  lastmmx,lastmmy:integer;

  priorkanji:string;
  cursorblinked:boolean;

function TfTranslate.GetDoc(ax,ay:integer):FChar;
begin
  if ay>=doc.Count then showmessage('Illegal doc access!');
  if ax>=flength(doc[ay]) then result:=UH_ZERO else result:=fgetch(doc[ay],ax+1);
end;

function TfTranslate.IsHalfWidth(x,y:integer):boolean;
begin
  result:=IsHalfWidthChar(GetDoc(x,y));
end;

function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var pl,xs,yc:integer;
begin
  plinl.Clear;
  fTranslate.RenderText(0,0,canvas,width div 50,height div 50,width-width div 25,
    height-height div 25,plinl,pl,xs,yc,true,true);
  printpl:=pl;
  result:=((plinl.Count-1) div pl)+1;
  if result<1 then result:=1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var pl,xs,yc:integer;
begin
  if plinl.Count<=(pagenum-1)*printpl then exit;
  fTranslate.RenderText(
    plinl[(pagenum-1)*printpl].xs,
    plinl[(pagenum-1)*printpl].ys,
    canvas,width div 50,height div 50,width-width div 25,height-height div 25,
    plinl,pl,xs,yc,true,false);
end;



procedure TfTranslate.FormCreate(Sender: TObject);
begin
  doc:=TStringList.Create;
  doctr:=TCharacterPropList.Create;
  docdic:=TStringList.Create;

  docfilename:='';
  doctp:=0;
  FileChanged:=false;

  view:=0; rviewx:=-1; rviewy:=-1;
  curx:=-1; cury:=-1;
  rcurx:=-1; rcury:=-1;
  oldcurx:=-1; oldcury:=-1;
  insx:=-1; insy:=-1; inslen:=0;
  cursorposcache:=-1;
  lastxsiz:=16;
  lastycnt:=2;
  printl:=1;
  mustrepaint:=true;

  linl:=TGraphicalLineList.Create;
  plinl:=TGraphicalLineList.Create;
  CopyShort:=fMenu.aEditorCopy.ShortCut;
  CutShort:=fMenu.aEditorCut.ShortCut;
  PasteShort:=fMenu.aEditorPaste.ShortCut;
  AllShort:=fMenu.aEditorSelectAll.ShortCut;
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;
end;

procedure TfTranslate.FormDestroy(Sender: TObject);
begin
  linl.Free;
  plinl.Free;
  EditorBitmap.Free;
  doc.Free;
  doctr.Free;
  docdic.Free;
end;

procedure TfTranslate.FormShow(Sender: TObject);
begin
  ShowText(true);
  ListBox1.ItemIndex:=0;
  ListBox1.SetFocus;
end;

procedure TfTranslate.FormHide(Sender: TObject);
begin
  if dictmodeset=0 then fUser.SpeedButton1.Down:=true;
  if dictmodeset=1 then fUser.SpeedButton2.Down:=true;
  if dictmodeset=2 then fUser.SpeedButton3.Down:=true;
//  fUser.Look(false);
end;

procedure TfTranslate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton8.Down:=false;
  fMenu.aDictEditor.Checked:=false;
end;

procedure TfTranslate.FormResize(Sender: TObject);
begin
  linl.Clear;
  Invalidate;
end;

//TODO: Convert to Unicode
procedure TfTranslate.OpenFile(filename:string;tp:byte);
var s,s2:string;
  s3: TCharacterLineProps;
  i:integer;
  w:word;
  f:file;
  reat:integer;
  buf:array[0..16383] of word;
  ws:array[0..31] of char;
  wss:array[0..4091] of char;
  wc:widechar;
  jtt,dot:boolean;
  l:integer;
  ls:string;
  dp:char;
  LoadAnnotMode: TTextAnnotMode;
begin
  docfilename:=filename;
  doctp:=tp;

  //by default we set SaveAnnotMode to default, meaning no preference has been chosen
  //auto-loaded rubys will be saved either way
  SaveAnnotMode := amDefault;

  //LoadAnnotMode governs how we treat incoming ruby (loading/pasting)
  if fSettings.cbLoadAozoraRuby.Checked then
    LoadAnnotMode := amRuby
  else
    LoadAnnotMode := amDefault;

  lblFilename.Caption:=uppercase(ExtractFilename(filename));
  doc.Clear;
  doctr.Clear;
  docdic.Clear;
  linl.Clear;
  Screen.Cursor:=crHourGlass;
  jtt:=false;
  if tp=255 then
  begin
    assignfile(f,docfilename);
    reset(f,2);
    blockread(f,w,1,reat);
    if (reat<1) or (w<>$f1ff) then
    begin
      Application.MessageBox(pchar(_l('#00679^eThis is not a valid UTF-8 or JTT file.')),pchar(_l('#00020^eError')),MB_OK);
      closefile(f);
      exit;
    end;
    jtt:=true;
  end;
  dot:=true;
  if jtt then
  begin
   //TODO: Convert this subblock to unicode
    blockread(f,ws,16);
    s:=ws;
    if copy(s,1,22)<>'WaKan Translated Text>'then
    begin
      Application.MessageBox(
        pchar(_l('#00679^eThis is not a valid UTF-8 or JTT file.')),
        pchar(_l('#00020^eError')),
        MB_OK);
      closefile(f);
      exit;
    end;
    delete(s,1,22);
    if copy(s,1,length(fStatistics.Label15.Caption))<>fStatistics.Label15.Caption then
    begin
      if Application.MessageBox(
        pchar(_l('#00680^eThis JTT file was made using different WAKAN.CHR version. Translation cannot be loaded.'#13#13'Do you want to continue?')),
        pchar(_l('#00090^eWarning')),
        MB_YESNO or MB_ICONWARNING)=idNo then
      begin
        closefile(f);
        exit;
      end;
      dot:=false;
    end;
    blockread(f,w,1);
    if w<>3294 then
    begin
      Application.MessageBox(
        pchar(_l('#00681^eThis JTT file was created by old version of WaKan.'#13'It is not compatible with the current version.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
      exit;
    end;
    blockread(f,w,1);
    blockread(f,wss,w);
    wss[w*2]:=#0;
    s:=wss;
    while (s<>'') and (s[1]<>'$') do
    begin
      s2:=copy(s,1,pos(',',s)-1);
      delete(s,1,pos(',',s));
      docdic.Add(s2);
    end;
  end;
  s:='';
  s3.Clear;
  if jtt then
  begin
    while not eof(f) do
    begin
      blockread(f,buf,16384,reat);
      if jtt then
        for i:=0 to (reat div 4)-1 do
        begin
          dp:=chr(buf[i*4] mod 256);
          if dp='$'then
          begin
            doc.Add(s);
            doctr.AddLine(s3);
            s:='';
            s3.Clear;
          end else
          begin
            wc:=widechar(buf[i*4+1]);
            l:=(buf[i*4+2] mod 256)*65536+buf[i*4+3];
            ls:=inttostr(l);
            while length(ls)<6 do ls:='0'+ls;
            s:=s+UnicodeToHex(wc);
            if length(dp+inttostr(buf[i*4] div 256)+ls+chr(buf[i*4+2] div 256))<>9 then begin
              showmessage('<<'+dp+'--'+inttostr(buf[i*4] div 256)+'--'+ls+'--'+chr(buf[i*4+2] div 256)+'>>');
            end;
            if not dot then
              s3.AddChar('-', 9, 0, 1)
            else
             //last param is apparently stored as character (ex. '1', '2') and we need it as int
              s3.AddChar(dp, buf[i*4] div 256, l, buf[i*4+2] div 256 - ord('0'));
          end;
        end;
    end;
    closefile(f);
    if s<>'' then
    begin
      doc.Add(s);
      doctr.AddLine(s3);
    end;
  end else
   //Not jtt
      LoadText(docfilename, tp, LoadAnnotMode);

  view:=0;
  curx:=0;
  cury:=0;
  mustrepaint:=true;
  ShowText(true);
  Screen.Cursor:=crDefault;
  FileChanged:=false;
end;

{ Doesn't save Filename, tp or AnnotMode choice. That is correct.
This function can be called by others to make one-time special-format save.
SaveAs does the choice remembering. }
procedure TfTranslate.SaveToFile(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
var f:file;
    i,j,bc:integer;
    buf:array[0..16383] of word;
    jtt:boolean;
    sig:word;
    s:string;
    l:integer;
    w:word;
  cp: PCharacterProps;
begin
  Screen.Cursor:=crHourGlass;
  assignfile(f,filename);
  rewrite(f,2);
  jtt:=pos('.WTT',UpperCase(filename))>0;
  if jtt then
  begin
   //TODO: Convert this subblock to unicode
    sig:=$f1ff;
    blockwrite(f,sig,1);
    s:='WaKan Translated Text>'+fStatistics.Label15.Caption;
    while length(s)<32 do s:=s+' ';
    blockwrite(f,s[1],16);
    s:='';
    for i:=0 to docdic.Count-1 do s:=s+docdic[i]+',';
    w:=3294;
    blockwrite(f,w,1);
    w:=(length(s)+1) div 2;
    blockwrite(f,w,1);
    s:=s+'$$$$';
    blockwrite(f,s[1],w);
    bc:=0;
    for i:=0 to doc.Count-1 do
    begin
      for j:=0 to (length(doc[i]) div 4)-1 do
      begin
        cp := @doctr[i].chars[j];
        buf[bc]:=ord(cp.wordstate)+cp.learnstate*256;
        l:=cp.dicidx;
        buf[bc+2]:=l div 65536+(Ord('0')+cp.docdic)*256; //apparently dic # is stored as a char ('1','2'...)
        buf[bc+3]:=l mod 65536;
        buf[bc+1]:=word(HexToUnicode(GetDoc(j,i))[1]);
        inc(bc,4);
        if bc=16384 then
        begin
          blockwrite(f,buf,bc);
          bc:=0;
        end;
      end;
      buf[bc]:=ord('$');
      buf[bc+1]:=0;
      buf[bc+2]:=0;
      buf[bc+3]:=0;
      inc(bc,4);
      if bc=16384 then
      begin
        blockwrite(f,buf,bc);
        bc:=0;
      end;
    end;
    blockwrite(f,buf,bc);
    bc:=0;
    closefile(f);
  end else begin
    SaveText(filename,tp,AnnotMode);
  end;
  Screen.Cursor:=crDefault;
  FileChanged:=false;
end;


//Receives a string of characters and their properties.
//Parses all aozora-ruby sequences and converts them to annotations, removing from the line
//Has no particular adherrence to TfTranslate, we should probably move it someplace else.
procedure TfTranslate.CollapseRuby(var s: FString; var sp: TCharacterLineProps);
var
  idx: integer; //current char index
  c: FChar; //current char

  explicitTextBreak: boolean;
  idxLastTextBreak: integer; //index of last ruby text break (|) on the current line, or 0
  idxRubyOpen: integer;
  inRubyText: boolean;

 //WARNING! Indexing in sp starts with 0, in s with 1, remember that while reading.

  { Tries to guess where's the start of the annotated word.
   Receives:
     idx -- index of a first character just before the annotation -- 0..Length(s)
   Returns: 1..idx-1
   When given 0, returns 0 which means ruby was the first char on the line. }
  function GuessLastTextBreak(curidx: integer): integer;
  begin
    while (curidx>1) and (sp.chars[curidx-1].wordstate='-') do //not yet annotated
    begin
     //Might do more guesswork in the future
      if EvalChar(fgetch(s, curidx))<>EvalChar(fgetch(s, curidx-1)) then
        break;
      Dec(curidx);
    end;
    Result := curidx;
  end;

  //Called to finalize currently open ruby
  procedure FinalizeRuby;
  var cp: PCharacterProps;
  begin
    if idxLastTextBreak<=0 then begin
     //this means ruby was the first char on the line, but we have to store it somehow
     //on save we'll ignore UH_RUBY_PLACEHOLDERs and only write ruby
      s := UH_RUBY_PLACEHOLDER + s;
      with sp.InsertChar(0)^ do
        Reset;
      idxLastTextBreak := 1;
      Inc(idx);
      Inc(idxRubyOpen);
    end else
    if explicitTextBreak then begin
     //Delete the char
      s := fcopy(s, 1, idxLastTextBreak-1) + fcopy(s, idxLastTextBreak+1, flength(s)-idxLastTextBreak);
      sp.DeleteChar(idxLastTextBreak-1);
      Dec(idx);
      Dec(idxRubyOpen);
    end;
    //Now idxLastTextBreak points to first character of an annotated word (or to <<AORUBY_OPEN)

    Assert(idxRubyOpen>=idxLastTextBreak);
    if idxRubyOpen=idxLastTextBreak then begin
     //Again, empty annotated expression
      s := fcopy(s, 1, idxLastTextBreak-1) + UH_RUBY_PLACEHOLDER + fcopy(s, idxLastTextBreak, flength(s)-idxLastTextBreak+1);
      with sp.InsertChar(idxLastTextBreak-1)^ do
        Reset; //init as default char
      Inc(idx);
      Inc(idxRubyOpen);
    end;
    //Now we have at least one character to be annotated

    //Copy annotation itself to char props..
    with sp.chars[idxLastTextBreak-1] do begin
      wordstate := 'F';
      ruby := fcopy(s, idxRubyOpen+1, idx-idxRubyOpen-1);
      if explicitTextBreak then
        rubyTextBreak := btBreak
      else
        rubyTextBreak := btSkip;
      flags := flags + [cfExplicitRuby, cfRoot];
    end;

   //..and delete it
    s := fcopy(s, 1, idxRubyOpen-1) + fcopy(s, idx+1, flength(s)-idx);
    sp.DeleteChars(idxRubyOpen-1, idx-idxRubyOpen+1);
    idx := idxRubyOpen-1;

   //idx is now at the last char of the word to annotate
   //first char is already marked, mark the rest as "word continuation"
    Inc(idxLastTextBreak);
    while idxLastTextBreak<=idx do begin
      with sp.chars[idxLastTextBreak-1] do begin
        wordstate := '<';
        flags := flags + [cfRoot];
      end;
      Inc(idxLastTextBreak);
    end;

   //for now we don't try to guess the tail of the word

   //reset everything
    explicitTextBreak := false;
    idxLastTextBreak := 0;
    idxRubyOpen := 0;
    inRubyText := false;
  end;

begin
  explicitTextBreak := false;
  idxLastTextBreak := 0;
  idxRubyOpen := 0;
  inRubyText := false;

  idx := 1;
  while idx<=flength(s) do begin
    c := fgetch(s,idx);

   //Inside of a ruby skip everything until close
    if inRubyText then begin
      if c=UH_AORUBY_CLOSE then
        FinalizeRuby;
    end else

   //Ruby text break
    if c=UH_AORUBY_TEXTBREAK then begin
      explicitTextBreak := true;
      idxLastTextBreak := idx;
     { And continue like nothing had happened.
      If we encounter another TEXTBREAK, that one will override this one,
      and this one will appear like a normal char.
      On the other hand, if we find <<ruby opener>> only then we'll go back and mark
      everything from here to there as "annotated". }
    end else

   //Ruby opener
    if c=UH_AORUBY_OPEN then
    begin
      if idxLastTextBreak<1 then
        idxLastTextBreak := GuessLastTextBreak(idx-1);
      idxRubyOpen := idx;
      inRubyText := true;
    end;

   { Ruby comments and tags are not parsed here.
    They are colored dynamically on render; see comments there. }

    Inc(idx);
  end;
end;

//Loads classic text file in any encoding.
procedure TfTranslate.LoadText(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
var c: FChar;
  cp: TCharacterProps;
  s: FString; //current line text
  sp: TCharacterLineProps; //current line props

  //Called before we go to the next line,
  //to save whatever needs saving and apply whatever needs applying.
  procedure FinalizeLine;
  begin
    if AnnotMode=amRuby then
      CollapseRuby(s, sp);
    doc.Add(s);
    doctr.AddLine(sp);
    s:='';
    sp.Clear;
  end;

begin
  s := '';
  sp.Clear;

  Conv_Open(filename,tp);
  c := Conv_ReadChar;
 {$IFDEF UNICODE}
  while c<>#$FFFF do
 {$ELSE}
  while c<>'' do
 {$ENDIF}
  begin
   //Default properties for this character
    cp.Reset;
    cp.SetChar('-', 9, 0, 1);
    cp.rubyTextBreak := btAuto;
    cp.ruby := '';
    cp.flags := [];

   //A linebreak breaks everything, even a ruby
    if c=UH_CR then
      FinalizeLine
    else

   //Normal symbol
    if c<>UH_LF then begin
      s:=s+c;
      sp.AddChar(cp);
    end;

    c := Conv_ReadChar;
  end;
  Conv_Close;
  if s<>'' then
    FinalizeLine();
end;

{
Ruby saving strategy:
- We always save "hard ruby" since if its present then we have loaded it from the file
- As for soft ruby, we save that to the file according to user's choice.
}

procedure TfTranslate.SaveText(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
var i,j,k: integer;
  inReading:boolean;
  meaning: string;
  reading,kanji:FString;
  rubyTextBreak: TRubyTextBreakType;
  rootLen: integer; //remaining length of the word's root, if calculated dynamically. <0 means ignore this check
  explicitRuby: boolean;

  procedure outp(s: FString);
  var i: integer;
  begin
    for i := 1 to flength(s) do
      Conv_WriteChar(fgetch(reading,i));
  end;

  procedure FinalizeRuby();
  begin
    if AnnotMode=amKana then begin
      if reading<>'' then
        reading:=UH_SPACE+reading
      else
       //Don't have a reading, write a char (maybe we didn't want it expanded)
        reading:=GetDoc(j,i);
      outp(reading);
    end else
    if (AnnotMode=amRuby) or explicitRuby then begin
     //We don't check that reading is not empty, because if it was empty
     //we wouldn't have set inReading, except if it was explicit ruby,
     //in which case we must write it even if empty.
      reading := UH_AORUBY_OPEN + reading + UH_AORUBY_CLOSE;
      outp(reading);
    end else
      Conv_WriteChar(GetDoc(j,i));
    inReading := false;
    reading := '';
    explicitRuby := false;
  end;

begin
  inReading := false;
  rootLen := -1;
  explicitRuby := true;

  Conv_Create(filename,tp);
  for i:=0 to doc.Count-1 do
  begin
    for j:=0 to flength(doc[i])-1 do
    begin
      if inReading then begin
        Dec(rootLen);
       //End of word
        if (doctr[i].chars[j].wordstate<>'<')
       //End of word root. That's where we output ruby and continue with just printing kana
        or (explicitRuby and not (cfRoot in doctr[i].chars[j].flags))
        or (rootLen=0) then begin
         //=> End of annotated chain
          FinalizeRuby;
         //and we continue through to the "no inReading" case where we might start a new chain
        end else begin
         //Inside of annotated chain
          if not (AnnotMode in [amKana]) then
            Conv_WriteChar(GetDoc(j,i));
         //in amKana we skip chars to be replaced
          continue; //handled this char
        end;
      end; //yep, no 'else'!

      if not inReading then begin
       //Explicit ruby load, even if ruby's empty
        if cfExplicitRuby in doctr[i].chars[j].flags then begin
          reading := doctr[i].chars[j].ruby;
          inReading := true;
          explicitRuby := true;
          rootLen := -1;
        end;

       //Implicit ruby load (if explicit is not loaded -- checked by inReading)
        if (AnnotMode in [amKana, amRuby]) and not inReading then begin
          GetTextWordInfo(j,i,meaning,reading,kanji);
          inReading := (reading<>'');
          explicitRuby := false;
          if inReading then begin
            rootLen := 0;
           //Explicit ruby has it from file, but with implicit we have to find the word root
            for k := j to min(j+flength(kanji), doctr[i].charcount) do
              if fgetch(kanji, k-j+1)=GetDoc(k,i) then
                Inc(rootLen)
              else
                break;
            if (rootLen=0)
           //we have found no match per kanji, let's try it the other way:
           //assume the whole word is matched and check if there's anything special to the reading
            and (reading<>fcopy(doc[i], j, j+flength(reading)))
            and (AnnotMode=amRuby) //better annotate something than nothing
              //in amKana it's the reverse: better replace nothing than too much
            then
              rootLen := -1; //annotate whole word

           //In these cases we also hide reading during the rendering.
           //I'm not yet sure what are these
            if (upcase(doctr[i].chars[j].wordstate)<>'F') and (upcase(doctr[i].chars[j].wordstate)<>'D') then
              rootLen := 0;
            if rootLen=0 then
              inReading := false;
          end;
        end;

       //Ruby break -- if we have some kind of reading
        if (AnnotMode<>amKana) and inReading then begin //We don't write ruby in kana mode at all
          rubyTextBreak := doctr[i].chars[j].rubyTextBreak;
          if rubyTextBreak=btAuto then
            if j<=0 then
              rubyTextBreak := btSkip //first char in a line
            else
              if EvalChar(GetDoc(j,i))=EvalChar(GetDoc(j-1,i)) then
                rubyTextBreak := btBreak //break when there are two characters of the same type in a row
              else
                rubyTextBreak := btSkip;
          if rubyTextBreak=btBreak then
            Conv_WriteChar(UH_AORUBY_TEXTBREAK);
        end;

        if ((AnnotMode<>amKana) or (reading=''))
        //placeholders are virtual
        and (GetDoc(j,i)<>UH_RUBY_PLACEHOLDER) then
          Conv_WriteChar(GetDoc(j,i));
      end;

    end;

    if inReading then
      FinalizeRuby;
    Conv_WriteChar(UH_CR);
    Conv_Write(UH_LF);
  end;
  Conv_Flush;
  Conv_Close;
end;

//Returns false if user have cancelled the dialog
function TfTranslate.SaveAs: boolean;
var tp: byte;
begin
 //If configured to, or if we have chosen this option before
  if (SaveAnnotMode=amRuby) or fSettings.cbSaveAnnotationsToRuby.Checked then
    SaveTextDialog.FilterIndex := 2 //"Text with readings as Aozora Ruby"
  else
    SaveTextDialog.FilterIndex := 1; //"Text file"

  Result := SaveTextDialog.Execute;
  if not Result then exit;

  case SaveTextDialog.FilterIndex of
    1: SaveAnnotMode := amDefault;
    2: SaveAnnotMode := amRuby;
   //WTT file option is handled differently, that's how it was inherited.
  end;

  //Choose encoding
  if pos('.WTT',uppercase(SaveTextDialog.FileName))=0 then
    tp:=Conv_ChooseType(curlang='c',0)
  else
    tp:=0; //WTT doesn't need types

  SaveToFile(SaveTextDialog.FileName,tp,SaveAnnotMode);
  docfilename:=SaveTextDialog.FileName;
  doctp:=tp;
  lblFilename.Caption:=uppercase(ExtractFilename(SaveTextDialog.FileName));
end;

procedure TfTranslate.SetFileChanged(Value: boolean);
begin
  FFileChanged:=Value;
  sbFileSave.Enabled:=Value;
  fMenu.aEditorSave.Enabled:=Value;
end;

{
This function saves the changes to the file, possibly interacting with the user.
It has three possible outcomes:
1. "Yes": No changes or changes saved, dirty flag cleared.
  Result = true
2. "No": Changes not saved, do not ask again (dirty flag cleared).
  Result = true
3. "Cancel": Changes not saved, dirty flag not cleared, operation cancelled.
  Result = false
}
function TfTranslate.CommitFile:boolean;
var i:integer;
begin
  Result := true;
  if not filechanged then exit;

  if (fSettings.CheckBox60.Checked) and (docfilename<>'') then begin
   //Auto-"Yes"
    SaveToFile(docfilename,doctp,SaveAnnotMode);
    filechanged := false;
    exit;
  end;

  if fSettings.cbNoSaveChangesWarning.Checked then begin
   //We've been asked not to bother the user with save warnings. So "No".
    filechanged := false;
    exit;
  end;

  i:=Application.MessageBox(pchar(_l('#00687^eDocument has been changed. Do you want to save it?')),
   pchar(_l('#00090^eWarning')),MB_ICONWARNING or MB_YESNOCANCEL);
  if i<>idYes then begin
   //"No" or "Cancel"
    if i=idCancel then Result:=false;
    if i=idNo then filechanged := false;
    exit;
  end;

  if docfilename<>'' then begin
   //"Yes"
    SaveToFile(docfilename,doctp,SaveAnnotMode);
    filechanged := false;
    exit;
  end;

  if not SaveAs then begin
   //"Cancel" through cancelling dialog
    Result := false;
    exit;
  end;

 //"Yes"
  filechanged:=false;
  Result := true;
end;


procedure TfTranslate.Button2Click(Sender: TObject);
begin
  if not CommitFile then exit;
  doc.Clear;
  doctr.Clear;
  docdic.Clear;
  linl.Clear;
  curx:=0;
  cury:=0;
  view:=0;
  lblFilename.Caption:=_l('#00678^e<UNNAMED>');
  docfilename:='';
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfTranslate.Button3Click(Sender: TObject);
var tp:byte;
begin
  if not CommitFile then exit;
  if OpenTextDialog.Execute then
  begin
    tp:=Conv_ChooseType(curlang='c',Conv_DetectType(OpenTextDialog.FileName));
    if tp=0 then exit;
    OpenFile(OpenTextDialog.Filename, tp);
  end;
end;

procedure TfTranslate.Button5Click(Sender: TObject);
begin
  if docfilename<>'' then
    SaveToFile(docfilename,doctp,SaveAnnotMode)
  else
    SaveAs;
end;

procedure TfTranslate.Button6Click(Sender: TObject);
begin
  Button2Click(Sender);
end;

procedure TfTranslate.Button7Click(Sender: TObject);
var i,j:integer;
  a:integer;
  oldcurx,oldcury:integer;
  bg,en:integer;

  sp: TSMPromptForm;
  updateProgressEvery: integer;
  lastUpdateProgress: integer;
  startTime: cardinal;

  st: TLookSettings;
begin
  oldcurx:=rcurx;
  oldcury:=rcury;
  if (blockx=rcurx) and (blocky=rcury) then
  begin
    if not fSettings.cbTranslateNoLongTextWarning.Checked then
      if Application.MessageBox(
        pchar(_l('#00682^eNo block is selected. Do you want generate translation for entire document?'#13#13
          +'This action can take a very long time.')),
        pchar(_l('#00683^eConfirmation')),
        MB_ICONWARNING or MB_YESNO)<>idYes then exit;
    blockfromx:=0;
    blockfromy:=0;
    blocktox:=flength(doc[doc.Count-1])-1;
    blocktoy:=doc.Count-1;
  end else CalcBlockFromTo(true);
  Screen.Cursor:=crHourGlass;

 //We don't want to update very often since redrawing is slow.
 //Let's only update on every percent or less.
  updateProgressEvery := (blocktoy-blockfromy+1) div 100;
  if updateProgressEvery<1 then updateProgressEvery := 1; //<100 items
  lastUpdateProgress := 0; //whatever, we'll set it when we show the window
 //We also don't show the window at all unless the operation is taking long time.
  sp := nil;
  startTime := GetTickCount;

 //Setup everything for translation
  fUser.SpeedButton1.Down:=false;
  fUser.SpeedButton2.Down:=false;
  fUser.SpeedButton3.Down:=false;
  fUser.Look_Setup(4, st);

  for i:=blockfromy to blocktoy do
  begin
    bg:=0;
    en:=flength(doc[i])-1;
    if i=blockfromy then bg:=blockfromx;
    if i=blocktoy then en:=blocktox;

    //If the operation is taking too long to be noticeable
    if (sp=nil) and (GetTickCount-startTime > 200) then begin
     //Bring up the progress window
      sp:=SMProgressDlg(_l('#00684^eTranslator'),
        _l('#00685^eTranslating...'),blocktoy-blockfromy+1);
      lastUpdateProgress := -updateProgressEvery-1; //update right now
    end;

    //Do not update progress too often
    if i-blockfromy > lastUpdateProgress + updateProgressEvery then begin
      if sp<>nil then
        sp.SetProgress(i-blockfromy);
      lastUpdateProgress := i-blockfromy;
    end;

    if sp<>nil then
      sp.ProcessModalMessages;

    j:=bg;
    while j<=en do
      if IsLocaseLatin(doctr[i].chars[j].wordstate) then
      begin
        inc(j);
        while doctr[i].chars[j].wordstate='<'do inc(j);
      end else
      begin
        rcurx:=j;
        rcury:=i;
        dic_ignorekana:=true;
        fUser.Look_Run(4, @st, {NoGridDisplay=}true, {NoScreenUpdates=}true);
        dic_ignorekana:=false;
        a:=SetWordTrans(j,i,true,true,false);
        if a=0 then a:=1;
        inc(j,a);
      end;
  end;
  sp.Free;
  rcurx:=oldcurx;
  rcury:=oldcury;
  mustrepaint:=true;
  ShowText(true);
  Screen.Cursor:=crDefault;
end;

procedure TfTranslate.Button8Click(Sender: TObject);
begin
  if (blockx=rcurx) and (blocky=rcury) then
  begin
    SetWordTrans(rcurx,rcury,true,false,true);
    mustrepaint:=true;
    ShowText(true);
  end else
    Button7Click(Sender);
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.PageControl1.ActivePage:=fSettings.TabSheet9;
  fSettings.ShowModal;
end;

procedure TfTranslate.Button9Click(Sender: TObject);
begin
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00686^eTranslated text'));
end;


procedure TfTranslate.CheckBox1Click(Sender: TObject);
begin
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfTranslate.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
procedure recalcy(oldy,newy:integer);
begin
  curx:=WidthToPos(PosToWidth(curx,oldy),newy);
  cury:=newy;
end;
var bx,by:integer;
    ukn:boolean;
begin
  bx:=curx; by:=cury;
  if (insx=-1) or (insconfirmed) then
  begin
    ukn:=false;
    if key=VK_RIGHT then
    begin
      inc(curx);
      if curx>=linl[cury].len then
        if cury+1<linl.Count then
        begin
          inc(cury);
          curx:=0;
        end else dec(curx);
      cursorend:=false;
    end
    else if key=VK_LEFT then
    begin
      dec(curx);
      if curx<0 then
        if cury>0 then
        begin
          dec(cury);
          curx:=linl[cury].len-1;
        end else inc(curx);
      cursorend:=false;
    end
    else if key=VK_UP then recalcy(cury,cury-1)
    else if key=VK_DOWN then recalcy(cury,cury+1)
    else if (key=VK_PRIOR) and (ssCtrl in Shift) then
    begin
      curx:=0;
      cury:=0;
    end
    else if (key=VK_NEXT) and (ssCtrl in Shift) then
    begin
      cury:=linl.Count-1;
      curx:=100;
    end
    else if key=VK_PRIOR then recalcy(cury,cury-printl)
    else if key=VK_NEXT then recalcy(cury,cury+printl)
    else if key=VK_HOME then
    begin
      if (cursorend) and (cury>0) then dec(cury) else curx:=0;
      cursorend:=false;
    end
    else if key=VK_END then
    begin
      if not cursorend then curx:=100;
    end
    else if key=VK_DELETE then
    begin
      ResolveInsert(true);
      if (blockx<>rcurx) or (blocky<>rcury) then BlockOp(false,true) else DeleteCharacter(rcurx,rcury);
      RefreshLines;
    end else ukn:=true;
    if not ukn then
    begin
      ClearInsBlock;
      leaveline:=true;
      if ssShift in Shift then shiftpressed:=true;
      if (bx<>curx) or (by<>cury) then ResolveInsert(true);
      if (bx<>curx) or (by<>cury) then ShowText(true);
      leaveline:=false;
    end;
  end else
  with fUser do
  begin
    if (key=VK_UP) and (StringGrid1.Row>1) then StringGrid1.Row:=StringGrid1.Row-1;
    if (key=VK_DOWN) and (StringGrid1.Row<StringGrid1.RowCount-1) then StringGrid1.Row:=StringGrid1.Row+1;
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (insx<>-1) then ShowHint else HideHint;
  end;
end;

procedure TfTranslate.EditorPaintBoxClick(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfTranslate.EditorPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  leaveline:=true;
  resolvebuffer:=false;
  shiftpressed:=false;
  if insertbuffer<>'' then ResolveInsert(true);
  ClearInsBlock;
  curx:=x div (lastxsiz);
  cury:=y div (lastxsiz*lastycnt)+view;
  curx:=WidthToPos(curx,cury);
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfTranslate.EditorPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin;
    curx:=(x+lastxsiz div 2) div (lastxsiz);
    cury:=y div (lastxsiz*lastycnt)+view;
    curx:=WidthToPos(curx,cury);
    if (curx=lastmmx) and (cury=lastmmy) then exit;
    lastmmx:=curx;
    lastmmy:=cury;
    shiftpressed:=true;
    ShowText(false);
  end;

  fMenu.IntTipPaintOver(EditorPaintBox,x,y,false);
end;

procedure TfTranslate.EditorPaintBoxDblClick(Sender: TObject);
begin
  if not fKanjiDetails.Visible then
    fMenu.aKanjiDetailsExecute(nil);
end;

procedure TfTranslate.EditorPaintBoxPaint(Sender: TObject);
begin
  MakeEditorBitmap;
end;

procedure TfTranslate.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  InsertCharacter(key);
end;

procedure TfTranslate.sbDisplayReadingClick(Sender: TObject);
begin
  fMenu.aEditorReading.Checked:=sbDisplayReading.Down;
  Self.CheckBox1Click(Sender);
end;

procedure TfTranslate.sbDisplayMeaningClick(Sender: TObject);
begin
  fMenu.aEditorMeaning.Checked:=sbDisplayMeaning.Down;
  Self.CheckBox1Click(Sender);
end;

procedure TfTranslate.sbClearTranslationClick(Sender: TObject);
var i,j:integer;
    bg,en:integer;
begin
  CalcBlockFromTo(true);
  for i:=blockfromy to blocktoy do
  begin
    bg:=0;
    en:=flength(doc[i])-1;
    if i=blockfromy then bg:=blockfromx;
    if i=blocktoy then en:=blocktox;
    for j:=bg to en do
      with doctr[i].chars[j] do begin
        wordstate := '-';
        learnstate := 9;
        dicidx := 0;
        docdic := 1;
      end;
  end;
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfTranslate.sbAutoTranslateClick(Sender: TObject);
begin
  Self.Button7Click(Sender);
end;

procedure TfTranslate.sbSetTranslationClick(Sender: TObject);
begin
  Self.Button8Click(Sender);
end;

procedure TfTranslate.sbPrintClick(Sender: TObject);
begin
  Self.Button9Click(Sender);
end;

procedure TfTranslate.sbFileOpenClick(Sender: TObject);
begin
  Self.Button3Click(Sender);
end;

procedure TfTranslate.sbFileSaveClick(Sender: TObject);
begin
  Self.Button5Click(Sender);
end;

procedure TfTranslate.sbFileNewClick(Sender: TObject);
begin
  Self.Button2Click(Sender);
end;

procedure TfTranslate.sbClipCutClick(Sender: TObject);
begin
  BlockOp(true,true);
end;

procedure TfTranslate.sbClipCopyClick(Sender: TObject);
begin
  BlockOp(true,false);
end;

procedure TfTranslate.sbClipPasteClick(Sender: TObject);
begin
  PasteOp;
end;

procedure TfTranslate.sbKanjiModeClick(Sender: TObject);
begin
  sbKanjiMode.Down:=true;
  fMenu.aEditorKanjiMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfTranslate.sbKanaModeClick(Sender: TObject);
begin
  sbKanaMode.Down:=true;
  fMenu.aEditorKanaMode.Checked:=true;
  fMenu.aEditorKanjiMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfTranslate.sbAsciiModeClick(Sender: TObject);
begin
  sbAsciiMode.Down:=true;
  fMenu.aEditorASCIIMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorKanjiMode.Checked:=false;
end;

procedure TfTranslate.FormActivate(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfTranslate.sbSmallFontClick(Sender: TObject);
begin
  fMenu.aEditorSmallFont.Checked:=true;
  fMenu.aEditorLargeFont.Checked:=false;
  fMenu.aEditorMedFont.Checked:=false;
  RefreshLines;
  //Self.CheckBox1Click(Sender);
end;

procedure TfTranslate.sbLargeFontClick(Sender: TObject);
begin
  fMenu.aEditorSmallFont.Checked:=false;
  fMenu.aEditorLargeFont.Checked:=true;
  fMenu.aEditorMedFont.Checked:=false;
  RefreshLines;
  //Self.CheckBox1Click(Sender);
end;

procedure TfTranslate.sbMiddleFontClick(Sender: TObject);
begin
  fMenu.aEditorSmallFont.Checked:=false;
  fMenu.aEditorLargeFont.Checked:=false;
  fMenu.aEditorMedFont.Checked:=true;
  RefreshLines;
  //Self.CheckBox1Click(Sender);
end;

procedure TfTranslate.EditorScrollBarChange(Sender: TObject);
begin
  view:=EditorScrollBar.Position;
  if (view>=0) and (view<linl.Count) then
  begin
    rviewx:=linl[view].xs;
    rviewy:=linl[view].ys;
  end;
  MakeEditorBitmap;
end;

procedure TfTranslate.FormDeactivate(Sender: TObject);
begin
  if fHint.Visible then fUser.HideHint;
end;

procedure TfTranslate.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  HandleWheel(false);
  handled:=true;
end;

procedure TfTranslate.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  HandleWheel(true);
  handled:=true;
end;

procedure TfTranslate.sbDockKanjiDetailsClick(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfTranslate.sbDockDictionaryClick(Sender: TObject);
begin
  fMenu.TabControl1Change(sender);
end;

procedure TfTranslate.ListBox1Enter(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=CopyShort;
  fMenu.aEditorCut.ShortCut:=CutShort;
  fMenu.aEditorPaste.ShortCut:=PasteShort;
  fMenu.aEditorSelectAll.ShortCut:=AllShort;
end;

procedure TfTranslate.ListBox1Exit(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;
end;

procedure TfTranslate.sbUseTlColorsClick(Sender: TObject);
begin
  fMenu.aEditorCOlors.Execute;
end;

procedure TfTranslate.BlinkCursorTimerTimer(Sender: TObject);
begin
  if Self.Visible then DrawCursor(true);
end;

procedure TfTranslate.SelectAll;
begin
  blockx:=0;
  blocky:=0;
  cury:=linl.Count-1;
  curx:=linl[linl.Count-1].len;
  shiftpressed:=true;
  ShowText(true);
end;

procedure TfTranslate.MakeEditorBitmap;
begin
  editorbitmap.Free;
  editorbitmap:=TBitmap.Create;
  editorbitmap.Width:=EditorPaintBox.Width;
  editorbitmap.Height:=EditorPaintBox.Height;
  RenderText(rviewx,rviewy,editorbitmap.Canvas,2,2,EditorPaintBox.Width-4,
    EditorPaintBox.Height-4,linl,printl,lastxsiz,lastycnt,false,false);
  oldcurx:=-1;
  oldcury:=-1;
  oldblockfromx:=-1;
  DrawBlock(EditorBitmap.Canvas);
  PasteEditorBitmap;
  DrawCursor(false);
end;

procedure TfTranslate.PasteEditorBitmap;
begin
  EditorPaintBox.Canvas.Draw(0,0,EditorBitmap);
end;

procedure TfTranslate.HandleWheel(down:boolean);
begin
  if not EditorScrollBar.Enabled then exit;
  if down then inc(view) else dec(view);
  if (view>=0) and (view<=EditorScrollBar.Max) then
  begin
    rviewx:=linl[view].xs;
    rviewy:=linl[view].ys;
  end else
    if view<0 then view:=0 else view:=EditorScrollBar.Max;
  EditorScrollBar.Position:=view;
  MakeEditorBitmap;
end;

procedure TfTranslate.ShowText(dolook:boolean);
var oldview:integer;
  s:string;
  wt:integer;
begin
  if not Visible then exit;
  oldview:=view;
  RenderText(-1,-1,EditorPaintBox.Canvas,0,0,EditorPaintBox.Width-4,
    EditorPaintBox.Height-4,linl,printl,lastxsiz,lastycnt,false,true);
  if linl.Count=0 then
  begin
    rcurx:=-1;
    rcury:=-1;
    rviewx:=0;
    rviewy:=0;
    EditorPaintBox.Invalidate;
    EditorScrollBar.Enabled:=false;
    exit;
  end;
  if cury<0 then cury:=0;
  if cury>=linl.Count then
  begin
    cury:=linl.Count-1;
    curx:=2555;
  end;
  if curx<0 then curx:=0;
  if (cursorend) and (curx=0) and (cury>0) and (linl[cury-1].ys<>linl[cury].ys) then
  begin
    dec(cury);
    curx:=linl[cury].len-1;
    cursorend:=false;
  end;
  if curx>=linl[cury].len then
  begin
    if (cury+1<linl.Count) and (linl[cury].ys=linl[cury+1].ys) then
    begin
      curx:=0;
      inc(cury);
      cursorend:=true;
    end else curx:=linl[cury].len-1;
  end;
  if view>cury then if cury>0 then view:=cury else view:=0;
  if view+printl-1<cury then view:=cury-printl+1;
  if view+printl-1>=linl.Count then view:=linl.Count-printl;
  if view<0 then view:=0;
  if view>=linl.Count then view:=0;
  rcury:=linl[cury].ys;
  rcurx:=curx+linl[cury].xs;
  rviewx:=linl[view].xs;
  rviewy:=linl[view].ys;
  if not shiftpressed then
  begin
    blockx:=rcurx;
    blocky:=rcury;
  end;
  fUser.SpeedButton1.Down:=false;
  fUser.SpeedButton2.Down:=false;
  fUser.SpeedButton3.Down:=false;
  if (dolook) and ((fUser.Visible) or (insertBuffer<>'')) then fUser.Look(false) else
  if dolook then begin
    s:=GetDocWord(rcurx,rcury,wt,false);
    if flength(s)>=1 then fKanjiDetails.SetCharDetails(fgetch(s,1));
  end;
  if oldview<>view then mustrepaint:=true;
  if mustrepaint then MakeEditorBitmap else
  begin
    DrawCursor(false);
    DrawBlock(EditorPaintBox.Canvas);
  end;
  if mustrepaint then oldblockfromx:=-1;
  mustrepaint:=false;
  shiftpressed:=false;
  if linl.Count-printl<=0 then
    EditorScrollBar.Enabled:=false
  else
  begin
    EditorScrollBar.Min:=0;
    EditorScrollBar.Max:=linl.Count-printl;
    EditorScrollBar.Position:=view;
    EditorScrollBar.Enabled:=true;
  end;
  with fUser do
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (insx<>-1) then ShowHint else HideHint;
  ListBox1.SetFocus;
end;

procedure TfTranslate.CalcBlockFromTo(backtrack:boolean);
begin
  if (rcury<blocky) or ((rcury=blocky) and (rcurx<blockx)) then
  begin
    blockfromx:=rcurx;
    blockfromy:=rcury;
    blocktox:=blockx;
    blocktoy:=blocky;
  end else
  begin
    blockfromx:=blockx;
    blockfromy:=blocky;
    blocktox:=rcurx;
    blocktoy:=rcury;
  end;
  if backtrack then
  begin
    while doctr[blockfromy].chars[blockfromx].wordstate='<'do dec(blockfromx);
    while doctr[blocktoy].chars[blocktox+1].wordstate='<'do inc(blocktox);
  end;
end;



procedure FixReading(gd0,gd1,gd2:FChar; var reading:FString);
var gd: FString;
begin
  if (EvalChar(gd1)=EC_KATAKANA) and not showroma then
    reading := RomajiToKana('H'+KanaToRomaji(gd1,1,'j'),1,true,'j');
  if (EvalChar(gd1) in [EC_HIRAGANA, EC_KATAKANA]) and showroma then
  begin
    gd := '';
   {$IFNDEF UNICODE}
    if (gd1='30C3') or (gd1='3063') then gd:='' else
    if ((gd0='30C3') or (gd0='3063')) and
       ((gd2='3041') or (gd2='3043') or (gd2='3045') or (gd2='3047') or (gd2='3049') or
       (gd2='3083') or (gd2='3085') or (gd2='3087') or
       (gd2='30A1') or (gd2='30A3') or (gd2='30A5') or (gd2='30A7') or (gd2='30A9') or
       (gd2='30E3') or (gd2='30E5') or (gd2='30E7')) then gd:=gd0+gd1+gd2 else
    if (gd0='30C3') or (gd0='3063') then gd:=gd0+gd1 else
    if (gd2='3041') or (gd2='3043') or (gd2='3045') or (gd2='3047') or (gd2='3049') or
       (gd2='3083') or (gd2='3085') or (gd2='3087') or
       (gd2='30A1') or (gd2='30A3') or (gd2='30A5') or (gd2='30A7') or (gd2='30A9') or
       (gd2='30E3') or (gd2='30E5') or (gd2='30E7') then gd:=gd1+gd2 else
    if (gd1='3041') or (gd1='3043') or (gd1='3045') or (gd1='3047') or (gd1='3049') or
       (gd1='3083') or (gd1='3085') or (gd1='3087') or
       (gd1='30A1') or (gd1='30A3') or (gd1='30A5') or (gd1='30A7') or (gd1='30A9') or
       (gd1='30E3') or (gd1='30E5') or (gd1='30E7') then gd:='';
   {$ELSE}
    if (gd1=#$30C3) or (gd1=#$3063) then gd:='' else
    if ((gd0=#$30C3) or (gd0=#$3063)) and
       ((gd2=#$3041) or (gd2=#$3043) or (gd2=#$3045) or (gd2=#$3047) or (gd2=#$3049) or
       (gd2=#$3083) or (gd2=#$3085) or (gd2=#$3087) or
       (gd2=#$30A1) or (gd2=#$30A3) or (gd2=#$30A5) or (gd2=#$30A7) or (gd2=#$30A9) or
       (gd2=#$30E3) or (gd2=#$30E5) or (gd2=#$30E7)) then gd:=gd0+gd1+gd2 else
    if (gd0=#$30C3) or (gd0=#$3063) then gd:=gd0+gd1 else
    if (gd2=#$3041) or (gd2=#$3043) or (gd2=#$3045) or (gd2=#$3047) or (gd2=#$3049) or
       (gd2=#$3083) or (gd2=#$3085) or (gd2=#$3087) or
       (gd2=#$30A1) or (gd2=#$30A3) or (gd2=#$30A5) or (gd2=#$30A7) or (gd2=#$30A9) or
       (gd2=#$30E3) or (gd2=#$30E5) or (gd2=#$30E7) then gd:=gd1+gd2 else
    if (gd1=#$3041) or (gd1=#$3043) or (gd1=#$3045) or (gd1=#$3047) or (gd1=#$3049) or
       (gd1=#$3083) or (gd1=#$3085) or (gd1=#$3087) or
       (gd1=#$30A1) or (gd1=#$30A3) or (gd1=#$30A5) or (gd1=#$30A7) or (gd1=#$30A9) or
       (gd1=#$30E3) or (gd1=#$30E5) or (gd1=#$30E7) then gd:='';
   {$ENDIF}
    if EvalChar(fgetch(gd,1))=3 then gd:=RomajiToKana('H'+KanaToRomaji(gd,1,'j'),1,true,'j');
    reading:=gd;
  end;
 {$IFNDEF UNICODE}
  if (gd1='30FC') then reading:='30FC';
 {$ELSE}
  if (gd1=#$30FC) then reading:=#$30FC;
 {$ENDIF}
end;

{
x, y:
  start point in logical line coordinates (line:first character)
l, t, w, h:
  left, top, width, height of the block to draw in on the canvas
ll:
  graphical line list (all lines for this control)
printl (out):
  total number of lines printed
}
procedure TfTranslate.RenderText(x,y:integer;canvas:TCanvas;l,t,w,h:integer;
  ll:TGraphicalLineList;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
var st0,lst0,st2,st3:boolean;
    linec,linec2,lineh,screenh,screenw,lines:integer;
    st2c:integer;
    rs:integer;
    vert:boolean; //vertical printing. Only for printing.
    cl,cx,cxsc,cy,px,py,wx,wxl:integer;
    kanaq:FString;
    undersolid:boolean;
    color,fcolor:TColor;
    boldness:boolean;
    meaning:string;
    reading,kanji:FString;
    learnstate:integer;
    wordstate,lastwordstate:char;
    kanjilearned:boolean;
    cnty,cntx:integer;
    realx,realy,realx2,realy2:integer;
    we:integer;
    rect:TRect;
    cont:boolean;
    i:integer;
    invert:boolean;
    dnam:string;
    dic:TJaletDic;
    markers:string;
    doall:boolean;
    insval:integer;
    worddict:integer; //word's dicidx
    lastworddict:integer; //last word's dicidx
    colback, coltext:TColor; //standard colors for editor's text and background
    gd1,gd2,gd0:string;
    a:integer;

  inRubyTag: boolean;
  inRubyComment: boolean;

  function RecodeChar(ch:FChar):FChar;
  begin
   {$IFNDEF UNICODE}
    if ch='FF00' then ch:='0020';
   {$ELSE}
    if ch=#$FF00 then ch:=#$0020;
   {$ENDIF}
    result:=ch;
  end;

 //Traces the logical string back until the first character which provides wordstate and dictionary.
 //Stores the result in lastwordstate, lastworddict
  procedure FindLastWordState(cl: integer);
  var xp, yp: integer;
    s: PCharacterProps;
  begin
    xp := ll[cl].xs;
    yp := ll[cl].ys;
    s := @doctr[yp].chars[xp];
    while (xp>0) and (s.wordstate='<') do begin
      Dec(xp);
      s := @doctr[yp].chars[xp];
    end;
    lastwordstate := s.wordstate;
    lastworddict := s.dicidx;
  end;

  //Goes back the line and checks if we're ALREADY inside a given tag (this symbol notwithstanding).
  //Note that ruby tags spanning multiple lines are not supported
  function IsInRubyTag(xp, yp: integer; tagOpenSymbol, tagCloseSymbol: FChar): boolean;
  var s: string;
  begin
    s := doc[yp];
    Dec(xp); //once, because the current symbol doesn't matter
    while (xp>0) and (s[xp]<>tagOpenSymbol) and (s[xp]<>tagCloseSymbol) do
      Dec(xp);
    Result := (xp>0) and (s[xp]=tagOpenSymbol);
  end;

begin
  colback:=Col('Editor_Back');
  coltext:=Col('Editor_Text');
  if doc.Count=0 then
  begin
    doc.Add('');
    doctr.AddNewLine();
  end;
  if printing then st2:=fSettings.cbPrintMeaning.Checked else st2:=sbDisplayMeaning.Down;
  lst0:=false;
  if fSettings.CheckBox56.Checked then lst0:=true;
  if printing then st0:=fSettings.cbPrintReading.Checked else st0:=sbDisplayReading.Down;
  st3:=fSettings.CheckBox42.Checked;
  linec:=2;
  if not TryStrToInt(fSettings.Edit17.Text, st2c) then
    st2c:=1;
  if st3 then inc(linec);
  if st0 or lst0 then inc(linec);
  if st2 then inc(linec,st2c);
  vert:=fSettings.cbVerticalPrint.Checked and printing;
  if vert then screenh:=w else screenh:=h;
  if vert then screenw:=h else screenw:=w;
  if not printing then
  begin
    if sbLargeFont.Down then rs:=16 else if sbMiddleFont.Down then rs:=10 else rs:=8;
  end else
  begin
    if not TryStrToInt(fSettings.Edit18.Text, lineh) then
      lineh:=20;
    rs:=screenh div lineh div linec;
  end;
  a:=GetTickCount;
  cx:=0;
  cy:=0;
  doall:=ll.Count=0;
  insval:=-1;
  try
  if doall then
  begin
    if not doall then
    begin
      cy:=y;
      i:=0;
      while i<ll.Count do
      begin
        if ll[i].ys=y then
        begin
          if insval=-1 then insval:=i;
          ll.Delete(i);
        end else inc(i);
      end;
    end;
    while ((cy=y) or (doall)) and (cy<doc.Count) do
    begin
      cont:=true;
      px:=0;
      wxl:=cx;
      while px<=screenw do
      begin
        if (not vert) and (IsHalfWidth(wxl,cy)) then inc(px,rs) else inc(px,rs*2);
//        inc(px,rs*2);
        inc(wxl);
      end;
      dec(wxl);
      wx:=flength(doc[cy]);
      if wx>wxl then wx:=wxl else wx:=wx+1;
      if (wx<=cx) then
      begin
        inc(cy);
        cx:=0;
        px:=0;
        cont:=false;
      end;
      if cont then
      begin
        if cy>=doc.Count then showmessage('Internal line-computing error!');
        if doall then
          ll.Add(cx, cy, wx-cx)
        else
          ll.Insert(insval, cx, cy, wx-cx);
        cx:=wx;
      end;
    end;
  end;
  except
    showmessage('Line count exception: '+(ExceptObject as Exception).Message);
  end;
  printl:=screenh div (rs*linec);
  xsiz:=rs;
  ycnt:=linec;
  if onlylinl then exit;

 //Find a graphical line which covers the starting point
  for i:=0 to ll.Count-1 do
    if (ll[i].ys=y) and (ll[i].xs<=x) then cl:=i;

  lineh:=rs;
  cx:=x;
  cy:=y;
  py:=0;
  kanaq:='';

 { Last character's dictionary and word state
  If next character has '<' as a wordstate, we extend these to it. }
  lastwordstate := '-';
  lastworddict := 0;

 { If we're starting from the middle of a paragraph, go back until we find a suitable wordstate }
  if ll[cl].xs > 0 then begin
    FindLastWordState(cl);
    inRubyTag := IsInRubyTag(cx+1,cy,UH_AORUBY_TAG_OPEN,UH_AORUBY_TAG_CLOSE);
    inRubyComment := IsInRubyTag(cx+1,cy,UH_AORUBY_COMM_OPEN,UH_AORUBY_COMM_CLOSE);
  end else begin
    inRubyTag := false;
    inRubyComment := false;
  end;
  //else we except first character of a paragraph to not be '<'

  if printing then
    Canvas.Brush.Color:=clWhite
  else
  if fSettings.cbNoColors.Checked then
    Canvas.Brush.Color:=clWindow
  else
    Canvas.Brush.Color:=colBack;
  rect.Left:=l-2;
  rect.Top:=t-2;
  rect.Right:=l+w+4;
  rect.Bottom:=t+h+4;
  Canvas.FillRect(rect);
  try
  while (py{+lineh*linec}<screenh) and (cl<ll.Count) do
  begin
    cx:=ll[cl].xs;
    cy:=ll[cl].ys;
    wx:=cx+ll[cl].len;
{    if (fSettings.cbDisplayLines.Checked) and (st2) then
    begin
      linec2:=linec;
      if st3 then dec(linec2);
      if vert then
      begin
        canvas.MoveTo(w-py-rs*linec2+l+1,t);
        canvas.LineTo(w-py-rs*linec2+l+1,t+(wx-cx)*rs*2);
      end else
      begin
        canvas.MoveTo(l,py+rs*linec2+t-1);
        canvas.LineTo(l+(wx-cx)*rs*2,py+rs*linec2+t-1);
      end;
    end; }
    while (cx<wx) and ((cx<flength(doc[cy])) or ((kanaq<>'') and st0)) do
    try
      wordstate:=doctr[cy].chars[cx].wordstate;
      learnstate:=doctr[cy].chars[cx].learnstate;
      CalcBlockFromTo(false);

      GetTextWordInfo(cx,cy,meaning,reading,kanji);
      if cfExplicitRuby in doctr[cy].chars[cx].flags then
        reading := doctr[cy].chars[cx].ruby; //replacing GetTextWordInfo's reading

      kanjilearned:=(FirstUnknownKanjiIndex(kanji)<0);
      worddict:=doctr[cy].chars[cx].dicidx;
      if wordstate='<'then begin
        worddict:=lastworddict;
        wordstate:=lastwordstate;
      end;
      lastwordstate:=wordstate;
      lastworddict:=worddict;
      undersolid:=worddict<>0;
      if (upcase(wordstate)<>'F') and (upcase(wordstate)<>'D') then reading:='';

      if (fSettings.CheckBox36.Checked) then
        FixReading(GetDoc(cx-1,cy),GetDoc(cx,cy),GetDoc(cx+1,cy),reading);

      if doc[cy][cx+1]=UH_AORUBY_TAG_OPEN then
        inRubyTag := true;
      if doc[cy][cx+1]=UH_AORUBY_COMM_OPEN then
        inRubyComment := true;
     //will check for closers after we draw current symbol as is (closers are still inside the tag)

      if not fSettings.cbDisplayLines.Checked then undersolid:=false;
      if fSettings.cbNoColors.Checked then color:=clWindow else color:=colBack;
      if fSettings.cbNoColors.Checked then fcolor:=clWindowText else fcolor:=colText;
      if printing then color:=clWhite;
      if not fSettings.cbNoColors.Checked then begin
        if printing and fSettings.cbNoPrintColors.Checked then begin
          case upcase(wordstate) of
            '-','X':color:=$00FFFFFF;
            '?':color:=$00FFFFFF;
            'P':color:=$00FFFFFF;
            'I':color:=$00FFFFFF;
            'F':color:=$00FFFFFF;
            'D':color:=$00FFFFFF;
            'H':color:=$00FFFFFF;
            'K':color:=$00FFFFFF;
          end;
        end else
        if fMenu.aEditorColors.Checked then begin
          case upcase(wordstate) of
            '-','X':color:=Col('Editor_Untranslated');
            '?':color:=Col('Editor_NotFound');
            'P':color:=Col('Editor_Particle');
            'I':color:=Col('Editor_Untranslated');
            'F':color:=Col('Editor_Translated');
            'D':color:=Col('Editor_Translated');
            'H':color:=Col('Editor_Translated');
            'K':color:=Col('Editor_Translated');
          end;
        end else
          color:=Col('Editor_Untranslated');
        if inRubyTag then fcolor:=Col('Editor_AozoraTag');
        if inRubyComment then fcolor:=Col('Editor_AozoraComment');
      end;
      invert:=false;
      if (fSettings.cbNoMeaningLearned.Checked) and (learnstate>1) and (learnstate<4) then meaning:='';
      if upcase(wordstate)<>wordstate then boldness:=true else boldness:=false;
      if printing and fSettings.cbNoPrintColors.Checked then
      begin
        //Nothing.
      end else
        if fMenu.aEditorColors.Checked then
          case learnstate of
            0: color:=Col('Editor_Problematic');
            1: color:=Col('Editor_Unlearned');
            2: color:=Col('Editor_Learned');
            3: color:=Col('Editor_Mastered');
          end;
      if not fSettings.cbUserBold.Checked then boldness:=false;
      if fSettings.cbNoReadingLearned.Checked and kanjilearned then reading:='';

      if printing then begin
        Canvas.Brush.Color:=clWhite;
        Canvas.Font.Color:=clBlack;
      end else
      if fSettings.cbNoColors.Checked then begin
        Canvas.Brush.Color:=clWindow;
        Canvas.Font.Color:=clWindowText;
      end else begin
        Canvas.Brush.Color:=colBack;
        Canvas.Font.Color:=colText;
      end;

      if (st2) and (meaning<>'') then
      begin
        cnty:=py+rs*2;
        we:=cx+1;
        cntx:=px+rs*2;
        while (we<wx) and (we<cx+6) and (doctr[cy].chars[we].dicidx=0) and (doctr[cy].chars[we].wordstate<>'?') do
        begin
          if IsHalfWidth(we,cy) and not vert then inc(cntx,rs) else inc(cntx,rs*2);
          inc(we);
        end;
        if st0 or lst0 then cnty:=cnty+rs;
        if vert then
        begin
          realx:=w-cnty-st2c*rs;
          realy:=px;
          realx2:=w-cnty;
          realy2:=cntx;
        end else
        begin
          realx:=px;
          realy:=cnty;
          realx2:=cntx;
          realy2:=cnty+st2c*rs;
        end;
        rect.left:=realx+l+2;
        rect.right:=realx2+l-2;
        rect.top:=realy+t;
        rect.bottom:=realy2+t;
        canvas.Font.Name:=FontEnglish;
        if not fSettings.CheckBox27.Checked then
          canvas.Font.Height:=rs else
          canvas.Font.Height:=rs*2;
        canvas.Font.Style:=[];
        DrawText(canvas.Handle,pchar(meaning),length(meaning),rect,DT_WORDBREAK);
        if fSettings.cbDisplayLines.Checked then
          if vert then
          begin
            canvas.MoveTo(realx2+l+1,realy+t);
            canvas.LineTo(realx+l+1,realy+t);
            canvas.LineTo(realx+l+1,realy2+t);
            canvas.LineTo(realx2+l+1,realy2+t);
          end else
          begin
            canvas.MoveTo(realx+l,realy+t-1);
            canvas.LineTo(realx+l,realy2+t-1);
            canvas.LineTo(realx2+l,realy2+t-1);
            canvas.LineTo(realx2+l,realy+t-1);
          end;
      end;
      if showroma then
        if curlang='c'then
          reading:=ConvertPinYin(KanaToRomaji(reading,romasys,curlang))
        else
          reading:=fstr(KanaToRomaji(reading,romasys,curlang));
      if reading<>'' then kanaq:=kanaq+reading;
      cntx:=px;

      if printing then
        Canvas.Font.Color:=clBlack
      else
      if fSettings.cbNoColors.Checked then
        Canvas.Font.Color:=clWindowText
      else
        Canvas.Font.Color:=colText;
      if not fSettings.cbNoColors.Checked then
      begin
        if (fSettings.CheckBox41.Checked) and ((EvalChar(GetDoc(cx,cy))>4) or (EvalChar(GetDoc(cx,cy))=0)) then
          canvas.Font.Color:=Col('Editor_ASCII');
        if wordstate='I'then
          canvas.Font.Color:=Col('Editor_Active')
        else
        begin
          canvas.Font.Color:=fcolor;
          if (cy=insy) and (cx>=insx) and (cx<insx+inslen) then
            canvas.Font.Color:=Col('Editor_Aftertouch');
          canvas.Brush.Color:=color;
        end;
      end;

      if st0 then for i:=1 to 2 do if kanaq<>'' then
      if (i=1) or (vert) or (not IsHalfWidth(cx,cy)) then
      begin
        if vert then
        begin
          realx:=w-py-rs-1;
          realy:=cntx;
        end else
        begin
          realx:=cntx;
          realy:=py+1;
        end;
        if showroma then
        begin
          if curlang='c'then
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,2),FontChineseGrid)
          else
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,2),FontJapaneseGrid);
          fdelete(kanaq,1,2);
        end else
        begin
          if curlang='c'then
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,1),FontChineseGrid)
          else
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,1),FontJapaneseGrid);
          fdelete(kanaq,1,1);
        end;
        inc(cntx,rs);
      end;
      if boldness then canvas.Font.Style:=[fsBold] else canvas.Font.Style:=[];
      if vert then
      begin
        realx:=w-py-rs*2;
        if st0 or lst0 then realx:=realx-rs;
        realy:=px;
      end else
      begin
        realx:=px;
        realy:=py;
        if st0 or lst0 then realy:=realy+rs;
      end;
      rect.Left:=realx+l;
      rect.Right:=realx+l+rs*2;
      if (not vert) and (IsHalfWidth(cx,cy)) then rect.Right:=realx+l+rs;
      rect.Top:=realy+t;
      rect.Bottom:=realy+t+rs*2;
      canvas.FillRect(rect);
      if curlang='c'then
        DrawUnicode(canvas,realx+l,realy+t,rs*2,RecodeChar(GetDoc(cx,cy)),FontChineseGrid)
      else
        DrawUnicode(canvas,realx+l,realy+t,rs*2,RecodeChar(GetDoc(cx,cy)),FontJapaneseGrid);

     //we check for openers before rendering, and for closers here
      if doc[cy][cx+1]=UH_AORUBY_TAG_CLOSE then
        inRubyTag := false;
      if doc[cy][cx+1]=UH_AORUBY_COMM_CLOSE then
        inRubyComment := false;

      if undersolid and st2 and fSettings.cbDisplayLines.Checked then
        if vert then
        begin
          canvas.MoveTo(realx+l,realy+t);
          canvas.LineTo(realx+l,realy+t+rs*2);
        end else
        begin
          canvas.MoveTo(realx+l,realy+t+rs*2);
          canvas.LineTo(realx+l+rs*2,realy+t+rs*2);
        end;

      canvas.Font.Style:=[];
      if printing then begin
        Canvas.Brush.Color:=clWhite;
        Canvas.Font.Color:=clBlack;
      end else
      if fSettings.cbNoColors.Checked then begin
        Canvas.Brush.Color:=clWindow;
        Canvas.Font.Color:=clWindowText;
      end else begin
        Canvas.Brush.Color:=colBack;
        Canvas.Font.Color:=colText;
      end;

      if (not vert) and (IsHalfWidth(cx,cy)) then inc(px,rs) else inc(px,rs*2);
      inc(cx);
    except
      on E: Exception do begin
        E.Message := 'Paint exception ('+inttostr(cx)+','+inttostr(cy)+': '+E.Message;
        raise;
      end;
    end;

   //Next line
    inRubyTag := false;
    inRubyComment := false;
    inc(py,rs*linec);
    px:=0;
    inc(cl);
  end;
  except
    showmessage('Paint exception: '+(ExceptObject as Exception).Message);
  end;
end;


procedure TfTranslate.DisplayInsert(convins:string;transins:TCharacterPropArray;leaveinserted:boolean);
var i:integer;
  s: string;
  lp: PCharacterLineProps;
begin
  if insx=-1 then
  begin
    insx:=rcurx;
    insy:=rcury;
    inslen:=0;
  end;
  s:=doc[insy];
  fdelete(s,insx+1,inslen);
  doc[insy]:=s;
  lp:=doctr[insy];
  lp.DeleteChars(insx,inslen);
  inslen:=flength(convins);
  if transins=nil then begin
    SetLength(transins, flength(convins));
    for i:=1 to flength(convins) do
      transins[i-1].SetChar('I', 9, 0, 1);
  end;
  doc[insy]:=fcopy(doc[insy],1,insx)+convins+fcopy(doc[insy],insx+1,flength(doc[insy])-insx);
  doctr[insy].InsertChars(insx,transins);
  linl.Clear;
  RenderText(curx,cury,EditorPaintBox.Canvas,0,0,EditorPaintBox.Width-4,
    EditorPaintBox.Height-4,linl,printl,lastxsiz,lastycnt,false,true);
  for i:=0 to linl.Count-1 do
  begin
    if (linl[i].ys=insy) and (linl[i].xs<=insx+inslen) and (insx+inslen-linl[i].xs<linl[i].len) then
    begin
      curx:=insx+inslen-linl[i].xs;
      cury:=i;
    end;
  end;
  if not leaveinserted then
  begin
    insconfirmed:=true;
  end;
end;

procedure TfTranslate.ResolveInsert(final:boolean);
var s,s2,s3:string;
  i:integer;
  lp: TCharacterPropArray;
begin
  if (insx=-1) and (final) then exit;
  if (buffertype='H') and (resolvebuffer) then
  begin
    with fUser do
    if StringGrid1.Visible then
    begin
      s:=curkanji;
      priorkanji:=curkanji;
      s2:=GetInsertKana(false);
      s3:=curphonetic;
      while (s<>'') and (s3<>'') and (fgetch(s,flength(s))=fgetch(s3,flength(s3))) do
      begin
        fdelete(s,flength(s),1);
        fdelete(s3,flength(s3),1);
      end;
      if (s='') and (curkanji[3]>='A') then //TODO: curkanji[3]? wtf?
        s:=curkanji
      else
        s:=s+copy(s2,length(s3)+1,length(s2)-length(s3));
      DisplayInsert(s,nil,true);
    end else
    if not final then
      DisplayInsert(GetInsertKana(true),nil,true);
    if final then
    begin
      SetWordTrans(insx,insy,false,false,true);
      insconfirmed:=true;
      mustrepaint:=true;
      ShowText(false);
    end;
  end else
  begin
    if final then
    begin
      s:=GetInsertKana(false);
      SetLength(lp, flength(s));
      for i:=0 to flength(s)-1 do
        if i=0 then
          lp[i].SetChar(buffertype, 9, 0, 1)
        else
          lp[i].SetChar('<', 9, 0, 1); //word continues
      DisplayInsert(s,lp,true);
      if resolvebuffer then SetWordTrans(insx,insy,false,false,true);
      insconfirmed:=true;
      mustrepaint:=true;
      ShowText(false);
    end else
      DisplayInsert(GetInsertKana(true),nil,true);
  end;
end;

function TfTranslate.GetInsertKana(display:boolean):string;
begin
  if curlang='j'then
  begin
    if buffertype='H'then
      result:=RomajiToKana('H'+lowercase(insertbuffer),romasys,false,curlang) else
    if buffertype='K'then
      result:=RomajiToKana('K'+lowercase(insertbuffer),romasys,false,curlang) else
    result:=UnicodeToHex(insertbuffer);
  end else
  begin
    if display then result:=UnicodeToHex(insertbuffer) else
    if buffertype='H'then result:=RomajiToKana(lowercase(insertbuffer),romasys,false,curlang) else
    result:=UnicodeToHex(insertbuffer);
  end;
end;

procedure TfTranslate.InsertCharacter(c:char);
const
  DEFCPROPS: TCharacterProps = (wordstate:'-';learnstate:9;dicidx:0;docdic:1);
var chartype:char;
    immchar:string;
    immmode:boolean;
begin
  if (c='[') or (c=']') then
  with fUser do
  begin
    if (c='[') and (StringGrid1.Row>1) then StringGrid1.Row:=StringGrid1.Row-1;
    if (c=']') and (StringGrid1.Row<StringGrid1.RowCount-1) then StringGrid1.Row:=StringGrid1.Row+1;
    if insconfirmed then ResolveInsert(true);
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (insx<>-1) then ShowHint else HideHint;
    exit;
  end;
  if insconfirmed then ClearInsBlock;
  FileChanged:=true;
  immmode:=sbAsciiMode.down;
  if (c=' ') and (insertbuffer<>'') then
  begin
    resolvebuffer:=sbKanjiMode.down;
    ResolveInsert(true);
    if sbKanjiMode.down then exit;
  end;
  if (c=#13) and (insertbuffer<>'') then
  begin
    resolvebuffer:=false;
    ResolveInsert(true);
    if sbKanjiMode.down then exit;
  end;
  if (c=#8) and (insertbuffer<>'') then
  begin
    delete(insertbuffer,length(insertbuffer),1);
    DisplayInsert(GetInsertKana(true),nil,insertbuffer<>'');
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;
  if c=#13 then
  begin
//    if blockfromx<>-1 then BlockOp(false,true);
    SplitLine(rcurx,rcury);
    curx:=0;
    inc(cury);
    RefreshLines;
    exit;
  end;
  if c=#8 then
  begin
    if (blockx<>rcurx) or (blocky<>rcury) then BlockOp(false,true) else
    begin
      if curx>0 then dec(curx) else
      begin
        if rcurx=0 then
        begin
          dec(cury);
          curx:=2550;
        end else
        begin
          dec(cury);
          curx:=linl[cury].len-1;
        end;
      end;
      ShowText(true);
      DeleteCharacter(rcurx,rcury);
    end;
    RefreshLines;
    exit;
  end;
  immchar:='';
  case c of
    ',':immchar:={$IFNDEF UNICODE}'3001'{$ELSE}#$3001{$ENDIF};
    '.':immchar:={$IFNDEF UNICODE}'3002'{$ELSE}#$3002{$ENDIF};
    '"':immchar:={$IFNDEF UNICODE}'3003'{$ELSE}#$3003{$ENDIF};
    '<':immchar:={$IFNDEF UNICODE}'3008'{$ELSE}#$3008{$ENDIF};
    '>':immchar:={$IFNDEF UNICODE}'3009'{$ELSE}#$3009{$ENDIF};
    '(':immchar:={$IFNDEF UNICODE}'300C'{$ELSE}#$300C{$ENDIF};
    ')':immchar:={$IFNDEF UNICODE}'300D'{$ELSE}#$300D{$ENDIF};
    '[':immchar:={$IFNDEF UNICODE}'3016'{$ELSE}#$3016{$ENDIF};
    ']':immchar:={$IFNDEF UNICODE}'3017'{$ELSE}#$3017{$ENDIF};
    '{':immchar:={$IFNDEF UNICODE}'3010'{$ELSE}#$3010{$ENDIF};
    '}':immchar:={$IFNDEF UNICODE}'3011'{$ELSE}#$3011{$ENDIF};
    ' ':immchar:={$IFNDEF UNICODE}'0020'{$ELSE}#$0020{$ENDIF};
  end;
  if (AnsiUppercase(c)=c) and ((c<'0') or (c>'9')) then
  begin
    if curlang='c'then chartype:='-'else chartype:='K'
  end else chartype:='H';
  if immmode then chartype:='-';
  if c='''' then chartype:='0';
  if c='+'then chartype:='H';
  if immchar<>'' then chartype:='-';
  if (chartype='-') then
  begin
    resolvebuffer:=false;
    if insertbuffer<>'' then ResolveInsert(true);
    ClearInsBlock;
    if (immchar<>'') and (not immmode) then
      DisplayInsert(immchar,CharPropArray(DEFCPROPS),false)
    else
      DisplayInsert(UnicodeToHex(c),CharPropArray(DEFCPROPS),false);
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;
  if insertbuffer='' then
  begin
    if chartype='0'then buffertype:='-'else buffertype:=chartype;
    insertbuffer:=c;
    insconfirmed:=false;
  end else
  begin
    if (chartype<>'0') and (chartype<>buffertype) then
    begin
      resolvebuffer:=false;
      ResolveInsert(true);
      ClearInsBlock;
      buffertype:=chartype;
      insertbuffer:=c;
    end else
      insertbuffer:=insertbuffer+c;
    insconfirmed:=false;
  end;
  DisplayInsert(GetInsertKana(true),nil,true);
//  resolvebuffer:=true;
//  ResolveInsert(false);
  mustrepaint:=true;
  ShowText(true);
//  Look(false);
end;

procedure TfTranslate.CheckTransCont(x,y:integer);
var cp: PCharacterProps;
begin
  cp := @doctr[y].chars[x];
  while cp.wordstate='<'do
  begin
    cp.SetChar('-', 9, 0, 1);
    inc(x);
    cp := @doctr[y].chars[x];
  end;
end;

procedure TfTranslate.SplitLine(x,y:integer);
var ins:string;
  lp: TCharacterLineProps;
begin
  if flength(doc[y])<=x then
  begin
    if doc.Count-1=y then
    begin
      doc.Add('');
      doctr.AddNewLine();
    end else
    begin
      doc.Insert(y+1,'');
      doctr.InsertNewLine(y+1);
    end;
  end else
  begin
    ins:=fcopy(doc[y],x+1,flength(doc[y])-x);
    lp := doctr[y].CopySubstr(x,0);
    if doc.Count-1=y then
    begin
      doc.Add(ins);
      doctr.AddLine(lp);
    end else
    begin
      doc.Insert(y+1,ins);
      doctr.InsertLine(y+1,lp);
    end;
    doc[y]:=fcopy(doc[y],1,x);
    doctr[y].DeleteChars(x);
    CheckTransCont(0,y+1);
  end;
end;

procedure TfTranslate.JoinLine(y:integer);
begin
  if y+1=doc.Count then exit;
  doc[y]:=doc[y]+doc[y+1];
  doctr[y].AddChars(doctr[y+1]^);
  doc.delete(y+1);
  doctr.DeleteLine(y+1);
end;

procedure TfTranslate.DeleteCharacter(x,y:integer);
begin
  if flength(doc[y])<=x then JoinLine(y) else
  begin
    doc[y]:=fcopy(doc[y],1,x)+fcopy(doc[y],x+2,flength(doc[y])-x-1);
    doctr[y].DeleteChar(x);
    CheckTransCont(x,y);
  end;
end;

procedure TfTranslate.RefreshLines;
begin
  linl.Clear;
  mustrepaint:=true;
  ShowText(true);
end;

function TfTranslate.SetWordTrans(x,y:integer;scanparticle:boolean;gridfirst:boolean;user:boolean):integer;
var wordpart:char;
    i:integer;
    rlen:integer;
    s,s2:string;
    wt:integer;
    dw:string;

  globdict_s: string;
  learnstate_s: string;

  wordstate: char;
  learnstate: byte;
  worddict: integer;
  globdict: integer;
begin
  FileChanged:=true;
  if fSettings.CheckBox34.Checked then scanparticle:=false;
  if (y=-1) or (y>=doctr.Count) or (x=-1) then exit;
  s2:=GetDoc(x,y);
  dw:=GetDocWord(x,y,wt,not user);
  rlen:=flength(dw);
  worddict:=0;
  globdict:=0;

  with fUser do
  begin
    if gridfirst then i:=0 else
      if not StringGrid1.Visible then
        i:=-1
      else
        i:=StringGrid1.Row-1;
    if ul.Count=0 then i:=-1;
    if i=-1 then
    begin
      wordpart:='-';
      worddict:=0;
      learnstate:=9;
      if wt=1 then rlen:=1;
    end else
    begin
      wordpart:=ul[i][15];
      worddict:=StrToint(copy(ul[i],7,6));
      s:=dicsl[i];
      globdict:=0;
      if (pos(UH_LBEG+'d',s)>0) then
      begin
        globdict_s:=copy(s,pos(UH_LBEG+'d',s)+2,length(s)-pos(UH_LBEG+'d',s)-1);
        globdict_s:=copy(globdict_s,1,pos(UH_LEND,globdict_s)-1);
        globdict := docdic.IndexOf(globdict_s);
        if globdict<0 then
        begin
          docdic.add(globdict_s);
          globdict:=docdic.Count-1;
        end;
      end;
      if copy(ul[i],1,6)='000000'then
        learnstate:=9
      else
      begin
        TUser.Locate('Index',copy(ul[i],1,6),true);
        learnstate_s:=TUser.Str(TUserScore);
        if learnstate_s='' then
          learnstate:=9
        else
        if (Ord(learnstate_s[1])>=Ord('0')) and (Ord(learnstate_s[1])<=Ord('9')) then
          learnstate := Ord(learnstate_s[1])-Ord(0)
        else
          learnstate := 9;
      end;

      rlen:=strtoint(copy(ul[i],13,2));
    end;
  end;
 //This subroutine ^^^:
 //local --- s, i, globdict_s, learnstate_s
 //in    --> wt
 //out   <-- wordpart, worddict, globdict, learnstate, rlen,

  if wordpart='-' then begin
    if wt<>1 then wordstate:='-' else wordstate:='?'
  end else
    case wt of
      2:if fSettings.CheckBox38.Checked then wordstate:='-'else wordstate:='H';
      3:if s2={$IFNDEF UNICODE}'30FC'{$ELSE}#$30FC{$ENDIF} then wordstate:='-' else wordstate:='K';
      1:if wordpart='I'then wordstate:='D' else wordstate:='F';
      else wordstate:='-';
    end;
  if wordpart='P'then wordstate:='P';
  if user then wordstate:=LoCase(wordstate);

  if wordstate='-' then
    doctr[y].chars[x].SetChar(wordstate, 9, 0, globdict)
  else
    doctr[y].chars[x].SetChar(wordstate, learnstate, worddict, globdict);
  for i:=2 to rlen do
    if (x+i-1)<flength(doc[y]) then
      doctr[y].chars[x+i-1].SetChar('<', learnstate, 0, globdict);

  fdelete(dw,1,rlen);
  if (wordstate='K') and (flength(doc[y])>x+rlen) then
  begin
    dw:=GetDocWord(x+rlen,y,wt,false);
    if wt<>2 then dw:='';
  end;
  if flength(dw)>4 then dw:=fcopy(dw,1,4); //yes 4 in unicode
  for i:=flength(dw) downto 1 do
    if EvalChar(fgetch(dw,i))=1 then fdelete(dw,i,length(dw)-i+1);
  result:=rlen;
  if (scanparticle) and (wordstate<>'-') and (partl.IndexOf(dw)>-1) then
  begin
    if user then
      doctr[y].chars[x+rlen].SetChar('p', 9, 0, 1)
    else
      doctr[y].chars[x+rlen].SetChar('P', 9, 0, 1);
    for i:=2 to flength(dw) do
      doctr[y].chars[x+rlen+i-1].SetChar('<', 9, 0, 1);
    result:=rlen+flength(dw);
    exit;
  end;
end;

procedure TfTranslate.DrawCursor(blink:boolean);
function OnScreen(x,y:integer):boolean;
begin
  if (x<0) or (y<0) or (y>=linl.Count) or (x>linl[y].len) then
  begin
    result:=false;
    exit;
  end;
  if (y<view) or (y>=view+printl) then result:=false else result:=true;
end;
procedure CalcCache(x,y:integer);
begin
  cursorposcache:=PosToWidth(x,y);
end;
procedure DrawIt(x,y:integer);
var rect:TRect;
begin
  rect.top:=y*lastxsiz*lastycnt+2;
  rect.left:=cursorposcache*lastxsiz;
  rect.bottom:=rect.top+lastxsiz*lastycnt-1;
  rect.right:=rect.left+2;
  InvertRect(EditorPaintBox.Canvas.Handle,rect);
end;
begin
  if not ListBox1.Focused then blink:=false;
  if cursorposcache=-1 then CalcCache(oldcurx,oldcury);
  if (OnScreen(oldcurx,oldcury)) and (not cursorblinked) then DrawIt(oldcurx,oldcury-view);
  if (cursorposcache=-1) or (oldcurx<>cursorscreenx) or (oldcury<>cursorscreeny) then CalcCache(cursorscreenx,cursorscreeny);
  if OnScreen(cursorscreenx,cursorscreeny) and ((not blink) or (cursorblinked)) then DrawIt(cursorscreenx,cursorscreeny-view);
  if blink then cursorblinked:=not cursorblinked;
  if not blink then cursorblinked:=false;
  oldcurx:=cursorscreenx; oldcury:=cursorscreeny;
end;

{
Updates text selection. A bit suboptimal, with two InSelection checks for every char.
This function can be used without buffering, so try to only draw where it's really needed.
Canvas:
  A canvas to draw on. Either edit control (when updating) or backbuffer.
}
procedure TfTranslate.DrawBlock(Canvas: TCanvas);
var rect:TRect;
    i,js:integer;
    hw: boolean;

    ypos: integer;      //logical line containing this graphical line
    xpos: integer;      //current symbol in the logical line
    llen: integer;      //graphical line length


  function InSelection(x, y: integer; x1, y1, x2, y2: integer): boolean;
  begin
    Result := ((y>y1) or ((y=y1) and (x>=x1)))
      and ((y<y2) or ((y=y2) and (x<x2)));
  end;

 //Inverts color for a character at graphical position (i, j),
 //where i is measured in lines and j in half-characters.
  procedure InvertColor(i, js: integer; halfwidth: boolean);
  begin
    rect.top:=(i-view)*lastxsiz*lastycnt+2;
    rect.left:=js*lastxsiz;
    rect.bottom:=rect.top+lastxsiz*lastycnt-1;
    if not halfwidth then
      rect.right:=rect.left+lastxsiz*2
    else
      rect.right:=rect.left+lastxsiz;
    InvertRect(Canvas.Handle,rect);
  end;

begin
  if oldblockfromx=-1 then begin
   //safe values for the rest of the algorithm
    oldblockfromx:=-1;
    oldblocktox:=-1;
    oldblockfromy:=-1;
    oldblocktoy:=-1;
  end;

  CalcBlockFromTo(false);

 {
  i: graphical line index
  j: graphical character index
 }
 //For every visible graphical line
  for i:=view to min(linl.Count-1, view+printl) do begin
    xpos := linl[i].xs;
    ypos := linl[i].ys;
    llen := linl[i].len;

    js:=0; //distantion in half-characters from the left
    while llen>0 do begin
      hw := IsHalfWidth(xpos, ypos);
      if InSelection(xpos, ypos, oldblockfromx, oldblockfromy, oldblocktox, oldblocktoy)
      xor InSelection(xpos, ypos, blockfromx, blockfromy, blocktox, blocktoy) then
        InvertColor(i, js, hw);
      if hw then inc(js) else inc(js,2);
      Inc(xpos);
      Dec(llen);
    end;
  end;

  oldblockfromx:=blockfromx;
  oldblockfromy:=blockfromy;
  oldblocktox:=blocktox;
  oldblocktoy:=blocktoy;
end;

procedure TfTranslate.RepaintText;
begin
  mustrepaint:=true;
  ShowText(false);
end;

procedure TfTranslate.SetCurPos(x,y:integer);
var i:integer;
begin
  for i:=0 to linl.Count-1 do
    if (linl[i].ys=y) and (linl[i].xs<=x) and (linl[i].xs+linl[i].len>x) then
  begin
    curx:=x-linl[i].xs;
    cury:=i;
  end;
end;


procedure TfTranslate.ClearInsBlock;
begin
  if priorkanji<>'' then
  begin
    if TUserPrior.Locate('Kanji',priorkanji,false) then
      TUserPrior.Edit([TUserPrior.Field('Count')],[inttostr(TUserPrior.Int(TUserPrior.Field('Count'))+1)]) else
      TUserPrior.Insert([priorkanji,'1']);
    priorkanji:='';
    fMenu.ChangeUserData;
  end;
  insx:=-1;
  insy:=-1;
  inslen:=0;
  insertbuffer:='';
end;
                                  

procedure TfTranslate.PasteOp;
var s: string;
  sp: TCharacterLineProps;
  AnnotMode: TTextAnnotMode;
  y:integer;
  i:integer;
  l:integer;

  procedure FinalizeLine;
  begin
    if AnnotMode=amRuby then
      CollapseRuby(s, sp);
    doc[y] := doc[y] + s;
    doctr[y].AddChars(sp);
    s := '';
    sp.Clear;
  end;

  procedure InsertNewLine;
  begin
    inc(y);
    doc.Insert(y,'');
    doctr.InsertNewLine(y);
  end;

begin
  resolvebuffer:=false;
  if insertbuffer<>'' then ResolveInsert(true);
  ClearInsBlock;

  if fSettings.cbLoadAozoraRuby.Checked then
    AnnotMode := amRuby
  else
    AnnotMode := amDefault;

  s := '';
  sp.Clear;

  SplitLine(rcurx,rcury);
  y:=rcury;
  for i:=1 to flength(clip) do
  begin
    if fgetch(clip,i)=UH_LF then begin
      FinalizeLine;
      InsertNewLine;
    end else
    if fgetch(clip,i)<>UH_CR then
    begin
      s:=s+fgetch(clip,i);
      if cliptrans.charcount>i-1 then
        sp.AddChar(cliptrans.chars[i-1])
      else
        sp.AddChar('-', 9, 0, 1);
    end;
  end;

  if Length(s)>0 then
    FinalizeLine;

  l:=flength(doc[y]);
  JoinLine(y);
  RefreshLines;
  SetCurPos(l,y);
  FileChanged:=true;
  ShowText(true);
end;

procedure TfTranslate.BlockOp(docopy,dodelete:boolean);
var i,j:integer;
  bx,tx:integer;
  newclip:string;
  newcliptrans:TCharacterLineProps;
begin
  CalcBlockFromTo(false);
  if docopy then
  begin
    newclip := '';
    newcliptrans.Clear;
    for i:=blockfromy to blocktoy do
    begin
      if i=blockfromy then bx:=blockfromx else bx:=0;
      if i=blocktoy then tx:=blocktox-1 else tx:=flength(doc[i])-1;
      for j:=bx to tx do
      begin
        newclip:=newclip+GetDoc(j,i);
        newcliptrans.AddChar(doctr[i].chars[j]);
      end;
      if i<>blocktoy then begin
        newclip:=newclip+UH_CR+UH_LF;
        newcliptrans.AddChar('-', 9, 0, 1);
        newcliptrans.AddChar('-', 9, 0, 1);
      end;
    end;
    if newclip<>'' then
    begin
      clip:=newclip;
      cliptrans:=newcliptrans;
    end;
    fMenu.ChangeClipboard;
  end;
  if dodelete then
  begin
    SetCurPos(blockfromx,blockfromy);
    if blockfromy=blocktoy then
    begin
      doc[blockfromy]:=fcopy(doc[blockfromy],1,blockfromx)
        +fcopy(doc[blockfromy],blocktox+1,flength(doc[blockfromy])-blocktox);
      doctr[blockfromy].DeleteChars(blockfromx, blocktox-blockfromx);
    end else
    begin
      doc[blockfromy]:=fcopy(doc[blockfromy],1,blockfromx);
      doc[blocktoy]:=fcopy(doc[blocktoy],blocktox+1,flength(doc[blocktoy])-blocktox);
      doctr[blockfromy].DeleteChars(blockfromx);
      doctr[blocktoy].DeleteChars(0, blocktox);
      for i:=blockfromy+1 to blocktoy-1 do
      begin
        doc.Delete(blockfromy+1);
        doctr.DeleteLine(blockfromy+1);
      end;
      JoinLine(blockfromy);
      RefreshLines;
    end;
    FileChanged:=true;
  end;
end;


procedure TfTranslate.GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:string);
var dnam:string;
    dic:TJaletDic;
    i:integer;
    markers,defy,s:string;
begin
  meaning:='';
  reading:='';
  kanji:='';
  if doctr[cy].chars[cx].dicidx<>0 then
  begin
    i := doctr[cy].chars[cx].docdic;
    dnam := docdic[i];
    dic:=nil;
    try
      for i:=0 to dicts.Count-1 do
      begin
        if (dicts.Objects[i] as TJaletDic).loaded then
          if (dicts.Objects[i] as TJaletDic).name=dnam then
            dic:=dicts.Objects[i] as TJaletDic;
      end;
    except showmessage('Invalid Dict'); end;
    if dic<>nil then
    begin
      s := IntToStr(doctr[cy].chars[cx].dicidx);
      while length(s)<6 do s := '0'+s;
      dic.Demand;
      dic.TDict.Locate('Index',s,true);
      if dic.TDictMarkers<>-1 then meaning:=dic.TDict.Str(dic.TDictEnglish) else
        meaning:=ConvertEdictEntry(dic.TDict.Str(dic.TDictEnglish),markers);
      reading:=dic.TDict.Str(dic.TDictPhonetic);
      kanji:=dic.TDict.Str(dic.TDictKanji);
    end;
  end else
  if doctr[cy].chars[cx].wordstate='?'then
  begin
    if TChar.Locate('Unicode',GetDoc(cx,cy),false) then
    begin
      TCharRead.SetOrder('');
      TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
      while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
      begin
        s:=TCharRead.Str(TCharReadReading);
        if ((curlang='j') and (TCharRead.Int(TCharReadType)=3)) or ((curlang='c') and (TCharRead.Int(TCharReadType)=7)) then
        begin
          if defy='' then defy:=defy+s else defy:=defy+', '+s;
        end;
        TCharRead.Next;
      end;
      meaning:=defy;
    end;
  end;
  while (reading<>'') and (kanji<>'') and (fgetch(reading,flength(reading))=fgetch(kanji,flength(kanji))) do
  begin
    fdelete(reading,flength(reading),1);
    fdelete(kanji,flength(kanji),1);
  end;
end;

function TfTranslate.GetDocWord(x,y:integer;var wordtype:integer;stopuser:boolean):string;
var wt2:integer;
    i:integer;
    nmk:boolean;
    tc:string;
    honor:boolean;
    stray:integer;
begin
  if (y=-1) or (y>doc.Count-1) or (x>flength(doc[y])-1) or (x=-1) then
  begin
    wordtype:=0;
    result:='';
    exit;
  end;
  if curlang='c'then
  begin
    result:='';
    wordtype:=1;
    for i:=1 to 4 do
    begin
      result:=result+fgetch(doc[y],x+1);
      inc(x);
      if x=flength(doc[y]) then exit;
    end;
    exit;
  end;
  tc:=fgetch(doc[y],x+1);
  honor:=false;
  if (tc={$IFNDEF UNICODE}'304A'{$ELSE}#$304A{$ENDIF})
  or (tc={$IFNDEF UNICODE}'3054'{$ELSE}#$3054{$ENDIF}) then honor:=true;
  if (honor) and (flength(doc[y])>x+2) and (EvalChar(fgetch(doc[y],x+2))<=2) then
    wordtype:=EvalChar(fgetch(doc[y],x+2))
  else
    wordtype:=EvalChar(fgetch(doc[y],x+1));
  if wordtype>4 then wordtype:=4;
  nmk:=false;
  stray:=0;
  result:=fgetch(doc[y],x+1);
  repeat
    inc(x);
    if stopuser and IsLocaseLatin(doctr[y].chars[x].wordstate) then exit;
    wt2:=0;
    if x<flength(doc[y]) then
    begin
      wt2:=EvalChar(fgetch(doc[y],x+1));
      if wt2>4 then wt2:=4;
      if (wordtype=1) and (wt2=2) then begin
        nmk:=true;
        if stray=0 then stray:=1 else stray:=-1;
      end;
      if (nmk) and (wt2=1) then
      begin
        if stray=1 then wt2:=1 else wt2:=4;
        stray:=-1;
        nmk:=false;
      end;
      if (wt2<>wordtype) and ((wordtype<>1) or (wt2<>2)) then exit;
    end;
    if wt2=0 then exit;
    result:=result+fgetch(doc[y],x+1);
  until false;
end;

function TfTranslate.CursorScreenX:integer;
begin
  if (cursorend) and (curx=0) and (cury>0) then result:=linl[cury-1].len else result:=curx;
end;

function TfTranslate.CursorScreenY:integer;
begin
  if (cursorend) and (curx=0) and (cury>0) then result:=cury-1 else result:=cury;
end;

procedure TfTranslate.CalcMouseCoords(x,y:integer;var rx,ry:integer);
var cx,cy:integer;
begin
  rx:=-1;
  ry:=-1;
  cx:=x div (lastxsiz);
  cy:=y div (lastxsiz*lastycnt)+view;
  if cy<0 then cy:=0;
  cx:=WidthToPos(cx,cy);
  if cy>=linl.Count then exit;
  ry:=linl[cy].ys;
  rx:=cx+linl[cy].xs;
  if (ry>=doc.Count) or (rx>=flength(doc[ry])) then
  begin
    ry:=-1;
    rx:=-1;
    exit;
  end;
end;

function TfTranslate.PosToWidth(x,y:integer):integer;
var i,cx,cy:integer;
begin
  if (x<0) or (y<0) or (y>=linl.Count) then
  begin
    result:=-1;
    exit;
  end;
  cx:=linl[y].xs;
  cy:=linl[y].ys;
  result:=0;
  for i:=0 to x-1 do
  begin
    if IsHalfWidth(cx,cy) then inc(result) else inc(result,2);
    inc(cx);
  end;
end;

function TfTranslate.WidthToPos(x,y:integer):integer;
var i,jx,cx,cy:integer;
begin
  if (x<0) or (y<0) or (y>=linl.Count) then
  begin
    result:=-1;
    exit;
  end;
  cx:=linl[y].xs;
  cy:=linl[y].ys;
  jx:=0;
  for i:=0 to x-1 do
  begin
    if IsHalfWidth(cx,cy) then inc(jx) else inc(jx,2);
    if jx>=x then
    begin
      if jx=x then result:=i+1 else result:=i;
      exit;
    end;
    inc(cx);
  end;
  result:=0;
end;



procedure EditorInit;
begin
  priorkanji:='';
  cursorblinked:=false;
  shiftpressed:=false;
  blockx:=-1;
  blocky:=-1;
  leaveline:=false;
  oldblockfromx:=-1; oldblockfromy:=-1; oldblocktox:=-1; oldblocktoy:=-1;
  insconfirmed:=false;
  cursorend:=false;
  lastmmx:=-1;
  lastmmy:=-1;
end;

initialization
  EditorInit;

end.
