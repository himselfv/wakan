unit JWBUnit;

interface
uses Graphics, Windows, SysUtils, Classes, Dialogs, Grids, Forms, ExtCtrls, Registry,
  JWBCore;

{ Known lists }

procedure CreateKnownList(listno:integer;charnumber:integer);
procedure FreeKnownLists;
procedure SaveKnownList(listno:integer;filename:string);
procedure LoadKnownList(listno:integer;stream:TStream);

function IsKnown(listno:integer;const char:FChar):boolean; overload;
procedure SetKnown(listno:integer;const char:FChar;known:boolean); overload;
function FirstUnknownKanjiIndex(const kanji:FString):integer;
function CheckKnownKanji(const kanji:FString): FString;
{$IFDEF UNICODE}
function IsKnown(listno:integer;const char:FString):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}
procedure SetKnown(listno:integer;const char:FString;known:boolean); overload; {$IFDEF INLINE}inline;{$ENDIF}
{$ENDIF}


{ WordGrid }

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
procedure AddWordGrid(var grid:TStringGrid;sp1,sp2,sp4,sp3:string);
procedure FinishWordGrid(grid:TStringGrid);


{ Colors }

const
  Color_Max=100;

procedure InitColors;
function GetColorString(i:integer):string;
procedure SetCol(col:integer;val:TColor);
function GetCol(col:integer):TColor;
function Col(col:string):TColor;
procedure WriteColors;
procedure SetColDefault(i:integer);


{ Painting }

procedure BeginDrawReg(p:TPaintBox);
procedure EndDrawReg;
function FindDrawReg(p:TPaintBox;x,y:integer;var cx,cy,cy2:integer):string;
procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);
procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:FString;fontface:string);
procedure DrawKana(c:TCanvas;x,y,fs:integer;ch:string;fontface:string;showr:boolean;romas:integer;lang:char);
function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean;fontsize:integer;boldfont:boolean):integer;
procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:string; ch:integer;boldfont:boolean);
procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
procedure PaintScreenTipBlock;
procedure SetScreenTipBlock(x1,y1,x2,y2:integer;canvas:TCanvas);


{ Rest }

function StateStr(i:integer):string;
function DateForm(s:string):string;
procedure WritelnMixUni(var f:file;s:string);
function StripCatName(s:string):string;
procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
function ChinTo(s:string):string;
function ChinFrom(s:string):string;


var
 //Fonts
  FontStrokeOrder,
  FontChinese,
  FontChineseGB,
  FontChineseGrid,
  FontChineseGridGB,
  FontJapaneseGrid,
  FontJapanese,
  FontSmall,
  FontRadical,
  FontEnglish,
  FontPinYin:string;

  kcchind,kcchcomp:TStringList;
  GridFontSize:integer;



implementation
uses StrUtils, JWBMenu, JWBSettings;


{ Known lists }

var
  KnownList:array[1..20000] of pointer;
  KnownListSize:integer;

procedure CreateKnownList(listno:integer;charnumber:integer);
begin
  KnownListSize:=65536 div 8;
  if listno>20000 then showmessage('ListNo size exceeded!');
  getmem(KnownList[listno],KnownListSize);
  fillchar(KnownList[listno]^,KnownListSize,0);
end;

procedure FreeKnownLists;
var i:integer;
begin
  for i:=1 to 20000 do if KnownList[i]<>nil then freemem(KnownList[i],KnownListSize);
end;

procedure SaveKnownList(listno:integer;filename:string);
var f:file;
begin
  if listno>20000 then showmessage('ListNo size exceeded!');
  assignfile(f,filename);
  rewrite(f,1);
  blockwrite(f,KnownList[listno]^,KnownListSize);
  closefile(f);
end;

procedure LoadKnownList(listno:integer;stream:TStream);
var f:file;
    TempStr:TStream;
    i,kj:integer;
    b:byte;
    w:integer;
begin
  if listno>20000 then showmessage('ListNo size exceeded!');
  if stream.Size<KnownListSize then
  begin
    w:=stream.Size;
    for i:=1 to w do
    begin
      stream.Read(b,1);
      for kj:=0 to 7 do
        if (((b) shr kj) and 1)<>0 then
        begin
          if TChar.Locate('Index',inttostr((i-1)*8+1+kj),true) then
          begin
            SetKnown(listno,TChar.Str(TChar.Field('Unicode')),true);
          end;
        end;
    end;
  end
//    Application.MessageBox(pchar(_l('#00635^cSeznam nauèených znakù má zastaralou strukturu a proto nebude nahrán.^eKnown kanji list has outdated structure and will not be loaded.')),pchar(_l('#00090^eWarning^cVarování')),MB_ICONWARNING or MB_OK)
  else
    stream.Read(KnownList[listno]^,KnownListSize);
end;

function IsKnown(listno:integer;const char:FChar):boolean;
var w:widechar{$IFDEF UNICODE} absolute char{$ENDIF};
  ki,kj:integer;
begin
 {$IFNDEF UNICODE}
  w:=HexToUnicode(char)[1];
 {$ENDIF}
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then
  begin
    result:=false;
    exit;
  end;
  result:=(((TByteArray(KnownList[listno]^)[ki]) shr kj) and 1)<>0;
end;

procedure SetKnown(listno:integer;const char:FChar;known:boolean);
var ki,kj:integer;
  a:byte;
  w:widechar{$IFDEF UNICODE} absolute char{$ENDIF};
begin
 {$IFNDEF UNICODE}
  w:=HexToUnicode(char)[1];
 {$ENDIF}
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then exit;
  a:=TByteArray(KnownList[listno]^)[ki];
  if known then a:=a or (1 shl kj) else a:=a and not (1 shl kj);
  TByteArray(KnownList[listno]^)[ki]:=a;
end;

{
Parses the string, fchar by fchar, checking that all kanji are "Learned".
If it encounters a character you haven't learned, it returns that character's
number, else it just returns -1.
}
function FirstUnknownKanjiIndex(const kanji:FString):integer;
{$IFDEF UNICODE}
var i: integer;
begin
  Result := -1;
  for i := 1 to Length(kanji) - 1 do
    if (Word(kanji[i]) and $F000 > $3000) and not IsKnown(KnownLearned, kanji[i]) then begin
      Result := i;
      break;
    end;
end;
{$ELSE}
var i, ch: integer;
begin
  Result := -1;
 //Original function had similar check so let's keep it
  if Length(kanji) mod 4 <> 0 then
    raise Exception.Create('Invalid FChar string at FirstUnknownKanjiIndex(): '+kanji);

  for i := 1 to Length(kanji) div 4 do begin
    ch := PInteger(@kanji[4*(i-1)+1])^;
    if (PFCharData(@ch)^[1]>'3') and not IsKnown(KnownLearned, fcopy(kanji, i, 1)) then begin
      Result := i;
      break;
    end;
  end;
end;
{$ENDIF}

{
Backward compability.
Prepends 'U' to the string if it contains kanjis not yet "learned".
}
function CheckKnownKanji(const kanji:FString): FString;
var i: integer;
begin
  i := FirstUnknownKanjiIndex(kanji);
  if i<0 then
    Result := kanji
  else
    Result := UH_UNKNOWN_KANJI + kanji;
end;


{$IFDEF UNICODE}
//Variants of the functions for cases where we pass a string
function IsKnown(listno:integer;const char:FString):boolean;
begin
  Result := (pointer(char)<>nil) and IsKnown(listno, PFChar(char)^);
end;
procedure SetKnown(listno:integer;const char:FString;known:boolean);
begin
  if Length(char)>=1 then
    SetKnown(listno, char[1], known);
end;
{$ENDIF}




type TIntTextInfo=record
        act:boolean;
        p:TPaintBox;
        x,y,fs:integer;
        s:string;
     end;

const MAX_INTTEXTINFO = 4000;

var wgcur:integer;
    itt:array[1..MAX_INTTEXTINFO] of TIntTextInfo;
    curpbox:TPaintBox;
    colarr:TStringList;
    colval:TStringList;
    colsval:array[0..Color_Max] of TColor;
    colsarr:TStringList;
    STB_x1,STB_y1,STB_x2,STB_y2:integer;
    STB_canvas:TCanvas;



procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
begin
  grid.Hide;
  grid.DefaultRowHeight:=GridFontSize+2;
  grid.FixedRows:=1;
  grid.Cells[0,0]:=_l('#00939^ePhonetic^cÈtení');
  grid.Cells[1,0]:=_l('#00632^eWritten^cZápis');
  grid.Cells[2,0]:=_l('#00317^eTranslation^cPøeklad');
  if stat then if learn then
    grid.Cells[3,0]:=_l('#00633^eAdded / Learned^cPøidáno / Nauèeno') else
    grid.Cells[3,0]:=_l('#00634^eCategories^cKategorie');
  wgcur:=1;
end;

procedure AddWordGrid(var grid:TStringGrid;sp1,sp2,sp4,sp3:string);
begin
  grid.Cells[0,wgcur]:=ALTCH_SHARP+sp2;
  grid.Cells[1,wgcur]:=ALTCH_AT+sp1;
  grid.Cells[2,wgcur]:=sp4;
  if sp3<>'' then grid.Cells[3,wgcur]:=sp3;
  inc(wgcur);
end;

procedure FinishWordGrid(grid:TStringGrid);
begin
  if wgcur=1 then grid.Hide else
  begin
    grid.RowCount:=wgcur;
    grid.Show;
  end;
end;

procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
begin
  sp2:='';
  sp1:='';
  sp3:='';
  sp1:='';
  while s[1]<>' 'do
  begin
    sp1:=sp1+s[1];
    delete(s,1,1);
  end;
  delete(s,1,1);
  if s[1]='['then
  begin
    delete(s,1,1);
    while s[1]<>']'do
    begin
      sp2:=sp2+s[1];
      delete(s,1,1);
    end;
    delete(s,1,2);
  end else sp2:=sp1;
  delete(s,1,1);
  sp3:=s;
  sp4:=copy(s,1,pos('}',s)-1);
  delete(sp3,1,length(sp4)+1);
end;

procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
var i:integer;
    s,sp1,sp2,sp3,sp4:string;
begin
  if sl.Count=0 then
  begin
    grid.Hide;
    exit;
  end;
  InitWordGrid(grid,stat,learn);
  for i:=0 to sl.Count-1 do
  begin
    s:=sl[i];
    SplitWord(s,sp1,sp2,sp4,sp3);
    AddWordGrid(grid,sp1,sp2,sp4,sp3);
  end;
  if fSettings.CheckBox53.Checked then
    for i:=1 to grid.RowCount-1 do
      grid.RowHeights[i]:=(GridFontSize+2)*DrawWordInfo(grid.Canvas,grid.CellRect(2,i),false,false,2,grid.Cells[2,i],true,true,GridFontSize,true);
  FinishWordGrid(grid);
end;



function StateStr(i:integer):string;
begin
  case i of
    0:result:=_l('#00638^eProblematic^cProblematické');
    1:result:=_l('#00639^eUnlearned^cNenauèené');
    2:result:=_l('#00640^eLearned^cNauèené');
    3:result:=_l('#00641^eMastered^cDobøe nauèené');
  end;
end;

function DateForm(s:string):string;
begin
  if s='00000000'then result:='-'else
  result:=copy(s,7,2)+'.'+copy(s,5,2)+'.'+copy(s,1,4);
end;

procedure WritelnMixUni(var f:file;s:string);
var bl,bh:byte;
    inuni:boolean;
    s2:string;
begin
  inuni:=false;
  while length(s)>0 do
  begin
    if inuni and (s[1]='}') then
    begin
      inuni:=false;
      delete(s,1,1);
    end else if not inuni and (s[1]='{') then
    begin
      inuni:=true;
      delete(s,1,1);
    end else if inuni then
    begin
      s2:=copy(s,1,2);
      delete(s,1,2);
      bh:=strtoint('0x'+s2);
      s2:=copy(s,1,2);
      delete(s,1,2);
      bl:=strtoint('0x'+s2);
      blockwrite(f,bl,1);
      blockwrite(f,bh,1);
    end else
    begin
      bh:=0;
      bl:=ord(s[1]);
      delete(s,1,1);
      blockwrite(f,bl,1);
      blockwrite(f,bh,1);
    end;
  end;
  bh:=0;
  bl:=10;
  blockwrite(f,bl,1);
  blockwrite(f,bh,1);
  bh:=0;
  bl:=13;
  blockwrite(f,bl,1);
  blockwrite(f,bh,1);
end;

function StripCatName(s:string):string;
begin
  if (length(s)>1) and (s[2]='~') then delete(s,1,2);
  result:=s;
end;

procedure InitColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  colarr:=TStringList.Create;
  colarr.add('0Kanji_Back=FFFFFF,^eBackground^cPozadí');
  colarr.add('0Kanji_Common=000000,^eCommon characters^cBìžné znaky');
  colarr.add('0Kanji_Rare=4F4F4F,^eRare characters^cVzácné znaky');
  colarr.add('0Kanji_Names=005F00,^eCharacters in names^cZnaky ve jménech');
  colarr.add('0Kanji_Learned=7F0000,^eLearned characters^cNauèené znaky');
  colarr.add('0Kanji_RadCommon=000000,^eCommon radicals^cBìžné radikály');
  colarr.add('0Kanji_RadRare=4F4F4F,^eRare radicals^cVzácné radikály');
  colarr.add('0Kanji_RadLearned=7F0000,^eLearned radicals^cNauèené radikály');
  colarr.add('1Dict_Back=FFFFFF,^eBackground^cPozadí');
  colarr.add('1Dict_Text=000000,^eText^cText');
  colarr.add('1Dict_UnknownChar=2F2F7F,^eUnknown characters^cNeznámé znaky');
  colarr.add('1Dict_Problematic=DDDDFF,^eProblematic words^cProblematická slova');
  colarr.add('1Dict_Unlearned=FFEEDD,^eUnlearned words^cNenauèená slova');
  colarr.add('1Dict_Learned=BBFFFF,^eLearned words^cNauèená slova');
  colarr.add('1Dict_Mastered=BBFFBB,^eMastered words^cDobøe nauèená slova');
  colarr.add('1Dict_SelBack=BBBBBB,^eBackground (selected)^cPozadí (vybrané)');
  colarr.add('1Dict_SelProblematic=9999BB,^eProblematic words (selected)^cProblematická slova (vybraná)');
  colarr.add('1Dict_SelUnlearned=BBAA99,^eUnlearned words (selected)^cNenauèená slova (vybraná)');
  colarr.add('1Dict_SelLearned=99BBBB,^eLearned words (selected)^cNauèená slova (vybraná)');
  colarr.add('1Dict_SelMastered=77BB77,^eMastered words (selected)^cDobøe nauèená slova (vybraná)');
  colarr.add('2Mark_Special=7F007F,^eSpecial markers^cSpeciální pøíznaky');
  colarr.add('2Mark_Usage=00007F,^eUsage markers^cPøíznaky použití');
  colarr.add('2Mark_Grammatical=7F0000,^eGrammatical markers^cGramatické pøíznaky');
  colarr.add('2Mark_Dict=4F4F4F,^eDictionary markers^cPøíznaky slovníku');
  colarr.add('2Mark_Lesson=004F00,^eLesson markers,^cPøíznaky lekce');
  colarr.add('3Editor_Back=FFFFFF,^eBackground^cPozadí');
  colarr.add('3Editor_Text=000000,^eText color^cBarva písma');
  colarr.add('3Editor_ASCII=2F2F2F,^eASCII text^cASCII text');
  colarr.add('3Editor_Active=FF0000,^eText being written^cText, který je právì psán');
  colarr.add('3Editor_Aftertouch=0000FF,^eText just converted^cText právì zkonvertován');
  colarr.add('3Editor_Untranslated=FFFFFF,^eUntranslated text^cNepøeložený text');
  colarr.add('3Editor_NotFound=003FFF,^eText where translation failed^cText, u kterého pøeklad selhal');
  colarr.add('3Editor_Particle=FFAAFF,^eEstimated particle^cOdhadovaná partikule');
  colarr.add('3Editor_Translated=EEEEEE,^eWord not in vocabulary^cSlovo, které není ve slovíèkách');
  colarr.add('3Editor_Problematic=DDDDFF,^eProblematic vocabulary word^cProblematické slovíèko');
  colarr.add('3Editor_Unlearned=FFEEDD,^eUnlearned vocabulary word^cNenauèené slovíèko');
  colarr.add('3Editor_Learned=BBFFFF,^eLearned vocabulary word^cNauèené slovíèko');
  colarr.add('3Editor_Mastered=BBFFBB,^eMastered vocabulary word^cDobøe nauèené slovíèko');
  colarr.add('3Editor_HintBack=EFEFEF,^eHint background^cPozadí nápovìdy');
  colarr.add('3Editor_HintSelected=00FFFF,^eHint selected background^cVybrané pozadí nápovìdy');
  colarr.add('3Editor_HintText=000000,^eHint text^cText nápovìdy');
  colarr.add('4Popup_Back=A0FFFF,^eBackground^cPozadí');
  colarr.add('4Popup_Lines=000000,^eLines^cÈáry');
  colarr.add('4Popup_Card=FFFFFF,^eCharacter card^cKarta znaku');
  colarr.add('4Popup_Text=000000,^eText on the caracter card^cText na kartì znaku');
  colval:=TStringList.Create;
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  colsarr:=TStringList.Create;
  for i:=0 to colarr.Count-1 do
  begin
    s:=colarr[i];
    delete(s,1,1);
    s2:=s;
    delete(s2,1,pos('=',s));
    s:=copy(s,1,pos('=',s)-1);
    s2:=copy(s2,1,pos(',',s2)-1);
    colval.Add(reg.ReadString('Colors',s,s2));
    colsarr.Add(s);
  end;
  colsarr.Sorted:=true;
  colsarr.Sort;
  reg.Free;
  SetCol(-1,clBlack);
end;

procedure SetColDefault(i:integer);
var s,s2:string;
begin
  s:=colarr[i];
  delete(s,1,1);
  s2:=s;
  delete(s2,1,pos('=',s));
  s:=copy(s,1,pos('=',s)-1);
  s2:=copy(s2,1,pos(',',s2)-1);
  colval[i]:=s2;
  SetCol(-1,clBlack);
end;

function GetColorString(i:integer):string;
var s:string;
begin
  result:='';
  if i>=colarr.Count then exit;
  s:=colarr[i];
  result:=s[1];
  delete(s,1,pos(',',s));
  result:=result+s;
end;

procedure SetCol(col:integer;val:TColor);
var i,j:integer;
begin
  if col>-1 then colval[col]:=Format('%6.6X',[val]);
  for i:=0 to colsarr.Count-1 do for j:=0 to colarr.Count-1 do if copy(colarr[j],2,length(colsarr[i]))=colsarr[i] then
    colsval[i]:=strtoint('0x'+colval[j]);
end;

function GetCol(col:integer):TColor;
begin
  result:=strtoint('0x'+colval[col]);
end;

function Col(col:string):TColor;
var i:integer;
begin
  result:=colsval[colsarr.IndexOf(col)];
end;

procedure WriteColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  for i:=0 to colarr.Count-1 do
  begin
    s:=colarr[i];
    delete(s,1,1);
    s2:=s;
    delete(s2,1,pos('=',s));
    s:=copy(s,1,pos('=',s)-1);
    s2:=copy(s,1,pos(',',s));
    reg.WriteString('Colors',s,colval[i]);
  end;
end;





function ChinTo(s:string):string;
var s2,cd:string;
    bk:string;
begin
  if (curlang='j') or (fSettings.RadioGroup5.ItemIndex<>1) then
  begin
    result:=s;
    exit;
  end;
  bk:=TChar.Str(TCharUnicode);
  result:='';
  while s<>'' do
  begin
    s2:=copy(s,1,4);
    delete(s,1,4);
    if (s2[1]>'3') and (TChar.Locate('Unicode',s2,false)) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),43);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk,false);
end;

//TODO: Upgrade this function to Unicode
function ChinFrom(s:string):string;
var s2,cd:string;
    bk:string;
begin
  if (curlang='j') or (fSettings.RadioGroup5.ItemIndex=0) then
  begin
    result:=s;
    exit;
  end;
  bk:=TChar.Str(TCharUnicode);
  result:='';
  while s<>'' do
  begin
    s2:=copy(s,1,4);
    delete(s,1,4);
    if (s2[1]>'3') and (TChar.Locate('Unicode',s2,false)) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),44);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk,false);
end;

procedure BeginDrawReg(p:TPaintBox);
var i:integer;
begin
  for i:=1 to MAX_INTTEXTINFO do if itt[i].p=p then itt[i].act:=false;
  curpbox:=p;
end;

procedure EndDrawReg;
begin
  curpbox:=nil;
end;

function FindDrawReg(p:TPaintBox;x,y:integer;var cx,cy,cy2:integer):string;
var i,j:integer;
begin
  result:='';
  for i:=1 to MAX_INTTEXTINFO do if itt[i].p=p then
    if (x>=itt[i].x) and (y>=itt[i].y) and (x<=itt[i].x+itt[i].fs*(length(itt[i].s) div 4)) and
    (y<=itt[i].y+itt[i].fs) then
    begin
      j:=(x-itt[i].x) div itt[i].fs;
      cy:=itt[i].y;
      cy2:=itt[i].y+itt[i].fs;
      cx:=itt[i].x+itt[i].fs*j;
      result:=copy(itt[i].s,j*4+1,length(itt[i].s)-j*4);
      exit;
    end;
end;

procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);
var i,l,r,m:integer;
    xx,yy:byte;
    p:pchar;
begin
  if sobin=nil then exit;
  l:=0;
  r:=sodir.Count-1;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    if (copy(sodir[m],1,4)<char) then l:=m+1 else
    if (copy(sodir[m],1,4)>char) then r:=m-1 else break;
  end;
  if l>r then exit;
  i:=strtoint('0x'+copy(sodir[m],5,4));
  p:=sobin;
  p:=p+i*2;
  xx:=255;
  yy:=255;
  i:=0;
  SetBkMode(canvas.Handle,TRANSPARENT);
  canvas.Font.Color:=color;
  canvas.Font.Style:=[fsBold];
  while (xx<>0) or (yy<>0) do
  begin
    xx:=byte(p^);
    p:=p+1;
    yy:=byte(p^);
    p:=p+1;
    inc(i);
    if (xx<>0) and (yy<>0) then
    begin
      canvas.Font.Color:=clWindow;
      DrawUnicode(canvas,round(x+w*(xx/256))+1,round(y+h*(yy/256)),fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256))-1,round(y+h*(yy/256)),fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256))+1,fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256))-1,fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      canvas.Font.Color:=color;
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256)),fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
    end;
  end;
  canvas.Font.Color:=clWindowText;
  canvas.Font.Style:=[];
end;

function DrawTone(c:TCanvas;x,y,fw:integer;s:FString;dodraw:boolean):string;
var tb:integer;
    s2,s3,s4:string;
    sc:char;
    i:integer;
    tr:double;
    w:pwidechar;
    oldtextflags:longint;
    s5:string;
begin
  s2:='';
  tb:=1;
  i:=0;
  s5:='';
  c.Font.Height:=fw;
  while s<>'' do
  begin
    s4:=copy(s,1,4);
    s3:=s4;
    if pos('F03',s3)=1 then delete(s3,1,3) else s3:='XXXX';
    if s3<>'' then sc:=s3[1] else sc:='?';
    sc:=upcase(sc);
    if (sc>='0') and (sc<='5') then
    begin
      tr:=(i-tb)/2+tb-1;
      w:='';
      oldtextflags:=c.TextFlags;
      c.TextFlags:=0;
      if dodraw then case sc of
        '3':w:=pwidechar(HexToUnicode('02C7'));
        '1':w:=pwidechar(HexToUnicode('02C9'));
        '2':w:=pwidechar(HexToUnicode('02CA'));
        '4':w:=pwidechar(HexToUnicode('02CB'));
      end;
      s5:=s5+floattostrf(tr,ffNumber,7,2)+',';
      if w<>'' then TextOutW(c.Handle,round(tr*fw),y,w,1);
      c.TextFlags:=oldtextflags;
      tb:=i+1;
    end else
    begin
      s2:=s2+s4;
      inc(i);
    end;
    delete(s,1,length(s4));
  end;
//  showmessage(s5);
  result:=s2;
end;

procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:FString;fontface:string);
var w:pwidechar;
    chn:string;
    i:integer;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
  c.Font.Name:=fontface;
  c.Font.Height:=fs;
//    c.Font.Style:=[];
  chn:=DrawTone(c,x,y,fs,ch,false);
 {$IFDEF UNICODE}
  w := PWideChar(chn);
 {$ELSE}
  w := PWideChar(HexToUnicode(chn));
 {$ENDIF}
  if chn<>ch then
  begin
    c.Font.Name:=FontRadical;
    fs:=round(fs/8*7);
    c.Font.Height:=fs;
    y:=y+fs div 4;
  end;
  chn:=DrawTone(c,x,y-fs div 4,fs,ch,true);
  if curpbox<>nil then for i:=1 to MAX_INTTEXTINFO do if not itt[i].act then
  begin
    itt[i].act:=true;
    itt[i].p:=curpbox;
    itt[i].x:=x;
    itt[i].y:=y;
    itt[i].fs:=fs;
    itt[i].s:=chn;
    break;
  end;
  TextOutW(c.Handle,x,y,w,flength(chn));
end;


function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean; fontsize:integer; boldfont:boolean):integer;
var x:integer;
    inmar,resinmar:boolean;
    curs:string;
    rect2:TRect;
    c:char;
    cursiv:boolean;
    w:integer;
    y:integer;
    cnt:integer;
    sbef:string;
    fontcolor:TColor;
begin
  if multiline then result:=1 else result:=0;
  Canvas.Brush.Color:=clWindow;
  Canvas.Font.Color:=clWindowText;
  Canvas.Font.Name:=FontEnglish;
  Canvas.Font.Style:=[];
  Canvas.Font.Size:=9;
  if (fSettings.CheckBox11.Checked) and (not fSettings.CheckBox9.Checked) and (not titrow) then
  begin
    c:=' ';
    if (length(s)>1) and (s[1]=ALTCH_EXCL) then c:=s[2];
    if (length(s)>2) and (s[2]=ALTCH_EXCL) then c:=s[3];
    case c of
      ' ':if sel then Canvas.Brush.Color:=Col('Dict_SelBack') else Canvas.Brush.Color:=Col('Dict_Back');
      '0':if sel then Canvas.Brush.Color:=Col('Dict_SelProblematic') else Canvas.Brush.Color:=Col('Dict_Problematic');
      '1':if sel then Canvas.Brush.Color:=Col('Dict_SelUnlearned') else Canvas.Brush.Color:=Col('Dict_Unlearned');
      '2':if sel then Canvas.Brush.Color:=Col('Dict_SelLearned') else Canvas.Brush.Color:=Col('Dict_Learned');
      '3':if sel then Canvas.Brush.Color:=Col('Dict_SelMastered') else Canvas.Brush.Color:=Col('Dict_Mastered');
    end;
  end;
  if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
  if (length(s)>2) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
  if (length(s)>0) and (Colx=0) and (s[1]=ALTCH_SHARP) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
//    if showroma then
//      Grid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,KanaToRomaji(s,romasys,curlang)) else
//    DrawUnicode(Grid.Canvas,Rect.Left+2,Rect.Top+2,12,s,FontSmall);
    DrawKana(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall,showroma,romasys,curlang);
  end else
  if (length(s)>0) and (s[1]=ALTCH_AT) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    if (s[1]=UH_UNKNOWN_KANJI) then
    begin
      if (fSettings.CheckBox10.Checked) then Canvas.Font.COlor:=Col('Dict_UnknownChar') else Canvas.Font.Color:=Col('Dict_Text');
      delete(s,1,1);
    end
    else Canvas.Font.Color:=Col('Dict_Text');
    if fSettings.CheckBox9.Checked then Canvas.Font.Color:=clWindowText;
    DrawUnicode(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall);
  end else if not titrow then
  begin
    cursiv:=false;
    FontColor:=Col('Dict_Text');
    if fSettings.CheckBox9.Checked then FontColor:=clWindowText;
    if (length(s)>1) and (s[1]=ALTCH_TILDE) then
    begin
      if s[2]='I'then cursiv:=true;
//      if not fUser.CheckBox1.Checked then cursiv:=false;
      delete(s,1,2);
    end;
    if (length(s)>1) and (s[1]=UH_SETCOLOR) then
    begin
      if (fSettings.CheckBox69.Checked) then
        if not TryStrToInt('0x'+copy(s,6,2)+copy(s,4,2)+copy(s,2,2), integer(FontColor)) then
          FontColor:=clWindowText;
      delete(s,1,7);
    end;
    if not onlycount then Canvas.FillRect(Rect);
    inmar:=false;
    x:=0;
    y:=0;
    cnt:=0;
    sbef:='';
    while length(s)>0 do
    begin
//      if sbef=s then
//      begin
//        showmessage(sbef);
//      end;
      sbef:=s;
      inc(cnt);
      if inmar then
        if pos(UH_LEND,s)>0 then curs:=copy(s,1,pos(UH_LEND,s)-1) else curs:=s;
      if not inmar then
        if pos(UH_LBEG,s)>0 then curs:=copy(s,1,pos(UH_LBEG,s)-1) else curs:=s;
      delete(s,1,length(curs));
      if (length(s)>0) and ((s[1]=UH_LBEG) or (s[1]=UH_LEND)) then delete(s,1,1);
      rect2:=rect;
      rect2.Left:=rect.left+x+2;
      rect2.Top:=rect.top+y;
      if x<rect.right-rect.left then
      begin
        if inmar then
        begin
          c:=curs[1];
          delete(curs,1,1);
          if fSettings.CheckBox9.Checked then Canvas.Font.Color:=FontColor;
          if not fSettings.CheckBox9.Checked then case c of
            '1':Canvas.Font.Color:=Col('Mark_Special');
            's':Canvas.Font.Color:=Col('Mark_Usage');
            'g':Canvas.Font.Color:=Col('Mark_Grammatical');
            'd':Canvas.Font.Color:=Col('Mark_Dict');
            'l':Canvas.Font.Color:=Col('Mark_Lesson');
          end;
          Canvas.Font.Height:=FontSize-3;
          Canvas.Font.Style:=[fsItalic];
        end else
        begin
          Canvas.Font.Color:=FontColor;
          Canvas.Font.Height:=FontSize;
          Canvas.Font.Style:=[];
          if boldfont then
            if cursiv then Canvas.Font.Style:=[fsItalic,fsBold] else
              Canvas.Font.Style:=[fsBold];
          if Colx=3 then Canvas.Font.Style:=[];
        end;
        w:=Canvas.TextExtent(curs).cx;
        if not multiline then result:=result+w;
        resinmar:=false;
        if (multiline) and (rect.left+2+x+w>rect.right) then
        begin
          if (length(curs)>0) and (curs[1]=' ') then curs[1]:=ALTCH_TILDE;
          if inmar or (pos(' ',curs)=0) or (Canvas.TextExtent(copy(curs,1,pos(' ',curs)-1)).cx+rect.left+2+x>rect.right) then
          begin
            if (length(curs)>0) and (curs[1]=ALTCH_TILDE) then curs[1]:=' ';
            x:=0;
            y:=y+FontSize+2;
            rect2.left:=rect.left+2;
            rect2.top:=rect.top+2+y;
            result:=result+1;
          end else
          if not inmar and (pos(' ',curs)>0) then
          begin
            if (length(curs)>0) and (curs[1]=' ') then curs[1]:=ALTCH_TILDE;
            s:=copy(curs,pos(' ',curs),length(curs)-pos(' ',curs)+1)+UH_LBEG+s;
            curs:=copy(curs,1,pos(' ',curs)-1);
            if (length(curs)>0) and (curs[1]=ALTCH_TILDE) then curs[1]:=' ';
            resinmar:=true;
          end else
          begin
            curs:=s;
            if (length(curs)>0) and (curs[1]=ALTCH_TILDE) then curs[1]:=' ';
            s:='';
          end;
        end;
        if not onlycount then
        begin
          if inmar then
            Canvas.TextRect(Rect2,Rect.Left+2+x,Rect.Top+5+y,curs) else
            Canvas.TextRect(Rect2,Rect.Left+2+x,Rect.Top+1+y,curs);
        end;
        x:=x+Canvas.TextExtent(curs).cx;
        if not resinmar then inmar:=not inmar;
      end else s:='';
    end;
  end else
  begin
    Canvas.Font.Style:=[fsBold];
    Canvas.Font.Size:=8;
    Canvas.FillRect(Rect);
    Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,s);
  end;
  Canvas.Font.Color:=clWindowText;
  Canvas.Font.Name:=FontEnglish;
  Canvas.Font.Style:=[];
  Canvas.Font.Size:=9;
end;

procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:string; ch:integer;boldfont:boolean);
var s1,sx1,s2,s3,s4:string;
begin
  SplitWord(s,s1,s2,s3,s4);
  if curlang='c'then
  begin
    if s2[1]=ALTCH_EXCL then delete(s2,1,2);
    s2:=KanaToRomaji(s2,romasys,curlang);
    s2:=ConvertPinYin(s2);
    sx1:=s1;
    s1:=s1+UH_SPACE+s2;
    DrawWordInfo(Canvas,rect,false,false,0,ALTCH_AT+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(sx1) div 4)*ch+ch+(length(s2) div 8)*ch;
  end else
  begin
    DrawWordInfo(Canvas,rect,false,false,0,ALTCH_AT+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(s1) div 4)*ch;
  end;
  if (s2<>s1) and (curlang='j') then
  begin
    if s2[1]=ALTCH_EXCL then s2:=ALTCH_EXCL+s1[2]+UH_UNKNOWN_KANJI+copy(s2,3,length(s2)-2) else s2:=UH_UNKNOWN_KANJI+s2;
    DrawWordInfo(Canvas,rect,false,false,1,ALTCH_AT+s2,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(s2) div 4)*ch;
  end;
  DrawWordInfo(Canvas,rect,false,false,2,s3,false,false,ch-3,boldfont);
end;

procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var s:string;
    gr:integer;
    rect2:TRect;
begin
  s:=Grid.Cells[ACol,ARow];
  rect2:=rect;
  rect2.bottom:=1000;
  if (fSettings.CheckBox53.Checked) and (ACol=2) and (ARow>0) then
  begin
    gr:=(2+GridFontSize)*DrawWordInfo(Grid.Canvas, Rect2, gdSelected in State, ARow=0, ACol, s, true, true, GridFontSize,true);
    if grid.rowheights[arow]<>gr then begin grid.rowheights[arow]:=gr; exit; end;
  end;
  DrawWordInfo(Grid.Canvas, Rect, gdSelected in State, ARow=0, ACol, s, true, false, GridFontSize,true);
  PaintScreenTipBlock;
end;

procedure DrawKana(c:TCanvas;x,y,fs:integer;ch:string;fontface:string;showr:boolean;romas:integer;lang:char);
var cnv,cnv2:string;
begin
  c.Font.Style:=[];
  if showr then
  begin
    cnv:=KanaToRomaji(ch,romas,lang);
    if lang='c'then cnv2:=ConvertPinYin(cnv) else cnv2:=UnicodeToHex(cnv);
    DrawUnicode(c,x,y,fs+1,cnv2,FontPinYin);
  end else DrawUnicode(c,x,y,fs,ch,fontface);
end;


procedure PaintScreenTipBlock;
var oldR2:integer;
begin
  if STB_Canvas<>nil then
  begin
    oldR2:=SetROP2(STB_Canvas.Handle,R2_NOT);
    STB_Canvas.Rectangle(STB_x1,STB_y1,STB_x2,STB_y2);
    SetROP2(STB_Canvas.Handle,R2_NOT);
  end;
end;

procedure SetScreenTipBlock(x1,y1,x2,y2:integer;canvas:TCanvas);
begin
  PaintScreenTipBlock;
  STB_x1:=x1;
  STB_y1:=y1;
  STB_x2:=x2;
  STB_y2:=y2;
  STB_canvas:=canvas;
  PaintScreenTipBlock;
end;



var i:integer;
begin
  CurPBox:=nil;
  for i:=1 to MAX_INTTEXTINFO do itt[i].act:=false;
  for i:=1 to 20000 do KnownList[i]:=nil;
  GridFontSize:=14;
  STB_Canvas:=nil;
  kcchind:=TStringList.Create;
  kcchcomp:=TStringList.Create;
end.
