unit JWBUnit;
{ Common stuff for Wakan. }

interface
uses Graphics, Windows, SysUtils, Classes, Dialogs, Grids, Forms, ExtCtrls, Registry,
  JWBStrings, JWBUtils, JWBKanaConv;

{ Misc }

var
  AppFilename: string = '';
  AppFolder: string = ''; //path to program, set on load
  WakanVer: string = ''; //taken from resources on load

const
  CurStructVer=2;
  CurDictVer=7;

const
  WakanAppName = 'WaKan - Japanese & Chinese Learning Tool';
  WakanCopyright = '(C) Filip Kabrt and others 2002-2013';
  WakanRegKey = 'Software\Labyrinth\Wakan';

  UserDataDir: string = '';
  PortableMode: boolean = false;

procedure SetStandaloneMode;
procedure SetPortableMode;


{ Romaji conversions }

var
 { Romaji translation table. Populated on load.
  See comments where class is defined. }
  roma_t: TRomajiTranslator;

 { Chinese version, not upcased. Someone upgrade this one too... }
  romac: TStringList;

function ConvertPinYin(const str:string):FString;
function DeconvertPinYin(const str:FString):string;
function KanaToRomaji(const s:FString;romatype:integer;lang:char):string;
function RomajiToKana(const s:string;romatype:integer;clean:boolean;lang:char):FString;


{ WordGrid }

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
procedure AddWordGrid(grid:TStringGrid;sp1,sp2,sp4,sp3:string);
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
procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:FString; ch:integer;boldfont:boolean);
procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
procedure PaintScreenTipBlock;
procedure SetScreenTipBlock(x1,y1,x2,y2:integer;canvas:TCanvas);


{ Rest }

function StateStr(i:integer):string;
function DateForm(s:string):string;
procedure WritelnMixUni(var f:file;s:string);
procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
function ChinTo(s:FString):FString;
function ChinFrom(s:FString):FString;

{ Upgrades vocabulary entry -- see implementation comments }
function FixVocabEntry(const s:string):string;
function UnfixVocabEntry(const s:string):string;

procedure DeleteDirectory(dir:string);
procedure Backup(const filename: string);


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

  GridFontSize:integer;


{ Translation }

function _l(const id:string):string; overload;
function _l(const id:string; args: array of const):string; overload;

implementation
uses StrUtils, ShlObj, JWBMenu, JWBSettings, JWBLanguage, TextTable;


{ Portable/standalone }

procedure SetStandaloneMode;
begin
  PortableMode := false;
  UserDataDir := GetSpecialFolderPath(CSIDL_APPDATA);
 //There's also CSIDL_LOCAL_APPDATA which might do if CSIDL_APPDATA is somehow not available.
 //But I don't think that can be the case!
  Assert(UserDataDir<>''); //just in case
  UserDataDir:=UserDataDir+'\Wakan';
  ForceDirectories(UserDataDir);
end;

procedure SetPortableMode;
begin
  PortableMode := true;
  UserDataDir := AppFolder;
end;

{ Romaji conversions }

type
  EvilString = string; //contains FString if we convert uni->pinyin, string otherwise

//Index of romac entry for this item or -1
//Text must be uppercased
function FindRomaC(const find: EvilString; roma_type: integer; partial: boolean): integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to (romac.count div 4)-1 do
    if (partial and (pos(find,romac[i*4+roma_type])=1))
    or ((not partial) and (romac[i*4+roma_type]=find)) then begin
      Result := i;
      break;
    end;
end;

//TODO: Test Unicode conversion
{
Converts chinese string between:
  0 - bopomofo (fstring)
  1 - pinyin
  2 - wade-giles
  3 - yale
TypeIn: conversion source
TypeOut: conversion target type
Clean: ignore unknown characters instead of adding '?'
}
function ResolveCrom(s:EvilString;typein,typeout:integer;clean:boolean):EvilString;
var s2:string;
  cr:string;
  cl:integer;
  i:integer;
begin
  s:=uppercase(s);
  s2:='';
  while s<>'' do
  begin
    //Find longest match for character sequence starting at this point
    cl:=0;
    for i:=0 to (romac.count div 4)-1 do
    begin
      if pos(uppercase(romac[i*4+typein]),s)=1 then
      begin
        if length(romac[i*4+typein])>cl then
        begin
          cl:=length(romac[i*4+typein]);
          cr:=romac[i*4+typeout];
        end;
      end;
    end;

    if cl>0 then s2:=s2+cr else
      if s[1]='-'then s2:=s2+UnicodeToHex('-') else
      if s[1]='_'then s2:=s2+UnicodeToHex('_') else
      if pos(UnicodeToHex('-'),s)=1 then s2:=s2+'-'else
      if pos(UnicodeToHex('_'),s)=1 then s2:=s2+'_'else
      if not clean then
     {$IFDEF UNICODE}
        s2 := s2 + '?'; //in unicode both strings are in native form
     {$ELSE}
      begin
        if typeout>0 then s2:=s2+'?'else s2:=s2+'003F';
      end;
     {$ENDIF}
    if cl>0 then delete(s,1,cl) else
      if typein>0 then delete(s,1,1) else fdelete(s,1,1);

   //Convert tone markers between Ansi and Unicode versions

    if typein=0 then
    begin
     {$IFNDEF UNICODE}
      if (length(s)>3) and (pos('F03',s)=1) and (s[4]>='0') and (s[4]<='5') then
      begin
        s2:=s2+s[4];
        delete(s,1,4);
      end else s2:=s2+'0';
     {$ELSE}
      if (length(s)>0) and (Ord(s[1]) and $FFF0 = $F030) and (Ord(s[1]) and $000F in [0..5]) then
      begin
        s2:=s2+Chr($30 + Ord(s[1]) and $000F); //to digit
        delete(s,1,1);
      end;
     {$ENDIF}
    end else
    begin
      if (length(s)>0) and (s[1]>='0') and (s[1]<='5') then
      begin
        if typeout>0 then s2:=s2+s[1] else
          s2:=s2+{$IFNDEF UNICODE}'F03'+s[1]{$ELSE}Chr($F030+Ord(s[1])-$30){$ENDIF};
        delete(s,1,1);
      end else
      begin
        if typeout>0 then s2:=s2+'0' else s2:=s2+{$IFNDEF UNICODE}'F030'{$ELSE}#$F030{$ENDIF};
      end;
    end;
  end;
  if typeout>0 then result:=lowercase(s2) else result:=s2;
end;

function KanaToRomaji(const s:FString;romatype:integer;lang:char):string;
begin
  if lang='j'then
  begin
    Result := roma_t.KanaToRomaji(s,romatype);
  end else
  if lang='c'then
  begin
    result:=ResolveCrom(s,0,romatype,false);
  end;
end;

function RomajiToKana(const s:string;romatype:integer;clean:boolean;lang:char):FString;
var s_rep: string;
begin
  if lang='j'then
  begin
    Result := roma_t.RomajiToKana(s,romatype,clean);
  end else
  if lang='c'then
  begin
    s_rep := s;
    repl(s_rep,'v','u:');
    result:=ResolveCrom(s_rep,romatype,0,clean);
  end;
end;

//TODO: Test unicode conversion
function ConvertPinYin(const str:string):FString;
const UH_DUMMY_CHAR:FChar = {$IFNDEF UNICODE}'XXXX'{$ELSE}#$F8F0{$ENDIF}; //used in place of a char when it's unknown or whatever
 //does not go out of this function
var cnv:string;
  li:integer;
  ali:FString;
  cnv2:FString;
  cc:char;
  i:integer;
  iscomma:boolean;
begin
  cnv:=lowercase(str);
  cnv2:='';
  li:=0;
  ali:='';
  iscomma:=false;
  for i:=1 to length(cnv) do
  begin
    if (li=0) and ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
    if (li<i) and ((cnv[li]='i') or (cnv[li]='u') or (cnv[li]='ь')) and
      ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
    if (cnv[i]>='0') and (cnv[i]<='5') and (li>0) then
    begin
      cc:=cnv[li];
      ali:=fcopy(cnv2,length(cnv2)-i-li,i-li-1);
      fdelete(cnv2,length(cnv2)-i-li-1,i-li);
      if iscomma and (cc='u') then cc:='w';
      case cnv[i] of
        '2':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$00E1{$ELSE}'00E1'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$00E9{$ELSE}'00E9'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$00ED{$ELSE}'00ED'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$00F3{$ELSE}'00F3'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$00FA{$ELSE}'00FA'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01D8{$ELSE}'01D8'{$ENDIF};
            end;
        '4':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$00E0{$ELSE}'00E0'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$00E8{$ELSE}'00E8'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$00EC{$ELSE}'00EC'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$00F2{$ELSE}'00F2'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$00F9{$ELSE}'00F9'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01DC{$ELSE}'01DC'{$ENDIF};
            end;
        '1':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$0101{$ELSE}'0101'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$0113{$ELSE}'0113'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$012B{$ELSE}'012B'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$014D{$ELSE}'014D'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$016B{$ELSE}'016B'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01D6{$ELSE}'01D6'{$ENDIF};
            end;
        '3':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$01CE{$ELSE}'01CE'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$011B{$ELSE}'011B'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$01D0{$ELSE}'01D0'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$01D2{$ELSE}'01D2'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$01D4{$ELSE}'01D4'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01DA{$ELSE}'01DA'{$ENDIF};
            end;
      end;
      li:=0;
      if (cnv[i]='0') or (cnv[i]='5') then
        if cc='w'then
          cnv2:=cnv2+{$IFDEF UNICODE}#$00FC{$ELSE}'00FC'{$ENDIF}
        else
          cnv2:=cnv2+cc;
      cnv2:=cnv2+ali;
      iscomma:=false;
    end else
    if cnv[i]=':'then begin
      cnv2:=cnv2+UH_DUMMY_CHAR;
      iscomma:=true
    end else
    if (cnv[i]<'0') or (cnv[i]>'5') then
      cnv2:=cnv2+cnv[i];
  end;

  //Remove dummy chars
  i := fpos(UH_DUMMY_CHAR,cnv2);
  while i>0 do begin
    fdelete(cnv2,i,1);
    i := fpos(UH_DUMMY_CHAR,cnv2);
  end;
  Result := cnv2;
end;

//TODO: Test Unicode conversion
//Due to the high number of char constants this function is implemented only in Unicode,
//and on exit we convert to FString if needed.
//This is slower, but FStrings are deprecated anyway.
function DeconvertPinYin(const str:FString):string;
var cnv:UnicodeString;
  curs:UnicodeString; //although by logic it ought to be a Char...
  nch:string;
    cnv2:string;
    i,j:integer;
    curcc:string;
    curp,curpx:char;
    mustbegin,mustnotbegin,befmustnotbegin,befbefmustnotbegin:boolean;
    number:boolean;
    putcomma,fnd:boolean;
    cc:char;
begin
  cnv:=fstrtouni(str);

  putcomma:=false;
  cnv2:='';
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    if putcomma and (curs<>'e') then begin curs:=':'+curs; putcomma:=false; end;
    if curs=#$01D6 then begin putcomma:=true; curs:=#$016B; end;
    if curs=#$01D8 then begin putcomma:=true; curs:=#$00FA; end;
    if curs=#$01DA then begin putcomma:=true; curs:=#$01D4; end;
    if curs=#$01DC then begin putcomma:=true; curs:=#$00F9; end;
    if curs=#$00FC then begin putcomma:=true; curs:=#$0075; end;
    if curs=#$2026 then curs:='_';
    cnv2:=cnv2+curs;
  end;
  if putcomma then cnv2:=cnv2+':';
  cnv:=cnv2;
  cnv2:='';

  mustbegin:=true;
  mustnotbegin:=false;
  befmustnotbegin:=false;
  number:=false;
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    cc:=curs[1];
    if (cc>='0') and (cc<='9') then number:=true; //WTF? Maybe we should test that ALL chars are digits, not ANY?
  end;
  if number then
  begin
    result:=fstrtouni(str);
    exit;
  end;

  curp:='0';
  cc:=' ';
  curcc:='';
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    curpx:='0';
    if Ord(curs[1])<$0080 then cc:=upcase(curs[1])
    else if curs=#$00E1 then begin cc:='A'; curpx:='2'; end
    else if curs=#$00E9 then begin cc:='E'; curpx:='2'; end
    else if curs=#$00ED then begin cc:='I'; curpx:='2'; end
    else if curs=#$00F3 then begin cc:='O'; curpx:='2'; end
    else if curs=#$00FA then begin cc:='U'; curpx:='2'; end
    else if curs=#$00E0 then begin cc:='A'; curpx:='4'; end
    else if curs=#$00E8 then begin cc:='E'; curpx:='4'; end
    else if curs=#$00EC then begin cc:='I'; curpx:='4'; end
    else if curs=#$00F2 then begin cc:='O'; curpx:='4'; end
    else if curs=#$00F9 then begin cc:='U'; curpx:='4'; end
    else if curs=#$0101 then begin cc:='A'; curpx:='1'; end
    else if curs=#$0113 then begin cc:='E'; curpx:='1'; end
    else if curs=#$012B then begin cc:='I'; curpx:='1'; end
    else if curs=#$014D then begin cc:='O'; curpx:='1'; end
    else if curs=#$016B then begin cc:='U'; curpx:='1'; end
    else if curs=#$0103 then begin cc:='A'; curpx:='3'; end
    else if curs=#$0115 then begin cc:='E'; curpx:='3'; end
    else if curs=#$012D then begin cc:='I'; curpx:='3'; end
    else if curs=#$014F then begin cc:='O'; curpx:='3'; end
    else if curs=#$016D then begin cc:='U'; curpx:='3'; end
    else if curs=#$01CE then begin cc:='A'; curpx:='3'; end
    else if curs=#$011B then begin cc:='E'; curpx:='3'; end
    else if curs=#$01D0 then begin cc:='I'; curpx:='3'; end
    else if curs=#$01D2 then begin cc:='O'; curpx:='3'; end
    else if curs=#$01D4 then begin cc:='U'; curpx:='3'; end
    else cc:='?';
    if (((cc>='A') and (cc<='Z')) or (cc=':')) and (cc<>'''') then curcc:=curcc+cc;

    fnd:=FindRomaC(uppercase(curcc),1,{Partial=}true)>=0;
    if ((cc<'A') or (cc>'Z')) and (cc<>':') then
    begin
      if curcc<>'' then cnv2:=cnv2+lowercase(curcc)+curp;
      curcc:='';
      cnv2:=cnv2+cc;
      curp:='0';
    end else
    if ((not fnd) or ((curpx<>'0') and (curp<>'0'))) and
       ((copy(curcc,length(curcc)-1,2)='GU') or
        (copy(curcc,length(curcc)-1,2)='NU') or
        (copy(curcc,length(curcc)-1,2)='NI') or
        (copy(curcc,length(curcc)-1,2)='NO')) then
    begin
      cnv2:=cnv2+lowercase(copy(curcc,1,length(curcc)-2))+curp;
      delete(curcc,1,length(curcc)-2);
      curp:='0';
    end else if (not fnd) or ((curpx<>'0') and (curp<>'0')) then
    begin
      cnv2:=cnv2+lowercase(copy(curcc,1,length(curcc)-1))+curp;
      delete(curcc,1,length(curcc)-1);
      curp:='0';
    end else if (cc='?') or (cc='''') then
    begin
      cnv2:=cnv2+lowercase(curcc)+curp;
      curcc:='';
      curp:='0';
    end;
    if curpx<>'0'then curp:=curpx;
  end;
  result:=cnv2+lowercase(curcc)+curp;
end;



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
  grid.Cells[0,0]:=_l('#00939^ePhonetic');
  grid.Cells[1,0]:=_l('#00632^eWritten');
  grid.Cells[2,0]:=_l('#00317^eTranslation');
  if stat then if learn then
    grid.Cells[3,0]:=_l('#00633^eAdded / Learned') else
    grid.Cells[3,0]:=_l('#00634^eCategories');
  wgcur:=1;
end;

procedure AddWordGrid(grid:TStringGrid;sp1,sp2,sp4,sp3:string);
begin
  grid.Cells[0,wgcur]:=UH_DRAWWORD_KANA+sp2;
  grid.Cells[1,wgcur]:=UH_DRAWWORD_KANJI+sp1;
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

//Splits translation record in old Wakan format into parts:
//  kanji [kana] {translation} rest
procedure SplitWord(s:FString; var sp1,sp2,sp4,sp3:FString);
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
    0:result:=_l('#00638^eProblematic');
    1:result:=_l('#00639^eUnlearned');
    2:result:=_l('#00640^eLearned');
    3:result:=_l('#00641^eMastered');
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

procedure InitColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  colarr:=TStringList.Create;
  colarr.add('0Kanji_Back=FFFFFF,^eBackground');
  colarr.add('0Kanji_Common=000000,^eCommon characters');
  colarr.add('0Kanji_Rare=4F4F4F,^eRare characters');
  colarr.add('0Kanji_Names=005F00,^eCharacters in names');
  colarr.add('0Kanji_Learned=7F0000,^eLearned characters');
  colarr.add('0Kanji_RadCommon=000000,^eCommon radicals');
  colarr.add('0Kanji_RadRare=4F4F4F,^eRare radicals');
  colarr.add('0Kanji_RadLearned=7F0000,^eLearned radicals');
  colarr.add('1Dict_Back=FFFFFF,^eBackground');
  colarr.add('1Dict_Text=000000,^eText');
  colarr.add('1Dict_UnknownChar=2F2F7F,^eUnknown characters');
  colarr.add('1Dict_Problematic=DDDDFF,^eProblematic words');
  colarr.add('1Dict_Unlearned=FFEEDD,^eUnlearned words');
  colarr.add('1Dict_Learned=BBFFFF,^eLearned words');
  colarr.add('1Dict_Mastered=BBFFBB,^eMastered words');
  colarr.add('1Dict_SelBack=BBBBBB,^eBackground (selected)');
  colarr.add('1Dict_SelProblematic=9999BB,^eProblematic words (selected)');
  colarr.add('1Dict_SelUnlearned=BBAA99,^eUnlearned words (selected)');
  colarr.add('1Dict_SelLearned=99BBBB,^eLearned words (selected)');
  colarr.add('1Dict_SelMastered=77BB77,^eMastered words (selected)');
  colarr.add('2Mark_Special=7F007F,^eSpecial markers');
  colarr.add('2Mark_Usage=00007F,^eUsage markers');
  colarr.add('2Mark_Grammatical=7F0000,^eGrammatical markers');
  colarr.add('2Mark_Dict=4F4F4F,^eDictionary markers');
  colarr.add('2Mark_Lesson=004F00,^eLesson markers');
  colarr.add('3Editor_Back=FFFFFF,^eBackground');
  colarr.add('3Editor_Text=000000,^eText color');
  colarr.add('3Editor_ASCII=2F2F2F,^eASCII text');
  colarr.add('3Editor_Active=FF0000,^eText being written');
  colarr.add('3Editor_Aftertouch=0000FF,^eText just converted');
  colarr.add('3Editor_Untranslated=FFFFFF,^eUntranslated text');
  colarr.add('3Editor_NotFound=003FFF,^eText where translation failed');
  colarr.add('3Editor_Particle=FFAAFF,^eEstimated particle');
  colarr.add('3Editor_Translated=EEEEEE,^eWord not in vocabulary');
  colarr.add('3Editor_Problematic=DDDDFF,^eProblematic vocabulary word');
  colarr.add('3Editor_Unlearned=FFEEDD,^eUnlearned vocabulary word');
  colarr.add('3Editor_Learned=BBFFFF,^eLearned vocabulary word');
  colarr.add('3Editor_Mastered=BBFFBB,^eMastered vocabulary word');
  colarr.add('3Editor_HintBack=EFEFEF,^eHint background');
  colarr.add('3Editor_HintSelected=00FFFF,^eHint selected background');
  colarr.add('3Editor_HintText=000000,^eHint text');
  colarr.add('3Editor_AozoraTag=C0C0C0,^eAozora Ruby <tag>');
  colarr.add('3Editor_AozoraComment=C0C0C0,^eAozora Ruby ［comment］');
  colarr.add('3Editor_AozoraRuby=C0C0C0,^eAozora Ruby 《ruby》');
  colarr.add('4Popup_Back=A0FFFF,^eBackground');
  colarr.add('4Popup_Lines=000000,^eLines');
  colarr.add('4Popup_Card=FFFFFF,^eCharacter card');
  colarr.add('4Popup_Text=000000,^eText on the caracter card');
  colval:=TStringList.Create;
  reg:=TRegIniFile.Create(WakanRegKey);
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
begin
  result:=colsval[colsarr.IndexOf(col)];
end;

procedure WriteColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  reg:=TRegIniFile.Create(WakanRegKey);
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


//TODO: Test Unicode conversion for the following functions

function ChinTo(s:FString):FString;
var s2:FString;
  cd:FString;
  bk:FString;
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
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if ({$IFNDEF UNICODE}s2[1]>'3'{$ELSE}Ord(s2[1])>$3000{$ENDIF})
    and TChar.Locate('Unicode',s2) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),43);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk);
end;

function ChinFrom(s:FString):FString;
var s2:FString;
  cd:FString;
  bk:FString;
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
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if ({$IFNDEF UNICODE}s2[1]>'3'{$ELSE}Ord(s2[1])>$3000{$ENDIF})
    and TChar.Locate('Unicode',s2) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),44);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk);
end;


{
Vocabulary entries are stored in severely deprecated format.
They have to be upgraded before working with them.
}
function FixVocabEntry(const s: string): string;
begin
  Result := s;
 {$IFDEF UNICODE}
 //User dictionaries often have inline markers in old format (<gvn>)
  repl(Result,'<',UH_LBEG);
  repl(Result,'>',UH_LEND);
 {$ENDIF}
end;

{ Reverts some fixes when storing the string back into vocabulary }
function UnfixVocabEntry(const s:string):string;
begin
  Result := s;
 {$IFDEF UNICODE}
  repl(Result,UH_LBEG,'<');
  repl(Result,UH_LEND,'>');
 {$ENDIF}
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
  m:=0;//not really used but shut up delphi
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
var w:UnicodeString;
  chn:string;
  i:integer;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
  c.Font.Name:=fontface;
  c.Font.Height:=fs;
//    c.Font.Style:=[];
  chn:=DrawTone(c,x,y,fs,ch,false);
  w := fstrtouni(chn);
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
  TextOutW(c.Handle,x,y,PWideChar(w),flength(chn));
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
  if (length(s)>0) and (Colx=0) and (s[1]=UH_DRAWWORD_KANA) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
//    if showroma then
//      Grid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,KanaToRomaji(s,romasys,curlang)) else
//    DrawUnicode(Grid.Canvas,Rect.Left+2,Rect.Top+2,12,s,FontSmall);
    DrawKana(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall,showroma,romasys,curlang);
  end else
  if (length(s)>0) and (s[1]=UH_DRAWWORD_KANJI) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    if (Length(s)>0) and (s[1]=UH_UNKNOWN_KANJI) then
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
    sbef:='';
    while length(s)>0 do
    begin
//      if sbef=s then
//      begin
//        showmessage(sbef);
//      end;
      sbef:=s;
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

procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:FString; ch:integer;boldfont:boolean);
var s1,sx1,s2,s3,s4:FString;
begin
  SplitWord(s,s1,s2,s3,s4);
  if curlang='c'then
  begin
    if s2[1]=ALTCH_EXCL then delete(s2,1,2);
    s2:=KanaToRomaji(s2,romasys,curlang);
    s2:=ConvertPinYin(s2);
    sx1:=s1;
    s1:=s1+UH_SPACE+s2;
    DrawWordInfo(Canvas,rect,false,false,0,UH_DRAWWORD_KANJI+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(sx1)*ch+ch+(flength(s2) div 2)*ch;
  end else
  begin
    DrawWordInfo(Canvas,rect,false,false,0,UH_DRAWWORD_KANJI+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(remexcl(s1))*ch;
  end;
  if (s2<>s1) and (curlang='j') then
  begin
    if s2[1]=ALTCH_EXCL then s2:=ALTCH_EXCL+s1[2]+UH_UNKNOWN_KANJI+copy(s2,3,length(s2)-2) else s2:=UH_UNKNOWN_KANJI+s2;
    DrawWordInfo(Canvas,rect,false,false,1,UH_DRAWWORD_KANJI+s2,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(remexcl(s2))*ch;
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
    SetROP2(STB_Canvas.Handle,R2_NOT); //Maybe oldR2?!
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

procedure DeleteDirectory(dir:string);
var sRec: TSearchRec;
begin
  if dir='' then exit; //just in case! don't delete random files
  if not FindFirst(dir + '\*.*', faAnyFile, sRec) = 0 then
    exit;
  repeat
    if sRec.Attr and faDirectory <> 0 then
      if (sRec.Name = '.') or (sRec.Name = '..') then begin
       //Nothing
      end else
        RemoveDirectory(PChar(dir + '\' + sRec.Name))
    else
      DeleteFile(PChar(dir + '\' + sRec.Name));
  until FindNext(sRec) <> 0;
  FindClose(sRec);
  Windows.RemoveDirectory(PChar(dir));
end;

{ Universal backup function. Backups everything to the directory designated for backups. }
procedure Backup(const filename: string);
begin
 //For now works as it did in previous Wakan versions.
 //Has to be reworked to put backups into user folder.
  {$I-}
  mkdir(UserDataDir+'\backup');
  {$I+}
  ioresult;
 //dir\wakan.usr --> wakan-20130111.usr
  CopyFile(PChar(filename),pchar(UserDataDir+'\backup\'+ChangeFileExt(ExtractFilename(filename),'')+'-'
    +FormatDateTime('yyyymmdd',now)+ExtractFileExt(filename)),false);
end;


function _l(const id:string):string;
begin
  result:=fLanguage.TranslateString(id);
end;

function _l(const id:string; args: array of const):string;
begin
  Result := Format(fLanguage.TranslateString(id), args);
end;


var
  i:integer;

initialization
  AppFilename := GetModuleFilenameStr(0);
  AppFolder := ExtractFilePath(AppFilename);
  WakanVer := GetFileVersionInfoStr(AppFilename)
    {$IFDEF UNICODE}+' unicode'{$ENDIF};

  CurPBox:=nil;
  for i:=1 to MAX_INTTEXTINFO do itt[i].act:=false;
  GridFontSize:=14;
  STB_Canvas:=nil;

  roma_t:=TRomajiTranslator.Create;
  romac:=TStringList.Create;

finalization
  romac.Free;
  roma_t.Free;


end.
