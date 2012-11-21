unit JWBUnit;

interface

uses Graphics, Windows, SysUtils, Classes, Dialogs, Grids, Forms, ExtCtrls, Registry;

const //Character types for EvalChar
  EC_UNKNOWN          = 0; // unrecognized
  EC_IDG_CHAR         = 1; // ideographic char
  EC_HIRAGANA         = 2; // hiragana
  EC_KATAKANA         = 3; // katakana
  EC_IDG_PUNCTUATION  = 4; // ideographic punctuation
  EC_IDG_OTHER        = 5; // ideographic other
  EC_LATIN_FW         = 6; // full-width latin
  EC_LATIN_HW         = 7; // half-width latin
  EC_KATAKANA_HW      = 8; // half-width katakana

function UnicodeToHex(s:widestring):string;
function HexToUnicode(s:string):widestring;
function CombUniToHex(s:string):string;
function HexToCombUni(s:string):string;
function UnicodeToML(s:widestring):string;
procedure BeginDrawReg(p:TPaintBox);
procedure EndDrawReg;
function FindDrawReg(p:TPaintBox;x,y:integer;var cx,cy,cy2:integer):string;
procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:string;fontface:string);
function ConvertPinYin(s:string):string;
function DeconvertPinYin(s:string):string;
procedure DrawKana(c:TCanvas;x,y,fs:integer;ch:string;fontface:string;showr:boolean;romas:integer;lang:char);
function KanaToRomaji(s:string;romatype:integer;lang:char):string;
function RomajiToKana(s:string;romatype:integer;clean:boolean;lang:char):string;
procedure BuildRomaList;
procedure FreeRomaList;
function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean;fontsize:integer;boldfont:boolean):integer;
procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:string; ch:integer;boldfont:boolean);
procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
procedure DrawKanjiCard(canvas:TCanvas;u:string;x,y:integer;ch:double;stcount,outlin,alt,rad,inlin,comp,read,mean,strokeorder,fullcomp,sortfreq:boolean;sizhor,sizvert,nofullcomp:integer;calfont:string);
procedure ClearKanjiCardCache;
procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
procedure CreateKnownList(listno:integer;charnumber:integer);
procedure FreeKnownLists;
procedure SaveKnownList(listno:integer;filename:string);
procedure LoadKnownList(listno:integer;stream:TStream);
procedure PaintScreenTipBlock;
procedure SetScreenTipBlock(x1,y1,x2,y2:integer;canvas:TCanvas);
function ConvertEdictEntry(s:string;var mark:string):string;
function GetMarkAbbr(mark:char):string;
function EnrichDictEntry(s,mark:string):string;
function IsKnown(listno:integer;charno:string):boolean;
function CheckKnownKanji(kanji:string):string;
function ChinTo(s:string):string;
function ChinFrom(s:string):string;
procedure SetKnown(listno:integer;charno:string;known:boolean);
function ChooseFont(charsets:array of TFontCharset;teststring:string;var supportedsets:string;defaultfont:string;selectfirst:boolean):string;
function EvalChar(char:string):integer;
function StateStr(i:integer):string;
function DateForm(s:string):string;
procedure WritelnMixUni(var f:file;s:string);
function StripCatName(s:string):string;
procedure WriteMarkTable;
procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
procedure AddWordGrid(var grid:TStringGrid;sp1,sp2,sp4,sp3:string);
procedure FinishWordGrid(grid:TStringGrid);
procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
procedure InitColors;
function GetColorString(i:integer):string;
procedure SetCol(col:integer;val:TColor);
function GetCol(col:integer):TColor;
function Col(col:string):TColor;
procedure WriteColors;
procedure SetColDefault(i:integer);
procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);

const Color_Max=100;

var FontStrokeOrder,FontChinese,FontChineseGB,FontChineseGrid,FontChineseGridGB,FontJapaneseGrid,FontJapanese,FontSmall,FontRadical,FontEnglish,FontPinYin:string;
    kcchind,kcchcomp,roma,romac:TStringList;
    GridFontSize:integer;

implementation

uses JWBMenu, UnicodeFont, JWBSettings, JWBUser;

type TIntTextInfo=record
        act:boolean;
        p:TPaintBox;
        x,y,fs:integer;
        s:string;
     end;

const MAX_INTTEXTINFO = 4000;

var KnownList:array[1..20000] of pointer;
    KnownListSize:integer;
    fontlist:TStringList;
    wgcur:integer;
    itt:array[1..MAX_INTTEXTINFO] of TIntTextInfo;
    curpbox:TPaintBox;
    colarr:TStringList;
    colval:TStringList;
    colsval:array[0..Color_Max] of TColor;
    colsarr:TStringList;
    STB_x1,STB_y1,STB_x2,STB_y2:integer;
    STB_canvas:TCanvas;

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

function UnicodeToHex(s:widestring):string;
var i:integer;
    c:widechar;
    d:word;
    s2,s3:string;
begin
  s2:='';
  for i:=1 to length(s) do
  begin
    c:=s[i];
    d:=word(c);
    s3:=format('%4.4X',[d]);
    s2:=s2+s3;
  end;
  result:=s2;
end;

function UnicodeToML(s:widestring):string;
var i:integer;
    c:widechar;
    d:word;
    s2,s3:string;
begin
  s2:='';
  for i:=1 to length(s) do
  begin
    c:=s[i];
    d:=word(c);
    s3:=format('&#%d,',[d]);
    s2:=s2+s3;
  end;
  result:=s2;
end;

function HexToUnicode(s:string):widestring;
var s2:widestring;
    d:word;
    c:widechar;
    i:integer;
begin
  s2:='';
  try
    if (length(s)>0) and (s[1]='U') then delete(s,1,1);
    for i:=1 to length(s) div 4 do
    begin
      d:=StrToInt('0x'+copy(s,(i-1)*4+1,4));
      c:=widechar(d);
      s2:=s2+c;
    end;
  except
    if Application.MessageBox(pchar('I''m really sorry but INTERNAL ERROR has occured. Please report this error to the author.'#13#13'HtU warning: '+s+#13#13'Raise exception?'),'Internal error',MB_ICONERROR or MB_YESNO)=idYes then raise;
  end;
  result:=s2;
end;

function DrawTone(c:TCanvas;x,y,fw:integer;s:string;unicode:boolean;dodraw:boolean):string;
var tb:integer;
    flat:boolean;
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
  flat:=false;
  s5:='';
  c.Font.Height:=fw;
  while s<>'' do
  begin
    if unicode then s4:=copy(s,1,4) else s4:=s[1];
    s3:=s4;
    if pos('F03',s3)=1 then delete(s3,1,3) else if unicode then s3:='XXXX';
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
      flat:=false;
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

procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:string;fontface:string);
var w:pwidechar;
    chn:string;
    i:integer;
begin
  if ch<>'' then
  begin
    SetBkMode(c.Handle,TRANSPARENT);
    c.Font.Name:=fontface;
    c.Font.Height:=fs;
//    c.Font.Style:=[];
    chn:=DrawTone(c,x,y,fs,ch,true,false);
    w:=pwidechar(HexToUnicode(chn));
    if chn<>ch then
    begin
      c.Font.Name:=FontRadical;
      fs:=round(fs/8*7);
      c.Font.Height:=fs;
      y:=y+fs div 4;
    end;
    chn:=DrawTone(c,x,y-fs div 4,fs,ch,true,true);
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
    TextOutW(c.Handle,x,y,w,length(chn) div 4);
  end;
end;

function repl(var s:string;sub,repl:string):string;
begin
  while pos(sub,s)>0 do
    s:=copy(s,1,pos(sub,s)-1)+repl+copy(s,pos(sub,s)+length(sub),length(s)-pos(sub,s)+1-length(sub));
  result:=s;
end;

function ConvertEdictEntry(s:string;var mark:string):string;
var postm,post2m:string;
    s2:string;
    inmarker:boolean;
    curm,marker:string;
    i:integer;
    markerd:boolean;
    oldm:string;
    mm:byte;
    insection:boolean;
begin
  s2:='';
  marker:='';
  markerd:=false;
  mark:='';
  insection:=false;
  for i:=1 to length(s) do
  begin
    if s[i]='/'then
    begin
      s2:=s2+', ';
      insection:=false;
    end
    else if s[i]=' 'then
    begin
      if markerd then markerd:=false else if inmarker then marker:=marker+' 'else s2:=s2+' ';
    end
    else if s[i]='('then inmarker:=true
    else if s[i]=')'then
    begin
      oldm:=marker;
      while length(marker)>0 do
      begin
        if pos(',',marker)>0 then curm:=copy(marker,1,pos(',',marker)-1) else curm:=marker;
        delete(marker,1,length(curm));
        if (length(marker)>0) and (marker[1]=',') then delete(marker,1,1);
        markerd:=true;
        if curm='abbr'then mm:=1 else
        if curm='arch'then mm:=2 else
        if curm='fam'then mm:=3 else
        if curm='fem'then mm:=4 else
        if curm='MA'then mm:=5 else
        if curm='male'then mm:=6 else
        if curm='m-sl'then mm:=7 else
        if curm='vulg'then mm:=8 else
        if curm='X'then mm:=9 else
        if curm='col'then mm:=10 else
        if curm='adj'then mm:=11 else
        if curm='adv'then mm:=12 else
        if curm='adj-na'then mm:=13 else
        if curm='adj-no'then mm:=14 else
        if curm='adj-pn'then mm:=15 else
        if curm='adj-s'then mm:=16 else
        if curm='adj-t'then mm:=17 else
        if curm='aux'then mm:=18 else
        if curm='aux-v'then mm:=19 else
        if curm='conj'then mm:=20 else
        if curm='exp'then mm:=21 else
        if curm='gikun'then mm:=22 else
        if curm='gram'then mm:=23 else
        if curm='id'then mm:=24 else
        if curm='int'then mm:=25 else
        if curm='n'then mm:=26 else
        if curm='n-adv'then mm:=27 else
        if curm='n-t'then mm:=28 else
        if curm='n-suf'then mm:=29 else
        if curm='neg'then mm:=30 else
        if curm='neg-v'then mm:=31 else
        if curm='pref'then mm:=32 else
        if curm='suf'then mm:=33 else
        if curm='v1'then mm:=34 else
        if curm='v5'then mm:=35 else
        if curm='v5u'then mm:=36 else
        if curm='v5k'then mm:=37 else
        if curm='v5g'then mm:=38 else
        if curm='v5s'then mm:=39 else
        if curm='v5t'then mm:=40 else
        if curm='v5n'then mm:=41 else
        if curm='v5b'then mm:=42 else
        if curm='v5m'then mm:=43 else
        if curm='v5r'then mm:=44 else
        if curm='v5k-s'then mm:=45 else
        if curm='v5z'then mm:=46 else
        if curm='v5aru'then mm:=47 else
        if curm='v5uru'then mm:=48 else
        if curm='vi'then mm:=49 else
        if curm='vs'then mm:=50 else
        if curm='vs-s'then mm:=51 else
        if curm='vk'then mm:=52 else
        if curm='vt'then mm:=53 else
        if curm='hon'then mm:=54 else
        if curm='hum'then mm:=55 else
        if curm='ik'then mm:=56 else
        if curm='iK'then mm:=57 else
        if curm='io'then mm:=58 else
        if curm='obs'then mm:=59 else
        if curm='obsc'then mm:=60 else
        if curm='ok'then mm:=61 else
        if curm='oK'then mm:=62 else
        if curm='pol'then mm:=63 else
        if curm='uk'then mm:=64 else
        if curm='uK'then mm:=65 else
        if curm='P'then mm:=66 else
        begin
          if (oldm='1') or (oldm='2') or (oldm='3') then insection:=true;
          s2:=s2+'('+oldm+')'; mm:=0; markerd:=false;
          marker:='';
        end;
        if insection then mm:=0;
        if mm<>0 then mark:=mark+chr(mm+32);
      end;
      marker:='';
      inmarker:=false;
    end else if inmarker then marker:=marker+s[i] else s2:=s2+s[i];
  end;
  if copy(s2,length(s2)-1,2)=', 'then delete(s2,length(s2)-1,2);
  s2:=trim(s2);
  result:=s2;
end;

function GetMarkEdict(mark:char):string;
begin
  result:='';
  case mark of
    #33:result:='abbr';
    #34:result:='arch';
    #35:result:='fam';
    #36:result:='fem';
    #37:result:='MA';
    #38:result:='male';
    #39:result:='m-sl';
    #40:result:='vulg';
    #41:result:='X';
    #42:result:='col';
    #43:result:='adj';
    #44:result:='adv';
    #45:result:='adj-na';
    #46:result:='adj-no';
    #47:result:='adj-pn';
    #48:result:='adj-s';
    #49:result:='adj-t';
    #50:result:='aux';
    #51:result:='aux-v';
    #52:result:='conj';
    #53:result:='exp';
    #54:result:='gikun';
    #55:result:='gram';
    #56:result:='id';
    #57:result:='int';
    #58:result:='n';
    #59:result:='n-adv';
    #60:result:='n-t';
    #61:result:='n-suf';
    #62:result:='neg';
    #63:result:='neg-v';
    #64:result:='pref';
    #65:result:='suf';
    #66:result:='v1';
    #67:result:='v5';
    #68:result:='v5u';
    #69:result:='v5k';
    #70:result:='v5g';
    #71:result:='v5s';
    #72:result:='v5t';
    #73:result:='v5n';
    #74:result:='v5b';
    #75:result:='v5m';
    #76:result:='v5r';
    #77:result:='v5k-s';
    #78:result:='v5z';
    #79:result:='v5aru';
    #80:result:='v5uru';
    #81:result:='vi';
    #82:result:='vs';
    #83:result:='vs-s';
    #84:result:='vk';
    #85:result:='vt';
    #86:result:='hon';
    #87:result:='hum';
    #88:result:='ik';
    #89:result:='iK';
    #90:result:='io';
    #91:result:='obs';
    #92:result:='obsc';
    #93:result:='ok';
    #94:result:='oK';
    #95:result:='pol';
    #96:result:='uk';
    #97:result:='uK';
    #98:result:='P';
  end;
end;

function GetMarkDescription(edictmark:String):string;
var i:integer;
    s:String;
begin
  for i:=0 to markersl.Count-1 do
  begin
    if (pos(edictmark+' ',markersl[i])=1) or
       (pos(edictmark+#9,markersl[i])=1) then
    begin
      s:=markersl[i];
      delete(s,1,length(edictmark));
      s:=trim(s);
      result:=s;
      exit;
    end;
  end;
  result:='';
end;

procedure WriteMarkTable;
var t:textfile;
    i:char;
    s:string;
begin
  assignfile(t,'marks.html');
  rewrite(t);
  writeln(t,'<table>');
  for i:=#33 to #98 do
  begin
    writeln(t,'  <tr>');
    s:=GetMarkAbbr(i);
    writeln(t,'    <td>'+copy(s,2,length(s)-1)+'</td>');
    writeln(t,'    <td>'+s[1]+'</td>');
    s:=GetMarkEdict(i);
    writeln(t,'    <td>'+s+'</td>');
    writeln(t,'    <td>'+GetMarkDescription(s)+'</td>');
    writeln(t,'  </tr>');
  end;
  closefile(t);
end;

function GetMarkAbbr(mark:char):string;
var mr:string;
begin
  mr:='1?';
  case mark of
    #33:mr:='1abbr';
    #34:mr:='1archaic';
    #35:mr:='1familiar';
    #36:mr:='1female';
    #37:mr:='1martial-arts';
    #38:mr:='1male';
    #39:mr:='1manga-slang';
    #40:mr:='1vulgar';
    #41:mr:='1rude';
    #42:mr:='1col';
    #43:mr:='gadj';
    #44:mr:='gadv';
    #45:mr:='gna-adj';
    #46:mr:='gno-adj';
    #47:mr:='gpren-adj';
    #48:mr:='gspec-adj';
    #49:mr:='gtaru-adj';
    #50:mr:='gaux';
    #51:mr:='gaux-v';
    #52:mr:='gconj';
    #53:mr:='gexpr';
    #54:mr:='ggikun';
    #55:mr:='ggram';
    #56:mr:='gid';
    #57:mr:='gint';
    #58:mr:='gn';
    #59:mr:='gn-adv';
    #60:mr:='gn-temp';
    #61:mr:='gn-suf';
    #62:mr:='gneg';
    #63:mr:='gneg-verb';
    #64:mr:='gpref';
    #65:mr:='gsuf';
    #66:mr:='gru-v';
    #67:mr:='gu-v';
    #68:mr:='gu-v';
    #69:mr:='gu-v';
    #70:mr:='gu-v';
    #71:mr:='gu-v';
    #72:mr:='gu-v';
    #73:mr:='gu-v';
    #74:mr:='gu-v';
    #75:mr:='gu-v';
    #76:mr:='gu-v';
    #77:mr:='gIku-v';
    #78:mr:='gzuru-v';
    #79:mr:='garu-v';
    #80:mr:='guru-v';
    #81:mr:='gintrans-verb';
    #82:mr:='gp-suru';
    #83:mr:='gsuru-v';
    #84:mr:='gkuru-v';
    #85:mr:='gtrans-verb';
    #86:mr:='shonor';
    #87:mr:='shum';
    #88:mr:='sirreg-kana';
    #89:mr:='sirreg-kanji';
    #90:mr:='sirreg-okurigana';
    #91:mr:='sobsolete';
    #92:mr:='sobscure';
    #93:mr:='soutdated-kana';
    #94:mr:='soutdated-kanji';
    #95:mr:='spolite';
    #96:mr:='skana';
    #97:mr:='skanji';
    #98:mr:='spop';
  end;
  result:=mr;
end;

function EnrichDictEntry(s,mark:string):string;
var s2:string;
    i:integer;
    s3:string;
    mar1,marg,mars:string;
begin
  s2:=s;
  mar1:=''; marg:=''; mars:='';
  for i:=1 to length(mark) do
  begin
    s3:=GetMarkAbbr(mark[i]);
    if s3[1]='s'then mars:=mars+' <'+s3+'>';
    if s3[1]='g'then marg:=marg+'<'+s3+'> ';
    if s3[1]='1'then mar1:=mar1+'<'+s3+'> ';
  end;
  result:=marg+mar1+s2+mars;
end;

function ResolveCrom(s:string;posin,posout:integer;clean:boolean):string;
var s2,s3:string;
    cr:string;
    cl:integer;
    i:integer;
begin
  s:=uppercase(s);
  s2:='';
  while s<>'' do
  begin
    cl:=0;
    for i:=0 to (romac.count div 4)-1 do
    begin
      if pos(uppercase(romac[i*4+posin]),s)=1 then
      begin
        if length(romac[i*4+posin])>cl then
        begin
          cl:=length(romac[i*4+posin]);
          cr:=romac[i*4+posout];
        end;
      end;
    end;
    if cl>0 then s2:=s2+cr else
      if s[1]='-'then s2:=s2+UnicodeToHex('-') else
      if s[1]='_'then s2:=s2+UnicodeToHex('_') else
      if pos(UnicodeToHex('-'),s)=1 then s2:=s2+'-'else
      if pos(UnicodeToHex('_'),s)=1 then s2:=s2+'_'else
      if not clean then
      begin
        if posout>0 then s2:=s2+'?'else s2:=s2+'003F';
      end;
    if cl>0 then delete(s,1,cl) else if posin>0 then delete(s,1,1) else delete(s,1,4);
    if posin=0 then
    begin
      if (length(s)>3) and (pos('F03',s)=1) and (s[4]>='0') and (s[4]<='5') then
      begin
        s2:=s2+s[4];
        delete(s,1,4);
      end else s2:=s2+'0';
    end else
    begin
      if (length(s)>0) and (s[1]>='0') and (s[1]<='5') then
      begin
        if posout>0 then s2:=s2+s[1] else s2:=s2+'F03'+s[1];
        delete(s,1,1);
      end else
      begin
        if posout>0 then s2:=s2+'0'else s2:=s2+'F030';
      end;
    end;
  end;
  if posout>0 then result:=lowercase(s2) else result:=s2;
end;

function KanaToRomaji(s:string;romatype:integer;lang:char):string;
var curkana:string;
    i:integer;
    fn:string;
    s2:string;
begin
  if lang='j'then
  begin
    while length(s)>0 do
    begin
      fn:='';
      if (copy(s,1,4)=UnicodeToHex('-')) then fn:='-';
      if (copy(s,1,4)=UnicodeToHex('_')) then fn:='_';
      curkana:=copy(s,1,8);
      for i:=0 to roma.Count-1 do if (i mod 5=0) or (i mod 5=1) then
        if (Uppercase(curkana)=Uppercase(roma[i])) then
          begin fn:=roma[(i div 5)*5+romatype+1]; break; end;
      if fn='' then delete(curkana,5,4);
      if fn='' then for i:=0 to roma.Count-1 do if (i mod 5=0) or (i mod 5=1) then
        if Uppercase(curkana)=Uppercase(roma[i]) then
          begin fn:=roma[(i div 5)*5+romatype+1]; break; end;
      if fn='' then if (curkana[1]='0') and (curkana[2]='0') then fn:=HexToUnicode(curkana);
      if fn='' then fn:='?';
      delete(s,1,length(curkana));
      if (fn='O') and (length(s2)>0) then fn:=upcase(s2[length(s2)]);
      s2:=s2+fn;
    end;
    repl(s2,'a-','aa');
    repl(s2,'i-','ii');
    repl(s2,'u-','uu');
    repl(s2,'e-','ee');
    repl(s2,'o-','oo');
    if romatype>2 then repl(s2,'ou','oo');
    if romatype>2 then repl(s2,'ei','ee');
    repl(s2,'Dk','kk');
    repl(s2,'Dg','gg');
    repl(s2,'Ds','ss');
    repl(s2,'Dz','zz');
    repl(s2,'Dt','tt');
    repl(s2,'Dd','dd');
    repl(s2,'Dn','nn');
    repl(s2,'Dh','hh');
    repl(s2,'Db','bb');
    repl(s2,'Dp','pp');
    repl(s2,'Dm','mm');
    repl(s2,'Dr','rr');
    repl(s2,'Dy','yy');
    repl(s2,'Dw','ww');
    repl(s2,'Dj','jj');
    repl(s2,'Dš','šš');
    repl(s2,'Dè','èè');
    repl(s2,'Df','ff');
    repl(s2,'Dc','cc');
    repl(s2,'''','');
    if romatype>1 then repl(s2,'nb','mb');
    if romatype>1 then repl(s2,'np','mp');
    if romatype>1 then repl(s2,'nm','mm');
    if romatype=3 then
    begin
      repl(s2,'aa','á');
      repl(s2,'ii','í');
      repl(s2,'uu','ú');
      repl(s2,'oo','ó');
      repl(s2,'ee','é');
    end;
    if (length(s2)>0) and (s2[length(s2)]='''') then delete(s2,length(s2),1);
    result:=s2;
  end;
  if lang='c'then
  begin
    result:=ResolveCrom(s,0,romatype,false);
  end;
end;

function RomajiToKana(s:string;romatype:integer;clean:boolean;lang:char):string;
var s2,s3,fn:string;
    kata:integer;
    l,i,j:integer;
begin
  if lang='j'then
  begin
    s2:=s;
    if romatype>1 then repl(s2,'mb','nb');
    if romatype>1 then repl(s2,'mp','np');
    if romatype>1 then repl(s2,'mm','nm');
    repl(s2,'+','x');
    repl(s2,'kk','Dk');
    repl(s2,'gg','Dg');
    repl(s2,'ss','Ds');
    repl(s2,'zz','Dz');
    repl(s2,'tt','Dt');
    repl(s2,'dd','Dd');
  //  repl(s2,'nn','Dn');
    repl(s2,'hh','Dh');
    repl(s2,'bb','Db');
    repl(s2,'pp','Dp');
    repl(s2,'mm','Dm');
    repl(s2,'rr','Dr');
    repl(s2,'yy','Dy');
    repl(s2,'ww','Dw');
    repl(s2,'jj','Dj');
    repl(s2,'šš','Dš');
    repl(s2,'èè','Dè');
    repl(s2,'ff','Df');
    repl(s2,'cc','Dc');
    if romatype=3 then
    begin
      repl(s2,'á','aa');
      repl(s2,'í','ii');
      repl(s2,'ó','oo');
      repl(s2,'ú','uu');
      repl(s2,'ù','uu');
      repl(s2,'é','ee');
    end;
    kata:=0;
    s3:='';
  //  if romatype>1 then repl(s2,'oo','ou');
  //  if romatype>1 then repl(s2,'ee','ei');
    if (length(s2)>0) and (s2[1]<>'Q') then
    begin
      if (s2[1]='K') then
      begin
        repl(s2,'aa','a-');
        repl(s2,'ii','i-');
        repl(s2,'uu','u-');
        repl(s2,'ee','e-');
        repl(s2,'oo','o-');
        repl(s2,'yaA','ya-');
        repl(s2,'yuU','yu-');
        repl(s2,'yoO','yo-');
      end;
    end;
    while length(s2)>0 do
    begin
      fn:='';
      if s2[1]='_'then fn:=UnicodeToHex('_');
      if s2[1]='-'then fn:=UnicodeToHex('-');
      for i:=0 to (roma.Count div 5)-1 do
      begin
        if (pos(roma[i*5+1+romatype],s2)=1) then
        begin
          l:=length(roma[i*5+1+romatype]);
          fn:=roma[i*5+kata];
          break;
        end else
        if (romatype>0) and (pos(roma[i*5+2],s2)=1) then
        begin
          l:=length(roma[i*5+2]);
          fn:=roma[i*5+kata];
          break;
        end;
      end;
      if fn='' then
      for j:=2 to 4 do for i:=0 to (roma.Count div 5)-1 do if pos(roma[i*5+j],s2)=1 then
      begin
        fn:=roma[i*5+kata];
        l:=length(roma[i*5+j]);
        break;
      end;
      if fn='' then
      begin
        if clean then fn:='' else if s2[1]<>'''' then fn:=Format('00%2.2X',[ord(s2[1])]) else fn:='';
        l:=1;
      end;
      if s2[1]='H'then
      begin
        kata:=0;
        l:=1;
        fn:='';
      end;
      if (s2[1]='K') or (s2[1]='Q') then
      begin
        kata:=1;
        l:=1;
        fn:='';
      end;
      delete(s2,1,l);
      s3:=s3+fn;
    end;
    result:=s3;
  end;
  if lang='c'then
  begin
    repl(s,'v','u:');
    result:=ResolveCrom(s,romatype,0,clean);
  end;
end;

procedure FreeRomaList;
begin
  roma.Free;
  romac.Free;
end;

procedure BuildRomaList;
var i,j:integer;
    s,s2:string;
begin
  roma:=TStringList.Create;
  TRomaji.First;
  while not TRomaji.EOF do
  begin
    roma.Add(TRomaji.Str(TRomaji.Field('Hiragana')));
    roma.Add(TRomaji.Str(TRomaji.Field('Katakana')));
    roma.Add(TRomaji.Str(TRomaji.Field('Japanese')));
    roma.Add(TRomaji.Str(TRomaji.Field('English')));
    roma.Add(TRomaji.Str(TRomaji.Field('Czech')));
    TRomaji.Next;
  end;
  romac:=TStringList.Create;
  for i:=0 to bopomofol.Count-1 do
  begin
    s:=bopomofol[i];
    j:=0;
    while s<>'' do
    begin
      while (s<>'') and (s[1]=' ') do delete(s,1,1);
      inc(j);
      if pos(' ',s)>0 then s2:=copy(s,1,pos(' ',s)-1) else s2:=s;
      delete(s,1,length(s2));
      if (s2<>'') and (s2[1]='<') then delete(s2,1,1);
      if (s2<>'') and (s2[length(s2)]='>') then delete(s2,length(s2),1);
      if (j>=4) and (j<=7) then romac.Add(s2);
      while (s<>'') and (s[1]=' ') do delete(s,1,1);
    end;
    if (j<>7) and (j<>0) then showmessage('BOPOMOFO.LST: Internal error: '+bopomofol[i]+' >>> '+inttostr(j));
  end;
end;

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
  grid.Cells[0,wgcur]:='#'+sp2;
  grid.Cells[1,wgcur]:='@'+sp1;
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
    for i:=1 to grid.RowCount-1 do grid.RowHeights[i]:=(GridFontSize+2)*DrawWordInfo(grid.Canvas,grid.CellRect(2,i),false,false,2,grid.Cells[2,i],true,true,GridFontSize,true);
  FinishWordGrid(grid);
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
    if (length(s)>1) and (s[1]='!') then c:=s[2];
    if (length(s)>2) and (s[2]='!') then c:=s[3];
    case c of
      ' ':if sel then Canvas.Brush.Color:=Col('Dict_SelBack') else Canvas.Brush.Color:=Col('Dict_Back');
      '0':if sel then Canvas.Brush.Color:=Col('Dict_SelProblematic') else Canvas.Brush.Color:=Col('Dict_Problematic');
      '1':if sel then Canvas.Brush.Color:=Col('Dict_SelUnlearned') else Canvas.Brush.Color:=Col('Dict_Unlearned');
      '2':if sel then Canvas.Brush.Color:=Col('Dict_SelLearned') else Canvas.Brush.Color:=Col('Dict_Learned');
      '3':if sel then Canvas.Brush.Color:=Col('Dict_SelMastered') else Canvas.Brush.Color:=Col('Dict_Mastered');
    end;
  end;
  if (length(s)>1) and (s[1]='!') then delete(s,1,2);
  if (length(s)>2) and (s[2]='!') then delete(s,2,2);
  if (length(s)>0) and (Colx=0) and (s[1]='#') then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
//    if showroma then
//      Grid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,KanaToRomaji(s,romasys,curlang)) else
//    DrawUnicode(Grid.Canvas,Rect.Left+2,Rect.Top+2,12,s,FontSmall);
    DrawKana(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall,showroma,romasys,curlang);
  end else
  if (length(s)>0) and (s[1]='@') then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    if (s[1]='U') then
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
    if (length(s)>1) and (s[1]='~') then
    begin
      if s[2]='I'then cursiv:=true;
//      if not fUser.CheckBox1.Checked then cursiv:=false;
      delete(s,1,2);
    end;
    if (length(s)>1) and (s[1]='%') then
    begin
      if (fSettings.CheckBox69.Checked) then try FontColor:=strtoint('0x'+copy(s,6,2)+copy(s,4,2)+copy(s,2,2)); except FontColor:=clWindowText; end;
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
        if pos('>',s)>0 then curs:=copy(s,1,pos('>',s)-1) else curs:=s;
      if not inmar then
        if pos('<',s)>0 then curs:=copy(s,1,pos('<',s)-1) else curs:=s;
      delete(s,1,length(curs));
      if (length(s)>0) and ((s[1]='<') or (s[1]='>')) then delete(s,1,1);
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
          if (length(curs)>0) and (curs[1]=' ') then curs[1]:='~';
          if inmar or (pos(' ',curs)=0) or (Canvas.TextExtent(copy(curs,1,pos(' ',curs)-1)).cx+rect.left+2+x>rect.right) then
          begin
            if (length(curs)>0) and (curs[1]='~') then curs[1]:=' ';
            x:=0;
            y:=y+FontSize+2;
            rect2.left:=rect.left+2;
            rect2.top:=rect.top+2+y;
            result:=result+1;
          end else
          if not inmar and (pos(' ',curs)>0) then
          begin
            if (length(curs)>0) and (curs[1]=' ') then curs[1]:='~';
            s:=copy(curs,pos(' ',curs),length(curs)-pos(' ',curs)+1)+'<'+s;
            curs:=copy(curs,1,pos(' ',curs)-1);
            if (length(curs)>0) and (curs[1]='~') then curs[1]:=' ';
            resinmar:=true;
          end else
          begin
            curs:=s;
            if (length(curs)>0) and (curs[1]='~') then curs[1]:=' ';
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
    if s2[1]='!'then delete(s2,1,2);
    s2:=KanaToRomaji(s2,romasys,curlang);
    s2:=ConvertPinYin(s2);
    sx1:=s1;
    s1:=s1+'0020'+s2;
    DrawWordInfo(Canvas,rect,false,false,0,'@'+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(sx1) div 4)*ch+ch+(length(s2) div 8)*ch;
  end else
  begin
    DrawWordInfo(Canvas,rect,false,false,0,'@'+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(s1) div 4)*ch;
  end;
  if (s2<>s1) and (curlang='j') then
  begin
    if s2[1]='!'then s2:='!'+s1[2]+'U'+copy(s2,3,length(s2)-2) else s2:='U'+s2;
    DrawWordInfo(Canvas,rect,false,false,1,'@'+s2,false,false,ch-3,boldfont);
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

procedure ClearKanjiCardCache;
begin
  kcchind.Clear;
  kcchcomp.Clear;
end;

procedure DrawKanjiCard(canvas:TCanvas;u:string;x,y:integer;ch:double;stcount,outlin,alt,rad,inlin,comp,read,mean,strokeorder,fullcomp,sortfreq:boolean;sizhor,sizvert,nofullcomp:integer;calfont:string);
var ony,kuny,defy:string;
    radf:integer;
    sl:TStringList;
    s,ws:string;
    p:integer;
    i,j,k:integer;
    rect:TRect;
    nch,ncv:integer;
    fontjpch:string;
    FontJpChGrid:string;
    FontJpEnGrid:string;
    adddot:integer;
    dic:TJaletDic;
    mark,freq:string;
begin
  if curlang='j'then fontjpch:=FontJapanese else fontjpch:=FontChinese;
  if curlang='j'then fontjpchgrid:=FontJapaneseGrid else fontjpchgrid:=FontChineseGrid;
  if curlang='j'then fontjpengrid:=FontJapaneseGrid else fontjpengrid:=FontPinYin;
  ncv:=sizvert;
  if read then inc(ncv,3);
  if mean then inc(ncv,2);
  if fullcomp then inc(ncv,1+nofullcomp);
  nch:=sizvert;
  if alt or rad then inc(nch,(sizvert div 2)+1);
  if comp then nch:=nch+1+sizhor;
  sl:=TStringList.Create;
  TChar.Locate('Unicode',u,false);
  DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc((sizvert-1)*ch),u,calfont);
  if stcount then
    if curlang<>'j'then DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc(ch),UnicodeToHex(TChar.Str(TCharStrokeCount)),FontEnglish);
  if stcount then
    if curlang='j'then DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc(ch),UnicodeToHex(TChar.Str(TCharJpStrokeCount)),FontEnglish);
  if strokeorder then
    DrawStrokeOrder(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc((sizvert-1)*ch),trunc((sizvert-1)*ch),u,trunc(ch/3*2),clBlack);
  {outer lines}
  if outlin then
  begin
    canvas.MoveTo(x,y);
    canvas.LineTo(trunc(x+nch*ch),y);
    canvas.LineTo(trunc(x+nch*ch),trunc(y+ncv*ch));
    canvas.LineTo(x,trunc(y+ncv*ch));
    canvas.LineTo(x,y);
  end;
  {alternate}
  radf:=fSettings.ComboBox1.ItemIndex+12;
  TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf)),true);
  if alt then
    DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2),trunc(sizvert/8*3*ch),TRadicals.Str(TRadicalsUnicode),FontRadical);
  {radical}
  if rad then
    DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2+(sizvert/2)*ch),trunc((sizvert/8*3)*ch),u,FontJpchGrid);
  if inlin then
  begin
    canvas.MoveTo(trunc(x+ch*sizvert),trunc(y+ch/2));
    canvas.LineTo(trunc(x+ch*sizvert),trunc(y+ch*sizvert-ch/2));
  end;
  {compounds}
  sl.Clear;
  if comp then
  begin
    TUserIdx.SetOrder('Kanji_Ind');
    TUserIdx.Locate('Kanji',u,false);
    while (not TUserIdx.EOF) and (TUserIdx.Str(TUserIdxKanji)=u) do
    begin
      TUser.Locate('Index',TUserIdx.Str(TUserIdxWord),true);
      if length(TUser.Str(TUserKanji)) div 4<10 then
//      if CheckKnownKanji(TUser.Str(TUserKanji))=TUser.Str(TUserKanji) then
        if TUserIdx.Bool(TUserIdxBegin) then
          sl.Add('+'+inttostr(length(TUser.Str(TUserKanji)) div 4)+TUser.Str(TUserKanji)) else
          sl.Add('-'+inttostr(length(TUser.Str(TUserKanji)) div 4)+TUser.Str(TUserKanji));
      TUserIdx.Next;
    end;
    sl.Sort;
    p:=0;
    for j:=0 to sizvert-2 do
    begin
      s:='';
      while (p<sl.Count) and ((length(sl[p])-2) div 4+length(s) div 4+1<=sizhor) do
      begin
        s:=s+copy(sl[p],3,length(sl[p])-2)+'3001';
        inc(p);
      end;
      if (p>=sl.Count) and (length(s)>3) then delete(s,length(s)-3,4);
      if alt or rad then
        DrawUnicode(canvas,trunc(x+((sizvert)*3*ch)/2+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid) else
        DrawUnicode(canvas,trunc(x+(sizvert)*ch+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid);
    end;
    if (alt or rad) and (inlin) then
    begin
      canvas.MoveTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch/2));
      canvas.LineTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch*sizvert-ch/2));
    end;
  end;
  {full compounds}
  sl.Clear;
  if fullcomp then
  begin
    if kcchind.IndexOf(u)=-1 then
    begin
      for i:=0 to dicts.Count-1 do if ((dicts.Objects[i] as TJaletDic).loaded) and (pos(','+(dicts.Objects[i] as TJaletDic).name,NotGroupDicts[4])=0)
        and ((dicts.Objects[i] as TJaletDic).TDictFrequency<>-1) then
      begin
        dic:=dicts.Objects[i] as TJaletDic;
        dic.Demand;
        dic.FindIndexString(false,u);
        k:=0;
        j:=dic.ReadIndex;
        while (j>0) do
        begin
          dic.TDict.Locate('Index',inttostr(j),true);
          inc(k);
          if dic.TDictMarkers<>-1 then mark:=dic.TDict.Str(dic.TDictMarkers) else mark:='';
          freq:='0000000';
          if (dic.TDictFrequency<>-1) and (sortfreq) then freq:=inttostr(9999999-dic.TDict.Int(dic.TDictFrequency));
          while length(freq)<7 do freq:='0'+freq;
          if pos('<spop>',EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark))=0 then freq[1]:='a';
          if freq<>'9999999'then
          sl.Add(freq+#9+ChinTo(dic.TDict.Str(dic.TDictKanji))+' ['+dic.TDict.Str(dic.TDictPhonetic)+'] {'+EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark)+'}{');
          j:=dic.ReadIndex;
        end;
      end;
      sl.Sort;
      for j:=0 to nofullcomp-1 do if sl.Count>j then kcchcomp.Add(sl[j]) else kcchcomp.Add('');
      kcchind.Add(u);
    end;
    p:=0;
    i:=kcchind.IndexOf(u);
    for j:=0 to nofullcomp-1 do
    begin
      s:=kcchcomp[i*nofullcomp+j];
      rect.left:=x+round(ch/2);
      rect.right:=x+round(nch*ch-ch/2);
      rect.top:=y+round(sizvert*ch+j*ch+ch/2);
      rect.bottom:=y+round(sizvert*ch+j*ch+ch+ch/2);
      if read then
      begin
        rect.top:=rect.top+trunc(ch*3);
        rect.bottom:=rect.bottom+trunc(ch*3);
      end;
      if mean then
      begin
        rect.top:=rect.top+trunc(ch*2);
        rect.bottom:=rect.bottom+trunc(ch*2);
      end;
      if s<>'' then DrawPackedWordInfo(canvas,rect,copy(s,9,length(s)-8),trunc(ch),false);
    end;
    if inlin then
    begin
      rect.top:=y+round(sizvert*ch);
      if read then
        rect.top:=rect.top+trunc(ch*3);
      if mean then
        rect.top:=rect.top+trunc(ch*2);
      canvas.MoveTo(trunc(x+ch/2),rect.top);
      canvas.LineTo(trunc(x+nch*ch-ch/2),rect.top);
    end;
  end;
  {readings}
  if read then
  begin
    if inlin then
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end;
    ony:='';
    kuny:='';
    TCharRead.SetOrder('');
    TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
    while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
    begin
      s:=TCharRead.Str(TCharReadReading);
      if curlang='j'then
        if (TCharRead.Int(TCharReadType)>3) and (TCharRead.Int(TCharReadType)<7) then
        begin
          ws:='';
          adddot:=0;
          if s[1]='+'then
          begin
            ws:='FF0B';
            delete(s,1,1);
            adddot:=1;
          end;
          if s[1]='-'then
          begin
            ws:=ws+'FF0D';
            delete(s,1,1);
            adddot:=1;
          end;
          if TCharRead.Int(TCharReadReadDot)>0 then
          begin
            ws:=ws+copy(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot);
            ws:=ws+'FF0E';
            delete(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot);
          end;
          if s[length(s)]='-'then ws:=ws+copy(s,1,length(s)-1)+'FF0D'
            else ws:=ws+s;
        end;
      if curlang='c'then ws:=TCharRead.Str(TCharReadReading);
      case TCharRead.Int(TCharReadType) of
        2:if curlang='c'then if ony='' then ony:=ConvertPinYin(ws) else ony:=ony+UnicodeToHex(', ')+ConvertPinYin(ws);
        8:if curlang='c'then if kuny='' then kuny:=UnicodeToHex(lowercase(ws)) else kuny:=kuny+UnicodeToHex(', ')+UnicodeToHex(lowercase(ws));
        4:if curlang='j'then if length(ony) div 4+length(ws) div 4+2<=nch then if ony='' then ony:=ws else ony:=ony+'3001'+ws;
        5:if curlang='j'then if length(kuny) div 4+length(ws) div 4+2<=nch then if kuny='' then kuny:=ws else kuny:=kuny+'3001'+ws;
      end;
      TCharRead.Next;
    end;
//        ony:=UnicodeToHex(KanaToRomaji(ony,3));
//        kuny:=UnicodeToHex(KanaToRomaji(ony,3));
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2),trunc(ch),ony,FontJpEnGrid);
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2+ch),trunc(ch),kuny,FontJpEnGrid);
  end;
  if mean then
  begin
    if inlin then if not read then
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end else
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*(3+sizvert)));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*(3+sizvert)));
    end;
    rect.left:=trunc(x+ch/2);
    rect.right:=trunc(x+nch*ch-ch/2);
    rect.top:=trunc(y+ch*(sizvert)+ch/2);
    rect.bottom:=trunc(y+ch*(1+sizvert)+ch/2);
    if read then
    begin
      rect.top:=rect.top+trunc(ch*3);
      rect.bottom:=rect.bottom+trunc(ch*3);
    end;
    defy:='';
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
    canvas.Font.Name:=FontEnglish;
    canvas.Font.Height:=trunc(ch);
    canvas.TextRect(rect,rect.left,rect.top,defy);
  end;
  sl.Free;
end;

function ConvertPinYin(s:string):string;
var nch:string;
    li:integer;
    ali:string;
    cnv,cnv2,cv2aft:string;
    cc:char;
    i:integer;
    iscomma:boolean;
begin
    cnv:=lowercase(s);
    cnv2:='';
    li:=0;
    ali:='';
    iscomma:=false;
    for i:=1 to length(cnv) do
    begin
      if (li=0) and ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
      if (li<i) and ((cnv[li]='i') or (cnv[li]='u') or (cnv[li]='ü')) and
        ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
      if (cnv[i]>='0') and (cnv[i]<='5') and (li>0) then
      begin
        cc:=cnv[li];
        ali:=copy(cnv2,length(cnv2)-(i-li-1)*4+1,(i-li-1)*4);
        delete(cnv2,length(cnv2)-(i-li-1)*4-3,(i-li-1)*4+4);
        if iscomma and (cc='u') then cc:='w';
        case cnv[i] of
          '2':case cc of
                'a':cnv2:=cnv2+'00E1';
                'e':cnv2:=cnv2+'00E9';
                'i':cnv2:=cnv2+'00ED';
                'o':cnv2:=cnv2+'00F3';
                'u':cnv2:=cnv2+'00FA';
                'w':cnv2:=cnv2+'01D8';
              end;
          '4':case cc of
                'a':cnv2:=cnv2+'00E0';
                'e':cnv2:=cnv2+'00E8';
                'i':cnv2:=cnv2+'00EC';
                'o':cnv2:=cnv2+'00F2';
                'u':cnv2:=cnv2+'00F9';
                'w':cnv2:=cnv2+'01DC';
              end;
          '1':case cc of
                'a':cnv2:=cnv2+'0101';
                'e':cnv2:=cnv2+'0113';
                'i':cnv2:=cnv2+'012B';
                'o':cnv2:=cnv2+'014D';
                'u':cnv2:=cnv2+'016B';
                'w':cnv2:=cnv2+'01D6';
              end;
          '3':case cc of
                'a':cnv2:=cnv2+'01CE';
                'e':cnv2:=cnv2+'011B';
                'i':cnv2:=cnv2+'01D0';
                'o':cnv2:=cnv2+'01D2';
                'u':cnv2:=cnv2+'01D4';
                'w':cnv2:=cnv2+'01DA';
              end;
        end;
        li:=0;
        if (cnv[i]='0') or (cnv[i]='5') then if cc='w'then cnv2:=cnv2+'00FC'else cnv2:=cnv2+UnicodeToHex(cc);
        cnv2:=cnv2+ali;
        iscomma:=false;
      end else if cnv[i]=':'then begin cnv2:=cnv2+'XXXX'; iscomma:=true end else if (cnv[i]<'0') or (cnv[i]>'5') then cnv2:=cnv2+UnicodeToHex(cnv[i]);
    end;
    while pos('XXXX',cnv2)>0 do delete(cnv2,pos('XXXX',cnv2),4);
    result:=cnv2;
//    result:=UnicodeToHex(s);
end;

function DeconvertPinYin(s:string):string;
var nch:string;
    cnv,cnv2:string;
    i,j:integer;
    curs,curcc:string;
    curp,curpx:char;
    mustbegin,mustnotbegin,befmustnotbegin,befbefmustnotbegin:boolean;
    number:boolean;
    putcomma,fnd:boolean;
    cc:char;
begin
  cnv:=s;
  putcomma:=false;
  cnv2:='';
  for i:=0 to (length(cnv) div 4)-1 do
  begin
    curs:=copy(cnv,i*4+1,4);
    if putcomma and (curs<>UnicodeToHex('e')) then begin curs:=UnicodeToHex(':')+curs; putcomma:=false; end;
    if curs='01D6'then begin putcomma:=true; curs:='016B'; end;
    if curs='01D8'then begin putcomma:=true; curs:='00FA'; end;
    if curs='01DA'then begin putcomma:=true; curs:='01D4'; end;
    if curs='01DC'then begin putcomma:=true; curs:='00F9'; end;
    if curs='00FC'then begin putcomma:=true; curs:='0075'; end;
    if curs='2026'then curs:=UnicodeToHex('_');
    cnv2:=cnv2+curs;
  end;
  if putcomma then cnv2:=cnv2+UnicodeToHex(':');
  cnv:=cnv2;
  cnv2:='';
  curp:='0';
  mustbegin:=true;
  mustnotbegin:=false;
  befmustnotbegin:=false;
  number:=false;
  for i:=0 to (length(cnv) div 4)-1 do
  begin
    curs:=copy(cnv,i*4+1,4);
    cc:=char((HexToUnicode(curs))[1]);
    if (cc>='0') and (cc<='9') then number:=true;
  end;
  if number then
  begin
    result:=HexToUnicode(s);
    exit;
  end;
  cc:=' ';
  curcc:='';
  for i:=0 to (length(cnv) div 4)-1 do
  begin
    curs:=copy(cnv,i*4+1,4);
    curpx:='0';
    if (curs[1]='0') and (curs[2]='0') and (curs[3]<'8') then cc:=upcase(char((HexToUnicode(curs))[1]))
    else if curs='00E1'then begin cc:='A'; curpx:='2'; end
    else if curs='00E9'then begin cc:='E'; curpx:='2'; end
    else if curs='00ED'then begin cc:='I'; curpx:='2'; end
    else if curs='00F3'then begin cc:='O'; curpx:='2'; end
    else if curs='00FA'then begin cc:='U'; curpx:='2'; end
    else if curs='00E0'then begin cc:='A'; curpx:='4'; end
    else if curs='00E8'then begin cc:='E'; curpx:='4'; end
    else if curs='00EC'then begin cc:='I'; curpx:='4'; end
    else if curs='00F2'then begin cc:='O'; curpx:='4'; end
    else if curs='00F9'then begin cc:='U'; curpx:='4'; end
    else if curs='0101'then begin cc:='A'; curpx:='1'; end
    else if curs='0113'then begin cc:='E'; curpx:='1'; end
    else if curs='012B'then begin cc:='I'; curpx:='1'; end
    else if curs='014D'then begin cc:='O'; curpx:='1'; end
    else if curs='016B'then begin cc:='U'; curpx:='1'; end
    else if curs='0103'then begin cc:='A'; curpx:='3'; end
    else if curs='0115'then begin cc:='E'; curpx:='3'; end
    else if curs='012D'then begin cc:='I'; curpx:='3'; end
    else if curs='014F'then begin cc:='O'; curpx:='3'; end
    else if curs='016D'then begin cc:='U'; curpx:='3'; end
    else if curs='01CE'then begin cc:='A'; curpx:='3'; end
    else if curs='011B'then begin cc:='E'; curpx:='3'; end
    else if curs='01D0'then begin cc:='I'; curpx:='3'; end
    else if curs='01D2'then begin cc:='O'; curpx:='3'; end
    else if curs='01D4'then begin cc:='U'; curpx:='3'; end
    else cc:='?';
    if (((cc>='A') and (cc<='Z')) or (cc=':')) and (cc<>'''') then curcc:=curcc+cc;
    fnd:=false;
    for j:=0 to (romac.count div 4)-1 do
      if pos(lowercase(curcc),lowercase(romac[j*4+1]))=1 then fnd:=true;
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

function IsKnown(listno:integer;charno:string):boolean;
var ki,kj:integer;
    w:widechar;
begin
  w:=HexToUnicode(charno)[1];
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then
  begin
    result:=false;
    exit;
  end;
  result:=(((TByteArray(KnownList[listno]^)[ki]) shr kj) and 1)<>0;
end;

procedure SetKnown(listno:integer;charno:string;known:boolean);
var ki,kj:integer;
    a:byte;
    w:widechar;
begin
  w:=HexToUnicode(charno)[1];
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then exit;
  a:=TByteArray(KnownList[listno]^)[ki];
  if known then a:=a or (1 shl kj) else a:=a and not (1 shl kj);
  TByteArray(KnownList[listno]^)[ki]:=a;
end;

function EnumProc(lpelf:pointer;lpnt:pointer;FontType:integer;lParam:integer):integer; stdcall;
var p:^ENUMLOGFONTEX;
begin
  p:=lpelf;
  fontlist.Add(p^.elfLogFont.lfFaceName+#9+inttostr(p^.elfLogFont.lfCharset));
end;

function ChooseFont(charsets:array of TFontCharset;teststring:string;var supportedsets:string;defaultfont:string;selectfirst:boolean):string;
var lf:LOGFONT;
    sl:TStringList;
    i:integer;
    curfont:string;
    csets:string;
    s:string;
    ci:integer;
    y:integer;
begin
  fillchar(lf,sizeof(lf),0);
  fontlist:=TStringList.Create;
{  for i:=0 to High(charsets) do
  begin
    lf.lfCharset:=charsets[i];
    EnumFontFamiliesEx(fMenu.Canvas.Handle,lf,@EnumProc,0,0);
  end;}
  lf.lfCharset:=ANSI_CHARSET;
//  EnumFontFamiliesEx(fMenu.Canvas.Handle,lf,@EnumProc,0,0);
  for i:=0 to Screen.Fonts.Count-1 do fontlist.Add(Screen.Fonts[i]+#9+inttostr(ANSI_CHARSET));
  fontlist.Sort;
  fSelectFont.StringGrid1.RowCount:=2;
  fSelectFont.StringGrid1.Cells[0,0]:=_l('#00636^eFont name^cNázev fontu');
  fSelectFont.StringGrid1.Cells[1,0]:=_l('#00637^eCharsets^cZnakové sady');
  curfont:='';
  y:=1;
  for i:=0 to fontlist.Count-1 do
  begin
    s:=fontlist[i];
    if (length(s)>0) and (s[1]<>'@') then
    begin
      ci:=strtoint(copy(s,pos(#9,s)+1,length(s)-pos(#9,s)));
      delete(s,pos(#9,s),length(s)-pos(#9,s)+1);
      if (curfont<>s) and (curfont<>'') then
      begin
        if length(csets)>0 then delete(csets,length(csets),1);
        fSelectFont.StringGrid1.Cells[0,y]:=curfont;
        fSelectFont.StringGrid1.Cells[1,y]:=csets;
        csets:='';
        curfont:=s;
        inc(y);
        fSelectFont.StringGrid1.RowCount:=y+1;
      end;
      curfont:=s;
      case ci of
        ANSI_CHARSET:csets:=csets+'ANSI,';
        ARABIC_CHARSET:csets:=csets+'Arabic,';
        BALTIC_CHARSET:csets:=csets+'Baltic,';
        DEFAULT_CHARSET:csets:=csets+'Def,';
        EASTEUROPE_CHARSET:csets:=csets+'EastEurope,';
        GB2312_CHARSET:csets:=csets+'GB2312,';
        GREEK_CHARSET:csets:=csets+'Greek,';
        HANGEUL_CHARSET:csets:=csets+'Hangeul,';
        HEBREW_CHARSET:csets:=csets+'Hebrew,';
        CHINESEBIG5_CHARSET:csets:=csets+'Big5,';
        JOHAB_CHARSET:csets:=csets+'Johab,';
        MAC_CHARSET:csets:=csets+'Mac,';
        OEM_CHARSET:csets:=csets+'OEM,';
        RUSSIAN_CHARSET:csets:=csets+'Russian,';
        SHIFTJIS_CHARSET:csets:=csets+'Shift-JIS,';
        SYMBOL_CHARSET:csets:=csets+'Symbol,';
        THAI_CHARSET:csets:=csets+'Thai,';
        TURKISH_CHARSET:csets:=csets+'Turkish,';
      end;
    end;
  end;
  fSelectFont.teststring:=teststring;
  fSelectFont.deffont:=defaultfont;
  fSelectFont.StringGrid1.RowCount:=y;
  s:='';
  fontlist.Free;
  if selectfirst then
  begin
    if y>1 then result:=curfont else result:='!';
    exit;
  end;
  if fSelectFont.ShowModal<>idOK then result:=defaultfont else result:=fSelectFont.selfont;
  supportedsets:=fSelectFont.selcoding;
end;

function CheckKnownKanji(kanji:string):string;
var s,s2:string;
    oldchar:string;
begin
  result:=kanji;
  s:=kanji;
  while s<>'' do
  begin
    s2:=copy(s,1,4);
    delete(s,1,4);
    if length(s2)<>4 then showmessage('Error in kanji: '+s2+' ('+kanji+')') else
    if (s2[1]>'3') and (not IsKnown(KnownLearned,s2)) then
    begin
      result:='U'+kanji;
      exit;
    end;
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

function EvalChar(char:string):integer;
begin
  if char='3005'then result:=1 else // kurikaeshi
  if (char>='3000') and (char<='303F') then result:=EC_IDG_PUNCTUATION else
  if (char>='3040') and (char<='309F') then result:=EC_HIRAGANA else
  if (char>='30A0') and (char<='30FF') then result:=EC_KATAKANA else
  if (char>='3200') and (char<='33FF') then result:=EC_IDG_OTHER else
  if (char>='3400') and (char<='9FFF') then result:=EC_IDG_CHAR else
  if (char>='F900') and (char<='FAFF') then result:=EC_IDG_CHAR else
  if (char>='FE30') and (char<='FE4F') then result:=EC_IDG_PUNCTUATION else
  if (char>='FF00') and (char<='FF5F') then result:=EC_LATIN_FW else
  if (char>='FF60') and (char<='FF9F') then result:=EC_KATAKANA_HW else
  if (char>='0000') and (char<='007F') then result:=EC_LATIN_HW else
  result:=EC_UNKNOWN;
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

function CombUniToHex(s:string):string;
var i,lhcount:integer;
begin
  result:='';
  lhcount:=0;
  for i:=1 to length(s) do
    if lhcount>0 then
    begin
      dec(lhcount);
      result:=result+format('%2.2X',[ord(s[i])]);
    end else if ord(s[i])>128 then lhcount:=(ord(s[i])-128)*2 else
      result:=result+'00'+format('%2.2X',[ord(s[i])]);
end;

function HexToCombUni(s:string):string;
var i,j,lhcount:integer;
begin
  result:='';
  lhcount:=0;
  for i:=0 to (length(s) div 4)-1 do
  begin
    j:=i;
    if (lhcount=0) then
    begin
      while (j<(length(s) div 4)) and ((s[j*4+1]<>'0') or (s[j*4+2]<>'0') or (s[j*4+3]>'7')) do
      begin
        inc(j); inc(lhcount);
      end;
      if lhcount>0 then result:=result+chr(128+lhcount);
    end;
    if lhcount>0 then
    begin
      dec(lhcount);
      result:=result+chr(StrToInt('0x'+copy(s,i*4+1,2)))+chr(StrToInt('0x'+copy(s,i*4+3,2)));
    end else result:=result+chr(StrToInt('0x'+copy(s,i*4+3,2)));
  end;
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
