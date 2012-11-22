unit JWBUser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  rxPlacemnt, StdCtrls, ExtCtrls, ComCtrls, Grids, RXCtrls, Buttons,
  JWBUtils;

type
  TfUser = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Shape11: TShape;
    Label16: TLabel;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton23: TSpeedButton;
    Edit1: TEdit;
    StringGrid1: TStringGrid;
    BitBtn1: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Timer1: TTimer;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    SpeedButton4: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    Label2: TLabel;
    SpeedButton17: TSpeedButton;
    Label3: TLabel;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    procedure Edit1Change(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Edit2Change(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure WordDetails_PaintBox1Paint(Sender: TObject);
    procedure WordDetails_PaintBox2Paint(Sender: TObject);
    procedure WordDetails_SpeedButton23Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WordDetails_PaintBox5Paint(Sender: TObject);
    procedure WordKanji_PaintBox7Paint(Sender: TObject);
    procedure WordKanji_PaintBox8Paint(Sender: TObject);
    procedure WordKanji_PaintBox9Paint(Sender: TObject);
    procedure WordKanji_PaintBoxK4Paint(Sender: TObject);
    procedure WordKanji_PaintBoxK5Paint(Sender: TObject);
    procedure WordKanji_PaintBoxK6Paint(Sender: TObject);
    procedure WordKanji_PaintBoxK7Paint(Sender: TObject);
    procedure WordKanji_PaintBoxK8Paint(Sender: TObject);
    procedure WordKanji_PaintBoxK9Paint(Sender: TObject);
    procedure Translate_Button2Click(Sender: TObject);
    procedure Translate_Button3Click(Sender: TObject);
    procedure Translate_Button4Click(Sender: TObject);
    procedure Translate_Button5Click(Sender: TObject);
    procedure Translate_CheckBox1Click(Sender: TObject);
    procedure Translate_ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Translate_PaintBox6Click(Sender: TObject);
    procedure Translate_Button8Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Translate_PaintBox6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Translate_PaintBox6MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Translate_Button7Click(Sender: TObject);
    procedure Translate_Button9Click(Sender: TObject);
    procedure Translate_Button6Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Translate_Button10Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton17Click(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
  private
    { Private declarations }
  public
    linl:TStringList;
    curkanji,curphonetic,curmeaning,curkanjid:string;
    docfilename:string;
    doctp:byte;
    procedure Look(nogriddisplay:boolean; NoScreenUpdates: boolean = false);
    procedure ShowWord;
    procedure ShowText(dolook:boolean);
    procedure RepaintText;
    procedure FormatClipboard;
    procedure Deflex(w:string;sl:TCandidateTranslationList;prior,priordfl:byte;mustSufokay,alwaysdeflect:boolean);
    function GetDocWord(x,y:integer;var wordtype:integer;stopuser:boolean):string;
    procedure RenderText(x,y:integer;canvas:TCanvas;l,t,w,h:integer;ll:TStringList;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
    function GetLineAttr(i,a:integer;ll:TStringList):integer;
    function SetWordTrans(x,y:integer;scanparticle:boolean;gridfirst:boolean;user:boolean):integer;
    procedure DisplayInsert(convins,transins:string;leaveinserted:boolean);
    procedure ResolveInsert(final:boolean);
    function GetInsertKana(display:boolean):string;
    procedure InsertCharacter(c:char);
    procedure DrawCursor(blink:boolean);
    procedure DrawBlock;
    procedure CheckTransCont(x,y:integer);
    procedure SplitLine(x,y:integer);
    procedure JoinLine(y:integer);
    procedure DeleteCharacter(x,y:integer);
    procedure RefreshLines;
    procedure CalcBlockFromTo(backtrack:boolean);
    procedure Translate_SaveAs;
    procedure Translate_SelectAll;
    procedure ClearInsBlock;
    procedure BlockOp(copy,delete:boolean);
    procedure PasteOp;
    procedure SaveToFile(filename:string;tp:byte;kana:boolean);
    procedure ChangeFile(changed:boolean);
    function CommitFile:boolean;
    function CursorScreenX:integer;
    function CursorScreenY:integer;
    procedure ShowHint;
    procedure HideHint;
    procedure PaintHint;
    procedure MakeEditorBitmap;
    procedure PasteEditorBitmap;
    procedure SetCurPos(x,y:integer);
    procedure HandleWheel(down:boolean);
    procedure DicSearch(search:string;a:integer;partial,reverse,full,plusplus:boolean;wt,maxwords:integer;sl:TStringList;dictgroup:integer;var wasfull:boolean);
    procedure CalcMouseCoords(x,y:integer;var rx,ry:integer);
    procedure DetailsForKanji(n:integer);
    procedure GetTextWordInfo(cx,cy:integer;var meaning,reading,kanji:string);
    procedure ChangeNotebook;
    procedure SetExamples(kanji:string);
    procedure ShowExample;
    procedure PaintExample;
    procedure MoveExample(right:boolean);
    procedure RandomExample;
    procedure ExampleClipboard(all:boolean);
    procedure GotoExample(num:integer);
    procedure OpenFile;
    function CompareUnicode(sub,str:string):boolean;
    function HalfWidthChar(c:string):boolean;
    function HalfWidth(x,y:integer):boolean;
    function PosToWidth(x,y:integer):integer;
    function WidthToPos(x,y:integer):integer;
    { Public declarations }
  end;

var
  fUser: TfUser;
  mustrepaint:boolean;

implementation

uses JWBUnit, JWBMenu, JWBWords, JWBSettings, JWBStatistics,
  JWBPrint, JWBTranslate, JWBWordDetails, JWBWordKanji, JWBWordAdd,
  JWBWordCategory, JWBHint, JWBKanjiDetails, JWBKanji, StdPrompt, JWBDicAdd, JWBConvert,
  Math;

var curword:integer;
    view,curx,cury,lcurx,lcury,insx,insy,inslen:integer;
    rcurx,rcury,rviewx,rviewy:integer;
    entermode:boolean;
    ul:TStringList;
    plinl:TStringList;
    lastxsiz,lastycnt,printl:integer;
    oldcurx,oldcury,blockx,blocky:integer;
    printpl:integer;
    insertbuffer:string;
    resolvebuffer:boolean;
    buffertype:char;
    cursorblinked:boolean;
    lastrendx,lastrendy:integer;
    leaveline:boolean;
    blockfromx,blockfromy,blocktox,blocktoy:integer;
    oldblockfromx,oldblockfromy,oldblocktox,oldblocktoy:integer;
    cursorposcache:integer;
    shiftpressed:boolean;
    filechanged:boolean;
    insconfirmed:boolean;
    cursorend:boolean;
    editorbitmap:TBitmap;
    hinthide:boolean;
    priorkanji:string;
    lastmmx,lastmmy:integer;
    dicsl:TStringList;
    ex_indfirst,ex_indlast,ex_indcur:integer;
    ex_jap,ex_en:string;
    randbank:TStringList;
    dic_ignorekana:boolean;
    donotsetbegset:boolean;

{$R *.DFM}

function remexcl(s:string):string;
begin
  if (length(s)>1) and (s[2]='!') then delete(s,2,2);
  if (length(s)>1) and (s[1]='!') then delete(s,1,2);
  if (length(s)>1) and (s[2]='U') then delete(s,2,1);
  if (length(s)>1) and (s[1]='U') then delete(s,1,1);
  if (length(s)>1) and (s[1]='~') then delete(s,1,2);
  result:=s;
end;

function strip(s:string):string;
begin
  while (pos('<',s)>0) and (pos('>',s)>0) and (pos('>',s)>pos('<',s)) do
  begin
    delete(s,pos('<',s),pos('>',s)-pos('<',s)+1);
  end;
  result:=trim(s);
end;

function GetDoc(ax,ay:integer):string;
begin
  if ay>=doc.Count then showmessage('Illegal doc access!');
  if ax>=length(doc[ay]) div 4 then result:='0000'else result:=copy(doc[ay],ax*4+1,4);
end;

function GetDocTr(ax,ay:integer):string;
begin
  if ay>=doctr.Count then showmessage('Illegal doctr access!');
  if ax>=length(doctr[ay]) div 9 then result:='!90000001'else result:=copy(doctr[ay],ax*9+1,9);
end;

procedure SetDocTr(ax,ay:integer;s:string);
begin
  doctr[ay]:=copy(doctr[ay],1,ax*9)+s+copy(doctr[ay],ax*9+10,length(doctr[ay])-(ax*9+9));
end;



procedure TfUser.Deflex(w:string;sl:TCandidateTranslationList;prior,priordfl:byte;mustsufokay,alwaysdeflect:boolean);
var ws: integer; //length of w in symbols. Not sure if needed but let's keep it for a while
    i,j,k:integer;
    s,s2:string;
    roma:string;
    core:string;
    pass:boolean;
    lastkanji:integer;
    fnd:boolean;
    ad:string;
    suf:string;
    sufokay:boolean;
    dr: TDeflectionRule;
    ct: TCandidateTranslation;
begin
  if prior>9 then prior:=9;
  if priordfl>9 then priordfl:=9;
  ws:=length(w) div 4;
  if curlang='j'then
  begin
    lastkanji:=0;
    for i:=1 to length(w) div 4 do if EvalChar(copy(w,i*4-3,4))<>EC_HIRAGANA then lastkanji:=i else break;
    core:=copy(w,1,lastkanji*4);
    roma:=copy(w,lastkanji*4+1,length(w)-lastkanji*4);
    if (SpeedButton4.Down) or (alwaysdeflect) then
      for i:=0 to defll.Count-1 do
      begin
        dr := defll[i]^; //copy to speed up access a bit (pointer deref)

        for j:=0 to (length(roma)-length(dr.infl)) div 4 do
          if (copy(roma,j*4+1,length(dr.infl))=dr.infl) or
             ((dr.infl='KKKK') and (j=0) and (core<>'')) then
          if ((dr.vt<>'K') and (dr.vt<>'I')) or ((dr.vt='K') and (j=0) and ((core='') or (core='6765')))
             or ((dr.vt='I') and (j=0) and ((core='') or (core='884C'))) then
          begin
            if (length(dr.defl)+j*4<=24) and (core+copy(roma,1,j*4)+dr.defl<>w) then
            begin
//              ws:=length(core+copy(roma,1,j*4)+dr.infl) div 4;
//              if dr.infl='KKKK'then ws:=length(core) div 4;
//              if ws<=length(core) div 4 then ws:=(length(core)+4) div 4;
//              if ws<=1 then ws:=2;
              ws:=length(w) div 4;
              ad:=core+copy(roma,1,j*4)+dr.defl;
              suf:=copy(roma,j*4+1+length(dr.infl),length(roma)-j*4-length(dr.infl));
              sufokay:=(suf='');
              for k:=0 to suffixl.Count-1 do
                if (dr.sufcat+suf=suffixl[k]) or ((dr.sufcat='*') and (suffixl[k][1]+suf=suffixl[k])) then
                  sufokay:=true;
              fnd:=false;
              for k:=0 to sl.Count-1 do
                if (sl[k].len=ws)
                and (sl[k].verbType=dr.vt)
                and (sl[k].str=ad) then
                  fnd:=true;
              if (not fnd) and (sufokay or not mustSufokay) then
              begin
                if (sufokay) and (dr.infl<>'KKKK') then sl.Add(priordfl, ws, dr.vt, ad) else sl.Add(1, ws, dr.vt, ad);
                Label1.Caption:=label1.Caption+IntToStr(ws)+dr.vt+ad+'#';
              end;
            end;
          end;
      end;
    ws:=length(w) div 4;
    sl.Add(prior, ws, 'F', w);
  end;
  if curlang='c'then
  begin
    sl.Add(prior, ws, 'F', w);
    if pos('?',KanaToRomaji(w,romasys,'c'))>0 then exit;
    repeat
      pass:=true;
      i:=0;
      while i<sl.Count do
      begin
        ct := sl[i]^;
        j := pos('F030',ct.str);
        if j>0 then begin
          pass:=false;
         //First version is modified in-place to avoid slow deletions
          sl[i]^.str[j] := '1';
         //Next versions are made into copies
          ct.str[j]:='2';
          sl.Add(ct);
          ct.str[j]:='3';
          sl.Add(ct);
          ct.str[j]:='4';
          sl.Add(ct);
          ct.str[j]:='5';
          sl.Add(ct);
        end else inc(i);
      end;
    until pass;
  end;
end;

function TfUser.GetDocWord(x,y:integer;var wordtype:integer;stopuser:boolean):string;
var wt2:integer;
    i:integer;
    nmk:boolean;
    tc:string;
    honor:boolean;
    stray:integer;
begin
  if (y=-1) or (y>doc.Count-1) or (x>length(doc[y]) div 4-1) or (x=-1) then
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
      result:=result+copy(doc[y],x*4+1,4);
      inc(x);
      if x=length(doc[y]) div 4 then exit;
    end;
    exit;
  end;
  tc:=copy(doc[y],x*4+1,4);
  honor:=false;
  if (tc='304A') or (tc='3054') then honor:=true;
  if (honor) and (length(doc[y])>x*4+5) and (EvalChar(copy(doc[y],x*4+5,4))<=2) then
    wordtype:=EvalChar(copy(doc[y],x*4+5,4)) else wordtype:=EvalChar(copy(doc[y],x*4+1,4));
  if wordtype>4 then wordtype:=4;
  nmk:=false;
  stray:=0;
  result:=copy(doc[y],x*4+1,4);
  repeat
    inc(x);
    if stopuser and (upcase(GetDocTr(x,y)[1])<>GetDocTr(x,y)[1]) then exit;
    wt2:=0;
    if (x<length(doc[y]) div 4) then
    begin
      wt2:=EvalChar(copy(doc[y],x*4+1,4));
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
    result:=result+copy(doc[y],x*4+1,4);
  until false;
end;

function TfUser.CursorScreenX:integer;
begin
  if (cursorend) and (curx=0) and (cury>0) then result:=GetLineAttr(cury-1,2,linl) else result:=curx;
end;

function TfUser.CursorScreenY:integer;
begin
  if (cursorend) and (curx=0) and (cury>0) then result:=cury-1 else result:=cury;
end;

function TfUser.CompareUnicode(sub,str:string):boolean;
var i:integer;
begin
  result:=false;
  for i:=0 to ((length(str)-length(sub)) div 4) do
    if copy(str,i*4+1,length(sub))=sub then
    begin
      result:=true;
      exit;
    end;
end;

{
Requirements:
- "search" has to be in 4-char-per-symbol hex encoding (=> length divisible by 4)
}
procedure TfUser.DicSearch(search:string;a:integer;partial,reverse,full,plusplus:boolean;wt,maxwords:integer;sl:TStringList;dictgroup:integer;var wasfull:boolean);
var i,j,k,l,ii:integer;
    wif:integer;
    di:integer;
    sort:integer;
    sorts,sorts2:string;
    s2:string;
    popclas:integer;
    status:integer;
    statpref:string;
    _s:string;
    cursearch:string;
    part:string;
    se:TCandidateTranslationList; { Candidate translations -- see comments where this type is declared }
    ui,sxx:string;
    sp:integer;
    sdef:char;
    slen:integer;
    limitkana:boolean;
    markers,converted:string;
    dic:TJaletDic;
    p4reading:boolean;
    sl2i:integer;
    tim:TDateTime;
    sxxr:string;
    sl2:TStringList;
    ts:string;
    partfound:boolean;
    every:boolean;
    a3kana:boolean;
    presentl,presindl:TStringList;
    willadd:boolean;
    scomp,ssig,scur:string;
    freq:integer;
    mess:TSMPromptForm;
    nowt:TDateTime;
begin
  mess:=nil;
  if search='' then exit;
  se:=TCandidateTranslationList.Create;
  presentl:=TStringList.Create;
  presindl:=TStringList.Create;
  p4reading:=false;
  wasfull:=true;
  nowt:=now;
  presentl.Sorted:=true;
  case a of
    1:if curlang='j'then Deflex(RomajiToKana('H'+search,romasys,true,'j'),se,9,8,true,false) else
       Deflex(RomajiToKana(search,romasys,true,'c'),se,9,8,true,false);
    2:se.Add(9, 1, 'F', search);
    3:Deflex(ChinFrom(search),se,9,8,true,false);
    4:begin
        if wt<0 then
        begin
          _s:=ChinFrom(search);
          Deflex(_s,se,9,8,false,true);
          p4reading:=wt=-1;
          if wt=-1 then if partl.IndexOf(search)>-1 then
            sl.Add('000000000000000000'+inttostr(length(search) div 4)+'P          '+_s+' ['+_s+'] {'+KanaToRomaji(search,1,'j')+' particle}');
        end else
        begin
          if (wt=1) or (wt=2) then
          begin
            _s:=ChinFrom(search);
            if wt=1 then Deflex(_s,se,9,8,false,true);
            for i:=(length(_s) div 4) downto 1 do
            begin
              j:=(length(_s) div 4)-i+1;
              if j<1 then j:=1;
              partfound:=false;
              every:=false;
              if EvalChar(copy(_s,i*4-3,4))=1 then every:=true;
              if (wt=2) and (i<(length(_s) div 4)) and (i>=(length(_s) div 4)-4) and (partl.IndexOf(copy(_s,i*4+1,length(_s)-i*4))>-1) then
                partfound:=true;
//              if ((i<(length(_s) div 4)) and every) or (wt=2) then
              if (every) or ((i>1) and (partial)) or (i=length(_s) div 4) or (partfound) then
                se.Add(i, i, 'F', copy(_s,1,i*4));
            end;
          end;
          if (wt=3) then
          begin
            _s:=ChinFrom(search);
            se.Add(9, Length(_s) div 4, 'F', _s);
          end;
        end;
      end;
  end;

  for di:=0 to dicts.Count-1 do if (dicts.Objects[di] as TJaletDic).loaded then
  begin
    dic:=dicts.Objects[di] as TJaletDic;
    if (dic.loaded) and (pos(','+dic.name,NotGroupDicts[dictgroup])=0) then
    begin
      dic.Demand;
      if a=1 then if reverse then dic.TDict.SetOrder('<Phonetic_Ind') else dic.TDict.SetOrder('Phonetic_Ind');
      if (a=3) or (a=4) then if (reverse and (a=3)) then dic.TDict.SetOrder('<Kanji_Ind') else dic.TDict.SetOrder('Kanji_Ind');
      if maxwords<10 then maxwords:=10;
      if (full) or ((fUser.SpeedButton13.Down) and (not plusplus)) then
          for j:=0 to se.Count-1 do
          begin
            if ((a=1) or (a=3)) and (plusplus) then begin dic.TDict.SetOrder('Index_Ind'); dic.TDict.First; end;
            sxx:=se[j].str;
            if sxx='' then continue;
            if (a=1) or (a=4) then sxxr:=KanaToRomaji(sxx,1,curlang);
            sp:=se[j].priority;
            sdef:=se[j].verbType;
            slen:=se[j].len;
            limitkana:=false;
            if a<3 then slen:=1;
            if a=3 then
            begin
              a3kana:=true;
              for i:=1 to length(sxx) div 4 do
              begin
                k:=EvalChar(copy(sxx,(i-1)*4+1,4));
                if (k<>2) and (k<>3) then a3kana:=false;
              end;
            end;
            if (a=3) and (a3kana) then sxxr:=KanaToRomaji(sxx,1,curlang);
            if (a=1) and (not plusplus) then if reverse then dic.TDict.Locate('<Sort',sxxr,false) else
                                        dic.TDict.Locate('Sort',sxxr,false);
            if a=2 then dic.FindIndexString(true,lowercase(sxx));
            if (a=3) and not a3kana and not plusplus then if reverse then dic.TDict.Locate('<Kanji',sxx,false) else
                                        dic.TDict.Locate('Kanji',sxx,false);
            if (a=3) and a3kana and not plusplus then if reverse then dic.TDict.Locate('<Sort',sxxr,false) else
                                        dic.TDict.Locate('Sort',sxxr,false);
            if a=4 then
            if p4reading then
            begin
              dic.TDict.SetOrder('Phonetic_Ind');
              dic.TDict.Locate('Sort',sxxr,false);
              limitkana:=true;
            end else
            begin
              if wt<>2 then
              begin
                dic.TDict.SetOrder('Kanji_Ind');
                dic.TDict.Locate('Kanji',sxx,false);
              end else
              begin
                limitkana:=true;
                dic.TDict.SetOrder('Phonetic_Ind');
                dic.TDict.Locate('Sort',sxxr,false);
              end;
            end;
        i:=0;
        if a=2 then wif:=dic.ReadIndex;
        while
          ((a=2) or (not dic.TDict.EOF)) and
          (((a=1) and (partial) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))=1)) or
          ((a=1) and (reverse) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))>0)) or
          ((a=1) and (sxxr=dic.TDict.Str(dic.TDictSort))) or
          ((a=3) and (a3kana) and (partial) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))=1)) or
          ((a=3) and (a3kana) and (reverse) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))>0)) or
          ((a=3) and (a3kana) and (sxxr=dic.TDict.Str(dic.TDictSort))) or
          ((a=2) and (wif<>0)) or
          ((a=3) and (not a3kana) and (partial) and (pos(sxx,dic.TDict.Str(dic.TDictKanji))=1)) or
          ((a=3) and (not a3kana) and (reverse) and (pos(sxx,dic.TDict.Str(dic.TDictKanji))>0)) or
          ((a=3) and (not a3kana) and (sxx=dic.TDict.Str(dic.TDictKanji))) or
          ((a=4) and (not limitkana) and (sxx=dic.TDict.Str(dic.TDictKanji))) or
          ((a=4) and (limitkana) and (sxxr=dic.TDict.Str(dic.TDictSort))) or
          (((a=1) or (a=3)) and (plusplus)))
        do
        begin
          if (mess=nil) and (now-nowt>1/24/60/60) then mess:=SMMessageDlg(_l('#00932^eDic.search^cSlovník'),_l('#00933^ePlease wait. Searching dictionary...^cProsím èekejte. Prohledávám slovník...'));
{          showmessage(dic.TDict.Str(dic.TDictSort));
          showmessage(dic.TDict.Str(dic.TDictEnglish));
          showmessage(dic.TDict.Str(dic.TDictKanji));
          showmessage(dic.TDict.Str(dic.TDictPhonetic));
          showmessage(KanaToRomaji(dic.TDict.Str(dic.TDictPhonetic),1,'j'));
          showmessage(dic.TDict.Str(dic.TDictMarkers));
}          if a=2 then dic.TDict.Locate('Index',inttostr(wif),true);
//          if a=2 then showmessage(Format('%4.4X',[wif])+'-'+dic.TDict.Str(dic.TDictEnglish));
          if sdef<>'F'then s2:='~I'else s2:='~F';
          if dic.TDictMarkers<>-1 then s2:=s2+EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),dic.TDict.Str(dic.TDictMarkers)) else
          begin
            converted:=ConvertEdictEntry(dic.TDict.Str(dic.TDictEnglish),markers);
            s2:=s2+EnrichDictEntry(converted,markers);
          end;
          if a=2 then ts:=lowercase(dic.TDict.Str(dic.TDictEnglish)+' ');
//          showmessage(sdef+KanaToRomaji(sxx,1,'j')+','+s2);
          if ((sdef='F') or
               ((sdef='2') and (pos('<gru-v>',s2)>0)) or
               ((sdef='S') and (pos('<gsuru-v>',s2)>0)) or
               ((sdef='K') and (pos('<gkuru-v>',s2)>0)) or
               ((sdef='I') and (pos('<gIku-v>',s2)>0)) or
               ((sdef='1') and (pos('-v>',s2)>0) and (pos('<gru-v>',s2)=0)) or
               ((sdef='A') and (pos('adj>',s2)>0) and (pos('<gna-adj>',s2)=0)) or
               ((sdef='N') and (pos('gna-adj',s2)>0)))
{            ((not p4reading) or (dic.TDict.Str(dic.TDictPhonetic)[3]<'A'))} then
          if (a<>2) or (partial and (pos(lowercase(sxx),ts)>0)) or
            (((pos(lowercase(sxx)+' ',ts)>0) or (pos(lowercase(sxx)+',',ts)>0) or (lowercase(sxx)=ts))) then
          if (not dic_ignorekana) or (not limitkana) or (pos('<skana>',s2)>0) then
          if (not plusplus) or (CompareUnicode(sxx,dic.TDict.Str(dic.TDictPhonetic)))
             or ((a=3) and (CompareUnicode(sxx,dic.TDict.Str(dic.TDictKanji)))) then
          begin
            popclas:=40;
            if (fSettings.CheckBox5.Checked) and (pos('<gn',s2)>0) then dec(popclas,5);
            if (fSettings.CheckBox5.Checked) and (pos('-v>',s2)>0) then dec(popclas,5);
            if (fSettings.CheckBox5.Checked) and (pos('<g',s2)=0) then dec(popclas,5);
            if (fSettings.CheckBox6.Checked) and (pos('<shonor>',s2)>0) then dec(popclas,1);
            if (fSettings.CheckBox6.Checked) and (pos('<shum>',s2)>0) then dec(popclas,2);
            if (fSettings.CheckBox6.Checked) and (pos('<spolite>',s2)>0) then dec(popclas,3);
            if pos('<sobsolete>',s2)>0 then inc(popclas,20);
            if pos('<sobscure>',s2)>0 then inc(popclas,20);
            if (fSettings.CheckBox7.Checked) and (pos('<spop>',s2)>0) then dec(popclas,150);
//            if ((a=1) or ((a=4) and (p4reading))) and (dic.TDict.Field('Priority')<>-1) then
//              if dic.TDict.Int(dic.TDict.Field('Priority'))>9 then inc(popclas,90) else
//              inc(popclas,dic.TDict.Int(dic.TDict.Field('Priority'))*10);
            if (a=4) and (p4reading) then
            begin
              if TUserPrior.Locate('Kanji',dic.TDict.Str(dic.TDictKanji),false) then
              begin
                dec(popclas,10*TUserPrior.Int(TUserPrior.Field('Count')));
              end;
            end;
            status:=-1;
//            TUser.SetOrder('Kanji_Ind');
            ui:='';
            if pos('<skana>',s2)>0 then
            begin
              TUser.Locate('Kanji',dic.TDict.Str(dic.TDictPhonetic),false);
              while (not TUser.EOF) and (dic.TDict.Str(dic.TDictPhonetic)=TUser.Str(TUserKanji)) do
              begin
                if dic.TDict.Str(dic.TDictPhonetic)=TUser.Str(TUserPhonetic) then
                begin
                  status:=TUser.Int(TUserScore);
                  ui:=TUser.Str(TUserIndex);
                end;
                TUser.Next;
              end;
            end;
            TUser.Locate('Kanji',dic.TDict.Str(dic.TDictKanji),false);
            while (not TUser.EOF) and (dic.TDict.Str(dic.TDictKanji)=TUser.Str(TUserKanji)) do
            begin
              if dic.TDict.Str(dic.TDictPhonetic)=TUser.Str(TUserPhonetic) then
              begin
                status:=TUser.Int(TUserScore);
                ui:=TUser.Str(TUserIndex);
              end;
              TUser.Next;
            end;
            if (status=-1) and (dic.TDict.Str(dic.TDictKanji)<>ChinTo(dic.TDict.Str(dic.TDictKanji))) then
            begin
              TUser.Locate('Kanji',ChinTo(dic.TDict.Str(dic.TDictKanji)),false);
              while (not TUser.EOF) and (ChinTo(dic.TDict.Str(dic.TDictKanji))=TUser.Str(TUserKanji)) do
              begin
                if dic.TDict.Str(dic.TDictPhonetic)=TUser.Str(TUserPhonetic) then
                begin
                  status:=TUser.Int(TUserScore);
                  ui:=TUser.Str(TUserIndex);
                end;
                TUser.Next;
              end;
            end;
            if (fSettings.CheckBox58.Checked) and (dic.TDictFrequency>-1) and (dic.TDict.Int(dic.TDictFrequency)>0) then s2:=s2+' <pwc'+dic.TDict.Str(dic.TDictFrequency)+'>';
            s2:=s2+' <d'+dic.name+'>';
            sort:=0; //the bigger the worse (will apear later in list)
            if a=2 then
            begin
              if (pos(trim(uppercase(Edit1.Text)),trim(uppercase(dic.TDict.Str(dic.TDictEnglish))))=1) then sort:=10000 else sort:=11000;
              sort:=sort+popclas*100;
            end;
            if a=1 then sort:=(10000*(9-min(sp,9)))+length(dic.TDict.Str(dic.TDictPhonetic))*1000+popclas*10;
            if (a=3) or (a=4) then sort:=(10000*(9-min(sp,9)))+popclas*10;
            sort:=sort+10000;
//            if (a=4) and (p4reading) then sort:=10000+popclas*10;
            if (fSettings.CheckBox4.Checked) and (status>-1) then dec(sort,1000);
            if (fSettings.CheckBox4.Checked) and (status>1) then dec(sort,1000);
            if dic.TDict.Str(dic.TDictPhonetic)[3]>='A'then inc(sort,1000);
            sort:=sort+dic.priority*20000;
            if (fSettings.CheckBox59.Checked) then
            begin
              if dic.TDictFrequency>-1 then
              begin
                freq:=dic.TDict.Int(dic.TDictFrequency);
                if freq>=500000 then freq:=10000 else
                if freq>=10000 then freq:=2000+(freq-10000) div 100 else
                if freq>=1000 then freq:=1000+(freq-1000) div 10;
              end;
              sort:=sort+10000-freq;
            end;
            if sort>99999 then sort:=99999;
            if sort<0 then sort:=0;
            if (a=4) and (p4reading) then s2:=copy(s2,1,2)+'<pp'+inttostr(sort div 100)+'> '+copy(s2,3,length(s2)-2);
            sorts:=dic.TDict.Str(dic.TDictIndex);
            while length(sorts)<6 do sorts:='0'+sorts;
            while length(ui)<6 do ui:='0'+ui;
            ui:=ui+sorts;
            sorts:=inttostr(sort);
            if length(sorts)>5 then sorts:='99999';
            while length(sorts)<5 do sorts:='0'+sorts;
            sorts:=sorts+ui+Format('%.2d',[slen]);
            // if sdef<>'F'then sorts:=sorts+'I'else sorts:=sorts+'F';
            if (pos('-v>',s2)>0) then sorts:=sorts+'I'else sorts:=sorts+'F';
            // 5 (Sort), 6 (Userindex), 6 (Dicindex), 2 (slen), 1 (sdef), 10 (dicname)
            sorts:=sorts+dic.name;
            while length(sorts)<30 do sorts:=sorts+' ';
            statpref:='';
            if (fSettings.CheckBox11.Checked) and (status>-1) then statpref:='!'+inttostr(status);
            ssig:=dic.TDict.Str(dic.TDictPhonetic)+'x'+dic.TDict.Str(dic.TDictKanji);
            if (fSettings.CheckBox8.Checked) and (pos('<skana>',s2)<>0) then
              scur:=sorts+statpref+dic.TDict.Str(dic.TDictPhonetic)+' ['+statpref+dic.TDict.Str(dic.TDictPhonetic)+'] {'+statpref+s2+'}'else
            scur:=sorts+statpref+CheckKnownKanji(ChinTo(dic.TDict.Str(dic.TDictKanji)))+' ['+statpref+dic.TDict.Str(dic.TDictPhonetic)+'] {'+statpref+s2+'}';
            if presentl.IndexOf(ssig)<>-1 then
            begin
              scomp:=sl[presindl.IndexOf(ssig)];
              if pos(copy(s2,3,length(s2)-2),scomp)=0 then
              begin
                if copy(scomp,1,5)>copy(sorts,1,5) then
                begin
                  delete(scomp,1,pos(' {',scomp));
                  delete(scomp,1,1);
                  if (length(scomp)>0) and (scomp[1]='!') then delete(scomp,1,2);
                  if (length(scomp)>0) and (scomp[1]='~') then delete(scomp,1,2);
                  delete(scur,length(scur),1);
                  scur:=scur+' / '+scomp;
                  sl[presindl.IndexOf(ssig)]:=scur;
                end else
                begin
                  delete(scomp,length(scomp),1);
                  if (length(s2)>0) and (s2[1]='~') then delete(s2,1,2);
                  scomp:=scomp+' / '+s2+'}';
                  sl[presindl.IndexOf(ssig)]:=scomp;
                end;
              end;
            end else
            begin
              if fMenu.IsAnnot then
              begin
                fMenu.AnnotSeekK(dic.TDict.Str(dic.TDictKanji),dic.TDict.Str(dic.TDictPhonetic));
                s2:=fMenu.AnnotGetAll('t',', ');
                if fMenu.AnnotGetOne('c')<>'' then s2:='%'+fMenu.AnnotGetOne('c')+s2;
                ii:=pos('{'+statpref,scur)+length(statpref)+1;
                if scur[ii]='!' then inc(ii,2);
                if scur[ii]='~' then inc(ii,2);
                if scur[ii]='!' then inc(ii,2);
                if s2<>'' then
                scur:=copy(scur,1,ii-1)+
                  s2+' >> '+
                  copy(scur,ii,length(scur)-ii+1);
              end;
              sl.Add(scur);
              presentl.Add(ssig);
              presindl.Add(ssig);
            end;
            inc(i);
          end;
          if (not full) and (i>=maxwords) then
          begin
            wasfull:=false;
            break;
          end;
          if a<>2 then dic.TDict.Next else wif:=dic.ReadIndex;
        end;
      end;
    end;
  end;
  sl.Sort;
  for i:=0 to sl.Count-1 do
  begin
    if copy(sl[i],6,6)<>'000000'then
    begin
      TUser.Locate('Index',inttostr(strtoint(copy(sl[i],6,6))),true);
      scomp:=sl[i];
      scur:=sl[i];
      delete(scomp,1,pos(' {',scomp));
      delete(scomp,1,1);
      if (length(scomp)>0) and (scomp[1]='!') then delete(scomp,1,2);
      if (length(scomp)>0) and (scomp[1]='~') then delete(scomp,1,2);
      scur:=copy(scur,1,length(sl[i])-length(scomp));
      s2:=TUser.Str(TUserEnglish);
      if pos(s2,scomp)>0 then scomp:=copy(scomp,1,pos(s2,scomp)-1)+copy(scomp,pos(s2,scomp)+length(s2),length(scomp)-length(s2)-pos(s2,scomp)+1) else scomp:=' // '+scomp;
      sl2:=TStringList.Create;
      fWords.ListWordCategories(TUser.Int(TUserIndex),sl2,'',false);
      for sl2i:=0 to sl2.Count-1 do s2:=s2+' <l'+copy(sl2[sl2i],3,length(sl2[sl2i])-2)+'>';
      sl2.Free;
      sl[i]:=scur+s2+scomp;
    end;
  end;
  se.Free;
  presentl.Free;
  presindl.Free;
  if Assigned(mess) then mess.Free;
end;

{
NoScreenUpdates: Do not update anything on the screen.
  Mainly, don't do fKanji.SetCharDetails, because we're translating a large block of text.
  I don't clearly understand why it's there, but surely it's not needed when translating.
}
procedure TfUser.Look(nogriddisplay:boolean; NoScreenUpdates: boolean);
var a:integer;
    wt:integer;
    full:boolean;
    i:integer;
    wasfull:boolean;
    s:string;
    b:boolean;
    maxwords:integer;
    dgroup:integer;
    clips:string;
begin
  donotsetbegset:=true;
  if SpeedButton1.Down then a:=1 else
    if SpeedButton2.Down then a:=2 else
    if SpeedButton3.Down then a:=3 else a:=4;
  if SpeedButton1.Down then dictmodeset:=0;
  if SpeedButton2.Down then dictmodeset:=1;
  if SpeedButton3.Down then dictmodeset:=2;
  if not((SpeedButton1.Down) or (SpeedButton2.Down)) then
  begin
    Edit1.enabled:=false;
    Edit1.Color:=clMenu;
  end else begin
    Edit1.enabled:=true;
    Edit1.Color:=clWindow;
  end;
  fMenu.aDictExact.Checked:=SpeedButton10.Down;
  fMenu.aDictBeginning.Checked:=SpeedButton11.Down;
  fMenu.aDictEnd.Checked:=SpeedButton12.Down;
  fMenu.aDictMiddle.Checked:=SpeedButton18.Down;
  fMenu.aDictBeginning.Enabled:=SpeedButton11.Enabled;
  fMenu.aDictEnd.Enabled:=SpeedButton12.Enabled;
  fMenu.aDictMiddle.Enabled:=SpeedButton18.Enabled;
  fMenu.aDictInflect.Checked:=SpeedButton4.Down;
  fMenu.aDictAuto.Checked:=SpeedButton13.Down;
  fMenu.aDictGroup1.Checked:=SpeedButton14.Down;
  fMenu.aDictGroup2.Checked:=SpeedButton15.Down;
  fMenu.aDictGroup3.Checked:=SpeedButton16.Down;
  if SpeedButton14.Down then dgroup:=1;
  if SpeedButton15.Down then dgroup:=2;
  if SpeedButton16.Down then dgroup:=3;
  SpeedButton11.Enabled:=true;
  SpeedButton12.Enabled:=true;
  SpeedButton18.Enabled:=true;
  case dictbeginset of
    0:fUser.SpeedButton10.Down:=true;
    1:fUser.SpeedButton11.Down:=true;
    2:fUser.SpeedButton12.Down:=true;
    3:fUser.SpeedButton18.Down:=true;
  end;
  Label1.Caption:='';
  if (not SpeedButton13.Down) or (SpeedButton18.Down) then BitBtn1.Caption:=_l('#00669^eSearch^cHledat') else BitBtn1.Caption:=_l('#00670^eAll^cVše');
  if (a<4) and (BitBtn1.Enabled) and ((not SpeedButton13.Down) or (SpeedButton18.Down)) then
  begin
    BitBtn1.Visible:=true;
    Label2.Visible:=false;
    StringGrid1.Visible:=false;
    Label16.Visible:=(edit1.text<>'') or (a=4);
    curword:=0;
    donotsetbegset:=false;
    ShowWord;
    exit;
  end;
  if ((a=2) or (a=4)) then
  begin
    if SpeedButton12.Down then SpeedButton10.Down:=true;
    if SpeedButton18.Down then SpeedButton10.Down:=true;
    SpeedButton12.Enabled:=false;
    SpeedButton18.Enabled:=false;
  end;
  if a=4 then
  begin
    if SpeedButton11.Down then SpeedButton10.Down:=true;
    SpeedButton11.Enabled:=false;
  end;
  donotsetbegset:=false;
  full:=not BitBtn1.Enabled;
//  if SpeedButton10.Down then full:=true;
  dicsl.Clear;
  StringGrid1.RowCount:=200;
  maxwords:=StringGrid1.VisibleRowCount;
  if a=3 then
  begin
    clips:='';
    for i:=1 to length(clip) div 4 do
      if copy(clip,i*4-3,2)='00'then break else clips:=clips+copy(clip,i*4-3,4);
  end;
  if (not fSettings.CheckBox12.Checked) or (SpeedButton10.Down) then full:=true;
  case a of
    1: DicSearch(Edit1.Text,1,SpeedButton11.Down,SpeedButton12.Down,full,SpeedButton18.Down,-1,maxwords,dicsl,dgroup,wasfull);
    2: DicSearch(Edit1.Text,2,SpeedButton11.Down,false,full,false,-1,maxwords,dicsl,dgroup,wasfull);
    3: DicSearch(clips,3,SpeedButton11.Down,SpeedButton12.Down,full,SpeedButton18.Down,-1,maxwords,dicsl,dgroup,wasfull);
    4: if insx<>-1 then
       begin
         if buffertype='H'then
           DicSearch(GetInsertKana(false),4,false,false,full,false,-1,maxwords,dicsl,5,wasfull) else
           DicSearch(GetInsertKana(false),4,false,false,full,false,-2,maxwords,dicsl,5,wasfull);
       end else
       begin
         s:=GetDocWord(rcurx,rcury,wt,nogriddisplay);
         if (not NoScreenUpdates) and (length(s)>=4) then fKanji.SetCharDetails(copy(s,1,4)); //TODO: For testing only! Enable back!
         DicSearch(s,4,true,false,full,false,wt,maxwords,dicsl,5,wasfull);
       end;
  end;
  ul.Clear;
  if dicsl.Count=0 then Label3.Caption:='-'else
    if not wasfull then Label3.Caption:=inttostr(dicsl.Count)+'+'else Label3.Caption:=inttostr(dicsl.Count);
  for i:=0 to dicsl.Count-1 do ul.Add(copy(dicsl[i],6,25));
  if not nogriddisplay then
  begin
    for i:=0 to dicsl.Count-1 do if (not full) and (i>=StringGrid1.VisibleRowCount) then dicsl.Delete(StringGrid1.VisibleRowCount) else dicsl[i]:=copy(dicsl[i],31,length(dicsl[i])-30);
    FillWordGrid(StringGrid1,dicsl,false,false);
    if not wasfull then s:=_l('#00671^eSearch results (partial)^cVýsledky hledání (èásteèné)') else s:=_l('#00672^eSearch results^cVýsledky hledání');
    BitBtn1.Visible:=not wasfull or (full and not BitBtn1.Enabled);
    Label2.Visible:=not BitBtn1.Visible;
    s:=s+' ';
    case a of
      1:s:=s+_l('#00673^eby phonetic^cpodle ètení');
      2:s:=s+_l('#00674^eby meaning^cpodle významu');
      3:s:=s+_l('#00675^eby written (clipboard)^cpodle zápisu (schránka)');
      4:s:=s+_l('#00676^eby written (text)^cpodle zápisu (text)');
    end;
    s:=s+' ('+inttostr(dicsl.Count)+')';
    curword:=0;
    if StringGrid1.Visible then StringGrid1SelectCell(self,0,1,b);
    if StringGrid1.Visible then StringGrid1.Row:=1;
    if StringGrid1.Visible then curword:=1;
    ShowWord;
  end;
end;

procedure TfUser.Edit1Change(Sender: TObject);
begin
  BitBtn1.Enabled:=true;
  Look(false);
end;

procedure TfUser.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(StringGrid1,ACol,ARow,Rect,State);
end;

procedure TfUser.Edit2Change(Sender: TObject);
begin
  Look(false);
end;

procedure TfUser.Edit2Click(Sender: TObject);
begin
  Look(false);
end;

procedure TfUser.Edit1Click(Sender: TObject);
begin
  Look(false);
end;

procedure TfUser.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var s:string;
begin
  curword:=ARow;
  if curword<=ul.Count then ShowWord;
end;

procedure TfUser.WordDetails_PaintBox1Paint(Sender: TObject);
begin
  fDicAdd.PaintBox1.Canvas.Brush.Color:=clWindow;
  if showroma then
    DrawUnicode(fDicAdd.PaintBox1.Canvas,1,1,22,ConvertPinYin(KanaToRomaji(curphonetic,romasys,curlang)),FontEnglish) else
  DrawUnicode(fDicAdd.PaintBox1.Canvas,1,1,22,curphonetic,FontJapanese);
end;

procedure TfUser.WordDetails_PaintBox2Paint(Sender: TObject);
begin
  fDicAdd.PaintBox2.Canvas.Brush.Color:=clWindow;
  DrawUnicode(fDicAdd.PaintBox2.Canvas,1,1,22,curkanji,FontJapanese);
end;

procedure TfUser.WordDetails_SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curkanji;
  fMenu.ChangeClipboard;
end;

procedure TfUser.FormatClipboard;
begin
end;

procedure TfUser.FormShow(Sender: TObject);
begin
//  fMenu.ShowForm(SpeedButton5,fMenu.aDictDetails,fWordDetails);
//  fMenu.ShowForm(SpeedButton6,fMenu.aDictKanji,fWordKanji);
//  fMenu.ShowForm(SpeedButton7,fMenu.aDictCategories,fWordCategory);
//  fMenu.ShowForm(SpeedButton9,fMenu.aDictAdd,fWordAdd);
//  fMenu.ShowForm(SpeedButton8,fMenu.aDictEditor,fTranslate);
  fMenu.aDict.Checked:=true;
  if Edit1.Enabled then Edit1.SetFocus;
  Look(false);
  ShowText(true);
  fTranslate.ListBox1.ItemIndex:=0;
  dic_ignorekana:=false;
end;

procedure EditorInit;
begin
  priorkanji:='';
  cursorblinked:=false;
  view:=0; curx:=-1; cury:=-1; lcurx:=-1; lcury:=-1; insx:=-1; insy:=-1;
  shiftpressed:=false;
  blockx:=-1;
  blocky:=-1;
  leaveline:=false;
  oldcurx:=-1; oldcury:=-1;
  printl:=1;
  lastxsiz:=16; lastycnt:=2;
  rcurx:=-1; rcury:=-1; rviewx:=-1; rviewy:=-1;
  oldblockfromx:=-1; oldblockfromy:=-1; oldblocktox:=-1; oldblocktoy:=-1;
  insconfirmed:=false;
  cursorend:=false;
  cursorposcache:=-1;
  mustrepaint:=true;
  FileChanged:=false;
  lastmmx:=-1;
  lastmmy:=-1;
end;

procedure TfUser.ShowWord;
var ki:integer;
    s,s2,s3:string;
    radf:integer;
    sl:TStringList;
    i:integer;
begin
  curkanjid:='00000000000000000000000000000000000000000000000000000000000000000000000000000000000000';
  curphonetic:='';
  curkanji:='';
  curmeaning:='';
  fDicAdd.Edit3.Text:='';
  SpeedButton17.Enabled:=false;
  SpeedButton23.Enabled:=false;
  SpeedButton19.Enabled:=false;
  fWordCategory.RxLabel9.Caption:='-';
  fWordCategory.Label55.Caption:='-';
  fWordCategory.Label11.Caption:='-';
  fWordCategory.Label12.Caption:='-';
  fWordCategory.Label13.Caption:='-';
  fWordCategory.Label14.Caption:='-';
  fWordKanji.Label8.Caption:='';
  fWordKanji.Label9.Caption:='';
  fWordKanji.Label10.Caption:='';
  fWordKanji.Label1.Caption:='';
  fWordKanji.Label4.Caption:='';
  fWordKanji.Label5.Caption:='';
  fWordKanji.Label6.Caption:='';
  fWordKanji.Label7.Caption:='';
  fWordKanji.Label11.Caption:='';
  fWordKanji.Shape4.Visible:=false;
  fWordKanji.Shape6.Visible:=false;
  fWordKanji.Shape8.Visible:=false;
  fWordKanji.Shape3.Visible:=false;
  fWordKanji.Shape2.Visible:=false;
  fWordKanji.Shape1.Visible:=false;
  fWordKanji.Shape9.Visible:=false;
  fWordKanji.Shape7.Visible:=false;
  fWordKanji.Shape5.Visible:=false;
  fWordKanji.PaintBox7.Visible:=false;
  fWordKanji.PaintBox8.Visible:=false;
  fWordKanji.PaintBox9.Visible:=false;
  fWordKanji.PaintBoxK4.Visible:=false;
  fWordKanji.PaintBoxK5.Visible:=false;
  fWordKanji.PaintBoxK6.Visible:=false;
  fWordKanji.PaintBoxK7.Visible:=false;
  fWordKanji.PaintBoxK8.Visible:=false;
  fWordKanji.PaintBoxK9.Visible:=false;
  if curword<>0 then
  begin
    curphonetic:=remexcl(copy(StringGrid1.Cells[0,curword],2,length(StringGrid1.Cells[0,curword])-1));
    curkanji:=remexcl(copy(StringGrid1.Cells[1,curword],2,length(StringGrid1.Cells[1,curword])-1));
    curmeaning:=strip(remexcl(StringGrid1.Cells[2,curword]));
    SetExamples(curkanji);
    s:=remexcl(StringGrid1.Cells[2,curword]);
    if pos(' >> ',s)>0 then delete(s,1,pos(' >> ',s)+3);
    fDicAdd.Edit3.Text:=s;
    SpeedButton17.Enabled:=true;
    SpeedButton23.Enabled:=true;
    fWordCategory.RxLabel9.Caption:=_l('#00677^eNot in vocabulary^cNení ve slovíèkách');
    ki:=0;
    s:=remexcl(curkanji);
    curkanjid:='';
    while length(s)>3 do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2,false) then
      begin
        inc(ki);
        radf:=fSettings.ComboBox1.ItemIndex+12;
        if TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf)),true) then
        begin
          curkanjid:=curkanjid+s2+TRadicals.Str(TRadicalsUnicode);
          if TChar.Bool(TCharChinese) then curkanjid:=curkanjid+'J'else
            if IsKnown(KnownLearned,TChar.Str(TCharUnicode)) then curkanjid:=curkanjid+'K'else
            if TChar.Int(TCharJouyouGrade)<9 then curkanjid:=curkanjid+'C'else
            if TChar.Int(TCharJouyouGrade)<10 then curkanjid:=curkanjid+'N'else
            curkanjid:=curkanjid+'U';
          TCharRead.SetOrder('');
          TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
          s3:='';
          while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
          begin
            if (curlang='j') and (TCharRead.Int(TCharReadType)=3) then s3:=s3+', '+TCharRead.Str(TCharReadReading);
            if (curlang='c') and (TCharRead.Int(TCharReadType)=7) then s3:=s3+', '+TCharRead.Str(TCharReadReading);
            TCharRead.Next;
          end;
          delete(s3,1,2);
          case ki of
            1:fWordKanji.Label8.Caption:=s3;
            2:fWordKanji.Label9.Caption:=s3;
            3:fWordKanji.Label10.Caption:=s3;
            4:fWordKanji.Label1.Caption:=s3;
            5:fWordKanji.Label4.Caption:=s3;
            6:fWordKanji.Label5.Caption:=s3;
            7:fWordKanji.Label6.Caption:=s3;
            8:fWordKanji.Label7.Caption:=s3;
            9:fWordKanji.Label11.Caption:=s3;
          end;
        end;
      end;
    end;
    if copy(ul[curword-1],1,6)<>'000000'then
    begin
      SpeedButton19.Enabled:=true;
      TUser.Locate('Index',copy(ul[curword-1],1,6),true);
      fWordCategory.Label11.Caption:=DateForm(TUser.Str(TUserAdded));
      fWordCategory.Label12.Caption:=DateForm(TUser.Str(TUserLearned));
      fWordCategory.Label13.Caption:=DateForm(TUser.Str(TUserMastered));
      fWordCategory.Label14.Caption:=DateForm(TUser.Str(TUserPrinted));
      if fWordCategory.Label13.Caption<>'-'then fWordCategory.Label13.Caption:=fWordCategory.Label13.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
      fWordCategory.RxLabel9.Caption:=StateStr(TUser.Int(TUserScore));
      sl:=TStringList.Create;
      sl.Clear;
      fWords.ListWordCategories(strtoint(copy(ul[curword-1],1,6)),sl,'',false);
      s:='';
      for i:=0 to sl.Count-1 do if s='' then s:=sl[i] else s:=s+', '+sl[i];
      fWordCategory.Label55.Caption:=s;
    end;
    if ki<1 then fWordKanji.Shape4.Visible:=false else fWordKanji.Shape4.Visible:=true;
    if ki<2 then fWordKanji.Shape6.Visible:=false else fWordKanji.Shape6.Visible:=true;
    if ki<3 then fWordKanji.Shape8.Visible:=false else fWordKanji.Shape8.Visible:=true;
    if ki<4 then fWordKanji.Shape3.Visible:=false else fWordKanji.Shape3.Visible:=true;
    if ki<5 then fWordKanji.Shape2.Visible:=false else fWordKanji.Shape2.Visible:=true;
    if ki<6 then fWordKanji.Shape1.Visible:=false else fWordKanji.Shape1.Visible:=true;
    if ki<7 then fWordKanji.Shape9.Visible:=false else fWordKanji.Shape9.Visible:=true;
    if ki<8 then fWordKanji.Shape7.Visible:=false else fWordKanji.Shape7.Visible:=true;
    if ki<9 then fWordKanji.Shape5.Visible:=false else fWordKanji.Shape5.Visible:=true;
    if ki<1 then fWordKanji.PaintBox7.Visible:=false else fWordKanji.PaintBox7.Visible:=true;
    if ki<2 then fWordKanji.PaintBox8.Visible:=false else fWordKanji.PaintBox8.Visible:=true;
    if ki<3 then fWordKanji.PaintBox9.Visible:=false else fWordKanji.PaintBox9.Visible:=true;
    if ki<4 then fWordKanji.PaintBoxK4.Visible:=false else fWordKanji.PaintBoxK4.Visible:=true;
    if ki<5 then fWordKanji.PaintBoxK5.Visible:=false else fWordKanji.PaintBoxK5.Visible:=true;
    if ki<6 then fWordKanji.PaintBoxK6.Visible:=false else fWordKanji.PaintBoxK6.Visible:=true;
    if ki<7 then fWordKanji.PaintBoxK7.Visible:=false else fWordKanji.PaintBoxK7.Visible:=true;
    if ki<8 then fWordKanji.PaintBoxK8.Visible:=false else fWordKanji.PaintBoxK8.Visible:=true;
    if ki<9 then fWordKanji.PaintBoxK9.Visible:=false else fWordKanji.PaintBoxK9.Visible:=true;
  end else SetExamples('');
  fWordDetails.PaintBox1.Invalidate;
  fWordDetails.PaintBox2.Invalidate;
  fWordDetails.PaintBox5.Invalidate;
  fWordKanji.PaintBox7.Invalidate;
  fWordKanji.PaintBox8.Invalidate;
  fWordKanji.PaintBox9.Invalidate;
  fWordKanji.PaintBoxK4.Invalidate;
  fWordKanji.PaintBoxK5.Invalidate;
  fWordKanji.PaintBoxK6.Invalidate;
  fWordKanji.PaintBoxK7.Invalidate;
  fWordKanji.PaintBoxK8.Invalidate;
  fWordKanji.PaintBoxK9.Invalidate;
  fMenu.AnnotShowMedia(curkanji,curphonetic);
end;

procedure TfUser.WordDetails_PaintBox5Paint(Sender: TObject);
begin
  fWordDetails.PaintBox5.Canvas.Brush.Color:=clWindow;
  DrawUnicode(fWordDetails.PaintBox5.Canvas,1,1,22,UnicodeToHex(curmeaning),FontEnglish);
end;

procedure TfUser.WordKanji_PaintBox7Paint(Sender: TObject);
begin
  if length(curkanjid)<9 then exit;
  BeginDrawReg(fWordKanji.PaintBox7);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBox7.Canvas,44,4,16,copy(curkanjid,5,4),FontJapaneseGrid);
  case curkanjid[9] of
    'K':fWordKanji.PaintBox7.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBox7.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBox7.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBox7.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBox7.Canvas,4,2,36,copy(curkanjid,1,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBox8Paint(Sender: TObject);
begin
  if length(curkanjid)<18 then exit;
  BeginDrawReg(fWordKanji.PaintBox8);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBox8.Canvas,44,4,16,copy(curkanjid,14,4),FontJapaneseGrid);
  case curkanjid[18] of
    'K':fWordKanji.PaintBox8.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBox8.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBox8.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBox8.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBox8.Canvas,4,2,36,copy(curkanjid,10,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBox9Paint(Sender: TObject);
begin
  if length(curkanjid)<27 then exit;
  BeginDrawReg(fWordKanji.PaintBox9);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBox9.Canvas,44,4,16,copy(curkanjid,23,4),FontJapaneseGrid);
  case curkanjid[27] of
    'K':fWordKanji.PaintBox9.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBox9.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBox9.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBox9.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBox9.Canvas,4,2,36,copy(curkanjid,19,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBoxK4Paint(Sender: TObject);
begin
  if length(curkanjid)<36 then exit;
  BeginDrawReg(fWordKanji.PaintBoxK4);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBoxK4.Canvas,44,4,16,copy(curkanjid,32,4),FontJapaneseGrid);
  case curkanjid[36] of
    'K':fWordKanji.PaintBoxK4.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBoxK4.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBoxK4.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBoxK4.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBoxK4.Canvas,4,2,36,copy(curkanjid,28,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBoxK5Paint(Sender: TObject);
begin
  if length(curkanjid)<45 then exit;
  BeginDrawReg(fWordKanji.PaintBoxK5);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBoxK5.Canvas,44,4,16,copy(curkanjid,41,4),FontJapaneseGrid);
  case curkanjid[45] of
    'K':fWordKanji.PaintBoxK5.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBoxK5.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBoxK5.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBoxK5.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBoxK5.Canvas,4,2,36,copy(curkanjid,37,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBoxK6Paint(Sender: TObject);
begin
  if length(curkanjid)<54 then exit;
  BeginDrawReg(fWordKanji.PaintBoxK6);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBoxK6.Canvas,44,4,16,copy(curkanjid,50,4),FontJapaneseGrid);
  case curkanjid[54] of
    'K':fWordKanji.PaintBoxK6.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBoxK6.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBoxK6.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBoxK6.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBoxK6.Canvas,4,2,36,copy(curkanjid,46,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBoxK7Paint(Sender: TObject);
begin
  if length(curkanjid)<63 then exit;
  BeginDrawReg(fWordKanji.PaintBoxK7);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBoxK7.Canvas,44,4,16,copy(curkanjid,59,4),FontJapaneseGrid);
  case curkanjid[63] of
    'K':fWordKanji.PaintBoxK7.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBoxK7.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBoxK7.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBoxK7.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBoxK7.Canvas,4,2,36,copy(curkanjid,55,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBoxK8Paint(Sender: TObject);
begin
  if length(curkanjid)<72 then exit;
  BeginDrawReg(fWordKanji.PaintBoxK8);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBoxK8.Canvas,44,4,16,copy(curkanjid,68,4),FontJapaneseGrid);
  case curkanjid[72] of
    'K':fWordKanji.PaintBoxK8.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBoxK8.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBoxK8.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBoxK8.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBoxK8.Canvas,4,2,36,copy(curkanjid,64,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.WordKanji_PaintBoxK9Paint(Sender: TObject);
begin
  if length(curkanjid)<81 then exit;
  BeginDrawReg(fWordKanji.PaintBoxK9);
  fWordKanji.PaintBox7.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(fWordKanji.PaintBoxK9.Canvas,44,4,16,copy(curkanjid,77,4),FontJapaneseGrid);
  case curkanjid[81] of
    'K':fWordKanji.PaintBoxK9.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':fWordKanji.PaintBoxK9.Canvas.Font.Color:=Col('Kanji_Common');
    'U':fWordKanji.PaintBoxK9.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':fWordKanji.PaintBoxK9.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(fWordKanji.PaintBoxK9.Canvas,4,2,36,copy(curkanjid,73,4),FontJapaneseGrid);
  EndDrawReg;
end;


procedure TfUser.ShowText(dolook:boolean);
var i:integer;
    oldview:integer;
    s:string;
    wt:integer;
begin
  if (not fTranslate.Visible) then exit;
  oldview:=view;
  RenderText(-1,-1,fTranslate.PaintBox6.Canvas,0,0,fTranslate.PaintBox6.Width-4,fTranslate.PaintBox6.Height-4,linl,printl,lastxsiz,lastycnt,false,true);
  if linl.Count=0 then
  begin
    rcurx:=-1;
    rcury:=-1;
    rviewx:=0;
    rviewy:=0;
    fTranslate.PaintBox6.Invalidate;
    fTranslate.ScrollBar1.Enabled:=false;
    exit;
  end;
  if cury<0 then cury:=0;
  if cury>=linl.Count div 3 then
  begin
    cury:=(linl.Count div 3)-1;
    curx:=2555;
  end;
  if curx<0 then curx:=0;
  if (cursorend) and (curx=0) and (cury>0) and (GetLineAttr(cury-1,1,linl)<>GetLineAttr(cury,1,linl)) then
  begin
    dec(cury);
    curx:=GetLineAttr(cury,2,linl)-1;
    cursorend:=false;
  end;
  if curx>=GetLineAttr(cury,2,linl) then
  begin
    if (cury+1<linl.Count div 3) and (GetLineAttr(cury,1,linl)=GetLineAttr(cury+1,1,linl)) then
    begin
      curx:=0;
      inc(cury);
      cursorend:=true;
    end else curx:=GetLineAttr(cury,2,linl)-1;
  end;
  if view>cury then if cury>0 then view:=cury else view:=0;
  if view+printl-1<cury then view:=cury-printl+1;
  if view+printl-1>=(linl.Count div 3) then view:=(linl.Count div 3)-printl;
  if view<0 then view:=0;
  if view>=(linl.Count div 3) then view:=0;
  rcury:=GetLineAttr(cury,1,linl);
  rcurx:=curx+GetLineAttr(cury,0,linl);
  rviewx:=GetLineAttr(view,0,linl);
  rviewy:=GetLineAttr(view,1,linl);
  if not shiftpressed then
  begin
    blockx:=rcurx;
    blocky:=rcury;
  end;
  lcury:=cury;
  lcurx:=curx;
  SpeedButton1.Down:=false;
  SpeedButton2.Down:=false;
  SpeedButton3.Down:=false;
  if (dolook) and ((fUser.Visible) or (insertBuffer<>'')) then Look(false) else
  if dolook then begin
    s:=GetDocWord(rcurx,rcury,wt,false);
    if (length(s)>=4) then fKanji.SetCharDetails(copy(s,1,4));
  end;
  if oldview<>view then mustrepaint:=true;
  if mustrepaint then MakeEditorBitmap else
  begin
    DrawCursor(false);
    DrawBlock;
  end;
  if mustrepaint then oldblockfromx:=-1;
  mustrepaint:=false;
  shiftpressed:=false;
  if (linl.Count div 3-printl<=0) then
    fTranslate.ScrollBar1.Enabled:=false
  else
  begin
    fTranslate.ScrollBar1.Min:=0;
    fTranslate.ScrollBar1.Max:=linl.Count div 3-printl;
    fTranslate.ScrollBar1.Position:=view;
    fTranslate.ScrollBar1.Enabled:=true;
  end;
  if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (insx<>-1) then ShowHint else HideHint;
  fTranslate.ListBox1.SetFocus;
end;

procedure TfUser.Translate_Button2Click(Sender: TObject);
var s,s2,s3:string;
    i:integer;
begin
  if not CommitFile then exit;
  doc.Clear;
  doctr.Clear;
  docdic.Clear;
  linl.Clear;
  curx:=0;
  cury:=0;
  view:=0;
  fTranslate.Label1.Caption:=_l('#00678^e<UNNAMED>^c<BEZEJMÉNA>');
  docfilename:='';
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfUser.Translate_Button3Click(Sender: TObject);
var tp:byte;
begin
  if not CommitFile then exit;
  if OpenDialog1.Execute then
  begin
    tp:=Conv_ChooseType(curlang='c',Conv_DetectType(OpenDialog1.FileName));
    if tp=0 then exit;
    docfilename:=OpenDialog1.FileName;
    doctp:=tp;
    OpenFile;
  end;
end;

procedure TfUser.OpenFile;
var s,s2,s3:string;
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
    tp:byte;
begin
    s:=docfilename;
    tp:=doctp;
    while pos('\',s)>0 do delete(s,1,pos('\',s));
    fTranslate.Label1.Caption:=uppercase(s);
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
        Application.MessageBox(pchar(_l('#00679^eThis is not a valid UTF-8 or JTT file.^cToto není platný UTF-8 nebo JTT soubor.')),pchar(_l('#00020^eError^cChyba')),MB_OK);
        closefile(f);
        exit;
      end;
      jtt:=true;
    end;
    dot:=true;
    if jtt then
    begin
      blockread(f,ws,16);
      s:=ws;
      if copy(s,1,22)<>'WaKan Translated Text>'then
      begin
        Application.MessageBox(pchar(_l('#00679^eThis is not a valid UTF-8 or JTT file.^cToto není platný UTF-8 nebo JTT soubor.')),pchar(_l('#00020^eError^cChyba')),MB_OK);
        closefile(f);
        exit;
      end;
      delete(s,1,22);
      if copy(s,1,length(fStatistics.Label15.Caption))<>fStatistics.Label15.Caption then
      begin
        if Application.MessageBox(pchar(_l('#00680^eThis JTT file was made using different WAKAN.CHR version. Translation cannot be loaded.'#13#13'Do you want to continue?'+
        '^cTento JTT soubor byl vytvoøen s použitím jiné verze WAKAN.CHR. Pøeklad nemùže být nahrán.'#13#13'Chcete pokraèovat?')),pchar(_l('#00090^eWarning^cVarování')),MB_YESNO or MB_ICONWARNING)=idNo then
        begin
          closefile(f);
          exit;
        end;
        dot:=false;
      end;
      blockread(f,w,1);
      if w<>3294 then
      begin
        Application.MessageBox(pchar(_l('#00681^eThis JTT file was created by old version of WaKan.'#13'It is not compatible with the current version.'+
          '^cTento JTT soubor byl vytvoøen starou verzí WaKanu.'#13'Se souèasnou verzí není kompatibilní.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
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
    s3:='';
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
              doctr.Add(s3);
              s:='';
              s3:='';
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
              if not dot then s3:=s3+'-90000001'else s3:=s3+dp+inttostr(buf[i*4] div 256)+ls+chr(buf[i*4+2] div 256);
            end;
          end;
        end;
      end else
        begin
          Conv_Open(docfilename,tp);
          s2:=Conv_Read;
          while s2<>'' do
          begin
            if s2='000D'then
            begin
              doc.Add(s);
              doctr.Add(s3);
              s:='';
              s3:='';
            end else
            if s2<>'000A'then begin
              s:=s+s2;
              s3:=s3+'-90000001';
            end;
            s2:=Conv_Read;
          end;
          Conv_Close;
    end;
    if jtt then closefile(f);
    if s<>'' then
    begin
      doc.Add(s);
      doctr.Add(s3);
    end;
    view:=0;
    curx:=0;
    cury:=0;
    mustrepaint:=true;
    ShowText(true);
    Screen.Cursor:=crDefault;
end;

procedure TfUser.Translate_Button4Click(Sender: TObject);
var i:integer;
begin
  clip:='';
  for i:=0 to doc.Count-1 do clip:=clip+doc[i]+'000E';
  delete(clip,length(clip)-3,4);
  fMenu.ChangeClipboard;
end;

procedure TfUser.Translate_Button5Click(Sender: TObject);
begin
  if docfilename<>'' then SaveToFile(docfilename,doctp,false) else Translate_SaveAs;
end;

procedure TfUser.SaveToFile(filename:string;tp:byte;kana:boolean);
var f:file;
    i,j,bc:integer;
    buf:array[0..16383] of word;
    jtt:boolean;
    sig:word;
    s:string;
    l:integer;
    w:word;
    meaning,reading,kanji:string;
    inreading:boolean;
begin
    Screen.Cursor:=crHourGlass;
    assignfile(f,filename);
    rewrite(f,2);
    jtt:=pos('.WTT',UpperCase(filename))>0;
    if jtt then
    begin
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
          buf[bc]:=ord(GetDocTr(j,i)[1])+strtoint(GetDocTr(j,i)[2])*256;
          l:=strtoint(copy(GetDocTr(j,i),3,6));
          buf[bc+2]:=l div 65536+ord(GetDocTr(j,i)[9])*256;
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
    end else
    begin
      Conv_Create(filename,tp);
      for i:=0 to doc.Count-1 do
      begin
        for j:=0 to (length(doc[i]) div 4)-1 do
        begin
          if (not inreading) or (GetDocTr(j,i)[1]<>'<') then
          begin
            reading:='';
            if kana then GetTextWordInfo(j,i,meaning,reading,kanji);
            if reading<>'' then reading:='0020'+reading;
            inreading:=reading<>'';
            if reading='' then reading:=GetDoc(j,i);
            while length(reading)>0 do
            begin
              Conv_Write(copy(reading,1,4));
              delete(reading,1,4);
            end;
          end;
        end;
        Conv_Write('000D');
        Conv_Write('000A');
      end;
      Conv_Flush;
      Conv_Close;
    end;
    Screen.Cursor:=crDefault;
    ChangeFile(false);
end;

function TfUser.GetLineAttr(i,a:integer;ll:TStringList):integer;
begin
  if i>=ll.Count then result:=0 else result:=strtoint(ll[i*3+a]);
end;

procedure TfUser.CalcBlockFromTo(backtrack:boolean);
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
    while GetDocTr(blockfromx,blockfromy)[1]='<'do dec(blockfromx);
    while GetDocTr(blocktox+1,blocktoy)[1]='<'do inc(blocktox);
  end;
end;

procedure TfUser.GetTextWordInfo(cx,cy:integer;var meaning,reading,kanji:string);
var dnam:string;
    dic:TJaletDic;
    i:integer;
    markers,defy,s:string;
begin
  meaning:='';
  reading:='';
  kanji:='';
  if copy(GetDocTr(cx,cy),3,6)<>'000000'then
  begin
    try
      dnam:=docdic[strtoint(copy(GetDocTr(cx,cy),9,1))];
    except dnam:='UNKNOWN'; end;
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
      dic.Demand;
      dic.TDict.Locate('Index',copy(GetDocTr(cx,cy),3,6),true);
      if dic.TDictMarkers<>-1 then meaning:=dic.TDict.Str(dic.TDictEnglish) else
        meaning:=ConvertEdictEntry(dic.TDict.Str(dic.TDictEnglish),markers);
      reading:=dic.TDict.Str(dic.TDictPhonetic);
      kanji:=dic.TDict.Str(dic.TDictKanji);
    end;
  end else if GetDocTr(cx,cy)[1]='?'then
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
  while (reading<>'') and (kanji<>'') and (copy(reading,length(reading)-3,4)=copy(kanji,length(kanji)-3,4)) do
  begin
    delete(reading,length(reading)-3,4);
    delete(kanji,length(kanji)-3,4);
  end;
end;

procedure TfUser.RenderText(x,y:integer;canvas:TCanvas;l,t,w,h:integer;ll:TStringList;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
var st0,lst0,st2,st3:boolean;
    linec,linec2,lineh,screenh,screenw,lines:integer;
    st2c:integer;
    rs:integer;
    vert:boolean;
    cl,cx,cxsc,cy,px,py,wx,wxo,wxl:integer;
    kanaq:string;
    undersolid:boolean;
    color,fcolor:TColor;
    boldness:boolean;
    meaning,reading,kanji:string;
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
    worddict,lastworddict:string;
    inblock:boolean;
    colback,coltext:TColor;
    gd1,gd2,gd0:string;
    a:integer;
function RecodeChar(ch:string):string;
var beg:string;
begin
  if ch='FF00'then ch:='0020';
  result:=ch;
end;
begin
  colback:=Col('Editor_Back');
  coltext:=COl('Editor_Text');
  if doc.Count=0 then
  begin
    doc.Add('');
    doctr.Add('');
  end;
  if printing then st2:=fSettings.CheckBox30.Checked else st2:=fTranslate.SpeedButton9.Down;
  lst0:=false;
  if fSettings.CheckBox56.Checked then lst0:=true;
  if printing then st0:=fSettings.CheckBox29.Checked else st0:=fTranslate.SpeedButton8.Down;
  st3:=(fSettings.CheckBox42.Checked);
  linec:=2;
  st2c:=1;
  try
    st2c:=strtoint(fSettings.Edit17.Text);
  except end;
  if st3 then inc(linec);
  if st0 or lst0 then inc(linec);
  if st2 then inc(linec,st2c);
  vert:=fSettings.CheckBox37.Checked and printing;
  if vert then screenh:=w else screenh:=h;
  if vert then screenw:=h else screenw:=w;
  if not printing then
  begin
    if fTranslate.SpeedButton17.Down then rs:=16 else if fTranslate.SpeedButton18.Down then rs:=10 else rs:=8;
  end else
  begin
    lineh:=20;
    try
      lineh:=strtoint(fSettings.Edit18.Text);
    except end;
    rs:=screenh div lineh div linec;
  end;
  a:=GetTickCount;
  cx:=0;
  cy:=0;
  doall:=ll.Count=0;
  insval:=-1;
  try
  if (doall) then
  begin
    if not doall then
    begin
      cy:=y;
      i:=0;
      while i<ll.Count div 3 do
      begin
        if strtoint(ll[i*3+1])=y then
        begin
          if insval=-1 then insval:=i;
          ll.delete(i*3);
          ll.delete(i*3);
          ll.delete(i*3);
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
        if (not vert) and (HalfWidth(wxl,cy)) then inc(px,rs) else inc(px,rs*2);
//        inc(px,rs*2);
        inc(wxl);
      end;
      dec(wxl);
      wx:=length(doc[cy]) div 4;
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
        wxo:=wx;
        if cy>=doc.Count then showmessage('Internal line-computing error!');
//        if fSettings.CheckBox43.Checked then while GetDocTr(wx,cy)[1]='<'do dec(wx);
        if wx<=cx then wx:=wxo;
        if doall then ll.Add(inttostr(cx)) else ll.Insert(insval*3,inttostr(cx));
        if doall then ll.Add(inttostr(cy)) else ll.Insert(insval*3+1,inttostr(cy));
        if doall then ll.Add(inttostr(wx-cx)) else ll.Insert(insval*3+2,inttostr(wx-cx));
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
  for i:=0 to (ll.Count div 3)-1 do
    if (GetLineAttr(i,1,ll)=y) and (GetLineAttr(i,0,ll)<=x) then cl:=i;
  lineh:=rs;
  cx:=x;
  cy:=y;
  py:=0;
  kanaq:='';
  lastwordstate:='-';
  if printing then Canvas.Brush.Color:=clWhite else
  if fSettings.CheckBox39.Checked then Canvas.Brush.Color:=clWindow else Canvas.Brush.Color:=colBack;
  rect.Left:=l-2;
  rect.Top:=t-2;
  rect.Right:=l+w+4;
  rect.Bottom:=t+h+4;
  Canvas.FillRect(rect);
  try
  while (py+lineh*linec<screenh) and (cl<ll.Count div 3) do
  begin
    if cl>0 then
    begin
//      showmessage('Here!'+inttostr(cl)+'-'+inttostr(py)+'-'+inttostr(x)+'-'+inttostr(y)+':'+inttostr(cx)+'-'+inttostr(cy));
//      showmessage(ll[0]+','+ll[1]+','+ll[2]+':'+ll[3]+','+ll[4]+','+ll[5]);
    end;
    cx:=GetLineAttr(cl,0,ll);
    cy:=GetLineAttr(cl,1,ll);
    wx:=cx+GetLineAttr(cl,2,ll);
{    if (fSettings.CheckBox32.Checked) and (st2) then
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
    end;
}    while (cx<wx) and ((cx<length(doc[cy]) div 4) or ((kanaq<>'') and st0)) do
    begin
      try
      wordstate:=GetDocTr(cx,cy)[1];
      try
        learnstate:=strtoint(GetDocTr(cx,cy)[2]);
      except showmessage('Invalid LState:'+GetDocTr(cx,cy)); end;
      CalcBlockFromTo(false);
      inblock:=false;
      GetTextWordInfo(cx,cy,meaning,reading,kanji);
      kanjilearned:=false;
      kanjilearned:=kanji=CheckKnownKanji(kanji);
      worddict:=copy(GetDocTr(cx,cy),3,6);
      if wordstate='<'then worddict:=lastworddict;
      if wordstate='<'then wordstate:=lastwordstate;
      lastwordstate:=wordstate;
      lastworddict:=worddict;
      undersolid:=worddict<>'000000';
      if (upcase(wordstate)<>'F') and (upcase(wordstate)<>'D') then reading:='';
      if (fSettings.CheckBox36.Checked) and (EvalChar(GetDoc(cx,cy))=3) and (not showroma) then reading:=
        RomajiToKana('H'+KanaToRomaji(GetDoc(cx,cy),1,'j'),1,true,'j');
      if (fSettings.CheckBox36.Checked) and (EvalChar(GetDoc(cx,cy))>1) and (EvalChar(GetDoc(cx,cy))<4) and (showroma) then
      begin
        gd1:=GetDoc(cx,cy);
        gd2:=GetDoc(cx+1,cy);
        gd0:=GetDoc(cx-1,cy);
        if (gd1='30C3') or (gd1='3063') then gd1:='' else
        if ((gd0='30C3') or (gd0='3063')) and
           ((gd2='3041') or (gd2='3043') or (gd2='3045') or (gd2='3047') or (gd2='3049') or
           (gd2='3083') or (gd2='3085') or (gd2='3087') or
           (gd2='30A1') or (gd2='30A3') or (gd2='30A5') or (gd2='30A7') or (gd2='30A9') or
           (gd2='30E3') or (gd2='30E5') or (gd2='30E7')) then gd1:=gd0+gd1+gd2 else
        if (gd0='30C3') or (gd0='3063') then gd1:=gd0+gd1 else
        if (gd2='3041') or (gd2='3043') or (gd2='3045') or (gd2='3047') or (gd2='3049') or
           (gd2='3083') or (gd2='3085') or (gd2='3087') or
           (gd2='30A1') or (gd2='30A3') or (gd2='30A5') or (gd2='30A7') or (gd2='30A9') or
           (gd2='30E3') or (gd2='30E5') or (gd2='30E7') then gd1:=gd1+gd2 else
        if (gd1='3041') or (gd1='3043') or (gd1='3045') or (gd1='3047') or (gd1='3049') or
           (gd1='3083') or (gd1='3085') or (gd1='3087') or
           (gd1='30A1') or (gd1='30A3') or (gd1='30A5') or (gd1='30A7') or (gd1='30A9') or
           (gd1='30E3') or (gd1='30E5') or (gd1='30E7') then gd1:='';
        if EvalChar(copy(gd1,1,4))=3 then gd1:=RomajiToKana('H'+KanaToRomaji(gd1,1,'j'),1,true,'j');
        reading:=gd1;
      end;
      if (fSettings.CheckBox36.Checked) and (GetDoc(cx,cy)='30FC') then reading:='30FC';
      if not fSettings.CheckBox32.Checked then undersolid:=false;
      if fSettings.CheckBox39.Checked then color:=clWindow else color:=colBack;
      if fSettings.CheckBox39.Checked then fcolor:=clWindowText else fcolor:=colText;
      if printing then color:=clWhite;
      if not fSettings.CheckBox39.Checked then
      begin
        if printing and fSettings.CheckBox31.Checked then
          case upcase(wordstate) of
            '-','X':color:=$00FFFFFF;
            '?':color:=$00FFFFFF;
            'P':color:=$00FFFFFF;
            'I':color:=$00FFFFFF;
            'F':color:=$00FFFFFF;
            'D':color:=$00FFFFFF;
            'H':color:=$00FFFFFF;
            'K':color:=$00FFFFFF;
          end else
          if fMenu.aEditorColors.Checked then case upcase(wordstate) of
            '-','X':color:=Col('Editor_Untranslated');
            '?':color:=Col('Editor_NotFound');
            'P':color:=Col('Editor_Particle');
            'I':color:=Col('Editor_Untranslated');
            'F':color:=Col('Editor_Translated');
            'D':color:=Col('Editor_Translated');
            'H':color:=Col('Editor_Translated');
            'K':color:=Col('Editor_Translated');
          end else color:=Col('Editor_Untranslated');
      end;
      invert:=false;
      if (fSettings.CheckBox33.Checked) and (learnstate>1) and (learnstate<4) then meaning:='';
      if upcase(wordstate)<>wordstate then boldness:=true else boldness:=false;
      if printing and fSettings.CheckBox31.Checked then begin end else
      if fMenu.aEditorColors.Checked then case learnstate of
        0: color:=Col('Editor_Problematic');
        1: color:=Col('Editor_Unlearned');
        2: color:=Col('Editor_Learned');
        3: color:=Col('Editor_Mastered');
      end;
      if not fSettings.CheckBox40.Checked then boldness:=false;
      if (fSettings.CheckBox35.Checked) and (kanjilearned) then reading:='';
      if printing then Canvas.Brush.Color:=clWhite else
      if fSettings.CheckBox39.Checked then Canvas.Brush.Color:=clWindow else Canvas.Brush.Color:=colBack;
      if printing then canvas.Font.Color:=clBlack else
      if fSettings.CheckBox39.Checked then canvas.Font.Color:=clWindowText else canvas.Font.Color:=ColText;
      if (st2) and (meaning<>'') then
      begin
        cnty:=py+rs*2;
        we:=cx+1;
        cntx:=px+rs*2;
        while (we<wx) and (we<cx+6) and (copy(GetDocTr(we,cy),3,6)='000000') and (GetDocTr(we,cy)[1]<>'?') do
        begin
          if (HalfWidth(we,cy)) and not vert then inc(cntx,rs) else inc(cntx,rs*2);
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
        if fSettings.CheckBox32.Checked then
        begin
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
      end;
      if showroma then if curlang='c'then reading:=ConvertPinYin(KanaToRomaji(reading,romasys,curlang)) else
                                           reading:=UnicodeToHex(KanaToRomaji(reading,romasys,curlang));
      if reading<>'' then kanaq:=kanaq+reading;
      cntx:=px;
      inblock:=false;
      if inblock then
      begin
        canvas.Font.Color:=color;
        canvas.Brush.Color:=ColText;
      end else
      begin
        if printing then canvas.Font.Color:=clBlack else
        if fSettings.CheckBox39.Checked then canvas.Font.Color:=clWindowText else canvas.Font.Color:=ColText;
        if (not fSettings.CheckBox39.Checked) then
        begin
          if (fSettings.CheckBox41.Checked) and ((EvalChar(GetDoc(cx,cy))>4) or (EvalChar(GetDoc(cx,cy))=0)) then canvas.Font.Color:=Col('Editor_ASCII');
          if wordstate='I'then canvas.Font.Color:=Col('Editor_Active') else
          begin
            canvas.Font.Color:=fcolor;
            if (cy=insy) and (cx>=insx) and (cx<insx+inslen) then canvas.Font.Color:=Col('Editor_Aftertouch');
            canvas.Brush.Color:=color;
          end;
        end;
      end;
      if st0 then for i:=1 to 2 do if kanaq<>'' then
      if (i=1) or (vert) or (not HalfWidth(cx,cy)) then
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
            DrawUnicode(canvas,realx+l,realy+t-1,rs,copy(kanaq,1,8),FontChineseGrid) else
            DrawUnicode(canvas,realx+l,realy+t-1,rs,copy(kanaq,1,8),FontJapaneseGrid);
          delete(kanaq,1,8);
        end else
        begin
          if curlang='c'then
            DrawUnicode(canvas,realx+l,realy+t-1,rs,copy(kanaq,1,4),FontChineseGrid) else
            DrawUnicode(canvas,realx+l,realy+t-1,rs,copy(kanaq,1,4),FontJapaneseGrid);
          delete(kanaq,1,4);
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
      if (not vert) and (HalfWidth(cx,cy)) then rect.Right:=realx+l+rs;
      rect.Top:=realy+t;
      rect.Bottom:=realy+t+rs*2;
      canvas.FillRect(rect);
      if curlang='c'then
        DrawUnicode(canvas,realx+l,realy+t,rs*2,RecodeChar(GetDoc(cx,cy)),FontChineseGrid) else
        DrawUnicode(canvas,realx+l,realy+t,rs*2,RecodeChar(GetDoc(cx,cy)),FontJapaneseGrid);
//        showmessage(inttostr(realx)+#13+inttostr(realy)+#13+RecodeChar(GetDoc(cx,cy)));
      if (undersolid) and (st2) and (fSettings.CheckBox32.Checked) then
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
      if printing then Canvas.Brush.Color:=clWhite else
      if fSettings.CheckBox39.Checked then Canvas.Brush.Color:=clWindow else Canvas.Brush.Color:=colBack;
      if printing then canvas.Font.Color:=clBlack else
      if fSettings.CheckBox39.Checked then canvas.Font.Color:=clWindowText else canvas.Font.Color:=colText;
      if (not vert) and (HalfWidth(cx,cy)) then inc(px,rs) else inc(px,rs*2);
      inc(cx);
      except
        showmessage('Paint exception ('+inttostr(cx)+','+inttostr(cy)+': '+(ExceptObject as Exception).Message);
      end;
    end;
    inc(py,rs*linec);
    px:=0;
    inc(cl);
  end;
  except
    showmessage('Paint exception: '+(ExceptObject as Exception).Message);
  end;
end;

procedure TfUser.Translate_CheckBox1Click(Sender: TObject);
begin
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfUser.Translate_ListBox1KeyDown(Sender: TObject; var Key: Word;
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
      if curx>=GetLineAttr(cury,2,linl) then
        if cury+1<linl.Count div 3 then
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
          curx:=GetLineAttr(cury,2,linl)-1;
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
      cury:=linl.Count div 3-1;
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
  begin
    if (key=VK_UP) and (StringGrid1.Row>1) then StringGrid1.Row:=StringGrid1.Row-1;
    if (key=VK_DOWN) and (StringGrid1.Row<StringGrid1.RowCount-1) then StringGrid1.Row:=StringGrid1.Row+1;
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (insx<>-1) then ShowHint else HideHint;
  end;
end;

procedure TfUser.DisplayInsert(convins,transins:string;leaveinserted:boolean);
var i:integer;
    s:string;
begin
  if insx=-1 then
  begin
    insx:=rcurx;
    insy:=rcury;
    inslen:=0;
  end;
  s:=doc[insy];
  delete(s,insx*4+1,inslen*4);
  doc[insy]:=s;
  s:=doctr[insy];
  delete(s,insx*9+1,inslen*9);
  doctr[insy]:=s;
  inslen:=length(convins) div 4;
  if transins='' then
    for i:=1 to length(convins) div 4 do transins:=transins+'I90000001';
  doc[insy]:=copy(doc[insy],1,insx*4)+convins+copy(doc[insy],insx*4+1,length(doc[insy])-insx*4);
  doctr[insy]:=copy(doctr[insy],1,insx*9)+transins+copy(doctr[insy],insx*9+1,length(doctr[insy])-insx*9);
  linl.Clear;
  RenderText(curx,cury,fTranslate.PaintBox6.Canvas,0,0,fTranslate.PaintBox6.Width-4,fTranslate.PaintBox6.Height-4,linl,printl,lastxsiz,lastycnt,false,true);
  for i:=0 to (linl.Count div 3)-1 do
  begin
    if (GetLineAttr(i,1,linl)=insy) and (GetLineAttr(i,0,linl)<=insx+inslen) and (insx+inslen-GetLineAttr(i,0,linl)<GetLineAttr(i,2,linl)) then
    begin
      curx:=insx+inslen-GetLineAttr(i,0,linl);
      cury:=i;
    end;
  end;
  if not leaveinserted then
  begin
    insconfirmed:=true;
  end;
end;

procedure TfUser.ResolveInsert(final:boolean);
var s,s2,s3:string;
    rlen:integer;
    wlen:integer;
    i:integer;
begin
  if (insx=-1) and (final) then exit;
  if (buffertype='H') and (resolvebuffer) then
  begin
    if StringGrid1.Visible then
    begin
      s:=curkanji;
      priorkanji:=curkanji;
      s2:=GetInsertKana(false);
      s3:=curphonetic;
      while (copy(s,length(s)-3,4)=copy(s3,length(s3)-3,4)) and (s<>'') do
      begin
        delete(s,length(s)-3,4);
        delete(s3,length(s3)-3,4);
      end;
      if (s='') and (curkanji[3]>='A') then s:=curkanji else
        s:=s+copy(s2,length(s3)+1,length(s2)-length(s3));
      DisplayInsert(s,'',true);
    end else if not final then DisplayInsert(GetInsertKana(true),'',true);
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
      s2:='';
      for i:=0 to (length(s) div 4)-1 do
        if i=0 then s2:=s2+buffertype+'90000001'else s2:=s2+'<90000001';
      DisplayInsert(s,s2,true);
      if resolvebuffer then SetWordTrans(insx,insy,false,false,true);
      insconfirmed:=true;
      mustrepaint:=true;
      ShowText(false);
    end else DisplayInsert(GetInsertKana(true),'',true);
  end;
end;

function TfUser.GetInsertKana(display:boolean):string;
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

procedure TfUser.InsertCharacter(c:char);
var chartype:char;
    immchar:string;
    immmode,kanamode:boolean;
begin
  if (c='[') or (c=']') then
  begin
    if (c='[') and (StringGrid1.Row>1) then StringGrid1.Row:=StringGrid1.Row-1;
    if (c=']') and (StringGrid1.Row<StringGrid1.RowCount-1) then StringGrid1.Row:=StringGrid1.Row+1;
    if insconfirmed then ResolveInsert(true);
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (insx<>-1) then ShowHint else HideHint;
    exit;
  end;
  if insconfirmed then ClearInsBlock;
  ChangeFile(true);
  immmode:=fTranslate.speedbutton5.down;
  kanamode:=fTranslate.speedbutton7.down;
  if (c=' ') and (insertbuffer<>'') then
  begin
    resolvebuffer:=fTranslate.speedbutton6.down;
    ResolveInsert(true);
    if fTRanslate.speedbutton6.down then exit;
  end;
  if (c=#13) and (insertbuffer<>'') then
  begin
    resolvebuffer:=false;
    ResolveInsert(true);
    if fTranslate.speedbutton6.down then exit;
  end;
  if (c=#8) and (insertbuffer<>'') then
  begin
    delete(insertbuffer,length(insertbuffer),1);
    DisplayInsert(GetInsertKana(true),'',insertbuffer<>'');
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
          curx:=GetLineAttr(cury,2,linl)-1;
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
    ',':immchar:='3001';
    '.':immchar:='3002';
    '"':immchar:='3003';
    '<':immchar:='3008';
    '>':immchar:='3009';
    '(':immchar:='300C';
    ')':immchar:='300D';
    '[':immchar:='3016';
    ']':immchar:='3017';
    '{':immchar:='3010';
    '}':immchar:='3011';
    ' ':immchar:='0020';
  end;
  chartype:='-';
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
      DisplayInsert(immchar,'-90000001',false)
      else DisplayInsert(UnicodeToHex(c),'-90000001',false);
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
  DisplayInsert(GetInsertKana(true),'',true);
//  resolvebuffer:=true;
//  ResolveInsert(false);
  mustrepaint:=true;
  ShowText(true);
//  Look(false);
end;

procedure TfUser.CheckTransCont(x,y:integer);
begin
  while GetDocTr(x,y)[1]='<'do
  begin
    SetDocTr(x,y,'-90000001');
    inc(x);
  end;
end;

procedure TfUser.SplitLine(x,y:integer);
var ins,ints:string;
begin
  if length(doc[y]) div 4<=x then
  begin
    if doc.Count-1=y then
    begin
      doc.Add('');
      doctr.Add('');
    end else
    begin
      doc.Insert(y+1,'');
      doctr.Insert(y+1,'');
    end;
  end else
  begin
    ins:=copy(doc[y],x*4+1,length(doc[y])-x*4);
    ints:=copy(doctr[y],x*9+1,length(doctr[y])-x*9);
    if doc.Count-1=y then
    begin
      doc.Add(ins);
      doctr.Add(ints);
    end else
    begin
      doc.Insert(y+1,ins);
      doctr.Insert(y+1,ints);
    end;
    ins:=copy(doc[y],1,x*4);
    ints:=copy(doctr[y],1,x*9);
    doc[y]:=ins;
    doctr[y]:=ints;
    CheckTransCont(0,y+1);
  end;
end;

procedure TfUser.JoinLine(y:integer);
var ins,ints:string;
begin
  if y+1=doc.Count then exit;
  ins:=doc[y]+doc[y+1];
  ints:=doctr[y]+doctr[y+1];
  doc[y]:=ins;
  doctr[y]:=ints;
  doc.delete(y+1);
  doctr.delete(y+1);
end;

procedure TfUser.DeleteCharacter(x,y:integer);
begin
  if length(doc[y]) div 4<=x then JoinLine(y) else
  begin
    doc[y]:=copy(doc[y],1,x*4)+copy(doc[y],x*4+5,length(doc[y])-x*4-4);
    doctr[y]:=copy(doctr[y],1,x*9)+copy(doctr[y],x*9+10,length(doctr[y])-x*9-9);
    CheckTransCont(x,y);
  end;
end;

procedure TfUser.RefreshLines;
begin
  linl.Clear;
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfUser.Translate_PaintBox6Click(Sender: TObject);
begin
  fTranslate.ListBox1.SetFocus;
end;

function TfUser.SetWordTrans(x,y:integer;scanparticle:boolean;gridfirst:boolean;user:boolean):integer;
var wordpart:char;
    worddict,worduser:string;
    lst:string;
    i:integer;
    rlen:integer;
    s,s2:string;
    wt:integer;
    wd:string;
    globdict:string;
    dw:string;
begin
  ChangeFile(true);
  if fSettings.CheckBox34.Checked then scanparticle:=false;
  if (y=-1) or (y>=doctr.Count) or (x=-1) then exit;
  s2:=GetDoc(x,y);
  dw:=GetDocWord(x,y,wt,not user);
  result:=0;
  rlen:=length(dw) div 4;
  worddict:='';
  globdict:='0';
  if worddict='' then
  begin
    if gridfirst then i:=0 else
      if not StringGrid1.Visible then i:=-1 else
        i:=StringGrid1.Row-1;
    if ul.Count=0 then i:=-1;
    if i=-1 then
    begin
      wordpart:='-';
      worddict:='000000';
      lst:='9';
      if wt=1 then rlen:=1;
    end else
    begin
      wordpart:=ul[i][15];
      worddict:=copy(ul[i],7,6);
      s:=dicsl[i];
      globdict:='0';
      if (pos('<d',s)>0) then
      begin
        globdict:=copy(s,pos('<d',s)+2,length(s)-pos('<d',s)-1);
        globdict:=copy(globdict,1,pos('>',globdict)-1);
        if docdic.IndexOf(globdict)<>-1 then globdict:=inttostr(docdic.IndexOf(globdict)) else
        begin
          docdic.add(globdict);
          globdict:=inttostr(docdic.Count-1);
        end;
        if length(globdict)>1 then globdict:='-';
      end;
//      while length(globdict)<10 do globdict:=globdict+' ';
      if copy(ul[i],1,6)<>'000000'then
      begin
        TUser.Locate('Index',copy(ul[i],1,6),true);
        lst:=TUser.Str(TUserScore);
        if lst='' then lst:='9';
      end else lst:='9';
      rlen:=strtoint(copy(ul[i],13,2));
    end;
  end;
  if wordpart='-'then if wt<>1 then s:='-'else s:='?';
  if wordpart<>'-'then
  case wt of
    2:if fSettings.CheckBox38.Checked then s:='-'else s:='H';
    3:if s2='30FC'then s:='-'else s:='K';
    1:if wordpart='I'then s:='D'else s:='F';
    else s:='-';
  end;
  if wordpart='P'then s:='P';
  if user then s:=lowercase(s);
  s:=s+lst+worddict;
  if s[1]='-'then s:='-9000000';
  SetDocTr(x,y,s+globdict);
  for i:=2 to rlen do
    if (x+i-1)*4<length(doc[y]) then
      SetDocTr(x+i-1,y,'<'+lst+'000000'+globdict);
  delete(dw,1,rlen*4);
  if (s[1]='K') and (length(doc[y])>(x+rlen)*4) then
  begin
    dw:=GetDocWord(x+rlen,y,wt,false);
    if wt<>2 then dw:='';
  end;
  if length(dw)>16 then dw:=copy(dw,1,16);
  for i:=length(dw) div 4 downto 1 do if EvalChar(copy(dw,i*4-3,4))=1 then delete(dw,i*4-3,length(dw)-i*4+4);
  result:=rlen;
  if (scanparticle) and (s[1]<>'-') and (partl.IndexOf(dw)>-1) then
  begin
    if user then s:='p90000001'else s:='P90000001';
    SetDocTr(x+rlen,y,s);
    for i:=2 to length(dw) div 4 do SetDocTr(x+rlen+i-1,y,'<90000001');
    result:=rlen+length(dw) div 4;
    exit;
  end;
end;

procedure TfUser.Translate_Button8Click(Sender: TObject);
begin
  if (blockx=rcurx) and (blocky=rcury) then
  begin
    SetWordTrans(rcurx,rcury,true,false,true);
    mustrepaint:=true;
    ShowText(true);
  end else Translate_Button7Click(Sender);
end;

procedure TfUser.SpeedButton1Click(Sender: TObject);
begin
  Look(false);
  if Edit1.Enabled then Edit1.SetFocus;
end;

procedure TfUser.CalcMouseCoords(x,y:integer;var rx,ry:integer);
var cx,cy:integer;
begin
  rx:=-1;
  ry:=-1;
  cx:=x div (lastxsiz);
  cy:=y div (lastxsiz*lastycnt)+view;
  if cy<0 then cy:=0;
  cx:=WidthToPos(cx,cy);
  if cy>=linl.Count div 3 then exit;
  ry:=GetLineAttr(cy,1,linl);
  rx:=cx+GetLineAttr(cy,0,linl);
  if (ry>=doc.Count) or (rx>=length(doc[ry]) div 4) then
  begin
    ry:=-1;
    rx:=-1;
    exit;
  end;
end;

procedure TfUser.Translate_PaintBox6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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

procedure TfUser.Translate_PaintBox6MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then exit;
  curx:=(x+lastxsiz div 2) div (lastxsiz);
  cury:=y div (lastxsiz*lastycnt)+view;
  curx:=WidthToPos(curx,cury);
  if (curx=lastmmx) and (cury=lastmmy) then exit;
  lastmmx:=curx;
  lastmmy:=cury;
  shiftpressed:=true;
  ShowText(false);
end;

procedure TfUser.ScrollBar1Change(Sender: TObject);
begin
  view:=fTranslate.ScrollBar1.Position;
  if (view>=0) and (view<linl.Count div 3) then
  begin
    rviewx:=GetLineAttr(view,0,linl);
    rviewy:=GetLineAttr(view,1,linl);
  end;
  MakeEditorBitmap;
end;

procedure TfUser.DrawCursor(blink:boolean);
function OnScreen(x,y:integer):boolean;
begin
  if (x<0) or (y<0) or (y>=linl.Count div 3) or (x>GetLineAttr(y,2,linl)) then
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
  InvertRect(fTranslate.PaintBox6.Canvas.Handle,rect);
end;
begin
  if not fTranslate.ListBox1.Focused then blink:=false;
  if cursorposcache=-1 then CalcCache(oldcurx,oldcury);
  if (OnScreen(oldcurx,oldcury)) and (not cursorblinked) then DrawIt(oldcurx,oldcury-view);
  if (cursorposcache=-1) or (oldcurx<>cursorscreenx) or (oldcury<>cursorscreeny) then CalcCache(cursorscreenx,cursorscreeny);
  if OnScreen(cursorscreenx,cursorscreeny) and ((not blink) or (cursorblinked)) then DrawIt(cursorscreenx,cursorscreeny-view);
  if blink then cursorblinked:=not cursorblinked;
  if not blink then cursorblinked:=false;
  oldcurx:=cursorscreenx; oldcury:=cursorscreeny;
end;

procedure TfUser.DrawBlock;
procedure DrawIt(fx,fy,tx,ty:integer);
var rect:TRect;
    i,j:integer;
    js:integer;
    xs,ys:integer;
begin
  for i:=0 to (linl.Count div 3)-1 do
    if (GetLineAttr(i,1,linl)>=fy) and (GetLineAttr(i,1,linl)<=ty) then
    begin
      js:=0;
      for j:=0 to GetLineAttr(i,2,linl)-1 do
      begin
        xs:=j+GetLineAttr(i,0,linl);
        ys:=GetLineAttr(i,1,linl);
        if  ((ys>fy) or ((ys=fy) and (xs>=fx))) and
            ((ys<ty) or ((ys=ty) and (xs<tx))) and
            (i>=view) and (i<view+printl) then
        begin
          rect.top:=(i-view)*lastxsiz*lastycnt+2;
          rect.left:=js*lastxsiz;
          rect.bottom:=rect.top+lastxsiz*lastycnt-1;
          if not HalfWidth(xs,ys) then rect.right:=rect.left+lastxsiz*2 else
            rect.right:=rect.left+lastxsiz;
          InvertRect(fTranslate.PaintBox6.Canvas.Handle,rect);
        end;
        if HalfWidth(xs,ys) then inc(js) else inc(js,2);
      end;
    end;
end;
begin
  if oldblockfromx<>-1 then DrawIt(oldblockfromx,oldblockfromy,oldblocktox,oldblocktoy);
  CalcBlockFromTo(false);
  DrawIt(blockfromx,blockfromy,blocktox,blocktoy);
  oldblockfromx:=blockfromx; oldblockfromy:=blockfromy; oldblocktox:=blocktox; oldblocktoy:=blocktoy;
end;

procedure TfUser.BlockOp(copy,delete:boolean);
var i,j:integer;
    bx,tx:integer;
    befclip,befcliptrans:string;
begin
  CalcBlockFromTo(false);
  if copy then
  begin
    befclip:=clip;
    befcliptrans:=cliptrans;
    clip:='';
    cliptrans:='';
    for i:=blockfromy to blocktoy do
    begin
      if i=blockfromy then bx:=blockfromx else bx:=0;
      if i=blocktoy then tx:=blocktox-1 else tx:=(length(doc[i]) div 4)-1;
      for j:=bx to tx do
      begin
        clip:=clip+GetDoc(j,i);
        cliptrans:=cliptrans+GetDocTr(j,i);
      end;
      if i<>blocktoy then clip:=clip+'000D000A';
      if i<>blocktoy then cliptrans:=cliptrans+'-90000001-90000001';
    end;
    if clip='' then
    begin
      clip:=befclip;
      cliptrans:=befcliptrans;
    end;
    fMenu.ChangeClipboard;
  end;
  if delete then
  begin
    SetCurPos(blockfromx,blockfromy);
    if blockfromy=blocktoy then
    begin
      doc[blockfromy]:=system.copy(doc[blockfromy],1,blockfromx*4)+system.copy(doc[blockfromy],blocktox*4+1,length(doc[blockfromy])-blocktox*4);
      doctr[blockfromy]:=system.copy(doctr[blockfromy],1,blockfromx*9)+system.copy(doctr[blockfromy],blocktox*9+1,length(doctr[blockfromy])-blocktox*9);
    end else
    begin
      doc[blockfromy]:=system.copy(doc[blockfromy],1,blockfromx*4);
      doctr[blockfromy]:=system.copy(doctr[blockfromy],1,blockfromx*9);
      doc[blocktoy]:=system.copy(doc[blocktoy],blocktox*4+1,length(doc[blocktoy])-blocktox*4);
      doctr[blocktoy]:=system.copy(doctr[blocktoy],blocktox*9+1,length(doctr[blocktoy])-blocktox*9);
      for i:=blockfromy+1 to blocktoy-1 do
      begin
        doc.Delete(blockfromy+1);
        doctr.Delete(blockfromy+1);
      end;
      JoinLine(blockfromy);
      RefreshLines;
    end;
    ChangeFile(true);
  end;
end;

procedure TfUser.PasteOp;
var y:integer;
    i:integer;
    l:integer;
begin
  resolvebuffer:=false;
  if insertbuffer<>'' then ResolveInsert(true);
  ClearInsBlock;

  SplitLine(rcurx,rcury);
  y:=rcury;
  for i:=1 to length(clip) div 4 do
  begin
    if copy(clip,i*4-3,4)='000A'then
    begin
      inc(y);
      doc.Insert(y,'');
      doctr.Insert(y,'');
    end else if copy(clip,i*4-3,4)<>'000D'then
    begin
      doc[y]:=doc[y]+copy(clip,i*4-3,4);
      if cliptrans<>'' then doctr[y]:=doctr[y]+copy(cliptrans,i*9-8,9) else doctr[y]:=doctr[y]+'-90000001';
    end;
  end;
  l:=length(doc[y]) div 4;
  JoinLine(y);
  RefreshLines;
  SetCurPos(l,y);
  ChangeFile(true);
  ShowText(true);
end;

procedure TfUser.Translate_Button7Click(Sender: TObject);
var i,j:integer;
    a:integer;
    oldcurx,oldcury:integer;
    p,pn:boolean;
    sp:TSMPromptForm;
    bg,en:integer;
  updateProgressEvery: integer;
  lastUpdateProgress: integer;
begin
  oldcurx:=rcurx;
  oldcury:=rcury;
  if (blockx=rcurx) and (blocky=rcury) then
  begin
    if Application.MessageBox(pchar(_l('#00682^eNo block is selected. Do you want generate translation for entire document?'#13#13'This action can take a very long time.^cNení zvolen žádný blok. Chcete vygenerovat informace o pøekladu pro celý dokument?'#13#13'Tato akce mùže trvat velmi dlouho.')),pchar(_l('#00683^eConfirmation^cPotvrzení')),
    MB_ICONWARNING or MB_YESNO)<>idYes then exit;
    blockfromx:=0;
    blockfromy:=0;
    blocktox:=(length(doc[doc.Count-1]) div 4)-1;
    blocktoy:=doc.Count-1;
  end else CalcBlockFromTo(true);
  Screen.Cursor:=crHourGlass;

  sp:=SMProgressDlg(_l('#00684^eTranslator^cPøekladaè'),_l('#00685^eTranslating...^cPøekládám...'),blocktoy-blockfromy+1);
 //We don't want to update very often since redrawing is slow.
 //Let's only update on every percent or less.
  updateProgressEvery := (blocktoy-blockfromy+1) div 100;
  if updateProgressEvery<1 then updateProgressEvery := 1; //<100 items
  lastUpdateProgress := -updateProgressEvery-1; //update now

  for i:=blockfromy to blocktoy do
  begin
    bg:=0;
    en:=(length(doc[i]) div 4)-1;
    if i=blockfromy then bg:=blockfromx;
    if i=blocktoy then en:=blocktox;

    //Do not update progress too often
    if i-blockfromy > lastUpdateProgress + updateProgressEvery then begin
      sp.SetProgress(i-blockfromy);
      lastUpdateProgress := i-blockfromy;
    end;

    j:=bg;
    while j<=en do if (upcase(GetDocTr(j,i)[1])<>GetDocTr(j,i)[1]) then
    begin
      inc(j);
      while GetDocTr(j,i)[1]='<'do inc(j);
    end else
    begin
      rcurx:=j;
      rcury:=i;
      SpeedButton1.Down:=false;
      SpeedButton2.Down:=false;
      SpeedButton3.Down:=false;
      dic_ignorekana:=true;
      Look(true, {NoScreenUpdates=}true);
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

function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var pl,xs,yc:integer;
begin
  plinl.Clear;
  fUser.RenderText(0,0,canvas,width div 50,height div 50,width-width div 25,height-height div 25,plinl,pl,xs,yc,true,true);
  printpl:=pl;
  result:=(((plinl.Count div 3)-1) div pl)+1;
  if result<1 then result:=1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var pl,xs,yc:integer;
begin
  if plinl.Count div 3<=(pagenum-1)*printpl then exit;
  fUser.RenderText(fUser.GetLineAttr((pagenum-1)*printpl,0,plinl),fUser.GetLineAttr((pagenum-1)*printpl,1,plinl),
    canvas,width div 50,height div 50,width-width div 25,height-height div 25,plinl,pl,xs,yc,true,false);
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.PageControl1.ActivePage:=fSettings.TabSheet9;
  fSettings.ShowModal;
end;

procedure TfUser.Translate_Button9Click(Sender: TObject);
begin
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00686^eTranslated text^cPøeložený text'));
end;

procedure TfUser.Translate_Button6Click(Sender: TObject);
var i,j:integer;
    s:string;
    bg,en:integer;
begin
  CalcBlockFromTo(true);
  for i:=blockfromy to blocktoy do
  begin
    bg:=0;
    en:=(length(doc[i]) div 4)-1;
    if i=blockfromy then bg:=blockfromx;
    if i=blocktoy then en:=blocktox;
    for j:=bg to en do SetDocTr(j,i,'-90000001');
  end;
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfUser.BitBtn1Click(Sender: TObject);
begin
  BitBtn1.Enabled:=false;
  Look(false);
end;

procedure TfUser.Translate_Button10Click(Sender: TObject);
function GetDoc(ax,ay:integer):string;
begin
  if ax>=length(doc[ay]) div 4 then result:='0000'else result:=copy(doc[ay],ax*4+1,4);
end;
function GetDocTr(ax,ay:integer):string;
begin
  if ax>=length(doctr[ay]) div 18 then result:='!9000000          'else result:=copy(doctr[ay],ax*18+1,18);
end;
var f:file;
    b:byte;
    i,j,k:integer;
    roma,romafrm:string;
    kan,kanfrm:string;
    kanji,meaning,reading:string;
    ml,jl:TStringList;
    wl,kl:TStringList;
    on,kun,def1,def2:string;
    radf:integer;
    dnam:string;
    dic:TJaletDic;
    markers:string;
begin
  if SaveDialog2.Execute then
  begin
    assignfile(f,SaveDialog2.FileName);
    rewrite(f,1);
    ml:=TStringList.Create;
    wl:=TStringList.Create;
    kl:=TStringList.Create;
    jl:=TStringList.Create;
    TCharRead.SetOrder('');
    b:=255;
    blockwrite(f,b,1);
    b:=254;
    blockwrite(f,b,1);
    for i:=0 to doc.Count-1 do
    begin
      ml.Clear;
      jl.Clear;
      kl.Clear;
      wl.Clear;
      writelnmixuni(f,'>{'+doc[i]+'}');
      kan:='';
      roma:='';
      romafrm:='';
      kanfrm:='';
      for j:=0 to length(doc[i]) div 4 do
      begin
        meaning:='';
        reading:='';
        kan:=kan+GetDoc(j,i);
        kanfrm:=kanfrm+GetDoc(j,i);
        if copy(GetDocTr(j,i),3,6)<>'000000'then
        begin
          dnam:=copy(GetDocTr(j,i),9,10);
          dic:=nil;
          for k:=0 to dicts.Count-1 do
          begin
            if (dicts.Objects[k] as TJaletDic).loaded then
              if trim(dnam)=(dicts.Objects[k] as TJaletDic).name then
                dic:=dicts.Objects[k] as TJaletDic;
          end;
          if dic<>nil then
          begin
            dic.Demand;
            dic.TDict.Locate('Index',copy(GetDocTr(j,i),3,6),true);
            if dic.TDictMarkers<>-1 then meaning:=dic.TDict.Str(dic.TDictEnglish) else
              meaning:=ConvertEdictEntry(dic.TDict.Str(dic.TDictEnglish),markers);
            reading:=dic.TDict.Str(dic.TDictPhonetic);
            if (GetDocTr(j,i)[1]='K') or (GetDocTr(j,i)[1]='H') then reading:='';
            kanji:=dic.TDict.Str(dic.TDictKanji);
            if (meaning<>'') and (wl.IndexOf(copy(GetDocTr(j,i),3,6))=-1) then
            begin
              if length(meaning)>60 then
              begin
                delete(meaning,58,length(meaning)-57);
                meaning:=meaning+'...';
              end;
              ml.Add('{'+kanji+'};{'+reading+'};'+meaning);
              wl.Add(copy(GetDocTr(j,i),3,6));
            end;
          end;
        end;
        while (reading<>'') and (kanji<>'') and (copy(reading,length(reading)-3,4)=copy(kanji,length(kanji)-3,4)) do
        begin
          delete(reading,length(reading)-3,4);
          delete(kanji,length(kanji)-3,4);
        end;
        while length(romafrm)<j*8 do if romafrm='' then romafrm:='3002'else romafrm:=romafrm+'3000';
        romafrm:=romafrm+reading;
        if (reading='') and (GetDocTr(j,i)[1]<>'<') then reading:=GetDoc(j,i);
        roma:=roma+reading;
        if TChar.Locate('Unicode',GetDoc(j,i),false) then
        begin
          if (kl.IndexOf(GetDoc(j,i))=-1) then
          begin
            kl.Add(GetDoc(j,i));
            on:='';
            kun:='';
            def1:='';
            def2:='';
            TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
            while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
            begin
              k:=TCharRead.Int(TCharReadType);
              if (on='') and (k=4) then on:=TCharRead.Str(TCharReadReading) else
              if (kun='') and (k=5) then kun:=TCharRead.Str(TCharReadReading) else
              if (def1='') and (k=3) then def1:=TCharRead.Str(TCharReadReading) else
              if (def2='') and (k=3) then def2:=TCharRead.Str(TCharReadReading);
              TCharRead.Next;
            end;
            if (length(kun)>0) and (kun[1]='-') then delete(kun,1,1);
            if (length(kun)>0) and (kun[1]='+') then delete(kun,1,1);
            if (length(kun)>0) and (kun[length(kun)]='-') then delete(kun,length(kun),1);
            if (length(on)>0) and (on[1]='-') then delete(on,1,1);
            if (length(on)>0) and (on[1]='+') then delete(on,1,1);
            if (length(on)>0) and (on[length(on)]='-') then delete(on,length(on),1);
            radf:=fSettings.ComboBox1.ItemIndex+12;
            TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf)),true);
            jl.Add('{'+GetDoc(j,i)+'};{'+TRadicals.Str(TRadicalsUnicode)+'};'+TChar.Str(TCharJouyouGrade)+';{'+on+'};{'+kun+'};'+def1+';'+def2);
          end;
        end;
      end;
      while length(kanfrm)<length(romafrm) div 2 do kanfrm:=kanfrm+'3000';
      while length(romafrm)*2<length(kanfrm) do romafrm:=romafrm+'3000';
      writelnmixuni(f,'r{'+roma+'}');
      writelnmixuni(f,'f{'+kanfrm+'}');
      writelnmixuni(f,'F{'+romafrm+'}');
      for j:=0 to ml.Count-1 do writelnmixuni(f,':'+ml[j]);
      for j:=0 to jl.Count-1 do writelnmixuni(f,'%'+jl[j]);
    end;
    closefile(f);
    ml.Free;
    wl.Free;
    kl.Free;
    jl.Free;
  end;
end;

procedure TfUser.SpeedButton8Click(Sender: TObject);
begin
  fMenu.ToggleForm(fTranslate,SpeedButton8,fMenu.aDictEditor);
end;

procedure TfUser.SpeedButton5Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordDetails,SpeedButton5,fMenu.aDictDetails);
end;

procedure TfUser.SpeedButton6Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordKanji,SpeedButton6,fMenu.aDictKanji);
end;

procedure TfUser.SpeedButton7Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordCategory,SpeedButton7,fMenu.aDictCategories);
end;

procedure TfUser.SpeedButton9Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordAdd,SpeedButton9,fMenu.aDictAdd);
end;

procedure TfUser.FormHide(Sender: TObject);
begin
//  fMenu.HideForm(SpeedButton5,fMenu.aDictDetails,fWordDetails);
//  fMenu.HideForm(SpeedButton6,fMenu.aDictKanji,fWordKanji);
//  fMenu.HideForm(SpeedButton7,fMenu.aDictCategories,fWordCategory);
//  fMenu.HideForm(SpeedButton9,fMenu.aDictAdd,fWordAdd);
//  fMenu.HideForm(SpeedButton8,fMenu.aDictEditor,fTranslate);
  fMenu.aDict.Checked:=false;
end;

procedure TfUser.SpeedButton23Click(Sender: TObject);
begin
  WordDetails_SpeedButton23Click(sender);
end;

procedure TfUser.FormCreate(Sender: TObject);
begin
  ul:=TStringList.Create;
  linl:=TStringList.Create;
  plinl:=TStringList.Create;
  dicsl:=TStringList.Create;
  randbank:=TStringList.Create;
  fUser.docfilename:='';
  fUser.doctp:=0;
end;

procedure TfUser.FormResize(Sender: TObject);
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[0]-20;
end;

procedure TfUser.Timer1Timer(Sender: TObject);
begin
  if fTranslate.Visible then DrawCursor(true);
end;

procedure TfUser.FormActivate(Sender: TObject);
begin
//  if SpeedButton4.Down then SpeedButton1.Down:=true;
//  SpeedButton1Click(sender);
end;

procedure TfUser.Translate_SaveAs;
var s:string;
begin
  if SaveDialog1.Execute then
  begin
    doctp:=0;
    if pos('.WTT',uppercase(SaveDialog1.FileName))=0 then doctp:=Conv_ChooseType(curlang='c',0); 
    SaveToFile(SaveDialog1.FileName,doctp,false);
    docfilename:=SaveDialog1.FileName;
    s:=SaveDialog1.FileName;
    while pos('\',s)>0 do delete(s,1,pos('\',s));
    fTranslate.Label1.Caption:=uppercase(s);
  end;
end;

procedure TfUser.Translate_SelectAll;
begin
  blockx:=0;
  blocky:=0;
  cury:=(linl.Count div 3)-1;
  curx:=GetLineAttr((linl.Count div 3)-1,2,linl);
  shiftpressed:=true;
  ShowText(true);
end;

procedure TfUser.ChangeFile(changed:boolean);
begin
  fTranslate.SpeedButton2.Enabled:=changed;
  fMenu.aEditorSave.Enabled:=changed;
  filechanged:=changed;
end;

function TfUser.CommitFile:boolean;
var i:integer;
    s:string;
begin
  result:=true;
  if (filechanged) and (fSettings.CheckBox60.Checked) and (docfilename<>'') then SaveToFile(docfilename,doctp,false) else
  if filechanged then
  begin
    i:=Application.MessageBox(pchar(_l('#00687^eDocument has been changed. Do you want to save it?^cDokument byl zmìnìn. Chcete ho uložit?')),
     pchar(_l('#00090^eWarning^cVarování')),MB_ICONWARNING or MB_YESNOCANCEL);
    if i=idYes then
    begin
      if docfilename<>'' then SaveToFile(docfilename,doctp,false) else
      begin
        if SaveDialog1.Execute then
        begin
          if pos('.WTT',uppercase(SaveDialog1.FileName))=0 then doctp:=Conv_ChooseType(curlang='c',0);
          SaveToFile(SaveDialog1.FileName,doctp,false);
          docfilename:=SaveDialog1.FileName;
          s:=SaveDialog1.FileName;
          while pos('\',s)>0 do delete(s,1,pos('\',s));
          fTranslate.Label1.Caption:=uppercase(s);
        end else result:=false;
      end;
    end;
    if i=idCancel then result:=false;
  end;
  filechanged:=false;
end;

procedure TfUser.RepaintText;
begin
  mustrepaint:=true;
  ShowText(false);
end;

procedure TfUser.ClearInsBlock;
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

procedure TfUser.ShowHint;
var l,t:integer;
    p:TPoint;
begin
  if (not fSettings.CheckBox2.Checked) or (not fTranslate.SpeedButton6.Down) then
  begin
    fHint.Hide;
    exit;
  end;
  hinthide:=true;
  p.x:=0;
  p.y:=4;
  p:=fTranslate.ClientToScreen(p);
  l:=p.x+fTranslate.PaintBox6.Left+cursorposcache*lastxsiz;
  t:=p.y+fTranslate.PaintBox6.Top+(cursorscreeny+1-view)*lastxsiz*lastycnt;
  if l+fHint.Width>Screen.Width then l:=Screen.Width-fHint.Width;
  if t+44>p.y+fTranslate.Height then t:=p.y+fTranslate.PaintBox6.Top+(fTranslate.Height-fTranslate.CLientHeight)+cursorscreeny*lastxsiz*lastycnt-fHint.Height;
  fHint.Left:=l;
  fHint.Top:=t;
  if fSettings.CheckBox13.Checked then fHint.Height:=44 else fHint.Height:=22;
  fHint.Show;
  fHint.Invalidate;
  fTranslate.ListBox1.SetFocus;
  hinthide:=false;
end;

procedure TfUser.HideHint;
begin
  if not hinthide then fHint.Hide;
end;

procedure TfUser.PaintHint;
var kanjis:string;
    i:integer;
    cw,cwl:integer;
    curk:string;
    fs,fsl:integer;
    rect:TRect;
begin
  fHint.PaintBox1.Canvas.Brush.Color:=Col('Editor_HintBack');
  cw:=-1;
  kanjis:='';
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    if kanjis<>'' then kanjis:=kanjis+'3000';
    curk:=remexcl(copy(StringGrid1.Cells[1,i],2,length(StringGrid1.Cells[1,i])-1));
    if StringGrid1.Row=i then
    begin
      cw:=length(kanjis) div 4;
      cwl:=length(curk) div 4;
    end;
    kanjis:=kanjis+curk;
  end;
  fs:=18;
  fsl:=fHint.PaintBox1.Width div fs;
  while (length(kanjis) div 4)>fsl do
  begin
    if cw>1 then
    begin
      while copy(kanjis,1,4)<>'3000'do
      begin
        delete(kanjis,1,4);
        dec(cw,1);
      end;
      delete(kanjis,1,4);
      kanjis:='2026'+kanjis;
    end else
    begin
      while copy(kanjis,length(kanjis)-3,4)<>'3000'do delete(kanjis,length(kanjis)-3,4);
      delete(kanjis,length(kanjis)-3,4);
      kanjis:=kanjis+'2026';
    end;
  end;
//  fHint.PaintBox1.Canvas.Font.Style:=[];
  fHint.PaintBox1.Canvas.Font.Color:=Col('Editor_HintText');
  DrawUnicode(fHint.PaintBox1.Canvas,2,2,fs,copy(kanjis,1,cw*4),FontJapaneseGrid);
//  fHint.PaintBox1.Canvas.Font.Style:=[fsBold];
  fHint.PaintBox1.Canvas.Brush.Color:=Col('Editor_HintSelected');
  rect.Left:=2+cw*fs;
  rect.Top:=2;
  rect.Bottom:=fs+2;
  rect.Right:=2+cw*fs+cwl*fs;
  fHint.PaintBox1.Canvas.FillRect(rect);
  DrawUnicode(fHint.PaintBox1.Canvas,2+cw*fs,2,fs,copy(kanjis,cw*4+1,cwl*4),FontJapaneseGrid);
//  fHint.PaintBox1.Canvas.Font.Style:=[];
  fHint.PaintBox1.Canvas.Brush.Color:=Col('Editor_HintBack');
  DrawUnicode(fHint.PaintBox1.Canvas,2+cw*fs+cwl*fs,2,fs,copy(kanjis,cw*4+cwl*4+1,length(kanjis)-cwl*4-cw*4),FontJapaneseGrid);
  if fSettings.CheckBox13.Checked then
  begin
    fHint.PaintBox1.Canvas.Font.Name:=FontEnglish;
    fHint.PaintBox1.Canvas.Font.Height:=fs-4;
    rect.top:=fs+2;
    rect.left:=2;
    rect.right:=fHint.PaintBox1.Width-4;
    rect.bottom:=fs*2;
    fHint.PaintBox1.Canvas.TextRect(rect,2,fs+2,strip(remexcl(StringGrid1.Cells[2,curword])));
  end;
end;

procedure TfUser.FormDestroy(Sender: TObject);
begin
  linl.Free;
end;

procedure TfUser.MakeEditorBitmap;
begin
  if editorbitmap<>nil then editorbitmap.Free;
  editorbitmap:=TBitmap.Create;
  editorbitmap.Width:=fTranslate.PaintBox6.Width;
  editorbitmap.Height:=fTranslate.PaintBox6.Height;
  RenderText(rviewx,rviewy,editorbitmap.Canvas,2,2,fTranslate.PaintBox6.Width-4,fTranslate.PaintBox6.Height-4,linl,printl,lastxsiz,lastycnt,false,false);
  oldcurx:=-1;
  oldcury:=-1;
  oldblockfromx:=-1;
  PasteEditorBitmap;
  DrawCursor(false);
  DrawBlock;
end;

procedure TfUser.PasteEditorBitmap;
begin
  fTranslate.PaintBox6.Canvas.Draw(0,0,editorbitmap);
end;

procedure TfUser.StringGrid1DblClick(Sender: TObject);
begin
  if SpeedButton17.Enabled then SpeedButton17Click(sender);
end;

procedure TfUser.SetCurPos(x,y:integer);
var i,j:integer;
begin
  for i:=0 to (linl.Count div 3)-1 do
    if (GetLineAttr(i,1,linl)=y) and (GetLineAttr(i,0,linl)<=x) and (GetLineAttr(i,2,linl)+GetLineAttr(i,0,linl)>x) then
  begin
    curx:=x-GetLineAttr(i,0,linl);
    cury:=i;
  end;
end;

procedure TfUser.HandleWheel(down:boolean);
begin
  if not fTranslate.ScrollBar1.Enabled then exit;
  if down then inc(view) else dec(view);
  if (view>=0) and (view<=fTranslate.ScrollBar1.Max) then
  begin
    rviewx:=GetLineAttr(view,0,linl);
    rviewy:=GetLineAttr(view,1,linl);
  end else if view<0 then view:=0 else view:=fTranslate.ScrollBar1.Max;
  fTranslate.ScrollBar1.Position:=view;
  MakeEditorBitmap;
end;

procedure TfUser.CheckBox1Click(Sender: TObject);
begin
//  Look(false);
end;

procedure TfUser.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipGridOver(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfUser.DetailsForKanji(n:integer);
begin
  if CharDetDocked then exit;
  fKanji.SetCharDetails(copy(curkanjid,(n-1)*9+1,4));
  if not fKanjiDetails.Visible then fMenu.aKanjiDetails.Execute else fKanjiDetails.SetFocus;
end;

procedure TfUser.ChangeNotebook;
begin
{  if fWordAdd.SpeedButton1.Down then fWordAdd.Notebook1.ActivePage:='Examples';
  if fWordAdd.SpeedButton2.Down then fWordAdd.Notebook1.ActivePage:='Add';
  if fWordAdd.SpeedButton3.Down then fWordAdd.Notebook1.ActivePage:='Filters';
  fMenu.aDictVoc1.Checked:=fWordAdd.SpeedButton1.Down;
  fMenu.aDictVoc2.Checked:=fWordAdd.SpeedButton2.Down;
  fMenu.aDictVoc3.Checked:=fWordAdd.SpeedButton3.Down;}
  fWordAdd.Notebook1.ActivePage:='Examples';
end;

procedure TfUser.SetExamples(kanji:string);
var l,r,m,max:integer;
    s2:string;
    p:pchar;
    w:word;
    j:integer;
begin
  ex_indfirst:=-1;
  if kanji='' then
  begin
    ShowExample;
    exit;
  end;
  if examindex=nil then
  begin
    ShowExample;
    exit;
  end;
  while length(kanji)<24 do kanji:=kanji+'0000';
  if length(kanji)>24 then kanji:=copy(kanji,1,24);
  l:=0;
  max:=examindexsiz;
  r:=max;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    p:=examindex;
    p:=p+m*16;
    s2:='';
    for j:=1 to 6 do
    begin
      move(p^,w,2);
      p:=p+2;
      s2:=s2+Format('%4.4X',[w]);
    end;
    if s2=kanji then break;
    if s2<kanji then l:=m+1 else r:=m-1;
  end;
  if l>r then ex_indfirst:=-1 else
  begin
    p:=examindex;
    p:=p+m*16+12;
    move(p^,ex_indfirst,4);
    p:=p+16;
    if m<max then move(p^,ex_indlast,4) else ex_indlast:=examstructsiz;
    dec(ex_indlast);
  end;
  ex_indcur:=ex_indfirst;
  randbank.Clear;
  randbank.Add(inttostr(ex_indfirst+random(ex_indlast-ex_indfirst+1)));
  if fWordAdd.SpeedButton11.Down then ex_indcur:=strtoint(randbank[0]);
  ShowExample;
end;

procedure TfUser.ShowExample;
var p:pchar;
    ofs:integer;
    buf:array[0..1023] of byte;
    i,j,siz,siz2:integer;
    ms:string;
    pos:integer;
begin
  ex_jap:='';
  ex_en:='';
  if (examindex=nil) or (examstruct=nil) or (exampackage=nil) then
  begin
    if curlang='j'then
      ex_jap:=UnicodeToHex(' === '+_l('#00688^eExample database was not found. Download it from WaKan website.^cDatabáze pøíkladù nebyla nalezena. Stáhnìte ji ze stránky WaKanu.')) else
      ex_jap:=UnicodeToHex(' === '+_l('^eExamples are not available in Chinese mode.^cV režimu èínštiny nejsou pøíklady k dispozici.'));
    ex_indfirst:=-1;
  end
  else if ex_indfirst=-1 then ex_jap:=UnicodeToHex(' === '+_l('#00689^eNo examples available.^cŽádné pøíklady nejsou k dispozici.')) else
  begin
    p:=examstruct;
    p:=p+ex_indcur*4;
    move(p^,ofs,4);
    exampackage.ReadRawData(buf,integer(examfile.Position)+ofs,1024);
    siz:=buf[0];
    for i:=1 to siz do ex_jap:=ex_jap+Format('%2.2X%2.2X',[buf[i*2],buf[i*2-1]]);
    siz2:=buf[siz*2+1];
    for j:=siz*2+2 to siz*2+1+siz2 do ex_en:=ex_en+chr(buf[j]);
  end;
  if ex_indfirst=-1 then ms:='0'else if (ex_indlast-ex_indfirst)<99 then ms:=inttostr(ex_indlast-ex_indfirst+1) else ms:='lot';
  pos:=0;
  if ex_indfirst=-1 then fWordAdd.Label2.Caption:='0/0'else
  begin
    if not fWordAdd.SpeedButton11.Down then pos:=ex_indcur-ex_indfirst else pos:=randbank.IndexOf(inttostr(ex_indcur));
    fWordAdd.Label2.Caption:=inttostr(pos+1)+'/'+ms;
  end;
  if ex_indfirst=-1 then fWordAdd.Label6.Caption:='-'else fWordAdd.Label6.Caption:=inttostr(ex_indcur-ex_indfirst+1);
  fWordAdd.SpeedButton7.Enabled:=(ex_indfirst>-1) and (pos>0);
  fWordAdd.SpeedButton8.Enabled:=(ex_indfirst>-1) and (pos<ex_indlast-ex_indfirst);
  fWordAdd.SpeedButton9.Enabled:=(ex_indfirst>-1);
  fWordAdd.SpeedButton10.Enabled:=(ex_indfirst>-1);
  fWordAdd.PaintBox3.Invalidate;
end;

procedure TfUser.PaintExample;
begin
  fWordAdd.PaintBox3.Canvas.Brush.Color:=clWindow;
  fWordAdd.PaintBox3.Canvas.Font.Style:=[];
  BeginDrawReg(fWordAdd.PaintBox3);
  if fWordAdd.SpeedButton4.Down then DrawUnicode(fWordAdd.PaintBox3.Canvas,3,3,16,ex_jap,FontSmall);
  if fWordAdd.SpeedButton5.Down then DrawUnicode(fWordAdd.PaintBox3.Canvas,3,15,16,ex_jap,FontSmall);
  if fWordAdd.SpeedButton6.Down then DrawUnicode(fWordAdd.PaintBox3.Canvas,3,5,24,ex_jap,FontJapanese);
  EndDrawReg;
  if fWordAdd.SpeedButton4.Down then DrawUnicode(fWordAdd.PaintBox3.Canvas,3,22,16,UnicodeToHex(ex_en),FontEnglish);
end;

procedure TfUser.MoveExample(right:boolean);
var pos:integer;
    a:integer;
begin
  if not fWordAdd.SpeedButton11.Down then
    if right then inc(ex_indcur) else dec(ex_indcur);
  if fWordAdd.SpeedButton11.Down then
  begin
    pos:=randbank.IndexOf(inttostr(ex_indcur));
    if not right then dec(pos) else inc(pos);
    if (pos>=randBank.Count) or (pos<0) then
    begin
      a:=random(ex_indlast-ex_indfirst+1-randbank.Count)+1;
      ex_indcur:=ex_indfirst-1;
      while a>0 do
      begin
        inc(ex_indcur);
        while (randbank.IndexOf(inttostr(ex_indcur))>-1) do inc(ex_indcur);
        dec(a);
      end;
      randbank.Add(inttostr(ex_indcur));
    end else ex_indcur:=strtoint(randbank[pos]);
  end;
  ShowExample;
end;

procedure TfUser.RandomExample;
begin
  ex_indcur:=ex_indfirst+random(ex_indlast-ex_indfirst+1);
  ShowExample;
end;

procedure TfUser.ExampleClipboard(all:boolean);
var i:integer;
    p:pchar;
    ofs:integer;
    buf:array[0..1023] of byte;
    j,siz,siz2:integer;
    max:integer;
begin
  if not all then
  begin
    if fWordAdd.SpeedButton4.Down then clip:=ex_jap+'000D000A'+UnicodeToHex(ex_en)
    else clip:=ex_jap;
  end else
  begin
    clip:='';
    if ex_indlast-ex_indfirst>99 then
    begin
      max:=ex_indfirst+99;
      Application.MessageBox(pchar(_l('^eThere are too many examples. Only first hundred have been copied.^cPøíkladù je pøíliš mnoho. Pouze prvních sto bylo zkopírováno.')),
        pchar(_l('#00364^eNotice^cUpozornìní')),MB_ICONINFORMATION or MB_OK);
    end else max:=ex_indlast;
    for i:=ex_indfirst to max do
    begin
      p:=examstruct;
      p:=p+i*4;
      move(p^,ofs,4);
      exampackage.ReadRawData(buf,integer(examfile.Position)+ofs,1024);
      siz:=buf[0];
      for j:=1 to siz do clip:=clip+Format('%2.2X%2.2X',[buf[j*2],buf[j*2-1]]);
      siz2:=buf[siz*2+1];
      clip:=clip+'000D000A';
      if fWordAdd.SpeedButton4.Down then for j:=siz*2+2 to siz*2+1+siz2 do clip:=clip+UnicodeToHex(chr(buf[j]));
      if fWordAdd.SpeedButton4.Down then clip:=clip+'000D000A';
    end;
  end;
  fMenu.ChangeClipboard;
end;

procedure TfUser.GotoExample(num:integer);
var pos:integer;
begin
  if num>ex_indlast-ex_indfirst then exit;
  if ex_indfirst=-1 then exit;
  if num<0 then exit;
  if not fWordAdd.SpeedButton11.Down then
    ex_indcur:=ex_indfirst+num-1 else
  if fWordAdd.SpeedButton11.Down then
  begin
    ex_indcur:=ex_indfirst+num-1;
    pos:=randbank.IndexOf(inttostr(num));
    if pos=-1 then randbank.Add(inttostr(ex_indfirst+num-1));
  end;
  ShowExample;
end;

procedure TfUser.SpeedButton17Click(Sender: TObject);
begin
  if fDicAdd.ShowModal=mrOK then
  begin
    if not fWords.AddWord(curkanji,curphonetic,fDicAdd.edit3.text,fDicAdd.ComboBox1.Text,'?',false,1) then exit;
    Look(false);
    if Edit1.Enabled then Edit1.SetFocus;
  end;
end;

procedure TfUser.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

function TfUser.HalfWidthChar(c:string):boolean;
begin
  result:=(c[1]='0') and (c[2]='0');
end;

function TfUser.HalfWidth(x,y:integer):boolean;
begin
  result:=HalfWidthChar(GetDoc(x,y));
end;

procedure TfUser.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

function TfUser.PosToWidth(x,y:integer):integer;
var i,js,cx,cy:integer;
begin
  if (x<0) or (y<0) or (y>=linl.Count div 3) then
  begin
    result:=-1;
    exit;
  end;
  cx:=GetLineAttr(y,0,linl);
  cy:=GetLineAttr(y,1,linl);
  result:=0;
  for i:=0 to x-1 do
  begin
    if HalfWidth(cx,cy) then inc(result) else inc(result,2);
    inc(cx);
  end;
end;

function TfUser.WidthToPos(x,y:integer):integer;
var i,jx,cx,cy:integer;
begin
  if (x<0) or (y<0) or (y>=linl.Count div 3) then
  begin
    result:=-1;
    exit;
  end;
  cx:=GetLineAttr(y,0,linl);
  cy:=GetLineAttr(y,1,linl);
  jx:=0;
  for i:=0 to x-1 do
  begin
    if HalfWidth(cx,cy) then inc(jx) else inc(jx,2);
    if jx>=x then
    begin
      if jx=x then result:=i+1 else result:=i;
      exit;
    end;
    inc(cx);
  end;
  result:=0;
end;

procedure TfUser.SpeedButton10Click(Sender: TObject);
begin
  if not donotsetbegset then
  begin
    if fUser.SpeedButton10.Down then dictbeginset:=0;
    if fUser.SpeedButton11.Down then dictbeginset:=1;
    if fUser.SpeedButton12.Down then dictbeginset:=2;
    if fUser.SpeedButton18.Down then dictbeginset:=3;
  end;
  Look(false);
  if Edit1.Enabled then Edit1.SetFocus;
end;

procedure TfUser.SpeedButton19Click(Sender: TObject);
begin
  fMenu.aMode5Execute(sender);
  if copy(ul[curword-1],1,6)<>'000000'then fWords.SearchWord(strtoint(copy(ul[curword-1],1,6)));
end;

initialization
  EditorInit;
  editorbitmap:=nil;

finalization
  ul.Free;

end.
