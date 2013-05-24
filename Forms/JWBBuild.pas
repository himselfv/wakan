unit JWBBuild;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DBTables, ArtLabel;

type
  TForm1 = class(TForm)
    dsKanjiDic: TDataSource;
    TKanjiDic: TTable;
    TKanjiDicKey: TAutoIncField;
    TKanjiDicIndex: TIntegerField;
    TKanjiDicChinese: TSmallintField;
    TKanjiDicType: TStringField;
    TKanjiDicUnicode: TStringField;
    TKanjiDicStrokeCount: TIntegerField;
    TKanjiDicJpStrokeCount: TIntegerField;
    TKanjiDicJpFrequency: TIntegerField;
    TKanjiDicChFrequency: TIntegerField;
    TKanjiDicJouyouGrade: TIntegerField;
    TKanjiRead: TTable;
    TKanjiReadKey: TAutoIncField;
    TKanjiReadIndex: TIntegerField;
    TKanjiReadKanji: TIntegerField;
    TKanjiReadType: TSmallintField;
    TKanjiReadReading: TStringField;
    TKanjiReadReadDot: TIntegerField;
    TKanjiReadPosition: TSmallintField;
    TUser: TTable;
    TUserKey: TAutoIncField;
    TUserIndex: TIntegerField;
    TUserEnglish: TStringField;
    TUserPhonetic: TStringField;
    TUserKanji: TStringField;
    TUserAdded: TDateField;
    TUserPrinted: TDateField;
    TUserLearned: TDateField;
    TUserMastered: TDateField;
    TUserLevelAdded: TSmallintField;
    TUserLevelLearned: TSmallintField;
    TUserLevelMastered: TSmallintField;
    TUserNoPrinted: TIntegerField;
    TUserSumScore: TIntegerField;
    TUserScore: TSmallintField;
    TUserNoDescend: TIntegerField;
    TUserMaxScore: TSmallintField;
    TUserSheetNo: TIntegerField;
    TUserSheetOrder: TIntegerField;
    TEdictIdx: TTable;
    TEdictIdxKey: TAutoIncField;
    TEdictIdxWord: TIntegerField;
    TEdictIdxKanji: TIntegerField;
    TEdictIdxBegin: TBooleanField;
    TUserIdx: TTable;
    TUserIdxKey: TAutoIncField;
    TUserIdxWord: TIntegerField;
    TUserIdxKanji: TIntegerField;
    TUserIdxBegin: TBooleanField;
    TKanjiReadRef: TTable;
    TKanjiReadRefKey: TAutoIncField;
    TKanjiReadRefKanji: TIntegerField;
    TKanjiReadRefType: TSmallintField;
    TKanjiReadRefReading: TStringField;
    TUserIdxRef: TTable;
    TUserIdxRefKey: TAutoIncField;
    TUserIdxRefWord: TIntegerField;
    TUserIdxRefKanji: TIntegerField;
    TUserIdxRefBegin: TBooleanField;
    TEdictWord: TTable;
    TEdictWordKey: TAutoIncField;
    TEdictWordWord: TStringField;
    TEdictWordEntry: TIntegerField;
    TEdictWordBegin: TBooleanField;
    TRadical: TTable;
    TRadicalKey: TAutoIncField;
    TRadicalNumber: TIntegerField;
    TRadicalVariant: TIntegerField;
    TRadicalUnicode: TStringField;
    TRadicalStrokeCount: TIntegerField;
    TRadicalBushuCount: TIntegerField;
    TRadicalUnicodeCount: TIntegerField;
    TRadicalJapaneseCount: TIntegerField;
    TRadicalKangXiCount: TIntegerField;
    Button5: TButton;
    Button1: TButton;
    TEdictSort: TStringField;
    TEdictIdxIndex: TIntegerField;
    TEdictWordIndex: TIntegerField;
    ArtLabel1: TArtLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Button6: TButton;
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  end;

const StructVer=7;

var
  Form1: TForm1;

implementation

uses MultiLang, StdPrompt, PKGWrite, JWBBuildSet, TextTable, MemSource;

var roma:TStringList;
    maxchinese:integer;

{$R *.DFM}

procedure ConvUniToMixUni(inpf,outf:string;var recn:integer);
var f:file of byte;
    t:textfile;
    bl,bh:byte;
    s:string;
    inuni:boolean;
begin
  recn:=0;
  assignfile(f,inpf);
  reset(f);
  assignfile(t,outf);
  rewrite(t);
  s:='';
  inuni:=false;
  while not eof(f) do
  begin
    read(f,bl);
    read(f,bh);
    if (bh=0) and (bl=10) then
    begin
      if inuni then s:=s+'>';
      writeln(t,s);
      s:='';
      inuni:=false;
      inc(recn);
    end;
    if (bh=0) and (bl<>10) and (bl<>13) then
    begin
      if inuni then s:=s+'>';
      inuni:=false;
      s:=s+chr(bl);
    end;
    if (bh<>0) then
    begin
      if not inuni then s:=s+'<';
      inuni:=true;
      s:=s+Format('%2.2X%2.2X',[bh,bl]);
    end;
  end;
  if s<>'' then writeln(t,s);
  closefile(f);
  closefile(t);
end;

function split(var s:string;c:char):string;
var s2:string;
begin
  if pos(c,s)=0 then
  begin
    result:='';
    exit;
  end;
  result:=copy(s,1,pos(c,s)-1);
  delete(s,1,pos(c,s));
end;

function GetDelim(s:string;i:integer;en:boolean):string;
var cur:integer;
begin
  cur:=1;
  while cur<i do
  begin
    delete(s,1,pos(',',s));
    inc(cur);
  end;
  if not en then delete(s,pos(',',s),length(s)-pos(',',s)+1);
  result:=s;
end;

procedure TForm1.Button5Click(Sender: TObject);
var t:textfile;
    s,s2,s3,s4:string;
    sp:string;
    i,j,k,rj:integer;
    t1,t2:boolean;
    dicn,kann,hann:integer;
    pro:TSMPromptForm;
    sl:TStringList;
    beg:boolean;
    rad:string;
    com:TStringList;
    unihan:boolean;
    uni:string;
    f:file of byte;
    b:byte;
    dl:string;
    readfirstw,readfirsty,readfirstk,readfirsto,readfirst:integer;
    readl:array[1..255] of integer;
    cfgl,radl:TStringList;
    tpe:integer;
    tpt:string;
    lite:boolean;
begin
  cfgl:=TStringList.Create;
  cfgl.LoadFromFile('wakan.cfg');
  while cfgl[0]<>'[CharInfo]'do cfgl.Delete(0);
  while cfgl[0][1]=';'do cfgl.Delete(0);

  if FileExists('UNIHAN.TXT') then unihan:=true else unihan:=false;

  lite:=SMYesNo(false,'Question','Build LITE database?');

  pro:=SMProgressDlg(_l('#00012^eDictionary generation^cGenerování slovníku'),'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',1280);
  Screen.Cursor:=crHourGlass;
  dicn:=100000;
  kann:=6000;
  hann:=50000;

  pro.SetMessage(_l('#00014^eConverting KANJIDIC.UNI...^cKonvertuji KANJIDIC.UNI...'));
  pro.SetProgress(20);
  ConvUniToMixUni('kanjidic.uni','kanjidic.cnv',kann);

  if unihan then
  begin
    pro.SetMessage(_l('#00015^eConverting UNIHAN.TXT...^cKonvertuji UNIHAN.TXT...'));
    pro.SetProgress(30);
    hann:=0;
    s:='';
    assignfile(f,'unihan.txt');
    reset(f);
    assignfile(t,'unihan.cnv');
    rewrite(t);
    while not eof(f) do
    begin
      read(f,b);
      if b<>10 then s:=s+chr(b) else
      begin
        writeln(t,s);
        inc(hann);
        s:='';
      end;
    end;
    closefile(f);
    closefile(t);
  end;

  pro.SetMessage(_l('#00016^eEmptying tables...^cVyprazdòuji tabulky...'));
  pro.SetProgress(75);
  TKanjiRead.MasterSource:=nil;
  TEdictIdx.MasterSource:=nil;
  TUserIdx.MasterSource:=nil;
  TKanjiDic.Close;
  TKanjiRead.Close;
  TEdictIdx.Close;
  TUserIdx.Close;
  TEdictWord.Close;
  TRadical.Close;
  TKanjiDic.EmptyTable;
  TKanjiRead.EmptyTable;
  TEdictIdx.EmptyTable;
  TUserIdx.EmptyTable;
  TEdictWord.EmptyTable;
//  if unihan then TRadical.EmptyTable;
  TEdict.Open;
  TKanjiDic.Open;
  TKanjiRead.Open;
  TEdictIdx.Open;
  TUserIdx.Open;
  TEdictWord.Open;
  TRadical.Open;
  
  pro.SetMessage(_l('#00021^eParsing KanjiDic...^cGeneruji KanjiDic...'));
  pro.SetProgress(4);
  radl:=TStringList.Create;  
  assignfile(t,'kanjidic.cnv');
  reset(t);
  readln(t,s);
  i:=0;
  rj:=0;
  sl:=TStringList.Create;
  while (not eof(t)) do
  begin
    if i mod 100=0 then pro.SetMessage(_l('#00022^eParsing KanjiDic (^cGeneruji KanjiDic (')+inttostr(trunc(i/kann*100))+'%)...');
    if i mod 100=0 then pro.SetProgress(480+trunc(i/kann*100));
    inc(i);
    readln(t,s);
    t1:=false;
    t2:=false;
    delete(s,1,1);
    TKanjiDic.Insert;
    TKanjiDicStrokeCount.AsInteger:=255;
    TKanjiDicJpStrokeCount.AsInteger:=255;
    TKanjiDicJpFrequency.AsInteger:=65535;
    TKanjiDicChFrequency.AsInteger:=255;
    TKanjiDicJouyouGrade.AsInteger:=255;
    TKanjiDicChinese.AsInteger:=0;
    TKanjiDicType.AsString:='J';
    TKanjiDicIndex.AsInteger:=i;
    TKanjiDicUnicode.AsString:=split(s,'>');
    sl.Add(UpperCase(TKanjiDicUnicode.AsString));
    delete(s,1,1);
    split(s,' ');
    readfirst:=1;
    readfirstw:=1;
    readfirsty:=1;
    readfirstk:=1;
    readfirsto:=1;
    for j:=1 to 255 do readl[j]:=1;
    repeat
      if s[1]='{'then
      begin
        sp:=split(s,'}');
        if length(s)>0 then delete(s,1,1);
      end else sp:=split(s,' ');
      if sp='' then
      begin
        sp:=s;
        s:='';
      end;
      if sp<>'' then case sp[1] of
        '<','-':begin
              TKanjiRead.Insert;
              inc(rj);
              TKanjiReadIndex.AsInteger:=rj;
              TKanjiReadKanji.AsInteger:=i;
              if sp[1]='-'then delete(sp,2,1) else delete(sp,1,1);
              if sp[length(sp)]='-'then delete(sp,length(sp)-1,1) else
              delete(sp,length(sp),1);
              TKanjiReadReadDot.AsInteger:=0;
              if t2 then sp:='+'+sp;
              if pos('.',sp)>0 then
              begin
              	//delete > and < around .
                delete(sp,pos('.',sp)-1,1);
                delete(sp,pos('.',sp)+1,1);
                TKanjiReadReadDot.AsInteger:=pos('.',sp);
                delete(sp,pos('.',sp),1);
              end;
              TKanjiReadReading.AsString:=sp;
              if t1 then TKanjiReadType.AsInteger:=6 else
              if sp[3]>='A'then TKanjiReadType.AsInteger:=4 else
              TKanjiReadType.AsInteger:=5;
              if sp[3]>='A'then
              begin
                TKanjiReadPosition.AsInteger:=readfirsto;
                inc(readfirsto);
              end else
              begin
                TKanjiReadPosition.AsInteger:=readfirstk;
                inc(readfirstk);
              end;
              TKanjiRead.Post;
            end;
        'T':if sp[2]='1'then t1:=true else if sp[2]='2'then t2:=true;
        '{':begin
              delete(sp,1,1);
              TKanjiRead.Insert;
              inc(rj);
              TKanjiReadIndex.AsInteger:=rj;
              TKanjiReadKanji.AsInteger:=i;
              TKanjiReadType.AsInteger:=3;
              TKanjiReadReading.AsString:=sp;
              TKanjiReadPosition.AsInteger:=readfirst;
              inc(readfirst);
              TKanjiRead.Post;
            end;
        else
        begin
          dl:=sp[1];
          delete(sp,1,1);
          if (length(sp)>0) and (sp[1]>='A') and (sp[1]<='Z') then
          begin
            dl:=dl+sp[1];
            delete(sp,1,1);
          end;
          for j:=0 to cfgl.Count-1 do if (GetDelim(cfgl[j],2,false)='D') and (pos('+'+dl+'+','+'+GetDelim(cfgl[j],3,false)+'+')>0) then
          if (not lite) or (strtoint(GetDelim(cfgl[j],1,false))<50) then
          begin
            TKanjiRead.Insert;
            inc(rj);
            TKanjiReadIndex.AsInteger:=rj;
            TKanjiReadKanji.AsInteger:=i;
            TKanjiReadType.AsInteger:=strtoint(GetDelim(cfgl[j],1,false));
            TKanjiReadReading.AsString:=sp;
            TKanjiReadPosition.AsInteger:=readl[strtoint(GetDelim(cfgl[j],1,false))];
            inc(readl[strtoint(GetDelim(cfgl[j],1,false))]);
            if (GetDelim(cfgl[j],4,false)='R') then
              if radl.IndexOf(inttostr(i)+'-'+sp)=-1 then
                radl.Add(inttostr(i)+'-'+sp);
          end;
          if dl='F'then TKanjiDicJpFrequency.AsInteger:=strtoint(sp);
          if (dl='S') and (TKanjiDicJpStrokeCount.AsInteger=255) then TKanjiDicJpStrokeCount.AsInteger:=strtoint(sp);
          if dl='G'then TKanjiDicJouyouGrade.AsInteger:=strtoint(sp);
        end;
      end;
    until s='';
    TKanjiDic.Post;
  end;
  closefile(t);
  kann:=i;
  
  pro.SetMessage(_l('#00023^eParsing UniHan...^cGeneruji UniHan...'));
  pro.SetProgress(530);
  TKanjiDic.IndexName:='ChUnicode_Ind';
  uni:='';
  if unihan then
  begin
    assignfile(t,'unihan.cnv');
    reset(t);
    k:=0;
    while (not eof(t)) do
    begin
      if k mod 100=0 then pro.SetMessage(_l('#00024^eParsing UniHan (^cGeneruji UniHan (')+inttostr(trunc(k/hann*100))+'%)...');
      if k mod 100=0 then pro.SetProgress(580+trunc(k/hann*600));
      inc(k);
      readln(t,s);
      if (length(s)<=0) or (s[1]<>'U') then continue;

      delete(s,1,2);
      s2:=split(s,#9);
      s3:=split(s,#9);
      if (s2[1]>'9') or (s2[1]<'4') or ((s2[1]='4') and (s2[2]<'E')) then continue;

      if s2<>uni then
      begin
        if uni<>'' then
          TKanjiDic.Post;
        if sl.IndexOf(UpperCase(s2))=-1 then
        begin
          inc(i);
          TKanjiDic.Insert;
          TKanjiDicStrokeCount.AsInteger:=255;
          TKanjiDicJpFrequency.AsInteger:=65535;
          TKanjiDicChFrequency.AsInteger:=255;
          TKanjiDicChinese.AsInteger:=1;
          TKanjiDicType.AsString:='N';
          TKanjiDicIndex.AsInteger:=i;
          TKanjiDicUnicode.AsString:=s2;
          kann:=i;
        end else
        begin
          TKanjiDic.Locate('Unicode',s2,[loCaseInsensitive]);
          TKanjiDic.Edit;
          TKanjiDicType.AsString:='N';
        end;
        uni:=s2;
      end;
      tpe:=0;
      for j:=0 to cfgl.Count-1 do if (GetDelim(cfgl[j],2,false)='U') and (pos('+'+s3+'+','+'+GetDelim(cfgl[j],3,false)+'+')>0) then
      begin
        tpe:=strtoint(GetDelim(cfgl[j],1,false));
        tpt:=GetDelim(cfgl[j],4,false);
      end;
      if s3='kFrequency'then TKanjiDicChFrequency.AsInteger:=strtoint(s);
      if s3='kTotalStrokes'then TKanjiDicStrokeCount.AsInteger:=strtoint(s);
      if s3='kBigFive'then if TKanjiDicType.AsString='N'then TKanjiDicType.AsString:='T'else TKanjiDicType.AsString:='A';
      if s3='kGB0'then if TKanjiDicType.AsString='N'then TKanjiDicType.AsString:='S'else TKanjiDicType.AsString:='A';
      if (s3='kCantonese') or (s3='kMandarin') or (s3='kDefinition') or (s3='kKorean') or ((tpe<>0) and ((not lite) or (tpe<50))) then
      begin
        readfirst:=0;
        while s<>'' do
        begin
          TKanjiRead.Insert;
          inc(rj);
          TKanjiReadIndex.AsInteger:=rj;
          TKanjiReadKanji.AsInteger:=TKanjiDicIndex.AsInteger;
          if s3='kCantonese'then TKanjiReadType.AsInteger:=8;
          if s3='kDefinition'then TKanjiReadType.AsInteger:=7;
          if s3='kMandarin'then TKanjiReadType.AsInteger:=2;
          if s3='kKorean'then TKanjiReadType.AsInteger:=1;
          if tpe<>0 then TKanjiReadType.AsInteger:=tpe;
          s4:='';
          while (length(s)>0) do
          begin
            if (s3='kDefinition') and ((s[1]=',') or (s[1]=';')) then
            begin
              delete(s,1,1);
              if (length(s)>0) and (s[1]=' ') then delete(s,1,1);
              break;
            end;
            if (s3<>'kDefinition') and (s[1]=' ') then
            begin
              delete(s,1,1);
              break;
            end;
            s4:=s4+s[1];
            delete(s,1,1);
          end;
          if (length(s4)>2) and (s4[1]='U') and (s4[2]='+') then delete(s4,1,2);
          TKanjiReadReading.AsString:=s4;
          if pos('.',s4)>0 then delete(s4,pos('.',s4),length(s4)-pos('.',s4)+1);
          if tpt='R'then if radl.IndexOf(TKanjiDicIndex.AsString+'-'+s4)=-1 then
            radl.Add(TKanjiDicIndex.AsString+'-'+s4);
          TKanjiReadPosition.AsInteger:=readfirst;
          inc(readfirst);
          TKanjiRead.Post;
        end;
      end;

    end;
  end;
  closefile(t);
  TKanjiDic.Post;

  pro.SetMessage(_l('^eCalculating radicals...^cKalkuluji radikály...'));
  pro.SetProgress(1180);
  for i:=0 to radl.Count-1 do
  begin
    TKanjiRead.Insert;
    inc(rj);
    s:=radl[i];
    s2:=copy(s,1,pos('-',s)-1);
    delete(s,1,pos('-',s));
    TKanjiReadIndex.AsInteger:=rj;
    TKanjiReadKanji.AsInteger:=strtoint(s2);
    TKanjiReadType.AsInteger:=10;
    TKanjiReadReading.AsString:=s;
    TKanjiReadPosition.AsInteger:=0;
    TKanjiRead.Post;
  end;

{  TKanjiDic.IndexName:='Unicode_Ind';
  TKanjiDic.First;
  i:=0;
  rad:='';
  com:=TStringList.Create;
  j:=-1;
  while not TKanjiDic.EOF do
  begin
    TKanjiDic.Edit;
    if TKanjiDicClassicalRadical.AsInteger<>i then
    begin
      if j<>-1 then
      begin
        com.Add(rad);
        com.Add(inttostr(j));
      end;
      TKanjiDicRadical.AsString:='X';
      j:=0;
      i:=TKanjiDicClassicalRadical.AsInteger;
      rad:=TKanjiDicUnicode.AsString;
    end else
    begin
      TKanjiDicRadical.AsString:=rad;
      if TKanjiDicJouyouGrade.AsInteger<=9 then inc(j);
    end;
    TKanjiDic.Post;
    TKanjiDic.Next;
  end;
  com.Add(rad);
  com.Add(inttostr(j));
  for i:=0 to (com.Count div 2)-1 do
  begin
    TKanjiDic.Locate('Unicode',com[i*2],[]);
    TKanjiDic.Edit;
    TKanjiDicRadical.AsString:='X'+com[i*2+1];
    TKanjiDic.Post;
  end;}

  pro.SetMessage(_l('^eCreating indexes...^cVytváøím indexy...'));
  pro.SetProgress(1220);
  TKanjiRead.MasterSource:=dsKanjiDic;
  TEdictIdx.MasterSource:=dsKanjiDic;
  TUserIdx.MasterSource:=dsKanjiDic;
  sl.Free;
  pro.Free;
  Screen.Cursor:=crDefault;
end;

procedure TForm1.Button1Click(Sender: TObject);
var pro:TSMPromptForm;
    fltchinese:boolean;
begin
  Screen.Cursor:=crHourGlass;
  if paramstr(1)='2'then fltchinese:=false else
    fltchinese:=Application.MessageBox('Build with chinese-only characters?','Chinese characters',MB_ICONINFORMATION or MB_YESNO)=idNo;
  pro:=SMProgressDlg('Building data files','XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',7);
  pro.SetMessage('Building table Radical...');
  pro.SetProgress(0);
//  SaveTableAsText(TRadical,'Number_Ind','bbxbwwww','Number;','','','Radicals',false);
  pro.SetMessage('Building table Dict...');
  pro.SetProgress(1);
//  SaveTableAsText(TEdict,'Index_Ind','isxxs','Index;Sort;Kanji;','Phonetic_Ind;Kanji_Ind;','Index','Dict',false);
  pro.SetMessage('Building table Char...');
  pro.SetProgress(2);
  maxchinese:=99999;
  TKanjiDic.First;
  while not TKanjiDic.EOF do
  begin
    if (TKanjiDic.FieldByName('Chinese').AsInteger=1) and (TKanjiDic.FieldByName('Index').AsInteger<maxchinese) then
      maxchinese:=TKanjiDic.FieldByName('Index').AsInteger;
    TKanjiDic.Next;
  end;
  SaveTableAsText(TKanjiDic,'Index_Ind','ibsxbwwbb',
    'Index;ChFrequency;StrokeCount;Unicode;JpFrequency;JpStrokeCount;;'
    ,'ChFrequency_Ind;ChStrokeCount_Ind;ChUnicode_Ind;'+
    'JpFrequency_Ind;JpStrokeCount_Ind;'+
    'JpUnicode_Ind;','Index','Char',fltchinese);
  pro.SetMessage('Building table CharRead...');
  pro.SetProgress(3);
  SaveTableAsText(TKanjiRead,'Main_Ind','iwbsbb','Kanji;Reading;Type;','Reading_Ind;Type_Ind;','Index','CharRead',fltchinese);
  pro.SetMessage('Building table DictIdx...');
  pro.SetProgress(4);
//  SaveTableAsText(TEdictIdx,'Main_Ind','iili','Kanji;','','Index','DictIdx',false);
  pro.SetMessage('Building table DictWord...');
  pro.SetProgress(5);
//  SaveTableAsText(TEdictWord,'Word_Ind','sili','Word;','','Index','DictWord',false);
  pro.SetMessage('Building table Romaji...');
  pro.SetProgress(6);
  pro.Free;
  screen.cursor:=crDefault;
  if paramstr(1)<>'2'then Application.MessageBox('Data files were generated.','Data files generation',MB_ICONINFORMATION or MB_OK);
end;

procedure TForm1.Button2Click(Sender: TObject);
var t:textfile;
begin
  if paramcount>2 then
  begin
    Form2.Edit2.Text:=paramstr(2);
    Form2.Edit1.Text:=paramstr(3);
  end;
  if paramstr(1)<>'3'then Form2.ShowModal;
  Screen.Cursor:=crHourGlass;
  assignfile(t,'build\jalet.ver');
  rewrite(t);
  writeln(t,'JALET.DIC');
  writeln(t,inttostr(structver));
  writeln(t,inttostr(trunc(now)));
  writeln(t,Form2.Edit3.Text);
  writeln(t,Form2.Edit2.Text);
  writeln(t,Form2.Edit1.Text);
  if Form2.CheckBox1.Checked then
    writeln(t,'CHINESE') else writeln(t,'NON-CHINESE');
  closefile(t);
  PKGWriteForm.PKGWriteCmd('PKGFileName wakan.chr');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan Character Dictionary');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan Character Dictionary Data File');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName (C) Filip Kábrt 2002-2003');
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by WaKan - Japanese & Chinese Learning Tool');
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 791564');
  PKGWriteForm.PKGWriteCmd('FileSysCode 978132');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  if Form2.CheckBox2.Checked then PKGWriteForm.PKGWriteCmd('CryptMode 2')
    else PKGWriteForm.PKGWriteCmd('CryptMode 0');
  if Form2.CheckBox3.Checked then PKGWriteForm.PKGWriteCmd('CRCMode 1')
    else PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode '+inttostr(Form2.RadioGroup1.ItemIndex));
  PKGWriteForm.PKGWriteCmd('CryptCode 978123');
  PKGWriteForm.PKGWriteCmd('Include build');
  PKGWriteForm.PKGWriteCmd('Finish');
  screen.cursor:=crDefault;
  if paramstr(1)<>'3'then Application.MessageBox('File "WAKAN.CHR" was successfully built.','Package build',MB_ICONINFORMATION or MB_OK);
end;

procedure TForm1.Button3Click(Sender: TObject);
var rn:integer;
    f,f2:file of byte;
    b,b2:byte;
    incc:boolean;
    t,t2:textfile;
    s,s2,s3,s4:string;
    i:integer;
    sl1,sl2:TStringList;
    storechar:char;
    conc:string;
    ttt:TTextTable;
    ps:TPackageSource;
begin
//  ConvUniToMixUni('build\bopomofo.txt','build\bopomofo.lst',rn);
{  assignfile(f,'wnn.dct');
  assignfile(f2,'wnn.cnn');
  reset(f);
  rewrite(f2);
  incc:=true;
  b2:=$a5;
  while not eof(f) do
  begin
    read(f,b);
    if b=9 then incc:=false;
    if b=10 then incc:=true;
    if (b>127) and (incc) then write(f2,b2);
    write(f2,b);
  end;
  closefile(f);
  closefile(f2);}
  COnvUniToMixUni('wnn.uni','wnn.cnv',rn);
  sl1:=TStringList.Create;
  sl2:=TStringList.Create;
  assignfile(t,'wnn.csv');
  rewrite(t);
  writeln(t,'>Kana;Kanji');
  assignfile(t2,'wnn.cnv');
  reset(t2);
  while not eof(t2) do
  begin
    readln(t2,s);
    if (length(s)>0) and (s[1]='<') then
    begin
      s2:=copy(s,1,pos(#9,s)-1);
      delete(s,1,pos(#9,s));
      storechar:=s[1];
      delete(s,1,pos(#9,s));
      delete(s2,1,1);
      delete(s2,length(s2),1);
      for i:=1 to length(s2) div 4 do s2[i*4-1]:=chr(ord(s2[i*4-1])-ord('A')+ord('4'));
      s3:='';
      conc:='';
      if storechar<>'*'then
      begin
        case storechar of
          'a':conc:='3042';
          'b':conc:='3076';
          'd':conc:='3065';
          'e':conc:='3048';
          'f':conc:='3075';
          'g':conc:='3050';
          'h':conc:='3075';
          'i':conc:='3044';
          'k':conc:='304F';
          'm':conc:='3080';
          'n':conc:='306C';
          'o':conc:='304A';
          'p':conc:='3077';
          'r':conc:='308B';
          's':conc:='3059';
          't':conc:='3064';
          'u':conc:='3046';
          'w':conc:='3046';
          'y':conc:='3086';
          'z':conc:='305A';
        end;
      end;
      while pos('/',s)>0 do
      begin
        s4:=copy(s,1,pos('/',s)-1);
        delete(s,1,pos('/',s));
        delete(s4,1,pos('<',s4));
        delete(s4,length(s4),1);
        s3:=s3+s4+conc+'0000';
      end;
      s4:=s;
      delete(s4,1,1);
      delete(s4,length(s4),1);
      s3:=s3+s4+conc;
      if storechar<>'*'then
      begin
        if sl1.IndexOf(s2+conc)>-1 then
        begin
          sl2[sl1.IndexOf(s2+conc)]:=sl2[sl1.IndexOf(s2+conc)]+'0000'+s3;
        end else
        begin
          sl1.Add(s2+conc);
          sl2.Add(s3);
        end;
      end else
      begin
        while sl1.IndexOf(s2)>-1 do
        begin
          s3:=s3+'0000'+sl2[sl1.IndexOf(s2)];
          sl2.Delete(sl1.IndexOf(s2));
          sl1.Delete(sl1.IndexOf(s2));
        end;
        if (pos('>',s2)=0) and (pos('<',s2)=0) then writeln(t,'+'+s2+';'+s3);
      end;
    end;
  end;
  for i:=0 to sl1.Count-1 do
  begin
    if (pos('>',sl1[i])=0) and (pos('<',sl1[i])=0) then writeln(t,'+'+sl1[i]+';'+sl2[i]);
  end;
  writeln(t,'.');
  closefile(t);
  closefile(t2);
  sl1.Free;
  sl2.Free;
  assignfile(t,'build\KanaKanji.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$FIELDS');
  writeln(t,'xKana');
  writeln(t,'xKanji');
  writeln(t,'$ORDERS');
  writeln(t,'Kana_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'0');
  writeln(t,'Kana');
  writeln(t,'$CREATE');
  closefile(t);
  PKGWriteForm.PKGWriteCmd('PKGFileName wakan.chr');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan Character Dictionary');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan Character Dictionary Data File');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName (C) Filip Kábrt 2002-2003');
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
  PKGWriteForm.PKGWriteCmd('Include build');
  PKGWriteForm.PKGWriteCmd('Finish');
  ps:=TPackageSource.Create('wakan.chr',791564,978132,978123);
  ttt:=TTextTable.Create(ps,'KanaKanji',false,false);
  assignfile(t,'wnn.csv');
  reset(t);
  ttt.ImportFromText(t,nil,'');
  closefile(t);
  ttt.WriteTable('build\KanaKanji',false);
  ps.Free;
  ttt.Free;
end;

procedure TForm1.Button6Click(Sender: TObject);
var f:file of word;
    w:word;
    w2:word;
    maxindex:integer;
begin
  assignfile(f,'jlpt.uni');
  reset(f);
  read(f,w);
  TKanjiRead.First;
  maxindex:=0;
  while not TKanjiRead.EOF do
  begin
    if TKanjiReadIndex.AsInteger>maxindex then maxindex:=TKanjiReadIndex.AsInteger;
    TKanjiRead.Next;
  end;
  TKanjiRead.First;
  while not eof(f) do
  begin
    read(f,w);
    read(f,w2);
    w2:=w2-ord('0');
    if not TKanjiDic.Locate('Unicode',Format('%4.4X',[w]),[]) then showmessage('Character not found!') else
    begin
      if not TKanjiRead.Locate('Kanji;Type',VarArrayOf([TKanjiDicIndex.AsInteger,35]),[]) then
      begin
        inc(maxindex);
        TKanjiRead.Insert;
        TKanjiReadIndex.AsInteger:=maxindex;
        TKanjiReadReading.AsString:=inttostr(w2);
        TKanjiReadType.AsInteger:=35;
        TKanjiReadPosition.AsInteger:=1;
        TKanjiReadKanji.AsInteger:=TKanjiDicIndex.AsInteger;
        TKanjiRead.Post;
      end;
    end;
    read(f,w);
    read(f,w);
  end;
  closefile(f);
end;

end.
