unit JWBMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, UnicodeEdit, Buttons, Db, DBTables, UnicodeMemo,
  UnicodeStringGrid, Gauges, UnicodeRichEdit, ExtCtrls;

type
  TfMain = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    UnicodeEdit1: TUnicodeEdit;
    Label3: TLabel;
    UnicodeEdit2: TUnicodeEdit;
    Label4: TLabel;
    Button1: TButton;
    Edit2: TEdit;
    Button2: TButton;
    Button3: TButton;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    Edit3: TEdit;
    Button4: TButton;
    Label6: TLabel;
    Tbl: TTable;
    GroupBox5: TGroupBox;
    UnicodeStringGrid1: TUnicodeStringGrid;
    TblKey: TAutoIncField;
    TblEnglish: TStringField;
    TblPhonetic: TStringField;
    TblKanji: TStringField;
    TblAdded: TDateField;
    TblPrinted: TDateField;
    TblLearned: TDateField;
    TblMastered: TDateField;
    TblLevelAdded: TSmallintField;
    TblLevelLearned: TSmallintField;
    TblLevelMastered: TSmallintField;
    TblNoPrinted: TIntegerField;
    TblSumScore: TIntegerField;
    TblScore: TSmallintField;
    TblNoDescend: TIntegerField;
    TblMaxScore: TSmallintField;
    TblSheetNo: TIntegerField;
    TblSheetOrder: TIntegerField;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Button5: TButton;
    Gauge1: TGauge;
    Gauge2: TGauge;
    UnicodeRichEdit1: TUnicodeRichEdit;
    Label14: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    GroupBox6: TGroupBox;
    Button6: TButton;
    RadioGroup1: TRadioGroup;
    GroupBox7: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Memo1: TMemo;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure UnicodeStringGrid1DblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button5Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;
  level:integer;

implementation

uses JWBReport, JWBRate, JWBTest;

{$R *.DFM}

var edf:file of byte;

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

function HexToUnicode(s:string):widestring;
var s2:widestring;
    d:word;
    c:widechar;
    i:integer;
begin
  s2:='';
  for i:=1 to length(s) div 4 do
  begin
    d:=StrToInt('0x'+copy(s,(i-1)*4+1,4));
    c:=widechar(d);
    s2:=s2+c;
  end;
  result:=s2;
end;

function ReadEdictLine:string;
var b1,b2:byte;
    s:string;
begin
  s:='';
  repeat
    read(edf,b1);
    read(edf,b2);
    if (b2=0) and (b1<>10) and (b1<>13) then s:=s+chr(b1);
    if (b2<>0) then s:=s+format('%2.2X%2.2X',[b2,b1]);
  until (b2=0) and (b1=10);
  result:=s;
end;


procedure TfMain.Button1Click(Sender: TObject);
var b:byte;
    s:string;
    sl:TStringList;
    l:boolean;
    i:integer;
    sp4,sp1,sp2,sp3:string;
    t:textfile;
    us:widestring;
    cs:string;
    flt:boolean;
    sts:string;
begin
  sl:=TStringList.Create;
  Screen.Cursor:=crHourGlass;
  if (length(UnicodeEdit1.WideText)=0) and (length(UnicodeEdit2.WideText)=0) and (length(Edit2.Text)=0) then
  begin
    cs:='User dictionary';
    case RadioGroup1.ItemIndex of
      0:tbl.IndexName:='Alphabet_Ind';
      1:tbl.IndexName:='Kanji_Ind';
      2:tbl.IndexName:='English_Ind';
      3:tbl.IndexName:='Main_Ind';
      4:tbl.IndexName:='Status_Ind';
    end;
    tbl.First;
    UnicodeRichEdit1.WideLines.Clear;
    UnicodeRichEdit1.WideLines.Add('JWB USER DICTIONARY');
    UnicodeRichEdit1.WideLines.Add('');
    while not tbl.Eof do
    begin
      flt:=true;
      if (not CheckBox2.checked) and (tblScore.AsInteger<1) then flt:=false;
      if (not CheckBox3.checked) and (tblScore.AsInteger>0) then flt:=false;
      if (not CheckBox4.Checked) and (tblScore.AsInteger>2) then flt:=false;
      if (not CheckBox5.Checked) and (tblLevelAdded.AsInteger<>level) then flt:=false;
      if (not CheckBox6.Checked) and (tblNoPrinted.AsInteger=0) then flt:=false;
      if (not CheckBox7.Checked) and (tblNoPrinted.AsInteger>0) then flt:=false;
      if flt then
      begin
        case tblScore.AsInteger of
          -1:sts:='N-';
          0:sts:='N.';
          1:sts:='L.';
          2:sts:='L+';
          3:sts:='M.';
          4:sts:='M+';
        end;
        sts:=sts+' Lv:'+tblLevelAdded.AsString+'/'+tblLevelLearned.AsString+
        '/'+tblLevelMastered.AsString+' Pr:'+tblNoPrinted.AsString+' Ds:'+tblNoDescend.AsString+' Mx:'+tblMaxScore.AsString+
        ' Ad:'+tblAdded.AsString;
        us:='';
        sl.Add(tblKanji.AsString+' ['+tblPhonetic.AsString+'] /'+tblEnglish.AsString+'/'+sts);
        us:=us+HexToUnicode(tblPhonetic.AsString);
        for i:=length(tblPhonetic.AsString) div 2 to 22 do us:=us+' ';
        us:=us+' ';
        us:=us+HexToUnicode(tblKanji.AsString);
        for i:=length(tblKanji.AsString) div 2 to 22 do us:=us+' ';
        us:=us+' ';
        if length(tblEnglish.AsString)>33 then
        us:=us+copy(tblEnglish.AsString,1,30)+'...'else us:=us+tblEnglish.AsString;
        UnicodeRichEdit1.WideLines.Add(us);
      end;
      tbl.Next;
    end;
    UnicodeRichEdit1.WideLines.SaveToFile('userdict.txt');
    if sl.Count=0 then
    begin
      sl.Free;
      showmessage('No words in user dictionary with current filter.');
      Screen.Cursor:=crDefault;
      exit;
    end;
  end else
  begin
    cs:='EDICT search results';
    assignfile(t,'edict.dat');
    reset(t);
    while not eof(t) do
    begin
      readln(t,s);
      l:=false;
      if (length(UnicodeEdit1.WideText)>0) and (pos(UnicodeToHex(UnicodeEdit1.WideText)+' ',s)=1) then l:=true;
      if (length(UnicodeEdit2.WideText)>0) and (pos(UnicodeToHex(UnicodeEdit2.WideText)+' ',s)=1) then l:=true;
      if (length(UnicodeEdit2.WideText)>0) and (pos('['+UnicodeToHex(UnicodeEdit2.WideText)+']',s)<>0) then l:=true;
      if (length(Edit2.Text)>0) and (pos(Edit2.Text,s)>0) then l:=true;
      if l then sl.Add(s+'EDICT');
    end;
    if sl.Count=0 then
    begin
      sl.Free;
      showmessage('No match found.');
      Screen.Cursor:=crDefault;
      exit;
    end;
    closefile(t);
  end;
  UnicodeStringGrid1.Visible:=true;
  UnicodeStringGrid1.RowCount:=sl.Count+1;
  UnicodeStringGrid1.FixedRows:=1;
  UnicodeStringGrid1.Cells[0,0]:='Phonetic';
  UnicodeStringGrid1.Cells[1,0]:='Written';
  UnicodeStringGrid1.Cells[2,0]:='English';
  UnicodeStringGrid1.Cells[3,0]:='Status';
  for i:=0 to sl.Count-1 do
  begin
    sp2:='';
    sp1:='';
    sp3:='';
    s:=sl[i];
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
    sp4:=copy(s,1,pos('/',s)-1);
    delete(sp3,1,length(sp4)+1);
    UnicodeStringGrid1.Cells[0,i+1]:=HexToUnicode(sp2);
    UnicodeStringGrid1.Cells[1,i+1]:=HexToUnicode(sp1);
    UnicodeStringGrid1.Cells[2,i+1]:=sp4;
    UnicodeStringGrid1.Cells[3,i+1]:=sp3;
  end;
  Screen.Cursor:=crDefault;
  GroupBox5.Caption:=cs+' ('+inttostr(sl.Count)+' entries)';
  sl.Free;
end;

procedure TfMain.UnicodeStringGrid1DblClick(Sender: TObject);
begin
  UnicodeEdit1.WideText:=UnicodeStringGrid1.Cells[0,UnicodeStringGrid1.Selection.Top];
  UnicodeEdit2.WideText:=UnicodeStringGrid1.Cells[1,UnicodeStringGrid1.Selection.Top];
  Edit2.Text:=UnicodeStringGrid1.Cells[2,UnicodeStringGrid1.Selection.Top];
  UnicodeEdit1.Font.Name:='MS Mincho';
  UnicodeEdit2.Font.Name:='MS Mincho';
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  tbl.Open;
  Button1Click(self);
  Button5Click(self);
end;

procedure TfMain.Button3Click(Sender: TObject);
var scr:integer;
begin
  scr:=0;
  if Edit2.Text='' then exit;
  if tbl.Locate('Kanji',UnicodeToHex(UnicodeEdit1.WideText),[]) then
  begin
    scr:=tblMaxScore.AsInteger;
    if Application.MessageBox(pchar('Word already added ("'+tblEnglish.AsString+'").'#13'Do you want still to add it?'),'Word already exists',MB_YESNO or MB_ICONWARNING)=IDNO then exit;
  end else if Application.MessageBox(pchar('Really add?'),'Warning',MB_YESNO or MB_ICONWARNING)=IDNO then exit;
  tbl.Insert;
  tblEnglish.AsString:=Edit2.Text;
  tblKanji.AsString:=UnicodeToHex(UnicodeEdit1.WideText);
  tblPhonetic.AsString:=UnicodeToHex(UnicodeEdit2.WideText);
  tblAdded.AsDateTime:=now;
  tblPrinted.Clear;
  tblLearned.Clear;
  tblMastered.Clear;
  tblLevelAdded.AsInteger:=level;
  tblLevelLearned.Clear;
  tblLevelMastered.Clear;
  tblNoPrinted.AsInteger:=0;
  tblSumScore.AsInteger:=0;
  tblScore.AsInteger:=0;
  tblNoDescend.AsInteger:=0;
  tblMaxScore.AsInteger:=scr;
  tblSheetNo.Clear;
  tblSheetOrder.Clear;
  tbl.Post;
  showmessage('Added.');
  UniCodeEdit1.WideText:='';
  UniCodeEdit2.WideText:='';
  Edit2.Text:='';
  Button1Click(self);
  Button5Click(self);
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tbl.Close;
end;

procedure TfMain.Button5Click(Sender: TObject);
var sadd,slea,smas,score,exp,expnd:integer;
    oldexpnd:integer;
    kana:array[0..255] of boolean;
    kanji:array[0..32000] of boolean;
    s:string;
    i,j:integer;
    d:word;
    ws:widestring;
begin
  sadd:=0;
  slea:=0;
  smas:=0;
  score:=0;
  exp:=0;
  tbl.First;
  while not tbl.Eof do
  begin
    inc(sadd);
    if tblScore.AsInteger>0 then inc(slea);
    if tblScore.AsInteger>2 then inc(smas);
    inc(score,tblScore.AsInteger);
    inc(exp,tblMaxScore.AsInteger);
    tbl.Next;
  end;
  Label15.Caption:=inttostr(sadd);
  Label16.Caption:=inttostr(slea);
  Label17.Caption:=inttostr(smas);
  Label18.Caption:=inttostr(exp);
  level:=0;
  expnd:=0;
  oldexpnd:=0;
  repeat
    inc(level);
    oldexpnd:=expnd;
    expnd:=expnd+level*200;
  until expnd>exp;
  Label19.Caption:=inttostr(expnd);
  Label21.Caption:=inttostr(score);
  Label22.Caption:=inttostr(level);
  if sadd>=expnd div 2 then Button3.Enabled:=false else Button3.Enabled:=true;
  Gauge2.MaxValue:=expnd div 2;
  Gauge2.Progress:=(expnd div 2)-sadd;
  Gauge1.MaxValue:=expnd-oldexpnd;
  Gauge1.Progress:=exp-oldexpnd;
  for i:=0 to 255 do kana[i]:=false;
  for i:=0 to 32000 do kanji[i]:=false;
  tbl.First;
  while not tbl.EOF do
  begin
    s:=tblKanji.AsString;
    for i:=1 to length(s) div 4 do
    begin
      if (s[i*4-3]>='4') and (s[i*4-3]<='9') then
      begin
        d:=strtoint('0x'+copy(s,i*4-3,4));
        kanji[d-64*256]:=true;
      end;
    end;
    s:=tblPhonetic.AsString;
    for i:=1 to length(s) div 4 do
    begin
      if (s[i*4-3]='3') and (s[i*4-2]='0') then
      begin
        d:=strtoint('0x'+copy(s,i*4-1,2));
        kana[d]:=true;
      end;
    end;
    tbl.Next;
  end;
  d:=0;
  for i:=0 to 255 do if kana[i] then inc(d);
  Label20.Caption:=inttostr(d);
  d:=0;
  UnicodeRichEdit1.WideLines.Clear;
  UnicodeRichEdit1.WideLines.Add('JWB USER KANJI LIST');
  UnicodeRichEdit1.WideLines.Add('');
  j:=0;
  ws:='';
  for i:=0 to 32000 do if kanji[i] then
  begin
    inc(d);
    if j=20 then
    begin
      j:=0;
      UnicodeRichEdit1.WideLines.Add(ws);
      ws:='';
    end;
    ws:=ws+widechar(i+64*256);
    inc(j);
  end;
  UnicodeRichEdit1.WideLines.Add(ws);
  UnicodeRichEdit1.WideLines.SaveToFile('userkanji.txt');
  UnicodeRichEdit1.WideLines.Clear;
  UnicodeRichEdit1.WideLines.Add('JWB RANDOM HIRAGANA');
  UnicodeRichEdit1.WideLines.Add('');
  randomize;
  for i:=1 to 60 do
  begin
    ws:='';
    for j:=1 to 30 do ws:=ws+widechar(48*256+64+11+random(73))+' ';
    UnicodeRichEdit1.WideLines.Add(ws);
  end;
  UnicodeRichEdit1.WideLines.SaveToFile('randomhira.txt');
  UnicodeRichEdit1.WideLines.Clear;
  UnicodeRichEdit1.WideLines.Add('JWB RANDOM KATAKANA');
  UnicodeRichEdit1.WideLines.Add('');
  randomize;
  for i:=1 to 60 do
  begin
    ws:='';
    for j:=1 to 30 do ws:=ws+widechar(48*256+64+96+11+random(73))+' ';
    UnicodeRichEdit1.WideLines.Add(ws);
  end;
  UnicodeRichEdit1.WideLines.SaveToFile('randomkata.txt');
  Label23.Caption:=inttostr(d);
end;

procedure TfMain.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfMain.Button2Click(Sender: TObject);
var sht,shnum,wd:integer;
    ax:array[1..500] of boolean;
    aw:array[1..500] of integer;
procedure SeekWord(sco:integer);
var maxs,maxr,curs:integer;
    f:file of byte;
begin
  randomize;
  maxs:=0;
  maxr:=0;
  tbl.Filter:='Score='+inttostr(sco);
  tbl.Filtered:=true;
  tbl.First;
  while not tbl.Eof do
  begin
    if tblSheetNo.IsNull then
    begin
      curs:=100-tblLevelAdded.AsInteger*3-tblNoPrinted.AsInteger*2+tblMaxScore.AsInteger*10+tblNoDescend.AsInteger*4
        +trunc(now-tblPrinted.AsDateTime)+random(10)+random(20)+random(20);
      if curs>maxs then
      begin
        maxs:=curs;
        maxr:=tblKey.AsInteger;
      end;
    end;
    tbl.Next;
  end;
  if maxr>0 then
  begin
    tbl.Locate('Key',maxr,[]);
    tbl.Edit;
    inc(shnum);
    dec(wd);
    tblSheetNo.AsInteger:=sht;
    tblSheetOrder.AsInteger:=shnum;
    tbl.Post;
  end;
  tbl.Filter:='';
  tbl.Filtered:=false;
end;
procedure SeekWords(sco,num:integer);
var i:integer;
begin
  for i:=1 to num do SeekWord(sco);
end;
var i,j:integer;
    us:WideString;
begin
  tbl.IndexName:='Main_Ind';
  sht:=1;
  shnum:=0;
  tbl.First;
  while not tbl.Eof do
  begin
    if tblSheetNo.AsInteger>=sht then sht:=tblSheetNo.AsInteger+1;
    tbl.Next;
  end;
  wd:=strtoint(edit1.text);
  if SpeedButton1.Down then
  begin
    SeekWords(3,1);
    SeekWords(2,wd div 15);
    SeekWords(1,wd div 10);
    SeekWords(-1,wd div 2);
    SeekWords(0,wd);
  end;
  if SpeedButton2.Down then
  begin
    SeekWords(4,1);
    SeekWords(3,wd div 15);
    SeekWords(-1,wd div 10);
    SeekWords(2,wd div 3);
    SeekWords(0,wd div 3);
    SeekWords(1,wd);
  end;
  if SpeedButton3.Down then
  begin
    SeekWords(0,wd div 15);
    SeekWords(1,wd div 5);
    SeekWords(2,wd div 3);
    SeekWords(3,wd div 3);
    SeekWords(4,wd div 4);
    SeekWords(-1,wd);
  end;
  if shnum=0 then
  begin
    showmessage('No words found.');
    exit;
  end;
  for i:=1 to shnum do ax[i]:=false;
  for i:=1 to shnum do
  begin
    tbl.Locate('SheetNo;SheetOrder',VarArrayOf([sht,i]),[]);
    repeat
      j:=random(shnum)+1;
    until not ax[j];
    aw[i]:=j;
    ax[j]:=true;
  end;
  tbl.First;
  while not tbl.Eof do
  begin
    if tblSheetNo.AsInteger=sht then
    begin
      tbl.Edit;
      tblSheetOrder.AsInteger:=aw[tblSheetOrder.AsInteger];
      tbl.Post;
    end;
    tbl.Next;
  end;
  tbl.IndexName:='Report_Ind';
  tbl.Filter:='SheetNo='+inttostr(sht);
  tbl.Filtered:=true;
  UnicodeRichEdit1.WideLines.Clear;
  tbl.First;
  UnicodeRichEdit1.WideLines.Add('JWB SHEET ('+inttostr(sht)+')');
  UnicodeRichEdit1.WideLines.Add('');
  while not tbl.Eof do
  begin
    us:='';
    if CheckBox1.Checked then
    begin
    us:=us+HexToUnicode(tblKanji.AsString);
    for i:=length(tblKanji.AsString) div 2 to 22 do us:=us+'.';
    us:=us+'|';
    end;
    us:=us+HexToUnicode(tblPhonetic.AsString);
    for i:=length(tblPhonetic.AsString) div 2 to 22 do us:=us+'.';
    if not CheckBox1.Checked then
    begin
    us:=us+'|';
    us:=us+HexToUnicode(tblKanji.AsString);
    for i:=length(tblKanji.AsString) div 2 to 22 do us:=us+'.';
    end;
    us:=us+'|';
    us:=us+tblEnglish.AsString;
    UnicodeRichEdit1.WideLines.Add(us);
    tbl.Edit;
    tblPrinted.AsDateTime:=now;
    tblNoPrinted.AsInteger:=tblNoPrinted.AsInteger+1;
    tbl.Post;
    tbl.Next;
  end;
  tbl.IndexName:='Main_Ind';
  tbl.Filter:='';
  tbl.Filtered:=false;
  UnicodeRichEdit1.WideLines.SaveToFile('ws_'+inttostr(sht)+'.txt');
  showmessage('Wordsheet contains '+inttostr(shnum)+' entries.'#13'Wordsheet saved as ''WS_'+inttostr(sht)+'.TXT''.');
end;

procedure TfMain.Button4Click(Sender: TObject);
var scr,scrp:integer;
begin
  tbl.IndexName:='Report_Ind';
  tbl.First;
  while not tbl.Eof do
  begin
    if tblSheetNo.AsInteger=strtoint(Edit3.Text) then
    begin
    fRate.Label1.Caption:=tblEnglish.AsString;
    fRate.ShowModal;
    tbl.Edit;
    if (tblLearned.IsNull) and (fRate.sc>0) then tblLearned.AsDateTime:=now;
    if (tblMastered.IsNull) and (fRate.sc>2) then tblMastered.AsDateTime:=now;
    if (tblLevelLearned.IsNull) and (fRate.sc>0) then tblLevelLearned.AsInteger:=level;
    if (tblLevelMastered.IsNull) and (fRate.sc>2) then tblLevelMastered.AsInteger:=level;
    scr:=tblScore.AsInteger;
    scrp:=scr;
    case fRate.sc of
      0:scr:=-1;
      1:if scr=-1 then scr:=0 else scr:=1;
      2:if scr<1 then scr:=1 else scr:=2;
      3:if scr<3 then scr:=3 else scr:=4;
    end;
    tblScore.AsInteger:=scr;
    tblSumScore.AsInteger:=tblSumScore.AsInteger+scr;
    if scrp>scr then tblNoDescend.AsInteger:=tblNoDescend.AsInteger+1;
    if tblMaxScore.AsInteger<scr then tblMaxScore.AsInteger:=scr;
    tblSheetNo.Clear;
    tblSheetOrder.Clear;
    tbl.Post;
    end;
    tbl.Next;
  end;
  tbl.IndexName:='Main_Ind';
  Button5Click(self);
end;

procedure TfMain.Button6Click(Sender: TObject);
begin
  UniCodeEdit1.WideText:='';
  UniCodeEdit2.WideText:='';
  Edit2.Text:='';
  Button1Click(self);
end;

procedure TfMain.Button7Click(Sender: TObject);
begin
  showmessage('Press ENTER to start.');
  fTest.ShowModal;
end;

end.
