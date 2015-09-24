unit JWBTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, UnicodeEdit, UnicodeMemo;

type
  TfTest = class(TForm)
    UnicodeEdit1: TUnicodeEdit;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    UnicodeEdit2: TUnicodeEdit;
    procedure FormShow(Sender: TObject);
    procedure UnicodeEdit1KeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Advance;
    function GetKana(b:integer):string;
    { Public declarations }
  end;

var
  fTest: TfTest;

implementation

uses JWBMain;

var a:array[1..400] of byte;
    mist:array[1..100,1..100] of byte;
    wrong:array[1..100] of byte;
    wrongsum,timingsum:integer;
    timing:array[1..100,1..4] of integer;
    timingxsum:array[1..100] of integer;
    i,j:integer;
    k,l,m:integer;
    curp:string;
    curkana:byte;
    pos:integer;
    starttim:TDateTime;

{$R *.DFM}

function TfTest.GetKana(b:integer):string;
var q,ix:byte;
    s:string;
begin
  q:=0;
  for ix:=1 to b do
  begin
    inc(q);
    while fMain.Memo1.Lines[q-1]='-'do inc(q);
  end;
  s:=fMain.Memo1.Lines[q-1]+',';
  s:=copy(s,1,system.pos(',',s)-1);
  result:=s;
end;

procedure TfTest.FormShow(Sender: TObject);
var gd:boolean;
begin
  k:=0;
  for i:=0 to fMain.Memo1.Lines.Count-1 do if fMain.Memo1.Lines[i]<>'-'then inc(k);
  for i:=1 to 4*k do
  begin
    repeat
    l:=random(4*k-i)+1;
    m:=0;
    for j:=1 to l do
    begin
      inc(m);
      while a[m]<>0 do inc(m);
    end;
    gd:=true;
    if (m>1) and (a[m-1]=((i-1) div 4)+1) then gd:=false;
    if (a[m+1]=((i-1) div 4)+1) then gd:=false;
    if gd then a[m]:=((i-1) div 4)+1;
    until gd;
  end;
  ProgressBar1.Max:=4*k;
  pos:=1;
  starttim:=now;
  wrongsum:=0;
  timingsum:=0;
  for i:=1 to 100 do for j:=1 to 100 do mist[i,j]:=0;
  for i:=1 to 100 do wrong[i]:=0;
  for i:=1 to 100 do for j:=1 to 4 do timing[i,j]:=0;
  for i:=1 to 100 do timingxsum[i]:=0;
  Advance;
end;

procedure TfTest.Advance;
var tim:integer;
    q:byte;
    s:widestring;
    sx:TStringList;
    s2:string;
    sperf,smist,stime,sbad:string;
begin
  sperf:='';
  smist:='';
  stime:='';
  sbad:='';
  if curkana<>0 then
  begin
    mist[curkana,a[pos]]:=mist[curkana,a[pos]]+1;
    if curkana<>a[pos] then wrong[a[pos]]:=wrong[a[pos]]+1;
    if curkana<>a[pos] then inc(wrongsum);
    tim:=round(frac(now-starttim)*24*60*60*100);
    q:=0;
    for i:=1 to 4 do if (timing[a[pos],i]=0) and (q=0) then q:=i;
    timing[a[pos],q]:=tim;
    timingsum:=timingsum+tim;
    timingxsum[a[pos]]:=timingxsum[a[pos]]+tim;
    inc(pos);
    starttim:=now;
    curp:='';
    ProgressBar1.Position:=pos;
  end;
  if pos>4*k then
  begin
    sx:=TStringList.Create;
    sx.Add('Mistakes: '+inttostr(wrongsum));
    sx.Add('Time per kana (ms): '+inttostr(round(timingsum/4/k*10)));
    sx.Add('');
    for i:=1 to k do
    begin
      s2:=inttostr(i)+'. '+GetKana(i)+': ';
      if (wrong[i]=0) and (timingxsum[i]<=200) then sperf:=sperf+GetKana(i)+' ';
      if (timingxsum[i]>400) and (timingxsum[i]<=800) then stime:=stime+GetKana(i)+' ';
      if (timingxsum[i]>800) then sbad:=sbad+GetKana(i)+' ';
      if wrong[i]>0 then
      begin
        smist:=smist+GetKana(i)+' ';
        s2:=s2+'(MIST:'+inttostr(wrong[i]);
        for j:=1 to k do if (mist[j,i]>0) and (j<>i) then s2:=s2+'; '+GetKana(j)+inttostr(mist[j,i]);
        s2:=s2+') ';
      end;
      s2:=s2+inttostr(round(timingxsum[i]/4*10));
      sx.Add(s2);
    end;
    sx.Add('');
    sx.Add('PERFECT: '+sperf);
    sx.Add('MISTAKEN: '+smist);
    sx.Add('TIMEOUT: '+stime);
    sx.Add('TERRIBLE: '+sbad);
    sx.SaveToFile('kanatest\'+formatdatetime('yymmddhhnnss',now)+'.ktr');
    sx.Free;
    showmessage('TEST FINISHED'#13#13'Mistakes: '+inttostr(wrongsum)+
      #13'Time per kana (ms): '+inttostr(round(timingsum/4/k*10))+
      #13#13+'PERFECT: '+sperf+#13'MISTAKEN: '+smist+#13'TIMEOUT: '+stime+#13'TERRIBLE: '+sbad);
    close;
    exit;
  end;
  s:='';
  for i:=((pos-1) div 40)*40+1 to ((pos-1) div 40)*40+40 do if i<=4*k then
  begin
    q:=0;
    for j:=1 to a[i] do
    begin
      inc(q);
      while fMain.Memo1.Lines[q-1]='-'do inc(q);
    end;
    s:=s+widechar(48*256+64+q-1);
  end;
  UnicodeEdit1.WideText:=s;
  s:='';
  for i:=((pos-1) div 40)*40+41 to ((pos-1) div 40)*40+80 do if i<=4*k then
  begin
    q:=0;
    for j:=1 to a[i] do
    begin
      inc(q);
      while fMain.Memo1.Lines[q-1]='-'do inc(q);
    end;
    s:=s+widechar(48*256+64+q-1);
  end;
  UnicodeEdit2.WideText:=s;
  UnicodeEdit1.SelStart:=0;
  UnicodeEdit1.SelLength:=((pos-1) mod 40);
  UnicodeEdit1.SetFocus;
end;

procedure TfTest.UnicodeEdit1KeyPress(Sender: TObject; var Key: Char);
var q:integer;
begin
  curp:=curp+Key;
  curkana:=0;
  q:=0;
  for i:=0 to fMain.Memo1.Lines.Count-1 do
  begin
    if fMain.Memo1.Lines[i]<>'-'then inc(q);
    if system.pos(','+curp+',',','+fMain.Memo1.Lines[i]+',')>0 then curkana:=q;
  end;
  if curkana<>0 then
  begin
    Advance;
  end;
  if length(curp)>2 then
  begin
    showmessage('Unrecognized romaji: "'+curp+'". Resetting.');
    curp:='';
    exit;
  end;
end;

procedure TfTest.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
