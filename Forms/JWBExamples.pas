unit JWBExamples;
{
Code to handle example sentences.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, MemSource, JWBUnit;

type
  TfExamples = class(TForm)
    Panel1: TPanel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PaintBox3: TPaintBox;
    Shape9: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure PaintBox3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    randbank:TStringList;

  public
    procedure ReloadExamples;

  public
    procedure SetExamples(kanji:string);
    procedure ShowExample;
    procedure PaintExample;
    procedure MoveExample(right:boolean);
    procedure RandomExample;
    procedure ExampleClipboard(all:boolean);
    procedure GotoExample(num:integer);

  end;

var
  fExamples: TfExamples;

  ex_indfirst,ex_indlast,ex_indcur:integer;
  ex_jap: FString;
  ex_en: string;

  examstruct,examindex:pointer;
  examstructsiz,examindexsiz:integer;
  exampackage: TPackageSource;
  examfile:TMemoryFile;

implementation

uses JWBUser, JWBMenu, JWBSettings, JWBWords;

{$R *.DFM}

procedure TfExamples.FormCreate(Sender: TObject);
begin
  randbank:=TStringList.Create;
end;

procedure TfExamples.FormDestroy(Sender: TObject);
begin
  FreeAndNil(randbank);
end;

procedure TfExamples.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton9.Down:=false;
  fMenu.aDictAdd.Checked:=false;
end;

procedure TfExamples.SpeedButton1Click(Sender: TObject);
var n:string;
begin
  n:=InputBox(_l('#00892^eGo to example'),
    _l('#00893^eEnter the number of the example:'),'');
  if n<>'' then
  try
    GotoExample(strtoint(n));
  except end;
end;

procedure TfExamples.PaintBox3Paint(Sender: TObject);
begin
  PaintExample;
end;

procedure TfExamples.PaintBox3MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfExamples.SpeedButton7Click(Sender: TObject);
begin
  MoveExample(false);
end;

procedure TfExamples.SpeedButton8Click(Sender: TObject);
begin
  MoveExample(true);
end;

procedure TfExamples.SpeedButton4Click(Sender: TObject);
begin
  PaintBox3.Invalidate;
end;

procedure TfExamples.SpeedButton11Click(Sender: TObject);
var cansel:boolean;
begin
  if fUser.Visible then fUser.ShowWord else
  if fWords.Visible then fWords.StringGrid1SelectCell(sender,fWords.StringGrid1.Col,fWords.StringGrid1.Row,cansel);
end;

procedure TfExamples.SpeedButton9Click(Sender: TObject);
begin
  ExampleClipboard(false);
end;

procedure TfExamples.SpeedButton10Click(Sender: TObject);
begin
  ExampleClipboard(true);
end;

procedure TfExamples.PaintBox3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfExamples.PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

//There can be different example packages for japanese and chinese,
//so we reload every time study language changes.
procedure TfExamples.ReloadExamples;
var mf:TMemoryFile;
begin
  if exampackage<>nil then exampackage.Free;
  if examstruct<>nil then FreeMem(examstruct);
  if examindex<>nil then FreeMem(examindex);
  exampackage:=nil;
  examstruct:=nil;
  examindex:=nil;
  try
    if FileExists('examples_'+curlang+'.pkg') then
    begin
      exampackage:=TPackageSource.Create('examples_'+curlang+'.pkg',791564,978132,978123);
      mf:=exampackage['struct.bin'];
      if mf=nil then raise Exception.Create('Important file missing.');
      GetMem(examstruct,mf.Size);
      exampackage.ReadRawData(examstruct^,integer(mf.Position),mf.Size);
      examstructsiz:=mf.Size div 4;
      mf:=exampackage['index.bin'];
      if mf=nil then raise Exception.Create('Important file missing.');
      GetMem(examindex,mf.Size);
      exampackage.ReadRawData(examindex^,integer(mf.Position),mf.Size);
      examindexsiz:=mf.Size div 16;
      examfile:=exampackage['examples.bin'];
      if examfile=nil then raise Exception.Create('Important file missing.');
    end;
  except
    Application.MessageBox(pchar(_l('#00333^eCouldn''t load example file EXAMPLES_'+upcase(curlang)+'.PKG.')),
      pchar(_l('#00020^eError')),MB_ICONERROR or MB_OK);
    exampackage:=nil;
    examstruct:=nil;
    examindex:=nil;
  end;
end;

procedure TfExamples.SetExamples(kanji:string);
var l,r,m,max:integer;
  s2:string;
  p:PByte;
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
  while flength(kanji)<6 do kanji:=kanji+UH_ZERO;
  if flength(kanji)>6 then kanji:=fcopy(kanji,1,6);
  l:=0;
  max:=examindexsiz;
  r:=max;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    p:=PByte(integer(examindex)+m*16);
    s2:='';
    for j:=1 to 6 do
    begin
      w := PWord(p)^;
      Inc(p, 2);
     {$IFNDEF UNICODE}
      s2:=s2+Format('%4.4X',[w]);
     {$ELSE}
      s2:=s2+WideChar(w);
     {$ENDIF}
    end;
    if s2=kanji then break;
    if s2<kanji then l:=m+1 else r:=m-1;
  end;
  if l>r then
    ex_indfirst:=-1
  else
  begin
    p:=PByte(integer(examindex)+m*16+12);
    ex_indfirst := PInteger(p)^;
    Inc(p, 16);
    if m<max then
      ex_indlast := PInteger(p)^
    else
      ex_indlast:=examstructsiz;
    dec(ex_indlast);
  end;
  ex_indcur:=ex_indfirst;
  randbank.Clear;
  randbank.Add(inttostr(ex_indfirst+random(ex_indlast-ex_indfirst+1)));
  if SpeedButton11.Down then ex_indcur:=strtoint(randbank[0]);
  ShowExample;
end;

procedure TfExamples.ShowExample;
var p:PByte;
    ofs:integer;
    buf:array[0..1023] of byte;
    i,siz,siz2:integer;
    ms:string;
    pos:integer;
begin
  ex_jap:='';
  ex_en:='';
  if (examindex=nil) or (examstruct=nil) or (exampackage=nil) then
  begin
    if curlang='j'then
      ex_jap:=fstr(' === '+_l('#00688^eExample database was not found. Download it from WaKan website.'))
    else
      ex_jap:=fstr(' === '+_l('^eExamples are not available in Chinese mode.'));
    ex_indfirst:=-1;
  end
  else
  if ex_indfirst=-1 then
    ex_jap:=fstr(' === '+_l('#00689^eNo examples available.'))
  else
  begin
    p:=PByte(integer(examstruct)+ex_indcur*4);
    ofs := PInteger(p)^;
    exampackage.ReadRawData(buf,integer(examfile.Position)+ofs,1024);

   //Example sentence
    siz:=buf[0];
   {$IFDEF UNICODE}
    SetLength(ex_jap, siz);
    move(PByte(@buf[1])^, PByte(ex_jap)^, siz*SizeOf(Char));
   {$ELSE}
    ex_jap := UnicodeToHex(PWideChar(@buf[1]), siz);
   {$ENDIF}

   //Translation
    siz2:=buf[siz*2+1];
    for i:=siz*2+2 to siz*2+1+siz2 do
      ex_en:=ex_en+chr(buf[i]);
  end;
  if ex_indfirst=-1 then ms:='0'else if (ex_indlast-ex_indfirst)<99 then ms:=inttostr(ex_indlast-ex_indfirst+1) else ms:='lot';
  pos:=0;
  if ex_indfirst=-1 then Label2.Caption:='0/0'else
  begin
    if not SpeedButton11.Down then pos:=ex_indcur-ex_indfirst else pos:=randbank.IndexOf(inttostr(ex_indcur));
    Label2.Caption:=inttostr(pos+1)+'/'+ms;
  end;
  if ex_indfirst=-1 then Label6.Caption:='-'else Label6.Caption:=inttostr(ex_indcur-ex_indfirst+1);
  SpeedButton7.Enabled:=(ex_indfirst>-1) and (pos>0);
  SpeedButton8.Enabled:=(ex_indfirst>-1) and (pos<ex_indlast-ex_indfirst);
  SpeedButton9.Enabled:=(ex_indfirst>-1);
  SpeedButton10.Enabled:=(ex_indfirst>-1);
  PaintBox3.Invalidate;
end;

procedure TfExamples.PaintExample;
begin
  PaintBox3.Canvas.Brush.Color:=clWindow;
  PaintBox3.Canvas.Font.Style:=[];
  BeginDrawReg(PaintBox3);
  if SpeedButton4.Down then DrawUnicode(PaintBox3.Canvas,3,3,16,ex_jap,FontSmall);
  if SpeedButton5.Down then DrawUnicode(PaintBox3.Canvas,3,15,16,ex_jap,FontSmall);
  if SpeedButton6.Down then DrawUnicode(PaintBox3.Canvas,3,5,24,ex_jap,FontJapanese);
  EndDrawReg;
  if SpeedButton4.Down then DrawUnicode(PaintBox3.Canvas,3,22,16,fstr(ex_en),FontEnglish);
end;

procedure TfExamples.MoveExample(right:boolean);
var pos:integer;
    a:integer;
begin
  if not SpeedButton11.Down then
    if right then inc(ex_indcur) else dec(ex_indcur);
  if SpeedButton11.Down then
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

procedure TfExamples.RandomExample;
begin
  ex_indcur:=ex_indfirst+random(ex_indlast-ex_indfirst+1);
  ShowExample;
end;

procedure TfExamples.ExampleClipboard(all:boolean);
var i:integer;
    p:pchar;
    ofs:integer;
    buf:array[0..1023] of byte;
    j,siz,siz2:integer;
    max:integer;
begin
  if not all then
  begin
    if SpeedButton4.Down then clip:=ex_jap+'000D000A'+UnicodeToHex(ex_en)
    else clip:=ex_jap;
  end else
  begin
    clip:='';
    if ex_indlast-ex_indfirst>99 then
    begin
      max:=ex_indfirst+99;
      Application.MessageBox(
        pchar(_l('^eThere are too many examples. Only first hundred have been copied.')),
        pchar(_l('#00364^eNotice')),MB_ICONINFORMATION or MB_OK);
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
      clip:=clip+UH_CR+UH_LF;
      if SpeedButton4.Down then for j:=siz*2+2 to siz*2+1+siz2 do clip:=clip+UnicodeToHex(chr(buf[j]));
      if SpeedButton4.Down then clip:=clip+UH_CR+UH_LF;
    end;
  end;
  fMenu.ChangeClipboard;
end;

procedure TfExamples.GotoExample(num:integer);
var pos:integer;
begin
  if num>ex_indlast-ex_indfirst then exit;
  if ex_indfirst=-1 then exit;
  if num<0 then exit;
  if not SpeedButton11.Down then
    ex_indcur:=ex_indfirst+num-1 else
  if SpeedButton11.Down then
  begin
    ex_indcur:=ex_indfirst+num-1;
    pos:=randbank.IndexOf(inttostr(num));
    if pos=-1 then randbank.Add(inttostr(ex_indfirst+num-1));
  end;
  ShowExample;
end;

initialization
  ex_jap := '';
  ex_en := '';

end.
