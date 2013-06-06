unit JWBExamples;
{
Code to handle example sentences.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, MemSource, JWBStrings, WakanPaintbox;

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
    btnGoToExample: TSpeedButton;
    btnCopyAllToClipboard: TSpeedButton;
    btnRandomOrder: TSpeedButton;
    btnDisplayTranslation: TSpeedButton;
    btnUseSmallFont: TSpeedButton;
    btnUseBigFont: TSpeedButton;
    btnPrevious: TSpeedButton;
    btnNext: TSpeedButton;
    btnCopyToClipboard: TSpeedButton;
    Paintbox: TWakanPaintbox;
    procedure btnGoToExampleClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnDisplayTranslationClick(Sender: TObject);
    procedure btnRandomOrderClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnCopyAllToClipboardClick(Sender: TObject);
    procedure PaintboxPaint(Sender: TObject; Canvas: TCanvas);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    randbank:TStringList;

  public
    procedure BuildExamplesPackage;
    procedure ReloadExamples;

  protected
    procedure ReadExample(ex_ind: integer; out ex_jap: FString; out ex_en: string);
  public
    procedure SetExamples(kanji:string);
    procedure ShowExample;
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

uses JWBWordLookup, JWBMenu, JWBSettings, JWBVocab, JWBUnit, PKGWrite;

{$R *.DFM}

procedure TfExamples.FormCreate(Sender: TObject);
begin
  randbank:=TStringList.Create;
end;

procedure TfExamples.FormDestroy(Sender: TObject);
begin
  FreeAndNil(randbank);
end;

procedure TfExamples.btnGoToExampleClick(Sender: TObject);
var n:string;
begin
  n:=InputBox(_l('#00892^eGo to example'),
    _l('#00893^eEnter the number of the example:'),'');
  if n<>'' then
  try
    GotoExample(strtoint(n));
  except end;
end;

procedure TfExamples.btnPreviousClick(Sender: TObject);
begin
  MoveExample(false);
end;

procedure TfExamples.btnNextClick(Sender: TObject);
begin
  MoveExample(true);
end;

procedure TfExamples.btnDisplayTranslationClick(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TfExamples.btnRandomOrderClick(Sender: TObject);
var cansel:boolean;
begin
  if fWordLookup.Visible then
    fWordLookup.ShowWord
  else
  if fVocab.Visible then
    fVocab.StringGrid1SelectCell(sender,fVocab.StringGrid1.Col,fVocab.StringGrid1.Row,cansel);
end;

procedure TfExamples.btnCopyToClipboardClick(Sender: TObject);
begin
  ExampleClipboard(false);
end;

procedure TfExamples.btnCopyAllToClipboardClick(Sender: TObject);
begin
  ExampleClipboard(true);
end;

procedure TfExamples.PaintboxPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  Canvas.Font.Style:=[];
  BeginDrawReg(Canvas); //I really hope this is no temporary canvss
  if btnDisplayTranslation.Down then DrawUnicode(Canvas,4,4,16,ex_jap,FontSmall);
  if btnUseSmallFont.Down then DrawUnicode(Canvas,4,16,16,ex_jap,FontSmall);
  if btnUseBigFont.Down then DrawUnicode(Canvas,4,6,24,ex_jap,FontJapanese);
  EndDrawReg;
  if btnDisplayTranslation.Down then DrawUnicode(Canvas,4,23,16,fstr(ex_en),FontEnglish);
end;

procedure TfExamples.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfExamples.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipMouseMove(PaintBox,x,y,ssLeft in Shift);
end;

procedure TfExamples.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.IntTipMouseUp;
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

procedure TfExamples.BuildExamplesPackage;
var
  tempDir: string;
  pack: TPackageBuilder;
  f:file of byte;
  f2:file of word;
  f3:file of byte;
  f4:file of integer;
  sl:TStringList;
  b,h,l:byte;
  i,j,curpos,nextpos:integer;
  s2:TStringList;
  poss:string;
  curs:string;
  act:boolean;
  w:word;
  s:string;
begin
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);

  assignfile(f,'examples.uni');
  assignfile(f2,tempDir+'\index.bin');
  assignfile(f4,tempDir+'\struct.bin');
  assignfile(f3,tempDir+'\examples.bin');
  system.reset(f);
  rewrite(f2);
  rewrite(f3);
  rewrite(f4);
  sl:=TStringList.Create;
  s2:=TStringList.Create;
  curpos:=0;
  read(f,b);
  read(f,b);
  while not eof(f) do
  begin
    read(f,b);
    while b=ord('#') do
    begin
      read(f,b);
      h:=0; l:=0;
      while (h<>0) or (l<>10) do
      begin
        read(f,l); read(f,h);
      end;
{      h:=0; l:=0;
      while (h<>0) or (l<>10) do
      begin
        read(f,l); read(f,h);
      end;}
      read(f,b);
    end;
    if b<>ord('A') then showmessage('BAD STRUCTURE - '+inttostr(b));
    for i:=1 to 5 do read(f,b);
    h:=0; l:=0;
    s2.Clear;
    while (h<>0) or (l<>9) do
    begin
      read(f,l);
      read(f,h);
      if ((h<>0) or (l<>9)) and (s2.Count<508) then
      begin
        s2.Add(inttostr(l));
        s2.Add(inttostr(h));
      end;
    end;
    if s2.Count>510 then showmessage('OVERFLOW1');
    b:=s2.Count div 2;
    write(f3,b);
    for i:=0 to s2.Count-1 do
    begin
      b:=strtoint(s2[i]);
      write(f3,b);
    end;
    inc(nextpos,s2.Count+1);
    s2.Clear;
    while (h<>0) or (l<>10) do
    begin
      read(f,l);
      read(f,h);
      if ((h<>0) or (l<>10)) and (s2.Count<255) then s2.Add(inttostr(l));
    end;
    if s2.Count>255 then showmessage('OVERFLOW2');
    b:=s2.Count;
    write(f3,b);
    for i:=0 to s2.Count-1 do
    begin
      b:=strtoint(s2[i]);
      write(f3,b);
    end;
    read(f,b);
    if b<>ord('B') then showmessage('BAD STRUCTURE');
    for i:=1 to 5 do read(f,b);
    inc(nextpos,s2.Count+1);
    poss:=Format('%8.8X',[curpos]);
    curs:='';
    b:=0;
    l:=0;
    act:=true;
    while (h<>0) or (l<>10) do
    begin
      read(f,l);
      read(f,h);
      if (h=0) and (l=ord('(')) then act:=false else
      if (h=0) and (l=ord('[')) then act:=false else
      if (h=0) and (l=ord(')')) then act:=true else
      if (h=0) and (l=ord(']')) then act:=true else
      if (h=0) and (l=32) then
      begin
        while length(curs)<24 do curs:=curs+'0000';
        if length(curs)>24 then curs:=copy(curs,1,24);
        sl.Add(curs+poss);
        curs:='';
      end else if (h<>0) and (act) then curs:=curs+Format('%2.2X%2.2X',[h,l]);
    end;
    curpos:=nextpos;
  end;
  sl.Sort;
  curpos:=0;
  s:='';
  for i:=0 to sl.Count-1 do
  begin
    curs:=sl[i];
    if (copy(curs,1,24)<>s) then
    begin
      s:=copy(curs,1,24);
      for j:=1 to 6 do
      begin
        h:=strtoint('0x'+copy(curs,(j-1)*4+1,2));
        l:=strtoint('0x'+copy(curs,(j-1)*4+3,2));
        w:=h*256+l;
        write(f2,w);
      end;
      w:=curpos mod 65536;
      write(f2,w);
      w:=curpos div 65536;
      write(f2,w);
    end;
    j:=strtoint('0x'+copy(curs,25,8));
    inc(curpos);
    write(f4,j);
  end;
  closefile(f2);
  closefile(f3);
  closefile(f4);
  closefile(f);

  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := 'examples_j.pkg';
    pack.MemoryLimit := 100000000;
    pack.Name := 'Examples';
    pack.TitleName := 'Japanese dictionary examples';
    pack.CompanyName := 'LABYRINTH';
    pack.CopyrightName := '(C) Gabriel SanRoman';
    pack.FormatName := 'Pure Package File';
    pack.CommentName := 'File is used by '+WakanAppName;
    pack.VersionName := '1.0';
    pack.HeaderCode := 791564;
    pack.FilesysCode := 978132;
    pack.WriteHeader;
    pack.LoadMode := lmTemporaryLoad;
    pack.CryptMode := 0;
    pack.CrcMode := 0;
    pack.PackMode := 0;
    pack.CryptCode := 978123;
    pack.Include(tempDir);
    pack.Finish;
  finally
    FreeAndNil(pack);
  end;

  DeleteDirectory(tempDir);
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
  if btnRandomOrder.Down then ex_indcur:=strtoint(randbank[0]);
  ShowExample;
end;

procedure TfExamples.ReadExample(ex_ind: integer; out ex_jap: FString; out ex_en: string);
var buf:array[0..1023] of byte;
  ofs:integer;
  i,siz,siz2:integer;
begin
  ofs := PInteger(integer(examstruct)+ex_ind*4)^;
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

procedure TfExamples.ShowExample;
var ms:string;
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
    ReadExample(ex_indcur, ex_jap, ex_en);
  if ex_indfirst=-1 then ms:='0'else if (ex_indlast-ex_indfirst)<99 then ms:=inttostr(ex_indlast-ex_indfirst+1) else ms:='lot';
  pos:=0;
  if ex_indfirst=-1 then Label2.Caption:='0/0'else
  begin
    if not btnRandomOrder.Down then pos:=ex_indcur-ex_indfirst else pos:=randbank.IndexOf(inttostr(ex_indcur));
    Label2.Caption:=inttostr(pos+1)+'/'+ms;
  end;
  if ex_indfirst=-1 then Label6.Caption:='-'else Label6.Caption:=inttostr(ex_indcur-ex_indfirst+1);
  btnPrevious.Enabled:=(ex_indfirst>-1) and (pos>0);
  btnNext.Enabled:=(ex_indfirst>-1) and (pos<ex_indlast-ex_indfirst);
  btnCopyToClipboard.Enabled:=(ex_indfirst>-1);
  btnCopyAllToClipboard.Enabled:=(ex_indfirst>-1);
  PaintBox.Invalidate;
end;

procedure TfExamples.MoveExample(right:boolean);
var pos:integer;
    a:integer;
begin
  if not btnRandomOrder.Down then begin
    if right then inc(ex_indcur) else dec(ex_indcur);
  end else
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
  max:integer;
  tmp_jap: FString;
  tmp_en: string;
begin
  if not all then
  begin
    if btnDisplayTranslation.Down then
      clip:=ex_jap+UH_CR+UH_LF+fstr(ex_en)
    else
      clip:=ex_jap;
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
      ReadExample(i, tmp_jap, tmp_en);
      if btnDisplayTranslation.Down then
        clip:=tmp_jap+UH_CR+UH_LF+fstr(tmp_en)+UH_CR+UH_LF
      else
        clip:=tmp_jap+UH_CR+UH_LF;
    end;
  end;
  fMenu.SetClipboard;
end;

procedure TfExamples.GotoExample(num:integer);
var pos:integer;
begin
  if num>ex_indlast-ex_indfirst then exit;
  if ex_indfirst=-1 then exit;
  if num<0 then exit;
  if not btnRandomOrder.Down then begin
    ex_indcur:=ex_indfirst+num-1;
  end else
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
