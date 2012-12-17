unit JWBDictImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfDictImport = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    ListBox1: TListBox;
    Edit3: TEdit;
    RadioGroup1: TRadioGroup;
    Edit4: TEdit;
    Edit5: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  public
    entries:integer;
    procedure CreateDictTables(dictlang:char);
    procedure WriteDictPackage(dictlang:char);
  end;

var
  fDictImport: TfDictImport;

implementation

uses JWBDictCoding, StdPrompt, JWBUnit, JWBMenu, PKGWrite, JWBConvert,
  JWBDicSearch, JWBStrings;

{$R *.DFM}

procedure TfDictImport.FormShow(Sender: TObject);
begin
  edit1.text:='noname';
  edit2.text:='NONAME';
  listbox1.items.clear;
  button2.enabled:=false;
  bitbtn1.enabled:=false;
  edit3.text:='N/A';
  RadioGroup1.ItemIndex:=0;
  edit4.text:='';
  edit5.text:='';
end;

procedure TfDictImport.Button1Click(Sender: TObject);
begin
  if Opendialog1.execute then
  begin
    ListBox1.Items.Add(OpenDialog1.FileName);
    button2.enabled:=true;
    bitbtn1.enabled:=true;
    if ListBox1.ItemIndex=-1 then ListBox1.ItemIndex:=0;
  end;
end;

procedure TfDictImport.Button2Click(Sender: TObject);
begin
  ListBox1.Items.Delete(ListBox1.ItemIndex);
  if ListBox1.Items.Count>0 then ListBox1.ItemIndex:=0 else
  begin
    Button2.Enabled:=false;
    bitbtn1.enabled:=false;
  end;
end;

procedure TfDictImport.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

function split(var s:string;c:char):string;
begin
  if pos(c,s)=0 then
  begin
    result:='';
    exit;
  end;
  result:=copy(s,1,pos(c,s)-1);
  delete(s,1,pos(c,s));
end;

function repl(var s:string;sub,repl:string):string;
begin
  while pos(sub,s)>0 do
    s:=copy(s,1,pos(sub,s)-1)+repl+copy(s,pos(sub,s)+length(sub),length(s)-pos(sub,s)+1-length(sub));
  result:=s;
end;

function booltostr(b:boolean):String;
begin
  if b then result:='T'else result:='F';
end;

procedure TfDictImport.CreateDictTables(dictlang:char);
var t:textfile;
begin
  {$I-}
  mkdir('dict');
  {$I+}
  ioresult;
  assignfile(t,'dict\Dict.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$PREBUFFER');
  writeln(t,'$RAWINDEX');
  writeln(t,'$FIELDS');
  writeln(t,'iIndex');
  writeln(t,'sEnglish');
  writeln(t,'xPhonetic');
  writeln(t,'xKanji');
  writeln(t,'sSort');
  writeln(t,'sMarkers');
  writeln(t,'iFrequency');
  writeln(t,'$ORDERS');
  writeln(t,'Phonetic_Ind');
  writeln(t,'Kanji_Ind');
  writeln(t,'<Phonetic_Ind');
  writeln(t,'<Kanji_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'Index');
  writeln(t,'Sort');
  writeln(t,'Kanji');
  writeln(t,'<Sort');
  writeln(t,'<Kanji');
  writeln(t,'$CREATE');
  closefile(t);
  WriteDictPackage(dictlang);
  DeleteFile('dict\Dict.info');
  DeleteFile('dict\dict.ver');
  {$I-}
  rmdir('dict');
  {$I+}
  ioresult;
end;

procedure TfDictImport.WriteDictPackage(dictlang:char);
var
  tempDir: string;
  f:textfile;
begin
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  assignfile(f,tempDir+'\dict.ver');
  rewrite(f);
  writeln(f,'DICT');
  writeln(f,'4');
  writeln(f,inttostr(trunc(now)));
  writeln(f,edit3.text);
  writeln(f,edit2.text);
  writeln(f,dictlang);
  writeln(f,edit4.text);
  writeln(f,inttostr(radiogroup1.itemindex));
  writeln(f,inttostr(entries));
  writeln(f,edit5.text);
  closefile(f);
  PKGWriteForm.PKGWriteCmd('PKGFileName '+edit1.text+'.dic');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name '+edit2.text);
  PKGWriteForm.PKGWriteCmd('TitleName '+edit2.text+' Dictionary');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName '+edit5.text);
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
  PKGWriteForm.PKGWriteCmd('Include '+tempDir);
  PKGWriteForm.PKGWriteCmd('Finish');
  DeleteDirectory(tempDir);
end;

procedure EnsortIndex(sl,sl2:TStringList;des,ind:string);
var i:integer;
    s:string;
    f:boolean;
begin
  i:=sl.IndexOf(des);
  s:='';
  if i<>-1 then s:=sl2[i];
  f:=false;
  if copy(s,length(s)-7,8)=ind then f:=true;
  if not f then
  begin
    s:=s+ind;
    if i=-1 then
    begin
      i:=sl.Add(des);
      sl2.Insert(i,s);
    end else sl2[i]:=s;
  end;
end;

function CustomSortCompare(list:TStringList;index1,index2:integer):integer;
begin
  result:=CompareStr(uppercase(list[index1]),uppercase(list[index2]));
end;

procedure TfDictImport.BitBtn1Click(Sender: TObject);
var phase:integer;
    fi:integer;
    fname:string;
    prog:TSMPromptForm;
    s:string;
    fo:textfile;
    fb:file;
    abort:boolean;
    cd:string;
    lpi:PROCESS_INFORMATION;
    si:STARTUPINFO;
    buf:array[0..3999] of byte;
    bufc:integer;
    bufp:integer;
    buff:file;
    cnt:integer;
    kanji,phon,writ:string;
    feof:boolean;
    lreat:boolean;
    uni,asc:string;
    ppp:integer;
    mes,s2,s3,s4:string;
    bl,bh:byte;
    beg:boolean;
    cnt2:integer;
    dic:TJaletDic;
    diclang:char;
    romap:textfile;
    pphon:string;
    prior:integer;
    skk,skk2:string;
    skki,skkj:integer;
    wordidx,charidx,wordidx2,charidx2:TStringList;
    hexacnt:string;
    linecount,lineno:integer;
    i,j:integer;
    freql:TStringList;
    freqf:file;
    freqi:integer;
    newline,nownum,comment:boolean;
    addkan,addnum:string;
procedure PutToBuf(b1,b2,b3,b4:byte);
begin
  buf[bufp]:=b1;
  buf[bufp+1]:=b2;
  buf[bufp+2]:=b3;
  buf[bufp+3]:=b4;
  inc(bufp,4);
  if bufp=4000 then begin
    blockwrite(fb,buf,4000);
    bufp:=0;
  end;
end;
procedure PutToBufL(l:integer);
var b:array[0..3] of byte;
begin
  move(l,b,4);
  PutToBuf(b[0],b[1],b[2],b[3]);
end;
begin
  prog:=SMProgressDlg(_l('#00071^eDictionary import'),
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',100);
  abort:=false;
  case RadioGroup2.ItemIndex of
    0:diclang:='j';
    1:diclang:='c';
  end;
  wordidx:=TStringList.Create;
  charidx:=TStringList.Create;
  wordidx2:=TStringList.Create;
  charidx2:=TStringList.Create;
  freql:=TStringList.Create;
  linecount:=0;
  if not FileExists('wordfreq_ck.uni') and CheckBox3.Checked then
  begin
    Application.MessageBox(
      pchar(_l('#00915^eCannot find WORDFREQ_CK.UNI file.')),
      pchar(_l('#00916^eError')),
      MB_ICONERROR or MB_OK);
    exit;
  end;
  try
  if CheckBox3.Checked then
  begin
    prog.SetMessage(_l('#00917^eCreating frequency chart...'));
    Conv_Open('wordfreq_ck.uni',1);
    s:=Conv_Read;
    newline:=true;
    nownum:=false;
    addkan:='';
    addnum:='';
    comment:=false;
    while s<>'' do
    begin
      if (newline) and (s=UnicodeToHex('#')) then comment:=true;
      newline:=false;
      if s='000A'then
      begin
        if not comment and (addkan<>'') and (addnum<>'') then
        begin
          freql.AddObject(addkan,TObject(strtoint(addnum)));
        end;
        addkan:='';
        addnum:='';
        newline:=true;
        comment:=false;
        nownum:=false;
      end else if not comment and (s='0009') then nownum:=true else
      if not comment then if nownum then
      begin
        if (s>=UnicodeToHex('0')) and (s<=UnicodeToHex('9')) then addnum:=addnum+HexToUnicode(s);
      end else addkan:=addkan+s;
      s:=Conv_Read;
    end;
  end;
  freql.Sorted:=true;
  freql.Sort;
  except
    Application.MessageBox(
      pchar('Frequency list creation failed. Exception:'+(ExceptObject as Exception).Message),
      'Error',
      MB_ICONERROR or MB_OK);
  end;
  assignfile(romap,'roma_problems.txt');
  rewrite(romap);
  for phase:=0 to 1 do
  begin
    lineno:=0;
    cnt:=0;
    wordidx.Sorted:=true;
    charidx.Sorted:=true;
    if phase>0 then assignfile(fo,'DICT_IMP'+inttostr(phase)+'.TMP');
    if phase>0 then rewrite(fo);
    case phase of
      1:begin
          writeln(fo,'>Index;English;Phonetic;Kanji;Sort;Markers;Frequency');
        end;
    end;
    for fi:=0 to ListBox1.Items.Count-1 do if not abort then
    begin
      fname:='DICT_'+inttostr(fi)+'.TMP';
      case phase of
        0:mes:=_l('#00085^eConverting ');
        1:mes:=_l('#00086^eReading && parsing ');
      end;
      prog.SetMessage(mes+ListBox1.Items[fi]+'...');
      if phase=0 then
      begin
        fDictCoding.Label2.Caption:=_l('#00087^eInput file: ')+ListBox1.Items[fi];
        if paramstr(1)<>'makedic'then fDictCoding.ShowModal;
        if (not fDictCoding.succeeded) and (paramstr(1)<>'makedic') then abort:=true else
        begin
          if paramstr(1)='makedic'then fDictCoding.RadioGroup1.ItemIndex:=1+RadioGroup2.ItemIndex;
          if fDictCoding.RadioGroup1.ItemIndex=0 then CopyFile(pchar(ListBox1.Items[fi]),pchar(fname),false) else
          begin
            case fDictCoding.RadioGroup1.ItemIndex of
              1:cd:='JapaneseAutoDetect';
              2:cd:='ChineseAutoDetect';
              3:cd:='KoreanAutoDetect';
            end;
            si.dwFlags:=STARTF_USESHOWWINDOW;
            si.wShowWindow:=SW_HIDE;
            if CreateProcess(nil,pchar('UNICONV.EXE '+cd+' "'+ListBox1.Items[fi]+'" UCS2 '+fname),nil,nil,false,0,nil,nil,si,lpi) then
            begin
              WaitForSingleObject(lpi.hProcess,300000);
//              sleep(10000);
            end;
          end;
          if not FileExists(fname) then
          begin
            Application.MessageBox(
              pchar(_l('^eFile conversion failed ('+ListBox1.Items[fi]+').')),
              pchar(_l('#00020^eError')),
              MB_ICONERROR or MB_OK);
            abort:=true;
          end;
        end;
        assignfile(buff,fname);
        reset(buff,1);
        feof:=false;
        while not feof do
        begin
          blockread(buff,buf,4000,bufc);
          if bufc<4000 then feof:=true;
          for i:=0 to (bufc div 2)-1 do
          begin
            bl:=buf[i*2];
            bh:=buf[i*2+1];
            uni:=format('%4.4X',[bh*256+bl]);
            if uni='000A'then inc(linecount);
          end;
        end;
        closefile(buff);
      end;
      if phase>0 then
      begin
        assignfile(buff,fname);
        reset(buff,1);
        bufc:=0;
        bufp:=0;
        cnt2:=0;
        blockread(buff,buf,2);
        if (buf[0]<>255) or (buf[1]<>254) then
        begin
          Application.MessageBox(
            pchar(_l('#00088^eUnsupported file coding ('+ListBox1.Items[fi]+').')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          abort:=true;
        end else
        begin
          feof:=false;
          lreat:=false;
          while (bufp<bufc) or (not feof) do
          begin
            lreat:=false;
            kanji:='';
            phon:='';
            writ:='';
            ppp:=0;
            while not lreat do
            begin
              if (bufp>=bufc) and (not feof) then
              begin
                blockread(buff,buf,4000,bufc);
                if bufc<4000 then feof:=true;
                bufp:=0;
              end;
              if bufp>=bufc then lreat:=true else
              begin
                bl:=buf[bufp];
                bh:=buf[bufp+1];
                inc(bufp,2);
                uni:=format('%4.4X',[bh*256+bl]);
                if bh>0 then asc:=' 'else asc:=chr(bl);
                if uni='000A'then lreat:=true;
                if (uni<>'000A') and (uni<>'000D') then
                begin
                  if ppp=0 then
                  begin
                    if uni='0020'then ppp:=1 else kanji:=kanji+uni;
                  end else if ppp=1 then
                  begin
                    if uni='005B'then ppp:=2 else ppp:=3;
                  end else if ppp=2 then
                  begin
                    if uni='002F'then ppp:=3 else if (uni<>'005D') and (uni<>'0020') then phon:=phon+uni;
                  end else writ:=writ+asc;
                end;
              end;
            end;
            if (linecount>0) and (lineno mod 100=0) then prog.SetProgress(round(lineno/linecount*100));
            inc(lineno);
            if (length(writ)>0) and (writ[length(writ)]='/') then delete(writ,length(writ),1);
            if cnt mod 100=0 then prog.SetMessage(mes+ListBox1.Items[fi]+' ('+inttostr(cnt)+')...');
            if phon='' then phon:=kanji;
            inc(cnt);
            if (pos('1FFF',kanji)=0) and (pos('0023',kanji)=0) then
              case phase of
                1:begin
                    pphon:=phon;
                    if diclang='c'then
                    begin
                      repl(phon,' ','');
                      if phon<>kanji then phon:=RomajiToKana(HexToUnicode(phon),1,false,diclang);
                    end;
                    s4:=KanaToRomaji(phon,1,diclang);
                    if pos('?',s4)>0 then
                    begin
                      writeln(romap,writ);
                      writeln(romap,s4);
                      writeln(romap,string(HexToUnicode(pphon)));
                    end;
//                      Application.MessageBox(pchar(_l('#00089^eCannot romanize following line:')+#13+#13+s2+#13+s4),
//                       pchar(_l('#00090^eWarning')),MB_ICONWARNING or MB_OK);
                    repl(s4,'?','');
                    if s4='' then s4:='XXX';
                    s3:=ConvertEdictEntry(writ,s2);
                    repl(s3,';',',');
//                    if length(s3)<3 then showmessage(s3+#13+writ);
                    prior:=0;
{                    if diclang='j'then
                    begin
                      TKanaKanji.SetOrder('Kana_Ind');
                      if TKanaKanji.Locate('Kana',phon,false) then
                      begin
                        skk:=TKanaKanji.Str(TKanaKanji.Field('Kanji'))+'0000';
                        skkj:=1;
                        while skk<>'' do
                        begin
                          for skki:=1 to length(skk) div 4 do if copy(skk,skki*4-3,4)='0000'then
                          begin
                            skk2:=copy(skk,1,skki*4-4);
                            delete(skk,1,skki*4);
                            if (skk2=kanji) and (prior=0) then prior:=skkj;
                            inc(skkj);
                            break;
                          end;
                        end;
                        if prior=0 then prior:=99;
                      end;
                    end;}
                    if CheckBox3.Checked then
                    begin
                      freqi:=freql.IndexOf(kanji);
                      if freqi<>-1 then prior:=integer(freql.Objects[freqi]);
                    end;
                    if s4<>'' then writeln(fo,'+'+inttostr(cnt)+';'+s3+';'+phon+';'+kanji+';'+s4+';'+s2+';'+inttostr(prior));
                    s:=kanji;
                    hexacnt:=format('%8.8X',[cnt]);;
                    if CheckBox2.Checked then
                    begin
                      while s<>'' do
                      begin
                        s2:=copy(s,1,4);
                        delete(s,1,4);
                        if TChar.Locate('Unicode',s2,false) then EnsortIndex(charidx,charidx2,s2,hexacnt);
                      end;
                    end;
                    s:=writ;
                    if CheckBox1.Checked then
                    begin
                      while s<>'' do
                      begin
                        s2:=split(s,'/');
                        if s2='' then
                        begin
                          s2:=s;
                          s:='';
                        end;
                        if s2<>'' then
                        begin
                          s2:=lowercase(s2);
                          while s2<>'' do
                          begin
                            while (length(s2)>0) and (s2[1]='(') do
                            begin
                              if pos(')',s2)>0 then delete(s2,1,pos(')',s2))
                              else s2:='';
                              while (length(s2)>0) and (s2[1]=' ') do delete(s2,1,1);
                            end;
                            s3:=split(s2,' ');
                            if s3='' then
                            begin
                              s3:=s2;
                              s2:='';
                            end;
                            repl(s2,';',',');
                            if ignorel.IndexOf(s3)=-1 then
                            begin
                              if length(s3)>4 then s3:=copy(s3,1,4);
                              if (length(s3)>0) and (s3[1]<>' ') and (s3[1]<>'"') and (s3[1]<>'-') and
                                (s3[1]<>'.') and ((s3[1]<'0') or (s3[1]>'9')) then EnsortIndex(wordidx,wordidx2,s3,hexacnt);
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                2:begin
                  end;
                3:begin
                  end;
              end;
          end;
        end;
        closefile(buff);
        entries:=cnt;
      end;
    end;
    if phase>0 then writeln(fo,'.');
    if phase>0 then closefile(fo);
    if phase>0 then
    begin
    end;
  end;
  closefile(romap);
  if not abort then
  begin
    CreateDictTables(diclang);
    dic:=TJaletDic.Create;
    dic.FillInfo(edit1.text+'.dic');
    if not dic.tested then abort:=true;
    if not abort then dic.Load;
    if not dic.loaded then abort:=true;
    dic.Demand;
    assignfile(fo,'DICT_IMP1.TMP');
    reset(fo);
    dic.TDict.ImportFromText(fo,prog,_l('#00091^eBuilding dictionary table'));
    closefile(fo);
    {$I-}
    mkdir('dict');
    {$I+}
    ioresult;
    prog.SetMessage(_l('#00092^eSorting indexes...'));
    wordidx.Sorted:=false;
    charidx.Sorted:=false;
    for i:=0 to wordidx.Count-1 do while length(wordidx[i])<4 do wordidx[i]:=wordidx[i]+' ';
    for i:=0 to wordidx.Count-1 do wordidx[i]:=wordidx[i]+'    '+wordidx2[i];
    for i:=0 to charidx.Count-1 do charidx[i]:=charidx[i]+'    '+charidx2[i];
    wordidx.CustomSort(CustomSortCompare);
    charidx.CustomSort(CustomSortCompare);
    prog.SetMessage(_l('^eWriting character index...'));
    assignfile(fb,'dict\CharIdx.bin');
    bufp:=0;
    rewrite(fb,1);
    PutToBufL(charidx.Count);
    j:=0;
    for i:=0 to charidx.Count-1 do
    begin
      PutToBuf(ord(charidx[i][1]),ord(charidx[i][2]),ord(charidx[i][3]),ord(charidx[i][4]));
      PutToBufL(j);
      inc(j,(length(charidx[i]) div 8)-1);
    end;
    for i:=0 to charidx.Count-1 do for j:=1 to (length(charidx[i]) div 8)-1 do
    begin
      PutToBuf(strtoint('0x'+copy(charidx[i],j*8+7,2)),strtoint('0x'+copy(charidx[i],j*8+5,2)),
               strtoint('0x'+copy(charidx[i],j*8+3,2)),strtoint('0x'+copy(charidx[i],j*8+1,2)));
    end;
    blockwrite(fb,buf,bufp);
    closefile(fb);
    prog.SetMessage(_l('^eWriting word index...'));
    assignfile(fb,'dict\WordIdx.bin');
    bufp:=0;
    rewrite(fb,1);
    PutToBufL(wordidx.Count);
    j:=0;
    for i:=0 to wordidx.Count-1 do
    begin
      PutToBuf(ord(wordidx[i][1]),ord(wordidx[i][2]),ord(wordidx[i][3]),ord(wordidx[i][4]));
      PutToBufL(j);
      inc(j,(length(wordidx[i]) div 8)-1);
//      if length(wordidx[i])>4000 then showmessage('Consider "'+copy(wordidx[i],1,4)+'"');
    end;
    for i:=0 to wordidx.Count-1 do for j:=1 to (length(wordidx[i]) div 8)-1 do
    begin
      PutToBuf(strtoint('0x'+copy(wordidx[i],j*8+7,2)),strtoint('0x'+copy(wordidx[i],j*8+5,2)),
               strtoint('0x'+copy(wordidx[i],j*8+3,2)),strtoint('0x'+copy(wordidx[i],j*8+1,2)));
    end;
    blockwrite(fb,buf,bufp);
    closefile(fb);
    dic.TDict.WriteTable('dict\Dict',true);
    dic.Free;
    WriteDictPackage(diclang);
//    showmessage('check prepared dic!');
    DeleteFile('dict\Dict.info');
    DeleteFile('dict\Dict.data');
    DeleteFile('dict\Dict.index');
    DeleteFile('dict\Dict.struct');
    DeleteFile('dict\WordIdx.bin');
    DeleteFile('dict\CharIdx.bin');
    DeleteFile('dict\dict.ver');
    {$I-}
    rmdir('dict');
    {$I+}
    ioresult;
  end;
  wordidx.Free;
  charidx.Free;
  wordidx2.Free;
  charidx2.Free;
  freql.Free;
  for fi:=0 to ListBox1.Items.Count-1 do
  begin
    fname:='DICT_'+inttostr(fi)+'.TMP';
    DeleteFile(fname);
  end;
  for phase:=1 to 3 do DeleteFile('DICT_IMP'+inttostr(phase)+'.TMP');
  prog.Free;
  if not abort then
  begin
    close;
    if paramstr(1)<>'makedic' then
      Application.MessageBox(
        pchar(_l('#00093^eDictionary was built.')),
        pchar(_l('#00094^eSuccess')),
        MB_ICONINFORMATION or MB_OK);
  end;
end;

end.
