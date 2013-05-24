unit JWBWords;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, Buttons, RXCtrls, Tabs, CheckLst, JWBStrings, Menus,
  WakanWordGrid, StdPrompt, WakanPaintbox;

type
  TMoveDirection = (mdUp, mdDown);

  TfWords = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    pnlDockFilters: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    RxLabel1: TRxLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Button15: TButton;
    Button18: TButton;
    Button19: TButton;
    pnlDockDetails: TPanel;
    pnlDockExamples: TPanel;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    miResetColumns: TMenuItem;
    splDockFilters: TSplitter;
    splDockDetails: TSplitter;
    BlankPanel1: TBlankPanel;
    StringGrid1: TWakanGrid;
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure UserAdd_Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Button14Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure Button19Click(sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure miResetColumnsClick(Sender: TObject);
    procedure StringGrid1ControlResize(Sender: TObject);

  public
    procedure SetDefaultColumnWidths;
    procedure AutoSizeColumns;

  public
    procedure SetWordsLearnState(ls: integer);
    procedure AddWordsToCategory(category: string);
    procedure MoveWordsInCategory(moveDir: TMoveDirection);
    procedure RemoveWordsFromCategory();
    procedure DeleteWords();

  public
    procedure ExportVocab;
    procedure ExportVocabToWkl(const filename: string);
    procedure ExportVocabToCsv(const filename: string);
    procedure ImportVocab;
    procedure ImportVocabFromWkl(const filename: string; out catw, addw: integer);
    procedure ImportVocabFromCsv(const filename: string; out catw, addw: integer);

  public
    WordListCount:integer;
    twkanji,twphonetic,twmeaning:string;
    procedure ShowIt(warningifnotfound:boolean);
    procedure Reset;
    function AddWord(kanji,phonetic,english,category:string;cattype:char;nomessages:boolean;status:integer):boolean;
    function LLGoNext(page:integer):integer;
    function LLGoPrev(page:integer):integer;
    procedure BuildWordList;
    procedure PrepareTestWord;
    procedure ShowTestWord;
    function FinishTestWord(button:integer):boolean;
    procedure SaveTestResults;
    procedure PrintWordList;
    procedure SaveWordList;
    procedure UpdateWordListStats;
    procedure SearchWord(wordind:integer);
    procedure BuildStrokeOrderPackage(sourceCsv: string);

  public
    curword:integer; //selected word ID, or -1 if not selected/multiple selected
    curphonetic:string;
    curkanji:string;

  end;

 { I have no idea wat iz this or how does it word, but I moved it to a separate class }

  TAddWordFast = class
  protected
    awf_lastcatname:string;
    awf_lastcatindex:integer;
    awf_catcount:array[0..65535] of integer;
    awf_insuser:TStringList;
    awf_insusersheet:TStringList;
    awf_insuseridx:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddWordFast(kanji,phonetic,english,category:string;cattype:char;status:integer):boolean;
    procedure Commit(sp: TSMPromptForm);
  end;

var
  fWords: TfWords;

const
  eWordNotLocated='INTERNAL ERROR. WORD NOT LOCATED'; //wakan likes this string


implementation

uses JWBMenu, JWBKanaConv, JWBUnit, JWBNewCategory, JWBPrint, JWBSettings,
  JWBWordList, JWBUserDetails, JWBUserAdd,
  JWBUserFilters, JWBExamples, JWBUser, JWBUserData,
  JWBConvert, JWBWordsExpChoose, JWBCategories, JWBAnnotations, PKGWrite,
  JWBCharData;

var wl,wlc:TStringList;
    ll,ltl:TStringList;
    lastwordind:integer;
    twi:integer;
    twchg:array[0..3,0..3] of integer;
    lastwordadded:boolean;

{$R *.DFM}

procedure TfWords.FormCreate(Sender: TObject);
begin
  wl:=TStringList.Create;
  wlc:=TStringList.Create;
  ll:=TStringList.Create;
  ltl:=TStringList.Create;
  lastwordind:=0;
end;

procedure TfWords.FormDestroy(Sender: TObject);
begin
  wl.Free;
  wlc.Free;
  ll.Free;
  ltl.Free;
end;

procedure TfWords.FormShow(Sender: TObject);
begin
  ShowIt(false);
end;

procedure TfWords.SetDefaultColumnWidths;
begin
  StringGrid1.ColWidths[0]:=110;
  StringGrid1.ColWidths[1]:=138;
  StringGrid1.ColWidths[2]:=306;
  StringGrid1.ColWidths[3]:=159;
  AutoSizeColumns;
end;

procedure TfWords.AutoSizeColumns;
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[3]-StringGrid1.ColWidths[0]-20;
end;

procedure TfWords.StringGrid1ControlResize(Sender: TObject);
begin
  AutoSizeColumns;
end;

procedure TfWords.PopupMenu1Popup(Sender: TObject);
var p: TPoint;
  ACol, ARow: integer;
begin
  p := StringGrid1.ScreenToClient(Mouse.CursorPos);
  StringGrid1.MouseToCell(p.X, p.Y, ACol, ARow);
  miResetColumns.Visible := (ARow=0); //click on header
end;

procedure TfWords.miResetColumnsClick(Sender: TObject);
begin
  SetDefaultColumnWidths;
end;

procedure TfWords.ShowIt(warningifnotfound:boolean);
var sl:TStringList;
    stp:string;
    cl:TStringList;
    i,j:integer;
    sw:integer;
    s:string;
    a:integer;
    cats:string;

  procedure AddVocabWord(CategoryOrder:boolean);
  var cat_str: string;
    i: integer;
  begin
   //Skip word if it's filtered out
    if ((not fUserFilters.cbFilterUnlearned.Checked) and (TUser.Int(TUserScore)=1))
    or ((not fUserFilters.cbFilterLearned.Checked) and (TUser.Int(TUserScore)=2))
    or ((not fUserFilters.cbFilterMastered.Checked) and (TUser.Int(TUserScore)=3))
    or ((not fUserFilters.cbFilterProblematic.Checked) and (TUser.Int(TUserScore)=0)) then exit;

    stp:=TUser.Str(TUserScore);
    if not CategoryOrder then begin
      ListWordCategories(TUser.Int(TUserIndex),cl);
      if not fUserFilters.CheckEnabledCategories(cl) then exit;
      cat_str:='';
      for i:=0 to cl.Count-1 do cat_str:=cat_str+', '+StripCatName(cl[i]);
      if length(cat_str)>0 then delete(cat_str,1,2);
    end else
      cat_str := cats+' #'+inttostr(j); //sic! not a markup element, just a visual one

   AddWordGrid(StringGrid1,
      ALTCH_EXCL+stp+CheckKnownKanji(TUser.Str(TUserKanji)),
      ALTCH_EXCL+stp+TUser.Str(TUserPhonetic),
      ALTCH_EXCL+stp+FixVocabEntry(TUser.Str(TUserEnglish)),
      ALTCH_EXCL+stp+cat_str);

    wl.Add(TUser.Str(TUserIndex));
    if CategoryOrder then
      wlc.Add(inttostr(a));
    if TUser.Int(TUserIndex)=lastwordind then sw:=wl.Count;
  end;

begin
  if not fWords.Visible then exit;
  wl.Clear;
  wlc.Clear;
  sw:=0;
  cl:=TStringList.Create;
  sl:=TStringList.Create;
  InitWordGrid(StringGrid1,true,false);

  if fUserFilters.rgSort.ItemIndex>0 then begin

    case fUserFilters.rgSort.ItemIndex of
      1:TUser.SetOrder('Phonetic_Ind');
      2:TUser.SetOrder('Kanji_Ind');
      3:TUser.SetOrder('English_Ind');
      4:TUser.SetOrder('Added_Ind');
      5:TUser.SetOrder('Score_Ind');
    end;
    TUser.First;
    while not TUser.EOF do begin
      AddVocabWord({CategoryOrder=}false);
      TUser.Next;
    end;

  end else

    for i:=0 to fUserFilters.lbCategories.Items.Count-1 do
    if fUserFilters.lbCategories.Checked[i] then
    begin
      cats:=fUserFilters.lbCategories.Items[i];
      TUserSheet.SetOrder('Sheet_Ind');
      a := GetCatIdx(fUserFilters.lbCategories,i);
      TUserSheet.Locate('Number',a);
      j:=0;
      while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetNumber)=a) do begin
        inc(j);
        TUser.Locate('Index',TUserSheet.Int(TUserSheetWord));
        AddVocabWord({CategoryOrder=}true);
        TUserSheet.Next;
      end;
    end;

  FinishWordGrid(StringGrid1);
  Reset;
  if (sw=0) and (warningifnotfound) then
    Application.MessageBox(
      pchar(_l('^eThe searched word does not match the current filters.')),
      pchar(_l('#00937^eWord not found')),MB_ICONWARNING or MB_OK);
  if sw=0 then sw:=1;
  if StringGrid1.Visible then StringGrid1Click(self);
  if StringGrid1.Visible then StringGrid1.Row:=sw;
  s:=_l('#00938^eDict');
  s:=s+' ('+inttostr(wl.Count)+')';
  RxLabel1.Caption:=s;
  cl.Free;
  sl.Free;
  WordListCount:=wl.Count;
  Button18.Enabled:=StringGrid1.Visible;
  Button19.Enabled:=StringGrid1.Visible;
  if StringGrid1.Visible then StringGrid1.SetFocus;
end;

procedure TfWords.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(StringGrid1,ACol,ARow,Rect,State);
end;

function TfWords.AddWord(kanji,phonetic,english,category:string;cattype:char;nomessages:boolean;status:integer):boolean;
var s,s2:string;
    beg,insertword:boolean;
    bs:string;
    cat,catord:integer;
    wordidx:integer;
    phonsort:string;
begin
  if (length(category)<2) or (category[2]<>'~') then category:=curlang+'~'+category;
  result:=false;
  if kanji='' then
  begin
    if not nomessages then
      Application.MessageBox(
        pchar(_l('^eWriting is not filled. Cannot add word.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
    exit;
  end;
  if phonetic='' then
  begin
    if not nomessages then
      Application.MessageBox(
        pchar(_l('#00840^ePhonetic is not filled. Cannot add word.')),
        pchar(_l('#00020^eError')),MB_ICONERROR or MB_OK);
    exit;
  end;
  if english='' then
  begin
    if not nomessages then
      Application.MessageBox(
        pchar(_l('#00841^eMeaning is not filled. Cannot add word.')),
        pchar(_l('#00020^eError')),MB_ICONERROR or MB_OK);
    exit;
  end;
  if category='' then
  begin
    if not nomessages then
      Application.MessageBox(
        pchar(_l('#00842^eCategory is not filled. Cannot add word.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
    exit;
  end;

  cat := NeedCategoryUI(category, cattype, {silent=}nomessages);
  if cat<0 then //user cancelled
    exit;

  wordidx := FindUserWord(kanji, phonetic);
  insertword := wordidx<0;

  catord:=1;

  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.Locate('Number',cat);
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetNumber)=cat) do
  begin
    if TUserSheet.Int(TUserSheetPos)>=catord then catord:=TUserSheet.Int(TUserSheetPos)+1;
    if TUserSheet.Int(TUserSheetWord)=wordidx then
    begin
      if not nomessages then
        Application.MessageBox(
          pchar(_l('#00843^eThis word is already in vocabulary and is in this category.')),
          pchar(_l('#00020^eError')),
          MB_ICONERROR or MB_OK);
      exit;
    end;
    TUserSheet.Next;
  end;

  if insertword then
  begin
    inc(MaxUserIndex);
    wordidx:=MaxUserIndex;
    phonsort:=GetPhoneticSortStr(phonetic,curlang);
    TUser.Insert([inttostr(MaxUserIndex),english,phonetic,phonsort,
      kanji,FormatDateTime('yyyymmdd',now),'00000000','00000000','00000000','0',inttostr(status),inttostr(status)]);
    s:=kanji;
    beg:=true;
    while s<>'' do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2) then
      begin
        if beg then bs:='T'else bs:='F';
        TUserIdx.Insert([inttostr(MaxUserIndex),TChar.Str(TCharUnicode),bs]);
      end;
      beg:=false;
    end;
  end;

  TUserSheet.Insert([inttostr(wordidx),inttostr(cat),inttostr(catord)]);

  if (not nomessages) and (fSettings.CheckBox70.Checked) then if insertword then
    Application.MessageBox(
      pchar(_l('#00844^eNew word was successfully added into vocabulary.')),
      pchar(_l('#00845^eSuccess')),
      MB_ICONINFORMATION or MB_OK)
  else
    Application.MessageBox(
      pchar(_l('#00846^eWord was added into new category.')),
      pchar(_l('#00845^eSuccess')),
      MB_ICONINFORMATION or MB_OK);
  if not nomessages then ShowIt(false);
  if not nomessages then fMenu.ChangeUserData;
  lastwordadded:=insertword;
  result:=true;
end;

constructor TAddWordFast.Create;
var i: integer;
begin
  inherited;
  awf_lastcatname:='';
  awf_lastcatindex:=0;
  for i:=0 to 65535 do awf_catcount[i]:=0;
  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.First;
  while not TUserSheet.EOF do
  begin
    if TUserSheet.Int(TUserSheetPos)>awf_catcount[TUserSheet.Int(TUserSheetNumber)] then
      awf_catcount[TUserSheet.Int(TUserSheetNumber)]:=TUserSheet.Int(TUserSheetPos);
    TUserSheet.Next;
  end;

  awf_insuser:=TStringList.Create;
  awf_insusersheet:=TStringList.Create;
  awf_insuseridx:=TStringList.Create;
end;

destructor TAddWordFast.Destroy;
begin
  awf_insuser.Free;
  awf_insuseridx.Free;
  awf_insusersheet.Free;
  inherited;
end;

function TAddWordFast.AddWordFast(kanji,phonetic,english,category:string;cattype:char;status:integer):boolean;
var s,s2:string;
    beg,insertword:boolean;
    bs:string;
    cat,catord:integer;
    wordidx:integer;
    phonsort:string;
begin
  if (length(category)<2) or (category[2]<>'~') then category:=curlang+'~'+category;
  result:=false;
  if category=awf_lastcatname then
    cat:=awf_lastcatindex
  else
  begin
    cat := NeedCategoryUI(category, cattype, {silent=}true);
    if cat<0 then //user cancelled
      raise EAbort.Create('User cancelled.'); //do not localize
  end;

  wordidx := FindUserWord(kanji, phonetic);
  insertword := wordidx<0;

  awf_lastcatname:=category;
  awf_lastcatindex:=cat;
  catord:=awf_catcount[cat]+1;
  awf_catcount[cat]:=catord;

  TUserSheet.SetOrder('Word_Ind');
  if not insertword then
  begin
    TUserSheet.Locate('Word',wordidx);
    while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=wordidx) do
    begin
      if TUserSheet.Int(TUserSheetNumber)=cat then
      begin
       //Already in vocab and in this category
        lastwordadded:=insertword;
        exit;
      end;
      TUserSheet.Next;
    end;
  end;

  if insertword then
  begin
    inc(MaxUserIndex);
    wordidx:=MaxUserIndex;
    phonsort:=GetPhoneticSortStr(phonetic,curlang);
    awf_insuser.Add(inttostr(MaxUserIndex));
    awf_insuser.Add(english);
    awf_insuser.Add(phonetic);
    awf_insuser.Add(phonsort);
    awf_insuser.Add(kanji);
    awf_insuser.Add(FormatDateTime('yyyymmdd',now));
    awf_insuser.Add('00000000');
    awf_insuser.Add('00000000');
    awf_insuser.Add('00000000');
    awf_insuser.Add('0');
    awf_insuser.Add(inttostr(status));
    awf_insuser.Add(inttostr(status));
    s:=kanji;
    beg:=true;
    while s<>'' do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2) then
      begin
        if beg then bs:='T'else bs:='F';
        awf_insuseridx.Add(inttostr(MaxUserIndex));
        awf_insuseridx.Add(TChar.Str(TCharUnicode));
        awf_insuseridx.Add(bs);
      end;
      beg:=false;
    end;
  end;
  awf_insusersheet.Add(inttostr(wordidx));
  awf_insusersheet.Add(inttostr(cat));
  awf_insusersheet.Add(inttostr(catord));
  lastwordadded:=insertword;
  result:=true;
end;

procedure TAddWordFast.Commit(sp: TSMPromptForm);
var i: integer;
begin
  TUser.nocommitting:=true;
  TUserSheet.nocommitting:=true;
  TUserIdx.nocommitting:=true;
  sp.SetMessage(_l('#00904^eBatch-adding words ')+'('+inttostr(awf_insuser.Count div 12)+')');
  for i:=0 to (awf_insuser.Count div 12)-1 do
    TUser.Insert([awf_insuser[i*12],awf_insuser[i*12+1],awf_insuser[i*12+2],awf_insuser[i*12+3],awf_insuser[i*12+4],awf_insuser[i*12+5],
                  awf_insuser[i*12+6],awf_insuser[i*12+7],awf_insuser[i*12+8],awf_insuser[i*12+9],awf_insuser[i*12+10],awf_insuser[i*12+11]]);
  sp.show;
  sp.SetMessage(_l('#00905^eBatch-adding word categories ')+'('+inttostr(awf_insusersheet.Count div 3)+')');
  for i:=0 to (awf_insusersheet.Count div 3)-1 do
    TUserSheet.Insert([awf_insusersheet[i*3],awf_insusersheet[i*3+1],awf_insusersheet[i*3+2]]);
  sp.show;
  sp.SetMessage(_l('#00906^eBatch-adding character indexes ')+'('+inttostr(awf_insuseridx.Count div 3)+')');
  for i:=0 to (awf_insuseridx.Count div 3)-1 do
    TUserIdx.Insert([awf_insuseridx[i*3],awf_insuseridx[i*3+1],awf_insuseridx[i*3+2]]);
  TUser.nocommitting:=false;
  TUserSheet.nocommitting:=false;
  TUserIdx.nocommitting:=false;
end;

procedure TfWords.UserAdd_Button1Click(Sender: TObject);
begin
  if AddWord(clip,RomajiToKana(fUserAdd.Edit1.Text,romasys,curlang,[rfDeleteInvalidChars]),
    fUserAdd.Edit3.Text,fUserAdd.ComboBox1.Text,'?',false,1) then
  begin
    fUserAdd.Edit1.Text:='';
    fUserAdd.PaintBox3.Invalidate;
    fUserAdd.Edit3.Text:='';
  end;
end;

procedure TfWords.ExportVocab;
begin
  if SaveDialog1.Execute then
    if pos('.WKL',uppercase(SaveDialog1.FileName))>0 then
      ExportVocabToWkl(SaveDialog1.FileName)
    else
      ExportVocabToCsv(SaveDialog1.FileName);
end;

procedure TfWords.ExportVocabToWkl(const filename: string);
var sp: TSMPromptForm;
  t:textfile;
  i,j:integer;
  sl:TStringList;
begin
  Application.MessageBox(
    pchar(_l('#00899^eWKL format is outdated. Import/export routine is '
      +'maintained for compatibility only. Please use CSV format in the future.'#13)),
    pchar(_l('#00900^eNotice')),
    MB_ICONWARNING or MB_OK);

  Screen.Cursor:=crHourGlass;
  sp := nil;
  sl := nil;
  try
    sp:=SMProgressDlgCreate(
      _l('#01007^eVocabulary export'),
      _l('#01008^eExporting vocabulary...'),
      wl.Count,
      {canCancel=}true);
    sp.AppearModal;

    assignfile(t,filename);
    rewrite(t);
    writeln(t,'WaKan Word List 1');
    writeln(t,'');
    writeln(t,'; created by '+WakanAppName+' '+WakanCopyright);
    writeln(t,'; This file lists words that were exported from user vocabulary.');
    writeln(t,'; Each entry consists of four lines:');
    writeln(t,'; Written (Unicode in hex), Phonetic (Unicode in hex), English (raw text), Category (raw text)');
    writeln(t,'');
    sl:=TStringList.Create;
    for i:=0 to wl.Count-1 do
    begin
      ListWordCategories(strtoint(wl[i]),sl);
      TUser.Locate('Index',strtoint(wl[i]));
      for j:=0 to fUserFilters.lbCategories.Items.Count-1 do
        if (fUserFilters.lbCategories.Checked[j]) and (sl.IndexOf(curlang+'~'+fUserFilters.lbCategories.Items[j])<>-1) then
      begin
        writeln(t,fstrtohex(TUser.Str(TUserKanji)));
        writeln(t,fstrtohex(TUser.Str(TUserPhonetic)));
        writeln(t,TUser.Str(TUserEnglish));
        TUserCat.Locate('Name',curlang+'~'+fUserFilters.lbCategories.Items[j]);
        writeln(t,chr(TUserCat.Int(TUserCatType))+TUserCat.Str(TUserCatName));
      end;

      sp.SetProgress(i);
      sp.ProcessMessages;
      if sp.ModalResult=mrCancel then
        raise EAbort.Create('User aborted');
    end;
    writeln(t,'.');

  finally
    sl.Free;
    if TTextRec(t).Mode<>fmClosed then
      closefile(t);
    Screen.Cursor:=crDefault;
    sp.Free;
  end;
end;

procedure TfWords.ExportVocabToCsv(const filename: string);
var sp: TSMPromptForm;
  i,j:integer;
  sl:TStringList;
begin
  Screen.Cursor:=crHourGlass;
  sl := nil;
  sp := nil;
  try
    sp:=SMProgressDlgCreate(
      _l('#01007^eVocabulary export'),
      _l('#01008^eExporting vocabulary...'),
      wl.Count,
      {canCancel=}true);
    sp.AppearModal;

    Conv_Create(filename,Conv_ChooseType(curlang='c',0));
    if fWordsExpChoose.ShowModal=mrCancel then exit;
    Conv_Write(fstr(#9' Wakan Word List'#13#10));
    Conv_Write(fstr(#9''#13#10));
    Conv_Write(fstr(#9' created by '+WakanAppName+' '+WakanCopyright+#13#10));
    Conv_Write(fstr(#9' This file lists words that were exported from user vocabulary.'#13#10));
    Conv_Write(fstr(#9' Each entry consists of one line where the following values are separated by a delimiter:'#13#10));
    Conv_Write(fstr(#9' <written>;<phonetic>;<meaning>[;<category>[;<learned>]]'#13#10));
    Conv_Write(fstr(#9' <category> and <learned> fields are optional.'#13#10));
    Conv_Write(fstr(#9' <written> - How the word is written (in kanji/hanzi/kana)'#13#10));
    Conv_Write(fstr(#9' <phonetic> - How the word is pronounced (in kana/Hepburn romaji/BoPoMoFo/PinYin)'#13#10));
    Conv_Write(fstr(#9' <meaning> - English meaning of the word (cannot contain semicolons!)'#13#10));
    Conv_Write(fstr(#9' <category> - Name of the category to place the word into (if new, user is asked to specify type) (optional)'#13#10));
    Conv_Write(fstr(#9' <learned> - Learned state of the word: "P" - problematic, "U" - unlearned, "L" - learned, "M" - mastered (optional)'#13#10));
    Conv_Write(fstr(#9' Delimiter is the first non-kanji character encountered.'#13#10));
    Conv_Write(fstr(#9''#13#10));
    sl:=TStringList.Create;
    for i:=0 to wl.Count-1 do
    begin
      ListWordCategories(strtoint(wl[i]),sl);
      TUser.Locate('Index',strtoint(wl[i]));
      for j:=0 to fUserFilters.lbCategories.Items.Count-1 do
        if (fUserFilters.lbCategories.Checked[j]) and (sl.IndexOf(curlang+'~'+fUserFilters.lbCategories.Items[j])<>-1) then
      begin
        Conv_Write(TUser.Str(TUserKanji));
        Conv_Write(fstr(#9));
        if not showroma then
          Conv_Write(TUser.Str(TUserPhonetic))
        else
          if curlang='c'then
            Conv_Write(fstr(KanaToRomaji(TUser.Str(TUserPhonetic),1,'c')))
          else
            Conv_Write(fstr(KanaToRomaji(TUser.Str(TUserPhonetic),2,'j')));
        Conv_Write(fstr(#9));
        Conv_Write(fstr(replc(TUser.Str(TUserEnglish),';',',')));
        if fWordsExpChoose.RadioGroup1.ItemIndex<2 then
        begin
          Conv_Write(fstr(#9));
          Conv_Write(fstr(fUserFilters.lbCategories.Items[j]));
          if fWordsExpChoose.RadioGroup1.ItemIndex=0 then
          begin
            Conv_Write(fstr(#9));
            case TUser.Int(TUserScore) of
              0:Conv_Write(fstr('P'));
              1:Conv_Write(fstr('U'));
              2:Conv_Write(fstr('L'));
              3:Conv_Write(fstr('M'));
            end;
          end;
        end;
        Conv_Write(fstr(#13#10));
      end;

      sp.SetProgress(i);
      sp.ProcessMessages;
      if sp.ModalResult=mrCancel then
        raise EAbort.Create('User aborted');
    end;
    Conv_Flush;

  finally
    sl.Free;
    if Conv_IsOpen then
      Conv_Close;
    Screen.Cursor:=crDefault;
    sp.Free;
  end;
end;

procedure TfWords.ImportVocab;
var addw,catw:integer;
begin
  if not OpenDialog1.Execute then
    exit;

  if pos('.WKL',uppercase(OpenDialog1.FileName))>0 then
    ImportVocabFromWkl(OpenDialog1.FileName, catw, addw)
  else
    ImportVocabFromCsv(OpenDialog1.FileName, catw, addw);

  //We've been doing silent AddCategories, so refresh now
  fMenu.RefreshCategory;
  fMenu.ChangeUserData;
  ShowIt(false);
  Application.MessageBox(
    pchar(_l('^e'+inttostr(catw)+' words imported'#13+inttostr(addw)+' new words')),
    pchar(_l('#00851^eWord list imported')),
    MB_ICONINFORMATION or MB_OK);
end;

procedure TfWords.ImportVocabFromWkl(const filename: string; out catw, addw: integer);
var t:textfile;
  sp:TSMPromptForm;
  s,s2,s3,s4:string;
  linc:integer;
begin
  Application.MessageBox(
    pchar(_l('#00899^eWKL format is outdated. Import/export routine '
      +'is maintained for compatibility only. Please use CSV format in the future.'#13)),
    pchar(_l('#00900^eNotice')),
    MB_ICONWARNING or MB_OK);

  catw:=0;
  addw:=0;

  sp := nil;
  try
    assignfile(t,filename);
    system.reset(t);
    linc:=0;
    while not eof(t) do
    begin
      readln(t,s);
      inc(linc);
    end;
    closefile(t);

    system.reset(t);
    readln(t,s);
    if pos('WaKan Word List',s)<>1 then
    begin
      Application.MessageBox(
        pchar(_l('#00847^eThis is not a WaKan word list file.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
      exit;
    end;
    if s<>'WaKan Word List 1'then
    begin
      Application.MessageBox(
        pchar(_l('#00848^eThis WaKan word list file version is not supported.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
      exit;
    end;

    sp:=SMProgressDlg(
      _l('#00215^eVocabulary'),
      _l('#00849^eImporting word list (000000 words imported - 000000 new words)...'),
      linc);
    Screen.Cursor:=crHourGlass;

    linc:=0;
    repeat
      inc(linc);
      readln(t,s);
    until (length(s)>0) and (s[1]<>';');

    while (not eof(t)) and (s<>'.') do
    begin
      readln(t,s2);
      readln(t,s3);
      readln(t,s4);
      AddWord(hextofstr(s),hextofstr(s2),s3,copy(s4,2,length(s4)-1),s4[1],true,1);
      inc(catw);
      if lastwordadded then inc(addw);
      if not eof(t) then readln(t,s);
      inc(linc,4);
      sp.show;
      sp.SetMessage(_l('#00901^eImporting word list (')
        +inttostr(catw)
        +_l('#00902^e words imported - ')
        +inttostr(addw)
        +_l('#00903^e new words)...'));
      sp.SetProgress(linc);
    end;
  finally
    if TTextRec(t).Mode<>fmClosed then
      closefile(t);
    Screen.Cursor:=crDefault;
    sp.Free;
  end;
end;

procedure TfWords.ImportVocabFromCsv(const filename: string; out catw, addw: integer);
var awf: TAddWordFast;
  sp:TSMPromptForm;
  linc:integer;
  curwrit,curphon,curmean,curcat,curstat:string;
  delimdetect:boolean;
  delim:string;
  s,s2:string;
  justbeginning,ignoreline:boolean;
  field:integer;
  unknowncat:string;
  st:integer;

  function IsPotentialDelimiter(const ch: WideChar): boolean;
  begin
    Result := (Ord(ch)<$00FF) //All non-ascii characters are NOT delimiters
      and (ch<>' ') //Space is not a delimiter
      and ((ch<'A') or (ch>'Z')) and ((ch<'a') or (ch>'z')) and ((ch<'0') or (ch>'9'));
  end;

  procedure FinalizeField;
  begin
    inc(field);
    case field of
      1:curwrit:=s2; //keen in fstring
      2:curphon:=s2;
      3:curmean:=fstrtouni(s2); //although it says "uni", contents is raw ansi
      4:curcat:=fstrtouni(s2);
      5:curstat:=fstrtouni(s2);
    end;
    s2:='';
  end;

begin
  catw:=0;
  addw:=0;

  awf := nil;
  sp := nil;
  try
    awf := TAddWordFast.Create;
    Conv_Open(filename,Conv_ChooseType(curlang='c',Conv_DetectType(filename)));

    linc:=0;
    repeat
      s:=Conv_Read;
      if s=UH_CR then inc(linc);
    until s='';

    Conv_Rewind;

    curwrit:='';
    curphon:='';
    curmean:='';
    curcat:='';
    curstat:='';
    justbeginning:=true;
    ignoreline:=false;
    field:=0;
    unknowncat:='';
    s2:='';

    sp:=SMProgressDlg(
      _l('#00215^eVocabulary'),
      _l('#00849^eImporting word list (000000 words imported - 000000 new words)...'),
      linc);

    linc:=0;
    Screen.Cursor:=crHourGlass;
    s:=Conv_Read;

    delim:=fstr(';');
    delimdetect:=true;

    repeat
      if delimdetect and IsPotentialDelimiter(fstrtouni(s)[1]) then
      begin
        delim:=s;
        delimdetect:=false;
      end;

      if justbeginning and (s=delim) then ignoreline:=true;
      justbeginning:=false;

      if not ignoreline then
      begin
        if s=delim then
          FinalizeField
        else
        if s=UH_LF then
        begin
          FinalizeField;
          if (curwrit<>'') and (curphon<>'') then
          begin
           //I do not know WTF are both of these checks
            if ({$IFNDEF UNICODE}curphon[1]='0'{$ELSE}Ord(curphon[1]) and $F000 = 0{$ENDIF})
            or (fgetch(curphon,1)={$IFNDEF UNICODE}'2026'{$ELSE}#$2026{$ENDIF}) then
              if curlang='c'then
              begin
                s2:=DeconvertPinYin(romac,curphon);
                curphon:=RomajiToKana(DeconvertPinYin(romac,curphon),1,'c',[rfDeleteInvalidChars])
              end else
                curphon:=RomajiToKana(HexToUnicode(curphon),2,'j',[rfDeleteInvalidChars]);
            if curcat='' then curcat:=unknowncat;
            if curcat='' then
            begin
              curcat:=InputBox(
                _l('#00894^eVocabulary category'),
                _l('#00895^eEnter category in which the new words will be added:'),
                'noname');
              unknowncat:=curcat;
            end;
            if curstat='' then curstat:='U';
            case uppercase(curstat)[1] of
              'P':st:=0;
              'U':st:=1;
              'L':st:=2;
              'M':st:=3;
            else
              st:=1; //unlearned
            end;
            curcat:=trim(curcat);
            awf.AddWordFast(curwrit,curphon,curmean,curcat,'?',st); //can raise EAbort
            inc(catw);
            if lastwordadded then inc(addw);
          end;
        end else
        if s<>UH_CR then
          s2:=s2+s;
      end;

      if s=UH_LF then
      begin
        inc(linc);
        field:=0;
        s2:='';
        justbeginning:=true;
        ignoreline:=false;
        if linc mod 10=0 then
        begin
          sp.show;
          sp.SetMessage(
            _l('#00901^eImporting word list (')
            +inttostr(catw)
            +_l('#00902^e words imported - ')
            +inttostr(addw)
            +_l('#00903^e new words)...'));
          sp.SetProgress(linc);
        end;
      end;

      s:=Conv_Read;
    until s='';
    Conv_Close;

    sp.show;
    awf.Commit(sp);
    FreeAndNil(awf);

    sp.show;
    sp.SetMessage(_l('#00907^eRebuilding indexes'));
    TUser.Reindex;
    TUserSheet.Reindex;
    TUserIdx.Reindex;

  finally
    FreeAndNil(awf);
    Screen.Cursor:=crDefault;
    sp.Free;
  end;
end;

procedure TfWords.Reset;
begin
  curphonetic:='';
  curkanji:='';
  fUserDetails.Reset;
end;

procedure TfWords.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect:=true;
end;

procedure TfWords.RemoveWordsFromCategory;
var i:integer;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
    if Application.MessageBox(
      pchar(_l('#00927^eThis operation will affect multiple words.'#13#13
        +'Do you want to continue?')),
      pchar(_l('#00926^eWarning')),
      MB_ICONWARNING or MB_YESNO)=idNo then exit;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    lastwordind:=strtoint(wl[i-1]);
    if not TUser.Locate('Index',lastwordind) then raise Exception.Create(eWordNotLocated);
    RemoveWordFromCategory(lastwordind, GetSelCatIdx(fUserDetails.lbCategories));
  end;
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.Button14Click(Sender: TObject);
begin
  showmessage(_l('#00150^eFeature not implemented yet.'));
end;


function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var pr,ph:integer;
    c:integer;
    ps:string;
begin
  ps:=fSettings.Edit16.Text;
  c:=1;
  if (ps[2]='2') or (ps[4]='2') or (ps[6]='2') or (ps[8]='2') then c:=2;
  if (ps[2]='3') or (ps[4]='3') or (ps[6]='3') or (ps[8]='3') then c:=3;
  if (ps[2]='4') or (ps[4]='4') or (ps[6]='4') or (ps[8]='4') then c:=4;
  GetPrintLine(width,height,width,height,strtoint(fSettings.Edit10.Text),ph,pr);
  result:=((wl.Count-1) div (pr*c))+1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var pr:integer;
    ph,pw:integer;
    i,j,k,l:integer;
    wn:integer;
    ps:string;
    tk,tr,tw,tp,tm,t,ft:string;
    c:integer;
    ls:integer;
begin
  ps:=fSettings.Edit16.Text;
  c:=1;
  if (ps[2]='2') or (ps[4]='2') or (ps[6]='2') or (ps[8]='2') then c:=2;
  if (ps[2]='3') or (ps[4]='3') or (ps[6]='3') or (ps[8]='3') then c:=3;
  if (ps[2]='4') or (ps[4]='4') or (ps[6]='4') or (ps[8]='4') then c:=4;
  GetPrintLine(width,height,origwidth,origheight,strtoint(fSettings.Edit10.Text),ph,pr);
  for i:=0 to pr-1 do
  for j:=0 to c-1 do
  begin
    if ((pagenum-1)*pr*c+i+j*pr)<wl.Count then
    begin
      wn:=strtoint(wl[(pagenum-1)*pr*c+i+j*pr]);
      TUser.Locate('Index',wn);
      tm:=UnicodeToHex(remmark(TUser.Str(TUserEnglish)));
      tk:=TUser.Str(TUserPhonetic);
      tr:=UnicodeToHex(KanaToRomaji(TUser.Str(TUserPhonetic),romasys,curlang));
      if showroma then
        tp:=UnicodeToHex(KanaToRomaji(TUser.Str(TUserPhonetic),romasys,curlang)) else
        tp:=TUser.Str(TUserPhonetic);
      if (not fSettings.CheckBox17.Checked) or (FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0) then
        tw:=TUser.Str(TUserKanji) else tw:='';
      for k:=1 to 4 do if (ps[k*2]=chr(ord('1')+j)) then
      begin
        l:=k;
        repeat
          canvas.Brush.Color:=clWhite;
          canvas.Pen.Color:=clWhite;
          if (fSettings.CheckBox16.Checked) and (i mod 2=0) then
          begin
            canvas.Brush.Color:=$00DDDDDD;
            canvas.Pen.Color:=$00DDDDDD;
          end;
          canvas.Rectangle(ph+((width-ph*2) div 4)*(l-1),ph*i+(height-ph*pr) div 2,ph+((width-ph*2) div 4)*l,(height-ph*pr) div 2+ph+ph*i);
          canvas.Pen.Color:=clBlack;
          if fSettings.CheckBox14.Checked then
          begin
            canvas.MoveTo(ph+((width-ph*2) div 4)*(l-1),ph*i+(height-ph*pr) div 2);
            canvas.LineTo(ph+((width-ph*2) div 4)*l,ph*i+(height-ph*pr) div 2);
            canvas.MoveTo(ph+((width-ph*2) div 4)*(l-1),ph+ph*i+(height-ph*pr) div 2);
            canvas.LineTo(ph+((width-ph*2) div 4)*l,ph+ph*i+(height-ph*pr) div 2);
          end;
          inc(l);
        until ps[l*2]<>'-';
        ls:=l;
        canvas.Pen.Color:=clBlack;
        case ps[k*2-1] of
          'r':begin t:=tr; ft:=FontEnglish; end;
          'k':begin t:=tk; ft:=FontJapanese; end;
          'p':begin t:=tp; ft:=FontJapanese; end;
          'w':begin t:=tw; ft:=FontJapanese; end;
          'm':begin t:=tm; ft:=FontEnglish; end;
          '-':begin t:=''; ft:=FontEnglish; end;
        end;
        if (ft=FontJapanese) and (curlang='c') then ft:=FontChinese;
        if (ps[k*2-1]<>'p') then
          DrawUnicode(canvas,ph+round(ph*0.4)+((width-ph*2) div 4)*(k-1),ph*i+round(ph*0.1)+(height-ph*pr) div 2,round(ph*0.8),t,ft) else
          DrawKana(canvas,ph+round(ph*0.4)+((width-ph*2) div 4)*(k-1),ph*i+round(ph*0.1)+(height-ph*pr) div 2,round(ph*0.8),tk,ft,showroma,romasys,curlang);
        if fSettings.CheckBox15.Checked then
        begin
          canvas.MoveTo(ph+((width-ph*2) div 4)*(k-1),ph*i+(height-ph*pr) div 2);
          canvas.LineTo(ph+((width-ph*2) div 4)*(k-1),ph+ph*i+(height-ph*pr) div 2);
        end;
      end;
      if fSettings.CheckBox15.Checked then
      begin
        canvas.MoveTo(ph+((width-ph*2) div 4)*(ls-1),ph*i+(height-ph*pr) div 2);
        canvas.LineTo(ph+((width-ph*2) div 4)*(ls-1),ph+ph*i+(height-ph*pr) div 2);
      end;
    end;
  end;
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsWordListPrinting;
  fSettings.ShowModal;
end;

procedure TfWords.Button11Click(Sender: TObject);
begin
  fMenu.SaveUserData;
//  fMenu.SpeedButton2.Enabled:=false;
//  fMenu.SpeedButton7.Enabled:=false;
end;

procedure TfWords.Button12Click(Sender: TObject);
begin
  TUser.Free;
  TUserIdx.Free;
  fMenu.LoadUserData;
  ShowIt(false);
//  fMenu.SpeedButton2.Enabled:=false;
//  fMenu.SpeedButton7.Enabled:=false;
end;

procedure TfWords.ListBox1Click(Sender: TObject);
begin
  ShowIt(false);
end;

{ Word learning states:
    0 - problematic
    1 - unlearned
    2 - learned
    3 - mastered }
procedure SetWordLearnState(WordId: integer; LearnState: integer);
var max_ls: integer;
begin
  if not TUser.Locate('Index',wordId) then raise Exception.Create(eWordNotLocated);
  max_ls := TUser.Int(TUserMaxScore);
  if LearnState>max_ls then max_ls:=LearnState;
  case LearnState of
    2: TUser.Edit([TUserScore,TUserMaxScore,TUserLearned],[inttostr(LearnState),inttostr(max_ls),FormatDateTime('yyyymmdd',now)]);
    3: TUser.Edit([TUserScore,TUserMaxScore,TUserMastered],[inttostr(LearnState),inttostr(max_ls),FormatDateTime('yyyymmdd',now)]);
  else
    TUser.Edit([TUserScore,TUserMaxScore],[inttostr(LearnState),inttostr(max_ls)]);
  end;
end;

procedure TfWords.SetWordsLearnState(ls: integer);
var i: integer;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
    if Application.MessageBox(
      pchar(_l('#00927^eThis operation will affect multiple words.'#13#13'Do you want to continue?')),
      pchar(_l('#00926^eWarning')),
      MB_ICONWARNING or MB_YESNO)=idNo then exit;

  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
    setWordLearnState({WordId=}strtoint(wl[i-1]), {LearnState=}ls);

  StringGrid1Click(self);
  fMenu.UserDataChanged := true;
  ShowIt(false);
end;

procedure TfWords.AddWordsToCategory(category: string);
var i:integer;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
    if Application.MessageBox(
      pchar(_l('#00927^eThis operation will affect multiple words.'#13#13
        +'Do you want to continue?')),
      pchar(_l('#00926^eWarning')),
      MB_ICONWARNING or MB_YESNO)=idNo then exit;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    lastwordind:=strtoint(wl[i-1]);
    if not TUser.Locate('Index',lastwordind) then raise Exception.Create(eWordNotLocated);
    AddWord(
      TUser.Str(TUserKanji),
      TUser.Str(TUserPhonetic),
      TUser.Str(TUserEnglish),
      category,
      '?',
      StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom,
      1);
  end;
  ShowIt(false);
end;

procedure TfWords.MoveWordsInCategory(moveDir: TMoveDirection);
var ai,bi:integer;
  ap,bp:integer;
  s:string;
  i:integer;
begin
  if moveDir=mdUp then begin
    ai:=strtoint(wl[StringGrid1.Row-1]);
    bi:=strtoint(wl[StringGrid1.Row-2]);
  end else begin
    ai:=strtoint(wl[StringGrid1.Row-1]);
    bi:=strtoint(wl[StringGrid1.Row]);
  end;
  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.Locate('Number',StrToInt(wlc[StringGrid1.Row-1]));
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetWord)=ai then ap:=TUserSheet.Int(TUserSheetPos);
    if TUserSheet.Int(TUserSheetWord)=bi then bp:=TUserSheet.Int(TUserSheetPos);
    TUserSheet.Next;
  end;
  TUserSheet.Locate('Number',StrToInt(wlc[StringGrid1.Row-1]));
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetPos)=ap then TUserSheet.Edit([TUserSheetPos],[inttostr(bp)]) else
    if TUserSheet.Int(TUserSheetPos)=bp then TUserSheet.Edit([TUserSheetPos],[inttostr(ap)]);
    TUserSheet.Next;
  end;
  for i:=0 to 3 do
  begin
    s:=StringGrid1.Cells[i,StringGrid1.Row];
    if i=3 then begin
      if moveDir=mdUp then begin
        StringGrid1.Cells[i,StringGrid1.Row]:= '!'+StringGrid1.Cells[i,stringGrid1.Row-1][2]+copy(s,3,length(s)-2);
        StringGrid1.Cells[i,StringGrid1.Row-1]:= '!'+s[2]+copy(StringGrid1.Cells[i,StringGrid1.Row-1],3,length(StringGrid1.Cells[i,StringGrid1.Row-1])-2);
      end else begin
        StringGrid1.Cells[i,StringGrid1.Row]:= '!'+StringGrid1.Cells[i,stringGrid1.Row+1][2]+copy(s,3,length(s)-2);
        StringGrid1.Cells[i,StringGrid1.Row+1]:= '!'+s[2]+copy(StringGrid1.Cells[i,StringGrid1.Row+1],3,length(StringGrid1.Cells[i,StringGrid1.Row+1])-2);
      end;
    end else begin
      if moveDir=mdUp then begin
        StringGrid1.Cells[i,stringGrid1.Row]:=StringGrid1.Cells[i,StringGrid1.Row-1];
        StringGrid1.Cells[i,StringGrid1.Row-1]:=s;
      end else begin
        StringGrid1.Cells[i,stringGrid1.Row]:=StringGrid1.Cells[i,StringGrid1.Row+1];
        StringGrid1.Cells[i,StringGrid1.Row+1]:=s;
      end;
    end;
  end;
  s:=wl[StringGrid1.Row-1];
  if moveDir=mdUp then begin
    wl[StringGrid1.Row-1]:=wl[StringGrid1.Row-2];
    wl[StringGrid1.Row-2]:=s;
    StringGrid1.Row:=StringGrid1.Row-1;
  end else begin
    wl[StringGrid1.Row-1]:=wl[StringGrid1.Row];
    wl[StringGrid1.Row]:=s;
    StringGrid1.Row:=StringGrid1.Row+1;
  end;
  fMenu.ChangeUserData;
end;

procedure TfWords.DeleteWords();
var i:integer;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
  begin
    if Application.MessageBox(
      pchar(_l('#00925^eReally delete all these words?')),
      pchar(_l('#00926^eWarning')),
      MB_ICONWARNING or MB_YESNO)=idNo then exit;
  end else
  begin
    if Application.MessageBox(
      pchar(_l('#00852^eReally delete this word?')),
      pchar(_l('#00853^eConfirmation')),
      MB_ICONWARNING or MB_YESNO)=idNo then
      exit;
  end;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    lastwordind:=strtoint(wl[i-1]);
    if not TUser.Locate('Index',lastwordind) then raise Exception.Create(eWordNotLocated);
    TUserIdx.First;
    while not TUserIdx.EOF do
    begin
      if TUserIdx.Int(TUserIdxWord)=TUser.Int(TUserIndex) then TUserIdx.Delete;
      TUserIdx.Next;
    end;
    TUserSheet.First;
    while not TUserSheet.EOF do
    begin
      if TUserSheet.Int(TUserSheetWord)=TUser.Int(TUserIndex) then TUserSheet.Delete;
      TUserSheet.Next;
    end;
    TUser.Delete;
  end;
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.Button15Click(Sender: TObject);
begin
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00828^eVocabulary list'));
end;

procedure TfWords.Button18Click(Sender: TObject);
var csl:TStringList;
    i,j,k:integer;
    s,s2:string;
    v,vb,mv:integer;
    vs,vbs:string;
    a,b:integer;
    tol:integer;
begin
  if Application.MessageBox(
    pchar(_l('#00946^eThis function inserts a list of some unlearned characters '
      +'that are in current vocabulary list')+#13+
      _l('#00945^einto the clipboard sorted by usefullness.'#13#13'Do you want '
      +'to start this function?')),
    pchar(_l('#00856^eCharacter recommendation')),
    MB_ICONINFORMATION or MB_YESNO)=idYes then
  begin
    csl:=TStringList.Create;
    for i:=0 to wl.Count-1 do
    begin
      TUser.Locate('Index',strtoint(wl[i]));
      s:=TUser.Str(TUserKanji);
      for j:=1 to length(s) div 4 do
      begin
        s2:=copy(s,((j-1)*4)+1,4);
        if TChar.Locate('Unicode',s2) then
        if not IsKnown(KnownLearned,TChar.Fch(TCharUnicode)) then
        begin
          v:=trunc(ln(TChar.Int(TCharStrokeCount))*5000);
          if TChar.Int(TCharJouyouGrade)<10 then
            v:=v+TChar.Int(TCharJouyouGrade)*4000 else v:=v+40000;
          v:=v+20000;
          if TChar.Int(TCharJpFrequency)<65535 then
            v:=v+TChar.Int(TCharJpFrequency)*3 else v:=v+7000;
          TRadicals.Locate('Number',GetCharValueRad(TChar.Int(TCharIndex),12));
          TChar.Locate('Unicode',TRadicals.Str(TRadicalsUnicode));
          if (TRadicals.Str(TRadicalsUnicode)<>s2) and (not IsKnown(KnownLearned,TChar.Fch(TCharUnicode))) then inc(v,8000);
          if TRadicals.Str(TRadicalsUnicode)=s2 then dec(v,3000);
          vb:=v;
          for k:=0 to csl.Count-1 do if copy(csl[k],11,4)=s2 then
          begin
            vb:=strtoint(copy(csl[k],6,5));
            v:=strtoint(copy(csl[k],1,5));
            csl.Delete(k);
            break;
          end;
          mv:=vb-v;
          for k:=1 to TUser.Int(TUserScore) do
            v:=v-(1000 div (mv div 1000+1));
          vs:=inttostr(v);
          while length(vs)<5 do vs:='0'+vs;
          vbs:=inttostr(vb);
          while length(vbs)<5 do vbs:='0'+vbs;
          csl.Add(vs+vbs+s2);
        end;
      end;
    end;
    csl.Sort;
    clip:='';
    b:=50000;
    tol:=10000;
    for i:=0 to csl.Count-1 do
    begin
      a:=strtoint(copy(csl[i],1,5));
      if a<b then b:=a;
      if a<b+tol then
      begin
        clip:=clip+copy(csl[i],11,4);
        tol:=tol-tol div 3;
      end;
    end;
//    s:='';
    fMenu.SetClipboard;
//    for i:=0 to csl.Count-1 do
//      if strtoint(copy(csl[i],1,5))<b+5000 then s:=s+copy(csl[i],1,5)+'-'+copy(csl[i],6,5)+'-'+copy(csl[i],11,4)+#13;
    csl.Free;
  end;
end;

procedure TfWords.StringGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Upcase(key)='P') and (fUserDetails.btnSetProblematic.Enabled) then SetWordsLearnState(0);
  if (Upcase(key)='U') and (fUserDetails.btnSetUnlearned.Enabled) then SetWordsLearnState(1);
  if (Upcase(key)='L') and (fUserDetails.btnSetLearned.Enabled) then SetWordsLearnState(2);
  if (Upcase(key)='M') and (fUserDetails.btnSetMastered.Enabled) then SetWordsLearnState(3);
  if (Upcase(key)='A') and (fUserDetails.btnAddToCategory.Enabled) then AddWordsToCategory(fUserDetails.cbAddCategory.text);
  if (key=',') and (fUserDetails.btnMoveUpInCategory.Enabled) and (fUserDetails.btnMoveUpInCategory.Visible) then MoveWordsInCategory(mdUp);
  if (key='.') and (fUserDetails.btnMoveDownInCategory.Enabled) and (fUserDetails.btnMoveDownInCategory.Visible) then MoveWordsInCategory(mdDown);
end;

function DateOld(s:string;threshold:integer):integer;
var d:TDateTime;
begin
  if s='00000000'then result:=threshold else
  d:=EncodeDate(strtoint(copy(s,1,4)),strtoint(copy(s,5,2)),strtoint(copy(s,7,2)));
  if trunc(now-d)<threshold then result:=trunc(now-d) else result:=threshold;
end;

procedure TfWords.Button19Click(Sender: TObject);
var lname:string;
    i:integer;
{    i,j,k:integer;
    catno:integer;
    cpos:array[1..500] of word;
    chos:array[1..500] of integer;
    ca:array[1..5] of double;
    ba:array[1..5] of double;
    pa:array[1..5] of integer;
    wa:array[1..5] of double;
    da:array[1..4,1..5] of integer;
    wnum:integer;
    best,bestw:integer;
    g,h:integer;
    s,s2:string;
    cur:double;
    curi:integer;
    rand:integer;
    ph:integer;
    fnd:boolean;
    coef:double;
    m:integer;
    masc,proc:integer;
    d:double;}
begin
  lname:=formatdatetime('yyyy/mm/dd',now);
  i:=1;
  while TUserCat.Locate('Name',lname+' ('+inttostr(i)+')') do inc(i);
  lname:=lname+' ('+inttostr(i)+')';
  fWordList.Edit1.Text:=lname;
  fWordList.ShowModal;
  ShowIt(false);
{  lname:=formatdatetime('yyyy/mm/dd',now);
  i:=1;
  while TUserCat.Locate('Name',lname+' ('+inttostr(i)+')',false) do inc(i);
  lname:=lname+' ('+inttostr(i)+')';
  fWordList.Edit1.Text:=lname;
  if fWordList.ShowModal=idOK then
  begin
    pa[1]:=fWordList.TrackBar1.Position;
    pa[2]:=fWordList.TrackBar2.Position;
    pa[3]:=fWordList.TrackBar3.Position;
    pa[4]:=fWordList.TrackBar4.Position;
    pa[5]:=fWordList.TrackBar5.Position;
    for i:=1 to 5 do wa[i]:=0;
    masc:=0; proc:=0;
    for i:=1 to 4 do for j:=1 to 5 do da[i,j]:=0;
    wnum:=0;
    try wnum:=strtoint(fWordList.Edit2.Text); except end;
    if (wnum<1) or (wnum>500) then
    begin
      Application.MessageBox(
        pchar(_l('#00858^eInvalid number of words (must be between 1 and 500).')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
      exit;
    end;
    if (wl.Count<3*wnum) then
    begin
      Application.MessageBox(
        pchar(_l('#00859^eNumber of words in displayed list must be at least three times higher.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
      exit;
    end;
    Screen.Cursor:=crHourglass;
    inc(MaxCategoryIndex);
    catno:=MaxCategoryIndex;
    TUserCat.Insert([inttostr(catno),fWordList.Edit1.Text,inttostr(ord('W')),FormatDateTime('yyyymmdd',now)]);
    for i:=1 to 500 do cpos[i]:=0;
    ph:=0;
    for i:=1 to wnum do
    begin
      bestw:=0;
      best:=0;
      for j:=0 to wl.Count-1 do
      begin
        fnd:=false;
        for k:=1 to ph do if chos[k]=strtoint(wl[j]) then fnd:=true;
        if not fnd then
        begin
          TUser.Locate('Index',wl[j],true);
          if TUser.Int(TUserScore)<=1 then ca[1]:=-400 else ca[1]:=600;
          ca[1]:=ca[1]-DateOld(TUser.Str(TUserLearned),200)-DateOld(TUser.Str(TUserMastered),200);
          ca[2]:=800-DateOld(TUser.Str(TUserAdded),500)*3;
          if pos('<spop>',TUser.Str(TUserEnglish))>0 then ca[3]:=-500 else ca[3]:=500;
          ca[3]:=ca[3]-length(TUser.Str(TUserPhonetic))*10;
          ca[4]:=-800;
          s:=TUser.Str(TUserKanji);
          while length(s)>0 do
          begin
            s2:=copy(s,1,4);
            delete(s,1,4);
            if TChar.Locate('Unicode',s2,false) then
            begin
              if not IsKnown(TChar.Int(TCharIndex)) then ca[4]:=ca[4]+500;
              if TChar.Int(TCharJouyouGrade)>9 then ca[4]:=ca[4]+500 else
                ca[4]:=ca[4]+TChar.Int(TCharJouyouGrade)*50;
              ca[4]:=ca[4]+TChar.Int(TCharStrokeCount)*10;
            end;
          end;
          if ca[4]>900 then ca[4]:=900;
          if TUser.Int(TUserNoPrinted)<=10 then ca[5]:=-200+TUser.Int(TUserNoPrinted)*50 else
            ca[5]:=200+TUser.Int(TUserNoPrinted)*10;
          ca[5]:=ca[5]-DateOld(TUser.Str(TUserPrinted),100)*10;
          if ca[5]>900 then ca[5]:=900;
          if ca[5]<-900 then ca[5]:=-900;
          cur:=0;
          for k:=1 to 5 do cur:=cur+wa[k]*ca[k];
          cur:=cur+50000;
          if (TUser.Int(TUserScore)=0) and (proc<wnum div 3) then cur:=cur+25000;
          if (TUser.Int(TUserScore)=3) and (masc>wnum div 20) then cur:=cur-25000;
          rand:=0;
          for k:=1 to 100 do rand:=rand+random(1000)-500;
          cur:=cur+rand;
          if cur>100000 then cur:=100000;
          if cur<0 then cur:=0;
          curi:=trunc(cur);
          if curi>=best then
          begin
            bestw:=strtoint(wl[j]);
            for k:=1 to 5 do ba[k]:=ca[k];
            best:=curi;
          end;
        end;
      end;
      if best>0 then
      begin
        inc(ph);
        chos[ph]:=bestw;
        TUser.Locate('Index',inttostr(bestw),true);
        TUser.Edit([TUserPrinted,TUserNoPrinted],[FormatDateTime('yyyymmdd',now),inttostr(TUser.Int(TUserNoPrinted)+1)]);
        if TUser.Int(TUserScore)=3 then inc(masc);
        if TUser.Int(TUserScore)=0 then inc(proc);
        rand:=random(wnum-ph+1)+1;
        k:=1;
        while rand>0 do
        begin
          if cpos[k]=0 then dec(rand);
          if rand=0 then cpos[k]:=bestw;
          inc(k);
        end;
        for k:=1 to 5 do
        begin
          if ba[k]<-500 then inc(da[1,k]) else
          if ba[k]<0 then inc(da[2,k]) else
          if ba[k]<500 then inc(da[3,k]) else inc(da[4,k]);
          case k of
            1:coef:=2;
            2:coef:=1;
            3:coef:=0.5;
            4:coef:=1;
            5:coef:=1.5;
          end;
          m:=pa[k];
          if ba[k]>0 then m:=-m;
          case m of
            -2:coef:=coef/5;
            -1:coef:=coef/2;
            0:coef:=coef;
            1:coef:=coef*2;
            2:coef:=coef*5;
          end;
          wa[k]:=wa[k]-(ba[k]/1000)*coef/2;
        end;
        s:='CHOSEN #'+inttostr(ph);
        s:=s+#13+'Score: '+inttostr(best);
        for k:=1 to 5 do
        begin
          s:=s+#13+inttostr(k)+': ';
          s:=s+'B:'+inttostr(trunc(ba[k]))+' W:'+floattostrf(wa[k],ffNumber,7,2);
        end;
//        showmessage(s);
      end;
    end;
    s:='';
    for i:=1 to 5 do
    begin
      s:=s+#13+inttostr(i)+': ';
      for j:=1 to 4 do
        s:=s+inttostr(da[j,i])+'-';
    end;
//    showmessage(s);
    for i:=1 to ph do
      TUserSheet.Insert([inttostr(cpos[i]),inttostr(catno),inttostr(i)]);
    Screen.Cursor:=crDefault;
    fMenu.RefreshCategory;
    TabSet1.TabIndex:=3;
    TabSet1Change(sender,3,fnd);
    for i:=0 to ListBOx1.Items.COunt-1 do ListBox1.Checked[i]:=ListBox1.Items[i]=lname;
    fMenu.ChangeUserData;
    ShowIt;
  end;}
end;

procedure TfWords.BuildWordList;
var il,sl:TStringList;
    i,j,k,l,m,n,o:integer;
    ca:array[1..4] of double;
    ba:array[1..4] of double;
    pa:array[1..4] of integer;
    wa:array[1..4] of double;
    da:array[1..4,1..4] of integer;
    qa:array[1..4] of integer;
    ia:array[1..4] of double;
    sa:array[1..4] of double;
    masc,proc:integer;
    wnum:integer;
    best,bestw:integer;
    cur:double;
    curi:integer;
    rand:integer;
    coef:double;
    s:string;

  sp: TSMPromptForm;

procedure SetPaQa(q:integer;w:integer);
begin
  case w of
    0:begin pa[q]:=-2; qa[q]:=2; end;
    1:begin pa[q]:=-1; qa[q]:=1; end;
    2:begin pa[q]:=1; qa[q]:=1; end;
    3:begin pa[q]:=2; qa[q]:=2; end;
    4:begin pa[q]:=0; qa[q]:=1; end;
    5:begin pa[q]:=0; qa[q]:=0; end;
  end;
end;
begin
  sp:=SMProgressDlg(
    _l('#00860^eBuilding learning list...'),
    _l('#00685^ePlease wait...'),
    100); //for now we don't track progress, so set max to 100

  fWordList.Label52.Caption:='';
  ll.Clear;
  Screen.Cursor:=crHourGlass;
  il:=TStringList.Create;
  sl:=TStringList.Create;
  randomize;
  if fWordList.RadioGroup1.ItemIndex=0 then
    il.Assign(wl) else
  begin
    TUser.SetOrder('Index_Ind');
    TUser.First;
    while not TUser.EOF do
    begin
      il.Add(TUser.Str(TUserIndex));
      TUser.Next;
    end;
  end;
  if fWordList.RadioGroup2.ItemIndex=1 then
  begin
    for i:=1 to strtoint(fWordList.Edit3.Text) do
    begin
      j:=random(il.Count);
      sl.Add(il[j]);
      il.Delete(j);
    end;
  end else if fWordList.RadioGroup2.ItemIndex=2 then
    sl.Assign(il) else
  begin
    SetPaQa(1,fWordList.RadioGroup3.ItemIndex);
    SetPaQa(2,fWordList.RadioGroup4.ItemIndex);
    SetPaQa(3,fWordList.RadioGroup5.ItemIndex);
    SetPaQa(4,fWordList.RadioGroup6.ItemIndex);
    for i:=1 to 4 do wa[i]:=qa[i]*2;
    for i:=1 to 4 do ia[i]:=0;
    for i:=1 to 4 do sa[i]:=pa[i]*250;
    for i:=1 to 4 do ia[i]:=sa[i];
    masc:=0; proc:=0;
    for i:=1 to 4 do for j:=1 to 4 do da[i,j]:=0;
    wnum:=strtoint(fWordList.Edit3.Text);
    for i:=1 to wnum do
    begin
      if i mod 10=0 then
      begin
        sp.SetProgress(trunc(i/wnum*100));
        sp.ProcessMessages;
      end;
      bestw:=0;
      best:=100001;
      for j:=0 to il.Count-1 do
      begin
        TUser.Locate('Index',strtoint(il[j]));
        if TUser.Int(TUserScore)<=1 then ca[1]:=-200 else ca[1]:=600;
        ca[1]:=ca[1]-DateOld(TUser.Str(TUserLearned),200)-DateOld(TUser.Str(TUserMastered),200);
        ca[2]:=800-DateOld(TUser.Str(TUserAdded),500)*3;
        if pos('<spop>',TUser.Str(TUserEnglish))>0 then ca[3]:=-500 else ca[3]:=0; //TODO: Note <spop> -- to be fixed when we upgrade dict format
        ca[3]:=ca[3]-length(TUser.Str(TUserPhonetic))*10;
        if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))>=0 then ca[3]:=ca[3]+800;
        if ca[3]>900 then ca[3]:=900;
        if ca[3]<-900 then ca[3]:=-900;
        if TUser.Int(TUserNoPrinted)<=10 then ca[4]:=-200+TUser.Int(TUserNoPrinted)*50 else
          ca[4]:=200+TUser.Int(TUserNoPrinted)*10;
        ca[4]:=ca[4]-DateOld(TUser.Str(TUserPrinted),100)*10;
        if ca[4]>900 then ca[4]:=900;
        if ca[4]<-900 then ca[4]:=-900;
        cur:=0;
        for k:=1 to 4 do cur:=cur+abs(wa[k]*(ca[k]-ia[k]));
        if (TUser.Int(TUserScore)=0) and (proc<wnum div 3) and (fWordList.CheckBox2.Checked) then cur:=cur-25000;
        if (TUser.Int(TUserScore)=3) and (masc>wnum div 20) and (fWordList.CheckBox3.Checked) then cur:=cur+25000;
        rand:=0;
        for k:=1 to 100 do rand:=rand+random(500);
        cur:=cur+rand;
        if cur>100000 then cur:=100000;
        if cur<1 then cur:=1;
        curi:=trunc(cur);
        if curi<=best then
        begin
          bestw:=strtoint(il[j]);
          for k:=1 to 4 do ba[k]:=ca[k];
          best:=curi;
        end;
      end;
      if best<=0 then showmessage('error');
      if best>0 then
      begin
        sl.Add(inttostr(bestw));
        il.Delete(il.IndexOf(inttostr(bestw)));
        TUser.Locate('Index',bestw);
        if TUser.Int(TUserScore)=3 then inc(masc);
        if TUser.Int(TUserScore)=0 then inc(proc);
        for k:=1 to 4 do
        begin
          if ba[k]<-500 then inc(da[1,k]) else
          if ba[k]<0 then inc(da[2,k]) else
          if ba[k]<500 then inc(da[3,k]) else inc(da[4,k]);
          case k of
            1:coef:=2*qa[1];
            2:coef:=1*qa[2];
            3:coef:=1*qa[3];
            4:coef:=1.5*qa[4];
          end;
{          m:=pa[k];
          if (ba[k]-ia[k])>0 then m:=-m;
          case m of
            -2:coef:=coef/5;
            -1:coef:=coef/2;
            0:coef:=coef;
            1:coef:=coef*2;
            2:coef:=coef*5;
          end;}
          ia[k]:=ia[k]-((ba[k]-sa[k])/20)*coef;
          if ia[k]<-900 then ia[k]:=-900;
          if ia[k]>900 then ia[k]:=900;
          wa[k]:=(wa[k]+(coef*abs(ba[k]-sa[k])/500))/2;
        end;
        s:='#'+inttostr(j)+' Sc:'+inttostr(best);
        for k:=1 to 4 do s:=s+#13+inttostr(k)+': B:'+floattostrf(ba[k],ffNumber,7,2)+' W:'+floattostrf(wa[k],ffNumber,7,2)+' I:'+floattostrf(ia[k],ffNumber,7,2);
//        showmessage(s);
      end;
    end;
    s:='DEBUG LEARNING LIST STATS:'#13;
    for i:=1 to 4 do
    begin
      s:=s+#13+inttostr(i)+': -';
      for j:=1 to 4 do
        s:=s+inttostr(da[j,i])+'-';
      s:=s+' I:'+floattostrf(ia[i],ffNumber,7,2)+' W:'+floattostrf(wa[i],ffNumber,7,2);
    end;
    fWordList.Label52.Caption:=s;
  end;
  if not fWordList.CheckBox1.Checked then ll.Assign(sl) else
  for i:=0 to sl.Count-1 do
  begin
    j:=random(sl.Count);
    ll.Add(sl[j]);
    sl.Delete(j);
  end;
  il.Free;
  sl.Free;
  i:=0; j:=0; k:=0; l:=0; m:=0; n:=0;
  for o:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[o]));
    if TUser.Int(TUserScore)=0 then inc(i);
    if TUser.Int(TUserScore)>=2 then inc(j);
    if TUser.Int(TUserScore)=3 then inc(k);
    if pos('<spop>',TUser.Str(TUserEnglish))=0 then inc(l);
    if (length(TUser.Str(TUserPhonetic))>1) and (TUser.Str(TUserPhonetic)[3]>='A') then inc(m);
    if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0then inc(n);
    TUser.Next;
  end;
  fWordList.Label26.Caption:=perc(j,ll.Count);
  fWordList.Label27.Caption:=perc(k,ll.Count);
  fWordList.Label28.Caption:=perc(i,ll.Count);
  fWordList.Label35.Caption:=perc(l,ll.Count);
  fWordList.Label29.Caption:=perc(m,ll.Count);
  fWordList.Label44.Caption:=perc(n,ll.Count);
  fWordList.RadioGroup7.ItemIndex:=0;
  fWordList.RadioGroup7.Enabled:=true;
  Screen.Cursor:=crDefault;
  sp.Free;
end;

function StateNew(i,j:integer):integer;
var k:integer;
begin
  case i of
    0:if j=0 then k:=0 else if j=1 then k:=1 else k:=2;
    1:if j=0 then k:=1 else if j=1 then k:=2 else k:=3;
    2:if j=0 then k:=0 else if j=1 then k:=2 else k:=3;
    3:if j=0 then k:=0 else if j=1 then k:=1 else k:=3;
  end;
  result:=k;
end;

procedure TfWords.PrepareTestWord;
var i:integer;
    s:string;
    sl:TStringList;
begin
  TUser.Locate('Index',strtoint(ll[twi]));
  twkanji:=CheckKnownKanji(TUser.Str(TUserKanji));
  twphonetic:=TUser.Str(TUserPhonetic);
  twmeaning:=remmark(TUser.Str(TUserEnglish));
  fWordList.Gauge1.MaxValue:=ll.Count;
  fWordList.Gauge1.Progress:=twi;
  fWordList.Label38.Caption:=DateForm(TUser.Str(TUserAdded));
  fWordList.Label39.Caption:=DateForm(TUser.Str(TUserLearned));
  fWordList.Label40.Caption:=DateForm(TUser.Str(TUserMastered));
  fWordList.Label41.Caption:=DateForm(TUser.Str(TUserPrinted));
  if fWordList.Label41.Caption<>'-'then fWordList.Label41.Caption:=fWordList.Label41.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
  fWordList.RxLabel9.Caption:=StateStr(TUser.Int(TUserScore));
  fWordList.RxLabel10.Caption:=StateStr(StateNew(TUser.Int(TUserScore),0));
  fWordList.RxLabel11.Caption:=StateStr(StateNew(TUser.Int(TUserScore),1));
  fWordList.RxLabel12.Caption:=StateStr(StateNew(TUser.Int(TUserScore),2));
  sl:=TStringList.Create;
  sl.Clear;
  ListWordCategories(strtoint(ll[twi]),sl);
  for i:=0 to sl.Count-1 do if s='' then s:=sl[i] else s:=s+', '+sl[i];
  fWordList.Label55.Caption:=s;
  sl.Free;
  fWordList.Label24.Visible:=false;
  fWordList.Label25.Visible:=false;
  fWordList.Label31.Visible:=false;
  fWordList.Label30.Visible:=false;
  fWordList.Label32.Visible:=false;
  fWordList.Label36.Visible:=false;
  fWordList.Label37.Visible:=false;
  fWordList.Label34.Visible:=false;
  fWordList.Label38.Visible:=false;
  fWordList.Label39.Visible:=false;
  fWordList.Label40.Visible:=false;
  fWordList.Label41.Visible:=false;
  fWordList.Label43.Visible:=false;
  fWordList.Label45.Visible:=false;
  fWordList.Label54.Visible:=false;
  fWordList.Label55.Visible:=false;
  fWordList.RxLabel9.Visible:=false;
  fWordList.RxLabel10.Visible:=false;
  fWordList.RxLabel11.Visible:=false;
  fWordList.RxLabel12.Visible:=false;
  fWordList.Button7.Visible:=false;
  fWordList.Button8.Visible:=false;
  fWordList.Button9.Visible:=false;
  fWordList.Shape1.Visible:=false;
  fWordList.Shape2.Visible:=false;
  fWordList.Shape5.Visible:=false;
  fWordList.PaintBox1.Visible:=false;
  fWordList.PaintBox2.Visible:=false;
  fWordList.PaintBox6.Visible:=false;
  fWordList.Button10.Visible:=true;
  i:=0;
  case fWordList.RadioGroup8.ItemIndex of
    0:i:=1;
    1:i:=2;
    2:i:=3;
    3:i:=4;
    4:i:=5;
    5:i:=6;
    6:if random(100)<50 then i:=1 else i:=2;
    7:if random(100)<66 then i:=1 else i:=2;
    8:if random(100)<33 then i:=1 else if random(66)<33 then i:=2 else i:=3;
    9:if random(100)<25 then i:=1 else if random(75)<25 then i:=2 else i:=3;
    10:if random(100)<50 then i:=1 else if random(50)<25 then i:=2 else i:=3;
  end;
  if (i=2) and (fWordList.CheckBox6.Checked) and (FirstUnknownKanjiIndex(TUser.Str(TUserKanji))>=0) then i:=1;
  if i=1 then
  begin
    fWordList.PaintBox1.Visible:=true;
    fWordList.Shape2.Visible:=true;
    fWordList.Label24.Visible:=true;
  end;
  if i=2 then
  begin
    fWordList.PaintBox6.Visible:=true;
    fWordList.Shape5.Visible:=true;
    fWordList.Label25.Visible:=true;
  end;
  if i=3 then
  begin
    fWordList.PaintBox2.Visible:=true;
    fWordList.Shape1.Visible:=true;
    fWordList.Label31.Visible:=true;
  end;
  if i=4 then
  begin
    fWordList.PaintBox1.Visible:=true;
    fWordList.Shape2.Visible:=true;
    fWordList.Label24.Visible:=true;
    fWordList.PaintBox6.Visible:=true;
    fWordList.Shape5.Visible:=true;
    fWordList.Label25.Visible:=true;
  end;
  if i=5 then
  begin
    fWordList.PaintBox1.Visible:=true;
    fWordList.Shape2.Visible:=true;
    fWordList.Label24.Visible:=true;
    fWordList.PaintBox2.Visible:=true;
    fWordList.Shape1.Visible:=true;
    fWordList.Label31.Visible:=true;
  end;
  if i=6 then
  begin
    fWordList.PaintBox6.Visible:=true;
    fWordList.Shape5.Visible:=true;
    fWordList.Label25.Visible:=true;
    fWordList.PaintBox2.Visible:=true;
    fWordList.Shape1.Visible:=true;
    fWordList.Label31.Visible:=true;
  end;
end;

procedure TfWords.ShowTestWord;
begin
  fWordList.Label24.Visible:=true;
  fWordList.Label25.Visible:=true;
  fWordList.Label31.Visible:=true;
  fWordList.Label30.Visible:=true;
  fWordList.Label32.Visible:=true;
  fWordList.Label36.Visible:=true;
  fWordList.Label37.Visible:=true;
  fWordList.Label34.Visible:=true;
  fWordList.Label38.Visible:=true;
  fWordList.Label39.Visible:=true;
  fWordList.Label40.Visible:=true;
  fWordList.Label41.Visible:=true;
  fWordList.Label43.Visible:=true;
  fWordList.Label45.Visible:=true;
  fWordList.Label54.Visible:=true;
  fWordList.Label55.Visible:=true;
  fWordList.RxLabel9.Visible:=true;
  fWordList.RxLabel10.Visible:=true;
  fWordList.RxLabel11.Visible:=true;
  fWordList.RxLabel12.Visible:=true;
  fWordList.Button7.Visible:=true;
  fWordList.Button8.Visible:=true;
  fWordList.Button9.Visible:=true;
  fWordList.Shape1.Visible:=true;
  fWordList.Shape2.Visible:=true;
  fWordList.Shape5.Visible:=true;
  fWordList.PaintBox1.Visible:=true;
  fWordList.PaintBox2.Visible:=true;
  fWordList.PaintBox6.Visible:=true;
  fWordList.Button10.Visible:=false;
end;

function TfWords.FinishTestWord(button:integer):boolean;
begin
  ltl.Add(inttostr(button));
  inc(twchg[TUser.Int(TUserScore),StateNew(TUser.Int(TUserScore),button)]);
  inc(twi);
  if twi<ll.Count then
  begin
    result:=true;
    exit;
  end;
  result:=false;
  fWordList.Label53.Caption:=perc(twchg[0,0]+twchg[1,1]+twchg[2,2]+twchg[3,3],ll.Count);
  fWordList.Label11.Caption:=perc(twchg[2,0],ll.Count);
  fWordList.Label12.Caption:=perc(twchg[3,0],ll.Count);
  fWordList.Label13.Caption:=perc(twchg[3,1],ll.Count);
  fWordList.Label46.Caption:=perc(twchg[0,1],ll.Count);
  fWordList.Label47.Caption:=perc(twchg[0,2],ll.Count);
  fWordList.Label48.Caption:=perc(twchg[1,2],ll.Count);
  fWordList.Label49.Caption:=perc(twchg[1,3],ll.Count);
  fWordList.Label50.Caption:=perc(twchg[2,3],ll.Count);
end;

procedure TfWords.SaveTestResults;
var i,n:integer;
    ms:integer;
begin
  Screen.Cursor:=crHourGlass;
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    n:=StateNew(TUser.Int(TUserScore),strtoint(ltl[i]));
    ms:=n;
    if TUser.Int(TUserMaxScore)>ms then ms:=TUser.Int(TUserMaxScore);
    case n of
      0,1:TUser.Edit([TUserScore,TUserMaxScore],[inttostr(n),inttostr(ms)]);
      2:TUser.Edit([TUserScore,TUserMaxScore,TUserLearned],['2',inttostr(ms),FormatDateTime('yyyymmdd',now)]);
      3:TUser.Edit([TUserScore,TUserMaxScore,TUserMastered],['3',inttostr(ms),FormatDateTime('yyyymmdd',now)]);
    end;
  end;
  Screen.Cursor:=crDefault;
  fMenu.ChangeUserData;
end;

procedure TfWords.PrintWordList;
var bk:TStringList;
    i:integer;
begin
  bk:=TStringList.Create;
  bk.Assign(wl);
  wl.Clear;
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    if (fWordList.RadioGroup10.ItemIndex=0) or
       ((fWordList.RadioGroup10.ItemIndex=1) and (TUser.Int(TUserScore)<3)) or
       (TUser.Int(TUserScore)<2) then
      wl.Add(ll[i]);
  end;
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00861^eLearning list'));
  wl.Clear;
  wl.Assign(bk);
  bk.Free;
end;

procedure TfWords.SaveWordList;
var i:integer;
    fnd:boolean;
begin
  Inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),fWordList.Edit1.Text,inttostr(ord('W')),FormatDateTime('yyyymmdd',now)]);
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    if (fWordList.RadioGroup10.ItemIndex=0) or
       ((fWordList.RadioGroup10.ItemIndex=1) and (TUser.Int(TUserScore)<3)) or
       (TUser.Int(TUserScore)<2) then
      TUserSheet.Insert([ll[i],inttostr(MaxCategoryIndex),inttostr(i)]);
  end;
  fMenu.RefreshCategory;
  fUserFilters.tabCatList.TabIndex:=3;
  fUserFilters.tabCatListChange(self,3,fnd);
  for i:=0 to fUserFilters.lbCategories.Items.COunt-1 do
    fUserFilters.lbCategories.Checked[i]:=fUserFilters.lbCategories.Items[i]=fWordList.Edit1.Text;
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UpdateWordListStats;
var i,j:integer;
begin
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    j:=TUser.Int(TUserNoPrinted);
    TUser.Edit([TUserNoPrinted,TUserPrinted],[inttostr(j+1),FormatDateTime('yyyymmdd',now)]);
  end;
  fMenu.ChangeUserData;
end;

function TfWords.LLGoNext(page:integer):integer;
var i,j:integer;
begin
  result:=page;
  case page of
    1:result:=2;
    2:begin
        i:=0; try i:=strtoint(fWordList.Edit3.Text); except end;
        j:=0; try j:=strtoint(fWordList.Label23.Caption); except end;
        if (fWordList.RadioGroup2.ItemIndex<2) and ((i<1) or (i>500)) then
        begin
          Application.MessageBox(
            pchar(_l('#00858^eInvalid number of words (must be between 1 and 500).')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if (fWordList.RadioGroup2.ItemIndex=0) and (2*i>j) then
        begin
          Application.MessageBox(
            pchar(_l('#00862^eNumber of input words must be at least two times higher than number of selected words.')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if (fWordList.RadioGroup2.ItemIndex=1) and (i>j) then
        begin
          Application.MessageBox(
            pchar(_l('#00863^eNumber of input words must not be lower than number of selected words.')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if j<1 then
        begin
          Application.MessageBox(
            pchar(_l('#00864^eInput list cannot be empty.')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if fWordList.RadioGroup2.ItemIndex=0 then result:=3 else result:=4;
        if result=4 then BuildWordList;
      end;
    3:begin
        result:=4;
        BuildWordList;
      end;
    4:if fWordList.RadioGroup7.ItemIndex=0 then result:=5 else result:=8;
    5:begin
        fWordList.RadioGroup7.ItemIndex:=1;
        fWordList.RadioGroup7.Enabled:=false;
        twi:=0;
        ltl.Clear;
        for i:=0 to 3 do for j:=0 to 3 do twchg[i,j]:=0;
        PrepareTestWord;
        result:=6;
      end;
    6:result:=7;
    7:begin
        if fWordList.RadioGroup9.ItemIndex=0 then SaveTestResults;
        result:=8;
      end;
    8:begin
        if fWordList.CheckBox4.Checked then UpdateWordListStats;
        if fWordList.CheckBox5.Checked then SaveWordList;
        result:=9;
      end;
  end;
end;

function TfWords.LLGoPrev(page:integer):integer;
begin
  result:=page;
  case page of
    1:result:=0;
    2:result:=1;
    3:result:=2;
    4:begin
        if Application.MessageBox(
          pchar(_l('#00865^eGenerated list will be lost. Do you want to continue?')),
          pchar(_l('#00573^eWarning')),
          MB_YESNO or MB_ICONWARNING)=idYes then result:=2;
      end;
    5:result:=4;
    6,7:begin
        if Application.MessageBox(
          pchar(_l('#00866^eCurrent test data will be lost. Do you want to continue?')),
          pchar(_l('#00573^eWarning')),
          MB_YESNO or MB_ICONWARNING)=idYes then result:=5;
      end;
    8:result:=4;
    9:result:=0;
  end;
end;

procedure TfWords.SpeedButton1Click(Sender: TObject);
begin
  fMenu.aUserExamples.Execute;
end;

procedure TfWords.SpeedButton2Click(Sender: TObject);
begin
  fMenu.aUserSettings.Execute;
end;

procedure TfWords.SpeedButton4Click(Sender: TObject);
begin
  fMenu.aUserDetails.Execute;
end;

procedure TfWords.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipMouseMove(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfWords.Button2Click(Sender: TObject);
begin
  if fUserAdd.ShowModal=mrOK then
    if AddWord(clip,RomajiToKana(fUserAdd.Edit1.Text,romasys,curlang,[rfDeleteInvalidChars]),
      fUserAdd.Edit3.Text,fUserAdd.ComboBox1.Text,'?',false,1) then
    begin
      fUserAdd.Edit1.Text:='';
      fUserAdd.PaintBox3.Invalidate;
      fUserAdd.Edit3.Text:='';
    end;
end;

{ Rebuilds wakan.sod from strokes.csv. Mostly used by a devs, but users can do this too. }
procedure TfWords.BuildStrokeOrderPackage(sourceCsv: string);
var
  tempDir: string;
  pack: TPackageBuilder;
  sl,sl2:TStringList;
  t:textfile;
  f:file of byte;
  s,s2,s3,s4,uni:string;
  i,j,k:integer;
  b:byte;
begin
  sl:=TStringList.Create;
  assignfile(t,sourceCsv);
  System.reset(t);
  while not eof(t) do
  begin
    readln(t,s);
    s2:='';
    uni:=format('%4.4X',[strtoint(copy(s,4,5))]);
    while (pos('";"',s)>0) do delete(s,1,pos('";"',s)+2);
    delete(s,length(s),1);
    while s<>'' do
    begin
      s3:=copy(s,2,pos(']',s)-2);
      delete(s,1,pos(']',s));
      s4:=copy(s3,1,pos(',',s3)-1);
      delete(s3,1,pos(',',s3));
      if (strtoint(s3)>255) or (strtoint(s4)>255) then showmessage('error');
      s2:=s2+format('%2.2X%2.2X',[strtoint(s4),strtoint(s3)]);
    end;
    sl.Add(uni+s2);
  end;
  closefile(t);
  sl2:=TStringList.Create;
  sl.Sort;

  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  assignfile(f,tempDir+'\strokes.bin');
  rewrite(f);
  k:=0;
  for i:=0 to sl.Count-1 do
  begin
    sl2.Add(copy(sl[i],1,4)+Format('%4.4X',[k]));
    s:=sl[i];
    k:=k+(length(s) div 4);
    for j:=2 to (length(s) div 2)-1 do
    begin
      b:=strtoint('0x'+copy(s,j*2+1,2));
      write(f,b);
    end;
    b:=0;
    write(f,b); write(f,b);
  end;
  closefile(f);
  sl2.SaveToFile(tempDir+'\dir.txt');
  sl.Free;
  sl2.Free;

  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := 'wakan.sod';
    pack.MemoryLimit := 100000000;
    pack.Name := 'Stroke order';
    pack.TitleName := 'Japanese stroke order charts';
    pack.CompanyName := 'LABYRINTH';
    pack.CopyrightName := '(C) Jim Breen, Yasuhito Tanaka';
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

procedure TfWords.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(true);
end;

procedure TfWords.StringGrid1Click(Sender: TObject);
var i:integer;
begin
  if StringGrid1.Selection.Bottom-StringGrid1.Selection.Top>0 then
  begin
    Reset;
    curword:=-1;
    curkanji:=fstr(_l('#00928^e<multiple words>'));
    fUserDetails.ClearCategories;
    for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
      fUserDetails.AddWordCategories(strtoint(wl[i-1]));
    fUserDetails.SetMultipleWords;
    exit;
  end;
//  Reset;
  lastwordind:=strtoint(wl[StringGrid1.Row-1]);
  if not TUser.Locate('Index',lastwordind) then raise Exception.Create(eWordNotLocated);
  curword:=lastwordind;
  curkanji:=TUser.Str(TUserKanji);
  curphonetic:=TUser.Str(TUserPhonetic);
  fExamples.SetExamples(curkanji);
  fUserDetails.ClearCategories;
  fUserDetails.AddWordCategories(lastwordind);
  fUserDetails.SetSingleWord;
  if fUserFilters.rgSort.ItemIndex=0 then
  begin
    fUserDetails.btnMoveUpInCategory.Enabled:=(StringGrid1.Row>1) and (wlc[StringGrid1.Row-2]=wlc[StringGrid1.Row-1]);
    fUserDetails.btnMoveDownInCategory.Enabled:=(StringGrid1.Row<wlc.Count) and (wlc[StringGrid1.Row]=wlc[StringGrid1.Row-1]);
  end;
  AnnotShowMedia(curkanji,curphonetic);
end;

procedure TfWords.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.IntTipMouseUp;
end;

procedure TfWords.SearchWord(wordind: integer);
begin
  lastwordind:=wordind;
  ShowIt(true);
end;

end.
