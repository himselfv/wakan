unit JWBVocab;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, Buttons, Tabs, CheckLst, Menus, WakanWordGrid,
  WakanPaintbox;

type
  TMoveDirection = (mdUp, mdDown);

  TfVocab = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    pnlDockFilters: TPanel;
    Panel1: TPanel;
    TopPanel: TPanel;
    RxLabel1: TLabel;
    btnPrintVocabList: TButton;
    btnRecommendKanji: TButton;
    btnLearningList: TButton;
    pnlDockDetails: TPanel;
    pnlDockExamples: TPanel;
    btnAddWord: TButton;
    PopupMenu1: TPopupMenu;
    miResetColumns: TMenuItem;
    splDockFilters: TSplitter;
    splDockDetails: TSplitter;
    BlankPanel: TBlankPanel;
    StringGrid1: TWakanGrid;
    BottomPanel: TPanel;
    btnExamples: TSpeedButton;
    btnListSettings: TSpeedButton;
    btnWordDetails: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Button14Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnPrintVocabListClick(Sender: TObject);
    procedure btnRecommendKanjiClick(Sender: TObject);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure btnLearningListClick(sender: TObject);
    procedure btnExamplesClick(Sender: TObject);
    procedure btnListSettingsClick(Sender: TObject);
    procedure btnWordDetailsClick(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnAddWordClick(Sender: TObject);
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
    procedure Reset;
    procedure ShowIt(warningifnotfound:boolean);
    procedure SetCategoryFilter(const ACategoryName: string);
    function AddWord(kanji,phonetic,english,category:string;cattype:char;nomessages:boolean;status:integer):boolean;
    procedure SearchWord(wordind:integer);

  public
    wl:TStringList;
    curword:integer; //selected word ID, or -1 if not selected/multiple selected
    curphonetic:string;
    curkanji:string;
    procedure PrintList(const ACaption: string);

  end;

 { Delays reindexing of TUser, TUserSheet, TUserIdx.
  Do not access any of those tables while this is in use. }

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
    procedure Commit();
  end;

var
  fVocab: TfVocab;

const
  eWordNotLocated='INTERNAL ERROR. WORD NOT LOCATED'; //wakan likes this string


implementation

uses JWBStrings, JWBCore, JWBLanguage, JWBMenu, KanaConv, JWBUnit, JWBPrint,
  JWBSettings, JWBWordList, JWBVocabDetails, JWBVocabAdd, JWBIO, JWBFileType,
  JWBVocabFilters, JWBExamples, JWBUserData, JWBWordsExpChoose, JWBCategories,
  JWBAnnotations, StdPrompt, PKGWrite, JWBCharData, TextTable, JWBLegacyMarkup,
  JWBWordGrid, JWBClipboard, JWBIntTip, JWBScreenTip, JWBVocab1;

var wlc:TStringList;
    lastwordind:integer;
    lastwordadded:boolean;

{$R *.DFM}

procedure TfVocab.FormCreate(Sender: TObject);
begin
  wl:=TStringList.Create;
  wlc:=TStringList.Create;
  lastwordind:=0;
end;

procedure TfVocab.FormDestroy(Sender: TObject);
begin
  wl.Free;
  wlc.Free;
end;

procedure TfVocab.FormShow(Sender: TObject);
begin
  ShowIt(false);
end;

procedure TfVocab.SetDefaultColumnWidths;
begin
  StringGrid1.ColWidths[0]:=110;
  StringGrid1.ColWidths[1]:=138;
  StringGrid1.ColWidths[2]:=306;
  StringGrid1.ColWidths[3]:=159;
  AutoSizeColumns;
end;

procedure TfVocab.AutoSizeColumns;
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[3]-StringGrid1.ColWidths[0]-20;
end;

procedure TfVocab.StringGrid1ControlResize(Sender: TObject);
begin
  AutoSizeColumns;
end;

procedure TfVocab.PopupMenu1Popup(Sender: TObject);
var p: TPoint;
  ACol, ARow: integer;
begin
  p := StringGrid1.ScreenToClient(Mouse.CursorPos);
  StringGrid1.MouseToCell(p.X, p.Y, ACol, ARow);
  miResetColumns.Visible := (ARow=0); //click on header
end;

procedure TfVocab.miResetColumnsClick(Sender: TObject);
begin
  SetDefaultColumnWidths;
end;

procedure TfVocab.ShowIt(warningifnotfound:boolean);
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
    if ((not fVocabFilters.cbFilterUnlearned.Checked) and (TUser.Int(TUserScore)=1))
    or ((not fVocabFilters.cbFilterLearned.Checked) and (TUser.Int(TUserScore)=2))
    or ((not fVocabFilters.cbFilterMastered.Checked) and (TUser.Int(TUserScore)=3))
    or ((not fVocabFilters.cbFilterProblematic.Checked) and (TUser.Int(TUserScore)=0)) then exit;

    stp:=TUser.Str(TUserScore);
    if not CategoryOrder then begin
      ListWordCategories(TUser.Int(TUserIndex),cl);
      if not fVocabFilters.CheckEnabledCategories(cl) then exit;
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
  if not fVocab.Visible then exit;
  wl.Clear;
  wlc.Clear;
  sw:=0;
  cl:=TStringList.Create;
  sl:=TStringList.Create;
  InitWordGrid(StringGrid1,true,false);

  if fVocabFilters.rgSort.ItemIndex>0 then begin

    case fVocabFilters.rgSort.ItemIndex of
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

    for i:=0 to fVocabFilters.lbCategories.Items.Count-1 do
    if fVocabFilters.lbCategories.Checked[i] then
    begin
      cats:=fVocabFilters.lbCategories.Items[i];
      TUserSheet.SetOrder('Sheet_Ind');
      a := GetCatIdx(fVocabFilters.lbCategories,i);
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
  btnRecommendKanji.Enabled:=StringGrid1.Visible;
  btnLearningList.Enabled:=StringGrid1.Visible;
  if StringGrid1.Visible then StringGrid1.SetFocus;
end;

procedure TfVocab.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(StringGrid1,ACol,ARow,Rect,State);
end;

{ Some would like to set our filters programmatically, e.g. after creating a
 category }
procedure TfVocab.SetCategoryFilter(const ACategoryName: string);
var i: integer;
  fnd:boolean;
begin
  fVocabFilters.tabCatList.TabIndex:=3;
  fVocabFilters.tabCatListChange(self,3,fnd);
  for i:=0 to fVocabFilters.lbCategories.Items.Count-1 do
    fVocabFilters.lbCategories.Checked[i]:=fVocabFilters.lbCategories.Items[i]=ACategoryName;
  ShowIt(false);
end;

function TfVocab.AddWord(kanji,phonetic,english,category:string;cattype:char;nomessages:boolean;status:integer):boolean;
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
    phonsort:=GetPhoneticSortStr(phonetic,curlang);
    TUser.Insert(['0',english,phonetic,phonsort,
      kanji,FormatDateTime('yyyymmdd',now),'00000000','00000000','00000000','0',
      inttostr(status),inttostr(status)]);
    wordidx:=TUser.TrueInt(TUserIndex);
    s:=kanji;
    beg:=true;
    while s<>'' do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2) then
      begin
        if beg then bs:='T'else bs:='F';
        TUserIdx.Insert([IntToStr(wordidx),TChar.Str(TChar.fUnicode),bs]);
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

  TUser.NoCommitting := true;
  TUserSheet.NoCommitting := true;
  TUserIdx.NoCommitting := true;
end;

destructor TAddWordFast.Destroy;
begin
 //Just in case there was no Commit()
  TUser.NoCommitting := false;
  TUserSheet.NoCommitting := false;
  TUserIdx.NoCommitting := false;
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
    TUser.Insert(['0', english, phonetic, phonsort, kanji,
      FormatDateTime('yyyymmdd',now), '00000000', '00000000', '00000000',
      '0', inttostr(status), inttostr(status)]);
    wordidx:=TUser.TrueInt(TUserIndex);
    phonsort:=GetPhoneticSortStr(phonetic,curlang);

    s:=kanji;
    beg:=true;
    while s<>'' do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2) then
      begin
        if beg then bs:='T'else bs:='F';
        TUserIdx.Insert([IntToStr(wordidx), TChar.Str(TChar.fUnicode), bs])
      end;
      beg:=false;
    end;
  end;

  TUserSheet.Insert([IntToStr(wordidx), IntToStr(cat), IntToStr(catord)]);
  lastwordadded:=insertword;
  result:=true;
end;

procedure TAddWordFast.Commit();
begin
  TUser.nocommitting:=false;
  TUserSheet.nocommitting:=false;
  TUserIdx.nocommitting:=false;
  TUser.Reindex;
  TUserSheet.Reindex;
  TUserIdx.Reindex;
end;

procedure TfVocab.ExportVocab;
begin
  if SaveDialog1.Execute then
    if pos('.WKL',uppercase(SaveDialog1.FileName))>0 then
      ExportVocabToWkl(SaveDialog1.FileName)
    else
      ExportVocabToCsv(SaveDialog1.FileName);
end;

procedure TfVocab.ExportVocabToWkl(const filename: string);
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
      for j:=0 to fVocabFilters.lbCategories.Items.Count-1 do
        if (fVocabFilters.lbCategories.Checked[j]) and (sl.IndexOf(curlang+'~'+fVocabFilters.lbCategories.Items[j])<>-1) then
      begin
        writeln(t,fstrtohex(TUser.Str(TUserKanji)));
        writeln(t,fstrtohex(TUser.Str(TUserPhonetic)));
        writeln(t,TUser.Str(TUserEnglish));
        TUserCat.Locate('Name',curlang+'~'+fVocabFilters.lbCategories.Items[j]);
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

procedure TfVocab.ExportVocabToCsv(const filename: string);
var sp: TSMPromptForm;
  i,j:integer;
  sl:TStringList;
  conv: TStreamEncoder;
  fWordsExpChoose: TfWordsExpChoose;
  FExportType: integer;
begin
  Screen.Cursor:=crHourGlass;
  sl := nil;
  sp := nil;
  conv := nil;
  try
    sp:=SMProgressDlgCreate(
      _l('#01007^eVocabulary export'),
      _l('#01008^eExporting vocabulary...'),
      wl.Count,
      {canCancel=}true);
    sp.AppearModal;

    conv := TStreamEncoder.CreateNew(filename,Conv_ChooseType(curlang='c',nil).Create);

    fWordsExpChoose := TfWordsExpChoose.Create(Application);
    try
      if fWordsExpChoose.ShowModal=mrCancel then exit;
      FExportType := fWordsExpChoose.RadioGroup1.ItemIndex;
    finally
      FreeAndNil(fWordsExpChoose);
    end;

    conv.Write(fstr(#9' Wakan Word List'#13#10));
    conv.Write(fstr(#9''#13#10));
    conv.Write(fstr(#9' created by '+WakanAppName+' '+WakanCopyright+#13#10));
    conv.Write(fstr(#9' This file lists words that were exported from user vocabulary.'#13#10));
    conv.Write(fstr(#9' Each entry consists of one line where the following values are separated by a delimiter:'#13#10));
    conv.Write(fstr(#9' <written>;<phonetic>;<meaning>[;<category>[;<learned>]]'#13#10));
    conv.Write(fstr(#9' <category> and <learned> fields are optional.'#13#10));
    conv.Write(fstr(#9' <written> - How the word is written (in kanji/hanzi/kana)'#13#10));
    conv.Write(fstr(#9' <phonetic> - How the word is pronounced (in kana/Hepburn romaji/BoPoMoFo/PinYin)'#13#10));
    conv.Write(fstr(#9' <meaning> - English meaning of the word (cannot contain semicolons!)'#13#10));
    conv.Write(fstr(#9' <category> - Name of the category to place the word into (if new, user is asked to specify type) (optional)'#13#10));
    conv.Write(fstr(#9' <learned> - Learned state of the word: "P" - problematic, "U" - unlearned, "L" - learned, "M" - mastered (optional)'#13#10));
    conv.Write(fstr(#9' Delimiter is the first non-kanji character encountered.'#13#10));
    conv.Write(fstr(#9''#13#10));
    sl:=TStringList.Create;
    for i:=0 to wl.Count-1 do
    begin
      ListWordCategories(strtoint(wl[i]),sl);
      TUser.Locate('Index',strtoint(wl[i]));
      for j:=0 to fVocabFilters.lbCategories.Items.Count-1 do
        if (fVocabFilters.lbCategories.Checked[j]) and (sl.IndexOf(curlang+'~'+fVocabFilters.lbCategories.Items[j])<>-1) then
      begin
        conv.Write(TUser.Str(TUserKanji));
        conv.Write(fstr(#9));
        if not showroma then
          conv.Write(TUser.Str(TUserPhonetic))
        else
          if curlang='c'then
            conv.Write(fstr(DbKanaToRomaji(TUser.Str(TUserPhonetic),'c')))
          else
            conv.Write(fstr(DbKanaToRomaji(TUser.Str(TUserPhonetic),'j')));
        conv.Write(fstr(#9));
        conv.Write(fstr(repl(TUser.Str(TUserEnglish),';',',')));
        if FExportType<2 then
        begin
          conv.Write(fstr(#9));
          conv.Write(fstr(fVocabFilters.lbCategories.Items[j]));
          if FExportType=0 then
          begin
            conv.Write(fstr(#9));
            case TUser.Int(TUserScore) of
              0:conv.Write(fstr('P'));
              1:conv.Write(fstr('U'));
              2:conv.Write(fstr('L'));
              3:conv.Write(fstr('M'));
            end;
          end;
        end;
        conv.Write(fstr(#13#10));
      end;

      sp.SetProgress(i);
      sp.ProcessMessages;
      if sp.ModalResult=mrCancel then
        raise EAbort.Create('User aborted');
    end;
    conv.Flush;

  finally
    sl.Free;
    FreeAndNil(conv);
    Screen.Cursor:=crDefault;
    sp.Free;
  end;
end;

procedure TfVocab.ImportVocab;
var addw,catw:integer;
begin
  if not OpenDialog1.Execute then
    exit;

  if pos('.WKL',uppercase(OpenDialog1.FileName))>0 then
    ImportVocabFromWkl(OpenDialog1.FileName, catw, addw)
  else
    ImportVocabFromCsv(OpenDialog1.FileName, catw, addw);

  //We've been doing silent AddCategories, so refresh now
  WordCategoryListChanged;
  ShowIt(false);
  Application.MessageBox(
    pchar(_l('^e'+inttostr(catw)+' words imported'#13+inttostr(addw)+' new words')),
    pchar(_l('#00851^eWord list imported')),
    MB_ICONINFORMATION or MB_OK);
end;

procedure TfVocab.ImportVocabFromWkl(const filename: string; out catw, addw: integer);
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

procedure TfVocab.ImportVocabFromCsv(const filename: string; out catw, addw: integer);
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
  conv: TStreamDecoder;

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
    conv := TStreamDecoder.Open(filename,Conv_ChooseType(curlang='c',Conv_DetectType(filename)).Create);

    linc:=0;
    repeat
      s:=conv.ReadLn();
      if s=UH_CR then inc(linc);
    until s='';

    conv.Rewind;

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
    s:=conv.ReadLn;

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
                s2:=DeconvertPinYin(rpy_user,curphon);
                curphon:=DbRomajiToKana(DeconvertPinYin(rpy_user,curphon),'c',[rfDeleteInvalidChars])
              end else
                curphon:=DbRomajiToKana(fstrtouni(curphon),'j',[rfDeleteInvalidChars]);
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

      s:=conv.ReadLn;
    until s='';
    FreeAndNil(conv);

    sp.show;
    sp.SetMessage(_l('#00907^eRebuilding indexes'));
    awf.Commit();
    FreeAndNil(awf);

  finally
    FreeAndNil(awf);
    Screen.Cursor:=crDefault;
    sp.Free;
  end;
end;

procedure TfVocab.Reset;
begin
  curphonetic:='';
  curkanji:='';
  fVocabDetails.Reset;
end;

procedure TfVocab.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect:=true;
end;

procedure TfVocab.RemoveWordsFromCategory;
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
    RemoveWordFromCategory(lastwordind, GetSelCatIdx(fVocabDetails.lbCategories));
  end;
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfVocab.Button14Click(Sender: TObject);
begin
  showmessage(_l('#00150^eFeature not implemented yet.'));
end;


type
  TVocabPainter = class(TPrintPainter)
    function GetPageNum(Canvas: TCanvas; Width, Height: integer): integer;
      override;
    procedure DrawPage(Canvas: TCanvas; PageNum: integer; Width, Height: integer;
      OrigWidth, OrigHeight: integer); override;
    procedure Configure; override;
  end;

function TVocabPainter.GetPageNum(canvas:TCanvas; width,height:integer):integer;
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
  result:=((fVocab.wl.Count-1) div (pr*c))+1;
end;

procedure TVocabPainter.DrawPage(canvas:TCanvas; pagenum:integer;
  width,height,origwidth,origheight:integer);
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
    if ((pagenum-1)*pr*c+i+j*pr)<fVocab.wl.Count then
    begin
      wn:=strtoint(fVocab.wl[(pagenum-1)*pr*c+i+j*pr]);
      TUser.Locate('Index',wn);
      tm:=fstr(remmark(TUser.Str(TUserEnglish)));
      tk:=TUser.Str(TUserPhonetic);
      tr:=fstr(KanaToRomaji(TUser.Str(TUserPhonetic),curlang));
      if showroma then
        tp:=fstr(KanaToRomaji(TUser.Str(TUserPhonetic),curlang))
      else
        tp:=TUser.Str(TUserPhonetic);
      if (not fSettings.CheckBox17.Checked) or (FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0) then
        tw:=TUser.Str(TUserKanji)
      else
        tw:='';
      ls := 0;
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
          if fSettings.cbInsideLines.Checked then
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
          DrawUnicode(canvas,ph+round(ph*0.4)+((width-ph*2) div 4)*(k-1),ph*i+round(ph*0.1)+(height-ph*pr) div 2,round(ph*0.8),t,ft)
        else
          DrawKana(canvas,ph+round(ph*0.4)+((width-ph*2) div 4)*(k-1),ph*i+round(ph*0.1)+(height-ph*pr) div 2,round(ph*0.8),tk,ft,showroma,curlang);
        if fSettings.cbOutsideLines.Checked then
        begin
          canvas.MoveTo(ph+((width-ph*2) div 4)*(k-1),ph*i+(height-ph*pr) div 2);
          canvas.LineTo(ph+((width-ph*2) div 4)*(k-1),ph+ph*i+(height-ph*pr) div 2);
        end;
      end;
      if fSettings.cbOutsideLines.Checked then
      begin
        canvas.MoveTo(ph+((width-ph*2) div 4)*(ls-1),ph*i+(height-ph*pr) div 2);
        canvas.LineTo(ph+((width-ph*2) div 4)*(ls-1),ph+ph*i+(height-ph*pr) div 2);
      end;
    end;
  end;
end;

procedure TVocabPainter.Configure();
begin
  fSettings.pcPages.ActivePage:=fSettings.tsWordListPrinting;
  fSettings.ShowModal;
end;

{ This is needed so that fWordList can print its list through us. Ugh. }
procedure TfVocab.PrintList(const ACaption: string);
var painter: TVocabPainter;
begin
  painter := TVocabPainter.Create;
  try
    PrintPreview(painter, ACaption);
  finally
    FreeAndNil(painter);
  end;
end;

procedure TfVocab.btnPrintVocabListClick(Sender: TObject);
begin
  PrintList(_l('#00828^eVocabulary list'));
end;


procedure TfVocab.Button11Click(Sender: TObject);
begin
  fMenu.SaveUserData;
end;

procedure TfVocab.Button12Click(Sender: TObject);
begin
  TUser.Free;
  TUserIdx.Free;
  fMenu.LoadUserData;
  ShowIt(false);
end;

procedure TfVocab.ListBox1Click(Sender: TObject);
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

procedure TfVocab.SetWordsLearnState(ls: integer);
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

procedure TfVocab.AddWordsToCategory(category: string);
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

procedure TfVocab.MoveWordsInCategory(moveDir: TMoveDirection);
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

  //Scan through category and determine word positions in it
  ap := -1;
  bp := -1;
  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.Locate('Number',StrToInt(wlc[StringGrid1.Row-1]));
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetWord)=ai then ap:=TUserSheet.Int(TUserSheetPos);
    if TUserSheet.Int(TUserSheetWord)=bi then bp:=TUserSheet.Int(TUserSheetPos);
    TUserSheet.Next;
  end;
  Assert(ap>0);
  Assert(bp>0);

  //Swap positions
  TUserSheet.Locate('Number',StrToInt(wlc[StringGrid1.Row-1]));
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetPos)=ap then
      TUserSheet.Edit([TUserSheetPos],[inttostr(bp)])
    else
      if TUserSheet.Int(TUserSheetPos)=bp then
        TUserSheet.Edit([TUserSheetPos],[inttostr(ap)]);
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

procedure TfVocab.DeleteWords();
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

//TODO: Convert to Unicode.
procedure TfVocab.btnRecommendKanjiClick(Sender: TObject);
var csl:TStringList;
    i,j,k:integer;
    s,s2:string;
    v,vb,mv:integer;
    vs,vbs:string;
    a,b:integer;
    tol:integer;
  newClipText: FString;
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
        if not IsKnown(KnownLearned,TChar.Fch(TChar.fUnicode)) then
        begin
          v:=trunc(ln(TChar.Int(TChar.fChStrokeCount))*5000);
          if TChar.Int(TChar.fJouyouGrade)<10 then
            v:=v+TChar.Int(TChar.fJouyouGrade)*4000 else v:=v+40000;
          v:=v+20000;
          if TChar.Int(TChar.fJpFrequency)<65535 then
            v:=v+TChar.Int(TChar.fJpFrequency)*3 else v:=v+7000;
         //If this char has a radical
          if TRadicals.Locate('Number',GetCharRadicalNumber(s2,12))
          and TChar.Locate('Unicode',TRadicals.Str(TRadicals.fUnicode)) then begin
            if TRadicals.Str(TRadicals.fUnicode)=s2 then
              dec(v,3000) //if the char is the radical itself
            else
            if not IsKnown(KnownLearned,TChar.Fch(TChar.fUnicode)) then
              inc(v,8000); //if the radical is not learned
          end;
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
    newClipText:='';
    b:=50000;
    tol:=10000;
    for i:=0 to csl.Count-1 do
    begin
      a:=strtoint(copy(csl[i],1,5));
      if a<b then b:=a;
      if a<b+tol then
      begin
        newClipText := newClipText + copy(csl[i],11,4);
        tol:=tol-tol div 3;
      end;
    end;
    Clipboard.Text := newClipText;
//    s:='';
//    for i:=0 to csl.Count-1 do
//      if strtoint(copy(csl[i],1,5))<b+5000 then s:=s+copy(csl[i],1,5)+'-'+copy(csl[i],6,5)+'-'+copy(csl[i],11,4)+#13;
    csl.Free;
  end;
end;

procedure TfVocab.StringGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Upcase(key)='P') and (fVocabDetails.btnSetProblematic.Enabled) then SetWordsLearnState(0);
  if (Upcase(key)='U') and (fVocabDetails.btnSetUnlearned.Enabled) then SetWordsLearnState(1);
  if (Upcase(key)='L') and (fVocabDetails.btnSetLearned.Enabled) then SetWordsLearnState(2);
  if (Upcase(key)='M') and (fVocabDetails.btnSetMastered.Enabled) then SetWordsLearnState(3);
  if (Upcase(key)='A') and (fVocabDetails.btnAddToCategory.Enabled) then AddWordsToCategory(fVocabDetails.cbAddCategory.text);
  if (key=',') and (fVocabDetails.btnMoveUpInCategory.Enabled) and (fVocabDetails.btnMoveUpInCategory.Visible) then MoveWordsInCategory(mdUp);
  if (key='.') and (fVocabDetails.btnMoveDownInCategory.Enabled) and (fVocabDetails.btnMoveDownInCategory.Visible) then MoveWordsInCategory(mdDown);
end;

procedure TfVocab.btnLearningListClick(Sender: TObject);
var fWordList: TfWordList;
  lname:string;
  i:integer;
begin
  lname:=formatdatetime('yyyy/mm/dd',now);
  i:=1;
  while TUserCat.Locate('Name',lname+' ('+inttostr(i)+')') do inc(i);
  lname:=lname+' ('+inttostr(i)+')';
  fWordList := TfWordList.Create(Self);
  try
    fWordList.Edit1.Text:=lname;
    fWordList.ShowModal;
  finally
    FreeAndNil(fWordList);
  end;
  ShowIt(false);
end;

procedure TfVocab.btnExamplesClick(Sender: TObject);
begin
  fMenu.aVocabExamples.Execute;
end;

procedure TfVocab.btnListSettingsClick(Sender: TObject);
begin
  fMenu.aVocabSettings.Execute;
end;

procedure TfVocab.btnWordDetailsClick(Sender: TObject);
begin
  fMenu.aVocabDetails.Execute;
end;

procedure TfVocab.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  IntTip.MouseMove(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfVocab.btnAddWordClick(Sender: TObject);
begin
  fVocabAdd.ModalAddWord();
end;

procedure TfVocab.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then ScreenTip.PopupImmediate(true);
end;

procedure TfVocab.StringGrid1Click(Sender: TObject);
var i:integer;
begin
  if StringGrid1.Selection.Bottom-StringGrid1.Selection.Top>0 then
  begin
    Reset;
    curword:=-1;
    curkanji:=fstr(_l('#00928^e<multiple words>'));
    fVocabDetails.ClearCategories;
    for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
      fVocabDetails.AddWordCategories(strtoint(wl[i-1]));
    fVocabDetails.SetMultipleWords;
    exit;
  end;
//  Reset;
  lastwordind:=strtoint(wl[StringGrid1.Row-1]);
  if not TUser.Locate('Index',lastwordind) then raise Exception.Create(eWordNotLocated);
  curword:=lastwordind;
  curkanji:=TUser.Str(TUserKanji);
  curphonetic:=TUser.Str(TUserPhonetic);
  fExamples.SetExamples(curkanji);
  fVocabDetails.ClearCategories;
  fVocabDetails.AddWordCategories(lastwordind);
  fVocabDetails.SetSingleWord;
  if fVocabFilters.rgSort.ItemIndex=0 then
  begin
    fVocabDetails.btnMoveUpInCategory.Enabled:=(StringGrid1.Row>1) and (wlc[StringGrid1.Row-2]=wlc[StringGrid1.Row-1]);
    fVocabDetails.btnMoveDownInCategory.Enabled:=(StringGrid1.Row<wlc.Count) and (wlc[StringGrid1.Row]=wlc[StringGrid1.Row-1]);
  end;
  AnnotShowMedia(curkanji,curphonetic);
end;

procedure TfVocab.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then IntTip.MouseUp;
end;

procedure TfVocab.SearchWord(wordind: integer);
begin
  lastwordind:=wordind;
  ShowIt(true);
end;

end.
