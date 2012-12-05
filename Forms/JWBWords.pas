unit JWBWords;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, Buttons, RXCtrls, Tabs, CheckLst;

type
  TfWords = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    Shape7: TShape;
    RxLabel1: TRxLabel;
    Label24: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    StringGrid1: TStringGrid;
    Button9: TButton;
    Button10: TButton;
    Button15: TButton;
    Button18: TButton;
    Button19: TButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure UserAdd_PaintBox2Paint(Sender: TObject);
    procedure UserAdd_Button1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure UserDetails_Edit4Change(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure UserDetails_Button3Click(Sender: TObject);
    procedure UserDetails_Button5Click(Sender: TObject);
    procedure UserDetails_Button6Click(Sender: TObject);
    procedure UserDetails_Button7Click(Sender: TObject);
    procedure UserDetails_Button8Click(Sender: TObject);
    procedure UserDetails_Button2Click(Sender: TObject);
    procedure UserDetails_PaintBox1Paint(Sender: TObject);
    procedure UserDetails_PaintBox6Paint(Sender: TObject);
    procedure UserDetails_Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure UserDetails_ComboBox2Change(Sender: TObject);
    procedure UserDetails_Button4Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure UserDetails_SpeedButton4Click(Sender: TObject);
    procedure UserDetails_SpeedButton5Click(Sender: TObject);
    procedure UserCategory_SpeedButton2Click(Sender: TObject);
    procedure UserCategory_SpeedButton3Click(Sender: TObject);
    procedure Button19Click(sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    WordListCount:integer;
    twkanji,twphonetic,twmeaning:string;
    procedure DoStatistic;
    procedure ShowIt(warningifnotfound:boolean);
    procedure Reset;
    function AddWord(kanji,phonetic,english,category:string;cattype:char;nomessages:boolean;status:integer):boolean;
    function ListWordCategories(word:integer;sl:TStringList;delcategory:string;scanallow:boolean):boolean;
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
    procedure SetGroupStatus(st:integer);
    procedure SearchWord(wordind:integer);
    { Public declarations }
  end;

var
  fWords: TfWords;

implementation

uses JWBMenu, JWBUnit, JWBNewCategory, JWBPrint, JWBSettings,
  JWBStatistics, JWBWordList, JWBWait, JWBUserDetails, JWBUserAdd,
  JWBUserFilters, JWBUserCategory, StdPrompt, PKGWrite, JWBExamples, JWBUser,
  JWBConvert, JWBWordsExpChoose;

var wl,wlc:TStringList;
    ll,ltl:TStringList;
    curphonetic,curkanji:string;
    lastwordind:integer;
    twi:integer;
    twchg:array[0..3,0..3] of integer;
    lastwordadded:boolean;

{$R *.DFM}

function TfWords.ListWordCategories(word:integer;sl:TStringList;delcategory:string;scanallow:boolean):boolean;
var i:integer;
    s:string;
begin
  result:=false;
  TUserSheet.SetOrder('Word_Ind');
  TUserSheet.Locate('Word',inttostr(word),true);
  sl.Clear;
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
  begin
    TUserCat.Locate('Index',TUserSheet.Str(TUserSheetNumber),true);
    if delcategory=copy(TUserCat.Str(TUserCatName),3,length(TUserCat.Str(TUserCatName))-2) then TUserSheet.Delete else
    begin
      s:=TUserCat.Str(TUserCatName);
      sl.Add(s);
      if scanallow then
      begin
        i:=fUserFilters.ListBox1.Items.IndexOf(copy(s,3,length(s)-2));
        if (i<>-1) and (fUserFilters.ListBox1.Checked[i]) and (copy(s,1,1)=curlang) then result:=true;
      end;
    end;
    TUserSheet.Next;
  end;
end;

procedure TfWords.ShowIt(warningifnotfound:boolean);
var sl:TStringList;
    status:string;
    b:boolean;
    stp:string;
    cl:TStringList;
    cls:string;
    i,j:integer;
    sw:integer;
    s:string;
    all:boolean;
    a:integer;
    cats:string;
    rect:TRect;
begin
  if not fWords.Visible then exit;
  wl.Clear;
  wlc.Clear;
  sw:=0;
  cl:=TStringList.Create;
  sl:=TStringList.Create;
  InitWordGrid(StringGrid1,true,false);
  if fUserFilters.RadioGroup2.ItemIndex>0 then
  begin
    case fUserFilters.RadioGroup2.ItemIndex of
      1:TUser.SetOrder('Phonetic_Ind');
      2:TUser.SetOrder('Kanji_Ind');
      3:TUser.SetOrder('English_Ind');
      4:TUser.SetOrder('Added_Ind');
      5:TUser.SetOrder('Score_Ind');
    end;
    TUser.First;
    while not TUser.EOF do
    begin
      if not (((not fUserFilters.CheckBox1.Checked) and (TUser.Int(TUserScore)=1)) or
              ((not fUserFilters.CheckBox8.Checked) and (TUser.Int(TUserScore)=2)) or
              ((not fUserFilters.CheckBox9.Checked) and (TUser.Int(TUserScore)=3)) or
              ((not fUserFilters.CheckBox11.Checked) and (TUser.Int(TUserScore)=0))) then
      begin
        stp:=TUser.Str(TUserScore);
        all:=ListWordCategories(TUser.Int(TUserIndex),cl,'',true);
//        for i:=0 to fUserFilters.ListBox1.Items.Count-1 do
//          if (fUserFilters.ListBox1.Checked[i]) and (cl.IndexOf(curlang+'~'+fUserFilters.ListBox1.Items[i])<>-1) then all:=true;
        if all then
        begin
          cls:='';
          for i:=0 to cl.Count-1 do cls:=cls+', '+StripCatName(cl[i]);
          if length(cls)>0 then delete(cls,1,2);
          AddWordGrid(StringGrid1,'!'+stp+CheckKnownKanji(TUser.Str(TUserKanji)),'!'+stp+TUser.Str(TUserPhonetic),'!'+stp+TUser.Str(TUserEnglish),'!'+stp+cls);
          wl.Add(TUser.Str(TUserIndex));
          if TUser.Int(TUserIndex)=lastwordind then sw:=wl.Count;
        end;
      end;
      TUser.Next;
    end;
  end else
  begin
    for i:=0 to fUserFilters.ListBox1.Items.Count-1 do if fUserFilters.ListBox1.Checked[i] then
    begin
      cats:=fUserFilters.ListBox1.Items[i];
      TUserCat.Locate('Name',curlang+'~'+fUserFilters.ListBox1.Items[i],false);
      a:=TUserCat.Int(TUserCatIndex);
      TUserSheet.SetOrder('Sheet_Ind');
      TUserSheet.Locate('Number',inttostr(a),true);
      j:=0;
      while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetNumber)=a) do
      begin
        inc(j);
        TUser.Locate('Index',TUserSheet.Str(TUserSheetWord),true);
        if not (((not fUserFilters.CheckBox1.Checked) and (TUser.Int(TUserScore)=1)) or
                ((not fUserFilters.CheckBox8.Checked) and (TUser.Int(TUserScore)=2)) or
                ((not fUserFilters.CheckBox9.Checked) and (TUser.Int(TUserScore)=3)) or
                ((not fUserFilters.CheckBox11.Checked) and (TUser.Int(TUserScore)=0))) then
        begin
          stp:=TUser.Str(TUserScore);
//          AddWordGrid(StringGrid1,'!14E00','4E00','al','al');
          AddWordGrid(StringGrid1,'!'+stp+CheckKnownKanji(TUser.Str(TUserKanji)),'!'+stp+TUser.Str(TUserPhonetic),'!'+stp+TUser.Str(TUserEnglish),'!'+stp+cats+' #'+inttostr(j));
          wl.Add(TUser.Str(TUserIndex));
          wlc.Add(inttostr(a));
          if TUser.Int(TUserIndex)=lastwordind then sw:=wl.Count;
        end;
        TUserSheet.Next;
      end;
    end;
  end;
  FinishWordGrid(StringGrid1);
  Reset;
  if (sw=0) and (warningifnotfound) then Application.MessageBox(pchar(_l('^eThe searched word does not match the current filters.^cHledané slovo neodpovídá aktuálním filtrùm.')),
    pchar(_l('#00937^eWord not found^cSlovo nebylo nalezeno')),MB_ICONWARNING or MB_OK);
  if sw=0 then sw:=1;
  if StringGrid1.Visible then StringGrid1Click(self);
  if StringGrid1.Visible then StringGrid1.Row:=sw;
  s:=_l('#00938^cSlovíèka^eVocabulary');
  s:=s+' ('+inttostr(wl.Count)+')';
  RxLabel1.Caption:=s;
  cl.Free;
  sl.Free;
  WordListCount:=wl.Count;
  Button18.Enabled:=StringGrid1.Visible;
  Button19.Enabled:=StringGrid1.Visible;
  if StringGrid1.Visible then StringGrid1.SetFocus;
end;

procedure TfWords.FormShow(Sender: TObject);
begin
  fMenu.ShowForm(SpeedButton2,fMenu.aUserSettings,fUserFilters);
  fMenu.ShowForm(SpeedButton1,fMenu.aUserExamples,fExamples);
//  fMenu.ShowForm(SpeedButton3,fMenu.aUserCategory,fUserCategory);
  fMenu.ShowForm(SpeedButton4,fMenu.aUserDetails,fUserDetails);
  fMenu.aUser.Checked:=true;
  ShowIt(false);
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
    i,j:integer;
    a1,a2:string;
begin
  if (length(category)<2) or (category[2]<>'~') then category:=curlang+'~'+category;
  result:=false;
  if kanji='' then
  begin
    if not nomessages then Application.MessageBox(pchar(_l('^eWriting is not filled. Cannot add word.^cZápis není vyplnìn. Nemohu pøidat slovo.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
    exit;
  end;
  if phonetic='' then
  begin
    if not nomessages then Application.MessageBox(pchar(_l('#00840^ePhonetic is not filled. Cannot add word.^cÈtení není vyplnìno. Nemohu pøidat slovo.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
    exit;
  end;
  if english='' then
  begin
    if not nomessages then Application.MessageBox(pchar(_l('#00841^eMeaning is not filled. Cannot add word.^cVýznam není vyplnìn. Nemohu pøidat slovo.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
    exit;
  end;
  if category='' then
  begin
    if not nomessages then Application.MessageBox(pchar(_l('#00842^eCategory is not filled. Cannot add word.^cKategorie není vyplnìna. Nemohu pøidat slovo.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
    exit;
  end;
  if TUserCat.Locate('Name',category,false) then cat:=TUserCat.Int(TUserCatIndex) else
  begin
    if cattype='?'then
    begin
      fNewCategory.Edit1.Text:=StripCatName(category);
      if fNewCategory.ShowModal<>idOK then exit;
      case fNewCategory.RadioGroup1.ItemIndex of
        0:cattype:='L';
        1:cattype:='G';
        2:cattype:='T';
      end;
      category:=curlang+'~'+fNewCategory.Edit1.Text;
    end;
    inc(MaxCategoryIndex);
    if TUserCat.Locate('Name',category,false) then cat:=TUserCat.Int(TUserCatIndex) else
    begin
      TUserCat.Insert([inttostr(MaxCategoryIndex),category,inttostr(ord(cattype)),FormatDateTime('yyyymmdd',now)]);
      fMenu.RefreshCategory;
      cat:=MaxCategoryIndex;
    end;
  end;
  TUser.SetOrder('Kanji_Ind');
  TUser.Locate('Kanji',kanji,false);
  insertword:=true;
  wordidx:=0;
  while (not TUser.EOF) and (TUser.Str(TUserKanji)=kanji) do
  begin
    if (TUser.Str(TUserPhonetic)=phonetic) then
    begin
      insertword:=false;
      wordidx:=TUser.Int(TUserIndex);
    end;
    TUser.Next;
  end;
  catord:=1;
  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.Locate('Number',inttostr(cat),true);
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetNumber)=cat) do
  begin
    if TUserSheet.Int(TUserSheetPos)>=catord then catord:=TUserSheet.Int(TUserSheetPos)+1;
    if TUserSheet.Int(TUserSheetWord)=wordidx then
    begin
      if not nomessages then Application.MessageBox(pchar(_l('#00843^eThis word is already in vocabulary and is in this category.^cToto slovo již bylo pøidáno a je v této kategorii.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
      exit;
    end;
    TUserSheet.Next;
  end;
  if insertword then
  begin
    inc(MaxUserIndex);
    wordidx:=MaxUserIndex;
    if curlang='j'then
    begin
      phonsort:='';
      s:=RomajiToKana('H'+KanaToRomaji(phonetic,1,'j'),1,true,'j');
      for i:=0 to length(s) div 4-1 do
      begin
        s2:=copy(s,i*4+1,4);
        a1:='';
        a2:='';
        for j:=0 to (romasortl.Count div 2)-1 do
        begin
          if romasortl[j*2]=s2 then a1:=romasortl[j*2+1];
          if romasortl[j*2]=copy(s2,1,3)+chr(ord(s2[4])+1) then a2:=romasortl[j*2+1];
        end;
        if a1='' then phonsort:=phonsort+a2 else phonsort:=phonsort+a1;
      end;
    end else phonsort:=KanaToRomaji(phonetic,1,'c');
//    showmessage(s+#13+KanaToRomaji(s,1)+#13+phonsort);
    TUser.Insert([inttostr(MaxUserIndex),english,phonetic,phonsort,
      kanji,FormatDateTime('yyyymmdd',now),'00000000','00000000','00000000','0',inttostr(status),inttostr(status)]);
    s:=kanji;
    beg:=true;
    while s<>'' do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2,false) then
      begin
        if beg then bs:='T'else bs:='F';
        TUserIdx.Insert([inttostr(MaxUserIndex),TChar.Str(TCharUnicode),bs]);
      end;
      beg:=false;
    end;
  end;
  TUserSheet.Insert([inttostr(wordidx),inttostr(cat),inttostr(catord)]);
  if (not nomessages) and (fSettings.CheckBox70.Checked) then if insertword then
    Application.MessageBox(pchar(_l('#00844^eNew word was successfully added into vocabulary.^cNové slovo bylo úspìšnì pøidáno.')),pchar(_l('#00845^eSuccess^cÚspìch')),MB_ICONINFORMATION or MB_OK)
    else Application.MessageBox(pchar(_l('#00846^eWord was added into new category.^cSlovo bylo pøidáno do nové kategorie.')),pchar(_l('#00845^eSuccess^cÚspìch')),MB_ICONINFORMATION or MB_OK);
  if not nomessages then ShowIt(false);
  if not nomessages then fMenu.ChangeUserData;
  lastwordadded:=insertword;
  result:=true;
end;

procedure TfWords.UserAdd_PaintBox2Paint(Sender: TObject);
begin
  fUserAdd.PaintBox2.Canvas.Brush.Color:=clWindow;
  DrawUnicode(fUserAdd.PaintBox2.Canvas,1,1,22,clip,FontJapanese);
end;

procedure TfWords.UserAdd_Button1Click(Sender: TObject);
begin
  if AddWord(clip,RomajiToKana(fUserAdd.Edit1.Text,romasys,true,curlang),fUserAdd.Edit3.Text,fUserAdd.ComboBox1.Text,'?',false,1) then
  begin
    fUserAdd.Edit1.Text:='';
    fUserAdd.PaintBox3.Invalidate;
    fUserAdd.Edit3.Text:='';
  end;
end;

procedure TfWords.RadioGroup2Click(Sender: TObject);
begin
  ShowIt(false);
end;

procedure TfWords.CheckBox1Click(Sender: TObject);
begin
  ShowIt(false);
end;

procedure TfWords.UserDetails_Edit4Change(Sender: TObject);
begin
  fUserDetails.Button3.Default:=true;
  fUserDetails.Button4.Default:=false;
  fUserDetails.Button2.Default:=false;
end;

procedure TfWords.Button9Click(Sender: TObject);
var t:textfile;
    i,j:integer;
    sl:TStringList;
function ReplSemi(s:string):string;
begin
  while pos(';',s)>0 do s[pos(';',s)]:=',';
  result:=s;
end;
begin
  if SaveDialog1.Execute then
  begin
    if pos('.WKL',uppercase(SaveDialog1.FileName))>0 then
    begin
      Application.MessageBox(pchar(_l('#00899^eWKL format is outdated. Import/export routine is maintained for compatibility only. Please use CSV format in the future.'#13+
        '^cFormát WKL je zastaralý. Importovací/exportovací rutina je zachována pouze kvùli kompatibilitì.')),pchar(_l('#00900^eNotice^cUpozornìní')),MB_ICONWARNING or MB_OK);
      Screen.Cursor:=crHourGlass;
      assignfile(t,SaveDialog1.FileName);
      rewrite(t);
      writeln(t,'WaKan Word List 1');
      writeln(t,'');
      writeln(t,'; created by WaKan - Japanese & Chinese Learning Tool (C) Filip Kabrt 2002-2004');
      writeln(t,'; This file lists words that were exported from user vocabulary.');
      writeln(t,'; Each entry consists of four lines:');
      writeln(t,'; Written (Unicode in hex), Phonetic (Unicode in hex), English (raw text), Category (raw text)');
      writeln(t,'');
      sl:=TStringList.Create;
      for i:=0 to wl.Count-1 do
      begin
        ListWordCategories(strtoint(wl[i]),sl,'',false);
        TUser.Locate('Index',wl[i],true);
        for j:=0 to fUserFilters.ListBox1.Items.Count-1 do
          if (fUserFilters.ListBox1.Checked[j]) and (sl.IndexOf(curlang+'~'+fUserFilters.ListBox1.Items[j])<>-1) then
        begin
          writeln(t,TUser.Str(TUserKanji));
          writeln(t,TUser.Str(TUserPhonetic));
          writeln(t,TUser.Str(TUserEnglish));
          TUserCat.Locate('Name',curlang+'~'+fUserFilters.ListBox1.Items[j],false);
          writeln(t,chr(TUserCat.Int(TUserCatType))+TUserCat.Str(TUserCatName));
        end;
      end;
      sl.Free;
      writeln(t,'.');
      closefile(t);
      Screen.Cursor:=crDefault;
    end else
    begin
      Screen.Cursor:=crHourGlass;
      Conv_Create(SaveDialog1.FileName,Conv_ChooseType(curlang='c',0));
      if fWordsExpChoose.ShowModal=mrCancel then exit;
      Conv_Write(UnicodeToHex(#9' Wakan Word List')+'000D000A');
      Conv_Write(UnicodeToHex(#9'')+'000D000A');
      Conv_Write(UnicodeToHex(#9' created by WaKan - Japanese & Chinese Learning Tool (C) Filip Kabrt 2002-2004')+'000D000A');
      Conv_Write(UnicodeToHex(#9' This file lists words that were exported from user vocabulary.')+'000D000A');
      Conv_Write(UnicodeToHex(#9' Each entry consists of one line where the following values are separated by a delimiter:')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <written>;<phonetic>;<meaning>[;<category>[;<learned>]]')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <category> and <learned> fields are optional.')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <written> - How the word is written (in kanji/hanzi/kana)')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <phonetic> - How the word is pronounced (in kana/Hepburn romaji/BoPoMoFo/PinYin)')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <meaning> - English meaning of the word (cannot contain semicolons!)')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <category> - Name of the category to place the word into (if new, user is asked to specify type) (optional)')+'000D000A');
      Conv_Write(UnicodeToHex(#9' <learned> - Learned state of the word: "P" - problematic, "U" - unlearned, "L" - learned, "M" - mastered (optional)')+'000D000A');
      Conv_Write(UnicodeToHex(#9' Delimiter is the first non-kanji character encountered.')+'000D000A');
      Conv_Write(UnicodeToHex(#9'')+'000D000A');
      sl:=TStringList.Create;
      for i:=0 to wl.Count-1 do
      begin
        ListWordCategories(strtoint(wl[i]),sl,'',false);
        TUser.Locate('Index',wl[i],true);
        for j:=0 to fUserFilters.ListBox1.Items.Count-1 do
          if (fUserFilters.ListBox1.Checked[j]) and (sl.IndexOf(curlang+'~'+fUserFilters.ListBox1.Items[j])<>-1) then
        begin
          Conv_Write(TUser.Str(TUserKanji));
          Conv_Write(UnicodeToHex(#9));
          if not showroma then Conv_Write(TUser.Str(TUserPhonetic)) else
            if curlang='c'then Conv_Write(UnicodeToHex(KanaToRomaji(TUser.Str(TUserPhonetic),1,'c'))) else
                                Conv_Write(UnicodeToHex(KanaToRomaji(TUser.Str(TUserPhonetic),2,'j')));
          Conv_Write(UnicodeToHex(#9));
          Conv_Write(UnicodeToHex(ReplSemi(TUser.Str(TUserEnglish))));
          if fWordsExpChoose.RadioGroup1.ItemIndex<2 then
          begin
            Conv_Write(UnicodeToHex(#9));
            Conv_Write(UnicodeToHex(fUserFilters.ListBox1.Items[j]));
            if fWordsExpChoose.RadioGroup1.ItemIndex=0 then
            begin
              Conv_Write(UnicodeToHex(#9));
              case TUser.Int(TUserScore) of
                0:Conv_Write(UnicodeToHex('P'));
                1:Conv_Write(UnicodeToHex('U'));
                2:Conv_Write(UnicodeToHex('L'));
                3:Conv_Write(UnicodeToHex('M'));
              end;
            end;
          end;
          Conv_Write('000D000A');
        end;
      end;
      sl.Free;
      Conv_Flush;
      Conv_Close;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

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

procedure TfWords.Button10Click(Sender: TObject);
var t:textfile;
    s,s2,s3,s4:string;
    linc:integer;
    sp:TSMPromptForm;
    addw,catw:integer;
    curwrit,curphon,curmean,curcat,curstat:string;
    st:integer;
    delimdetect:boolean;
    delim:string;
    justbeginning,ignoreline:boolean;
    field:integer;
    unknowncat:string;
    awf_lastcatname:string;
    awf_lastcatindex:integer;
    awf_catcount:array[0..65535] of integer;
    awf_insuser:TStringList;
    awf_insusersheet:TStringList;
    awf_insuseridx:TStringList;
    abortprocess:boolean;
  function AddWordFast(kanji,phonetic,english,category:string;cattype:char;nomessages:boolean;status:integer):boolean;
  var s,s2:string;
      beg,insertword:boolean;
      bs:string;
      cat,catord:integer;
      wordidx:integer;
      phonsort:string;
      i,j:integer;
      a1,a2:string;
  begin
    if (length(category)<2) or (category[2]<>'~') then category:=curlang+'~'+category;
    result:=false;
    if category=awf_lastcatname then cat:=awf_lastcatindex
    else if TUserCat.Locate('Name',category,false) then cat:=TUserCat.Int(TUserCatIndex) else
    begin
      if cattype='?'then
      begin
        fNewCategory.Edit1.Text:=StripCatName(category);
        if fNewCategory.ShowModal<>idOK then begin abortprocess:=true; exit; end;
        case fNewCategory.RadioGroup1.ItemIndex of
          0:cattype:='L';
          1:cattype:='G';
          2:cattype:='T';
        end;
        category:=curlang+'~'+fNewCategory.Edit1.Text;
      end;
      if TUserCat.Locate('Name',category,false) then cat:=TUserCat.Int(TUserCatIndex) else
      begin
        inc(MaxCategoryIndex);
        TUserCat.Insert([inttostr(MaxCategoryIndex),category,inttostr(ord(cattype)),FormatDateTime('yyyymmdd',now)]);
        cat:=MaxCategoryIndex;
      end;
    end;
    awf_lastcatname:=category;
    awf_lastcatindex:=cat;
    insertword:=true;
    wordidx:=0;
    TUser.SetOrder('Kanji_Ind');
    TUser.Locate('Kanji',kanji,false);
    while (not TUser.EOF) and (TUser.Str(TUserKanji)=kanji) do
    begin
      if (TUser.Str(TUserPhonetic)=phonetic) then
      begin
        insertword:=false;
        wordidx:=TUser.Int(TUserIndex);
        break;
      end;
      TUser.Next;
    end;
    catord:=awf_catcount[cat]+1;
    awf_catcount[cat]:=catord;
    TUserSheet.SetOrder('Word_Ind');
    if not insertword then
    begin
      TUserSheet.Locate('Word',inttostr(wordidx),true);
      while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=wordidx) do
      begin
        if TUserSheet.Int(TUserSheetNumber)=cat then
        begin
          lastwordadded:=insertword;
          if not nomessages then Application.MessageBox(pchar(_l('#00843^eThis word is already in vocabulary and is in this category.^cToto slovo již bylo pøidáno a je v této kategorii.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
          exit;
        end;
        TUserSheet.Next;
      end;
    end;
    if insertword then
    begin
      inc(MaxUserIndex);
      wordidx:=MaxUserIndex;
      if curlang='j'then
      begin
        phonsort:='';
        s:=RomajiToKana('H'+KanaToRomaji(phonetic,1,'j'),1,true,'j');
        for i:=0 to length(s) div 4-1 do
        begin
          s2:=copy(s,i*4+1,4);
          a1:='';
          a2:='';
          for j:=0 to (romasortl.Count div 2)-1 do
          begin
            if romasortl[j*2]=s2 then a1:=romasortl[j*2+1];
            if romasortl[j*2]=copy(s2,1,3)+chr(ord(s2[4])+1) then a2:=romasortl[j*2+1];
          end;
          if a1='' then phonsort:=phonsort+a2 else phonsort:=phonsort+a1;
        end;
      end else phonsort:=KanaToRomaji(phonetic,1,'c');
  //    showmessage(s+#13+KanaToRomaji(s,1)+#13+phonsort);
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
        if TChar.Locate('Unicode',s2,false) then
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
    if not nomessages then if insertword then
      Application.MessageBox(pchar(_l('#00844^eNew word was successfully added into vocabulary.^cNové slovo bylo úspìšnì pøidáno.')),pchar(_l('#00845^eSuccess^cÚspìch')),MB_ICONINFORMATION or MB_OK)
      else Application.MessageBox(pchar(_l('#00846^eWord was added into new category.^cSlovo bylo pøidáno do nové kategorie.')),pchar(_l('#00845^eSuccess^cÚspìch')),MB_ICONINFORMATION or MB_OK);
    if not nomessages then ShowIt(false);
    if not nomessages then fMenu.ChangeUserData;
    lastwordadded:=insertword;
    result:=true;
  end;
var i:integer;
begin
  if OpenDialog1.Execute then
  begin
    awf_lastcatname:='';
    awf_lastcatindex:=0;
    for i:=0 to 65535 do awf_catcount[i]:=0;
    TUserSheet.SetOrder('Sheet_Ind');
    TUserSheet.First;
    while not TUserSheet.EOF do
    begin
      if TUserSheet.Int(TUserSheetPos)>awf_catcount[TUserSheet.Int(TUserSheetNumber)] then awf_catcount[TUserSheet.Int(TUserSheetNumber)]:=TUserSheet.Int(TUserSheetPos);
      TUserSheet.Next;
    end;
    TUserSheet.First;
    if pos('.WKL',uppercase(OpenDialog1.FileName))>0 then
    begin
      Application.MessageBox(pchar(_l('#00899^eWKL format is outdated. Import/export routine is maintained for compatibility only. Please use CSV format in the future.'#13+
        '^cFormát WKL je zastaralý. Importovací/exportovací rutina je zachována pouze kvùli kompatibilitì.')),pchar(_l('#00900^eNotice^cUpozornìní')),MB_ICONWARNING or MB_OK);
      assignfile(t,OpenDialog1.Filename);
      system.reset(t);
      linc:=0;
      while not eof(t) do
      begin
        readln(t,s);
        inc(linc);
      end;
      catw:=0;
      addw:=0;
      closefile(t);
      system.reset(t);
      readln(t,s);
      if pos('WaKan Word List',s)<>1 then
      begin
        Application.MessageBox(pchar(_l('#00847^eThis is not a WaKan word list file.^cToto není soubor se slovy WaKan.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
        exit;
      end;
      if s<>'WaKan Word List 1'then
      begin
        Application.MessageBox(pchar(_l('#00848^eThis WaKan word list file version is not supported.^cTato verze souboru se slovy WaKan není podporována.')),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
        exit;
      end;
      sp:=SMProgressDlg(_l('#00215^eVocabulary^cSlovíèka'),_l('#00849^eImporting word list (000000 words imported - 000000 new words)...^cImportuji seznam slovíèek (00000 slov importováno - 000000 nových slov...'),linc);
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
        AddWord(s,s2,s3,copy(s4,2,length(s4)-1),s4[1],true,1);
        inc(catw);
        if lastwordadded then inc(addw);
        if not eof(t) then readln(t,s);
        inc(linc,4);
        sp.show;
        sp.SetMessage(_l('#00901^eImporting word list (^cImportuji seznam slovíèek (')+inttostr(catw)+_l('#00902^e words imported - ^c slov importováno - ')+inttostr(addw)+_l('#00903^e new words)...^c nových slov...'));
        sp.SetProgress(linc);
      end;
      Screen.Cursor:=crDefault;
      closefile(t);
      sp.Free;
      fMenu.ChangeUserData;
      ShowIt(false);
      Application.MessageBox(pchar(_l('^e'+inttostr(catw)+' words imported'#13+inttostr(addw)+' new words^c'+inttostr(catw)+' slov importováno'#13+inttostr(addw)+' nových slov')),pchar(_l('#00851^eWord list imported^cSeznam slovíèek naimportován')),MB_ICONINFORMATION or MB_OK);
    end else
    begin
      abortprocess:=false;
      Conv_Open(OpenDialog1.FileName,Conv_ChooseType(curlang='c',Conv_DetectType(OpenDialog1.FileName)));
      linc:=0;
      repeat
        s:=Conv_Read;
        if s='000D'then inc(linc);
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
      catw:=0;
      addw:=0;
      sp:=SMProgressDlg(_l('#00215^eVocabulary^cSlovíèka'),_l('#00849^eImporting word list (000000 words imported - 000000 new words)...^cImportuji seznam slovíèek (00000 slov importováno - 000000 nových slov...'),linc);
      linc:=0;
      Screen.Cursor:=crHourGlass;
      s:=Conv_Read;
      delim:=UnicodeToHex(';');
      delimdetect:=true;
      awf_insuser:=TStringList.Create;
      awf_insusersheet:=TStringList.Create;
      awf_insuseridx:=TStringList.Create;
      repeat
        if (delimdetect) and (copy(s,1,2)='00') and ((HexToUnicode(s)<'A') or (HexToUnicode(s)>'Z'))
           and ((HexToUnicode(s)<'a') or (HexToUnicode(s)>'z')) and ((HexToUnicode(s)<'0') or (HexToUnicode(s)>'9')) and (s<>'0020') then
        begin
          delim:=s;
          delimdetect:=false;
        end;
        if justbeginning and (s=delim) then ignoreline:=true;
        justbeginning:=false;
        if not ignoreline then
        begin
          if (s=delim) then
          begin
            inc(field);
            case field of
              1:curwrit:=s2;
              2:curphon:=s2;
              3:curmean:=HexToUnicode(s2);
              4:curcat:=HexToUnicode(s2);
              5:curstat:=HexToUnicode(s2);
            end;
            s2:='';
          end else if s='000A'then
          begin
            inc(field);
            case field of
              1:curwrit:=s2;
              2:curphon:=s2;
              3:curmean:=HexToUnicode(s2);
              4:curcat:=HexToUnicode(s2);
              5:curstat:=HexToUnicode(s2);
            end;
            s2:='';
            if (curwrit<>'') and (curphon<>'') then
            begin
              if (curphon[1]='0') or (copy(curphon,1,4)='2026') then
                if curlang='c'then
                begin
                  s2:=DeconvertPinYin(curphon);
//                  if pos(UnicodeToHex('?'),ConvertPinYin(KanaToRomaji(RomajiToKana(s2,1,false,'c'),1,'c')))<>0 then showmessage(curmean+#13+s2);
//                  if curmean='other people'then showmessage(s2+#13+ConvertPinYin(s2));
//                  if (pos('?',s2)=0) and (pos('r0',s2)=0) then
//                    if ConvertPinYin(s2)<>curphon then showmessage('Mismatch: '+#13+curmean+#13+ConvertPinYin(s2)+#13+curphon+#13+s2);
                  curphon:=RomajiToKana(DeconvertPinYin(curphon),1,true,'c')
                end else curphon:=RomajiToKana(HexToUnicode(curphon),2,true,'j');
              if curcat='' then curcat:=unknowncat;
              if curcat='' then
              begin
                curcat:=InputBox(_l('#00894^eVocabulary category^cKategorie slovíèek'), _l('#00895^eEnter category in which the new words will be added:^cZadejte kategorii do které budou pøidána nová slovíèka'),
                  'noname');
                unknowncat:=curcat;
              end;
              if curstat='' then curstat:='U';
              case uppercase(curstat)[1] of
                'P':st:=0;
                'U':st:=1;
                'L':st:=2;
                'M':st:=3;
              end;
              curcat:=trim(curcat);
              AddWordFast(curwrit,curphon,curmean,curcat,'?',true,st);
              if abortprocess then
              begin
                Screen.Cursor:=crDefault;
                sp.Free;
                exit;
              end;
              inc(catw);
              if lastwordadded then inc(addw);
            end;
          end else if s<>'000D'then s2:=s2+s;
        end;
        if s='000A'then
        begin
          inc(linc);
          field:=0;
          s2:='';
          justbeginning:=true;
          ignoreline:=false;
          if linc mod 10=0 then
          begin
            sp.show;
            sp.SetMessage(_l('#00901^eImporting word list (^cImportuji seznam slovíèek (')+inttostr(catw)+_l('#00902^e words imported - ^c slov importováno - ')+inttostr(addw)+_l('#00903^e new words)...^c nových slov...'));
            sp.SetProgress(linc);
          end;
        end;
        s:=Conv_Read;
      until s='';
      Conv_Close;
      TUser.nocommitting:=true;
      TUserSheet.nocommitting:=true;
      TUserIdx.nocommitting:=true;
      sp.show;
      sp.SetMessage(_l('#00904^eBatch-adding words ^cPøidávám slova ')+'('+inttostr(awf_insuser.Count div 12)+')');
      for i:=0 to (awf_insuser.Count div 12)-1 do
        TUser.Insert([awf_insuser[i*12],awf_insuser[i*12+1],awf_insuser[i*12+2],awf_insuser[i*12+3],awf_insuser[i*12+4],awf_insuser[i*12+5],
                      awf_insuser[i*12+6],awf_insuser[i*12+7],awf_insuser[i*12+8],awf_insuser[i*12+9],awf_insuser[i*12+10],awf_insuser[i*12+11]]);
      sp.show;
      sp.SetMessage(_l('#00905^eBatch-adding word categories ^cPøidávám kategorie slov ')+'('+inttostr(awf_insusersheet.Count div 3)+')');
      for i:=0 to (awf_insusersheet.Count div 3)-1 do
        TUserSheet.Insert([awf_insusersheet[i*3],awf_insusersheet[i*3+1],awf_insusersheet[i*3+2]]);
      sp.show;
      sp.SetMessage(_l('#00906^eBatch-adding character indexes ^cPøidávám indexy znakù ')+'('+inttostr(awf_insuseridx.Count div 3)+')');
      for i:=0 to (awf_insuseridx.Count div 3)-1 do
        TUserIdx.Insert([awf_insuseridx[i*3],awf_insuseridx[i*3+1],awf_insuseridx[i*3+2]]);
      TUser.nocommitting:=false;
      TUserSheet.nocommitting:=false;
      TUserIdx.nocommitting:=false;
      awf_insuser.Free;
      awf_insuseridx.Free;
      awf_insusersheet.Free;
      sp.show;
      sp.SetMessage(_l('#00907^eRebuilding indexes^cVytváøím indexy...'));
      TUser.Reindex;
      TUserSheet.Reindex;
      TUserIdx.Reindex;
      Screen.Cursor:=crDefault;
      sp.Free;
      fMenu.ChangeUserData;
      ShowIt(false);
      Application.MessageBox(pchar(_l('^e'+inttostr(catw)+' words imported'#13+inttostr(addw)+' new words^c'+inttostr(catw)+' slov importováno'#13+inttostr(addw)+' nových slov')),pchar(_l('#00851^eWord list imported^cSeznam slovíèek naimportován')),MB_ICONINFORMATION or MB_OK);
    end;
  end;
  fMenu.RefreshCategory;
end;

procedure TfWords.Reset;
begin
  curphonetic:='';
  curkanji:='';
  fUserDetails.RxLabel3.Caption:='-';
  fUserDetails.Edit4.Text:='';
  fUserDetails.Edit4.ReadOnly:=true;
  fUserDetails.Button3.Enabled:=false;
  fUserDetails.Button4.Enabled:=false;
  fUserDetails.Button13.Enabled:=false;
  fUserDetails.ComboBox2.Enabled:=false;
  fUserDetails.ListBox2.Enabled:=false;
  fUserDetails.ListBox2.Items.Clear;
  fUserDetails.Label15.Caption:='-';
  fUserDetails.Label16.Caption:='-';
  fUserDetails.Label17.Caption:='-';
  fUserDetails.Label18.Caption:='-';
  fUserDetails.Button2.Enabled:=false;
  fUserDetails.Button5.Enabled:=false;
  fUserDetails.Button6.Enabled:=false;
  fUserDetails.Button7.Enabled:=false;
  fUserDetails.Button8.Enabled:=false;
  fUserDetails.SpeedButton4.Visible:=fUserFilters.RadioGroup2.ItemIndex=0;
  fUserDetails.SpeedButton5.Visible:=fUserFilters.RadioGroup2.ItemIndex=0;
  fUserDetails.SpeedButton4.Enabled:=false;
  fUserDetails.SpeedButton5.Enabled:=false;
  fUserDetails.PaintBox1.Invalidate;
  fUserDetails.PaintBox6.Invalidate;
end;

procedure TfWords.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect:=true;
end;

procedure TfWords.UserDetails_Button3Click(Sender: TObject);
begin
  TUser.Edit([TUserEnglish],[fUserDetails.Edit4.Text]);
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UserDetails_Button5Click(Sender: TObject);
var ms:integer;
    b:boolean;
begin
  ms:=0;
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
  begin
    SetGroupStatus(ms);
    exit;
  end;
  if TUser.Int(TUserMaxScore)>ms then ms:=TUser.Int(TUserMaxScore);
  TUser.Edit([TUserScore,TUserMaxScore],['0',inttostr(ms)]);
  StringGrid1Click(self);
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UserDetails_Button6Click(Sender: TObject);
var ms:integer;
    b:boolean;
begin
  ms:=1;
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
  begin
    SetGroupStatus(ms);
    exit;
  end;
  if TUser.Int(TUserMaxScore)>ms then ms:=TUser.Int(TUserMaxScore);
  TUser.Edit([TUserScore,TUserMaxScore],['1',inttostr(ms)]);
  StringGrid1Click(self);
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UserDetails_Button7Click(Sender: TObject);
var ms:integer;
    b:boolean;
begin
  ms:=2;
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
  begin
    SetGroupStatus(ms);
    exit;
  end;
  if TUser.Int(TUserMaxScore)>ms then ms:=TUser.Int(TUserMaxScore);
  TUser.Edit([TUserScore,TUserMaxScore,TUserLearned],['2',inttostr(ms),FormatDateTime('yyyymmdd',now)]);
  StringGrid1Click(self);
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UserDetails_Button8Click(Sender: TObject);
var ms:integer;
    b:boolean;
begin
  ms:=3;
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
  begin
    SetGroupStatus(ms);
    exit;
  end;
  if TUser.Int(TUserMaxScore)>ms then ms:=TUser.Int(TUserMaxScore);
  TUser.Edit([TUserScore,TUserMaxScore,TUserMastered],['3',inttostr(ms),FormatDateTime('yyyymmdd',now)]);
  StringGrid1Click(self);
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UserDetails_Button2Click(Sender: TObject);
var i:integer;
    s:string;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then
  begin
    if Application.MessageBox(pchar(_l('#00925^eReally delete all these words?Opravdu smazat vsechna tato slova?')),
      pchar(_l('#00926^eWarning^cVarovani')),MB_ICONWARNING or MB_YESNO)=idNo then exit;
  end else
  begin
    if Application.MessageBox(pchar(_l('#00852^eReally delete this word?^cOpravdu chcete smazat toto slovo?')),pchar(_l('#00853^eConfirmation^cOvìøení')),MB_ICONWARNING or MB_YESNO)=idNo then
      exit;
  end;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    s:=wl[i-1];
    lastwordind:=strtoint(s);
    if not TUser.Locate('Index',s,true) then showmessage('INTERNAL ERROR. WORD NOT LOCATED');
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

procedure TfWords.UserDetails_PaintBox1Paint(Sender: TObject);
begin
  fUserDetails.PaintBox1.Canvas.Brush.Color:=clWindow;
  DrawKana(fUserDetails.PaintBox1.Canvas,1,1,22,curphonetic,FontJapanese,showroma,romasys,curlang);
end;

procedure TfWords.UserDetails_PaintBox6Paint(Sender: TObject);
begin
  fUserDetails.PaintBox6.Canvas.Brush.Color:=clWindow;
  BeginDrawReg(fUserDetails.paintBox6);
  DrawUnicode(fUserDetails.PaintBox6.Canvas,1,1,22,curkanji,FontJapanese);
  EndDrawReg;
end;

procedure TfWords.UserDetails_Button13Click(Sender: TObject);
var cl:TStringList;
    i:integer;
    s:string;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then if Application.MessageBox(pchar(_l('#00927^eThis operation will affect multiple words.'#13#13'Do you want to continue?^cTato operace ovlivni mnoho slov.'#13#13'Chcete pokracovat?')),
    pchar(_l('#00926^eWarning^cVarovani')),MB_ICONWARNING or MB_YESNO)=idNo then exit;
  cl:=TStringList.Create;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    s:=wl[i-1];
    lastwordind:=strtoint(s);
    if not TUser.Locate('Index',s,true) then showmessage('INTERNAL ERROR. WORD NOT LOCATED');
    ListWordCategories(lastwordind,cl,fUserDetails.ListBox2.Items[fUserDetails.ListBox2.ItemIndex],false);
  end;
  cl.Free;
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.Button14Click(Sender: TObject);
begin
  showmessage(_l('#00150^cFunkce není doposud implementována.^eFeature not implemented yet.'));
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
      TUser.Locate('Index',inttostr(wn),true);
      tm:=UnicodeToHex(strip_fl(TUser.Str(TUserEnglish)));
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
  fSettings.PageControl1.ActivePage:=fSettings.TabSheet5;
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

procedure TfWords.UserDetails_ComboBox2Change(Sender: TObject);
begin
  fUserDetails.Button3.Default:=false;
  fUserDetails.Button4.Default:=true;
  fUserDetails.Button2.Default:=false;
end;

procedure TfWords.UserDetails_Button4Click(Sender: TObject);
var i:integer;
    s:string;
begin
  if StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom then if Application.MessageBox(pchar(_l('#00927^eThis operation will affect multiple words.'#13#13'Do you want to continue?^cTato operace ovlivni mnoho slov.'#13#13'Chcete pokracovat?')),
    pchar(_l('#00926^eWarning^cVarovani')),MB_ICONWARNING or MB_YESNO)=idNo then exit;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    s:=wl[i-1];
    lastwordind:=strtoint(s);
    if not TUser.Locate('Index',s,true) then showmessage('INTERNAL ERROR. WORD NOT LOCATED');
    AddWord(TUser.Str(TUserKanji),TUser.Str(TUserPhonetic),TUser.Str(TUserEnglish),fUserDetails.ComboBox2.text,'?',StringGrid1.Selection.Top<>StringGrid1.Selection.Bottom,1);
  end;
  ShowIt(false);
end;

procedure TfWords.Button15Click(Sender: TObject);
begin
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00828^eVocabulary list^cSeznam slovíèek'));
end;

function perc(i,j:integer):string;
begin
  if j>0 then
  result:=inttostr(i)+' ('+inttostr(round(i/j*100))+'%)'else
  result:=inttostr(i)+' (?%)';
end;

procedure TfWords.DoStatistic;
var i,j,k,l,m,n,o,p,q:integer;
  t:textfile;
  a:pointer;
  KanjiKnown: boolean;
  JouyouGrade: integer;
  InUserIdx: boolean;
begin
  Screen.Cursor:=crHourGlass;
  if ChinesePresent then
    fStatistics.Label10.Caption:=_l('#00854^ePresent^cPøítomna') else
    fStatistics.Label10.Caption:=_l('#00855^eAbsent^cNepøítomna');
  TChar.First;
  i:=0;
  while not TChar.EOF do
  begin
    if TChar.Int(TCharChinese)=0 then inc(i);
    TChar.Next;
  end;
  fStatistics.Label11.Caption:=inttostr(i);
  p:=i;
  fStatistics.Label12.Caption:=inttostr(TChar.RecordCount);
  i:=0; j:=0; k:=0; l:=0; m:=0; n:=0;
  TChar.First;
  TUser.First;
  while not TUser.EOF do
  begin
    if TUser.Int(TUserScore)=0 then inc(i);
    if TUser.Int(TUserScore)>=2 then inc(j);
    if TUser.Int(TUserScore)=3 then inc(k);
    if pos('<spop>',TUser.Str(TUserEnglish))=0 then inc(l);
    if (length(TUser.Str(TUserPhonetic))>1) and (TUser.Str(TUserPhonetic)[3]>='A') then inc(m);
    if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0 then inc(n);
    TUser.Next;
  end;
  TUser.First;
  fStatistics.Label25.Caption:=inttostr(TUser.RecordCount);
  fStatistics.Label26.Caption:=perc(j,TUser.RecordCount);
  fStatistics.Label27.Caption:=perc(k,TUser.RecordCount);
  fStatistics.Label28.Caption:=perc(i,TUser.RecordCount);
  fStatistics.Label35.Caption:=perc(l,TUser.RecordCount);
  fStatistics.Label29.Caption:=perc(m,TUser.RecordCount);
  fStatistics.Label44.Caption:=perc(n,TUser.RecordCount);
  if fSettings.CheckBox26.Checked then
  begin
    {$I-}
    mkdir('stat');
    {$I+}
    ioresult;
    assignfile(t,'stat\'+formatdatetime('yyyymmdd',now)+'.log');
    rewrite(t);
    writeln(t,'WAKAN STATISTICS LOG');
    writeln(t,'date = '+formatdatetime('yyyy/mm/dd',now));
    writeln(t,'total_words = '+inttostr(TUser.RecordCount));
    writeln(t,'learned_words = '+inttostr(j));
    writeln(t,'mastered_words = '+inttostr(k));
    writeln(t,'problematic_words = '+inttostr(i));
    writeln(t,'unpopular_words = '+inttostr(l));
    writeln(t,'katakana_words = '+inttostr(m));
    writeln(t,'known_writing_words = '+inttostr(n));
  end;
  i:=0; j:=0; k:=0; l:=0; m:=0; n:=0; o:=0; q:=0;
  TChar.First;
  while not TChar.EOF do
  begin
    KanjiKnown := IsKnown(KnownLearned,TChar.Fch(TCharUnicode));
    JouyouGrade := TChar.Int(TCharJouyouGrade);
    InUserIdx := TUserIdx.Locate('Kanji',TChar.Str(TCharUnicode),false);
    if KanjiKnown then inc(i);
    if KanjiKnown and (JouyouGrade>=9) then inc(j);
    if (not KanjiKnown) and (JouyouGrade<9) then inc(k);
    if JouyouGrade<9 then inc(o);
    if InUserIdx then inc(l);
    if InUserIdx and (JouyouGrade>=9) then inc(m);
    if KanjiKnown and (TChar.Int(TCharChinese)=1) then inc(n);
    if KanjiKnown then
    begin
      TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),13)),true);
      if TRadicals.Str(TRadicalsUnicode)=TChar.Str(TCharUnicode) then inc(q);
    end;
    TChar.Next;
  end;
  TChar.First;
  fStatistics.Label30.Caption:=perc(l,p);
  fStatistics.Label31.Caption:=perc(i,l);
  fStatistics.Label32.Caption:=perc(k,o);
  fStatistics.Label36.Caption:=perc(j,i);
  fStatistics.Label39.Caption:=perc(m,l);
  fStatistics.Label41.Caption:=perc(n,i);
  fStatistics.Label45.Caption:=perc(q,TRadicals.RecordCount);
  if fSettings.CheckBox26.Checked then
  begin
    writeln(t,'kanji_in_words = '+inttostr(l));
    writeln(t,'known_kanji = '+inttostr(i));
    writeln(t,'unknown_basic_kanji = '+inttostr(k));
    writeln(t,'known_nonbasic_kanji = '+inttostr(j));
    writeln(t,'nonbasic_kanji_in_words = '+inttostr(m));
    writeln(t,'known_chineseonly_chars = '+inttostr(n));
    writeln(t,'known_radicals = '+inttostr(q));
    closefile(t);
  end;
  i:=0;
  Screen.Cursor:=crDefault;
  fStatistics.ShowModal;
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
  if Application.MessageBox(pchar(_l('#00946^cTato funkce vloží do schránky seznam nìkterých doposud nenauèených znakù v aktuálním seznamu slovíèek^eThis function inserts a list of some unlearned characters that are in current vocabulary list')+#13+
  _l('#00945^cseøazený podle vhodnosti znakù k nauèení.'#13#13'Chcete tuto funkci spustit?^einto the clipboard sorted by usefullness.'#13#13'Do you want to start this function?')),pchar(_l('#00856^cDoporuèení znakù^eCharacter recommendation')),MB_ICONINFORMATION or MB_YESNO)=idYes then
  begin
    csl:=TStringList.Create;
    for i:=0 to wl.Count-1 do
    begin
      TUser.Locate('Index',wl[i],true);
      s:=TUser.Str(TUserKanji);
      for j:=1 to length(s) div 4 do
      begin
        s2:=copy(s,((j-1)*4)+1,4);
        if TChar.Locate('Unicode',s2,false) then
        if not IsKnown(KnownLearned,TChar.Fch(TCharUnicode)) then
        begin
          v:=trunc(ln(TChar.Int(TCharStrokeCount))*5000);
          if TChar.Int(TCharJouyouGrade)<10 then
            v:=v+TChar.Int(TCharJouyouGrade)*4000 else v:=v+40000;
          v:=v+20000;
          if TChar.Int(TCharJpFrequency)<65535 then
            v:=v+TChar.Int(TCharJpFrequency)*3 else v:=v+7000;
          TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),12)),true);
          TChar.Locate('Unicode',TRadicals.Str(TRadicalsUnicode),false);
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
    fMenu.ChangeClipboard;
//    for i:=0 to csl.Count-1 do
//      if strtoint(copy(csl[i],1,5))<b+5000 then s:=s+copy(csl[i],1,5)+'-'+copy(csl[i],6,5)+'-'+copy(csl[i],11,4)+#13;
    csl.Free;
  end;
end;

procedure TfWords.StringGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Upcase(key)='L') and (fUserDetails.Button7.Enabled) then UserDetails_Button7Click(sender);
  if (Upcase(key)='M') and (fUserDetails.Button8.Enabled) then UserDetails_Button8Click(sender);
  if (Upcase(key)='U') and (fUserDetails.Button6.Enabled) then UserDetails_Button6Click(sender);
  if (Upcase(key)='P') and (fUserDetails.Button5.Enabled) then UserDetails_Button5Click(sender);
  if (Upcase(key)='A') and (fUserDetails.Button4.Enabled) then UserDetails_Button4Click(sender);
  if (key=',') and (fUserDetails.SpeedButton4.Enabled) and (fUserDetails.SpeedButton4.Visible) then UserDetails_SpeedButton4Click(sender);
  if (key='.') and (fUserDetails.SpeedButton5.Enabled) and (fUserDetails.SpeedButton5.Visible) then UserDetails_SpeedButton5Click(sender);
end;

procedure TfWords.UserDetails_SpeedButton4Click(Sender: TObject);
var ai,bi:integer;
    ap,bp:integer;
    s:string;
    i:integer;
begin
  ai:=strtoint(wl[StringGrid1.Row-1]);
  bi:=strtoint(wl[StringGrid1.Row-2]);
  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.Locate('Number',wlc[StringGrid1.Row-1],true);
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetWord)=ai then ap:=TUserSheet.Int(TUserSheetPos);
    if TUserSheet.Int(TUserSheetWord)=bi then bp:=TUserSheet.Int(TUserSheetPos);
    TUserSheet.Next;
  end;
  TUserSheet.Locate('Number',wlc[StringGrid1.Row-1],true);
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetPos)=ap then TUserSheet.Edit([TUserSheetPos],[inttostr(bp)]) else
    if TUserSheet.Int(TUserSheetPos)=bp then TUserSheet.Edit([TUserSheetPos],[inttostr(ap)]);
    TUserSheet.Next;
  end;
  for i:=0 to 3 do
  begin
    s:=StringGrid1.Cells[i,StringGrid1.Row];
    if i=3 then
      StringGrid1.Cells[i,StringGrid1.Row]:=
        '!'+StringGrid1.Cells[i,stringGrid1.Row-1][2]+copy(s,3,length(s)-2)
      else StringGrid1.Cells[i,stringGrid1.Row]:=StringGrid1.Cells[i,StringGrid1.Row-1];
    if i=3 then
      StringGrid1.Cells[i,StringGrid1.Row-1]:=
        '!'+s[2]+copy(StringGrid1.Cells[i,StringGrid1.Row-1],3,length(StringGrid1.Cells[i,StringGrid1.Row-1])-2)
      else StringGrid1.Cells[i,StringGrid1.Row-1]:=s;
  end;
  s:=wl[StringGrid1.Row-1];
  wl[StringGrid1.Row-1]:=wl[StringGrid1.Row-2];
  wl[StringGrid1.Row-2]:=s;
  StringGrid1.Row:=StringGrid1.Row-1;
  fMenu.ChangeUserData;
end;

procedure TfWords.UserDetails_SpeedButton5Click(Sender: TObject);
var ai,bi:integer;
    ap,bp:integer;
    s:string;
    i:integer;
begin
  ai:=strtoint(wl[StringGrid1.Row-1]);
  bi:=strtoint(wl[StringGrid1.Row]);
  TUserSheet.SetOrder('Sheet_Ind');
  TUserSheet.Locate('Number',wlc[StringGrid1.Row-1],true);
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetWord)=ai then ap:=TUserSheet.Int(TUserSheetPos);
    if TUserSheet.Int(TUserSheetWord)=bi then bp:=TUserSheet.Int(TUserSheetPos);
    TUserSheet.Next;
  end;
  TUserSheet.Locate('Number',wlc[StringGrid1.Row-1],true);
  while (not TUserSheet.EOF) and (TUserSheet.Str(TUserSheetNumber)=wlc[StringGrid1.Row-1]) do
  begin
    if TUserSheet.Int(TUserSheetPos)=ap then TUserSheet.Edit([TUserSheetPos],[inttostr(bp)]) else
    if TUserSheet.Int(TUserSheetPos)=bp then TUserSheet.Edit([TUserSheetPos],[inttostr(ap)]);
    TUserSheet.Next;
  end;
  for i:=0 to 3 do
  begin
    s:=StringGrid1.Cells[i,StringGrid1.Row];
    if i=3 then
      StringGrid1.Cells[i,StringGrid1.Row]:=
        '!'+StringGrid1.Cells[i,stringGrid1.Row+1][2]+copy(s,3,length(s)-2)
      else StringGrid1.Cells[i,stringGrid1.Row]:=StringGrid1.Cells[i,StringGrid1.Row+1];
    if i=3 then
      StringGrid1.Cells[i,StringGrid1.Row+1]:=
        '!'+s[2]+copy(StringGrid1.Cells[i,StringGrid1.Row+1],3,length(StringGrid1.Cells[i,StringGrid1.Row+1])-2)
      else StringGrid1.Cells[i,StringGrid1.Row+1]:=s;
  end;
  s:=wl[StringGrid1.Row-1];
  wl[StringGrid1.Row-1]:=wl[StringGrid1.Row];
  wl[StringGrid1.Row]:=s;
  StringGrid1.Row:=StringGrid1.Row+1;
  fMenu.ChangeUserData;
end;

procedure TfWords.UserCategory_SpeedButton2Click(Sender: TObject);
var ct:string;
begin
  if fUserFilters.ListBox1.ItemIndex=-1 then exit;
  fNewCategory.Caption:=_l('^eEdit category^cUpravit kategorii');
  TUserCat.Locate('Name',curlang+'~'+fUserFilters.ListBox1.Items[fUserFilters.ListBox1.ItemIndex],false);
  fNewCategory.Edit1.Text:=StripCatName(TUserCat.Str(TUserCatName));
  case chr(TUserCat.Int(TUserCatType)) of
    'L':fNewCategory.RadioGroup1.ItemIndex:=0;
    'G':fNewCategory.RadioGroup1.ItemIndex:=1;
    'T':fNewCategory.RadioGroup1.ItemIndex:=2;
  end;
  if fNewCategory.ShowModal=idOK then
  begin
    case fNewCategory.RadioGroup1.ItemIndex of
      0:ct:='L';
      1:ct:='G';
      2:ct:='T';
    end;
    TUserCat.Edit([TUserCatName,TUserCatType],[curlang+'~'+fNewCategory.Edit1.Text,inttostr(ord(ct[1]))]);
    fMenu.RefreshCategory;
    fMenu.ChangeUserData;
  end;
  fNewCategory.Caption:=_l('^eNew category^cNová kategorie')
end;

procedure TfWords.UserCategory_SpeedButton3Click(Sender: TObject);
var sl:TStringList;
    confirmed:boolean;
begin
  if fUserFilters.ListBox1.ItemIndex=-1 then exit;
  confirmed:=false;
  if Application.MessageBox(pchar(_l(
    '#00857^eDo you really want to delete the category including all word links to it?'+
    '^cOpravdu chcete smazat kategorii vèetnì pøiøazení všech slov do ní?')),
    pchar(_l('#00573^eWarning^cUpozornìní')),MB_ICONWARNING or MB_YESNO)=idYes then
  begin
    sl:=TStringList.Create;
    TUser.First;
    while not TUser.EOF do
    begin
      ListWordCategories(TUser.Int(TUserIndex),sl,'',false);
      if (sl.Count=1) and (sl[0]=curlang+'~'+fUserFilters.ListBox1.Items[fUserFilters.ListBox1.ItemIndex]) then
      begin
        if not confirmed then if Application.MessageBox(pchar(_l('^eSome word(s) are assigned only to this category. Do you want to remove them from vocabulary?'+
          '^cNìkterá slovíèka jsou pøiøazena jen do této kategorie. Chcete je odstranit ze slovíèek?')),
          pchar(_l('#00885^eWarning^cVarování')),MB_ICONWARNING or MB_YESNO)=idNo then
          begin
            Application.MessageBox(pchar(_l('#00886^eCategory was not deleted.^cKategorie nebyla smazána.')),pchar(_l('#00887^eAborted^cZrušeno')),MB_ICONERROR or MB_OK);
            exit;
          end;
        confirmed:=true;
        TUser.Delete;
      end;
      TUser.Next;
    end;
    TUserCat.Locate('Name',curlang+'~'+fUserFilters.ListBox1.Items[fUserFilters.ListBox1.ItemIndex],false);
    TUserSheet.First;
    while not TUserSheet.EOF do
    begin
      if TUserSheet.Int(TUserSheetNumber)=TUserCat.Int(TUserCatIndex) then TUserSheet.Delete;
      TUserSheet.Next;
    end;
    TUserCat.Delete;
    sl.Free;
    fMenu.RefreshCategory;
    fMenu.RebuildUserIndex;
    fMenu.ChangeUserData;
  end;
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
  while TUserCat.Locate('Name',lname+' ('+inttostr(i)+')',false) do inc(i);
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
      Application.MessageBox(pchar(_l('#00858^eInvalid number of words (must be between 1 and 500).^cNeplatný poèet slov (musí být mezi 1 a 500).')),
        pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
      exit;
    end;
    if (wl.Count<3*wnum) then
    begin
      Application.MessageBox(pchar(_l('#00859^eNumber of words in displayed list must be at least three times higher.^cPoèet slovíèek v zobrazeném seznamu musí být alespoò tøikrát vìtší.')),
        pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
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
  fWait.Panel1.Caption:=_l('#00860^eBuilding learning list...^cVytváøím uèební seznam...');
  fWait.Show;
  fWordList.Label52.Caption:='';
  fWait.Invalidate;
  fWait.Update;
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
        fWait.Panel1.Caption:=_l('#00860^eBuilding learning list...^cVytváøím uèební seznam...')+' ('+inttostr(trunc(i/wnum*100))+'%)';
        fWait.Panel1.Invalidate;
        fWait.Panel1.Update;
        Application.ProcessMessages;
      end;
      bestw:=0;
      best:=100001;
      for j:=0 to il.Count-1 do
      begin
        TUser.Locate('Index',il[j],true);
        if TUser.Int(TUserScore)<=1 then ca[1]:=-200 else ca[1]:=600;
        ca[1]:=ca[1]-DateOld(TUser.Str(TUserLearned),200)-DateOld(TUser.Str(TUserMastered),200);
        ca[2]:=800-DateOld(TUser.Str(TUserAdded),500)*3;
        if pos('<spop>',TUser.Str(TUserEnglish))>0 then ca[3]:=-500 else ca[3]:=0;
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
        TUser.Locate('Index',inttostr(bestw),true);
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
    TUser.Locate('Index',ll[o],true);
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
  fWait.Hide;
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
  TUser.Locate('Index',ll[twi],true);
  twkanji:=CheckKnownKanji(TUser.Str(TUserKanji));
  twphonetic:=TUser.Str(TUserPhonetic);
  twmeaning:=strip_fl(TUser.Str(TUserEnglish));
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
  ListWordCategories(strtoint(ll[twi]),sl,'',false);
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
    TUser.Locate('Index',ll[i],true);
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
    TUser.Locate('Index',ll[i],true);
    if (fWordList.RadioGroup10.ItemIndex=0) or
       ((fWordList.RadioGroup10.ItemIndex=1) and (TUser.Int(TUserScore)<3)) or
       (TUser.Int(TUserScore)<2) then
      wl.Add(ll[i]);
  end;
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00861^eLearning list^cUèební seznam'));
  wl.Clear;
  wl.Assign(bk);
  bk.Free;
end;

procedure TfWords.SaveWordList;
var i:integer;
    fnd:boolean;
begin
  inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),fWordList.Edit1.Text,inttostr(ord('W')),FormatDateTime('yyyymmdd',now)]);
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',ll[i],true);
    if (fWordList.RadioGroup10.ItemIndex=0) or
       ((fWordList.RadioGroup10.ItemIndex=1) and (TUser.Int(TUserScore)<3)) or
       (TUser.Int(TUserScore)<2) then
      TUserSheet.Insert([ll[i],inttostr(MaxCategoryIndex),inttostr(i)]);
  end;
  fMenu.RefreshCategory;
  fUserFilters.TabSet1.TabIndex:=3;
  fUserFilters.TabSet1Change(self,3,fnd);
  for i:=0 to fUserFilters.ListBOx1.Items.COunt-1 do fUserFilters.ListBox1.Checked[i]:=fUserFilters.ListBox1.Items[i]=fWordList.Edit1.Text;
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.UpdateWordListStats;
var i,j:integer;
begin
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',ll[i],true);
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
          Application.MessageBox(pchar(_l('#00858^eInvalid number of words (must be between 1 and 500).^cNeplatný poèet slov (musí být mezi 1 a 500).')),
            pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
          exit;
        end;
        if (fWordList.RadioGroup2.ItemIndex=0) and (2*i>j) then
        begin
          Application.MessageBox(pchar(_l('#00862^eNumber of input words must be at least two times higher than number of selected words.^cPoèet vstupních slovíèek musí být alespoò dvakrát vìtší než poèet vybíraných slovíèek.')),
            pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
          exit;
        end;
        if (fWordList.RadioGroup2.ItemIndex=1) and (i>j) then
        begin
          Application.MessageBox(pchar(_l('#00863^eNumber of input words must not be lower than number of selected words.^cPoèet vstupních slovíèek nesmí být menší než poèet vybíraných slovíèek.')),
            pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
          exit;
        end;
        if j<1 then
        begin
          Application.MessageBox(pchar(_l('#00864^eInput list cannot be empty.^cVstupní seznam nemùže být prázdný.')),
            pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
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
        if Application.MessageBox(pchar(_l('#00865^eGenerated list will be lost. Do you want to continue?^cVygenerovaný seznam bude ztracen. Chcete pokraèovat?')),
        pchar(_l('#00573^eWarning^cUpozornìní')),MB_YESNO or MB_ICONWARNING)=idYes then result:=2;
      end;
    5:result:=4;
    6,7:begin
        if Application.MessageBox(pchar(_l('#00866^eCurrent test data will be lost. Do you want to continue?^cSouèasná testovací data budou ztracena. Chcete pokraèovat?')),
        pchar(_l('#00573^eWarning^cUpozornìní')),MB_YESNO or MB_ICONWARNING)=idYes then result:=5;
      end;
    8:result:=4;
    9:result:=0;
  end;
end;

procedure TfWords.SpeedButton1Click(Sender: TObject);
begin
  fMenu.ToggleForm(fExamples,SpeedButton1,fMenu.aUserExamples);
end;

procedure TfWords.SpeedButton2Click(Sender: TObject);
begin
  fMenu.ToggleForm(fUserFilters,SpeedButton2,fMenu.aUserSettings);
  FormResize(sender);
end;

procedure TfWords.SpeedButton3Click(Sender: TObject);
begin
  fMenu.ToggleForm(fUserCategory,SpeedButton3,fMenu.aUserCategory);
end;

procedure TfWords.SpeedButton4Click(Sender: TObject);
begin
  fMenu.ToggleForm(fUserDetails,SpeedButton4,fMenu.aUserDetails);
end;

procedure TfWords.FormResize(Sender: TObject);
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[3]-StringGrid1.ColWidths[0]-20;
end;

procedure TfWords.FormHide(Sender: TObject);
begin
//  fMenu.HideForm(SpeedButton1,fMenu.aUserAdd,fUserAdd);
//  fMenu.HideForm(SpeedButton2,fMenu.aUserSettings,fUserFilters);
//  fMenu.HideForm(SpeedButton3,fMenu.aUserCategory,fUserCategory);
//  fMenu.HideForm(SpeedButton4,fMenu.aUserDetails,fUserDetails);
  fMenu.aUser.Checked:=false;
end;

procedure TfWords.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipGridOver(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfWords.Button1Click(Sender: TObject);
var f:file of byte;
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
  assignfile(f,'examples.uni');
  assignfile(f2,'exam\index.bin');
  assignfile(f4,'exam\struct.bin');
  assignfile(f3,'exam\examples.bin');
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
  PKGWriteForm.PKGWriteCmd('PKGFileName examples_j.pkg');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name Examples');
  PKGWriteForm.PKGWriteCmd('TitleName Japanese dictionary examples');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName (C) Gabriel SanRoman');
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
  PKGWriteForm.PKGWriteCmd('Include exam');
  PKGWriteForm.PKGWriteCmd('Finish');
end;

procedure TfWords.Button2Click(Sender: TObject);
begin
  if fUserAdd.ShowModal=mrOK then
    if AddWord(clip,RomajiToKana(fUserAdd.Edit1.Text,romasys,true,curlang),fUserAdd.Edit3.Text,fUserAdd.ComboBox1.Text,'?',false,1) then
    begin
      fUserAdd.Edit1.Text:='';
      fUserAdd.PaintBox3.Invalidate;
      fUserAdd.Edit3.Text:='';
    end;
end;

procedure TfWords.Button3Click(Sender: TObject);
var sl,sl2:TStringList;
    t:textfile;
    f:file of byte;
    s,s2,s3,s4,uni:string;
    i,j,k:integer;
    b:byte;
begin
  sl:=TStringList.Create;
  assignfile(t,'STROKES.CSV');
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
  assignfile(f,'stroke\strokes.bin');
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
  sl2.SaveToFile('stroke\dir.txt');
  sl.Free;
  sl2.Free;
  PKGWriteForm.PKGWriteCmd('PKGFileName wakan.sod');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name Stroke order');
  PKGWriteForm.PKGWriteCmd('TitleName Japanese stroke order charts');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName (C) Jim Breen, Yasuhito Tanaka');
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
  PKGWriteForm.PKGWriteCmd('Include stroke');
  PKGWriteForm.PKGWriteCmd('Finish');
end;

procedure TfWords.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(true);
end;

procedure TfWords.StringGrid1Click(Sender: TObject);
var s:string;
    cl:TStringList;
    i,j:integer;
begin
  if StringGrid1.Selection.Bottom-StringGrid1.Selection.Top>0 then
  begin
    Reset;
    curkanji:=UnicodeToHex(_l('#00928^e<multiple words>^c<více slov>'));
    fUserDetails.ListBox2.Items.Clear;
    cl:=TStringList.Create;
    for j:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
    begin
      s:=wl[j-1];
      ListWordCategories(strtoint(s),cl,'',false);
      for i:=0 to cl.Count-1 do if fUserDetails.ListBox2.Items.IndexOf(copy(cl[i],3,length(cl[i])-2))=-1 then fUserDetails.ListBOx2.Items.Add(copy(cl[i],3,length(cl[i])-2));
    end;
    if cl.Count>1 then fUserDetails.Button13.Enabled:=true;
    cl.Free;
    fUserDetails.Button2.Enabled:=true;
    fUserDetails.Button4.Enabled:=true;
    fUserDetails.Button5.Enabled:=true;
    fUserDetails.Button6.Enabled:=true;
    fUserDetails.Button7.Enabled:=true;
    fUserDetails.Button8.Enabled:=true;
    fUserDetails.ComboBox2.Enabled:=true;
    fUserDetails.ListBox2.Enabled:=true;
    exit;
  end;
//  Reset;
  fUserDetails.ListBox2.Items.Clear;
  cl:=TStringList.Create;
  s:=wl[StringGrid1.Row-1];
  lastwordind:=strtoint(s);
  if not TUser.Locate('Index',s,true) then showmessage('INTERNAL ERROR. WORD NOT LOCATED');
  curkanji:=TUser.Str(TUserKanji);
  curphonetic:=TUser.Str(TUserPhonetic);
  fExamples.SetExamples(curkanji);
  case TUser.Int(TUserScore) of
    0:fUserDetails.RxLabel3.Caption:=_l('#00638^eProblematic^cProblematické');
    1:fUserDetails.RxLabel3.Caption:=_l('#00639^eUnlearned^cNenauèené');
    2:fUserDetails.RxLabel3.Caption:=_l('#00640^eLearned^cNauèené');
    3:fUserDetails.RxLabel3.Caption:=_l('#00641^eMastered^cDobøe nauèené');
  end;
  fUserDetails.Edit4.Text:=TUser.Str(TUserEnglish);
  fUserDetails.Edit4.ReadOnly:=false;
  fUserDetails.Button3.Enabled:=true;
  fUserDetails.Button4.Enabled:=true;
  fUserDetails.Label15.Caption:=DateForm(TUser.Str(TUserAdded));
  fUserDetails.Label16.Caption:=DateForm(TUser.Str(TUserLearned));
  fUserDetails.Label17.Caption:=DateForm(TUser.Str(TUserMastered));
  fUserDetails.Label18.Caption:=DateForm(TUser.Str(TUserPrinted));
  if fUserDetails.Label18.Caption<>'-'then fUserDetails.Label18.Caption:=fUserDetails.Label18.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
  fUserDetails.Button2.Enabled:=true;
  fUserDetails.Button5.Enabled:=true;
  fUserDetails.Button6.Enabled:=true;
  fUserDetails.Button7.Enabled:=true;
  fUserDetails.Button8.Enabled:=true;
  fUserDetails.ComboBox2.Enabled:=true;
  fUserDetails.ListBox2.Enabled:=true;
  fUserDetails.PaintBox1.Invalidate;
  fUserDetails.PaintBox6.Invalidate;
  fUserDetails.Button2.Default:=true;
  fUserDetails.Button3.Default:=false;
  fUserDetails.Button4.Default:=false;
  if fUserFilters.RadioGroup2.ItemIndex=0 then
  begin
    fUserDetails.SpeedButton4.Enabled:=(StringGrid1.Row>1) and (wlc[StringGrid1.Row-2]=wlc[StringGrid1.Row-1]);
    fUserDetails.SpeedButton5.Enabled:=(StringGrid1.Row<wlc.Count) and (wlc[StringGrid1.Row]=wlc[StringGrid1.Row-1]);
  end;
  ListWordCategories(strtoint(s),cl,'',false);
  for i:=0 to cl.Count-1 do fUserDetails.ListBOx2.Items.Add(copy(cl[i],3,length(cl[i])-2));
  if cl.Count>1 then
  fUserDetails.Button13.Enabled:=true;
  fUserDetails.ListBox2.ItemIndex:=0;
  cl.Free;
  fMenu.AnnotShowMedia(curkanji,curphonetic);
end;

procedure TfWords.SetGroupStatus(st: integer);
var i,ms:integer;
    s:string;
    b:boolean;
begin
  if Application.MessageBox(pchar(_l('#00927^eThis operation will affect multiple words.'#13#13'Do you want to continue?^cTato operace ovlivni mnoho slov.'#13#13'Chcete pokracovat?')),
    pchar(_l('#00926^eWarning^cVarovani')),MB_ICONWARNING or MB_YESNO)=idNo then exit;
  for i:=StringGrid1.Selection.Top to StringGrid1.Selection.Bottom do
  begin
    s:=wl[i-1];
    lastwordind:=strtoint(s);
    if not TUser.Locate('Index',s,true) then showmessage('INTERNAL ERROR. WORD NOT LOCATED');
    if TUser.Int(TUserMaxScore)>st then ms:=TUser.Int(TUserMaxScore) else ms:=st;
    TUser.Edit([TUserScore,TUserMaxScore],[inttostr(st),inttostr(ms)]);
  end;
  StringGrid1Click(self);
  fMenu.ChangeUserData;
  ShowIt(false);
end;

procedure TfWords.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWords.SearchWord(wordind: integer);
begin
  lastwordind:=wordind;
  ShowIt(true);
end;

end.
