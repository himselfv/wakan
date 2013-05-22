unit JWBStatistics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RXCtrls, Buttons;

type
  TfStatistics = class(TForm)
    BitBtn1: TBitBtn;
    RxLabel16: TRxLabel;
    Bevel1: TBevel;
    RxLabel1: TRxLabel;
    RxLabel2: TRxLabel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lblDicBuildDate: TLabel;
    lblKanjidicVersion: TLabel;
    lblUnihanVersion: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    procedure FormShow(Sender: TObject);
  protected
    procedure DoStatistic;
  end;

var
  fStatistics: TfStatistics;

implementation
uses JWBCharData, JWBUserData, JWBCategories, JWBSettings, JWBUnit;

{$R *.DFM}

procedure TfStatistics.FormShow(Sender: TObject);
begin
  lblDicBuildDate.Caption := CharDataProps.DicBuildDate;
  lblKanjidicVersion.Caption := CharDataProps.KanjidicVersion;
  lblUnihanVersion.Caption := CharDataProps.UnihanVersion;
  DoStatistic;
end;

procedure TfStatistics.DoStatistic;
var i,j,k,l,m,n,o,p,q:integer;
  t:textfile;
  KanjiKnown: boolean;
  JouyouGrade: integer;
  InUserIdx: boolean;
begin
  Screen.Cursor:=crHourGlass;
  if CharDataProps.ChinesePresent then
    Label10.Caption:=_l('#00854^ePresent')
  else
    Label10.Caption:=_l('#00855^eAbsent');
  TChar.First;
  i:=0;
  while not TChar.EOF do
  begin
    if TChar.Int(TCharChinese)=0 then inc(i);
    TChar.Next;
  end;
  Label11.Caption:=inttostr(i);
  p:=i;
  Label12.Caption:=inttostr(TChar.RecordCount);
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
  Label25.Caption:=inttostr(TUser.RecordCount);
  Label26.Caption:=perc(j,TUser.RecordCount);
  Label27.Caption:=perc(k,TUser.RecordCount);
  Label28.Caption:=perc(i,TUser.RecordCount);
  Label35.Caption:=perc(l,TUser.RecordCount);
  Label29.Caption:=perc(m,TUser.RecordCount);
  Label44.Caption:=perc(n,TUser.RecordCount);
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
    InUserIdx := TUserIdx.Locate('Kanji',TChar.Str(TCharUnicode));
    if KanjiKnown then inc(i);
    if KanjiKnown and (JouyouGrade>=9) then inc(j);
    if (not KanjiKnown) and (JouyouGrade<9) then inc(k);
    if JouyouGrade<9 then inc(o);
    if InUserIdx then inc(l);
    if InUserIdx and (JouyouGrade>=9) then inc(m);
    if KanjiKnown and (TChar.Int(TCharChinese)=1) then inc(n);
    if KanjiKnown then
    begin
      TRadicals.Locate('Number',GetCharValueRad(TChar.Int(TCharIndex),13));
      if TRadicals.Str(TRadicalsUnicode)=TChar.Str(TCharUnicode) then inc(q);
    end;
    TChar.Next;
  end;
  TChar.First;
  Label30.Caption:=perc(l,p);
  Label31.Caption:=perc(i,l);
  Label32.Caption:=perc(k,o);
  Label36.Caption:=perc(j,i);
  Label39.Caption:=perc(m,l);
  Label41.Caption:=perc(n,i);
  Label45.Caption:=perc(q,TRadicals.RecordCount);
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
  Screen.Cursor:=crDefault;
end;

end.
