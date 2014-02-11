unit JWBStatistics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JwbForms;

type
  TfStatistics = class(TJwbForm)
    BitBtn1: TBitBtn;
    RxLabel16: TLabel;
    Bevel1: TBevel;
    RxLabel1: TLabel;
    RxLabel2: TLabel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblChinesePresent: TLabel;
    lblTotalJapanese: TLabel;
    lblTotalKanji: TLabel;
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
    lblVocabTotal: TLabel;
    lblVocabLearned: TLabel;
    lblVocabMastered: TLabel;
    lblVocabProblematic: TLabel;
    lblVocabKatakana: TLabel;
    lblKanjiInWords: TLabel;
    lblLearnedKanji: TLabel;
    lblUnlearnedBasic: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    lblVocabUnpopular: TLabel;
    lblLearnedRare: TLabel;
    Label38: TLabel;
    lblRareKanjiInWords: TLabel;
    Label40: TLabel;
    lblLearnedChinese: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    lblVocabWriting: TLabel;
    lblLearnedRadicals: TLabel;
    lblVocabLearnedButNotMasteredCaption: TLabel;
    lblVocabLearnedButNotMastered: TLabel;
    procedure FormShow(Sender: TObject);
  protected
    procedure DoStatistic;
  end;

implementation
uses TextTable, JWBCharData, JWBUserData, JWBCategories, JWBSettings, JWBUnit;

{$R *.DFM}

procedure TfStatistics.FormShow(Sender: TObject);
begin
  lblDicBuildDate.Caption := DateToStr(CharDataProps.DicBuildDate);
  lblKanjidicVersion.Caption := CharDataProps.KanjidicVersion;
  lblUnihanVersion.Caption := CharDataProps.UnihanVersion;
  DoStatistic;
end;

procedure TfStatistics.DoStatistic;
var
  vs: record
    learned: integer;
    mastered: integer;
    problematic: integer;
    unpopular: integer;
    katakana: integer;
    writing: integer;
  end;
  ks: record
    totalJapanese: integer;
    kanjiInWords: integer;
    rareKanjiInWords: integer;
    learnedKanji: integer;
    learnedBasic: integer;
    unlearnedBasic: integer;
    learnedRare: integer;
    learnedChinese: integer;
    learnedRadicals: integer;
  end;

  t:textfile;
  KanjiKnown: boolean;
  JouyouGrade: integer;
  InUserIdx: boolean;
begin
  Screen.Cursor:=crHourGlass;
  if CharDataProps.ChinesePresent then
    lblChinesePresent.Caption:=_l('#00854^ePresent')
  else
    lblChinesePresent.Caption:=_l('#00855^eAbsent');

  FillChar(vs, SizeOf(vs), 0);
  FillChar(ks, SizeOf(ks), 0);

  TChar.First;
  while not TChar.EOF do
  begin
    if TChar.Int(TChar.fChinese)=0 then inc(ks.totalJapanese);
    TChar.Next;
  end;
  lblTotalJapanese.Caption:=inttostr(ks.totalJapanese);
  lblTotalKanji.Caption:=inttostr(TChar.RecordCount);

  TChar.First;
  TUser.First;
  while not TUser.EOF do
  begin
    if TUser.Int(TUserScore)=0 then inc(vs.problematic);
    if TUser.Int(TUserScore)=2 then inc(vs.learned);
    if TUser.Int(TUserScore)=3 then inc(vs.mastered);
    if pos('<spop>',TUser.Str(TUserEnglish))=0 then inc(vs.unpopular);
    if (length(TUser.Str(TUserPhonetic))>1) and (TUser.Str(TUserPhonetic)[3]>='A') then inc(vs.katakana);
    if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0 then inc(vs.writing);
    TUser.Next;
  end;
  TUser.First;
  lblVocabTotal.Caption:=inttostr(TUser.RecordCount);
  lblVocabLearned.Caption:=perc(vs.learned+vs.mastered,TUser.RecordCount);
  lblVocabLearnedButNotMastered.Caption:=perc(vs.learned,TUser.RecordCount);
  lblVocabMastered.Caption:=perc(vs.mastered,TUser.RecordCount);
  lblVocabProblematic.Caption:=perc(vs.problematic,TUser.RecordCount);
  lblVocabUnpopular.Caption:=perc(vs.unpopular,TUser.RecordCount);
  lblVocabKatakana.Caption:=perc(vs.katakana,TUser.RecordCount);
  lblVocabWriting.Caption:=perc(vs.writing,TUser.RecordCount);

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
    writeln(t,'learned_words = '+inttostr(vs.learned));
    writeln(t,'mastered_words = '+inttostr(vs.mastered));
    writeln(t,'problematic_words = '+inttostr(vs.problematic));
    writeln(t,'unpopular_words = '+inttostr(vs.unpopular));
    writeln(t,'katakana_words = '+inttostr(vs.katakana));
    writeln(t,'known_writing_words = '+inttostr(vs.writing));
  end;

  TChar.First;
  while not TChar.EOF do
  begin
    KanjiKnown := IsKnown(KnownLearned,TChar.Fch(TChar.fUnicode));
    JouyouGrade := TChar.Int(TChar.fJouyouGrade);
    InUserIdx := TUserIdx.Locate('Kanji',TChar.Str(TChar.fUnicode));
    if KanjiKnown then inc(ks.learnedKanji);
    if KanjiKnown and (JouyouGrade>=9) then inc(ks.learnedRare);
    if (not KanjiKnown) and (JouyouGrade<9) then inc(ks.unlearnedBasic);
    if JouyouGrade<9 then inc(ks.learnedBasic);
    if InUserIdx then inc(ks.kanjiInWords);
    if InUserIdx and (JouyouGrade>=9) then inc(ks.rareKanjiInWords);
    if KanjiKnown and (TChar.Int(TChar.fChinese)=1) then inc(ks.learnedChinese);
    if KanjiKnown
      and TRadicals.Locate('Number',GetCharRadicalNumber(TChar.Str(TChar.fUnicode),ptRSUnicode))
      and (TRadicals.Str(TRadicals.fUnicode)=TChar.Str(TChar.fUnicode)) then
        inc(ks.learnedRadicals);

    TChar.Next;
  end;
  TChar.First;
  lblKanjiInWords.Caption:=perc(ks.kanjiInWords,ks.totalJapanese);
  lblLearnedKanji.Caption:=perc(ks.learnedKanji,ks.kanjiInWords);
  lblUnlearnedBasic.Caption:=perc(ks.unlearnedBasic,ks.learnedBasic);
  lblLearnedRare.Caption:=perc(ks.learnedRare,ks.learnedKanji);
  lblRareKanjiInWords.Caption:=perc(ks.rareKanjiInWords,ks.kanjiInWords);
  lblLearnedChinese.Caption:=perc(ks.learnedChinese,ks.learnedKanji);
  lblLearnedRadicals.Caption:=perc(ks.learnedRadicals,TRadicals.RecordCount);

  if fSettings.CheckBox26.Checked then
  begin
    writeln(t,'kanji_in_words = '+inttostr(ks.kanjiInWords));
    writeln(t,'known_kanji = '+inttostr(ks.learnedKanji));
    writeln(t,'unknown_basic_kanji = '+inttostr(ks.unlearnedBasic));
    writeln(t,'known_nonbasic_kanji = '+inttostr(ks.learnedRare));
    writeln(t,'nonbasic_kanji_in_words = '+inttostr(ks.rareKanjiInWords));
    writeln(t,'known_chineseonly_chars = '+inttostr(ks.learnedChinese));
    writeln(t,'known_radicals = '+inttostr(ks.learnedRadicals));
    closefile(t);
  end;
  Screen.Cursor:=crDefault;
end;

end.
