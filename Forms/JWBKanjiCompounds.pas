unit JWBKanjiCompounds;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, Buttons, JWBStrings, WakanWordGrid, Menus,
  WakanPaintbox, JWBWordLookupBase, Vcl.ImgList;

type
  TfKanjiCompounds = class(TfWordLookupBase)
    cbLeftMatchOnly: TCheckBox;
    cbPopularOnly: TCheckBox;
    cbSortByFrequency: TCheckBox;
    sbShowDict: TSpeedButton;
    sbShowVocab: TSpeedButton;
    procedure OptionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);

  protected
    FCurChar: FChar;
    procedure FillDictResults(const ch: FString);
    procedure FillVocabResults(const ch: FString);
  public
    procedure Clear; override;
    procedure Refresh; override;
    procedure SetCharCompounds(ch:FChar);

  end;

var
  fKanjiCompounds: TfKanjiCompounds;

implementation

uses TextTable, JWBKanji, JWBUnit, JWBMenu, JWBDic, JWBDicSearch, JWBWordLookup,
  JWBVocab, JWBSettings, JWBEdictMarkers, JWBUserData, JWBCategories, JWBVocabAdd,
  JWBLegacyMarkup;

{$R *.DFM}

procedure TfKanjiCompounds.FormCreate(Sender: TObject);
begin
  FCurChar := UH_NOCHAR;
end;

procedure TfKanjiCompounds.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.btnCompounds.Down:=false;
  fMenu.aKanjiCompounds.Checked:=false;
end;

procedure TfKanjiCompounds.FormShow(Sender: TObject);
begin
  SetCharCompounds(FCurChar);
end;

procedure TfKanjiCompounds.Clear;
begin
  FCurChar := UH_NOCHAR;
  inherited;
end;

procedure TfKanjiCompounds.Refresh;
begin
  SetCharCompounds(FCurChar);
end;

procedure TfKanjiCompounds.SetCharCompounds(ch:FChar);
var kj:string;
  maxItems: integer;
begin
  FCurChar := ch;
  if not Self.Visible then exit;

  StringGrid.Visible:=false;
  cbSortByFrequency.Enabled:=(not sbShowVocab.Down) and (curlang='j');

  kj:=ChinTraditional(ch);
  FResults.Clear;
  if sbShowDict.Down then
    FillDictResults(kj)
  else
  if sbShowVocab.Down then
    FillVocabResults(kj);

  if cbSortByFrequency.Checked then
    FResults.SortByFrequency
  else
    FResults.SortByKanji;

  maxItems := strtoint(fSettings.Edit34.Text);
  if sbShowDict.Down and cbSortByFrequency.Checked and (maxItems<>0) then
    FResults.Trim(maxItems);

  ResultsChanged;
end;

{ Fills dictionary lookup results for words containing the specified kanji }
procedure TfKanjiCompounds.FillDictResults(const ch: FString);
var i, k: integer;
  dic:TDicIndexCursor;
  kmark:TMarkers;
  ent:TEntries;
begin
  for i:=0 to dicts.Count-1 do
    if dicts[i].loaded and dicts.IsInGroup(dicts[i], 4) then
  begin
    dic:=TDicIndexCursor.Create(dicts[i]);
    try
      dic.Find(itChar,fstrtouni(ch));
      k:=0;
      while dic.Next do
      begin
        inc(k);
        if pos(ch,dic.GetKanji)=0 then
          ShowMessage('Dictionary has corrupted index: '+ch+'-'+inttostr(k)+'-'+dic.GetArticleBody);

        if cbLeftMatchOnly.Checked and (pos(ch,dic.GetKanji)<>1) then
          continue;

        kmark := dic.GetKanjiKanaMarkers;
        ent := dic.GetEntries;

        if cbPopularOnly.Checked and not (
          TestMarkers(kmark, MarkPop)
          or TestMarkers(ent.MergeMarkers, MarkPop)
        ) then
          continue;

        with FResults.AddResult^ do begin
          if not dic.dic.SupportsFrequency then
            score := 9999999
          else
            score := 9999999 - dic.GetFrequency;
          userindex := 0; //TODO: try to find
          userscore := -1; //or they'll be red
          with AddArticle^ do begin
            dicindex := i;
            dicname := dic.dic.name;
            entries := ent;
          end;
          kanji := ChinSimplified(dic.GetKanji);
          kana := dic.GetPhonetic;
        end;

      end;
    finally
      FreeAndNil(dic);
    end;
  end;
end;

{ Fills vocabulary lookup results }
procedure TfKanjiCompounds.FillVocabResults(const ch: FString);
var CUser, CUserIdx: TTextTableCursor;
  sl2:TStringList;
  relatedWord:boolean;
  l, i: integer;
begin
  CUser := TUser.NewCursor;
  CUserIdx := TUserIdx.NewCursor;
  sl2 := TStringList.Create;
  try
    CUserIdx.SetOrder('Kanji_Ind');
    CUserIdx.Locate('Kanji',ch);
    while (not CUserIdx.EOF) and (CUserIdx.Str(TUserIdxKanji)=ch) do
    begin
      if (not cbLeftMatchOnly.Checked) or (CUserIdx.Bool(TUserIdxBegin)) then
      begin
       //Test that this word is related to current language (ugh)
        sl2.Clear;
        ListWordCategories(CUserIdx.Int(TUserIdxWord),sl2);
        relatedWord:=false;
        for l:=0 to sl2.Count-1 do
          if (pos(curlang+'~',sl2[l])=1) or (length(sl2[l])<2)
          or (copy(sl2[l],2,1)<>'~') then
            relatedWord:=true;

        if relatedWord then begin
          Assert(CUser.Locate('Index',CUserIdx.TrueInt(TUserIdxWord)));
          with FResults.AddResult^ do begin
            score := CUser.Int(TUserScore);
            userindex := CUser.Int(TUserIndex);
            userscore := score;
            kanji := ChinSimplified(CUser.Str(TUserKanji));
            kana := CUser.Str(TUserPhonetic);
            AddArticle^ := ParseLegacyArticle(FixVocabEntry(CUser.Str(TUserEnglish)));
          end;
        end;

      end;
      CUserIdx.Next;
    end;
  finally
    FreeAndNil(sl2);
    FreeAndNil(CUserIdx);
    FreeAndNil(CUser);
  end;
end;

procedure TfKanjiCompounds.OptionChanged(Sender: TObject);
begin
  Self.Refresh;
end;

procedure TfKanjiCompounds.FormResize(Sender: TObject);
begin
 //Remember width/height preferences in UndockWidth/Height
  Self.UndockHeight := Self.Height;
 //width is never changed since this has no non-portrait mode
end;

end.
