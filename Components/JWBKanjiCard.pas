unit JWBKanjiCard;
{ Kanji card painting }

interface
uses SysUtils, Classes, Types, Graphics, Grids, Windows;

type
  TKanjiCardOption = (
    koPrintStrokeCount,
    koPrintOuterLines,
    koPrintRadical,
    koPrintAlternateForm,
    koPrintInnerLines,
    koPrintVocabularyCompounds,
    koPrintReadings,
    koPrintDefinition,
    koPrintStrokeOrder,
    koPrintFullCompounds,
    koSortCompoundsByFrequency
  );
  TKanjiCardOptions = set of TKanjiCardOption;

  TKanjiCard = class
  protected
    FChar: string;
    FFlags: TKanjiCardOptions;
    FCharDim: double;
    FSizHor: integer;
    FSizVert: integer;
    FNoFullComp: integer;
    FCalFont: string;
    FLoaded: boolean;
    FVocabCompounds: TStringList;
    FDictCompounds: TStringList;
    FOnReadings: string;
    FKunReadings: string;
    FDefinition: string;
    FStrokeCount: integer;
    FRadical: string;
    function LoadVocabCompounds: TStringList;
    function LoadDictCompounds: TStringList;
  public
    constructor Create(AChar: string);
    destructor Destroy; override;
    procedure Reload;
    procedure Paint(Canvas: TCanvas; TargetRect: TRect);
    procedure Measure(out AWidth: integer; out AHeight: integer);
    property Char: string read FChar;
    property Flags: TKanjiCardOptions read FFlags write FFlags;
    property CharDim: double read FCharDim write FCharDim;
    property SizHor: integer read FSizHor write FSizHor;
    property SizVert: integer read FSizVert write FSizVert;
    property NoFullComp: integer read FNoFullComp write FNoFullComp;
    property CalFont: string read FCalFont write FCalFont;
  end;

//u --> FChar
//x,y --> Rect.x,y
//ch --> CharDim

implementation
uses TextTable, JWBStrings, JWBUnit, JWBEdictMarkers, JWBDic, JWBSettings,
  JWBUserData, JWBKanaConv, JWBCharData, JWBLegacyMarkup, JWBWordGrid;

constructor TKanjiCard.Create(AChar: string);
begin
  inherited Create();
  FChar := AChar;
end;

destructor TKanjiCard.Destroy;
begin
  FreeAndNil(FVocabCompounds);
  FreeAndNil(FDictCompounds);
  inherited;
end;

//Preprocess data, find dictionary examples etc.
procedure TKanjiCard.Reload;
var CCharProp: TCharPropertyCursor;
  radf:integer;
  rad_idx: integer;
begin
  FreeAndNil(FVocabCompounds);
  if koPrintVocabularyCompounds in flags then
    FVocabCompounds := LoadVocabCompounds;

  FreeAndNil(FDictCompounds);
  if koPrintFullCompounds in flags then
    FDictCompounds := LoadDictCompounds;

  FOnReadings := '';
  FKunReadings := '';
  FDefinition := '';
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    if curlang='j' then begin
      FOnReadings:=CCharProp.GetCharProps(FChar, ptOnReading);
      FKunReadings:=CCharProp.GetCharProps(FChar, ptKunReading);
      FDefinition := CCharProp.GetCharProps(FChar, ptJapaneseDefinition)
    end else
    if curlang='c' then begin
      FOnReadings:=ConvertPinyin(CCharProp.GetCharProps(FChar, ptMandarinReading));
      FKunReadings:=lowercase(CCharProp.GetCharProps(FChar, ptCantoneseReading));
      FDefinition := CCharProp.GetCharProps(FChar, ptChineseDefinition)
    end;
  finally
    FreeAndNil(CCharProp);
  end;

  FStrokeCount := 0;
  if koPrintStrokeCount in flags then begin
    TChar.Locate('Unicode', FChar);
    if curlang='j' then
      FStrokeCount := TChar.Int(TChar.fJpStrokeCount)
    else
      FStrokeCount := TChar.Int(TChar.fChStrokeCount);
  end;

  FRadical := '';
  if koPrintRadical in flags then begin
    radf:=fSettings.GetPreferredRadicalType;
    rad_idx := GetCharRadicalNumber(FChar,radf);
    if TRadicals.Locate('Number',rad_idx) then
      FRadical := TRadicals.Str(TRadicals.fUnicode);
  end;

  FLoaded := true;
end;

//Returns matching vocabulary compounds in a special format
function TKanjiCard.LoadVocabCompounds: TStringList;
begin
  Result := TStringList.Create;
  TUserIdx.SetOrder('Kanji_Ind');
  TUserIdx.Locate('Kanji',FChar);
  while (not TUserIdx.EOF) and (TUserIdx.Str(TUserIdxKanji)=FChar) do
  begin
    TUser.Locate('Index',TUserIdx.TrueInt(TUserIdxWord));
    if flength(TUser.Str(TUserKanji))<10 then
    //if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0 then
      if TUserIdx.Bool(TUserIdxBegin) then
        Result.Add('+'+inttostr(flength(TUser.Str(TUserKanji)))+TUser.Str(TUserKanji))
      else
        Result.Add('-'+inttostr(flength(TUser.Str(TUserKanji)))+TUser.Str(TUserKanji));
    TUserIdx.Next;
  end;
  Result.Sort;
end;

//Returns matching dictionary compounds in a special format (different from Vocab* one)
function TKanjiCard.LoadDictCompounds: TStringList;
var i: integer;
  dic:TDicIndexCursor;
  freq:string;
  tmp:string;
  mark:TMarkers;
begin
  Result := TStringList.Create;
  for i:=0 to dicts.Count-1 do
    if dicts[i].loaded and dicts.IsInGroup(dicts[i], 4) and dicts[i].SupportsFrequency then
  begin
    dic:=TDicIndexCursor.Create(dicts[i]);
    try
      dic.Find(itChar,fstrtouni(FChar));
      while dic.Next do
      begin
        mark:=dic.GetArticleMarkers;
        freq:='0000000';
        if dic.dic.SupportsFrequency and (koSortCompoundsByFrequency in flags) then
          freq:=inttostr(9999999-dic.GetFrequency);
        while length(freq)<7 do freq:='0'+freq;
        tmp := EnrichDictEntry(dic.GetArticleBody,mark);
        if pos(UH_LBEG+'spop'+UH_LEND,tmp)=0 then freq[1]:='a';
        if freq<>'9999999'then
        Result.Add(freq+#9+ChinSimplified(dic.GetKanji)+' ['+dic.GetPhonetic+'] {'+tmp+'}{');
      end;
    finally
      FreeAndNil(dic);
    end;
  end;
  Result.Sort;
  while Result.Count > NoFullComp do
    Result.Delete(Result.Count-1);
end;

{ Returns total suggested width and heigh needed to draw the card. You can
 sometimes shirnk it further but some info will be lost. }
procedure TKanjiCard.Measure(out AWidth: integer; out AHeight: integer);
begin
  if not FLoaded then
    Reload;

  AHeight := sizvert; //sic
  if (koPrintReadings in flags) then inc(AHeight, 3);
  if (koPrintDefinition in flags) then inc(AHeight, 2);
  if (koPrintFullCompounds in flags) then inc(AHeight, 1+FDictCompounds.Count);
  AHeight := Round(AHeight * FCharDim);

  AWidth := sizvert;
  if (koPrintAlternateForm in flags) or (koPrintRadical in flags) then inc(AWidth,(sizvert div 2)+1);
  if (koPrintVocabularyCompounds in flags) then Inc(AWidth, 1+sizhor);
  AWidth := Round(AWidth * FCharDim);
end;

procedure TKanjiCard.Paint(Canvas: TCanvas; TargetRect: TRect);
var
  Rect, Rect2: TRect;
  HalfKanjiSz: integer;
  FontJpCh:string;
  FontJpChGrid:string;
  FontJpEnGrid:string;
  j, p: integer;
  s: string;

{
    s:string; //can contain fstring too
    ws:FString;
    p:integer;
    i,j:integer;

  rt: integer; //TCharProp.Int(TCharPropType)
}

begin
  if not FLoaded then
    Reload;

  if curlang='j' then begin
    fontjpch:=FontJapanese;
    fontjpchgrid:=FontJapaneseGrid;
    fontjpengrid:=FontJapaneseGrid;
  end else begin
    fontjpch:=FontChinese;
    fontjpchgrid:=FontChineseGrid;
    fontjpengrid:=FontPinYin;
  end;

  InflateRect(TargetRect, Round(-FCharDim/2), Round(-FCharDim/2)); //empty fields

  {outer lines}
  if koPrintOuterLines in flags then begin
    Canvas.MoveTo(TargetRect.Left,TargetRect.Top);
    Canvas.LineTo(TargetRect.Right,TargetRect.Top);
    Canvas.LineTo(TargetRect.Right,TargetRect.Bottom);
    Canvas.LineTo(TargetRect.Left,TargetRect.Bottom);
    Canvas.LineTo(TargetRect.Left,TargetRect.Top);
  end;

  {character box}
  Rect := TargetRect;
  Rect.Height := Round((sizvert-1)*FCharDim); //1 unit goes to margins
  Rect.Width := Rect.Height;
  DrawUnicode(Canvas,Rect.Left,Rect.Top,Rect.Height,FChar,calfont);
  if koPrintStrokeCount in flags then
    DrawUnicode(Canvas,Rect.Left,Rect.Top,Round(FCharDim),fstr(IntToStr(FStrokeCount)),FontEnglish);
  if koPrintStrokeOrder in flags then
    DrawStrokeOrder(Canvas,Rect.Left,Rect.Top,Rect.Height,Rect.Height,FChar,Round(FCharDim/3*2),clBlack);

  Rect.Left := Rect.Right+Round(FCharDim/2);
  Rect.Right := TargetRect.Right;
  if koPrintInnerLines in flags then begin
    Canvas.MoveTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Left, Rect.Bottom);
  end;

  {alternate form and radical}
  if (koPrintAlternateForm in flags) or (koPrintRadical in flags) then begin
    Rect.Left := Rect.Left + Round((1/16)*FCharDim*SizVert);
    Rect.Height := Round(sizvert*FCharDim/2 - 5*FCharDim/8); //keep top/bottom margins + half that margin inbetween 2 parts
    if koPrintAlternateForm in flags then
      DrawUnicode(Canvas,Rect.Left,Rect.Top,Rect.Height,FChar,FontJpchGrid);
    Inc(Rect.Top,Rect.Height+Round(FCharDim/4));
    if (koPrintRadical in flags) and (FRadical<>'') then
      DrawUnicode(Canvas,Rect.Left,Rect.Top,Rect.Height,FRadical,FontRadical);
    Rect.Left := Rect.Left + Rect.Height; //sic
    if koPrintInnerLines in flags then begin
      canvas.MoveTo(Rect.Left, TargetRect.Top); //sic
      canvas.LineTo(Rect.Left, Rect.Bottom);
    end;
  end;

  Rect.Left := Rect.Left + Round(CharDim + CharDim/2);

 { Vocabulary entries -- print only words, without readings/definitions (these
  words are supposed to be known) }
  if koPrintVocabularyCompounds in flags then begin
    Rect.Top := 0;

    p:=0;
    for j:=0 to sizvert-2 do
    begin
      s:='';
      while (p<FVocabCompounds.Count) and (flenfc(length(FVocabCompounds[p])-2)+flength(s)+1<=sizhor) do
      begin
        s:=s+copy(FVocabCompounds[p],3,length(FVocabCompounds[p])-2)+UH_IDG_COMMA;
        inc(p);
      end;
      if (p>=FVocabCompounds.Count) and (flength(s)>0) then fdelete(s,flength(s),1);
      DrawUnicode(canvas,Rect.Left,Rect.Top,Round(CharDim),s,FontJpChGrid);
      Inc(Rect.Top, Round(CharDim));
    end;
  end;

  Rect.Left := TargetRect.Left;
  Rect.Right := TargetRect.Right;
  Rect.Top := TargetRect.Top + Round(sizvert*CharDim); //just below the kanji

  {readings}
  if koPrintReadings in flags then
  begin
    if koPrintInnerLines in flags then begin
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Top);
    end;
   { if Length(ony)>=nch then SetLength(ony, nch);
    if Length(kuny)>=nch then SetLength(kuny, nch); }
    Inc(Rect.Top, Round(CharDim/2));
    DrawUnicode(canvas,Rect.Left,Rect.Top,Round(CharDim),FOnReadings,FontJpEnGrid);
    Inc(Rect.Top, Round(CharDim));
    DrawUnicode(canvas,Rect.Left,Rect.Top,Round(CharDim),FKunReadings,FontJpEnGrid);
    Inc(Rect.Top, Round(CharDim + CharDim/2));
  end;

  if koPrintDefinition in flags then
  begin
    if koPrintInnerLines in flags then begin
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Top);
    end;
    Inc(Rect.Top, Round(CharDim/2));
    Rect.Bottom := Rect.Top + Round(CharDim);
    Rect2 := Rect;
    canvas.Font.Name:=FontEnglish;
    canvas.Font.Height:=Round(CharDim);
    canvas.TextRect(Rect2, Rect.Left, Rect.Top, FDefinition);
    Inc(Rect.Top, Round(CharDim + CharDim/2));
  end;

  {full compounds}
  if koPrintFullCompounds in flags then
  begin
    if koPrintInnerLines in flags then begin
      canvas.MoveTo(Rect.Left, Rect.Top);
      canvas.LineTo(Rect.Right, Rect.Top);
    end;

    Rect.Top := Rect.Top + Round(CharDim/2); //free space
    Rect.Bottom := Rect.Top + Round(CharDim);
    for j:=0 to FDictCompounds.Count-1 do begin
      s := FDictCompounds[j];
      if s<>'' then
        DrawPackedWordInfo(canvas,rect,copy(s,9,length(s)-8),Round(CharDim),false);
      Rect.Top := Rect.Top + Round(CharDim);
      Rect.Bottom := Rect.Bottom + Round(CharDim);
    end;
  end;

end;

end.
