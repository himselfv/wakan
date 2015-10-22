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
    FFontSize: integer; //font size of normal text (pixels, square characters assumed)
    FMarginSize: integer; //margin size in pixels
    FMainCharSize: integer; //font size of main char (pixels)
    FMaxFullComp: integer; //show at most this # of dict compounds
    FSuggestedAdditionalWidth: integer; //if set, will affect Measure() but not Paint()
    FCalFont: string;
    FLoaded: boolean;
    FVocabCompounds: TStringList;
    FFullCompounds: TStringList;
    FOnReadings: string;
    FKunReadings: string;
    FDefinition: string;
    FStrokeCount: integer;
    FRadical: string;
    function LoadVocabCompounds: TStringList;
    function LoadDictCompounds: TStringList;
  public
    constructor Create(const AChar: string);
    destructor Destroy; override;
    procedure Reload;
    procedure Paint(Canvas: TCanvas; TargetRect: TRect);
    procedure Measure(out AWidth: integer; out AHeight: integer;
      AAbstract: boolean = false);
    property Char: string read FChar;
    property Flags: TKanjiCardOptions read FFlags write FFlags;
    property FontSize: integer read FFontSize write FFontSize;
    property MarginSize: integer read FMarginSize write FMarginSize;
    property MainCharSize: integer read FMainCharSize write FMainCharSize;
    property MaxFullComp: integer read FMaxFullComp write FMaxFullComp;
    property SuggestedAdditionalWidth: integer read FSuggestedAdditionalWidth
      write FSuggestedAdditionalWidth;
    property CalFont: string read FCalFont write FCalFont;
  end;

//u --> FChar
//x,y --> Rect.x,y
//ch --> FontSize

//Defaults:
//Margin size: FFontSize div 2
//Main char size: (sizevert-1)*FFontSize

implementation
uses TextTable, JWBStrings, JWBUnit, StrokeOrder, JWBEdictMarkers, JWBDictionaries, JWBDic, JWBSettings,
  JWBUserData, KanaConv, JWBCharData, JWBLegacyMarkup, JWBWordGrid;

constructor TKanjiCard.Create(const AChar: string);
begin
  inherited Create();
  FChar := AChar;
  FSuggestedAdditionalWidth := 25; //arbitrary;
end;

destructor TKanjiCard.Destroy;
begin
  FreeAndNil(FVocabCompounds);
  FreeAndNil(FFullCompounds);
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

  FreeAndNil(FFullCompounds);
  if koPrintFullCompounds in flags then
    FFullCompounds := LoadDictCompounds;

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
    radf := fSettings.GetPreferredRadicalType;
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
    if Length(TUser.Str(TUserKanji))<10 then
    //if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0 then
      if TUserIdx.Bool(TUserIdxBegin) then
        Result.Add('+'+IntToStr(Length(TUser.Str(TUserKanji)))+TUser.Str(TUserKanji))
      else
        Result.Add('-'+IntToStr(Length(TUser.Str(TUserKanji)))+TUser.Str(TUserKanji));
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
      dic.Find(itChar, FChar);
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
  while Result.Count > MaxFullComp do
    Result.Delete(Result.Count-1);
end;

{ Returns total suggested width and heigh needed to draw the card. You can
 sometimes shirnk it further but some info will be lost.
 AAbstract: ignore available data and just measure the maximal size. }
procedure TKanjiCard.Measure(out AWidth: integer; out AHeight: integer;
  AAbstract: boolean);
begin
  if not FLoaded and not AAbstract then
    Reload;

  AHeight := MainCharSize;
  if koPrintReadings in flags then begin
    if koPrintInnerLines in flags then
      Inc(AHeight, MarginSize);
    Inc(AHeight, MarginSize + 2 * FontSize);
  end;
  if koPrintDefinition in flags then begin
    if koPrintInnerLines in flags then
      Inc(AHeight, MarginSize);
    Inc(AHeight, MarginSize + FontSize);
  end;
  if koPrintFullCompounds in flags then begin
    if koPrintInnerLines in flags then
      Inc(AHeight, MarginSize);
    if AAbstract then
      Inc(AHeight, MarginSize + FMaxFullComp * FontSize)
    else
      Inc(AHeight, MarginSize + FFullCompounds.Count * FontSize);
  end;
  Inc(AHeight, 2*MarginSize); //outer margins

  AWidth := MainCharSize + MarginSize;
  if (koPrintAlternateForm in flags) or (koPrintRadical in flags) then begin
    if koPrintInnerLines in flags then
      Inc(AWidth, MarginSize);
    Inc(AWidth, MarginSize + MainCharSize div 2);
  end;
  if koPrintVocabularyCompounds in flags then begin
    if koPrintInnerLines in flags then
      Inc(AWidth, MarginSize);
    Inc(AWidth, MarginSize + FontSize*FSuggestedAdditionalWidth);
  end;
  Inc(AWidth, 2*MarginSize); //outer margins
end;

procedure TKanjiCard.Paint(Canvas: TCanvas; TargetRect: TRect);
var
  Rect, Rect2: TRect;
  HalfCharSz: integer;
  FontJpCh:string;
  FontJpChGrid:string;
  FontJpEnGrid:string;
  j, p: integer;
  s: string;
begin
  if not FLoaded then
    Reload;

  if curlang = 'j' then begin
    FontJpCh := FontJapanese;
    FontJpChGrid := FontJapaneseGrid;
    FontJpEnGrid := FontJapaneseGrid;
  end else begin
    FontJpCh := FontChinese;
    FontJpChGrid := FontChineseGrid;
    FontJpEnGrid := FontPinYin;
  end;

  InflateRect(TargetRect, -MarginSize div 2, -MarginSize div 2);

  {outer lines}
  if koPrintOuterLines in flags then begin
    Canvas.MoveTo(TargetRect.Left, TargetRect.Top);
    Canvas.LineTo(TargetRect.Right, TargetRect.Top);
    Canvas.LineTo(TargetRect.Right, TargetRect.Bottom);
    Canvas.LineTo(TargetRect.Left, TargetRect.Bottom);
    Canvas.LineTo(TargetRect.Left, TargetRect.Top);
  end;

  InflateRect(TargetRect, -MarginSize div 2, -MarginSize div 2);

  {character box}
  Rect := TargetRect;
  Rect.Height := MainCharSize;
  Rect.Width := MainCharSize;
  DrawUnicode(Canvas, Rect.Left, Rect.Top, Rect.Height, FChar, CalFont);
  if koPrintStrokeCount in flags then
    DrawUnicode(Canvas, Rect.Left, Rect.Top, FFontSize, IntToStr(FStrokeCount), FontEnglish);
  if koPrintStrokeOrder in flags then
    DrawStrokeOrder(Canvas, Rect.Left, Rect.Top, Rect.Width, Rect.Height, FChar, Round(FFontSize*2/3), clBlack);

  Rect.Left := Rect.Right;

  {alternate form and radical}
  if (koPrintAlternateForm in flags) or (koPrintRadical in flags) then begin
    if koPrintInnerLines in flags then begin
      Inc(Rect.Left, MarginSize);
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Left, Rect.Bottom);
    end;
    Inc(Rect.Left, MarginSize);
    HalfCharSz := Round((MainCharSize - MarginSize div 2) div 2); //two + half-margin must fit in MainCharSize
    if (koPrintRadical in flags) and (FRadical<>'') then
      DrawUnicode(Canvas, Rect.Left, Rect.Top, HalfCharSz, FRadical, FontRadical);
    Inc(Rect.Top, HalfCharSz + MarginSize div 2);
    if koPrintAlternateForm in flags then
      DrawUnicode(Canvas, Rect.Left, Rect.Top, HalfCharSz, FChar, FontJpchGrid);
    Rect.Top := TargetRect.Top; //restore top + height
    Rect.Height := MainCharSize;
    Rect.Left := Rect.Left + HalfCharSz;
    Rect.Right := TargetRect.Right;
  end;

 { Vocabulary entries -- print only words, without readings/definitions (these
  words are supposed to be known) }
  if koPrintVocabularyCompounds in flags then begin
    if koPrintInnerLines in flags then begin
      Inc(Rect.Left, MarginSize);
      canvas.MoveTo(Rect.Left, Rect.Top);
      canvas.LineTo(Rect.Left, Rect.Bottom);
    end;
    Inc(Rect.Left, MarginSize);
    p:=0;
    Rect.Top := TargetRect.Top;
    while Rect.Top + FontSize <= TargetRect.Top + MainCharSize do begin
      s:='';
      while (p<FVocabCompounds.Count)
      and (FontSize*(Length(s)+1+Length(FVocabCompounds[p])-2) <= Rect.Width) do
      begin
        s := s + copy(FVocabCompounds[p], 3, length(FVocabCompounds[p])-2) + UH_IDG_COMMA;
        Inc(p);
      end;
      if (p>=FVocabCompounds.Count) and (Length(s)>0) then delete(s, Length(s), 1); //remove last comma
      DrawUnicode(Canvas, Rect.Left, Rect.Top, FontSize, s, FontJpChGrid);
      Inc(Rect.Top, FontSize);
    end;
  end;

  Rect.Left := TargetRect.Left;
  Rect.Right := TargetRect.Right;
  Rect.Top := TargetRect.Top + MainCharSize; //just below the kanji

  {readings}
  if koPrintReadings in flags then
  begin
    if koPrintInnerLines in flags then begin
      Inc(Rect.Top, MarginSize);
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Top);
    end;
    Inc(Rect.Top, MarginSize);
    DrawUnicode(Canvas, Rect.Left, Rect.Top, FontSize, FOnReadings, FontJpEnGrid);
    Inc(Rect.Top, FontSize);
    DrawUnicode(Canvas, Rect.Left, Rect.Top, FontSize, FKunReadings, FontJpEnGrid);
    Inc(Rect.Top, FontSize);
  end;

  if koPrintDefinition in flags then
  begin
    if koPrintInnerLines in flags then begin
      Inc(Rect.Top, MarginSize);
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Top);
    end;
    Inc(Rect.Top, MarginSize);
    Rect2 := Rect;
    Rect2.Height := FontSize;
    Canvas.Font.Name := FontEnglish;
    Canvas.Font.Height := FontSize;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(Rect2, Rect.Left, Rect.Top, FDefinition);
    Inc(Rect.Top, FontSize);
  end;

  {full compounds}
  if koPrintFullCompounds in flags then
  begin
    Inc(Rect.Left, 1);
    if koPrintInnerLines in flags then begin
      Inc(Rect.Top, MarginSize);
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Top);
    end;
    Inc(Rect.Top, MarginSize);
    for j:=0 to FFullCompounds.Count-1 do begin
      Rect.Height := FontSize;
      s := FFullCompounds[j];
      if s<>'' then
        DrawPackedWordInfo(canvas,rect,copy(s,9,length(s)-8),FontSize,false);
      Inc(Rect.Top, FontSize);
    end;
    Dec(Rect.Left, 1);
  end;
end;

end.
