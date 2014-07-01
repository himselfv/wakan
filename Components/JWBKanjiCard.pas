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
    function LoadVocabCompounds: TStringList;
    function LoadDictCompounds: TStringList;
  public
    constructor Create(AChar: string);
    destructor Destroy; override;
    procedure Reload;
    procedure Paint(Canvas: TCanvas; TargetRect: TRect);
    procedure MeasureHeight(const AWidth: integer; out AHeight: integer);
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

procedure TKanjiCard.MeasureHeight(const AWidth: integer; out AHeight: integer);
begin
  if not FLoaded then
    Reload;

end;

procedure TKanjiCard.Paint(Canvas: TCanvas; TargetRect: TRect);
var
  KanjiRect: TRect;
    radf:integer;
    sl:TStringList;
    s:string; //can contain fstring too
    ws:FString;
    p:integer;
    i,j:integer;
//    rect:TRect;
    nch,ncv:integer;
    fontjpch:string;
    FontJpChGrid:string;
    FontJpEnGrid:string;


  rt: integer; //TCharProp.Int(TCharPropType)
  rad_idx: integer;
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

  ncv:=sizvert;
  if (koPrintReadings in flags) then inc(ncv,3);
  if (koPrintDefinition in flags) then inc(ncv,2);
  if (koPrintFullCompounds in flags) then inc(ncv,1+nofullcomp);

  nch:=sizvert;
  if (koPrintAlternateForm in flags) or (koPrintRadical in flags) then inc(nch,(sizvert div 2)+1);
  if (koPrintVocabularyCompounds in flags) then nch:=nch+1+sizhor;

  KanjiRect := TargetRect;
  KanjiRect.Width := sizhor*FCharDim; //should be less
  KanjiRect.Height := sizvert*FCharDim;
  InflateRect(KanjiRect, Trunc(-FCharDim/2), Trunc(-FCharDim/2)); //empty space


  DrawUnicode(canvas,KanjiRect.Left,KanjiRect.Top,KanjiRect.Height,FChar,calfont);
  if koPrintStrokeCount in flags then
    DrawUnicode(canvas,KanjiRect.Left,KanjiRect.Top,trunc(FCharDim),fstr(IntToStr(FStrokeCount)),FontEnglish);
  if koPrintStrokeOrder in flags then
    DrawStrokeOrder(canvas,KanjiRect.Left,KanjiRect.Top,KanjiRect.Height,KanjiRect.Height,FChar,trunc(FCharDim/3*2),clBlack);

  {outer lines}
  if koPrintOuterLines in flags then begin
    canvas.MoveTo(TargetRect.Left,TargetRect.Top);
    canvas.LineTo(TargetRect.Right,TargetRect.Top);
    canvas.LineTo(TargetRect.Right,TargetRect.Bottom);
    canvas.LineTo(TargetRect.Left,TargetRect.Bottom);
    canvas.LineTo(TargetRect.Left,TargetRect.Top);
  end;

  //HERE
  {alternate form}
  if koPrintAlternateForm in flags then begin
    radf:=fSettings.GetPreferredRadicalType;
    rad_idx := GetCharRadicalNumber(FChar,radf);
    if TRadicals.Locate('Number',rad_idx) then
      DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2),trunc(sizvert/8*3*ch),TRadicals.Str(TRadicals.fUnicode),FontRadical);
  end;

  {radical}
  if koPrintRadical in flags then
    DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2+(sizvert/2)*ch),trunc((sizvert/8*3)*ch),FChar,FontJpchGrid);
  if koPrintInnerLines in flags then begin
    canvas.MoveTo(trunc(x+ch*sizvert),trunc(y+ch/2));
    canvas.LineTo(trunc(x+ch*sizvert),trunc(y+ch*sizvert-ch/2));
  end;

 { Vocabulary entries -- print only words, without readings/definitions (these
  words are supposed to be known) }
  if koPrintVocabularyCompounds in flags then
  begin


    p:=0;
    for j:=0 to sizvert-2 do
    begin
      s:='';
      while (p<sl.Count) and (flenfc(length(sl[p])-2)+flength(s)+1<=sizhor) do
      begin
        s:=s+copy(sl[p],3,length(sl[p])-2)+UH_IDG_COMMA;
        inc(p);
      end;
      if (p>=sl.Count) and (flength(s)>0) then fdelete(s,flength(s),1);
      if (koPrintAlternateForm in flags) or (koPrintRadical in flags) then
        DrawUnicode(canvas,trunc(x+((sizvert)*3*ch)/2+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid)
      else
        DrawUnicode(canvas,trunc(x+(sizvert)*ch+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid);
    end;
    if ((koPrintAlternateForm in flags) or (koPrintRadical in flags)) and ((koPrintInnerLines in flags)) then
    begin
      canvas.MoveTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch/2));
      canvas.LineTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch*sizvert-ch/2));
    end;
  end;

  {full compounds}
  if koPrintFullCompounds in flags then
  begin


    for j:=0 to nofullcomp-1 do
    begin
      s:=kcchcomp[j];
      rect.left:=x+round(ch/2);
      rect.right:=x+round(nch*ch-ch/2);
      rect.top:=y+round(sizvert*ch+j*ch+ch/2);
      rect.bottom:=y+round(sizvert*ch+j*ch+ch+ch/2);
      if koPrintReadings in flags then
      begin
        rect.top:=rect.top+trunc(ch*3);
        rect.bottom:=rect.bottom+trunc(ch*3);
      end;
      if koPrintDefinition in flags then
      begin
        rect.top:=rect.top+trunc(ch*2);
        rect.bottom:=rect.bottom+trunc(ch*2);
      end;
      if s<>'' then DrawPackedWordInfo(canvas,rect,copy(s,9,length(s)-8),trunc(ch),false);
    end;
    if koPrintInnerLines in flags then
    begin
      rect.top:=y+round(sizvert*ch);
      if (koPrintReadings in flags) then
        rect.top:=rect.top+trunc(ch*3);
      if (koPrintDefinition in flags) then
        rect.top:=rect.top+trunc(ch*2);
      canvas.MoveTo(trunc(x+ch/2),rect.top);
      canvas.LineTo(trunc(x+nch*ch-ch/2),rect.top);
    end;
  end;

  {readings}
  if koPrintReadings in flags then
  begin
    if koPrintInnerLines in flags then begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end;
    if Length(ony)>=nch then SetLength(ony, nch);
    if Length(kuny)>=nch then SetLength(kuny, nch);
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2),trunc(ch),ony,FontJpEnGrid);
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2+ch),trunc(ch),kuny,FontJpEnGrid);
  end;

  if koPrintDefinition in flags then
  begin
    if koPrintInnerLines in flags then
      if not (koPrintReadings in flags) then begin
        canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
        canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
      end else begin
        canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*(3+sizvert)));
        canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*(3+sizvert)));
      end;
    rect.left:=trunc(x+ch/2);
    rect.right:=trunc(x+nch*ch-ch/2);
    rect.top:=trunc(y+ch*(sizvert)+ch/2);
    rect.bottom:=trunc(y+ch*(1+sizvert)+ch/2);
    if koPrintReadings in flags then begin
      rect.top:=rect.top+trunc(ch*3);
      rect.bottom:=rect.bottom+trunc(ch*3);
    end;

    canvas.Font.Name:=FontEnglish;
    canvas.Font.Height:=trunc(ch);
    canvas.TextRect(rect,rect.left,rect.top,FDefinition);
  end;
end;

end.
