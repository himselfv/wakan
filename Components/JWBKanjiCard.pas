unit JWBKanjiCard;
{ Kanji card painting }

interface
uses SysUtils, Classes, Graphics, Grids, Windows, JWBStrings;

var
  kcchind,
  kcchcomp:TStringList;

procedure DrawKanjiCard(canvas:TCanvas;u:string;x,y:integer;ch:double;
  stcount,outlin,alt,rad,inlin,comp,read,mean,strokeorder,fullcomp,sortfreq:boolean;
  sizhor,sizvert,nofullcomp:integer;calfont:string);
procedure ClearKanjiCardCache;

implementation
uses TextTable, JWBUnit, JWBEdictMarkers, JWBDic, JWBDicSearch, JWBMenu,
  JWBSettings, JWBUserData, JWBKanaConv, JWBCharData, JWBLegacyMarkup,
  JWBWordGrid;

procedure DrawKanjiCard(canvas:TCanvas;u:string;x,y:integer;ch:double;
  stcount,outlin,alt,rad,inlin,comp,read,mean,strokeorder,fullcomp,sortfreq:boolean;
  sizhor,sizvert,nofullcomp:integer;calfont:string);
var ony,kuny,defy,defyu:string;
    radf:integer;
    sl:TStringList;
    s:string; //can contain fstring too
    ws:FString;
    p:integer;
    i,j:integer;
    rect:TRect;
    nch,ncv:integer;
    fontjpch:string;
    FontJpChGrid:string;
    FontJpEnGrid:string;
    dic:TDicIndexCursor;
    freq:string;
    mark:TMarkers;
  rt: integer; //TCharProp.Int(TCharPropType)
  tmp:string;
  CCharProp: TCharPropertyCursor;
  rad_idx: integer;
begin
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
  if read then inc(ncv,3);
  if mean then inc(ncv,2);
  if fullcomp then inc(ncv,1+nofullcomp);
  nch:=sizvert;
  if alt or rad then inc(nch,(sizvert div 2)+1);
  if comp then nch:=nch+1+sizhor;
  sl:=TStringList.Create;
  TChar.Locate('Unicode',u);
  DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc((sizvert-1)*ch),u,calfont);
  if stcount then
    if curlang<>'j' then DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc(ch),fstr(TChar.Str(TChar.fChStrokeCount)),FontEnglish);
  if stcount then
    if curlang='j' then DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc(ch),fstr(TChar.Str(TChar.fJpStrokeCount)),FontEnglish);
  if strokeorder then
    DrawStrokeOrder(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc((sizvert-1)*ch),trunc((sizvert-1)*ch),u,trunc(ch/3*2),clBlack);

  {outer lines}
  if outlin then
  begin
    canvas.MoveTo(x,y);
    canvas.LineTo(trunc(x+nch*ch),y);
    canvas.LineTo(trunc(x+nch*ch),trunc(y+ncv*ch));
    canvas.LineTo(x,trunc(y+ncv*ch));
    canvas.LineTo(x,y);
  end;

  {alternate}
  if alt then begin
    radf:=fSettings.GetPreferredRadicalType;
    rad_idx := GetCharRadicalNumber(u,radf);
    if TRadicals.Locate('Number',rad_idx) then
      DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2),trunc(sizvert/8*3*ch),TRadicals.Str(TRadicals.fUnicode),FontRadical);
  end;

  {radical}
  if rad then
    DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2+(sizvert/2)*ch),trunc((sizvert/8*3)*ch),u,FontJpchGrid);
  if inlin then
  begin
    canvas.MoveTo(trunc(x+ch*sizvert),trunc(y+ch/2));
    canvas.LineTo(trunc(x+ch*sizvert),trunc(y+ch*sizvert-ch/2));
  end;

  {compounds}
  sl.Clear;
  if comp then
  begin
    TUserIdx.SetOrder('Kanji_Ind');
    TUserIdx.Locate('Kanji',u);
    while (not TUserIdx.EOF) and (TUserIdx.Str(TUserIdxKanji)=u) do
    begin
      TUser.Locate('Index',TUserIdx.TrueInt(TUserIdxWord));
      if flength(TUser.Str(TUserKanji))<10 then
//      if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0 then
        if TUserIdx.Bool(TUserIdxBegin) then
          sl.Add('+'+inttostr(flength(TUser.Str(TUserKanji)))+TUser.Str(TUserKanji))
        else
          sl.Add('-'+inttostr(flength(TUser.Str(TUserKanji)))+TUser.Str(TUserKanji));
      TUserIdx.Next;
    end;
    sl.Sort;
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
      if alt or rad then
        DrawUnicode(canvas,trunc(x+((sizvert)*3*ch)/2+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid)
      else
        DrawUnicode(canvas,trunc(x+(sizvert)*ch+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid);
    end;
    if (alt or rad) and (inlin) then
    begin
      canvas.MoveTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch/2));
      canvas.LineTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch*sizvert-ch/2));
    end;
  end;

  {full compounds}
  sl.Clear;
  if fullcomp then
  begin
    if kcchind.IndexOf(u)=-1 then
    begin
      for i:=0 to dicts.Count-1 do
        if dicts[i].loaded and dicts.IsInGroup(dicts[i], 4) and dicts[i].SupportsFrequency then
      begin
        dic:=TDicIndexCursor.Create(dicts[i]);
        try
          dic.Find(itChar,fstrtouni(u));
          while dic.Next do
          begin
            mark:=dic.GetArticleMarkers;
            freq:='0000000';
            if dic.dic.SupportsFrequency and sortfreq then
              freq:=inttostr(9999999-dic.GetFrequency);
            while length(freq)<7 do freq:='0'+freq;
            tmp := EnrichDictEntry(dic.GetArticleBody,mark);
            if pos(UH_LBEG+'spop'+UH_LEND,tmp)=0 then freq[1]:='a';
            if freq<>'9999999'then
            sl.Add(freq+#9+ChinSimplified(dic.GetKanji)+' ['+dic.GetPhonetic+'] {'+tmp+'}{');
          end;
        finally
          FreeAndNil(dic);
        end;
      end;
      sl.Sort;
      for j:=0 to nofullcomp-1 do if sl.Count>j then kcchcomp.Add(sl[j]) else kcchcomp.Add('');
      kcchind.Add(u);
    end;

    i:=kcchind.IndexOf(u);
    for j:=0 to nofullcomp-1 do
    begin
      s:=kcchcomp[i*nofullcomp+j];
      rect.left:=x+round(ch/2);
      rect.right:=x+round(nch*ch-ch/2);
      rect.top:=y+round(sizvert*ch+j*ch+ch/2);
      rect.bottom:=y+round(sizvert*ch+j*ch+ch+ch/2);
      if read then
      begin
        rect.top:=rect.top+trunc(ch*3);
        rect.bottom:=rect.bottom+trunc(ch*3);
      end;
      if mean then
      begin
        rect.top:=rect.top+trunc(ch*2);
        rect.bottom:=rect.bottom+trunc(ch*2);
      end;
      if s<>'' then DrawPackedWordInfo(canvas,rect,copy(s,9,length(s)-8),trunc(ch),false);
    end;
    if inlin then
    begin
      rect.top:=y+round(sizvert*ch);
      if read then
        rect.top:=rect.top+trunc(ch*3);
      if mean then
        rect.top:=rect.top+trunc(ch*2);
      canvas.MoveTo(trunc(x+ch/2),rect.top);
      canvas.LineTo(trunc(x+nch*ch-ch/2),rect.top);
    end;
  end;

  {readings}
  if read then
  begin
    if inlin then
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end;
    CCharProp := TCharPropertyCursor.Create(TCharProp);
    try
      if curlang='j' then begin
        ony:=CCharProp.GetCharProps(u, ptOnReading);
        kuny:=CCharProp.GetCharProps(u, ptKunReading);
        if Length(ony)>=nch then SetLength(ony, nch);
        if Length(kuny)>=nch then SetLength(kuny, nch);
      end else
      if curlang='c' then begin
        ony:=ConvertPinyin(CCharProp.GetCharProps(u, ptMandarinReading));
        kuny:=lowercase(CCharProp.GetCharProps(u, ptCantoneseReading));
      end;
    finally
      FreeAndNil(CCharProp);
    end;
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2),trunc(ch),ony,FontJpEnGrid);
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2+ch),trunc(ch),kuny,FontJpEnGrid);
  end;

  if mean then
  begin
    if inlin then if not read then
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end else
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*(3+sizvert)));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*(3+sizvert)));
    end;
    rect.left:=trunc(x+ch/2);
    rect.right:=trunc(x+nch*ch-ch/2);
    rect.top:=trunc(y+ch*(sizvert)+ch/2);
    rect.bottom:=trunc(y+ch*(1+sizvert)+ch/2);
    if read then
    begin
      rect.top:=rect.top+trunc(ch*3);
      rect.bottom:=rect.bottom+trunc(ch*3);
    end;
    CCharProp := TCharPropertyCursor.Create(TCharProp);
    try
      if curlang='j' then
        defy := CCharProp.GetCharProps(u, ptJapaneseDefinition)
      else
      if curlang='c' then
        defy := CCharProp.GetCharProps(u, ptChineseDefinition)
      else
        defy := '';
    finally
      FreeAndNil(CCharProp);
    end;
    canvas.Font.Name:=FontEnglish;
    canvas.Font.Height:=trunc(ch);
    canvas.TextRect(rect,rect.left,rect.top,defy);
  end;
  sl.Free;
end;

procedure ClearKanjiCardCache;
begin
  kcchind.Clear;
  kcchcomp.Clear;
end;

initialization
  kcchind:=TStringList.Create;
  kcchcomp:=TStringList.Create;

finalization
  kcchind.Free;
  kcchcomp.Free;

end.
