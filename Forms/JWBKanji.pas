unit JWBKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, JWBStrings,
  Grids, DB, ShellAPI, WakanPaintbox;

//{$DEFINE INVALIDATE_WITH_DELAY}
// If set, InvalidateList() will use timer and not just update instanteneously.

//{$DEFINE DRAW_UNSUPPORTED_CHAR_CODES}
//  For chars which are not found in the specified font, draw char codes instead.
//  Neat but confusing.

{$DEFINE AUTODEFOCUS}
//  If the previously selected character is not available under the new filters,
//  automatically set focus to one of the available characters.

type
  TReadFilterFlag = (rfPartial, rfSpace, rfNumber, rfTakedot);
  TReadFilterFlags = set of TReadFilterFlag;

  TfKanji = class(TForm)
    Panel1: TPanel;
    RxLabel15: TLabel;
    btnSearchSort: TSpeedButton;
    btnKanjiDetails: TSpeedButton;
    btnCompounds: TSpeedButton;
    DrawGrid1: TDrawGrid;
    btnPrintCards: TButton;
    pnlDockCompounds: TPanel;
    pnlDockSearch: TPanel;
    SaveDialog1: TSaveDialog;
    UpdateTimer: TTimer;
    splDockCompounds: TSplitter;
    BlankPanel1: TBlankPanel;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure btnPrintCardsClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSearchSortClick(Sender: TObject);
    procedure btnKanjiDetailsClick(Sender: TObject);
    procedure btnCompoundsClick(Sender: TObject);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1DblClick(Sender: TObject);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGrid1Click(Sender: TObject);

  protected
    FFocusedChars: FString;
    function GetCellSize: integer;
    function GetCellFontSize: integer;
    function GetExpectedColCount: integer;
    procedure KanjiGridSelectionChanged;
    function KanjiGridGetSelection: FString;
    function KanjiGridSetSelection(const chars: FString): boolean;
    procedure SetFocusedCharsLow(const Value: FString);
    procedure SetFocusedChars(const Value: FString);
  public
    procedure Reload;
    procedure InvalidateList;
    procedure SaveChars;
    procedure FilterByRadical(const radno: integer);
    function GetKanji(cx,cy:integer):string;
    property FocusedChars: FString read FFocusedChars write SetFocusedChars;

  protected
    procedure ReadFilter(flt:TStringList;const tx:string;typ:integer;flags:TReadFilterFlags);
    procedure ReadRaineFilter(fltradical:TStringList;const ARadicals:string);

  end;

var
  fKanji: TfKanji;
  testkanji:string;

implementation

uses JWBMenu, JWBRadical, JWBSettings, JWBPrint,
  JWBKanjiSearch, JWBKanjiCompounds, JWBKanjiDetails,
  MemSource, JWBVocab, JWBIO, JWBFileType,
  JWBDicSearch, JWBKanjiCard, JWBKanaConv, JWBUnit, JWBWakanText, JWBCategories,
  JWBAnnotations, TextTable, JWBDic, JWBEdictMarkers, JWBCharData;

var ki:TStringList;
    calfonts:TStringList;
    caltype:integer;

{$R *.DFM}

procedure TfKanji.FormShow(Sender: TObject);
begin
  if fKanjiSearch<>nil then
    fKanjiSearch.ReloadOtherTypes;
  Reload;
  caltype:=0;
  if fKanjiSearch<>nil then
    Self.btnKanjiDetails.Down:=fKanjiDetails.Visible;
end;

procedure TfKanji.FormHide(Sender: TObject);
begin
end;

//split value string into string list. values are separated by comma or ;
//if numeric, values can have format n1-n2 (edtPinYin.g. 3-5 is expanded into 3,4,5)
procedure MakeList(tx:string;number:boolean;sl:TStringList);
var fnd:boolean;
    curtx,txleft:string;
    min,max:integer;
    i:integer;
begin
  if sl.Count>0 then exit;
  txleft:=tx;
  sl.Sorted:=true;
  fnd:=false;
  while (not fnd) and (txleft<>'') do
  begin
    curtx:='';
    while (length(txleft)>0) and (txleft[1]<>';') and (txleft[1]<>',') do
    begin
      curtx:=curtx+txleft[1];
      delete(txleft,1,1);
    end;
    if (length(txleft)>0) and ((txleft[1]=';') or (txleft[1]=',')) then delete(txleft,1,1);
    if not number then
      sl.Add(curtx) {Previously this added uppercase(curtx), but I removed that
        because that breaks searching for Definitions (they're not uppercase).
        I suppose uppercase was added when everything passed here was already
        in hex, which is not the case now.
        When the contents is in hex, it has to be uppercased just before
        searching for it. }
    else
      if pos('-',curtx)=0 then
        sl.Add(curtx)
      else
      begin
        if not TryStrToInt(copy(curtx,1,pos('-',curtx)-1), min) then
          min := 1;
        delete(curtx,1,pos('-',curtx));
        if not TryStrToInt(curtx, max) then
          max := min;
        for i:=min to max do sl.Add(inttostr(i));
      end;
  end;
  if sl.Count=0 then sl.Add('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$');
end;

procedure TfKanji.ReadFilter(flt:TStringList;const tx:string;typ:integer;flags:TReadFilterFlags);
var CCharProp: TCharPropertyCursor;
  sl:TStringList;
  s_fltval:string;
  s_val:string;
  i:integer;
  dot:integer;
  propType: PCharPropType;
  match: boolean;
  s_sp: string;
  s_dot: string;
begin
  sl:=TStringList.Create;
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
   //Convert filter value into a list of exact values to match
    MakeList(tx,rfNumber in flags,sl);

    propType := FindCharPropType(typ);
    s_sp := ' ';
    s_dot := '.';

    for i:=0 to sl.Count-1 do
    begin
      s_fltval:=uppercase(sl[i]); //Locate is case-insensitive anyway

      CCharProp.LocateRawValue(s_fltval);
      while not CCharProp.EOF do
      begin
        s_val:=uppercase(CCharProp.RawValue);
        if s_val<>s_fltval then
          if not (rfPartial in flags) then
            break
          else
          if (rfSpace in flags) and (pos(s_fltval+s_sp, s_val)<>1) then
            break
          else
          if pos(s_fltval,s_val)<>1 then
            break;

        match := (CCharProp.Int(TCharProp.fTypeId)=typ);
        case propType.dataType of
          'R': begin //Radical format: radical['][.stroke_count]
            match := match and (
                 (pos(s_fltval+s_dot, s_val)=1)
              or (pos(s_fltval+s_dot, s_val)=1)
              or (s_fltval=s_val)
            );
          end;
        else
          if rfTakedot in flags then
            dot:=CCharProp.Int(TCharProp.fReadDot)
          else
            dot:=0;
          match := match and (
              not (rfTakedot in flags)
              or (s_val=s_fltval)
              or ((dot>0) and (s_fltval=copy(s_val,dot-1)))
            );
        end;

        if match then
          flt.Add(CCharProp.Kanji);
        CCharProp.Next;
      end;
    end;

  finally
    FreeAndNil(CCharProp);
    FreeAndNil(sl);
  end;
  flt.Sort;
  flt.Sorted:=true;
end;

{ Converts RadicalIndexes to the standard filter string acceptable by ReadFilter }
function RadicalIndexesToFilter(const AIndexes: TRadicalIndexes): string;
var i: integer;
begin
  if Length(AIndexes)<1 then begin
    Result := '';
    exit;
  end;

  Result := IntToStr(AIndexes[0]);
  for i := 1 to Length(AIndexes)-1 do
    Result := Result + ';' + IntToStr(AIndexes[i]);
end;

procedure TfKanji.ReadRaineFilter(fltradical:TStringList;const ARadicals:string);
var ARadIndexes: TRadicalIndexes;
  sltemp:TStringList;
  s2:string;
  rrind:integer;
  rrus:boolean;
  p:PWideChar;
  i,j:integer;
  rchars:FString;
begin
  ARadIndexes := RadicalsToIndexes(stRaine, ARadicals);
  sltemp:=TStringList.Create;
  sltemp.Sorted:=true;
  fltradical.Sorted:=true;
  rrus:=false;
  for j := 0 to Length(ARadIndexes)-1 do
  begin
    rrind:=ARadIndexes[j];
    rchars := RaineRadicals.GetContainingChars(rrind);
    p:=PWideChar(rchars);
    for i:=1 to Length(rchars) do begin
      s2:=fstr(p^);
      p:=p+1;
      if not rrus or (sltemp.IndexOf(s2)<>-1) then fltradical.Add(s2);
    end;
    if j<Length(ARadIndexes)-1 then
    begin
      sltemp.Assign(fltradical);
      fltradical.Clear;
      rrus:=true;
    end;
  end;
  sltemp.Free;
end;

//---------------------------------------
//filter kanji and show them in the grid
//---------------------------------------
procedure TfKanji.Reload;

function InRange(tx,fld:string;number:boolean;sl:TStringList):boolean;
begin
  MakeList(tx,number,sl);
  result:=sl.IndexOf(uppercase(fld))>-1;
end;

var fltclip,fltpinyin,fltyomi,fltmean:TStringList;
    accept:boolean;
    i,j,grs:integer;
    s1,s2,s3:string;
    sbJouyou:string;
    x:integer;
    sl4,sl10:TStringList;
    fltradical,fltskip,fltother:TStringList;
    clipsort:boolean;
    clipind:integer;

  categories: array of integer; //of checked category indexes
  flags: TReadFilterFlags;
  kclass: char;

  procedure CopyCategories; //they're UNIMAGINABLY slow if used as is
  var i: integer;
  begin
    SetLength(categories, 0);
    for i:=0 to fKanjiSearch.lbCategories.Items.Count-1 do
      if fKanjiSearch.lbCategories.Checked[i] then
    begin
      SetLength(categories, Length(categories)+1);
      categories[Length(categories)-1] := GetCatIdx(fKanjiSearch.lbCategories, i);
    end;
  end;

  function CheckCategories: boolean;
  var i: integer;
  begin
    if Length(categories)<=0 then begin
      Result := true;
      exit;
    end;

   { Filter mode: 0=OR, 1=AND }
    Result := (fKanjiSearch.rgOrAnd.ItemIndex=1); //default result: true if ANDs, false if ORs
    for i := 0 to Length(categories) - 1 do
      if IsKnown(categories[i], TChar.FCh(TChar.fUnicode)) then begin
        if fKanjiSearch.rgOrAnd.ItemIndex=0 then begin Result:=true; break; end;
      end else begin
        if fKanjiSearch.rgOrAnd.ItemIndex=1 then Result:=false;
      end;
    if fKanjiSearch.cbNot.Checked then Result:=not Result;
  end;

begin
  if not Visible then exit;
  if fKanjiSearch=nil then exit;
  DrawGrid1.Perform(WM_SETREDRAW, 0, 0); //disable redraw
  try
    Screen.Cursor:=crHourGlass;
    fMenu.aKanjiLearned.Checked:=fKanjiSearch.SpeedButton1.Down;
    fMenu.aKanjiCommon.Checked:=fKanjiSearch.btnOnlyCommon.Down;
    fMenu.aKanjiClipboard.Checked:=fKanjiSearch.btnInClipboard.Down;
    fltclip:=TStringList.Create;
    fltpinyin:=TStringList.Create;
    fltyomi:=TStringList.Create;
    fltmean:=TStringList.Create;
    fltradical:=TStringList.Create;
    fltskip:=TStringList.Create;
    fltother:=TStringList.Create;
    sl4:=TStringList.Create;
    sl10:=TStringList.Create;
    CopyCategories;
    if fKanjiSearch.btnInClipboard.Down then
      for i:=1 to flength(clip) do
        if (Word(fgetch(clip,i)) >= $4000) and (fltclip.IndexOf(fgetch(clip,i))<0) then
          fltclip.Add(fgetch(clip,i));
    if fKanjiSearch.sbPinYin.Down then begin
      ReadFilter(fltpinyin,fKanjiSearch.edtPinYin.text,ptMandarinReading,[rfPartial]); //Mandarin
      ReadFilter(fltpinyin,fKanjiSearch.edtPinYin.text,ptCantoneseReading,[rfPartial]); //Canton
    end;
    if fKanjiSearch.sbYomi.Down then begin
     //ON and KUN
      if fSettings.cbYomiIgnoreOkurigana.Checked then
        flags := [rfPartial, rfTakedot]
      else
        flags := [];
      ReadFilter(fltyomi,RomajiToKana('H'+fKanjiSearch.edtYomi.Text,'j',[rfDeleteInvalidChars]),ptOnReading,flags);
      ReadFilter(fltyomi,RomajiToKana('H'+fKanjiSearch.edtYomi.Text,'j',[rfDeleteInvalidChars]),ptKunReading,flags);
      ReadFilter(fltyomi,RomajiToKana('K'+fKanjiSearch.edtYomi.Text,'j',[rfDeleteInvalidChars]),ptOnReading,flags);
      ReadFilter(fltyomi,RomajiToKana('K'+fKanjiSearch.edtYomi.Text,'j',[rfDeleteInvalidChars]),ptKunReading,flags);
    end;
    if fKanjiSearch.sbSKIP.Down then
      ReadFilter(fltskip,fKanjiSearch.edtSKIP.Text,ptSKIP,[rfPartial]); //SKIP
   { Raine filters multi-selection with AND (only the characters with all the chosen parts are shown),
    Classical with OR (characters which match at least one radical are shown).
    This is because a character has only one Classical Radical so AND is pointless. }
    if fKanjiSearch.sbRadicals.Down then
      case fKanjiSearch.curRadSearchType of
        stClassic: ReadFilter(fltradical,
          RadicalIndexesToFilter(RadicalsToIndexes(stClassic,fKanjiSearch.CurRadChars)),
          fSettings.GetPreferredRadicalType,[rfNumber]); //Radicals
        stRaine: ReadRaineFilter(fltradical,fKanjiSearch.CurRadChars);
      end;
    if fKanjiSearch.sbOther.Down then
    begin
      if fKanjiSearch.cbOtherType.ItemIndex=0 then
        fltother.Add(fKanjiSearch.edtOther.Text)
      else
        j:=0;
      for i:=0 to Length(CharPropTypes)-1 do
        if CharPropTypes[i].id>20 then
        begin
          inc(j);
          if j=fKanjiSearch.cbOtherType.ItemIndex then begin
            if CharPropTypes[i].dataType='N' then
              flags := [rfNumber]
            else
              flags := [];
            ReadFilter(fltother,fKanjiSearch.edtOther.Text,CharPropTypes[i].id,flags);
          end;
        end;
    end;
    if fKanjiSearch.sbDefinition.Down then
      if curLang='c' then begin
        ReadFilter(fltmean,fKanjiSearch.edtDefinition.text,ptChineseDefinition,[rfPartial,rfSpace]); //Chinese definition
      end else begin
        ReadFilter(fltmean,fKanjiSearch.edtDefinition.text,ptJapaneseDefinition,[rfPartial,rfSpace]); //Japanese definition
      end;
    grs := GetCellSize();
    if curlang='j' then
      case fKanjiSearch.rgSortBy.ItemIndex of
        0,3,4:TChar.SetOrder('JpUnicode_Ind');
        1:TChar.SetOrder('JpStrokeCount_Ind');
        2:TChar.SetOrder('JpFrequency_Ind');
      end
    else
      case fKanjiSearch.rgSortBy.ItemIndex of
        0,3,4:TChar.SetOrder('ChUnicode_Ind');
        1:TChar.SetOrder('ChStrokeCount_Ind');
        2:TChar.SetOrder('ChFrequency_Ind');
      end;
    ki.Clear;
    clipsort:=(fKanjiSearch.btnInClipboard.Down) and (fKanjiSearch.rgSortBy.ItemIndex=4);
    clipind:=0;
  //  if not clipsort then fltclip.Sort;
    while ((not clipsort) and ((not TChar.EOF) and ((curlang='c') or (TChar.Int(TChar.fChinese)=0)))) or
          ((clipsort) and (clipind<fltclip.Count)) do
    begin
      accept:=true;
      if clipsort then accept:=TChar.Locate('Unicode',fltclip[clipind]);
      if accept and (curlang='c') and (fSettings.RadioGroup5.ItemIndex=0) and (TChar.Str(TChar.fType)='S') then accept:=false;
      if accept and (curlang='c') and (fSettings.RadioGroup5.ItemIndex=1) and (TChar.Str(TChar.fType)='T') then accept:=false;
      if accept and (fKanjiSearch.btnOnlyCommon.Down) and (curlang='c') and (TChar.Int(TChar.fChFrequency)>=255) then accept:=false;
      if accept and (fKanjiSearch.btnOnlyCommon.Down) and (curlang<>'c') and (TChar.Int(TChar.fJouyouGrade)>=10) then accept:=false;
      if accept and (not clipsort) and (fKanjiSearch.btnInClipboard.Down) and (fltclip.IndexOf(uppercase(TChar.Str(TChar.fUnicode)))=-1) then accept:=false;
      if accept and (fKanjiSearch.sbPinYin.Down) and (fltpinyin.IndexOf(TChar.Str(TChar.fUnicode))=-1) then accept:=false;
      if accept and (fKanjiSearch.sbYomi.Down) and (fltyomi.IndexOf(TChar.Str(TChar.fUnicode))=-1) then accept:=false;
      if accept and (fKanjiSearch.sbDefinition.Down) and (fltmean.IndexOf(TChar.Str(TChar.fUnicode))=-1) then accept:=false;
      if accept and (fKanjiSearch.sbSKIP.Down) and (fltskip.IndexOf(TChar.Str(TChar.fUnicode))=-1) then accept:=false;
      if accept and (fKanjiSearch.sbRadicals.Down) then
        case fKanjiSearch.curRadSearchType of
          stClassic: if fltradical.IndexOf(TChar.Str(TChar.fUnicode))=-1 then accept:=false;
          stRaine: if fltradical.IndexOf(TChar.Str(TChar.fUnicode))=-1 then accept:=false;
        end;
      if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.cbOtherType.ItemIndex=0) and (fltother.IndexOf(TChar.Str(TChar.fUnicode))=-1) then accept:=false;
      if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.cbOtherType.ItemIndex>0) and (fltOther.IndexOf(TChar.Str(TChar.fUnicode))=-1) then accept:=false;
  //    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.SpeedButton25.Down) and not InRange(fKanjiSearch.edtOther.text,TChar.Str(TCharUnicode),false,sl1) then accept:=false;
  //    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.SpeedButton26.Down) and not InRange(fKanjiSearch.edtOther.text,TChar.Str(TCharUnicode),true,sl2) then accept:=false;
  //    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.SpeedButton27.Down) and not InRange(fKanjiSearch.edtOther.text,TChar.Str(TCharUnicode),true,sl3) then accept:=false;
      if (curlang='c') and accept and (fKanjiSearch.sbStrokeCount.Down) and not InRange(fKanjiSearch.edtStrokeCount.Text,TChar.Str(TChar.fChStrokeCount),true,sl4) then accept:=false;
      if (curlang<>'c') and accept and (fKanjiSearch.sbStrokeCount.Down) and not InRange(fKanjiSearch.edtStrokeCount.Text,TChar.Str(TChar.fJpStrokeCount),true,sl4) then accept:=false;
      if accept and (fKanjiSearch.sbSKIP.Down) then
      begin
        s1:=fKanjiSearch.edtSKIP.Text;
        s2:='0';
        s3:='0';
        if pos('-',s1)>0 then
        begin
          s2:=s1;
          s1:=copy(s2,1,pos('-',s2)-1);
          delete(s2,1,pos('-',s2));
        end;
        if pos('-',s2)>0 then
        begin
          s3:=s2;
          s2:=copy(s3,1,pos('-',s3)-1);
          delete(s3,1,pos('-',s3));
        end;
  //      if accept then accept:=InRange(s1,TChar.Str(TCharSKIP1),false,sl6);
  //      if accept then accept:=InRange(s2,TChar.Str(TCharSKIP2),false,sl7);
  //      if accept then accept:=InRange(s3,TChar.Str(TCharSKIP3),false,sl8);
      end;
      if accept then
        accept := CheckCategories;

  {    if accept and (fKanjiSearch.SpeedButton16.Down) then
      begin
        s1:=fKanjiSearch.Edit5.Text;
        if pos('.',s1)>0 then delete(s1,pos('.',s1),1);
        if accept then accept:=InRange(s1,TChar.Str(TCharFourCornerCode),false,sl9);
      end; }
      if accept and (fKanjiSearch.sbJouyou.Down) and not InRange(fKanjiSearch.edtJouyou.Text,TChar.Str(TChar.fJouyouGrade),true,sl10) then accept:=false;
      if accept then
      begin
        kclass := GetCharClass(TChar.Str(TChar.fUnicode));
        if ((curlang<>'c') and (fKanjiSearch.rgSortBy.ItemIndex=3))
        or ((curlang='c') and (fKanjiSearch.rgSortBy.ItemIndex=3)) then
          ki.Insert(random(ki.Count),kclass+TChar.Str(TChar.fUnicode))
        else
          ki.Add(kclass+TChar.Str(TChar.fUnicode));
      end;
      if clipsort then inc(clipind) else TChar.Next;
    end;
    fltclip.Free;
    fltpinyin.Free;
    fltyomi.Free;
    fltmean.Free;
    fltradical.Free;
    fltskip.Free;
    fltother.Free;
    sl4.Free;
    sl10.Free;

   //First try new translation strings
    if curlang='c' then begin
      case fSettings.RadioGroup5.ItemIndex of
        0: sbJouyou:=_l('#00958^eFound traditional characters (%d):', [ki.Count]);
        1: sbJouyou:=_l('#00959^eFound simplified characters (%d):', [ki.Count]);
      else sbJouyou:=_l('#00960^eFound characters (%d):', [ki.Count]);
      end;
    end else
      sbJouyou:=_l('#00961^eFound kanji (%d):',[ki.Count]);

    RxLabel15.Caption:=sbJouyou;
    DrawGrid1.ColCount:=GetExpectedColCount();
    x:=DrawGrid1.ColCount;
    if ki.Count=0 then DrawGrid1.RowCount:=1 else
      DrawGrid1.RowCount:=((ki.Count-1) div x)+1;
    DrawGrid1.DefaultRowHeight:=grs;
    DrawGrid1.DefaultColWidth:=grs;
    testkanji:='';
    for i:=0 to 14 do if i<ki.Count then testkanji:=testkanji+copy(ki[i],2,4);
    TChar.SetOrder('ChUnicode_Ind');
    if not KanjiGridSetSelection(FocusedChars) then begin //previous kanji not in list
   {$IFDEF AUTODEFOCUS}
      Self.KanjiGridSelectionChanged; //as if the user did that
   {$ENDIF}
    end;
  finally
    DrawGrid1.Perform(WM_SETREDRAW, 1, 0); //enable redraw
   { WM_SETREDRAW(1) implicitly makes DrawGrid visible without telling Delphi,
    so we'd have to do Show(); Hide(); to re-hide it.
    But we'd still have problems because sometimes window is implicitly shown
    again on load.
    This approach works: }
    if ki.Count>0 then
      ShowWindow(DrawGrid1.Handle,1)
    else
      ShowWindow(DrawGrid1.Handle,0);
  end;

  Screen.Cursor:=crDefault;
  DrawGrid1.Invalidate;
end;

function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var lh,lc:integer;
    ncv,nch:integer;
    ch:double;
    numh:integer;
begin
  GetPrintLine(width,height,width,height,strtoint(fSettings.Edit11.Text),lh,lc);
  ncv:=strtoint(fSettings.edit13.text);
  if fSettings.CheckBox22.Checked then inc(ncv,3);
  if fSettings.CheckBox44.Checked then inc(ncv,2);
  if fSettings.CheckBox62.Checked then inc(ncv,1+strtoint(fSettings.Edit35.Text));
  ch:=lh/ncv;
  nch:=strtoint(fSettings.edit13.text);
  if (fSettings.CheckBox19.Checked) or (fSettings.CheckBox20.Checked) then inc(nch,(strtoint(fSettings.Edit13.Text) div 2)+1);
  if fSettings.CheckBox18.Checked then nch:=nch+1+strtoint(fSettings.edit12.text);
  numh:=trunc(width-ch*2) div trunc(nch*ch);
  lc:=trunc(height-ch*2) div trunc(ncv*ch);
  result:=((ki.Count-1) div (numh*lc))+1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var lh,lc:integer;
    ncv,nch:integer;
    ch:double;
    numh,i:integer;
    u:string;
    x,xp,y,yp:integer;
begin
  GetPrintLine(origwidth,origheight,origwidth,origheight,strtoint(fSettings.Edit11.Text),lh,lc);
//  lh:=round(0.98*lh);
  ncv:=strtoint(fSettings.edit13.text);
  if fSettings.CheckBox22.Checked then inc(ncv,3);
  if fSettings.CheckBox44.Checked then inc(ncv,2);
  if fSettings.CheckBox62.Checked then inc(ncv,1+strtoint(fSettings.Edit35.Text));
  ch:=lh/ncv;
  nch:=strtoint(fSettings.edit13.text);
  if (fSettings.CheckBox19.Checked) or (fSettings.CheckBox20.Checked) then inc(nch,(strtoint(fSettings.Edit13.Text) div 2)+1);
  if fSettings.CheckBox18.Checked then nch:=nch+1+strtoint(fSettings.edit12.text);
  numh:=trunc(origwidth-ch*2) div trunc(nch*ch);
  lc:=trunc(origheight-ch*2) div trunc(ncv*ch);
  ch:=ch/origwidth*width;
  for i:=0 to numh*lc-1 do
  begin
    if ((pagenum-1)*numh*lc+i)<ki.Count then
    begin
      u:=ki[((pagenum-1)*numh*lc)+i];
      delete(u,1,1);
      if fSettings.CheckBox24.Checked then
      begin
        yp:=i mod lc;
        xp:=numh-(i div lc)-1;
      end else
      begin
        xp:=i mod numh;
        yp:=i div numh;
      end;
      y:=trunc((height-lc*ncv*ch)/2+(yp*ncv*ch));
      if (numh>1) and (fSettings.CheckBox25.Checked) then
      x:=trunc(ch+((width-(numh*nch*ch+2*ch))/(numh-1))*xp+xp*nch*ch) else x:=trunc(ch+xp*nch*ch);
      try
        DrawKanjiCard(canvas,u,x,y,ch,fSettings.CheckBox45.Checked,fSettings.CheckBox21.Checked,
          fSettings.CheckBox19.Checked,fSettings.CheckBox20.Checked,fSettings.CheckBox23.Checked,
          fSettings.CheckBox18.Checked,fSettings.CheckBox22.Checked,fSettings.CheckBox44.Checked,fSettings.CheckBox52.Checked,
          fSettings.CheckBox62.Checked,fSettings.CheckBox63.Checked,
          strtoint(fSettings.Edit12.Text),strtoint(fSettings.Edit13.Text),strtoint(fSettings.Edit35.Text),fSettings.Edit14.Text);
      except end;
    end;
  end;
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsCharacterCardPrinting;
  fSettings.ShowModal;
end;

procedure TfKanji.btnPrintCardsClick(Sender: TObject);
begin
  ClearKanjiCardCache;
  PrintPreview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00134^eKanji cards'));
end;

procedure TfKanji.RadioGroup1Click(Sender: TObject);
begin
  Reload;
end;

{ Called when a kanji selection changes }
procedure TfKanji.KanjiGridSelectionChanged;
begin
 { Store selected chars and pass to all child windows, but do not apply selection
  again }
  SetFocusedCharsLow(KanjiGridGetSelection);
end;

function TfKanji.KanjiGridGetSelection: FString;
var sel: TGridRect;
  i, j: integer;
  char:FString;
begin
  Result := '';
  sel := DrawGrid1.Selection;
  for i := sel.Top to sel.Bottom do
    for j := sel.Left to sel.Right do begin
      if DrawGrid1.ColCount*i+j>=ki.Count then //selection can cover unused cells
        continue;
      char := ki[DrawGrid1.ColCount*i+j];
      Result := Result + copy(char,2,Length(char)-1); //delete first char
    end;
end;

{ Highlights the specified characters in the grid. There are limitations on what
 we can highlight at this time, so returns false if the highlight differs from
 what was requested.
 Basic function. Does not call any additional handlers. }
function TfKanji.KanjiGridSetSelection(const chars: FString): boolean;
var i, j, cols: integer;
  mr: TGridRect;
begin
  if flength(chars)<>1 then begin
    Result := false;
    exit;
  end;

 //Find first matching char
  Result := false;
  mr.Left := 0;
  mr.Top := 0;
  mr.Right := 0;
  mr.Bottom := 0;
  cols := DrawGrid1.ColCount;
  for i:=0 to ki.Count-1 do
    for j := 1 to flength(chars) do
      if fgetch(chars, j)=copy(ki[i],2,4) then begin
        mr.Left:=i mod cols;
        mr.Top:=i div cols;
        mr.Right:=i mod cols;
        mr.Bottom:=i div cols;
        Result:=true;
        break;
      end;

 { If nothing was highlighted, we must highlight something so highlight first
  element }
  if Result=false then begin
    mr.Left := 0;
    mr.Top := 0;
    mr.Right := 0;
    mr.Bottom := 0;
  end;

 { At this time we cannot highlight more than one char programmatically }
  if flength(chars)>1 then
    Result := false;

 //Apply whatever highlight we generated
  DrawGrid1.Selection:=mr;
  if (mr.Top>1) and (DrawGrid1.RowCount>DrawGrid1.VisibleRowCount) then
    DrawGrid1.TopRow:=mr.Top-1
  else
    DrawGrid1.TopRow:=0;
end;

procedure TfKanji.DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
 //Some cells are off limits
  CanSelect := not (DrawGrid1.ColCount*ARow+ACol>=ki.Count);
end;

procedure TfKanji.DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  KanjiGridSelectionChanged;
end;

procedure TfKanji.DrawGrid1Click(Sender: TObject);
begin
 { We need to react to kanji selection change on mouse wheel,
  but MouseWheelDown/Up happen before the selection is updated.
  Click() seems to happen at any time focus changes so we do it here,
  but if this fails us we'll switch to reacting from SelectCell() I guess and
  manually constructing actual selection (because it's not saved yet there too) }
  KanjiGridSelectionChanged;
end;

procedure TfKanji.DrawGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KanjiGridSelectionChanged;
end;

procedure TfKanji.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var w:widechar;
  kix:FString;
  kig:string;
  sbJouyou:string;
  r_copy: TRect;
  fontface: string;
  fontsize: integer;
begin
  if (ARow*DrawGrid1.ColCount+ACol>=ki.Count) then
  begin
    DrawGrid1.Canvas.Pen.Color:=clWindow;
    DrawGrid1.Canvas.Brush.Color:=clWindow;
    DrawGrid1.Canvas.FillRect(Rect);
    exit;
  end;
  kix:=ki[DrawGrid1.ColCount*ARow+Acol];
  delete(kix,1,1);
 {$IFDEF UNICODE}
  w := kix[1];
 {$ELSE}
  w:=HexToUnicode(kix)[1];
 {$ENDIF}
  if gdSelected in State then DrawGrid1.Canvas.Brush.Color:=clHighlight else
  DrawGrid1.Canvas.Brush.Color:=clWindow;
  if gdSelected in state then DrawGrid1.Canvas.Font.Color:=clHighlightText else
  DrawGrid1.Canvas.Font.Color:=clWindowText;
  if (not fSettings.CheckBox3.Checked) and not (gdSelected in State) then
  begin
    DrawGrid1.Canvas.Brush.Color:=Col('Kanji_Back');
    TChar.Locate('Unicode',kix);
    kig:=ki[DrawGrid1.ColCount*ARow+Acol];
    case kig[1] of
      'K':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Learned');
      'C':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Common');
      'U':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Rare');
      'N':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Names');
    end;
  end;
  if fSettings.CheckBox69.Checked and HaveAnnotations then
  begin
    Annot.SeekK(kix,'');
    sbJouyou:=Annot.GetOne('c');
    if sbJouyou<>'' then try
      DrawGrid1.Canvas.Font.Color:=strtoint('0x'+copy(sbJouyou,5,2)+copy(sbJouyou,3,2)+copy(sbJouyou,1,2));
    except end;
  end;
  DrawGrid1.Canvas.FillRect(Rect);
  if curLang='j' then
    fontface:=FontJapaneseGrid
  else
  case fSettings.RadioGroup5.ItemIndex of
    0:fontface:=FontChineseGrid;
    1:fontface:=FontChineseGridGB;
    2:fontface:=FontRadical;
  else fontface:=FontChineseGrid;
  end;
  fontsize := GetCellFontSize();
  DrawGrid1.Canvas.Font.Style:=[];

 { Some glyphs may be outright impossible to draw -- no suitable fonts, even with substitution }
  r_copy := rect;
  r_copy.Left := r_copy.Left + 5;
  r_copy.Top := r_copy.Top + 4;
  DrawUnicodeChar(DrawGrid1.Canvas, r_copy, fontsize, w, fontface);

 {$IFDEF DRAW_UNSUPPORTED_CHAR_CODES}
  if GetGlyphIndices(DrawGrid1.Canvas.Handle,@w,1,@w_ind, GGI_MARK_NONEXISTING_GLYPHS)=GDI_ERROR then
    RaiseLastOsError();
  if w_ind<>$FFFF then
    TextOutW(DrawGrid1.Canvas.Handle,Rect.Left+5,Rect.Top+4,@w,1)
  else begin
   //Draw unicode index instaed
    ws := IntToHex(Utf16ToUnicodeIndex(w),4);
    DrawGrid1.Canvas.Font.Name:=FontEnglish;
    case fSettings.rgKanjiGridSize.ItemIndex of
      0:DrawGrid1.Canvas.Font.Height:=10;
      1:DrawGrid1.Canvas.Font.Height:=14;
      2:DrawGrid1.Canvas.Font.Height:=22;
    end;
    r_copy:=Rect;
    DrawText(DrawGrid1.Canvas.Handle,PChar(ws),Length(ws),r_copy,DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end;
 {$ENDIF}

  if fSettings.CheckBox1.Checked then
  begin
    TChar.Locate('Unicode',kix);
    DrawGrid1.Canvas.Font.Name:=FontEnglish;
    DrawGrid1.Canvas.Font.Height:=8+4*fSettings.rgKanjiGridSize.ItemIndex;
    DrawGrid1.Canvas.Font.Color:=clWindowText;
    if curlang='c' then DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TChar.fChStrokeCount));
    if curlang<>'c' then DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TChar.fJpStrokeCount));
  end;
  fKanjiDetails.pbKanji.Invalidate;
  fKanjiDetails.pbRadical.Invalidate;
  fKanjiDetails.pbSimplified.Invalidate;
end;

procedure TfKanji.DrawGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var sel: TGridRect;
begin
  if (Key=Ord('A')) and (ssCtrl in Shift) then begin
    sel.Left := 0;
    sel.Top := 0;
    sel.Right := DrawGrid1.ColCount-1;
    sel.Bottom := DrawGrid1.RowCount-1;
    DrawGrid1.Selection := sel;
  end;
end;

procedure TfKanji.DrawGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=' ' then begin
    clip:=clip+curkanji;
    fMenu.SetClipboard;
  end;
  if key=Chr(VK_RETURN) then
    if not fMenu.aKanjiDetails.Checked then
      fMenu.aKanjiDetails.Execute
    else
      if fKanjiDetails.Visible then
        fKanjiDetails.SetFocus;
  if key=Chr(VK_BACK) then begin
    if length(clip)>0 then delete(clip,length(clip)-3,4);
    fMenu.SetClipboard;
  end;
end;

procedure TfKanji.InvalidateList;
begin
{$IFDEF INVALIDATE_WITH_DELAY}
  UpdateTimer.Interval:=1000;
  UpdateTimer.Enabled:=true;
{$ELSE}
  Reload;
{$ENDIF}
end;

procedure TfKanji.UpdateTimerTimer(Sender: TObject);
begin
  UpdateTimer.Enabled:=false;
  Reload;
end;

procedure TfKanji.BitBtn1Click(Sender: TObject);
begin
  DrawGrid1.SetFocus;
end;

procedure TfKanji.SpeedButton25Click(Sender: TObject);
begin
  Reload;
end;

{ Returns cell width/height under current settings }
function TfKanji.GetCellSize: integer;
begin
  case fSettings.rgKanjiGridSize.ItemIndex of
    0: Result:=30;
    1: Result:=45;
    2: Result:=60;
  else Result:=60;
  end;
end;

function TfKanji.GetCellFontSize: integer;
begin
  case fSettings.rgKanjiGridSize.ItemIndex of
    0:Result:=22;
    1:Result:=37;
    2:Result:=52;
  else Result:=52;
  end;
end;


{ Returns expected column count according to current cell size / width }
function TfKanji.GetExpectedColCount: integer;
var grs:integer;
begin
  grs := GetCellSize;
  Result := (DrawGrid1.ClientWidth-24) div grs
end;

procedure TfKanji.FormResize(Sender: TObject);
begin
  if DrawGrid1.ColCount<>GetExpectedColCount then Reload;
end;

procedure TfKanji.btnSearchSortClick(Sender: TObject);
begin
  fMenu.aKanjiSearch.Execute;
end;

procedure TfKanji.btnKanjiDetailsClick(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfKanji.btnCompoundsClick(Sender: TObject);
var CanSelect:boolean;
begin
  fMenu.aKanjiCompounds.Execute;
  DrawGrid1SelectCell(Sender, DrawGrid1.Col, DrawGrid1.Row, CanSelect);
end;

procedure TfKanji.FilterByRadical(const radno: integer);
begin
  if radno=NoRadical then exit;
  fKanjiSearch.CurRadSearchType:=stClassic;
  fKanjiSearch.CurRadChars:=RadicalUnicode(radno);
  fKanjiSearch.pbRadicals.Invalidate;
  Self.Reload;
end;

procedure TfKanji.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then
    fMenu.PopupImmediate(false);
end;

procedure TfKanji.DrawGrid1DblClick(Sender: TObject);
begin
  if not fMenu.aKanjiDetails.Checked then fMenu.aKanjiDetails.Execute else if fKanjiDetails.Visible then fKanjiDetails.SetFocus;
end;

procedure TfKanji.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipMouseMove(DrawGrid1,x,y,false);
end;

function TfKanji.GetKanji(cx,cy:integer):string;
begin
  if (cy*DrawGrid1.ColCount+cx>=ki.Count) or (cx<0) or (cy<0) then
  begin
    result:='';
    exit;
  end;
  result:=ki[DrawGrid1.ColCount*cy+cx];
  delete(result,1,1);
end;

procedure TfKanji.SaveChars;
var i:integer;
  conv: TStreamEncoder;
begin
  if not SaveDialog1.Execute then
    exit;

  conv := CreateTextFile(SaveDialog1.FileName,Conv_ChooseType(false,TUnicodeEncoding));
  try
    for i:=0 to ki.Count-1 do
      conv.Write(copy(ki[i],2,4));
  finally
    FreeAndNil(conv);
  end;
end;

{ Changes the set of characters selected in the Kanji grid and in all related
 tool windows. }
procedure TfKanji.SetFocusedCharsLow(const Value: FString);
begin
  if FFocusedChars=Value then exit;
  FFocusedChars := Value;

  fKanjiDetails.SetCharDetails(Value);

 { Pass selected chars to all tool windows }
  if flength(Value)=1 then begin //single char
    fKanjiCompounds.SetCharCompounds(fgetch(Value, 1));
    AnnotShowMedia(Value,'');
  end else begin //multiple or none
    fKanjiCompounds.Clear;
  end;
end;

procedure TfKanji.SetFocusedChars(const Value: FString);
begin
  if FFocusedChars=Value then exit;
  SetFocusedChars(Value);
 //TODO: exit if the grid is not yet filled (we'll do the last part later)
  if not KanjiGridSetSelection(Value) then begin
 {$IFDEF AUTODEFOCUS}
    Self.KanjiGridSelectionChanged; //as if the user did that
 {$ENDIF}
  end;
end;



initialization
  calfonts:=TStringList.Create;
  ki:=TStringList.Create;
  curkanji:=UH_NOCHAR;

finalization
  ki.Free;
  calfonts.Free;

end.
