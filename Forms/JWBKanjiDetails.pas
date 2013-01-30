unit JWBKanjiDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, RXCtrls, rxPlacemnt, UrlLabel, JWBStrings,
  TextTable;

type
  TCharReadings = record
    piny,engy,kory,cany,chiny:string;
    ony,kuny,nany:widestring;
  end;
  PCharReadings = ^TCharReadings;

 {
  HellspawnString contains data in various formats depending on the circumstances:
   - for 'U' and 'P' type properties it stores FStrings
   - for the rest of types just strings (Ansi data in fact)
 }
  HellspawnString = string;
  THellspawnStringList = TStringList;

  TfKanjiDetails = class(TForm)
    lblMeaning: TLabel;
    RxLabel21: TRxLabel;
    ShapeKanji: TShape;
    ShapeRadical: TShape;
    ShapeSimplified: TShape;
    pbKanji: TPaintBox;
    pbRadical: TPaintBox;
    pbSimplified: TPaintBox;
    RxLabel10: TRxLabel;
    RxLabel35: TRxLabel;
    RxLabel38: TRxLabel;
    lblStrokeCount: TLabel;
    sbAddToGroup: TSpeedButton;
    RxLabel39: TRxLabel;
    ScrollBox1: TScrollBox;
    pbKanjiInfo: TPaintBox;
    Shape1: TShape;
    btnClose: TButton;
    FormPlacement1: TFormPlacement;
    ProUrlLabel1: TUrlLabel;
    ProUrlLabel2: TUrlLabel;
    ProUrlLabel3: TUrlLabel;
    ProUrlLabel4: TUrlLabel;
    ProUrlLabel5: TUrlLabel;
    lblRadicalNo: TLabel;
    btnDock: TButton;
    sbStrokeOrder: TSpeedButton;
    cbGroups: TComboBox;
    RxLabel1: TRxLabel;
    lblCategories: TLabel;
    procedure pbKanjiPaint(Sender: TObject);
    procedure pbRadicalPaint(Sender: TObject);
    procedure pbSimplifiedPaint(Sender: TObject);
    procedure sbAddToGroupClick(Sender: TObject);
    procedure SpeedButton28Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton23Click(Sender: TObject);
    procedure sbStrokeOrderClick(Sender: TObject);
    procedure pbKanjiInfoPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure pbRadicalDblClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseKeyPress(Sender: TObject; var Key: Char);
    procedure btnDockClick(Sender: TObject);
    procedure pbRadicalMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbKanjiInfoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbSimplifiedMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbRadicalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbSimplifiedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbKanjiInfoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbGroupsChange(Sender: TObject);
    procedure pbRadicalMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbSimplifiedMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbKanjiInfoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  protected
    curChar: FChar; //displaying information for this character
    kval: THellspawnStringList;
    procedure ReloadReadings(CChar: TTextTableCursor; out read: TCharReadings);
    procedure PopulateKval(CChar: TTextTableCursor; const read: TCharReadings);
  public
    procedure Clear;
    procedure SetCharDetails(unicode: FString);
    procedure Reload;


  protected
   { Info box painting }
    function InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
    procedure InfoDrawItem(canvas:TCanvas;its,txs:string;l,r:integer;
      var x,y,rh:integer;onlycount:boolean);
    procedure DrawSingleText(canvas:TCanvas;tp:char;l,t,r,fh:integer;s:string);
    function FitText(canvas:TCanvas;tp:char;wrap:boolean;w,fh:integer;
      fname:string;var l:integer;var s:string):string;

  end;

var
  fKanjiDetails: TfKanjiDetails;

  curradno: integer;
  curradical: string;

implementation

uses ShellApi, MemSource, JWBDicSearch, JWBKanji, JWBMenu, JWBTranslate,
  JWBSettings, JWBStrokeOrder, JWBUnit, JWBCategories;

{$R *.DFM}

var
  cursimple: FString;

procedure TfKanjiDetails.FormCreate(Sender: TObject);
begin
  kval:=TStringList.Create;
  cursimple:='';
end;

procedure TfKanjiDetails.FormDestroy(Sender: TObject);
begin
  kval.Free;
end;

procedure TfKanjiDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.btnKanjiDetails.Down:=false;
  fTranslate.sbDockKanjiDetails.Down:=false;
  fMenu.aKanjiDetails.Checked:=false;
  if not CharDetDocked then FormPlacement1.SaveFormPlacement;
end;

procedure TfKanjiDetails.FormShow(Sender: TObject);
begin
  if Visible then fMenu.aKanjiDetails.Checked:=true;
  if CharDetDocked then
    btnDock.Caption:=_l('#00172^eUndock')
  else
    btnDock.Caption:=_l('#00173^eDock');
  btnDock.Enabled:=CharDetDocked or (curdisplaymode=1) or (curdisplaymode=3) or
    (curdisplaymode=4);
  btnClose.Default:=not CharDetDocked;
end;

procedure TfKanjiDetails.FormHide(Sender: TObject);
begin
  fKanji.btnKanjiDetails.Down:=false;
  fMenu.aKanjiDetails.Checked:=false;
end;

procedure TfKanjiDetails.pbKanjiPaint(Sender: TObject);
var f:string;
begin
  if chin then
    case fSettings.RadioGroup5.ItemIndex of
      0:f:=FontChinese;
      1:f:=FontChineseGB;
      2:f:=FontRadical;
    end
  else f:=FontJapanese;
  if sbStrokeOrder.Down then
    if chin then f:=FontChinese else f:=FontStrokeOrder;
  pbKanji.Canvas.Brush.Color:=clWindow;
  pbKanji.Canvas.Font.Style:=[];
  DrawUnicode(pbKanji.Canvas,1,1,137,curChar,f);
  if sbStrokeOrder.Down then DrawStrokeOrder(pbKanji.Canvas,1,1,137,137,curChar,12,clBlue);
end;

procedure TfKanjiDetails.pbRadicalPaint(Sender: TObject);
var f:string;
begin
  f:=FontRadical;
  pbRadical.Canvas.Brush.Color:=clWindow;
  pbRadical.Canvas.Font.Style:=[];
  BeginDrawReg(pbRadical);
  DrawUnicode(pbRadical.Canvas,1,1,48,curradical,f);
  EndDrawReg;
end;

procedure TfKanjiDetails.pbSimplifiedPaint(Sender: TObject);
begin
  pbSimplified.Canvas.Brush.Color:=clWindow;
  pbSimplified.Canvas.Font.Style:=[];
  BeginDrawReg(pbSimplified);
  DrawUnicode(pbSimplified.Canvas,1,1,48,cursimple,FontRadical);
  EndDrawReg;
end;


procedure TfKanjiDetails.sbAddToGroupClick(Sender: TObject);
var catIndex: integer;
begin
  if curChar<>UH_NOCHAR then begin
    catIndex := GetSelCatIdx(cbGroups);
    SetKnown(catIndex, curChar, not IsKnown(catIndex,curChar));
  end;
  fMenu.ChangeUserData;
  SetCharDetails(curChar); //reload
end;

procedure TfKanjiDetails.SpeedButton28Click(Sender: TObject);
var s:string;
begin
  if curChar=UH_NOCHAR then exit;
  s:='StrokeOrder\'+FStrToHex(curChar)+'.gif';
  ShellExecute(handle,nil,pchar(s),nil,nil,SW_SHOW);
end;

procedure TfKanjiDetails.SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curChar;
  fMenu.ChangeClipboard;
end;

procedure TfKanjiDetails.sbStrokeOrderClick(Sender: TObject);
begin
  pbKanji.Invalidate;
end;

procedure TfKanjiDetails.pbKanjiInfoPaint(Sender: TObject);
begin
  BeginDrawReg(pbKanjiInfo);
  InfoPaint(pbKanjiInfo.Canvas,pbKanjiInfo.Width,false);
  EndDrawReg;
end;

procedure TfKanjiDetails.pbRadicalDblClick(Sender: TObject);
begin
  fKanji.SelRadical;
end;

procedure TfKanjiDetails.btnCloseClick(Sender: TObject);
begin
  if CharDetDocked then fMenu.aKanjiDetails.Execute else Close;
end;

procedure TfKanjiDetails.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then CLose;
end;

procedure TfKanjiDetails.btnCloseKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then CLose;
end;

procedure TfKanjiDetails.btnDockClick(Sender: TObject);
begin
  if CharDetDocked then
  begin
    CharDetDocked:=false;
    fMenu.ChangeDisplay;
    fMenu.aKanjiDetails.Execute;
  end else
  begin
    FormPlacement1.SaveFormPlacement;
    CharDetDocked:=true;
    CharDetDockedVis1:=true;
    CharDetDockedVis2:=true;
    fMenu.ChangeDisplay;
  end;
end;

procedure TfKanjiDetails.pbRadicalMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(pbRadical,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.pbKanjiInfoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(pbKanjiInfo,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.pbSimplifiedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(pbSimplified,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.pbRadicalMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.pbSimplifiedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.pbKanjiInfoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.cbGroupsChange(Sender: TObject);
begin
  fKanji.RefreshDetails;
end;

procedure TfKanjiDetails.pbRadicalMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.pbSimplifiedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.pbKanjiInfoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.Reload;
begin
  SetCharDetails(curChar);
end;

procedure TfKanjiDetails.Clear;
begin
  SetCharDetails('');
end;

{
"unicode" can be nil which means "Clear the form".
It can also be an unsupported character, then don't show any info except for char itself.
}
procedure TfKanjiDetails.SetCharDetails(unicode:FString);
var s: string;
  radf:integer;
  i:integer;
  kig:string;
  mf:TMemoryFile;
  ms:TMemoryStream;
//  ld:boolean;
  h:integer;
  cv:string;
  scat:string;
  cv_i1, cv_i2: integer;
  charval: string; //GetCharValue
  read: TCharReadings;
  curindex: integer;
  CChar: TTextTableCursor;
begin
  if flength(unicode)<1 then
    curChar := UH_NOCHAR
  else
    curChar := fgetch(unicode,1);

  curkanji:=UH_NOCHAR;
  curradical:='';
  cursimple:='';
  pbKanji.Invalidate;
  pbRadical.Invalidate;
  pbKanjiInfo.Invalidate;
  RxLabel35.Hide;
  pbSimplified.Hide;
  ShapeSimplified.Hide;
  lblMeaning.Caption:='-';

  CChar := TChar.NewCursor;
  try
    Screen.Cursor:=crHourGlass;
    if (unicode='') or not CChar.Locate('Unicode',unicode) then begin
     //unicode <> '' but not found in the char db -- valid caseS
      curindex:=-1;
      curkanji:=UH_NOCHAR;
    end else begin
      curindex:=CChar.Int(TCharIndex);
      curkanji:=CChar.FCh(TCharUnicode); //I'm not sure why we're doing that, "unicode" should be the same?
    end;

    { Url labels }
    if curindex<0 then begin
      ProURLLabel1.Enabled := false;
      ProURLLabel1.URL := '';
      ProURLLabel2.Enabled := false;
      ProURLLabel2.URL := '';
      ProURLLabel3.Enabled := false;
      ProURLLabel3.URL := '';
      ProURLLabel4.Enabled := false;
      ProURLLabel4.URL := '';
      ProURLLabel5.Enabled := false;
      ProURLLabel5.URL := '';
    end else begin
      cv:=fMenu.GetCharValue(curindex,51);
      ProURLLabel1.Enabled:=cv<>'';
      ProURLLabel1.URL:='http://www.zhongwen.com/cgi-bin/zipux2.cgi?b5=%'+copy(cv,1,2)+'%'+copy(cv,3,2);
      ProURLLabel2.Enabled:=CChar.Int(TCharChinese)=0;
      ProURLLabel2.URL:='http://www.csse.monash.edu.au/cgi-bin/cgiwrap/jwb/wwwjdic?1MKU'+lowercase(FStrToHex(curChar));
      ProURLLabel3.Enabled:=true;
      ProURLLabel3.URL:='http://charts.unicode.org/unihan/unihan.acgi$0x'+lowercase(FStrToHex(curChar));
      cv:=fMenu.GetCharValue(curindex,54);
      ProURLLabel4.Enabled:=(cv<>'')
        and TryStrToInt(copy(cv,1,2), cv_i1)
        and TryStrToInt(copy(cv,3,2), cv_i2);
      if ProURLLabel4.Enabled then
        ProURLLabel4.URL:='www.ocrat.com/chargif/GB/horiz/'+lowercase(Format('%2x%2x',[cv_i1+160,cv_i2+160]))+'.html';
      cv:=fMenu.GetCharValue(curindex,57);
      ProURLLabel5.Enabled:=cv<>'';
      ProURLLabel5.URL:='http://web.mit.edu/jpnet/ji/data/'+cv+'.html';
    end;

   { Stroke order }
   //TODO: Remove.
   { Not used. A remnant of the old way to display stroke order, through GIFS.
    fMenu.StrokeOrderPackage is always nil }
    {
    ld:=false;
    fStrokeOrder.TrackBar1.Max:=0;
    if (fMenu.StrokeOrderPackage<>nil) and (fMenu.GetCharValueInt(CChar.Int(TCharIndex),101)<65535) then
    begin
      try
        s:=fMenu.GetCharValue(CChar.Int(TCharIndex),101);
        while length(s)<4 do s:='0'+s;
        mf:=fMenu.StrokeOrderPackage.Files['so'+s+'.gif'];
        if mf<>nil then
        begin
          ms:=mf.Lock;
          fStrokeOrder.RxGIFAnimator1.Image.LoadFromStream(ms);
          ld:=true;
          fStrokeOrder.strokenum:=0;
          i:=0;
          fStrokeOrder.RxGIFAnimator1.FrameIndex:=1;
          while fStrokeOrder.RxGIFAnimator1.FrameIndex>i do
          begin
            inc(i);
            fStrokeOrder.RxGIFAnimator1.FrameIndex:=i+1;
          end;
          fStrokeOrder.strokenum:=i;
          fStrokeOrder.TrackBar1.Max:=i;
          fStrokeOrder.TrackBar1.Position:=i;
          fStrokeOrder.RxGIFAnimator1.FrameIndex:=i;
        end;
      except
      end;
    end;
    fStrokeOrder.RxGIFAnimator1.Visible:=ld;
    fStrokeOrder.Label1.Visible:=not ld;
    }

    if curindex<0 then
      charval := ''
    else begin
      charval := fMenu.GetCharValue(CChar.Int(TCharIndex),43);
      if charval<>'' then
        RxLabel35.Caption:=_l('#00135^eSimplified:')
      else begin
        charval := fMenu.GetCharValue(CChar.Int(TCharIndex),44);
        if charval<>'' then
          RxLabel35.Caption:=_l('#00136^eTraditional:');
      end;
    end;
    if charval<>'' then
    begin
      cursimple:=hextofstr(charval);
      RxLabel35.Show;
      pbSimplified.Show;
      ShapeSimplified.Show;
    end else begin
      cursimple:='';
      RxLabel35.Hide;
      pbSimplified.Hide;
      ShapeSimplified.Hide;
    end;

    if curindex<0 then
      curradno := 255
    else
      curradno:=fMenu.GetCharValueRad(CChar.Int(TCharIndex),
        fSettings.ComboBox1.ItemIndex+12 {Chosen radical to use});

    lblRadicalNo.Caption:=IntToStr(curradno);
    if curradno=255 then curradical:='' else
    begin
      if TRadicals.Locate('Number',curradno) then
        curradical:=TRadicals.Str(TRadicalsUnicode) else curradical:='';
    end;
  {  if chin then
      RxLabel21.Caption:=_l('#00137^eChar #')+inttostr(DrawGrid1.ColCount*Arow+Acol+1)
    else
      RxLabel21.Caption:='Kanji #'+inttostr(DrawGrid1.ColCount*Arow+Acol+1);}
    if not chin then
    begin
      if CChar.Int(TCharJouyouGrade)<9 then kig:='C'else
      if CChar.Int(TCharJouyouGrade)<10 then kig:='N'else
      kig:='U';
    end else
      if CChar.Int(TCharChFrequency)<=5 then kig:='C'else kig:='U';
    if IsKnown(KnownLearned,unicode) then kig:='K';
    if not IsKnown(GetSelCatIdx(cbGroups),unicode) then
      sbAddToGroup.Caption:='+'
    else
      sbAddToGroup.Caption:='-';
    if chin then begin
      if CChar.Int(TCharStrokeCount)<255 then lblStrokeCount.Caption:=CChar.Str(TCharStrokeCount) else lblStrokeCount.Caption:='-';
    end else begin
      if CChar.Int(TCharJpStrokeCount)<255 then lblStrokeCount.Caption:=CChar.Str(TCharJpStrokeCount) else lblStrokeCount.Caption:='-';
    end;
    scat:='';
    for i:=0 to Length(KanjiCats)-1 do
    begin
      if IsKnown(KanjiCats[i].idx,unicode) then
        if scat='' then
          scat:=KanjiCats[i].name
        else
          scat:=scat+', '+KanjiCats[i].name;
    end;
    if scat='' then scat:='-';
    lblCategories.Caption:=scat;
    case kig[1] of
      'K':RxLabel38.Font.Color:=Col('Kanji_Learned');
      'C':RxLabel38.Font.Color:=Col('Kanji_Common');
      'U':RxLabel38.Font.Color:=Col('Kanji_Rare');
      'N':RxLabel38.Font.Color:=Col('Kanji_Names');
    end;
    case kig[1] of
      'K':RxLabel38.Caption:=_l('#00140^eLearned');
      'C':RxLabel38.Caption:=_l('#00141^eCommon');
      'U':RxLabel38.Caption:=_l('#00142^eRare');
      'N':RxLabel38.Caption:=_l('#00143^eUsed in names');
      'A':RxLabel38.Caption:=_l('#00144^eJapanese and chinese');
      'J':RxLabel38.Caption:=_l('#00145^eJapanese only');
    end;

    if curindex<0 then
      lblMeaning.Caption := _l('#01001^eMultiple kanji selected')
    else begin
      ReloadReadings(CChar, read);
      if chin then
        lblMeaning.Caption:=read.chiny
      else
        lblMeaning.Caption:=read.engy;
      PopulateKVal(CChar, read);
    end;

  finally
    FreeAndNil(CChar);
  end;

 { Repaint }
  if curindex>0 then begin
    h:=InfoPaint(pbKanjiInfo.Canvas,pbKanjiInfo.Width,true);
    pbKanjiInfo.Height:=h;
  end else begin
    pbKanjiInfo.Height:=0;
  end;
  Screen.Cursor:=crDefault;
end;

{ Reloads various character readings from TCharRead table }
procedure TfKanjiDetails.ReloadReadings(CChar: TTextTableCursor; out read: TCharReadings);
var s: string;
  ws:widestring;
  adddot:integer;
  CCharRead: TTextTableCursor;
begin
  FillChar(read, sizeof(read), 00); //initializes all strings to ''

  CCharRead := TCharRead.NewCursor;
  try
    CCharRead.SetOrder('');
    CCharRead.Locate('Kanji',CChar.TrueInt(TCharIndex));
    while (not CCharRead.EOF) and (CCharRead.Int(TCharReadKanji)=CChar.Int(TCharIndex)) do
    begin
     //Readings are stored as STRINGS which contain HEX.
     //So there's nothing we can do to avoid HexToUnicode conversion.
      s:=CCharRead.Str(TCharReadReading);
      if (CCharRead.Int(TCharReadType)>3) and (CCharRead.Int(TCharReadType)<7) then
      begin
        ws:='';
        adddot:=0;
        if s[1]='+'then
        begin
          ws:=#$FF0B;
          delete(s,1,1);
          adddot:=1;
        end;
        if s[1]='-'then
        begin
          ws:=ws+#$FF0D;
          delete(s,1,1);
          adddot:=1;
        end;
        if CCharRead.Int(TCharReadReadDot)>0 then
        begin
          ws:=ws+HexToUnicode(copy(s,1,CCharRead.Int(TCharReadReadDot)-1-adddot));
          ws:=ws+#$FF0E;
          delete(s,1,CCharRead.Int(TCharReadReadDot)-1-adddot);
        end;
        if s[length(s)]='-'then
          ws:=ws+HexToUnicode(copy(s,1,length(s)-1))+#$FF0D
        else
          ws:=ws+HexToUnicode(s);
      end;
      case CCharRead.Int(TCharReadType) of
        1:if read.kory='' then read.kory:=s else read.kory:=read.kory+', '+s;
        2:if read.piny='' then read.piny:=s else read.piny:=read.piny+','+s;
        4:if read.ony='' then read.ony:=ws else read.ony:=read.ony+#$FF0C+ws;
        5:if read.kuny='' then read.kuny:=ws else read.kuny:=read.kuny+#$FF0C+ws;
        6:if read.nany='' then read.nany:=ws else read.nany:=read.nany+#$FF0C+ws;
        7:if read.chiny='' then read.chiny:=s else read.chiny:=read.chiny+', '+s;
        3:if read.engy='' then read.engy:=s else read.engy:=read.engy+', '+s;
        8:if read.cany='' then read.cany:=s else read.cany:=read.cany+', '+s;
      end;
      CCharRead.Next;
    end;
  finally
    FreeAndNil(CCharRead);
  end;
end;

{
PopulateKVal()
Repopulates KVal list which is used to draw various kanji information box
}
procedure TfKanjiDetails.PopulateKval(CChar: TTextTableCursor; const read: TCharReadings);
var
  s,s2: string;
  i,j,k:integer;
  charPropId: string;
  propType: string;
  CCharRead: TTextTableCursor;
begin
  kval.Clear;

  CCharRead := TCharRead.NewCursor;
  try
    for i:=0 to chardetl.Count-1 do
    begin
      charPropId := GetCharDet(i,0);
      j := FindCharPropType(charPropId);
      if j<0 then continue;

      k:=strtoint(charPropId);
      s:='';
      case k of
        0:s:='---';
        1:s:=LowerCase(read.kory);
        2:s:=ConvertPinYin(read.piny);
        3:s:=read.engy;
        4:s:=fstr(read.ony);
        5:s:=fstr(read.kuny);
        6:s:=fstr(read.nany);
        7:s:=read.chiny;
        8:s:=LowerCase(read.cany);
        100:s:=curChar;
        else begin
          CCharRead.SetOrder('');
          CCharRead.Locate('Kanji',CChar.TrueInt(TCharIndex));
          while (not CCharRead.EOF) and (CCharRead.Int(TCharReadKanji)=CChar.Int(TCharIndex)) do
          begin
            if CCharRead.Int(TCharReadType)=k then
            begin
              propType := GetCharPropType(j,3);
             { Different property types have data in different formats! }
              if propType='R' then
              begin
                s2:=CCharRead.Str(TCharReadReading);
                if (length(s2)>0) and (s2[1]='''') then delete(s2,1,1);
                if (length(s2)>0) and (s2[length(s2)]='''') then delete(s2,length(s2),1);
                TRadicals.Locate('Number',strtoint(s2));
                s:=s+TRadicals.Str(TRadicalsUnicode);
              end else
             { 'U' and 'P' have reading in 'a'-type hex }
              if (propType='U') or (propType='P') then
              begin
                if (propType<>'U') and (s<>'') then s:=s+', ';
                s:=s+CCharRead.Dehex(TCharReadReading);
              end else
             { Rest is read as it is }
              begin
                s:=s+CCharRead.Str(TCharReadReading);
              end;
            end;
            CCharRead.Next;
          end;
        end;
      end;

      if GetCharDet(i,6)<>'' then
        kval.Add(GetCharPropType(j,3)+';'+chardetl[i])
      else
        kval.Add(GetCharPropType(j,3)+';'+chardetl[i]+_l('^e'+GetCharPropType(j,4)));
      kval.Add(s);

    end; //of chardetl enum
  finally
    FreeAndNil(CCharRead);
  end;
end;


{ Info box painting }

function TfKanjiDetails.InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
var i:integer;
    x,y,rh:integer;
begin
  x:=6;
  y:=3;
  rh:=0;
  for i:=0 to (kval.Count div 2)-1 do
    InfoDrawItem(canvas,kval[i*2],kval[i*2+1],6,w-2,x,y,rh,onlycount);
  result:=y;
end;

procedure TfKanjiDetails.InfoDrawItem(canvas:TCanvas;its,txs:string;l,r:integer;
  var x,y,rh:integer;onlycount:boolean);
var fh:integer;
    fname:string;
    lbl:string;

  function DoesFit(s:string):boolean;
  var l:integer;
  begin
    FitText(canvas,its[1],true,r-x,fh,fname,l,s);
    if s<>'' then result:=false else result:=true;
  end;
  function GetDet(j:integer):string;
  var s:string;
  begin
    s:=its;
    while j>0 do
    begin
      delete(s,1,pos(';',s));
      dec(j);
    end;
    delete(s,pos(';',s),length(s)-pos(';',s)+1);
    result:=s;
  end;

var lw,rr:integer;
    ws:string;
    s:string;
begin
  if (GetDet(4)='C') and not chin then exit;
  if (GetDet(4)='J') and chin then exit;
  if (GetDet(5)='N') and (txs='') and (its[1]<>'-') then exit;
  if its[1]='R'then its[1]:='U';
  if txs='' then if (its[1]<>'U') and (its[1]<>'R') and (its[1]<>'P') then txs:='-';
  canvas.Font.Style:=[fsBold];
  case GetDet(6)[1] of
    'B':fh:=20;
    'M':fh:=16;
    'S':fh:=12;
  end;
  fname:=FontEnglish;
  if GetDet(3)<>'N'then
  begin
    lbl:=GetDet(7);
    if its[1]<>'-'then lbl:=lbl+':';
    if (GetDet(2)='C') and (x>l) then
    begin
      if not DoesFit(lbl) then
      begin
        x:=l;
        y:=y+rh;
      end;
    end else if x>l then
    begin
      x:=l;
      y:=y+rh;
    end;
    lbl:=FitText(canvas,'S',false,r-x,fh,fname,lw,lbl);
    if not onlycount then if txs<>'---'then DrawSingleText(canvas,'S',x,y,r,fh,lbl);
    lw:=lw+5;
    canvas.Font.Style:=[];
    if (GetDet(3)='W') or ((GetDet(3)='L') and (GetDet(2)='W') and (lw>(r-x) div 2)) then
    begin
      x:=l;
      y:=y+fh+2;
    end else x:=x+lw;
  end;
  canvas.Font.Style:=[];
    if (its[1]='U') or (its[1]='R') then fname:=FontSmall else fname:=FontEnglish;
  rh:=fh+2;
  rr:=r;
  if (GetDet(2)='C') and (x<r div 2) then rr:=(r div 2)-5;
  if (GetDet(2)='W') and (txs<>'---') then
  begin
    s:=txs;
    while s<>'' do
    begin
      ws:=FitText(canvas,its[1],true,r-x,fh,fname,lw,s);
      if not onlycount then if txs<>'---'then DrawSingleText(canvas,its[1],x,y,r,fh,ws);
      x:=l;
      inc(y,rh);
    end;
  end else
  begin
    s:=txs;
    ws:=FitText(canvas,its[1],false,rr-x,fh,fname,lw,s);
    if not onlycount then if txs<>'---'then DrawSingleText(canvas,its[1],x,y,rr,fh,ws);
    if (not onlycount) and (txs='---') then
    begin
      canvas.MoveTo(l,y+7);
      canvas.LineTo(r,y+7);
    end;
    if rr=r then
    begin
      x:=l;
      inc(y,rh);
    end else x:=rr+5;
  end;
end;

procedure TfKanjiDetails.DrawSingleText(canvas:TCanvas;tp:char;l,t,r,fh:integer;s:string);
var font:string;
begin
  if chin then font:=FontRadical else font:=FontSmall;
  if tp='P'then font:=FontEnglish;
  if (tp='U') or (tp='P') then DrawUnicode(canvas,l,t,fh-2,s,font) else
  if (tp='N') or (tp='T') then canvas.TextOut(r-canvas.TextExtent(s).cx,t,s) else
    canvas.TextOut(l,t,s);
end;

function TfKanjiDetails.FitText(canvas:TCanvas;tp:char;wrap:boolean;w,fh:integer;
  fname:string;var l:integer;var s:string):string;

  function countwidth(tp:char;fh:integer;s:string):integer;
  var ts:TSize;
  begin
    if tp='P'then result:=0 else
    if (tp='U') then result:=flength(s)*(fh-2)
    else begin
      ts:=canvas.TextExtent(s);
      result:=ts.cx;
    end;
  end;

var st,stl:string;
    i,ii,iii:integer;
begin
  canvas.Font.Name:=fname;
  canvas.Font.Height:=fh;
  if countwidth(tp,fh,s)<=w then
  begin
    l:=countwidth(tp,fh,s);
    result:=s;
    s:='';
    exit;
  end;
  i:=0;
  stl:='';
  st:='';
  ii:=0;
  repeat
    stl:=st;
    iii:=ii;
    ii:=i;
    if (tp='U') or (tp='P') then st:=fcopy(s,1,i) else st:=copy(s,1,i);
    if not wrap then
      if (tp='U') or (tp='P') then st:=st+UH_ELLIPSIS else st:=st+'...';
    if (tp='U') or (tp='P') or (not wrap) then inc(i) else
    begin
      inc(i);
      while (i<length(s)) and (s[i+1]<>' ') do inc(i);
    end;
  until countwidth(tp,fh,st)>w;
  if stl='' then stl:=st;
  i:=iii;
  if i<0 then i:=0;
  if wrap then
  begin
    result:=stl;
    if (tp='U') or (tp='P') then fdelete(s,1,i) else delete(s,1,i);
    l:=countwidth(tp,fh,stl);
  end else
  begin
    result:=stl;
    s:='';
    l:=countwidth(tp,fh,stl);
  end;
end;

initialization
  curradical:='';

end.
