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
    Shape2: TShape;
    Label1: TLabel;
    RxLabel21: TRxLabel;
    Shape8: TShape;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Shape10: TShape;
    PaintBox4: TPaintBox;
    RxLabel10: TRxLabel;
    RxLabel35: TRxLabel;
    RxLabel38: TRxLabel;
    Label9: TLabel;    
    SpeedButton21: TSpeedButton;
    RxLabel39: TRxLabel;
    ScrollBox1: TScrollBox;
    PaintBox3: TPaintBox;
    Shape1: TShape;
    Button1: TButton;
    FormPlacement1: TFormPlacement;
    ProUrlLabel1: TUrlLabel;
    ProUrlLabel2: TUrlLabel;
    ProUrlLabel3: TUrlLabel;
    ProUrlLabel4: TUrlLabel;
    ProUrlLabel5: TUrlLabel;
    Label2: TLabel;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    ComboBox1: TComboBox;
    RxLabel1: TRxLabel;
    Label3: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox4Paint(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton28Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PaintBox2DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button1KeyPress(Sender: TObject; var Key: Char);
    procedure Button2Click(Sender: TObject);
    procedure PaintBox2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  protected
    curChar: FChar; //displaying information for this character
    kval: THellspawnStringList;
    procedure ReloadReadings(CChar: TTextTableCursor; out read: TCharReadings);
    procedure PopulateKval(CChar: TTextTableCursor; const read: TCharReadings);
  public
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

  curradno: string;
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

procedure TfKanjiDetails.PaintBox1Paint(Sender: TObject);
var f:string;
begin
  if chin then
  case fSettings.RadioGroup5.ItemIndex of
    0:f:=FontChinese;
    1:f:=FontChineseGB;
    2:f:=FontRadical;
  end else f:=FontJapanese;
  if (fKanjiDetails.SpeedButton1.Down) then
    if chin then f:=FontChinese else f:=FontStrokeOrder;
  fKanjiDetails.PaintBox1.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox1.Canvas.Font.Style:=[];
  DrawUnicode(fKanjiDetails.PaintBox1.Canvas,1,1,137,JWBKanji.curkanji,f);
  if fKanjiDetails.SpeedButton1.Down then DrawStrokeOrder(fKanjiDetails.PaintBox1.Canvas,1,1,137,137,curkanji,12,clBlue);
end;

procedure TfKanjiDetails.PaintBox2Paint(Sender: TObject);
var f:string;
begin
  f:=FontRadical;
  fKanjiDetails.PaintBox2.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox2.Canvas.Font.Style:=[];
  BeginDrawReg(fKanjiDetails.PaintBox2);
  DrawUnicode(fKanjiDetails.PaintBox2.Canvas,1,1,48,curradical,f);
  EndDrawReg;
end;

procedure TfKanjiDetails.PaintBox4Paint(Sender: TObject);
begin
  fKanjiDetails.PaintBox4.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox4.Canvas.Font.Style:=[];
  BeginDrawReg(fKanjiDetails.PaintBox4);
  DrawUnicode(fKanjiDetails.PaintBox4.Canvas,1,1,48,cursimple,FontRadical);
  EndDrawReg;
end;


procedure TfKanjiDetails.SpeedButton21Click(Sender: TObject);
var catIndex: integer;
begin
  if curkanji<>'' then begin
    catIndex := GetSelCatIdx(fKanjiDetails.ComboBox1);
    SetKnown(catIndex, curkanji, not IsKnown(catIndex,curkanji));
  end;
  fMenu.ChangeUserData;
  fKanjiDetails.SetCharDetails(curkanji);
end;

procedure TfKanjiDetails.SpeedButton28Click(Sender: TObject);
var s:string;
begin
  s:='StrokeOrder\'+curkanji+'.gif';
  ShellExecute(handle,nil,pchar(s),nil,nil,SW_SHOW);
end;

procedure TfKanjiDetails.SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curkanji;
  fMenu.ChangeClipboard;
end;

procedure TfKanjiDetails.SpeedButton1Click(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TfKanjiDetails.PaintBox3Paint(Sender: TObject);
begin
  BeginDrawReg(PaintBox3);
  InfoPaint(PaintBox3.Canvas,PaintBox3.Width,false);
  EndDrawReg;
end;

procedure TfKanjiDetails.FormShow(Sender: TObject);
begin
  if Visible then fMenu.aKanjiDetails.Checked:=true;
  if CharDetDocked then Button2.Caption:=_l('#00172^eUndock') else
    Button2.Caption:=_l('#00173^eDock');
  Button2.Enabled:=CharDetDocked or (curdisplaymode=1) or (curdisplaymode=3) or
    (curdisplaymode=4);
  Button1.Default:=not CharDetDocked;
end;

procedure TfKanjiDetails.FormHide(Sender: TObject);
begin
  fKanji.btnKanjiDetails.Down:=false;
  fMenu.aKanjiDetails.Checked:=false;
end;

procedure TfKanjiDetails.PaintBox2DblClick(Sender: TObject);
begin
  fKanji.SelRadical;
end;

procedure TfKanjiDetails.Button1Click(Sender: TObject);
begin
  if CharDetDocked then fMenu.aKanjiDetails.Execute else Close;
end;

procedure TfKanjiDetails.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then CLose;
end;

procedure TfKanjiDetails.Button1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then CLose;
end;

procedure TfKanjiDetails.Button2Click(Sender: TObject);
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

procedure TfKanjiDetails.PaintBox2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox2,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.PaintBox3MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.PaintBox4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox4,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.PaintBox2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.PaintBox4MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.PaintBox3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.ComboBox1Change(Sender: TObject);
begin
  fKanji.RefreshDetails;
end;

procedure TfKanjiDetails.PaintBox2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.PaintBox4MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.PaintBox3MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.Reload;
begin
  SetCharDetails(curChar);
end;

procedure TfKanjiDetails.SetCharDetails(unicode:FString);
var s: string;
  radf:integer;
  i:integer;
  kix:FString;
  kig:string;
  mf:TMemoryFile;
  ms:TMemoryStream;
  ld:boolean;
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
  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
  PaintBox3.Invalidate;
  RxLabel35.Hide;
  PaintBox4.Hide;
  Shape10.Hide;
  Label1.Caption:='-';
  kix:=unicode;

  CChar := TChar.NewCursor;
  try
    if not CChar.Locate('Unicode',kix) then exit;
    Screen.Cursor:=crHourGlass;
    curkanji:=CChar.FCh(TCharUnicode);
    curindex:=CChar.Int(TCharIndex);
    ld:=false;
    cv:=fMenu.GetCharValue(curindex,51);

    { Url labels }
    ProURLLabel1.Enabled:=cv<>'';
    ProURLLabel1.URL:='http://www.zhongwen.com/cgi-bin/zipux2.cgi?b5=%'+copy(cv,1,2)+'%'+copy(cv,3,2);
    ProURLLabel2.Enabled:=CChar.Int(TCharChinese)=0;
    ProURLLabel2.URL:='http://www.csse.monash.edu.au/cgi-bin/cgiwrap/jwb/wwwjdic?1MKU'+lowercase(curKanji);
    ProURLLabel3.Enabled:=true;
    ProURLLabel3.URL:='http://charts.unicode.org/unihan/unihan.acgi$0x'+lowercase(curKanji);
    cv:=fMenu.GetCharValue(curindex,54);
    ProURLLabel4.Enabled:=(cv<>'')
      and TryStrToInt(copy(cv,1,2), cv_i1)
      and TryStrToInt(copy(cv,3,2), cv_i2);
    if ProURLLabel4.Enabled then
      ProURLLabel4.URL:='www.ocrat.com/chargif/GB/horiz/'+lowercase(Format('%2x%2x',[cv_i1+160,cv_i2+160]))+'.html';
    cv:=fMenu.GetCharValue(curindex,57);
    ProURLLabel5.Enabled:=cv<>'';
    ProURLLabel5.URL:='http://web.mit.edu/jpnet/ji/data/'+cv+'.html';

   { Stroke order }
   //TODO: Remove.
   { Not used. A remnant of the old way to display stroke order, through GIFS.
    fMenu.StrokeOrderPackage is always nil }
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
    radf:=fSettings.ComboBox1.ItemIndex+12;

    charval := fMenu.GetCharValue(CChar.Int(TCharIndex),43);
    if charval<>'' then
    begin
      cursimple:=hextofstr(charval);
      RxLabel35.Caption:=_l('#00135^eSimplified:');
      RxLabel35.Show;
      PaintBox4.Show;
      Shape10.Show;
    end else begin
      charval := fMenu.GetCharValue(CChar.Int(TCharIndex),44);
      if charval<>'' then
      begin
        cursimple:=hextofstr(charval);
        RxLabel35.Caption:=_l('#00136^eTraditional:');
        RxLabel35.Show;
        PaintBox4.Show;
        Shape10.Show;
      end else
      begin
        cursimple:='';
        RxLabel35.Hide;
        PaintBox4.Hide;
        Shape10.Hide;
      end;
    end;

    i:=fMenu.GetCharValueRad(CChar.Int(TCharIndex),radf);
    curradno:=inttostr(i);
    Label2.Caption:=curradno;
    if i=255 then curradical:='' else
    begin
      if TRadicals.Locate('Number',i) then
        curradical:=TRadicals.Str(TRadicalsUnicode) else curradical:='';
    end;
  {  if chin then
      RxLabel21.Caption:=_l('#00137^eChar #')+inttostr(DrawGrid1.ColCount*Arow+Acol+1) else
      RxLabel21.Caption:='Kanji #'+inttostr(DrawGrid1.ColCount*Arow+Acol+1);}
    if not chin then
    begin
      if CChar.Int(TCharJouyouGrade)<9 then kig:='C'else
      if CChar.Int(TCharJouyouGrade)<10 then kig:='N'else
      kig:='U';
    end else
      if CChar.Int(TCharChFrequency)<=5 then kig:='C'else kig:='U';
    if IsKnown(KnownLearned,kix) then kig:='K';
    if not IsKnown(GetSelCatIdx(Combobox1),kix) then
      SpeedButton21.Caption:='+'
    else
      SpeedButton21.Caption:='-';
    if chin then if CChar.Int(TCharStrokeCount)<255 then Label9.Caption:=CChar.Str(TCharStrokeCount) else Label9.Caption:='-';
    if not chin then if CChar.Int(TCharJpStrokeCount)<255 then Label9.Caption:=CChar.Str(TCharJpStrokeCount) else Label9.Caption:='-';
    scat:='';
    for i:=0 to Length(KanjiCats)-1 do
    begin
      if IsKnown(KanjiCats[i].idx,kix) then
        if scat='' then
          scat:=KanjiCats[i].name
        else
          scat:=scat+', '+KanjiCats[i].name;
    end;
    if scat='' then scat:='-';
    Label3.Caption:=scat;
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

    ReloadReadings(CChar, read);

    if chin then
      Label1.Caption:=read.chiny
    else
      Label1.Caption:=read.engy;

    PopulateKVal(CChar, read);
  finally
    FreeAndNil(CChar);
  end;

 { Repaint }
  h:=InfoPaint(PaintBox3.Canvas,PaintBox3.Width,true);
  PaintBox3.Height:=h;
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
        100:s:=curkanji;
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
