unit JWBWordGrid;

interface
uses SysUtils, Classes, Graphics, Grids, Windows, JWBStrings;

procedure SplitWord(s: string; out AKanji, AKana, ABody, ARest: string);

function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer;
  s:string; multiline,onlycount:boolean;fontsize:integer;boldfont:boolean):integer;
procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:FString; ch:integer;
  boldfont:boolean);

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
procedure AddWordGrid(grid:TStringGrid;sp1,sp2,sp4,sp3:string);
procedure FinishWordGrid(grid:TStringGrid);
procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);

implementation
uses Messages, Controls, JWBLanguage, JWBUnit, JWBLegacyMarkup, JWBSettings,
  KanaConv, JWBDrawText, JWBIntTip, AnnotationsSettings;

//Splits translation record in old Wakan format into parts:
//  kanji [kana] {translation} rest
procedure SplitWord(s: string; out AKanji, AKana, ABody, ARest: string);
begin
  AKanji:='';
  AKana:='';
  ARest:='';
  ABody:='';
  while s[1]<>' 'do
  begin
    AKanji:=AKanji+s[1];
    delete(s,1,1);
  end;
  delete(s,1,1);
  if s[1]='['then
  begin
    delete(s,1,1);
    while s[1]<>']'do
    begin
      AKana:=AKana+s[1];
      delete(s,1,1);
    end;
    delete(s,1,2);
  end else AKana:=AKanji;
  delete(s,1,1);
  ARest:=s;
  ABody:=copy(s,1,pos('}',s)-1);
  delete(ARest,1,length(ABody)+1);
end;


//NOTE: If you update fonts here, update DrawGridUpdateSelection() too.
function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean; fontsize:integer; boldfont:boolean):integer;
var x:integer;
    inmar,resinmar:boolean;
    curs:string;
    rect2:TRect;
    c:char;
    cursiv:boolean;
    w:integer;
    y:integer;
    sbef:string;
    fontcolor:TColor;
begin
  if multiline then result:=1 else result:=0;
  Canvas.Brush.Color:=clWindow;
  Canvas.Font.Color:=clWindowText;
  Canvas.Font.Name:=FontEnglish;
  Canvas.Font.Style:=[];
  Canvas.Font.Size:=9;
  if (fSettings.cbStatusColors.Checked) and (not fSettings.cbNoWordGridColors.Checked) and (not titrow) then
  begin
    c:=' ';
    if (length(s)>1) and (s[1]=ALTCH_EXCL) then c:=s[2];
    if (length(s)>2) and (s[2]=ALTCH_EXCL) then c:=s[3];
    case c of
      ' ':if sel then Canvas.Brush.Color:=Col('Dict_SelBack') else Canvas.Brush.Color:=Col('Dict_Back');
      '0':if sel then Canvas.Brush.Color:=Col('Dict_SelProblematic') else Canvas.Brush.Color:=Col('Dict_Problematic');
      '1':if sel then Canvas.Brush.Color:=Col('Dict_SelUnlearned') else Canvas.Brush.Color:=Col('Dict_Unlearned');
      '2':if sel then Canvas.Brush.Color:=Col('Dict_SelLearned') else Canvas.Brush.Color:=Col('Dict_Learned');
      '3':if sel then Canvas.Brush.Color:=Col('Dict_SelMastered') else Canvas.Brush.Color:=Col('Dict_Mastered');
    end;
  end;
  if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
  if (length(s)>2) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
  if (length(s)>0) and (Colx=0) and (s[1]=UH_DRAWWORD_KANA) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    DrawKana(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall,showroma,curlang);
  end else
  if (length(s)>0) and (s[1]=UH_DRAWWORD_KANJI) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    if (Length(s)>0) and (s[1]=UH_UNKNOWN_KANJI) then
    begin
      if (fSettings.CheckBox10.Checked) then Canvas.Font.Color:=Col('Dict_UnknownChar') else Canvas.Font.Color:=Col('Dict_Text');
      delete(s,1,1);
    end
    else Canvas.Font.Color:=Col('Dict_Text');
    if fSettings.cbNoWordGridColors.Checked then Canvas.Font.Color:=clWindowText;
    DrawUnicode(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall);
  end else if not titrow then
  begin
    cursiv:=false;
    FontColor:=Col('Dict_Text');
    if fSettings.cbNoWordGridColors.Checked then FontColor:=clWindowText;
    if (length(s)>1) and (s[1]=UH_WORDTYPE) then
    begin
      if s[2]='I'then cursiv:=true;
//      if not fUser.CheckBox1.Checked then cursiv:=false;
      delete(s,1,2);
    end;
    if (length(s)>1) and (s[1]=UH_SETCOLOR) then
    begin
      if (AnnotationsSettingsPage.cbAnnotateWithColors.Checked) then
        if not TryStrToInt('0x'+copy(s,6,2)+copy(s,4,2)+copy(s,2,2), integer(FontColor)) then
          FontColor:=clWindowText;
      delete(s,1,7);
    end;
    if not onlycount then Canvas.FillRect(Rect);
    inmar:=false;
    x:=0;
    y:=0;
    sbef:='';
    while length(s)>0 do
    begin
//      if sbef=s then
//      begin
//        showmessage(sbef);
//      end;
      sbef:=s;
      if inmar then
        if pos(UH_LEND,s)>0 then curs:=copy(s,1,pos(UH_LEND,s)-1) else curs:=s;
      if not inmar then
        if pos(UH_LBEG,s)>0 then curs:=copy(s,1,pos(UH_LBEG,s)-1) else curs:=s;
      delete(s,1,length(curs));
      if (length(s)>0) and ((s[1]=UH_LBEG) or (s[1]=UH_LEND)) then delete(s,1,1);
      rect2:=rect;
      rect2.Left:=rect.left+x+2;
      rect2.Top:=rect.top+y;
      if x<rect.right-rect.left then
      begin
        if inmar then
        begin
          c:=curs[1];
          delete(curs,1,1);
          if fSettings.cbNoWordGridColors.Checked then
            Canvas.Font.Color:=FontColor
          else
          case c of
            '1':Canvas.Font.Color:=Col('Mark_Special');
            's':Canvas.Font.Color:=Col('Mark_Usage');
            'g':Canvas.Font.Color:=Col('Mark_Grammatical');
            'd':Canvas.Font.Color:=Col('Mark_Dict');
            'l':Canvas.Font.Color:=Col('Mark_Lesson');
          end;
          Canvas.Font.Height:=FontSize-3;
          Canvas.Font.Style:=[fsItalic];
        end else
        begin
          Canvas.Font.Color:=FontColor;
          Canvas.Font.Height:=FontSize;
          Canvas.Font.Style:=[];
          if boldfont then
            if cursiv then Canvas.Font.Style:=[fsItalic,fsBold] else
              Canvas.Font.Style:=[fsBold];
          if Colx=3 then Canvas.Font.Style:=[];
        end;
        w:=Canvas.TextExtent(curs).cx;
        if not multiline then result:=result+w;
        resinmar:=false;
        if (multiline) and (rect.left+2+x+w>rect.right) then
        begin
          if (length(curs)>0) and (curs[1]=' ') then curs[1]:=UH_WORDTYPE;
          if inmar or (pos(' ',curs)=0) or (Canvas.TextExtent(copy(curs,1,pos(' ',curs)-1)).cx+rect.left+2+x>rect.right) then
          begin
            if (length(curs)>0) and (curs[1]=UH_WORDTYPE) then curs[1]:=' ';
            x:=0;
            y:=y+FontSize+2;
            rect2.left:=rect.left+2;
            rect2.top:=rect.top+2+y;
            result:=result+1;
          end else
          if not inmar and (pos(' ',curs)>0) then
          begin
            if (length(curs)>0) and (curs[1]=' ') then curs[1]:=UH_WORDTYPE;
            s:=copy(curs,pos(' ',curs),length(curs)-pos(' ',curs)+1)+UH_LBEG+s;
            curs:=copy(curs,1,pos(' ',curs)-1);
            if (length(curs)>0) and (curs[1]=UH_WORDTYPE) then curs[1]:=' ';
            resinmar:=true;
          end else
          begin
            curs:=s;
            if (length(curs)>0) and (curs[1]=UH_WORDTYPE) then curs[1]:=' ';
            s:='';
          end;
        end;
        if not onlycount then
        begin
          if inmar then
            Canvas.TextRect(Rect2,Rect.Left+2+x,Rect.Top+5+y,curs) else
            Canvas.TextRect(Rect2,Rect.Left+2+x,Rect.Top+1+y,curs);
        end;
        x:=x+Canvas.TextExtent(curs).cx;
        if not resinmar then inmar:=not inmar;
      end else s:='';
    end;
  end else
  begin
    Canvas.Font.Style:=[fsBold];
    Canvas.Font.Size:=8;
    Canvas.FillRect(Rect);
    Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,s);
  end;
  Canvas.Font.Color:=clWindowText;
  Canvas.Font.Name:=FontEnglish;
  Canvas.Font.Style:=[];
  Canvas.Font.Size:=9;
end;

procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var s:string;
    gr:integer;
    rect2:TRect;
begin
  s:=Grid.Cells[ACol,ARow];
  rect2:=rect;
  rect2.bottom:=1000;
  if fSettings.cbMultilineGrids.Checked and (ACol=2) and (ARow>0) then
  begin
    gr:=(2+GridFontSize)*DrawWordInfo(Grid.Canvas, Rect2, gdSelected in State, ARow=0, ACol, s, true, true, GridFontSize,true);
    if grid.rowheights[arow]<>gr then begin grid.rowheights[arow]:=gr; exit; end;
  end;
  DrawWordInfo(Grid.Canvas, Rect, gdSelected in State, ARow=0, ACol, s, true, false, GridFontSize,true);
  PaintSelectionHighlight(Grid.Canvas,@rect);
end;

procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:FString; ch:integer;boldfont:boolean);
var s1,sx1,s2,s3,s4:FString;
begin
  SplitWord(s,s1,s2,s3,s4);
  if curlang='c'then
  begin
    if s2[1]=ALTCH_EXCL then delete(s2,1,2);
    s2:=KanaToRomajiF(s2,curlang);
    sx1:=s1;
    s1:=s1+UH_SPACE+s2;
    DrawWordInfo(Canvas,rect,false,false,0,UH_DRAWWORD_KANJI+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(sx1)*ch+ch+(flength(s2) div 2)*ch;
  end else
  begin
    DrawWordInfo(Canvas,rect,false,false,0,UH_DRAWWORD_KANJI+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(remexcl(s1))*ch;
  end;
  if (s2<>s1) and (curlang='j') then
  begin
    if s2[1]=ALTCH_EXCL then s2:=ALTCH_EXCL+s1[2]+UH_UNKNOWN_KANJI+copy(s2,3,length(s2)-2) else s2:=UH_UNKNOWN_KANJI+s2;
    DrawWordInfo(Canvas,rect,false,false,1,UH_DRAWWORD_KANJI+s2,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(remexcl(s2))*ch;
  end;
  DrawWordInfo(Canvas,rect,false,false,2,s3,false,false,ch-3,boldfont);
end;

var wgcur:integer;

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
begin
  grid.Perform(WM_SETREDRAW, 0, 0);
  grid.DefaultRowHeight:=GridFontSize+2;
  grid.FixedRows:=1;
  grid.Cells[0,0]:=_l('#00939^ePhonetic');
  grid.Cells[1,0]:=_l('#00632^eWritten');
  grid.Cells[2,0]:=_l('#00317^eTranslation');
  if stat then if learn then
    grid.Cells[3,0]:=_l('#00633^eAdded / Learned') else
    grid.Cells[3,0]:=_l('#00634^eCategories');
  wgcur:=1;
end;

procedure AddWordGrid(grid:TStringGrid;sp1,sp2,sp4,sp3:string);
begin
  grid.Cells[0,wgcur]:=UH_DRAWWORD_KANA+sp2;
  grid.Cells[1,wgcur]:=UH_DRAWWORD_KANJI+sp1;
  grid.Cells[2,wgcur]:=sp4;
  if sp3<>'' then grid.Cells[3,wgcur]:=sp3;
  inc(wgcur);
end;

procedure FinishWordGrid(grid:TStringGrid);
begin
  grid.Perform(WM_SETREDRAW, 1, 0);
  if wgcur=1 then begin
  { Careful! WM_SETREDRAW(1) causes control to become functionally visible
   even with Visible set to false.
   The only way to fix this for sure is to Show() and Hide() it again.
   So try to avoid InitWordGrid/FinishWordGrid: if you have no items, just hide it. }
    grid.Show;
    grid.Hide
  end else begin
    grid.RowCount:=wgcur;
    if not grid.Visible then
      grid.Show;
  end;
  grid.Invalidate;
end;

procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
var i:integer;
    s,sp1,sp2,sp3,sp4:string;
begin
  if sl.Count=0 then
  begin
    grid.Show;
    grid.Hide;
    exit;
  end;
  InitWordGrid(grid,stat,learn);
  for i:=0 to sl.Count-1 do
  begin
    s:=sl[i];
    SplitWord(s,sp1,sp2,sp4,sp3);
    AddWordGrid(grid,sp1,sp2,sp4,sp3);
  end;
  if fSettings.cbMultilineGrids.Checked then
    for i:=1 to grid.RowCount-1 do
      grid.RowHeights[i]:=(GridFontSize+2)*DrawWordInfo(grid.Canvas,grid.CellRect(2,i),false,false,2,grid.Cells[2,i],true,true,GridFontSize,true);
  FinishWordGrid(grid);
end;


//NOTE: Fonts here must match DrawWordInfo() and DrawKana() choices for each cell.
function DrawGridHighlightContent(Control: TControl; DragStart, MousePos: TPoint): string;
var p: TStringGrid;
  gc,gc2:TGridCoord;
  rect:TRect;
  mox1,mox2:integer;
  text:FString;
  FontName:string;
  FontSize:integer;
begin
  p := TStringGrid(Control);
  gc:=p.MouseCoord(DragStart.x,DragStart.y);

  if (gc.x<0) or (gc.x>=2) or (gc.y<=0) then begin
   //Drag from header or drag from no-cell
    Result:='';
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

 //Select font name and actual text which was presented (differs from internal presentation sometimes)
 //This is dirty! We have to remember text/font/size which were used for drawing.
  case gc.x of
    0: begin //kana/romaji
      Result:=ConvertKana(remexcl(p.Cells[gc.x,gc.y]));
      if showroma then begin
        FontName:=FontPinYin; //DrawKana draws all romaji with this one
        FontSize:=GridFontSize+1;
      end else begin
        FontName:=FontSmall;
        FontSize:=GridFontSize;
      end;
    end;
    1: begin //kanji
      Result := remexcl(p.Cells[gc.x,gc.y]);
      FontName:=FontSmall;
      FontSize:=GridFontSize;
    end
  else //not selectable
    Result:='';
    FontName:=FontEnglish;
    FontSize:=GridFontSize;
  end;

  rect:=p.CellRect(gc.x,gc.y);
  if (DragStart.X=MousePos.X) and (DragStart.Y=MousePos.Y) then begin
   //No drag, mouse over
    fdelete(Result,1,((MousePos.x-rect.left-2) div GridFontSize));
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  gc2:=p.MouseCoord(MousePos.x,MousePos.y);
  if (gc2.x<>gc.x) or (gc2.y<>gc.y) then begin //mouse over different control/line
   //Try again, with Y set to that of DragStart
    MousePos.Y := DragStart.Y;
    gc2:=p.MouseCoord(MousePos.x,MousePos.y);
    if (gc2.x<>gc.x) or (gc2.y<>gc.y) then begin
     //Just set the endpoint to the start or the end of the capturing line
      if MousePos.X>DragStart.X then
        MousePos.X:=rect.Right
      else
        MousePos.X:=rect.Left;
     //and continue with normal handling
    end;
  end;

 //Swap points so that mox2 is to the right
  if DragStart.x>MousePos.x then
  begin
    mox1:=MousePos.x;
    mox2:=DragStart.x;
  end else
  begin
    mox1:=DragStart.x;
    mox2:=MousePos.x;
  end;

 //calculate char count -- if half of the char is covered, it's covered
  mox1 := GetCoveredCharNo(p.Canvas,FontName,FontSize,Result,mox1-rect.left-2,true);
  mox2 := GetCoveredCharNo(p.Canvas,FontName,FontSize,Result,mox2-rect.left-2,true);
  if mox1<0 then mox1 := 0;
  if mox2<0 then mox2 := 0;
  if mox1>flength(Result) then mox1 := flength(Result);
  if mox2>flength(Result) then mox2 := flength(Result);

  text:=Result;
  Result:=fcopy(text,1+mox1,mox2-mox1);
  if flength(Result)<mox2-mox1 then mox2:=mox1+flength(Result); //don't select over the end of text

  SetSelectionHighlight(
    rect.Left+2+CalcStrWidth(p.Canvas,FontName,FontSize,copy(text,1,1+mox1-1)), //TODO: Hardcoded FontSmall
    rect.Top,
    rect.Left+2+CalcStrWidth(p.Canvas,FontName,FontSize,copy(text,1,1+mox2-1)),
    rect.Bottom,
    p.Canvas);
end;

initialization
  IntTip.RegisterHighlightHandler(TStringGrid, DrawGridHighlightContent);

end.
