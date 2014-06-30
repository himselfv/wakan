unit JWBWordKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, JWBStrings, JWBForms;

type
  TKanjiEntry = record
    tp: char; //J, K, C, N, U -- see below in ShowKanjiFromString()
    char: FChar; //character itself
    rad: FChar; //its radical
  end;

  TKanjiBox = record
    lbl: TLabel;
    sh: TShape;
    pb: TPaintBox;
  end;
  PKanjiBox = ^TKanjiBox;

  TfWordKanji = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBoxK1Paint(Sender: TObject);
    procedure PaintBoxK1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK1Click(Sender: TObject);
    procedure PaintBoxK1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

 //Low-level: boxes
  protected
    FBoxes: array of TKanjiBox;
    procedure PaintBoxKNPaint(pb: TPaintBox; KN: integer);
  public
    procedure ClearBoxes;
    procedure AddBox(const meaning: string);
    procedure InvalidateBoxes;

 //High-level: kanji/word
  protected
    FKanji: array of TKanjiEntry;
    procedure DetailsForKanji(n:integer);
  public
    procedure Clear;
    procedure ShowKanjiFromString(s: FString);

  protected
   //Undocked width and height are used as permanent storage for dock width/height
   //Docker reads from those by default, and they are never changed.
    FDockMode: TAlign;
    procedure WMSetDockMode(var msg: TMessage); message WM_SET_DOCK_MODE;
    procedure AlignBox(idx: integer);
    procedure UpdateAlignment;

  end;

var
  fWordKanji: TfWordKanji;

implementation
uses TextTable, JWBUnit, JWBWordLookup, JWBMenu, JWBCategories, JWBCharData,
  JWBKanjiDetails, JWBSettings, JWBIntTip, JWBScreenTip;

{$R *.DFM}

procedure TfWordKanji.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TfWordKanji.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fWordLookup.btnWordKanji.Down:=false;
  fMenu.aDictKanji.Checked:=false;
end;

procedure TfWordKanji.FormResize(Sender: TObject);
begin
  UpdateAlignment(); //we might be able to pack more boxes into new column size
end;

//Hides all kanji boxes
procedure TfWordKanji.ClearBoxes;
var i: integer;
begin
  for i := Length(FBoxes) - 1 downto 0 do begin
    FreeAndNil(FBoxes[i].lbl);
    FreeAndNil(FBoxes[i].sh);
    FreeAndNil(FBoxes[i].pb);
  end;
  SetLength(FBoxes, 0);
end;

procedure TfWordKanji.AddBox(const meaning: string);
var idx: integer;
  box: PKanjiBox;
begin
  idx := Length(FBoxes);
  SetLength(FBoxes, idx+1);
  box := @FBoxes[idx];

  box.sh := TShape.Create(Self);
  box.sh.Height := 43;
  box.sh.Width := 202;
  box.sh.Tag := idx+1;
  box.sh.Parent := Self;

  box.pb := TPaintBox.Create(Self);
  box.pb.Height := 41;
  box.pb.Width := 89;
  box.pb.Tag := idx+1;
  box.pb.OnClick := PaintBoxK1Click;
  box.pb.OnMouseDown := PaintBoxK1MouseDown;
  box.pb.OnMouseMove := PaintBoxK1MouseMove;
  box.pb.OnMouseUp := PaintBoxK1MouseUp;
  box.pb.OnPaint := PaintBoxK1Paint;
  box.pb.Parent := Self;

  box.lbl := TLabel.Create(Self);
  box.lbl.AutoSize := false;
  box.lbl.Caption := meaning;
  box.lbl.Height := 41;
  box.lbl.Width := 125;
  box.lbl.Tag := idx+1;
  box.lbl.Transparent := true;
  box.lbl.WordWrap := true;
  box.lbl.Parent := Self;

  AlignBox(idx);
end;

procedure TfWordKanji.InvalidateBoxes;
var i: integer;
begin
  for i := 0 to Length(FBoxes) - 1 do
    FBoxes[i].pb.Invalidate;
end;

procedure TfWordKanji.PaintBoxK1Click(Sender: TObject);
begin
  DetailsForKanji(TPaintBox(Sender).Tag);
end;

procedure TfWordKanji.PaintBoxK1Paint(Sender: TObject);
begin
  PaintBoxKNPaint(TPaintBox(Sender), TPaintBox(Sender).Tag);
end;

//Paints fWordKanji.PaintBoxK1...PaintBoxK9 contents.
//KN: 1..9
procedure TfWordKanji.PaintBoxKNPaint(pb: TPaintBox; KN: integer);
begin
  Assert((KN>=1) and (KN<=9));
  if length(FKanji)<KN then exit;
  BeginDrawReg(pb.Canvas);
  pb.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(pb.Canvas,44,4,16,FKanji[KN-1].rad,FontJapaneseGrid);
  case FKanji[KN-1].tp of
    'K':pb.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':pb.Canvas.Font.Color:=Col('Kanji_Common');
    'U':pb.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':pb.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(pb.Canvas,4,2,36,FKanji[KN-1].char,FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfWordKanji.PaintBoxK1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then ScreenTip.PopupImmediate(false);
end;

procedure TfWordKanji.PaintBoxK1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  IntTip.MouseMove(TPaintBox(Sender),x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then IntTip.MouseUp;
end;

procedure TfWordKanji.UpdateAlignment;
var idx: integer;
begin
  for idx := 0 to Length(FBoxes) - 1 do
    AlignBox(idx);
end;

procedure TfWordKanji.AlignBox(idx: integer);
var box: PKanjiBox;
begin
  box := @FBoxes[idx];
  if FDockMode in [alLeft,alRight] then begin
    box.sh.Left := 8;
    box.sh.Top := 26 + 48*idx;
  end else begin
    box.sh.Left := 8 + (202+8)*idx;
    box.sh.Top := 26;
  end;
  box.pb.Left := box.sh.Left + 1;
  box.pb.Top := box.sh.Top + 1;
  box.lbl.Left := box.sh.Left + 77;
  box.lbl.Top := box.sh.Top + 2;
 //TODO: Wrap boxes row-by-row or column-by-column, depending on the alignment
end;

procedure TfWordKanji.WMSetDockMode(var msg: TMessage);
begin
  if FDockMode=TAlign(msg.WParam) then exit;
  FDockMode := TAlign(msg.WParam);
  UpdateAlignment;
end;

procedure TfWordKanji.Clear;
begin
  ClearBoxes;
  SetLength(FKanji,0);
end;

procedure TfWordKanji.ShowKanjiFromString(s: FString);
var s2: FString;
  meaning: FString;
  radf:integer;
  rad:FString;
  CCharProp: TCharPropertyCursor;
begin
  SetLength(FKanji,0);
  while flength(s)>0 do
  begin
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if TChar.Locate('Unicode',s2) then
    begin
      radf:=fSettings.GetPreferredRadicalType();
      if TRadicals.Locate('Number',GetCharRadicalNumber(s2,radf)) then
      begin
        rad := TRadicals.Str(TRadicals.fUnicode);
        SetLength(FKanji, Length(FKanji)+1);
        if flength(s2)>0 then
          FKanji[Length(FKanji)-1].char := fgetch(s2, 1)
        else
          FKanji[Length(FKanji)-1].char := UH_NOCHAR;
        if flength(rad)>0 then
          FKanji[Length(FKanji)-1].rad := fgetch(rad, 1)
        else
          FKanji[Length(FKanji)-1].rad := UH_NOCHAR;
        if TChar.Bool(TChar.fChinese) then
          FKanji[Length(FKanji)-1].tp := 'J'
        else
        if IsKnown(KnownLearned,TChar.Fch(TChar.fUnicode)) then
          FKanji[Length(FKanji)-1].tp := 'K'
        else
        if TChar.Int(TChar.fJouyouGrade)<9 then
          FKanji[Length(FKanji)-1].tp := 'C'
        else
        if TChar.Int(TChar.fJouyouGrade)<10 then
          FKanji[Length(FKanji)-1].tp := 'N'
        else
          FKanji[Length(FKanji)-1].tp := 'U';
        CCharProp := TCharPropertyCursor.Create(TCharProp);
        try
          if curlang='j' then
            meaning := CCharProp.GetCharProps(s2, ptJapaneseDefinition)
          else
          if curlang='c' then
            meaning := CCharProp.GetCharProps(s2, ptChineseDefinition)
          else
            meaning := '';
        finally
          FreeAndNil(CCharProp);
        end;
        fWordKanji.AddBox(meaning);
      end;
    end;
  end;
end;

procedure TfWordKanji.DetailsForKanji(n:integer);
begin
  if fMenu.CharDetDocked then exit;
  if n<=Length(FKanji) then
    fKanjiDetails.SetCharDetails(FKanji[n-1].char);
  if not fKanjiDetails.Visible then fMenu.aKanjiDetails.Execute else fKanjiDetails.SetFocus;
end;

end.
