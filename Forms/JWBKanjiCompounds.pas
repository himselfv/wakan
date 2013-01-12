unit JWBKanjiCompounds;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, Buttons, JWBStrings;

type
  TfKanjiCompounds = class(TForm)
    sbShowVocab: TSpeedButton;
    sbShowDict: TSpeedButton;
    Shape7: TShape;
    Label25: TLabel;
    cbLeftMatchOnly: TCheckBox;
    StringGrid1: TStringGrid;
    Bevel1: TBevel;
    cbPopularOnly: TCheckBox;
    SpeedButton23: TSpeedButton;
    SpeedButton17: TSpeedButton;
    cbSortByFrequency: TCheckBox;
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SpeedButton11Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  protected
    FCurChar: FChar;
  public
    procedure SetCharCompounds(ch:FChar);
  end;

var
  fKanjiCompounds: TfKanjiCompounds;

implementation

uses JWBKanji, JWBUnit, JWBMenu, JWBDic, JWBDicAdd, JWBUser, JWBWords,
  JWBSettings, JWBEdictMarkers, JWBCategories;

var curcphonetic,curckanji,curcmeaning:string;

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

procedure TfKanjiCompounds.FormResize(Sender: TObject);
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[0]-24;
end;

procedure TfKanjiCompounds.SetCharCompounds(ch:FChar);
var sl,sl2:TStringList;
    pass:boolean;
    i,k,l:integer;
    dic:TDicIndexCursor;
    freq:string;
    stp:string;
    kj:string;
  usefreq: boolean;
  kmark:TMarkers;
  ent:TEntries;
begin
  FCurChar := ch;
  if not Self.Visible then exit;
  StringGrid1.Visible:=false;
  cbSortByFrequency.Enabled:=(not sbShowVocab.Down) and (curlang='j');
  kj:=ChinFrom(ch);
  sl:=TStringList.Create;
  sl2:=TStringList.Create;
  if sbShowDict.Down then
  begin
    usefreq := (curlang='j') and (cbSortByFrequency.Checked);

    for i:=0 to dicts.Count-1 do
      if dicts[i].loaded and dicts.IsInGroup(dicts[i], 4)
      and ((not usefreq) or dicts[i].SupportsFrequency) then
    begin
      dic:=TDicIndexCursor.Create(dicts[i]);
      try
        dic.Find(itChar,fstrtouni(kj));
        k:=0;
        while dic.Next do
        begin
          inc(k);
          if pos(kj,dic.GetKanji)=0 then
            showmessage('Dictionary has corrupted index: '+kj+'-'+inttostr(k)+'-'+dic.GetArticleBody);
          if (not cbLeftMatchOnly.Checked) or (pos(kj,dic.GetKanji)=1) then
          begin
            kmark := dic.GetKanjiKanaMarkers;
            ent := dic.GetEntries;
            if usefreq then begin
              freq:=inttostr(9999999-dic.GetFrequency);
              while length(freq)<7 do freq:='0'+freq;
            end else
              freq := dic.GetKanji; //sort alphabetically
            if (not usefreq) or (freq<>'9999999') then
              if ((not cbPopularOnly.Checked)
              or TestMarkers(kmark, MarkPop)
              or TestMarkers(ent.MergeMarkers, MarkPop)) then
                sl.Add(freq+#9
                  +CheckKnownKanji(ChinTo(dic.GetKanji))
                  +' ['+dic.GetPhonetic+']'
                  +' {'+ent.ToEnrichedString+'}');
          end;
        end;
      finally
        FreeAndNil(dic);
      end;
    end;
    sl.Sort;
  end else if sbShowVocab.Down then
  begin
    TUserIdx.SetOrder('Kanji_Ind');
    TUserIdx.Locate('Kanji',kj);
    while (not TUserIdx.EOF) and (TUserIdx.Str(TUserIdxKanji)=kj) do
    begin
      if (not cbLeftMatchOnly.Checked) or (TUserIdx.Bool(TUserIdxBegin)) then
      begin
        sl2.Clear;
        ListWordCategories(TUserIdx.Int(TUserIdxWord),sl2);
        pass:=false;
        for l:=0 to sl2.Count-1 do if (pos(curlang+'~',sl2[l])=1) or (length(sl2[l])<2) or (copy(sl2[l],2,1)<>'~') then pass:=true;
        if (pass) and (TUser.Locate('Index',TUserIdx.TrueInt(TUserIdxWord))) then
        begin
          stp:=TUser.Str(TUserScore);
          sl.Add(TUser.Str(TUserKanji)+#9
            +ALTCH_EXCL+stp+CheckKnownKanji(ChinTo(TUser.Str(TUserKanji)))
            +' ['+ALTCH_EXCL+stp+TUser.Str(TUserPhonetic)+']'
            +' {'+ALTCH_EXCL+stp+FixVocabEntry(TUser.Str(TUserEnglish))+'}');
        end;
      end;
      TUserIdx.Next;
    end;
  end;
  sl.Sort;
  if sbShowDict.Down and cbSortByFrequency.Checked and (strtoint(fSettings.Edit34.Text)<>0) then
    while sl.Count>strtoint(fSettings.Edit34.Text) do sl.Delete(strtoint(fSettings.Edit34.Text));
  for i:=0 to sl.Count-1 do
    sl[i]:=copy(sl[i],pos(#9,sl[i])+1,length(sl[i])-pos(#9,sl[i]));
  FillWordGrid(StringGrid1,sl,false,false);
  sl.Free;
  sl2.Free;
end;

procedure TfKanjiCompounds.StringGrid1DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(StringGrid1,ACol,ARow,Rect,State);
end;

procedure TfKanjiCompounds.SpeedButton11Click(Sender: TObject);
begin
  fKanji.KanjiCompounds_CheckBox1Click(sender);
end;

procedure TfKanjiCompounds.StringGrid1DblClick(Sender: TObject);
begin
  if StringGrid1.Row>-1 then showmessage(StringGrid1.Cells[2,StringGrid1.Row]);
end;

procedure TfKanjiCompounds.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipGridOver(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfKanjiCompounds.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiCompounds.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  curcphonetic:=remexcl(copy(StringGrid1.Cells[0,ARow],2,length(StringGrid1.Cells[0,ARow])-1));
  curckanji:=remexcl(copy(StringGrid1.Cells[1,ARow],2,length(StringGrid1.Cells[1,ARow])-1));
  curcmeaning:=strip_fl(remexcl(StringGrid1.Cells[2,ARow]));
  fDicAdd.Edit3.Text:=remexcl(StringGrid1.Cells[2,ARow]);
end;

procedure TfKanjiCompounds.SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curckanji;
  fMenu.ChangeClipboard;
end;

procedure TfKanjiCompounds.SpeedButton17Click(Sender: TObject);
begin
  fUser.curkanji:=curckanji;
  fUser.curphonetic:=curcphonetic;
  if fDicAdd.ShowModal=mrOK then
  begin
    if not fWords.AddWord(curckanji,curcphonetic,fDicAdd.edit3.text,fDicAdd.ComboBox1.Text,'?',false,1) then exit;
  end;
end;

procedure TfKanjiCompounds.StringGrid1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
