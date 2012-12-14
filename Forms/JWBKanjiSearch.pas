unit JWBKanjiSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, CheckLst;

type
  TfKanjiSearch = class(TForm)
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Bevel2: TBevel;
    SpeedButton14: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton24: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    SpeedButton8: TSpeedButton;
    Bevel1: TBevel;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton23: TSpeedButton;
    RadioGroup1: TRadioGroup;
    Bevel3: TBevel;
    ComboBox1: TComboBox;
    PaintBox1: TPaintBox;
    Shape1: TShape;
    ListBox1: TCheckListBox;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton1: TSpeedButton;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton18Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1ClickCheck(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure SpeedButton20Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure RadioGroup2Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fKanjiSearch: TfKanjiSearch;

implementation

uses JWBKanji, JWBSettings, JWBUnit, JWBMenu, JWBRadical, JWBNewCategory,
  JWBKanjiDetails, JWBCategories;

{$R *.DFM}

procedure TfKanjiSearch.SpeedButton4Click(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSearch.SpeedButton18Click(Sender: TObject);
begin
  SpeedButton4.Down:=false;
  SpeedButton5.Down:=false;
  SpeedButton7.Down:=false;
  SpeedButton22.Down:=false;
  SpeedButton1.Down:=false;
  SpeedButton2.Down:=false;
  SpeedButton3.Down:=false;
  SpeedButton12.Down:=false;
  SpeedButton6.Down:=false;
  SpeedButton14.Down:=false;
//  SpeedButton16.Down:=false;
  SpeedButton24.Down:=false;
  fKanji.DoIt;
end;

procedure TfKanjiSearch.Edit1Change(Sender: TObject);
begin
  SpeedButton4.Down:=Edit1.Text<>'';;
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit6Change(Sender: TObject);
begin
  SpeedButton5.Down:=Edit6.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit3Change(Sender: TObject);
begin
  SpeedButton7.Down:=Edit3.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit2Change(Sender: TObject);
begin
  SpeedButton6.Down:=Edit2.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit4Change(Sender: TObject);
begin
  SpeedButton14.Down:=Edit4.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit5Change(Sender: TObject);
begin
//  SpeedButton16.Down:=Edit5.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit8Change(Sender: TObject);
begin
  SpeedButton22.Down:=Edit8.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit7Change(Sender: TObject);
begin
  SpeedButton12.Down:=Edit7.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.Edit9Change(Sender: TObject);
begin
  SpeedButton24.Down:=Edit9.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.SpeedButton19Click(Sender: TObject);
var i:integer;
begin
  for i:=0 to ListBox1.Items.Count-1 do ListBox1.Checked[i]:=false;
  fKanji.DoIt;
end;

procedure TfKanjiSearch.SpeedButton8Click(Sender: TObject);
begin
  fRadical.ShowModal;
  PaintBox1.Invalidate;
end;

procedure TfKanjiSearch.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.btnSearchSort.Down:=false;
  fMenu.aKanjiSearch.Checked:=false;
end;

procedure TfKanjiSearch.SpeedButton10Click(Sender: TObject);
begin
  edit7.text:=fKanji.OffsetRange(edit7.text,1,1);
end;

procedure TfKanjiSearch.SpeedButton9Click(Sender: TObject);
begin
  edit7.text:=fKanji.OffsetRange(edit7.text,-1,-1);
end;

procedure TfKanjiSearch.SpeedButton11Click(Sender: TObject);
begin
  edit7.text:=fKanji.OffsetRange(edit7.text,-1,1);
end;

procedure TfKanjiSearch.SpeedButton13Click(Sender: TObject);
begin
  edit7.text:=fKanji.OffsetRange(edit7.text,1,-1);
end;

procedure TfKanjiSearch.SpeedButton17Click(Sender: TObject);
begin
  edit9.text:=fKanji.OffsetRange(edit9.text,1,1);
end;

procedure TfKanjiSearch.SpeedButton15Click(Sender: TObject);
begin
  edit9.text:=fKanji.OffsetRange(edit9.text,-1,-1);
end;

procedure TfKanjiSearch.SpeedButton21Click(Sender: TObject);
begin
  edit9.text:=fKanji.OffsetRange(edit9.text,-1,1);
end;

procedure TfKanjiSearch.SpeedButton23Click(Sender: TObject);
begin
  edit9.text:=fKanji.OffsetRange(edit9.text,1,-1);
end;

procedure TfKanjiSearch.RadioGroup1Click(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSearch.ComboBox1Change(Sender: TObject);
begin
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color:=clBtnFace;
  PaintBox1.Canvas.Font.Style:=[];
  DrawUnicode(PaintBox1.Canvas,1,1,16,curradsearch,FontRadical);
end;

procedure TfKanjiSearch.SpeedButton1Click(Sender: TObject);
var category:string;
begin
  fNewCategory.RadioGroup1.Enabled:=false;
  fNewCategory.Edit1.Text:='';
  if fNewCategory.ShowModal<>idOK then
  begin
    fNewCategory.RadioGroup1.Enabled:=true;
    exit;
  end;
  fNewCategory.RadioGroup1.Enabled:=false;
  category:='k~'+fNewCategory.Edit1.Text;
  inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),category,inttostr(ord('K')),FormatDateTime('yyyymmdd',now)]);
  CreateKnownList(MaxCategoryIndex,0);
  fMenu.RefreshKanjiCategory;
  fMenu.ChangeUserData;
end;

procedure TfKanjiSearch.ListBox1Click(Sender: TObject);
begin
  SpeedButton25.Enabled:=strtoint(kanjicatuniqs[ListBox1.ItemIndex])<>KnownLearned;
  SpeedButton20.Enabled:=strtoint(kanjicatuniqs[ListBox1.ItemIndex])<>KnownLearned;
end;

procedure TfKanjiSearch.ListBox1ClickCheck(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSearch.SpeedButton25Click(Sender: TObject);
begin
  if fKanjiSearch.ListBox1.ItemIndex=-1 then exit;
  if Application.MessageBox(
    pchar(_l('#00882^eDo you really want to delete the category including all character links to it?')),
    pchar(_l('#00573^eWarning')),
    MB_ICONWARNING or MB_YESNO)=idYes then
  begin
    TUserCat.Locate('Name','k~'+fKanjiSearch.ListBox1.Items[fKanjiSearch.ListBox1.ItemIndex],false);
    TUserCat.Delete;
    fMenu.RefreshKanjiCategory;
    fMenu.ChangeUserData;
  end;
end;

procedure TfKanjiSearch.SpeedButton20Click(Sender: TObject);
var catname: string;
begin
  if fKanjiSearch.ListBox1.ItemIndex=-1 then exit;
  TUserCat.Locate('Name','k~'+fKanjiDetails.ComboBox1.Items[fKanjiSearch.ListBox1.ItemIndex],false);
  catname := StripCatName(TUserCat.Str(TUserCatName));
  if fNewCategory.EditCategory(catname) then begin
    TUserCat.Edit([TUserCatName],['k~'+catname]);
    fMenu.RefreshKanjiCategory;
    fMenu.ChangeUserData;
  end;
end;

procedure TfKanjiSearch.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if strtoint(kanjicatuniqs[index])=KnownLearned then
    ListBox1.Canvas.Font.Style:=[fsBold] else
    ListBox1.Canvas.Font.Style:=[];
  ListBox1.Canvas.TextOut(Rect.Left,Rect.Top,ListBox1.Items[index]);
end;

procedure TfKanjiSearch.RadioGroup2Click(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSearch.ListBox1DblClick(Sender: TObject);
var i:integer;
begin
  if ListBox1.ItemIndex<>-1 then
  begin
    for i:=0 to ListBox1.Items.Count-1 do
      ListBox1.Checked[i]:=i=ListBox1.ItemIndex;
    fKanji.DoIt;
  end;
end;

end.
