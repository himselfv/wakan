unit JWBVocabDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, WakanPaintbox;

type
  TfVocabDetails = class(TForm)
    Panel3: TPanel;
    btnMoveUpInCategory: TSpeedButton;
    btnMoveDownInCategory: TSpeedButton;
    btnDelete: TButton;
    ScrollBox1: TScrollBox;
    FlowPanel1: TFlowPanel;
    pnlEntryContents: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    btnSaveMeaning: TButton;
    edtMeaning: TMemo;
    GroupBox1: TGroupBox;
    lblLearnState: TLabel;
    btnSetProblematic: TButton;
    btnSetUnlearned: TButton;
    btnSetLearned: TButton;
    btnSetMastered: TButton;
    GroupBox3: TGroupBox;
    cbAddCategory: TComboBox;
    btnAddToCategory: TButton;
    lbCategories: TListBox;
    btnRemoveFromCategory: TButton;
    Panel2: TPanel;
    Label11: TLabel;
    lblDateCreated: TLabel;
    Label13: TLabel;
    lblDateLearned: TLabel;
    Label14: TLabel;
    lblDateMastered: TLabel;
    Label12: TLabel;
    lblDatePrinted: TLabel;
    pbPhonetic: TWakanPaintbox;
    pbKanji: TWakanPaintbox;
    procedure pbPhoneticPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbKanjiPaint(Sender: TObject; Canvas: TCanvas);
    procedure btnAddToCategoryClick(Sender: TObject);
    procedure btnSetProblematicClick(Sender: TObject);
    procedure btnRemoveFromCategoryClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpInCategoryClick(Sender: TObject);
    procedure btnMoveDownInCategoryClick(Sender: TObject);
    procedure btnSaveMeaningClick(Sender: TObject);
    procedure cbAddCategoryChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbKanjiMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure edtMeaningChange(Sender: TObject);
    procedure edtMeaningKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);

  protected
    cl:TStringList; //cached instance
  public
    procedure Reset;
    procedure SetMultipleWords;
    procedure SetSingleWord;
    procedure ClearCategories;
    procedure AddWordCategories(id:integer);

    procedure SetWordControlsEnabled(Value: boolean);

  end;

var
  fVocabDetails: TfVocabDetails;

implementation

uses TextTable, JWBVocab, JWBMenu, JWBUnit, JWBUserData, JWBVocabFilters,
  JWBCategories;

{$R *.DFM}

procedure TfVocabDetails.FormDestroy(Sender: TObject);
begin
  FreeAndNil(cl);
end;

procedure TfVocabDetails.Reset;
begin
  lblLearnState.Caption:='-';
  edtMeaning.Text:='';
  edtMeaning.ReadOnly:=true;
  btnSaveMeaning.Enabled:=false;
  btnAddToCategory.Enabled:=false;
  btnRemoveFromCategory.Enabled:=false;
  cbAddCategory.Enabled:=false;
  lbCategories.Enabled:=false;
  lbCategories.Items.Clear;
  lblDateCreated.Caption:='-';
  lblDateLearned.Caption:='-';
  lblDateMastered.Caption:='-';
  lblDatePrinted.Caption:='-';
  SetWordControlsEnabled(false);
  btnMoveUpInCategory.Visible:=fVocabFilters.rgSort.ItemIndex=0;
  btnMoveDownInCategory.Visible:=fVocabFilters.rgSort.ItemIndex=0;
  btnMoveUpInCategory.Enabled:=false;
  btnMoveDownInCategory.Enabled:=false;
  pbPhonetic.Invalidate;
  pbKanji.Invalidate;
end;

procedure TfVocabDetails.ClearCategories;
begin
  fVocabDetails.lbCategories.Items.Clear;
end;

procedure TfVocabDetails.AddWordCategories(id:integer);
var i: integer;
begin
  if cl=nil then
    cl:=TStringList.Create
  else
    cl.Clear;

  ListWordCategories(id,cl);
  for i:=0 to cl.Count-1 do
    if fVocabDetails.lbCategories.Items.IndexOf(copy(cl[i],3,length(cl[i])-2))=-1 then
      fVocabDetails.lbCategories.Items.Add(copy(cl[i],3,length(cl[i])-2));
end;

//Called when multiple words are selected in vocabulary list
procedure TfVocabDetails.SetMultipleWords;
begin
  SetWordControlsEnabled(true);
  btnAddToCategory.Enabled:=true;
  cbAddCategory.Enabled:=true;
  lbCategories.Enabled:=true;

  if lbCategories.Count>1 then begin
    btnRemoveFromCategory.Enabled:=true;
    lbCategories.ItemIndex:=0;
  end else
    btnRemoveFromCategory.Enabled:=false;
end;

procedure TfVocabDetails.SetSingleWord;
begin
  case TUser.Int(TUserScore) of
    0:lblLearnState.Caption:=_l('#00638^eProblematic');
    1:lblLearnState.Caption:=_l('#00639^eUnlearned');
    2:lblLearnState.Caption:=_l('#00640^eLearned');
    3:lblLearnState.Caption:=_l('#00641^eMastered');
  end;
  edtMeaning.Text:=TUser.Str(TUserEnglish);
  edtMeaning.ReadOnly:=false;
  btnSaveMeaning.Enabled:=true;
  btnAddToCategory.Enabled:=true;
  lblDateCreated.Caption:=DateForm(TUser.Str(TUserAdded));
  lblDateLearned.Caption:=DateForm(TUser.Str(TUserLearned));
  lblDateMastered.Caption:=DateForm(TUser.Str(TUserMastered));
  lblDatePrinted.Caption:=DateForm(TUser.Str(TUserPrinted));
  if lblDatePrinted.Caption<>'-'then lblDatePrinted.Caption:=lblDatePrinted.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
  SetWordControlsEnabled(true);
  cbAddCategory.Enabled:=true;
  lbCategories.Enabled:=true;
  pbPhonetic.Invalidate;
  pbKanji.Invalidate;
  btnSaveMeaning.Enabled:=false;
  btnAddToCategory.Default:=false;

  if lbCategories.Count>1 then begin
    btnRemoveFromCategory.Enabled:=true;
    lbCategories.ItemIndex:=0;
  end else
    btnRemoveFromCategory.Enabled:=false;
end;

procedure TfVocabDetails.SetWordControlsEnabled(Value: boolean);
begin
  btnDelete.Enabled:=Value;
  btnSetProblematic.Enabled:=Value;
  btnSetUnlearned.Enabled:=Value;
  btnSetLearned.Enabled:=Value;
  btnSetMastered.Enabled:=Value;
end;

procedure TfVocabDetails.pbPhoneticPaint(Sender: TObject; Canvas: TCanvas);
begin
  Brush.Color:=clWindow;
  DrawKana(Canvas,1,1,22,fVocab.curphonetic,FontJapanese,showroma,curlang);
end;

procedure TfVocabDetails.pbKanjiPaint(Sender: TObject; Canvas: TCanvas);
begin
  pbKanji.Canvas.Brush.Color:=clWindow;
  BeginDrawReg(Canvas);
  DrawUnicode(Canvas,1,1,22,fVocab.curkanji,FontJapanese);
  EndDrawReg;
end;

procedure TfVocabDetails.btnAddToCategoryClick(Sender: TObject);
begin
  fVocab.AddWordsToCategory(cbAddCategory.text);
  btnAddToCategory.Default := false;
end;

//SetProblematic/Unlearned/Learned/Mastered
procedure TfVocabDetails.btnSetProblematicClick(Sender: TObject);
begin
  fVocab.SetWordsLearnState(TControl(Sender).Tag);
end;

procedure TfVocabDetails.btnRemoveFromCategoryClick(Sender: TObject);
begin
  fVocab.RemoveWordsFromCategory();
end;

procedure TfVocabDetails.btnDeleteClick(Sender: TObject);
begin
  fVocab.DeleteWords();
end;

procedure TfVocabDetails.btnMoveUpInCategoryClick(Sender: TObject);
begin
  fVocab.MoveWordsInCategory(mdUp);
end;

procedure TfVocabDetails.btnMoveDownInCategoryClick(Sender: TObject);
begin
  fVocab.MoveWordsInCategory(mdDown);
end;

procedure TfVocabDetails.btnSaveMeaningClick(Sender: TObject);
begin
  if not TUser.Locate('Index',fVocab.curword) then raise Exception.Create(eWordNotLocated);
  TUser.Edit([TUserEnglish],[edtMeaning.Text]);
  fMenu.ChangeUserData;
  fVocab.ShowIt(false);
end;

procedure TfVocabDetails.cbAddCategoryChange(Sender: TObject);
begin
  btnAddToCategory.Default:=true;
end;

procedure TfVocabDetails.edtMeaningChange(Sender: TObject);
begin
  btnSaveMeaning.Enabled := true;
  btnAddToCategory.Default:=false;
end;

procedure TfVocabDetails.edtMeaningKeyPress(Sender: TObject; var Key: Char);
begin
  if (Ord(Key)=VK_RETURN) and btnSaveMeaning.Enabled then
    btnSaveMeaningClick(btnSaveMeaning);
end;

procedure TfVocabDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fVocab.SpeedButton4.Down:=false;
  fMenu.aUserDetails.Checked:=false;
end;

procedure TfVocabDetails.pbKanjiMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipMouseMove(pbKanji,x,y,ssLeft in Shift);
end;

procedure TfVocabDetails.FormResize(Sender: TObject);
begin
 { All controls fit and still some space - use it for content }
  if ClientWidth>1065 then
    pnlEntryContents.Width := 525 + (ClientWidth-1065)
  else
 { Normal mode }
  if ClientWidth>615 then
    pnlEntryContents.Width := 525
  else
 { If there's not enough place even just for contents, scale it down }
    pnlEntryContents.Width := 525-(615-ClientWidth);

 //Remember width/height preferences in UndockWidth/Height
  Self.UndockHeight := Self.Height;
end;

procedure TfVocabDetails.ScrollBox1Resize(Sender: TObject);
begin
 //Auto-size FlowPanel uses ExplicitWidth/Height as a base when reflowing items.
 //We need it to reflow with current width though.
  FlowPanel1.Width := FlowPanel1.Width; //Update explicit bounds
  FlowPanel1.Height := 0; //Start with 0 and grow
//  FlowPanel1.Realign;
end;


end.
