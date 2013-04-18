unit JWBUserDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, RXCtrls, ExtCtrls, WakanPaintbox;

type
  TfUserDetails = class(TForm)
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
    lblLearnState: TRxLabel;
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
    lblTimesPrinted: TLabel;
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
  fUserDetails: TfUserDetails;

implementation

uses JWBWords, JWBMenu, JWBUnit, JWBUserData, JWBUserFilters, JWBCategories;

{$R *.DFM}

procedure TfUserDetails.FormDestroy(Sender: TObject);
begin
  FreeAndNil(cl);
end;

procedure TfUserDetails.Reset;
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
  lblTimesPrinted.Caption:='-';
  SetWordControlsEnabled(false);
  btnMoveUpInCategory.Visible:=fUserFilters.rgSort.ItemIndex=0;
  btnMoveDownInCategory.Visible:=fUserFilters.rgSort.ItemIndex=0;
  btnMoveUpInCategory.Enabled:=false;
  btnMoveDownInCategory.Enabled:=false;
  pbPhonetic.Invalidate;
  pbKanji.Invalidate;
end;

procedure TfUserDetails.ClearCategories;
begin
  fUserDetails.lbCategories.Items.Clear;
end;

procedure TfUserDetails.AddWordCategories(id:integer);
var i: integer;
begin
  if cl=nil then
    cl:=TStringList.Create
  else
    cl.Clear;

  ListWordCategories(id,cl);
  for i:=0 to cl.Count-1 do
    if fUserDetails.lbCategories.Items.IndexOf(copy(cl[i],3,length(cl[i])-2))=-1 then
      fUserDetails.lbCategories.Items.Add(copy(cl[i],3,length(cl[i])-2));
end;

//Called when multiple words are selected in vocabulary list
procedure TfUserDetails.SetMultipleWords;
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

procedure TfUserDetails.SetSingleWord;
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
  lblTimesPrinted.Caption:=DateForm(TUser.Str(TUserPrinted));
  if lblTimesPrinted.Caption<>'-'then lblTimesPrinted.Caption:=lblTimesPrinted.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
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

procedure TfUserDetails.SetWordControlsEnabled(Value: boolean);
begin
  btnDelete.Enabled:=Value;
  btnSetProblematic.Enabled:=Value;
  btnSetUnlearned.Enabled:=Value;
  btnSetLearned.Enabled:=Value;
  btnSetMastered.Enabled:=Value;
end;

procedure TfUserDetails.pbPhoneticPaint(Sender: TObject; Canvas: TCanvas);
begin
  Brush.Color:=clWindow;
  DrawKana(Canvas,1,1,22,fWords.curphonetic,FontJapanese,showroma,romasys,curlang);
end;

procedure TfUserDetails.pbKanjiPaint(Sender: TObject; Canvas: TCanvas);
begin
  pbKanji.Canvas.Brush.Color:=clWindow;
  BeginDrawReg(Canvas);
  DrawUnicode(Canvas,1,1,22,fWords.curkanji,FontJapanese);
  EndDrawReg;
end;

procedure TfUserDetails.btnAddToCategoryClick(Sender: TObject);
begin
  fWords.AddWordsToCategory(cbAddCategory.text);
  btnAddToCategory.Default := false;
end;

//SetProblematic/Unlearned/Learned/Mastered
procedure TfUserDetails.btnSetProblematicClick(Sender: TObject);
begin
  fWords.SetWordsLearnState(TControl(Sender).Tag);
end;

procedure TfUserDetails.btnRemoveFromCategoryClick(Sender: TObject);
begin
  fWords.RemoveWordsFromCategory();
end;

procedure TfUserDetails.btnDeleteClick(Sender: TObject);
begin
  fWords.DeleteWords();
end;

procedure TfUserDetails.btnMoveUpInCategoryClick(Sender: TObject);
begin
  fWords.MoveWordsInCategory(mdUp);
end;

procedure TfUserDetails.btnMoveDownInCategoryClick(Sender: TObject);
begin
  fWords.MoveWordsInCategory(mdDown);
end;

procedure TfUserDetails.btnSaveMeaningClick(Sender: TObject);
begin
  if not TUser.Locate('Index',fWords.curword) then raise Exception.Create(eWordNotLocated);
  TUser.Edit([TUserEnglish],[edtMeaning.Text]);
  fMenu.ChangeUserData;
  fWords.ShowIt(false);
end;

procedure TfUserDetails.cbAddCategoryChange(Sender: TObject);
begin
  btnAddToCategory.Default:=true;
end;

procedure TfUserDetails.edtMeaningChange(Sender: TObject);
begin
  btnSaveMeaning.Enabled := true;
  btnAddToCategory.Default:=false;
end;

procedure TfUserDetails.edtMeaningKeyPress(Sender: TObject; var Key: Char);
begin
  if (Ord(Key)=VK_RETURN) and btnSaveMeaning.Enabled then
    btnSaveMeaningClick(btnSaveMeaning);
end;

procedure TfUserDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fWords.SpeedButton4.Down:=false;
  fMenu.aUserDetails.Checked:=false;
end;

procedure TfUserDetails.pbKanjiMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipMouseMove(pbKanji,x,y,ssLeft in Shift);
end;

procedure TfUserDetails.FormResize(Sender: TObject);
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

procedure TfUserDetails.ScrollBox1Resize(Sender: TObject);
begin
 //Auto-size FlowPanel uses ExplicitWidth/Height as a base when reflowing items.
 //We need it to reflow with current width though.
  FlowPanel1.Width := FlowPanel1.Width; //Update explicit bounds
  FlowPanel1.Height := 0; //Start with 0 and grow
//  FlowPanel1.Realign;
end;


end.
