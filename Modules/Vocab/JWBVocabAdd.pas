unit JWBVocabAdd;
{
Has two modes: "enter anything" and "copy dictionary word (possibly alter meaning)".
Watches clipboard data for changes through Clipboard.Watchers.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, WakanPaintbox, JWBStrings, JwbForms;

type
  TfVocabAdd = class(TJwbForm)
    lblPhonetic: TLabel;
    lblWritten: TLabel;
    lblCategory: TLabel;
    lblMeaning: TLabel;
    edtPhonetic: TEdit;
    cbCategories: TComboBox;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pbWritten: TWakanPaintbox;
    pbPhonetic: TWakanPaintbox;
    edtMeaning: TMemo;
    procedure edtPhoneticChange(Sender: TObject);
    procedure pbWrittenPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbPhoneticPaint(Sender: TObject; Canvas: TCanvas);
    procedure edtMeaningKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  protected
    FMeaningOnly: boolean;
    FFixedKanji: FString;
    FFixedPhonetic: FString;
    FClipText: FString;
    procedure SetMeaningOnly(const Value: boolean);
    procedure SetClipText(const Value: FString);
    procedure ReloadCategories;
    procedure ClipboardChanged(Sender: TObject);
  public
    function ModalAddWord(): TModalResult;
    function ModalAddFixed(const AKanji, APhonetic: FString; const AMeaning: FString): TModalResult;
    property MeaningOnly: boolean read FMeaningOnly write SetMeaningOnly;
    property ClipText: FString read FClipText write SetClipText; //call when clipboard text changes
  end;

{ This form is static because user may expect the choice of category to persist
 between additions.
 E.g. *double click* Add word, category "Verbs". *double click* Why is the
 category reset to "Animals"?
 We don't create the form until the first time it's used. }
function fVocabAdd: TfVocabAdd;

implementation
uses UITypes, TextTable, JWBLanguage, JWBVocab, JWBUnit, JWBDrawText, KanaConv,
  JWBUserData, JWBCategories, JWBClipboard;

{$R *.DFM}

var
  fVocabAddInstance: TfVocabAdd;

function fVocabAdd: TfVocabAdd;
begin
  if fVocabAddInstance=nil then
    Application.CreateForm(TfVocabAdd, fVocabAddInstance);
  Result := fVocabAddInstance;
end;

procedure TfVocabAdd.SetMeaningOnly(const Value: boolean);
begin
  FMeaningOnly := Value;
  edtPhonetic.Visible := not Value;
  if Value then
    pbPhonetic.Color := clWindow
  else
    pbPhonetic.Color := clBtnFace;
  if FMeaningOnly then begin
    lblWritten.Caption := _l('#00061^eWritten:');
    lblMeaning.Caption := _l('#00058^eMeaning (editable):');
  end else begin
    lblWritten.Caption := _l('#00690^eWritten (from clipboard):');
    lblMeaning.Caption := _l('#00691^eMeaning:');
  end;
end;

procedure TfVocabAdd.SetClipText(const Value: FString);
begin
  FClipText := Value;
  if not MeaningOnly then
    pbWritten.Invalidate;
end;

procedure TfVocabAdd.pbPhoneticPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=Self.Color;
  if MeaningOnly then
    DrawUnicode(Canvas,2,2,22,FFixedPhonetic,GetCJKFont())
  else
    DrawUnicode(Canvas,2,2,22,RomajiToKana(edtPhonetic.Text,curlang,[]),GetCJKFont());
end;

procedure TfVocabAdd.edtPhoneticChange(Sender: TObject);
begin
  pbPhonetic.Invalidate;
end;

procedure TfVocabAdd.pbWrittenPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=Self.Color;
  if MeaningOnly then
    DrawUnicode(Canvas,2,2,22,FFixedKanji,GetCJKFont())
  else
    DrawUnicode(Canvas,2,2,22,FClipText,GetCJKFont());
end;

procedure TfVocabAdd.edtMeaningKeyPress(Sender: TObject; var Key: Char);
var str: string;
begin
 //TMemo doesn't support Ctrl-A by itself
  if Key=^A then begin
    (Sender as TMemo).SelectAll;
    Key := #00;
  end;

 { Pressing Ctrl-C without a text being selected copies everything for this article.
  I'd like this to work anywhere on a form, but it doesnt :(
  We need to override Ctrl-C only when it's meaningless otherwise, and first of
  all, the focus mostly stays in this TMemo where Ctrl-C HAS meaning.
  And TForm has no way to "receive keypress but only if no one handled it". }
  if (Key=^C) and ((Sender as TMemo).SelLength<=0) then begin
    if MeaningOnly then
      str := FFixedKanji + ' [' + FFixedPhonetic + '] ' + edtMeaning.Text
    else
      str := FClipText + ' [' + RomajiToKana(edtPhonetic.Text,curlang,[rfDeleteInvalidChars]) + ']'
        + edtMeaning.Text;
    Clipboard.Text := str;
    Key := #00;
  end;
end;

procedure TfVocabAdd.FormShow(Sender: TObject);
begin
 //Reload categories
  ReloadCategories;
  Clipboard.Watchers.Add(Self.ClipboardChanged);
  Self.ClipboardChanged(Self);
end;

procedure TfVocabAdd.FormHide(Sender: TObject);
begin
  Clipboard.Watchers.Remove(Self.ClipboardChanged)
end;

procedure TfVocabAdd.ReloadCategories;
var s, oldcat: string;
  lc: char;
  i: integer;
begin
  oldcat := cbCategories.Text;
  cbCategories.Items.Clear;

  TUserCat.First;
  while not TUserCat.EOF do
  begin
    s:=TUserCat.Str(TUserCatName);
    lc:=GetCatPrefix(s);
    if lc='?' then begin
      lc := 'j';
      TUserCat.Edit([TUserCatName],['j~'+s])
    end;
    s:=StripCatName(s);
    if lc=curlang then
      cbCategories.Items.Add(s);
    TUserCat.Next;
  end;

  if oldcat<>'' then begin
   //Restore previously selected category -- see Issue 215
    for i := 0 to cbCategories.Items.Count-1 do
      if cbCategories.Items[i]=oldcat then begin
        cbCategories.ItemIndex := i;
        break;
      end;
  end else
  if cbCategories.Items.Count>0 then
    cbCategories.Text:=cbCategories.Items[0];

end;

procedure TfVocabAdd.ClipboardChanged(Sender: TObject);
begin
  Self.ClipText := Clipboard.Text;
end;

function TfVocabAdd.ModalAddWord(): TModalResult;
begin
  Self.MeaningOnly := false;
  Self.FFixedKanji := '';
  Self.FFixedPhonetic := '';
  Self.edtPhonetic.Text := '';
  Self.edtMeaning.Text := '';
  Result := Self.ShowModal;
  if IsPositiveResult(Result) then
    fVocab.AddWord(
      FClipText,
      RomajiToKana(edtPhonetic.Text,curlang,[rfDeleteInvalidChars]),
      edtMeaning.Text,
      cbCategories.Text,
      '?', false, 1
    );
end;

function TfVocabAdd.ModalAddFixed(const AKanji, APhonetic: FString;
  const AMeaning: FString): TModalResult;
begin
  Self.MeaningOnly := true;
  Self.FFixedKanji := AKanji;
  Self.FFixedPhonetic := APhonetic;
  Self.edtPhonetic.Text := '';
  Self.edtMeaning.Text := fstrtouni(AMeaning);
  Result := Self.ShowModal;
  if IsPositiveResult(Result) then
    fVocab.AddWord(
      AKanji, APhonetic,
      edtMeaning.Text,
      cbCategories.Text,
      '?', false, 1
    );
end;

end.
