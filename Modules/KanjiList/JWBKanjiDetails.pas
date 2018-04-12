unit JWBKanjiDetails;

//{$DEFINE NO_KD_PLACEMENT}
{ Disable saving and restoring of form placement for this window.
 Used to debug form position handling - we must be able to keep track of it even
 with position saving turned off in options }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, Menus, Grids, FormPlacemnt, TextTable,
  WakanWordGrid, JWBStrings, JWBForms;

type
  TCharReadings = record
    piny,kory,cany,chiny:string;
    ony,kuny,nany:UnicodeString;
    def:UnicodeString;
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
    pnlSecond: TPanel;
    pnlFooter: TPanel;
    btnClose: TButton;
    btnDock: TButton;
    Scrollbox: TScrollBox;
    pbKanjiInfo: TPaintBox;
    pnlCategories: TFlowPanel;
    pnlLinks: TFlowPanel;
    pnlFirst: TPanel;
    FormPlacement1: TFormPlacement;
    ShapeKanji: TShape;
    lblMeaning: TLabel;
    ShapeRadical: TShape;
    pbKanji: TPaintBox;
    pbRadical: TPaintBox;
    ShapeSimplified: TShape;
    RxLabel10: TLabel;
    RxLabel35: TLabel;
    lblRadicalNo: TLabel;
    btnStrokeOrder: TSpeedButton;
    pmCategoryMenu: TPopupMenu;
    pmAddCategoryMenu: TPopupMenu;
    pmGoToCategory: TMenuItem;
    pmDelete: TMenuItem;
    pmAddToAll: TMenuItem;
    PopupMenu: TPopupMenu;
    Configure1: TMenuItem;
    btnGoToWords: TSpeedButton;
    pnlOldCategories: TFlowPanel;
    cbCategories: TComboBox;
    btnAddToCategory: TSpeedButton;
    lblCharClass: TLabel;
    pbSimplified: TPaintBox;
    btnGoToChars: TSpeedButton;
    procedure pbKanjiPaint(Sender: TObject);
    procedure pbRadicalPaint(Sender: TObject);
    procedure pbSimplifiedPaint(Sender: TObject);
    procedure btnAddToCategoryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton23Click(Sender: TObject);
    procedure btnStrokeOrderClick(Sender: TObject);
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
    procedure cbCategoriesChange(Sender: TObject);
    procedure pbRadicalMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbSimplifiedMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbKanjiInfoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollboxClick(Sender: TObject);
    procedure pmAddToAllClick(Sender: TObject);
    procedure pmDeleteClick(Sender: TObject);
    procedure pmGoToCategoryClick(Sender: TObject);
    procedure Configure1Click(Sender: TObject);
    procedure btnGoToWordsClick(Sender: TObject);
    procedure btnGoToCharsClick(Sender: TObject);

  protected
    curChars: FString; //displaying information for these characters
    curSingleChar: FChar; //shortcut if we're displaying a single character
    kval: THellspawnStringList;
    procedure ReloadReadings(CChar: TTextTableCursor; out read: TCharReadings);
    procedure PopulateKval(CChar: TTextTableCursor; const read: TCharReadings);
  public
    procedure Clear;
    procedure SetCharDetails(chars: FString);
    procedure RefreshDetails;
    function GetSelectedCharacters: FString;

  protected
   { Info box painting }
    function InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
    procedure InfoDrawItem(canvas:TCanvas;its,txs:string;l,r:integer;
      var x,y,rh:integer;onlycount:boolean);
    procedure DrawSingleText(canvas:TCanvas;tp:char;l,t,r,fh:integer;s:string);
    function FitText(canvas:TCanvas;tp:char;wrap:boolean;w,fh:integer;
      fname:string;out l:integer;var s: HellspawnString): HellspawnString;

  protected
    FDockMode: TAlign; //this window keeps different size settings for docked and undocked mode
    FDockedWidth: integer; //store docked width/height settings while undocked
    FDockedHeight: integer;
    function GetDockedWidth: integer;
    function GetDockedHeight: integer;
    procedure SetDockedWidth(Value: integer);
    procedure SetDockedHeight(Value: integer);
    procedure WMGetDockedW(var msg: TMessage); message WM_GET_DOCKED_W;
    procedure WMGetDockedH(var msg: TMessage); message WM_GET_DOCKED_H;
    procedure WMSaveDockedWH(var msg: TMessage); message WM_SAVE_DOCKED_WH;
    procedure WMSetDockMode(var msg: TMessage); message WM_SET_DOCK_MODE;
  public
    procedure BeforeDestruction; override;
    procedure UpdateAlignment;
    procedure SetDocked(Value: boolean; Loading: boolean);
    procedure UpdateVisible;
    property DockedWidth: integer read GetDockedWidth write SetDockedWidth;
    property DockedHeight: integer read GetDockedHeight write SetDockedHeight;

  protected
    procedure CategoryListChanged(Sender: TObject);
    procedure KanjiCategoryEntriesChanged(Sender: TObject);
    procedure ClearCategories;
    procedure ReloadCategories;
    procedure ReloadAddCategoryMenu;
    procedure CategoryButtonClick(Sender: TObject);
    procedure AddCategoryButtonClick(Sender: TObject);
    procedure AddCategoryClick(Sender: TObject);
    procedure NewCategoryClick(Sender: TObject);

  protected
    procedure ClearReferenceLinks;
    procedure ReloadReferenceLinks;

  end;

var
  fKanjiDetails: TfKanjiDetails;

  curkanji: FChar;
  curradno: integer;
  curradical: string;

{
User configuration for KanjiDetails info box -- stored in WAKAN.CDT
}
var
  chardetl:TStringList;

function GetCharDet(i,j:integer):string;

implementation

uses UITypes, ShellApi, JWBKanjiList, JWBMenu, JWBSettings, JWBUnit, StrokeOrder, JWBCategories,
  KanaConv, JWBCharData, JWBKanjiCompounds, JWBRefLinks, JWBLanguage,
  JWBClipboard, JWBIntTip, JWBScreenTip;

{$R *.DFM}

var
  cursimple: FString;

{
User configuration for KanjiDetails info box
}

function GetCharDet(i,j:integer):string;
var s:string;
begin
  s:=chardetl[i];
  while j>0 do
  begin
    delete(s,1,pos(';',s));
    dec(j);
  end;
  delete(s,pos(';',s),length(s)-pos(';',s)+1);
  result:=s;
end;

procedure TfKanjiDetails.FormCreate(Sender: TObject);
begin
  kval:=TStringList.Create;
  cursimple:='';
  DockedWidth:=321; //fixed docked size
  DockedHeight:=220;
  OnCategoryListChanged.Add(Self.CategoryListChanged);
  OnKanjiCategoryEntriesChanged.Add(Self.KanjiCategoryEntriesChanged);
end;

procedure TfKanjiDetails.BeforeDestruction;
begin
 {$IFNDEF NO_KD_PLACEMENT}
 { We do this here because TForm.BeforeDestruction hides the form and there's
  no way to know if it was Visible. }
  if (not fMenu.CharDetDocked) and FormPlacement1.PlacementRestored then
    FormPlacement1.SaveFormPlacement;
 {$ENDIF}
  inherited;
end;

procedure TfKanjiDetails.FormDestroy(Sender: TObject);
begin
  OnKanjiCategoryEntriesChanged.Remove(Self.KanjiCategoryEntriesChanged);
  OnCategoryListChanged.Remove(Self.CategoryListChanged);
  kval.Free;
end;

procedure TfKanjiDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  UpdateVisible();
end;

procedure TfKanjiDetails.FormShow(Sender: TObject);
begin
  UpdateVisible();
 { AutoSize does not work while invisible, so we have to trigger AdjustSize for
  autosized controls }
  pnlCategories.Height := pnlCategories.Height + 1;
  pnlLinks.Height := pnlLinks.Height + 1;
end;

procedure TfKanjiDetails.FormHide(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then //forms might already cease to exist on destruction
    UpdateVisible();
end;

procedure TfKanjiDetails.UpdateVisible;
begin
  fMenu.aKanjiDetails.Checked:=Self.Visible;
  btnDock.Enabled:=fMenu.CharDetDocked or (fMenu.DisplayMode in [1,3,4]);
  btnClose.Default:=not fMenu.CharDetDocked;
end;

procedure TfKanjiDetails.pbKanjiPaint(Sender: TObject);
var f:string;
begin
  if curLang='c' then
    case fSettings.RadioGroup5.ItemIndex of
      0:f:=FontChinese;
      1:f:=FontChineseGB;
      2:f:=FontRadical;
    end
  else f:=FontJapanese;
  if btnStrokeOrder.Down then
    if curLang='c' then f:=FontChinese else f:=FontStrokeOrder;
  pbKanji.Canvas.Brush.Color:=clWindow;
  pbKanji.Canvas.Font.Style:=[];
  if flength(curChars)=1 then begin //can be 0 or multiple chars
    DrawUnicodeChar(pbKanji.Canvas,Rect(1,0,137,136),137,curChars,f);
    if btnStrokeOrder.Down then DrawStrokeOrder(pbKanji.Canvas,1,0,137,136,curChars,12,clBlue);
  end;
end;

procedure TfKanjiDetails.pbRadicalPaint(Sender: TObject);
var f:string;
begin
  f:=FontRadical;
  pbRadical.Canvas.Brush.Color:=clWindow;
  pbRadical.Canvas.Font.Style:=[];
  BeginDrawReg(pbRadical.Canvas);
  DrawUnicodeChar(pbRadical.Canvas,Rect(1,1,48,48),48,curradical,f);
  EndDrawReg;
end;

procedure TfKanjiDetails.pbSimplifiedPaint(Sender: TObject);
begin
  pbSimplified.Canvas.Brush.Color:=clWindow;
  pbSimplified.Canvas.Font.Style:=[];
  BeginDrawReg(pbSimplified.Canvas);
  DrawUnicodeChar(pbSimplified.Canvas,Rect(1,1,48,48),48,cursimple,FontRadical);
  EndDrawReg;
end;

procedure TfKanjiDetails.btnAddToCategoryClick(Sender: TObject);
var catIndex: integer;
  newState: boolean;
  i: integer;
begin
  if curChars='' then exit;
  catIndex := GetSelCatIdx(cbCategories);

 //If any of the chars is not in group, we add all chars to group,
 //else we remove all chars
  newState := not IsAllKnown(catIndex,curChars);
  for i := 1 to flength(curChars) do
    SetKnown(catIndex, fgetch(curChars,i), newState);
  fMenu.ChangeUserData;
  RefreshDetails;
end;

procedure TfKanjiDetails.SpeedButton23Click(Sender: TObject);
begin
  if curSingleChar=UH_NOCHAR then exit;
  Clipboard.Text := Clipboard.Text + curSingleChar;
end;

procedure TfKanjiDetails.btnStrokeOrderClick(Sender: TObject);
begin
  pbKanji.Invalidate;
end;

procedure TfKanjiDetails.btnGoToWordsClick(Sender: TObject);
begin
  fMenu.DisplayMode := 6;
  fKanjiCompounds.SetCharCompounds(Self.curSingleChar);
end;

//Locates the character in the character grid, resetting the filters if needed
procedure TfKanjiDetails.btnGoToCharsClick(Sender: TObject);
var i: integer;
begin
  if not fKanji.Visible then
    fMenu.aModeKanji.Execute;
  //Reset filters only if some of the characters are not visible as is
  for i := 1 to flength(Self.curChars) do
    if not fKanji.IsCharacterVisible(fgetch(Self.curChars, i)) then begin
      fKanji.ResetFilters;
      fKanji.InvalidateList;
      break;
    end;
  fKanji.FocusedChars := Self.curChars;
end;

procedure TfKanjiDetails.pbKanjiInfoPaint(Sender: TObject);
begin
  BeginDrawReg(pbKanjiInfo.Canvas);
  InfoPaint(pbKanjiInfo.Canvas,pbKanjiInfo.Width,false);
  EndDrawReg;
end;

procedure TfKanjiDetails.pbRadicalDblClick(Sender: TObject);
begin
  if curradno<>NoRadical then
    fKanji.FilterByRadical(curradno);
end;

procedure TfKanjiDetails.btnCloseClick(Sender: TObject);
begin
  if fMenu.CharDetDocked then fMenu.aKanjiDetails.Execute else Close;
end;

procedure TfKanjiDetails.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then Close;
end;

procedure TfKanjiDetails.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  HandleScrollboxMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TfKanjiDetails.ScrollboxClick(Sender: TObject);
begin
  Scrollbox.SetFocus; //does not set by default
end;

procedure TfKanjiDetails.btnCloseKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then Close;
end;

procedure TfKanjiDetails.btnDockClick(Sender: TObject);
begin
  fMenu.SetCharDetDocked(not fMenu.CharDetDocked, false);
end;

procedure TfKanjiDetails.pbRadicalMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  IntTip.MouseMove(pbRadical,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.pbKanjiInfoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  IntTip.MouseMove(pbKanjiInfo,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.pbSimplifiedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  IntTip.MouseMove(pbSimplified,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.pbRadicalMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then ScreenTip.PopupImmediate(false);
end;

procedure TfKanjiDetails.pbSimplifiedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then ScreenTip.PopupImmediate(false);
end;

procedure TfKanjiDetails.pbKanjiInfoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then ScreenTip.PopupImmediate(false);
end;

procedure TfKanjiDetails.cbCategoriesChange(Sender: TObject);
begin
  RefreshDetails;
end;

procedure TfKanjiDetails.pbRadicalMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then IntTip.MouseUp;
end;

procedure TfKanjiDetails.pbSimplifiedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then IntTip.MouseUp;
end;

procedure TfKanjiDetails.pbKanjiInfoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then IntTip.MouseUp;
end;

procedure TfKanjiDetails.RefreshDetails;
begin
  SetCharDetails(curChars);
end;

procedure TfKanjiDetails.Clear;
begin
  SetCharDetails('');
end;

function TfKanjiDetails.GetSelectedCharacters: FString;
begin
  Result := curChars;
end;

{
"unicode" can be:
   nil      === no char selected, clear the form
   1 char
   multiple chars
It can also contain unsupported characters, in which case we only show what
is obvious from the char itself.
If there are multiple chars selected, only a limited set of operations is
available.
}
procedure TfKanjiDetails.SetCharDetails(chars:FString);
var i:integer;
  h:integer;
  cv:string;
  scat:string;
  cv_i1, cv_i2: integer;
  charvar: string; //simplified/traditional variant
  read: TCharReadings;
  CChar: TTextTableCursor;
  kclass: char;
begin
  //Only Kanji are allowed
  for i := 1 to flength(chars) do
    if EvalChar(chars[i])<>EC_IDG_CHAR then begin
      chars := ''; //illegal chars => just clear this window
      break;
    end;

  curChars := chars;
  if flength(curChars)=1 then
    curSingleChar := fgetch(curChars,1)
  else {0 or multiple}
    curSingleChar := UH_NOCHAR;

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

  pnlOldCategories.Visible := fSettings.rgDetailsCategoryEditorType.ItemIndex in [1,2];

  CChar := TChar.NewCursor;
  try
    Screen.Cursor:=crHourGlass;
    if (curSingleChar=UH_NOCHAR) or not CChar.Locate('Unicode',curSingleChar) then
     //specific char, but not found in the char db -- valid case
      curkanji:=UH_NOCHAR
    else
      curkanji:=CChar.FCh(TChar.fUnicode);

   //Url labels
    pnlLinks.Visible := fSettings.cbDetailsShowLinks.Checked;
    if pnlLinks.Visible then
      ReloadReferenceLinks;

    //Simplified form
    if curkanji=UH_NOCHAR then
      charvar := ''
    else begin
      charvar := GetCharProp(curkanji,ptSimplifiedVariant);
      if charvar<>'' then
        RxLabel35.Caption:=_l('#00135^eSimplified:')
      else begin
        charvar := GetCharProp(curkanji,ptTraditionalVariant);
        if charvar<>'' then
          RxLabel35.Caption:=_l('#00136^eTraditional:');
      end;
    end;
    if charvar<>'' then
    begin
      cursimple:=charvar;
      RxLabel35.Show;
      pbSimplified.Show;
      ShapeSimplified.Show;
      if fSettings.cbDetailsKanjiInColor.Checked then begin
        kclass := GetCharClass(cursimple[1]);
        case kclass of
          'K':pbSimplified.Font.Color:=Col('Kanji_Learned');
          'C':pbSimplified.Font.Color:=Col('Kanji_Common');
          'U':pbSimplified.Font.Color:=Col('Kanji_Rare');
          'N':pbSimplified.Font.Color:=Col('Kanji_Names');
        end;
      end else
        pbSimplified.Font.Color := clBlack;
    end else begin
      cursimple:='';
      RxLabel35.Hide;
      pbSimplified.Hide;
      ShapeSimplified.Hide;
    end;

    //Radical
    if curkanji=UH_NOCHAR then
      curradno:=NoRadical
    else
      curradno:=GetCharRadicalNumber(curkanji, fSettings.GetPreferredRadicalType());
    if curradno=NoRadical then begin
      lblRadicalNo.Caption:=_l('#01088^eNone');
      curradical := ''
    end else begin
      lblRadicalNo.Caption:=IntToStr(curradno);
      curradical := RadicalUnicode(curradno);
      if fSettings.cbDetailsKanjiInColor.Checked then begin
        kclass := GetCharClass(curradical[1]);
        case kclass of
          'K':pbRadical.Font.Color:=Col('Kanji_Learned');
          'C':pbRadical.Font.Color:=Col('Kanji_Common');
          'U':pbRadical.Font.Color:=Col('Kanji_Rare');
          'N':pbRadical.Font.Color:=Col('Kanji_Names');
        end;
      end else
        pbRadical.Font.Color := clBlack;
    end;

    //AddToCategory -- "add" if any of the chars is not in it
    btnAddToCategory.Enabled := flength(curChars)>0;
    cbCategories.Enabled := btnAddToCategory.Enabled;
    if not btnAddToCategory.Enabled then
      btnAddToCategory.Caption := '+' //default will be +
    else
    if (cbCategories.ItemIndex<0)
    or not IsAllKnown(GetSelCatIdx(cbCategories),curChars) then
      btnAddToCategory.Caption:='+'
    else
      btnAddToCategory.Caption:='-';

    //Categories
    ReloadCategories;

    //Stroke count/order
    btnStrokeOrder.Enabled := (curkanji<>UH_NOCHAR);
    if curkanji=UH_NOCHAR then
      btnStrokeOrder.Caption := _l('#01117^Strokes')
    else
    if curLang='c' then begin
      if CChar.Int(TChar.fChStrokeCount)<255 then
        btnStrokeOrder.Caption:=_l('#00162^Strokes:')+' '+CChar.Str(TChar.fChStrokeCount)
      else
        btnStrokeOrder.Caption:=_l('#01117^Strokes');
    end else begin
      if CChar.Int(TChar.fJpStrokeCount)<255 then
        btnStrokeOrder.Caption:=_l('#00162^Strokes:')+' '+CChar.Str(TChar.fJpStrokeCount)
      else
        btnStrokeOrder.Caption:=_l('#01117^Strokes');
    end;

    //Words button
    btnGoToWords.Enabled := curkanji<>UH_NOCHAR; //when 0 or multiple chars, can't "go to words"

    //Kanji class/color
    if curkanji=UH_NOCHAR then
      kclass := 'U'
    else
      kclass := GetCharClass(curkanji);

    lblCharClass.Visible := (curkanji<>UH_NOCHAR) and fSettings.cbDetailsShowKanjiClass.Checked;
    if lblCharClass.Visible then begin
      case kclass of
        'K':lblCharClass.Font.Color:=Col('Kanji_Learned');
        'C':lblCharClass.Font.Color:=Col('Kanji_Common');
        'U':lblCharClass.Font.Color:=Col('Kanji_Rare');
        'N':lblCharClass.Font.Color:=Col('Kanji_Names');
      end;
      case kclass of
        'K':lblCharClass.Caption:=_l('#00140^eLearned');
        'C':lblCharClass.Caption:=_l('#00141^eCommon');
        'U':lblCharClass.Caption:=_l('#00142^eRare');
        'N':lblCharClass.Caption:=_l('#00143^eUsed in names');
        'A':lblCharClass.Caption:=_l('#00144^eJapanese and chinese');
        'J':lblCharClass.Caption:=_l('#00145^eJapanese only');
      end;
    end;
    if fSettings.cbDetailsKanjiInColor.Checked then begin
      case kclass of
        'K':pbKanji.Font.Color:=Col('Kanji_Learned');
        'C':pbKanji.Font.Color:=Col('Kanji_Common');
        'U':pbKanji.Font.Color:=Col('Kanji_Rare');
        'N':pbKanji.Font.Color:=Col('Kanji_Names');
      end;
    end else
      pbKanji.Font.Color := clBlack;

    if flength(curChars)<1 then
      lblMeaning.Caption := ''
    else
    if flength(curChars)>1 then
      lblMeaning.Caption := _l('#01001^eMultiple kanji selected')
    else begin
      ReloadReadings(CChar, read);
      if curLang='c' then
        lblMeaning.Caption:=read.chiny
      else
        lblMeaning.Caption:=read.def;
      PopulateKVal(CChar, read);
    end;

  finally
    FreeAndNil(CChar);
  end;

 { Repaint }
  if curkanji<>UH_NOCHAR then begin
    h:=InfoPaint(pbKanjiInfo.Canvas,pbKanjiInfo.Width,true);
    pbKanjiInfo.Height:=h;
  end else begin
    pbKanjiInfo.Height:=1; //do not make it zero or we'll break panel order (yeah, really)
  end;
  Screen.Cursor:=crDefault;
end;

{ Reloads various character readings from TCharProp table }
procedure TfKanjiDetails.ReloadReadings(CChar: TTextTableCursor; out read: TCharReadings);
var ch:FString;
  CCharProp: TCharPropertyCursor;
begin
  FillChar(read, sizeof(read), 00); //initializes all strings to ''
  ch := CChar.Str(TChar.fUnicode);

  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    read.kory := CCharProp.GetCharProps(ch, ptKoreanReading);
    read.piny := CCharProp.GetCharProps(ch, ptMandarinReading);
    read.def := CCharProp.GetCharProps(ch, ptJapaneseDefinition);
    read.ony := CCharProp.GetCharProps(ch, ptOnReading);
    read.kuny := CCharProp.GetCharProps(ch, ptKunReading);
    read.nany := CCharProp.GetCharProps(ch, ptNanoriReading);
    read.chiny := CCharProp.GetCharProps(ch, ptChineseDefinition);
    read.cany := CCharProp.GetCharProps(ch, ptCantoneseReading);
  finally
    FreeAndNil(CCharProp);
  end;
end;

{
PopulateKVal()
Repopulates KVal list which is used to draw various kanji information box
}
procedure TfKanjiDetails.PopulateKval(CChar: TTextTableCursor; const read: TCharReadings);
var
  s: string;
  i:integer;
  propTypeId: integer;
  propType: PCharPropType;
  CCharProp: TCharPropertyCursor;
begin
  kval.Clear;

  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    for i:=0 to chardetl.Count-1 do
    begin
      propTypeId := strtoint(GetCharDet(i,0));
      propType := FindCharPropType(propTypeId);
      if propType=nil then continue;

      s:='';
      case propTypeId of
        0:s:='---';
        1:s:=LowerCase(read.kory);
        2:s:=ConvertPinYin(read.piny);
        3:s:=read.def;
        4:s:=fstr(read.ony);
        5:s:=fstr(read.kuny);
        6:s:=fstr(read.nany);
        7:s:=read.chiny;
        8:s:=LowerCase(read.cany);
        100:s:=curSingleChar;
        else
          s := CCharProp.GetCharProps(CChar.Str(TChar.fUnicode), propTypeId)
      end;

      if GetCharDet(i,6)<>'' then
        kval.Add(propType.dataType+';'+chardetl[i])
      else
        kval.Add(propType.dataType+';'+chardetl[i]+_l('^e'+propType.englishName));
      kval.Add(s);

    end; //of chardetl enum
  finally
    FreeAndNil(CCharProp);
  end;
end;


{ Categories }

type
  TCatButton = class(TButton)
  protected
    FPartial: boolean;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
  public
    DropdownMenu: TPopupMenu;
    constructor Create(AOwner: TComponent); override;
    procedure SetPartial(const APartial: boolean);
  end;

  TCatLabel = class(TLabel)
  protected
    FPartial: boolean;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
  public
    DropdownMenu: TPopupMenu;
    constructor Create(AOwner: TComponent); override;
    procedure SetPartial(const APartial: boolean);
  end;

 { Only one is currently used, but it's possible to switch }
  TTagItem = TCatLabel;

constructor TCatButton.Create(AOwner: TComponent);
begin
  inherited;
  FPartial := false;
  Self.Margins.Top := 0;
  Self.Margins.Left := 0;
  Self.Margins.Bottom := 4;
  Self.Margins.Right := 4;
  Self.AlignWithMargins := true;
  Self.Height := 20;
end;

procedure TCatButton.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  inherited;
  SetPartial(FPartial); //re-apply
end;

procedure TCatButton.SetPartial(const APartial: boolean);
begin
  FPartial := APartial;
 //Color cannot be changed for TButton so we resort to this
  if FPartial then
    Self.Font.Style := Self.Font.Style + [fsUnderline]
  else
    Self.Font.Style := Self.Font.Style - [fsUnderline];
end;

constructor TCatLabel.Create(AOwner: TComponent);
begin
  inherited;
  FPartial := false;
  Self.Margins.Top := 0;
  Self.Margins.Left := 0;
  Self.Margins.Bottom := 5;
  Self.Margins.Right := 5;
  Self.AlignWithMargins := true;
  Self.Height := 20;
  Self.Cursor := crHandPoint;
end;

procedure TCatLabel.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  inherited;
  SetPartial(FPartial); //re-apply
end;

procedure TCatLabel.SetPartial(const APartial: boolean);
begin
  FPartial := APartial;
  if FPartial then
    Self.Font.Color := clGrayText
  else
    Self.Font.Color := clBlue;
  Self.Font.Style := Self.Font.Style + [fsUnderline]; //always underline links
end;

procedure TfKanjiDetails.ClearCategories;
var i: integer;
begin
  for i := pnlCategories.ControlCount-1 downto 0 do
    pnlCategories.Controls[i].Destroy;
end;

procedure TfKanjiDetails.ReloadCategories;
var i: integer;
  btn: TTagItem;
begin
  if Self.Visible then
    SendMessage(Handle, WM_SETREDRAW, WPARAM(False), 0);
  pnlCategories.DisableAlign;
  try
    ClearCategories;

   //include if any of the chars is in it
    for i:=0 to Length(KanjiCats)-1 do
      if IsAnyKnown(KanjiCats[i].idx,curChars) then begin
        btn := TTagItem.Create(Self);
        btn.Caption := KanjiCats[i].name;
        Self.Canvas.Font.Assign(pnlCategories.Font); //will be applied to btn on insert
        btn.Width := Self.Canvas.TextWidth(btn.Caption)+16;
        btn.DropdownMenu := pmCategoryMenu;
        btn.OnClick := CategoryButtonClick;
        btn.Tag := i;
        pnlCategories.InsertControl(btn);
        if not IsAllKnown(KanjiCats[i].idx,curChars) then
          btn.SetPartial(true);
      end;

    ReloadAddCategoryMenu;

    if fSettings.rgDetailsCategoryEditorType.ItemIndex in [0,2] then begin
      btn := TTagItem.Create(Self);
      btn.Caption := _l('#01110^+Category');
      btn.Hint := _l('#01111^Add to category');
      Self.Canvas.Font.Assign(pnlCategories.Font); //will be applied to btn on insert
      btn.Width := Self.Canvas.TextWidth(btn.Caption)+16;
      btn.DropdownMenu := pmAddCategoryMenu;
      btn.OnClick := AddCategoryButtonClick;
      btn.Enabled := flength(curChars)>0; //cannot add categories to "no chars"
      pnlCategories.InsertControl(btn);
    end;

  finally
    pnlCategories.EnableAlign;
    if Self.Visible then begin
      SendMessage(Handle, WM_SETREDRAW, WPARAM(True), 0);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
     //Normal Invalidate is not enough
    end;
  end;
end;

//Called when category list changes
procedure TfKanjiDetails.CategoryListChanged;
begin
  PasteKanjiCategoriesTo(Self.cbCategories.Items);
  Self.cbCategories.ItemIndex:=0;
  ReloadCategories; //some could've been deleted
 //ReloadAddCategoryMenu; //called from ReloadCategories
end;

procedure TfKanjiDetails.KanjiCategoryEntriesChanged(Sender: TObject);
begin
 //This could have changed the learned/unlearned/category status of curren char,
 //so reload
  Self.RefreshDetails;
end;

procedure TfKanjiDetails.CategoryButtonClick(Sender: TObject);
var btn: TTagItem;
begin
  btn := TTagItem(Sender);
  pmAddToAll.Visible := not IsAllKnown(KanjiCats[btn.Tag].idx, curChars);
  AddCategoryButtonClick(Sender); //standard popup routine
end;

procedure TfKanjiDetails.AddCategoryButtonClick(Sender: TObject);
var btn: TTagItem;
  pm: TPopupMenu;
  pos: TPoint;
begin
  btn := TTagItem(Sender);
  pm := btn.DropdownMenu;
  pm.PopupComponent := btn;
  pos := btn.ClientOrigin;
  pm.Popup(pos.X, pos.Y+btn.Height);
end;

procedure TfKanjiDetails.ReloadAddCategoryMenu;
var i: integer;
  item: TMenuItem;
begin
  pmAddCategoryMenu.Items.Clear;

  for i:=0 to Length(KanjiCats)-1 do
    if not IsAnyKnown(KanjiCats[i].idx,curChars) then begin
      item := TMenuItem.Create(Self);
      item.Caption := KanjiCats[i].name;
      item.Tag := i;
      item.OnClick := AddCategoryClick;
      pmAddCategoryMenu.Items.Add(item);
    end;

  item := TMenuItem.Create(Self);
  item.Caption := '-';
  pmAddCategoryMenu.Items.Add(item);

  item := TMenuItem.Create(Self);
  item.Caption := _l('#01112^New...');
  item.OnClick := NewCategoryClick;
  pmAddCategoryMenu.Items.Add(item);
end;

//Called from AddCategoryMenu with Sender set to one of TMenuItems
procedure TfKanjiDetails.AddCategoryClick(Sender: TObject);
var catIndex, i: integer;
begin
  catIndex := KanjiCats[TMenuItem(Sender).Tag].idx;
  for i := 1 to flength(curChars) do
    SetKnown(catIndex, fgetch(curChars,i), true);
  fMenu.ChangeUserData;
  RefreshDetails;
end;

procedure TfKanjiDetails.NewCategoryClick(Sender: TObject);
var catIndex, i: integer;
begin
  catIndex := NewKanjiCategoryUI();
  if catIndex<0 then exit;
  for i := 1 to flength(curChars) do
    SetKnown(catIndex, fgetch(curChars,i), true);
  fMenu.ChangeUserData;
  RefreshDetails;
end;

//Called from CategoryMenu, with PopupItem set to one of Category buttons
procedure TfKanjiDetails.pmAddToAllClick(Sender: TObject);
var i: integer;
  btn: TTagItem;
begin
  btn := TTagItem(TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent);
  for i := 1 to flength(curChars) do
    SetKnown(KanjiCats[btn.Tag].idx, fgetch(curChars,i), true);
  fMenu.ChangeUserData;
  RefreshDetails;
end;

procedure TfKanjiDetails.pmDeleteClick(Sender: TObject);
var i: integer;
  btn: TTagItem;
begin
  btn := TTagItem(TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent);
  for i := 1 to flength(curChars) do
    SetKnown(KanjiCats[btn.Tag].idx, fgetch(curChars,i), false);
  fMenu.ChangeUserData;
  RefreshDetails;
end;

procedure TfKanjiDetails.pmGoToCategoryClick(Sender: TObject);
var btn: TTagItem;
begin
  btn := TTagItem(TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent);
  if not fKanji.Visible then
    fMenu.aModeKanji.Execute;
 { For now we just reset filters to "show this group only".
  Alternative would be to hide filters and apply some other filters without
  resetting these. If the user wants to get back to his filters he just opens
  those back. }
  fKanji.ResetFilters;
  fKanji.SetCategoryFilter([KanjiCats[btn.Tag].idx], true, false);
  fKanji.InvalidateList;
end;

procedure TfKanjiDetails.Configure1Click(Sender: TObject);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsCharacterDetailsItems;
  fSettings.ShowModal;
  Self.RefreshDetails;
end;


{ Reference links }

procedure TfKanjiDetails.ClearReferenceLinks;
var i: integer;
begin
  for i := pnlLinks.ControlCount-1 downto 0 do
    pnlLinks.Controls[i].Destroy;
end;

procedure TfKanjiDetails.ReloadReferenceLinks;
var lbl: TRefLabel;
  fname: string;
  ref: TRefLink;
begin
  if Self.Visible then
    SendMessage(Handle, WM_SETREDRAW, WPARAM(False), 0);
  pnlLinks.DisableAlign;
  try
    ClearReferenceLinks;

    if curSingleChar=UH_NOCHAR then begin
      pnlLinks.Visible := false;
      exit;
    end;

    for fname in GetCharacterLinks() do begin
      ref := LoadLink(fname);
      if ref<>nil then
      try
        if ref.MatchesLang(curLang) then begin
          lbl := TRefLabel.Create(Self, ref, curSingleChar);
          lbl.Margins.Left := 0;
          lbl.Margins.Top := 0;
          lbl.Margins.Right := 5;
          lbl.Margins.Bottom := 5;
          lbl.AlignWithMargins := true;
          lbl.Left := 10;
          pnlLinks.InsertControl(lbl);
        end;
      finally
        FreeAndNil(ref);
      end;
    end;

  finally
    pnlLinks.EnableAlign;
    if Self.Visible then begin
      SendMessage(Handle, WM_SETREDRAW, WPARAM(True), 0);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
     //Normal Invalidate is not enough
    end;
  end;
end;


{ Info box painting }

function TfKanjiDetails.InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
const Margin: TRect = (left: 0; top: 0; right: 0; bottom: 0);
var i:integer;
  x,y,rh:integer;
begin
  x:=Margin.Left;
  y:=Margin.Top;
  rh:=0;
  for i:=0 to (kval.Count div 2)-1 do
    InfoDrawItem(canvas,kval[i*2],kval[i*2+1],Margin.Left,w-Margin.Right,x,y,rh,onlycount);
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
  if (GetDet(4)='C') and (curLang<>'c') then exit;
  if (GetDet(4)='J') and (curLang='c') then exit;
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
  if curLang='c' then font:=FontRadical else font:=FontSmall;
  if tp='P' then font:=FontEnglish;
  if (tp='U') or (tp='P') then DrawUnicode(canvas,l,t,fh-2,s,font) else
  if (tp='N') or (tp='T') then canvas.TextOut(r-canvas.TextExtent(s).cx,t,s) else
    canvas.TextOut(l,t,s);
end;

{
Prints as much as possible on a canvas in a specified font.
 canvas: The canvas to print on.
 tp: Text type (non exhaustive list: U for unicode, P for what? )
 wrap: Whether wrapping is allowed (?)
 w: ?
 fh: Font height (also width for full-width unicode)
 fname: Font name
 l: [out] Width of the printed part of text.
 s: [in] Text to print. [out] The non-printed remainer of text.
Returns: width taken by printed part of text
}
function TfKanjiDetails.FitText(canvas:TCanvas;tp:char;wrap:boolean;w,fh:integer;
  fname:string;out l:integer;var s: HellspawnString): HellspawnString;

  function countwidth(tp:char;fh:integer;s:string):integer;
  var ts:TSize;
  begin
    if tp='P' then Result:=0 else
    if tp='U' then Result:=flength(s)*(fh-2) else
    begin
      ts:=canvas.TextExtent(s);
      Result:=ts.cx;
    end;
  end;

var cur_span,last_span:string;
  last_i,i,next_i:integer;
begin
  canvas.Font.Name:=fname;
  canvas.Font.Height:=fh;
  if countwidth(tp,fh,s)<=w then
  begin //Text fits completely
    l:=countwidth(tp,fh,s);
    Result:=s;
    s:='';
    exit;
  end;

  cur_span:='';
  i:=0;
  next_i:=0;
  repeat
    last_span:=cur_span;
    last_i:=i;
    i:=next_i;
    if (tp='U') or (tp='P') then cur_span:=fcopy(s,1,next_i) else cur_span:=copy(s,1,next_i);
    if not wrap then begin
      if (tp='U') or (tp='P') then cur_span:=cur_span+UH_ELLIPSIS else cur_span:=cur_span+'...';
      Inc(next_i);
    end else
    if (tp='U') or (tp='P') then
      Inc(next_i)
    else begin
      Inc(next_i);
      while (next_i<length(s)) and (s[next_i+1]<>' ') do inc(next_i);
    end;
  until countwidth(tp,fh,cur_span)>w;

  if last_span='' then begin
    last_span:=cur_span; //if nothing matches at all, at least print one char
    last_i:=next_i;
  end;

  if last_i<0 then last_i:=0;
  if wrap then
  begin
    result:=last_span;
    if (tp='U') or (tp='P') then fdelete(s,1,last_i) else delete(s,1,last_i);
    l:=countwidth(tp,fh,last_span);
  end else
  begin
    result:=last_span;
    s:='';
    l:=countwidth(tp,fh,last_span);
  end;
end;


{ Readings }

{
Disabled until we get to doing word examples properly

procedure TfKanjiDetails.ReloadWordExamples(const read: TCharReadings);
var parts: TStringArray;
  i, j, sep: integer;
  kanji: string;
  kana: string;
  dic: TDicLookupCursor;
  tmp: string;
begin
//  mmWords.Caption := '';
//  mmWords.Height := 1;
  if self.curSingleChar=UH_NOCHAR then exit;

  tmp := '';
  parts := SplitStr(read.kuny, #$FF0C);
  for i := 0 to Length(parts)-1 do begin
    parts[i] := Trim(parts[i]);
    sep := pos('.', parts[i]);
    if sep>0 then begin
      kanji := self.curSingleChar + copy(parts[i], sep+1, MaxInt);
      kana := repl(parts[i],'.','');
    end else begin
      kanji := self.curSingleChar;
      kana := parts[i];
    end;

    for j := 0 to dicts.Count-1 do
      if dicts[j].loaded then begin
        dic := dicts[j].NewLookup(mtExactMatch);
        try
          dic.LookupKanji(kanji);
          while dic.HaveMatch do begin
            tmp := tmp + kanji+' ['+kana+'] '+dic.GetEntries.ToString+' - '+dicts[j].name + #13#10;
            dic.NextMatch;
          end;
        finally
          FreeAndNil(dic);
        end;
      end;
  end;

  mmWords.Caption := tmp;
end;
}


{ Form alignment / docking }

procedure TfKanjiDetails.UpdateAlignment;
begin
  if FDockMode in [alNone,alLeft,alRight,alClient] then begin //in free floating mode always not Portrait
    pnlFirst.Align := alTop;
    pnlFirst.Height := 176;
    pnlFooter.Parent := pnlSecond;
  end else begin
    pnlFirst.Align := alLeft;
    pnlFirst.Width := 317;
    pnlFooter.Parent := pnlFirst;
  end;
  pnlSecond.Align := alClient;
end;

procedure TfKanjiDetails.FormResize(Sender: TObject);
begin
 { If kept on AutoSize, FlowPanels would not properly realign controls when
  their width is expanded, at all }
  if pnlCategories.AutoSize then begin
    pnlCategories.AutoSize := false;
    pnlCategories.Realign;
    pnlCategories.AutoSize := true;
  end;
  if pnlLinks.AutoSize then begin
    pnlLinks.AutoSize := false;
    pnlLinks.Realign;
    pnlLinks.AutoSize := true;
  end;
end;

function TfKanjiDetails.GetDockedWidth: integer;
begin
  if FDockMode in [alLeft,alRight] then
    Result := ClientWidth
  else
    Result := FDockedWidth;
end;

function TfKanjiDetails.GetDockedHeight: integer;
begin
  if FDockMode in [alTop,alBottom] then
    Result := ClientHeight
  else
    Result := FDockedHeight;
end;

procedure TfKanjiDetails.SetDockedWidth(Value: integer);
begin
  FDockedWidth := Value;
//It would be nice to support this, but it might break stuff:
{  if FDockMode in [alLeft,alRight] then
    if HostDockSite<>nil then
      HostDockSite.Width := Value; }
end;

procedure TfKanjiDetails.SetDockedHeight(Value: integer);
begin
  FDockedHeight := Value;
//See comment above.
{  if FDockMode in [alTop,alBottom] then
    if HostDockSite<>nil then
      HostDockSite.Height := Value; }
end;

{ Docker calls these to get docked control sizes }
procedure TfKanjiDetails.WMGetDockedW(var msg: TMessage);
begin
  msg.Result := FDockedWidth;
end;

procedure TfKanjiDetails.WMGetDockedH(var msg: TMessage);
begin
  msg.Result := FDockedHeight;
end;

{ Docker calls this to save docked sizes before undocking }
procedure TfKanjiDetails.WMSaveDockedWH(var msg: TMessage);
begin
  if FDockMode in [alLeft,alRight] then
    FDockedWidth := ClientWidth;
  if FDockMode in [alTop,alBottom] then
    FDockedHeight := ClientHeight;
end;

{ Called before docking or after undocking.
 Configures the form to appear in either Docked or Floating mode,
 rearranging controls. }
procedure TfKanjiDetails.WMSetDockMode(var msg: TMessage);
var Value: TAlign;
begin
  Value := TAlign(msg.WParam);
  if FDockMode=Value then exit;
  FDockMode := Value;
  if Value<>alNone then begin //before dock
   //Remove constraints
    Constraints.MinWidth := 0;
    Constraints.MaxWidth := 0;
    Constraints.MinHeight := 0;
   //Realign
    Self.Hide; //it's okay, we're going to be hidden as part of docking anyway
   { UpdateAlignment may rearrange controls, say, from vertical to horizontal
    and some controls may end up with negative width/height due to current width
    being insufficient.
    See JWBForms for comments }
    with SafeRearrange(Self) do try
      UpdateAlignment;
    finally
      Unlock;
    end;
   //Docked sizes will be applied at docking
    btnDock.Caption:=_l('#00172^eUndock');
  end else begin //after undock
   //The form is hidden.
   //Realign
    with SafeRearrange(Self) do try
      UpdateAlignment;
    finally
      Unlock;
    end;
   //Add constraints when undocked
    Constraints.MinWidth := 337;
    Constraints.MaxWidth := 337;
    Constraints.MinHeight := 320;
    btnDock.Caption:=_l('#00173^eDock');
  end;
end;

{ In contrast to WMSetDockMode, this is called before and after a LOGICAL
 dock mode change.
 In other words, while fKanjiDetails is hidden on some pages and PRACTICALLY
 undocked, logically it stays docked and needs not to restore it's undocked
 position. }
procedure TfKanjiDetails.SetDocked(Value: boolean; Loading: boolean);
begin
  if Value then begin //before dock
   {$IFNDEF NO_KD_PLACEMENT}
    if (not Loading) and FormPlacement1.PlacementRestored then
      FormPlacement1.SaveFormPlacement; //save placement before breaking it with docking
   {$ENDIF}
  end else begin //after undock
    if Loading then begin
     //Issue 161: if Loading and undocked, we won't get SetDockMode otherwise, and we have stuff to configure
     //This is hackish.
      Self.Hide;
      FDockMode := alCustom;
      Perform(WM_SET_DOCK_MODE, integer(alNone), 0);
    end;
   {$IFNDEF NO_KD_PLACEMENT}
    FormPlacement1.RestoreFormPlacement([]); //docking breaks placement so we restore it
   {$ENDIF}
  end;
end;

initialization
  curradical:='';
  chardetl:=TStringList.Create;

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(chardetl);
 {$ENDIF}

end.
