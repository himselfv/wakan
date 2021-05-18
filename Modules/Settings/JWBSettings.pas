﻿unit JWBSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, IniFiles, registry, ImgList,
  CheckLst, SimpleControls, ExtListBoxes, WakanPaintbox;

type
  TfSettings = class(TForm)
    pcPages: TPageControl;
    tsRomanization: TTabSheet;
    tsCharacterList: TTabSheet;
    rgKanjiGridSize: TRadioGroup;
    CheckBox1: TCheckBox;
    tsDictionary: TTabSheet;
    GroupBox3: TGroupBox;
    cbPreferUserWords: TCheckBox;
    cbPreferNounsAndVerbs: TCheckBox;
    cbPreferPolite: TCheckBox;
    cbPreferPopular: TCheckBox;
    cbReplaceKanji: TCheckBox;
    tsWordListPrinting: TTabSheet;
    cbInsideLines: TCheckBox;
    cbOutsideLines: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    GroupBox4: TGroupBox;
    lbWordPrintFormat: TListBox;
    Label11: TLabel;
    Edit10: TEdit;
    tsCharacterCardPrinting: TTabSheet;
    Label12: TLabel;
    edtKanjiCardRowsOnPage: TEdit;
    Label13: TLabel;
    edtKanjiCardAdditionalWidth: TEdit;
    Label14: TLabel;
    edtKanjiCardCharSize: TEdit;
    cbKanjiCardPrintCompounds: TCheckBox;
    cbKanjiCardPrintRadical: TCheckBox;
    cbKanjiCardPrintAlternate: TCheckBox;
    cbKanjiCardPrintOuterLines: TCheckBox;
    cbKanjiCardPrintReadings: TCheckBox;
    cbKanjiCardPrintInnerLines: TCheckBox;
    Label15: TLabel;
    edtKanjiCardFont: TEdit;
    SpeedButton10: TSpeedButton;
    cbKanjiCardPrintVertical: TCheckBox;
    cbKanjiCardColumnSpace: TCheckBox;
    tsDatabaseMaintenance: TTabSheet;
    Button2: TButton;
    Button1: TButton;
    OpenDialog2: TOpenDialog;
    SaveDialog2: TSaveDialog;
    Button3: TButton;
    tsGeneral: TTabSheet;
    Label23: TLabel;
    Edit16: TEdit;
    tsTextTranslator: TTabSheet;
    cbKanjiCardPrintDefinition: TCheckBox;
    cbKanjiCardPrintStrokeCount: TCheckBox;
    Button4: TButton;
    Label26: TLabel;
    Edit19: TEdit;
    GroupBox6: TGroupBox;
    Label16: TLabel;
    rgShowKana: TRadioGroup;
    edtTestRomaji: TEdit;
    GroupBox7: TGroupBox;
    Label27: TLabel;
    rgShowBopomofo: TRadioGroup;
    edtTestPinyin: TEdit;
    tsCharacterDetailsItems: TTabSheet;
    Label34: TLabel;
    ListBox2: TListBox;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label35: TLabel;
    cbRadicalType: TComboBox;
    Button10: TButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    tsPopupTool: TTabSheet;
    cbScreenTipForJapanese: TCheckBox;
    cbScreenTipForEnglish: TCheckBox;
    cbScreenTipForKanji: TCheckBox;
    Label36: TLabel;
    Edit21: TEdit;
    Label37: TLabel;
    Label38: TLabel;
    Edit22: TEdit;
    Label39: TLabel;
    Edit23: TEdit;
    Label40: TLabel;
    edtScreenTipMaxDictEntries: TEdit;
    Button11: TButton;
    cbDictLimitAutoResults: TCheckBox;
    CheckBox50: TCheckBox;
    tsColors: TTabSheet;
    ListBox3: TListBox;
    Shape2: TShape;
    Button12: TButton;
    Button14: TButton;
    Button15: TButton;
    Label42: TLabel;
    ComboBox2: TComboBox;
    cbNoCharColors: TCheckBox;
    cbNoEditorColors: TCheckBox;
    cbNoWordGridColors: TCheckBox;
    ColorDialog1: TColorDialog;
    CheckBox51: TCheckBox;
    cbKanjiCardPrintStrokeOrder: TCheckBox;
    Label43: TLabel;
    edtScreenTipSizeFactor: TEdit;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    edtScreenTipMinCompounds: TEdit;
    edtScreenTipMaxCompounds: TEdit;
    cbYomiIgnoreOkurigana: TCheckBox;
    cbShowFreq: TCheckBox;
    cbOrderFreq: TCheckBox;
    Edit34: TEdit;
    Label52: TLabel;
    tsEditor: TTabSheet;
    Label53: TLabel;
    cbKanjiCardPrintFullComp: TCheckBox;
    cbKanjiCardSortFrequency: TCheckBox;
    edtKanjiCardFullCompounds: TEdit;
    pnlButtons: TPanel;
    btnChangeLanguage: TButton;
    btnOk: TBitBtn;
    sbGeneral: TScrollBox;
    Label41: TLabel;
    Label47: TLabel;
    CheckBox26: TCheckBox;
    CheckBox10: TCheckBox;
    cbStatusColors: TCheckBox;
    CheckBox46: TCheckBox;
    Edit25: TEdit;
    CheckBox49: TCheckBox;
    cbMultilineGrids: TCheckBox;
    cbAutosave: TCheckBox;
    edtAutoSavePeriod: TEdit;
    CheckBox55: TCheckBox;
    RadioGroup5: TRadioGroup;
    CheckBox70: TCheckBox;
    cbShowSplashscreen: TCheckBox;
    cbSaveColumnWidths: TCheckBox;
    cbSaveSearchParams: TCheckBox;
    tvContents: TTreeView;
    tsEditorPrinting: TTabSheet;
    cbNoSearchParticles: TCheckBox;
    cbNoTranslateHiragana: TCheckBox;
    cbTranslateNoLongTextWarning: TCheckBox;
    cbMultithreadedTranslation: TCheckBox;
    tsEditorSaving: TTabSheet;
    tsEditorAozoraRuby: TTabSheet;
    lblAozoraRuby: TLabel;
    cbLoadAozoraRuby: TCheckBox;
    cbAozoraTagsInColor: TCheckBox;
    lblAozoraTagsInColor: TLabel;
    cbSaveAnnotationsToRuby: TCheckBox;
    lblSaveAnnotationsToRubyDesc: TLabel;
    lblSavingAndLoading: TLabel;
    cbEditorAutosave: TCheckBox;
    cbEditorAutoloadLast: TCheckBox;
    cbNoSaveChangesWarning: TCheckBox;
    Label48: TLabel;
    CheckBox43: TCheckBox;
    cbDisplayLines: TCheckBox;
    cbNoMeaningLearned: TCheckBox;
    cbNoReadingLearned: TCheckBox;
    cbReadingKatakana: TCheckBox;
    CheckBox41: TCheckBox;
    cbSpaceBetweenLines: TCheckBox;
    cbReserveSpaceForReading: TCheckBox;
    cbUserBold: TCheckBox;
    Label25: TLabel;
    edtMeaningLines: TEdit;
    CheckBox27: TCheckBox;
    cbHintMeaning: TCheckBox;
    cbShowEditorHint: TCheckBox;
    Label54: TLabel;
    cbAdjustCharPriorities: TCheckBox;
    rgReleaseCursorMode: TRadioGroup;
    Label57: TLabel;
    cbPrintReading: TCheckBox;
    cbPrintMeaning: TCheckBox;
    cbVerticalPrint: TCheckBox;
    cbNoPrintColors: TCheckBox;
    Label24: TLabel;
    edtPrintLines: TEdit;
    Label58: TLabel;
    Spacer: TPanel;
    btnImportKanjidic: TButton;
    pbRomajiAsHiragana: TWakanPaintbox;
    pbRomajiAsKatakana: TWakanPaintbox;
    pbPinyinAsBopomofo: TWakanPaintbox;
    cbMultipleRoma: TCheckBox;
    btnRomaSystemUp: TBitBtn;
    btnRomaSystemDown: TBitBtn;
    lbRomaSystems: TRearrangeCheckListBox;
    pbKanaAsRomaji: TWakanPaintbox;
    lblKanaToRomajiHint: TLabel;
    pbBopomofoAsPinyin: TWakanPaintbox;
    Label17: TLabel;
    btnPinyinSystemUp: TBitBtn;
    btnPinyinSystemDown: TBitBtn;
    lbPinyinSystems: TRearrangeCheckListBox;
    cbMultiplePinyin: TCheckBox;
    tsExprCopyFormats: TTabSheet;
    Label18: TLabel;
    lbExprCopyFormats: TListBox;
    mmExprCopyFormatExample: TMemo;
    lblExprCopyFormatsEdit: TUrlLabel;
    lblExprCopyFormatsDocs: TUrlLabel;
    tsCharacterDetailsGeneral: TTabSheet;
    cbDetailsShowKanjiClass: TCheckBox;
    cbDetailsKanjiInColor: TCheckBox;
    rgDetailsCategoryEditorType: TRadioGroup;
    cbDetailsShowLinks: TCheckBox;
    cbDictRefLinksInSubmenu: TCheckBox;
    tsKanjiCopyFormats: TTabSheet;
    Label19: TLabel;
    lbKanjiCopyFormats: TListBox;
    lblKanjiCopyFormatsEdit: TUrlLabel;
    lblKanjiCopyFormatsDocs: TUrlLabel;
    mmKanjiCopyFormatExample: TMemo;
    procedure RadioGroup1Click(Sender: TObject);
    procedure btnChangeLanguageClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure edtTestRomajiChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure lbWordPrintFormatClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure edtTestPinyinChange(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ListBox3Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure cbNoCharColorsClick(Sender: TObject);
    procedure cbNoEditorColorsClick(Sender: TObject);
    procedure cbNoWordGridColorsClick(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvContentsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure btnImportKanjidicClick(Sender: TObject);
    procedure tvContentsChange(Sender: TObject; Node: TTreeNode);
    procedure pbRomajiAsHiraganaPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbRomajiAsKatakanaPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbPinyinAsBopomofoPaint(Sender: TObject; Canvas: TCanvas);
    procedure btnRomaSystemUpClick(Sender: TObject);
    procedure btnRomaSystemDownClick(Sender: TObject);
    procedure lbRomaSystemsClick(Sender: TObject);
    procedure cbMultipleRomaClick(Sender: TObject);
    procedure lbRomaSystemsSelectionChanged(Sender: TObject);
    procedure pbKanaAsRomajiPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbKanaAsRomajiClick(Sender: TObject);
    procedure lbPinyinSystemsClick(Sender: TObject);
    procedure lbPinyinSystemsSelectionChanged(Sender: TObject);
    procedure btnPinyinSystemUpClick(Sender: TObject);
    procedure btnPinyinSystemDownClick(Sender: TObject);
    procedure cbMultiplePinyinClick(Sender: TObject);
    procedure pbBopomofoAsPinyinClick(Sender: TObject);
    procedure pbBopomofoAsPinyinPaint(Sender: TObject; Canvas: TCanvas);
    procedure rgShowBopomofoClick(Sender: TObject);
    procedure tsExprCopyFormatsShow(Sender: TObject);
    procedure lbExprCopyFormatsClick(Sender: TObject);
    procedure lbKanjiCopyFormatsClick(Sender: TObject);
    procedure tsKanjiCopyFormatsShow(Sender: TObject);

  protected
    procedure ResetCharDetl;
    procedure LoadCharDetl(reg: TCustomIniFile);
    procedure SaveCharDetl(reg: TCustomIniFile);
  public
    procedure ReloadCharDetlList;

  protected
    FContentsInitialized: boolean;
    procedure InitContents;
    procedure SelectActiveContentItem;
    function FindContentItemByTag(const ATag: integer): TTreeNode;
    function FindNextFreeTag: integer;
    procedure BroadcastToPages(Msg: cardinal; wParam: NativeUInt; lParam: NativeInt);
  public
    procedure AddSettingsPage(page: TForm; category: string);

  protected //Romanization
    FKanaExample: string; //current string to romanize, see pbKanaAsRomajiClick
    FBopomofoExample: string;
    procedure ReloadRomaSystems;
    procedure ReloadPinyinSystems;
    function GetRomaList: string;
    procedure SetRomaList(const Value: string);
    function GetPinyinList: string;
    procedure SetPinyinList(const Value: string);
    procedure SetEnhancedRomaListOn(const Value: boolean);
    procedure SetEnhancedPinyinListOn(const Value: boolean);
    procedure CheckRomaSystemMoveButtonsEnabled;
    procedure CheckPinyinSystemMoveButtonsEnabled;
  public
    procedure ReloadRomaSetup;
    procedure ReloadPinyinSetup;

  protected //CopyFormats
    FDefaultExprCopyFormatName: string;
    procedure ReloadExprCopyFormats;
    procedure UpdateExprCopyFormatExample;
  public
    property DefaultExprCopyFormatName: string read FDefaultExprCopyFormatName
      write FDefaultExprCopyFormatName;

  protected //KanjiCopyFormats
    FDefaultKanjiCopyFormatName: string;
    procedure ReloadKanjiCopyFormats;
    procedure UpdateKanjiCopyFormatExample;
  public
    property DefaultKanjiCopyFormatName: string read FDefaultKanjiCopyFormatName
      write FDefaultKanjiCopyFormatName;

  public
   { Layout settings are loaded into these variables and applied later }
    SetLayout: integer;
    SetWindows: integer;
    setUserCompounds: boolean;
    setPortraitMode: boolean;
    setKanjiCustomSearch: integer;
    CharDetDocked: boolean;
    CharDetDockedVis1: boolean;
    CharDetDockedVis2: boolean;
  protected
    procedure LoadRegistrySettings(reg: TCustomIniFile);
    procedure SaveRegistrySettings(reg: TCustomIniFile);
  public
    procedure LoadSettings;
    procedure SaveSettings;
    procedure AcceptSettings;

  public
    function GetPreferredRadicalType: integer;

  end;

const
 //Sent to all custom pages on events.
  WM_SETTINGS = WM_USER + 2000;
  WM_LOADSETTINGS = WM_SETTINGS + 1;   //wParam = TCustomIniFile
  WM_SAVESETTINGS = WM_SETTINGS + 2;   //wParam = TCustomIniFile

var
  fSettings: TfSettings;

const
 //Values must be reasonable
  DefaultAutoSavePeriod: integer = 10;

 //Default romanization files
  KunreishikiRoma = 'Kunreishiki.roma'; //important
  HepburnRoma = 'Hepburn.roma'; //important
  CzechRoma = 'Czech.roma';
  PinYinRoma = 'PinYin.rpy';
  WadeGilesRoma = 'Wade-Giles.rpy'; //important
  YaleRoma = 'Yale.rpy';
 //Extensions
  RomajiExt = '.roma';
  PinyinExt = '.rpy';

 //Examples for romanization
  KanaExamples: array[0..5] of string = (
   'だじゃれなしゃみせん',
   'きたないまちへ',
   'ずっとつづくれっしゃ',
   'マスコミのだいこうしん',
   'おっさんをよべ',
   'えいえん'
  );
  BopomofoExamples: array[0..2] of string = (
   'ㄓㄨˋㄧㄣㄈㄨˊㄏㄠˋ',
   'ㄆㄧㄥˊㄗ˙',
   'ㄦˊㄦˇㄦˋㄦ˙ㄦ'
  );

implementation
uses
  JWBStrings, JWBUnit, JWBDrawText, TextTable,
{$IFNDEF AUTOTEST}
  ActnList, JWBCore, AppData, JWBIO, JWBLanguage, KanaConv, JWBFontSelect,
  JWBMenu, JWBEditor, JWBKanjiList, JWBKanjiCard, JWBKanjiDetails,
  WakanWordGrid, JWBWordLookupBase, JWBWordLookup, JWBKanjiCompounds, JWBExamples,
  JWBUserData, JWBVocab, JWBVocabDetails, JWBVocabFilters, JWBCharItem,
  JWBCharData, JWBCharDataImport, JWBScreenTip,
{$ENDIF}
  JWBDic, JWBDicSearch, JWBDictionaries
  ;

var colorfrom:integer;

{$R *.DFM}

procedure TfSettings.FormShow(Sender: TObject);
begin
  if not FContentsInitialized then
    InitContents();

 { pcPages.OnChange is not triggered when ActivePage is loaded initially from FormSettings,
  so whatever, we'll do this on show just to be safe: }
  SelectActiveContentItem();

  edtTestRomajiChange(sender);
  edtTestPinyinChange(sender);
  Edit19.Text:=dicts.NotUsedDicts;
  ReloadCharDetlList;
  ComboBox2.ItemIndex:=0;
  ComboBox2Change(sender);
end;

{
Called when user clicks "OK" in the dialog.
Use this chance to:
 1. Complain about invalid data in some fields.
 2. Auto-correct the data if you use it without checking somewhere (still a bad idea)
 3. Apply the new settings to the program, if needed.
}
procedure TfSettings.AcceptSettings;
var tmp:integer;
begin
 //Verify control
  if edtKanjiCardRowsOnPage.text='0' then edtKanjiCardRowsOnPage.text:='1';
  if not TryStrToInt(Edit10.Text, tmp) then Edit10.Text := '0';
  if not TryStrToInt(edtKanjiCardRowsOnPage.Text, tmp) then edtKanjiCardRowsOnPage.Text := '0';
  if not TryStrToInt(edtKanjiCardAdditionalWidth.Text, tmp) then edtKanjiCardAdditionalWidth.Text := '0';
  if not TryStrToInt(edtKanjiCardCharSize.Text, tmp) then edtKanjiCardCharSize.Text := '0';
  if not TryStrToInt(edtAutoSavePeriod.Text, tmp) then
    edtAutoSavePeriod.Text := IntToStr(DefaultAutoSavePeriod);
  GridFontSize:=strtoint(Edit25.text);
end;

procedure TfSettings.btnOkClick(Sender: TObject);
begin
{
 Currently the majority of the settings is changed on the fly,
 so no matter which way the dialog is closed, it must run "OK" code.
 (There's simply no "Cancel")
 Therefore all handling is in FormClose (triggered by any closing of the form).
}
end;

procedure TfSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AcceptSettings(); //this can raise exceptions on invalid data => dialog not closed
  SaveSettings();
end;


{ Contents }

procedure TfSettings.InitContents;
var i: integer;
begin
  for i := 0 to tvContents.Items.Count - 1 do
    tvContents.Items[i].Expand({Recurse=}true);
  SelectActiveContentItem;
  tvContents.Width := tvContents.Width + 1;
  tvContents.Width := tvContents.Width - 1;
  FContentsInitialized := true;
end;

procedure TfSettings.SelectActiveContentItem;
var i: integer;
  found: boolean;
begin
  found := false;
  for i := 0 to tvContents.Items.Count - 1 do
    if tvContents.Items[i].StateIndex=pcPages.ActivePage.Tag then begin
      found := true;
      tvContents.Selected := tvContents.Items[i];
      break;
    end;
  if not found then
    tvContents.Selected := nil; //deselect
end;

function TfSettings.FindContentItemByTag(const ATag: integer): TTreeNode;
var i: integer;
begin
  Result := nil;
  for i := 0 to tvContents.Items.Count - 1 do
    if tvContents.Items[i].StateIndex=ATag then begin
      Result := tvContents.Items[i];
      break;
    end;
end;

function TfSettings.FindNextFreeTag: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to pcPages.PageCount - 1 do
    if pcPages.Pages[i].Tag >= Result then
      Result := pcPages.Pages[i].Tag + 1;
end;

procedure TfSettings.tvContentsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TfSettings.tvContentsChange(Sender: TObject; Node: TTreeNode);
var i: integer;
begin
  if (tvContents.Selected = nil) or (tvContents.Selected.StateIndex<0) then exit;
  for i := 0 to pcPages.PageCount - 1 do
    if pcPages.Pages[i].Tag=tvContents.Selected.StateIndex then begin
      pcPages.ActivePageIndex := i;
      break;
    end;
end;

procedure TfSettings.pcPagesChange(Sender: TObject);
begin
  SelectActiveContentItem();
end;

{ Custom settings pages }

procedure TfSettings.AddSettingsPage(page: TForm; category: string);
var parent, item: TTreeNode;
begin
  page.Visible := false; //чтобы не переключилось на него
  page.ManualDock(Self.pcPages, nil, alClient);
  page.Visible := true;
  Assert(page.Parent is TTabSheet);
  page.Parent.Tag := FindNextFreeTag;
  TTabSheet(page.Parent).TabVisible := false;

  if SameText(category, 'General') then
    parent := FindContentItemByTag(tsGeneral.Tag)
  else
  if SameText(category, 'Characters') then
    parent := FindContentItemByTag(tsCharacterList.Tag)
  else
  if SameText(category, 'Dictionary') then
    parent := FindContentItemByTag(tsDictionary.Tag)
  else
  if SameText(category, 'Editor') then
    parent := FindContentItemByTag(tsEditor.Tag)
  else
    parent := nil;

  item := tvContents.Items.AddChild(parent, page.Caption);
  item.StateIndex := page.Parent.Tag;
end;

//Broadcasts custom event message to all pages
procedure TfSettings.BroadcastToPages(Msg: cardinal; wParam: NativeUInt; lParam: NativeInt);
var i, j: integer;
begin
  for i := 0 to pcPages.PageCount-1 do
    for j := 0 to pcPages.Pages[i].ControlCount-1 do
      if pcPages.Pages[i].Controls[j] is TCustomForm then begin
        SendMessage(TCustomForm(pcPages.Pages[i].Controls[j]).Handle, Msg, wParam, lParam); //TODO: send to docked form, not the placeholder
      end;
end;






procedure TfSettings.LoadSettings;
{$IFNDEF AUTOTEST}
var ini: TCustomIniFile;
begin
  ini := GetSettingsStore;

 //Configure various FormPlacement components throught the application
 //-- but don't load placement just now.
  if PortabilityMode=pmPortable then begin
    fMenu.FormPlacement1.UseRegistry := false;
    fMenu.FormPlacement1.IniFileName := AppFolder + '\wakan.ini';
  end else begin
    fMenu.FormPlacement1.UseRegistry := true;
    fMenu.FormPlacement1.IniFileName := WakanRegKey;
  end;
  fMenu.FormPlacement1.IniSection := 'MainPos';
  if fKanjiDetails<>nil then begin
    fKanjiDetails.FormPlacement1.UseRegistry := fMenu.FormPlacement1.UseRegistry;
    fKanjiDetails.FormPlacement1.IniFileName := fMenu.FormPlacement1.IniFileName;
    fKanjiDetails.FormPlacement1.IniSection := 'DetailPos';
  end;
 //Placement must be configured before doing LoadRegistrySettings
 //because applying some settings requires being able to load Placement

  LoadRegistrySettings(ini);
  InitColors(ini);
  LoadCharDetl(ini);

  BroadcastToPages(WM_LOADSETTINGS, NativeUInt(ini), 0);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.SaveSettings;
{$IFNDEF AUTOTEST}
var ini: TCustomIniFile;
begin
  FreeSettings(); //trigger reload in case files were changed

  ini := GetWakanIni;
  case PortabilityMode of
    pmStandalone: ini.WriteString('General', 'Install', 'Standalone');
    pmPortable: ini.WriteString('General', 'Install', 'Portable');
  end;

  ini := GetSettingsStore;
  try
    SaveRegistrySettings(ini);
    WriteColors(ini);
    SaveCharDetl(ini);
    BroadcastToPages(WM_SAVESETTINGS, NativeUInt(ini), 0);
  finally
    ini.UpdateFile;
    FreeSettings();
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.LoadRegistrySettings(reg: TCustomIniFile);
{$IFNDEF AUTOTEST}
var s: string;
  tmp_int: integer;
  exmode:integer;
begin
  CheckBox46.Checked:=reg.ReadBool('Vocabulary','AutoSave',false);
  CheckBox70.Checked:=reg.ReadBool('Vocabulary','DisplayMessage',true);
  cbAutoSave.Checked:=reg.ReadBool('Vocabulary','AutoSaveTimer',true);
  edtAutoSavePeriod.Text:=inttostr(reg.ReadInteger('Vocabulary','AutoSavePeriod',DefaultAutoSavePeriod));
  CheckBox55.Checked:=reg.ReadBool('Vocabulary','MakeBackups',true);
  cbShowSplashscreen.Checked := reg.ReadBool('Vocabulary','ShowSplashscreen',true);

  ReloadRomaSystems; //can't do before we know Portability mode
  ReloadPinyinSystems;

 //Backward compatible setting
  tmp_int:=reg.ReadInteger('Romanization','System',-1);
  case tmp_int of
    0: SetRomaList(ChangeFileExt(KunreishikiRoma,''));
    1: SetRomaList(ChangeFileExt(HepburnRoma,''));
    2: SetRomaList(ChangeFileExt(CzechRoma,''));
  else
    SetRomaList(reg.ReadString('Romanization','RomaList',ChangeFileExt(KunreishikiRoma,'')+','+ChangeFileExt(HepburnRoma,'')));
  end;

  tmp_int:=reg.ReadInteger('Romanization','ChineseSystem',-1);
  case tmp_int of
    0: SetPinyinList(ChangeFileExt(PinYinRoma,''));
    1: SetPinyinList(ChangeFileExt(WadeGilesRoma,''));
    2: SetPinyinList(ChangeFileExt(YaleRoma,''));
  else
    SetPinyinList(reg.ReadString('Romanization','ChineseRomaList',ChangeFileExt(PinYinRoma,'')));
  end;

  rgShowKana.ItemIndex:=reg.ReadInteger('Romanization','ShowKana',0);
  rgShowBopomofo.ItemIndex:=reg.ReadInteger('Romanization','ShowBopomofo',1);
  jshowroma := Self.rgShowKana.ItemIndex=1;
  cshowroma := Self.rgShowBopomofo.ItemIndex=1;

  if fKanjiCompounds<>nil then begin
    fKanjiCompounds.cbLeftMatchOnly.Checked:=reg.ReadBool('Characters','CompoundsBeg',false);
    fKanjiCompounds.cbPopularOnly.Checked:=reg.ReadBool('Characters','CompoundsPop',true);
    fKanjiCompounds.cbSortByFrequency.Checked:=reg.ReadBool('Characters','CompoundsFreq',true);
  end;
  RadioGroup5.ItemIndex:=reg.ReadInteger('Characters','Chinese',0);
  rgKanjiGridSize.ItemIndex:=reg.ReadInteger('Characters','GridSize',1);
  cbRadicalType.ItemIndex:=reg.ReadInteger('Characters','RadicalType',0);
  CheckBox1.Checked:=reg.ReadBool('Characters','ShowStrokes',false);
  CheckBox51.Checked:=reg.ReadBool('Characters','StrokeOrderGridFont',false);
  cbNoCharColors.Checked:=reg.ReadBool('Characters','NoShowColors',false);
  cbYomiIgnoreOkurigana.Checked:=reg.ReadBool('Characters','YomiOkurigana',false);

  cbDetailsShowKanjiClass.Checked:=reg.ReadBool('KanjiDetails','ShowKanjiClass',true);
  cbDetailsKanjiInColor.Checked:=reg.ReadBool('KanjiDetails','KanjiInColor',true);
  rgDetailsCategoryEditorType.ItemIndex:=reg.ReadInteger('KanjiDetails','CategoryEditorType',0);
  cbDetailsShowLinks.Checked:=reg.ReadBool('KanjiDetails','ShowLinks',true);

  cbPreferUserWords.Checked:=reg.ReadBool('Dict','PreferUser',true);
  cbPreferNounsAndVerbs.Checked:=reg.ReadBool('Dict','PreferNouns',true);
  cbPreferPolite.Checked:=reg.ReadBool('Dict','PreferPolite',true);
  cbPreferPopular.Checked:=reg.ReadBool('Dict','PreferPopular',true);
  if fWordLookup<>nil then begin
    fWordLookup.aAutoPreview.Checked:=reg.ReadBool('Dict','QuickSearch',true);
    fWordLookup.aInflect.Checked:=reg.ReadBool('Dict','DeflexItalic',true);
    if reg.ReadBool('Dict','Meaning',false) then
      fWordLookup.LookupMode := lmEn
    else
      fWordLookup.LookupMode := lmAuto;
    case reg.ReadInteger('Dict','SearchBeg',0) of
      1: fWordLookup.aMatchLeft.Checked := true;
      2: fWordLookup.aMatchRight.Checked := true;
      3: fWordLookup.aMatchAnywhere.Checked := true;
    else fWordLookup.aMatchExact.Checked := true;
    end;
  end;
  cbReplaceKanji.Checked:=reg.ReadBool('Dict','ReplaceKanji',true);
  cbNoWordGridColors.Checked:=reg.ReadBool('Dict','NoUseColors',false);
  CheckBox10.Checked:=reg.ReadBool('Dict','UseGrey',false);
  cbStatusColors.Checked:=reg.ReadBool('Dict','StatusColors',true);
  cbDictLimitAutoResults.Checked:=reg.ReadBool('Dict','AutoPage',true);
  CheckBox49.Checked:=reg.ReadBool('Dict','DemandLoad',true);
  CheckBox50.Checked:=reg.ReadBool('Dict','AutoExamples',true);
  cbMultilineGrids.Checked:=reg.ReadBool('Dict','MultiLineGrids',true);
  cbShowFreq.Checked:=reg.ReadBool('Dict','ShowFreq',false);
  cbOrderFreq.Checked:=reg.ReadBool('Dict','OrderFreq',true);
  cbDictRefLinksInSubmenu.Checked:=reg.ReadBool('Dict','RefLinksInSubmenu',false);

  cbEditorAutosave.Checked:=reg.ReadBool('Editor','AutoSave',false);
  cbEditorAutoloadLast.Checked:=reg.ReadBool('Editor','AutoLoad',false);
  cbNoSaveChangesWarning.Checked:=reg.ReadBool('Editor','NoSaveChangesWarning',false);
  cbLoadAozoraRuby.Checked:=reg.readBool('Editor','LoadAozoraRuby', true);
  cbAozoraTagsInColor.Checked:=reg.readBool('Editor','AozoraTagsInColor', true);
  cbSaveAnnotationsToRuby.Checked:=reg.readBool('Editor','SaveAnnotationsToRuby', false);
  cbAdjustCharPriorities.Checked:=reg.readBool('Editor','AdjustCharPriorities', true);
  rgReleaseCursorMode.ItemIndex := reg.ReadInteger('Editor','ReleaseCursorMode',0);
  if (fEditor<>nil) and cbEditorAutoloadLast.Checked then begin
    fEditor.DocFilename := Reg.ReadString('Editor','DocFileName',''); //Will load later if DocFileName<>''
    fEditor.DocType := TDocType(Reg.ReadInteger('Editor','DocType',0));
    fEditor.DocEncoding := FindEncodingByClassName(Reg.ReadString('Editor','DocEncoding',''));
  end;
  Edit34.Text:=inttostr(reg.ReadInteger('Characters','FreqLimit',0));
  exmode:=reg.ReadInteger('Dict','ExMode',0);
  if fExamples<>nil then begin
    fExamples.btnRandomOrder.Down:=reg.ReadBool('Dict','RandomExamples',false);
    if exmode=0 then fExamples.btnDisplayTranslation.Down:=true;
    if exmode=1 then fExamples.btnUseBigFont.Down:=true;
    if exmode=2 then fExamples.btnUseSmallFont.Down:=true;
  end;
  Edit25.Text:=inttostr(reg.ReadInteger('Dict','FontSize',14));
  DefaultExprCopyFormatName:=reg.ReadString('Dict','CopyFormat','');
  DefaultKanjiCopyFormatName:=reg.ReadString('Characters','CopyFormat','');
  GridFontSize:=strtoint(Edit25.text);
  lbWordPrintFormat.ItemIndex:=reg.ReadInteger('WordSheet','Columns',0);
  cbInsideLines.Checked:=reg.ReadBool('WordSheet','InsideLines',true);
  cbOutsideLines.Checked:=reg.ReadBool('WordSheet','OutsideLines',true);
  CheckBox16.Checked:=reg.ReadBool('WordSheet','VaryColors',true);
  CheckBox17.Checked:=reg.ReadBool('WordSheet','PrintUnlearned',true);
  Edit10.Text:=inttostr(reg.ReadInteger('WordSheet','NoLines',40));
  Edit16.Text:=reg.ReadString('WordSheet','UserColumns','p1--m1--');
  dicts.NotUsedDicts:=reg.ReadString('Dict','NotUsedDicts','');
  dicts.NotGroupDicts[1]:=reg.ReadString('Dict','NotGroup1Dicts','');
  dicts.NotGroupDicts[2]:=reg.ReadString('Dict','NotGroup2Dicts','');
  dicts.NotGroupDicts[3]:=reg.ReadString('Dict','NotGroup3Dicts','');
  dicts.NotGroupDicts[4]:=reg.ReadString('Dict','NotGroup4Dicts','');
  dicts.NotGroupDicts[5]:=reg.ReadString('Dict','NotGroup5Dicts','');
  dicts.OfflineDicts:=reg.ReadString('Dict','OfflineDicts','');
  dicts.Priority.Text := repl(reg.ReadString('Dict','Priority',''),',',#13#10);
  edtKanjiCardRowsOnPage.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCharacters',10));
  edtKanjiCardAdditionalWidth.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCompoundsH',10));
  edtKanjiCardCharSize.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCompoundsV',6));
  edtKanjiCardFullCompounds.Text:=inttostr(reg.ReadInteger('KanjiCards','NoFullCompounds',4));
  edtKanjiCardFont.Text:=reg.ReadString('KanjiCards','Font','MingLiU');
  cbKanjiCardPrintCompounds.Checked:=reg.ReadBool('KanjiCards','PrintCompounds',true);
  cbKanjiCardPrintRadical.Checked:=reg.ReadBool('KanjiCards','PrintRadical',true);
  cbKanjiCardPrintAlternate.Checked:=reg.ReadBool('KanjiCards','PrintAlternate',true);
  cbKanjiCardPrintReadings.Checked:=reg.ReadBool('KanjiCards','PrintReadings',true);
  cbKanjiCardPrintOuterLines.Checked:=reg.ReadBool('KanjiCards','PrintOuterLines',true);
  cbKanjiCardPrintInnerLines.Checked:=reg.ReadBool('KanjiCards','PrintInnerLines',true);
  cbKanjiCardPrintVertical.Checked:=reg.ReadBool('KanjiCards','PrintVertical',true);
  cbKanjiCardColumnSpace.Checked:=reg.ReadBool('KanjiCards','ColumnSpace',true);
  cbKanjiCardPrintDefinition.Checked:=reg.ReadBool('KanjiCards','PrintDefinition',true);
  cbKanjiCardPrintStrokeCount.Checked:=reg.ReadBool('KanjiCards','PrintStrokeCount',false);
  cbKanjiCardPrintStrokeOrder.Checked:=reg.ReadBool('KanjiCards','PrintStrokeOrder',false);
  cbKanjiCardPrintFullComp.Checked:=reg.ReadBool('KanjiCards','PrintFullComp',true);
  cbKanjiCardSortFrequency.Checked:=reg.ReadBool('KanjiCards','SortFrequency',true);
  CheckBox26.Checked:=reg.ReadBool('Vocabulary','SaveStat',false);
  CheckBox43.Checked:=reg.ReadBool('Translate','BreakLines',true);
  cbDisplayLines.Checked:=reg.ReadBool('Translate','DisplayLines',true);
  CheckBox41.Checked:=reg.ReadBool('Translate','DisplayNonJapanese',true);
  cbNoMeaningLearned.Checked:=reg.ReadBool('Translate','NoMeaningLearned',false);
  cbNoReadingLearned.Checked:=reg.ReadBool('Translate','NoReadingLearned',false);
  cbReadingKatakana.Checked:=reg.ReadBool('Translate','ReadingKatakana',true);
  cbNoSearchParticles.Checked:=reg.ReadBool('Translate','NoSearchParticles',false);
  cbNoTranslateHiragana.Checked:=reg.ReadBool('Translate','NoTranslateHiragana',false);
  cbNoEditorColors.Checked:=reg.ReadBool('Translate','NoUseColors',false);
  cbUserBold.Checked:=reg.ReadBool('Translate','UserBold',true);
  cbSpaceBetweenLines.Checked:=reg.ReadBool('Translate','LeaveSpace',false);
  cbReserveSpaceForReading.Checked:=reg.ReadBool('Translate','LeaveSpaceAlways',true);
  CheckBox27.Checked:=reg.ReadBool('Translate','HalfSizeMeaning',false);
  cbPrintReading.Checked:=reg.ReadBool('Translate','PrintReading',true);
  cbPrintMeaning.Checked:=reg.ReadBool('Translate','PrintMeaning',true);
  cbNoPrintColors.Checked:=reg.ReadBool('Translate','NoPrintColors',true);
  cbVerticalPrint.Checked:=reg.ReadBool('Translate','VerticalPrint',false);
  cbTranslateNoLongTextWarning.Checked := reg.ReadBool('Translate','NoLongTextWarning',true);
  cbMultithreadedTranslation.Checked := reg.ReadBool('Translate','MultithreadedTranslation',true);
  cbScreenTipForJapanese.Checked:=reg.ReadBool('ScreenTrans','Japanese',true);
  cbScreenTipForEnglish.Checked:=reg.ReadBool('ScreenTrans','English',true);
  cbScreenTipForKanji.Checked:=reg.ReadBool('ScreenTrans','Kanji',true);
  Edit21.Text:=reg.ReadString('ScreenTrans','Delay','10');
  Edit22.Text:=reg.ReadString('ScreenTrans','LeftRange','20');
  Edit23.Text:=reg.ReadString('ScreenTrans','RightRange','100');
  edtScreenTipMaxDictEntries.Text:=reg.ReadString('ScreenTrans','DictEntries','4');
  edtScreenTipSizeFactor.Text:=reg.ReadString('ScreenTrans','SizeFactor','16');
  edtScreenTipMinCompounds.Text:=reg.ReadString('ScreenTrans','MinCompounds','10');
  edtScreenTipMaxCompounds.Text:=reg.ReadString('ScreenTrans','MaxCompounds','40');
  fMenu.btnScreenModeWk.Down:=reg.ReadBool('ScreenTrans','WakanToolTip',true);
  ScreenTip.EnabledInWakan:=fMenu.btnScreenModeWk.Down;
  if fEditor<>nil then begin
    fEditor.aDisplayReading.Checked:=reg.ReadBool('Translate','Reading',true);
    fEditor.aDisplayMeaning.Checked:=reg.ReadBool('Translate','Meaning',true);
    fEditor.aUseColors.Checked:=reg.ReadBool('Translate','TransColors',true);
    fEditor.aFullwidthLatin.Checked:=reg.ReadBool('Translate','FullwidthLatin',true);
    fEditor.sbDockDictionary.Down:=reg.ReadBool('Translate','Dictionary',false);
    tmp_int := reg.ReadInteger('Translate','FontSizeInt',0);
    if tmp_int>0 then
      fEditor.FontSize := tmp_int
    else
      case reg.ReadInteger('Translate','FontSize',2) of
        0:fEditor.FontSize := FontSizeSmall;
        1:fEditor.FontSize := FontSizeMedium;
        2:fEditor.FontSize := FontSizeLarge;
      end;
  end;
  edtMeaningLines.Text:=reg.ReadString('Translate','MeaningLines','2');
  edtPrintLines.Text:=reg.ReadString('Translate','PrintLines','20');
  setUserCompounds:=reg.ReadBool('Characters','UserCompounds',false);

  cbShowEditorHint.Checked:=reg.ReadBool('Translate','ShowHint',true);
  cbHintMeaning.Checked:=reg.ReadBool('Translate','HintMeaning',true);
  s:=reg.ReadString('Dict','CurLanguage','j');
  if Length(s)>=1 then curlang:=s[1] else curlang:='j';
  lbWordPrintFormatClick(self);

  cbSaveColumnWidths.Checked:=reg.ReadBool('General','SaveColumnWidths',true);
  cbSaveSearchParams.Checked:=reg.ReadBool('General','SaveSearchParams',true);

 //Column widths
  if fVocab<>nil then
    fVocab.SetDefaultColumnWidths;
  if fWordLookup<>nil then
    fWordLookup.SetDefaultColumnWidths;
  if fKanjiCompounds<>nil then
    fKanjiCompounds.SetDefaultColumnWidths;
  if cbSaveColumnWidths.Checked then begin
    if fVocab<>nil then begin
      fVocab.StringGrid1.ColWidths[0]:=reg.ReadInteger('Grids','UserCol1',fVocab.StringGrid1.ColWidths[0]);
      fVocab.StringGrid1.ColWidths[1]:=reg.ReadInteger('Grids','UserCol2',fVocab.StringGrid1.ColWidths[1]);
      fVocab.StringGrid1.ColWidths[2]:=reg.ReadInteger('Grids','UserCol3',fVocab.StringGrid1.ColWidths[2]);
      fVocab.StringGrid1.ColWidths[3]:=reg.ReadInteger('Grids','UserCol4',fVocab.StringGrid1.ColWidths[3]);
    end;

    if fWordLookup<>nil then begin
      fWordLookup.StringGrid.ColWidths[0]:=reg.ReadInteger('Grids','DictCol1',fWordLookup.StringGrid.ColWidths[0]);
      fWordLookup.StringGrid.ColWidths[1]:=reg.ReadInteger('Grids','DictCol2',fWordLookup.StringGrid.ColWidths[1]);
      fWordLookup.StringGrid.ColWidths[2]:=reg.ReadInteger('Grids','DictCol3',fWordLookup.StringGrid.ColWidths[2]);
    end;

    if fKanjiCompounds<>nil then begin
      fKanjiCompounds.StringGrid.ColWidths[0]:=reg.ReadInteger('Grids','KanjiCompCol1',fKanjiCompounds.StringGrid.ColWidths[0]);
      fKanjiCompounds.StringGrid.ColWidths[1]:=reg.ReadInteger('Grids','KanjiCompCol2',fKanjiCompounds.StringGrid.ColWidths[1]);
      fKanjiCompounds.StringGrid.ColWidths[2]:=reg.ReadInteger('Grids','KanjiCompCol3',fKanjiCompounds.StringGrid.ColWidths[2]);
    end;
  end;

  //Search params
  if cbSaveSearchParams.Checked and (fKanji<>nil) then
    fKanji.LoadSettings(reg);
   //else they're empty by default

  setPortraitMode := reg.ReadBool('Layout','PortraitMode',false);

 //Panel sizes
  if fVocabFilters<>nil then begin
    fVocabFilters.ClientWidth := reg.ReadInteger('Layout','UserFiltersWidth',192);
    fVocabFilters.ClientHeight := reg.ReadInteger('Layout','UserFiltersHeight',120);
  end;
  if fVocabDetails<>nil then begin
    fVocabDetails.ClientHeight := reg.ReadInteger('Layout','UserDetailsHeight',120);
  end;
  if fKanjiCompounds<>nil then begin
   //Read into ClientHeight, then copy to fMenu.KanjiCompoundsDockedHeight in fMenu.ApplyUI
    fKanjiCompounds.ClientHeight := reg.ReadInteger('Layout','KanjiCompoundsHeight',178);
  end;

 //Read UI settings to apply later
  CharDetDocked := reg.ReadBool('Layout','CharDetailsDocked',false);
  CharDetDockedVis1 := reg.ReadBool('Layout','CharDetailsVisible1',true);
  CharDetDockedVis2 := reg.ReadBool('Layout','CharDetailsVisible2',true);
  SetLayout := reg.ReadInteger('Layout','DisplayLayout',1);
  SetWindows := reg.ReadInteger('Layout','SecondaryWindows',72);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.SaveRegistrySettings(reg: TCustomIniFile);
{$IFNDEF AUTOTEST}
var setwindows:integer;
  exmode:integer;
  tmp_str: string;
  tmp_int: integer;
begin
  reg.WriteBool('Vocabulary','AutoSave',CheckBox46.Checked);
  reg.WriteBool('Vocabulary','DisplayMessage',CheckBox70.Checked);
  reg.WriteBool('Vocabulary','AutoSaveTimer',cbAutoSave.Checked);
  reg.WriteInteger('Vocabulary','AutoSavePeriod',StrToIntDef(edtAutoSavePeriod.text,DefaultAutoSavePeriod));
  reg.WriteBool('Vocabulary','MakeBackups',CheckBox55.Checked);
  reg.WriteBool('Vocabulary','ShowSplashscreen',cbShowSplashscreen.Checked);

  tmp_str := GetRomaList;
  reg.WriteString('Romanization','RomaList',tmp_str);
 //Also write older param or we'll always read the inherited value
  if tmp_str=KunreishikiRoma then
    reg.WriteInteger('Romanization','System',0)
  else
  if tmp_str=HepburnRoma then
    reg.WriteInteger('Romanization','System',1)
  else
  if tmp_str=CzechRoma then
    reg.WriteInteger('Romanization','System',2)
  else
    reg.DeleteKey('Romanization','System');

  tmp_str := GetPinyinList;
  reg.WriteString('Romanization','ChineseRomaList',tmp_str);
 //Also write older param or we'll always read the inherited value
  if tmp_str=PinYinRoma then
    reg.WriteInteger('Romanization','ChineseSystem',0)
  else
  if tmp_str=WadeGilesRoma then
    reg.WriteInteger('Romanization','ChineseSystem',1)
  else
  if tmp_str=YaleRoma then
    reg.WriteInteger('Romanization','ChineseSystem',2)
  else
    reg.DeleteKey('Romanization','ChineseSystem');

  reg.WriteInteger('Romanization','ShowKana',rgShowKana.ItemIndex);
  reg.WriteInteger('Romanization','ShowBopomofo',rgShowBopomofo.ItemIndex);

  reg.WriteInteger('Characters','Chinese',RadioGroup5.ItemIndex);
  reg.WriteInteger('Characters','GridSize',rgKanjiGridSize.ItemIndex);
  reg.WriteInteger('Characters','RadicalType',cbRadicalType.ItemIndex);
  reg.WriteBool('Characters','ShowStrokes',CheckBox1.Checked);
  reg.WriteBool('Characters','StrokeOrderGridFont',CheckBox51.Checked);
  reg.WriteBool('Characters','NoShowColors',cbNoCharColors.Checked);
  reg.WriteBool('Characters','YomiOkurigana',cbYomiIgnoreOkurigana.Checked);
  reg.WriteBool('Characters','CompoundsBeg',fKanjiCompounds.cbLeftMatchOnly.Checked);
  reg.WriteBool('Characters','CompoundsPop',fKanjiCompounds.cbPopularOnly.Checked);
  reg.WriteBool('Characters','CompoundsFreq',fKanjiCompounds.cbSortByFrequency.Checked);

  reg.WriteBool('KanjiDetails','ShowKanjiClass',cbDetailsShowKanjiClass.Checked);
  reg.WriteBool('KanjiDetails','KanjiInColor',cbDetailsKanjiInColor.Checked);
  reg.WriteInteger('KanjiDetails','CategoryEditorType',rgDetailsCategoryEditorType.ItemIndex);
  reg.WriteBool('KanjiDetails','ShowLinks',cbDetailsShowLinks.Checked);

  reg.WriteBool('Dict','PreferUser',cbPreferUserWords.Checked);
  reg.WriteBool('Dict','PreferNouns',cbPreferNounsAndVerbs.Checked);
  reg.WriteBool('Dict','PreferPolite',cbPreferPolite.Checked);
  reg.WriteBool('Dict','PreferPopular',cbPreferPopular.Checked);
  if fWordLookup<>nil then begin
    reg.WriteBool('Dict','QuickSearch',fWordLookup.aAutoPreview.Checked);
    reg.WriteBool('Dict','DeflexItalic',fWordLookup.aInflect.Checked);
    reg.WriteBool('Dict','Meaning',fWordLookup.LookupMode=lmEn);
    if fWordLookup.aMatchLeft.Checked then tmp_int := 1 else
    if fWordLookup.aMatchRight.Checked then tmp_int := 2 else
    if fWordLookup.aMatchAnywhere.Checked then tmp_int := 3 else
      tmp_int := 0;
    reg.WriteInteger('Dict','SearchBeg',tmp_int);
  end;
  reg.WriteBool('Dict','ReplaceKanji',cbReplaceKanji.Checked);
  reg.WriteBool('Dict','NoUseColors',cbNoWordGridColors.Checked);
  reg.WriteBool('Dict','UseGrey',CheckBox10.Checked);
  reg.WriteBool('Dict','StatusColors',cbStatusColors.Checked);
  reg.WriteBool('Dict','AutoPage',cbDictLimitAutoResults.Checked);
  reg.WriteBool('Dict','DemandLoad',CheckBox49.Checked);
  reg.WriteBool('Dict','AutoExamples',CheckBox50.Checked);
  reg.WriteBool('Dict','RandomExamples',fExamples.btnRandomOrder.Down);
  reg.WriteBool('Dict','ShowFreq',cbShowFreq.Checked);
  reg.WriteBool('Dict','OrderFreq',cbOrderFreq.Checked);
  reg.WriteBool('Dict','RefLinksInSubmenu',cbDictRefLinksInSubmenu.Checked);

  reg.WriteBool('Editor','AutoSave',cbEditorAutosave.Checked);
  reg.WriteBool('Editor','AutoLoad',cbEditorAutoloadLast.Checked);
  reg.WriteBool('Editor','NoSaveChangesWarning',cbNoSaveChangesWarning.Checked);
  reg.WriteBool('Editor','LoadAozoraRuby',cbLoadAozoraRuby.Checked);
  reg.WriteBool('Editor','AozoraTagsInColor',cbAozoraTagsInColor.Checked);
  reg.WriteBool('Editor','SaveAnnotationsToRuby',cbSaveAnnotationsToRuby.Checked);
  reg.WriteBool('Editor','AdjustCharPriorities',cbAdjustCharPriorities.Checked);
  reg.WriteInteger('Editor','ReleaseCursorMode',rgReleaseCursorMode.ItemIndex);
  if fEditor<>nil then begin
    reg.WriteString('Editor','DocFilename',fEditor.DocFilename); //For autoload
    reg.WriteInteger('Editor','DocType',integer(fEditor.DocType)); //This too.
    if fEditor.DocEncoding<>nil then
      reg.WriteString('Editor','DocEncoding',string(fEditor.DocEncoding.Classname))
    else
      reg.WriteString('Editor','DocEncoding','');
  end;
  reg.WriteInteger('Characters','FreqLimit',strtoint(Edit34.Text));
  if fExamples.btnDisplayTranslation.Down then exmode:=0 else
  if fExamples.btnUseBigFont.Down then exmode:=1 else
  if fExamples.btnUseSmallFont.Down then exmode:=2 else
    exmode := 0;
  reg.WriteInteger('Dict','ExMode',exmode);
  reg.WriteInteger('Dict','FontSize',strtoint(Edit25.text));
  reg.WriteBool('Dict','MultiLineGrid',cbMultilineGrids.Checked);
  reg.WriteString('Dict','CopyFormat',DefaultExprCopyFormatName);
  reg.WriteString('Characters','CopyFormat',DefaultKanjiCopyFormatName);
  reg.WriteInteger('WordSheet','Columns',lbWordPrintFormat.ItemIndex);
  reg.WriteBool('WordSheet','InsideLines',cbInsideLines.Checked);
  reg.WriteBool('WordSheet','OutsideLines',cbOutsideLines.Checked);
  reg.WriteBool('WordSheet','VaryColors',CheckBox16.Checked);
  reg.WriteBool('WordSheet','PrintUnlearned',CheckBox17.Checked);
  reg.WriteInteger('WordSheet','NoLines',strtoint(Edit10.Text));
  reg.WriteString('WordSheet','UserColumns',Edit16.Text);
  reg.WriteInteger('KanjiCards','NoCharacters',strtoint(edtKanjiCardRowsOnPage.Text));
  reg.WriteInteger('KanjiCards','NoCompoundsH',strtoint(edtKanjiCardAdditionalWidth.Text));
  reg.WriteInteger('KanjiCards','NoCompoundsV',strtoint(edtKanjiCardCharSize.Text));
  reg.WriteInteger('KanjiCards','NoFullCompounds',strtoint(edtKanjiCardFullCompounds.Text));
  reg.WriteString('KanjiCards','Font',edtKanjiCardFont.text);
  reg.WriteBool('KanjiCards','PrintCompounds',cbKanjiCardPrintCompounds.Checked);
  reg.WriteBool('KanjiCards','PrintRadical',cbKanjiCardPrintRadical.Checked);
  reg.WriteBool('KanjiCards','PrintAlternate',cbKanjiCardPrintAlternate.Checked);
  reg.WriteBool('KanjiCards','PrintReadings',cbKanjiCardPrintReadings.Checked);
  reg.WriteBool('KanjiCards','PrintOuterLines',cbKanjiCardPrintOuterLines.Checked);
  reg.WriteBool('KanjiCards','PrintInnerLines',cbKanjiCardPrintInnerLines.Checked);
  reg.WriteBool('KanjiCards','PrintVertical',cbKanjiCardPrintVertical.Checked);
  reg.WriteBool('KanjiCards','PrintStrokeOrder',cbKanjiCardPrintStrokeOrder.Checked);
  reg.WriteBool('KanjiCards','ColumnSpace',cbKanjiCardColumnSpace.Checked);
  reg.WriteBool('KanjiCards','PrintDefinition',cbKanjiCardPrintDefinition.Checked);
  reg.WriteBool('KanjiCards','PrintStrokeCount',cbKanjiCardPrintStrokeCount.Checked);
  reg.WriteBool('KanjiCards','PrintFullComp',cbKanjiCardPrintFullComp.Checked);
  reg.WriteBool('KanjiCards','SortFrequency',cbKanjiCardSortFrequency.Checked);
  reg.WriteBool('Vocabulary','SaveStat',CheckBox26.Checked);
  reg.WriteBool('Translate','BreakLines',CheckBox43.Checked);
  reg.WriteBool('Translate','DisplayLines',cbDisplayLines.Checked);
  reg.WriteBool('Translate','DisplayNonJapanese',CheckBox41.Checked);
  reg.WriteBool('Translate','NoMeaningLearned',cbNoMeaningLearned.Checked);
  reg.WriteBool('Translate','NoReadingLearned',cbNoReadingLearned.Checked);
  reg.WriteBool('Translate','ReadingKatakana',cbReadingKatakana.Checked);
  reg.WriteBool('Translate','NoSearchParticles',cbNoSearchParticles.Checked);
  reg.WriteBool('Translate','NoTranslateHiragana',cbNoTranslateHiragana.Checked);
  reg.WriteBool('Translate','NoUseColors',cbNoEditorColors.Checked);
  reg.WriteBool('Translate','UserBold',cbUserBold.Checked);
  reg.WriteBool('Translate','LeaveSpace',cbSpaceBetweenLines.Checked);
  reg.WriteBool('Translate','LeaveSpaceAlways',cbReserveSpaceForReading.Checked);
  reg.WriteBool('Translate','HalfSizeMeaning',CheckBox27.Checked);
  reg.WriteBool('Translate','PrintReading',cbPrintReading.Checked);
  reg.WriteBool('Translate','PrintMeaning',cbPrintMeaning.Checked);
  reg.WriteBool('Translate','NoPrintColors',cbNoPrintColors.Checked);
  reg.WriteBool('Translate','VerticalPrint',cbVerticalPrint.Checked);
  reg.WriteBool('Translate','NoLongTextWarning',cbTranslateNoLongTextWarning.Checked);
  reg.WriteBool('Translate','MultithreadedTranslation',cbMultithreadedTranslation.Checked);
  if fEditor<>nil then begin
    reg.WriteBool('Translate','Reading',fEditor.aDisplayReading.Checked);
    reg.WriteBool('Translate','Meaning',fEditor.aDisplayMeaning.Checked);
    reg.WriteBool('Translate','TransColors',fEditor.aUseColors.Checked);
    reg.WriteBool('Translate','FullwidthLatin',fEditor.aFullwidthLatin.Checked);
    reg.WriteBool('Translate','Dictionary',fEditor.sbDockDictionary.Down);
    reg.WriteInteger('Translate','FontSizeInt',fEditor.FontSize);
    //These values are used only by Wakan previous version, but let's play nice
    //and update those too to the best that we can.
    if fEditor.FontSize<=((FontSizeMedium+FontSizeSmall) div 2) then
      reg.WriteInteger('Translate','FontSize',0)
    else
    if fEditor.FontSize<=((FontSizeLarge+FontSizeMedium) div 2) then
      reg.WriteInteger('Translate','FontSize',1)
    else
      reg.WriteInteger('Translate','FontSize',2);
  end;
  reg.WriteString('Translate','MeaningLines',edtMeaningLines.text);
  reg.WriteString('Translate','PrintLines',edtPrintLines.text);
  reg.WriteString('Dict','NotUsedDicts',dicts.NotUsedDicts);
  reg.WriteString('Dict','NotGroup1Dicts',dicts.NotGroupDicts[1]);
  reg.WriteString('Dict','NotGroup2Dicts',dicts.NotGroupDicts[2]);
  reg.WriteString('Dict','NotGroup3Dicts',dicts.NotGroupDicts[3]);
  reg.WriteString('Dict','NotGroup4Dicts',dicts.NotGroupDicts[4]);
  reg.WriteString('Dict','NotGroup5Dicts',dicts.NotGroupDicts[5]);
  reg.WriteString('Dict','OfflineDicts',dicts.OfflineDicts);
  reg.WriteString('Dict','CurLanguage',curlang);
  reg.WriteString('Dict','Priority',repl(dicts.Priority.Text,#13#10,','));
{
  for i := 0 to dicts.Priority.Count - 1 do
    reg.WriteString('DictPriority', IntToStr(i), dicts.Priority[i]);
}
  reg.WriteBool('Characters','UserCompounds',fKanjiCompounds.sbShowVocab.Down);

  reg.WriteBool('Translate','ShowHint',cbShowEditorHint.Checked);
  reg.WriteBool('Translate','HintMeaning',cbHintMeaning.Checked);
  reg.WriteInteger('Layout','DisplayLayout',fMenu.DisplayMode);
  reg.WriteBool('Layout','CharDetailsDocked',fMenu.CharDetDocked);
  reg.WriteBool('Layout','CharDetailsVisible1',fMenu.CharDetDockedVis1);
  reg.WriteBool('Layout','CharDetailsVisible2',fMenu.CharDetDockedVis2);
  reg.WriteBool('ScreenTrans','Japanese',cbScreenTipForJapanese.Checked);
  reg.WriteBool('ScreenTrans','English',cbScreenTipForEnglish.Checked);
  reg.WriteBool('ScreenTrans','Kanji',cbScreenTipForKanji.Checked);
  reg.WriteString('ScreenTrans','Delay',Edit21.Text);
  reg.WriteString('ScreenTrans','LeftRange',Edit22.Text);
  reg.WriteString('ScreenTrans','RightRange',Edit23.Text);
  reg.WriteString('ScreenTrans','DictEntries',edtScreenTipMaxDictEntries.Text);
  reg.WriteString('ScreenTrans','SizeFactor',edtScreenTipSizeFactor.Text);
  reg.WriteString('ScreenTrans','MinCompounds',edtScreenTipMinCompounds.Text);
  reg.WriteString('ScreenTrans','MaxCompounds',edtScreenTipMaxCompounds.Text);
  reg.WriteBool('ScreenTrans','WakanToolTip',fMenu.btnScreenModeWk.Down);

  setwindows:=0;
  if fKanji.aSearch.Checked then inc(setwindows,1);
  if fMenu.aKanjiCompounds.Checked then inc(setwindows,2);
  if fMenu.aDictKanji.Checked then inc(setwindows,4);
  if fMenu.aDictExamples.Checked then inc(setwindows,8);
  if fMenu.aVocabExamples.Checked then inc(setwindows,16);
  if fMenu.aVocabDetails.Checked then inc(setwindows,32);
  if fMenu.aVocabSettings.Checked then inc(setwindows,64);
  if fKanjiDetails.Visible then inc(setwindows,128);
  reg.WriteInteger('Layout','SecondaryWindows',setwindows);

  reg.WriteBool('General','SaveColumnWidths',cbSaveColumnWidths.Checked);
  reg.WriteBool('General','SaveSearchParams',cbSaveSearchParams.Checked);

  if cbSaveColumnWidths.Checked then begin
    reg.WriteInteger('Grids','DictCol1',fWordLookup.StringGrid.ColWidths[0]);
    reg.WriteInteger('Grids','DictCol2',fWordLookup.StringGrid.ColWidths[1]);
    reg.WriteInteger('Grids','DictCol3',fWordLookup.StringGrid.ColWidths[2]);

    reg.WriteInteger('Grids','UserCol1',fVocab.StringGrid1.ColWidths[0]);
    reg.WriteInteger('Grids','UserCol2',fVocab.StringGrid1.ColWidths[1]);
    reg.WriteInteger('Grids','UserCol3',fVocab.StringGrid1.ColWidths[2]);
    reg.WriteInteger('Grids','UserCol4',fVocab.StringGrid1.ColWidths[3]);

    reg.WriteInteger('Grids','KanjiCompCol1',fKanjiCompounds.StringGrid.ColWidths[0]);
    reg.WriteInteger('Grids','KanjiCompCol2',fKanjiCompounds.StringGrid.ColWidths[1]);
    reg.WriteInteger('Grids','KanjiCompCol3',fKanjiCompounds.StringGrid.ColWidths[2]);
  end;

  if cbSaveSearchParams.Checked and (fKanji<>nil) then
    fKanji.SaveSettings(reg);

  reg.WriteBool('Layout','PortraitMode',fMenu.aPortraitMode.Checked);

 //These are updated with the actual docked width and height on resizes,
 //while ClientWidth and ClientHeight might be invalid because the window's aligned to a full parent width or height
  reg.WriteInteger('Layout','UserFiltersWidth',fVocabFilters.UndockWidth);
  reg.WriteInteger('Layout','UserFiltersHeight',fVocabFilters.UndockHeight);
  reg.WriteInteger('Layout','UserDetailsHeight',fVocabDetails.UndockHeight);
  reg.WriteInteger('Layout','KanjiCompoundsHeight',fMenu.KanjiCompoundsDockedHeight);
{$ELSE}
begin
{$ENDIF}
end;


procedure TfSettings.btnChangeLanguageClick(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  fLanguage.SelectLanguage;
{$ENDIF}
end;


procedure TfSettings.Button1Click(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  if SaveDialog2.Execute then
  begin
    Screen.Cursor:=crHourGlass;
    fMenu.ExportUserData(SaveDialog2.FileName);
    Screen.Cursor:=crDefault;
    Application.MessageBox(
      pchar(_l('#00568^eExport was finished.')),
      'Export',
      MB_ICONINFORMATION or MB_OK);
  end;
{$ENDIF}
end;

procedure TfSettings.Button2Click(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  Screen.Cursor:=crHourglass;
  if (TChar.CheckIndex) and (TCharProp.CheckIndex) and (TRadicals.CheckIndex) then
    Application.MessageBox(
      pchar(_l('#00569^eIndexes are okay.')),
      pchar(_l('#00570^eIndex check')),
      MB_OK or MB_ICONINFORMATION)
  else
    Application.MessageBox(
      pchar(_l('#00571^eIndexes are damaged. Replace the dictionary file WAKAN.CHR.')),
      pchar(_l('#00570^eIndex check')),
      MB_OK or MB_ICONERROR);
  Screen.Cursor:=crDefault;
{$ENDIF}
end;

procedure TfSettings.Button3Click(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  if OpenDialog2.Execute then
  if Application.MessageBox(
    pchar(_l('#00572^eThis action replaces all data in user database.'#13
      +'It can take a long time.'#13#13'Do you want to continue?')),
    pchar(_l('#00573^eWarning')),
    MB_ICONWARNING or MB_YESNO)=idYes then
  begin
    fMenu.ImportUserData(OpenDialog2.FileName);
    Application.MessageBox(
      pchar(_l('#00574^eImport was finished.')),
      'Import',
      MB_ICONINFORMATION or MB_OK);
  end;
{$ENDIF}
end;

procedure TfSettings.lbWordPrintFormatClick(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  Edit16.Enabled:=false;
  Edit16.Color:=clBtnFace;
  case lbWordPrintFormat.ItemIndex of
    0:Edit16.Text:='p1--m1--';
    1:Edit16.Text:='w1--m1--';
    2:Edit16.Text:='p1w1m1--';
    3:Edit16.Text:='w1p1m1--';
    4:Edit16.Text:='r1k1m1--';
    5:Edit16.Text:='p1--p2--';
    6:Edit16.Text:='w1--w2--';
    7:Edit16.Text:='m1--m2--';
    8:Edit16.Text:='p1p2p3p4';
    9:Edit16.Text:='w1w2w3w4';
    10:Edit16.Text:='m1m2m3m4';
    11:Edit16.Text:='p1w1p2w2';
    12:Edit16.Text:='p1m1p2m2';
    13:Edit16.Text:='w1m1w2m2';
    14:begin Edit16.Enabled:=true; Edit16.Color:=clWindow; end;
  end;
{$ENDIF}
end;

procedure TfSettings.Button4Click(Sender: TObject);
{$IFNDEF AUTOTEST}
var lfound:boolean;
    gfound,gmore:boolean;
    lfn,gfn,gmn:integer;
    lfs,gfs,gms:string;
    s:string;
    ssum:string;
    word:integer;
begin
  if Application.MessageBox(
    pchar(_l('#00575^eThis function tests whether all the words in user vocabulary'#13
      +'are in exactly one group category and at least in one lesson category.'#13#13
      +'Do you want to continue?')),
    pchar(_l('#00576^eCheck categories')),
    MB_ICONINFORMATION or MB_YESNO)=idYes then
  begin
      Screen.Cursor:=crHourGlass;
      TUser.First;
      lfn:=0; gfn:=0; gmn:=0;
      lfs:=''; gfs:=''; gms:='';
      while not TUser.EOF do
      begin
        TUserSheet.SetOrder('Word_Ind');
        TUserSheet.Locate('Word',TUser.Int(TUserIndex));
        gfound:=false; gmore:=false; lfound:=false;
        word:=TUser.Int(TUserIndex);
        while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
        begin
          TUserCat.Locate('Index',TUserSheet.TrueInt(TUserSheetNumber));
          if chr(TUserCat.Int(TUserCatType))='L'then lfound:=true;
          if chr(TUserCat.Int(TUserCatType))='G'then if gfound then gmore:=true;
          if chr(TUserCat.Int(TUserCatType))='G'then gfound:=true;
          TUserSheet.Next;
        end;
        s:=KanaToRomaji(TUser.Str(TUserPhonetic),'j');
        if not gfound then begin
          inc(gfn);
          if gfn=10 then gfs:=gfs+',...';
          if gfn=1 then gfs:=gfs+s;
          if (gfn>1) and (gfn<10) then gfs:=gfs+','+s;
        end;
        if not lfound then begin
          inc(lfn);
          if lfn=10 then lfs:=lfs+',...';
          if lfn=1 then lfs:=lfs+s;
          if (lfn>1) and (lfn<10) then lfs:=lfs+','+s;
        end;
        if gmore then begin
          inc(gmn);
          if gmn=10 then gms:=gms+',...';
          if gmn=1 then gms:=gms+s;
          if (gmn>1) and (gmn<10) then gms:=gms+','+s;
        end;
        TUser.Next;
      end;
      ssum:='';
      Screen.Cursor:=crDefault;
      if lfn>0 then ssum:=ssum+_l('^eWords without assigned lesson')+' ('+inttostr(lfn)+'):'#13+lfs+#13#13;
      if gfn>0 then ssum:=ssum+_l('^eWords without assigned group')+' ('+inttostr(gfn)+'):'#13+gfs+#13#13;
      if gmn>0 then ssum:=ssum+_l('^eWords with more assigned group')+' ('+inttostr(gmn)+'):'#13+gms+#13#13;
      if ssum<>'' then
        Application.MessageBox(
          pchar(ssum),
          pchar(_l('^eErrors')),
          MB_ICONERROR or MB_OK)
      else
        Application.MessageBox(
          pchar(_l('#00577^eNo errors were found.')),
          pchar(_l('#00094^eSuccess')),
          MB_ICONINFORMATION or MB_OK);
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.SpeedButton10Click(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  Application.MessageBox(
    pchar(_l('#00567^eSome fonts may not be able to display some characters. '
      +'If you see that some characters look different in style or aren''t '
      +'displayed properly, select another font.')),
    pchar(_l('#00566^eNotice')),
  MB_OK or MB_ICONINFORMATION);
  edtKanjiCardFont.Text:=ChooseFont([CHINESEBIG5_CHARSET,GB2312_CHARSET,SHIFTJIS_CHARSET],testkanji,sup,edtKanjiCardFont.Text,false);
{$ELSE}
begin
{$ENDIF}
end;

{ Populates chardetl with default property set }
procedure TfSettings.ResetCharDetl;
begin
{$IFNDEF AUTOTEST}
  chardetl.Clear;
  chardetl.Add('4;W;N;J;N;M;');
  chardetl.Add('5;W;N;J;N;M;');
  chardetl.Add('2;W;L;B;N;M;');
  chardetl.Add('8;W;L;C;N;M;');
  chardetl.Add('1;W;L;C;N;M;');
  chardetl.Add('0;L;L;B;N;M;');
  chardetl.Add('10;W;L;B;N;M;');
  chardetl.Add('21;C;L;C;N;S;');
  chardetl.Add('22;C;L;J;N;S;');
  chardetl.Add('23;C;L;J;N;S;');
  chardetl.Add('24;C;L;J;N;S;S&H code');
  chardetl.Add('26;C;L;J;N;S;');
  chardetl.Add('31;C;L;J;N;S;');
  chardetl.Add('32;C;L;J;N;S;');
  chardetl.Add('35;C;L;J;N;S;');
  chardetl.Add('33;C;L;C;N;S;');
  chardetl.Add('34;C;L;C;N;S;');
  chardetl.Add('41;L;L;B;N;M;');
  chardetl.Add('42;L;L;B;N;M;');
  chardetl.Add('43;L;L;C;N;M;');
  chardetl.Add('44;L;L;C;N;M;');
  chardetl.Add('45;L;L;B;N;M;');
  chardetl.Add('0;L;L;B;N;M;');
  chardetl.Add('51;C;L;C;N;S;');
  chardetl.Add('52;C;L;C;N;S;');
  chardetl.Add('53;C;L;C;N;S;');
  chardetl.Add('54;C;L;C;N;S;');
  chardetl.Add('55;C;L;C;N;S;');
  chardetl.Add('56;C;L;J;N;S;');
  chardetl.Add('57;C;L;J;N;S;');
  chardetl.Add('58;C;L;J;N;S;');
  chardetl.Add('100;L;L;B;N;M;');
  chardetl.Add('101;L;L;J;N;M;');
  chardetl.Add('102;L;L;J;N;M;');
{$ENDIF}
end;

procedure TfSettings.LoadCharDetl(reg: TCustomIniFile);
{$IFNDEF AUTOTEST}
var keys: TStringList;
  i: integer;
begin
 { Char details configuration was previously being saved to Wakan.cdt.
  If it's present, load it. }
  if FileExists(UserDataDir+'\WAKAN.CDT') then begin
    chardetl.LoadFromFile(UserDataDir+'\WAKAN.CDT');
    exit;
  end;

 //Load from registry
  chardetl.Clear;
  keys := TStringList.Create;
  try
    reg.ReadSectionValues('CharDetl', keys);
    if keys.Count<=0 then begin //section not available
      ResetCharDetl;
    end else begin
      keys.Sort;
      for i := 0 to keys.Count-1 do
        chardetl.Add(keys.ValueFromIndex[i]);
    end;
  finally
    FreeAndNil(keys);
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.SaveCharDetl(reg: TCustomIniFile);
{$IFNDEF AUTOTEST}
var i: integer;
begin
 //Save to registry
  reg.EraseSection('CharDetl');
  for i := 0 to chardetl.Count-1 do
    reg.WriteString('CharDetl', IntToStr(i), chardetl[i]);

 //Delete wakan.cdt if it's present. In fact, move it to backup.
  BackupMove(UserDataDir+'\WAKAN.CDT');
{$ELSE}
begin
{$ENDIF}
end;


procedure TfSettings.ReloadCharDetlList;
{$IFNDEF AUTOTEST}
var i:integer;
  ii:integer;
  propType: PCharPropType;
begin
  ii:=ListBox2.ItemIndex;
  ListBox2.Items.Clear;
  for i:=0 to chardetl.Count-1 do
  begin
    propType := FindCharPropType(StrToInt(GetCharDet(i,0)));
    if propType=nil then continue;

    if GetCharDet(i,6)<>'' then
      ListBox2.Items.Add(_l('^e'+propType.englishName)+' ('+GetCharDet(i,6)+')')
    else
      ListBox2.Items.Add(_l('^e'+propType.englishName));
  end;
  Button8.Enabled:=ListBox2.Items.Count<>0;
  Button9.Enabled:=ListBox2.Items.Count<>0;
  SpeedButton11.Enabled:=ListBox2.Items.Count<>0;
  SpeedButton12.Enabled:=ListBox2.Items.Count<>0;
  if ListBox2.Items.Count<>0 then ListBox2.ItemIndex:=0;
  if (ii>=0) and (ii<ListBox2.Items.Count) then ListBox2.ItemIndex:=ii;
  SpeedButton11.Enabled:=ListBox2.ItemIndex>0;
  SpeedButton12.Enabled:=ListBox2.ItemIndex<ListBox2.Items.Count-1;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.Button7Click(Sender: TObject);
{$IFNDEF AUTOTEST}
var fCharItem: TfCharItem;
begin
  fCharItem := TfCharItem.Create(Application);
  try
    fCharItem.inputs:='';
    if fCharItem.ShowModal=mrOK then chardetl.Add(fCharItem.results);
  finally
    FreeAndNil(fCharItem);
  end;
  ReloadCharDetlList;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.Button8Click(Sender: TObject);
{$IFNDEF AUTOTEST}
var fCharItem: TfCharItem;
begin
  if ListBox2.ItemIndex<0 then exit;
  fCharItem := TfCharItem.Create(Application);
  try
    fCharItem.inputs:=chardetl[ListBox2.ItemIndex];
    if fCharItem.ShowModal=mrOK then chardetl[ListBox2.ItemIndex]:=fCharItem.results;
  finally
    FreeAndNil(fCharItem);
  end;
  ReloadCharDetlList;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.Button9Click(Sender: TObject);
{$IFNDEF AUTOTEST}
begin
  if ListBox2.ItemIndex<>-1 then
  begin
    chardetl.Delete(ListBox2.ItemIndex);
    ReloadCharDetlList;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.Button10Click(Sender: TObject);
begin
  ResetCharDetl;
  ReloadCharDetlList;
end;

procedure TfSettings.ListBox2Click(Sender: TObject);
begin
  SpeedButton11.Enabled:=ListBox2.ItemIndex>0;
  SpeedButton12.Enabled:=ListBox2.ItemIndex<ListBox2.Items.Count-1;
end;

procedure TfSettings.SpeedButton11Click(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  chardetl.Insert(ListBox2.ItemIndex-1,chardetl[ListBox2.ItemIndex]);
  chardetl.Delete(ListBox2.ItemIndex+1);
  ListBox2.ItemIndex:=ListBox2.ItemIndex-1;
  ReloadCharDetlList;
{$ENDIF}
end;

procedure TfSettings.SpeedButton12Click(Sender: TObject);
begin
{$IFNDEF AUTOTEST}
  chardetl.Insert(ListBox2.ItemIndex+2,chardetl[ListBox2.ItemIndex]);
  chardetl.Delete(ListBox2.ItemIndex);
  ListBox2.ItemIndex:=ListBox2.ItemIndex+1;
  ReloadCharDetlList;
{$ENDIF}
end;

procedure TfSettings.Button11Click(Sender: TObject);
begin
  ShowAllocStats;
end;

procedure TfSettings.ComboBox2Change(Sender: TObject);
{$IFNDEF AUTOTEST}
var i:integer;
    s:string;
    v:boolean;
begin
  ListBox3.Items.Clear;
  colorfrom:=-1;
  i:=0;
  repeat
    s:=GetColorString(i);
    if s<>'' then
    if s[1]=inttostr(ComboBox2.ItemIndex) then
    begin
      if (colorfrom=-1) then colorfrom:=i;
      delete(s,1,1);
      ListBox3.Items.Add(_l(s));
    end;
    inc(i);
  until s='';
  ListBox3.ItemIndex:=0;
  ListBox3Click(Sender);
  cbNoCharColors.Visible:=ComboBox2.ItemIndex=0;
  cbNoWordGridColors.Visible:=(ComboBox2.ItemIndex=1) or (ComboBox2.ItemIndex=2);
  cbNoEditorColors.Visible:=ComboBox2.ItemIndex=3;
  case Combobox2.ItemIndex of
    0: v:=not cbNoCharColors.Checked;
    1: v:=not cbNoWordGridColors.Checked;
    2: v:=not cbNoWordGridColors.Checked;
    3: v:=not cbNoEditorColors.Checked;
  else
    v := false;
  end;
  ListBox3.Enabled:=v;
  Button12.Enabled:=v;
  Button14.Enabled:=v;
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.ListBox3Click(Sender: TObject);
begin
  Shape2.Brush.Color:=GetCol(colorfrom+ListBox3.ItemIndex);
end;

procedure TfSettings.Button15Click(Sender: TObject);
{$IFNDEF AUTOTEST}
var i:integer;
begin
  if Application.MessageBox(
    pchar(_l('#00584^eThis function will replace all your settings.'#13#13'Do you want to continue?')),
    pchar(_l('#00090^eWarning')),
    MB_ICONWARNING or MB_YESNO)=idNo then exit;
  i:=0;
  repeat
    SetColDefault(i);
    inc(i);
  until GetColorString(i)='';
  ComboBox2Change(self);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.Button14Click(Sender: TObject);
begin
  SetColDefault(colorfrom+ListBox3.ItemIndex);
  ListBox3Click(Sender);
end;

procedure TfSettings.Button12Click(Sender: TObject);
begin
  ColorDialog1.Color:=GetCol(colorFrom+ListBox3.ItemIndex);
  if ColorDialog1.Execute then SetCol(colorFrom+ListBox3.ItemIndex,ColorDialog1.Color);
  ListBox3Click(Sender);
end;

procedure TfSettings.cbNoCharColorsClick(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.cbNoEditorColorsClick(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.cbNoWordGridColorsClick(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.btnImportKanjidicClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var fCharDataImport: TfCharDataImport;
begin
  fCharDataImport := TfCharDataImport.Create(Self);
  try
    fCharDataImport.ShowModal;
  finally
    FreeAndNil(fCharDataImport);
  end;
{$ELSE}
begin
{$ENDIF}
end;

//Returns the type ID of the chosen preferred radical type
function TfSettings.GetPreferredRadicalType: integer;
begin
  Result := cbRadicalType.ItemIndex+12;
end;

{ Romaji systems }

{ Reloads the list of available romanizations, preserving user selection as
 possible. }
procedure TfSettings.ReloadRomaSystems;
{$IFNDEF AUTOTEST}
var OldList: string;
  sr: TSearchRec;
  res: integer;
begin
  OldList := GetRomaList;
  lbRomaSystems.Clear;
  res := FindFirst(ProgramDataDir+'\*'+RomajiExt, faAnyFile and not faDirectory, sr);
  while res=0 do begin
    lbRomaSystems.Items.Add(ChangeFileExt(ExtractFilename(sr.Name),''));
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
  SetRomaList(OldList);
{$ELSE}
begin
{$ENDIF}
end;

{ Returns comma-separated list of selected romanizations, in the priority order. }
function TfSettings.GetRomaList: string;
begin
  Result := AnsiLowerCase(lbRomaSystems.Selection);
end;

{ Sets the priority order and selection of romanizations. If any is missing,
 ignores it }
procedure TfSettings.SetRomaList(const Value: string);
begin
  SetEnhancedRomaListOn(pos(',',Value)>0);
  lbRomaSystems.Selection := Value;
end;

{ Reconfigures the romanization list to display in basic (one romanization)
 or enhanced format. Tries to preserve roma selection }
procedure TfSettings.SetEnhancedRomaListOn(const Value: boolean);
begin
  cbMultipleRoma.Checked := Value;
  btnRomaSystemUp.Visible := Value;
  btnRomaSystemDown.Visible := Value;
  CheckRomaSystemMoveButtonsEnabled();
  lbRomaSystems.MultiSelect := Value;
  lbRomaSystems.Invalidate;
end;

procedure TfSettings.cbMultipleRomaClick(Sender: TObject);
begin
  SetEnhancedRomaListOn(cbMultipleRoma.Checked);
end;

procedure TfSettings.lbRomaSystemsClick(Sender: TObject);
begin
  CheckRomaSystemMoveButtonsEnabled();
end;

procedure TfSettings.lbRomaSystemsSelectionChanged(Sender: TObject);
begin
  ReloadRomaSetup();
  edtTestRomajiChange(nil); //Update preview
end;

procedure TfSettings.CheckRomaSystemMoveButtonsEnabled;
begin
  btnRomaSystemUp.Enabled := (lbRomaSystems.ItemIndex>0);
  btnRomaSystemDown.Enabled := (lbRomaSystems.ItemIndex<lbRomaSystems.Count-1);
end;

procedure TfSettings.btnRomaSystemUpClick(Sender: TObject);
begin
  if lbRomaSystems.ItemIndex>0 then begin
    lbRomaSystems.Items.Exchange(lbRomaSystems.ItemIndex, lbRomaSystems.ItemIndex-1);
    lbRomaSystemsSelectionChanged(nil);
  end;
  CheckRomaSystemMoveButtonsEnabled();
end;

procedure TfSettings.btnRomaSystemDownClick(Sender: TObject);
begin
  if (lbRomaSystems.ItemIndex>=0)
  and (lbRomaSystems.ItemIndex<lbRomaSystems.Count-1) then begin
    lbRomaSystems.Items.Exchange(lbRomaSystems.ItemIndex, lbRomaSystems.ItemIndex+1);
    lbRomaSystemsSelectionChanged(nil);
  end;
  CheckRomaSystemMoveButtonsEnabled();
end;

//Reloads all user romanization files in the order they are configured to be load.
//Called at settings load and every time the setup changes.
procedure TfSettings.ReloadRomaSetup;
{$IFNDEF AUTOTEST}
var list: string;
  parts: TStringArray;
  i: integer;
begin
  roma_user.Clear;
  list := GetRomaList;
  parts := SplitStr(list,',');
  for i := 0 to Length(parts)-1 do
    roma_user.LoadFromFile(ProgramDataDir+'\'+parts[i]+RomajiExt);
{$ELSE}
begin
{$ENDIF}
end;

{ Same for pinyin }

procedure TfSettings.ReloadPinyinSystems;
{$IFNDEF AUTOTEST}
var OldList: string;
  sr: TSearchRec;
  res: integer;
begin
  OldList := GetPinyinList;
  lbPinyinSystems.Clear;
  res := FindFirst(ProgramDataDir+'\*'+PinyinExt, faAnyFile and not faDirectory, sr);
  while res=0 do begin
    lbPinyinSystems.Items.Add(ChangeFileExt(ExtractFilename(sr.Name),''));
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
  SetPinyinList(OldList);
{$ELSE}
begin
{$ENDIF}
end;

function TfSettings.GetPinyinList: string;
begin
  Result := AnsiLowerCase(lbPinyinSystems.Selection);
end;

procedure TfSettings.SetPinyinList(const Value: string);
begin
  SetEnhancedPinyinListOn(pos(',',Value)>0);
  lbPinyinSystems.Selection := Value;
end;

procedure TfSettings.SetEnhancedPinyinListOn(const Value: boolean);
begin
  cbMultiplePinyin.Checked := Value;
  btnPinyinSystemUp.Visible := Value;
  btnPinyinSystemDown.Visible := Value;
  CheckPinyinSystemMoveButtonsEnabled();
  lbPinyinSystems.MultiSelect := Value;
  lbPinyinSystems.Invalidate;
end;

procedure TfSettings.lbPinyinSystemsClick(Sender: TObject);
begin
  CheckPinyinSystemMoveButtonsEnabled();
end;

procedure TfSettings.lbPinyinSystemsSelectionChanged(Sender: TObject);
begin
  ReloadPinyinSetup();
  edtTestPinyinChange(nil); //Update preview
end;

procedure TfSettings.CheckPinyinSystemMoveButtonsEnabled;
begin
  btnPinyinSystemUp.Enabled := (lbPinyinSystems.ItemIndex>0);
  btnPinyinSystemDown.Enabled := (lbPinyinSystems.ItemIndex<lbPinyinSystems.Count-1);
end;

procedure TfSettings.btnPinyinSystemUpClick(Sender: TObject);
begin
  if lbPinyinSystems.ItemIndex>0 then begin
    lbPinyinSystems.Items.Exchange(lbPinyinSystems.ItemIndex, lbPinyinSystems.ItemIndex-1);
    lbPinyinSystemsSelectionChanged(nil);
  end;
  CheckPinyinSystemMoveButtonsEnabled();
end;

procedure TfSettings.btnPinyinSystemDownClick(Sender: TObject);
begin
  if (lbPinyinSystems.ItemIndex>=0)
  and (lbPinyinSystems.ItemIndex<lbPinyinSystems.Count-1) then begin
    lbPinyinSystems.Items.Exchange(lbPinyinSystems.ItemIndex, lbPinyinSystems.ItemIndex+1);
    lbPinyinSystemsSelectionChanged(nil);
  end;
  CheckPinyinSystemMoveButtonsEnabled();
end;

procedure TfSettings.cbMultiplePinyinClick(Sender: TObject);
begin
  SetEnhancedPinyinListOn(cbMultiplePinyin.Checked);
end;

procedure TfSettings.ReloadPinyinSetup;
{$IFNDEF AUTOTEST}
var list: string;
  parts: TStringArray;
  i: integer;
begin
  rpy_user.Clear;
  list := GetPinyinList;
  parts := SplitStr(list,',');
  for i := 0 to Length(parts)-1 do
    rpy_user.LoadFromFile(ProgramDataDir+'\'+parts[i]+PinyinExt);
{$ELSE}
begin
{$ENDIF}
end;


{ Test romaji/pinyin }

procedure TfSettings.pbRomajiAsHiraganaPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color := clBtnFace;
  DrawUnicode(Canvas,1,1,16,RomajiToKana('H'+edtTestRomaji.Text,'j',[]),GetCJKFont('j'));
end;

procedure TfSettings.pbRomajiAsKatakanaPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color := clBtnFace;
  DrawUnicode(Canvas,1,1,16,RomajiToKana('K'+edtTestRomaji.Text,'j',[]),GetCJKFont('j'));
end;

procedure TfSettings.pbKanaAsRomajiClick(Sender: TObject);
var i: integer;
  found: boolean;
begin
  found := false;
  for i := Low(KanaExamples) to High(KanaExamples)-1{sic} do
    if FKanaExample = KanaExamples[i] then begin
      FKanaExample := KanaExamples[i+1];
      found := true;
      break;
    end;
  if not found then
    FKanaExample := KanaExamples[0]; //init or rewind
  pbKanaAsRomaji.Invalidate;
end;

procedure TfSettings.pbKanaAsRomajiPaint(Sender: TObject; Canvas: TCanvas);
begin
  if FKanaExample='' then pbKanaAsRomajiClick(nil); //select one
  Canvas.Brush.Color := clBtnFace;
  DrawKana(Canvas,1,1,16,FKanaExample,'j');
end;

procedure TfSettings.edtTestRomajiChange(Sender: TObject);
begin
  pbRomajiAsHiragana.Invalidate;
  pbRomajiAsKatakana.Invalidate;
  pbKanaAsRomaji.Invalidate; //this doesn't belong here, but whatever
end;

procedure TfSettings.pbPinyinAsBopomofoPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clBtnFace;
  DrawKanaForced(Canvas,1,1,16,RomajiToKana(edtTestPinyin.Text,'c',[]));
end;

procedure TfSettings.pbBopomofoAsPinyinClick(Sender: TObject);
var i: integer;
  found: boolean;
begin
  found := false;
  for i := Low(BopomofoExamples) to High(BopomofoExamples)-1{sic} do
    if FBopomofoExample = BopomofoExamples[i] then begin
      FBopomofoExample := BopomofoExamples[i+1];
      found := true;
      break;
    end;
  if not found then
    FBopomofoExample := BopomofoExamples[0]; //init or rewind
  pbBopomofoAsPinyin.Invalidate;
end;

procedure TfSettings.pbBopomofoAsPinyinPaint(Sender: TObject; Canvas: TCanvas);
begin
  if FBopomofoExample='' then pbBopomofoAsPinyinClick(nil); //select one
  Canvas.Brush.Color := clBtnFace;
  DrawKana(Canvas,1,1,16,FBopomofoExample,'c');
end;

procedure TfSettings.edtTestPinyinChange(Sender: TObject);
begin
  pbPinyinAsBopomofo.Invalidate;
  pbBopomofoAsPinyin.Invalidate; //this doesn't belong here, but whatever
end;

procedure TfSettings.RadioGroup1Click(Sender: TObject);
begin
  jshowroma:=rgShowKana.ItemIndex=1;
  if curlang='j' then
    showroma:=jshowroma;
  pbKanaAsRomaji.Invalidate;
end;

procedure TfSettings.rgShowBopomofoClick(Sender: TObject);
begin
  cshowroma:=rgShowBopomofo.ItemIndex=1;
  if curlang='c' then
    showroma:=cshowroma;
  pbBopomofoAsPinyin.Invalidate;
end;


{ Copy formats }

procedure TfSettings.tsExprCopyFormatsShow(Sender: TObject);
begin
  ReloadExprCopyFormats;
{$IFNDEF AUTOTEST}
  lblExprCopyFormatsEdit.URL := GetExprCopyFormatsDir;
  lblexprCopyFormatsDocs.URL := WikiUrl('CopyFormats');
{$ENDIF}
end;

procedure TfSettings.ReloadExprCopyFormats;
{$IFNDEF AUTOTEST}
var fname: string;
begin
  lbExprCopyFormats.Clear;
  for fname in GetExprCopyFormats do
    lbExprCopyFormats.Items.Add(ChangeFileExt(ExtractFilename(fname),''));
  lbExprCopyFormats.ItemIndex := lbExprCopyFormats.Items.IndexOf(DefaultExprCopyFormatName);
  lbExprCopyFormatsClick(lbExprCopyFormats);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.lbExprCopyFormatsClick(Sender: TObject);
begin
  if lbExprCopyFormats.ItemIndex<0 then exit;
  DefaultExprCopyFormatName := lbExprCopyFormats.Items[lbExprCopyFormats.ItemIndex];
  UpdateExprCopyFormatExample;
end;

procedure TfSettings.UpdateExprCopyFormatExample;
{$IFNDEF AUTOTEST}
var res: TSearchResult;
begin
  if DefaultExprCopyFormatName='' then begin
    mmExprCopyFormatExample.Text := '';
    exit;
  end;

  res.Reset;
  res.kanji := '来る';
  res.kana := 'くる';
  with res.AddArticle^ do begin
    dicname := 'EDICT2';
    entries.Add(
      'to come (spatially or temporally); to approach; to arrive',
      #75#81#51#98 //verb intan-verb aux-v pop
    );
    entries.Add(
      'to come back; to do ... and come back',
      #75#81#51#98 //verb intan-verb aux-v pop
    );
    entries.Add(
      'to come to be; to become; to get; to grow; to continue',
      #75#81#51#98 //verb intan-verb aux-v pop
    );
  end;

  mmExprCopyFormatExample.Text :=
    XsltTransform(res.ToEdictXml, GetExprCopyFormatsDir+'\'+DefaultExprCopyFormatName+'.xslt');
{$ELSE}
begin
{$ENDIF}
end;


{ Kanji copy formats }

procedure TfSettings.tsKanjiCopyFormatsShow(Sender: TObject);
begin
  ReloadKanjiCopyFormats;
{$IFNDEF AUTOTEST}
  lblKanjiCopyFormatsEdit.URL := GetKanjiCopyFormatsDir;
  lblKanjiCopyFormatsDocs.URL := WikiUrl('CopyFormats');
{$ENDIF}
end;

procedure TfSettings.ReloadKanjiCopyFormats;
{$IFNDEF AUTOTEST}
var fname: string;
begin
  lbKanjiCopyFormats.Clear;
  for fname in GetKanjiCopyFormats do
    lbKanjiCopyFormats.Items.Add(ChangeFileExt(ExtractFilename(fname),''));
  lbKanjiCopyFormats.ItemIndex := lbKanjiCopyFormats.Items.IndexOf(DefaultKanjiCopyFormatName);
  lbKanjiCopyFormatsClick(lbKanjiCopyFormats);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfSettings.lbKanjiCopyFormatsClick(Sender: TObject);
begin
  if lbKanjiCopyFormats.ItemIndex<0 then exit;
  DefaultKanjiCopyFormatName := lbKanjiCopyFormats.Items[lbKanjiCopyFormats.ItemIndex];
  UpdateKanjiCopyFormatExample;
end;

procedure TfSettings.UpdateKanjiCopyFormatExample;
begin
{$IFNDEF AUTOTEST}
  if DefaultExprCopyFormatName='' then begin
    mmExprCopyFormatExample.Text := '';
    exit;
  end;

  mmKanjiCopyFormatExample.Text :=
    XsltTransform(KanjiInfoToXml('間'),
      GetKanjiCopyFormatsDir+'\'+DefaultKanjiCopyFormatName+'.xslt');
{$ENDIF}
end;



end.
