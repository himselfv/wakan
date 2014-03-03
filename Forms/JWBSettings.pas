unit JWBSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, IniFiles, registry, UrlLabel,
  ImgList, WakanPaintbox, Vcl.CheckLst, WakanListBoxes;

type
  TfSettings = class(TForm)
    pcPages: TPageControl;
    tsRomanization: TTabSheet;
    tsCharacterList: TTabSheet;
    rgKanjiGridSize: TRadioGroup;
    CheckBox1: TCheckBox;
    tsFonts: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Edit2: TEdit;
    SpeedButton2: TSpeedButton;
    Label4: TLabel;
    SpeedButton4: TSpeedButton;
    Edit4: TEdit;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    SpeedButton6: TSpeedButton;
    Label7: TLabel;
    SpeedButton7: TSpeedButton;
    Label8: TLabel;
    SpeedButton8: TSpeedButton;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Label3: TLabel;
    SpeedButton3: TSpeedButton;
    Edit3: TEdit;
    Label9: TLabel;
    SpeedButton9: TSpeedButton;
    Edit9: TEdit;
    Label10: TLabel;
    tsDictionary: TTabSheet;
    GroupBox3: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
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
    Edit11: TEdit;
    Label13: TLabel;
    Edit12: TEdit;
    Label14: TLabel;
    Edit13: TEdit;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    Label15: TLabel;
    Edit14: TEdit;
    SpeedButton10: TSpeedButton;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
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
    CheckBox44: TCheckBox;
    CheckBox45: TCheckBox;
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
    Button5: TButton;
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
    CheckBox28: TCheckBox;
    CheckBox47: TCheckBox;
    CheckBox48: TCheckBox;
    Label36: TLabel;
    Edit21: TEdit;
    Label37: TLabel;
    Label38: TLabel;
    Edit22: TEdit;
    Label39: TLabel;
    Edit23: TEdit;
    Label40: TLabel;
    Edit24: TEdit;
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
    CheckBox3: TCheckBox;
    cbNoEditorColors: TCheckBox;
    cbNoGridColors: TCheckBox;
    ColorDialog1: TColorDialog;
    CheckBox51: TCheckBox;
    CheckBox52: TCheckBox;
    Label43: TLabel;
    Edit26: TEdit;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Edit27: TEdit;
    Edit28: TEdit;
    Label50: TLabel;
    Edit32: TEdit;
    SpeedButton13: TSpeedButton;
    cbYomiIgnoreOkurigana: TCheckBox;
    Label5: TLabel;
    SpeedButton5: TSpeedButton;
    Edit5: TEdit;
    Label51: TLabel;
    SpeedButton14: TSpeedButton;
    Edit33: TEdit;
    cbShowFreq: TCheckBox;
    cbOrderFreq: TCheckBox;
    Edit34: TEdit;
    Label52: TLabel;
    tsEditor: TTabSheet;
    Label53: TLabel;
    CheckBox62: TCheckBox;
    CheckBox63: TCheckBox;
    Edit35: TEdit;
    tsAnnotations: TTabSheet;
    cbEnableAnnotations: TCheckBox;
    cbRebuildAnnotations: TCheckBox;
    CheckBox66: TCheckBox;
    CheckBox67: TCheckBox;
    CheckBox68: TCheckBox;
    CheckBox69: TCheckBox;
    Bevel1: TBevel;
    Button16: TButton;
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
    tsPortability: TTabSheet;
    lblWakanMode: TLabel;
    Label49: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    lblSettingsPath: TUrlLabel;
    lblDictionariesPath: TUrlLabel;
    lblUserDataPath: TUrlLabel;
    btnUpgradeToStandalone: TButton;
    lblUpgradeToStandalone: TLabel;
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
    CheckBox60: TCheckBox;
    CheckBox61: TCheckBox;
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
    lblBackupPath: TUrlLabel;
    Label59: TLabel;
    pbRomajiAsHiragana: TWakanPaintbox;
    pbRomajiAsKatakana: TWakanPaintbox;
    pbPinyinAsBopomofo: TWakanPaintbox;
    cbMultipleRoma: TCheckBox;
    btnRomaSystemUp: TBitBtn;
    btnRomaSystemDown: TBitBtn;
    lbRomaSystems: TWakanCheckListBox;
    pbKanaAsRomaji: TWakanPaintbox;
    lblKanaToRomajiHint: TLabel;
    pbBopomofoAsPinyin: TWakanPaintbox;
    Label17: TLabel;
    btnPinyinSystemUp: TBitBtn;
    btnPinyinSystemDown: TBitBtn;
    lbPinyinSystems: TWakanCheckListBox;
    cbMultiplePinyin: TCheckBox;
    tsDictCopyFormats: TTabSheet;
    Label18: TLabel;
    lbCopyFormats: TListBox;
    mmCopyFormatExample: TMemo;
    Label19: TLabel;
    lblCopyFormatsIni: TUrlLabel;
    lblCopyFormatsDocumentation: TUrlLabel;
    Label20: TLabel;
    tsCharacterDetailsGeneral: TTabSheet;
    cbDetailsShowKanjiClass: TCheckBox;
    cbDetailsKanjiInColor: TCheckBox;
    rgDetailsCategoryEditorType: TRadioGroup;
    cbDetailsShowLinks: TCheckBox;
    cbDictRefLinksInSubmenu: TCheckBox;
    procedure RadioGroup1Click(Sender: TObject);
    procedure btnChangeLanguageClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
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
    procedure Button5Click(Sender: TObject);
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
    procedure CheckBox3Click(Sender: TObject);
    procedure cbNoColorsClick(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure btnUpgradeToStandaloneClick(Sender: TObject);
    procedure lblSettingsPathClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvContentsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure btnImportKanjidicClick(Sender: TObject);
    procedure tvContentsChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
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
    procedure tsDictCopyFormatsShow(Sender: TObject);
    procedure lbCopyFormatsClick(Sender: TObject);

  protected
    procedure UpdateFontNames;
  public
    function AutoDetectFonts(Silent:boolean):boolean;
    procedure CheckFontsPresent;

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
    procedure UpdatePortabilityPage;

  protected
   { Shared settings. To avoid reading the same file twice, call GetSettingsStore
    to receive a shared copy. }
    FWakanIni: TCustomIniFile;
    FSettingsStore: TCustomIniFile;
    FPortabilityLoaded: boolean;
    function GetWakanIni: TCustomIniFile;
  public
    function GetSettingsStore: TCustomIniFile;
    procedure FreeSettings;

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
    FDefaultCopyFormat: integer;
    FDefaultCopyFormatName: string; //see commens for GetDefaultCopyFormat
    procedure ReloadCopyFormats;
    procedure UpdateCopyFormatExample;
    function GetDefaultCopyFormat: integer;
    procedure SetDefaultCopyFormat(const Value: integer);
    procedure SetDefaultCopyFormatName(const Value: string);
  public
    property DefaultCopyFormat: integer read GetDefaultCopyFormat
      write SetDefaultCopyFormat;
    property DefaultCopyFormatName: string read FDefaultCopyFormatName
      write SetDefaultCopyFormatName;


  public
   { Layout settings are loaded into these variables and applied later }
    SetLayout: integer;
    SetWindows: integer;
    setsort: integer;
    setusercompounds: boolean;
    setPortraitMode: boolean;
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
    function GetTranslationFile: string;
    procedure SetTranslationFile(const Value: string);

  public
    function GetPreferredRadicalType: integer;

  end;

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
  BopomofoExamples: array[0..1] of string = (
   'ㄓㄨˋㄧㄣㄈㄨˊㄏㄠˋ',
   'ㄆㄧㄥˊㄗ˙'
  );

implementation

uses JWBMenu, JWBStrings, JWBCore, JWBKanaConv, JWBUnit, JWBKanji, JWBEditor,
  JWBKanjiSearch, JWBRadical, JWBKanjiCompounds, JWBWordLookup, JWBCharItem,
  JWBExamples, JWBVocabDetails, JWBVocabFilters, JWBKanjiDetails, TextTable,
  JWBLanguage, UnicodeFont, JWBKanjiCard, JWBVocab, WakanWordGrid,
  JWBUserData, JWBPortableMode, JWBCharData, ActnList, JWBCharDataImport,
  JWBIO, JWBDicSearch, JWBCopyFormats;

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
  ClearKanjiCardCache;
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
  if edit11.text='0' then edit11.text:='1';
  if not TryStrToInt(Edit10.Text, tmp) then Edit10.Text := '0';
  if not TryStrToInt(Edit11.Text, tmp) then Edit11.Text := '0';
  if not TryStrToInt(Edit12.Text, tmp) then Edit12.Text := '0';
  if not TryStrToInt(Edit13.Text, tmp) then Edit13.Text := '0';
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

procedure TfSettings.FormDestroy(Sender: TObject);
begin
  FreeSettings();
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


{ Settings store }

{ Opens wakan.ini file from the application directory which either contains
 the settings or an instruction to look in registry. }
function TfSettings.GetWakanIni: TCustomIniFile;
begin
  if FWakanIni<>nil then begin
    Result := FWakanIni;
    exit;
  end;
  Result := TMemIniFile.Create(AppFolder+'\wakan.ini', nil); //read everything, Ansi/UTF8/UTF16
  TMemIniFile(Result).Encoding := SysUtils.TEncoding.UTF8; //write UTF8 only
  FWakanIni := Result;
end;

{ Opens whatever settings store is configured for the application.
I.e.:
  portable mode => wakan.ini
  registry mode => registry key
If you're using this before proper LoadSettings, only read, do not write,
and be ready to receive nil }
function TfSettings.GetSettingsStore: TCustomIniFile;
var ini: TCustomIniFile;
  s: string;
begin
  if FSettingsStore<>nil then begin
    Result := FSettingsStore;
    exit;
  end;

 { If the settings has been loaded then the application is running in a
  particular mode and it doesn't matter what ini says anymore }
  if FPortabilityLoaded then begin
    case PortabilityMode of
      pmPortable: Result := GetWakanIni;
    else Result := TRegistryIniFile.Create(WakanRegKey);
    end;
    exit;
  end;

 { The settings has not been loaded yet, read those from ini }
  ini := GetWakanIni;

  s := LowerCase(ini.ReadString('General', 'Install', ''));
  if (s='') or (s='upgrade') or (s='compatible') or (s='standalone') then begin
    Result := TRegistryIniFile.Create(WakanRegKey);
  end else
  if (s='portable') then begin
    Result := ini;
  end else
    raise Exception.Create('Invalid installation mode configuration.');

  FSettingsStore := Result;
end;

{ Releases the shared settings objects. As things stand, there's no hurry as file
 is not kept locked. }
procedure TfSettings.FreeSettings;
begin
  if FSettingsStore=FWakanIni then
    FSettingsStore := nil; //same object
  FreeAndNil(FSettingsStore);
  FreeAndNil(FWakanIni);
end;

//See comments in JWBPortableMode.pas about Wakan modes
procedure TfSettings.LoadSettings;
var ini: TCustomIniFile;
  fPortableMode: TfPortableMode;
  s: string;
begin
  ini := GetWakanIni;

  s := LowerCase(ini.ReadString('General', 'Install', ''));

  fPortableMode := nil;
  if s='' then begin
    fPortableMode := TfPortableMode.Create(Application);
    s := LowerCase(fPortableMode.Initialize(ini))
  end else
  if s='upgrade' then begin
    fPortableMode := TfPortableMode.Create(Application);
   //We cannot just copy some of the files. If we start this operation,
   //we have to continue it even after restart.
    s := LowerCase(fPortableMode.ContinueUpgrade(ini));
  end;
  if fPortableMode<>nil then
    FreeAndNil(FSettingsStore); //settings store location could have changed -- recreate later
  FreeAndNil(fPortableMode);

  if s='standalone' then begin
    SetPortabilityMode(pmStandalone);
  end else
  if s='compatible' then begin
    SetPortabilityMode(pmCompatible);
  end else
  if s='portable' then begin
    SetPortabilityMode(pmPortable);
  end else
    raise Exception.Create('Invalid installation mode configuration.');
  FPortabilityLoaded := true;

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

  UpdatePortabilityPage;
end;

procedure TfSettings.SaveSettings;
var ini: TCustomIniFile;
begin
  FreeSettings(); //trigger reload in case files were changed

  ini := GetWakanIni;
  case PortabilityMode of
    pmStandalone: ini.WriteString('General', 'Install', 'Standalone');
    pmCompatible: ini.WriteString('General', 'Install', 'Compatible');
    pmPortable: ini.WriteString('General', 'Install', 'Portable');
  end;

  ini := GetSettingsStore;
  try
    SaveRegistrySettings(ini);
    WriteColors(ini);
    SaveCharDetl(ini);
  finally
    ini.UpdateFile;
    FreeSettings();
  end;
end;

procedure TfSettings.LoadRegistrySettings(reg: TCustomIniFile);
var s: string;
  tmp_int: integer;
  exmode:integer;
begin
  cbEnableAnnotations.Checked:=reg.ReadBool('Annotate','Enabled',true);
  cbRebuildAnnotations.Checked:=reg.ReadBool('Annotate','Rebuild',true);
  CheckBox66.Checked:=reg.ReadBool('Annotate','Sound',true);
  CheckBox67.Checked:=reg.ReadBool('Annotate','Pictures',true);
  CheckBox68.Checked:=reg.ReadBool('Annotate','WebPages',true);
  CheckBox69.Checked:=reg.ReadBool('Annotate','Colors',true);
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
    SetRomaList(reg.ReadString('Romanization','RomaList',ChangeFileExt(KunreishikiRoma,'')));
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
  CheckBox3.Checked:=reg.ReadBool('Characters','NoShowColors',false);
  cbYomiIgnoreOkurigana.Checked:=reg.ReadBool('Characters','YomiOkurigana',false);

  cbDetailsShowKanjiClass.Checked:=reg.ReadBool('KanjiDetails','ShowKanjiClass',true);
  cbDetailsKanjiInColor.Checked:=reg.ReadBool('KanjiDetails','KanjiInColor',true);
  rgDetailsCategoryEditorType.ItemIndex:=reg.ReadInteger('KanjiDetails','CategoryEditorType',0);
  cbDetailsShowLinks.Checked:=reg.ReadBool('KanjiDetails','ShowLinks',true);

  if reg.ReadString('Fonts','FontSet','0')<>'1' then
    AutoDetectFonts({Silent=}true)
  else begin
   //There's some safety net for cases when font settings are added:
   //although we don't run detection again (it'd reset other fonts), we choose
   //a default font name for a new setting and later check if it's available.
    FontJapanese:=reg.ReadString('Fonts','Japanese','MS Mincho');
    FontJapaneseGrid:=reg.ReadString('Fonts','JapaneseGrid','MS Mincho');
    FontChinese:=reg.ReadString('Fonts','Chinese','MingLiU');
    FontChineseGrid:=reg.ReadString('Fonts','ChineseGrid','MingLiU');
    FontChineseGB:=reg.ReadString('Fonts','ChineseGB','SimSun');
    FontChineseGridGB:=reg.ReadString('Fonts','ChineseGridGB','SimSun');
    FontSmall:=reg.ReadString('Fonts','Small','MS Gothic');
    FontRadical:=reg.ReadString('Fonts','Radical','MingLiU');
    FontEnglish:=reg.ReadString('Fonts','English','Verdana');
    FontStrokeOrder:=reg.ReadString('Fonts','StrokeOrder','MS Mincho');
    FontPinYin:=reg.ReadString('Fonts','PinYin','Arial');
  end;
  UpdateFontNames;
  CheckBox4.Checked:=reg.ReadBool('Dict','PreferUser',true);
  CheckBox5.Checked:=reg.ReadBool('Dict','PreferNouns',true);
  CheckBox6.Checked:=reg.ReadBool('Dict','PreferPolite',true);
  CheckBox7.Checked:=reg.ReadBool('Dict','PreferPopular',true);
  if fWordLookup<>nil then
    fWordLookup.sbAutoPreview.Down:=reg.ReadBool('Dict','QuickSearch',true);
  cbReplaceKanji.Checked:=reg.ReadBool('Dict','ReplaceKanji',true);
  cbNoGridColors.Checked:=reg.ReadBool('Dict','NoUseColors',false);
  CheckBox10.Checked:=reg.ReadBool('Dict','UseGrey',false);
  cbStatusColors.Checked:=reg.ReadBool('Dict','StatusColors',true);
  cbDictLimitAutoResults.Checked:=reg.ReadBool('Dict','AutoPage',true);
  CheckBox49.Checked:=reg.ReadBool('Dict','DemandLoad',true);
  CheckBox50.Checked:=reg.ReadBool('Dict','AutoExamples',true);
  cbMultilineGrids.Checked:=reg.ReadBool('Dict','MultiLineGrids',true);
  cbShowFreq.Checked:=reg.ReadBool('Dict','ShowFreq',false);
  cbOrderFreq.Checked:=reg.ReadBool('Dict','OrderFreq',true);
  cbDictRefLinksInSubmenu.Checked:=reg.ReadBool('Dict','RefLinksInSubmenu',true);

  CheckBox60.Checked:=reg.ReadBool('Editor','AutoSave',false);
  CheckBox61.Checked:=reg.ReadBool('Editor','AutoLoad',false);
  cbNoSaveChangesWarning.Checked:=reg.ReadBool('Editor','NoSaveChangesWarning',false);
  cbLoadAozoraRuby.Checked:=reg.readBool('Editor','LoadAozoraRuby', true);
  cbAozoraTagsInColor.Checked:=reg.readBool('Editor','AozoraTagsInColor', true);
  cbSaveAnnotationsToRuby.Checked:=reg.readBool('Editor','SaveAnnotationsToRuby', false);
  cbAdjustCharPriorities.Checked:=reg.readBool('Editor','AdjustCharPriorities', true);
  rgReleaseCursorMode.ItemIndex := reg.ReadInteger('Editor','ReleaseCursorMode',0);
  if CheckBox61.Checked then
  begin
    fEditor.DocFileName:=Reg.ReadString('Editor','DocFileName',''); //Will load later if DocFileName<>''
    fEditor.DocType:=TDocType(Reg.ReadInteger('Editor','DocType',0));
    fEditor.DocEncoding:=FindEncoding(Reg.ReadString('Editor','DocType',''));
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
  DefaultCopyFormatName:=reg.ReadString('Dict','CopyFormat','');
  GridFontSize:=strtoint(Edit25.text);
  lbWordPrintFormat.ItemIndex:=reg.ReadInteger('WordSheet','Columns',0);
  cbInsideLines.Checked:=reg.ReadBool('WordSheet','InsideLines',true);
  cbOutsideLines.Checked:=reg.ReadBool('WordSheet','OutsideLines',true);
  CheckBox16.Checked:=reg.ReadBool('WordSheet','VaryColors',true);
  CheckBox17.Checked:=reg.ReadBool('WordSheet','PrintUnlearned',true);
  Edit10.Text:=inttostr(reg.ReadInteger('WordSheet','NoLines',40));
  Edit16.Text:=reg.ReadString('WordSheet','UserColumns','p1--m1--');
  Edit11.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCharacters',10));
  Edit12.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCompoundsH',10));
  Edit13.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCompoundsV',6));
  Edit35.Text:=inttostr(reg.ReadInteger('KanjiCards','NoFullCompounds',4));
  Edit14.Text:=reg.ReadString('KanjiCards','Font','MingLiU');
  dicts.NotUsedDicts:=reg.ReadString('Dict','NotUsedDicts','');
  dicts.NotGroupDicts[1]:=reg.ReadString('Dict','NotGroup1Dicts','');
  dicts.NotGroupDicts[2]:=reg.ReadString('Dict','NotGroup2Dicts','');
  dicts.NotGroupDicts[3]:=reg.ReadString('Dict','NotGroup3Dicts','');
  dicts.NotGroupDicts[4]:=reg.ReadString('Dict','NotGroup4Dicts','');
  dicts.NotGroupDicts[5]:=reg.ReadString('Dict','NotGroup5Dicts','');
  dicts.OfflineDicts:=reg.ReadString('Dict','OfflineDicts','');
  dicts.Priority.Text := repl(reg.ReadString('Dict','Priority',''),',',#13#10);
  CheckBox18.Checked:=reg.ReadBool('KanjiCards','PrintCompounds',true);
  CheckBox19.Checked:=reg.ReadBool('KanjiCards','PrintRadical',true);
  CheckBox20.Checked:=reg.ReadBool('KanjiCards','PrintAlternate',true);
  CheckBox21.Checked:=reg.ReadBool('KanjiCards','PrintReadings',true);
  CheckBox22.Checked:=reg.ReadBool('KanjiCards','PrintOuterLines',true);
  CheckBox23.Checked:=reg.ReadBool('KanjiCards','PrintInnerLines',true);
  CheckBox24.Checked:=reg.ReadBool('KanjiCards','PrintVertical',true);
  CheckBox25.Checked:=reg.ReadBool('KanjiCards','ColumnSpace',true);
  CheckBox44.Checked:=reg.ReadBool('KanjiCards','PrintDefinition',true);
  CheckBox45.Checked:=reg.ReadBool('KanjiCards','PrintStrokeCount',false);
  CheckBox52.Checked:=reg.ReadBool('KanjiCards','PrintStrokeOrder',false);
  CheckBox62.Checked:=reg.ReadBool('KanjiCards','PrintFullComp',true);
  CheckBox63.Checked:=reg.ReadBool('KanjiCards','SortFrequency',true);
  CheckBox26.Checked:=reg.ReadBool('Vocabulary','SaveStat',false);
  if fWordLookup<>nil then
    fWordLookup.btnInflect.Down:=reg.ReadBool('Dict','DeflexItalic',true);
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
  if fEditor<>nil then begin
    fEditor.sbDisplayReading.Down:=reg.ReadBool('Translate','Reading',true);
    fEditor.sbDisplayMeaning.Down:=reg.ReadBool('Translate','Meaning',true);
    fEditor.sbUseTlColors.Down:=reg.ReadBool('Translate','TransColors',true);
    fEditor.sbDisplayReadingClick(fEditor.sbDisplayReading);
    fEditor.sbDisplayMeaningClick(fEditor.sbDisplayMeaning);
    fEditor.sbUseTlColorsClick(fEditor.sbUseTlColors);
    fEditor.sbDockDictionary.Down:=reg.ReadBool('Translate','Dictionary',false);
  end;
  CheckBox28.Checked:=reg.ReadBool('ScreenTrans','Japanese',true);
  CheckBox47.Checked:=reg.ReadBool('ScreenTrans','English',true);
  CheckBox48.Checked:=reg.ReadBool('ScreenTrans','Kanji',true);
  Edit21.Text:=reg.ReadString('ScreenTrans','Delay','10');
  Edit22.Text:=reg.ReadString('ScreenTrans','LeftRange','20');
  Edit23.Text:=reg.ReadString('ScreenTrans','RightRange','100');
  Edit24.Text:=reg.ReadString('ScreenTrans','DictEntries','4');
  Edit26.Text:=reg.ReadString('ScreenTrans','SizeFactor','12');
  Edit27.Text:=reg.ReadString('ScreenTrans','MinCompounds','10');
  Edit28.Text:=reg.ReadString('ScreenTrans','MaxCompounds','40');
  fMenu.SpeedButton2.Down:=reg.ReadBool('ScreenTrans','WakanToolTip',true);
  fMenu.screenModeWk:=fMenu.SpeedButton2.Down;
  if fEditor<>nil then begin
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
  setsort:=reg.ReadInteger('Characters','Sort',0);
  kanji_othersearch:=reg.ReadInteger('Characters','OtherSearch',0);
  setusercompounds:=reg.ReadBool('Characters','UserCompounds',false);

  if fWordLookup<>nil then begin
    if reg.ReadBool('Dict','Meaning',false) then
      fWordLookup.dictModeSet := lmEn
    else
      fWordLookup.dictModeSet := lmAuto;
    fWordLookup.dictBeginSet := reg.ReadInteger('Dict','SearchBeg',0);
  end;

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
  if cbSaveSearchParams.Checked and (fKanjiSearch<>nil) then begin
    fKanjiSearch.btnOnlyCommon.Down :=reg.ReadBool('KanjiSearch','OnlyCommon',false);
//    fKanjiSearch.btnInClipboard.Down :=reg.ReadBool('KanjiSearch','InClipboard',false); //do not save-restore this for now (by design)
    fKanjiSearch.edtPinYin.Text :=reg.ReadString('KanjiSearch','PinYin','');
    fKanjiSearch.edtYomi.Text :=reg.ReadString('KanjiSearch','Yomi','');
    fKanjiSearch.edtDefinition.Text :=reg.ReadString('KanjiSearch','Definition','');
    fKanjiSearch.edtStrokeCount.Text :=reg.ReadString('KanjiSearch','Strokes','');
    fKanjiSearch.curRadSearchType :=TRadSearchType(reg.ReadInteger('KanjiSearch','RadSearchType',0));
    fKanjiSearch.curRadChars := reg.ReadString('KanjiSearch','RadSearch','');
    fKanjiSearch.edtSKIP.Text :=reg.ReadString('KanjiSearch','SKIP','');
    fKanjiSearch.edtJouyou.Text :=reg.ReadString('KanjiSearch','Jouyou','');
    fKanjiSearch.cbOtherType.ItemIndex :=reg.ReadInteger('KanjiSearch','OtherCriteriaIndex',-1);
    fKanjiSearch.edtOther.text :=reg.ReadString('KanjiSearch','Other','');
  end; //else they're empty by default

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
    fKanjiCompounds.ClientHeight := reg.ReadInteger('Layout','KanjiCompoundsHeight',178);
  end;

 //Read UI settings to apply later
  CharDetDocked := reg.ReadBool('Layout','CharDetailsDocked',false);
  CharDetDockedVis1 := reg.ReadBool('Layout','CharDetailsVisible1',true);
  CharDetDockedVis2 := reg.ReadBool('Layout','CharDetailsVisible2',true);
  SetLayout := reg.ReadInteger('Layout','DisplayLayout',1);
  SetWindows := reg.ReadInteger('Layout','SecondaryWindows',72);
end;

procedure TfSettings.SaveRegistrySettings(reg: TCustomIniFile);
var setwindows:integer;
  exmode:integer;
  tmp_str: string;
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
  reg.WriteBool('Characters','NoShowColors',CheckBox3.Checked);
  reg.WriteBool('Characters','YomiOkurigana',cbYomiIgnoreOkurigana.Checked);
  reg.WriteBool('Characters','CompoundsBeg',fKanjiCompounds.cbLeftMatchOnly.Checked);
  reg.WriteBool('Characters','CompoundsPop',fKanjiCompounds.cbPopularOnly.Checked);
  reg.WriteBool('Characters','CompoundsFreq',fKanjiCompounds.cbSortByFrequency.Checked);

  reg.WriteBool('KanjiDetails','ShowKanjiClass',cbDetailsShowKanjiClass.Checked);
  reg.WriteBool('KanjiDetails','KanjiInColor',cbDetailsKanjiInColor.Checked);
  reg.WriteInteger('KanjiDetails','CategoryEditorType',rgDetailsCategoryEditorType.ItemIndex);
  reg.WriteBool('KanjiDetails','ShowLinks',cbDetailsShowLinks.Checked);

  reg.WriteString('Fonts','JapaneseGrid',FontJapaneseGrid);
  reg.WriteString('Fonts','Japanese',FontJapanese);
  reg.WriteString('Fonts','Small',FontSmall);
  reg.WriteString('Fonts','ChineseGrid',FontChineseGrid);
  reg.WriteString('Fonts','ChineseGridGB',FontChineseGridGB);
  reg.WriteString('Fonts','Chinese',FontChinese);
  reg.WriteString('Fonts','ChineseGB',FontChineseGB);
  reg.WriteString('Fonts','Radical',FontRadical);
  reg.WriteString('Fonts','English',FontEnglish);
  reg.WriteString('Fonts','StrokeOrder',FontStrokeOrder);
  reg.WriteString('Fonts','PinYin',FontPinYin);
  reg.WriteString('Fonts','FontSet','1');
  reg.WriteBool('Dict','PreferUser',CheckBox4.Checked);
  reg.WriteBool('Dict','PreferNouns',CheckBox5.Checked);
  reg.WriteBool('Dict','PreferPolite',CheckBox6.Checked);
  reg.WriteBool('Dict','PreferPopular',CheckBox7.Checked);
  reg.WriteBool('Dict','QuickSearch',fWordLookup.sbAutoPreview.Down);
  reg.WriteBool('Dict','ReplaceKanji',cbReplaceKanji.Checked);
  reg.WriteBool('Dict','NoUseColors',cbNoGridColors.Checked);
  reg.WriteBool('Dict','UseGrey',CheckBox10.Checked);
  reg.WriteBool('Dict','StatusColors',cbStatusColors.Checked);
  reg.WriteBool('Dict','AutoPage',cbDictLimitAutoResults.Checked);
  reg.WriteBool('Dict','DemandLoad',CheckBox49.Checked);
  reg.WriteBool('Dict','AutoExamples',CheckBox50.Checked);
  reg.WriteBool('Dict','RandomExamples',fExamples.btnRandomOrder.Down);
  reg.WriteBool('Dict','ShowFreq',cbShowFreq.Checked);
  reg.WriteBool('Dict','OrderFreq',cbOrderFreq.Checked);
  reg.WriteBool('Dict','RefLinksInSubmenu',cbDictRefLinksInSubmenu.Checked);

  reg.WriteBool('Editor','AutoSave',CheckBox60.Checked);
  reg.WriteBool('Editor','AutoLoad',CheckBox61.Checked);
  reg.WriteBool('Editor','NoSaveChangesWarning',cbNoSaveChangesWarning.Checked);
  reg.WriteBool('Editor','LoadAozoraRuby',cbLoadAozoraRuby.Checked);
  reg.WriteBool('Editor','AozoraTagsInColor',cbAozoraTagsInColor.Checked);
  reg.WriteBool('Editor','SaveAnnotationsToRuby',cbSaveAnnotationsToRuby.Checked);
  reg.WriteBool('Editor','AdjustCharPriorities',cbAdjustCharPriorities.Checked);
  reg.WriteInteger('Editor','ReleaseCursorMode',rgReleaseCursorMode.ItemIndex);
  reg.WriteString('Editor','DocFilename',fEditor.DocFilename); //For autoload
  reg.WriteInteger('Editor','DocType',integer(fEditor.DocType)); //This too.
  if fEditor.DocEncoding<>nil then
    reg.WriteString('Editor','DocEncoding',string(fEditor.DocEncoding.Classname))
  else
    reg.WriteString('Editor','DocEncoding','');
  reg.WriteInteger('Characters','FreqLimit',strtoint(Edit34.Text));
  if fExamples.btnDisplayTranslation.Down then exmode:=0 else
  if fExamples.btnUseBigFont.Down then exmode:=1 else
  if fExamples.btnUseSmallFont.Down then exmode:=2 else
    exmode := 0;
  reg.WriteInteger('Dict','ExMode',exmode);
  reg.WriteInteger('Dict','FontSize',strtoint(Edit25.text));
  reg.WriteBool('Dict','MultiLineGrid',cbMultilineGrids.Checked);
  reg.WriteString('Dict','CopyFormat',DefaultCopyFormatName);
  reg.WriteInteger('WordSheet','Columns',lbWordPrintFormat.ItemIndex);
  reg.WriteBool('WordSheet','InsideLines',cbInsideLines.Checked);
  reg.WriteBool('WordSheet','OutsideLines',cbOutsideLines.Checked);
  reg.WriteBool('WordSheet','VaryColors',CheckBox16.Checked);
  reg.WriteBool('WordSheet','PrintUnlearned',CheckBox17.Checked);
  reg.WriteInteger('WordSheet','NoLines',strtoint(Edit10.Text));
  reg.WriteString('WordSheet','UserColumns',Edit16.Text);
  reg.WriteInteger('KanjiCards','NoCharacters',strtoint(Edit11.Text));
  reg.WriteInteger('KanjiCards','NoCompoundsH',strtoint(Edit12.Text));
  reg.WriteInteger('KanjiCards','NoCompoundsV',strtoint(Edit13.Text));
  reg.WriteInteger('KanjiCards','NoFullCompounds',strtoint(Edit35.Text));
  reg.WriteString('KanjiCards','Font',Edit14.text);
  reg.WriteBool('KanjiCards','PrintCompounds',CheckBox18.Checked);
  reg.WriteBool('KanjiCards','PrintRadical',CheckBox19.Checked);
  reg.WriteBool('KanjiCards','PrintAlternate',CheckBox20.Checked);
  reg.WriteBool('KanjiCards','PrintReadings',CheckBox21.Checked);
  reg.WriteBool('KanjiCards','PrintOuterLines',CheckBox22.Checked);
  reg.WriteBool('KanjiCards','PrintInnerLines',CheckBox23.Checked);
  reg.WriteBool('KanjiCards','PrintVertical',CheckBox24.Checked);
  reg.WriteBool('KanjiCards','PrintStrokeOrder',CheckBox52.Checked);
  reg.WriteBool('KanjiCards','ColumnSpace',CheckBox25.Checked);
  reg.WriteBool('KanjiCards','PrintDefinition',CheckBox44.Checked);
  reg.WriteBool('KanjiCards','PrintStrokeCount',CheckBox45.Checked);
  reg.WriteBool('KanjiCards','PrintFullComp',CheckBox62.Checked);
  reg.WriteBool('KanjiCards','SortFrequency',CheckBox63.Checked);
  reg.WriteBool('Vocabulary','SaveStat',CheckBox26.Checked);
  reg.WriteBool('Dict','DeflexItalic',fWordLookup.btnInflect.Down);
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
  reg.WriteBool('Translate','Reading',fEditor.sbDisplayReading.Down);
  reg.WriteBool('Translate','Meaning',fEditor.sbDisplayMeaning.Down);
  reg.WriteBool('Translate','TransColors',fEditor.sbUseTlColors.Down);
  reg.WriteBool('Translate','Dictionary',fEditor.sbDockDictionary.Down);
  reg.WriteBool('Translate','NoLongTextWarning',cbTranslateNoLongTextWarning.Checked);
  reg.WriteBool('Translate','MultithreadedTranslation',cbMultithreadedTranslation.Checked);
  reg.WriteBool('Annotate','Enabled',cbEnableAnnotations.Checked);
  reg.WriteBool('Annotate','Rebuild',cbRebuildAnnotations.Checked);
  reg.WriteBool('Annotate','Sound',CheckBox66.Checked);
  reg.WriteBool('Annotate','Pictures',CheckBox67.Checked);
  reg.WriteBool('Annotate','WebPages',CheckBox68.Checked);
  reg.WriteBool('Annotate','Colors',CheckBox69.Checked);
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
  reg.WriteInteger('Characters','Sort',fKanjiSearch.rgSortBy.ItemIndex);
  reg.WriteInteger('Characters','OtherSearch',fKanjiSearch.cbOtherType.ItemIndex);
  reg.WriteBool('Characters','UserCompounds',fKanjiCompounds.sbShowVocab.Down);

  if fWordLookup<>nil then begin
    reg.WriteBool('Dict','Meaning',fWordLookup.dictModeSet=lmEn);
    reg.WriteInteger('Dict','SearchBeg',fWordLookup.dictBeginSet);
  end;

  reg.WriteBool('Translate','ShowHint',cbShowEditorHint.Checked);
  reg.WriteBool('Translate','HintMeaning',cbHintMeaning.Checked);
  reg.WriteInteger('Layout','DisplayLayout',fMenu.DisplayMode);
  reg.WriteBool('Layout','CharDetailsDocked',fMenu.CharDetDocked);
  reg.WriteBool('Layout','CharDetailsVisible1',fMenu.CharDetDockedVis1);
  reg.WriteBool('Layout','CharDetailsVisible2',fMenu.CharDetDockedVis2);
  reg.WriteBool('ScreenTrans','Japanese',CheckBox28.Checked);
  reg.WriteBool('ScreenTrans','English',CheckBox47.Checked);
  reg.WriteBool('ScreenTrans','Kanji',CheckBox48.Checked);
  reg.WriteString('ScreenTrans','Delay',Edit21.Text);
  reg.WriteString('ScreenTrans','LeftRange',Edit22.Text);
  reg.WriteString('ScreenTrans','RightRange',Edit23.Text);
  reg.WriteString('ScreenTrans','DictEntries',Edit24.Text);
  reg.WriteString('ScreenTrans','SizeFactor',Edit26.Text);
  reg.WriteString('ScreenTrans','MinCompounds',Edit27.Text);
  reg.WriteString('ScreenTrans','MaxCompounds',Edit28.Text);
  reg.WriteBool('ScreenTrans','WakanToolTip',fMenu.SpeedButton2.Down);

  setwindows:=0;
  if fMenu.aKanjiSearch.Checked then inc(setwindows,1);
  if fMenu.aKanjiCompounds.Checked then inc(setwindows,2);
  if fMenu.aDictKanji.Checked then inc(setwindows,4);
  if fMenu.aDictExamples.Checked then inc(setwindows,8);
  if fMenu.aUserExamples.Checked then inc(setwindows,16);
  if fMenu.aUserDetails.Checked then inc(setwindows,32);
  if fMenu.aUserSettings.Checked then inc(setwindows,64);
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

  if cbSaveSearchParams.Checked then begin
    reg.WriteBool('KanjiSearch','OnlyCommon',fKanjiSearch.btnOnlyCommon.Down);
//    reg.WriteBool('KanjiSearch','InClipboard',fKanjiSearch.btnInClipboard.Down); //do not save-restore this for now (by design)
    if fKanjiSearch.sbPinYin.Down then
      reg.WriteString('KanjiSearch','PinYin',fKanjiSearch.edtPinYin.Text)
    else
      reg.DeleteKey('KanjiSearch','PinYin');
    if fKanjiSearch.sbYomi.Down then
      reg.WriteString('KanjiSearch','Yomi',fKanjiSearch.edtYomi.Text)
    else
      reg.DeleteKey('KanjiSearch','Yomi');
    if fKanjiSearch.sbDefinition.Down then
      reg.WriteString('KanjiSearch','Definition',fKanjiSearch.edtDefinition.Text)
    else
      reg.DeleteKey('KanjiSearch','Definition');
    if fKanjiSearch.sbStrokeCount.Down then
      reg.WriteString('KanjiSearch','Strokes',fKanjiSearch.edtStrokeCount.Text)
    else
      reg.DeleteKey('KanjiSearch','Strokes');
    if fKanjiSearch.sbRadicals.Down then begin
      reg.WriteInteger('KanjiSearch','RadSearchType',integer(fKanjiSearch.curRadSearchType));
      reg.WriteString('KanjiSearch','RadSearch',fKanjiSearch.curRadChars);
    end else begin
      reg.DeleteKey('KanjiSearch','RadSearchType');
      reg.DeleteKey('KanjiSearch','RadSearch');
      reg.DeleteKey('KanjiSearch','RadIndexes');
    end;
    if fKanjiSearch.sbSKIP.Down then
      reg.WriteString('KanjiSearch','SKIP',fKanjiSearch.edtSKIP.Text)
    else
      reg.DeleteKey('KanjiSearch','SKIP');
    if fKanjiSearch.sbJouyou.Down then
      reg.WriteString('KanjiSearch','Jouyou',fKanjiSearch.edtJouyou.Text)
    else
      reg.DeleteKey('KanjiSearch','Jouyou');
    if fKanjiSearch.sbOther.Down then begin
      reg.WriteInteger('KanjiSearch','OtherCriteriaIndex',fKanjiSearch.cbOtherType.ItemIndex);
      reg.WriteString('KanjiSearch','Other',fKanjiSearch.edtOther.Text);
    end else begin
      reg.DeleteKey('KanjiSearch','OtherCriteriaIndex');
      reg.DeleteKey('KanjiSearch','Other');
    end;
  end;

  reg.WriteBool('Layout','PortraitMode',fMenu.aPortraitMode.Checked);

 //These are updated with the actual docked width and height on resizes,
 //while ClientWidth and ClientHeight might be invalid because the window's aligned to a full parent width or height
  reg.WriteInteger('Layout','UserFiltersWidth',fVocabFilters.UndockWidth);
  reg.WriteInteger('Layout','UserFiltersHeight',fVocabFilters.UndockHeight);
  reg.WriteInteger('Layout','UserDetailsHeight',fVocabDetails.UndockHeight);
  reg.WriteInteger('Layout','KanjiCompoundsHeight',fKanjiCompounds.UndockHeight);
end;

function TfSettings.GetTranslationFile: string;
var ini: TCustomIniFile;
begin
  ini := GetSettingsStore;
  if ini=nil then
    Result := ''
  else
    Result := ini.ReadString('Language','LNGFile','');
end;

procedure TfSettings.SetTranslationFile(const Value: string);
var ini: TCustomIniFile;
begin
  ini := GetSettingsStore;
  if ini=nil then exit; //no store yet configured, cannot save
  ini.WriteString('Language', 'LNGFile', curTransFile);
  ini.UpdateFile;
end;



procedure TfSettings.btnChangeLanguageClick(Sender: TObject);
begin
  fLanguage.SelectLanguage;
end;

procedure TfSettings.SpeedButton1Click(Sender: TObject);
var sup:string;
begin
  FontJapaneseGrid:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edit1.text,false);
  Edit1.Text:=FontJapaneseGrid;
end;

procedure TfSettings.SpeedButton4Click(Sender: TObject);
var sup:string;
begin
  FontEnglish:=ChooseFont([ANSI_CHARSET],FS_ENGLISH_CHARTEST,sup,edit4.text,false);
  Edit4.Text:=FontEnglish;
end;

procedure TfSettings.SpeedButton5Click(Sender: TObject);
var sup:string;
begin
  FontSmall:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edit5.text,false);
  Edit5.Text:=FontSmall;
end;

procedure TfSettings.SpeedButton2Click(Sender: TObject);
var sup:string;
begin
  FontJapanese:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edit2.text,false);
  Edit2.Text:=FontJapanese;
end;

procedure TfSettings.SpeedButton6Click(Sender: TObject);
var sup:string;
begin
  FontChineseGrid:=ChooseFont([CHINESEBIG5_CHARSET],FS_CHINESE_CHARTEST,sup,edit6.text,false);
  Edit6.Text:=FontChineseGrid;
end;

procedure TfSettings.SpeedButton7Click(Sender: TObject);
var sup:string;
begin
  FontChinese:=ChooseFont([CHINESEBIG5_CHARSET],FS_CHINESE_CHARTEST,sup,Edit7.text,false);
  Edit7.Text:=FontChinese;
end;

procedure TfSettings.SpeedButton8Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(
    pchar(_l('#00564^ePlease make sure that the font you select is a Unicode font with '
      +'complete "CJK Unified Ideographs" range.'#13'MingLiu is an example of such font.')),
    pchar(_l('#00364^eNotice')),
    MB_OK or MB_ICONINFORMATION);
  FontRadical:=ChooseFont([CHINESEBIG5_CHARSET],FS_RADICAL_CHARTEST,sup,Edit8.Text,false);
  Edit8.Text:=FontRadical;
end;

procedure TfSettings.SpeedButton3Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(
    pchar(_l('#00565^eWhen selecting the font you must ensure that it is really '
      +'a simplified chinese font.'#13'Some fonts support GB2312 standard but '
      +'instead of simplified characters they show the traditional ones.'#13
      +'These fonts would display improper characters in this program.'#13
      +'You can identify the right font very easily. In the font selection dialog'#13
      +'look at the first character. If it does not look like an arrow pointing up, '
      +'you should select another font.')),
    pchar(_l('#00566^eNotice')),
    MB_OK or MB_ICONINFORMATION);
  FontChineseGridGB:=ChooseFont([GB2312_CHARSET],FS_CHINESEGB_CHARTEST,sup,Edit3.Text,false);
  Edit3.Text:=FontChineseGridGB;
end;

procedure TfSettings.SpeedButton9Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(
    pchar(_l('#00565^eWhen selecting the font you must ensure that it is really '
      +'a simplified chinese font.'#13'Some fonts support GB2312 standard but '
      +'instead of simplified characters they show the traditional ones.'#13
      +'These fonts would display improper characters in this program.'#13
      +'You can identify the right font very easily. In the font selection dialog'#13
      +'look at the first character. If it does not look like an arrow pointing up, '
      +'you should select another font.')),
    pchar(_l('#00566^eNotice')),
    MB_OK or MB_ICONINFORMATION);
  FontChineseGB:=ChooseFont([GB2312_CHARSET],FS_CHINESEGB_CHARTEST,sup,Edit9.Text,false);
  Edit9.Text:=FontChineseGB;
end;

procedure TfSettings.SpeedButton10Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(
    pchar(_l('#00567^eSome fonts may not be able to display some characters. '
      +'If you see that some characters look different in style or aren''t '
      +'displayed properly, select another font.')),
    pchar(_l('#00566^eNotice')),
  MB_OK or MB_ICONINFORMATION);
  Edit14.Text:=ChooseFont([CHINESEBIG5_CHARSET,GB2312_CHARSET,SHIFTJIS_CHARSET],testkanji,sup,Edit9.Text,false);
end;

procedure TfSettings.Button1Click(Sender: TObject);
begin
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
end;

procedure TfSettings.Button2Click(Sender: TObject);
begin
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
end;

procedure TfSettings.Button3Click(Sender: TObject);
begin
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
end;

procedure TfSettings.lbWordPrintFormatClick(Sender: TObject);
begin
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
end;

procedure TfSettings.Button4Click(Sender: TObject);
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
end;

//Silent: do not say anything in case of success
function TfSettings.AutoDetectFonts(Silent:boolean):boolean;
var s:string;
  missingfonts:string;
  substituted:boolean; //found substitutions for all missing fonts
  FMincho,FGothic: string;
  FMingLiu,FSimSun: string;

  function _FindFont(const face: string; charset: integer): string;
  begin
    Result := FindFont(face,charset);
    if (face<>'') and (Result='') then
      missingfonts := missingfonts+','+face;
  end;

begin
  missingfonts:='';
  substituted:=true;

 { NOTE: Some asian fonts have two names, English one and localized one.
  Since Windows 2000 both CreateFont and EnumFontFamilies understand both names,
  although Delphi's Screen.Fonts will only list one. }

  //Japanese fonts
  FMincho:=_FindFont('MS Mincho',DEFAULT_CHARSET);
  FGothic:=_FindFont('MS Gothic',DEFAULT_CHARSET);

  //Substitutions
  if (FMincho='') and (FGothic<>'') then
    FMincho:=FGothic
  else
  if (FMincho<>'') and (FGothic='') then
    FGothic:=FMincho
  else
  if (FMincho='') and (FGothic='') then begin
    FMincho:=_FindFont('',SHIFTJIS_CHARSET); //any japanese
    if FMincho='' then begin
      substituted:=false; //no japanese at all
      FMincho:='Arial';
    end;
    FGothic:=FMincho;
  end;

  //Chinese fonts
  FMingLiu:=_FindFont('MingLiU',DEFAULT_CHARSET);
  FSimSun:=_FindFont('SimSun',DEFAULT_CHARSET);

  //Substitutions
  if (FMingLiu='') and (FSimSun<>'') then
    FMingLiu:=FSimSun
  else
  if (FMingLiu<>'') and (FSimSun='') then
    FSimSun:=FMingLiu
  else
  if (FMingLiu='') and (FSimSun='') then begin //both missing
    FMingLiu:=_FindFont('',CHINESEBIG5_CHARSET);
    FSimSun:=_FindFont('',GB2312_CHARSET);
    if (FMingLiu='') or (FSimSun='') then begin
      substituted:=false;
      if FMingLiu='' then FMingLiu:='Arial';
      if FSimSun='' then FSimSun:='Arial';
    end;
  end;

 //Use
  FontJapanese:=FMincho;
  FontJapaneseGrid:=FMincho;
  FontChinese:=FMingLiu;
  FontChineseGrid:=FMingLiu;
  FontChineseGB:=FSimSun;
  FontChineseGridGB:=FSimSun;
  FontSmall:=FGothic;
  FontRadical:=FMingLiu;
  FontEnglish:='Verdana';
  FontPinYin:='Arial';
  FontStrokeOrder:=FMincho;
  UpdateFontNames;

  if missingfonts<>'' then
  begin
    delete(missingfonts,1,1);
    s:=_l('#00578^eFollowing recommended fonts were not found on your system:')+#13#13;
    s:=s+missingfonts+#13#13;
    if substituted then
      s:=s+_l('#00579^eReasonable substitution was found, however for better '
        +'font quality installation of these fonts and restart'#13'of the '
        +'autodetection routine is recommended.')
    else
      s:=s+_l('#00580^eNo reasonable substitution was found, program will '
        +'probably display characters incorrectly.'#13+'Installation of these '
        +'fonts and restart of the autodetection routine is HIGHLY recommended.');
    s:=s+#13#13;
    s:=s+_l('#00581^eThese fonts can be found in following Microsoft(R) products:'#13#13
      +'1. Microsoft(R) Windows(R) XP - Select "Install eastern-Asian fonts" in language settings in Control Panel.'#13
      +'2. Microsoft(R) Internet Explorer - Install "support for Japanese, traditional Chinese and simplified Chinese writing".'#13
      +'3. Microsoft(R) Office 2000/XP - Install Japanese, traditional Chinese and simplified Chinese fonts.');
    if substituted then
      Application.MessageBox(
        pchar(s),
        pchar(_l('#00582^eMissing fonts')),
        MB_ICONWARNING or MB_OK)
    else
      Application.MessageBox(
        pchar(s),
        pchar(_l('#00582^eMissing fonts')),
        MB_ICONERROR or MB_OK);
  end else
    if not Silent then
      Application.MessageBox(
        pchar(_l('#00583^eCongratulations!'#13#13'All recommended fonts were '
          +'located on your system and were installed.')),
        pchar(_l('#00350^eFont autodetection')),
        MB_ICONINFORMATION or MB_OK);
  result:=(missingfonts='') or (substituted);
end;

procedure TfSettings.Button5Click(Sender: TObject);
begin
  AutoDetectFonts({Silent=}false);
end;

function CheckFont(var face:string):boolean;
begin
  while (Length(face)>0) and (face[1]='!') do
    delete(face,1,1);
  Result := FindFont(face,DEFAULT_CHARSET)<>'';
  if not Result then
    face:='!'+face;
end;

{ Checks that all fonts are configured correctly and present in the system.
If not, opens the configuration dialog to let the user choose some new ones.
Used at loading:
1. As a part of auto-detection (fonts which weren't detected can be left blank to later ask the user)
2. When new font setting is added to the app, the user will be asked on the next run to provide a font, if the default one is missing }
procedure TfSettings.CheckFontsPresent;
var OldPosition: TPosition;
begin
  while
       not CheckFont(FontJapanese)
    or not CheckFont(FontJapaneseGrid)
    or not CheckFont(FontChinese)
    or not CheckFont(FontChineseGrid)
    or not CheckFont(FontChineseGB)
    or not CheckFont(FontChineseGridGB)
    or not CheckFont(FontSmall)
    or not CheckFont(FontRadical)
    or not CheckFont(FontEnglish)
    or not CheckFont(FontPinYin)
    or not CheckFont(FontStrokeOrder)
  do
  begin
    Application.MessageBox(
      pchar(_l('#00353^eSome standard fonts were not found on your system.'#13
        +'Please reselect all fonts in the following dialog. Missing fonts are '
        +'preceded by !.'#13
        +'Application cannot continue unless all fonts are selected.')),
      pchar(_l('#00090^eWarning')),
      MB_ICONWARNING or MB_OK);
    OldPosition := Self.Position;
    Self.Position := poScreenCenter;
    Self.UpdateFontNames;
    Self.pcPages.ActivePage:=fSettings.tsFonts;
    Self.ShowModal;
    Self.Position := OldPosition;
  end;
end;

//Updates font names on font selection page
procedure TfSettings.UpdateFontNames;
begin
  Edit2.Text:=FontJapanese;
  Edit1.Text:=FontJapaneseGrid;
  Edit7.Text:=FontChinese;
  Edit6.Text:=FontChineseGrid;
  Edit9.Text:=FontChineseGB;
  Edit3.Text:=FontChineseGridGB;
  Edit5.Text:=FontSmall;
  Edit8.Text:=FontRadical;
  Edit4.Text:=FontEnglish;
  Edit33.Text:=FontPinYin;
  Edit32.Text:=FontStrokeOrder;
end;

{ Populates chardetl with default property set }
procedure TfSettings.ResetCharDetl;
begin
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
end;

procedure TfSettings.LoadCharDetl(reg: TCustomIniFile);
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
end;

procedure TfSettings.SaveCharDetl(reg: TCustomIniFile);
var i: integer;
begin
 //Save to registry
  reg.EraseSection('CharDetl');
  for i := 0 to chardetl.Count-1 do
    reg.WriteString('CharDetl', IntToStr(i), chardetl[i]);

 //Delete wakan.cdt if it's present
  DeleteFile(UserDataDir+'\WAKAN.CDT');
end;


procedure TfSettings.ReloadCharDetlList;
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
end;

procedure TfSettings.Button7Click(Sender: TObject);
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
end;

procedure TfSettings.Button8Click(Sender: TObject);
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
end;

procedure TfSettings.Button9Click(Sender: TObject);
begin
  if ListBox2.ItemIndex<>-1 then
  begin
    chardetl.Delete(ListBox2.ItemIndex);
    ReloadCharDetlList;
  end;
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
  chardetl.Insert(ListBox2.ItemIndex-1,chardetl[ListBox2.ItemIndex]);
  chardetl.Delete(ListBox2.ItemIndex+1);
  ListBox2.ItemIndex:=ListBox2.ItemIndex-1;
  ReloadCharDetlList;
end;

procedure TfSettings.SpeedButton12Click(Sender: TObject);
begin
  chardetl.Insert(ListBox2.ItemIndex+2,chardetl[ListBox2.ItemIndex]);
  chardetl.Delete(ListBox2.ItemIndex);
  ListBox2.ItemIndex:=ListBox2.ItemIndex+1;
  ReloadCharDetlList;
end;

procedure TfSettings.Button11Click(Sender: TObject);
begin
  ShowAllocStats;
end;

procedure TfSettings.ComboBox2Change(Sender: TObject);
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
  CheckBox3.Visible:=ComboBox2.ItemIndex=0;
  cbNoGridColors.Visible:=(ComboBox2.ItemIndex=1) or (ComboBox2.ItemIndex=2);
  cbNoEditorColors.Visible:=ComboBox2.ItemIndex=3;
  case Combobox2.ItemIndex of
    0: v:=not CheckBox3.Checked;
    1: v:=not cbNoGridColors.Checked;
    2: v:=not cbNoGridColors.Checked;
    3: v:=not cbNoEditorColors.Checked;
  else
    v := false;
  end;
  ListBox3.Enabled:=v;
  Button12.Enabled:=v;
  Button14.Enabled:=v;
end;

procedure TfSettings.ListBox3Click(Sender: TObject);
begin
  Shape2.Brush.Color:=GetCol(colorfrom+ListBox3.ItemIndex);
end;

procedure TfSettings.Button15Click(Sender: TObject);
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

procedure TfSettings.CheckBox3Click(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.cbNoColorsClick(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.CheckBox9Click(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.SpeedButton13Click(Sender: TObject);
var sup:string;
begin
  FontStrokeOrder:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edit32.text,false);
  Edit32.Text:=FontStrokeOrder;
end;

procedure TfSettings.SpeedButton14Click(Sender: TObject);
var sup:string;
begin
  FontPinYin:=ChooseFont([ANSI_CHARSET],FS_PINYIN_CHARTEST,sup,edit33.text,false);
  Edit33.Text:=FontPinYin;
end;

procedure TfSettings.Button16Click(Sender: TObject);
begin
  ShellOpen(WikiUrl('Annotations'));
end;

procedure TfSettings.UpdatePortabilityPage;
begin
  case PortabilityMode of
    pmStandalone: lblWakanMode.Caption := _l('#01026^eWakan is running in standalone mode');
    pmCompatible: lblWakanMode.Caption := _l('#01027^eWakan is running in compatible mode');
    pmPortable: lblWakanMode.Caption := _l('#01028^eWakan is running in portable mode');
  else
    lblWakanMode.Caption := '[error]';
  end;

  if PortabilityMode=pmPortable then begin
    lblSettingsPath.Caption := AppFolder + '\wakan.ini';
    lblSettingsPath.URL := 'file://'+repl(lblSettingsPath.Caption,'\','/');
  end else begin
    lblSettingsPath.Caption := 'HKEY_CURRENT_USER\'+WakanRegKey;
    lblSettingsPath.URL := '';
  end;

  lblDictionariesPath.Caption := DictionaryDir;
  lblDictionariesPath.URL := 'file://'+repl(DictionaryDir,'\','/');
  lblUserDataPath.Caption := UserDataDir;
  lblUserDataPath.URL := 'file://'+repl(UserDataDir,'\','/');
  lblBackupPath.Caption := BackupDir;
  lblBackupPath.URL := 'file://'+repl(BackupDir,'\','/');

  btnUpgradeToStandalone.Visible := PortabilityMode=pmCompatible;
  lblUpgradeToStandalone.Visible := btnUpgradeToStandalone.Visible;
end;

procedure TfSettings.lblSettingsPathClick(Sender: TObject);
begin
  if PortabilityMode<>pmPortable then
    RegeditAtKey(lblSettingsPath.Caption);
end;

procedure TfSettings.btnUpgradeToStandaloneClick(Sender: TObject);
var ini: TCustomIniFile;
begin
  if PortabilityMode<>pmCompatible then exit;

  if Application.MessageBox(
    PChar(_l('#01034^eDo you really want to move all your files to Application Data and convert this installation to standalone?')),
    PChar(_l('#01035^eConfirmation')),
    MB_YESNO or MB_ICONQUESTION
  ) <> ID_YES then exit;

  ini := GetWakanIni;
  try
    ini.WriteString('General','Install','Upgrade');
    ini.UpdateFile;
  finally
    FreeSettings;
  end;

  //Localize
  Application.MessageBox(
    PChar(_l('#01036^eWakan has to be restarted for the upgrade to be completed. The application will now exit.')),
    PChar(_l('#01037^eRestart needed')),
    MB_OK
  );
  Application.Terminate;
end;

procedure TfSettings.btnImportKanjidicClick(Sender: TObject);
var fCharDataImport: TfCharDataImport;
begin
  fCharDataImport := TfCharDataImport.Create(Self);
  try
    fCharDataImport.ShowModal;
  finally
    FreeAndNil(fCharDataImport);
  end;
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
var list: string;
  parts: TStringArray;
  i: integer;
begin
  roma_user.Clear;
  list := GetRomaList;
  parts := SplitStr(list,',');
  for i := 0 to Length(parts)-1 do
    roma_user.LoadFromFile(parts[i]+RomajiExt);
end;

{ Same for pinyin }

procedure TfSettings.ReloadPinyinSystems;
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
var list: string;
  parts: TStringArray;
  i: integer;
begin
  rpy_user.Clear;
  list := GetPinyinList;
  parts := SplitStr(list,',');
  for i := 0 to Length(parts)-1 do
    rpy_user.LoadFromFile(parts[i]+PinyinExt);
end;


{ Test romaji/pinyin }

procedure TfSettings.pbRomajiAsHiraganaPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color := clBtnFace;
  DrawUnicode(Canvas,1,1,16,RomajiToKana('H'+edtTestRomaji.Text,'j',[]),FontSmall);
end;

procedure TfSettings.pbRomajiAsKatakanaPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color := clBtnFace;
  DrawUnicode(Canvas,1,1,16,RomajiToKana('K'+edtTestRomaji.Text,'j',[]),FontSmall);
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
  if jshowroma then
    DrawUnicode(Canvas,1,1,16,KanaToRomaji(FKanaExample,'j',[]),FontEnglish)
  else
    DrawUnicode(Canvas,1,1,16,FKanaExample,FontSmall);
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
  DrawUnicode(Canvas,1,1,16,RomajiToKana(edtTestPinyin.Text,'c',[]),FontSmall);
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
  if cshowroma then
    DrawUnicode(Canvas,1,1,16,KanaToRomaji(FBopomofoExample,'c',[]),FontPinyin)
  else
    DrawUnicode(Canvas,1,1,16,FBopomofoExample,FontSmall);
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

{ Default copy format is stored in the registry and we need something invariant
 there, preferably format name, not number.
 But when loading the settings the formats are not yet available, so we read
 the name and resolve it on the first request }
function TfSettings.GetDefaultCopyFormat: integer;
begin
  if FDefaultCopyFormat>=0 then begin
    Result := FDefaultCopyFormat;
    exit;
  end;
  if CopyFormats.Count<1 then begin
   //Nothing we could return even if we wanted to!
    Result := -1;
    exit;
  end else
  if FDefaultCopyFormatName<>'' then begin
   //Resolve format name set earlier
    Result := CopyFormats.Find(FDefaultCopyFormatName);
    FDefaultCopyFormat := Result;
  end else begin
   //Use first one available
    Result := 0;
    exit;
  end;
end;

procedure TfSettings.SetDefaultCopyFormat(const Value: integer);
begin
  FDefaultCopyFormat := Value;
  if (Value<0) or (Value>CopyFormats.Count-1) then
    FDefaultCopyFormatName := ''
  else
    FDefaultCopyFormatName := CopyFormats[FDefaultCopyFormat].Name;
end;

procedure TfSettings.SetDefaultCopyFormatName(const Value: string);
begin
  FDefaultCopyFormatName := Value;
  FDefaultCopyFormat := -1; //resolve next time it's needed
end;

procedure TfSettings.tsDictCopyFormatsShow(Sender: TObject);
begin
  ReloadCopyFormats;
  lblCopyFormatsIni.URL := AppFolder + '/CopyFormats.ini';
  lblCopyFormatsDocumentation.URL := WikiUrl('CopyFormats');
end;

procedure TfSettings.ReloadCopyFormats;
var i: integer;
begin
  lbCopyFormats.Clear;
  for i := 0 to CopyFormats.Count-1 do
    lbCopyFormats.Items.Add(CopyFormats[i].Name);
  lbCopyFormats.ItemIndex := DefaultCopyFormat;
  lbCopyFormatsClick(lbCopyFormats);
end;

procedure TfSettings.lbCopyFormatsClick(Sender: TObject);
begin
  if lbCopyFormats.ItemIndex<0 then exit;
  DefaultCopyFormat := lbCopyFormats.ItemIndex;
  UpdateCopyFormatExample;
end;

procedure TfSettings.UpdateCopyFormatExample;
var res: TSearchResult;
begin
  if FDefaultCopyFormat<0 then begin
    mmCopyFormatExample.Text := '';
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

  mmCopyFormatExample.Text :=
    CopyFormats[FDefaultCopyFormat].FormatResult(@res);
end;

end.
