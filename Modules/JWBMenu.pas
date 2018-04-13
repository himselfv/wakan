unit JWBMenu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Db, ExtCtrls, Grids, Buttons, ShellApi, ActnList, Menus,
  FormPlacemnt, WakanPaintbox, CheckAction, Actions, AppEvnts;

type
  TfMenu = class(TForm)
    Panel1: TPanel;
    tab1: TSpeedButton;
    tab5: TSpeedButton;
    tab2: TSpeedButton;
    btnChineseMode: TSpeedButton;
    Bevel3: TBevel;
    btnJapaneseMode: TSpeedButton;
    btnClipboardClear: TSpeedButton;
    Bevel5: TBevel;
    tab3: TSpeedButton;
    ActionList1: TActionList;
    aSaveUser: TAction;
    aCancelUser: TAction;
    aStatistics: TAction;
    aExit: TAction;
    aKanjiDetails: TCheckAction;
    aKanjiCompounds: TCheckAction;
    aDictKanji: TCheckAction;
    aDictExamples: TCheckAction;
    aVocabAdd: TAction;
    aVocabSettings: TCheckAction;
    aVocabDetails: TCheckAction;
    aVocabPrint: TAction;
    aVocabGenerate: TAction;
    aSettings: TAction;
    aSettingsDict: TAction;
    aHelp: TAction;
    aAbout: TAction;
    aJapanese: TAction;
    aChinese: TAction;
    aKanjiAddClipboard: TAction;
    aKanjiSetLearned: TAction;
    MainMenu: TMainMenu;
    miDatabase: TMenuItem;
    JapMode1: TMenuItem;
    ChinMode1: TMenuItem;
    miSaveUserChanges: TMenuItem;
    miCancelUserChanges: TMenuItem;
    N1: TMenuItem;
    miStatistics: TMenuItem;
    miSettings: TMenuItem;
    miDictionaryManager: TMenuItem;
    N15: TMenuItem;
    miExit: TMenuItem;
    miCharacters: TMenuItem;
    N5: TMenuItem;
    miDisplayAll: TMenuItem;
    miCommonOnly: TMenuItem;
    miInClipboardOnly: TMenuItem;
    miSearchByRadical: TMenuItem;
    miAddToClipboard: TMenuItem;
    miSetAsLearned: TMenuItem;
    miPrintCards: TMenuItem;
    N13: TMenuItem;
    miDetails: TMenuItem;
    miCompounds: TMenuItem;
    eDictionarycSlovnk1: TMenuItem;
    miSearchJapanese: TMenuItem;
    miSearchEnglish: TMenuItem;
    miSearchByClipboard: TMenuItem;
    N8: TMenuItem;
    miSearchExactWord: TMenuItem;
    miSearchBeginning: TMenuItem;
    miSearchEnding: TMenuItem;
    N12: TMenuItem;
    N16: TMenuItem;
    miCharactersInWord: TMenuItem;
    miExamples: TMenuItem;
    Editor2: TMenuItem;
    N22: TMenuItem;
    miEditorNew: TMenuItem;
    miEditorOpen: TMenuItem;
    miEditorSave: TMenuItem;
    miEditorSaveAs: TMenuItem;
    N17: TMenuItem;
    miEditorCut: TMenuItem;
    miEditorCopy: TMenuItem;
    miEditorPaste: TMenuItem;
    miEditorSelectAll: TMenuItem;
    N18: TMenuItem;
    miCharacterMode: TMenuItem;
    miKanaMode: TMenuItem;
    miASCIIMode: TMenuItem;
    N19: TMenuItem;
    miDisplayReading: TMenuItem;
    miDisplayMeaning: TMenuItem;
    miFontSize: TMenuItem;
    miFontSizeSmall: TMenuItem;
    miFontSizeMedium: TMenuItem;
    miFontSizeLarge: TMenuItem;
    N20: TMenuItem;
    miTranslation: TMenuItem;
    miClearTranslation: TMenuItem;
    miSetTranslation: TMenuItem;
    miAutoFillTranslation: TMenuItem;
    N21: TMenuItem;
    miPrint: TMenuItem;
    miVocabulary1: TMenuItem;
    miAddWord: TMenuItem;
    miListSettings: TMenuItem;
    miWordDetails: TMenuItem;
    N7: TMenuItem;
    miPrintList: TMenuItem;
    miListGenerator: TMenuItem;
    miHelp: TMenuItem;
    miHelpContents: TMenuItem;
    N11: TMenuItem;
    miAbout: TMenuItem;
    MainPanel: TPanel;
    BottomPanel: TPanel;
    aModeKanji: TAction;
    aModeDict: TAction;
    aModeEditor: TAction;
    aModeVocab: TAction;
    miTools: TMenuItem;
    miCharacterList: TMenuItem;
    miDictionary: TMenuItem;
    miTextEditor: TMenuItem;
    miVocabulary: TMenuItem;
    miCharacterList2: TMenuItem;
    miDictionary2: TMenuItem;
    N4: TMenuItem;
    miTextEditor2: TMenuItem;
    miVocabulary2: TMenuItem;
    N9: TMenuItem;
    Bevel1: TBevel;
    ScreenTimer: TTimer;
    RightPanel: TPanel;
    btnScreenModeSc: TSpeedButton;
    btnScreenModeWk: TSpeedButton;
    miEditorExportAs: TMenuItem;
    miSearchInflectedWords: TMenuItem;
    miAutoSearchWhileTyping: TMenuItem;
    miDictionaryGroup: TMenuItem;
    N10: TMenuItem;
    miDictionaryGroup1: TMenuItem;
    miDictionaryGroup2: TMenuItem;
    miDictionaryGroup3: TMenuItem;
    aVocabExamples: TCheckAction;
    miExamples2: TMenuItem;
    miUseColors: TMenuItem;
    miSearchSubstring: TMenuItem;
    N23: TMenuItem;
    N25: TMenuItem;
    N2: TMenuItem;
    miSaveCharactersToFile: TMenuItem;
    miAddWordIntoVocabulary: TMenuItem;
    miChangeLanguage: TMenuItem;
    aChangeLanguage: TAction;
    FormPlacement1: TFormPlacement;
    aFullscreenMode: TAction;
    FullscreenMode1: TMenuItem;
    N26: TMenuItem;
    aCategoryManager: TAction;
    miManageCategories: TMenuItem;
    N24: TMenuItem;
    aPortraitMode: TAction;
    PortraitMode1: TMenuItem;
    aVocabExport: TAction;
    aVocabImport: TAction;
    N14: TMenuItem;
    N00934eExport1: TMenuItem;
    aVocabImport1: TMenuItem;
    ClipboardPaintbox: TWakanPaintbox;
    ApplicationEvents1: TApplicationEvents;
    N01132Autoall1: TMenuItem;
    aDownloader: TAction;
    Download1: TMenuItem;
    N27: TMenuItem;
    N01225Search1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnScreenModeWkClick(Sender: TObject);
    procedure btnJapaneseModeClick(Sender: TObject);
    procedure btnChineseModeClick(Sender: TObject);
    procedure btnClipboardClearClick(Sender: TObject);
    procedure aSaveUserExecute(Sender: TObject);
    procedure aCancelUserExecute(Sender: TObject);
    procedure aStatisticsExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aKanjiDetailsExecute(Sender: TObject);
    procedure aKanjiCompoundsExecute(Sender: TObject);
    procedure aDictKanjiExecute(Sender: TObject);
    procedure aDictExamplesExecute(Sender: TObject);
    procedure aVocabAddExecute(Sender: TObject);
    procedure aVocabSettingsExecute(Sender: TObject);
    procedure aVocabDetailsExecute(Sender: TObject);
    procedure aVocabPrintExecute(Sender: TObject);
    procedure aVocabGenerateExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure aSettingsDictExecute(Sender: TObject);
    procedure aHelpExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure aJapaneseExecute(Sender: TObject);
    procedure aChineseExecute(Sender: TObject);
    procedure aKanjiAddClipboardExecute(Sender: TObject);
    procedure aKanjiSetLearnedExecute(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure aModeKanjiExecute(Sender: TObject);
    procedure aModeDictExecute(Sender: TObject);
    procedure aModeEditorExecute(Sender: TObject);
    procedure aModeVocabExecute(Sender: TObject);
    procedure btnScreenModeScClick(Sender: TObject);
    procedure ScreenTimerTimer(Sender: TObject);
    procedure aVocabExamplesExecute(Sender: TObject);
    procedure aChangeLanguageExecute(Sender: TObject);
    procedure aFullscreenModeExecute(Sender: TObject);
    procedure aCategoryManagerExecute(Sender: TObject);
    procedure aPortraitModeExecute(Sender: TObject);
    procedure aVocabExportExecute(Sender: TObject);
    procedure aVocabImportExecute(Sender: TObject);
    procedure ClipboardPaintboxPaint(Sender: TObject; Canvas: TCanvas);
    procedure ClipboardPaintboxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ClipboardPaintboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aKanjiCompoundsChecked(Sender: TObject);
    procedure aDictKanjiChecked(Sender: TObject);
    procedure aDictExamplesChecked(Sender: TObject);
    procedure aVocabSettingsChecked(Sender: TObject);
    procedure aVocabDetailsChecked(Sender: TObject);
    procedure aVocabExamplesChecked(Sender: TObject);
    procedure aKanjiDetailsChecked(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure FormShow(Sender: TObject);
    procedure aDownloaderExecute(Sender: TObject);
    procedure EditorActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure WordLookupActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure KanjiListActionExecute(Action: TBasicAction; var Handled: Boolean);

  private
    initdone:boolean;
    procedure ApplyUI;
  public
    procedure InitializeWakan;
    function SaveEverything: boolean;

 {
  Wakan has four types of dockable forms:
  1. Simple forms: docked once and remain where they are.
  2. Pages.
  3. Redockable forms: on some pages shown, on others hidden.
  4. KanjiDetails form.

  Forms #2-4 and type #1 forms which depend on other flags such as PortraitMode
  are called 'dynamically docked'.

  Wakan has 4 main pages and 6+ display modes (pages + some variations). On page
  change ApplyDisplayMode is called, and it completely redocks all dynamic forms
  as needed (trying to keep what's not changed). It takes into account all
  secondary settings such as PortraitMode.
  In other words, ApplyDisplayMode completely applies docking state.

  Each static form has an associated action, usually TCheckAction descendant:
    OnExecute: switch to containing page, flip Checked
    OnChecked: update all controls, dock/undock+hide the form
  Static form actions are Checked/Unchecked at load according to a last saved
  state, and left alone.

  Each redockable form has as many actions as there are docking places for it:
    actShowHintsOnPage1: Checked==true
    actShowHintsOnPage2: Checked==false
  ApplyDisplayMode uses this information to dock or hide the form accordingly
  when switching pages. It does so by first showing the new page:
    ShowPage(ActivePage)
  And then calling the ***Redock function for each dynamic form:
    HintsRedock;
    ExamplesRedock;
  The task of such a function is to check for active page and:
    1. Undock+hide if it shouldn't be shown where it is now. (Maybe it's already where it should be).
    2. Dock+show if it should be shown on the active page, taking into account
     any secondary flags.
  ***Redock is suitable for any adjustment to the form docking.

  ...This is theory, but in fact ApplyDisplayMode is not so nice yet and there
  are no ***Redock functions for most of dynamically docked forms.
  But this is how it should be.

  Each action handles only one dock position. It switches the flag for that
  position and then calls ***Redock to possibly adjust docking (if needed).
  Stuff that must be updated according to form's absolute visibility should be
  updated in Form's OnShow/OnHide.

  KanjiDetails is special: it has two dock positions, but it can also be
  undocked and then only has one state (shown everywhere/hidden everywhere).

  PortraitMode and other such flags work in a similar way: they do any permanent
  changes (reconfigure the form), and then adjust docking as ApplyDisplayMode
  would have done.
  If they're not smart, they may just call ApplyDisplayMode.

  When loading, all functions are instructed to skip any dynamic docking. After
  the form is fully configured to the saved state, ApplyDisplayMode is called once.
  See Issue 187.

  On technical details of docking and preserving width/height, see JwbForms.
 }

  private
   { Kanji details can work in docked or free-floating mode.
    If CharDetDocked is set, KanjiDetails is docked and will be shown/hidden by
    ApplyDisplayMode, like other panels. }
    FCharDetDocked: boolean;
    procedure MainDock(form:TForm;panel:TPanel);
    procedure MainUndock(form:TForm);
  protected
    FCurDisplayMode:integer; //currently applied mode
    FSetDisplayMode: integer; //will be applied on ApplyDisplayMode
    procedure ToggleForm(form:TForm;state:boolean);
    procedure ToggleExamples();
    procedure SetDisplayMode(const AMode: integer);
    procedure ApplyDisplayMode;
  public
   { In docked mode, we remember whether to show KanjiDetails on page 3 and on
    page 4 separately. }
    CharDetDockedVis1,
    CharDetDockedVis2:boolean;
    KanjiCompoundsDockedHeight: integer; //can be lost when it's undocked
    function DockExpress(form:TForm;dock:boolean):boolean;
    procedure SetCharDetDocked(Value: boolean; Loading: boolean);
    procedure SetPortraitMode(Value: boolean; Loading: boolean);
    procedure UpdateWindowTitle;
    property DisplayMode: integer read FCurDisplayMode write SetDisplayMode;
    property CharDetDocked: boolean read FCharDetDocked;

  protected
    procedure ClipboardChanged(Sender: TObject);
  public
    procedure SwitchLanguage(lanchar:char);
    procedure ScreenTipButtonClick(ASender: TObject; AButtonId: integer);

  protected
    UserDataLoaded:boolean;
    FUserDataChanged:boolean;
    LastSaveTime:TDatetime; //for UserData
    procedure SetUserDataChanged(Value: boolean);
    function FlushUserData:boolean;
    procedure AutosaveUserData; //check if it's time for autosave
  public
    procedure SaveUserData;
    procedure LoadUserData;
    procedure ChangeUserData;
    procedure ExportUserData(filename:string);
    procedure ImportUserData(filename:string);
    property UserDataChanged: boolean read FUserDataChanged write SetUserDataChanged;

  end;


var
  fMenu: TfMenu;

implementation
uses Types, MemSource, TextTable, JWBStrings, AppData, JWBCore, JWBClipboard, JWBUnit, StrokeOrder,
 JWBForms, JWBSplash, JWBIO, JWBDictionaries, JWBDic, JWBDicSearch, JWBLanguage, JWBCharData,
 JWBCharDataImport, JWBUserData, JWBSettings, JWBFontSettings, JWBRadicalList, JWBWordLookup,
 JWBKanjiCompounds, JWBExamples, JWBEditor, JWBVocab, JWBVocabDetails,
 JWBVocabFilters, JWBStatistics, JWBKanjiList, JWBKanjiDetails, JWBWordKanji,
 JWBDictMan, JWBDictImport, JWBScreenTip, JWBCategories, JWBAnnotations,
 JWBCommandLine, JWBAutoImport, JWBComponents, JWBDownloader, JWBCategoryMgr,
 JWBIntTip, UpgradeFiles, WakanCfg;

{$R *.DFM}


{ TfMenu }

procedure TfMenu.FormCreate(Sender: TObject);
begin
  initdone:=false;

  curlang:='j';

 //Nothing is docked to these so initialized them to hidden
  BottomPanel.Width := 0;
  BottomPanel.Height := 0;
  RightPanel.Width := 0;
  RightPanel.Height := 0;

  Clipboard.Watchers.Add(Self.ClipboardChanged);
  ScreenTip.OnTipButtonClick := Self.ScreenTipButtonClick;
end;

procedure TfMenu.FormDestroy(Sender: TObject);
begin
  Clipboard.Watchers.Remove(Self.ClipboardChanged);

  FreeCharData;
  FreeKnownLists;
end;


procedure MakeDic;
var fDictImport: TfDictImport;
begin
  fDictImport := TfDictImport.Create(Application);
  try
    fDictImport.ImportDictionary(
      MakeDicParams.Name,
      DecodeInfoField(MakeDicParams.Description),
      MakeDicParams.Files,
      MakeDicParams.Language,
      {Silent=}true
    );
  finally
    FreeAndNil(fDictImport);
  end;
end;

procedure TfMenu.InitializeWakan;
var fSplash: TfSplash;
  fCharDataImport: TfCharDataImport;
begin
  if initdone then exit;

  Self.Enabled := false; //or MainForm will receive shortcuts and crash

  try
    ParseCommandLine();
    if (JWBCommandLine.CustomCommand <> nil)
    and (JWBCommandLine.CustomCommand.ExecutionTime = etBeforeInit) then begin
      System.ExitCode := CustomCommand.Run();
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

    InitLanguage;

    InitLocalData;
    if Command='upgradelocaldata' then begin
      UpgradeLocalData();
      Application.ShowMainForm := false;
      Application.Terminate();
      exit;
    end;

    if (JWBCommandLine.CustomCommand <> nil)
    and (JWBCommandLine.CustomCommand.ExecutionTime = etAfterAppData) then begin
      System.ExitCode := CustomCommand.Run();
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

    fSettings.LoadSettings;

    if fSettings.cbShowSplashscreen.Checked
    and not IsElevatedWorker
    and (JWBCommandLine.CustomCommand = nil) then begin
      fSplash := TfSplash.Create(Application);
      fSplash.Show;
      fSplash.Update;
    end;

   //Configuration file
    if not FileExists(AppFolder+'\wakan.cfg') then
      raise Exception.Create(_l('#00347^eFile WAKAN.CFG is missing.'#13
          +'This file contains important configuration parameters and is required'
          +'for application to run.'#13#13'Application will now be terminated.'));
    try
      LoadWakanCfg(AppFolder+'\wakan.cfg');
      fLanguage.LocalizePropertyTypes();
    except
      raise Exception.Create(_l('#00352^eCannot load main configuration file.'#13
          +'File WAKAN.CFG is corrupted.'#13#13'Application will now exit.'));
    end;

   { At this point we have loaded basic settings and functionality.
    Package enhancements are going to be loaded now. }
    AppComponents.LoadFromFile(AppFolder+'\Components.ini');

   { Import now before these packages are loaded }
    if Command='makeexamples'then
    begin
      fExamples.BuildExamplesPackage;
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end else
    if Command='makedic'then
    begin
      MakeDic();
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

    AutoImportDicts();

   //Force user to select fonts
    FontSettingsPage.CheckFontsPresent;

   { Wakan.chr }

    if (Command='makechars') and MakeCharsParams.ResetDb then begin
     { Do not load wakan.chr, we don't need it }
      TChar := nil;
      TCharProp := nil;
      TRadicals := nil;
    end else
    if FileExists(AppFolder+'\wakan.chr') then begin
      try
        LoadCharData(AppFolder+'\wakan.chr');
      except
        on E: ECharDataException do
          raise;
        on E: Exception do begin
          E.Message := E.Message + #13 +
            _l('#00356^eCannot load main dictionary file.'#13
              +'File WAKAN.CHR is corrupted.'#13#13'Application will now exit.');
          raise;
        end;
      end;
    end else
    if Command='makechars' then begin
     { Let the import routine handle what exists and whatnot }
    end else
    if FileExists(AppFolder+'\KANJIDIC') and DirectoryExists(AppFolder+'\Unihan') and FileExists(AppFolder+'\radicals.txt') then begin
     { We can try autoimport }
    end else
      raise Exception.Create(_l('#00346^eFile WAKAN.CHR was not found.'#13
          +'This file is required for application to run.'#13
          +'Please download this file from WAKAN website.'#13#13
          +'Application will now be terminated.'));

   //Console/auto-import
    if (Command='makechars') or (TChar=nil) then begin
      if (TChar=nil) or MakeCharsParams.ResetDb then begin
        FreeAndNil(TChar);
        FreeAndNil(TCharProp);
        FreeAndNil(TRadicals);
        ClearCharDbProps();
      end;
      fCharDataImport := TfCharDataImport.Create(Self);
      try
        if Command='makechars' then begin
          fCharDataImport.edtKanjidicFilename.Text := MakeCharsParams.KanjidicFilename;
          fCharDataImport.edtUnihanFolder.Text := MakeCharsParams.UnihanFolder;
        end else begin
          fCharDataImport.edtKanjidicFilename.Text := AppFolder+'\KANJIDIC';
          fCharDataImport.edtUnihanFolder.Text := AppFolder+'\Unihan';
        end;
        fCharDataImport.Import;
      finally
        FreeAndNil(fCharDataImport);
      end;
      if Command='makechars' then begin //if that was autoimport, continue
        Application.ShowMainForm := false;
        Application.Terminate;
      end;
    end;


    if (JWBCommandLine.CustomCommand <> nil)
    and (JWBCommandLine.CustomCommand.ExecutionTime = etAfterCharData) then begin
      System.ExitCode := CustomCommand.Run();
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

   { Annotations }
    JWBAnnotations.Initialize;

   { Radical search }

   //Raine radical rebuilding -- before complaining about missing rad
    if Command='makerad' then
    begin
     //If no filename is set, assume defaults
      if Length(MakeRadParams.Files)<=0 then begin
        if FileExists(AppFolder+'\RADKFILE') then AddFilename(MakeRadParams.Files, AppFolder+'\RADKFILE');
       // if FileExists('RADKFILE2') then AddFilename(MakeRadParams.Files, 'RADKFILE2'); //not ready for this, it has chars in EUC we can't handle
        if Length(MakeRadParams.Files)<=0 then
          raise Exception.Create(_l('No RADKFILE is found in the application directory. '
            +'Either put this file there or explicitly specify which files to use to MAKERAD.'));
      end;

      BuildRadicalPackage(MakeRadParams.Files);
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

   //Auto-rebuild
    if not FileExists(AppFolder+'\wakan.rad') then begin
      SetLength(MakeRadParams.Files,0);
      if FileExists(AppFolder+'\RADKFILE') then AddFilename(MakeRadParams.Files, AppFolder+'\RADKFILE');
     // if FileExists(AppFolder+'\RADKFILE2') then AddFilename(MakeRadParams.Files, AppFolder+'\RADKFILE2'); //see above
      if Length(MakeRadParams.Files)>0 then //else just continue and fail later
        BuildRadicalPackage(MakeRadParams.Files);
    end;

   //Radical search
    if not FileExists(AppFolder+'\wakan.rad') then
    begin
      Application.MessageBox(
        pchar(_l('#00357^eFile WAKAN.RAD was not found.'#13
          +'Japanese advanced radicals search will be disabled.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      RaineRadicals:=nil;
    end else
      try
        LoadRaineRadicals(AppFolder+'\WAKAN.RAD');
      except
        on E: Exception do begin
          E.Message := _l('#00358^eCannot load Japanese radicals file.'#13
            +'File WAKAN.RAD is corrupted.'#13#13'Application will now exit.')+#13
            +E.Message;
          raise;
        end;
      end;


   { Stroke order display }

   //Stroke-order rebuilding -- before complaining about missing sod
    if Command='makesod' then
    begin
      StrokeOrder.BuildStrokeOrderPackage(AppFolder+'\STROKES.CSV', AppFolder+'\wakan.sod');
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

   //Auto-rebuild
    if not FileExists(AppFolder+'\wakan.sod')
    and FileExists(AppFolder+'\STROKES.CSV') then
      StrokeOrder.BuildStrokeOrderPackage(AppFolder+'\STROKES.CSV', AppFolder+'\wakan.sod');

   //Stroke-order display
    if not FileExists(AppFolder+'\wakan.sod') then
      Application.MessageBox(
        pchar(_l('#00359^eFile WAKAN.SOD was not found.'#13
          +'Japanese stroke-order display will be disabled.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR)
    else
      try
        StrokeOrder.LoadStrokeOrder(AppFolder+'\wakan.sod');
      except
        on E: Exception do begin
          E.Message := _l('#00360^eCannot load Japanese stroke-order file.'#13
            +'File WAKAN.SOD is corrupted.'#13#13'Application will now exit.')+#13
            +E.Message;
          raise;
        end;
      end;


   { User data }

    try
      userdataloaded:=false;
      LoadUserData;
      LastSaveTime := now;
    except
      if FileExists(UserDataDir+'\WAKAN.USR') then
        raise Exception.Create(_l('#00361^eCannot load user data file.'#13'File WAKAN.USR is corrupted.'#13
          +'If you delete this file, it will be created anew.'#13#13'Application will now exit.'))
      else
        raise Exception.Create(_l('#00362^eUnable to create user data file WAKAN.USR.'#13
          +'Please run this program from a folder that is not read-only.'#13#13
          +'Application will now exit.'));
    end;
    if Application.Terminated then exit;

    if (JWBCommandLine.CustomCommand <> nil)
    and (JWBCommandLine.CustomCommand.ExecutionTime = etAfterUserData) then begin
      System.ExitCode := CustomCommand.Run();
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;


   //Prepare for SwitchLanguage->RescanDicts->AutoUpdate(dic)
    if Command='updatedics' then begin
     //Fix filenames now, or they won't match and force updates in AutoUpdate
      AutoFixFilenames(UpdateDicsParams.Files);
      JWBAutoImport.ForceUpdates := true;
      JWBAutoImport.ForceUpdateList := UpdateDicsParams.Files;
    end;

   //Attach forms
    if fEditor<>nil then
      fEditor.Actions.OnExecute := Self.EditorActionExecute;
    if fWordLookup<>nil then
      fWordLookup.Actions.OnExecute := Self.WordLookupActionExecute;
    if fKanji<>nil then
      fKanji.Actions.OnExecute := Self.KanjiListActionExecute;

    SwitchLanguage(curlang);
    { SwitchLanguage will do this:
    RescanDicts;
    ReloadExamples;
    RefreshCategory;
    RefreshKanjiCategory; }
    dicts.AutoUpgradeListed();

    if Command='updatedics' then begin
      if Length(UpdateDicsParams.Files)>0 then
        JWBAutoImport.AutoUpdateFiles(UpdateDicsParams.Files);
      Application.ShowMainForm := false;
      Application.Terminate;
      exit; //so this can be run in batch mode
    end;

    FreeAndNil(fSplash);

    if IsElevatedWorker then begin
      Application.ShowMainForm := false;
      Application.Terminate;
      exit;
    end;

    Self.ApplyUI;

    initdone:=true;
  except
    on E: EBadUsage do begin
      ShowUsage(E.Message);
      Application.ShowMainForm := false;
      Application.Terminate;
      System.ExitCode := 2;
    end;
    on E: EAbort do begin
      Application.ShowMainForm := false;
      Application.Terminate; //Silently
    end;
    on E: Exception do begin
      Application.MessageBox(
        pchar(E.Message+' ('+E.Classname+')'),
        pchar('Cannot load Wakan.'), //Do not translate! The translation might not even be loaded yet.
        MB_ICONERROR or MB_OK
      );
     //It's better to exit right now than to continue uninitialized.
      Application.ShowMainForm := false;
      Application.Terminate;
      System.ExitCode := 1;
    end;
  end;

  Self.Enabled := true;
  ScreenTimer.Enabled:=true;

 //The neccessary initialization is finished, failures in the rest should not terminate the app
  try

   { Open file in the editor. }
    if fEditor<>nil then begin
     //Explicitly specified file
      if Command='open' then begin
        fEditor.OpenAnyFile(OpenParams.Filename);
       //Press "Editor" programmatically
        tab3.Down := true;
        TabControl1Change(tab3);
      end else

     //Last opened file in Editor
      if fSettings.cbEditorAutoloadLast.Checked then
        fEditor.AutoloadLastFile;
    end;

  except
    on E: EAbort do begin end;
    on E: Exception do begin
      Application.MessageBox(
        pchar(E.Message+' ('+E.Classname+')'),
        pchar('Error'),
        MB_ICONERROR or MB_OK
      );
    end;
  end;

 { Done. }
end;

procedure TfMenu.FormShow(Sender: TObject);
begin
  UpdateWindowTitle;
end;

procedure TfMenu.UpdateWindowTitle;
var subttl: string;
begin
  case DisplayMode of
    1: subttl := fKanji.Caption;
    2: subttl := fWordLookup.Caption;
    3: subttl := fEditor.Caption;
    4: subttl := fEditor.Caption;
    5: subttl := fVocab.Caption;
    6: subttl := fKanjiCompounds.Caption;
  else
    subttl := _l('^eTool for learning Japanese & Chinese');
  end;
  subttl := 'Wakan ' + WakanVer + ' - ' + subttl;
  if Self.Caption <> subttl then //at least avoid redrawing
    Self.Caption := subttl;
end;

procedure TfMenu.ApplyUI;
begin
  Self.Visible := false;
  FCurDisplayMode := 0;
 //Hide everything, and most importantly, turn all actions off
 //This will do no harm if the form is already hidden.
  Self.aKanjiDetails.Checked := false;
  fKanji.aSearch.Checked := false;
  Self.aKanjiCompounds.Checked := false;
  Self.aDictKanji.Checked := false;
  Self.aVocabExamples.Checked := false;
  Self.aDictExamples.Checked := false;
  Self.aVocabDetails.Checked := false;
  Self.aVocabSettings.Checked := false;

  FSetDisplayMode:=fSettings.setlayout;

  if fKanjiCompounds<>nil then
   //Set to value loaded with settings / design height
    KanjiCompoundsDockedHeight := fKanjiCompounds.Height;

 { Issue 187:	Focus lost in the editor when starting the program.
  SetPortraitMode may redock forms appropriately, and excessive redocking leads
  to losing input focus.
  Explicitly ask for NO REDOCKING with Loading:=true. Docking state will be
  applied directly with ApplyDisplayMode later. }
  fMenu.aPortraitMode.Checked := fSettings.setPortraitMode;
  SetPortraitMode(fSettings.setPortraitMode, {Loading=}true);

 { Again, pass Loading:=true to ask for no ApplyDisplayMode, no docking updates }
  fMenu.SetCharDetDocked(fSettings.CharDetDocked, true); //after KanjiDetails.DockedWidth/Height
  fMenu.CharDetDockedVis1:=fSettings.CharDetDockedVis1;
  fMenu.CharDetDockedVis2:=fSettings.CharDetDockedVis2;

 //Before fKanji->OnShow => first possible Compounds reload
  if fKanjiCompounds<>nil then begin
    if fSettings.setusercompounds then fKanjiCompounds.sbShowVocab.Down:=true else fKanjiCompounds.sbShowDict.Down:=true;
    if Assigned(fKanjiCompounds.sbShowVocab.OnClick) then
      fKanjiCompounds.sbShowVocab.OnClick(fKanjiCompounds.sbShowVocab);
  end;

  fMenu.ApplyDisplayMode;
  if fSettings.setwindows and 1=1 then fKanji.aSearch.Checked := true;
  if fSettings.setwindows and 2=2 then fMenu.aKanjiCompounds.Checked := true;
  if fSettings.setwindows and 4=4 then fMenu.aDictKanji.Checked := true;
  if fSettings.setwindows and 8=8 then fMenu.aDictExamples.Checked := true;
  if fSettings.setwindows and 16=16 then fMenu.aVocabExamples.Checked := true;
  if fSettings.setwindows and 32=32 then fMenu.aVocabDetails.Checked := true;
  if fSettings.setwindows and 64=64 then fMenu.aVocabSettings.Checked := true;
  if (fSettings.setwindows and 128=128) and (not fMenu.CharDetDocked) then fMenu.aKanjiDetails.Checked := true;

  if fWordLookup<>nil then
    fWordLookup.RestoreLookupMode;

  FormPlacement1.RestoreFormPlacement([roActivate, roJustWrite]); //activate main form, we're starting
end;


procedure TfMenu.SwitchLanguage(lanchar:char);
var mb_res: integer;
begin
  if fEditor<>nil then
    fEditor.LanguageChanging;

  curlang:=lanchar;
  if lanchar='j'then
  begin
    showroma:=fSettings.rgShowKana.ItemIndex=1;
    btnJapaneseMode.Down:=true;
    aJapanese.Checked:=true;
    aChinese.Checked:=false;
  end else
  begin
    showroma:=fSettings.rgShowBopomofo.ItemIndex=1;
    btnChineseMode.Down:=true;
    aJapanese.Checked:=false;
    aChinese.Checked:=true;
  end;

  dicts.Rescan(false);
  if dicts.Count<=0 then
    dicts.Rescan(true); //try even the disabled ones
  if dicts.Count<=0 then begin
    if curlang='j'then
      mb_res := Application.MessageBox(PChar(_l('#01143^No valid japanese dictionary was found.'#13
        +'Do you want to open the downloader and choose some japanese dictionaries to download?')),
        PChar(_l('#00090^eWarning')),
        MB_ICONWARNING or MB_YESNO)
    else
      mb_res := Application.MessageBox(PChar(_l('#01144^No valid chinese dictionary was found.'#13
        +'Do you want to open the downloader and choose some chinese dictionaries to download?')),
        PChar(_l('#00090^eWarning')),
        MB_ICONWARNING or MB_YESNO);
    if mb_res=ID_YES then
      OpenDownloader(Self, 'dic', curlang);
     //we don't care if they proceeded or not because we can't do much anyway
  end;

  if fKanji<>nil then
    fKanji.LanguageChanged;
  if fWordLookup<>nil then
    fWordLookup.LanguageChanged;
  if fEditor<>nil then
    fEditor.LanguageChanged;
  if fExamples<>nil then
    fExamples.ReloadExamplesPackage;
  if fWordLookup<>nil then
    fWordLookup.Look();
  JWBCategories.CategoryListChanged;
  if fKanjiDetails<>nil then
    fKanjiDetails.RefreshDetails;
end;

procedure TfMenu.SetUserDataChanged(Value: boolean);
begin
  FUserDataChanged := Value;
  aSaveUser.Enabled:=FUserDataChanged;
  aCancelUser.Enabled:=FUserDataChanged;
end;

procedure TfMenu.ChangeUserData;
begin
  UserDataChanged:=true;
end;

procedure TfMenu.LoadUserData;
var ps:TPackageSource;
begin
  UserDataChanged:=false;
  aSaveUser.Enabled:=false;
  aCancelUser.Enabled:=false;
  Screen.Cursor:=crHourGlass;
  try
    if userdataloaded then begin
      FreeCategories;
      FreeUserPackage;
      userdataloaded:=false;
    end;

    if not FileExists(UserDataDir+'\wakan.usr') then
      InitializeUserPackage(UserDataDir+'\wakan.usr'); //create empty WAKAN.USR

    ps := LoadUserPackage(UserDataDir+'\wakan.usr');
    LoadCategories(ps); //build category list, find KnownLearned etc

    if not SkipAutoRepair then

      if FixDuplicateCategories() then begin //have to do this after we (perhaps) found the KnownLearned
        UserDataChanged := true;
        Application.MessageBox(
          PChar(_l('^eYour user data contained duplicate kanji/word groups. This is a result '
          +'of a bug in older versions of Wakan.'#13
          +'The data has been repaired and duplicates have been merged. Please check '
          +'your user groups, save user data, and you should never see this message again.')),
          PChar(_l('User data repaired')),
          MB_ICONINFORMATION or MB_OK
        );
      end;

   //Add "LEARNED" category, if missing
    if KnownLearned<0 then begin
      AddKnownLearnedCategory(ps);
      UserDataChanged := true;
    end;

    ps.Free;

    if not SkipAutoRepair then

      if UserDataAutoRepair() then //health test
        UserDataChanged:=true; //or we'd be reindexing each load

    UserDataLoaded:=true;
  finally
    Screen.Cursor:=crDefault;
  end;

 //Refresh everything
  JWBCategories.CategoryListChanged;
  if fKanjiDetails<>nil then
    fKanjiDetails.RefreshDetails;
end;

procedure TfMenu.SaveUserData;
var tempDir: string;
begin
  if not UserDataChanged then exit;
  Screen.Cursor:=crHourGlass;
  try
    CopyFile(PChar(UserDataDir+'\wakan.usr'),PChar(UserDataDir+'\wakan.bak'),false);
    ReloadKanjiCategories(); //in case they weren't loaded which shouldn't happen

    tempDir := CreateRandomTempDir();
    SaveCategories(tempDir);
    SaveUserPackage(tempDir,UserDataDir+'\wakan.usr');
    DeleteDirectory(tempDir);
    Backup(UserDataDir+'\wakan.usr');

    UserDataChanged:=false;
    LastSaveTime:=now;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfMenu.ExportUserData(filename:string);
var t:TStreamEncoder;
  i:integer;
  w:widechar;
begin
  if not FlushUserData then exit;
  //User data is stored in Ansi, because compability.
  t := AnsiFileWriter(filename);
  t.Writeln('$User');
  TUser.ExportToText(t,'Index_Ind');
  t.Writeln('$UserIdx');
  TUserIdx.ExportToText(t,'Kanji_Ind');
  t.Writeln('$UserCat');
  TUserCat.ExportToText(t,'Index_Ind');
  t.Writeln('$UserSheet');
  TUserSheet.ExportToText(t,'Sheet_Ind');
  t.Writeln('$KnownKanji');
  t.Writeln('>Unicode');
  for i:=1 to 65536 do
  begin
    w:=widechar(i);
    if IsKnown(KnownLearned,{$IFDEF UNICODE}w{$ELSE}UnicodeToHex(w){$ENDIF}) then
      t.Writeln('+'+UnicodeToHex(w));
  end;
  t.Writeln('.');
  t.Free;
end;

procedure TfMenu.ImportUserData(filename:string);
var t:TStreamDecoder;
  s:string;
begin
  DeleteFile(UserDataDir+'\wakan.usr');
  LoadUserData;
  Screen.Cursor:=crHourGlass;
 //User data is stored in Ansi, because compability.
  t := AnsiFileReader(filename);
  while not t.Eof() do
  begin
    s := t.ReadLn();
    if s[1]='$'then
    begin
      if s='$User'then TUser.ImportFromText(t);
      if s='$UserIdx'then TUserIdx.ImportFromText(t);
      if s='$UserCat'then TUserCat.ImportFromText(t);
      if s='$UserSheet'then TUserSheet.ImportFromText(t);
      if s='$KnownKanji'then
      begin
        t.ReadLn();
        s := t.ReadLn();
        while s[1]<>'.'do
        begin
          delete(s,1,1);
          SetKnown(KnownLearned,s,true);
          s := t.ReadLn();
        end;
      end;
    end;
  end;
  t.Free;
  ChangeUserData;
  SaveUserData;
  JWBCategories.CategoryListChanged;
  Screen.Cursor:=crDefault;
end;

function TfMenu.FlushUserData:boolean;
var res:integer;
begin
  result:=true;
  if not UserDataChanged then
    exit;

  if fSettings.CheckBox46.Checked then
    SaveUserData
  else
  begin
    res:=Application.MessageBox(
      pchar(_l('#00340^eUser data was changed. Do you want to save it?')),
      pchar(_l('#00341^eApplication exit')),
      MB_YESNOCANCEL or MB_ICONQUESTION);
    case res of
      idYes:SaveUserData;
      idNo:UserDataChanged:=false; //do not save
      idCancel:Result:=false;
    end;
  end;
end;

procedure TfMenu.AutosaveUserData; //check if it's time for autosave
var curt: TDatetime;
  AutoSavePeriod:integer;
begin
  if not fSettings.cbAutosave.Checked then exit;
  curt:=now-LastSaveTime;
  if not TryStrToInt(fSettings.edtAutoSavePeriod.Text, AutoSavePeriod) then
    AutoSavePeriod := DefaultAutoSavePeriod;
  if curt>1/24/60*AutoSavePeriod then
    SaveUserData; //updates LastSaveTime
end;


procedure TfMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not SaveEverything then
    Action := caNone;
  if Action<>caNone then begin
    ScreenTip.EnabledSystemWide := false;
    if fEditor <> nil then
      fEditor.Close;
    if fWordLookup <> nil then
      fWordLookup.Close;
    if fVocab <> nil then
      fVocab.Close;
    if fKanji <> nil then
      fKanji.Close;
  end;
end;

{ Called when the app is closing but everything is yet in place.
Saves all settings and data and may let the user one last chance to cancel
the close process.
Call before proceeding with any work that will require terminating the app
in the end. }
function TfMenu.SaveEverything: boolean;
begin
  Result := FlushUserData;
  if fEditor<>nil then
    Result := Result and fEditor.CommitFile;
  if not Result then exit;
  if FormPlacement1.PlacementRestored then
    FormPlacement1.SaveFormPlacement;
  fSettings.SaveSettings;
end;

procedure TfMenu.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  if E is EAbort then exit;
  E.Message := _l(E.Message);
  Application.ShowException(E);
end;


procedure TfMenu.btnJapaneseModeClick(Sender: TObject);
begin
  SwitchLanguage('j');
end;

procedure TfMenu.btnChineseModeClick(Sender: TObject);
begin
  SwitchLanguage('c');
end;

procedure TfMenu.aJapaneseExecute(Sender: TObject);
begin
  btnJapaneseModeClick(Sender);
end;

procedure TfMenu.aChineseExecute(Sender: TObject);
begin
  btnChineseModeClick(Sender);
end;


procedure TfMenu.ClipboardChanged(Sender: TObject);
begin
  ClipboardPaintbox.Invalidate;
end;

procedure TfMenu.ClipboardPaintboxPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  Canvas.Font.Color:=clWindowText;
  DrawUnicodeText(Canvas,2,2,22,copy(Clipboard.Text,1,200),FontRadical);
end;

procedure TfMenu.ClipboardPaintboxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  IntTip.MouseMove(ClipboardPaintbox,x,y,ssLeft in Shift);
end;

procedure TfMenu.ClipboardPaintboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then IntTip.MouseUp;
end;

procedure TfMenu.btnClipboardClearClick(Sender: TObject);
begin
  Clipboard.Clear;
end;


procedure TfMenu.aSaveUserExecute(Sender: TObject);
begin
  SaveUserData;
end;

procedure TfMenu.aCancelUserExecute(Sender: TObject);
begin
  LoadUserData;
end;

procedure TfMenu.aStatisticsExecute(Sender: TObject);
var fStatistics: TfStatistics;
begin
  fStatistics := TfStatistics.Create(Self);
  try
    fStatistics.ShowModal;
  finally
    FreeAndNil(fStatistics);
  end;
end;


procedure TfMenu.aSettingsExecute(Sender: TObject);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsGeneral;
  fSettings.ShowModal;
  if fKanji.Visible then fKanji.Reload();
  if fWordLookup.Visible then fWordLookup.Look();
  if fVocab.Visible then fVocab.ShowIt(false);
  if fEditor.Visible then fEditor.RepaintText;
end;

procedure TfMenu.aChangeLanguageExecute(Sender: TObject);
begin
  fLanguage.SelectLanguage;
end;

procedure TfMenu.aSettingsDictExecute(Sender: TObject);
var fDictMan: TfDictMan;
begin
  fDictMan := TfDictMan.Create(Self);
  try
    fDictMan.ShowModal;
  finally
    FreeAndNil(fDictMan);
  end;
end;

procedure TfMenu.aCategoryManagerExecute(Sender: TObject);
var fCategoryMgr: TfCategoryMgr;
begin
  fCategoryMgr := TfCategoryMgr.Create(Application);
  try
    fCategoryMgr.ShowModal;
  finally
    FreeAndNil(fCategoryMgr);
  end;
end;

procedure TfMenu.aDownloaderExecute(Sender: TObject);
begin
  OpenDownloader(Self);
end;

procedure TfMenu.aHelpExecute(Sender: TObject);
begin
  if FileExists('wakan_'+curGUILanguage+'.chm') then
    ShellExecute(fMenu.handle,nil,pchar('wakan_'+curGUILanguage+'.chm'),nil,nil,SW_SHOW) else
  if FileExists('wakan.chm') then
    ShellExecute(fMenu.handle,nil,'wakan.chm',nil,nil,SW_SHOW) else
  if FileExists('wakan_bld.chm') then
  begin
    Application.MessageBox(
      pchar(_l('#00363^eHelp file is under construction, the information may be inaccurate.')),
      pchar(_l('#00364^eNotice')),
      MB_ICONWARNING or MB_OK);
    ShellExecute(fMenu.handle,nil,'wakan_bld.chm',nil,nil,SW_SHOW);
  end else
  if FileExists('wakan_en.chm') then
    Application.MessageBox(
      pchar(_l('#00365^eHelp file is out of date. Please download new help file '
        +'from WaKan website: wakan.manga.cz.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK)
  else
    Application.MessageBox(
      pchar(_l('#00366^eCannot find file WAKAN.CHM.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
end;

procedure TfMenu.aAboutExecute(Sender: TObject);
var fSplash: TfSplash;
begin
  fSplash := TfSplash.Create(Self);
  try
    fSplash.BitBtn1.Show;
    fSplash.ShowModal;
  finally
    FreeAndNil(fSplash);
  end;
end;

procedure TfMenu.aExitExecute(Sender: TObject);
begin
  Close;
end;


//switch between resizable window with borders and menu
//and fullscreen borderless, menuless mode
procedure TfMenu.aFullscreenModeExecute(Sender: TObject);
{$J+} //writeable constants on
const
  rect: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  ws : TWindowState = wsNormal;
{$J-} //writeable constants off
var
  r : TRect;
begin
  if BorderStyle <> bsNone then
  begin
    ws := WindowState;
    rect := BoundsRect;
		fmenu.Menu := nil;	//hide menu
    BorderStyle := bsNone;
    r := Screen.MonitorFromWindow(Handle).BoundsRect;
    SetBounds(r.Left, r.Top, r.Right-r.Left, r.Bottom-r.Top) ;
  end
  else
  begin
  	fmenu.Menu := fmenu.MainMenu; //show menu
    BorderStyle := bsSizeable;
    if ws = wsMaximized then
      WindowState := wsMaximized
    else
      SetBounds(rect.Left, rect.Top, rect.Right-rect.Left, rect.Bottom-rect.Top) ;
  end;
end;

{ Changes the mode of KanjiDetails window: docked or free-floating }
procedure TfMenu.SetCharDetDocked(Value: boolean; Loading: boolean);
begin
  if (not Loading) and (FCharDetDocked=Value) then exit;
  FCharDetDocked := Value;
  if Value then begin
    if fKanjiDetails<>nil then
      fKanjiDetails.SetDocked(Value,Loading);
    if not Loading then begin
      CharDetDockedVis1:=true;
      CharDetDockedVis2:=true;
      ApplyDisplayMode; //docks it and shows if on appropriate page
    end;
  end else begin
    if fKanjiDetails<>nil then begin
      if not Loading then
        DockExpress(fKanjiDetails,false); //hides and undocks it
      fKanjiDetails.SetDocked(false, Loading);
      if (not Loading) and (not fKanjiDetails.Visible) then
        aKanjiDetails.Execute; //shows it as free floating
    end;
  end;
end;

{ Shows/hides KanjiDetails, whether it's in free-floating mode or not.
 In docked mode this appropriately updates CharDetDockedVis*  }
procedure TfMenu.aKanjiDetailsExecute(Sender: TObject);
begin
  if CharDetDocked then
  begin
    if DisplayMode in [2,5] then
    begin
     //Make it undocked unvisible to hide dock panel
      SetCharDetDocked(false,false);
     //This is probably wrong because SetCharDetDocked will call aKanjiDetails.Execute again
    end else
    begin
      if DisplayMode=1 then
        CharDetDockedVis1:=not CharDetDockedVis1
      else
        CharDetDockedVis2:=not CharDetDockedVis2;
      ApplyDisplayMode;
    end;
  end;
  if not CharDetDocked then
  begin
   //In free-floating mode just show or hide it
    if fKanjiDetails.Visible then
      fKanjiDetails.Visible := false
    else
     //We have to be careful if this form has just been undocked and it's window
     //is not yet properly recreated on first show.
     //This requires special treatment:
      UndockedMakeVisible(fKanjiDetails);
  end;
  fMenu.aKanjiDetails.Checked:=fKanjiDetails.Visible;
end;

procedure TfMenu.aKanjiDetailsChecked(Sender: TObject);
begin
 { Set both btn.Down without checking which form is visible:
  if the form is invisible, the button is invisible too }
  if fKanji<>nil then
    fKanji.btnKanjiDetails.Down:=fKanjiDetails.Visible;
  if fEditor<>nil then
    fEditor.btnKanjiDetails.Down:=fKanjiDetails.Visible;
end;

procedure TfMenu.aKanjiCompoundsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aKanjiCompounds.Checked;
  if not fKanji.Visible then aModeKanji.Execute;
  if aKanjiCompounds.Checked<>pre then exit;
  aKanjiCompounds.Checked := not aKanjiCompounds.Checked;
end;

procedure TfMenu.aKanjiCompoundsChecked(Sender: TObject);
begin
  if fKanji<>nil then
    fKanji.btnCompounds.Down := aKanjiCompounds.Checked;
  ApplyDisplayMode;
end;

procedure TfMenu.aDictKanjiExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictKanji.Checked;
  if not fWordLookup.Visible then aModeDict.Execute;
  if aDictKanji.Checked<>pre then exit;
  aDictKanji.Checked := not aDictKanji.Checked;
end;

procedure TfMenu.aDictKanjiChecked(Sender: TObject);
begin
  ToggleForm(fWordKanji, aDictKanji.Checked);
  fWordLookup.btnWordKanji.Down := aDictKanji.Checked;
end;

procedure TfMenu.aDictExamplesExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictExamples.Checked;
  if not fWordLookup.Visible then aModeDict.Execute;
  if aDictExamples.Checked<>pre then exit;
  aDictExamples.Checked := not aDictExamples.Checked;
end;

procedure TfMenu.aDictExamplesChecked(Sender: TObject);
begin
//  ToggleForm(fExamples, aDictExamples.Checked); //with Examples we need complex treatment
  ToggleExamples();
  fWordLookup.btnExamples.Down := aDictExamples.Checked;
end;

procedure TfMenu.aVocabAddExecute(Sender: TObject);
begin
  if fVocab.Visible then
    fVocab.btnAddWordClick(Sender)
  else
  if fWordLookup.Visible then
    fWordLookup.btnAddToVocabClick(Sender)
  else
  if fKanjiCompounds.Visible then
    fKanjiCompounds.btnAddToVocabClick(Sender);
end;

procedure TfMenu.aVocabSettingsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aVocabSettings.Checked;
  if not fVocab.Visible then aModeVocab.Execute;
  if aVocabSettings.Checked<>pre then exit;
  aVocabSettings.Checked := not aVocabSettings.Checked;
end;

procedure TfMenu.aVocabSettingsChecked(Sender: TObject);
begin
  if fVocabFilters<>nil then
    ToggleForm(fVocabFilters, aVocabSettings.Checked);
  if fVocab<>nil then
    fVocab.btnListSettings.Down := aVocabSettings.Checked;
end;

procedure TfMenu.aVocabDetailsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aVocabDetails.Checked;
  if not fVocab.Visible then aModeVocab.Execute;
  if aVocabDetails.Checked<>pre then exit;
  aVocabDetails.Checked := not aVocabDetails.Checked;
end;

procedure TfMenu.aVocabDetailsChecked(Sender: TObject);
begin
  ToggleForm(fVocabDetails, aVocabDetails.Checked);
  fVocab.btnWordDetails.Down := aVocabDetails.Checked;
end;

procedure TfMenu.aVocabPrintExecute(Sender: TObject);
begin
  fVocab.btnPrintVocabListClick(sender);
end;

procedure TfMenu.aVocabGenerateExecute(Sender: TObject);
begin
  fVocab.btnLearningListClick(sender);
end;

procedure TfMenu.aVocabExportExecute(Sender: TObject);
begin
  fVocab.ExportVocab;
end;

procedure TfMenu.aVocabImportExecute(Sender: TObject);
begin
  fVocab.ImportVocab;
end;

procedure TfMenu.aKanjiAddClipboardExecute(Sender: TObject);
begin
  if not fKanji.Visible then aModeKanji.Execute;
  fKanjiDetails.SpeedButton23Click(Sender);
end;

procedure TfMenu.aKanjiSetLearnedExecute(Sender: TObject);
begin
  if not fKanji.Visible then aModeKanji.Execute;
  fKanjiDetails.btnAddToCategoryClick(Sender);
end;

procedure TfMenu.aVocabExamplesExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aVocabExamples.Checked;
  if not fVocab.Visible then aModeVocab.Execute;
  if aVocabExamples.Checked<>pre then exit;
  aVocabExamples.Checked := not aVocabExamples.Checked;
end;

procedure TfMenu.aVocabExamplesChecked(Sender: TObject);
begin
//  ToggleForm(fExamples, aUserExamples.Checked); //with Examples we need complex treatment
  ToggleExamples();
  fVocab.btnExamples.Down := aVocabExamples.Checked;
end;

{ Panel docker.
 If given "dock", docks the form to its rightful place and shows it,
 else hides it and undocks it.
 Returns true if the form was docked before this call. }
function TfMenu.DockExpress(form:TForm;dock:boolean): boolean;
begin
  if form=fKanjiDetails then begin
    if aPortraitMode.Checked then
      Result:=DockProc(fKanjiDetails,RightPanel,alBottom,dock)
    else
      Result:=DockProc(fKanjiDetails,RightPanel,alRight,dock);
  end else
  if (form=fExamples) and (DisplayMode<>5) then
    Result:=DockProc(fExamples,fWordLookup.pnlDockExamples,alBottom,dock)
  else
  if (form=fExamples) and (DisplayMode=5) then
    Result:=DockProc(fExamples,fVocab.pnlDockExamples,alBottom,dock)
  else
  if form=fWordKanji then
    if aPortraitMode.Checked then
      Result:=DockProc(fWordKanji,fWordLookup.CharInWordDock,alBottom,dock)
    else
      Result:=DockProc(fWordKanji,fWordLookup.CharInWordDock,alRight,dock)
  else
  if form=fVocabFilters then begin
    fVocab.splDockFilters.Visible := dock;
    if aPortraitMode.Checked then begin
      Result:=DockProc(fVocabFilters,fVocab.pnlDockFilters,alBottom,dock);
      fVocab.splDockFilters.Top := fVocab.pnlDockFilters.Top - 1;
    end else begin
      Result:=DockProc(fVocabFilters,fVocab.pnlDockFilters,alRight,dock);
      fVocab.splDockFilters.Left := fVocab.pnlDockFilters.Left - 1;
    end;
  end
  else
  if form=fVocabDetails then begin
    Result:=DockProc(fVocabDetails,fVocab.pnlDockDetails,alBottom,dock);
    fVocab.splDockDetails.Visible := dock;
    fVocab.splDockDetails.Top := fVocab.pnlDockDetails.Top - 1;
  end else
    Result := false;
end;

{ Only use this to dock and undock modules of a main form.
 Modules are docked once, then sometimes hidden (form.Hide) and shown with MainDock(form,panel) again.
 Some are redocked to other places at times. }
procedure TfMenu.MainDock(form:TForm;panel:TPanel);
begin
  if form.HostDockSite<>panel then begin
    form.Width:=panel.ClientWidth;
    form.Height:=panel.ClientHeight;
    form.ManualDock(panel);
    form.Align:=alClient;
  end;
  form.Show;
end;

{ Modules are undocked at the end of the program. After undocking they remain hidden. }
procedure TfMenu.MainUndock(form:TForm);
var rect:TRect;
begin
  form.Hide;
  rect.left:=0;
  rect.top:=0;
  rect.right:=form.width;
  rect.bottom:=form.height;
  form.ManualFloat(rect);
  form.Align:=alNone;
end;

{
Shows/hides a simple secondary form.
Simple secondary form is one which can either be:
  - undocked and hidden
  - docked and visible at a fixed position
Forms which can be docked at several positions require special treatment
which is given in ApplyDisplayMode.
}
procedure TfMenu.ToggleForm(form:TForm;state:boolean);
begin
  if state <> form.Visible then
    if state then begin
      DockExpress(form,true);
      form.show;
    end else begin
      form.hide;
      DockExpress(form,false);
    end;
end;

{ Updates the visibility/docking of Examples form. }
procedure TfMenu.ToggleExamples();
begin
 //Undock from wherever it was
  if (fExamples<>nil) and fExamples.visible then
    DockExpress(fExamples,false);
 //Dock to wherever it belongs to
  if fExamples<>nil then
  if ((DisplayMode in [2, 4]) and aDictExamples.Checked) or
     ((DisplayMode=5) and aVocabExamples.Checked) then
    DockExpress(fExamples,true);
end;

{
Switches between main display modes of the main form (Kanji/Dict/Editor/etc)
Some modes are combinations of others (e.g. Editor + Dict), some are hidden and
cannot be selected from tab panel.

}
procedure TfMenu.SetDisplayMode(const AMode: integer);
begin
  if FCurDisplayMode=AMode then exit;
 { Activation mechanism is through a variable so that you can be smart and delay
  ApplyDisplayMode when changing a lot of things (used when loading) }
  FSetDisplayMode := AMode;
  ApplyDisplayMode;
end;

{
Reacts to mode changes and updates main form, showing or hiding various panels.
Handles main panel mostly, but also some secondary panels which can be open as
main (fKanjiCompounds).
Updates:
  aMode*.Checked
  tab*.Down
Adjusts visibility/dock state of various forms which depend on active page
(fExamples).
Relatively heavyweight/flickering, so avoid for simple panels (use ToggleForm)
and only call once after all changes.
}
procedure TfMenu.ApplyDisplayMode;
begin
 //Hide and detach active modules
  case FCurDisplayMode of
    1:if fKanji<>nil then begin
      fKanji.Hide;
      if (fKanjiCompounds<>nil) and (fKanjiCompounds.HostDockSite=fKanji.pnlDockCompounds) then begin
        KanjiCompoundsDockedHeight := fKanjiCompounds.Height;
        DockProc(fKanjiCompounds,nil,alBottom);
      end;
      fKanji.splDockCompounds.Visible := false;
      fKanji.splDockCompounds.Top := fKanji.pnlDockCompounds.Top - 1;
    end;
    2:if fWordLookup<>nil then fWordLookup.Hide;
    3:if fEditor<>nil then fEditor.Hide;
    4:begin
        if fWordLookup<>nil then fWordLookup.Hide;
        if fEditor<>nil then fEditor.Hide;
      end;
    5:if fVocab<>nil then fVocab.Hide;
    6:if fKanjiCompounds<>nil then begin
        fKanjiCompounds.Hide;
        DockProc(fKanjiCompounds,nil,alClient);
      end;
  end;

  BottomPanel.Height:=0;

  aModeKanji.Checked:=false;
  aModeDict.Checked:=false;
  aModeEditor.Checked:=false;
  aModeVocab.Checked:=false;

  FCurDisplayMode := FSetDisplayMode;

 { If KanjiDetails is in docked mode, show or hide it as needed.
  When in free-floating mode it doesn't need our attention. }
  if CharDetDocked then begin
    if (DisplayMode=1) and CharDetDockedVis1 then
      DockExpress(fKanjiDetails,true)
    else
    if (DisplayMode in [3, 4]) and CharDetDockedVis2 then
      DockExpress(fKanjiDetails,true)
    else
      DockExpress(fKanjiDetails,false);
  end else begin
   //Hide dock panel
    RightPanel.Width := 0;
    RightPanel.Height := 0;
  end;

  if (fExamples<>nil) and fExamples.visible then
    DockExpress(fExamples,false);

  case displaymode of
    1:begin
        if fKanji<>nil then
          MainDock(fKanji,MainPanel);
        if (fKanji<>nil) and (fKanjiCompounds<>nil) and aKanjiCompounds.Checked then begin
          fKanjiCompounds.Height := KanjiCompoundsDockedHeight;
          DockProc(fKanjiCompounds,fKanji.pnlDockCompounds,alBottom);
          fKanji.splDockCompounds.Visible := true;
          fKanji.splDockCompounds.Top := fKanji.pnlDockCompounds.Top - 1;
        end;
        tab1.Down:=true;
        if fKanji<>nil then
          if fKanji.DrawGrid1.CanFocus then
            fKanji.DrawGrid1.SetFocus;
        aModeKanji.Checked:=true;
      end;
    2:begin
        if fWordLookup<>nil then
          MainDock(fWordLookup,MainPanel);
        tab2.Down:=true;
        aModeDict.Checked:=true;
      end;
    3:begin
        if fEditor<>nil then MainDock(fEditor,MainPanel);
        tab3.Down:=true;
        if fEditor<>nil then
          fEditor.sbDockDictionary.Down:=false;
        aModeEditor.Checked:=true;
      end;
    4:begin
        BottomPanel.height:=250;
        if fWordLookup<>nil then
          MainDock(fWordLookup,BottomPanel);
        if fEditor<>nil then MainDock(fEditor,MainPanel);
        tab3.Down:=true;
        if fEditor<>nil then
          fEditor.sbDockDictionary.Down:=true;
        aModeEditor.Checked:=true;
      end;
    5:begin
        if fVocab<>nil then
          MainDock(fVocab,MainPanel);
        tab5.Down:=true;
        aModeVocab.Checked:=true;
      end;
    6:begin
        if fKanjiCompounds<>nil then
          DockProc(fKanjiCompounds, MainPanel, alClient);
         //Do not use MainDock, it will ruin undockwidth/undockheight
        tab1.Down := false;
        tab2.Down := false;
        tab3.Down := false;
        tab5.Down := false;
      end;
  end;

  UpdateWindowTitle();

  if fKanjiDetails<>nil then
    fKanjiDetails.UpdateVisible();
  ToggleExamples();
end;

{ Reacts to tabs above but does not get called otherwise }
procedure TfMenu.TabControl1Change(Sender: TObject);
begin
  if tab1.Down then DisplayMode:=1;
  if tab2.Down then DisplayMode:=2;
  if tab3.Down then
    if (fEditor=nil) or not fEditor.sbDockDictionary.Down then
      DisplayMode := 3
    else
      DisplayMode := 4;
  if tab5.Down then DisplayMode:=5;
end;

procedure TfMenu.aModeKanjiExecute(Sender: TObject);
begin
  DisplayMode:=1;
end;

procedure TfMenu.aModeDictExecute(Sender: TObject);
begin
  DisplayMode:=2;
end;

procedure TfMenu.aModeEditorExecute(Sender: TObject);
begin
  if (fEditor<>nil) and fEditor.sbDockDictionary.Down then
    DisplayMode:=4
  else
    DisplayMode:=3;
end;

procedure TfMenu.aModeVocabExecute(Sender: TObject);
begin
  DisplayMode:=5;
end;

//Attached to run before any of fEditor.Actions
procedure TfMenu.EditorActionExecute(Action: TBasicAction; var Handled: Boolean);
begin
  if not fEditor.Visible then aModeEditor.Execute;
end;

//Attached to run before any of fWordLookup.Actions
procedure TfMenu.WordLookupActionExecute(Action: TBasicAction; var Handled: Boolean);
begin
 //Dictionary may be used alone or from Editor
  if not fWordLookup.Visible then
    if fEditor.Visible then begin
      fEditor.sbDockDictionary.Down := true;
      fEditor.sbDockDictionaryClick(fEditor.sbDockDictionary);
    end else
      aModeDict.Execute;
end;

//Attached to run before any of fKanji.Actions
procedure TfMenu.KanjiListActionExecute(Action: TBasicAction; var Handled: Boolean);
begin
  if not fKanji.Visible then aModeKanji.Execute;
end;

{ Reconfigures the form to a landscape or portrait mode.
 If Loading is set, only applies the configuration part (button captions,
 panel alignment etc), and does not do actual docking.

 Why? See Issue 187. We want to configure everything first, then dock once,
 by ApplyDisplayMode.

 If there are ever forms that only need to be redocked here, still redock them
 as needed in ApplyDisplayMode. This function must be able to skip all redocking.

 I don't know what happens if Loading==true and some forms are docked by that
 point. Maybe their layout will be broken.}
procedure TfMenu.SetPortraitMode(Value: boolean; Loading: boolean);
var UserFiltersDocked: boolean;
  WordKanjiDocked: boolean;
  KanjiDetailsDocked: boolean;
begin
  UserFiltersDocked := (not Loading) and (fVocabFilters<>nil) and DockExpress(fVocabFilters,false);
  WordKanjiDocked := (not Loading) and (fWordKanji<>nil) and DockExpress(fWordKanji,false);
  KanjiDetailsDocked := (not Loading) and (fKanjiDetails<>nil) and CharDetDocked and DockExpress(fKanjiDetails,false);

  if Value then begin
    RightPanel.Align := alBottom;
    if fVocab<>nil then begin
      fVocab.pnlDockFilters.Align := alBottom;
      fVocab.splDockFilters.Align := alBottom;
      fVocab.splDockFilters.Top := fVocab.pnlDockFilters.Top - 1;
    end;
    if fWordLookup<>nil then
      fWordLookup.CharInWordDock.Align := alBottom;
  end else begin
    RightPanel.Align := alRight;
    if fVocab<>nil then begin
      fVocab.pnlDockFilters.Align := alRight;
      fVocab.splDockFilters.Align := alRight;
      fVocab.splDockFilters.Left := fVocab.pnlDockFilters.Left - 1;
    end;
    if fWordLookup<>nil then
      fWordLookup.CharInWordDock.Align := alRight;
  end;

 //New dock mode will be applied to forms on re-docking

 //If CharDetDocked was false (logically Undocked), then KanjiDetailsDocked
 //will be false too, and we won't even try to redock fKanjiDetails, which is right.

  if fVocabFilters<>nil then
    if UserFiltersDocked then DockExpress(fVocabFilters,true);
  if fWordKanji<>nil then
    if WordKanjiDocked then DockExpress(fWordKanji,true);
  if (fKanjiDetails<>nil) and KanjiDetailsDocked then
    DockExpress(fKanjiDetails,true);
 //ApplyDisplayMode -- should not be needed
end;

procedure TfMenu.aPortraitModeExecute(Sender: TObject);
begin
  SetPortraitMode(aPortraitMode.Checked, {Loading=}false);
end;

procedure TfMenu.btnScreenModeWkClick(Sender: TObject);
begin
  ScreenTip.EnabledInWakan := btnScreenModeWk.Down;
end;

procedure TfMenu.btnScreenModeScClick(Sender: TObject);
begin
  ScreenTip.EnabledSystemWide := btnScreenModeSc.Down;
end;

procedure TfMenu.ScreenTimerTimer(Sender: TObject);
begin
  if not initdone then exit;
  AutosaveUserData;
  ScreenTip.MaybePopup;
  UpdateWindowTitle; //have no way of knowing when child form caption changes =(
end;

//Called by ScreenTip to handle button clicking
procedure TfMenu.ScreenTipButtonClick(ASender: TObject; AButtonId: integer);
begin
  if fRadical.Visible then exit;
  if AButtonID=0 then exit;
  if (AButtonID>2) and (not Self.Focused) then Self.Show;
  case AButtonID of
    1: Clipboard.Text := Clipboard.Text + TfScreenTipForm(ASender).Text;
    2: Clipboard.Text := TfScreenTipForm(ASender).Text;
    3:begin
        Clipboard.Text := TfScreenTipForm(ASender).Text;
        if not fRadical.Visible then fWordLookup.aLookupClip.Execute;
      end;
    4:begin
        if fRadical.Visible then exit;
        if not fKanjiDetails.Visible then aKanjiDetails.Execute;
        fKanjiDetails.SetCharDetails(fcopy(TfScreenTipForm(ASender).Text ,1,1));
      end;
  end;
end;


end.
