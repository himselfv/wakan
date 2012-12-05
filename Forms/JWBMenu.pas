unit JWBMenu;

interface

{$R WINXP.RES}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RXCtrls, Db,
  DBTables, ExtCtrls, Grids, TextTable, Buttons, {ThemeMgr,} MemSource, ShellApi,
  ActnList, Menus, rxPlacemnt{MCH, madCodeHook}, JWBUtils;

type
  TfMenu = class(TForm)
    Panel1: TPanel;
    tab1: TSpeedButton;
    tab5: TSpeedButton;
    tab2: TSpeedButton;
    SpeedButton13: TSpeedButton;
    Bevel3: TBevel;
    SpeedButton14: TSpeedButton;
    Bevel4: TBevel;
    SpeedButton22: TSpeedButton;
    Bevel5: TBevel;
    tab3: TSpeedButton;
    Timer1: TTimer;
    ActionList1: TActionList;
    aSaveUser: TAction;
    aCancelUser: TAction;
    aStatistics: TAction;
    aExit: TAction;
    aKanji: TAction;
    aKanjiSearch: TAction;
    aKanjiSort: TAction;
    aKanjiDetails: TAction;
    aKanjiCompounds: TAction;
    aKanjiPrint: TAction;
    aDict: TAction;
    aDictDetails: TAction;
    aDictKanji: TAction;
    aDictCategories: TAction;
    aDictAdd: TAction;
    aDictEditor: TAction;
    aUser: TAction;
    aUserAdd: TAction;
    aUserSettings: TAction;
    aUserCategory: TAction;
    aUserDetails: TAction;
    aUserPrint: TAction;
    aUserGenerate: TAction;
    aSettings: TAction;
    aSettingsDict: TAction;
    aLayout: TAction;
    aQuick1: TAction;
    aQuick2: TAction;
    aQuick3: TAction;
    aQuick4: TAction;
    aQuick5: TAction;
    aQuick6: TAction;
    aBorders: TAction;
    aHelp: TAction;
    aAbout: TAction;
    aJapanese: TAction;
    aChinese: TAction;
    aEditorNew: TAction;
    aEditorOpen: TAction;
    aEditorSave: TAction;
    aEditorSaveAs: TAction;
    aEditorCut: TAction;
    aEditorCopy: TAction;
    aEditorPaste: TAction;
    aEditorSelectAll: TAction;
    aEditorKanjiMode: TAction;
    aEditorKanaMode: TAction;
    aEditorASCIIMode: TAction;
    aEditorReading: TAction;
    aEditorMeaning: TAction;
    aEditorClear: TAction;
    aEditorFill: TAction;
    aEditorSet: TAction;
    aEditorPrint: TAction;
    aKanjiAll: TAction;
    aKanjiLearned: TAction;
    aKanjiCommon: TAction;
    aKanjiClipboard: TAction;
    aKanjiPinYin: TAction;
    aKanjiYomi: TAction;
    aKanjiRadical: TAction;
    aKanjiAddClipboard: TAction;
    aKanjiFullDetails: TAction;
    aDictJapanese: TAction;
    aDictEnglish: TAction;
    aDictClipboard: TAction;
    aDictAddClipboard: TAction;
    aDictExact: TAction;
    aDictBeginning: TAction;
    aDictEnd: TAction;
    aKanjiWindow: TAction;
    aKanjiSetLearned: TAction;
    aKanjiMeaning: TAction;
    aEditorWindow: TAction;
    aEditorSmallFont: TAction;
    aEditorLargeFont: TAction;
    aEditorMedFont: TAction;
    MainMenu1: TMainMenu;
    fdfd1: TMenuItem;
    JapMode1: TMenuItem;
    ChinMode1: TMenuItem;
    eSaveuserchangescUloituivatelskzmny1: TMenuItem;
    eCanceluserchangescZruituivatelskzmny1: TMenuItem;
    N1: TMenuItem;
    eStatisticscStatistika1: TMenuItem;
    eSettingscNastaven2: TMenuItem;
    eDictionarymanagercManaerslovnk1: TMenuItem;
    N15: TMenuItem;
    eExitcUkonit1: TMenuItem;
    eCharacterscZnaky1: TMenuItem;
    N5: TMenuItem;
    eDisplayallcZobrazitvechny1: TMenuItem;
    eLearnedonlycPouzenauen1: TMenuItem;
    eCommononlycPouzebn1: TMenuItem;
    eInclipboardonlycPouzeveschrnce1: TMenuItem;
    N3: TMenuItem;
    eSearchbyPinYincHledatpodlePinYin1: TMenuItem;
    aSearchbyYomicHledatpodleYomi1: TMenuItem;
    eSearchbymeaningcHledatpodlevznamu1: TMenuItem;
    eSearchbyradicalcHledatpodleradiklu1: TMenuItem;
    N6: TMenuItem;
    eAddtoclipboardcPidatdoschrnky1: TMenuItem;
    eSetasunlearnedcNastavitjakonenauen1: TMenuItem;
    ePrintcardscTiskkaret1: TMenuItem;
    N13: TMenuItem;
    eSearchcHledn1: TMenuItem;
    eDetailscDetaily1: TMenuItem;
    eCompoundscSloeniny1: TMenuItem;
    eDictionarycSlovnk1: TMenuItem;
    eJapaneseChineseEnglishcJaponskynskyAnglicky1: TMenuItem;
    eEnglishJapaneseChinesecAnglickyJaponskynsky1: TMenuItem;
    eSearchbyclipboardcHledatpodleschrnky1: TMenuItem;
    N8: TMenuItem;
    eSearchexactwordcHledatpesnslovo1: TMenuItem;
    eSearchbeginningcHledatzatek1: TMenuItem;
    eSearchendingcHledatkonec1: TMenuItem;
    N12: TMenuItem;
    eAddtoclipboardcPidatdoschrnky2: TMenuItem;
    N16: TMenuItem;
    eCharactersinwordcZnakyveslov1: TMenuItem;
    eAddtovocabularycPidatdoslovek1: TMenuItem;
    Editor2: TMenuItem;
    N22: TMenuItem;
    eNewcNov1: TMenuItem;
    eOpencOtevt1: TMenuItem;
    eSavecUloit1: TMenuItem;
    eSaveascUloitjako1: TMenuItem;
    N17: TMenuItem;
    eCutcVyjmout1: TMenuItem;
    eCopycKoprovat1: TMenuItem;
    ePastecVloit1: TMenuItem;
    eSelectallcVybratve1: TMenuItem;
    N18: TMenuItem;
    eCharactermodecReimznak1: TMenuItem;
    eKanamodecReimkany1: TMenuItem;
    eASCIImodecReimASCII1: TMenuItem;
    N19: TMenuItem;
    eDisplayreadingcZobrazitten1: TMenuItem;
    eDisplaymeaningcZobrazitzpis1: TMenuItem;
    eFontsizecVelikostpsma1: TMenuItem;
    eSmallcMal1: TMenuItem;
    eMediumcStedn1: TMenuItem;
    eLargecVelk1: TMenuItem;
    N20: TMenuItem;
    eTranslationcPeklad1: TMenuItem;
    eCleartranslationcSmazatpeklad1: TMenuItem;
    eSettranslationcNastavitpeklad1: TMenuItem;
    eAutofilltranslationcAutomatickydoplnitpeklad1: TMenuItem;
    N21: TMenuItem;
    ePrintcTisk1: TMenuItem;
    eVocabularycSlovka1: TMenuItem;
    eAddwordcPidatslovko1: TMenuItem;
    eListsettingscNastavenseznamu1: TMenuItem;
    eWorddetailscDetailyslovka1: TMenuItem;
    N7: TMenuItem;
    ePrintlistcTiskseznamu1: TMenuItem;
    eListgeneratorcGenertorseznamu1: TMenuItem;
    eHelpcNpovda1: TMenuItem;
    eHelpcontentscObsahnpovdy1: TMenuItem;
    N11: TMenuItem;
    eAboutcOprogramu1: TMenuItem;
    Timer2: TTimer;
    Panel3: TPanel;
    Panel2: TPanel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    aMode1: TAction;
    aMode2: TAction;
    aMode3: TAction;
    aMode4: TAction;
    aMode5: TAction;
    eModecReim1: TMenuItem;
    eCharacterlistcSeznamznak1: TMenuItem;
    eDictionarycSlovnk2: TMenuItem;
    eTexteditorcTextoveditor1: TMenuItem;
    eVocabularycSlovka2: TMenuItem;
    eCharacterlistcSeznamznak2: TMenuItem;
    eDictionarycSlovnk3: TMenuItem;
    N4: TMenuItem;
    eTexteditorcTextoveditor2: TMenuItem;
    eVocabularycSlovka3: TMenuItem;
    N9: TMenuItem;
    Bevel1: TBevel;
    ScreenTimer: TTimer;
    Panel4: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    eSavekanatranscriptcUloitpepisdokany1: TMenuItem;
    SaveDialog1: TSaveDialog;
    aDictInflect: TAction;
    aDictAuto: TAction;
    aDictGroup1: TAction;
    aDictGroup2: TAction;
    aDictGroup3: TAction;
    eSearchinflectedwordscHledatinflektovanslova1: TMenuItem;
    eAutosearchwhiletypingcAutomatickyhledatpipsan1: TMenuItem;
    eUseddictionarygroupcPouitskupinaslovnk1: TMenuItem;
    N10: TMenuItem;
    eUsedictionariesingroup1cPouvatslovnkyzeskupiny11: TMenuItem;
    eUsedictionariesingroup2cPouvatslovnkyzeskupiny21: TMenuItem;
    eUsedictionariesingroup3cPouvatslovnkyzeskupiny31: TMenuItem;
    aDictVoc1: TAction;
    aDictVoc2: TAction;
    aDictVoc3: TAction;
    aUserExamples: TAction;
    Pklady1: TMenuItem;
    aEditorColors: TAction;
    eUsecolorscPouvatbarvy1: TMenuItem;
    aDictMiddle: TAction;
    eSearchsubstringcHledatpodetzec1: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N2: TMenuItem;
    eSavecharacterstofilecUloitznakydosouboru1: TMenuItem;
    N00242eAddwordcPidatslovko1: TMenuItem;
    N00929eChangelanguage1: TMenuItem;
    aChangeLanguage: TAction;
    FormPlacement1: TFormPlacement;
    procedure Button6Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton5Click(Sender: TObject);
    procedure ArtLabel1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure tab1Click(Sender: TObject);
    procedure tab2Click(Sender: TObject);
    procedure tab5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure aSaveUserExecute(Sender: TObject);
    procedure aCancelUserExecute(Sender: TObject);
    procedure aStatisticsExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aKanjiExecute(Sender: TObject);
    procedure aDictExecute(Sender: TObject);
    procedure aUserExecute(Sender: TObject);
    procedure aKanjiSearchExecute(Sender: TObject);
    procedure aKanjiSortExecute(Sender: TObject);
    procedure aKanjiDetailsExecute(Sender: TObject);
    procedure aKanjiCompoundsExecute(Sender: TObject);
    procedure aKanjiPrintExecute(Sender: TObject);
    procedure aDictDetailsExecute(Sender: TObject);
    procedure aDictKanjiExecute(Sender: TObject);
    procedure aDictCategoriesExecute(Sender: TObject);
    procedure aDictAddExecute(Sender: TObject);
    procedure aDictEditorExecute(Sender: TObject);
    procedure aUserAddExecute(Sender: TObject);
    procedure aUserSettingsExecute(Sender: TObject);
    procedure aUserCategoryExecute(Sender: TObject);
    procedure aUserDetailsExecute(Sender: TObject);
    procedure aUserPrintExecute(Sender: TObject);
    procedure aUserGenerateExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure aSettingsDictExecute(Sender: TObject);
    procedure aQuick1Execute(Sender: TObject);
    procedure aQuick2Execute(Sender: TObject);
    procedure aQuick3Execute(Sender: TObject);
    procedure aQuick4Execute(Sender: TObject);
    procedure aQuick5Execute(Sender: TObject);
    procedure aQuick6Execute(Sender: TObject);
    procedure aBordersExecute(Sender: TObject);
    procedure aHelpExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure aJapaneseExecute(Sender: TObject);
    procedure aChineseExecute(Sender: TObject);
    procedure aEditorNewExecute(Sender: TObject);
    procedure aEditorOpenExecute(Sender: TObject);
    procedure aEditorSaveExecute(Sender: TObject);
    procedure aEditorSaveAsExecute(Sender: TObject);
    procedure aEditorCutExecute(Sender: TObject);
    procedure aEditorCopyExecute(Sender: TObject);
    procedure aEditorPasteExecute(Sender: TObject);
    procedure aEditorSelectAllExecute(Sender: TObject);
    procedure aEditorKanjiModeExecute(Sender: TObject);
    procedure aEditorKanaModeExecute(Sender: TObject);
    procedure aEditorASCIIModeExecute(Sender: TObject);
    procedure aEditorReadingExecute(Sender: TObject);
    procedure aEditorMeaningExecute(Sender: TObject);
    procedure aEditorClearExecute(Sender: TObject);
    procedure aEditorFillExecute(Sender: TObject);
    procedure aEditorSetExecute(Sender: TObject);
    procedure aEditorPrintExecute(Sender: TObject);
    procedure aKanjiAllExecute(Sender: TObject);
    procedure aKanjiLearnedExecute(Sender: TObject);
    procedure aKanjiCommonExecute(Sender: TObject);
    procedure aKanjiClipboardExecute(Sender: TObject);
    procedure aKanjiPinYinExecute(Sender: TObject);
    procedure aKanjiYomiExecute(Sender: TObject);
    procedure aKanjiRadicalExecute(Sender: TObject);
    procedure aKanjiAddClipboardExecute(Sender: TObject);
    procedure aKanjiSetLearnedExecute(Sender: TObject);
    procedure aKanjiFullDetailsExecute(Sender: TObject);
    procedure aDictJapaneseExecute(Sender: TObject);
    procedure aDictEnglishExecute(Sender: TObject);
    procedure aDictClipboardExecute(Sender: TObject);
    procedure aDictAddClipboardExecute(Sender: TObject);
    procedure aDictExactExecute(Sender: TObject);
    procedure aDictBeginningExecute(Sender: TObject);
    procedure aDictEndExecute(Sender: TObject);
    procedure aKanjiWindowExecute(Sender: TObject);
    procedure aKanjiMeaningExecute(Sender: TObject);
    procedure aEditorWindowExecute(Sender: TObject);
    procedure aEditorSmallFontExecute(Sender: TObject);
    procedure aEditorLargeFontExecute(Sender: TObject);
    procedure aEditorMedFontExecute(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure tab3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tab4Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure eCharacterlistcSeznamznak1Click(Sender: TObject);
    procedure eDictionarycSlovnk2Click(Sender: TObject);
    procedure eTexteditorcTextoveditor1Click(Sender: TObject);
    procedure aMode1Execute(Sender: TObject);
    procedure aMode2Execute(Sender: TObject);
    procedure aMode3Execute(Sender: TObject);
    procedure aMode4Execute(Sender: TObject);
    procedure aMode5Execute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ScreenTimerTimer(Sender: TObject);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure eSavekanatranscriptcUloitpepisdokany1Click(Sender: TObject);
    procedure aDictInflectExecute(Sender: TObject);
    procedure aDictAutoExecute(Sender: TObject);
    procedure aDictGroup1Execute(Sender: TObject);
    procedure aDictGroup2Execute(Sender: TObject);
    procedure aDictGroup3Execute(Sender: TObject);
    procedure aDictVoc1Execute(Sender: TObject);
    procedure aDictVoc2Execute(Sender: TObject);
    procedure aDictVoc3Execute(Sender: TObject);
    procedure aUserExamplesExecute(Sender: TObject);
    procedure aEditorColorsExecute(Sender: TObject);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure eSavecharacterstofilecUloitznakydosouboru1Click(
      Sender: TObject);
    procedure aDictMiddleExecute(Sender: TObject);
    procedure aChangeLanguageExecute(Sender: TObject);

  private
    initdone:boolean;
    { Private declarations }
  public
    ResFixVal:integer;
    StrokeOrderPackage:TPackageSource;
    screenTipShown:boolean;
    screenTipText:string;
    screenTipWt:integer;
    screenTipDebug:string;
    screenTipList:TStringList;
    screenTipWords,ScreenTipWidth:integer;
    screenTipButton:integer;
    screenModeSc,screenModeWk:boolean;
    screenTipImmediate:boolean;
    ctlFileMap:cardinal;
    ptrFileMap:pointer;

    procedure XPResFix(form:TForm);
    procedure Clipboard_PaintBox3Paint(Sender: TObject);
    procedure Clipboard_Timer1Timer(Sender: TObject);
    procedure Clipboard_SpeedButton7Click(Sender: TObject);
    procedure TranslateAll;
    procedure SetFormPos(form:TForm);
    procedure ChangeClipboard;
    procedure WriteUserPackage;
    procedure SaveUserData;
    procedure LoadUserData;
    procedure ChangeUserData;
    procedure RefreshCategory;
    procedure RefreshKanjiCategory;
    procedure ExportUserData(filename:string);
    procedure ImportUserData(filename:string);
    function FlushUserData:boolean;
    procedure ToggleForm(form:TForm;sb:TSpeedButton;action:TAction);
    procedure WriteLayout(filename:string);
    procedure ReadLayout(filename:string);
    procedure ShowForm(button:TSpeedButton;action:TAction;form:TForm);
    procedure HideForm(button:TSpeedButton;action:TAction;form:TForm);
    procedure StandardLayout(lay:integer;perc:integer);
    procedure RescanDicts;
    procedure ClearDicts;
    procedure SwitchLanguage(lanchar:char);
    procedure DockExpress(form:TForm;dock:boolean);
    procedure FixConstraints(form:TForm;w,h:integer);
    function GetCharDet(i,j:integer):string;
    function GetCharType(i,j:integer):string;
    function GetCharValue(index,vt:integer):string;
    function GetCharValueInt(index,vt:integer):integer;
    function GetCharValueRad(index,vt:integer):integer;
    procedure ChangeDisplay;
    procedure FormUndock(form:TForm);
    procedure ShowScreenTip(x,y:integer;s:string;wt:integer;immediate:boolean);
    procedure HideScreenTip;
    procedure PaintScreenTip;
    procedure DrawPopupButtons(sel:integer);
    procedure PopupMouseMove(x,y:integer);
    procedure PopupMouseUp(button:TMouseButton;shift:TShiftState;x,y:integer);
    procedure PopupImmediate(left:boolean);
    procedure RebuildUserIndex;
    procedure RebuildAnnotations;
    procedure LoadAnnotations;
    function IsAnnot:boolean;
    procedure AnnotSeek(des:array of string);
    procedure AnnotSeekK(kanji,kana:string);
    procedure AnnotFirst;
    function AnnotGet(cmd:char):string;
    function AnnotGetOne(cmd:char):string;
    function AnnotGetAll(cmd:char; delimit:string):string;
    procedure AnnotShowMedia(kanji,kana:string);
//    procedure LoadLayout(filename:string);
//    procedure SaveFixedLayout(filename:string);


  private //Text under mouse
    intcurString:string; { String under the mouse pointer right now.
      Only CalculateCurString changes this member, and only ScreenTimerTimer uses it.
      And CalculateCurString is only called from IntTipPaintOver/IntTipGridOver }
    //What. Are. These.
    //And why are there two of them?
    intorgPaint:TPaintBox;
    intorgGrid:TDrawGrid;
    intorgcx,intorgcy,intorgsx,intorgsy:integer;
    intmoPaint:TPaintBox;
    intmoGrid:TDrawGrid;
    intmocx,intmocy,intmosx,intmosy:integer;
    procedure CalculateCurString;
  public
    procedure IntTipPaintOver(p:TPaintBox;x,y:integer;leftDown:boolean);
    procedure IntTipGridOver(sg:TDrawGrid;x,y:integer;leftDown:boolean);

  protected //Clipboard viewer
   { SetClipboardViewer is supported starting with Windows 2000,
    so if there's a need we can implement dynamic linking and fall back to polling -
    it's kept as a safety measure anyway since CB chains are prone to breaking }
    CbNextViewer: HWND;
    procedure WmChangeCbChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    procedure WmDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;

  end;

  TTextInfo = record
    hwnd:HWND;
    hdc:HDC;
    x,y,w,h:integer;
    slen:byte;
    str:array[0..255] of word;
    len:array[0..255] of byte;
    dcinfo:integer;
  end;
  TBitInfo = record
    hwnd:HWND;
    destdc,srcdc:HDC;
    xofs,yofs:integer;
  end;

var
  fMenu: TfMenu;
  curAnnot:string;
  curAnnotPos:integer;
  romasys,jromasys,cromasys:integer;
  showroma,jshowroma,cshowroma:boolean;
  CharDetDocked,CharDetNowDocked,CharDetDockedVis1,CharDetDockedVis2:boolean;
  dicts:TStringList;
  TAnnots,TChar,TCharRead,TRadicals,TUser,TUserIdx,TUserSheet,TUserCat,TKanaKanji,TUserPrior:TTextTable;
  TCharIndex,TCharChinese,TCharType,TCharUnicode,
  TCharStrokeCount,TCharJpStrokeCount,TCharJpFrequency,TCharChFrequency,
  TCharJouyouGrade,TCharReadKanji,TCharReadType,TCharReadReading,TCharReadIndex,
  TCharReadReadDot,TCharReadPosition,
  TRadicalsNumber,TRadicalsVariant,TRadicalsUnicode,
  TRadicalsStrokeCount,TRadicalsUnicodeCount,TRadicalsBushuCount,
  TRadicalsJapaneseCount,TRadicalsKangXiCount,
  TUserEnglish,TUserPhonetic,TUserPhoneticSort,TUserKanji,TUserAdded,
  TUserPrinted,TUserLearned,TUserMastered,TUserNoPrinted,TUserScore,TUserMaxScore,
  TUserIdxWord,TUserIdxKanji,TUserIdxBegin,TUserIdxIndex,TUserIndex,
  TUserSheetWord,TUserSheetNumber,TUserSheetPos,TUserCatIndex,TUserCatName,TUserCatType,TUserCatCreated:integer;
  TUserConvertKanji,TUserConvertCount:integer;
  KnownLearned:integer;
  Clip,cliptrans:string;
  NotUsedDicts:string;
  NotGroupDicts:array[1..5] of string;
  OfflineDicts:string;
  oldhandle:THandle;
  critsec:boolean;
  UserDataChanged:boolean;
  globheight:integer;
  MaxCategoryIndex,MaxUserIndex:integer;
  ChinesePresent:boolean;
  doc,doctr,docdic:TStringList;
  defll: TDeflectionList;
  partl,bopomofol,markersl,suffixl,ignorel,readchl:TStringList;
  firstact:boolean;
  userdataloaded:boolean;
  curlang:char;
  curqlayout:integer;
  proposedlayout:integer;
  borderchange:boolean;
  chardetl:TStringList;
  chartypel:TStringList;
  romasortl:TStringList;
  displaymode:integer;
  curdisplaymode:integer;
  oldpt,begpt:TPoint;
  tim:integer;
  rdcnt,bitcnt:integer;
  curtext:array[1..100] of TTextInfo;
  curbit:array[1..100] of TBitInfo;
  ftext:array[0..255] of word;
  ftextbeg:array[0..255] of integer;
  ftextend:array[0..255] of integer;
  ftextpos:integer;
  inproc:boolean;
  popcreated:boolean;
  vocmode,exmode:integer;
  rainesearch:pointer;
  raineradicals:TStringList;
  sodir:TStringList;
  sobin:pointer;
  lastautosave:TDateTime;
  kanjicatuniqs:TStringList;
  dictbeginset,dictmodeset:integer;
  kanji_othersearch:integer;

var
  WakanVer: string = ''; //taken from resources on load

const
  CurStructVer=2;
  CurDictVer=7;
  CurDicVer=4;

  PopupButtonNum=4;
  PopupButtonWidth=23;
  PopupButtonSep=2;

function _l(id:string):string;

implementation

uses JWBKanji, StdPrompt, JWBUnit, JWBRadical,
  JWBSettings, JWBSplash, PKGWrite, JWBAlphabet, JWBUser, UnicodeFont, registry, clipbrd,
  JWBWords, JWBNewCategory, JWBPrint, JWBSizeCheck, JWBStatistics,
  JWBWordList, JWBBitmap, JWBClipboard, JWBKanjiCompounds,
  JWBWordAdd, JWBUserDetails, JWBUserAdd, JWBUserCategory, JWBUserFilters,
  JWBKanjiDetails, JWBKanjiSort, JWBKanjiSearch, JWBWordDetails,
  JWBWordCategory, JWBWordKanji, JWBTranslate, JWBLayout, JWBStrokeOrder,
  JWBDictMan, JWBDictImport, JWBDictCoding, JWBCharItem, JWBScreenTip,
  JWBInvalidator, JWBDicAdd, JWBLanguage, JWBPopupButton, JWBFileType, JWBConvert,
  JWBWordsExpChoose, JWBMedia, JWBDicSearch;

{$R *.DFM}

{ TfMenu }

function TfMenu.GetCharDet(i,j:integer):string;
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

function TfMenu.GetCharValueInt(index,vt:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,vt);
  try
    if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
    if s='' then result:=65535 else result:=strtoint(s);
  except end;
end;

function TfMenu.GetCharValueRad(index,vt:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,vt);
  if pos('.',s)>0 then delete(s,pos('.',s),length(s)-pos('.',s)+1);
  try
    if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
    if s='' then result:=65535 else result:=strtoint(s);
  except end;
end;

function TfMenu.GetCharValue(index,vt:integer):string;
begin
  TCharRead.SetOrder('');
  if TCharRead.Locate('Kanji',inttostr(index),true) then
  while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=index) do
  begin
    if TCharRead.Int(TCharReadType)=vt then
    begin
      result:=TCharRead.Str(TCharReadReading);
      exit;
    end;
    TCharRead.Next;
  end;
  result:='';
end;

function TfMenu.GetCharType(i,j:integer):string;
var s:string;
begin
  s:=chartypel[i];
  while j>0 do
  begin
    delete(s,1,pos(',',s));
    dec(j);
  end;
  if j<6 then delete(s,pos(',',s),length(s)-pos(',',s)+1);
  result:=s;
end;

procedure TfMenu.ClearDicts;
var i:integer;
begin
  for i:=0 to dicts.Count-1 do
  begin
    if (dicts.Objects[i] as TJaletDic).loaded then
      (dicts.Objects[i] as TJaletDIc).unload;
    (dicts.Objects[i] as TJaletDic).Free;
  end;
  dicts.Clear;
end;

procedure TfMenu.RescanDicts;
var sr:TSearchRec;
    dic:TJaletDic;
begin
  ClearDicts;
  fDictMan.CheckListBox1.Items.Clear;
  if FindFirst('*.dic',faAnyFile,sr)=0 then
  repeat
    dic:=TJaletDic.Create;
    dic.FillInfo(sr.name);
    if dic.tested then
    begin
      if Uppercase(dic.pname)='JALET.DIC'then
        Application.MessageBox(pchar(_l('#00326^eIt is not recommended to use old style JALET.DIC dictionary.^cPouûiv·nÌ starÈ verze slovnÌku JALET.DIC nenÌ doporuËeno.')),pchar(_l('#00090^eWarning^cVarov·nÌ')),MB_ICONWARNING or MB_OK);
      if (curlang=dic.language) then
      begin
        dicts.AddObject(dic.language+dic.name,dic);
        fDictMan.CheckListBox1.Items.Add(dic.name);
        if (pos(','+dic.name,NotUsedDicts)=0) then
        begin
          if (paramstr(1)<>'makedic') and (paramstr(1)<>'makeexamples') then dic.Load;
          fDictMan.CheckListBox1.Checked[fDictMan.CheckListBox1.Items.Count-1]:=true;
        end;
      end;
    end else dic.Free;
  until FindNext(sr)<>0;
  FindClose(sr);
  if (dicts.Count=0) and (paramstr(1)<>'makedic') and (paramstr(1)<>'makeexamples') then
  begin
    if curlang='j'then
      Application.MessageBox(pchar(_l('#00327^eNo valid japanese dictionary was found.'#13'Please download some japanese .DIC files from WAKAN website.^cNebyl nalezen û·dn˝ japonsk˝ slovnÌk.'#13'Nahrajte si prosÌm nÏjakÈ japonskÈ .DIC soubory z webovÈ str·nky WAKANu.')),pchar(_l('#00090^eWarning^cVarov·nÌ')),MB_ICONWARNING or MB_OK) else
      Application.MessageBox(pchar(_l('#00328^eNo valid chinese dictionary was found.'#13'Please download some chinese .DIC files from WAKAN website.^cNebyl nalezen û·dn˝ ËÌnsk˝ slovnÌk.'#13'Nahrajte si prosÌm nÏjakÈ ËÌnskÈ .DIC soubory z webovÈ str·nky WAKANu.')),pchar(_l('#00090^eWarning^cVarov·nÌ')),MB_ICONWARNING or MB_OK);
  end else
    fDictMan.CheckListBox1.ItemIndex:=0;
end;

procedure TfMenu.SwitchLanguage(lanchar:char);
var mf:TMemoryFile;
begin
  curlang:=lanchar;
  if lanchar='j'then
  begin
    romasys:=fSettings.RadioGroup1.ItemIndex+1;
    showroma:=fSettings.RadioGroup2.ItemIndex=1;
    SpeedButton14.Down:=true;
    aJapanese.Checked:=true;
    aChinese.Checked:=false;
    fUser.SpeedButton1.Caption:=_l('#00329^eJapanese ->English^cJaponsky -> Anglicky');
    fUser.SpeedButton2.Caption:=_l('#00330^eEnglish -> Japanese^cAnglicky -> Japonsky');
  end else
  begin
    romasys:=fSettings.RadioGroup6.ItemIndex+1;
    showroma:=fSettings.RadioGroup7.ItemIndex=1;
    SpeedButton13.Down:=true;
    aJapanese.Checked:=false;
    aChinese.Checked:=true;
    fUser.SpeedButton1.Caption:=_l('#00331^eChinese ->English^c»Ìnsky -> Anglicky');
    fUser.SpeedButton2.Caption:=_l('#00332^eEnglish -> Chinese^cAnglicky -> »Ìnsky');
  end;
  RescanDicts;
  fKanji.KanjiSearch_SpeedButton20Click(self);
//  fUser.SpeedButton4.Enabled:=lanchar='j';
  if (not fUser.SpeedButton3.Enabled) and (fUser.SpeedButton3.Down) then fUser.SpeedButton1.Down:=true;
  if exampackage<>nil then exampackage.Free;
  if examstruct<>nil then FreeMem(examstruct);
  if examindex<>nil then FreeMem(examindex);
  exampackage:=nil;
  examstruct:=nil;
  examindex:=nil;
  try
    if FileExists('examples_'+lanchar+'.pkg') then
    begin
      exampackage:=TPackageSource.Create('examples_'+lanchar+'.pkg',791564,978132,978123);
      mf:=exampackage['struct.bin'];
      if mf=nil then raise Exception.Create('Important file missing.');
      GetMem(examstruct,mf.Size);
      exampackage.ReadRawData(examstruct^,integer(mf.Position),mf.Size);
      examstructsiz:=mf.Size div 4;
      mf:=exampackage['index.bin'];
      if mf=nil then raise Exception.Create('Important file missing.');
      GetMem(examindex,mf.Size);
      exampackage.ReadRawData(examindex^,integer(mf.Position),mf.Size);
      examindexsiz:=mf.Size div 16;
      examfile:=exampackage['examples.bin'];
      if examfile=nil then raise Exception.Create('Important file missing.');
    end;
  except
    Application.MessageBox(pchar(_l('#00333^eCouldn''t load example file EXAMPLES_'+upcase(lanchar)+'.PKG.^cNepoda¯ilo se nahr·t soubor EXAMPLES_'+upcase(lanchar)+'.PKG.')),
      pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
    exampackage:=nil;
    examstruct:=nil;
    examindex:=nil;
  end;
  fUser.Look(false);
  RefreshCategory;
  RefreshKanjiCategory;
end;

procedure TfMenu.XPResFix(form:TForm);
var sl:TStringList;
    i:integer;
    ct:TControl;
    s:string;
begin
  exit;
  if ResFixVal<>0 then
  begin
    if form.Constraints.MinHeight>0 then form.Constraints.MinHeight:=form.Constraints.MinHeight+ResFixVal;
    if form.Constraints.MaxHeight>0 then form.Constraints.MaxHeight:=form.Constraints.MaxHeight+ResFixVal;
    sl:=TStringList.Create;
    for i:=0 to form.ComponentCount-1 do
    begin
      if form.Components[i] is TControl then
      begin
        ct:=form.Components[i] as TControl;
        s:='';
        if akTop in ct.Anchors then s:=s+'T';
        if akBottom in ct.Anchors then s:=s+'B';
        sl.Add(s);
        ct.Anchors:=ct.Anchors+[akTop];
        ct.Anchors:=ct.Anchors-[akBottom];
      end else sl.Add('');
    end;
    form.height:=form.height+ResFixVal;
    for i:=0 to form.ComponentCount-1 do
    begin
      if form.Components[i] is TControl then
      begin
        ct:=form.Components[i] as TControl;
        if (sl[i]='') or (sl[i]='B') then
          ct.Anchors:=ct.Anchors-[akTop];
        if (sl[i]='B') or (sl[i]='TB') then
          ct.Anchors:=ct.Anchors+[akBottom];
      end;
    end;
  end;
end;

procedure TfMenu.ShowForm(button:TSpeedButton;action:TAction;form:TForm);
begin
  exit;
  if borderchange then exit;
  if aBorders.Checked then
  begin
    if form.tag=1 then
    begin
      form.show;
      DockExpress(form,true);
    end else
    begin
      form.Show;
      DockExpress(form,true);
      form.hide;
    end;
  end else if form.tag=1 then form.show else form.hide;
  button.down:=form.visible;
  if action<>nil then action.checked:=form.visible;
end;

procedure TfMenu.HideForm(button:TSpeedButton;action:TAction;form:TForm);
begin
  exit;
  if borderchange then exit;
  if (not aBorders.Checked) or (form=fKanjiDetails) then
  begin
    if form.visible then form.tag:=1 else form.tag:=0;
    form.hide;
  end;
  button.down:=false;
  if action<>nil then action.checked:=false;
end;

procedure TfMenu.ToggleForm(form:TForm;sb:TSpeedButton;action:TAction);
begin
  if form=fKanji then
  begin
    displaymode:=1;
    ChangeDisplay;
    exit;
  end;
  if form=fUser then
  begin
    displaymode:=2;
    ChangeDisplay;
    exit;
  end;
  if form=fTranslate then
  begin
    displaymode:=3;
    ChangeDisplay;
    exit;
  end;
  if form=fWords then
  begin
    displaymode:=5;
    ChangeDisplay;
    exit;
  end;
  if (not aBorders.Checked) or (sb=nil) or (form=fKanjiDetails) then
  begin
    if form.visible then form.hide else form.show;
    if form.visible then form.tag:=1 else form.tag:=0;
  end else
  begin
    if action.checked then
    begin
      if form.visible then
      begin
        form.hide;
        DockExpress(form,false);
        form.tag:=0;
      end;
    end else
    begin
      if not form.visible then
      begin
        form.tag:=1;
        DockExpress(form,true);
        form.show;
        form.tag:=2;
      end;
    end;
  end;
  if sb<>nil then sb.down:=form.visible;
  if action<>nil then action.checked:=form.visible;
end;

procedure TfMenu.TranslateAll;
begin
  fLanguage.TranslateForm(self);
  fLanguage.TranslateForm(fKanji);
  fLanguage.TranslateForm(fRadical);
  fLanguage.TranslateForm(fAlphabet);
  fLanguage.TranslateForm(fWords);
  fLanguage.TranslateForm(fUser);
  fLanguage.TranslateForm(fSettings);
  fLanguage.TranslateForm(fSelectFont);
  fLanguage.TranslateForm(fNewCategory);
  fLanguage.TranslateForm(fPrint);
  fLanguage.TranslateForm(fStatistics);
  fLanguage.TranslateForm(fWordList);
  fLanguage.TranslateForm(fBitmap);
  fLanguage.TranslateForm(fUserAdd);
  fLanguage.TranslateForm(fUserCategory);
  fLanguage.TranslateForm(fUserFilters);
  fLanguage.TranslateForm(fUserDetails);
  fLanguage.TranslateForm(fKanjiDetails);
  fLanguage.TranslateForm(fKanjiSort);
  fLanguage.TranslateForm(fKanjiSearch);
  fLanguage.TranslateForm(fKanjiCompounds);
  fLanguage.TranslateForm(fWordDetails);
  fLanguage.TranslateForm(fExamples);
  fLanguage.TranslateForm(fWordCategory);
  fLanguage.TranslateForm(fWordKanji);
  fLanguage.TranslateForm(fTranslate);
//  fLanguage.TranslateForm(fClipboard);
  fLanguage.TranslateForm(fLayout);
  fLanguage.TranslateForm(fMedia);
  fLanguage.TranslateForm(fStrokeOrder);
  fLanguage.TranslateForm(fDictMan);
  fLanguage.TranslateForm(fDictImport);
  fLanguage.TranslateForm(fDictCoding);
  fLanguage.TranslateForm(fCharItem);
  fLanguage.TranslateForm(fDicAdd);
  fLanguage.TranslateForm(fFileType);
  fLanguage.TranslateForm(fWordsExpChoose);
end;

procedure TfMenu.RefreshCategory;
var b:boolean;
    lc:char;
    s:string;
begin
  fDicAdd.ComboBox1.Items.Clear;
  fUserAdd.ComboBox1.Items.Clear;
  fUserDetails.ComboBox2.Items.Clear;
  fUserFilters.TabSet1Change(fMenu,fUserFilters.TabSet1.TabIndex,b);
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    s:=TUserCat.Str(TUserCatName);
    lc:='j';
    if (length(s)>1) and (s[2]='~') then lc:=s[1] else TUserCat.Edit([TUserCatName],['j~'+s]);
    s:=StripCatName(s);
    if lc=curlang then
    begin
      fDicAdd.ComboBox1.Items.Add(s);
      fUserAdd.ComboBox1.Items.Add(s);
      fUserDetails.ComboBox2.Items.Add(s);
    end;
    TUserCat.Next;
  end;
  if fDicAdd.ComboBox1.Items.Count>0 then fDicAdd.ComboBox1.Text:=fDicAdd.ComboBox1.Items[0];
  if fUserAdd.ComboBox1.Items.Count>0 then fUserAdd.ComboBox1.Text:=fUserAdd.ComboBox1.Items[0];
end;

procedure TfMenu.RefreshKanjiCategory;
var b:boolean;
    lc:char;
    s:string;
begin
  fKanjiDetails.ComboBox1.Items.Clear;
  fKanjiSearch.ListBox1.Items.Clear;
  kanjicatuniqs.Clear;
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    s:=TUserCat.Str(TUserCatName);
    lc:='j';
    if (length(s)>1) and (s[2]='~') then lc:=s[1] else TUserCat.Edit([TUserCatName],['j~'+s]);
    s:=StripCatName(s);
    if lc='k'then
    begin
      fKanjiDetails.ComboBox1.Items.Add(s);
      fKanjiSearch.ListBox1.Items.Add(s);
      kanjicatuniqs.Add(TUserCat.Str(TUserCatIndex));
    end;
    TUserCat.Next;
  end;
  if fKanjiDetails.ComboBox1.Items.Count=0 then showmessage('Internal error: No category!');
  fKanjiDetails.ComboBox1.Text:=fKanjiDetails.ComboBox1.Items[0];
  fKanjiDetails.ComboBox1.ItemIndex:=0;
  fKanjiSearch.ListBox1.ItemIndex:=0;
  fKanjiSearch.SpeedButton25.Enabled:=strtoint(kanjicatuniqs[fKanjiSearch.ListBox1.ItemIndex])<>KnownLearned;
  fKanjiSearch.SpeedButton20.Enabled:=strtoint(kanjicatuniqs[fKanjiSearch.ListBox1.ItemIndex])<>KnownLearned;
end;

function GetFormLayout(form:TForm):string;
var s:string;
begin
  if form.visible then s:='VISIBLE'else if form=fKanjiDetails then s:='PARENT'else s:='HIDDEN';
  if form.BorderStyle=bsNone then s:=s+',N'else s:=s+',Y';
  if (form=fMenu) or (form=fKanji) or (form=fUser) or (form=fWords) or (form=fKanjiDetails) or (form=fStrokeOrder) or (form=fTranslate) then
    s:=s+',ABSOLUTE,'else s:=s+',LEAVE,';
  s:=s+inttostr(form.left)+','+inttostr(form.top)+','+inttostr(form.width)+','+inttostr(form.height);
  result:=s;
end;

procedure SetFormLayout(form:TForm;s:string;fullborder:TFormBorderStyle;var screenorig:TPoint; var screendim:TPoint);
var params:array[1..10] of string;
    i:integer;
    v:integer;
    vf,vt:integer;
    sf,sfw:boolean;
begin
  i:=1;
  while pos(',',s)>0 do
  begin
    params[i]:=uppercase(copy(s,1,pos(',',s)-1));
    delete(s,1,pos(',',s));
    inc(i);
  end;
  params[i]:=uppercase(s);
  if (params[1]='VISIBLE') or (params[1]='PARENT') then form.tag:=1 else form.tag:=0;
//  if params[2]='N'then if form<>fMenu then form.borderStyle:=bsNone else form.borderStyle:=fullborder;
//  if (form=fMenu) then if params[2]='N'then form.tag:=1 else form.tag:=0;
  if uppercase(params[3])='LEAVE'then
  begin end else
  if uppercase(params[3])='ABSOLUTE'then
  begin
    form.left:=strtoint(params[4]);
    form.top:=strtoint(params[5]);
    sf:=((form.Constraints.minHeight=form.Constraints.MaxHeight) and
      (form.Constraints.minHeight>0)) or (fullborder=bsToolWindow);
    sfw:=((form.Constraints.minWidth=form.Constraints.MaxWidth) and
      (form.Constraints.minWidth>0)) or (fullborder=bsToolWindow);
    if not sfw then form.width:=strtoint(params[6]);
    if not sf then form.height:=strtoint(params[7]);
  end else if uppercase(params[3])='CENTER'then
  begin
    form.width:=strtoint(params[4]);
    form.height:=strtoint(params[5]);
    form.left:=screendim.x div 2-form.width div 2;
    form.top:=screendim.y div 2-form.height div 2;
  end else
  begin
    if (params[4]='%') and ((params[3]='TOP') or (params[3]='BOTTOM')) then v:=screendim.y*strtoint(params[5]) div 100 else
    if (params[4]='%') and ((params[3]='LEFT') or (params[3]='RIGHT')) then v:=screendim.x*strtoint(params[5]) div 100 else
    if (params[4]='.') and ((params[3]='TOP') or (params[3]='BOTTOM')) then v:=form.height else
    if (params[4]='.') and ((params[3]='LEFT') or (params[3]='RIGHT')) then v:=form.width else
      v:=strtoint(params[5]);
    if (params[6]='%') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vf:=screendim.x*strtoint(params[7]) div 100 else
    if (params[6]='%') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vf:=screendim.y*strtoint(params[7]) div 100 else
      vf:=strtoint(params[7]);
    if (params[8]='%') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vt:=screendim.x*strtoint(params[9]) div 100 else
    if (params[8]='%') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vt:=screendim.y*strtoint(params[9]) div 100 else
    if (params[8]='.') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vt:=vf+form.width else
    if (params[8]='.') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vt:=vf+form.height else
    if (params[8]='R') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vt:=screendim.x else
    if (params[8]='R') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vt:=screendim.y else
      vt:=strtoint(params[9]);
    if (params[3]='TOP') or (params[3]='BOTTOM') then form.height:=v else form.width:=v;
    if params[3]='TOP'then
    begin
      form.top:=screenorig.y;
      if params[10]='SET'then
      begin
        screenorig.y:=screenorig.y+form.height;
        screendim.y:=screendim.y-form.height;
      end;
      form.left:=vf+screenorig.x; form.width:=vt-vf;
    end;
    if params[3]='BOTTOM'then
    begin
      form.top:=screenorig.y+screendim.y-form.height;
      if params[10]='SET'then screendim.y:=screendim.y-form.height;
      form.left:=vf+screenorig.x; form.width:=vt-vf;
    end;
    if params[3]='LEFT'then
    begin
      form.left:=screenorig.x;
      if params[10]='SET'then
      begin
        screenorig.x:=screenorig.x+form.width;
        screendim.x:=screendim.x-form.width;
      end;
      form.top:=vf+screenorig.y; form.height:=vt-vf;
    end;
    if params[3]='RIGHT'then
    begin
      form.left:=screenorig.x+screendim.x-form.width;
      if params[10]='SET'then screendim.x:=screendim.x-form.width;
      form.top:=vf+screenorig.y; form.height:=vt-vf;
    end;
  end;
end;

procedure TfMenu.ReadLayout(filename:string);
var t:textfile;
    s:string;
    so,sd:TPoint;
    v2:boolean;
begin
  assignfile(t,filename);
  reset(t);
  readln(t,s);
  v2:=false;
  if s<>'VERSION,6'then
  begin
    Application.MessageBox(pchar(_l('#00334^eCannot load layout. Outdated version.'#13#13'Settings standard layout instead.^cNemohu nahr·t rozvrûenÌ. Zastaral· verze.'#13#13'Nastavuji mÌsto toho standardnÌ rozvrûenÌ.')),pchar(_l('#00335^eLayout loading^cNahr·v·nÌ rozvrûenÌ')),MB_ICONINFORMATION or MB_OK);
    closefile(t);
    StandardLayout(0,100);
    exit;
  end;
  readln(t,s);
  fUser.Hide;
  fKanji.Hide;
  fWords.Hide;
//  fClipboard.Hide;
  SetFormLayout(fMenu,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fKanji,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fWords,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fUser,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fExamples,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fUserCategory,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fUserFilters,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fUserDetails,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fKanjiDetails,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fKanjiSort,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fKanjiSearch,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fKanjiCompounds,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fWordDetails,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fExamples,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fWordCategory,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fWordKanji,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fTranslate,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fClipboard,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fStrokeOrder,s,bsToolWindow,so,sd); readln(t,s);
  if StrokeOrderPackage=nil then fStrokeOrder.Tag:=0;
  if fKanji.tag=1 then fKanji.Show;
  if fUser.tag=1 then fUser.Show;
  if fWords.tag=1 then fWords.Show;
  if fKanjiDetails.tag=1 then fKanjiDetails.Show;
  if fTranslate.tag=1 then fTranslate.Show;
//  if fClipboard.tag=1 then fClipboard.Show;
  closefile(t);
end;

procedure TfMenu.WriteLayout(filename:string);
var t:textfile;
    s:string;
    i:integer;
begin
  assignfile(t,filename);
  rewrite(t);
  writeln(t,'VERSION,7');
  s:=GetFormLayout(fMenu); writeln(t,s);
  s:=GetFormLayout(fKanji); writeln(t,s);
  s:=GetFormLayout(fWords); writeln(t,s);
  s:=GetFormLayout(fUser); writeln(t,s);
  s:=GetFormLayout(fExamples); writeln(t,s);
//  s:=GetFormLayout(fUserCategory); writeln(t,s);
  s:=GetFormLayout(fUserFilters); writeln(t,s);
  s:=GetFormLayout(fUserDetails); writeln(t,s);
  s:=GetFormLayout(fKanjiDetails); writeln(t,s);
//  s:=GetFormLayout(fKanjiSort); writeln(t,s);
  s:=GetFormLayout(fKanjiSearch); writeln(t,s);
  s:=GetFormLayout(fKanjiCompounds); writeln(t,s);
//  s:=GetFormLayout(fWordDetails); writeln(t,s);
  s:=GetFormLayout(fExamples); writeln(t,s);
//  s:=GetFormLayout(fWordCategory); writeln(t,s);
  s:=GetFormLayout(fWordKanji); writeln(t,s);
  s:=GetFormLayout(fTranslate); writeln(t,s);
  s:=GetFormLayout(fStrokeOrder); writeln(t,s);
  if not aBorders.Checked then writeln(t,'HIDEBORDERS') else writeln(t,'SHOWBORDERS');
  for i:=0 to chardetl.Count-1 do writeln(t,chardetl[i]);
  closefile(t);
end;

procedure TfMenu.StandardLayout(lay:integer;perc:integer);
var so,sd,so1,sd1:TPoint;
procedure sfl(form:TForm;s:string);
begin
  if (form=fUserFilters)
    or (form=fKanjiSort) or (form=fWordCategory) or (form=fWordKanji) or (form=fUserDetails) or (form=fKanjiCompounds) then
    SetFormLayout(form,s,bsToolWindow,so,sd) else
    SetFormLayout(form,s,bsSizeToolWin,so,sd);
end;
procedure res;
begin
  so.x:=0; so.y:=0;
  sd.x:=Screen.Width;
  sd.y:=(Screen.height-24)*perc div 100;
end;
procedure StdVocab;
begin
  sd:=sd1; so:=so1;
  sfl(fUserFilters,'VISIBLE,Y,RIGHT,.,0,=,0,.,0,LEAVE');
  sfl(fWords,'HIDDEN,Y,TOP,=,'+inttostr(fUserFilters.height)+',=,0,=,'+inttostr(sd.x-fUserFilters.width)+',SET');
  sfl(fUserDetails,'VISIBLE,Y,TOP,.,0,=,0,.,0,LEAVE');
//  sfl(fUserCategory,'VISIBLE,Y,TOP,=,'+inttostr(fUserDetails.height)+',=,'+inttostr(fUserDetails.width)+',r,0,SET');
  sfl(fUserAdd,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
end;
var sx:string;
begin
  if ((Screen.Width<1024) or (Screen.Height<768)) and (lay>0) then
  begin
    Application.MessageBox(pchar(_l('#00336^eAdvanced layouts require at least 1024x768 resolution.^cPokroËil· rozvrûenÌ vyûadujÌ rozliöenÌ alespoÚ 1024x768.')),pchar(_l('#00337^eResolution too low^cP¯Ìliö nÌzkÈ rozliöenÌ')),MB_ICONERROR or MB_OK);
    exit;
  end;
  fUser.Hide;
  fKanji.Hide;
  fWords.Hide;
  fTranslate.Hide;
  fKanjiDetails.Hide;
  DockExpress(nil,false);
//  fClipboard.Hide;
  if (lay>0) and (aBorders.Checked) then
  begin
    aBorders.Checked:=false;
    SpeedButton9Click(self);
  end;
  if (lay=0) and (not aBorders.Checked) then
  begin
    aBorders.Checked:=true;
    SpeedButton9Click(self);
  end;
  if fMenu.BorderStyle=bsNone then sx:='40'else sx:='74';
  res;
  sfl(fMenu,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
//  sfl(fClipboard,'VISIBLE,Y,TOP,=,'+inttostr(fMenu.Height)+',=,'+inttostr(fMenu.width)+',r,0,SET');
  so1:=so; sd1:=sd;
{  case lay of
    0:begin
        sfl(fKanjiDetails,'VISIBLE,Y,RIGHT,%,100,=,0,r,0,SET');
        sfl(fKanji,'VISIBLE,Y,TOP,%,100,=,0,r,0,SET');
        fKanjiCompounds.Tag:=0;
        fKanjiSearch.Tag:=1;
        sd:=sd1; so:=so1;
        sfl(fUser,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        fWordKanji.Tag:=1;
        fExamples.Tag:=0;
        sfl(fTranslate,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        sfl(fWords,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        fUserFilters.Tag:=1;
        fUserAdd.Tag:=0;
        fUserDetails.Tag:=0;
      end;
    1:begin
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,BOTTOM,.,0,=,'+inttostr(sd.x-fWordCategory.Width)+',r,0,LEAVE');
        fKanjiDetails.Height:=350;
        sfl(fUser,'VISIBLE,Y,BOTTOM,=,180,=,0,=,'+inttostr(sd.x-fWordKanji.Width)+',LEAVE');
        sfl(fWordKanji,'VISIBLE,Y,BOTTOM,=,180,=,'+inttostr(sd.x-fWordKanji.Width)+',.,0,SET');
        sfl(fExamples,'HIDDEN,Y,BOTTOM,.,0,=,0,r,0,LEAVE');
        sfl(fTranslate,'VISIBLE,Y,BOTTOM,=,'+inttostr(sd.y-fKanjiDetails.Height)+',=,0,r,0,SET');
        sfl(fStrokeOrder,'HIDDEN,Y,BOTTOM,.,0,=,0,.,0,LEAVE');
        sfl(fKanjiDetails,'VISIBLE,Y,RIGHT,.,0,=,0,.,0,SET');
        sfl(fKanji,'VISIBLE,Y,TOP,=,170,=,0,r,0,SET');
        sfl(fKanjiSearch,'HIDDEN,Y,LEFT,.,0,=,0,.,0,LEAVE');
//        sfl(fKanjiSort,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,%,100,=,0,r,0,SET');
        StdVocab;
      end;
    2:begin
        fKanjiDetails.Height:=350;
        sfl(fKanji,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-fKanjiDetails.height)+',=,0,r,0,SET');
        sfl(fKanjiDetails,'VISIBLE,Y,LEFT,.,0,=,0,.,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-fKanjiSearch.height)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'VISIBLE,Y,LEFT,%,100,=,0,.,0,SET');
//        sfl(fKanjiSort,'VISIBLE,Y,LEFT,.,0,=,0,.,0,SET');
        sd:=sd1; so:=so1;
//        sfl(fWordDetails,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
        sfl(fUser,'HIDDEN,Y,TOP,=,'+inttostr(sd.y-180-fExamples.Height)+',=,0,r,0,SET');
        sfl(fExamples,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
//        sfl(fWordCategory,'HIDDEN,Y,BOTTOM,.,0,=,'+inttostr(sd.x-fWordCategory.Width)+',r,0,LEAVE');
        sfl(fWordKanji,'VISIBLE,Y,RIGHT,.,0,=,0,r,0,SET');
        sfl(fTranslate,'HIDDEN,Y,BOTTOM,%,100,=,0,r,0,LEAVE');
        StdVocab;
      end;
    3:begin
        fKanjiDetails.Height:=300;
        sfl(fTranslate,'HIDDEN,Y,BOTTOM,%,100,=,0,r,0,LEAVE');
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fExamples,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
        sfl(fWordKanji,'VISIBLE,Y,RIGHT,.,0,=,0,=,180,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fUser,'VISIBLE,Y,TOP,=,180,=,0,=,'+inttostr(sd.x-fWordKanji.width)+',SET');
        sfl(fKanji,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-300)+',=,0,r,0,SET');
        sfl(fKanjiDetails,'VISIBLE,Y,LEFT,.,0,=,0,.,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-fKanjiSearch.height)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'VISIBLE,Y,LEFT,%,100,=,0,.,0,SET');
//        sfl(fKanjiSort,'VISIBLE,Y,LEFT,.,0,=,0,.,0,SET');
        StdVocab;
      end;
    4:begin
        fKanjiDetails.Height:=350;
        sfl(fKanjiDetails,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,SET');
        sfl(fKanji,'HIDDEN,Y,TOP,=,190,=,0,r,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,=,'+inttostr(fKanjiDetails.height-190)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'HIDDEN,Y,LEFT,.,0,=,0,r,0,LEAVE');
//        sfl(fKanjiSort,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sd:=sd1; so:=so1;
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fWordKanji,'VISIBLE,Y,RIGHT,.,0,=,0,=,180,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fUser,'VISIBLE,Y,TOP,=,180,=,0,=,'+inttostr(sd.x-fWordKanji.width)+',SET');
        sfl(fExamples,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fTranslate,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        StdVocab;
      end;
    5:begin
        sfl(fKanjiDetails,'VISIBLE,Y,RIGHT,.,0,=,0,=,300,LEAVE');
        sfl(fKanji,'HIDDEN,Y,BOTTOM,=,190,=,0,r,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,BOTTOM,=,'+inttostr(fKanjiDetails.height-190)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'HIDDEN,Y,LEFT,.,0,=,0,.,0,LEAVE');
//        sfl(fKanjiSort,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sd:=sd1; so:=so1;
        sfl(fWordKanji,'HIDDEN,Y,RIGHT,.,0,=,0,=,180,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fUser,'VISIBLE,Y,TOP,=,300,=,0,=,'+inttostr(sd.x-fKanjiDetails.width)+',SET');
        sfl(fExamples,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fTranslate,'VISIBLE,Y,TOP,%,100,=,0,r,0,LEAVE');
        StdVocab;
      end;
  end;
}  if fKanji.tag=1 then fKanji.Show;
  if fUser.tag=1 then fUser.Show;
  if fWords.tag=1 then fWords.Show;
  if fTranslate.tag=1 then fTranslate.Show;
  if fKanjiDetails.tag=1 then fKanjiDetails.Show;
{  if lay=0 then SpeedButton16.Down:=true;
  if lay=1 then SpeedButton18.Down:=true;
  if lay=2 then SpeedButton17.Down:=true;
  if lay=3 then SpeedButton21.Down:=true;
  if lay=4 then SpeedButton19.Down:=true;
  if lay=5 then SpeedButton20.Down:=true;
  if lay=5 then aEditorWindow.Execute;
  if lay<3 then aKanjiWindow.Execute;
  if (lay=3) or (lay=4) then aDictJapanese.Execute;}
  aQuick1.Checked:=lay=0;
  aQuick2.Checked:=lay=1;
  aQuick3.Checked:=lay=2;
  aQuick4.Checked:=lay=3;
  aQuick5.Checked:=lay=4;
  aQuick6.Checked:=lay=5;
  curqlayout:=lay;
  fKanjiCompounds.FormResize(fMenu);
  fWords.FormResize(fMenu);
  fUser.FormResize(fMenu);
  fKanji.ManualDock(Panel3);
  fKanji.Align:=alClient;
//  if fClipboard.tag=1 then fClipboard.Show;
end;

procedure TfMenu.WriteUserPackage;
var f:file of byte;
    b:byte;
begin
  assignfile(f,'user\struct.ver');
  rewrite(f);
  b:=CurStructVer;
  write(f,b);
  closefile(f);
  PKGWriteForm.PKGWriteCmd('NotShow');
  PKGWriteForm.PKGWriteCmd('PKGFileName wakan.usr');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan User Data');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan User Data File');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName (C) Filip K·brt 2002');
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by WaKan - Japanese & Chinese Learning Tool');
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 621030');
  PKGWriteForm.PKGWriteCmd('FileSysCode 587135');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  PKGWriteForm.PKGWriteCmd('CryptMode 0');
  PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode 0');
  PKGWriteForm.PKGWriteCmd('CryptCode 978312');
  PKGWriteForm.PKGWriteCmd('Include user');
  PKGWriteForm.PKGWriteCmd('Finish');
end;

procedure TfMenu.ChangeUserData;
begin
  UserDataChanged:=true;
//  SpeedButton2.Enabled:=true;
//  SpeedButton7.Enabled:=true;
  aSaveUser.Enabled:=true;
  aCancelUser.Enabled:=true;
end;

procedure TfMenu.SaveUserData;
var un,i:integer;
begin
  if UserDataChanged then
  begin
    CopyFile('wakan.usr','wakan.bak',false);
    RefreshKanjiCategory;
    Screen.Cursor:=crHourGlass;
    {$I-}
    mkdir('user');
    {$I+}
    ioresult;
    for i:=0 to kanjicatuniqs.Count-1 do
    begin
      un:=strtoint(kanjicatuniqs[i]);
      if un=KnownLearned then SaveKnownList(un,'user\knownchar.bin')
        else SaveKnownList(un,'user\char'+inttostr(un)+'.bin');
    end;
    TUser.WriteTable('user\User',false);
    TUserIdx.WriteTable('user\UserIdx',false);
    TUserSheet.WriteTable('user\UserSheet',false);
    TUserCat.WriteTable('user\UserCat',false);
    TUserPrior.WriteTable('user\UserPrior',false);
    WriteUserPackage;
    DeleteFile('user\knownchar.bin');
    DeleteFile('user\User.info');
    DeleteFile('user\UserIdx.info');
    DeleteFile('user\UserSheet.info');
    DeleteFile('user\UserCat.info');
    DeleteFile('user\UserPrior.info');
    DeleteFile('user\User.data');
    DeleteFile('user\UserIdx.data');
    DeleteFile('user\UserSheet.data');
    DeleteFile('user\UserCat.data');
    DeleteFile('user\UserPrior.data');
    DeleteFile('user\User.struct');
    DeleteFile('user\UserIdx.struct');
    DeleteFile('user\UserSheet.struct');
    DeleteFile('user\UserCat.struct');
    DeleteFile('user\UserPrior.struct');
    DeleteFile('user\User.index');
    DeleteFile('user\UserIdx.index');
    DeleteFile('user\UserSheet.index');
    DeleteFile('user\UserCat.index');
    DeleteFile('user\UserPrior.index');
    DeleteFile('user\struct.ver');
    {$I-}
    rmdir('user');
    {$I+}
    ioresult;
    {$I-}
    mkdir('backup');
    {$I+}
    ioresult;
    CopyFile('wakan.usr',pchar('backup\'+FormatDateTime('yyyymmdd',now)+'.usr'),false);
    Screen.Cursor:=crDefault;
    UserDataChanged:=false;
//    SpeedButton2.Enabled:=false;
//    SpeedButton7.Enabled:=false;
    aSaveUser.Enabled:=false;
    aCancelUser.Enabled:=false;
  end;
end;

procedure TfMenu.LoadUserData;
var t:textfile;
    ps:TPackageSource;
    ms:TMemoryStream;
    ver:integer;
    lcat:boolean;
    ux:integer;
begin
  UserDataChanged:=false;
//  SpeedButton2.Enabled:=false;
//  SpeedButton7.Enabled:=false;
  aSaveUser.Enabled:=false;
  aCancelUser.Enabled:=false;
  Screen.Cursor:=crHourGlass;
  if userdataloaded then
  begin
    TUser.Free;
    TUserIdx.Free;
    TUserSheet.Free;
    TUserCat.Free;
    userdataloaded:=false;
  end;
  if FileExists('wakan.usr') then
  begin
    ps:=TPackageSource.Create('wakan.usr',621030,587135,978312);
    ver:=0;
    TUser:=TTextTable.Create(ps,'User',false,false);
    TUserIdx:=TTextTable.Create(ps,'UserIdx',false,false);
    TUserSheet:=TTextTable.Create(ps,'UserSheet',false,false);
    TUserCat:=TTextTable.Create(ps,'UserCat',false,false);
    if ps['UserPrior.info']<>nil then
      TUserPrior:=TTextTable.Create(ps,'UserPrior',false,false) else
    begin
      assignfile(t,'UserPrior.info');
      rewrite(t);
      writeln(t,'$TEXTTABLE');
      writeln(t,'$PREBUFFER');
      writeln(t,'$FIELDS');
      writeln(t,'xKanji');
      writeln(t,'wCount');
      writeln(t,'$ORDERS');
      writeln(t,'Kanji_Ind');
      writeln(t,'Count_Ind');
      writeln(t,'$SEEKS');
      writeln(t,'0');
      writeln(t,'Kanji');
      writeln(t,'Count');
      writeln(t,'$CREATE');
      closefile(t);
      TUserPrior:=TTextTable.Create(nil,'UserPrior',false,false);
      DeleteFile('UserPrior.info');
    end;
    TCharIndex:=TChar.Field('Index');
    TCharChinese:=TChar.Field('Chinese');
    TCharType:=TChar.Field('Type');
    TCharUnicode:=TChar.Field('Unicode');
    TCharStrokeCount:=TChar.Field('StrokeCount');
    TCharJpStrokeCount:=TChar.Field('JpStrokeCount');
    TCharJpFrequency:=TChar.Field('JpFrequency');
    TCharChFrequency:=TChar.Field('ChFrequency');
    TCharJouyouGrade:=TChar.Field('JouyouGrade');
    TCharReadKanji:=TCharRead.Field('Kanji');
    TCharReadType:=TCharRead.Field('Type');
    TCharReadReading:=TCharRead.Field('Reading');
    TCharReadIndex:=TCharRead.Field('Index');
    TCharReadReadDot:=TCharRead.Field('ReadDot');
    TCharReadPosition:=TCharRead.Field('Position');
    TRadicalsNumber:=TRadicals.Field('Number');
    TRadicalsVariant:=TRadicals.Field('Variant');
    TRadicalsUnicode:=TRadicals.Field('Unicode');
    TRadicalsStrokeCount:=TRadicals.Field('StrokeCount');
    TRadicalsUnicodeCount:=TRadicals.Field('UnicodeCount');
    TRadicalsBushuCount:=TRadicals.Field('BushuCount');
    TRadicalsJapaneseCount:=TRadicals.Field('JapaneseCount');
    TRadicalsKangXiCount:=TRadicals.Field('KangXiCount');
    TUserIndex:=TUser.Field('Index');
    TUserEnglish:=TUser.Field('English');
    TUserPhonetic:=TUser.Field('Phonetic');
    TUserPhoneticSort:=TUser.Field('PhoneticSort');
    TUserKanji:=TUser.Field('Kanji');
    TUserAdded:=TUser.Field('Added');
    TUserPrinted:=TUser.Field('Printed');
    TUserLearned:=TUser.Field('Learned');
    TUserMastered:=TUser.Field('Mastered');
    TUserNoPrinted:=TUser.Field('NoPrinted');
    TUserScore:=TUser.Field('Score');
    TUserMaxScore:=TUser.Field('MaxScore');
    TUserIdxWord:=TUserIdx.Field('Word');
    TUserIdxKanji:=TUserIdx.Field('Kanji');
    TUserIdxBegin:=TUserIdx.Field('Begin');
    TUserIdxIndex:=TUserIdx.Field('Index');
    TUserSheetWord:=TUserSheet.Field('Word');
    TUserSheetNumber:=TUserSheet.Field('Number');
    TUserSheetPos:=TUserSheet.Field('Pos');
    TUserCatIndex:=TUserCat.Field('Index');
    TUserCatName:=TUserCat.Field('Name');
    TUserCatType:=TUserCat.Field('Type');
    TUserCatCreated:=TUserCat.Field('Created');
    TUserCat.First;
    lcat:=false;
    ux:=0;
    while not TUserCat.EOF do
    begin
      ux:=strtoint(TUserCat.Str(TUserCat.Field('Index')));
      if TUserCat.Int(TUserCat.Field('Type'))=ord('Q') then
      begin
        lcat:=true;
        ms:=ps['knownchar.bin'].Lock;
        KnownLearned:=ux;
        CreateKnownList(ux,0);
        LoadKnownList(ux,ms);
        ps['knownchar.bin'].Unlock;
      end;
      if TUserCat.Int(TUserCat.Field('Type'))=ord('K') then
      begin
        ms:=ps['char'+inttostr(ux)+'.bin'].Lock;
        CreateKnownList(ux,0);
        LoadKnownList(ux,ms);
        ps['char'+inttostr(ux)+'.bin'].Unlock;
      end;
      TUserCat.Next;
    end;
    if not lcat then
    begin
      TUserCat.Insert([inttostr(ux+1),'k~'+_l('LEARNED'),inttostr(ord('Q')),FormatDateTime('yyyymmdd',now)]);
      ms:=ps['knownchar.bin'].Lock;
      CreateKnownList(ux+1,0);
      KnownLearned:=ux+1;
      LoadKnownList(ux+1,ms);
      ps['knownchar.bin'].Unlock;
    end;
    try
      ms:=ps['struct.ver'].Lock;
      ms.Read(ver,1);
      ps['struct.ver'].UnLock;
    except end;
    ps.Free;
    if ver<>CurStructVer then
    begin
      if Application.MessageBox(pchar(_l('#00338^eFile WAKAN.USR has old structure.'#13'It must be converted.'#13'You should make backup before converting'#13#13'Do you want to convert it now?'+
      '^cSoubor WAKAN.USR m· starou strukturu.'#13'Bude muset b˝t zkonvertov·n.'#13'P¯ed konverzÌ byste mÏli udÏlat z·lohu.'#13#13'Chcete provÈst konverzi teÔ?')),
      pchar(_l('#00339^eOld structure^cStar· struktura')),MB_YESNO or MB_ICONWARNING)=idYes then
      begin
        if ver<=0 then
        begin
          ExportUserData('wakan_temp.jbk');
          ImportUserData('wakan_temp.jbk');
          DeleteFile('wakan_temp.jbk');
        end;
        if ver<=1 then
        begin
          RebuildUserIndex;
          ChangeUserData;
          SaveUserData;
        end;
      end else begin
        Application.Terminate;
        exit;
      end;
    end;
    if not TUser.CheckIndex then TUser.Reindex;
    if not TUserIdx.CheckIndex then TUserIdx.Reindex;
    if not TUserSheet.CheckIndex then TUserSheet.Reindex;
    if not TUserCat.CheckIndex then TUserCat.Reindex;
  end else
  begin
    {$I-}
    mkdir('user');
    {$I+}
    ioresult;
    CreateKnownList(1,0);
    SaveKnownList(1,'user\knownchar.bin');
    assignfile(t,'user\User.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iIndex');
    writeln(t,'sEnglish');
    writeln(t,'xPhonetic');
    writeln(t,'sPhoneticSort');
    writeln(t,'xKanji');
    writeln(t,'sAdded');
    writeln(t,'sPrinted');
    writeln(t,'sLearned');
    writeln(t,'sMastered');
    writeln(t,'iNoPrinted');
    writeln(t,'bScore');
    writeln(t,'bMaxScore');
    writeln(t,'$ORDERS');
    writeln(t,'Index_Ind');
    writeln(t,'Kanji_Ind');
    writeln(t,'Phonetic_Ind');
    writeln(t,'PhoneticSeek_Ind');
    writeln(t,'English_Ind');
    writeln(t,'Added_Ind');
    writeln(t,'Printed_Ind');
    writeln(t,'Score_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Index');
    writeln(t,'Kanji+PhoneticSort');
    writeln(t,'PhoneticSort+Phonetic');
    writeln(t,'Phonetic');
    writeln(t,'English');
    writeln(t,'Added+PhoneticSort');
    writeln(t,'Printed+PhoneticSort');
    writeln(t,'Score+PhoneticSort');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,'user\UserIdx.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iWord');
    writeln(t,'xKanji');
    writeln(t,'lBegin');
    writeln(t,'$ORDERS');
    writeln(t,'Kanji_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Kanji');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,'user\UserSheet.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iWord');
    writeln(t,'wNumber');
    writeln(t,'wPos');
    writeln(t,'$ORDERS');
    writeln(t,'Word_Ind');
    writeln(t,'Sheet_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Word+Number+Pos');
    writeln(t,'Number+Pos');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,'user\UserCat.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iIndex');
    writeln(t,'sName');
    writeln(t,'bType');
    writeln(t,'sCreated');
    writeln(t,'$ORDERS');
    writeln(t,'Index_Ind');
    writeln(t,'Type_Ind');
    writeln(t,'Name_Ind');
    writeln(t,'Created_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Index');
    writeln(t,'Type+Name');
    writeln(t,'Name');
    writeln(t,'Created+Name');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,'user\UserPrior.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'xKanji');
    writeln(t,'wCount');
    writeln(t,'$ORDERS');
    writeln(t,'Kanji_Ind');
    writeln(t,'Count_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Kanji');
    writeln(t,'Count');
    writeln(t,'$CREATE');
    closefile(t);
    WriteUserPackage;
    DeleteFile('user\knownchar.bin');
    DeleteFile('user\User.info');
    DeleteFile('user\UserIdx.info');
    DeleteFile('user\UserSheet.info');
    DeleteFile('user\UserCat.info');
    DeleteFile('user\UserPrior.info');
    DeleteFile('user\struct.ver');
    {$I-}
    rmdir('user');
    {$I+}
    ioresult;
    LoadUserData;
  end;
  Screen.Cursor:=crDefault;
  userdataloaded:=true;
end;

procedure TfMenu.ExportUserData(filename:string);
var t:textfile;
    i:integer;
    w:widechar;
begin
  if not FlushUserData then exit;
  assignfile(t,filename);
  rewrite(t);
  writeln(t,'$User');
  TUser.ExportToText(t,'Index_Ind');
  writeln(t,'$UserIdx');
  TUserIdx.ExportToText(t,'Kanji_Ind');
  writeln(t,'$UserCat');
  TUserCat.ExportToText(t,'Index_Ind');
  writeln(t,'$UserSheet');
  TUserSheet.ExportToText(t,'Sheet_Ind');
  writeln(t,'$KnownKanji');
  writeln(t,'>Unicode');
  for i:=1 to 65536 do
  begin
    w:=widechar(i);
    if IsKnown(KnownLearned,{$IFDEF UNICODE}w{$ELSE}UnicodeToHex(w){$ENDIF}) then
      writeln(t,'+'+UnicodeToHex(w));
  end;
  writeln(t,'.');
  closefile(t);
end;

procedure TfMenu.ImportUserData(filename:string);
var t:textfile;
    s:string;
begin
  DeleteFile('wakan.usr');
  LoadUserData;
  Screen.Cursor:=crHourGlass;
  assignfile(t,filename);
  reset(t);
  while not eof(t) do
  begin
    readln(t,s);
    if s[1]='$'then
    begin
      if s='$User'then TUser.ImportFromText(t,nil,'');
      if s='$UserIdx'then TUserIdx.ImportFromText(t,nil,'');
      if s='$UserCat'then TUserCat.ImportFromText(t,nil,'');
      if s='$UserSheet'then TUserSheet.ImportFromText(t,nil,'');
      if s='$KnownKanji'then
      begin
        readln(t,s);
        readln(t,s);
        while s[1]<>'.'do
        begin
          delete(s,1,1);
          SetKnown(KnownLearned,s,true);
          readln(t,s);
        end;
      end;
    end;
  end;
  closefile(t);
  ChangeUserData;
  SaveUserData;
  MaxUserIndex:=0;
  while not TUser.EOF do
  begin
    if TUser.Int(TUserIndex)>MaxUserIndex then MaxUserIndex:=TUser.Int(TUserIndex);
    TUser.Next;
  end;
  MaxCategoryIndex:=0;
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    if TUserCat.Int(TUserCatIndex)>MaxCategoryIndex then MaxCategoryIndex:=TUserCat.Int(TUserCatIndex);
    TUserCat.Next;
  end;
  RefreshCategory;
  Screen.Cursor:=crDefault;
end;

function TfMenu.FlushUserData:boolean;
var res:integer;
begin
  result:=true;
  if UserDataChanged then
  begin
    if fSettings.CheckBox46.Checked then SaveUserData else
    begin
    res:=Application.MessageBox(
      pchar(_l('#00340^eUser data was changed. Do you want to save it?^cUûivatelsk· data byla zmÏnÏna. Chcete je uloûit?')),
      pchar(_l('#00341^eApplication exit^cUkonËenÌ aplikace')),MB_YESNOCANCEL or MB_ICONQUESTION);
    case res of
      idYes:SaveUserData;
      idNo:LoadUserData;
      idCancel:result:=false;
    end;
    end;
  end;
end;

procedure TfMenu.SetFormPos(form:TForm);
var maxh:integer;
begin
{  form.Left:=0;
  form.Width:=Width;
  form.WindowState:=wsNormal;
  form.Height:=globheight-48;
  form.Top:=40;
  form.Borderstyle:=bsNone;}
end;

function checkfont(s:string):string;
begin
  if Screen.Fonts.IndexOf(s)=-1 then result:='!'+s else result:=s;
end;

function ReturnStdFont(curfont:string;japanese:boolean):string;
begin
  if curfont[1]<>'!'then result:=curfont else
  if (japanese) then
  begin
    if Screen.Fonts.IndexOf('MS Mincho')>-1 then result:='MS Mincho'else
    if Screen.Fonts.IndexOf('MS Gothic')>-1 then result:='MS Gothic'else result:='!';
  end else
  if (not japanese) then
  begin
    if Screen.Fonts.IndexOf('MingLiU')>-1 then result:='MingLiU'else
    if Screen.Fonts.IndexOf('SimSun')>-1 then result:='SimSun'else result:='!';
  end;
end;

procedure TfMenu.Button6Click(Sender: TObject);
begin
//  ML_ChooseLanguage('Japanese Word Builder',false,'e+c+');
//  showmessage('Language has been changed. Please restart the application.'#13'Jazyk byl zmÏnÏn. ProsÌm restartujte aplikaci.');
//  Application.Terminate;
end;

procedure ConvUniToMixUni(inpf,outf:string;var recn:integer);
var f:file of byte;
    t:textfile;
    bl,bh:byte;
    s:string;
    inuni:boolean;
begin
  recn:=0;
  assignfile(f,inpf);
  reset(f);
  assignfile(t,outf);
  rewrite(t);
  s:='';
  inuni:=false;
  while not eof(f) do
  begin
    read(f,bl);
    read(f,bh);
    if (bh=0) and (bl=10) then
    begin
      if inuni then s:=s+'>';
      writeln(t,s);
      s:='';
      inuni:=false;
      inc(recn);
    end;
    if (bh=0) and (bl<>10) then
    begin
      if inuni then s:=s+'>';
      inuni:=false;
      s:=s+chr(bl);
    end;
    if (bh<>0) then
    begin
      if not inuni then s:=s+'<';
      inuni:=true;
      s:=s+Format('%2.2X%2.2X',[bh,bl]);
    end;
  end;
  if s<>'' then writeln(t,s);
  closefile(f);
  closefile(t);
end;

function split(var s:string;c:char):string;
var s2:string;
begin
  if pos(c,s)=0 then
  begin
    result:='';
    exit;
  end;
  result:=copy(s,1,pos(c,s)-1);
  delete(s,1,pos(c,s));
end;

procedure TfMenu.Button3Click(Sender: TObject);
var t,t2,t3:textfile;
    s:string;
    kanj,phon:string;
    dic:TJaletDic;
    recn:integer;
    en:string;
    prob:boolean;
    mark:string;
begin
  ConvUniToMixUni(fSettings.edit31.text+'.uni',fSettings.edit31.text+'.cnv',recn);
  assignfile(t,fSettings.edit31.text+'.cnv');
  reset(t);
  assignfile(t2,fSettings.Edit31.text+'.wkl');
  rewrite(t2);
  assignfile(t3,'convert.err');
  rewrite(t3);
  writeln(t2,'WaKan Word List 1');
  writeln(t2,'');
  writeln(t2,'; created by WaKan - Japanese & Chinese Learning Tool (C) Filip Kabrt 2002-2003');
  writeln(t2,'; This file lists words that were exported from user vocabulary.');
  writeln(t2,'; Each entry consists of four lines:');
  writeln(t2,'; Written (Unicode in hex), Phonetic (Unicode in hex), English (raw text), Category (raw text)');
  writeln(t2,'');
  dic:=(dicts.Objects[0]) as TJaletDic;
  dic.Demand;
  while not eof(t) do
  begin
    readln(t,s);
    if pos('<',s)>0 then
    begin
      delete(s,1,pos('<',s));
      kanj:=copy(s,1,pos('>',s)-1);
      delete(s,1,pos('>',s));
      phon:='';
      if pos('<',s)>0 then
      begin
        delete(s,1,pos('<',s));
        phon:=copy(s,1,pos('>',s)-1);
        delete(s,1,pos('>',s));
      end;
      if phon='' then phon:=kanj;
      if pos('FF08',phon)>0 then phon:=kanj;
      dic.TDict.SetOrder('Kanji_Ind');
      prob:=false;
      if dic.TDict.Locate('Kanji',kanj,false) then
      begin
        en:=EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),dic.TDict.Str(dic.TDictMarkers));
        while (dic.TDict.Str(dic.TDictPhonetic)<>phon) and (dic.TDict.Str(dic.TDictKanji)=kanj) do
          dic.TDict.Next;
        if dic.TDict.Str(dic.TDictPhonetic)<>phon then
        begin
          writeln(t3,'Phonetic problem:'+KanaToRomaji(phon,1,'j'));
          prob:=true;
        end else en:=EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),dic.TDict.Str(dic.TDictMarkers));
        writeln(t2,kanj);
        writeln(t2,phon);
        writeln(t2,en);
        writeln(t2,fSettings.edit30.text);
        dic.TDict.Next;
      end else
      begin
        dic.TDict.SetOrder('Phonetic_Ind');
        if dic.TDict.Locate('Sort',KanaToRomaji(phon,1,'j'),false) then
        begin
          en:=EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),dic.TDict.Str(dic.TDictMarkers));
          writeln(t2,dic.TDict.Str(dic.TDictPhonetic));
          writeln(t2,dic.TDict.Str(dic.TDictPhonetic));
          dic.TDict.Next;
          if dic.TDict.Str(dic.TDictPhonetic)=phon then
            writeln(t3,'Duplicate phon:'+KanaToRomaji(phon,1,'j'));
          writeln(t2,en);
          writeln(t2,fSettings.edit30.text);
        end else if pos('?',KanaToRomaji(phon,1,'j'))=0 then
        begin
          writeln(t3,'Not found:'+KanaToRomaji(phon,1,'j')+'   -'+phon);
          writeln(t2,kanj);
          writeln(t2,phon);
          writeln(t2,'?');
          writeln(t2,fSettings.edit30.text);
        end;
      end;
    end;
  end;
  closefile(t);
  closefile(t2);
  closefile(t3);
  winexec('notepad.exe convert.err',SW_SHOW);
end;

procedure TfMenu.FormDestroy(Sender: TObject);
begin
  TChar.Free;
  TCharRead.Free;
  TRadicals.Free;
  FreeKnownLists;
  FreeRomaList;
  doc.Free;
  doctr.Free;
  docdic.Free;
  defll.Free;
  suffixl.Free;
  partl.Free;
  bopomofol.Free;
  markersl.Free;
  dicts.Free;
  kanjicatuniqs.Free;
end;

procedure TfMenu.RadioGroup1Click(Sender: TObject);
begin
//  romasys:=RadioGroup1.ItemIndex+1;
end;

procedure TfMenu.RadioGroup2Click(Sender: TObject);
begin
//  showroma:=RadioGroup2.ItemIndex=1;
end;

procedure TfMenu.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 { Disconnect from clipboard viewer }
  ChangeClipboardChain(Self.Handle, CbNextViewer);

  if not FlushUserData then Action:=caNone;
  if not fUser.CommitFile then Action:=caNone;
  FormPlacement1.SaveFormPlacement;
  if Action<>caNone then
  begin
    if fExamples.SpeedButton4.Down then exmode:=0;
    if fExamples.SpeedButton5.Down then exmode:=1;
    if fExamples.SpeedButton6.Down then exmode:=2;
    if SpeedButton1.Down then
    begin
      Screen.Cursor:=crHourGlass;
{MCH      UninjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
      Screen.Cursor:=crDefault;
    end;
    fSettings.BitBtn1Click(self);
    if curqlayout=0 then WriteLayout('wakan.lay');
    fTranslate.Close;
    fUser.Close;
    fWords.Close;
    fKanji.Close;
  end;
end;

procedure TfMenu.SpeedButton5Click(Sender: TObject);
begin
  fSettings.PageControl1.ActivePage:=fSettings.TabSheet1;
  fSettings.ShowModal;
  if fKanji.Visible then fKanji.DoIt;
  if fUser.Visible then fUser.Look(false);
  if fWords.Visible then fWords.ShowIt(false);
  if fTranslate.Visible then fUser.RepaintText;
end;

procedure TfMenu.WmChangeCbChain(var Msg: TMessage);
begin
  if Msg.wParam=CbNextViewer then begin
   // Replace next window and return
    CbNextViewer := Msg.lParam;
    Msg.Result := 0;
  end else
   // Pass to the next window
    if CbNextViewer<>0 then
      Msg.Result := SendMessage(CbNextViewer, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TfMenu.WmDrawClipboard(var Msg: TMessage);
begin
  Clipboard_Timer1Timer(Timer1);
  if CbNextViewer<>0 then
    SendMessage(CbNextViewer, Msg.Msg, Msg.wParam, Msg.lParam); //call next viewer
end;

procedure TfMenu.Clipboard_PaintBox3Paint(Sender: TObject);
var s:string;
begin
  PaintBox3.Canvas.Brush.Color:=clWindow;
  PaintBox3.Canvas.Font.Color:=clWindowText;
  DrawUnicode(PaintBox3.Canvas,1,1,22,copy(clip,1,200),FontRadical);
end;

procedure TfMenu.Clipboard_Timer1Timer(Sender: TObject);
var i:integer;
    h:boolean;
    oldclip:string;
    MyHandle:THandle;
    textptr:PWideChar;
    s:widestring;
begin
  if critsec then exit;
  critsec:=true;
  oldclip:=clip;
  try
    Clipboard.Open;
    h:=false;
    for i:=0 to Clipboard.FormatCount-1 do
      if Clipboard.Formats[i]=CF_UNICODETEXT then h:=true;
    if h then
    begin
      MyHandle:=Clipboard.GetAsHandle(CF_UNICODETEXT);
      TextPtr:=GlobalLock(MyHandle);
      s:=textptr;
      if length(s)>64000 then s:=_l('#00342^eToo much data.^cP¯Ìliö mnoho dat.');
      clip := {$IFDEF UNICODE}s{$ELSE}UnicodeToHex(s){$ENDIF};
      cliptrans:='';
      GlobalUnlock(MyHandle);
      oldhandle:=MyHandle;
    end;
    Clipboard.Close;
  except
    clip := {$IFDEF UNICODE}'ERROR'{$ELSE}UnicodeToHex('ERROR'){$ENDIF};
  end;
  if clip<>oldclip then
  begin
    PaintBox3.Invalidate;
    fUserAdd.PaintBox2.Invalidate;
    if (fKanji.Visible) and (fKanjiSearch.SpeedButton3.Down) then fKanji.DoIt;
    if (fUser.Visible) and (fUser.SpeedButton3.Down) then fUser.Look(false);
  end;
  critsec:=false;
end;

procedure TfMenu.Clipboard_SpeedButton7Click(Sender: TObject);
begin
  clip:='';
  ChangeClipboard;
end;

procedure TfMenu.ChangeClipboard;
var
  DataHandle :  THandle;
  ToPointer  :  Pointer;
  ws         :  UnicodeString;
begin
 {$IFDEF UNICODE}
  ws := clip;
 {$ELSE}
  ws := HexToUnicode(clip);
 {$ENDIF}

 //Copy data + final NULL
  DataHandle := GlobalAlloc(GMEM_DDESHARE OR GMEM_MOVEABLE,
                            (Length(ws)+1)*SizeOf(WChar));

  ToPointer   := GlobalLock(DataHandle);
  if pointer(ws)<>nil then
    Move(Pointer(ws)^, ToPointer^, (Length(ws)+1)*SizeOf(WChar))
  else
   //Just set the terminating null
    PChar(ToPointer)^ := #00;
  GlobalUnlock(DataHandle);

  OpenClipboard(Handle);
  EmptyClipboard;
  SetClipboardData(CF_UNICODETEXT, DataHandle);
  CloseClipboard;
  PaintBox3.Invalidate;
  fUserAdd.PaintBox2.Invalidate;

  if (fKanji.Visible) and (fKanjiSearch.SpeedButton3.Down) then fKanji.DoIt;
  if (fUser.Visible) and (fUser.SpeedButton3.Down) then fUser.Look(false);
end;

procedure TfMenu.ArtLabel1Click(Sender: TObject);
begin
  fSplash.ProgressBar1.Hide;
  fSplash.BitBtn1.Show;
  fSplash.ShowModal;
end;

procedure TfMenu.SpeedButton2Click(Sender: TObject);
begin
  screenModeWk:=SpeedButton2.Down;
end;

procedure TfMenu.SpeedButton7Click(Sender: TObject);
begin
  LoadUserData;
end;

procedure TfMenu.SpeedButton8Click(Sender: TObject);
begin
  fDictMan.ShowModal;
end;

procedure TfMenu.tab1Click(Sender: TObject);
begin
  ToggleForm(fKanji,nil,nil);
end;

procedure TfMenu.tab2Click(Sender: TObject);
begin
  ToggleForm(fUser,nil,nil);
end;

procedure TfMenu.tab5Click(Sender: TObject);
begin
  ToggleForm(fWords,nil,nil);
end;

procedure TfMenu.FormCreate(Sender: TObject);
begin
  initdone:=false;
end;

procedure TfMenu.FormShow(Sender: TObject);
var tt:TTextTable;
    ps:TPackageSource;
    sl:TStringList;
    reg:TRegIniFile;
    s,sx:string;
    vi:TStringList;
    ms:TMemoryStream;
    setlayout,setwindows:integer;
    tim,timbeg,timload,timoth,timuser,timproc,timdict:TDateTime;
    conft:textfile;
    sect:integer;
    sortset,otherset:integer;
    meanset,userset:boolean;
    t:textfile;
    s_parts: TStringArray;
begin
  TAnnots:=nil;
  lastautosave:=now;
  screenTipImmediate:=false;
  examstruct:=nil;
  examindex:=nil;
  exampackage:=nil;
  screenModeSc:=false;
  screenModeWk:=false;
  tim:=now;
  firstact:=true;
  if initdone then exit;
  curlang:='j';
  intmopaint:=nil;
  intmogrid:=nil;
  doc:=TStringList.Create;
  doctr:=TStringList.Create;
  docdic:=TStringList.Create;
  defll:=TDeflectionList.Create;
  suffixl:=TStringList.Create;
  partl:=TStringList.Create;
  chardetl:=TStringList.Create;
  chartypel:=TStringList.Create;
  bopomofol:=TStringList.Create;
  markersl:=TStringList.Create;
  romasortl:=TStringList.Create;
  ignorel:=TStringList.Create;
  readchl:=TStringList.Create;
  kanjicatuniqs:=TStringList.Create;

 //Load language or suggest to choose one
  fLanguage.LoadRegistrySettings;

  fLanguage.TranslateForm(fSplash);
  fSplash.Label4.Caption:=WakanVer;
  Caption:='WaKan '+WakanVer+' - '+_l('^eTool for learning Japanese & Chinese^cN·stroj pro studenty japonötiny a ËÌnötiny');
{  Application.MessageBox(
    pchar(_l('#00343^eThis is a public beta version of WaKan.'#13#13'This version is not freely distributable but the final version will be.'+
    #13#13'You use this software on your own risk, I take no responsibility for the program behavior.'#13'Please mail comments to dreamfly@centrum.cz.^cToto je ve¯ejn· beta verze programu JaLeT.'#13#13+
    'Tato verze nenÌ volnÏ öÌ¯iteln·, aËkoli fin·lnÌ verze bude.'#13#13'PouûitÌ tohoto software je na vlastnÌ nebezpeËÌ, autor nenese û·dnou odpovÏdnost za chov·nÌ programu.'#13'P¯ipomÌnky posÌlejte na dreamfly@centrum.cz')+
    _l('^e'#13#13'For more information please visit http://jalet.fbi.cz^c'#13#13'Pro vÌce informacÌ prosÌm navötivte http://jalet.fbi.cz')),
    pchar(_l('#00344^ePublic beta version^cVe¯ejn· v˝vojov· verze')),MB_ICONWARNING);
}  if (Screen.Width<800) or (Screen.Height<600) then
  begin
    if Application.MessageBox(
      pchar(_l('^eThis version of WaKan requires at least 800x600 resolution.'#13#13'Do you really want to continue?'+
      '^cTato verze programu WaKan vyûaduje rozliöenÌ alespoÚ 800x600.'#13#13'Opravdu chcete pokraËovat?')),
      pchar(_l('#00020^eError^cChyba')),MB_YESNO or MB_ICONERROR)=idNo then
      begin
        Application.Terminate;
        exit;
      end;
  end;
  if (not FileExists('wakan.chr')) then
  begin
    Application.MessageBox(
      pchar(_l('#00346^eFile WAKAN.CHR was not found.'#13'This file is required for application to run.'#13'Please download this file from WAKAN website.'#13#13'Application will now be terminated.'+
      '^cSoubor WAKAN.CHR nebyl nalezen.'#13'Tento soubor je pot¯eba ke spuötÏnÌ aplikace.'#13'Nahrajte si tento soubor ze str·nky WAKANu'#13#13'Aplikace bude nynÌ ukonËena.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    Application.Terminate;
    exit;
  end;
  if (not FileExists('wakan.cfg')) then
  begin
    Application.MessageBox(
      pchar(_l('#00347^eFile WAKAN.CFG is missing.'#13'This file contains important configuration parameters and is required for application to run.'#13#13'Application will now be terminated.'+
      '^cSoubor WAKAN.CFG nebyl nalezen.'#13'Tento soubor obsahuje d˘leûitÈ konfiguraËnÌ parametry a je pot¯eba ke spuötÏnÌ aplikace.'#13#13'Aplikace bude nynÌ ukonËena.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    Application.Terminate;
    exit;
  end;
  sx:='';
{  if ChooseFont([SHIFTJIS_CHARSET],'',s,'',true)='!'then sx:=sx+',Shift-JIS';
  if ChooseFont([CHINESEBIG5_CHARSET],'',s,'',true)='!'then sx:=sx+',Big5';
  if ChooseFont([GB2312_CHARSET],'',s,'',true)='!'then sx:=sx+',GB2312';
  if sx<>'' then
  begin
    delete(sx,1,1);
    Application.MessageBox(
      pchar(_l('#00348^eNo fonts of there character sets were found on your computer:'#13#13+sx+#13#13'You must have at least one font of each of these sets on your computer to run this application.'#13#13+
      'I recommend installing Ms Mincho, MS Gothic, SimSun and MingLiU fonts.'#13'These fonts are automatically installed when you install support for reading Japanese & Chinese language in windows.'#13#13+
      'Please install required fonts and run this application again.'+
      '^cNa vaöem poËÌtaËi nebyly nalezeny û·dnÈ fonty z tÏchto znakov˝ch sad:'#13#13+sx+#13#13'MusÌte mÌt nainstalov·n alespoÚ jeden font z kaûdÈ z tÏchto znakov˝ch sad, abyste mohli spustit aplikaci.'#13#13+
      'DoporuËuji nainstalovat fonty MS Mincho, MS Gothic, SimSun a MingLiU.'#13'Tyto fonty se nainstalujÌ automaticky, jakmile nainstalujete podporu pro ËtenÌ ËÌnötiny a japonötiny ve Windows.'#13#13+
      'ProsÌm nainsalujte vyûadovanÈ fonty a spusùte tuto aplikaci znovu.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    Application.Terminate;
    exit;
  end;
}  oldhandle:=0;
  critsec:=false;
  TranslateAll;
  romasys:=1;
  showroma:=false;
  clip:='';
  dicts:=TStringList.Create;
  InitColors;
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  fSettings.CheckBox64.Checked:=reg.ReadBool('Annotate','Enabled',true);
  fSettings.CheckBox65.Checked:=reg.ReadBool('Annotate','Rebuild',true);
  fSettings.CheckBox66.Checked:=reg.ReadBool('Annotate','Sound',true);
  fSettings.CheckBox67.Checked:=reg.ReadBool('Annotate','Pictures',true);
  fSettings.CheckBox68.Checked:=reg.ReadBool('Annotate','WebPages',true);
  fSettings.CheckBox69.Checked:=reg.ReadBool('Annotate','Colors',true);
  fSettings.CheckBox46.Checked:=reg.ReadBool('Vocabulary','AutoSave',false);
  fSettings.CheckBox70.Checked:=reg.ReadBool('Vocabulary','DisplayMessage',true);
  fSettings.CheckBox54.Checked:=reg.ReadBool('Vocabulary','AutoSaveTimer',true);
  fSettings.CheckBox55.Checked:=reg.ReadBool('Vocabulary','MakeBackups',true);
  fSettings.Edit29.Text:=inttostr(reg.ReadInteger('Vocabulary','AutoSavePeriod',10));
  fSettings.RadioGroup1.ItemIndex:=reg.ReadInteger('Romanization','System',1);
  fSettings.RadioGroup2.ItemIndex:=reg.ReadInteger('Romanization','ShowKana',0);
  fSettings.RadioGroup6.ItemIndex:=reg.ReadInteger('Romanization','ChineseSystem',0);
  fSettings.RadioGroup7.ItemIndex:=reg.ReadInteger('Romanization','ShowBopomofo',1);
  fKanjiCompounds.CheckBox1.Checked:=reg.ReadBool('Characters','CompoundsBeg',false);
  fKanjiCompounds.CheckBox2.Checked:=reg.ReadBool('Characters','CompoundsPop',true);
  fKanjiCompounds.CheckBox3.Checked:=reg.ReadBool('Characters','CompoundsFreq',true);
  fSettings.RadioGroup5.ItemIndex:=reg.ReadInteger('Characters','Chinese',0);
  fSettings.RadioGroup3.ItemIndex:=reg.ReadInteger('Characters','GridSize',1);
  fSettings.ComboBox1.ItemIndex:=reg.ReadInteger('Characters','RadicalType',0);
  fSettings.CheckBox1.Checked:=reg.ReadBool('Characters','ShowStrokes',false);
  fSettings.CheckBox51.Checked:=reg.ReadBool('Characters','StrokeOrderGridFont',false);
  fSettings.CheckBox3.Checked:=reg.ReadBool('Characters','NoShowColors',false);
  fSettings.CheckBox57.Checked:=reg.ReadBool('Characters','YomiOkurigana',false);
  if FileExists('WAKAN.CDT') then chardetl.LoadFromFile('WAKAN.CDT') else fSettings.Button10Click(sender);
  if reg.ReadString('Fonts','FontSet','0')<>'1'then
  begin
    Application.MessageBox(pchar(_l('#00349^eYou are running WaKan for the first time.'#13'WaKan will now try to locate and set all the recommended fonts.'#13+
    'You can restart this process by selecting "Select recommended fonts" in settings.^cSpouötÌte WaKan poprvÈ.'#13+
    'NynÌ probÏhne pokus o nalezenÌ a nastavenÌ doporuËovan˝ch font˘.'#13+
    'Tento proces m˘ûete zopakovat volbou "Nastavit doporuËenÈ fonty" v nastavenÌ.')),
    pchar(_l('#00350^eFont autodetection^cAutodetekce font˘')),MB_ICONINFORMATION or MB_OK);
    if not fSettings.AutoDetectFonts then
    begin
      if Application.MessageBox(pchar(_l('#00351^eFont autodetection failed. Some characters may not be displayed correctly.'#13#13+
      'Do you want to continue?^cAutodetekce font˘ selhala. NÏkterÈ znaky nemusÌ b˝t zobrazeny spr·vnÏ.'#13#13'Chcete pokraËovat?')),
      pchar(_l('#00090^eWarning^cVarov·nÌ')),MB_ICONERROR or MB_YESNO)=idNo then
      begin
        Application.Terminate;
        exit;
      end;
    end;
  end else
  begin
    fSettings.Edit1.Text:=checkfont(reg.ReadString('Fonts','JapaneseGrid','MS Mincho'));
    fSettings.Edit2.Text:=checkfont(reg.ReadString('Fonts','Japanese','MS Mincho'));
    fSettings.Edit5.Text:=checkfont(reg.ReadString('Fonts','Small','MS Gothic'));
    fSettings.Edit6.Text:=checkfont(reg.ReadString('Fonts','ChineseGrid','MingLiU'));
    fSettings.Edit3.Text:=checkfont(reg.ReadString('Fonts','ChineseGridGB','SimSun'));
    fSettings.Edit7.Text:=checkfont(reg.ReadString('Fonts','Chinese','MingLiU'));
    fSettings.Edit9.Text:=checkfont(reg.ReadString('Fonts','ChineseGB','SimSun'));
    fSettings.Edit8.Text:=checkfont(reg.ReadString('Fonts','Radical','MingLiU'));
    fSettings.Edit4.Text:=checkfont(reg.ReadString('Fonts','English','Verdana'));
    fSettings.Edit32.Text:=checkfont(reg.ReadString('Fonts','StrokeOrder','MS Mincho'));
    fSettings.Edit33.Text:=checkfont(reg.ReadString('Fonts','PinYin','Arial'));
  end;
  fSettings.CheckBox4.Checked:=reg.ReadBool('Dict','PreferUser',true);
  fSettings.CheckBox5.Checked:=reg.ReadBool('Dict','PreferNouns',true);
  fSettings.CheckBox6.Checked:=reg.ReadBool('Dict','PreferPolite',true);
  fSettings.CheckBox7.Checked:=reg.ReadBool('Dict','PreferPopular',true);
  fUser.SpeedButton13.Down:=reg.ReadBool('Dict','QuickSearch',true);
  fSettings.CheckBox8.Checked:=reg.ReadBool('Dict','ReplaceKanji',true);
  fSettings.CheckBox9.Checked:=reg.ReadBool('Dict','NoUseColors',false);
  fSettings.CheckBox10.Checked:=reg.ReadBool('Dict','UseGrey',false);
  fSettings.CheckBox11.Checked:=reg.ReadBool('Dict','StatusColors',true);
  fSettings.CheckBox12.Checked:=reg.ReadBool('Dict','AutoPage',true);
  fSettings.CheckBox49.Checked:=reg.ReadBool('Dict','DemandLoad',true);
  fSettings.CheckBox50.Checked:=reg.ReadBool('Dict','AutoExamples',true);
  fSettings.CheckBox53.Checked:=reg.ReadBool('Dict','MultiLineGrids',true);
  fSettings.CheckBox58.Checked:=reg.ReadBool('Dict','ShowFreq',false);
  fSettings.CheckBox59.Checked:=reg.ReadBool('Dict','OrderFreq',true);
  fSettings.CheckBox60.Checked:=reg.ReadBool('Editor','AutoSave',false);
  fSettings.CheckBox61.Checked:=reg.ReadBool('Editor','AutoLoad',false);
  fSettings.cbNoSaveChangesWarning.Checked:=reg.ReadBool('Editor','NoSaveChangesWarning',false);  
  if fSettings.CheckBox61.Checked then
  begin
    fUser.DocFileName:=Reg.ReadString('Editor','DocFileName','');
    fUser.DocTp:=Reg.ReadInteger('Editor','DocType',0);
  end;
  fExamples.SpeedButton11.Down:=reg.ReadBool('Dict','RandomExamples',false);
  vocmode:=reg.ReadInteger('Dict','VocMode',0);
  exmode:=reg.ReadInteger('Dict','ExMode',0);
  fSettings.Edit34.Text:=inttostr(reg.ReadInteger('Characters','FreqLimit',0));
  if exmode=0 then fExamples.SpeedButton4.Down:=true;
  if exmode=1 then fExamples.SpeedButton5.Down:=true;
  if exmode=2 then fExamples.SpeedButton6.Down:=true;
  fSettings.Edit25.Text:=inttostr(reg.ReadInteger('Dict','FontSize',14));
  GridFontSize:=strtoint(fSettings.Edit25.text);
  fSettings.ListBox1.ItemIndex:=reg.ReadInteger('WordSheet','Columns',0);
  fSettings.CheckBox14.Checked:=reg.ReadBool('WordSheet','InsideLines',true);
  fSettings.CheckBox15.Checked:=reg.ReadBool('WordSheet','OutsideLines',true);
  fSettings.CheckBox16.Checked:=reg.ReadBool('WordSheet','VaryColors',true);
  fSettings.CheckBox17.Checked:=reg.ReadBool('WordSheet','PrintUnlearned',true);
  fSettings.Edit10.Text:=inttostr(reg.ReadInteger('WordSheet','NoLines',40));
  fSettings.Edit16.Text:=reg.ReadString('WordSheet','UserColumns','p1--m1--');
  fSettings.Edit11.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCharacters',10));
  fSettings.Edit12.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCompoundsH',10));
  fSettings.Edit13.Text:=inttostr(reg.ReadInteger('KanjiCards','NoCompoundsV',6));
  fSettings.Edit35.Text:=inttostr(reg.ReadInteger('KanjiCards','NoFullCompounds',4));
  fSettings.Edit14.Text:=reg.ReadString('KanjiCards','Font','MingLiU');
  NotUsedDicts:=reg.ReadString('Dict','NotUsedDicts','');
  NotGroupDicts[1]:=reg.ReadString('Dict','NotGroup1Dicts','');
  NotGroupDicts[2]:=reg.ReadString('Dict','NotGroup2Dicts','');
  NotGroupDicts[3]:=reg.ReadString('Dict','NotGroup3Dicts','');
  NotGroupDicts[4]:=reg.ReadString('Dict','NotGroup4Dicts','');
  NotGroupDicts[5]:=reg.ReadString('Dict','NotGroup5Dicts','');
  OfflineDicts:=reg.ReadString('Dict','OfflineDicts','');
  fSettings.CheckBox18.Checked:=reg.ReadBool('KanjiCards','PrintCompounds',true);
  fSettings.CheckBox19.Checked:=reg.ReadBool('KanjiCards','PrintRadical',true);
  fSettings.CheckBox20.Checked:=reg.ReadBool('KanjiCards','PrintAlternate',true);
  fSettings.CheckBox21.Checked:=reg.ReadBool('KanjiCards','PrintReadings',true);
  fSettings.CheckBox22.Checked:=reg.ReadBool('KanjiCards','PrintOuterLines',true);
  fSettings.CheckBox23.Checked:=reg.ReadBool('KanjiCards','PrintInnerLines',true);
  fSettings.CheckBox24.Checked:=reg.ReadBool('KanjiCards','PrintVertical',true);
  fSettings.CheckBox25.Checked:=reg.ReadBool('KanjiCards','ColumnSpace',true);
  fSettings.CheckBox44.Checked:=reg.ReadBool('KanjiCards','PrintDefinition',true);
  fSettings.CheckBox45.Checked:=reg.ReadBool('KanjiCards','PrintStrokeCount',false);
  fSettings.CheckBox52.Checked:=reg.ReadBool('KanjiCards','PrintStrokeOrder',false);
  fSettings.CheckBox62.Checked:=reg.ReadBool('KanjiCards','PrintFullComp',true);
  fSettings.CheckBox63.Checked:=reg.ReadBool('KanjiCards','SortFrequency',true);
  fSettings.CheckBox26.Checked:=reg.ReadBool('Vocabulary','SaveStat',false);
  fUser.SpeedButton4.Down:=reg.ReadBool('Dict','DeflexItalic',true);
  fSettings.CheckBox43.Checked:=reg.ReadBool('Translate','BreakLines',true);
  fSettings.CheckBox32.Checked:=reg.ReadBool('Translate','DisplayLines',true);
  fSettings.CheckBox41.Checked:=reg.ReadBool('Translate','DisplayNonJapanese',true);
  fSettings.CheckBox33.Checked:=reg.ReadBool('Translate','NoMeaningLearned',false);
  fSettings.CheckBox35.Checked:=reg.ReadBool('Translate','NoReadingLearned',false);
  fSettings.CheckBox36.Checked:=reg.ReadBool('Translate','ReadingKatakana',true);
  fSettings.CheckBox34.Checked:=reg.ReadBool('Translate','NoSearchParticles',false);
  fSettings.CheckBox38.Checked:=reg.ReadBool('Translate','NoTranslateHiragana',false);
  fSettings.CheckBox39.Checked:=reg.ReadBool('Translate','NoUseColors',false);
  fSettings.CheckBox40.Checked:=reg.ReadBool('Translate','UserBold',true);
  fSettings.CheckBox42.Checked:=reg.ReadBool('Translate','LeaveSpace',false);
  fSettings.CheckBox56.Checked:=reg.ReadBool('Translate','LeaveSpaceAlways',true);
  fSettings.CheckBox27.Checked:=reg.ReadBool('Translate','HalfSizeMeaning',false);
  fSettings.CheckBox29.Checked:=reg.ReadBool('Translate','PrintReading',true);
  fSettings.CheckBox30.Checked:=reg.ReadBool('Translate','PrintMeaning',true);
  fSettings.CheckBox31.Checked:=reg.ReadBool('Translate','NoPrintColors',true);
  fSettings.CheckBox37.Checked:=reg.ReadBool('Translate','VerticalPrint',false);
  fSettings.cbTranslateNoLongTextWarning.Checked := reg.ReadBool('Translate','NoLongTextWarning',false);  
  aEditorColors.Checked:=reg.ReadBool('Translate','TransColors',true);
  fTranslate.SpeedButton21.Down:=aEditorColors.Checked;
  fTranslate.SpeedButton8.Down:=reg.ReadBool('Translate','Reading',true);
  fTranslate.SpeedButton9.Down:=reg.ReadBool('Translate','Meaning',true);
  fTranslate.SpeedButton19.Down:=reg.ReadBool('Translate','Dictionary',false);
  CharDetDocked:=reg.ReadBool('Layout','CharDetailsDocked',false);
  CharDetDockedVis1:=reg.ReadBool('Layout','CharDetailsVisible1',true);
  CharDetDockedVis2:=reg.ReadBool('Layout','CharDetailsVisible2',true);
  fSettings.CheckBox28.Checked:=reg.ReadBool('ScreenTrans','Japanese',true);
  fSettings.CheckBox47.Checked:=reg.ReadBool('ScreenTrans','English',true);
  fSettings.CheckBox48.Checked:=reg.ReadBool('ScreenTrans','Kanji',true);
  fSettings.Edit21.Text:=reg.ReadString('ScreenTrans','Delay','10');
  fSettings.Edit22.Text:=reg.ReadString('ScreenTrans','LeftRange','20');
  fSettings.Edit23.Text:=reg.ReadString('ScreenTrans','RightRange','100');
  fSettings.Edit24.Text:=reg.ReadString('ScreenTrans','DictEntries','4');
  fSettings.Edit26.Text:=reg.ReadString('ScreenTrans','SizeFactor','12');
  fSettings.Edit27.Text:=reg.ReadString('ScreenTrans','MinCompounds','10');
  fSettings.Edit28.Text:=reg.ReadString('ScreenTrans','MaxCompounds','40');
  SpeedButton2.Down:=reg.ReadBool('ScreenTrans','WakanToolTip',true);
  screenModeWk:=SpeedButton2.Down;
  setlayout:=reg.ReadInteger('Layout','DisplayLayout',1);
  setwindows:=reg.ReadInteger('Layout','SecondaryWindows',72);
  aEditorReading.Checked:=fTranslate.SpeedButton8.Down;
  aEditorMeaning.Checked:=fTranslate.SpeedButton9.Down;
  case reg.ReadInteger('Translate','FontSize',2) of
    0:fTranslate.SpeedButton16.Down:=true;
    1:fTranslate.SpeedButton18.Down:=true;
    2:fTranslate.SpeedButton17.Down:=true;
  end;
  case reg.ReadInteger('Translate','FontSize',2) of
    0:aEditorSmallFont.Checked:=true;
    1:aEditorMedFont.Checked:=true;
    2:aEditorLargeFont.Checked:=true;
  end;
  fSettings.Edit17.Text:=reg.ReadString('Translate','MeaningLines','2');
  fSettings.Edit18.Text:=reg.ReadString('Translate','PrintLines','20');
  sortset:=reg.ReadInteger('Characters','Sort',0);
  otherset:=reg.ReadInteger('Characters','OtherSearch',0);
  userset:=reg.ReadBool('Characters','UserCompounds',false);
  if reg.ReadBool('Dict','Meaning',false) then dictmodeset:=1 else dictmodeset:=0;
  dictbeginset:=reg.ReadInteger('Dict','SearchBeg',0);
  fSettings.CheckBox2.Checked:=reg.ReadBool('Translate','ShowHint',true);
  fSettings.CheckBox13.Checked:=reg.ReadBool('Translate','HintMeaning',true);
  s:=reg.ReadString('Dict','CurLanguage','j');
  curlang:=s[1];
  fSettings.ListBox1Click(self);
  FontJapanese:=fSettings.Edit2.Text;
  FontJapaneseGrid:=fSettings.Edit1.Text;
  FontChinese:=fSettings.Edit7.Text;
  FontChineseGrid:=fSettings.Edit6.Text;
  FontChineseGB:=fSettings.Edit9.Text;
  FontChineseGridGB:=fSettings.Edit3.Text;
  FontSmall:=fSettings.Edit5.Text;
  FontRadical:=fSettings.Edit8.Text;
  FontEnglish:=fSettings.Edit4.Text;
  FontPinYin:=fSettings.Edit33.Text;
  FontStrokeOrder:=fSettings.Edit32.Text;
  reg.Free;
  fSplash.Show;
  fSplash.Update;
  try
  ps:=TPackageSource.Create('wakan.chr',791564,978132,978123);
  romac:=TStringList.Create;
  roma_t := TRomajiTranslationTable.Create;
  vi:=TStringList.Create;
  ms:=ps['jalet.ver'].Lock;
  vi.LoadFromStream(ms);
  ps['jalet.ver'].Unlock;
  ms:=ps['markers.lst'].Lock;
  markersl.LoadFromStream(ms);
  ps['markers.lst'].Unlock;
  try
    assignfile(conft,'wakan.cfg');
    reset(conft);
    sect:=0;
    defll.Clear;
    suffixl.Clear;
    partl.Clear;
    romac.Clear;
    roma_t.Clear;
    while not eof(conft) do
    begin
      readln(conft,s);
      if (length(s)>0) and (s[1]<>';') then
      begin
        if s[1]='['then
        begin
          delete(s,length(s),1);
          delete(s,1,1);
          if s='Particles'then sect:=1 else
          if s='Deflection'then sect:=2 else
          if s='Romaji'then sect:=3 else
          if s='PinYin'then sect:=4 else
          if s='CharInfo'then sect:=5 else
          if s='RomajiSort'then sect:=6 else
          if s='Suffixes'then sect:=7 else
          if s='IgnoreWords'then sect:=8 else
          if s='ReadingChart'then sect:=9 else
          sect:=0;
        end else
        begin
          if sect=1 then partl.Add(s);
          if sect=2 then defll.Add(s);
          if sect=3 then roma_t.Add(s);
          if sect=4 then splitadd(romac,s,4);
          if sect=5 then chartypel.Add(s);
          if sect=6 then splitadd(romasortl,s,2);
          if sect=7 then suffixl.Add(s);
          if sect=8 then ignorel.Add(s);
          if sect=9 then readchl.Add(s);
        end;
      end;
    end;
    closefile(conft);
    suffixl.Sorted:=true;
    suffixl.Sort;
  except
    Application.MessageBox(
      pchar(_l('#00352^eCannot load main configuration file.'#13'File WAKAN.CFG is corrupted.'#13#13'Application will now exit.'+
      '^cNepoda¯ilo se nahr·t hlavnÌ soubor s konfiguracÌ.'#13'Soubor WAKAN.CFG je poökozen.'#13#13'Aplikace bude nynÌ ukonËena.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    Application.Terminate;
    exit;
  end;
  while (pos('!',FontJapanese)>0) or (pos('!',FontJapaneseGrid)>0) or
    (pos('!',FontChinese)>0) or (pos('!',FontChineseGrid)>0) or
    (pos('!',FontChineseGB)>0) or (pos('!',FontChineseGridGB)>0) or
    (pos('!',FontSmall)>0) or (pos('!',FontRadical)>0) or (pos('!',FontEnglish)>0) or (pos('!',FontPinYin)>0) or (pos('!',FontStrokeOrder)>0) do
  begin
    Application.MessageBox(pchar(_l('#00353^cNa vaöem systÈmu nebyl nalezen nÏkter˝ ze standardnÌch font˘.'+
    #13'V n·sledujÌcÌm dialogu prosÌm vyberte spr·vnÈ fonty. ChybÏjÌcÌ fonty jsou oznaËeny vyk¯iËnÌkem.'+
    #13'Bez vybr·nÌ vöech font˘ nem˘ûe aplikace spr·vnÏ fungovat.^eSome standard fonts were not found on your system.'+
    #13'Please reselect all fonts in the following dialog. Missing fonts are preceded by !.'#13+
    'Application cannot continue unless all fonts are selected.')),pchar(_l('#00090^eWarning^cVarov·nÌ')),MB_ICONWARNING or MB_OK);
    fSettings.PageControl1.ActivePage:=fSettings.TabSheet3;
    fSettings.ShowModal;
  end;
  if (vi[0]<>'JALET.DIC') and (vi[0]<>'JALET.CHR') then raise Exception.Create('Unknown DICT.VER header.');
  if strtoint(vi[1])<CurDictVer then
  begin
    Application.MessageBox(pchar(_l('#00354^eWAKAN.CHR has old structure. Please download new version.'#13#13'Application will now exit.'+
      '^cWAKAN.CHR m· starou strukturu. St·hnÏte si prosÌm novou verzi.'#13#13'Aplikace bude ukonËena.')),pchar(_l('#00020^eError^cChyba')),
      MB_ICONERROR or MB_OK);
    Application.Terminate;
    exit;
  end;
  if strtoint(vi[1])>CurDictVer then
  begin
    Application.MessageBox(pchar(_l('#00355^eWAKAN.CHR has newer structure. Please download new WAKAN.EXE.'#13#13'Application will now exit.'+
      '^cWAKAN.CHR m· novÏjöÌ strukturu. St·hnÏte si prosÌm novou verzi WAKAN.EXE.'#13#13'Aplikace bude ukonËena.')),pchar(_l('#00020^eError^cChyba')),
      MB_ICONERROR or MB_OK);
    Application.Terminate;
    exit;
  end;
  fStatistics.Label13.Caption:=datetostr(strtoint(vi[2]));
  fStatistics.Label15.Caption:=vi[4];
  fStatistics.Label16.Caption:=vi[5];
  ChinesePresent:=vi[6]='CHINESE';
  vi.Free;
  timbeg:=now-tim;
  tim:=now;
  fSplash.ProgressBar1.Position:=1;
  fSplash.ProgressBar1.Update;
  TChar:=TTextTable.Create(ps,'Char',true,false);
  fSplash.ProgressBar1.Position:=2;
  fSplash.ProgressBar1.Update;
  TCharRead:=TTextTable.Create(ps,'CharRead',true,false);
  fSplash.ProgressBar1.Position:=3;
  fSplash.ProgressBar1.Update;
  TRadicals:=TTextTable.Create(ps,'Radicals',true,false);
//  TKanaKanji:=TTextTable.Create(ps,'KanaKanji',true);
  timload:=now-tim;
  tim:=now;
  if (fSettings.CheckBox64.Checked) and (fSettings.CheckBox65.Checked) then RebuildAnnotations;
  if (fSettings.CheckBox64.Checked) then LoadAnnotations;
//  showmessage(TChar.GetField(0,3));
//  showmessage(TChar.GetField(1,3));
//  showmessage(TChar.GetField(2,3));
  except
    Application.MessageBox(
      pchar(_l('#00356^eCannot load main dictionary file.'#13'File WAKAN.CHR is corrupted.'#13#13'Application will now exit.'+
      '^cNepoda¯ilo se nahr·t hlavnÌ soubor se slovnÌkem.'#13'Soubor WAKAN.CHR je poökozen.'#13#13'Aplikace bude nynÌ ukonËena.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    Application.Terminate;
    exit;
  end;
  if (not FileExists('wakan.rad')) then
  begin
    Application.MessageBox(
      pchar(_l('#00357^eFile WAKAN.RAD was not found.'#13'Japanese advanced radicals search will be disabled.'+
      '^cSoubor WAKAN.RAD nebyl nalezen.'#13'VylepöenÈ hled·nÌ podle japonsk˝ch radik·l˘ bude zak·z·no.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    rainesearch:=nil;
  end else
  begin
    try
      ps:=TPackageSource.Create('wakan.rad',791564,978132,978123);
      ms:=ps['search.bin'].Lock;
      GetMem(rainesearch,ms.Size);
      ms.Read(rainesearch^,ms.Size);
      ps['search.bin'].Unlock;
      ms:=ps['radicals.txt'].Lock;
      raineradicals:=TStringList.Create;
      raineradicals.LoadFromStream(ms);
      ps['radicals.txt'].Unlock;
      ps.Free;
    except
      Application.MessageBox(
        pchar(_l('#00358^eCannot load Japanese radicals file.'#13'File WAKAN.RAD is corrupted.'#13#13'Application will now exit.'+
        '^cNepoda¯ilo se nahr·t soubor s japonsk˝mi radik·ly.'#13'Soubor WAKAN.RAD je poökozen.'#13#13'Aplikace bude nynÌ ukonËena.')),
        pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;
  end;
  if (not FileExists('wakan.sod')) then
  begin
    Application.MessageBox(
      pchar(_l('#00359^eFile WAKAN.SOD was not found.'#13'Japanese stroke-order display will be disabled.'+
      '^cSoubor WAKAN.SOD nebyl nalezen.'#13'Zobrazov·nÌ po¯adÌ tah˘ bude zak·z·no.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    sobin:=nil;
  end else
  begin
    try
      ps:=TPackageSource.Create('wakan.sod',791564,978132,978123);
      ms:=ps['strokes.bin'].Lock;
      GetMem(sobin,ms.Size);
      ms.Read(sobin^,ms.Size);
      ps['strokes.bin'].Unlock;
      ms:=ps['dir.txt'].Lock;
      sodir:=TStringList.Create;
      sodir.LoadFromStream(ms);
      ps['dir.txt'].Unlock;
      ps.Free;
    except
      Application.MessageBox(
        pchar(_l('#00360^eCannot load Japanese stroke-order file.'#13'File WAKAN.SOD is corrupted.'#13#13'Application will now exit.'+
        '^cNepoda¯ilo se nahr·t soubor s po¯adÌm tah˘.'#13'Soubor WAKAN.SOD je poökozen.'#13#13'Aplikace bude nynÌ ukonËena.')),
        pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;
  end;
  try
  timuser:=now-tim;
  tim:=now;
  userdataloaded:=false;
  LoadUserData;
  except
    if FileExists('WAKAN.USR') then Application.MessageBox(
      pchar(_l('#00361^eCannot load user data file.'#13'File WAKAN.USR is corrupted.'#13'If you delete this file, it will be created anew.'#13#13'Application will now exit.'+
      '^cNepoda¯ilo se nahr·t hlavnÌ soubor s uûivatelsk˝mi daty.'#13'Soubor WAKAN.USR je poökozen.'#13'Pokud tento soubor odstranÌte, bude znovu vytvo¯en.'#13#13'Aplikace bude nynÌ ukonËena.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR)
    else Application.MessageBox(
      pchar(_l('#00362^eUnable to create user data file WAKAN.USR.'#13'Please run this program from a folder that is not read-only.'#13#13'Application will not exit.^cNemohu vytvo¯it soubor s uûivatelsk˝mi daty WAKAN.USR'+#13+
      'ProsÌm spusùte tento program ze sloûky, kter· nenÌ pouze pro ËtenÌ.'#13#13+'Aplikace bude nynÌ ukonËena.')),
      pchar(_l('#00020^eError^cChyba')),MB_OK or MB_ICONERROR);
    Application.Terminate;
    exit;
  end;
  if Application.Terminated then exit;
  jromasys:=fSettings.RadioGroup1.ItemIndex+1;
  jshowroma:=fSettings.RadioGroup2.ItemIndex=1;
  cromasys:=fSettings.RadioGroup6.ItemIndex+1;
  cshowroma:=fSettings.RadioGroup7.ItemIndex=1;
//  BuildRomaList;
//  Left:=0;
//  Top:=0;
//  fSizeCheck.WindowState:=wsMaximized;
//  fSizeCheck.Show;
//  globheight:=fSizeCheck.Height;
//  fSizeCheck.Close;
  SetFormPos(fKanji);
  SetFormPos(fWords);
  SetFormPos(fUser);
  TUser.First;
  MaxUserIndex:=0;
  while not TUser.EOF do
  begin
    if TUser.Int(TUserIndex)>MaxUserIndex then MaxUserIndex:=TUser.Int(TUserIndex);
    TUser.Next;
  end;
  MaxCategoryIndex:=0;
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    if TUserCat.Int(TUserCatIndex)>MaxCategoryIndex then MaxCategoryIndex:=TUserCat.Int(TUserCatIndex);
    TUserCat.Next;
  end;
  RefreshCategory;
  RefreshKanjiCategory;
  StrokeOrderPackage:=nil;
  if FileExists('wakan.sod') then
{  try
    StrokeOrderPackage:=TPackageSource.Create('wakan.sod',932147,513478,314798);
  except
    StrokeOrderPackage:=nil;
  end;}
  fKanji.SpeedButton4.Visible:=StrokeOrderPackage<>nil;
  XPResFix(fMenu);
  XPResFix(fKanji);
  XPResFix(fWords);
  XPResFix(fUser);
  XPResFix(fUserAdd);
  XPResFix(fUserCategory);
//  XPResFix(fKanjiSearch);
  XPResFix(fKanjiCompounds);
  XPResFix(fWordDetails);
//  XPResFix(fExamples);
  XPResFix(fTranslate);
  XPResFix(fClipboard);
  timproc:=now-tim;
  tim:=now;
  SwitchLanguage(curlang);
  fKanjiSearch.RadioGroup1.ItemIndex:=sortset;
  kanji_othersearch:=otherset;
  fKanjiSearch.ComboBox1.ItemIndex:=-1;
  if dictmodeset=1 then fUser.SpeedButton2.Down:=true else fUser.SpeedButton1.Down:=true;
  if userset then fKanjiCompounds.SpeedButton8.Down:=true else fKanjiCompounds.SpeedButton9.Down:=true;
  timdict:=now-tim;
{  showmessage('Beg:'+formatdatetime('hh:nn:ss.zzz',timbeg)+#13+
              'Load:'+formatdatetime('hh:nn:ss.zzz',timload)+#13+
              'User:'+formatdatetime('hh:nn:ss.zzz',timuser)+#13+
              'Proc:'+formatdatetime('hh:nn:ss.zzz',timproc)+#13+
              'Dict:'+formatdatetime('hh:nn:ss.zzz',timdict)+#13);}
  fSplash.Hide;
{  if ((Screen.Width<1024) or (Screen.Height<768)) then
  begin
    fMenu.Constraints.MinHeight:=80;
    fMenu.Constraints.MaxHeight:=80;
    fMenu.Height:=80;
    fMenu.Image1.Top:=0;
  end;}
  aBorders.Checked:=true;
  proposedlayout:=0;
  borderchange:=false;
//  if (FileExists('wakan.lay')) and (setlayout=0) then ReadLayout('wakan.lay') else
//  begin
//    proposedlayout:=setlayout;
//    timer2.enabled:=true;
//  end;
//  StandardLayout(0,100);
  curdisplaymode:=0;
  FormPlacement1.RestoreFormPlacement;
  if paramstr(1)='makeexamples'then
  begin
    fWords.Button1Click(self);
    exit;
  end;
  if paramstr(1)='makedic'then
  begin
    assignfile(t,paramstr(2));
    reset(t);
    readln(t,s);
    fDictImport.Edit1.Text:=s;
    readln(t,s);
    fDictImport.Edit2.Text:=s;
    readln(t,s);
    fDictImport.Edit3.Text:=s;
    readln(t,s);
    if s='C'then fDictImport.RadioGroup2.ItemIndex:=1 else fDictImport.RadioGroup2.ItemIndex:=0;
    readln(t,s);
    fDictImport.RadioGroup1.ItemIndex:=strtoint(s);
    readln(t,s);
    fDictImport.Edit4.Text:=s;
    readln(t,s);
    fDictImport.Edit5.Text:=s;
    readln(t,s);
    if s='Y'then fDictImport.CheckBox1.Checked:=true else fDictImport.CheckBox1.Checked:=false;
    readln(t,s);
    if s='Y'then fDictImport.CheckBox2.Checked:=true else fDictImport.CheckBox2.Checked:=false;
    readln(t,s);
    if s='Y'then fDictImport.CheckBox3.Checked:=true else fDictImport.CheckBox3.Checked:=false;
    while not eof(t) do
    begin
      readln(t,s);
      fDictImport.ListBox1.Items.Add(s);
    end;
    fDictImport.BitBtn1Click(self);
    Application.Terminate;
    exit;
  end;
  fMenu.ToggleForm(fKanjiCompounds,fKanji.SpeedButton3,fMenu.aKanjiCompounds);
  fMenu.ToggleForm(fWordKanji,fUser.SpeedButton6,fMenu.aDictKanji);
  fMenu.ToggleForm(fExamples,fUser.SpeedButton9,fMenu.aDictAdd);
  fMenu.ToggleForm(fUserDetails,fWords.SpeedButton4,fMenu.aUserDetails);
  fMenu.ToggleForm(fUserFilters,fWords.SpeedButton2,fMenu.aUserSettings);
//  fMenu.ToggleForm(fKanjiDetails,fKanji.SpeedButton2,fMenu.aKanjiDetails);
  fMenu.ToggleForm(fKanjiSearch,fKanji.SpeedButton5,fMenu.aKanjiSearch);
  displaymode:=setlayout;
  CharDetNowDocked:=false;
  if (setwindows and 128<>128) and (CharDetDocked) then aKanjiDetails.Checked:=true;
  ChangeDisplay;
  if setwindows and 1<>1 then fMenu.ToggleForm(fKanjiSearch,fKanji.SpeedButton5,fMenu.aKanjiSearch);
  if setwindows and 2<>2 then fMenu.ToggleForm(fKanjiCompounds,fKanji.SpeedButton3,fMenu.aKanjiCompounds);
  if setwindows and 4<>4 then fMenu.ToggleForm(fWordKanji,fUser.SpeedButton6,fMenu.aDictKanji);
  if setwindows and 8<>8 then fMenu.ToggleForm(fExamples,fUser.SpeedButton9,fMenu.aDictAdd);
  if setwindows and 16=16 then fMenu.ToggleForm(fExamples,fWords.SpeedButton1,fMenu.aUserExamples);
  if setwindows and 32<>32 then fMenu.ToggleForm(fUserDetails,fWords.SpeedButton4,fMenu.aUserDetails);
  if setwindows and 64<>64 then fMenu.ToggleForm(fUserFilters,fWords.SpeedButton2,fMenu.aUserSettings);
  if (setwindows and 128=128) and (not CharDetDocked) then fMenu.ToggleForm(fKanjiDetails,fKanji.SpeedButton2,fMenu.aKanjiDetails);
  fTranslate.SpeedButton20.Down:=fKanji.SpeedButton2.Down;
  screenTipShown:=false;
  fUser.ChangeFile(false);
  if (fSettings.CheckBox61.Checked) and (fUser.docfilename<>'') then
  begin
    try
      fUser.OpenFile;
    except end;
  end;

 { Init clipboard viewer }
  CbNextViewer := SetClipboardViewer(Self.Handle);

  initdone:=true;

  Timer1.Enabled:=true;
  Timer1Timer(Sender);
//  fKanji.Show;
end;

procedure TfMenu.SpeedButton9Click(Sender: TObject);
procedure SetBorder(form:Tform;bs:TFormBorderStyle);
var bch,bcw,bw,bh,rw,rh:integer;
    sf,sfw:boolean;
    cx1,cx2,cy1,cy2:integer;
begin
  bch:=form.ClientHeight;
  bcw:=form.ClientWidth;
  bw:=form.Width;
  bh:=form.Height;
  cx1:=form.Constraints.minWidth;
  cx2:=form.Constraints.MaxWidth;
  cy1:=form.Constraints.minHeight;
  cy2:=form.Constraints.maxHeight;
  sf:=((form.Constraints.minHeight=form.Constraints.MaxHeight) and
    (form.Constraints.minHeight>0)) or (bs=bsToolWindow);
  sfw:=((form.Constraints.minWidth=form.Constraints.MaxWidth) and
    (form.Constraints.minWidth>0)) or (bs=bsToolWindow);
  form.Constraints.minWidth:=0;
  form.Constraints.maxWidth:=0;
  form.Constraints.minheight:=0;
  form.Constraints.maxheight:=0;
  if not aBorders.Checked then
  begin
    form.BorderStyle:=bsNone;
  end else
  begin
    form.BorderStyle:=bs;
  end;
  if sf then form.ClientHeight:=bch;
  if sfw then form.ClientWidth:=bcw;
  rh:=form.Height-bh;
  rw:=form.Width-bw;
  if cx1>0 then form.Constraints.minWidth:=cx1+rw;
  if cx2>0 then form.Constraints.maxWidth:=cx2+rw;
  if cy1>0 then form.Constraints.minHeight:=cy1+rh;
  if cy2>0 then form.Constraints.maxHeight:=cy2+rh;
end;
begin
//  if not aBorders.Checked then DockExpress(nil,false);
//  showmessage('bef');
  borderchange:=true;
  SetBorder(fMenu,bsSizeToolWin);
  SetBorder(fKanji,bsSizeToolWin);
  SetBorder(fWords,bsSizeToolWin);
  SetBorder(fUser,bsSizeToolWin);
//  SetBorder(fUserAdd,bsSizeToolWin);
//  SetBorder(fUserCategory,bsSizeToolWin);
//  SetBorder(fUserFilters,bsToolWindow);
//  SetBorder(fUserDetails,bsToolWindow);
  SetBorder(fKanjiDetails,bsToolWindow);
//  SetBorder(fKanjiSort,bsToolWindow);
//  SetBorder(fKanjiSearch,bsSizeToolWin);
//  SetBorder(fKanjiCompounds,bsSizeToolWin);
//  SetBorder(fWordDetails,bsSizeToolWin);
//  SetBorder(fExamples,bsSizeToolWin);
//  SetBorder(fWordCategory,bsToolWindow);
//  SetBorder(fWordKanji,bsToolWindow);
  SetBorder(fTranslate,bsSizeToolWin);
//  SetBorder(fClipboard,bsSizeToolWin);
  SetBorder(fStrokeOrder,bsToolWindow);
//  if aBorders.Checked then SpeedButton16.Down:=true;
//  if aBorders.Checked then DockExpress(nil,true);
//  showmessage('aft');
  borderchange:=false;
end;

procedure TfMenu.Timer1Timer(Sender: TObject);
begin
//  SpeedButton9Click(self);
//  timer1.enabled:=false;
  Clipboard_Timer1Timer(sender);
end;

procedure TfMenu.SpeedButton11Click(Sender: TObject);
begin
  fWords.DoStatistic;
end;

procedure TfMenu.SpeedButton12Click(Sender: TObject);
begin
  Close;
end;

procedure TfMenu.SpeedButton14Click(Sender: TObject);
begin
  SwitchLanguage('j');
end;

procedure TfMenu.SpeedButton13Click(Sender: TObject);
begin
  SwitchLanguage('c');
end;

procedure TfMenu.Image1Click(Sender: TObject);
begin
  fSplash.ProgressBar1.Hide;
  fSplash.BitBtn1.Show;
  fSplash.ShowModal;
end;

procedure TfMenu.SpeedButton15Click(Sender: TObject);
begin
  if FileExists('wakan_'+curGUILanguage+'.chm') then
    ShellExecute(fMenu.handle,nil,pchar('wakan_'+curGUILanguage+'.chm'),nil,nil,SW_SHOW) else
  if FileExists('wakan.chm') then
    ShellExecute(fMenu.handle,nil,'wakan.chm',nil,nil,SW_SHOW) else
  if FileExists('wakan_bld.chm') then
  begin
    Application.MessageBox(pchar(_l('#00363^eHelp file is under construction, the information may be inaccurate.'+
      '^cNa n·povÏdÏ se pracuje, v souËasnÈm stavu mohou b˝t nÏkterÈ informace nep¯esnÈ.')),
      pchar(_l('#00364^eNotice^cUpozornÏnÌ')),MB_ICONWARNING or MB_OK);
    ShellExecute(fMenu.handle,nil,'wakan_bld.chm',nil,nil,SW_SHOW);
  end else if FileExists('wakan_en.chm') then
    Application.MessageBox(pchar(_l('#00365^eHelp file is out of date. Please download new help file from WaKan website: wakan.manga.cz.'+
      '^cSoubor n·povÏdy je zastaral˝. St·hnÏte si prosÌm novou n·povÏdu ze str·nky wakan.manga.cz.')),
      pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK) else
    Application.MessageBox(pchar(_l('#00366^eCannot find file WAKAN.CHM.'+
      '^cNemohu nalÈzt soubor WAKAN.CHM.')),
      pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
end;

procedure TfMenu.Action1Execute(Sender: TObject);
begin
  fUser.SpeedButton2.Down:=true;
end;

procedure TfMenu.SpeedButton16Click(Sender: TObject);
begin
  StandardLayout((sender as TComponent).tag-1,100);
end;

procedure TfMenu.PaintBox3Paint(Sender: TObject);
begin
  Clipboard_PaintBox3Paint(sender);
end;

procedure TfMenu.SpeedButton22Click(Sender: TObject);
begin
  Clipboard_SpeedButton7Click(sender);
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
var a:pointer;
begin
  fWords.DoStatistic;
end;

procedure TfMenu.aExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfMenu.aKanjiExecute(Sender: TObject);
begin
  displaymode:=1;
  ChangeDisplay;
end;

procedure TfMenu.aDictExecute(Sender: TObject);
begin
  displaymode:=2;
  ChangeDisplay;
end;

procedure TfMenu.aUserExecute(Sender: TObject);
begin
  displaymode:=5;
  ChangeDisplay;
end;

procedure TfMenu.aKanjiSearchExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aKanjiSearch.Checked;
  if not fKanji.Visible then ToggleForm(fKanji,nil,nil);
  if aKanjiSearch.Checked<>pre then exit;
  ToggleForm(fKanjiSearch,fKanji.SpeedButton5,aKanjiSearch);
end;

procedure TfMenu.aKanjiSortExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aKanjiSort.Checked;
  if not fKanji.Visible then ToggleForm(fKanji,nil,nil);
  if aKanjiSort.Checked<>pre then exit;
  ToggleForm(fKanjiSort,fKanji.SpeedButton1,aKanjiSort);
end;

procedure TfMenu.aKanjiDetailsExecute(Sender: TObject);
begin
  if CharDetDocked then
  begin
    if (curdisplaymode=2) or (curdisplaymode=5) then
    begin
      fMenu.aKanjiDetails.Checked:=false;
      fKanji.SpeedButton2.Down:=false;
      fTranslate.SpeedButton20.Down:=false;
      CharDetDocked:=false
    end else
    begin
      if curdisplaymode=1 then
        CharDetDockedVis1:=not CharDetDockedVis1 else
        CharDetDockedVis2:=not CharDetDockedVis2;
      ChangeDisplay;
    end;
  end;
  if not CharDetDocked then
  begin
    fMenu.ToggleForm(fKanjiDetails,fKanji.SpeedButton2,fMenu.aKanjiDetails);
    fTranslate.SpeedButton20.Down:=fKanji.SpeedButton2.Down;
    fKanjiDetails.FormPlacement1.RestoreFormPlacement;
  end;
end;

procedure TfMenu.aKanjiCompoundsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aKanjiCompounds.Checked;
  if not fKanji.Visible then ToggleForm(fKanji,nil,nil);
  if aKanjiCompounds.Checked<>pre then exit;
  ToggleForm(fKanjiCompounds,fKanji.SpeedButton3,aKanjiCompounds);
end;

procedure TfMenu.aKanjiPrintExecute(Sender: TObject);
begin
  fKanji.Button1Click(sender);
end;

procedure TfMenu.aDictDetailsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictDetails.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictDetails.Checked<>pre then exit;
  ToggleForm(fWordDetails,fUser.SpeedButton5,aDictDetails);
end;

procedure TfMenu.aDictKanjiExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictKanji.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictKanji.Checked<>pre then exit;
  ToggleForm(fWordKanji,fUser.SpeedButton6,aDictKanji);
end;

procedure TfMenu.aDictCategoriesExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictCategories.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictCategories.Checked<>pre then exit;
  ToggleForm(fWordCategory,fUser.SpeedButton7,aDictCategories);
end;

procedure TfMenu.aDictAddExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictAdd.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictAdd.Checked<>pre then exit;
  ToggleForm(fExamples,fUser.SpeedButton9,aDictAdd);
end;

procedure TfMenu.aDictEditorExecute(Sender: TObject);
var pre:boolean;
begin
  displaymode:=3;
  ChangeDisplay;
end;

procedure TfMenu.aUserAddExecute(Sender: TObject);
begin
  if fWords.Visible then
    fWords.Button2Click(Sender) else
  if fUser.Visible then
    fUser.SpeedButton17Click(Sender) else
  if fKanjiCompounds.Visible then
    fKanjiCompounds.SpeedButton17Click(Sender) else
end;

procedure TfMenu.aUserSettingsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserSettings.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserSettings.Checked<>pre then exit;
  ToggleForm(fUserFilters,fWords.SpeedButton2,aUserSettings);
end;

procedure TfMenu.aUserCategoryExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserCategory.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserCategory.Checked<>pre then exit;
  ToggleForm(fUserCategory,fWords.SpeedButton3,aUserCategory);
end;

procedure TfMenu.aUserDetailsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserDetails.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserDetails.Checked<>pre then exit;
  ToggleForm(fUserDetails,fWords.SpeedButton4,aUserDetails);
end;

procedure TfMenu.aUserPrintExecute(Sender: TObject);
begin
  fWords.Button15Click(sender);
end;

procedure TfMenu.aUserGenerateExecute(Sender: TObject);
begin
  fWords.Button19Click(sender);
end;

procedure TfMenu.aSettingsExecute(Sender: TObject);
begin
  SpeedButton5Click(Sender);
end;

procedure TfMenu.aChangeLanguageExecute(Sender: TObject);
begin
  fLanguage.ShowModal;
end;

procedure TfMenu.aSettingsDictExecute(Sender: TObject);
begin
  SpeedButton8Click(sender);
end;

procedure TfMenu.aQuick1Execute(Sender: TObject);
begin
  StandardLayout(0,100);
end;

procedure TfMenu.aQuick2Execute(Sender: TObject);
begin
  StandardLayout(1,100);
end;

procedure TfMenu.aQuick3Execute(Sender: TObject);
begin
  StandardLayout(2,100);
end;

procedure TfMenu.aQuick4Execute(Sender: TObject);
begin
  StandardLayout(3,100);
end;

procedure TfMenu.aQuick5Execute(Sender: TObject);
begin
  StandardLayout(4,100);
end;

procedure TfMenu.aQuick6Execute(Sender: TObject);
begin
  StandardLayout(5,100);
end;

procedure TfMenu.aBordersExecute(Sender: TObject);
begin
  aBorders.Checked:=not aBorders.Checked;
  SpeedButton9Click(Sender);
end;

procedure TfMenu.aHelpExecute(Sender: TObject);
begin
  SpeedButton15Click(Sender);
end;

procedure TfMenu.aAboutExecute(Sender: TObject);
begin
  Image1Click(Sender);
end;

procedure TfMenu.aJapaneseExecute(Sender: TObject);
begin
  SpeedButton14Click(Sender);
end;


procedure TfMenu.aChineseExecute(Sender: TObject);
begin
  SpeedButton13Click(Sender);
end;

procedure TfMenu.aEditorNewExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton3Click(sender);
end;

procedure TfMenu.aEditorOpenExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton1Click(sender);
end;

procedure TfMenu.aEditorSaveExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton2Click(sender);
end;

procedure TfMenu.aEditorSaveAsExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fUser.Translate_SaveAs;
end;

procedure TfMenu.aEditorCutExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton4Click(sender);
end;

procedure TfMenu.aEditorCopyExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton14Click(sender);
end;

procedure TfMenu.aEditorPasteExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton15Click(sender);
end;

procedure TfMenu.aEditorSelectAllExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fUser.Translate_SelectAll;
end;

procedure TfMenu.aEditorKanjiModeExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton6Click(sender);
end;

procedure TfMenu.aEditorKanaModeExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton7Click(sender);
end;

procedure TfMenu.aEditorASCIIModeExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton5Click(sender);
end;

procedure TfMenu.aEditorReadingExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton8.Down:=not fTranslate.SpeedButton8.Down;
  fTranslate.SpeedButton8Click(sender);
end;

procedure TfMenu.aEditorMeaningExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton9.Down:=not fTranslate.SpeedButton9.Down;
  fTranslate.SpeedButton9Click(sender);
end;

procedure TfMenu.aEditorClearExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton10Click(sender);
end;

procedure TfMenu.aEditorFillExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton11Click(sender);
end;

procedure TfMenu.aEditorSetExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton12Click(sender);
end;

procedure TfMenu.aEditorPrintExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton13Click(sender);
end;

procedure TfMenu.aKanjiAllExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.SpeedButton18Click(Sender);
end;

procedure TfMenu.aKanjiLearnedExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.SpeedButton1.Down:=not fKanjiSearch.SpeedButton1.Down;
  fKanjiSearch.SpeedButton4Click(Sender);
end;

procedure TfMenu.aKanjiCommonExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.SpeedButton2.Down:=not fKanjiSearch.SpeedButton2.Down;
  fKanjiSearch.SpeedButton4Click(Sender);
end;

procedure TfMenu.aKanjiClipboardExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.SpeedButton3.Down:=not fKanjiSearch.SpeedButton3.Down;
  fKanjiSearch.SpeedButton4Click(Sender);
end;

procedure TfMenu.aKanjiPinYinExecute(Sender: TObject);
begin
  if (not fKanji.Visible) then aKanjiExecute(Sender);
  if (not fKanjiSearch.Visible) then aKanjiSearchExecute(Sender);
  fKanjiSearch.Edit1.SetFocus;
end;

procedure TfMenu.aKanjiYomiExecute(Sender: TObject);
begin
  if (not fKanji.Visible) then aKanjiExecute(Sender);
  if (not fKanjiSearch.Visible) then aKanjiSearchExecute(Sender);
  fKanjiSearch.Edit6.SetFocus;
end;

procedure TfMenu.aKanjiRadicalExecute(Sender: TObject);
begin
  if (not fKanji.Visible) then aKanjiExecute(Sender);
  if (not fKanjiSearch.Visible) then aKanjiSearchExecute(Sender);
  fKanjiSearch.SpeedButton8Click(Sender);
end;

procedure TfMenu.aKanjiAddClipboardExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiDetails.SpeedButton23Click(Sender);
end;

procedure TfMenu.aKanjiSetLearnedExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiDetails.SpeedButton21Click(Sender);
end;

procedure TfMenu.aKanjiFullDetailsExecute(Sender: TObject);
begin
  if not fKanjiDetails.Visible then aKanjiDetailsExecute(Sender);
  fKanjiDetails.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictJapaneseExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton1.Down:=true;
//  fUser.Edit1.Text:='';
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictEnglishExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton2.Down:=true;
//  fUser.Edit1.Text:='';
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictClipboardExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton3.Down:=true;
//  fUser.Edit1.Text:='';
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictAddClipboardExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton23Click(Sender);
end;

procedure TfMenu.aDictExactExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton10.Down:=true;
  dictbeginset:=0;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictBeginningExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton11.Down:=true;
  dictbeginset:=1;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictEndExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton12.Down:=true;
  dictbeginset:=2;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aKanjiWindowExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanji.DrawGrid1.SetFocus;
end;

procedure TfMenu.aKanjiMeaningExecute(Sender: TObject);
begin
  if not fKanjiSearch.Visible then aKanjiSearchExecute(Sender);
  fKanjiSearch.Edit3.SetFocus;
end;

procedure TfMenu.aEditorWindowExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.ListBox1.SetFocus;
end;

procedure TfMenu.aEditorSmallFontExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton16.Down:=true;
  fTranslate.SpeedButton16Click(sender);
end;

procedure TfMenu.aEditorLargeFontExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton17.Down:=true;
  fTranslate.SpeedButton17Click(sender);
end;

procedure TfMenu.aEditorMedFontExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SpeedButton18.Down:=true;
  fTranslate.SpeedButton18Click(sender);
end;

procedure TfMenu.Timer2Timer(Sender: TObject);
begin
  timer2.enabled:=false;
  StandardLayout(proposedlayout,100);
end;

procedure TfMenu.tab3Click(Sender: TObject);
begin
  ToggleForm(fTranslate,nil,nil);
end;

procedure TfMenu.FixConstraints(form:TForm;w,h:integer);
begin
  if (form.Constraints.MinHeight>0) then form.Constraints.MinHeight:=form.Constraints.MinHeight+h;
  if (form.Constraints.MinWidth>0) then form.Constraints.MinWidth:=form.Constraints.MinWidth+w;
  if (form.Constraints.MaxHeight>0) then form.Constraints.MaxHeight:=form.Constraints.MaxHeight+h;
  if (form.Constraints.MaxWidth>0) then form.Constraints.MaxWidth:=form.Constraints.MaxWidth+w;
end;

procedure TfMenu.DockExpress(form:TForm;dock:boolean);
procedure DockProc(slave,host:TForm;panel:TPanel;dir:integer);
var adocked:boolean;
    vert:boolean;
    fixsiz,flomin,clisiz:integer;
    rect:TRect;
    bch,bcw:integer;
    smw,smh,sxw,sxh:integer;
    simulshow:boolean;
begin
  adocked:=slave.tag>=2;
  simulshow:=slave.tag=0;
  if adocked=dock then exit;
  vert:=(dir=1) or (dir=3);
  bch:=slave.Height;
  bcw:=slave.Width;
  if vert then fixsiz:=slave.ClientHeight else fixsiz:=slave.ClientWidth;
  if vert then flomin:=slave.ClientWidth else flomin:=slave.ClientHeight;
{  if (vert) and (slave.Constraints.MaxWidth<>0) then showmessage('INTERNAL ERROR: Fixed Slave constraints!');
  if (not vert) and (slave.Constraints.MinWidth<>0) then showmessage('INTERNAL ERROR: Fixed Slave constraints!');
  if (dock) and (vert) then host.Constraints.MinHeight:=host.Constraints.MinHeight+fixsiz;
  if (dock) and (not vert) then host.Constraints.MinWidth:=host.Constraints.MinWidth+fixsiz;
  if (not dock) and (vert) then host.Constraints.MinHeight:=host.Constraints.MinHeight-fixsiz;
  if (not dock) and (not vert) then host.Constraints.MinWidth:=host.Constraints.MinWidth-fixsiz;}
  if (dock) then if (vert) then panel.height:=fixsiz else panel.width:=fixsiz;
  smw:=slave.Constraints.MinWidth;
  smh:=slave.Constraints.MinHeight;
  sxw:=slave.Constraints.MaxWidth;
  sxh:=slave.Constraints.MaxHeight;
  if not vert then slave.Width:=panel.width else slave.Height:=panel.Height;
  if dock then
  begin
    slave.Show;
    slave.Hide;
  end;
{  slave.Constraints.MinWidth:=0;
  slave.Constraints.MaxWidth:=0;
  slave.Constraints.MinHeight:=0;
  slave.Constraints.MaxHeight:=0;}
  if dock then slave.ManualDock(panel);
  slave.ClientHeight:=bch;
  if (dock) then if (vert) then panel.height:=fixsiz else panel.width:=fixsiz;
  if dock then slave.align:=alClient else slave.align:=alNone;
  rect.left:=0;
  rect.top:=0;
  if vert then rect.right:=flomin else rect.right:=fixsiz;
  if vert then rect.bottom:=fixsiz else rect.bottom:=flomin;
  if not dock then slave.ManualFloat(rect);
  if (not dock) then if (vert) then panel.height:=0 else panel.width:=0;
  if dock then slave.tag:=2;
  if not dock then slave.tag:=0;
  if not dock then slave.hide;
{  if smw>0 then slave.Constraints.MinWidth:=smw+slave.Width-bcw;
  if smh>0 then slave.Constraints.MinHeight:=smh+slave.Height-bch;
  if sxw>0 then slave.Constraints.MaxWidth:=sxw+slave.Width-bcw;
  if sxh>0 then slave.Constraints.MaxHeight:=sxh+slave.Height-bch;         }
//  if (dock) and (slave.tag=2) and (simulshow) then DockExpress(form,false);
end;
begin
  if (form=fKanjiSearch) or (form=nil) then DockProc(fKanjiSearch,fKanji,fKanji.Panel3,1);
  if (form=fKanjiCompounds) or (form=nil) then DockProc(fKanjiCompounds,fKanji,fKanji.Panel2,3);
  if ((form=fExamples) or (form=nil)) and (curdisplaymode<>5) then DockProc(fExamples,fUser,fUser.Panel2,3);
  if ((form=fExamples) or (form=nil)) and (curdisplaymode=5) then DockProc(fExamples,fWords,fWords.Panel5,3);
  if (form=fWordKanji) or (form=nil) then DockProc(fWordKanji,fUser,fUser.Panel3,2);
  if (form=fUserFilters) or (form=nil) then DockProc(fUserFilters,fWords,fWords.Panel2,2);
  if (form=fUserDetails) or (form=nil) then DockProc(fUserDetails,fWords,fWords.Panel4,3);
end;

procedure TfMenu.Button1Click(Sender: TObject);
var t:textfile;
    gb:array[0..96*96-1] of word;
    big5:array[0..96*160-1] of word;
    gbs,b5s:string;
    i1,i2:integer;
    s:string;
begin
  for i1:=0 to 96*96-1 do gb[i1]:=0;
  for i1:=0 to 96*160-1 do big5[i1]:=0;
  TChar.First;
  while not TChar.EOF do
  begin
    b5s:=GetCharValue(TChar.Int(TCharIndex),51);
    gbs:=GetCharValue(TChar.Int(TCharIndex),54);
    if b5s<>'' then
    begin
      i1:=strtoint('0x'+copy(b5s,1,2))-160;
      i2:=strtoint('0x'+copy(b5s,3,4));
      if i2>$a0 then i2:=i2-160 else i2:=i2-64;
      big5[i1*160+i2]:=strtoint('0x'+TChar.Str(TCharUnicode));
    end;
    if gbs<>'' then
    begin
      i1:=strtoint(copy(gbs,1,2));
      i2:=strtoint(copy(gbs,3,4));
      gb[i1*96+i2]:=strtoint('0x'+TChar.Str(TCharUnicode));
    end;
    TChar.Next;
  end;
  assignfile(t,'table.dat');
  rewrite(t);
  s:='  ';
  for i1:=0 to 96*96-1 do
  begin
    s:=s+'$'+lowercase(Format('%4.4x',[gb[i1]]))+',';
    if i1 mod 10=9 then begin writeln(t,s); s:='  '; end;
  end;
  writeln(t,s);
  writeln(t,'');
  s:='  ';
  for i1:=0 to 96*160-1 do
  begin
    s:=s+'$'+lowercase(Format('%4.4x',[big5[i1]]))+',';
    if i1 mod 10=9 then begin writeln(t,s); s:='  '; end;
  end;
  writeln(t,s);
  closefile(t);
end;

procedure TfMenu.Button2Click(Sender: TObject);
begin
  DockExpress(nil,false);
end;

procedure TfMenu.FormResize(Sender: TObject);
begin
//  PaintBox3.Visible:=Width>815;
//  Shape9.Visible:=Width>815;
//  SpeedButton22.Visible:=Width>815;
//  Bevel5.Visible:=Width>815;
end;

procedure TfMenu.tab4Click(Sender: TObject);
begin
  ToggleForm(fKanjiDetails,nil,nil);
end;

procedure TfMenu.FormUndock(form:TForm);
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

procedure TfMenu.ChangeDisplay;
begin
  case curdisplaymode of
    1:FormUndock(fKanji);
    2:fUser.Hide;
    5:fWords.Hide;
    3:fTranslate.Hide;
    4:begin
        fUser.Hide;
        fTranslate.Hide;
      end;
  end;
  if CharDetNowDocked then FormUndock(fKanjiDetails);
  CharDetNowDocked:=false;
  Panel2.Height:=0;
  Panel4.Width:=0;
  aMode1.Checked:=false;
  aMode2.Checked:=false;
  aMode3.Checked:=false;
  aMode4.Checked:=false;
  aMode5.Checked:=false;
  if CharDetDocked then
  begin
    if displaymode=1 then
    begin
      if CharDetDockedVis1 then
      begin
        Panel4.Width:=fKanjiDetails.Width;
        fKanjiDetails.ManualDock(Panel4);
        fKanjiDetails.Align:=alClient;
        fKanjiDetails.Show;
      end;
      fKanji.SpeedButton2.Down:=CharDetDockedVis1;
      aKanjiDetails.Checked:=CharDetDockedVis1;
      CharDetNowDocked:=CharDetDockedVis1;
    end;
    if (displaymode=4) or (displaymode=3) then
    begin
      if CharDetDockedVis2 then
      begin
        Panel4.Width:=fKanjiDetails.Width;
        fKanjiDetails.ManualDock(Panel4);
        fKanjiDetails.Align:=alClient;
        fKanjiDetails.Show;
      end;
      fTranslate.SpeedButton20.Down:=CharDetDockedVis2;
      aKanjiDetails.Checked:=CharDetDockedVis2;
      CharDetNowDocked:=CharDetDockedVis2;
    end;
  end;
  if fExamples.visible then
  begin
    fExamples.hide;
    DockExpress(fExamples,false);
    fExamples.tag:=0;
  end;
  case displaymode of
    1:begin
        fKanji.Width:=Panel3.ClientWidth;
        fKanji.Height:=Panel3.ClientHeight;
        fKanji.ManualDock(Panel3);
        fKanji.Align:=alClient;
        fKanji.Show;
        tab1.Down:=true;
        try
        fKanji.DrawGrid1.SetFocus;
        except end;
        aMode1.Checked:=true;
      end;
    2:begin
        fUser.Width:=Panel3.ClientWidth;
        fUser.Height:=Panel3.ClientHeight;
        fUser.ManualDock(Panel3);
        fUser.Align:=alClient;
        fUser.Show;
        tab2.Down:=true;
        aMode2.Checked:=true;
      end;
    5:begin
        fWords.Width:=Panel3.ClientWidth;
        fWords.Height:=Panel3.ClientHeight;
        fWords.ManualDock(Panel3);
        fWords.Align:=alClient;
        fWords.Show;
        tab5.Down:=true;
        aMode5.Checked:=true;
      end;
    3:begin
        fTranslate.Width:=Panel3.ClientWidth;
        fTranslate.Height:=Panel3.ClientHeight;
        fTranslate.ManualDock(Panel3);
        fTranslate.Align:=alClient;
        fTranslate.Show;
        tab3.Down:=true;
        fTranslate.SpeedButton19.Down:=false;
        aMode3.Checked:=true;
      end;
    4:begin
        Panel2.height:=250;
        fUser.Width:=Panel2.ClientWidth;
        fUser.Height:=Panel2.ClientHeight;
        fUser.ManualDock(Panel2);
        fUser.Align:=alClient;
        fUser.Show;
        fTranslate.ManualDock(Panel3);
        fTranslate.Align:=alClient;
        fTranslate.Show;
        fTranslate.Width:=Panel3.ClientWidth;
        fTranslate.Height:=Panel3.ClientHeight;
        tab3.Down:=true;
        fTranslate.SpeedButton19.Down:=true;
        aMode3.Checked:=true;
      end;
  end;
  curdisplaymode:=displaymode;
  fKanjiDetails.FormShow(fMenu);
  if (((curdisplaymode=2) or (displaymode=4)) and (aDictAdd.Checked)) or
     ((curdisplaymode=5) and (aUserExamples.Checked)) then
  begin
    fExamples.tag:=1;
    DockExpress(fExamples,true);
    fExamples.show;
    fExamples.tag:=2;
  end;
end;

procedure TfMenu.TabControl1Change(Sender: TObject);
begin
  if tab1.Down then displaymode:=1;
  if tab2.Down then displaymode:=2;
  if tab3.Down and not fTranslate.SpeedButton19.Down then displaymode:=3;
  if tab3.Down and fTranslate.SpeedButton19.Down then displaymode:=4;
  if tab5.Down then displaymode:=5;
  ChangeDisplay;
end;

procedure TfMenu.eCharacterlistcSeznamznak1Click(Sender: TObject);
begin
  displaymode:=1;
  ChangeDisplay;
end;

procedure TfMenu.eDictionarycSlovnk2Click(Sender: TObject);
begin
  displaymode:=2;
  ChangeDisplay;
end;

procedure TfMenu.eTexteditorcTextoveditor1Click(Sender: TObject);
begin
  displaymode:=3;
  ChangeDisplay;
end;

procedure TfMenu.aMode1Execute(Sender: TObject);
begin
  displaymode:=1;
  ChangeDisplay;
end;

procedure TfMenu.aMode2Execute(Sender: TObject);
begin
  displaymode:=2;
  ChangeDisplay;
end;

procedure TfMenu.aMode3Execute(Sender: TObject);
begin
  if fTranslate.SpeedButton19.Down then displaymode:=4 else displaymode:=3;
  ChangeDisplay;
end;

procedure TfMenu.aMode4Execute(Sender: TObject);
begin
  displaymode:=4;
  ChangeDisplay;
end;

procedure TfMenu.aMode5Execute(Sender: TObject);
begin
  displaymode:=5;
  ChangeDisplay;
end;

procedure TfMenu.SpeedButton1Click(Sender: TObject);
begin
  if SpeedButton1.Down then
  begin
    if not FileExists('wakanh.dll') then
    begin
      Application.MessageBox(pchar(_l('#00367^eCannot find file WAKANH.DLL.^cNemohu najÌt soubor WAKANH.DLL.')),
        pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
      exit;
    end;
{    if Application.MessageBox(pchar(_l(
      '#00368^e!!THIS FEATURE IS EXPERIMENTAL!!'#13#13'WARNING: Screen popup tool is currently under heavy testing.'+
      #13'Because it needs to install a system-wide hook DLL, you are advised to save all your'+
      #13'critical work before continuing. Please report to me any system unstability you encounter.'#13#13'Do you want to continue?'+
      '^c!!TAKO FUNKCE JE SILNÃ EXPERIMENT¡LNÕ!!'#13#13'UPOZORNÃNÕ: VyskakovacÌ popup pro obrazovku je nynÌ testov·n.'+
      #13'Protoûe je pot¯eba nainstalovat hook DLL do celÈho systÈmu, doporuËuji uloûit'+
      #13'vöechnu rozdÏlanou pr·ci v jin˝ch aplikacÌch p¯ed pokraËov·nÌm. Nahlaste mi prosÌm, pokud narazÌte na nestabilitu systÈmu.'#13#13'Chcete pokraËovat?')),
      pchar(_l('#00090^eWarning^cVarov·nÌ')),MB_ICONWARNING or MB_YESNO)=idNo then
      begin
        SpeedButton1.Down:=false;
        exit;
      end;}
    Screen.Cursor:=crHourGlass;
    ctlFileMap:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,1,'wakanh_ctl_sharemem');
    if ctlFileMap=0 then
    begin
      showmessage('Win32 API error: CreateFileMap() failed.');
      exit;
    end;
    ptrFileMap:=MapViewOfFile(ctlFileMap,FILE_MAP_WRITE,0,0,1);
    if ptrFileMap=nil then
    begin
      showmessage('Win32 API error: MapViewOfFile() failed.');
      exit;
    end;
    byte(ptrFileMap^):=0;
{MCH    InjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
    Screen.Cursor:=crDefault;
    screenModeSc:=true;
  end else
  begin
    screenModeSc:=false;
    Screen.Cursor:=crHourGlass;
    UnMapViewOfFile(ptrFileMap);
    CloseHandle(ctlFileMap);
{MCH    UninjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfMenu.ShowScreenTip(x,y:integer;s:string;wt:integer;immediate:boolean);
var maxwords,maxwordss:integer;
    wasfull:boolean;
    s1,s2,s3,s4:string;
    ss:string;
    ch,kch:integer;
    rect:TRect;
    kkch,kkcw:integer;
    vsiz,hsiz,vfsiz,hfsiz:integer;
    i:integer;
    sep:integer;
    tpp:integer;
    optwidth,cw:integer;
    proposeds:string;
    maxslen,slen:integer;
begin
  SetScreenTipBlock(0,0,0,0,nil);
  intorgPaint:=nil;
  intorgGrid:=nil;
  if ((wt=7) and (not fSettings.CheckBox47.Checked)) then exit;
  if ((wt<7) and (not fSettings.CheckBox28.Checked)) then exit;
  fScreenTip:=TfScreenTip.Create(nil);
  screenTipShown:=true;
  screenTipList:=TStringList.Create;
  maxwords:=strtoint(fSettings.Edit24.Text);
  if maxwordss<10 then maxwordss:=10;
  if wt=7 then
  begin
    DicSearch(HexToUnicode(s),2,mtExactMatch,false,7,maxwordss,screenTipList,5,wasfull);
    if (screenTipList.Count=0) then
    begin
      ss:=HexToUnicode(s);
      if (length(ss)>2) and (copy(ss,length(ss)-1,2)='ed') then delete(ss,length(ss)-1,2) else
        if (length(ss)>1) and (ss[length(ss)]='s') then delete(ss,length(ss),1);
      DicSearch(ss,2,mtExactMatch,false,7,maxwordss,screenTipList,5,wasfull);
    end;
  end;
  if wt<7 then
    DicSearch(s,4,mtExactMatch,false,wt,maxwordss,screenTipList,5,wasfull);
  if maxwords>screenTipList.Count then maxwords:=screenTipList.Count;
  screenTipWords:=maxwords;
  tpp:=20;
  ch:=GridFontSize+3;
  kch:=strtoint(fSettings.Edit26.Text);
  optwidth:=0;
  proposeds:='';
  maxslen:=0;
  for i:=0 to maxwords-1 do
  begin
    ss:=screenTipList[i];
    slen:=strtoint(copy(ss,18,2));
    if slen>maxslen then maxslen:=slen;
    delete(ss,1,30);
    SplitWord(ss,s1,s2,s3,s4);
    rect.left:=0;
    rect.right:=Screen.Width;
    rect.top:=0;
    rect.bottom:=100;
    //TODO: replace with remexcl?
    if (length(s1)>0) and (s1[1]=ALTCH_EXCL) then delete(s1,1,2);
    if (length(s2)>0) and (s2[1]=ALTCH_EXCL) then delete(s2,1,2);
    if (length(s1)>0) and (s1[1]=UH_UNKNOWN_KANJI) then delete(s1,1,1);
    if (length(s2)>0) and (s2[1]=UH_UNKNOWN_KANJI) then delete(s2,1,1);
    cw:=DrawWordInfo(fScreenTip.pb.Canvas,rect,false,false,2,s3,false,true,GridFontSize,true)+GridFontSize*(3+length(s1+s2) div 4);
    if cw>optwidth then optwidth:=cw;
  end;
  if maxslen>0 then proposeds:=copy(s,1,maxslen*4);
  vsiz:=5;
  hsiz:=20;
  optwidth:=optwidth-5*kch;
  optwidth:=optwidth div kch;
  if optwidth<strtoint(fSettings.Edit27.Text) then optwidth:=strtoint(fSettings.Edit27.Text);
  if optwidth>strtoint(fSettings.Edit28.Text) then optwidth:=strtoint(fSettings.Edit28.Text);
  ScreenTipWidth:=optwidth;
  hsiz:=optwidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*kch;
  kkch:=vfsiz*kch;
  screenTipText:=s;
  if proposeds<>'' then screenTipText:=proposeds;
  screenTipWt:=wt;
  fScreenTip.Left:=x;
  fScreenTip.Top:=y;
  fScreenTip.Width:=kkcw+sep*2+1;
  if (wt=1) and (fSettings.CheckBox48.Checked) then
    fScreenTip.Height:=maxwords*ch+sep*3+kkch+tpp else fScreenTip.Height:=maxwords*ch+sep*2+1+tpp;
  if not immediate then
  begin
    if y+fScreenTip.Height>Screen.Height then fScreenTip.Top:=y-20-fScreenTip.Height;
    if x+fScreenTip.Width>Screen.Width then fScreenTip.Left:=Screen.Width-fScreenTip.Width;
  end;
  SetWindowPos(fScreenTip.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  screenTipButton:=0;
  PopupMouseMove(Mouse.CursorPos.x-fScreenTip.left,Mouse.CursorPos.y-fScreenTip.Top);
end;

procedure TfMenu.DrawPopupButtons(sel:integer);
var i:integer;
    rect:TRect;
    s1,s2:string;
begin
  for i:=0 to PopupButtonNum-1 do
  begin
    fScreenTip.pb.Canvas.Pen.Color:=clBlack;
    if sel=i+1 then fScreenTip.pb.Canvas.Brush.Color:=clBlack else fScreenTip.pb.Canvas.Brush.Color:=clWhite;
    rect.left:=2+PopupButtonWidth*i+PopupButtonSep*(i+1);
    rect.right:=rect.left+PopupButtonWidth;
    rect.top:=3;
    rect.bottom:=22;
    fScreenTip.pb.Canvas.Rectangle(rect);
    case i of
      0:s1:='#00869^eClip^cSchr';
      1:s1:='#00869^eClip^cSchr';
      2:s1:='#00870^eShow^cUkaû';
      3:s1:='#00870^eShow^cUkaû';
    end;
    case i of
      0:s2:='#00871^eAdd^cCP¯id';
      1:s2:='#00872^eRepl^cP¯ep';
      2:s2:='#00873^eDict^cSlov';
      3:s2:='#00874^eChar^cZnak';
    end;
    rect.left:=rect.left+1;
    rect.top:=rect.top+1;
    rect.right:=rect.right-1;
    rect.bottom:=rect.bottom-1;
    if sel=i+1 then fScreenTip.pb.Canvas.Font.Color:=clWhite else fScreenTip.pb.Canvas.Font.Color:=clBlack;
    fScreenTip.pb.Canvas.Font.Name:='Arial';
    fScreenTip.pb.Canvas.Font.Height:=9;
    fScreenTip.pb.Canvas.Font.Style:=[];
    fScreenTip.pb.Canvas.TextRect(rect,rect.left+1,rect.top,_l(s1));
    rect.top:=rect.top+9;
    fScreenTip.pb.Canvas.TextRect(rect,rect.left+1,rect.top,_l(s2));
  end;
end;

procedure TfMenu.PopupMouseMove(x,y:integer);
var sb:integer;
begin
  x:=x-2-PopupButtonSep;
  sb:=(x div (PopupButtonWidth+PopupButtonSep))+1;
  if (sb>PopupButtonNum) or (y<3) or (y>22) or (x mod (PopupButtonWidth+PopupButtonSep)>PopupButtonWidth) then
    sb:=0;
  if sb<>screenTipButton then
  begin
    screenTipButton:=sb;
    DrawPopupButtons(screenTipButton);
  end;
end;

procedure TfMenu.PopupMouseUp(button:TMouseButton;shift:TShiftState;x,y:integer);
begin
  if fRadical.Visible then exit;
  PopupMouseMove(x,y);
  if screenTipButton=0 then exit;
  if (screenTipButton>2) and (not fMenu.Focused) then fMenu.Show;
  case screenTipButton of
    1:begin
        clip:=clip+screenTipText;
        ChangeClipboard;
      end;
    2:begin
        clip:=screenTipText;
        ChangeClipboard;
      end;
    3:begin
        clip:=screenTipText;
        ChangeClipboard;
        if not fRadical.Visible then aDictClipboard.Execute;
      end;
    4:begin
        if fRadical.Visible then exit;
        if not fKanjiDetails.Visible then aKanjiDetails.Execute;
        fKanji.SetCharDetails(fcopy(screenTipText,1,1));
      end;
  end;
end;

procedure TfMenu.PopupImmediate(left:boolean);
begin
  screenTipImmediate:=true;
  ScreenTimerTimer(nil);
  screenTipImmediate:=false;
end;

procedure TfMenu.PaintScreenTip;
var sl:TStringList;
    maxwords,maxwordss:integer;
    wasfull:boolean;
    ss:string;
    ch,kch:integer;
    rect:TRect;
    kkch,kkcw:integer;
    vsiz,hsiz,vfsiz,hfsiz:integer;
    i:integer;
    s:string;
    wt:integer;
    sep:integer;
    tpp:integer;
    cl:TColor;
    FontJpCh:string;
begin
  s:=screenTipText;
  wt:=screenTipWt;
  sl:=screenTipList;
  cl:=Col('Popup_Back');
  maxwords:=strtoint(fSettings.Edit24.Text);
  if maxwords>sl.Count then maxwords:=sl.Count;
  maxwords:=screenTipWords;
  tpp:=20;
  ch:=GridFontSize+3;
  kch:=strtoint(fSettings.Edit26.Text);
  vsiz:=5;
  hsiz:=ScreenTipWidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*kch;
  kkch:=vfsiz*kch;
  fScreenTip.Width:=kkcw+sep*2;
  fScreenTip.pb.Canvas.Brush.Color:=cl;
  fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
  fScreenTip.pb.Canvas.Rectangle(0,0,fScreenTip.Width,fScreenTip.Height);
//  fScreenTip.pb.Canvas.Brush.Color:=cl;
//  fScreenTip.pb.Canvas.Pen.Color:=clBlack;
//  fScreenTip.pb.Canvas.Rectangle(sep,sep,fScreenTip.Width-sep,tpp+1+sep);
  DrawPopupButtons(screenTipButton);
  fScreenTip.pb.Canvas.Brush.Color:=cl;
  fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
  DrawUnicode(fScreenTip.pb.Canvas,sep+2+PopupButtonNum*PopupButtonWidth+PopupButtonNum*PopupButtonSep+sep,sep,tpp-4,s,FontSmall);
  fScreenTip.pb.Canvas.Rectangle(sep,sep+tpp,fScreenTip.Width-sep,sep+maxwords*ch+tpp);
  if curlang='c'then FontJpCh:=FontChinese else FontJpCh:=FontJapanese;
  for i:=0 to maxwords-1 do
  begin
    ss:=sl[i];
    delete(ss,1,30);
    rect.left:=sep+1;
    rect.right:=fScreenTip.Width-sep-2;
    rect.top:=sep+ch*i+1+tpp;
    rect.bottom:=sep+ch*i+ch+tpp;
    DrawPackedWordInfo(fScreenTip.pb.Canvas,rect,ss,ch,true);
    fScreenTip.pb.Canvas.MoveTo(sep+1,sep+ch*i+ch+tpp);
    fScreenTip.pb.Canvas.LineTo(fScreenTip.Width-sep-1,sep+ch*i+ch+tpp);
  end;
  fScreenTip.pb.canvas.Font.Style:=[];
  fScreenTip.pb.canvas.Font.Color:=Col('Popup_Text');
  if (wt=1) and (fSettings.CheckBox48.Checked) then
  begin
    fScreenTip.pb.Canvas.Brush.Color:=Col('Popup_Card');
    fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
    fScreenTip.pb.Canvas.Rectangle(sep,sep*2+maxwords*ch+tpp,fScreenTip.Width-sep,sep*2+maxwords*ch+kkch+tpp);
    fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
    DrawKanjiCard(fScreenTip.pb.Canvas,copy(s,1,4),sep,sep*2+maxwords*ch+tpp,
    kch,false,false,true,true,true,true,true,true,false,true,true,hsiz,vsiz,2,FontJpCh);
    fScreenTip.pb.Canvas.Brush.Color:=Col('Popup_Text');
    fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
    rect.left:=sep;
    rect.top:=tpp+sep*2+maxwords*ch;
    rect.right:=fScreenTip.Width-sep;
    rect.bottom:=fScreenTip.Height-sep;
    fScreenTip.pb.Canvas.FrameRect(rect);
  end;
  fScreenTip.pb.Canvas.Brush.Color:=Col('Popup_Text');
  fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
  rect.left:=sep;
  rect.top:=tpp+sep;
  rect.right:=fScreenTip.Width-sep;
  rect.bottom:=tpp+sep+maxwords*ch+1;
  fScreenTip.pb.Canvas.FrameRect(rect);
  fScreenTip.pb.Canvas.Font.Height:=12;
  fScreenTip.pb.Canvas.Font.Name:='Arial';
  fScreenTip.pb.Canvas.Brush.Color:=clWhite;
  fScreenTip.pb.Canvas.Pen.Color:=clBlack;
//  DrawUnicode(fScreenTip.pb.Canvas,7,fScreenTip.Height-20,12,screenTipDebug,FontSmall);
end;

procedure TfMenu.HideScreenTip;
begin
  fScreenTip.Hide;
  fScreenTip.Free;
  screenTipList.Free;
  screenTipShown:=false;
end;

procedure IPCCallback(name: pchar; messageBuf : pointer; messageLen : dword;
                      answerBuf  : pointer; answerLen  : dword); stdcall;
var mycnt:integer;
begin
  if messageLen<>sizeof(TTextInfo) then exit;
  inc(rdcnt);
  if rdcnt>90 then exit;
  mycnt:=rdcnt;
  move(messageBuf^,curtext[mycnt],sizeof(TTextInfo));
end;

procedure BitCallback(name: pchar; messageBuf : pointer; messageLen : dword;
                      answerBuf  : pointer; answerLen  : dword); stdcall;
var mycnt:integer;
begin
  if messageLen<>sizeof(TBitInfo) then exit;
  inc(bitcnt);
  if bitcnt>90 then exit;
  mycnt:=bitcnt;
  move(messageBuf^,curbit[mycnt],sizeof(TBitInfo));
end;

procedure TfMenu.ScreenTimerTimer(Sender: TObject);
procedure wwadd(bg,en:integer;w:word);
var b:integer;
    i,j,k:integer;
    ass:boolean;
begin
  b:=0;
  for i:=0 to ftextpos-1 do if (ftextend[i]=en) and (ftextbeg[i]=bg) then
  begin end else
  begin
    ftext[b]:=ftext[i];
    ftextbeg[b]:=ftextbeg[i];
    ftextend[b]:=ftextend[i];
    inc(b);
  end;
  ftextpos:=b;
  ass:=false;
  for i:=0 to ftextpos-1 do if (not ass) and (ftextbeg[i]>bg) then
  begin
    for j:=ftextpos-1 downto i do
    begin
      ftext[j+1]:=ftext[j];
      ftextbeg[j+1]:=ftextbeg[j];
      ftextend[j+1]:=ftextend[j];
    end;
    ftext[i]:=w;
    ftextbeg[i]:=bg;
    ftextend[i]:=en;
    inc(ftextpos);
    ass:=true;
  end;
  if not ass then
  begin
    ftext[ftextpos]:=w;
    ftextbeg[ftextpos]:=bg;
    ftextend[ftextpos]:=en;
    inc(ftextpos);
    ass:=true;
  end;
end;
function EvChar(i:integer):integer;
begin
  if ((i>=ord('a')) and (i<=ord('z'))) or
     ((i>=ord('A')) and (i<=ord('Z'))) then
     begin
       result:=7;
       exit;
     end;
  if (i>=$3040) and (i<=$309F) then
  begin
    result:=2;
    exit;
  end;
  if (i>=$30A0) and (i<=$30FF) then
  begin
    result:=3;
    exit;
  end;
  if (i>=$4E00) and (i<=$9FFF) then
  begin
    result:=1;
    exit;
  end;
  result:=0;
end;
var pt:TPoint;
    s,s2:string;
    i,j:integer;
    b:byte;
    wbg,wen:integer;
    lp:TPoint;
    cx:integer;
    last:integer;
    wtp:integer;
    k:integer;
    gbg:array[0..255] of integer;
    gen:array[0..255] of integer;
    wp:WINDOWPLACEMENT;
    ct:TTextInfo;
    ev,cev:integer;
    ttim,tleft,tright:integer;
    wnd:HWnd;
    wt:shortstring;
    wr:TRect;
    savedx:array[1..100] of integer;
    savedy:array[1..100] of integer;
    evc:integer;
    rx,ry:integer;
    gc:TGridCoord;
    rect:TRect;
    wtt:integer;
    curt:TDateTime;
begin
  if not initdone then exit;
  curt:=now-lastautosave;
  if curt>1/24/60*strtoint(fSettings.Edit29.Text) then
  begin
    lastautosave:=now;
    SaveUserData;
  end;
  if not screenModeWk and not screenModeSc and not screenTipImmediate and not popcreated then exit;
  if inproc then exit;
  inproc:=true;
  try
    pt:=Mouse.CursorPos;
  except inproc:=false; exit; end;
  try
  if not TryStrToInt(fSettings.Edit21.Text, ttim) then ttim := 10;
  if not TryStrToInt(fSettings.Edit22.Text, tleft) then tleft:=10;
  if not TryStrToInt(fSettings.Edit23.Text, tright) then tright:=100;

  if (popcreated) and (pt.x>=fScreenTip.Left-10) and
     (pt.y>=fScreenTip.Top-10) and (pt.x<=fScreenTip.Left+fScreenTip.Width+10) and
     (pt.y<=fScreenTip.Top+fScreenTip.Height+10) then
  begin
    inproc:=false;
     exit;
  end;
  if ((pt.x<>oldpt.x) or (pt.y<>oldpt.y)) and (not screenTipImmediate) then
  begin
    if popcreated then
    begin
      fMenu.HideScreenTip;
      popcreated:=false;
    end;
    oldpt:=pt;
    tim:=0;
    inproc:=false;
    exit;
  end;
//  Label1.Caption:=datetimetostr(now)+' <'+inttostr(tim)+'> '+inttostr(pt.x)+'-'+inttostr(pt.y);
  if tim<>-1 then inc(tim);
  if (tim<ttim) and (not screenTipImmediate) then begin
    inproc:=false;
    exit;
  end;
  ftextpos:=0;
  s:='';
  if (screenModeWk) or (screenTipImmediate) then
  begin
    if ((intmoPaint<>nil) or (intmoGrid<>nil)) and (pt.x=intmosx) and (pt.y=intmosy) then
    begin
      s:=intcurString;
    end;
    begpt:=pt;
    if length(s)>=4 then evc:=EvChar(word(HexToUnicode(copy(s,1,4))[1]));
  end;
  if (s='') and screenModeSc then
  begin
    fInvalidator:=TfInvalidator.Create(nil);
    fInvalidator.Width:=tleft+tright;
    fInvalidator.Height:=1;
    fInvalidator.Top:=pt.y;
    fInvalidator.Left:=pt.x-tleft;
    for i:=1 to 100 do curtext[i].slen:=0;
    rdcnt:=0;
    bitcnt:=0;
{MCH    CreateIPCQueue('texthook_data',IPCCallback);
    CreateIPCQueue('texthook_bit',BitCallback);}
    byte(ptrFileMap^):=1;
    SetWindowPos(fInvalidator.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    fInvalidator.Hide;
    fInvalidator.Free;
    sleep(100);
    byte(ptrFileMap^):=0;
{MCH    DestroyIPCQueue('texthook_data');
    DestroyIPCQueue('texthook_bit');}
    begpt:=pt;
    s:='';
    s2:='';
    ftextpos:=0;
    if rdcnt>90 then rdcnt:=90;
    if bitcnt>90 then bitcnt:=90;
    // create debug
    for i:=1 to rdcnt do
    begin
      ct:=curtext[i];
      savedx[i]:=ct.x;
      savedy[i]:=ct.y;
      if (ct.dcinfo and (TA_TOP or TA_BOTTOM or TA_BASELINE))=TA_BOTTOM then ct.y:=ct.y-ct.h;
      if (ct.dcinfo and (TA_TOP or TA_BOTTOM or TA_BASELINE))=TA_BASELINE then ct.y:=ct.y-ct.h;
      if (ct.dcinfo and (TA_RIGHT or TA_LEFT or TA_CENTER))=TA_CENTER then ct.x:=ct.x-ct.w div 2;
      if (ct.dcinfo and (TA_LEFT or TA_RIGHT or TA_CENTER))=TA_RIGHT then ct.x:=ct.x-ct.w;
      curtext[i]:=ct;
    end;
    wnd:=WindowFromPoint(pt);
    wt[0]:=AnsiChar(chr(Windows.GetWindowText(wnd,@(wt[1]),255)));
    s2:=s2+UnicodeToHex('Window name: '+wt)+'000D000A';
    Windows.GetWindowRect(wnd,wr);
    s2:=s2+UnicodeToHex('Window rect: ['+inttostr(wr.Left)+':'+
      inttostr(wr.Top)+']-['+inttostr(wr.Right)+':'+inttostr(wr.Bottom)+']')+'000D000A';
    Windows.GetClientRect(wnd,wr);
    s2:=s2+UnicodeToHex('Client area: '+inttostr(wr.Right)+':'+inttostr(wr.Bottom)+'')+'000D000A';
    s2:=s2+UnicodeToHex('Cursor pos: '+inttostr(pt.x)+':'+inttostr(pt.y))+'000D000A';
    s2:=s2+'000D000A'+UnicodeToHex('BitBlts:')+'000D000A';
    for i:=1 to bitcnt do
    begin
      s2:=s2+UnicodeToHex(inttostr(i)+'# Mod:');
      for j:=1 to rdcnt do if curbit[i].srcdc=curtext[j].hdc then
      begin
        s2:=s2+UnicodeToHex(inttostr(j)+';');
        curtext[j].hwnd:=curbit[i].hwnd;
        curtext[j].x:=curtext[j].x+curbit[i].xofs;
        curtext[j].y:=curtext[j].y+curbit[i].yofs;
    //    s:=s+'T+'+inttostr(curbit[i].xofs)+'='+inttostr(curbit[i].yofs)+'+';
      end;
      s2:=s2+UnicodeToHex(' Ofs:'+inttostr(curbit[i].xofs)+':'+inttostr(curbit[i].yofs))+'000D000A';
    end;
    s2:=s2+'000D000A'+UnicodeToHex('TextOuts:')+'000D000A';
    for i:=1 to rdcnt do
    begin
      lp.x:=curtext[i].x;
      lp.y:=curtext[i].y;
      Windows.GetWindowRect(wnd,wr);
      lp.x:=lp.x+wr.Left;
      lp.y:=lp.y+wr.Top;
      s2:=s2+UnicodeToHex(inttostr(i)+'# "');
      for j:=0 to curtext[i].slen-1 do if curtext[i].str[j]>=32 then s2:=s2+UnicodeToHex(widechar(curtext[i].str[j]));
      s2:=s2+UnicodeToHex('"'+
        ' Org:'+inttostr(savedx[i])+':'+inttostr(savedy[i])+
        ' Align:'+inttostr(curtext[i].x)+':'+inttostr(curtext[i].y)+
        ' Trans:'+inttostr(lp.x)+':'+inttostr(lp.y)+
        ' Size:'+inttostr(curtext[i].w)+':'+inttostr(curtext[i].h)+' ');
      if curtext[i].hwnd=wnd then s2:=s2+UnicodeToHex('OK') else s2:=s2+UnicodeToHex('BAD WND');
      s2:=s2+'000D000A';
    end;
    for i:=1 to rdcnt do if (curtext[i].slen>0) and (curtext[i].hwnd=wnd) then
    begin
      wbg:=0;
      lp.x:=curtext[i].x;
      lp.y:=curtext[i].y;
      Windows.GetWindowRect(wnd,wr);
      lp.x:=lp.x+wr.Left;
      lp.y:=lp.y+wr.Top;
      if (lp.y<=pt.y) and (lp.x+curtext[i].w>=pt.x) and (lp.y+curtext[i].h>=pt.y) then
      begin
        cx:=lp.x;
        wen:=-1;
        for j:=0 to curtext[i].slen-1 do
        begin
          gbg[j]:=cx;
          gen[j]:=cx+curtext[i].len[j];
  //        s2:=s2+chr(curtext[i].str[j] mod 256);
          if wen=-1 then
          begin
            if (curtext[i].str[j]<ord('A')) or ((curtext[i].str[j]>ord('Z')) and
                (curtext[i].str[j]<ord('a'))) or (curtext[i].str[j]>ord('z')) then
                begin wbg:=j+1; wtp:=2; end else wtp:=1;
          end;
          if (cx+curtext[i].len[j]>pt.x) and (wen=-1) then
          begin
            if wbg>j then wbg:=j;
            if wbg=-1 then wbg:=0;
            wen:=wbg;
            if wtp=1 then while (wen+1<curtext[i].slen) and
              (((curtext[i].str[wen+1]>=ord('a')) and (curtext[i].str[wen+1]<=ord('z'))) or
               ((curtext[i].str[wen+1]>=ord('A')) and (curtext[i].str[wen+1]<=ord('Z')))) do inc(wen);
            if wtp=2 then wen:=wbg+10;
            if wen>=curtext[i].slen then wen:=curtext[i].slen-1;
          end;
          cx:=cx+curtext[i].len[j];
        end;
        if wen<>-1 then for k:=wbg to wen do
          wwadd(gbg[k],gen[k],curtext[i].str[k]);
        if wtp=1 then wwadd(gen[wen],gen[wen],32);
      end;
    end;
    cev:=0;
    cx:=-100;
    last:=0;
    if (ftextpos>0) and (ftextbeg[0]<=pt.x+2) then
    for i:=0 to ftextpos-1 do
    begin
      if cev=0 then cev:=EvChar(ftext[i]);
      ev:=EvChar(ftext[i]);
      if (cev=ev) or ((cev=1) and (ev=2)) then
      begin if (ev<>0) and ((ftext[i]<>last) or (ftextbeg[i]>cx+2)) then s:=s+UnicodeToHex(widechar(ftext[i]))
      end else break;
      cx:=ftextbeg[i];
      last:=ftext[i];
      cev:=EvChar(ftext[i]);
    end;
    if s<>'' then evc:=EvChar(ftext[0]);
  end;
  screenTipDebug:=s2;
  if paramstr(1)='debug'then
  begin
    clip:=s2;
    ChangeClipboard;
  end;
  if s<>'' then
  begin
    if screenTipImmediate then fMenu.ShowScreenTip(pt.x-10,pt.y-10,s,evc,true) else
      fMenu.ShowScreenTip(pt.x+10,pt.y+10,s,evc,false);
    if screenTipShown then popcreated:=true;
  end;
  tim:=-1;
  finally
  inproc:=false;
  end;
end;


{
IntTipPaintOver(), IntTipGridOver()
Various controls from all over the program call these on mouse move,
to determine which characters are under mouse cursor right now.
This information is used only by ScreenTimerTimer (showing popup tip),
so we could have calculated it there, but we'd need to figure out the class
of control mouse is over.
}
procedure TfMenu.IntTipPaintOver(p:TPaintBox;x,y:integer;leftDown:boolean);
begin
  if leftDown and ((intorgGrid<>nil) or (intorgPaint<>p)) then
  begin
    intorgcx:=x; intorgcy:=y; intorgsx:=Mouse.CursorPos.x; intorgsy:=Mouse.CursorPos.y;
    intorgPaint:=p; intorgGrid:=nil;
  end;
  //TODO: Why are we doing this through common vars? Does anyone else use these?
  //  Maybe make them into params?
  intmocx:=x; intmocy:=y; intmosx:=Mouse.CursorPos.x; intmosy:=Mouse.CursorPos.y;
  intmopaint:=p;
  intmogrid:=nil;
  CalculateCurString;
end;

procedure TfMenu.IntTipGridOver(sg:TDrawGrid;x,y:integer;leftDown:boolean);
begin
  if leftDown and ((intorgGrid<>sg) or (intorgPaint<>nil)) then
  begin
    intorgcx:=x; intorgcy:=y; intorgsx:=Mouse.CursorPos.x; intorgsy:=Mouse.CursorPos.y;
    intorgPaint:=nil; intorgGrid:=sg;
  end;
  intmocx:=x; intmocy:=y; intmosx:=Mouse.CursorPos.x; intmosy:=Mouse.CursorPos.y;
  intmopaint:=nil;
  intmogrid:=sg;
  CalculateCurString;
end;

procedure TfMenu.CalculateCurString;
var s,s2:string;
    rx,ry,wtt:integer;
    x1,y1,x2,y2:integer;
    gc,gc2:TGridCoord;
    rect:TRect;
    mox1,mox2:integer;
    mo:boolean;
begin
  SetScreenTipBlock(0,0,0,0,nil);
  if (intmoPaint<>nil) and (intmoPaint<>fTranslate.PaintBox6) then
  begin
    s:=FindDrawReg(intmoPaint,intmocx,intmocy,x1,y1,y2);
    if (intorgPaint=intmoPaint) then
    begin
      s2:=FindDrawReg(intorgPaint,intorgcx,intorgcy,x2,y1,y2);
      if length(s2)>length(s) then
      begin
        s2:=s;
        x2:=x1;
        s:=FindDrawReg(intorgPaint,intorgcx,intorgcy,x1,y1,y2);
      end;
      if copy(s,length(s)-length(s2)+1,length(s2))=s2 then
      begin
        s:=copy(s,1,length(s)-length(s2));
        SetScreenTipBlock(x1,y1,x2,y2,intmoPaint.Canvas);
      end;
    end;
  end else
  if intmoPaint=fTranslate.PaintBox6 then
  begin
    fUser.CalcMouseCoords(intmocx,intmocy,rx,ry);
    if ry<>-1 then s:=fUser.GetDocWord(rx,ry,wtt,false);
  end else
  begin
    gc:=intmoGrid.MouseCoord(intmocx,intmocy);
    if (intmoGrid<>fKanji.DrawGrid1) and (intmoGrid<>fRadical.DrawGrid1) then
    begin
      if (gc.x>=0) and (gc.x<2) and (gc.y>0) then
      begin
        if intorgGrid=intmoGrid then
        begin
          gc2:=intmoGrid.MouseCoord(intorgcx,intorgcy);
          mo:=(gc2.x=gc.x) and (gc2.y=gc.y);
        end else mo:=false;
        s:=(TStringGrid(intmoGrid)).Cells[gc.x,gc.y];
        if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
        if (length(s)>2) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
        if (length(s)>1) and (s[1]=ALTCH_SHARP) then delete(s,1,1);
        if (length(s)>1) and (s[1]=ALTCH_AT) then delete(s,1,1);
        rect:=intmoGrid.CellRect(gc.x,gc.y);
        if (length(s)>0) and (s[1]=UH_UNKNOWN_KANJI) then delete(s,1,1);
        if not mo then fdelete(s,1,((intmocx-rect.left-2) div GridFontSize));
        if mo then
        begin
          if intorgcx>intmocx then
          begin
            mox1:=intmocx;
            mox2:=intorgcx;
          end else
          begin
            mox1:=intorgcx;
            mox2:=intmocx;
          end;
          mox1:=(mox1-rect.left-2) div GridFontSize;
          mox2:=(mox2-rect.left-2) div GridFontSize;
          s:=fcopy(s,mox1,(mox2-mox1));
          SetScreenTipBlock(mox1*GridFontSize+rect.left+2,rect.top,mox2*GridFontSize+rect.left+2,rect.bottom,intmoGrid.Canvas);
        end;
      end;
    end else if intmoGrid=fKanji.DrawGrid1 then
      s:=fKanji.GetKanji(gc.x,gc.y) else s:=fRadical.GetKanji(gc.x,gc.y);
  end;
  intcurString:=s;
end;


procedure TfMenu.PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfMenu.eSavekanatranscriptcUloitpepisdokany1Click(
  Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  Application.MessageBox(pchar(_l('#00369^eDo not forget to fill kana readings or use the auto-fill function before using this feature.'+
    '^cNezapomeÚte p¯ed pouûitÌm tÈto funkce vyplnit kana ËtenÌ anebo pouûijte funkci pro automatick˝ p¯eklad.')),
    pchar(_l('#00364^eNotice^cUpozornÏnÌ')),MB_ICONINFORMATION or MB_OK);
  if SaveDialog1.Execute then
    fUser.SaveToFile(SaveDialog1.FileName,Conv_ChooseType(false,0),true);
end;

procedure TfMenu.aDictInflectExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton4.Down:=not fUser.SpeedButton4.Down;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictAutoExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton13.Down:=not fUser.SpeedButton13.Down;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictGroup1Execute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton14.Down:=true;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictGroup2Execute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton15.Down:=true;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictGroup3Execute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton16.Down:=true;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.aDictVoc1Execute(Sender: TObject);
begin
{  if not fUser.Visible then aDictExecute(Sender);
  fExamples.SpeedButton1.Down:=true;}
end;

procedure TfMenu.aDictVoc2Execute(Sender: TObject);
begin
{  if not fUser.Visible then aDictExecute(Sender);
  fExamples.SpeedButton2.Down:=true;}
end;

procedure TfMenu.aDictVoc3Execute(Sender: TObject);
begin
{  if not fUser.Visible then aDictExecute(Sender);
  fExamples.SpeedButton3.Down:=true;}
end;

procedure TfMenu.aUserExamplesExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserExamples.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserExamples.Checked<>pre then exit;
  ToggleForm(fExamples,fWords.SpeedButton1,aUserExamples);
end;

procedure TfMenu.aEditorColorsExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  aEditorColors.Checked:=not aEditorColors.Checked;
  fTranslate.SpeedButton21.Down:=aEditorColors.Checked;
  mustrepaint:=true;
  fUser.ShowText(true);
end;

function _l(id:string):string;
begin
  result:=fLanguage.TranslateString(id);
end;

procedure TfMenu.PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfMenu.RebuildUserIndex;
begin
  TUserIdx.First;
  while not TUserIdx.EOF do
  begin
    if not TUser.Locate('Index',TUserIdx.Str(TUserIdxWord),true) then
      TUserIdx.Delete;
    TUserIdx.Next;
  end;
end;

procedure TfMenu.eSavecharacterstofilecUloitznakydosouboru1Click(
  Sender: TObject);
begin
  fKanji.SaveChars;
end;

procedure TfMenu.aDictMiddleExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton18.Down:=true;
  dictbeginset:=3;
  fUser.SpeedButton1Click(Sender);
end;

procedure TfMenu.RebuildAnnotations;
procedure WriteAnnotPackage;
begin
  PKGWriteForm.PKGWriteCmd('NotShow');
  PKGWriteForm.PKGWriteCmd('PKGFileName annotate.pkg');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan Compiled Annotations');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan Compiled Annotations');
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName (C) Filip K·brt 2005');
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by WaKan - Japanese & Chinese Learning Tool');
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 621030');
  PKGWriteForm.PKGWriteCmd('FileSysCode 587135');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  PKGWriteForm.PKGWriteCmd('CryptMode 0');
  PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode 0');
  PKGWriteForm.PKGWriteCmd('CryptCode 453267');
  PKGWriteForm.PKGWriteCmd('Include annot');
  PKGWriteForm.PKGWriteCmd('Finish');
end;
var t:textfile;
    chk,nchk:TStringList;
    s:string;
    sr:TSearchRec;
    bld:boolean;
    pd:TSMPromptForm;
    ps:TPackageSource;
    tt:TTextTable;
    ftp:byte;
    moded:boolean;
    curt,curd:string;
    dd:string;
begin
  chk:=TStringList.Create;
  nchk:=TStringList.Create;
  bld:=false;
  if FileExists('ANNOTATE.CHK') then chk.LoadFromFile('ANNOTATE.CHK');
  if FindFirst('*.ANO',faAnyFile,sr)=0 then
  repeat
    s:=sr.name+';'+inttostr(sr.size)+';'+inttostr(sr.time);
    if chk.IndexOf(s)=-1 then bld:=true;
    nchk.Add(s);
  until FindNext(sr)<>0;
  FindClose(sr);
  nchk.SaveToFile('ANNOTATE.CHK');
  chk.Free;
  nchk.Free;
  if not FileExists('ANNOTATE.PKG') then bld:=true;
  if bld then
  begin
    pd:=SMMessageDlg(_l('^eAnnotations^cPozn·mky'),_l('^eRebuilding annotations...^cNahr·v·m pozn·mky...'));
    {$I-}
    mkdir('annot');
    {$I+}
    ioresult;
    assignfile(t,'annot\Annot.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'xTag');
    writeln(t,'bUser');
    writeln(t,'sData');
    writeln(t,'$ORDERS');
    writeln(t,'Tag_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Tag');
    writeln(t,'$CREATE');
    closefile(t);
    WriteAnnotPackage;
    DeleteFile('annot\Annot.Info');
    ps:=TPackageSource.Create('annotate.pkg',621030,587135,453267);
    tt:=TTextTable.Create(ps,'annot',false,false);
    tt.Nocommitting:=true;
    if FindFirst('*.ANO',faAnyFile,sr)=0 then
    repeat
      ftp:=Conv_DetectType(sr.name);
      if ftp=0 then ftp:=98;
      Conv_Open(sr.name,ftp);
      s:=Conv_Read;
      if Uppercase(sr.name)='_USER.ANO' then dd:='1' else dd:='0';
      moded:=false;
      curd:=''; curt:='';
      while s<>'' do
      begin
        if s=UH_LF then
        begin
          if (curd<>'') and (curt<>'') and (copy(curt,1,4)<>UnicodeToHex('#')) then
            tt.Insert([curt,dd,HexToCombUni(curd)]);
          curd:='';
          curt:='';
          moded:=false;
        end else if (not moded) and ((s=UnicodeToHex(';')) or (s=UnicodeToHex(':'))) then
          moded:=true else if s<>UH_CR then
          if moded then curd:=curd+s else curt:=curt+s;
        s:=Conv_Read;
      end;
      Conv_Close;
    until FindNext(sr)<>0;
    tt.Nocommitting:=false;
    tt.ReIndex;
    tt.WriteTable('annot\Annot',true);
    WriteAnnotPackage;
{    DeleteFile('annot\Annot.info');
    DeleteFile('annot\Annot.data');
    DeleteFile('annot\Annot.struct');
    DeleteFile('annot\Annot.index');}
    {$I-}
    rmdir('annot');
    {$I+}
    ioresult;
    pd.Free;
  end;
end;

procedure TfMenu.LoadAnnotations;
var ps:TPackageSource;
begin
  try
    if not fileexists('annotate.pkg') then RebuildAnnotations;
    if not fileexists('annotate.pkg') then exit;
    ps:=TPackageSource.Create('annotate.pkg',621030,587135,453267);
    TAnnots:=TTextTable.Create(ps,'annot',false,false);
    ps.Free;
  except
    Application.MessageBox(pchar(_l('^eAnnotations file ANNOTATE.PKG is corrupt and wasn''t loaded.'#13'If you delete it, it will be recreated.'+
      '^cSoubor s anotacemi ANNOTATE.PKG je poökozen a nebyl nahr·n. Pokud ho smaûete, bude vytvo¯en znovu.')),pchar(_l('^eError^cChyba')),MB_ICONERROR or MB_OK);
  end;
end;

procedure TfMenu.AnnotSeek(des: array of string);
var i:integer;
    tagf,dataf:integer;
begin
  tagf:=TAnnots.Field('Tag');
  dataf:=TAnnots.Field('Data');
  curAnnot:='';
  for i:=0 to High(des) do
  begin
    if TAnnots=nil then exit;
    TAnnots.SetOrder('Tag_Ind');
    TAnnots.Locate('Tag',des[i],false);
    while (not TAnnots.EOF) and (TAnnots.Str(tagf)=des[i]) do
    begin
      if curAnnot<>'' then curAnnot:=curAnnot+',';
      curAnnot:=curAnnot+TAnnots.Str(dataf);
      TAnnots.Next;
    end;
  end;
  AnnotFirst;
end;

procedure TfMenu.AnnotFirst;
begin
  curAnnotPos:=1;
end;

function TfMenu.AnnotGet(cmd: char): string;
begin
  result:='';
  if curAnnotPos+1>length(curAnnot) then exit;
  while (curAnnotPos<length(curAnnot)) and
    ((upcase(curAnnot[curAnnotPos])<>upcase(cmd)) or (curAnnot[curAnnotPos+1]<>':')) do
    begin
      while (curAnnotPos<length(curAnnot)) and (curAnnot[curAnnotPos]<>',') do inc(curAnnotPos);
      inc(curAnnotPos);
    end;
  inc(curAnnotPos,2);
  if curAnnotPos>length(curAnnot) then exit;
  while (curAnnotPos<=length(curAnnot)) and (curAnnot[curAnnotPos]<>',') do
  begin
    result:=result+curAnnot[curAnnotPos];
    inc(curAnnotPos);
  end;
  inc(curAnnotPos);
end;

function TfMenu.AnnotGetAll(cmd: char; delimit: string): string;
var s:string;
begin
  AnnotFirst;
  result:='';
  s:=AnnotGet(cmd);
  while s<>'' do
  begin
    if result<>'' then result:=result+delimit;
    result:=result+s;
    s:=AnnotGet(cmd);
  end;
end;

function TfMenu.AnnotGetOne(cmd: char): string;
begin
  AnnotFirst;
  result:=AnnotGet(cmd);
end;

procedure TfMenu.AnnotSeekK(kanji, kana: string);
begin
  if (curlang<>'j') or (kana='') then
  begin
    if length(kanji)>4 then AnnotSeek([kanji]) else AnnotSeek([kanji,UnicodeToHex(UH_UNKNOWN_KANJI+kanji)]);
  end else
    AnnotSeek([kanji,kana,UnicodeToHex(KanaToRomaji(kana,1,'j')),kanji+UnicodeToHex('+')+kana]);
end;

function TfMenu.IsAnnot: boolean;
begin
  result:=TAnnots<>nil;
end;

procedure TfMenu.AnnotShowMedia(kanji, kana: string);
var s:string;
    b:boolean;
begin
  AnnotSeekK(kanji,kana);
  fMedia.media.Clear;
  fMedia.TabSet1.Tabs.Clear;
 // images
  AnnotFirst;
  s:=AnnotGet('I');
  while s<>'' do
  begin
    fMedia.media.Add(s);
    fMedia.TabSet1.Tabs.Add(inttostr(fMedia.TabSet1.Tabs.Count+1)+': '+copy(s,1,pos('.',s)-1));
    s:=AnnotGet('I');
  end;
  // pages
  AnnotFirst;
  s:=AnnotGet('W');
  while s<>'' do
  begin
    fMedia.media.Add('http://'+s);
    fMedia.TabSet1.Tabs.Add(inttostr(fMedia.TabSet1.Tabs.Count+1)+': '+s);
    s:=AnnotGet('W');
  end;
  if fMedia.media.Count>0 then
  begin
    fMedia.Show;
    fMedia.TabSet1.Visible:=fMedia.media.Count>1;
    fMedia.TabSet1.TabIndex:=0;
    fMedia.TabSet1Change(self,0,b);
    SetFocus;
  end else fMedia.Hide;
end;

initialization
  tim:=0;
  rdcnt:=0;
  inproc:=false;
  popcreated:=false;
  WakanVer := GetFileVersionInfoStr(GetModuleFilenameStr(0));

end.
