unit JWBSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RXCtrls, Buttons, ComCtrls, registry;

type
  TfSettings = class(TForm)
    pcPages: TPageControl;
    tsRomanization: TTabSheet;
    tsCharacterList: TTabSheet;
    RadioGroup3: TRadioGroup;
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
    CheckBox8: TCheckBox;
    tsWordListPrinting: TTabSheet;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    GroupBox4: TGroupBox;
    ListBox1: TListBox;
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
    CheckBox26: TCheckBox;
    Label23: TLabel;
    Edit16: TEdit;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    tsTextTranslator: TTabSheet;
    CheckBox44: TCheckBox;
    CheckBox45: TCheckBox;
    Button4: TButton;
    Label26: TLabel;
    Edit19: TEdit;
    GroupBox6: TGroupBox;
    Shape1: TShape;
    Label16: TLabel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    PaintBox1: TPaintBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Edit15: TEdit;
    GroupBox7: TGroupBox;
    Label27: TLabel;
    Shape3: TShape;
    PaintBox2: TPaintBox;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    RadioGroup6: TRadioGroup;
    RadioGroup7: TRadioGroup;
    Edit20: TEdit;
    Button5: TButton;
    tsCharacterDetails: TTabSheet;
    Label34: TLabel;
    ListBox2: TListBox;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label35: TLabel;
    ComboBox1: TComboBox;
    Button10: TButton;
    CheckBox46: TCheckBox;
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
    Label41: TLabel;
    Edit25: TEdit;
    Button11: TButton;
    CheckBox12: TCheckBox;
    CheckBox49: TCheckBox;
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
    cbNoColors: TCheckBox;
    CheckBox9: TCheckBox;
    ColorDialog1: TColorDialog;
    CheckBox51: TCheckBox;
    CheckBox52: TCheckBox;
    CheckBox53: TCheckBox;
    Label43: TLabel;
    Edit26: TEdit;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Edit27: TEdit;
    Edit28: TEdit;
    CheckBox54: TCheckBox;
    Edit29: TEdit;
    Label47: TLabel;
    CheckBox55: TCheckBox;
    Button13: TButton;
    Edit30: TEdit;
    Edit31: TEdit;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Edit32: TEdit;
    SpeedButton13: TSpeedButton;
    RadioGroup5: TRadioGroup;
    CheckBox57: TCheckBox;
    Label5: TLabel;
    SpeedButton5: TSpeedButton;
    Edit5: TEdit;
    Label51: TLabel;
    SpeedButton14: TSpeedButton;
    Edit33: TEdit;
    CheckBox58: TCheckBox;
    CheckBox59: TCheckBox;
    Edit34: TEdit;
    Label52: TLabel;
    tsEditor: TTabSheet;
    CheckBox60: TCheckBox;
    CheckBox61: TCheckBox;
    Label53: TLabel;
    CheckBox62: TCheckBox;
    CheckBox63: TCheckBox;
    Edit35: TEdit;
    tsAnnotations: TTabSheet;
    CheckBox64: TCheckBox;
    CheckBox65: TCheckBox;
    CheckBox66: TCheckBox;
    CheckBox67: TCheckBox;
    CheckBox68: TCheckBox;
    CheckBox69: TCheckBox;
    Bevel1: TBevel;
    Button16: TButton;
    CheckBox70: TCheckBox;
    cbNoSaveChangesWarning: TCheckBox;
    cbLoadAozoraRuby: TCheckBox;
    lblAozoraRuby: TLabel;
    cbAozoraTagsInColor: TCheckBox;
    lblSavingAndLoading: TLabel;
    cbSaveAnnotationsToRuby: TCheckBox;
    lblSaveAnnotationsToRubyDesc: TLabel;
    lblAozoraTagsInColor: TLabel;
    sbTextTranslator: TScrollBox;
    Label25: TLabel;
    GroupBox5: TGroupBox;
    Label24: TLabel;
    cbPrintReading: TCheckBox;
    cbPrintMeaning: TCheckBox;
    cbNoPrintColors: TCheckBox;
    cbVerticalPrint: TCheckBox;
    Edit18: TEdit;
    cbDisplayLines: TCheckBox;
    cbNoMeaningLearned: TCheckBox;
    cbNoSearchParticles: TCheckBox;
    cbNoReadingLearned: TCheckBox;
    CheckBox36: TCheckBox;
    cbNoTranslateHiragana: TCheckBox;
    cbUserBold: TCheckBox;
    Edit17: TEdit;
    CheckBox42: TCheckBox;
    CheckBox43: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox41: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox56: TCheckBox;
    cbTranslateNoLongTextWarning: TCheckBox;
    cbMultithreadedTranslation: TCheckBox;
    lbContents: TListBox;
    pnlButtons: TPanel;
    Button6: TButton;
    btnOk: TBitBtn;
    Label54: TLabel;
    cbAdjustCharPriorities: TCheckBox;
    rgReleaseCursorMode: TRadioGroup;
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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
    procedure PaintBox3Paint(Sender: TObject);
    procedure Edit15Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
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
    procedure Button13Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure lbContentsClick(Sender: TObject);
  public
    function CheckFontBool(s:string):boolean;
    function AutoDetectFonts:boolean;
    procedure ResetDetList;

  protected
    procedure BuildContents;
    procedure SelectActiveContentItem;

  private
   { When doing LoadSettings with DelayUI=true, we load some settings into
    these variables and apply them later }
    setlayout: integer;
    setwindows: integer;
    setsort: integer;
    setothersearch: integer;
    setusercompounds: boolean;
  protected
    procedure LoadRegistrySettings(reg: TRegIniFile);
    procedure SaveRegistrySettings(reg: TRegIniFile);
  public
    procedure LoadSettings(DelayUI: boolean);
    procedure ApplyUISettings;
    procedure SaveSettings;


  end;

var
  fSettings: TfSettings;

implementation

uses JWBMenu, JWBStrings, JWBUnit, JWBKanji, JWBTranslate,
  JWBKanjiSearch, JWBKanjiCompounds, JWBUser, JWBCharItem, JWBWordKanji,
  JWBExamples, JWBUserAdd, JWBUserDetails, JWBUserFilters, JWBKanjiDetails, TextTable,
  JWBLanguage, UnicodeFont, JWBKanjiCard, JWBWords;

var colorfrom:integer;

{$R *.DFM}

function checkfont(s:string):string; forward;
function ReturnStdFont(curfont:string;japanese:boolean):string; forward;

procedure TfSettings.FormCreate(Sender: TObject);
begin
  BuildContents;
end;

//Populates contents list from available pages
procedure TfSettings.BuildContents;
var i: integer;
begin
  lbContents.Clear;
  for i := 0 to pcPages.PageCount - 1 do
    lbContents.AddItem(pcPages.Pages[i].Caption, pcPages.Pages[i]);
  SelectActiveContentItem;
end;

procedure TfSettings.SelectActiveContentItem;
var i: integer;
  found: boolean;
begin
  found := false;
  for i := 0 to lbContents.Items.Count - 1 do
    if lbContents.Items.Objects[i] = pcPages.ActivePage then begin
      found := true;
      lbContents.ItemIndex := i;
      break;
    end;
  if not found then
    lbContents.ItemIndex := -1; //deselect
end;

procedure TfSettings.FormShow(Sender: TObject);
begin
 { pcPages.OnChange is not triggered when ActivePage is loaded initially from FormSettings,
  so whatever, we'll do this on show just to be safe: }
  SelectActiveContentItem();

  Button13.Visible:=paramstr(1)='debug';
  Edit30.Visible:=paramstr(1)='debug';
  Edit31.Visible:=paramstr(1)='debug';
  Label48.Visible:=paramstr(1)='debug';
  Label49.Visible:=paramstr(1)='debug';
  Edit15Change(sender);
  Edit20Change(sender);
  Edit19.Text:=dicts.NotUsedDicts;
  ResetDetList;
  ComboBox2.ItemIndex:=0;
  ClearKanjiCardCache;
  ComboBox2Change(sender);
end;


procedure TfSettings.LoadSettings(DelayUI: boolean);
var reg: TRegIniFile;
begin
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  try
    LoadRegistrySettings(reg);
  finally
    reg.Free;
  end;

  if FileExists('WAKAN.CDT') then
    chardetl.LoadFromFile('WAKAN.CDT')
  else
    Button10Click(Self);

  InitColors;

  if not DelayUI then
    ApplyUISettings();
end;

procedure TfSettings.SaveSettings;
var reg: TRegIniFile;
begin
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  try
    SaveRegistrySettings(reg);
  finally
    reg.Free;
  end;

  chardetl.SaveToFile('WAKAN.CDT');
  WriteColors;
end;

procedure TfSettings.LoadRegistrySettings(reg: TRegIniFile);
var s: string;
  i: integer;
begin
  CheckBox64.Checked:=reg.ReadBool('Annotate','Enabled',true);
  CheckBox65.Checked:=reg.ReadBool('Annotate','Rebuild',true);
  CheckBox66.Checked:=reg.ReadBool('Annotate','Sound',true);
  CheckBox67.Checked:=reg.ReadBool('Annotate','Pictures',true);
  CheckBox68.Checked:=reg.ReadBool('Annotate','WebPages',true);
  CheckBox69.Checked:=reg.ReadBool('Annotate','Colors',true);
  CheckBox46.Checked:=reg.ReadBool('Vocabulary','AutoSave',false);
  CheckBox70.Checked:=reg.ReadBool('Vocabulary','DisplayMessage',true);
  CheckBox54.Checked:=reg.ReadBool('Vocabulary','AutoSaveTimer',true);
  CheckBox55.Checked:=reg.ReadBool('Vocabulary','MakeBackups',true);
  Edit29.Text:=inttostr(reg.ReadInteger('Vocabulary','AutoSavePeriod',10));
  RadioGroup1.ItemIndex:=reg.ReadInteger('Romanization','System',1);
  RadioGroup2.ItemIndex:=reg.ReadInteger('Romanization','ShowKana',0);
  RadioGroup6.ItemIndex:=reg.ReadInteger('Romanization','ChineseSystem',0);
  RadioGroup7.ItemIndex:=reg.ReadInteger('Romanization','ShowBopomofo',1);
  fKanjiCompounds.CheckBox1.Checked:=reg.ReadBool('Characters','CompoundsBeg',false);
  fKanjiCompounds.CheckBox2.Checked:=reg.ReadBool('Characters','CompoundsPop',true);
  fKanjiCompounds.CheckBox3.Checked:=reg.ReadBool('Characters','CompoundsFreq',true);
  RadioGroup5.ItemIndex:=reg.ReadInteger('Characters','Chinese',0);
  RadioGroup3.ItemIndex:=reg.ReadInteger('Characters','GridSize',1);
  ComboBox1.ItemIndex:=reg.ReadInteger('Characters','RadicalType',0);
  CheckBox1.Checked:=reg.ReadBool('Characters','ShowStrokes',false);
  CheckBox51.Checked:=reg.ReadBool('Characters','StrokeOrderGridFont',false);
  CheckBox3.Checked:=reg.ReadBool('Characters','NoShowColors',false);
  CheckBox57.Checked:=reg.ReadBool('Characters','YomiOkurigana',false);
  if reg.ReadString('Fonts','FontSet','0')<>'1'then
  begin
    Application.MessageBox(
      pchar(_l('#00349^eYou are running WaKan for the first time.'#13
        +'WaKan will now try to locate and set all the recommended fonts.'#13
        +'You can restart this process by selecting "Select recommended fonts" '
        +'in settings.')),
      pchar(_l('#00350^eFont autodetection')),
      MB_ICONINFORMATION or MB_OK);
    if not AutoDetectFonts then
    begin
      if Application.MessageBox(
        pchar(_l('#00351^eFont autodetection failed. Some characters may not be '
          +'displayed correctly.'#13#13+'Do you want to continue?')),
        pchar(_l('#00090^eWarning')),
        MB_ICONERROR or MB_YESNO)=idNo then
      begin
        Application.Terminate;
        exit;
      end;
    end;
  end else
  begin
    Edit1.Text:=checkfont(reg.ReadString('Fonts','JapaneseGrid','MS Mincho'));
    Edit2.Text:=checkfont(reg.ReadString('Fonts','Japanese','MS Mincho'));
    Edit5.Text:=checkfont(reg.ReadString('Fonts','Small','MS Gothic'));
    Edit6.Text:=checkfont(reg.ReadString('Fonts','ChineseGrid','MingLiU'));
    Edit3.Text:=checkfont(reg.ReadString('Fonts','ChineseGridGB','SimSun'));
    Edit7.Text:=checkfont(reg.ReadString('Fonts','Chinese','MingLiU'));
    Edit9.Text:=checkfont(reg.ReadString('Fonts','ChineseGB','SimSun'));
    Edit8.Text:=checkfont(reg.ReadString('Fonts','Radical','MingLiU'));
    Edit4.Text:=checkfont(reg.ReadString('Fonts','English','Verdana'));
    Edit32.Text:=checkfont(reg.ReadString('Fonts','StrokeOrder','MS Mincho'));
    Edit33.Text:=checkfont(reg.ReadString('Fonts','PinYin','Arial'));
  end;
  CheckBox4.Checked:=reg.ReadBool('Dict','PreferUser',true);
  CheckBox5.Checked:=reg.ReadBool('Dict','PreferNouns',true);
  CheckBox6.Checked:=reg.ReadBool('Dict','PreferPolite',true);
  CheckBox7.Checked:=reg.ReadBool('Dict','PreferPopular',true);
  fUser.SpeedButton13.Down:=reg.ReadBool('Dict','QuickSearch',true);
  CheckBox8.Checked:=reg.ReadBool('Dict','ReplaceKanji',true);
  CheckBox9.Checked:=reg.ReadBool('Dict','NoUseColors',false);
  CheckBox10.Checked:=reg.ReadBool('Dict','UseGrey',false);
  CheckBox11.Checked:=reg.ReadBool('Dict','StatusColors',true);
  CheckBox12.Checked:=reg.ReadBool('Dict','AutoPage',true);
  CheckBox49.Checked:=reg.ReadBool('Dict','DemandLoad',true);
  CheckBox50.Checked:=reg.ReadBool('Dict','AutoExamples',true);
  CheckBox53.Checked:=reg.ReadBool('Dict','MultiLineGrids',true);
  CheckBox58.Checked:=reg.ReadBool('Dict','ShowFreq',false);
  CheckBox59.Checked:=reg.ReadBool('Dict','OrderFreq',true);
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
    fTranslate.DocFileName:=Reg.ReadString('Editor','DocFileName',''); //Will load later if DocFileName<>''
    fTranslate.DocTp:=Reg.ReadInteger('Editor','DocType',0);
  end;
  fExamples.btnRandomOrder.Down:=reg.ReadBool('Dict','RandomExamples',false);
  vocmode:=reg.ReadInteger('Dict','VocMode',0);
  exmode:=reg.ReadInteger('Dict','ExMode',0);
  Edit34.Text:=inttostr(reg.ReadInteger('Characters','FreqLimit',0));
  if exmode=0 then fExamples.btnDisplayTranslation.Down:=true;
  if exmode=1 then fExamples.btnUseBigFont.Down:=true;
  if exmode=2 then fExamples.btnUseSmallFont.Down:=true;
  Edit25.Text:=inttostr(reg.ReadInteger('Dict','FontSize',14));
  GridFontSize:=strtoint(Edit25.text);
  ListBox1.ItemIndex:=reg.ReadInteger('WordSheet','Columns',0);
  CheckBox14.Checked:=reg.ReadBool('WordSheet','InsideLines',true);
  CheckBox15.Checked:=reg.ReadBool('WordSheet','OutsideLines',true);
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
  reg.ReadSectionValues('DictPriority', dicts.Priority);
  dicts.Priority.Sort;
 //Cut index part
  for i := 0 to dicts.Priority.Count - 1 do begin
    s := dicts.Priority.ValueFromIndex[i];
    if s<>'' then dicts.Priority[i] := s;
  end;
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
  fUser.SpeedButton4.Down:=reg.ReadBool('Dict','DeflexItalic',true);
  CheckBox43.Checked:=reg.ReadBool('Translate','BreakLines',true);
  cbDisplayLines.Checked:=reg.ReadBool('Translate','DisplayLines',true);
  CheckBox41.Checked:=reg.ReadBool('Translate','DisplayNonJapanese',true);
  cbNoMeaningLearned.Checked:=reg.ReadBool('Translate','NoMeaningLearned',false);
  cbNoReadingLearned.Checked:=reg.ReadBool('Translate','NoReadingLearned',false);
  CheckBox36.Checked:=reg.ReadBool('Translate','ReadingKatakana',true);
  cbNoSearchParticles.Checked:=reg.ReadBool('Translate','NoSearchParticles',false);
  cbNoTranslateHiragana.Checked:=reg.ReadBool('Translate','NoTranslateHiragana',false);
  cbNoColors.Checked:=reg.ReadBool('Translate','NoUseColors',false);
  cbUserBold.Checked:=reg.ReadBool('Translate','UserBold',true);
  CheckBox42.Checked:=reg.ReadBool('Translate','LeaveSpace',false);
  CheckBox56.Checked:=reg.ReadBool('Translate','LeaveSpaceAlways',true);
  CheckBox27.Checked:=reg.ReadBool('Translate','HalfSizeMeaning',false);
  cbPrintReading.Checked:=reg.ReadBool('Translate','PrintReading',true);
  cbPrintMeaning.Checked:=reg.ReadBool('Translate','PrintMeaning',true);
  cbNoPrintColors.Checked:=reg.ReadBool('Translate','NoPrintColors',true);
  cbVerticalPrint.Checked:=reg.ReadBool('Translate','VerticalPrint',false);
  cbTranslateNoLongTextWarning.Checked := reg.ReadBool('Translate','NoLongTextWarning',true);
  cbMultithreadedTranslation.Checked := reg.ReadBool('Translate','MultithreadedTranslation',true);
  fMenu.aEditorColors.Checked:=reg.ReadBool('Translate','TransColors',true);
  fTranslate.sbUseTlColors.Down:=fMenu.aEditorColors.Checked;
  fTranslate.sbDisplayReading.Down:=reg.ReadBool('Translate','Reading',true);
  fTranslate.sbDisplayMeaning.Down:=reg.ReadBool('Translate','Meaning',true);
  fTranslate.sbDockDictionary.Down:=reg.ReadBool('Translate','Dictionary',false);
  CharDetDocked:=reg.ReadBool('Layout','CharDetailsDocked',false);
  CharDetDockedVis1:=reg.ReadBool('Layout','CharDetailsVisible1',true);
  CharDetDockedVis2:=reg.ReadBool('Layout','CharDetailsVisible2',true);
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
  setlayout:=reg.ReadInteger('Layout','DisplayLayout',1);
  setwindows:=reg.ReadInteger('Layout','SecondaryWindows',72);
  fMenu.aEditorReading.Checked:=fTranslate.sbDisplayReading.Down;
  fMenu.aEditorMeaning.Checked:=fTranslate.sbDisplayMeaning.Down;
  case reg.ReadInteger('Translate','FontSize',2) of
    0:fTranslate.sbSmallFont.Down:=true;
    1:fTranslate.sbMiddleFont.Down:=true;
    2:fTranslate.sbLargeFont.Down:=true;
  end;
  case reg.ReadInteger('Translate','FontSize',2) of
    0:fMenu.aEditorSmallFont.Checked:=true;
    1:fMenu.aEditorMedFont.Checked:=true;
    2:fMenu.aEditorLargeFont.Checked:=true;
  end;
  Edit17.Text:=reg.ReadString('Translate','MeaningLines','2');
  Edit18.Text:=reg.ReadString('Translate','PrintLines','20');
  setsort:=reg.ReadInteger('Characters','Sort',0);
  setothersearch:=reg.ReadInteger('Characters','OtherSearch',0);
  setusercompounds:=reg.ReadBool('Characters','UserCompounds',false);
  if reg.ReadBool('Dict','Meaning',false) then dictmodeset:=1 else dictmodeset:=0;
  dictbeginset:=reg.ReadInteger('Dict','SearchBeg',0);
  CheckBox2.Checked:=reg.ReadBool('Translate','ShowHint',true);
  CheckBox13.Checked:=reg.ReadBool('Translate','HintMeaning',true);
  s:=reg.ReadString('Dict','CurLanguage','j');
  if Length(s)>1 then curlang:=s[1] else curlang:='j';
  ListBox1Click(self);
  FontJapanese:=Edit2.Text;
  FontJapaneseGrid:=Edit1.Text;
  FontChinese:=Edit7.Text;
  FontChineseGrid:=Edit6.Text;
  FontChineseGB:=Edit9.Text;
  FontChineseGridGB:=Edit3.Text;
  FontSmall:=Edit5.Text;
  FontRadical:=Edit8.Text;
  FontEnglish:=Edit4.Text;
  FontPinYin:=Edit33.Text;
  FontStrokeOrder:=Edit32.Text;
end;

procedure TfSettings.ApplyUISettings;
begin
  fMenu.ToggleForm(fKanjiCompounds,fKanji.btnCompounds,fMenu.aKanjiCompounds);
  fMenu.ToggleForm(fWordKanji,fUser.SpeedButton6,fMenu.aDictKanji);
  fMenu.ToggleForm(fExamples,fUser.SpeedButton9,fMenu.aDictAdd);
  fMenu.ToggleForm(fUserDetails,fWords.SpeedButton4,fMenu.aUserDetails);
  fMenu.ToggleForm(fUserFilters,fWords.SpeedButton2,fMenu.aUserSettings);
//  fMenu.ToggleForm(fKanjiDetails,fKanji.btnKanjiDetails,fMenu.aKanjiDetails);
  fMenu.ToggleForm(fKanjiSearch,fKanji.btnSearchSort,fMenu.aKanjiSearch);
  displaymode:=setlayout;
  CharDetNowDocked:=false;
  if (setwindows and 128<>128) and (CharDetDocked) then fMenu.aKanjiDetails.Checked:=true;
  fMenu.ChangeDisplay;
  if setwindows and 1<>1 then fMenu.ToggleForm(fKanjiSearch,fKanji.btnSearchSort,fMenu.aKanjiSearch);
  if setwindows and 2<>2 then fMenu.ToggleForm(fKanjiCompounds,fKanji.btnCompounds,fMenu.aKanjiCompounds);
  if setwindows and 4<>4 then fMenu.ToggleForm(fWordKanji,fUser.SpeedButton6,fMenu.aDictKanji);
  if setwindows and 8<>8 then fMenu.ToggleForm(fExamples,fUser.SpeedButton9,fMenu.aDictAdd);
  if setwindows and 16=16 then fMenu.ToggleForm(fExamples,fWords.SpeedButton1,fMenu.aUserExamples);
  if setwindows and 32<>32 then fMenu.ToggleForm(fUserDetails,fWords.SpeedButton4,fMenu.aUserDetails);
  if setwindows and 64<>64 then fMenu.ToggleForm(fUserFilters,fWords.SpeedButton2,fMenu.aUserSettings);
  if (setwindows and 128=128) and (not CharDetDocked) then fMenu.ToggleForm(fKanjiDetails,fKanji.btnKanjiDetails,fMenu.aKanjiDetails);
  fTranslate.sbDockKanjiDetails.Down:=fKanji.btnKanjiDetails.Down;

  fKanjiSearch.rgSortBy.ItemIndex:=setsort;
  kanji_othersearch:=setothersearch;
  fKanjiSearch.cbOtherType.ItemIndex:=-1;
  if dictmodeset=1 then fUser.SpeedButton2.Down:=true else fUser.SpeedButton1.Down:=true;
  if setusercompounds then fKanjiCompounds.SpeedButton8.Down:=true else fKanjiCompounds.SpeedButton9.Down:=true;
end;

procedure TfSettings.SaveRegistrySettings(reg: TRegIniFile);
var setwindows:integer;
  i: integer;
begin
  reg.WriteBool('Vocabulary','AutoSave',CheckBox46.Checked);
  reg.WriteBool('Vocabulary','DisplayMessage',CheckBox70.Checked);
  reg.WriteBool('Vocabulary','AutoSaveTimer',CheckBox54.Checked);
  reg.WriteBool('Vocabulary','MakeBackups',CheckBox55.Checked);
  reg.WriteInteger('Vocabulary','AutoSavePeriod',strtoint(edit29.text));
  reg.WriteInteger('Romanization','System',RadioGroup1.ItemIndex);
  reg.WriteInteger('Romanization','ShowKana',RadioGroup2.ItemIndex);
  reg.WriteInteger('Romanization','ChineseSystem',RadioGroup6.ItemIndex);
  reg.WriteInteger('Romanization','ShowBopomofo',RadioGroup7.ItemIndex);
  reg.WriteInteger('Characters','Chinese',RadioGroup5.ItemIndex);
  reg.WriteInteger('Characters','GridSize',RadioGroup3.ItemIndex);
  reg.WriteInteger('Characters','RadicalType',ComboBox1.ItemIndex);
  reg.WriteBool('Characters','ShowStrokes',CheckBox1.Checked);
  reg.WriteBool('Characters','StrokeOrderGridFont',CheckBox51.Checked);
  reg.WriteBool('Characters','NoShowColors',CheckBox3.Checked);
  reg.WriteBool('Characters','YomiOkurigana',CheckBox57.Checked);
  reg.WriteBool('Characters','CompoundsBeg',fKanjiCompounds.CheckBox1.Checked);
  reg.WriteBool('Characters','CompoundsPop',fKanjiCompounds.CheckBox2.Checked);
  reg.WriteBool('Characters','CompoundsFreq',fKanjiCompounds.CheckBox3.Checked);
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
  reg.WriteBool('Dict','QuickSearch',fUser.SpeedButton13.Down);
  reg.WriteBool('Dict','ReplaceKanji',CheckBox8.Checked);
  reg.WriteBool('Dict','NoUseColors',CheckBox9.Checked);
  reg.WriteBool('Dict','UseGrey',CheckBox10.Checked);
  reg.WriteBool('Dict','StatusColors',CheckBox11.Checked);
  reg.WriteBool('Dict','AutoPage',CheckBox12.Checked);
  reg.WriteBool('Dict','DemandLoad',CheckBox49.Checked);
  reg.WriteBool('Dict','AutoExamples',CheckBox50.Checked);
  reg.WriteBool('Dict','RandomExamples',fExamples.btnRandomOrder.Down);
  reg.WriteBool('Dict','ShowFreq',CheckBox58.Checked);
  reg.WriteBool('Dict','OrderFreq',CheckBox59.Checked);
  reg.WriteBool('Editor','AutoSave',CheckBox60.Checked);
  reg.WriteBool('Editor','AutoLoad',CheckBox61.Checked);
  reg.WriteBool('Editor','NoSaveChangesWarning',cbNoSaveChangesWarning.Checked);
  reg.WriteBool('Editor','LoadAozoraRuby',cbLoadAozoraRuby.Checked);
  reg.WriteBool('Editor','AozoraTagsInColor',cbAozoraTagsInColor.Checked);
  reg.WriteBool('Editor','SaveAnnotationsToRuby',cbSaveAnnotationsToRuby.Checked);
  reg.WriteBool('Editor','AdjustCharPriorities',cbAdjustCharPriorities.Checked);
  reg.WriteInteger('Editor','ReleaseCursorMode',rgReleaseCursorMode.ItemIndex);
  reg.WriteString('Editor','DocFilename',fTranslate.DocFilename); //For autoload
  reg.WriteInteger('Editor','DocType',fTranslate.DocTp);          //This too.
  reg.WriteInteger('Characters','FreqLimit',strtoint(Edit34.Text));
  reg.WriteInteger('Dict','VocMode',vocmode);
  reg.WriteInteger('Dict','ExMode',exmode);
  reg.WriteInteger('Dict','FontSize',strtoint(Edit25.text));
  reg.WriteBool('Dict','MultiLineGrid',CheckBox53.Checked);
  reg.WriteInteger('WordSheet','Columns',ListBox1.ItemIndex);
  reg.WriteBool('WordSheet','InsideLines',CheckBox14.Checked);
  reg.WriteBool('WordSheet','OutsideLines',CheckBox15.Checked);
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
  reg.WriteBool('Dict','DeflexItalic',fUser.SpeedButton4.Down);
  reg.WriteBool('Translate','BreakLines',CheckBox43.Checked);
  reg.WriteBool('Translate','DisplayLines',cbDisplayLines.Checked);
  reg.WriteBool('Translate','DisplayNonJapanese',CheckBox41.Checked);
  reg.WriteBool('Translate','NoMeaningLearned',cbNoMeaningLearned.Checked);
  reg.WriteBool('Translate','NoReadingLearned',cbNoReadingLearned.Checked);
  reg.WriteBool('Translate','ReadingKatakana',CheckBox36.Checked);
  reg.WriteBool('Translate','NoSearchParticles',cbNoSearchParticles.Checked);
  reg.WriteBool('Translate','NoTranslateHiragana',cbNoTranslateHiragana.Checked);
  reg.WriteBool('Translate','NoUseColors',cbNoColors.Checked);
  reg.WriteBool('Translate','UserBold',cbUserBold.Checked);
  reg.WriteBool('Translate','LeaveSpace',CheckBox42.Checked);
  reg.WriteBool('Translate','LeaveSpaceAlways',CheckBox56.Checked);
  reg.WriteBool('Translate','HalfSizeMeaning',CheckBox27.Checked);
  reg.WriteBool('Translate','PrintReading',cbPrintReading.Checked);
  reg.WriteBool('Translate','PrintMeaning',cbPrintMeaning.Checked);
  reg.WriteBool('Translate','NoPrintColors',cbNoPrintColors.Checked);
  reg.WriteBool('Translate','VerticalPrint',cbVerticalPrint.Checked);
  reg.WriteBool('Translate','Reading',fTranslate.sbDisplayReading.Down);
  reg.WriteBool('Translate','Meaning',fTranslate.sbDisplayMeaning.Down);
  reg.WriteBool('Translate','Dictionary',fTranslate.sbDockDictionary.Down);
  reg.WriteBool('Translate','TransColors',fMenu.aEditorColors.Checked);
  reg.WriteBool('Translate','NoLongTextWarning',cbTranslateNoLongTextWarning.Checked);
  reg.WriteBool('Translate','MultithreadedTranslation',cbMultithreadedTranslation.Checked);
  reg.WriteBool('Annotate','Enabled',CheckBox64.Checked);
  reg.WriteBool('Annotate','Rebuild',CheckBox65.Checked);
  reg.WriteBool('Annotate','Sound',CheckBox66.Checked);
  reg.WriteBool('Annotate','Pictures',CheckBox67.Checked);
  reg.WriteBool('Annotate','WebPages',CheckBox68.Checked);
  reg.WriteBool('Annotate','Colors',CheckBox69.Checked);
  if fTranslate.sbSmallFont.Down then reg.WriteInteger('Translate','FontSize',0);
  if fTranslate.sbMiddleFont.Down then reg.WriteInteger('Translate','FontSize',1);
  if fTranslate.sbLargeFont.Down then reg.WriteInteger('Translate','FontSize',2);
  reg.WriteInteger('Layout','QLayout',curqlayout);
  reg.WriteString('Translate','MeaningLines',Edit17.text);
  reg.WriteString('Translate','PrintLines',Edit18.text);
  reg.WriteString('Dict','NotUsedDicts',dicts.NotUsedDicts);
  reg.WriteString('Dict','NotGroup1Dicts',dicts.NotGroupDicts[1]);
  reg.WriteString('Dict','NotGroup2Dicts',dicts.NotGroupDicts[2]);
  reg.WriteString('Dict','NotGroup3Dicts',dicts.NotGroupDicts[3]);
  reg.WriteString('Dict','NotGroup4Dicts',dicts.NotGroupDicts[4]);
  reg.WriteString('Dict','NotGroup5Dicts',dicts.NotGroupDicts[5]);
  reg.WriteString('Dict','OfflineDicts',dicts.OfflineDicts);
  reg.WriteString('Dict','CurLanguage',curlang);
  for i := 0 to dicts.Priority.Count - 1 do
    reg.WriteString('DictPriority', IntToStr(i), dicts.Priority[i]);
  reg.WriteInteger('Characters','Sort',fKanjiSearch.rgSortBy.ItemIndex);
  reg.WriteInteger('Characters','OtherSearch',fKanjiSearch.cbOtherType.ItemIndex);
  reg.WriteBool('Characters','UserCompounds',fKanjiCompounds.SpeedButton8.Down);
  reg.WriteBool('Dict','Meaning',dictmodeset=1);
  reg.WriteInteger('Dict','SearchBeg',dictbeginset);
  reg.WriteBool('Translate','ShowHint',CheckBox2.Checked);
  reg.WriteBool('Translate','HintMeaning',CheckBox13.Checked);
  reg.WriteInteger('Layout','DisplayLayout',curdisplaymode);
  reg.WriteBool('Layout','CharDetailsDocked',CharDetDocked);
  reg.WriteBool('Layout','CharDetailsVisible1',CharDetDockedVis1);
  reg.WriteBool('Layout','CharDetailsVisible2',CharDetDockedVis2);
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
  if fMenu.aDictAdd.Checked then inc(setwindows,8);
  if fMenu.aUserExamples.Checked then inc(setwindows,16);
  if fMenu.aUserDetails.Checked then inc(setwindows,32);
  if fMenu.aUserSettings.Checked then inc(setwindows,64);
  if fKanjiDetails.Visible then inc(setwindows,128);
  reg.WriteInteger('Layout','SecondaryWindows',setwindows);
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



procedure TfSettings.RadioGroup1Click(Sender: TObject);
begin
  jromasys:=RadioGroup1.ItemIndex+1;
  jshowroma:=RadioGroup2.ItemIndex=1;
  cromasys:=RadioGroup6.ItemIndex+1;
  cshowroma:=RadioGroup7.ItemIndex=1;
  if curlang='c'then
  begin
    romasys:=cromasys; showroma:=cshowroma;
  end else begin
    romasys:=jromasys; showroma:=jshowroma;
  end;
end;

procedure TfSettings.Button6Click(Sender: TObject);
begin
  fLanguage.ShowModal;
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

procedure TfSettings.btnOkClick(Sender: TObject);
var i:integer;
begin
  if edit11.text='0' then edit11.text:='1';
  if not TryStrToInt(Edit10.Text, i) then Edit10.Text := '0';
  if not TryStrToInt(Edit11.Text, i) then Edit11.Text := '0';
  if not TryStrToInt(Edit12.Text, i) then Edit12.Text := '0';
  if not TryStrToInt(Edit13.Text, i) then Edit13.Text := '0';
  GridFontSize:=strtoint(Edit25.text);

  SaveSettings();
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

procedure TfSettings.PaintBox3Paint(Sender: TObject);
begin
  PaintBox3.Canvas.Brush.Color:=clBtnFace;
  DrawUnicode(PaintBox3.Canvas,1,1,16,RomajiToKana('H'+Edit15.Text,RadioGroup1.ItemIndex+1,false,'j'),FontSmall);
end;

procedure TfSettings.lbContentsClick(Sender: TObject);
begin
  if lbContents.ItemIndex >= 0 then
    pcPages.ActivePage := TTabSheet(lbContents.Items.Objects[lbContents.ItemIndex]);
    //will trigger pointless SelectActiveContentItem(), but whatever
end;

procedure TfSettings.pcPagesChange(Sender: TObject);
begin
  SelectActiveContentItem();
end;

procedure TfSettings.Edit15Change(Sender: TObject);
begin
  PaintBox3.Invalidate;
  PaintBox1.Invalidate;
  Label20.Caption:=KanaToRomaji(RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,true,'j'),1,'j');
  Label21.Caption:=KanaToRomaji(RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,true,'j'),2,'j');
  Label22.Caption:=KanaToRomaji(RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,true,'j'),3,'j');
end;

procedure TfSettings.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color:=clBtnFace;
  DrawUnicode(PaintBox1.Canvas,1,1,16,RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,false,'j'),FontSmall);
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
  if (TChar.CheckIndex) and (TCharRead.CheckIndex) and (TRadicals.CheckIndex) then
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

procedure TfSettings.ListBox1Click(Sender: TObject);
begin
  Edit16.Enabled:=false;
  Edit16.Color:=clBtnFace;
  case ListBox1.ItemIndex of
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
        s:=KanaToRomaji(TUser.Str(TUserPhonetic),romasys,'j');
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

procedure TfSettings.PaintBox2Paint(Sender: TObject);
begin
  PaintBox2.Canvas.Brush.Color:=clBtnFace;
  DrawUnicode(PaintBox2.Canvas,1,1,16,RomajiToKana(Edit20.Text,RadioGroup6.ItemIndex+1,false,'c'),FontSmall);
end;

procedure TfSettings.Edit20Change(Sender: TObject);
begin
  PaintBox2.Invalidate;
  Label31.Caption:=KanaToRomaji(RomajiToKana(Edit20.Text,RadioGroup6.ItemIndex+1,true,'c'),1,'c');
  Label32.Caption:=KanaToRomaji(RomajiToKana(Edit20.Text,RadioGroup6.ItemIndex+1,true,'c'),2,'c');
  Label33.Caption:=KanaToRomaji(RomajiToKana(Edit20.Text,RadioGroup6.ItemIndex+1,true,'c'),3,'c');
end;

function TfSettings.checkfontbool(s:string):boolean;
begin
  if Screen.Fonts.IndexOf(s)=-1 then result:=false else result:=true;
end;

function TfSettings.AutoDetectFonts:boolean;
var missingfonts:string;
    substituted:boolean;
function ReturnStdFont(curfont,substfont:string):string;
begin
  if checkfontbool(curfont) then
  begin
    result:=curfont;
    exit;
  end;
  if pos(','+curfont,missingfonts)=0 then missingfonts:=missingfonts+','+curfont;
  if checkfontbool(substfont) then
  begin
    result:=substfont;
    exit;
  end;
  if pos(','+substfont,missingfonts)=0 then missingfonts:=missingfonts+','+substfont;
  substituted:=false;
  result:='Arial';
end;
var s:string;
begin
  substituted:=true;
  fSettings.Edit1.Text:=ReturnStdFont('MS Mincho','MS Gothic');
  fSettings.Edit2.Text:=ReturnStdFont('MS Mincho','MS Gothic');
  fSettings.Edit5.Text:=ReturnStdFont('MS Gothic','MS Mincho');
  fSettings.Edit6.Text:=ReturnStdFont('MingLiU','SimSun');
  fSettings.Edit3.Text:=ReturnStdFont('SimSun','MingLiU');
  fSettings.Edit7.Text:=ReturnStdFont('MingLiU','SimSun');
  fSettings.Edit9.Text:=ReturnStdFont('SimSun','MingLiU');
  fSettings.Edit8.Text:=ReturnStdFont('MingLiU','SimSun');
  fSettings.Edit4.Text:=ReturnStdFont('Verdana','Tahoma');
  fSettings.Edit32.Text:=ReturnStdFont('MS Mincho','MS Gothic');
  fSettings.Edit33.Text:=ReturnStdFont('Arial','Arial');
  FontJapanese:=fSettings.Edit1.Text;
  FontJapaneseGrid:=fSettings.Edit2.Text;
  FontChinese:=fSettings.Edit7.Text;
  FontChineseGrid:=fSettings.Edit6.Text;
  FontChineseGB:=fSettings.Edit9.Text;
  FontChineseGridGB:=fSettings.Edit3.Text;
  FontSmall:=fSettings.Edit5.Text;
  FontRadical:=fSettings.Edit8.Text;
  FontEnglish:=fSettings.Edit4.Text;
  FontPinYin:=fSettings.Edit33.Text;
  FontStrokeOrder:=fSettings.Edit32.Text;
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
    Application.MessageBox(
      pchar(_l('#00583^eCongratulations!'#13#13'All recommended fonts were '
        +'located on your system and were installed.')),
      pchar(_l('#00350^eFont autodetection')),
      MB_ICONINFORMATION or MB_OK);
  result:=(missingfonts='') or (substituted);
end;

procedure TfSettings.Button5Click(Sender: TObject);
begin
  AutoDetectFonts;
end;

procedure TfSettings.ResetDetList;
var i,j:integer;
    ii:integer;
begin
  ii:=ListBox2.ItemIndex;
  ListBox2.Items.Clear;
  for i:=0 to chardetl.Count-1 do
  begin
    j := FindCharPropType(GetCharDet(i,0));
    if j<0 then continue;

    if GetCharDet(i,6)<>'' then
      ListBox2.Items.Add(_l('^e'+GetCharPropType(j,4))+' ('+GetCharDet(i,6)+')')
    else
      ListBox2.Items.Add(_l('^e'+GetCharPropType(j,4)));
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
begin
  fCharItem.inputs:='';
  if fCharItem.ShowModal=mrOK then chardetl.Add(fCharItem.results);
  ResetDetList;
end;

procedure TfSettings.Button8Click(Sender: TObject);
begin
  if ListBox2.ItemIndex<>-1 then
  begin
    fCharItem.inputs:=chardetl[ListBox2.ItemIndex];
    if fCharItem.ShowModal=mrOK then chardetl[ListBox2.ItemIndex]:=fCharItem.results;
    ResetDetList;
  end;
end;

procedure TfSettings.Button9Click(Sender: TObject);
begin
  if ListBox2.ItemIndex<>-1 then
  begin
    chardetl.Delete(ListBox2.ItemIndex);
    ResetDetList;
  end;
end;

procedure TfSettings.Button10Click(Sender: TObject);
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
  ResetDetList;
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
  ResetDetList;
end;

procedure TfSettings.SpeedButton12Click(Sender: TObject);
begin
  chardetl.Insert(ListBox2.ItemIndex+2,chardetl[ListBox2.ItemIndex]);
  chardetl.Delete(ListBox2.ItemIndex);
  ListBox2.ItemIndex:=ListBox2.ItemIndex+1;
  ResetDetList;
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
  CheckBox9.Visible:=(ComboBox2.ItemIndex=1) or (ComboBox2.ItemIndex=2);
  cbNoColors.Visible:=ComboBox2.ItemIndex=3;
  case Combobox2.ItemIndex of
    0: v:=not CheckBox3.Checked;
    1: v:=not CheckBox9.Checked;
    2: v:=not CheckBox9.Checked;
    3: v:=not cbNoColors.Checked;
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

procedure TfSettings.Button13Click(Sender: TObject);
begin
  fMenu.Button3Click(Sender);
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
  winexec('notepad annotate.txt',SW_SHOW);
end;

end.
