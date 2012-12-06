unit JWBSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RXCtrls, Buttons, ComCtrls;

type
  TfSettings = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    RadioGroup3: TRadioGroup;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    Button6: TButton;
    TabSheet3: TTabSheet;
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
    TabSheet4: TTabSheet;
    GroupBox3: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    TabSheet5: TTabSheet;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    GroupBox4: TGroupBox;
    ListBox1: TListBox;
    Label11: TLabel;
    Edit10: TEdit;
    TabSheet6: TTabSheet;
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
    TabSheet7: TTabSheet;
    Button2: TButton;
    Button1: TButton;
    OpenDialog2: TOpenDialog;
    SaveDialog2: TSaveDialog;
    Button3: TButton;
    TabSheet8: TTabSheet;
    CheckBox26: TCheckBox;
    Label23: TLabel;
    Edit16: TEdit;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    TabSheet9: TTabSheet;
    GroupBox5: TGroupBox;
    CheckBox29: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox31: TCheckBox;
    CheckBox32: TCheckBox;
    CheckBox33: TCheckBox;
    CheckBox34: TCheckBox;
    CheckBox35: TCheckBox;
    CheckBox36: TCheckBox;
    CheckBox37: TCheckBox;
    CheckBox38: TCheckBox;
    CheckBox40: TCheckBox;
    Label24: TLabel;
    Label25: TLabel;
    Edit17: TEdit;
    Edit18: TEdit;
    CheckBox42: TCheckBox;
    CheckBox43: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox41: TCheckBox;
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
    CheckBox2: TCheckBox;
    CheckBox13: TCheckBox;
    TabSheet10: TTabSheet;
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
    TabSheet11: TTabSheet;
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
    TabSheet12: TTabSheet;
    ListBox3: TListBox;
    Shape2: TShape;
    Button12: TButton;
    Button14: TButton;
    Button15: TButton;
    Label42: TLabel;
    ComboBox2: TComboBox;
    CheckBox3: TCheckBox;
    CheckBox39: TCheckBox;
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
    CheckBox56: TCheckBox;
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
    TabSheet13: TTabSheet;
    CheckBox60: TCheckBox;
    CheckBox61: TCheckBox;
    Label53: TLabel;
    CheckBox62: TCheckBox;
    CheckBox63: TCheckBox;
    Edit35: TEdit;
    TabSheet14: TTabSheet;
    CheckBox64: TCheckBox;
    CheckBox65: TCheckBox;
    CheckBox66: TCheckBox;
    CheckBox67: TCheckBox;
    CheckBox68: TCheckBox;
    CheckBox69: TCheckBox;
    Bevel1: TBevel;
    Button16: TButton;
    CheckBox70: TCheckBox;
    cbTranslateNoLongTextWarning: TCheckBox;
    cbNoSaveChangesWarning: TCheckBox;
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
    procedure BitBtn1Click(Sender: TObject);
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
    procedure CheckBox39Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
  private
    { Private declarations }
  public
    function CheckFontBool(s:string):boolean;
    function AutoDetectFonts:boolean;
    procedure ResetDetList;
    { Public declarations }
  end;

var
  fSettings: TfSettings;

implementation

uses JWBMenu, JWBCore, JWBUnit, registry, JWBKanji, JWBTranslate,
  JWBKanjiSearch, JWBKanjiCompounds, JWBUser, JWBCharItem, JWBWordKanji,
  JWBExamples, JWBUserAdd, JWBUserDetails, JWBUserFilters, JWBKanjiDetails, TextTable,
  JWBLanguage, UnicodeFont, JWBKanjiCard;

var fontlist:TStringList;
    colorfrom:integer;

{$R *.DFM}

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
  Application.MessageBox(pchar(_l('#00564^ePlease make sure that the font you select is a Unicode font with'+
  ' complete "CJK Unified Ideographs" range.'#13'MingLiu is an example of such font.^cUjistìte se, že font, který vyberete je Unicode font s'+
  ' kompletní "CJK Unified Ideographs" rozsahem.'#13'Takovým fontem je napøíklad MingLiu.')),
  pchar(_l('#00364^eNotice^cUpozornìní')),MB_OK or MB_ICONINFORMATION);
  FontRadical:=ChooseFont([CHINESEBIG5_CHARSET],FS_RADICAL_CHARTEST,sup,Edit8.Text,false);
  Edit8.Text:=FontRadical;
end;

procedure TfSettings.SpeedButton3Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(pchar(_l('#00565^eWhen selecting the font you must ensure that it is really a simplified chinese font.'+
  #13+'Some fonts support GB2312 standard but instead of simplified characters they show the traditional ones.'+#13
  +'These fonts would display improper characters in this program.'+#13+
  'You can identify the right font very easily. In the font selection dialog'+#13+
  'look at the first character. If it does not look like arrow pointing up, you should select another font.'+
  '^cPøi výbìru znakù se musíte ujistit, že se jedná opravdu o zjednodušený font.'+#13+
  'Nìkteré fonty podporují standard GB2312, ale místo zjednodušených znakù zobrazují znaky tradièní.'+#13
  +'Tyto fonty by zobrazovaly v tomto programu špatné znaky.'+#13+
  'Správný font poznáte jednoduše. Ve výbìru fontù se podívejte na první znak,'+
  #13+'pokud nevypadá jako šipka smìøující vzhùru, vyberte jiný font.')),pchar(_l('#00566^eNotice^cVarování')),MB_OK or MB_ICONINFORMATION);
  FontChineseGridGB:=ChooseFont([GB2312_CHARSET],FS_CHINESEGB_CHARTEST,sup,Edit3.Text,false);
  Edit3.Text:=FontChineseGridGB;
end;

procedure TfSettings.SpeedButton9Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(pchar(_l('#00565^eWhen selecting the font you must ensure that it is really a simplified chinese font.'+
  #13+'Some fonts support GB2312 standard but instead of simplified characters they show the traditional ones.'+#13
  +'These fonts would display improper characters in this program.'+#13+
  'You can identify the right font very easily. In the font selection dialog'+#13+
  'look at the first character. If it does not look like arrow pointing up, you should select another font.'+
  '^cPøi výbìru znakù se musíte ujistit, že se jedná opravdu o zjednodušený font.'+#13+
  'Nìkteré fonty podporují standard GB2312, ale místo zjednodušených znakù zobrazují znaky tradièní.'+#13
  +'Tyto fonty by zobrazovaly v tomto programu špatné znaky.'+#13+
  'Správný font poznáte jednoduše. Ve výbìru fontù se podívejte na první znak,'+
  #13+'pokud nevypadá jako šipka smìøující vzhùru, vyberte jiný font.')),pchar(_l('#00566^eNotice^cVarování')),MB_OK or MB_ICONINFORMATION);
  FontChineseGB:=ChooseFont([GB2312_CHARSET],FS_CHINESEGB_CHARTEST,sup,Edit9.Text,false);
  Edit9.Text:=FontChineseGB;
end;

procedure TfSettings.BitBtn1Click(Sender: TObject);
var reg:TRegIniFile;
    i:integer;
    setwindows:integer;
begin
  if edit11.text='0' then edit11.text:='1';
  i:=0;
  try
    i:=strtoint(Edit10.Text);
  except i:=0; end;
  if i=0 then Edit10.Text:='0';
  i:=0;
  try
    i:=strtoint(Edit11.Text);
  except i:=0; end;
  if i=0 then Edit11.Text:='0';
  i:=0;
  try
    i:=strtoint(Edit12.Text);
  except i:=0; end;
  if i=0 then Edit12.Text:='0';
  i:=0;
  try
    i:=strtoint(Edit13.Text);
  except i:=0; end;
  if i=0 then Edit13.Text:='0';
  reg:=TRegIniFile.Create('Software\Labyrinth\WaKan');
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
  reg.WriteBool('Dict','RandomExamples',fExamples.SpeedButton11.Down);
  reg.WriteBool('Dict','ShowFreq',CheckBox58.Checked);
  reg.WriteBool('Dict','OrderFreq',CheckBox59.Checked);
  reg.WriteBool('Editor','AutoSave',CheckBox60.Checked);
  reg.WriteBool('Editor','AutoLoad',CheckBox61.Checked);
  reg.WriteBool('Editor','NoSaveChangesWarning',cbNoSaveChangesWarning.Checked);
  reg.WriteString('Editor','DocFilename',fTranslate.DocFilename); //For autoload
  reg.WriteInteger('Editor','DocType',fTranslate.DocTp);          //This too.
  reg.WriteInteger('Characters','FreqLimit',strtoint(Edit34.Text));
  reg.WriteInteger('Dict','VocMode',vocmode);
  reg.WriteInteger('Dict','ExMode',exmode);
  reg.WriteInteger('Dict','FontSize',strtoint(Edit25.text));
  reg.WriteBool('Dict','MultiLineGrid',CheckBox53.Checked);
  reg.WriteInteger('WordSheet','Columns',ListBox1.ItemIndex);
  GridFontSize:=strtoint(Edit25.text);
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
  reg.WriteBool('Translate','DisplayLines',CheckBox32.Checked);
  reg.WriteBool('Translate','DisplayNonJapanese',CheckBox41.Checked);
  reg.WriteBool('Translate','NoMeaningLearned',CheckBox33.Checked);
  reg.WriteBool('Translate','NoReadingLearned',CheckBox35.Checked);
  reg.WriteBool('Translate','ReadingKatakana',CheckBox36.Checked);
  reg.WriteBool('Translate','NoSearchParticles',CheckBox34.Checked);
  reg.WriteBool('Translate','NoTranslateHiragana',CheckBox38.Checked);
  reg.WriteBool('Translate','NoUseColors',CheckBox39.Checked);
  reg.WriteBool('Translate','UserBold',CheckBox40.Checked);
  reg.WriteBool('Translate','LeaveSpace',CheckBox42.Checked);
  reg.WriteBool('Translate','LeaveSpaceAlways',CheckBox56.Checked);
  reg.WriteBool('Translate','HalfSizeMeaning',CheckBox27.Checked);
  reg.WriteBool('Translate','PrintReading',CheckBox29.Checked);
  reg.WriteBool('Translate','PrintMeaning',CheckBox30.Checked);
  reg.WriteBool('Translate','NoPrintColors',CheckBox31.Checked);
  reg.WriteBool('Translate','VerticalPrint',CheckBox37.Checked);
  reg.WriteBool('Translate','Reading',fTranslate.sbDisplayReading.Down);
  reg.WriteBool('Translate','Meaning',fTranslate.sbDisplayMeaning.Down);
  reg.WriteBool('Translate','Dictionary',fTranslate.sbDockDictionary.Down);
  reg.WriteBool('Translate','TransColors',fMenu.aEditorColors.Checked);
  reg.WriteBool('Translate','NoLongTextWarning',cbTranslateNoLongTextWarning.Checked);
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
  reg.WriteString('Dict','NotUsedDicts',NotUsedDicts);
  reg.WriteString('Dict','NotGroup1Dicts',NotGroupDicts[1]);
  reg.WriteString('Dict','NotGroup2Dicts',NotGroupDicts[2]);
  reg.WriteString('Dict','NotGroup3Dicts',NotGroupDicts[3]);
  reg.WriteString('Dict','NotGroup4Dicts',NotGroupDicts[4]);
  reg.WriteString('Dict','NotGroup5Dicts',NotGroupDicts[5]);
  reg.WriteString('Dict','OfflineDicts',OfflineDicts);
  reg.WriteString('Dict','CurLanguage',curlang);
  reg.WriteInteger('Characters','Sort',fKanjiSearch.RadioGroup1.ItemIndex);
  reg.WriteInteger('Characters','OtherSearch',fKanjiSearch.ComboBox1.ItemIndex);
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
  reg.Free;
  chardetl.SaveToFile('WAKAN.CDT');
  WriteColors;
end;

procedure TfSettings.SpeedButton10Click(Sender: TObject);
var sup:string;
begin
  Application.MessageBox(pchar(_l('#00567^eSome fonts may not be able to display some characters. If you see that some characters look different in style or aren''t displayed properly, select another font.'+
  '^cNìkteré fonty nemusí být schopny zobrazit všechny znaky. Pokud nebudou nìkteré znaky zobrazeny v poøádku nebo budou zobrazeny v jiném stylu, vyberte jiný font.')),pchar(_l('#00566^eNotice^cVarování')),MB_OK or MB_ICONINFORMATION);
  Edit14.Text:=ChooseFont([CHINESEBIG5_CHARSET,GB2312_CHARSET,SHIFTJIS_CHARSET],testkanji,sup,Edit9.Text,false);
end;

procedure TfSettings.PaintBox3Paint(Sender: TObject);
begin
  PaintBox3.Canvas.Brush.Color:=clBtnFace;
  DrawUnicode(PaintBox3.Canvas,1,1,16,RomajiToKana('H'+Edit15.Text,RadioGroup1.ItemIndex+1,false,'j'),FontSmall);
end;

procedure TfSettings.Edit15Change(Sender: TObject);
begin
  PaintBox3.Invalidate;
  PaintBox1.Invalidate;
  Label20.Caption:=KanaToRomaji(RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,true,'j'),1,'j');
  Label21.Caption:=KanaToRomaji(RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,true,'j'),2,'j');
  Label22.Caption:=KanaToRomaji(RomajiToKana('K'+Edit15.Text,RadioGroup1.ItemIndex+1,true,'j'),3,'j');
end;

procedure TfSettings.FormShow(Sender: TObject);
begin
  Button13.Visible:=paramstr(1)='debug';
  Edit30.Visible:=paramstr(1)='debug';
  Edit31.Visible:=paramstr(1)='debug';
  Label48.Visible:=paramstr(1)='debug';
  Label49.Visible:=paramstr(1)='debug';
  Edit15Change(sender);
  Edit20Change(sender);
  Edit19.Text:=NotUsedDicts;
  ResetDetList;
  ComboBox2.ItemIndex:=0;
  ClearKanjiCardCache;
  ComboBox2Change(sender);
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
    Application.MessageBox(pchar(_l('#00568^eExport was finished.^cExport byl dokonèen.')),'Export',MB_ICONINFORMATION or MB_OK);
  end;
end;

procedure TfSettings.Button2Click(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  if (TChar.CheckIndex) and (TCharRead.CheckIndex) and
     (TRadicals.CheckIndex) then
       Application.MessageBox(pchar(_l('#00569^eIndexes are okay.^cIndexy jsou v poøádku.')),
       pchar(_l('#00570^eIndex check^cTest indexù')),MB_OK or MB_ICONINFORMATION)
  else Application.MessageBox(pchar(_l('#00571^eIndexes are damaged. Replace the dictionary file WAKAN.CHR.^cIndexy jsou v poškozeny. Vymìòte soubor WAKAN.CHR.')),
       pchar(_l('#00570^eIndex check^cTest indexù')),MB_OK or MB_ICONERROR);
  Screen.Cursor:=crDefault;
end;

procedure TfSettings.Button3Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  if Application.MessageBox(pchar(_l('#00572^eThis action replaces all data in user database.'#13'It can take a long time.'#13#13'Do you want to continue?'+
  '^cTato akce nahradí všechna data v uživatelské databázi.'#13'Akce mùže trvat velmi dlouho.'#13#13'Opravdu chcete pokraèovat?')),
  pchar(_l('#00573^eWarning^cUpozornìní')),MB_ICONWARNING or MB_YESNO)=idYes then
  begin
    fMenu.ImportUserData(OpenDialog2.FileName);
    Application.MessageBox(pchar(_l('#00574^eImport was finished.^cImport byl dokonèen.')),'Import',MB_ICONINFORMATION or MB_OK);
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
  if Application.MessageBox(pchar(_l('#00575^eThis function tests whether all the words in user vocabulary'#13+
    'are in exactly one group category and at least in one lesson category.'#13#13+'Do you want to continue?^cTato funkce otestuje jestli jsou všechna slovíèka'#13+
    'v právì jedné skupinové kategorii a alespoò jedné kategorii lekce.'#13#13'Chcete pokraèovat?')),pchar(_l('#00576^eCheck categories^cOtestovat kategorie')),
    MB_ICONINFORMATION or MB_YESNO)=idYes then
    begin
      Screen.Cursor:=crHourGlass;
      TUser.First;
      lfn:=0; gfn:=0; gmn:=0;
      lfs:=''; gfs:=''; gms:='';
      while not TUser.EOF do
      begin
        TUserSheet.SetOrder('Word_Ind');
        TUserSheet.Locate('Word',inttostr(TUser.Int(TUserIndex)),true);
        gfound:=false; gmore:=false; lfound:=false;
        word:=TUser.Int(TUserIndex);
        while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
        begin
          TUserCat.Locate('Index',TUserSheet.Str(TUserSheetNumber),true);
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
      if lfn>0 then ssum:=ssum+_l('^eWords without assigned lesson^cSlova bez pøiøazené lekce')+' ('+inttostr(lfn)+'):'#13+lfs+#13#13;
      if gfn>0 then ssum:=ssum+_l('^eWords without assigned group^cSlova bez pøiøazené skupiny')+' ('+inttostr(gfn)+'):'#13+gfs+#13#13;
      if gmn>0 then ssum:=ssum+_l('^eWords with more assigned group^cSlova s více pøiøazenými skupinami')+' ('+inttostr(gmn)+'):'#13+gms+#13#13;
      if ssum<>'' then Application.MessageBox(pchar(ssum),pchar(_l('^eErrors^cChyby')),MB_ICONERROR or MB_OK) else
        Application.MessageBox(pchar(_l('#00577^eNo errors were found.^cŽádné chyby nebyly nalezeny.')),pchar(_l('#00094^eSuccess^cHotovo')),MB_ICONINFORMATION or MB_OK);
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
  result:=true;
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
    s:=_l('#00578^eFollowing recommended fonts were not found on your system:^cNásledující doporuèené fonty nebyly na vašem systému nalezeny:')+#13#13;
    s:=s+missingfonts+#13#13;
    if substituted then
      s:=s+_l('#00579^eReasonable substitution was found, however for better font quality installation of these fonts and restart'#13'of the autodetection routine is recommended.'+
        '^cByla nalezena rozumná substituce za tyto fonty, avšak pro lepší kvalitu písma'#13'+doporuèujeme nainstalovat tyto fonty a restartovat autodetekci.') else
      s:=s+_l('#00580^eNo reasonable substitution was found, program will probably display characters incorrectly.'#13+
              'Installation of these fonts and restart of the autodetection routine is HIGHLY recommended.'+
              '^cZa tyto fonty nebyla nalezena rozumná náhrada, program mùže zobrazovat znaky nesprávnì.'#13+
              'Instalace tìchto fontù a restartování autodetekce je SILNÌ doporuèováno.');
    s:=s+#13#13;
    s:=s+_l('#00581^eThese fonts can be found in following Microsoft(R) products:'#13#13+
      '1. Microsoft(R) Windows(R) XP - Select "Install eastern-Asian fonts" in language settings in Control Panel.'#13+
      '2. Microsoft(R) Internet Explorer - Install "support for Japanese, traditional Chinese and simplified Chinese writing".'#13+
      '3. Microsoft(R) Office 2000/XP - Install Japanese, traditional Chinese and simplified Chinese fonts.'+
      '^cTyto fonty se nacházejí v tìchto produktech Microsoftu:'#13#13+
      '1. Microsoft(R) Windows(R) XP - Zvolte "Nainstalovat soubory pro jazyky východní Asie" v jazykových nastaveních v Ovládacím panelu.'#13+
      '2. Microsoft(R) Internet Explorer - Nainstalujte "podporu pro zápis japonštiny, tradièní a zjednodušené èínštiny".'#13+
      '3. Microsoft(R) Office 2000/XP - Nainstalujte japonské, tradièní a zjednodušené èínské fonty.');
    if substituted then
      Application.MessageBox(pchar(s),pchar(_l('#00582^eMissing fonts^cChybìjící fonty')), MB_ICONWARNING or MB_OK) else
      Application.MessageBox(pchar(s),pchar(_l('#00582^eMissing fonts^cChybìjící fonty')), MB_ICONERROR or MB_OK);
  end else Application.MessageBox(pchar(_l('#00583^eCongratulations!'#13#13'All recommended fonts were located on your system and were installed.^cGratulujeme!'#13#13+
  'Všechny doporuèované fonty byly na vašem systému nalezeny a byly nainstalovány.')),pchar(_l('#00350^eFont autodetection^cAutodetekce fontù')),MB_ICONINFORMATION or MB_OK);
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
    for j:=0 to chartypel.Count-1 do if fMenu.GetCharType(j,0)=fMenu.GetCharDet(i,0) then
    begin
      if fMenu.GetCharDet(i,6)<>'' then
        ListBox2.Items.Add(_l('^e'+fMenu.GetCharType(j,4)+'^c'+fMenu.GetCharType(j,5))+' ('+fMenu.GetCharDet(i,6)+')') else
        ListBox2.Items.Add(_l('^e'+fMenu.GetCharType(j,4)+'^c'+fMenu.GetCharType(j,5)));
    end;
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
  CheckBox39.Visible:=ComboBox2.ItemIndex=3;
  if ComboBox2.ItemIndex=0 then v:=not CheckBox3.Checked;
  if ComboBox2.ItemIndex=1 then v:=not CheckBox9.Checked;
  if ComboBox2.ItemIndex=2 then v:=not CheckBox9.Checked;
  if ComboBox2.ItemIndex=3 then v:=not CheckBox39.Checked;
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
  if Application.MessageBox(pchar(_l('#00584^eThis function will replace all your settings.'#13#13'Do you want to continue?'+
  '^cTato funkce nahradí všechna vaše nastavení.'#13#13'Chcete pokraèovat?')),
  pchar(_l('#00090^eWarning^cVarování')),MB_ICONWARNING or MB_YESNO)=idNo then exit;
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
  ColorDialog1.COlor:=GetCol(colorFrom+ListBox3.ItemIndex);
  if ColorDialog1.Execute then SetCol(colorFrom+ListBox3.ItemIndex,ColorDialog1.Color);
  ListBox3Click(Sender);
end;

procedure TfSettings.CheckBox3Click(Sender: TObject);
begin
  if fSettings.Visible then ComboBox2Change(sender);
end;

procedure TfSettings.CheckBox39Click(Sender: TObject);
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
