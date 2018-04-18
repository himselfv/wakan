unit JWBFontSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, JWBSettings, IniFiles;

type
  TFontSettingsPage = class(TForm)
    btnAutodetectFonts: TButton;
    pnlJapaneseFonts: TGroupBox;
    pnlChineseFonts: TGroupBox;
    lblFontJapaneseGrid: TLabel;
    lblFontJapanese: TLabel;
    lblFontStrokeOrder: TLabel;
    edtFontJapaneseGrid: TEdit;
    edtFontJapanese: TEdit;
    edtFontStrokeOrder: TEdit;
    btnFontJapaneseGrid: TSpeedButton;
    btnFontJapanese: TSpeedButton;
    btnFontStrokeOrder: TSpeedButton;
    lblFontChineseGrid: TLabel;
    lblFontChineseGridGB: TLabel;
    lblFontChinese: TLabel;
    lblFontChineseGB: TLabel;
    btnFontChineseGrid: TSpeedButton;
    btnFontChineseGridGB: TSpeedButton;
    btnFontChinese: TSpeedButton;
    btnFontChineseGB: TSpeedButton;
    lblFontRadical: TLabel;
    btnFontRadical: TSpeedButton;
    edtFontChineseGrid: TEdit;
    edtFontChinese: TEdit;
    edtFontRadical: TEdit;
    edtFontChineseGridGB: TEdit;
    edtFontChineseGB: TEdit;
    lblFontChineseGBWarning: TLabel;
    lblFontSmall: TLabel;
    lblFontEnglish: TLabel;
    lblFontPinYin: TLabel;
    edtFontPinYin: TEdit;
    edtFontEnglish: TEdit;
    edtFontSmall: TEdit;
    btnFontSmall: TSpeedButton;
    btnFontEnglish: TSpeedButton;
    btnFontPinYin: TSpeedButton;
    procedure btnAutodetectFontsClick(Sender: TObject);
    procedure btnFontJapaneseGridClick(Sender: TObject);
    procedure btnFontEnglishClick(Sender: TObject);
    procedure btnFontSmallClick(Sender: TObject);
    procedure btnFontJapaneseClick(Sender: TObject);
    procedure btnFontChineseGridClick(Sender: TObject);
    procedure btnFontChineseClick(Sender: TObject);
    procedure btnFontRadicalClick(Sender: TObject);
    procedure btnFontChineseGridGBClick(Sender: TObject);
    procedure btnFontChineseGBClick(Sender: TObject);
    procedure btnFontStrokeOrderClick(Sender: TObject);
    procedure btnFontPinYinClick(Sender: TObject);
  protected
    procedure WmLoadSettings(var Msg: TMessage); message WM_LOADSETTINGS;
    procedure WmSaveSettings(var Msg: TMessage); message WM_SAVESETTINGS;
    procedure LoadSettings(ini: TCustomIniFile);
    procedure SaveSettings(ini: TCustomIniFile);

  protected
    procedure UpdateFontNames;
  public
    function AutoDetectFonts(Silent:boolean):boolean;
    procedure CheckFontsPresent;

  end;

var
  FontSettingsPage: TFontSettingsPage;

procedure Register;

implementation
uses ComCtrls, JWBLanguage, JWBFontSelect, JWBUnit;

{$R *.dfm}

procedure Register;
begin
  FontSettingsPage := TFontSettingsPage.Create(fSettings);
  fSettings.AddSettingsPage(FontSettingsPage, 'General');
end;

procedure TFontSettingsPage.WmLoadSettings(var Msg: TMessage);
begin
  LoadSettings(TCustomIniFile(msg.WParam));
end;

procedure TFontSettingsPage.WmSaveSettings(var Msg: TMessage);
begin
  SaveSettings(TCustomIniFile(msg.WParam));
end;

procedure TFontSettingsPage.LoadSettings(ini: TCustomIniFile);
begin
  if ini.ReadString('Fonts','FontSet','0')<>'1' then
    AutoDetectFonts({Silent=}true)
  else begin
   //There's some safety net for cases when font settings are added:
   //although we don't run detection again (it'd reset other fonts), we choose
   //a default font name for a new setting and later check if it's available.
    FontJapanese:=ini.ReadString('Fonts','Japanese','MS Mincho');
    FontJapaneseGrid:=ini.ReadString('Fonts','JapaneseGrid','MS Mincho');
    FontChinese:=ini.ReadString('Fonts','Chinese','MingLiU');
    FontChineseGrid:=ini.ReadString('Fonts','ChineseGrid','MingLiU');
    FontChineseGB:=ini.ReadString('Fonts','ChineseGB','SimSun');
    FontChineseGridGB:=ini.ReadString('Fonts','ChineseGridGB','SimSun');
    FontSmall:=ini.ReadString('Fonts','Small','MS Gothic');
    FontRadical:=ini.ReadString('Fonts','Radical','MingLiU');
    FontEnglish:=ini.ReadString('Fonts','English','Verdana');
    FontStrokeOrder:=ini.ReadString('Fonts','StrokeOrder','MS Mincho');
    FontPinYin:=ini.ReadString('Fonts','PinYin','Arial');
  end;
  UpdateFontNames;
end;

procedure TFontSettingsPage.SaveSettings(ini: TCustomIniFile);
begin
  ini.WriteString('Fonts','JapaneseGrid',FontJapaneseGrid);
  ini.WriteString('Fonts','Japanese',FontJapanese);
  ini.WriteString('Fonts','Small',FontSmall);
  ini.WriteString('Fonts','ChineseGrid',FontChineseGrid);
  ini.WriteString('Fonts','ChineseGridGB',FontChineseGridGB);
  ini.WriteString('Fonts','Chinese',FontChinese);
  ini.WriteString('Fonts','ChineseGB',FontChineseGB);
  ini.WriteString('Fonts','Radical',FontRadical);
  ini.WriteString('Fonts','English',FontEnglish);
  ini.WriteString('Fonts','StrokeOrder',FontStrokeOrder);
  ini.WriteString('Fonts','PinYin',FontPinYin);
  ini.WriteString('Fonts','FontSet','1');
end;


const
 {
 Recommended fonts for different styles.
 Overview: http://hayataki-masaharu.jp/web-typography-in-japanese/

 If you don't have Japanese locale installed, most of these can be not installed
 by default. Notably, Windows 10 has Yu Gothic anyway.

 Windows 10-optional fonts:
   https://docs.microsoft.com/en-us/windows/deployment/windows-10-missing-fonts

 "P-" versions of some fonts have non-monospaced (proportional) latin. We prefer
 monospaced ones but if pressed, these will do.

 "-UI" versions of many fonts have various differences. Some of these work,
 others are incompatible (e.g. narrower characters).
 }

  GothicFonts: array[0..7] of string = (
    //Since Windows 8.1, installed by default in Windows 10
    'Yu Gothic', 'YuGothic', '游ゴシック',

    //Since Windows 7
    'Meiryo', 'メイリオ',

    //Since XP. Ugly.
    'MS Gothic', 'ＭＳ ゴシック', 'MS PGothic'

    //No. Narrower characters.
    //'MS UI Gothic'
  );
  GothicFontsSummary: string = 'Yu Gothic/Meiryo/MS Gothic';

  MinchoFonts: array[0..5] of string = (
    //Since Windows 8.1
    'Yu Mincho', 'YuMincho', '游明朝',

    //Since XP. Okay-looking.
    'MS Mincho', 'ＭＳ 明朝', 'MS PMincho'
  );
  MinchoFontsSummary: string = 'Yu Mincho/MS Mincho';

 {
  Chinese fonts.
  Overview: https://webdesign.tutsplus.com/articles/the-complete-beginners-guide-to-chinese-fonts

  HKSCS = https://en.wikipedia.org/wiki/Hong_Kong_Supplementary_Character_Set
  (Read up on the historical Windows struggles)
  ExtB = https://en.wikipedia.org/wiki/CJK_Unified_Ideographs_Extension_B

  Some fonts have had seen a lot of enhancement versions such as _HKSCS, -ExtB
  and combinations. These days they're usually all implemented in a single
  font file but IF we have to resort to them, we should.
 }
  //Also known as songti
  ChineseSimplifiedSerifFonts: array[0..2] of string = (
    //+. Ideal size. Okay looking.
    //Since Windows XP. Present by default on Windows 10
    'SimSun', 'SimSun-ExtB',
    //Older version, even though it's "New" (N). No difference in looks.
    'NSimSun'
  );
  ChineseSimplifiedSerifFontsSummary = 'SimSun';

  ChineseTraditionalSerifFonts: array[0..2] of string = (
    //+. Ideal size. Okay-looking.
    //Since at least XP
    'MingLiU', '細明體', 'PMingLiU'

    //HKCS and ExtB extensions. These are only extension character sets.
    //Normally Windows auto-substitutes basic MingLiU for basic characters,
    //but Wakan circumvents this atm so extensions sets cover almost no characters.
    //'MingLiU_HKSCS', '細明體_HKSCS',
    //'MingLiU-ExtB', 'PMingLiU-ExtB', 'MingLiU_HKSCS-ExtB'
  );
  ChineseTraditionalSerifFontsSummary = 'YaHei/MingLiU';

  //Also known as heiti
  ChineseSimplifiedSansSerifFonts: array[0..2] of string = (
    //Since Windows Vista. Ugly-ish.
    'Microsoft YaHei', 'Microsoft YaHei UI',

    //Ugly-ish.
    'SimHei'
  );

  ChineseTraditionalSansSerifFonts: array[0..1] of string = (
    //Okay-looking.
    'Microsoft JhengHei', 'Microsoft JhengHei UI'
  );


//Silent: do not say anything in case of success
function TFontSettingsPage.AutoDetectFonts(Silent:boolean):boolean;
{$IFNDEF AUTOTEST}
var s:string;
  MissingFonts: string;
  SubstitutionFailed: boolean; //some font families missing entirely
  FMincho,FGothic: string;
  FMingLiu,FSimSun: string;

  function TryFonts(const fonts: array of string): string;
  var i: integer;
  begin
    Result := '';
    for i := Low(fonts) to High(fonts) do begin
      Result := FindFont(fonts[i], DEFAULT_CHARSET);
      if Result <> '' then
        break;
    end;
  end;

begin
  MissingFonts := '';
  SubstitutionFailed := false;

 { NOTE: Some fonts have multiple names, e.g. English one and localized one.
  Since Windows 2000 both CreateFont and EnumFontFamilies understand both names,
  although Delphi's Screen.Fonts will only list one. }

  //Japanese fonts
  FMincho := TryFonts(MinchoFonts);
  FGothic := TryFonts(GothicFonts);
  if FMincho='' then
    MissingFonts := MissingFonts + '* '+MinchoFontsSummary + #13;
  if FGothic='' then
    MissingFonts := MissingFonts + '* '+GothicFontsSummary + #13;

  //Substitutions
  if (FMincho='') and (FGothic<>'') then
    FMincho := FGothic
  else
  if (FMincho<>'') and (FGothic='') then
    FGothic := FMincho
  else
  if (FMincho='') and (FGothic='') then begin
    FMincho := FindFont('',SHIFTJIS_CHARSET); //any japanese
    if FMincho = '' then begin
      SubstitutionFailed := true; //no japanese at all
      FMincho := 'Arial';
    end;
    FGothic:=FMincho;
  end;

  //Chinese fonts
  FMingLiu := TryFonts(ChineseTraditionalSerifFonts);
  FSimSun := TryFonts(ChineseSimplifiedSerifFonts);
  if FMingLiu='' then
    MissingFonts := MissingFonts + '* '+ChineseTraditionalSerifFontsSummary + #13;
  if FSimSun='' then
    MissingFonts := MissingFonts + '* '+ChineseSimplifiedSerifFontsSummary + #13;

  //Substitute the default sans-serifs if the push comes to shove
  if FMingLiu='' then
    FMingLiu := TryFonts(ChineseTraditionalSansSerifFonts);
  if FSimSun='' then
    FSimSun := TryFonts(ChineseSimplifiedSansSerifFonts);

  //Substitutions
  if (FMingLiu='') and (FSimSun<>'') then
    FMingLiu := FSimSun
  else
  if (FMingLiu<>'') and (FSimSun='') then
    FSimSun := FMingLiu
  else
  if (FMingLiu='') and (FSimSun='') then begin //both missing
    FMingLiu := FindFont('',CHINESEBIG5_CHARSET);
    FSimSun := FindFont('',GB2312_CHARSET);
    if (FMingLiu='') or (FSimSun='') then begin
      SubstitutionFailed := true;
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
    s:=_l('#00578^eFollowing recommended fonts were not found on your system:')+#13#13;
    s:=s+missingfonts+#13;
    if not SubstitutionFailed then
      s:=s+_l('#00579^eReasonable substitution was found, however for better '
        +'font quality installation of these fonts and restart'#13'of the '
        +'autodetection routine is recommended.')
    else
      s:=s+_l('#00580^eNo reasonable substitution was found, program will '
        +'probably display characters incorrectly.'#13+'Installation of these '
        +'fonts and restart of the autodetection routine is HIGHLY recommended.');
    s:=s+#13#13;
    s:=s+_l('#00581^These fonts can usually be installed by enabling support for Japanese, '
    +'traditional Chinese and simplified Chinese writing in your operating system '
    +'optional components.'#13
    +'Please consult the internet for more information');
    if not SubstitutionFailed then
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
  result:=(missingfonts='') or (not SubstitutionFailed);
{$ELSE}
begin
  Result := false;
{$ENDIF}
end;

procedure TFontSettingsPage.btnAutodetectFontsClick(Sender: TObject);
begin
  AutoDetectFonts({Silent=}false);
end;

{$IFNDEF AUTOTEST}
function CheckFont(var face:string):boolean;
begin
  while (Length(face)>0) and (face[1]='!') do
    delete(face,1,1);
  Result := FindFont(face,DEFAULT_CHARSET)<>'';
  if not Result then
    face:='!'+face;
end;
{$ENDIF}

{ Checks that all fonts are configured correctly and present in the system.
If not, opens the configuration dialog to let the user choose some new ones.
Used at loading:
1. As a part of auto-detection (fonts which weren't detected can be left blank to later ask the user)
2. When new font setting is added to the app, the user will be asked on the next run to provide a font, if the default one is missing }
procedure TFontSettingsPage.CheckFontsPresent;
{$IFNDEF AUTOTEST}
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
    fSettings.pcPages.ActivePage := Self.Parent as TTabSheet; //our proxy in fSettings
    Self.ShowModal;
    Self.Position := OldPosition;
  end;
{$ELSE}
begin
{$ENDIF}
end;

//Updates font names on font selection page
procedure TFontSettingsPage.UpdateFontNames;
begin
  edtFontJapaneseGrid.Text:=FontJapaneseGrid;
  edtFontJapanese.Text:=FontJapanese;
  edtFontStrokeOrder.Text:=FontStrokeOrder;
  edtFontChineseGrid.Text:=FontChineseGrid;
  edtFontChineseGridGB.Text:=FontChineseGridGB;
  edtFontChinese.Text:=FontChinese;
  edtFontChineseGB.Text:=FontChineseGB;
  edtFontRadical.Text:=FontRadical;
  edtFontSmall.Text:=FontSmall;
  edtFontEnglish.Text:=FontEnglish;
  edtFontPinYin.Text:=FontPinYin;
end;

procedure TFontSettingsPage.btnFontJapaneseGridClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontJapaneseGrid:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edtFontJapaneseGrid.text,false);
  edtFontJapaneseGrid.Text:=FontJapaneseGrid;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontJapaneseClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontJapanese:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edtFontJapanese.text,false);
  edtFontJapanese.Text:=FontJapanese;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontChineseGridClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontChineseGrid:=ChooseFont([CHINESEBIG5_CHARSET],FS_CHINESE_CHARTEST,sup,edtFontChineseGrid.text,false);
  edtFontChineseGrid.Text:=FontChineseGrid;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontChineseClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontChinese:=ChooseFont([CHINESEBIG5_CHARSET],FS_CHINESE_CHARTEST,sup,edtFontChinese.text,false);
  edtFontChinese.Text:=FontChinese;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontChineseGridGBClick(Sender: TObject);
{$IFNDEF AUTOTEST}
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
  FontChineseGridGB:=ChooseFont([GB2312_CHARSET],FS_CHINESEGB_CHARTEST,sup,edtFontChineseGridGB.Text,false);
  edtFontChineseGridGB.Text:=FontChineseGridGB;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontChineseGBClick(Sender: TObject);
{$IFNDEF AUTOTEST}
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
  FontChineseGB:=ChooseFont([GB2312_CHARSET],FS_CHINESEGB_CHARTEST,sup,edtFontChineseGB.Text,false);
  edtFontChineseGB.Text:=FontChineseGB;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontRadicalClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  Application.MessageBox(
    pchar(_l('#00564^ePlease make sure that the font you select is a Unicode font with '
      +'complete "CJK Unified Ideographs" range.'#13'MingLiu is an example of such font.')),
    pchar(_l('#00364^eNotice')),
    MB_OK or MB_ICONINFORMATION);
  FontRadical:=ChooseFont([CHINESEBIG5_CHARSET],FS_RADICAL_CHARTEST,sup,edtFontRadical.Text,false);
  edtFontRadical.Text:=FontRadical;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontStrokeOrderClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontStrokeOrder:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edtFontStrokeOrder.text,false);
  edtFontStrokeOrder.Text:=FontStrokeOrder;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontEnglishClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontEnglish:=ChooseFont([ANSI_CHARSET],FS_ENGLISH_CHARTEST,sup,edtFontEnglish.text,false);
  edtFontEnglish.Text:=FontEnglish;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontSmallClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontSmall:=ChooseFont([SHIFTJIS_CHARSET],FS_JAPANESE_CHARTEST,sup,edtFontSmall.text,false);
  edtFontSmall.Text:=FontSmall;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFontSettingsPage.btnFontPinYinClick(Sender: TObject);
{$IFNDEF AUTOTEST}
var sup:string;
begin
  FontPinYin:=ChooseFont([ANSI_CHARSET],FS_PINYIN_CHARTEST,sup,edtFontPinYin.text,false);
  edtFontPinYin.Text:=FontPinYin;
{$ELSE}
begin
{$ENDIF}
end;

end.
