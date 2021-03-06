; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Wakan"
; There's better and heavier way via ParseVersion but this will suffice
#define MyAppVersion StringChange(GetFileVersion('..\Release\Wakan.exe'), '.0.0', '')
#define MyAppPublisher "Wakan developers"
#define MyAppURL "https://bitbucket.org/himselfv/wakan/"
#define MyAppExeName "wakan.exe"

#define CommonData "{app}"
;#define CommonData "{userappdata}\Wakan"  ; this is not correct; wakan can be used by different users


[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{FE77F2CB-64CA-4024-8A07-DAF151D8B3A7}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=.\Output
OutputBaseFilename=wakan-{#MyAppVersion}-setup
Compression=lzma
SolidCompression=yes
SetupIconFile="..\wakan.ico"
UninstallDisplayIcon={app}\{#MyAppExeName}

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Messages]
WelcomeLabel2=This will install [name/ver] on your computer.%n%nIf you're upgrading from Wakan 1.67 or earlier, please uninstall that version first. Your user data will be preserved.

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[InstallDelete]
Type: files; Name: {app}\wakan.cdt

[Files]
Source: "..\Release\wakan.exe"; DestDir: "{app}"; DestName: "wakan.exe"; Flags: ignoreversion
Source: "..\Release\7z.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\wakanh.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\wakan.cfg"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Components.ini"; DestDir: "{app}"; Flags: ignoreversion
; Install default "standalone" wakan.ini
; Keep existing one (it can contain settings)
Source: "wakan.ini"; DestDir: "{app}"; Flags: ignoreversion onlyifdoesntexist
; Languages
Source: "..\Release\lng\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; Flags
Source: "..\Release\Flags\*"; DestDir: "{app}\Flags"; Flags: ignoreversion recursesubdirs createallsubdirs
; Help
Source: "..\Release\wakan_cz.chm"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\wakan_en.chm"; DestDir: "{app}"; DestName: "wakan.chm"; Flags: ignoreversion
; Dictionaries (optional but included in 1.67)
Source: "..\Release\ccedict.dic"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\edict2.dic"; DestDir: "{app}"; Flags: ignoreversion
; Character and other data
Source: "..\Release\wakan.chr"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\wakan.rad"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\wakan.sod"; DestDir: "{app}"; Flags: ignoreversion
; Examples (optional)
Source: "..\Release\examples_j.pkg"; DestDir: "{app}"; Flags: ignoreversion
; Raw data (.rad and .sod can be rebuilt from this)
Source: "..\Release\radicals.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\strokes.csv"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\wordfreq_ck"; DestDir: "{app}"; Flags: ignoreversion
; Utils (optional)
Source: "..\Release\uniconv.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\btuc21d3.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\jwbpkg.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\jwbtab.exe"; DestDir: "{app}"; Flags: ignoreversion
; Transliterations
Source: "..\Release\Awful kiriji.roma"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Czech.roma"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Hepburn.roma"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Kiriji - Polivanov.roma"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Kunreishiki.roma"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\PinYin.rpy"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Wade-Giles.rpy"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Yale.rpy"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Palladius.rpy"; DestDir: "{app}"; Flags: ignoreversion
; Expression copy formats
Source: "..\Release\ExprCopyFormats\EDICT line.xslt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
Source: "..\Release\ExprCopyFormats\example-xml.txt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
Source: "..\Release\ExprCopyFormats\Expression (HTML ruby).xslt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
Source: "..\Release\ExprCopyFormats\Expression.xslt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
Source: "..\Release\ExprCopyFormats\HTML definition list.xslt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
Source: "..\Release\ExprCopyFormats\Text line, no numbers.xslt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
Source: "..\Release\ExprCopyFormats\Text line.xslt"; DestDir: "{#CommonData}\ExprCopyFormats"; Flags: ignoreversion
; Expression links
Source: "..\Release\ExprLinks\00. Look up in Google.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\01. Look up IMI in Google.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\02. Look up DOUIGO in Google.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\03. Kotobank.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\04. Japanese Wikipedia.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\05. Wiktionary EN.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\06. Wiktionary JP.url"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\google.ico"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\kotobank.ico"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\wikipedia.ico"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\wiktionary-en.ico"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
Source: "..\Release\ExprLinks\wiktionary-ja.ico"; DestDir: "{#CommonData}\ExprLinks"; Flags: ignoreversion
; Kanji copy formats
Source: "..\Release\KanjiCopyFormats\Bracket Text.xslt"; DestDir: "{#CommonData}\KanjiCopyFormats"; Flags: ignoreversion
Source: "..\Release\KanjiCopyFormats\example-xml.txt"; DestDir: "{#CommonData}\KanjiCopyFormats"; Flags: ignoreversion
Source: "..\Release\KanjiCopyFormats\HTML.xslt"; DestDir: "{#CommonData}\KanjiCopyFormats"; Flags: ignoreversion
Source: "..\Release\KanjiCopyFormats\Text.xslt"; DestDir: "{#CommonData}\KanjiCopyFormats"; Flags: ignoreversion
; Kanji links
Source: "..\Release\KanjiLinks\example-link.txt"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\KanjiProject.url"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\UniHan.ico"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\UniHan.url"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\Wiktionary EN.url"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\Wiktionary JP.url"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\wiktionary-en.ico"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\wiktionary-ja.ico"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\WWWJDIC.ico"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\WWWJDIC.url"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\ZhongWen.ico"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion
Source: "..\Release\KanjiLinks\ZhongWen.url"; DestDir: "{#CommonData}\KanjiLinks"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Parameters: "upgradelocaldata"; Flags: runascurrentuser;
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
