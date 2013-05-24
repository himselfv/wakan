program jwbpkg;
{
Wakan package editor tool.
}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, MemSource, JWBStrings, PKGWrite;

type
  EBadUsage = class(Exception);

procedure BadUsage(msg: string); forward;
procedure ShowUsage(const errmsg: string = ''); forward;

procedure BadUsage(msg: string);
begin
  raise EBadUsage.Create(msg);
end;

procedure ShowUsage(const errmsg: string);
var s: string;
begin
  s := 'Usage: '+ExtractFilename(Paramstr(0))+' <package> <command> [/options option params]'#13#10
    +''#13#10
    +'Package requires header, fs and crypt codes:'#13#10
    +'  <package> == <filename> <header> <fs> <crypt>'#13#10
    +''#13#10
    +'Supported commands:'#13#10
    +'  no command: display information'#13#10
    +'  extract <foldername>'#13#10
    +'  pack <foldername> [foldername]'#13#10
    +'   [/name <name>] [/title <title>] [/company <company>] [/copyright <copyrigh>]'#13#10
    +'   [/format <format>] [/comment <comment>] [/version <version>]';

  if errmsg<>'' then
    s := errmsg + #13#10#13#10 + s;

  writeln(s);
end;


var
  Command: string;
  HeaderCode: integer;
  FilesysCode: integer;
  CryptCode: integer;
  PackageFile: string; //common to many commands
  ExtractedDir: string; //common to many commands

  PackParams: record
    Sources: array of string;
    Name: string;
    Title: string;
    Company: string;
    Copyright: string;
    Format: string;
    Comment: string;
    Version: string;
  end;

procedure ParseCommandLine();
var i: integer;
  s: string;

  procedure ReadPackageFile();
  begin
    PackageFile := ParamStr(i);
    if i+3 > ParamCount() then
      BadUsage('Package file needs codes');
    Inc(i);
    if not TryStrToInt(ParamStr(i), HeaderCode) then
      BadUsage('Invalid header code: '+ParamStr(i));
    Inc(i);
    if not TryStrToInt(ParamStr(i), FilesysCode) then
      BadUsage('Invalid fs code: '+ParamStr(i));
    Inc(i);
    if not TryStrToInt(ParamStr(i), FilesysCode) then
      BadUsage('Invalid crypt code: '+ParamStr(i));
  end;

  function NextParam(const param: string): string;
  begin
    Inc(i);
    if i > ParamCount() then
      BadUsage(param+' requires value');
    Result := ParamStr(i);
  end;

begin
 //Set to default
  Command := '';
  HeaderCode := 0;
  FilesysCode := 0;
  CryptCode := 0;
  PackageFile := '';
  ExtractedDir := '';

 //No params at all => nothing to parse
  if ParamCount<1 then exit;

 //First several params must always be a package description
  i := 1;
  ReadPackageFile;
  Inc(i);

 //Then commands
  while i<=ParamCount() do begin
    s := ParamStr(i);
    if Length(s)<=0 then continue;

   //Options
    if s[1]='/' then begin

     //Common options
     //none.

     //Command-related options
      if Command='extract' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
      if Command='pack' then begin
        if s='/name' then
          PackParams.Name := NextParam('/name')
        else
        if s='/title' then
          PackParams.Title := NextParam('/title')
        else
        if s='/company' then
          PackParams.Company := NextParam('/company')
        else
        if s='/copyright' then
          PackParams.Copyright := NextParam('/copyright')
        else
        if s='/format' then
          PackParams.Format := NextParam('/format')
        else
        if s='/comment' then
          PackParams.Comment := NextParam('/comment')
        else
        if s='/version' then
          PackParams.Version := NextParam('/version')
        else
          BadUsage('Invalid option: '+s);

      end else
        BadUsage('Invalid option: '+s);

    end else

   //Command
    if Command='' then begin
      Command := s;

      if Command='extract' then begin
       //Nothing to initialize
      end else
      if Command='pack' then begin
        FillChar(PackParams, sizeof(PackParams), 0);

      end else
        BadUsage('Invalid command or file: "'+s+'"');

    end else

   //Non-command non-option params (filename list etc)
    begin
      if Command='extract' then begin
        if ExtractedDir='' then
          ExtractedDir := ParamStr(i)
        else
          BadUsage('extract does not support more than one destination: '+ParamStr(i));
      end else
      if Command='pack' then begin
        SetLength(PackParams.Sources, Length(PackParams.Sources)+1);
        PackParams.Sources[Length(PackParams.Sources)-1] := ParamStr(i);
      end else
        BadUsage('Invalid param: "'+s+'"');

    end;

    Inc(i);
  end; //of ParamStr enumeration

 //Check that post-parsing conditions are met (non-conflicting options etc)
  if (Command='extract') then begin
    if ExtractedDir='' then
      BadUsage('Path for extracted files not specified');
  end;
  if (Command='pack') then begin
    if Length(PackParams.Sources)<=0 then
      BadUsage('Source path not specified');
  end;

end;


var
  Package: TPackageSource;

procedure RunInfo();
var i: integer;
  files: TStringList;
begin
  package := TPackageSource.Create(PackageFile, HeaderCode, FileSysCode, CryptCode);
  files := package.GetFileList;
  for i := 0 to files.Count - 1 do
    writeln(files[i]);
  FreeAndNil(package);
end;

procedure RunExtract();
var i: integer;
  files: TStringList;
  filename: string;
begin
  package := TPackageSource.Create(PackageFile, HeaderCode, FileSysCode, CryptCode);

  files := package.GetFileList;
  for i := 0 to files.Count - 1 do begin
    filename := ExtractedDir+'\'+files[i];
    writeln(filename);
    ForceDirectories2(ExtractFilePath(filename)); //original dir as well as any internal substructure
    package.Files[files[i]].SaveToFile(filename);
  end;

  FreeAndNil(package);
end;

type
  TLogClient = class
  public
    procedure HandleLogMessage(Sender: TObject; const msg: string);
  end;

procedure TLogClient.HandleLogMessage(Sender: TObject; const msg: string);
begin
  writeln(msg);
end;

procedure RunPack();
var pack: TPackageBuilder;
  logcli: TLogClient;
  i: integer;
  fname: string;
begin

  logcli := TLogClient.Create;
  pack := TPackageBuilder.Create;
  try
    pack.OnLogMessage := logcli.HandleLogMessage;
    pack.PackageFile := PackageFile;
    pack.MemoryLimit := 100000000;
    pack.Name := AnsiString(PackParams.Name);
    pack.TitleName := AnsiString(PackParams.Title);
    pack.CompanyName := AnsiString(PackParams.Company);
    pack.CopyrightName := AnsiString(PackParams.Copyright);
    pack.FormatName := AnsiString(PackParams.Format);
    pack.CommentName := AnsiString(PackParams.Comment);
    pack.VersionName := AnsiString(PackParams.Version);
    pack.HeaderCode := HeaderCode;
    pack.FilesysCode := FilesysCode;
    pack.WriteHeader;
    pack.LoadMode := lmTemporaryLoad;
    pack.CryptMode := 0;
    pack.CrcMode := 0;
    pack.PackMode := 0;
    pack.CryptCode := CryptCode;
    for i := 0 to Length(PackParams.Sources) - 1 do begin
      fname := PackParams.Sources[i];
      if Length(fname)<=0 then continue; //wtf user
    {
      This would have been useful it Include() accepted file mask, but due to
      the way things work in TPackageBuilder, it accepts directory only.
      Will fix that someday so the code is here, till then you can only add dirs.

      if fname[Length(fname)]='\' then
        fname := fname + '*.*' //clearly a dir
      else
      if (pos('*',fname)<=0) and (pos('?',fname)<=0) and DirectoryExists(fname) then
        fname := fname + '\*.*';
    }
      pack.Include(fname);
    end;

    pack.Finish;
  finally
    FreeAndNil(pack);
    FreeAndNil(logcli);
  end;

end;

begin
  try
    ParseCommandLine();

    if PackageFile='' then
      ShowUsage()
    else
    if Command='' then
      RunInfo()
    else
    if Command='extract' then
      RunExtract()
    else
    if Command='pack' then
      RunPack()
    else
      BadUsage('Invalid command: '+Command);

  except
    on E: EBadUsage do
      ShowUsage(E.Message);
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
