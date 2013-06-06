program jwbpkg;
{
Wakan package editor tool.
}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Masks, Windows, MemSource, JWBStrings, PKGWrite;

function KnownPackagesFilename: string; forward;

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
    +'  For known packages (see '+ExtractFilename(KnownPackagesFilename)+') codes can be omitted.'#13#10
    +''#13#10
    +'Supported commands:'#13#10
    +'  no command: display information'#13#10
    +'  extract <filename|*> <target path>'#13#10
    +'  cat <filename> -- print file on screen'#13#10
    +'  pack <foldername> [foldername]'#13#10
    +'   [/name <name>] [/title <title>] [/company <company>] [/copyright <copyrigh>]'#13#10
    +'   [/format <format>] [/comment <comment>] [/version <version>]'#13#10
    +'Common options:'#13#10
    +'  /force -- overwrite files';

  if errmsg<>'' then
    s := errmsg + #13#10#13#10 + s;

  writeln(s);
end;


{
It's possible to put codes for commonly used packages into a file called jwbpkg.cfg.
You can also use masks:
  *.dic HeaderCode FilesysCode CryptCode
After that just omit the codes:
  jwbpkg test.dic pack "C:\test\contents"
}

var
  KnownPackages: array of record
    mask: string;
    HeaderCode: integer;
    FilesysCode: integer;
    CryptCode: integer;
  end;

function EatWord(var s: string): string;
var i: integer;
begin
   i := pos(' ', s);
   if i<=0 then begin
     Result := s;
     s := '';
   end else begin
     Result := copy(s, 1, i-1);
     s := Trim(copy(s, i+1, Length(s)-i));
   end;
end;

function KnownPackagesFilename: string;
begin
  Result := ChangeFileExt(Paramstr(0), '.cfg');
end;

procedure LoadKnownPackages;
var cfg: TStringList;
  cfgfname: string;
  i,pkgno: integer;
  line: string;
begin
  cfg := TStringList.Create;
  try
    cfgfname := KnownPackagesFilename;
    SetLength(KnownPackages, 0);
    if not FileExists(cfgfname) then exit; //it's okay
    cfg.LoadFromFile(cfgfname);

    for i := 0 to cfg.Count - 1 do try
      line := Trim(cfg[i]);
      if Length(line)<=0 then continue;
      if line[1]=';' then continue; //comment

      pkgno := Length(KnownPackages);
      SetLength(KnownPackages,pkgno+1);
      KnownPackages[pkgno].mask := EatWord(line);
      KnownPackages[pkgno].HeaderCode := StrToInt(EatWord(line));
      KnownPackages[pkgno].FilesysCode := StrToInt(EatWord(line));
      KnownPackages[pkgno].CryptCode := StrToInt(EatWord(line));
      if line<>'' then
        raise Exception.Create('Unexpected text after the end of rule: "'+line+'"');
    except
      on E: Exception do begin
        E.Message := 'Cannot load configuration file; cannot parse the line: "'
          +cfg[i]+'". '+E.Message;
        raise;
      end;
    end;
  finally
    FreeAndNil(cfg);
  end;
end;

function FindKnownPackage(const filename: string): integer;
var i: integer;
  filename_only: string;
begin
  Result := -1;
  filename_only := ExtractFilename(filename);
  for i := 0 to Length(KnownPackages) - 1 do
    if MatchesMask(filename, KnownPackages[i].mask)
    or MatchesMask(filename_only, KnownPackages[i].mask) then begin
      Result := i;
      break;
    end;
end;


var
  Command: string;
  HeaderCode: integer;
  FilesysCode: integer;
  CryptCode: integer;
  PackageFile: string; //common to many commands
  PackedFile: string; //common to many commands, one file path in the archive
  ExtractTarget: string; //common to many commands, file or folder path outside the archive
  ForceOverwrite: boolean;

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

  procedure ReadPackageFileParam();
  var knownid: integer;
    i_stored: integer;

    procedure RollbackUseKnown;
    begin
      i := i_stored;
      HeaderCode := KnownPackages[knownid].HeaderCode;
      FilesysCode := KnownPackages[knownid].FilesysCode;
      CryptCode := KnownPackages[knownid].CryptCode;
    end;

  begin
    i_stored := i;
    PackageFile := ParamStr(i);

   { If we have known package we'll assume any errors => there's just no package info,
    so we have to roll back and reparse it as normal stuff }
    knownid := FindKnownPackage(PackageFile);

    if i+3 > ParamCount() then
      if knownid>=0 then begin
        RollbackUseKnown;
        exit;
      end else
        BadUsage('Package file needs codes');
    Inc(i);
    if not TryStrToInt(ParamStr(i), HeaderCode) then
      if knownid>=0 then begin
        RollbackUseKnown;
        exit;
      end else
        BadUsage('Invalid header code: '+ParamStr(i));
    Inc(i);
    if not TryStrToInt(ParamStr(i), FilesysCode) then
      if knownid>=0 then begin
        RollbackUseKnown;
        exit;
      end else
        BadUsage('Invalid fs code: '+ParamStr(i));
    Inc(i);
    if not TryStrToInt(ParamStr(i), FilesysCode) then
      if knownid>=0 then begin
        RollbackUseKnown;
        exit;
      end else
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
  PackedFile := '';
  ExtractTarget := '';
  ForceOverwrite := false;

 //No params at all => nothing to parse
  if ParamCount<1 then exit;

 //First several params must always be a package description
  i := 1;
  ReadPackageFileParam;
  Inc(i);

 //Then commands
  while i<=ParamCount() do begin
    s := ParamStr(i);
    if Length(s)<=0 then continue;

   //Options
    if s[1]='/' then begin

     //Common options
      if s='/force' then
        ForceOverwrite := true
      else

     //Command-related options
      if Command='extract' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
     //Command-related options
      if Command='cat' then begin
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
      if Command='cat' then begin
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
        if PackedFile='' then
          PackedFile := ParamStr(i)
        else
        if ExtractTarget='' then
          ExtractTarget := ParamStr(i)
        else
          BadUsage('extract does not support more than one source and destination: '+ParamStr(i));
      end else
      if Command='cat' then begin
        if PackedFile='' then
          PackedFile := ParamStr(i)
        else
          BadUsage('cat does not support more than one source: '+ParamStr(i));
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
    if PackedFile='' then
      BadUsage('Internal file name not specified');
  end;
  if (Command='cat') then begin
    if PackedFile='' then
      BadUsage('Internal file name not specified');
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
  writeln(package.Name);
  writeln(package.Header);
  files := package.GetFileList;
  for i := 0 to files.Count - 1 do
    writeln(files[i]);
  FreeAndNil(package);
end;

//Maybe raise EAbort if the user aborts, otherwise returns True to overwrite, False to skip
function ConfirmOverwrite(const TargetFile, SourceFile: string): boolean;
var line: string;
begin
  repeat
    writeln('File '+TargetFile+' already exists!');
    write('Do you want to replace it with the extracted file '+SourceFile+'? [Yes/No/Abort]: ');
    Readln(line);
    line := AnsiUpperCase(line);
    if line='Y' then begin
      Result := true;
      exit;
    end else
    if line='N' then begin
      Result := false;
      exit;
    end else
    if line='A' then
      raise EAbort.Create('User aborted.');
  until false;
end;

procedure RunExtract();
var i: integer;
  files: TStringList;
  filename: string;
  mfile: TMemoryFile;
begin
  package := TPackageSource.Create(PackageFile, HeaderCode, FileSysCode, CryptCode);

  if PackedFile<>'*' then begin
   { Any of these are accepted:
       empty string    => fname.ext
       dir\name        => dir\name\fname.ext
       dir\name\       => dir\name\fname.ext
       new\fname       => new\fname }
    if IsPathDelimiter(ExtractTarget, Length(ExtractTarget))
    or DirectoryExists(ExtractTarget)
    or (ExtractTarget='') then begin
      if ExtractTarget<>'' then
        ExtractTarget := IncludeTrailingPathDelimiter(ExtractTarget);
      ExtractTarget := ExtractTarget + ExtractFilename(PackedFile);
    end;

    mfile := package.Files[PackedFile];
    if mfile=nil then
      raise Exception.Create('File '+PackedFile+' not found in the package.');
    ForceDirectories2(ExtractFilePath(ExtractTarget));
    if (not ForceOverwrite) and FileExists(ExtractTarget) then
      if not ConfirmOverwrite(ExtractTarget, PackedFile) then exit;
    mfile.SaveToFile(ExtractTarget);
  end else begin
   //If not specified, extract to a folder with automatic name
    if ExtractTarget='' then begin
      ExtractTarget := ChangeFileExt(ExtractFilename(PackageFile),'');
      if FileExists(ExtractTarget) then
        raise Exception.Create('File '+ExtractFilename(ExtractTarget)+' exists, '
          +'cannot extract to default folder, please specify target path.');
    end;
    ExtractTarget := IncludeTrailingPathDelimiter(ExtractTarget);

    files := package.GetFileList;
    for i := 0 to files.Count - 1 do begin
      filename := ExtractTarget+files[i];
      if (not ForceOverwrite) and FileExists(filename) then
        if not ConfirmOverwrite(filename, files[i]) then continue;
      writeln(filename);
      ForceDirectories2(ExtractFilePath(filename)); //original dir as well as any internal substructure
      package.Files[files[i]].SaveToFile(filename);
    end;
  end;

  FreeAndNil(package);
end;

procedure RunCat();
var mfile: TMemoryFile;
  inp: TStream;
  outp: TFileStream;
begin
  package := TPackageSource.Create(PackageFile, HeaderCode, FileSysCode, CryptCode);
  mfile := package.Files[PackedFile];
  if mfile=nil then
    raise Exception.Create('File '+PackedFile+' not found in the package.');
  outp := TFileStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
  inp := mfile.Lock;
  try
    outp.CopyFrom(inp, 0);
  finally
    mfile.Unlock;
    FreeAndNil(outp);
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
    pack.Builder := 'jwbpkg';
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
    LoadKnownPackages();
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
    if Command='cat' then
      RunCat()
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
