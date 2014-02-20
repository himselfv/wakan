unit JWBCommandLine;

interface
uses SysUtils, JWBStrings;

type
  EBadUsage = class(Exception);

procedure BadUsage(msg: string);
procedure ShowUsage(errmsg: string = '');

var
  Command: string;
  SkipAutoRepair: boolean;

 { Each block of params is only valid (initialized) if that command is in Command }

  OpenParams: record
    Filename: string;
  end;

  MakeDicParams: record
    Name: string;
    Description: string;
    Language: char;
    Files: TFilenameList;
    AddFrequencyInfo: boolean;
  end;

  MakeRadParams: record
    Files: TFilenameList;
  end;

 {
  Instructs Wakan to update dictionaries even if it sees no need to.
  If files are specified, only those are updated. Extension is appended
  automatically, when needed. Files from other directories can be specified,
  source files are looked there then.
  Update is forceful, and if something breaks Wakan will not silently ignore that.
 }
  UpdateDicsParams: record
    Files: TFilenameList;
  end;

  MakeCharsParams: record
    ResetDb: boolean;
    KanjidicFilename: string;
    UnihanFolder: string;
  end;

procedure ParseCommandLine();

implementation
uses Forms, Windows, JWBUnit;

procedure BadUsage(msg: string);
begin
  raise EBadUsage.Create(msg);
end;

procedure ShowUsage(errmsg: string);
var s: string;
  flags: cardinal;
begin
  if errmsg<>'' then flags := MB_ICONERROR else flags := MB_ICONINFORMATION;

  s := 'Usage: '+ExtractFilename(Paramstr(0))+' <command> [/options option params]'#13
    +'Supported commands:'#13
    +'* open <filename>'#13
    +'* makeexamples'#13
    +'* makesod'#13
    +'* makerad [RADKFILE_filename] [...]'#13
    +'* makechars [/resetdb] [/kanjidic <kanjidic-filename>] [/unihan <unihan-folder>]'#13
    +'* makedic <dicfilename> </include filename> [/include filename] [/name dic_name] '
      +'[/description text] [/copyright text] [/priority int] [/version text] '
      +'[/language <j|c>] [/unicode] [/addwordindex] [/addcharacterindex] '
      +'[/addfrequencyinfo]'#13
    +'* updatedics [dicname dicname ...]'
    +'Supported flags:'
    +'* [/fast]';

  if errmsg<>'' then
    s := errmsg + #13#13 + s;

  Application.MessageBox(
    PChar(s),
    PChar('WaKan '+WakanVer),
    flags or MB_OK
  );
end;

procedure ParseCommandLine();
var i: integer;
  s: string;
begin
 //Set to default
  Command := '';
  SkipAutoRepair := false;

 //Parse
  i := 1;
  while i<=ParamCount() do begin
    s := ParamStr(i);
    if Length(s)<=0 then continue;

   //Options
    if s[1]='/' then begin

     //Common options
      if s='/fast' then begin
        SkipAutoRepair := true;
      end else

     //Command-related options
      if Command='makeexamples' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
      if Command='makesod' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
      if Command='makerad' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
      if Command='makedic' then begin
        if s='/name' then begin
          Inc(i);
          if i>ParamCount() then BadUsage('/name requires name value');
          MakeDicParams.Name := ParamStr(i);
        end else
        if s='/description' then begin
          Inc(i);
          if i>ParamCount() then BadUsage('/description requires description value');
          MakeDicParams.Description := ParamStr(i);
        end else
        if s='/language' then begin
          Inc(i);
          if i>ParamCount() then BadUsage('/language requires language value');
          s := ParamStr(i);
          if (Length(s)<=0) or (Length(s)>1) or ((s[1]<>'j') and (s[1]<>'c')) then
            BadUsage('invalid /language value');
          MakeDicParams.Language := s[1];
        end else
        if s='/include' then begin
          Inc(i);
          if i>ParamCount() then BadUsage('/include requires file name');
          s := ParamStr(i);
          if s='' then BadUsage('invalid /include file name');
          AddFilename(MakeDicParams.Files, s);
        end else
        if s='/addfrequencyinfo' then begin
          MakeDicParams.AddFrequencyInfo := true;
        end else
          BadUsage('Invalid option: '+s);

      end else
      if Command='makechars' then begin
        if s='/resetdb' then begin
          MakeCharsParams.ResetDb := true;
        end else
        if s='/kanjidic' then begin
          Inc(i);
          if i>ParamCount() then BadUsage('/kanjidic requires file name');
          MakeCharsParams.KanjidicFilename := ParamStr(i);
        end else
        if s='/unihan' then begin
          Inc(i);
          if i>ParamCount() then BadUsage('/unihan requires folder path');
          MakeCharsParams.UnihanFolder := ParamStr(i);
        end else
          BadUsage('Invalid option: '+s);

      end else
        BadUsage('Invalid option: '+s);

    end else


   //Command
    if Command='' then begin
      Command := s;

      if Command='makeexamples' then begin
       //Nothing to initialize
      end else
      if Command='makesod' then begin
       //Nothing to initialize
      end else
      if Command='makerad' then begin
        FillChar(MakeRadParams, sizeof(MakeRadParams), 0);
       //Filenames are expected in the following params.
       //If none are specified, that's fine too (see this command handling).
      end else
      if Command='makedic' then begin
        FillChar(MakeDicParams, sizeof(MakeDicParams), 0);
        Inc(i);
        if i>ParamCount() then BadUsage('"makedic" requires dictionary file name');
        MakeDicParams.Name := Paramstr(i);
        MakeDicParams.Language := 'j';
       //but no frequency info by default because it requires additional file which may be missing
      end else
      if Command='updatedics' then begin
        FillChar(UpdateDicsParams, sizeof(UpdateDicsParams), 0);
       //Filenames are expected in the following params.
       //If none are specified, update ALL dics
      end else
      if Command='makechars' then begin
        FillChar(MakeCharsParams, sizeof(MakeCharsParams), 0);
      end else
      if Command='open' then begin
        FillChar(OpenParams, sizeof(OpenParams), 0);
        Inc(i);
        if i>ParamCount() then BadUsage('"open" requires file name');
        OpenParams.Filename := ParamStr(i);
      end else
      if FileExists(s) then begin
        FillChar(OpenParams, sizeof(OpenParams), 0);
        Command := 'open';
        OpenParams.Filename := s;
      end else
        BadUsage('Invalid command or file: "'+s+'"');

    end else

   //Non-command non-option params (filename list etc)
    begin
      if Command='makerad' then begin
        AddFilename(MakeRadParams.Files, ParamStr(i));

      end else
      if Command='updatedics' then begin
        AddFilename(UpdateDicsParams.Files, ParamStr(i));

      end else
        BadUsage('Invalid param: "'+s+'"');

    end;

    Inc(i);
  end; //of ParamStr enumeration

 //Check that post-parsing conditions are met (non-conflicting options etc)
  if Command='makedic' then begin
    if Length(MakeDicParams.Files)<0 then
      BadUsage('makedic requires at least one input file');
  end;
  if Command='makechars' then begin
    if (MakeCharsParams.KanjidicFilename='')
    and (MakeCharsParams.UnihanFolder='') then
      BadUsage('makechars requires /kanjidic filename or /unihan folder');
  end;

end;

end.
