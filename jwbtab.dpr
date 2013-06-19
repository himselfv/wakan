program jwbtab;
{
Wakan TTextTable mgr tool.
}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows, TextTable, JWBIO;

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
  s := 'Usage: '+ExtractFilename(Paramstr(0))+' <table> <command> [/options option params]'#13#10
    +''#13#10
    +'<table> is a path and prefix to table files, i.e.'#13#10
    +'  .\some\path\Table'#13#10
    +''#13#10
    +'Supported commands:'#13#10
    +'  no command: display information'#13#10
    +'  export-text|dump -- export to text file / console'#13#10
    +'  import-text -- import from text file / keyboard'#13#10
    +'  dump-index <index-id> [/signatures] -- prints index contents'#13#10
    +'  check-index [index-id] -- checks all indices'#13#10;

  if errmsg<>'' then
    s := errmsg + #13#10#13#10 + s;

  writeln(s);
end;



var
  Command: string;
  TablePath: string; //common to many commands
  IndexName: string;

  DumpIndexParams: record
    DumpSignatures: boolean;
  end;

procedure ParseCommandLine();
var i: integer;
  s: string;
begin
 //Set to default
  Command := '';
  TablePath := '';
  IndexName := '';

 //No params at all => nothing to parse
  if ParamCount<1 then exit;

  TablePath := ParamStr(1);

  i := 2;
  while i<=ParamCount() do begin
    s := AnsiLowerCase(ParamStr(i));
    if Length(s)<=0 then continue;

   //Aliases
    if Command='dump' then
      Command := 'export-text';

   //Options
    if s[1]='/' then begin

     //Common options
     //none

     //Command-related options
      if Command='export-text' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
     //Command-related options
      if Command='import-text' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
      if Command='dump-index' then begin
        if s='/signatures' then
          DumpIndexParams.DumpSignatures := true
        else
          BadUsage('Invalid option: '+s);

      end else
      if Command='check-index' then begin
       //No options
        BadUsage('Invalid option: '+s);

      end else
        BadUsage('Invalid option: '+s);

    end else

   //Command
    if Command='' then begin
      Command := s;

      if Command='export-text' then begin
       //Nothing to initialize
      end else
      if Command='import-text' then begin
       //Nothing to initialize
      end else
      if Command='dump-index' then begin
        FillChar(DumpIndexParams, SizeOf(DumpIndexParams), 0);
      end else
      if Command='check-index' then begin
       //Nothing to initialize
      end else
        BadUsage('Invalid command or file: "'+s+'"');

    end else

   //Non-command non-option params (filename list etc)
    begin
     //Command-related options
      if Command='export-text' then begin
       //No options
        BadUsage('Invalid param: '+s);

      end else
      if Command='import-text' then begin
       //No options
        BadUsage('Invalid param: '+s);

      end else
      if Command='dump-index' then begin
        if IndexName='' then
          IndexName := s
        else
         //No options
          BadUsage('Invalid param: '+s);

      end else
      if Command='check-index' then begin
        if IndexName='' then
          IndexName := s
        else
         //No options
          BadUsage('Invalid param: '+s);

      end else
        BadUsage('Invalid param: "'+s+'"');
    end;

    Inc(i);
  end; //of ParamStr enumeration

 //Check that post-parsing conditions are met (non-conflicting options etc)
  if TablePath='' then
    BadUsage('Invalid table path.');
  if Command='dump-index' then
    if IndexName='' then
      BadUsage('dump-index needs index id or name');
end;

function DataTypeToStr(const dt: char): string;
begin
  case dt of
    'b': Result := 'byte';
    'w': Result := 'word';
    'i': Result := 'integer';
    'l': Result := 'bool';
    's': Result := 'AnsiString';
    'x': Result := 'WideString';
  else
    Result := '?';
  end;
end;

procedure RunInfo();
var tt: TTextTable;
  i: integer;
begin
  tt := TTextTable.Create(nil, TablePath, true, false);
  writeln('Table: '+TablePath);
  writeln('Records: '+IntToStr(tt.RecordCount));
  writeln('');
  writeln('Fields:');
  for i := 0 to Length(tt.fields) - 1 do
    writeln('  '+tt.Fields[i].Name+': '+DataTypeToStr(tt.Fields[i].DataType) + ' ['+tt.Fields[i].DataType+']');
  writeln('');
  writeln('Seeks:');
  for i := 0 to tt.seeks.Count - 1 do
    writeln('  '+tt.Seeks[i].Name+': '+tt.Seeks[i].Declaration);
  writeln('');
  writeln('Orders:');
  for i := 0 to tt.Orders.Count - 1 do
    if i<tt.Seeks.Count-1 then
      writeln('  '+tt.Orders[i]+' -> '+tt.Seeks[i+1].Name)
    else
      writeln('  '+tt.Orders[i]);
  FreeAndNil(tt);
end;

procedure RunExportText();
var tt: TTextTable;
  wri: TCustomFileWriter;
begin
  tt := TTextTable.Create(nil, TablePath, true, false);
  SetConsoleOutputCP(CP_UTF8);
  wri := TConsoleUTF8Writer.Create();
  tt.ExportToText(wri, '');
  FreeAndNil(wri);
  FreeAndNil(tt);
end;

procedure RunImportText();
var tt: TTextTable;
  rea: TCustomFileReader;
begin
  tt := TTextTable.Create(nil, TablePath, true, false);
  rea := TConsoleReader.Create();
  tt.ImportFromText(rea);
  FreeAndNil(rea);
  FreeAndNil(tt);
end;

type
  TTextTableEx = class(TTextTable)
  public
    procedure DumpIndex(wri: TCustomFileWriter; IndexId: integer; DumpSignatures: boolean);
  end;

procedure TTextTableEx.DumpIndex(wri: TCustomFileWriter; IndexId: integer; DumpSignatures: boolean);
var i: integer;
  RecId: integer;
  sign: string;
begin
  for i := 0 to Self.RecordCount - 1 do begin
    RecId := Self.TransOrder(i,IndexId);
    if DumpSignatures then begin
      sign := Self.SortRec(RecId, IndexId+1);
      wri.Writeln(Format('%.8d = %.8d = %s', [i, RecId, sign]));
    end else
      wri.Writeln(Format('%.8d = %.8d', [i, RecId]));
  end;
end;

function IndexParamToId(tt: TTextTable; const param: string): integer;
begin
  if TryStrToInt(param, Result) then begin
    if Result>=tt.Orders.Count then
      raise Exception.Create('No index with id='+IntToStr(Result));
  end else
  if not tt.Orders.Find(param, Result) then begin
    Result := tt.Seeks.Find(param);
    if Result<0 then
      raise Exception.Create('Index not found: '+param);
  end;
end;

procedure RunDumpIndex();
var tt: TTextTableEx;
  wri: TConsoleUTF8Writer;
  IndexId: integer;
begin
  SetConsoleOutputCP(CP_UTF8);
  tt := TTextTableEx.Create(nil, TablePath, true, false);
  IndexId := IndexParamToId(tt,IndexName);
  wri := TConsoleUTF8Writer.Create;
  tt.DumpIndex(wri, IndexId, DumpIndexParams.DumpSignatures);
  FreeAndNil(wri);
  FreeAndNil(tt);
end;

procedure RunCheckIndex();
var tt: TTextTable;
  IndexId, BadId: integer;
begin
  tt := TTextTable.Create(nil, TablePath, true, false);
  if IndexName<>'' then
    IndexId := IndexParamToId(tt,IndexName)
  else
    IndexId := -1;

  if IndexId>0 then begin
    if tt.CheckIndex(IndexId) then
      writeln('Index OK.')
    else
      writeln('Index invalid.')
  end else begin
    BadId := tt.CheckIndices;
    if BadId < 0 then
      writeln('All indices OK.')
    else
      writeln('Invalid index '+IntToStr(BadId));
  end;

  FreeAndNil(tt);
end;

begin
  try
    ParseCommandLine();

    if TablePath='' then
      ShowUsage()
    else
    if Command='' then
      RunInfo()
    else
    if Command='export-text' then
      RunExportText()
    else
    if Command='import-text' then
      RunImportText()
    else
    if Command='dump-index' then
      RunDumpIndex()
    else
    if Command='check-index' then
      RunCheckIndex()
    else
      BadUsage('Invalid command: '+Command);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
