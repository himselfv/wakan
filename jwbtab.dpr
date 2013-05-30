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
    +'  export-text -- export to text file / console'#13#10
    +'  import-text -- import from text file / keyboard'#13#10
    +'  dump-index <index-id> -- prints index contents';

  if errmsg<>'' then
    s := errmsg + #13#10#13#10 + s;

  writeln(s);
end;



var
  Command: string;
  TablePath: string; //common to many commands
  IndexName: string;

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
    s := ParamStr(i);
    if Length(s)<=0 then continue;

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
     //Command-related options
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

procedure RunDumpIndex();
var tt: TTextTable;
  IndexId, i: integer;
begin
  tt := TTextTable.Create(nil, TablePath, true, false);
  if TryStrToInt(IndexName, IndexId) then begin
    if IndexId>=tt.Orders.Count then
      raise Exception.Create('No index with id='+IntToStr(IndexId));
  end else
  if not tt.Orders.Find(IndexName, IndexId) then begin
    IndexId := tt.Seeks.Find(IndexName);
    if IndexId<0 then
      raise Exception.Create('Index not found: '+IndexName);
  end;
  for i := 0 to tt.RecordCount - 1 do
    writeln(Format('%.8d = %.8d', [i, tt.TransOrder(i,IndexId)]));
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
      BadUsage('Invalid command: '+Command);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
