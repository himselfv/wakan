unit JWBUnpackJob;
{ Core archive unpacking. Can come in a variety of formats, e.g. disk->disk,
 disk->memory, memory->memory }

interface
uses JWBJobs;

type
 //Extracts the archive file to a folder
  TFileUnpackJob = class(TJob)
  protected
    FSourceFile: string;
    FTargetFolder: string;
    FForceFormat: string; //if empty, guess from filename
  public
    constructor Create(const ASourceFile, ATargetFolder: string;
      AForceFormat: string = '');
    destructor Destroy; override;
    procedure ProcessChunk; override;
    property SourceFile: string read FSourceFile write FSourceFile;
    property TargetFolder: string read FTargetFolder write FTargetFolder;
    property ForceFormat: string read FForceFormat write FForceFormat;
  end;

 //Just moves a file
  TFileMoveJob = class(TJob)
  protected
    FSourceFile: string;
    FTargetFile: string;
  public
    constructor Create(const ASourceFile, ATargetFile: string);
    procedure ProcessChunk; override;
    property SourceFile: string read FSourceFile write FSourceFile;
    property TargetFile: string read FTargetFile write FTargetFile;
  end;

//Shortcut to silently unpack file to folder
procedure Unpack(const ASourceFile, ATargetFolder: string;
  AForceFormat: string = '');

implementation
uses SysUtils, Windows, SevenZip, SevenZipUtils;

{ Converts a file extension to a 7-zip CLSID describing the format.
 The guess is not guaranteed to be correct as "zip" archives for example can
 contain 7zip or gzip data.
 But every supported CLSID is assigned at least one "own" extension which you
 can use to indicate preffered format when working with this unit. }
function FormatToCLSID(const AFormat: string; out AGuid: TGUID): boolean;
begin
  Result := true;
  if AFormat='zip' then
    AGuid := CLSID_CFormatZip
  else
  if AFormat='7z' then
    AGuid := CLSID_CFormat7z
  else
  if AFormat='gz' then
    AGuid := CLSID_CFormatGZip
  else
  if AFormat='rar' then
    AGuid := CLSID_CFormatRar
  else
  if AFormat='tar' then
    AGuid := CLSID_CFormatTar
  else
    Result := false;
end;

constructor TFileUnpackJob.Create(const ASourceFile, ATargetFolder: string;
  AForceFormat: string);
begin
  inherited Create;
  FSourceFile := ASourceFile;
  FTargetFolder := ATargetFolder;
  FForceFormat := AForceFormat;
end;

destructor TFileUnpackJob.Destroy;
begin
  inherited;
end;

procedure TFileUnpackJob.ProcessChunk;
var zip: TSevenZipArchive;
  zipCLSID: TGUID;
  AFormat: string;
  i: integer;
  zname: string;
begin
  FState := jsWorking;

  if FForceFormat<>'' then
    AFormat := FForceFormat
  else begin
    AFormat := ExtractFileExt(FSourceFile);
    if copy(AFormat,1,1)='.' then delete(AFormat,1,1);
  end;
  if not FormatToCLSID(AFormat, zipCLSID) then
    raise Exception.Create('Unknown archive type: '+AFormat);

  zip := TSevenZipArchive.Create(CLSID_CFormatZip, FSourceFile);
  try
    SetMaxProgress(zip.NumberOfItems);
    SetProgress(0);
    for i := 0 to zip.NumberOfItems - 1 do begin
      zname := zip.StrProperty(i, kpidPath);
      if zname='' then continue; //wtf though?
      if zip.BoolProperty(i, kpidIsDir) then begin
        ForceDirectories(FTargetFolder+'\'+zname);
      end else
        with zip.ExtractFile(i) do try
          if ExtractFilePath(zname)<>'' then continue; //at least don't extract them to the root!
          SaveToFile(FTargetFolder+'\'+zname);
        finally
          Free;
        end;
      SetProgress(i+1);
    end;
  finally
    FreeAndNil(zip);
    FState := jsCompleted;
  end;
end;

constructor TFileMoveJob.Create(const ASourceFile, ATargetFile: string);
begin
  inherited Create();
  FSourceFile := ASourceFile;
  FTargetFile := ATargetFile;
end;

procedure TFileMoveJob.ProcessChunk;
begin
  MoveFile(PChar(FSourceFile), PChar(FTargetFile));
  FState := jsCompleted;
end;

procedure Unpack(const ASourceFile, ATargetFolder: string;
  AForceFormat: string = '');
begin
  DoJob(TFileUnpackJob.Create(ASourceFile, ATargetFolder, AForceFormat));
end;

end.
