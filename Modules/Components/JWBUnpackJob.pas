unit JWBUnpackJob;
{ Core archive unpacking. Can come in a variety of formats, e.g. disk->disk,
 disk->memory, memory->memory }

interface
uses JWBJobs, SevenZipUtils;

type
 //Extracts the archive file to a folder
  TFileUnpackJob = class(TJob)
  protected
    FSourceFile: string;
    FTargetFileOrFolder: string;
    { Target folder or folder+filename. If filename is specified, source must
     only have a single file inside. }
    FDefaultFilename: string;
    { If specified and source is a single-file archive lacking file name,
     this will be used -- otherwise source filename minus extension }
    FForceFormat: string; //if empty, guess from filename
  protected
    zip: TSevenZipArchive;
    FActiveZipEntry: integer;
    FNonameUsed: boolean; //if true, we've already had one noname file
    procedure Initialize;
    procedure Cleanup;
    procedure MakePath(const ASubFolder: string);
  public
    constructor Create(const ASourceFile, ATargetFileOrFolder: string;
      AForceFormat: string = '');
    destructor Destroy; override;
    procedure ProcessChunk; override;
    property SourceFile: string read FSourceFile write FSourceFile;
    property TargetFileOrFolder: string read FTargetFileOrFolder
      write FTargetFileOrFolder;
    property DefaultFilename: string read FDefaultFilename write FDefaultFilename;
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
procedure Unpack(const ASourceFile, ATargetFileOrFolder: string;
  AForceFormat: string = '');

implementation
uses SysUtils, Windows, SevenZip;

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

constructor TFileUnpackJob.Create(const ASourceFile, ATargetFileOrFolder: string;
  AForceFormat: string);
begin
  inherited Create;
  FSourceFile := ASourceFile;
  FTargetFileOrFolder := ATargetFileOrFolder;
  FForceFormat := AForceFormat;
end;

destructor TFileUnpackJob.Destroy;
begin
  Cleanup;
  inherited;
end;

procedure TFileUnpackJob.Initialize;
var zipCLSID: TGUID;
  AFormat: string;
begin
  if FForceFormat<>'' then
    AFormat := FForceFormat
  else begin
    AFormat := ExtractFileExt(FSourceFile);
    if copy(AFormat,1,1)='.' then delete(AFormat,1,1);
  end;
  if not FormatToCLSID(AFormat, zipCLSID) then
    raise Exception.Create('Unknown archive type: '+AFormat);

  zip := TSevenZipArchive.Create(zipCLSID, FSourceFile);
  FActiveZipEntry := 0;
  SetMaxProgress(zip.NumberOfItems);
  SetProgress(FActiveZipEntry);
end;

procedure TFileUnpackJob.Cleanup;
begin
  FreeAndNil(zip);
  FState := jsFinished;
end;

resourcestring
  eSimpleFilenameComplexStructure = 'Simple filename specified as extraction '
    +'target but archive has complex structure.';

procedure TFileUnpackJob.ProcessChunk;
var zname: string;
begin
  if FState = jsPending then begin
    Initialize;
    FState := jsWorking;
  end;

  zname := zip.StrProperty(FActiveZipEntry, kpidPath);

 //Some formats (notably GZ) support single-file nameless archives
  if zname='' then begin
    if FNonameUsed then
      raise Exception.Create('Two nameless files in a single archive are not supported.'); //is this even possible?
    zname := ExtractFilename(FTargetFileOrFolder);
    if zname='' then
      zname := FDefaultFilename;
    if zname='' then
     //Lacking explicit filename, use original minus archive extension
      zname := ChangeFileExt(ExtractFilename(FSourceFile), '');
    FNonameUsed := true;
  end;

  if zip.BoolProperty(FActiveZipEntry, kpidIsDir) then
    MakePath(zname)
  else
    with zip.ExtractFile(FActiveZipEntry) do try
      if ExtractFilePath(zname)<>'' then
        MakePath(ExtractFilePath(zname)); //just in case
      if ExtractFilename(FTargetFileOrFolder)<>'' then begin
        if FActiveZipEntry>0 then
          raise Exception.Create(eSimpleFilenameComplexStructure);
        SaveToFile(FTargetFileOrFolder);
      end else
        SaveToFile(FTargetFileOrFolder+'\'+zname);
    finally
      Free;
    end;

 //Next file
  Inc(FActiveZipEntry);
  SetProgress(FActiveZipEntry);
  if cardinal(FActiveZipEntry)>=zip.NumberOfItems then begin
    Cleanup;
    FState := jsFinished;
  end;
end;

//Creates the specified subfolder in the extraction place
procedure TFileUnpackJob.MakePath(const ASubFolder: string);
begin
  if ExtractFilename(FTargetFileOrFolder)<>'' then
    raise Exception.Create(eSimpleFilenameComplexStructure);
 //TODO: Check that the path is not absolute / outside of target folder?
  ForceDirectories(FTargetFileOrFolder+'\'+ASubFolder);
end;

constructor TFileMoveJob.Create(const ASourceFile, ATargetFile: string);
begin
  inherited Create();
  FSourceFile := ASourceFile;
  FTargetFile := ATargetFile;
end;

procedure TFileMoveJob.ProcessChunk;
begin
  if ExtractFilename(FTargetFile)='' then
    FTargetFile := FTargetFile + ExtractFilename(FSourceFile);
  ForceDirectories(ExtractFilePath(FTargetFile));
  MoveFile(PChar(FSourceFile), PChar(FTargetFile));
  FState := jsFinished;
end;

procedure Unpack(const ASourceFile, ATargetFileOrFolder: string;
  AForceFormat: string = '');
begin
  Run(TFileUnpackJob.Create(ASourceFile, ATargetFileOrFolder, AForceFormat), true);
end;

end.
