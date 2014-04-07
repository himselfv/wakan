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
    FTargetFileOrFolder: string;
   //If filename is specified, target archive must be single-file archive.
    FForceFormat: string; //if empty, guess from filename
    FNonameUsed: boolean; //if true, we've already had one noname file
  public
    constructor Create(const ASourceFile, ATargetFileOrFolder: string;
      AForceFormat: string = '');
    destructor Destroy; override;
    procedure ProcessChunk; override;
    property SourceFile: string read FSourceFile write FSourceFile;
    property TargetFileOrFolder: string read FTargetFileOrFolder
      write FTargetFileOrFolder;
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

  zip := TSevenZipArchive.Create(zipCLSID, FSourceFile);
  try
    SetMaxProgress(zip.NumberOfItems);
    SetProgress(0);
    for i := 0 to zip.NumberOfItems - 1 do begin
      zname := zip.StrProperty(i, kpidPath);
     //Some formats (notably GZ) support single-file nameless archives
      if zname='' then begin
        if FNonameUsed then
          raise Exception.Create('Two nameless files in a single archive are not supported.'); //what is this, even
        zname := ExtractFilename(FTargetFileOrFolder);
        if zname='' then
         //Lacking explicit filename, use original minus archive extension
          zname := ChangeFileExt(ExtractFilename(FSourceFile), '');
        FNonameUsed := true;
      end;
      if zip.BoolProperty(i, kpidIsDir) then begin
        if ExtractFilename(FTargetFileOrFolder)<>'' then
          raise Exception.Create('Simple filename specified but archive has complicated structure.');
        ForceDirectories(FTargetFileOrFolder+'\'+zname);
      end else
        with zip.ExtractFile(i) do try
          if ExtractFilePath(zname)<>'' then continue; //at least don't extract them to the root!
          if ExtractFilename(FTargetFileOrFolder)<>'' then begin
            if i>0 then
              raise Exception.Create('Simple filename specified but archive has complicated structure.');
            SaveToFile(FTargetFileOrFolder);
          end else
            SaveToFile(FTargetFileOrFolder+'\'+zname);
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

procedure Unpack(const ASourceFile, ATargetFileOrFolder: string;
  AForceFormat: string = '');
begin
  Run(TFileUnpackJob.Create(ASourceFile, ATargetFileOrFolder, AForceFormat), true);
end;

end.
