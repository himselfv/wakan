unit UpgradeFilesTests;

interface
uses Classes, TestFramework, UpgradeFiles;

type
  TTestProgressHandler = class(TProgressHandler)
  protected
    Errors: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function HandleProgress(const Total, Done: int64): boolean; override;
    procedure ReportError(ErrorText: string); override;
  end;

  TMoveFileTestCase = class(TTestCase)
  protected
    action: TMoveFile;
    progress: TTestProgressHandler;
    tempDir1: string;
    tempDir2: string;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MoveFile;
    procedure MoveFileTargetExists;
    procedure MoveFileAutoRename;
    procedure MoveFolder;
    procedure MoveFolderTargetExists;
    procedure MoveFolderAutoRename;
    procedure MoveFolderAutoMerge;
    procedure MoveFolderComplicatedMerge;
  end;

implementation
uses SysUtils, JWBStrings;

constructor TTestProgressHandler.Create;
begin
  inherited;
  Errors := TStringList.Create;
end;

destructor TTestProgressHandler.Destroy;
begin
  FreeAndNil(Errors);
  inherited;
end;

function TTestProgressHandler.HandleProgress(const Total, Done: int64): boolean;
begin
  Result := true;
end;

procedure TTestProgressHandler.ReportError(ErrorText: string);
begin
  Errors.Add(ErrorText);
end;

procedure TMoveFileTestCase.SetUp;
begin
  progress := TTestProgressHandler.Create;
  tempDir1 := CreateRandomTempDir;
  tempDir2 := CreateRandomTempDir;
end;

procedure TMoveFileTestCase.TearDown;
begin
  DeleteDirectory(tempDir2);
  DeleteDirectory(tempDir1);
  FreeAndNil(progress);
end;

procedure CreateRandomFile(const Filename: string);
var fs: TFileStream;
  text: string;
begin
  text := DatetimeToStr(now());
  fs := TFileStream.Create(Filename, fmCreate);
  try
    fs.Write(text[1], Length(text) * SizeOf(text));
  finally
    FreeAndNil(fs);
  end;
end;

procedure TMoveFileTestCase.MoveFile;
begin
  CreateRandomFile(tempDir1+'\file1');

  action := TMoveFile.Create(tempDir1, 'file1', tempDir2, []);
  try
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(FileExists(tempDir2+'\file1'));
end;

procedure TMoveFileTestCase.MoveFileTargetExists;
begin
  CreateRandomFile(tempDir1+'\file1');
  CreateRandomFile(tempDir2+'\file1');

  action := TMoveFile.Create(tempDir1, 'file1', tempDir2, []);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count > 0);
  Check(not FileExists(tempDir2+'\file1 (1)'));
end;

procedure TMoveFileTestCase.MoveFileAutoRename;
begin
  CreateRandomFile(tempDir1+'\file1');
  CreateRandomFile(tempDir2+'\file1');
  CreateRandomFile(tempDir2+'\file1 (1)');

  action := TMoveFile.Create(tempDir1, 'file1', tempDir2, [mfAutoRename]);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count = 0);
  Check(FileExists(tempDir2+'\file1 (2)'));
end;

procedure TMoveFileTestCase.MoveFolder;
begin
  CreateDir(tempDir1+'\dir1');
  CreateRandomFile(tempDir1+'\dir1\file1');
  CreateRandomFile(tempDir1+'\dir1\file2');
  CreateRandomFile(tempDir1+'\dir1\file3');

  action := TMoveFile.Create(tempDir1, 'dir1', tempDir2, []);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count = 0);
  Check(DirectoryExists(tempDir2+'\dir1'));
  Check(FileExists(tempDir2+'\dir1\file1'));
  Check(FileExists(tempDir2+'\dir1\file2'));
  Check(FileExists(tempDir2+'\dir1\file3'));
end;

procedure TMoveFileTestCase.MoveFolderTargetExists;
begin
  CreateDir(tempDir1+'\dir1');
  CreateRandomFile(tempDir1+'\dir1\file1');
  CreateRandomFile(tempDir1+'\dir1\file2');
  CreateRandomFile(tempDir1+'\dir1\file3');
  CreateDir(tempDir2+'\dir1');

  action := TMoveFile.Create(tempDir1, 'dir1', tempDir2, []);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count > 0);
  Check(not DirectoryExists(tempDir2+'\dir1 (1)'));
  Check(not FileExists(tempDir2+'\dir1\file1'));
  Check(not FileExists(tempDir2+'\dir1\file2'));
  Check(not FileExists(tempDir2+'\dir1\file3'));
end;

procedure TMoveFileTestCase.MoveFolderAutoRename;
begin
  CreateDir(tempDir1+'\dir1');
  CreateRandomFile(tempDir1+'\dir1\file1');
  CreateRandomFile(tempDir1+'\dir1\file2');
  CreateRandomFile(tempDir1+'\dir1\file3');
  CreateDir(tempDir2+'\dir1');
  CreateDir(tempDir2+'\dir1 (1)');

  action := TMoveFile.Create(tempDir1, 'dir1', tempDir2, [mfAutoRename]);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count = 0);
  Check(not FileExists(tempDir2+'\dir1\file1'));
  Check(not FileExists(tempDir2+'\dir1\file2'));
  Check(not FileExists(tempDir2+'\dir1\file3'));
  Check(DirectoryExists(tempDir2+'\dir1 (2)'));
  Check(FileExists(tempDir2+'\dir1 (2)\file1'));
  Check(FileExists(tempDir2+'\dir1 (2)\file2'));
  Check(FileExists(tempDir2+'\dir1 (2)\file3'));
end;

procedure TMoveFileTestCase.MoveFolderAutoMerge;
begin
  CreateDir(tempDir1+'\dir1');
  CreateRandomFile(tempDir1+'\dir1\file1');
  CreateRandomFile(tempDir1+'\dir1\file2');
  CreateRandomFile(tempDir1+'\dir1\file3');
  CreateDir(tempDir2+'\dir1');

  action := TMoveFile.Create(tempDir1, 'dir1', tempDir2, [mfAutoMerge]);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count = 0);
  Check(FileExists(tempDir2+'\dir1\file1'));
  Check(FileExists(tempDir2+'\dir1\file2'));
  Check(FileExists(tempDir2+'\dir1\file3'));
  Check(not DirectoryExists(tempDir2+'\dir1 (1)'));
end;

//Complicated merge/rename case
procedure TMoveFileTestCase.MoveFolderComplicatedMerge;
begin
  CreateDir(tempDir1+'\dir1');
  CreateDir(tempDir1+'\dir1\dir2');
  CreateRandomFile(tempDir1+'\dir1\dir2\file1');
  CreateRandomFile(tempDir1+'\dir1\dir2\file2');
  CreateRandomFile(tempDir1+'\dir1\file3');
  CreateRandomFile(tempDir1+'\dir1\file4');

  CreateDir(tempDir2+'\dir1');
  CreateDir(tempDir2+'\dir1\dir2');
  CreateRandomFile(tempDir2+'\dir1\dir2\file1');
  CreateRandomFile(tempDir2+'\dir1\file3');

  action := TMoveFile.Create(tempDir1, 'dir1', tempDir2, [mfAutoMerge, mfAutoRename]);
  try
    action.ProgressHandler := progress;
    action.Run;
  finally
    FreeAndNil(action);
  end;

  Check(progress.Errors.Count = 0);
  Check(FileExists(tempDir2+'\dir1\dir2\file1'));
  Check(FileExists(tempDir2+'\dir1\dir2\file2'));
  Check(FileExists(tempDir2+'\dir1\dir2\file1 (1)'));
  Check(FileExists(tempDir2+'\dir1\file3'));
  Check(FileExists(tempDir2+'\dir1\file4'));
  Check(FileExists(tempDir2+'\dir1\file3 (1)'));
  Check(not DirectoryExists(tempDir2+'\dir1 (1)'));
end;


initialization
  RegisterTest(TMoveFileTestCase.Suite);

end.

