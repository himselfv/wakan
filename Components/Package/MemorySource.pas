unit MemorySource;

interface

type EMemorySourceError=class(Exception);
     TMemoryLoadMode=( mlPermanentLoad, mlAutoLoad, mlDemandLoad, mlTemporaryLoad );
     TMemorySource=class;
     TMemoryFile=class
                   private
                     fLoadMode:TMemoryLoadMode;
                     fLoaded:boolean;
                     fLockCount:longint;
                     fSize:longint;
                   protected
                     Source:TMemorySource;
                     Locator:variant;
                     Stream:TMemoryStream;
                     constructor Create( MName:string; MSource:TMemorySource;
                       const MLocator:variant; MLoadMode:TMemoryLoadMode );
                     procedure Fill; virtual; abstract;
                     procedure Load;
                     procedure Unload;
                     procedure Refresh; virtual;
                     procedure SetLoadMode( LM:TMemoryLoadMode );
                     property LockCount:longint read vLockCount;
                     property Loaded:boolean read vLoaded;
                     property LoadMode:TMemoryLoadMode read vLoadMode write SetLoadMode;
                     destructor Done; virtual;
                   public
                     function Lock:TMemoryStream;
                     procedure Unlock;
                     property Size:longint read fSize;
                   end;
     TMemorySource=class
                     private
                       fFiles:TStringList;
                       function GetFile( Name:string ):TMemoryFile;
                       procedure Add( Name:string;BF:TMemoryFile );
                       fMemoryUsed,fMemoryFloor,fMemoryLimit,fMemoryCeiling:longint;
                     public
                       constructor Create; abstract;
                       destructor Done; virtual;
                       procedure SetMemoryFloor( mFloor:longint );
                       procedure SetMemoryLimit( mLimit:longint );
                       procedure SetMemoryCeiling( mCeiling:longint );
                       procedure RefreshAll;
                       property Files[ const Name:string ]:TMemoryFile read GetFile; default;
                       property MemoryFloor:longint read fMemoryFloor write SetMemoryFloor;
                       property MemoryLimit:longint read fMemoryLimit write SetMemoryLimit;
                       property MemoryCeiling:longint read fMemoryCeiling write SetMemoryCeiling;
                       property MemoryUsed:longint read fMemoryUsed;
                   end;

implementation

constructor TMemoryFile.Create( MSource:TMemorySource; MName:string; MSize:longint;
  const MLocator:variant; MLoadMode:TMemoryLoadMode );
begin
  if MSource=nil then raise EMemorySourceError.Create('Unassigned memory source.');
  Source:=MSource;
  Locator:=MLocator;
  Stream:=nil;
  fSize:=MSize;
  fLoaded:=false;
  fLockCount:=0;
  fLoadMode:=MLoadMode;
  Source.Add(MName,self);
end;

procedure TMemoryFile.Load;
begin
  if Loaded then exit;
  Fill;
  Source.MemoryUsed:=Source.MemoryUsed+Size;
  if Source.MemoryUsed>Source.MemoryLimit then
  begin
    //add temporary lock to prevent unloading
    fLockCount:=1;
    Source.RefreshAll;
    fLockCount:=0;
  end;
end;

procedure TMemoryFile.Unload;
begin
  if not Loaded then exit;
  if LockCount>0 then exit;
  Stream.Free;
  fLoaded:=false;
end;

procedure TMemoryFile.Refresh;
begin
  case LoadMode of
    mlPermanentLoad:
      begin
        if (not Loaded) and (Source.MemoryUsed+Size<Source.MemoryCeiling) then Load;
        if (Loaded) and (Source.MemoryUsed>Source.MemoryCeiling) then Unload;
      end;
    mlAutoLoad:
      begin
        if (not Loaded) and (Source.MemoryUsed+Size<Source.MemoryLimit) then Load;
        if (Loaded) and (Source.MemoryUsed>Source.MemoryLimit) then Unload;
      end;
    mlDemandLoad:
      begin
        if (not Loaded) and (Source.MemoryUsed+Size<Source.MemoryFloor) then Load;
        if (Loaded) and (Source.MemoryUsed>Source.MemoryLimit) then Unload;
      end;
    mlTemporaryLoad:
        if (Loaded) and (Source.MemoryUsed>Source.MemoryFloor) then Unload;
  end;
end;

procedure TMemoryFile.SetLoadMode( LM:TMemoryLoadMode );
begin
  fLoadMode:=LM;
  Refresh;
end;

destructor TMemoryFile.Done;
begin
  if LockCount>0 then raise EMemorySourceError.Create('Cannot destroy locked memory file.');
  Unload;
end;

function TMemoryFile.Lock:TMemoryStream;
begin
  if not Loaded then Load;
  fLockCount:=fLockCount+1;
  result:=Stream;
end;

procedure TMemoryFile.Unlock;
begin
  if fLockCount=0 then raise EMemorySourceError.Create('Cannot unlock already unlocked memory file.');
  fLockCount:=fLockCount-1;
  if fLockCount=0 then Refresh;
end;

function TMemorySource.GetFile( Name:string ):TMemoryFile;
var foundno:integer;
begin
  if not fFiles.Find(Name,foundno) then
  begin
    result:=nil;
    exit;
  end;
  result:=TMemoryFile(fFiles.Objects[foundno]);
end;

procedure TMemorySource.Add( Name:string;BF:TMemoryFile );
var foundno:integer;
begin
  if fFiles.Find(Name,foundno) then raise EMemorySourceError.Create('Duplicate file name.');
  fFiles.InsertObject(foundno,Name,BF);
end;

procedure TMemorySource.SetMemoryFloor( mFloor:longint );
begin
  fMemoryFloor:=mFloor;
  RefreshAll;
end;

procedure TMemorySource.SetMemoryLimit( mLimit:longint );
begin
  fMemoryLimit:=mLimit;
  RefreshAll;
end;

procedure TMemorySource.SetMemoryCeiling( mCeiling:longint );
begin
  fMemoryCeiling:=mCeiling;
  RefreshAll;
end;

procedure TMemorySource.RefreshAll;
var i:longint;
begin
  if fMemoryUsed>fMemoryLimit then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlTemporaryLoad then Refresh;
  if fMemoryUsed>fMemoryLimit then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlDemandLoad then Refresh;
  if fMemoryUsed>fMemoryLimit then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlAutoLoad then Refresh;
  if fMemoryUsed>fMemoryCeiling then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlPermanentLoad then Refresh;
  if fMemoryUsed<fMemoryCeiling then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlPermanentLoad then Refresh;
  if fMemoryUsed<fMemoryLimit then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlAutoLoad then Refresh;
  if fMemoryUsed<fMemoryFloor then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlDemandLoad then Refresh;
  if fMemoryUsed<fMemoryFloor then
    for i:=0 to fFiles.Count-1 do with TMemoryFile(fFiles.Objects[i]) do
      if LoadMode=mlTemporaryLoad then Refresh;
end;

end.
