unit JWBJobs;

interface
uses SysUtils, Classes, Generics.Collections;

{
A job is a component of work. You can run it synchronously or asynchronously,
it reports the progress. It's done in chunks and can be terminated at any time.
Override ProcessChunk() to implement a job.
}

type
  TJobState = (
    jsPending,   //job haven't been started yet, some fields may be inaccurate (e.g. total size estimate)
    jsWorking,   //job in progress, continue calling ProcessChunk
    jsFinished   //job over (completed, aborted or failed), don't call ProcessChunk anymore
  );

  TJob = class(TObject)
  protected
    FState: TJobState;
    FAborted: boolean;
    FProgress: integer;
    FMaxProgress: integer;
    FOperation: string;
    FOnOperationChanged: TNotifyEvent;
    FOnProgressChanged: TNotifyEvent;
    FOnYield: TNotifyEvent;
    procedure Yield;
    procedure SetOperation(const AValue: string);
    procedure SetProgress(const AValue: integer);
    procedure SetMaxProgress(const AValue: integer);
    procedure StartOperation(const AOperation: string; const AMaxProgress: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessChunk; virtual; //Make chunks small enough
    procedure Abort; virtual;
    property State: TJobState read FState;
    property Operation: string read FOperation;
    property Progress: integer read FProgress;
    property MaxProgress: integer read FMaxProgress; //if 0, total size is unknown
    property Aborted: boolean read FAborted;
    property OnProgressChanged: TNotifyEvent read FOnProgressChanged
      write FOnProgressChanged;
    property OnOperationChanged: TNotifyEvent read FOnOperationChanged
      write FOnOperationChanged;
    property OnYield: TNotifyEvent read FOnYield write FOnYield;
  end;
  PJob = ^TJob;

 { ChainJob: Executes several jobs one after another.
  Usage:
    chain := TChainJob.Create;
    chain.Add(DownloadJob, 'Downloading...');
    chain.Add(UnpackJob, 'Unpacking...');
    chain.Add(ImportJob, 'Importing...');
    chain.Run;
  }
  TChainJob = class(TJob)
  type
    TJobChainEntry = record
      Job: TJob;
      Title: string;
    end;
  protected
    FJobs: TList<TJobChainEntry>;
    FCurrentJobIndex: integer;
    FCurrentJob: TJob;
    function FindNextJobIndex: integer;
    procedure SetCurrentJobIndex(const AIndex: integer);
    procedure ChildJobProgressChanged(ASender: TObject);
    procedure ChildJobOperationChanged(ASender: TObject);
    procedure UpdateOperation;
    procedure UpdateProgress;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessChunk; override;
    procedure Add(AJob: TJob; const ATitle: string = '');
    procedure Abort; override;
    property CurrentJobIndex: integer read FCurrentJobIndex write SetCurrentJobIndex;
  end;

 { WorkerThread: Executes several jobs in a separate thread.
  Usage:
     worker := TWorkerThread.Create;
     worker.AddJob(DownloadJob1);
     worker.AddJob(DownloadJob2);
     worker.Start;
   Will auto-stop when done. Destroy to destroy all job objects. }
  TExceptionEvent = procedure(Sender: TObject; Job: TJob; E: Exception;
    out StopJob: boolean) of object;
  TWorkerThread = class(TThread)
  protected
    FJobs: TObjectList<TJob>;
    FOnException: TExceptionEvent;
    function FindNextJob: TJob;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddJob(AJob: TJob);
    property Jobs: TObjectList<TJob> read FJobs;
    property OnException: TExceptionEvent read FOnException write FOnException;
  end;

procedure Run(const AJob: TJob; AAutoDestroy: boolean = false);
procedure RunInParallel(const AJob: TJob);

implementation

constructor TJob.Create;
begin
  inherited;
  FState := jsPending;
  FProgress := 0;
  FMaxProgress := 0;
  FOperation := '';
end;

destructor TJob.Destroy;
begin
  inherited;
end;

{
Override ProcessChunk() to implement a job:
 * process a chunk of data
 * update progress/state

You can also implement all job in a single call of ProcessChunk. In this case
call SetProgress() or Yield() from time to time and be ready for EAbort.
}
procedure TJob.ProcessChunk;
begin
  FState := jsFinished;
end;

{ The job calls this to relay execution to housekeeping code if called from
 the same thread, and to check if the job has been terminated }
procedure TJob.Yield;
begin
  if FAborted then
    raise EAbort.Create('');
  if Assigned(FOnYield) then
    FOnYield(Self);
end;

{ Call to mark the job for premature termination. If the job finds it's been
 aborted, EAbort will be raised }
procedure TJob.Abort;
begin
  FAborted := true;
end;

procedure TJob.SetOperation(const AValue: string);
begin
  FOperation := AValue;
  if Assigned(FOnOperationChanged) then
    FOnOperationChanged(Self);
  Yield;
end;

procedure TJob.SetProgress(const AValue: integer);
begin
  if FProgress=AValue then exit;
  FProgress := AValue;
  if Assigned(FOnProgressChanged) then
    FOnProgressChanged(Self);
  Yield;
end;

procedure TJob.SetMaxProgress(const AValue: integer);
begin
  if FMaxProgress=AValue then exit;
  FMaxProgress := AValue;
  if Assigned(FOnProgressChanged) then
    FOnProgressChanged(Self);
end;

procedure TJob.StartOperation(const AOperation: string; const AMaxProgress: integer);
begin
  SetOperation(AOperation);
  SetMaxProgress(AMaxProgress);
end;

constructor TChainJob.Create;
begin
  inherited;
  FJobs := TList<TJobChainEntry>.Create;
  FCurrentJobIndex := -1;
end;

destructor TChainJob.Destroy;
begin
  FreeAndNil(FJobs);
  inherited;
end;

procedure TChainJob.Add(AJob: TJob; const ATitle: string = '');
var AEntry: TJobChainEntry;
begin
  AEntry.Job := AJob;
  AEntry.Title := ATitle;
  FJobs.Add(AEntry);
end;

function TChainJob.FindNextJobIndex: integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to FJobs.Count-1 do
    if (FJobs[i].Job.State<>jsFinished) then begin
      Result := i;
      break;
    end;
end;

procedure TChainJob.SetCurrentJobIndex(const AIndex: integer);
begin
  if FCurrentJobIndex=AIndex then exit;
  FCurrentJobIndex := AIndex;
  if (FCurrentJobIndex<0) or (FCurrentJobIndex>FJobs.Count-1) then begin
    FCurrentJob := nil;
    SetMaxProgress(0);
    SetOperation('');
  end else begin
    FCurrentJob := FJobs[FCurrentJobIndex].Job;
    SetMaxProgress(FCurrentJob.MaxProgress);
    SetOperation(FJobs[FCurrentJobIndex].Title);
  end;
end;

procedure TChainJob.ChildJobProgressChanged(ASender: TObject);
begin
  UpdateProgress;
end;

procedure TChainJob.ChildJobOperationChanged(ASender: TObject);
begin
  UpdateOperation;
end;

procedure TChainJob.UpdateOperation;
begin
  if FCurrentJob=nil then
    SetOperation('')
  else
    SetOperation(FCurrentJob.Operation);
end;

procedure TChainJob.UpdateProgress;
begin
  if FCurrentJob=nil then begin
    SetMaxProgress(0);
    SetProgress(0);
  end else begin
    SetMaxProgress(FCurrentJob.MaxProgress);
    SetProgress(FCurrentJob.Progress);
  end;
end;

procedure TChainJob.ProcessChunk;
begin
  FState := jsWorking;
  if (FCurrentJobIndex<0) or (FCurrentJobIndex>FJobs.Count-1)
  or (FJobs[FCurrentJobIndex].Job.State=jsFinished) then
    CurrentJobIndex := FindNextJobIndex;
  if FCurrentJobIndex<0 then begin
    FState := jsFinished;
    exit;
  end;

  FCurrentJob.ProcessChunk;
  Self.SetProgress(FCurrentJob.Progress);
end;

procedure TChainJob.Abort;
begin
  if FCurrentJob<>nil then
    FCurrentJob.Abort;
  inherited;
end;




constructor TWorkerThread.Create;
begin
  inherited Create({CreateSuspended=}true);
  FJobs := TObjectList<TJob>.Create;
end;

destructor TWorkerThread.Destroy;
begin
  Self.Terminate;
  Self.WaitFor;
  FreeAndNil(FJobs);
  inherited Destroy;
end;

procedure TWorkerThread.Execute;
var Job: TJob;
  StopJob: boolean;
begin
  Job := nil;
  while not Terminated do try
    if (Job=nil) or (Job.State=jsFinished) then begin
      Job := FindNextJob();
      if Job=nil then break;
    end;
    Job.ProcessChunk;
  except
    on E: Exception do begin
      StopJob := true;
      if Assigned(FOnException) then
        FOnException(Self, Job, E, StopJob);
      if StopJob then
        Job.FState := jsFinished;
      Job := nil; //in any case, re-choose it/other job
    end;
  end;
end;

procedure TWorkerThread.AddJob(AJob: TJob);
begin
  FJobs.Add(AJob);
end;

function TWorkerThread.FindNextJob: TJob;
var i: integer;
begin
  Result := nil;
  for i := 0 to FJobs.Count-1 do
    if (FJobs[i].State<>jsFinished) then begin
      Result := FJobs[i];
      break;
    end;
end;

//Executes a job without displaying any progress.
procedure Run(const AJob: TJob; AAutoDestroy: boolean);
begin
  try
    while AJob.State<>jsFinished do
      AJob.ProcessChunk;
  finally
    if AAutoDestroy then
      AJob.Destroy;
  end;
end;

//Executes a job in a parallel thread created specifically for it.
//The job is always auto-destroyed at the end.
procedure RunInParallel(const AJob: TJob);
var worker: TWorkerThread;
begin
  worker := TWorkerThread.Create;
  worker.AddJob(AJob);
  worker.FreeOnTerminate := true;
  worker.Start;
end;

end.
