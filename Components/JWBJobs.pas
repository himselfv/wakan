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
    jsPending,
    jsWorking,    //set when ProcessChunk is first called for a Job
    jsCompleted   //set when there's no need to call ProcessChunk anymore
  );

  TJob = class(TObject)
  protected
    FState: TJobState;
    FAborted: boolean;
    FProgress: integer;
    FMaxProgress: integer;
    FOperation: string;
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
    procedure Abort;
    property State: TJobState read FState;
    property Operation: string read FOperation;
    property Progress: integer read FProgress;
    property MaxProgress: integer read FMaxProgress; //if 0, total size is unknown
    property Aborted: boolean read FAborted;
    property OnYield: TNotifyEvent read FOnYield write FOnYield;
  end;
  PJob = ^TJob;

 { Usage:
     worker := TWorkerThread.Create;
     worker.AddJob();
     worker.AddJob();
     worker.Start;
   Will auto-stop when done. Destroy to destroy all job objects. }
  TWorkerThread = class(TThread)
  protected
    FJobs: TObjectList<TJob>;
    function FindNextJob: TJob;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddJob(AJob: TJob);
    property Jobs: TObjectList<TJob> read FJobs;
  end;

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
  FState := jsCompleted;
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
  Yield;
end;

procedure TJob.SetProgress(const AValue: integer);
begin
  FMaxProgress := AValue;
  Yield;
end;

procedure TJob.SetMaxProgress(const AValue: integer);
begin
  FMaxProgress := AValue;
end;

procedure TJob.StartOperation(const AOperation: string; const AMaxProgress: integer);
begin
  SetOperation(AOperation);
  SetMaxProgress(AMaxProgress);
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
begin
  Job := nil;
  while not Terminated do begin
    if (Job=nil) or (Job.State=jsCompleted) then begin
      Job := FindNextJob();
      if Job=nil then break;
    end;
    Job.ProcessChunk;
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
    if (FJobs[i].State<>jsCompleted) then begin
      Result := FJobs[i];
      break;
    end;
end;

end.
