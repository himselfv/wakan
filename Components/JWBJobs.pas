unit JWBJobs;

interface
uses SysUtils, Classes, Generics.Collections;

type
  TJobState = (
    jsPending,
    jsWorking,    //set when ProcessChunk is first called for a Job
    jsCompleted   //set when there's no need to call ProcessChunk anymore
  );

  TJob = class(TObject)
  protected
    FState: TJobState;
    FProgress: integer;
    FTotalSize: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessChunk; virtual; //Make chunks small enough
    property State: TJobState read FState;
    property TotalSize: integer read FTotalSize; //if 0, total size is unknown
    property Progress: integer read FProgress;
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
  FTotalSize := 0;
end;

destructor TJob.Destroy;
begin
  inherited;
end;

procedure TJob.ProcessChunk;
begin
  FState := jsCompleted;
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
