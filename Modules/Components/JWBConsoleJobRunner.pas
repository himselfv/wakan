unit JWBConsoleJobRunner;
{ Runs jobs in console }

interface
uses JWBJobs;

type
  TProgressStyle = (
    psPercentage,   // 75%
    psValues        // 1320 / 1500
  );

  // How to show status updates
  TLineStyle = (
    lsSameLine,     // Unpacking [75%]
    lsLineByLine    // Downloading... Unpacking... Importing...
  );

  TConsoleJobProgressHandler = class
  protected
    FSilent: boolean;
    FProgressStyle: TProgressStyle;
    FLineStyle: TLineStyle;
    FJob: TJob;
    FTitle: string;
    FLastLineLength: integer;
    FOperation: string;
    FProgress: integer;
    FMaxProgress: integer;
    procedure SetOperation(AOperation: string);
    procedure SetProgress(AProgress: integer);
    procedure SetMaxProgress(AMaxProgress: integer);
    procedure UpdateProgressLine;
  public
    procedure JobOperationChanged(Sender: TObject);
    procedure JobProgressChanged(Sender: TObject);
    procedure JobYield(Sender: TObject);
    procedure ExecuteJob(AJob: TJob; ATitle: string);
    property Silent: boolean read FSilent write FSilent;
    property ProgressStyle: TProgressStyle read FProgressStyle write FProgressStyle;
    property LineStyle: TLineStyle read FLineStyle write FLineStyle;
  end;

procedure RunInConsole(AJob: TJob; AJobTitle: string; ASilent: boolean = false);

implementation
uses SysUtils;

procedure TConsoleJobProgressHandler.JobOperationChanged(Sender: TObject);
begin
  SetOperation(FJob.Operation);
end;

procedure TConsoleJobProgressHandler.JobProgressChanged(Sender: TObject);
begin
  SetProgress(FJob.Progress);
  SetMaxProgress(FJob.MaxProgress);
end;

procedure TConsoleJobProgressHandler.JobYield(Sender: TObject);
begin
end;

procedure TConsoleJobProgressHandler.SetOperation(AOperation: string);
begin
  FOperation := AOperation;
  UpdateProgressLine;
end;

procedure TConsoleJobProgressHandler.SetProgress(AProgress: integer);
begin
  if AProgress = FProgress then exit;
  FProgress := AProgress;
  if LineStyle = lsSameLine then
    UpdateProgressLine;
end;

procedure TConsoleJobProgressHandler.SetMaxProgress(AMaxProgress: integer);
begin
  if AMaxProgress = FMaxProgress then exit;
  FMaxProgress := AMaxProgress;
  if LineStyle = lsSameLine then
    UpdateProgressLine;
end;

procedure TConsoleJobProgressHandler.UpdateProgressLine;
var line: string;
  ALineLength: integer;
begin
  if Silent then exit;

  if LineStyle = lsLineByLine then begin
    if not FOperation.EndsWith('...') then
      line := FOperation + '...'
    else
      line := FOperation;
  end else begin
    if not FTitle.EndsWith('...') then
      line := FTitle + '... ' + FOperation
    else
      line := FTitle + ' ' + FOperation;
  end;

  if (LineStyle = lsSameLine) and ((FMaxProgress > 0) or (FProgress > 0)) then
    if (ProgressStyle = psValues) or (FMaxProgress <= 0) then begin
      if FMaxProgress > 0 then
        line := line + ' [' + IntToStr(FProgress) + '/' + IntToStr(FMaxProgress)+']'
      else
        line := line + ' [' + IntToStr(FProgress) + ']';
    end else begin
      line := line + ' [' + FloatToStrF(100*FProgress / FMaxProgress, ffNumber, 3, 0) + '%' + ']';
    end;

  case LineStyle of
    lsSameLine: begin
      //When overwriting, we have to erase the remainder of the previous line
      ALineLength := Length(line);
      while Length(line) < FLastLineLength do
        line := line + ' ';
      FLastLineLength := ALineLength;
      write(#13+line);
    end;
    lsLineByLine: writeln('  ' + line);
  end;
end;

procedure TConsoleJobProgressHandler.ExecuteJob(AJob: TJob; ATitle: string);
begin
  FJob := AJob;
  FTitle := ATitle;
  FLastLineLength := 0;
  FProgress := -1;
  FMaxProgress := -1;
  AJob.OnOperationChanged := Self.JobOperationChanged;
  AJob.OnProgressChanged := Self.JobProgressChanged;
  if LineStyle = lsLineByLine then begin
    if not FSilent then
      writeln(ATitle);
    FOperation := '';
  end else
    SetOperation('');
  repeat
    AJob.ProcessChunk;
    JobYield(AJob); //in case this is a straight chunked job
  until (AJob.State=jsFinished);
  if LineStyle = lsSameLine then begin
    if (ProgressStyle = psPercentage) and (FMaxProgress > 0) then
      FProgress := FMaxProgress; //make sure it's 100% because "Done [60%]" looks strange
    SetOperation('Done');
    if not FSilent then
      writeln('');
  end;
  AJob.OnProgressChanged := nil;
  AJob.OnOperationChanged := nil;
  AJob.OnYield := nil;
  FJob := nil;
  FTitle := '';
end;

procedure RunInConsole(AJob: TJob; AJobTitle: string; ASilent: boolean = false);
var prog: TConsoleJobProgressHandler;
begin
  prog := TConsoleJobProgressHandler.Create();
  try
    prog.Silent := ASilent;
    prog.ProgressStyle := psPercentage;
    prog.LineStyle := lsSameLine;
    prog.ExecuteJob(AJob, AJobTitle);
  finally
    FreeAndNil(prog);
  end;
end;

end.
