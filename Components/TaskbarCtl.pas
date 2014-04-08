unit TaskbarCtl;

interface
uses Windows, WinAPI.ShlObj;

type
 { Does nothing if the taskbar progress is not supported. Potentially implements
  this on other OSes in the appropriate way }
  TTaskbarProgressState = (
    tsNoProgress,
    tsIndeterminate,
    tsNormal,
    tsError,
    tsPaused
  );
  TTaskbarProgress = class
  protected
    FWindowHandle: HWND;
    TaskbarList: ITaskbarList;
    TaskbarList2: ITaskbarList2;
    TaskbarList3: ITaskbarList3;
    TaskbarList4: ITaskbarList4;
    FState: TTaskbarProgressState;
    FProgress: integer;
    FMax: integer;
    procedure SetState(const AValue: TTaskbarProgressState);
    procedure ApplyTaskbarState;
  public
    constructor Create(const AWindowHandle: HWND = 0);
    destructor Destroy; override;
    procedure SetProgress(const AProgress, AMax: integer);
    property State: TTaskbarProgressState read FState write SetState;
  end;

implementation
uses SysUtils, Forms, ComObj;

{ Potentially, if there are collisions, we can make all TTaskbarProgress
 instances cooperate with some ProgressKeeper to track combined state }

//If Handle is not specified, uses application handle.
constructor TTaskbarProgress.Create(const AWindowHandle: HWND = 0);
var hr: HRESULT;
begin
  inherited Create();
  FState := tsNoProgress;
  if CheckWin32Version(6, 1) then begin
    TaskbarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList;
    TaskbarList.HrInit;
    Supports(TaskbarList, IID_ITaskbarList2, TaskbarList2);
    Supports(TaskbarList, IID_ITaskbarList3, TaskbarList3);
    Supports(TaskbarList, IID_ITaskbarList4, TaskbarList4);

    FWindowHandle := AWindowHandle;
    if FWindowHandle=0 then
      FWindowHandle := Application.Handle;

    hr := TaskbarList.AddTab(Self.FWindowHandle);
    if FAILED(hr) then
      raise Exception.Create('Cannot query taskbar tab: 0x'+IntToHex(hr, 8));
   //Don't need to apply anything since progress is off by default
  end
  else begin
    TaskbarList := nil;
    TaskbarList2 := nil;
    TaskbarList3 := nil;
    TaskbarList4 := nil;
  end
end;

destructor TTaskbarProgress.Destroy;
begin
  if TaskbarList3<>nil then
    TaskbarList3.SetProgressState(FWindowHandle, TBPF_NOPROGRESS);
  TaskbarList := nil;
  TaskbarList2 := nil;
  TaskbarList3 := nil;
  TaskbarList4 := nil;
  inherited;
end;

procedure TTaskbarProgress.SetProgress(const AProgress, AMax: integer);
begin
  if (FProgress=AProgress) and (FMax=AMax) then exit;
  FProgress := AProgress;
  FMax := AMax;
  ApplyTaskbarState;
end;

procedure TTaskbarProgress.SetState(const AValue: TTaskbarProgressState);
begin
  if AValue=FState then exit;
  FState := AValue;
  ApplyTaskbarState;
end;

procedure TTaskbarProgress.ApplyTaskbarState;
begin
  if TaskbarList3 = nil then exit;
  case FState of
    tsNoProgress: TaskbarList3.SetProgressState(FWindowHandle, TBPF_NOPROGRESS);
    tsIndeterminate: TaskbarList3.SetProgressState(FWindowHandle, TBPF_INDETERMINATE);
    tsError: TaskbarList3.SetProgressState(FWindowHandle, TBPF_ERROR);
    tsPaused: TaskbarList3.SetProgressState(FWindowHandle, TBPF_PAUSED);
  else //Normal
    TaskbarList3.SetProgressValue(FWindowHandle, FProgress, FMax);
    if FMax>0 then
      TaskbarList3.SetProgressState(FWindowHandle, TBPF_NORMAL)
    else
      TaskbarList3.SetProgressState(FWindowHandle, TBPF_INDETERMINATE)
  end;
end;

end.
