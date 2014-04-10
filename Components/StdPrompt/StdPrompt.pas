(*************************************************************)
(*                                                           *)
(*         LABYRINTH ENHANCEMENT UNITS FOR DELPHI 3          *)
(*                                                           *)
(*                  STANDARD PROMPT DIALOGS                  *)
(*                                                           *)
(*         Copyright (C) LABYRINTH Filip Kábrt 1999          *)
(*                                                           *)
(*************************************************************)

(*************************************************************

 This unit is completely freeware including the source code.
 You are allowed to modify this unit as you wish and distribute
 it anywhere you want as long as you mention the author's name
 and email address. Using this unit to build some commercial
 application is free but if you want to sell some enhanced
 version of this unit for money you must contact the author.

 Name: Standard Prompt Dialogs
 Purpose: Provides an alternative to Delphi MessageDlg
          and MessageBox procedures and also provides
          procedures to create message/progress window
          that is closed by the application when some
          task is done.
 Prerequisites: none
 Files: STDPROMPT.PAS (this file) - unit file
        STDPROMPT.DFM - form file, by altering this file you
                        can easily change the appearance of prompt
 Language: Standardly english, configurable to any language
 Programmed by: Filip Kábrt
 E-mail: filipkabrt@atlas.cz
 Build date: 26.4.1999
 Installation: add this unit to your project and remove the
               form associated with this unit from the
               auto-create forms list
 Usage: Unit contains a set of procedures described carefully
        below
 Known bugs: none :-)

***************************************************************)

unit StdPrompt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ComObj, ShlObj, TaskbarCtl, JWBJobs;

type
 {$IF CompilerVersion<21}
  TTaskWindowList = pointer; //not defined on older compilers
 {$IFEND}

 {
  Vertical layout:
    [72-icon] 16 left-border 8 (label) 8 right-border 16
  Horizontal layout:
    16 top-border 8 (label) 8 [progress-bar 12] bottom-border 4 [8 buttons] 12

  []: optional
  (): stretch
 }

  TCancelQueryEvent = procedure(ASender: TObject; var DoAbort: boolean) of object;

  TSMPromptForm = class(TForm)
    Sign1Label: TLabel;
    Sign2Label: TLabel;
    Sign3Label: TLabel;
    AllButton: TBitBtn;
    YesButton: TBitBtn;
    NoButton: TBitBtn;
    CancelButton: TBitBtn;
    RetryButton: TBitBtn;
    IgnoreButton: TBitBtn;
    HelpButton: TBitBtn;
    FrameBevel: TBevel;
    MessageEdit: TLabel;
    AbortButton: TBitBtn;
    ProgressBar: TProgressBar;
    OKButton: TBitBtn;
    YesToAllButton: TBitBtn;
    NoToAllButton: TBitBtn;
    procedure IgnoreButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure NoButtonClick(Sender: TObject);
    procedure YesButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure RetryButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure AllButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure YesToAllButtonClick(Sender: TObject);
    procedure NoToAllButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);

  protected //Progress bar
    updateProgressEvery: integer;
    lastUpdateProgress: integer;
    FProgressPaused: boolean;
    FProgressError: boolean;
    taskbar: TTaskbarProgress; //Task bar progress indicator
    procedure UpdateProgressBarState;
    procedure UpdateTaskbarProgress;
  public
    procedure SetMaxProgress(maxprogr:integer);
    procedure SetProgress(i:integer); //Set progress and repaint if needed
    procedure SetProgressPaused(Value: boolean); //call these when the operation is paused/in error
    procedure SetProgressError(Value: boolean);

  { This form can be displayed as "Modal non-execution-stealing". This is useful
   for displaying the responsive progress bar.
   Modal enter/exit code is copied with some omissions from TForm.ShowModal }
  protected
    LastProcessMessages: cardinal;
    //Some of the stuff ShowModal saves
    WindowList: TTaskWindowList;
    LSaveFocusState: TFocusState;
    ActiveWindow: HWnd;
    g_wmTBC: UINT;
    FJob: TJob;
    FOnCancelQuery: TCancelQueryEvent;
    procedure EnterModal;
    procedure ReleaseModal;
    procedure JobOperationChanged(Sender: TObject);
    procedure JobProgressChanged(Sender: TObject);
    procedure JobYield(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Dispatch(var Message); override;
    procedure ExecuteJob(AJob: TJob);
    property OnCancelQuery: TCancelQueryEvent read FOnCancelQuery write
      FOnCancelQuery;

  protected //Buttons
    FButtonYPos: integer; //set by FormResize
    FVisibleButtons: TMsgDlgButtons;
    FVisibleButtonCount: integer; //available after setting visible buttons
    FTotalButtonWidth: integer; //same
    procedure SetVisibleButtons(buttons: TMsgDlgButtons);
  public
    property ButtonYPos: integer read FButtonYPos;
    property VisibleButtons: TMsgDlgButtons read FVisibleButtons write SetVisibleButtons;
    property VisibleButtonCount: integer read FVisibleButtonCount;
    property TotalButtonWidth: integer read FTotalButtonWidth;

  protected
    CurRes:TMsgDlgBtn; //Why do we have this when there's ModalResult?..
  public
    procedure SetMessage(s:string); // changes the message of the dialog, but DOES NOT resize the dialog to fit the new message size
    procedure Appear; // shows the dialog
    procedure AppearModal; // shows the dialog in modal mode (but doesn't steal the execution like ShowModal does. Call ProcessModalMessages please.)
    procedure Refresh; // repaints the dialog
    procedure ProcessMessages; // processes messages from the queue so that the system doesn't think we're stuck.

  end;

type TPromptType=(InfoStyle,WarningStyle,SuccessStyle,AskStyle,ErrorStyle);
  // styles of a prompt
  // InfoStyle: information window, 'i' sign
  // WarningStyle: warning window, '!' sign
  // SuccessStyle: success window, 'i' sign
  // AskStyle: ask window, '?' sign
  // ErrorStyle: error window, '#' sign

const YesNoButtons:TMsgDlgButtons=[mbYes,mbNo];
      YesNoCancelButtons:TMsgDlgButtons=[mbYes,mbNo,mbCancel];
      OKCancelButtons:TMsgDlgButtons=[mbOK,mbCancel];
      OKButtons:TMsgDlgButtons=[mbOK];
      AbortRetryIgnoreButtons:TMsgDlgButtons=[mbAbort,mbRetry,mbIgnore];
  // predefined button sets

procedure SMSetButtonCaption(button:TMsgDlgBtn;cap:string);
// sets the caption of button to cap
// button is one of standard dialog buttons defined in Dialogs unit
// valid buttons are:
//   mbOK, mbCancel, mbAll, mbYes, mbYesToAll, mbNo, mbNoToAll,
//   mbAbort, mbRetry, mbIgnore, mbHelp

function SMCreateDlg(buttons:TMsgDlgButtons;minsizex,minsizey:integer;
  hasprogress:boolean;sign,title,mess:string):TSMPromptForm;
// creates the dialog form (but does not show it, use Appear method to
// show it) and returns the created form
// buttons - buttons on the form, [] means form without buttons
// minsizex, minsizey - minimum size of the form (the form is autosized
//                      to match the size of the buttons and the message
// hasprogress - True if you want the form to contain a progress bar
// sign - caption of the big 3D sign on the left of the form
// title - title of the form
// mess - initial message
// WARNING: Do not forget to free the form, when you want to close it
function SMMessageDlg(title,mess:string):TSMPromptForm;
// creates buttonless message form and displays it
function SMMessageFixDlg(title,mess:string;minsizex:integer):TSMPromptForm;

function SMProgressDlg(title,mess:string;maxprogress:integer;canCancel:boolean=false):TSMPromptForm;
function SMProgressDlgCreate(title,mess:string;maxprogress:integer;canCancel:boolean=false):TSMPromptForm;

// creates buttonless message form with a progress bar that has its
// maximum position maxprogress and displays it
function SMPromptDlg(buttons:TMsgDlgButtons;sign,title,mess:string):TMsgDlgBtn;
// shows a prompt and waits for an user button press; returns the code of
// the button user pressed
procedure WriteSM(s:string);
// adds a line to the string buffer
procedure ClearSM;
// clears string buffer
procedure SMPrompt(style:TPromptType;title,mess:string);
// displays standard dialog with an OK button and waits for user press
procedure SMSPrompt(style:TPromptType;title:string);
// displays standard dialog with an OK button with a message given
// by the string buffer and waits for user press
function SMAsk(style:TPromptType;buttons:TMsgDlgButtons;title,mess:string):TMsgDlgBtn;
// displays standard dialog with buttons and returns code of the button user
// pressed
function SMSAsk(style:TPromptType;buttons:TMsgDlgButtons;title:string):TMsgDlgBtn;
// displays standard dialog with buttons and message given by the string buffer
// and returns code of the button user pressed
function SMYesNo(warningst:boolean;title,mess:string):boolean;
// displays standard dialog with buttons Yes and No and returns True if
// user pressed Yes button and False if user pressed No button

implementation
uses Consts, Types;

{$R *.DFM}

const
 //SMPromptForm.ProcessMessages won't check for messages more often than in this interval
 //(don't set too high - this'll slow down code which uses the progress form)
  PROCESS_MESSAGES_EVERY_MSEC = 50;

var
 //Initialized with english defaults
  SMButtonCaps:array[TMsgDlgBtn] of string;
  SMText:string='';

constructor TSMPromptForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CurRes := mbOK; //by default
  ModalResult := 0;
end;

destructor TSMPromptForm.Destroy;
begin
  ReleaseModal();
  inherited;
end;

procedure TSMPromptForm.FormShow(Sender: TObject);
begin
  taskbar := TTaskbarProgress.Create;
  UpdateTaskbarProgress;
end;

procedure TSMPromptForm.FormHide(Sender: TObject);
begin
  FreeAndNil(taskbar);
end;

procedure TSMPromptForm.Dispatch(var Message);
begin
  inherited Dispatch(Message);
end;

//Makes this form Modal, but doesn't steal the execution like ShowModal does.
procedure TSMPromptForm.EnterModal;
begin
  CancelDrag;
  if Visible or not Enabled or (fsModal in FFormState) or
    (FormStyle = fsMDIChild) then
    raise EInvalidOperation.Create(SCannotShowModal);
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  ReleaseCapture;
  Application.ModalStarted;

  { RecreateWnd could change the active window }
  ActiveWindow := GetActiveWindow;
  Include(FFormState, fsModal);
  if (PopupMode = pmNone) and (Application.ModalPopupMode <> pmNone) then
  begin
    RecreateWnd;
    HandleNeeded;
    { The active window might have become invalid, refresh it }
    if (ActiveWindow = 0) or not IsWindow(ActiveWindow) then
      ActiveWindow := GetActiveWindow;
  end;
  LSaveFocusState := SaveFocusState;
  Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
  Screen.FocusedForm := Self;
  WindowList := DisableTaskWindows(0);
  Show;
end;

procedure TSMPromptForm.ReleaseModal;
begin
  if not (fsModal in FormState) then exit;
  SendMessage(Handle, CM_DEACTIVATE, 0, 0);
  Hide;

  EnableTaskWindows(WindowList);
  if Screen.SaveFocusedList.Count > 0 then
  begin
    Screen.FocusedForm := TCustomForm(Screen.SaveFocusedList.First);
    Screen.SaveFocusedList.Remove(Screen.FocusedForm);
  end else Screen.FocusedForm := nil;
  { ActiveWindow might have been destroyed and using it as active window will
    force Windows to activate another application }
  if (ActiveWindow <> 0) and not IsWindow(ActiveWindow) then
    ActiveWindow := 0;
  if ActiveWindow <> 0 then
    SetActiveWindow(ActiveWindow);
  RestoreFocusState(LSaveFocusState);
  Exclude(FFormState, fsModal);

  Application.ModalFinished;
end;

{
  Vertical layout:
    [72-icon] 16 left-border 8 (label) 8 right-border 16
  Horizontal layout:
    16 top-border 8 (label) [progress-bar 11] bottom-border [16 buttons] 16
}

procedure TSMPromptForm.FormResize(Sender: TObject);
var frameh: integer;
begin
  //Frame bevel
  FrameBevel.Width:=ClientWidth-FrameBevel.Left-16;
  frameh:=ClientHeight-FrameBevel.Top-16;
  if VisibleButtonCount>0 then
    frameh:=frameh-8-OKButton.Height;
  FrameBevel.Height := frameh;

  FButtonYPos := FrameBevel.Top+FrameBevel.Height+12;

  //Progress bar - may be invisible
  ProgressBar.Left:=MessageEdit.Left;
  ProgressBar.Top:=FrameBevel.Top+FrameBevel.Height-12-ProgressBar.Height;
  ProgressBar.Width:=FrameBevel.Width-16;

  //Reposition buttons
  SetVisibleButtons(FVisibleButtons);
end;

procedure TSMPromptForm.SetVisibleButtons(buttons: TMsgDlgButtons);
const
  SZ_MINBTNSPACE = 4; //we can't leave less than this space between buttons
var btnlim: integer;   //total space available for buttons
  btnspace: integer; //space between buttons -- grows when there's too much of available space
  btnx: integer; //current button x
  btn: TBitBtn;
  vi:TMsgDlgBtn;
begin
  FVisibleButtons := buttons;

  //Calculate button count
  FVisibleButtonCount:=0;
  for vi:=Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if vi in buttons then inc(FVisibleButtonCount);

  //Calculate button space and total width
  btnlim:=FrameBevel.Width+12;
  btnspace:=(btnlim-(VisibleButtonCount*OKButton.Width)) div (VisibleButtonCount+1);
  if btnspace<SZ_MINBTNSPACE then btnspace := SZ_MINBTNSPACE;
  FTotalButtonWidth:=VisibleButtonCount*(OKButton.Width+btnspace)+btnspace;

  //Reposition buttons
  btnx := (FrameBevel.Left-6)+(btnlim - TotalButtonWidth) div 2 + btnspace; //can be negative
  for vi:=Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    case vi of
      mbOK:btn:=OKButton;
      mbCancel:btn:=CancelButton;
      mbYes:btn:=YesButton;
      mbNo:btn:=NoButton;
      mbAll:btn:=AllButton;
      mbHelp:btn:=HelpButton;
      mbAbort:btn:=AbortButton;
      mbRetry:btn:=RetryButton;
      mbIgnore:btn:=IgnoreButton;
      mbNoToAll:btn:=NoToAllButton;
      mbYesToAll:btn:=YesToAllButton;
    else
      btn := nil; //crash and burn
    end;
    if btn<>nil then
      if not (vi in FVisibleButtons) then
        btn.Visible:=false
      else
      begin
        btn.Visible:=true;
        btn.Left:=btnx; //minus 3 because they have icons... they look unbalanced when centered properly
        btn.Caption:=SMButtonCaps[vi];
        btn.Top:=ButtonYPos;
        btn.TabOrder:=integer(vi);
        Inc(btnx, btnspace+btn.Width);
      end;
  end;

  //Somehow Help button is the pariah...
  HelpButton.Visible:=false;
end;


procedure TSMPromptForm.SetMessage(s:string);
begin
  MessageEdit.Caption:=s;
  MessageEdit.Invalidate;
  MessageEdit.Update;
end;

procedure TSMPromptForm.Refresh;
begin
  Invalidate;
  Update;
end;

procedure TSMPromptForm.Appear;
begin
  if not Application.MainForm.Visible then
    Self.Position := poScreenCenter;
  LastProcessMessages := GetTickCount - PROCESS_MESSAGES_EVERY_MSEC;
  Show;
  Refresh;
end;

procedure TSMPromptForm.AppearModal;
begin
  if not Application.MainForm.Visible then
    Self.Position := poScreenCenter;
  LastProcessMessages := GetTickCount - PROCESS_MESSAGES_EVERY_MSEC;
  EnterModal;
  Show;
  SendMessage(Handle, CM_ACTIVATE, 0, 0);
  Refresh;
end;

procedure TSMPromptForm.SetMaxProgress(maxprogr:integer);
begin
  ProgressBar.Max := maxprogr;
 //We don't want to update very often since redrawing is slow.
 //Let's only update on every percent or less.
  updateProgressEvery := maxprogr div 100;
  if updateProgressEvery<1 then updateProgressEvery := 1; //<100 items
  lastUpdateProgress := -updateProgressEvery-1; //update right now
  SetProgress(0);
end;

procedure TSMPromptForm.SetProgress(i:integer);
begin
  //Do not update progress too often
  if i < lastUpdateProgress + updateProgressEvery then exit;
  //This only works if the progress always goes up. If it goes down, we'll
  //have to check in the reverse direction too.

  ProgressBar.Position:=i;
  ProgressBar.Invalidate;
  ProgressBar.Update;
  lastUpdateProgress := i;
  UpdateProgressBarState();
  UpdateTaskbarProgress();
end;

procedure TSMPromptForm.UpdateProgressBarState;
begin
  if ProgressBar.Max>0 then
    ProgressBar.Style := pbstNormal
  else
    Progressbar.Style := pbstMarquee;

  if FProgressError then
    ProgressBar.State := pbsError
  else
  if FProgressPaused then
    ProgressBar.State := pbsPaused
  else
    ProgressBar.State := pbsNormal;
end;

procedure TSMPromptForm.UpdateTaskbarProgress;
begin
  if taskbar=nil then exit;
  if not ProgressBar.Visible then
    taskbar.State := tsNoProgress
  else
  if FProgressError then
    taskbar.State := tsError
  else
  if FProgressPaused then
    taskbar.State := tsPaused
  else begin
    taskbar.SetProgress(ProgressBar.Position, ProgressBar.Max);
    taskbar.State := tsNormal;
  end;
end;

procedure TSMPromptForm.SetProgressPaused(Value: boolean);
begin
  FProgressPaused := Value;
  UpdateProgressBarState();
  UpdateTaskbarProgress();
end;

procedure TSMPromptForm.SetProgressError(Value: boolean);
begin
  FProgressError := Value;
  UpdateProgressBarState();
  UpdateTaskbarProgress();
end;


procedure TSMPromptForm.ProcessMessages;
var tm: cardinal;
begin
  tm := GetTickCount;
  if tm-LastProcessMessages > PROCESS_MESSAGES_EVERY_MSEC then begin
    Application.ProcessMessages;
    LastProcessMessages := tm;
  end;
end;

procedure TSMPromptForm.IgnoreButtonClick(Sender: TObject);
begin
  curres:=mbIgnore;
  ModalResult:=mrIgnore;
  Close;
end;

procedure TSMPromptForm.OKButtonClick(Sender: TObject);
begin
  curres:=mbOK;
  ModalResult:=mrOK;
  Close;
end;

procedure TSMPromptForm.NoButtonClick(Sender: TObject);
begin
  curres:=mbNo;
  ModalResult:=mrNo;
  Close;
end;

procedure TSMPromptForm.YesButtonClick(Sender: TObject);
begin
  curres:=mbYes;
  ModalResult:=mrYes;
  Close;
end;

procedure TSMPromptForm.CancelButtonClick(Sender: TObject);
begin
  curres:=mbCancel;
  ModalResult:=mrCancel;
  Close;
end;

procedure TSMPromptForm.RetryButtonClick(Sender: TObject);
begin
  curres:=mbRetry;
  ModalResult:=mrRetry;
  Close;
end;

procedure TSMPromptForm.HelpButtonClick(Sender: TObject);
begin
  curres:=mbHelp;
  ModalResult:=mrCancel;
  Close;
end;

procedure TSMPromptForm.AllButtonClick(Sender: TObject);
begin
  curres:=mbAll;
  ModalResult:=mrAll;
  Close;
end;

procedure TSMPromptForm.AbortButtonClick(Sender: TObject);
begin
  curres:=mbAbort;
  ModalResult:=mrAbort;
  Close;
end;

procedure TSMPromptForm.YesToAllButtonClick(Sender: TObject);
begin
  curres:=mbYesToAll;
  ModalResult:=mrYesToAll;
  Close;
end;

procedure TSMPromptForm.NoToAllButtonClick(Sender: TObject);
begin
  curres:=mbNoToAll;
  ModalResult:=mrNoToAll;
  Close;
end;

{ Executes a job in this thread until finished or failed. Updates operation,
 progress indicators. Changes AJob's event handlers.
 If cancelled by user, raises EAbort. }
procedure TSMPromptForm.ExecuteJob(AJob: TJob);
begin
  FJob := AJob;
  AJob.OnOperationChanged := Self.JobOperationChanged;
  AJob.OnProgressChanged := Self.JobProgressChanged;
  AJob.OnYield := Self.JobYield;
  Self.SetMessage(AJob.Operation);
  Self.SetMaxProgress(AJob.MaxProgress);
  Self.SetProgress(AJob.Progress);
  repeat
    AJob.ProcessChunk;
    JobYield(AJob); //in case this is a straight chunked job
  until (AJob.State=jsFinished);
  AJob.OnProgressChanged := nil;
  AJob.OnOperationChanged := nil;
  AJob.OnYield := nil;
end;

procedure TSMPromptForm.JobOperationChanged(Sender: TObject);
begin
  Self.SetMessage(FJob.Operation);
end;

procedure TSMPromptForm.JobProgressChanged(Sender: TObject);
begin
  if Self.ProgressBar.Max<>FJob.MaxProgress then
    Self.SetMaxProgress(FJob.MaxProgress);
  if Self.ProgressBar.Position<>FJob.Progress then
    Self.SetProgress(FJob.Progress);
end;

//Called when we have a moment to process messages and so on.
procedure TSMPromptForm.JobYield(Sender: TObject);
var DoAbort: boolean;
begin
  Self.ProcessMessages;
  if Self.ModalResult=mrCancel then begin
    Self.SetProgressPaused(true);
    DoAbort := true;
    if Assigned(FOnCancelQuery) then
      FOnCancelQuery(Self, DoAbort);
    if DoAbort then
      raise EAbort.Create('Aborted by user'); //no need to localize
   //Restore the window
    Self.ModalResult := 0;
    Self.SetProgressPaused(false);
    Self.Show; //ModalResult hides it
  end;
end;



procedure SMSetButtonCaption(button:TMsgDlgBtn;cap:string);
begin
  SMButtonCaps[button]:=cap;
end;

function SMCreateDlg(buttons:TMsgDlgButtons;minsizex,minsizey:integer;
  hasprogress:boolean;sign,title,mess:string):TSMPromptForm;
var frm:TSMPromptForm;
  formw: integer;
  formh: integer;
  cli: TRect;
begin
  Application.CreateForm(TSMPromptForm,frm);
  frm.Caption:=title;
  if sign='' then
  begin
    frm.Sign1Label.Visible:=false;
    frm.Sign2Label.Visible:=false;
    frm.Sign3Label.Visible:=false;
    frm.FrameBevel.Left:=16;
    frm.MessageEdit.Left:=24;
  end;
  frm.Sign1Label.Caption:=sign;
  frm.Sign2Label.Caption:=sign;
  frm.Sign3Label.Caption:=sign;
  frm.MessageEdit.Caption:=mess;

  //Adjust width and height to the size of the content
  formw := frm.FrameBevel.Left+(8+frm.MessageEdit.Width+8)+16;
  formh := frm.FrameBevel.Top+(8+frm.MessageEdit.Height+8)+16;
  if hasprogress then formh := formh+frm.ProgressBar.Height+12;

  //Calculate buttons size
  frm.VisibleButtons := buttons;
  if frm.VisibleButtonCount > 0 then
    formh := formh + 8 + frm.OKButton.Height;

  //Adjust sizes some more
  if (formh<120) and (sign<>'') then formh:=120;
  if formh<minsizey then formh:=minsizey;
  if (formw<330) and (frm.VisibleButtonCount>0) then formw:=330;
  if formw<minsizex then formw:=minsizex;
  if formw<frm.TotalButtonWidth then formw := frm.TotalButtonWidth;

  //Apply form size
  cli := frm.GetClientRect;
  frm.SetBounds(cli.Left, cli.Right, frm.Width - cli.Right + formw, frm.Height - cli.Bottom + formh);

  //Position form at the center of the screen by default
  frm.Left:=Screen.Width div 2-(frm.Width div 2);
  frm.Top:=Screen.Height div 2-(frm.Height div 2);

  frm.ProgressBar.Visible:=hasprogress;  
  result:=frm;
end;

function SMMessageDlg(title,mess:string):TSMPromptForm;
begin
  result:=SMCreateDlg([],0,0,false,'',title,mess);
  result.Appear;
end;

function SMMessageFixDlg(title,mess:string;minsizex:integer):TSMPromptForm;
begin
  result:=SMCreateDlg([],minsizex,0,false,'',title,mess);
  result.Appear;
end;

//Creates buttonless message form of given minimal size and displays it
function SMProgressDlg(title,mess:string;maxprogress:integer;canCancel:boolean):TSMPromptForm;
var btns: TMsgDlgButtons;
begin
  if canCancel then
    btns := [mbCancel]
  else
    btns := [];
  Result:=SMCreateDlg(btns,200,0,true,'',title,mess);
  Result.SetMaxProgress(maxprogress);
  Result.AppearModal;
end;

//Same but doesn't display it
function SMProgressDlgCreate(title,mess:string;maxprogress:integer;canCancel:boolean=false):TSMPromptForm;
var btns: TMsgDlgButtons;
begin
  if canCancel then
    btns := [mbCancel]
  else
    btns := [];
  Result:=SMCreateDlg(btns,200,0,true,'',title,mess);
  Result.SetMaxProgress(maxprogress);
end;

function SMPromptDlg(buttons:TMsgDlgButtons;sign,title,mess:string):TMsgDlgBtn;
var frm:TSMPromptForm;
begin
  frm:=SMCreateDlg(buttons,0,0,false,sign,title,mess);
  frm.ShowModal;
  result:=frm.curres;
end;

procedure ClearSM;
begin
  smtext:='';
end;

procedure WriteSM;
begin
  if smtext='' then smtext:=s else smtext:=smtext+#13+s;
end;

function SMAsk;
begin
  case style of
    SuccessStyle,InfoStyle:result:=SMPromptDlg(buttons,'i',title,mess);
    WarningStyle:result:=SMPromptDlg(buttons,'!',title,mess);
    ErrorStyle:result:=SMPromptDlg(buttons,'#',title,mess);
    AskStyle:result:=SMPromptDlg(buttons,'?',title,mess);
  else Result := mbCancel;
  end;
end;

function SMSAsk;
begin
  result:=SMAsk(style,buttons,title,smtext);
  smtext:='';
end;

procedure SMPrompt;
begin
  SMAsk(style,[mbOK],title,mess);
end;

procedure SMSPrompt;
begin
  SMAsk(style,[mbOK],title,smtext);
  smtext:='';
end;

function SMYesNo;
begin
  if warningst then
    result:=SMAsk(WarningStyle,YesNoButtons,title,mess)=mbYes else
    result:=SMAsk(InfoStyle,YesNoButtons,title,mess)=mbYes;
end;



initialization
  SMSetButtonCaption(mbOK,'&OK');
  SMSetButtonCaption(mbCancel,'&Cancel');
  SMSetButtonCaption(mbAll,'&All');
  SMSetButtonCaption(mbAbort,'A&bort');
  SMSetButtonCaption(mbRetry,'&Retry');
  SMSetButtonCaption(mbIgnore,'&Ignore');
  SMSetButtonCaption(mbYes,'&Yes');
  SMSetButtonCaption(mbYesToAll,'Yes to all');
  SMSetButtonCaption(mbNo,'&No');
  SMSetButtonCaption(mbNoToAll,'No to all');
  SMSetButtonCaption(mbHelp,'&Help');

end.
