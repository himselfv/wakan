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
  StdCtrls, ExtCtrls, Buttons, ComCtrls;

type
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
  public
    procedure SetMessage(s:string);
    // changes the message of the dialog, but DOES NOT resize the dialog
    // to fit the new message size
    procedure Appear;
    // shows the dialog
    procedure Refresh;
    // repaints the dialog
    procedure SetProgress(i:integer);
    // if the dialog contains the progress bar, sets the Progress of the
    // progress bar to i and repaints it
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
// creates buttonless message form of given minimal size and displays it
function SMProgressDlg(title,mess:string;maxprogress:integer):TSMPromptForm;
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

var SMButtonCaps:array[TMsgDlgBtn] of string;
    CurRes:TMsgDlgBtn;
    SMText:string='';

{$R *.DFM}

procedure SMSetButtonCaption(button:TMsgDlgBtn;cap:string);
begin
  SMButtonCaps[button]:=cap;
end;

function SMCreateDlg(buttons:TMsgDlgButtons;minsizex,minsizey:integer;hasprogress:boolean;sign,title,mess:string):TSMPromptForm;
var frm:TSMPromptForm;
    vi:TMsgDlgBtn;
    nobuttons:byte;
    buttspace,butty,buttx,buttlim:integer;
    butt:TButton;
begin
  Application.CreateForm(TSMPromptForm,frm);
  if sign='' then
  begin
    frm.Sign1Label.Visible:=false;
    frm.Sign2Label.Visible:=false;
    frm.Sign3Label.Visible:=false;
    frm.FrameBevel.Left:=16;
    frm.MessageEdit.Left:=24;
  end;
  frm.MessageEdit.Caption:=mess;
  frm.FrameBevel.Width:=frm.MessageEdit.Width+16;
  frm.FrameBevel.Height:=frm.MessageEdit.Height+16;
  if hasprogress then
    frm.FrameBevel.Height:=frm.MessageEdit.Height+48;
  nobuttons:=0;
  for vi:=Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if vi in buttons then inc(nobuttons);
  butty:=frm.FrameBevel.Top+frm.FrameBevel.Height+24;
  if (butty<120) and (sign<>'') then butty:=120;
  if butty<minsizey then butty:=minsizey;
  buttlim:=frm.FrameBevel.Left+frm.FrameBevel.Width+16;
  if (buttlim<330) and (nobuttons>0) then buttlim:=330;
  if buttlim<minsizex then buttlim:=minsizex;
  if buttlim<12+(8+frm.OKButton.Width)*nobuttons then
    buttlim:=12+(8+frm.OKButton.Width)*nobuttons;
  buttspace:=(buttlim-4-(nobuttons*frm.OKButton.Width)) div (nobuttons+1);
  buttx:=2+buttspace;
  for vi:=Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    case vi of
      mbOK:butt:=frm.OKButton;
      mbCancel:butt:=frm.CancelButton;
      mbYes:butt:=frm.YesButton;
      mbNo:butt:=frm.NoButton;
      mbAll:butt:=frm.AllButton;
      mbHelp:butt:=frm.HelpButton;
      mbAbort:butt:=frm.AbortButton;
      mbRetry:butt:=frm.RetryButton;
      mbIgnore:butt:=frm.IgnoreButton;
      mbNoToAll:butt:=frm.NoToAllButton;
      mbYesToAll:butt:=frm.YesToAllButton;
    end;
    if not (vi in buttons) then butt.Visible:=false else
    begin
      butt.Visible:=true;
      butt.Left:=buttx;
      butt.Caption:=SMButtonCaps[vi];
      buttx:=buttx+buttspace+butt.Width;
      butt.Top:=butty;
      butt.TabOrder:=integer(vi);
    end;
  end;
  frm.HelpButton.Visible:=false;
  frm.Sign1Label.Caption:=sign;
  frm.Sign2Label.Caption:=sign;
  frm.Sign3Label.Caption:=sign;
  frm.Caption:=title;
  frm.ClientWidth:=buttlim;
  frm.ClientHeight:=butty+frm.OKButton.Height+12;
  frm.Left:=Screen.Width div 2-(frm.Width div 2);
  frm.Top:=Screen.Height div 2-(frm.Height div 2);
  frm.FrameBevel.Width:=frm.ClientWidth-frm.FrameBevel.Left-16;
  frm.FrameBevel.Height:=frm.ClientHeight-frm.FrameBevel.Top-32-frm.OKButton.Height;
  if nobuttons=0 then frm.ClientHeight:=butty;
  if hasprogress then
  begin
    frm.ProgressBar.Visible:=true;
    frm.ProgressBar.Left:=frm.MessageEdit.Left;
    frm.ProgressBar.Top:=frm.FrameBevel.Top+frm.FrameBevel.Height-24;
    frm.ProgressBar.Width:=frm.FrameBevel.Width-16;
  end else frm.ProgressBar.Visible:=false;
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

function SMProgressDlg(title,mess:string;maxprogress:integer):TSMPromptForm;
begin
  result:=SMCreateDlg([],200,0,true,'',title,mess);
  result.ProgressBar.Max:=maxprogress;
  result.Appear;
end;

function SMPromptDlg(buttons:TMsgDlgButtons;sign,title,mess:string):TMsgDlgBtn;
var frm:TSMPromptForm;
begin
  frm:=SMCreateDlg(buttons,0,0,false,sign,title,mess);
  frm.ShowModal;
  result:=curres;
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

procedure TSMPromptForm.IgnoreButtonClick(Sender: TObject);
begin
  curres:=mbIgnore;
  Close;
end;

procedure TSMPromptForm.OKButtonClick(Sender: TObject);
begin
  curres:=mbOK;
  Close;
end;

procedure TSMPromptForm.NoButtonClick(Sender: TObject);
begin
  curres:=mbNo;
  Close;
end;

procedure TSMPromptForm.YesButtonClick(Sender: TObject);
begin
  curres:=mbYes;
  Close;
end;

procedure TSMPromptForm.CancelButtonClick(Sender: TObject);
begin
  curres:=mbCancel;
  Close;
end;

procedure TSMPromptForm.RetryButtonClick(Sender: TObject);
begin
  curres:=mbRetry;
  Close;
end;

procedure TSMPromptForm.HelpButtonClick(Sender: TObject);
begin
  curres:=mbHelp;
  Close;
end;

procedure TSMPromptForm.AllButtonClick(Sender: TObject);
begin
  curres:=mbAll;
  Close;
end;

procedure TSMPromptForm.AbortButtonClick(Sender: TObject);
begin
  curres:=mbAbort;
  Close;
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
  Show;
  Refresh;
end;

procedure TSMPromptForm.SetProgress(i:integer);
begin
  ProgressBar.Position:=i;
  ProgressBar.Invalidate;
  ProgressBar.Update;
end;

procedure TSMPromptForm.YesToAllButtonClick(Sender: TObject);
begin
  curres:=mbYesToAll;
  Close;
end;

procedure TSMPromptForm.NoToAllButtonClick(Sender: TObject);
begin
  curres:=mbNoToAll;
  Close;
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
