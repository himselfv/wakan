unit SimpleControls;

interface

uses
  SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Messages, Windows,
  Themes, ImgList, UITypes;

type
 { Adds Autosize like in TLabel }
  TJwbCheckbox = class(TCheckBox)
  protected
    FAutoSize: Boolean;
    procedure AdjustBounds; dynamic;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
  end;

 { Label which opens URL when clicked; blue and hand-pointed by default }
  TUrlLabel = class(TLabel)
  protected
    FURL: string;
    procedure Click; override;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged);
      message CM_PARENTFONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property URL: string read FURL write FURL;
  end;

 { TPanel with some routines for popup-menu like support }
  TPopupPanel = class(TPanel)
  protected
    procedure CMCancelMode(var message: TCMCancelMode); message CM_CANCELMODE;
    procedure WndProc(var Message: TMessage); override;
  public
    procedure Popup;
  end;


procedure Register;

implementation
uses Types, Forms, ActnList, UxTheme, CommCtrl,
{$IFDEF MSWINDOWS}
  ShellAPI
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib
{$ENDIF POSIX};

procedure Register;
begin
  RegisterComponents('Wakan', [TJwbCheckbox]);
  RegisterComponents('Wakan', [TUrlLabel]);
  RegisterComponents('Wakan', [TPopupPanel]);
end;


{ JwbCheckbox }

constructor TJwbCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
end;

//See TCustomLabel.SetAutoSize
procedure TJwbCheckbox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TJwbCheckbox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

//See TCustomLabel.AdjustBounds
procedure TJwbCheckbox.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  X, W: Integer;
  Canvas: TControlCanvas;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    X := Left;
    Canvas := TControlCanvas.Create;
    try
      Canvas.Control := Self;
      Canvas.Font := Self.Font;
      W := GetSystemMetrics(SM_CXMENUCHECK) + 4 + Canvas.TextWidth(Self.Caption);
      SetBounds(X, Top, W, Height);
    finally
      FreeAndNil(Canvas);
    end;
  end;
end;


{ URL label }

constructor TUrlLabel.Create(AOwner: TComponent);
begin
  inherited;
  Self.Cursor := crHandPoint;
  Self.Font.Style := Self.Font.Style + [fsBold, fsUnderline];
  Self.Font.Color := clHighlight;
end;

procedure TUrlLabel.CMParentFontChanged(var Message: TCMParentFontChanged);
var ChangeEvent: TNotifyEvent;
begin
  inherited; //copies font from parent; sets FParentFont := true;
  if Self.ParentFont then begin
   //Auto-upgrade font but try not to break FParentFont==true
    ChangeEvent := Self.Font.OnChange;
    Self.Font.OnChange := nil;
    try
      Self.Font.Style := Self.Font.Style + [fsBold, fsUnderline];
      Self.Font.Color := clHighlight;
    finally
      Self.Font.OnChange := ChangeEvent;
    end;
  end;
end;

procedure ShellOpen(const sCommand: string; const sParams: string = '');
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(sCommand), PChar(sParams), '', SW_SHOW);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if sParams<>'' then
    _system(PAnsiChar('open ' + AnsiString(sCommand)+' '+AnsiString(sParams)))
  else
    _system(PAnsiChar('open ' + AnsiString(sCommand)));
{$ENDIF POSIX}
end;

procedure TUrlLabel.Click;
begin
  if URL<>'' then
    ShellOpen(URL);
  inherited;
end;


{ TPopupPanel }

function IsChildrenOf(AControl, AParent: TControl): boolean;
begin
  while AControl<>nil do begin
    if AControl=AParent then begin
      Result := true;
      exit;
    end;
    AControl := AControl.Parent;
  end;
  Result := false;
end;

function MsgToStr(const AMsg: integer): string;
begin
  case AMsg of
    WM_ACTIVATE: Result := 'WM_ACTIVATE';
    WM_ACTIVATEAPP: Result := 'WM_ACTIVATEAPP';
    WM_SETFOCUS: Result := 'WM_SETFOCUS';
    WM_KILLFOCUS: Result := 'WM_KILLFOCUS';
    WM_LBUTTONDOWN: Result := 'WM_LBUTTONDOWN';
    WM_LBUTTONUP: Result := 'WM_LBUTTONUP';
    WM_LBUTTONDBLCLK: Result := 'WM_LBUTTONDBLCLK';
    WM_RBUTTONDOWN: Result := 'WM_RBUTTONDOWN';
    WM_RBUTTONUP: Result := 'WM_RBUTTONUP';
    WM_RBUTTONDBLCLK: Result := 'WM_RBUTTONDBLCLK';
    WM_MBUTTONDOWN: Result := 'WM_MBUTTONDOWN';
    WM_MBUTTONUP: Result := 'WM_MBUTTONUP';
    WM_MBUTTONDBLCLK: Result := 'WM_MBUTTONDBLCLK';
    WM_XBUTTONDOWN: Result := 'WM_XBUTTONDOWN';
    WM_XBUTTONUP: Result := 'WM_XBUTTONUP';
    WM_XBUTTONDBLCLK: Result := 'WM_XBUTTONDBLCLK';
    WM_KEYDOWN: Result := 'WM_KEYDOWN';
    WM_KEYUP: Result := 'WM_KEYUP';
    WM_CANCELMODE: Result := 'WM_CANCELMODE';
    WM_GETTEXTLENGTH: Result := 'WM_GETTEXTLENGTH';
    WM_ERASEBKGND: Result := 'WM_ERASEBKGND';
    WM_CTLCOLOREDIT: Result := 'WM_CTLCOLOREDIT';
    WM_COMMAND: Result := 'WM_COMMAND';
    WM_IME_NOTIFY: Result := 'WM_IME_NOTIFY';
    WM_IME_SETCONTEXT: Result := 'WM_IME_SETCONTEXT';
    WM_NCHITTEST: Result := 'WM_NCHITTEST';
    WM_PRINTCLIENT: Result := 'WM_PRINTCLIENT';
    WM_SETCURSOR: Result := 'WM_SETCURSOR';
    WM_TIMER: Result := 'WM_TIMER';
    WM_MOUSEMOVE: Result := 'WM_MOUSEMOVE';
    WM_NCMOUSEMOVE: Result := 'WM_NCMOUSEMOVE';
    WM_PAINT: Result := 'WM_PAINT';
    CM_ACTIVATE: Result := 'CM_ACTIVATE';
    CM_DEACTIVATE: Result := 'CM_DEACTIVATE';
    CM_TEXTCHANGED: Result := 'CM_TEXTCHANGED';
    CM_MOUSEENTER: Result := 'CM_MOUSEENTER';
    CM_MOUSELEAVE: Result := 'CM_MOUSELEAVE';
    CM_HINTSHOW: Result := 'CM_HINTSHOW';
    CM_HINTSHOWPAUSE: Result := 'CM_HINTSHOWPAUSE';
  else Result := IntToStr(AMsg);
  end;
end;

function IsHiddenMsg(const AMsg: integer): boolean;
begin
  case AMsg of
    WM_TIMER,
    WM_MOUSEMOVE,
    WM_NCMOUSEMOVE,
    WM_NCHITTEST,
    WM_PAINT,
    WM_SETCURSOR,
    WM_CTLCOLOREDIT,
    CM_HINTSHOWPAUSE:
      Result := true;
  else Result := false;
  end;
end;

procedure TPopupPanel.Popup;
var Msg: TMsg;
begin
  try
    Self.Show;
    Self.SetFocus;
    Self.SelectFirst;
    repeat
      try
        if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
{          if not IsHiddenMsg(Msg.message) then
            OutputDebugString(PChar('Msg: '+MsgToStr(Msg.message))); }

          if (Msg.message=WM_ACTIVATEAPP) and (Msg.wParam=0) then
            break;
          if (Msg.message=WM_ACTIVATE) and (Msg.wParam=WA_INACTIVE) and IsChildrenOf(Self, FindControl(Msg.hwnd)) then
            break;
          if (Msg.message=WM_KILLFOCUS) and IsChildrenOf(FindControl(msg.hwnd), Self)
          and ((Msg.wParam=0) or not IsChildrenOf(FindControl(Msg.wParam), Self)) then
            break;
          if ((Msg.message=WM_LBUTTONDOWN) or (Msg.message=WM_LBUTTONDBLCLK)
          or (Msg.message=WM_RBUTTONDOWN) or (Msg.message=WM_RBUTTONDBLCLK)
          or (Msg.message=WM_MBUTTONDOWN) or (Msg.message=WM_MBUTTONDBLCLK)
          or (Msg.message=WM_XBUTTONDOWN) or (Msg.message=WM_XBUTTONDBLCLK))
          and not IsChildrenOf(FindControl(Msg.hwnd), Self) then
            break;
          if (Msg.message=WM_KEYDOWN) and (Msg.lParam=VK_ESCAPE) then
            break;
          if (Msg.message=WM_CANCELMODE) and IsChildrenOf(FindControl(Msg.hwnd), Self) then
            break;

          Application.HandleMessage;
        end;
      except
        Application.HandleException(Self);
      end;
    until Application.Terminated;
  finally
    Self.Hide;
  end;
end;

procedure TPopupPanel.CMCancelMode(var message: TCMCancelMode);
begin
  if not IsChildrenOf(message.Sender, Self) then
    Self.Hide;
end;

procedure TPopupPanel.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
{  if not IsHiddenMsg(message.Msg) then
    OutputDebugString(PChar('Panel msg: '+MsgToStr(Message.Msg))); }
end;

end.
