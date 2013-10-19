unit JWBForms;

{$DEFINE ANCHORFAILFIX}
{ If set: Use more visually pleasant and thorough solution to anchors breaking
 on RecreateWnd(), which might just have consequences.
 If clear: Use ugly but proven and safe solution with form blinking. }

interface
uses Types, Forms, Graphics, Controls, ExtCtrls, Messages;

{
Docking guide.

1. Every form has docked and undocked dimensions. Undocked ones are saved on dock
 and kept in UndockWidth/UndockHeight.

2. When docking, the form is asked to provide docked dimensions it preserved.
 By default, undocked dimensions are used.

3. When undocking, the form is given the chance to preserve last docked dimensions.
 By default, they're discarded.

3a. Depending on the docking type, you may want to update not both of the docked
 dimensions. E.g. when docked left, the height is always full height of parent.

4. The type of docking is indicated with TAlign. The form can rearrange controls
 differently depending on the type.
   alLeft, alRight: horizontal docking
   alTop, alBottom: vertical docking
   alCustom: undocked

5. Wakan supports "Portrait mode", this is simply re-docking the forms vertically.
 Support all types of docking and you're good to go.

6. Mind the resizing problems when rearranging controls (see below).
}

const
  WM_GET_DOCKED_W = WM_APP + 1; { Docker calls these to get docked control sizes before docking }
  WM_GET_DOCKED_H = WM_APP + 2;
  WM_SAVE_DOCKED_WH = WM_APP + 3; { Docker calls this to save docked sizes before undocking }
  WM_SET_DOCK_MODE = WM_APP + 4; { Docker calls this before docking or after undocking. Use this chance to prepare the form by rearranging controls }

function DockProc(slave:TForm;panel:TPanel;dir:TAlign;dock:boolean): boolean;
procedure UndockedMakeVisible(slave:TForm);


{
Many forms are built on Anchors, and those are broken. See:
  http://stackoverflow.com/questions/15062571/workaround-for-anchors-being-broken-when-recreating-a-window
Any time you recreate a window, controls which are less than 0 in size due to
anchoring get messed up.
Setting negative control sizes explicitly (.Width := -14) also breaks layout.

Be careful:
1. When you dock/undock the form.
2. When you manually rearrange the controls (e.g. set Top/Left/Width/Height from
 code)

Workaround 1. Disable UpdateAnchorRules() by setting csLoading while
 recreating the window.
Workaround 2. Make the window huge when showing it, so that all controls
 fit, then resize it back.

Workaround 2 is ugly, but you can use it to rearrange controls while hidden.
}

{
These are some helpers. Usage:
  with SafeRecreate(AForm) do try
    RecreateHandle;
  finally
    Unlock;
  end;
}
type
  TDisableAnchorRulesHelper = record
    FForm: TForm;
    procedure Lock;
    procedure Unlock;
  end;
  TTemporarilyResizeHelper = record
    FForm: TForm;
    OldSize: TPoint;
    procedure Lock;
    procedure Unlock;
    function SetWH(const wh: TPoint): TPoint;
  end;
 {$IFDEF ANCHORFAILFIX}
  TSafeRecreateHelper = TDisableAnchorRulesHelper;
 {$ELSE}
  TSafeRecreateHelper = TTemporarilyResizeHelper;
 {$ENDIF}
  TSafeRearrangeHelper = TTemporarilyResizeHelper;

function SafeRecreate(AForm: TForm): TSafeRecreateHelper;
function SafeRearrange(AForm: TForm): TSafeRearrangeHelper;


{
Common form ancestor for Wakan.
You don't *have* to inherit from TJwbForm but it does give you some benefits:
 1. Automatic translation on creation.
  It's impossible to hook creation event and translate all forms on the fly,
  but all static forms are translated at the start even without this.
}
type
  TJwbForm = class(TForm)
  protected
    procedure DoCreate; override;
  end;

implementation
uses Classes, JWBLanguage;

type
  TComponentHack = class helper for TComponent
  public
    procedure SetCsLoading(Value: boolean);
  end;

procedure TComponentHack.SetCsLoading(Value: boolean);
var i: integer;
begin
  if Value then
    Self.FComponentState := Self.FComponentState + [csLoading]
  else
    Self.FComponentState := Self.FComponentState - [csLoading];
  for i := 0 to Self.ComponentCount-1 do
    if Self.Components[i] is TControl then
      TControl(Self.Components[i]).SetCsLoading(Value);
end;

procedure TDisableAnchorRulesHelper.Lock;
begin
  FForm.SetCsLoading(true);
end;

procedure TDisableAnchorRulesHelper.Unlock;
begin
  FForm.SetCsLoading(false);
end;

function TTemporarilyResizeHelper.SetWH(const wh: TPoint): TPoint;
begin
  Result.X := FForm.ClientWidth;
  Result.Y := FForm.ClientHeight;
  FForm.ClientWidth := wh.X;
  FForm.ClientHeight := wh.Y;
end;

procedure TTemporarilyResizeHelper.Lock;
begin
  OldSize := SetWH(Point(10000,10000));
end;

procedure TTemporarilyResizeHelper.Unlock;
begin
  SetWH(OldSize);
end;

function SafeRecreate(AForm: TForm): TSafeRecreateHelper;
begin
  Result.FForm := AForm;
  Result.Lock;
end;

function SafeRearrange(AForm: TForm): TSafeRearrangeHelper;
begin
  Result.FForm := AForm;
  Result.Lock;
end;


{
- Dock the form and make it visible, or
- Hide and undock it, restore its permanent width and height.
Returns true if the form was docked before the call.
}
function DockProc(slave:TForm;panel:TPanel;dir:TAlign;dock:boolean): boolean;
var vert:boolean;
  rect:TRect;
  sz: integer;
begin
  Result := slave.HostDockSite<>nil;
  if Result=dock then exit;
  vert:=dir in [alTop,alBottom];

  if dock then begin
    if vert then begin
      sz := slave.Perform(WM_GET_DOCKED_H,0,0);
      if sz<=0 then sz := slave.Height;
      panel.Height := sz;
    end else begin
      sz := slave.Perform(WM_GET_DOCKED_W,0,0);
      if sz<=0 then sz := slave.Width;
      panel.Width := sz;
    end;
    slave.Perform(WM_SET_DOCK_MODE,integer(dir),0);
    slave.Visible := false;
//    if slave.HandleAllocated then
//      TSlaveHack(slave).DestroyWindowHandle;
//   ^ don't do or some forms will lose their state (such as TCheckListBox contents)
    slave.ManualDock(panel);
   {$IFDEF ANCHORFAILFIX}
    slave.SetCsLoading(true);
    try
   {$ELSE}
    slave.Width := 10000;
    slave.Height := 10000;
   {$ENDIF}
    slave.Visible := true; //UpdateExplicitBounds!
   {$IFDEF ANCHORFAILFIX}
    finally
      slave.SetCsLoading(false);
    end;
   {$ENDIF}
    slave.Align := alClient;
  end else begin
    slave.Perform(WM_SAVE_DOCKED_WH,0,0);
    slave.Hide;
    rect.Left:=0;
    rect.Top:=0;
    rect.Right:=slave.UndockWidth; //non-docked width and height
    rect.Bottom:=slave.UndockHeight; //only available when docked
    slave.Align:=alNone;
    slave.ManualFloat(rect);
    if vert then panel.height:=0 else panel.width:=0;
    slave.Perform(WM_SET_DOCK_MODE,integer(alNone),0);
  end;
end;

{
Undocked form is hidden by default.
Call this to make it visible in a safe way which doesn't break form layout
because of anchor layout bug.
It's not compulsory, you may make it visible by yourself if you know what to
watch out for.
}
procedure UndockedMakeVisible(slave:TForm);
begin
 {$IFDEF ANCHORFAILFIX}
  slave.SetCsLoading(true);
  try
 {$ELSE}
  slave.Width := 10000;
  slave.Height := 10000;
 {$ENDIF}
  slave.Visible := true;
 {$IFDEF ANCHORFAILFIX}
  finally
    slave.SetCsLoading(false);
  end;
 {$ENDIF}
end;

procedure TJwbForm.DoCreate;
begin
  inherited;
  if fLanguage<>nil then
    fLanguage.TranslateForm(Self);
end;

end.
