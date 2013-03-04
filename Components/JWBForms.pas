unit JWBForms;

interface
uses Types, Forms, Graphics, Controls, ExtCtrls, Messages;

{$DEFINE ANCHORFAILFIX}
{ If set: Use more visually pleasant and thorough solution to anchors breaking
 on RecreateWnd(), which might just have consequences.
 If clear: Use ugly but proven and safe solution with form blinking. }

{
Docking guide.
Every form has docked and undocked dimensions. Undocked ones are saved on dock
and kept in UndockWidth/UndockHeight.
When docking, the form is asked to provide docked dimensions it preserved.
By default, undocked dimensions are used.
When undocking, the form is given the chance to preserve last docked dimensions.
By default, they're discarded.
}

const
  WM_GET_DOCKED_W = WM_APP + 1; { Docker calls these to get docked control sizes before docking }
  WM_GET_DOCKED_H = WM_APP + 2;
  WM_SAVE_DOCKED_WH = WM_APP + 3; { Docker calls this to save docked sizes before undocking }
  WM_SET_DOCK_MODE = WM_APP + 4; { Docker calls this before docking or after undocking. Use this chance to prepare the form by rearranging controls }

function DockProc(slave:TForm;panel:TPanel;dir:TAlign;dock:boolean): boolean;
procedure UndockedMakeVisible(slave:TForm);

implementation
uses Classes;

{
Many forms are built on Anchors, and those are broken. See:
  http://stackoverflow.com/questions/15062571/workaround-for-anchors-being-broken-when-recreating-a-window
Any time you recreate a window, controls which are less than 0 in size due to
anchoring get messed up.
Workaround 1. Disable UpdateAnchorRules() by setting csLoading while
 recreating the window.
Workaround 2. Make the window huge when showing it, so that all controls
 fit, then resize it.
}

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

end.
