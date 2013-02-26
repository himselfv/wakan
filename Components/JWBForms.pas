unit JWBForms;

interface
uses Types, Forms, Graphics, Controls, ExtCtrls, Messages;

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

implementation

type
  TSlaveHack = class(TForm)
  end;

{
Dock the form and make it visible, or
Hide and undock it, restore its permanent width and height.
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

 {
  Many forms are built on Anchors, and those are broken. See:
    http://stackoverflow.com/questions/15062571/workaround-for-anchors-being-broken-when-recreating-a-window
  Any time you recreate a window, controls which are less than 0 in size due to
  anchoring get messed up.
  The workaround is to make the window huge when showing it, so that all controls
  fit, then resize it. This way internal "explicit control size" is preserved.
 }

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
    slave.Width := 10000;
    slave.Height := 10000;
    slave.Visible := true; //UpdateExplicitBounds!
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
    slave.Perform(WM_SET_DOCK_MODE,integer(dir),0);
  end;
end;

end.
