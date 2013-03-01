unit AnchorFailFix;

interface
uses Classes, Controls;
{
One proposed fix for the anchors layout problem.

THE PROBLEM:
When you RecreateWnd() or do anything which triggers it, there are several calls
to UpdateAnchorRules for each of the child controls.
If at that time any of the controls is less than 0 in size due to anchoring,
it gets registered as having size of 0 instead and the layout is broken.

THE SOLUTION:
UpdateAnchorRules is ineffective when csLoading is set. So set csLoading before
recreating the form and clear afterwards.
Notes:
1. It needs to be set for every child control (handled in this unit for you).
2. It might have side effects (no telling where else csLoading will disable
  something useful)
3. It needs to be done just before showing the form for the first time after
  recreating it, and then disabled just after that.

OTHER SOLUTIONS:
1. Make your form big enough for all anchored controls to be non-zero,
 then show it and resize it to normal.
 Con: no way around blinking.
2. Put a panel which holds all controls onto your form, then resize the panel
 per (1), show the form, downsize the panel.
 Pro: blinking is not so obvious.
 Con: additional panel which must hold all controls.
}

type
  TComponentHack = class helper for TComponent
  public
    procedure SetCsLoading(Value: boolean);
  end;

implementation

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

end.
