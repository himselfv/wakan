unit SelectionGrids;

interface
uses
  SysUtils, Classes, Windows, Controls, Grids;

type
 { TDrawGrid which treats Selection in a different way: as a continuous span,
  line by line, instead of a rectangle }
  TSpanSelectionGrid = class(TDrawGrid)
  public
    function InSelection(ACol, ARow: Longint): boolean;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Wakan', [TSpanSelectionGrid]);
end;

function TSpanSelectionGrid.InSelection(ACol, ARow: Longint): boolean;
var sel: TGridRect;
begin
  sel := Self.Selection;
  Result := ((ARow>sel.Top) and (ARow<sel.Bottom))
    or ((ARow=sel.Top) and (ACol>=sel.Left))
    or ((ARow=sel.Bottom) and (ACol<=sel.Right));
end;

procedure TSpanSelectionGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  Hold: Integer;
begin
  if Assigned(OnDrawCell) then
  begin
    if UseRightToLeftAlignment then
    begin
      ARect.Left := ClientWidth - ARect.Left;
      ARect.Right := ClientWidth - ARect.Right;
      Hold := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := Hold;
      ChangeGridOrientation(False);
    end;
    if InSelection(ACol,ARow) then
      AState := AState + [gdSelected]
    else
      AState := AState - [gdSelected];
    OnDrawCell(Self, ACol, ARow, ARect, AState);
    if UseRightToLeftAlignment then ChangeGridOrientation(True);
  end;
end;




end.
