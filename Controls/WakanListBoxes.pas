unit WakanListBoxes;

interface

uses
  SysUtils, Types, Classes, Controls, StdCtrls, CheckLst, Windows, Messages;

type
  TStringArray = array of string;
  TSelectionArray = TStringArray;

 { Base enhanced checkbox list class.
  At this time it's only used for RomaSystem selection; at some point has to be
  split into basic functionality and RomaSystem-related stuff. }
  TWakanCheckListBox = class(TCheckListBox)
  protected
    FAutoDragItems: boolean;
    FAutoCtrlMove: boolean;
    FMultiSelect: boolean;
    FDragStartPoint: TPoint;
    FOnSelectionChanged: TNotifyEvent;
    procedure SetAutoDragItems(const Value: boolean);
    procedure SetAutoCtrlMove(const Value: boolean);
    procedure SetMultiselect(const Value: boolean); reintroduce;
    procedure Click; override;
    procedure ClickCheck; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OrderChanged;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
   //Comma-separated list of selected items
    function GetSelection: string;
    procedure SetSelection(const Value: string);
    function GetSelectionArray: TSelectionArray;
    procedure SetSelectionArray(const Value: TSelectionArray);
  public
    property Selection: string read GetSelection write SetSelection;
  published
   //Automatically support arranging items by dragging
    property AutoDragItems: boolean read FAutoDragItems write SetAutoDragItems default false;
   //Automatically support Ctrl+UP/Ctrl+DOWN to rearrange items
    property AutoCtrlMove: boolean read FAutoCtrlMove write SetAutoCtrlMove default false;
   //Allow only one item to be selected or many. Can be dynamically changed.
    property MultiSelect: boolean read FMultiselect write SetMultiselect default true;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

procedure Register;

implementation

function SplitStr(const s: string; sep: string): TStringArray;
var cnt, i, i_pos: integer;
begin
 //1. count the number of occurences
  i := 1;
  cnt := 0;
  while i<=Length(s) do begin
    i_pos := pos(sep,s,i);
    if i_pos<=0 then break;
    i:=i+i_pos+Length(sep)-1;
    Inc(cnt);
  end;
  if Length(s)>i then
    Inc(cnt);


 //2. allocate
  SetLength(Result, cnt);

 //3. copy
  i := 1;
  cnt := 0;
  while i<=Length(s) do begin
    i_pos := pos(sep,s,i);
    if i_pos<=0 then break;
    Result[cnt] := copy(s,i,i_pos-1);
    i:=i+i_pos+Length(sep)-1;
    Inc(cnt);
  end;

 //last part
  if Length(s)>i then
    Result[cnt] := copy(s,i,MaxInt);
end;

function JoinStr(const parts: TStringArray; sep: string): string;
var i: integer;
begin
  if Length(parts)<=0 then begin
    Result := '';
    exit;
  end;

  Result := parts[0];
  for i := 1 to Length(parts)-1 do
    Result := Result + sep + parts[i];
end;

constructor TWakanCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoDragItems := false;
  FAutoCtrlMove := false;
  FMultiSelect := true;
end;

function TWakanCheckListBox.GetSelection(): string;
begin
  Result := JoinStr(TStringArray(GetSelectionArray), ',');
end;

procedure TWakanCheckListBox.SetSelection(const Value: string);
begin
  SetSelectionArray(TStringArray(SplitStr(Value,',')));
end;

function TWakanCheckListBox.GetSelectionArray: TSelectionArray;
var i, cnt: integer;
begin
  if not FMultiSelect then begin
    if Self.ItemIndex<0 then begin
      SetLength(Result, 0);
      exit;
    end;
    SetLength(Result,1);
    Result[0] := Self.Items[Self.ItemIndex];
    exit;
  end;

 //count the number of checked items
  cnt := 0;
  for i := 0 to Self.Count-1 do
    if Checked[i] then Inc(cnt);
  SetLength(Result, cnt);

  cnt := 0;
  for i := 0 to Self.Count-1 do
    if Checked[i] then begin
      Result[cnt] := Self.Items[i];
      Inc(cnt);
    end;
end;

procedure TWakanCheckListBox.SetSelectionArray(const Value: TSelectionArray);
var i, j, j_pos: integer;
begin
  j := 0;
  for i := 0 to Length(Value)-1 do begin
    j_pos := Self.Items.IndexOf(Value[i]); //IndexOf is case insensitive
    if j_pos<0 then continue; //not found
    Self.Checked[j_pos] := true;
    Inc(j);
    if j_pos<j then continue; //already there or higher
   //else move to the next place from the top
    Self.Items.Move(j_pos, j-1);
  end;

  if j>0 then //at least one was found
    Self.ItemIndex := 0
  else
    Self.ItemIndex := -1;

 //The rest are not checked
  for j_pos := j to Self.Count-1 do
    Self.Checked[j_pos] := false;

  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TWakanCheckListBox.SetMultiselect(const Value: boolean);
var i, cnt: integer;
begin
  cnt := 0;
  for i := 0 to Self.Count-1 do begin
    Self.ItemEnabled[i] := Value;
    if Self.Checked[i] then
      Inc(cnt);
  end;

  FMultiSelect := Value;

 //For cnt==1 the selection stays the same... I think
  if (cnt>1) and Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);

 { We don't reset checked items here because there's no need to. In Multiselect
  we ignore Checked. Though if the user explicitly clicks any item now, we will
  reset Checked to it }
end;

procedure TWakanCheckListBox.Click;
begin
  inherited;
  if not Multiselect and ((ItemIndex<0) or not Checked[ItemIndex]) then begin
   //Uncheck all elements and check the focused one, or it'd be strange if
   //Multiselect is disabled afterwards.
    Self.CheckAll(cbUnchecked);
    if ItemIndex>=0 then
      Checked[ItemIndex] := true;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;

procedure TWakanCheckListBox.ClickCheck;
begin
  inherited;
  if Multiselect and Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TWakanCheckListBox.CNDrawItem(var Message: TWMDrawItem);
var LDrawItemStruct: PDrawItemStruct;
begin
  if csDestroying in ComponentState then exit;
  if Items.Count=0 then exit;

 //Prepare the rect which inherited CNDrawItem fucks up
  LDrawItemStruct := Message.DrawItemStruct;
  with LDrawItemStruct^ do
    if (not MultiSelect) and (not Header[itemID]) then
    begin
      if not UseRightToLeftAlignment then
        rcItem.Left := rcItem.Left - GetCheckWidth
      else
        rcItem.Right := rcItem.Right + GetCheckWidth;
    end;

  inherited;
end;

procedure TWakanCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Longint;
  Data: String;
begin
  if MultiSelect then begin
    inherited;
    exit;
  end;
 //Else no checkboxes; code copied from TCustomListBox because there's no fucking
 //way to call it.

  if Assigned(OnDrawItem) then OnDrawItem(Self, Index, Rect, State) else
  begin
    Canvas.FillRect(Rect);
    if Index < Count then
    begin
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      if not UseRightToLeftAlignment then
        Inc(Rect.Left, 2)
      else
        Dec(Rect.Right, 2);
      Data := '';
      if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
        Data := DoGetData(Index)
      else
        Data := Items[Index];
      DrawText(Canvas.Handle, Data, Length(Data), Rect, Flags);
    end;
  end;
end;

procedure TWakanCheckListBox.SetAutoDragItems(const Value: boolean);
begin
  FAutoDragItems := Value;
  if Value then
    Self.DragMode := dmAutomatic
  else
    Self.DragMode := dmManual;
end;

procedure TWakanCheckListBox.DragDrop(Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;
begin
  if not FAutoDragItems then begin
    inherited;
    exit;
  end;

  DropPoint.X := X;
  DropPoint.Y := Y;
  StartPosition := ItemAtPos(FDragStartPoint, True);
  DropPosition := ItemAtPos(DropPoint, True);
  Items.Move(StartPosition, DropPosition);
  OrderChanged;
end;

procedure TWakanCheckListBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if FAutoDragItems then
    Accept := Source = Self
  else
    inherited;
end;

procedure TWakanCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FAutoDragItems and (Button=mbLeft) then begin
    FDragStartPoint.X := X;
    FDragStartPoint.Y := Y;
  end;
  inherited;
end;

procedure TWakanCheckListBox.SetAutoCtrlMove(const Value: boolean);
begin
  FAutoCtrlMove := Value;
end;

procedure TWakanCheckListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FAutoCtrlMove then begin
    if (Key=VK_UP) and (ssCtrl in Shift) then begin
      if ItemIndex>0 then begin
        Items.Exchange(ItemIndex,ItemIndex-1);
        OrderChanged;
      end;
      Key:=0; //eat it anyway
    end;
    if (Key=VK_DOWN) and (ssCtrl in Shift) then begin
      if ItemIndex<Count-1 then begin
        Items.Exchange(ItemIndex,ItemIndex+1);
        OrderChanged;
      end;
      Key:=0; //eat it anyway
    end;
  end;
  inherited;
end;

//Item order has been changed. This means focus-change should be fired,
//and possibly selection-change.
procedure TWakanCheckListBox.OrderChanged;
begin
  Self.Click; //focus changed; also covers selection change in Single-Select
  if Multiselect then //have to do this manually
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TWakanCheckListBox]);
end;

end.
