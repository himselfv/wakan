unit BalancedTree;
{
  Taken from Nicklaus Wirth :
    Algorithmen und Datenstrukturen ( in Pascal )
    Balanced Binary Trees p 250 ++

  Fixed By Giacomo Policicchio
  pgiacomo@tiscalinet.it
  19/05/2000

  Modifications by Wakan project,
  http://code.google.com/p/wakan/
}

interface
uses Classes;

type
  TBinTreeItem = class(TObject)
  private
    count:integer;
    left,right:TBinTreeItem;
    bal:-1..1;
  public
    constructor Create;
    // a < self :-1  a=self :0  a > self :+1
    function CompareData(const a):Integer; virtual; abstract;
    function Compare(a:TBinTreeItem):Integer; virtual; abstract;
    procedure Copy(ToA:TBinTreeItem); virtual; abstract; // data
  end;

  TBinTreeEnumerator = record
    FStack: array of TBinTreeItem;
    FStackUsed: integer;
    FBOF: boolean;
    procedure ResetEmpty; overload;
    procedure Reset(const ARoot: TBinTreeItem); overload;
    procedure Push(const AItem: TBinTreeItem);
    function Pop: TBinTreeItem;
    function GetCurrent: TBinTreeItem;
    function MoveNext: boolean;
    property Current: TBinTreeItem read GetCurrent;
  end;

  TBinTree=class(TPersistent)
  private
    root:TBinTreeItem;
    procedure Delete(item:TBinTreeItem;var p:TBinTreeItem;var h:boolean;var ok:boolean);
    function SearchAndInsert(item:TBinTreeItem;var p:TBinTreeItem;var h:boolean): TBinTreeItem;
    procedure balanceLeft(var p:TBinTreeItem;var h:boolean;dl:boolean);
    procedure balanceRight(var p:TBinTreeItem;var h:boolean;dl:boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(item:TBinTreeItem):boolean;
    function AddOrSearch(item:TBinTreeItem):TBinTreeItem;
    function Remove(item:TBinTreeItem):boolean;
    function Search(item:TBinTreeItem):boolean;
    function SearchItem(const item:TBinTreeItem):TBinTreeItem;
    function SearchData(const data):TBinTreeItem;
    function GetEnumerator: TBinTreeEnumerator;
  end;


implementation

constructor TBinTreeItem.Create;
begin
  inherited create;
  count:=0;
end;

procedure TBinTreeEnumerator.ResetEmpty;
begin
  FStackUsed := 0;
  FBOF := true;
end;

procedure TBinTreeEnumerator.Reset(const ARoot: TBinTreeItem);
begin
  if ARoot=nil then begin
    ResetEmpty;
    exit;
  end;
  if Length(FStack)<1 then
    SetLength(FStack, 4);
  FStack[0] := ARoot;
  FStackUsed := 1;
  FBOF := true;
end;

procedure TBinTreeEnumerator.Push(const AItem: TBinTreeItem);
begin
  if FStackUsed>=Length(FStack) then
    SetLength(FStack, Length(FStack)*2+1);
  FStack[FStackUsed] := AItem;
  Inc(FStackUsed);
end;

function TBinTreeEnumerator.Pop: TBinTreeItem;
begin
  if FStackUsed<=0 then begin
    Result := nil;
    exit;
  end;
  Dec(FStackUsed);
  Result := FStack[FStackUsed];
end;

function TBinTreeEnumerator.GetCurrent: TBinTreeItem;
begin
  if FStackUsed<=0 then
    Result := nil
  else
    Result := FStack[FStackUsed-1];
end;

function TBinTreeEnumerator.MoveNext: boolean;
var item: TBinTreeItem;
begin
  if FStackUsed=0 then //no entries left
    Result := false
  else
  if FBOF then begin
   //Move to the leftmost node or remain on this one
    while Current.left<>nil do
      Push(Current.left);
    FBOF := false;
    Result := true;
  end else
  if Current.right<>nil then begin
    Push(Current.right);
    while Current.left<>nil do
      Push(Current.left);
    Result := true;
  end else
  begin
    repeat
      item := Pop();
    until (Current=nil) or (item=Current.left); //search for the first parent where we were on the left subtree
    if Current=nil then
      Result := false
    else
      Result := true;
  end;
end;

constructor TBinTree.Create;
begin
  inherited create;
  root:=nil;
end;

destructor TBinTree.Destroy;
begin
  Clear;
  inherited destroy;
end;

procedure TBinTree.Clear;
begin
 //Pretty stupid for a Clear(), we can do better.
  while root <> nil do remove(root);
end;

//Tries to locate identical item in the tree, or inserts the given item.
//Returns the item located or the item inserted => if (Result==item) then Added else Found.
function TBinTree.SearchAndInsert(item:TBinTreeItem;var p:TBinTreeItem;var h:boolean): TBinTreeItem;
var cmp: integer;
begin
  if p=nil then begin // word not in tree, insert it
    p:=item;
    p.count:=1;
    p.left:=nil;
    p.right:=nil;
    p.bal:=0;
    if root=nil then root:=p;
    Result:=p;
    h:=true;
    exit;
  end;

  cmp := item.compare(p);
  if cmp > 0 then      // new < current
  begin
    Result := searchAndInsert(item,p.left,h);
    if h and (Result=item) then BalanceLeft(p,h,false);
  end
  else
  if cmp < 0 then     // new > current
  begin
    Result := searchAndInsert(item,p.right,h);
    if h and (Result=item) then balanceRight(p,h,false);
  end
  else
  begin
    p.count:=p.count+1;
    h:=false;
    Result:=p;
  end;
end;      //searchAndInsert

// returns a pointer to the equal item if found, nil otherwise
function TBinTree.SearchItem(const item:TBinTreeItem):TBinTreeItem;
var i: integer;
begin
  Result := root;
  while Result<>nil do begin
    i := Result.Compare(item);
    if i=0 then
      exit
    else
    if i>0 then //item > Result
      Result := Result.right
    else //item < Result
      Result := Result.left;
  end;
end;

//Same but by Data (your BinTreeItems must support CompareData)
function TBinTree.SearchData(const data):TBinTreeItem;
var i: integer;
begin
  Result := root;
  while Result<>nil do begin
    i := Result.CompareData(data);
    if i=0 then
      exit
    else
    if i>0 then //item > Result
      Result := Result.right
    else //item < Result
      Result := Result.left;
  end;
end;

function TBinTree.GetEnumerator: TBinTreeEnumerator;
begin
  Result.Reset(root);
end;


procedure TBinTree.balanceRight(var p:TBinTreeItem;var h:boolean;Dl:boolean);
var p1,p2:TBinTreeItem;
Begin
  case p.bal of
      -1:begin
          p.bal:=0;
          if not dl then h:=false;
         end;
      0: begin
          p.bal:=+1;
          if dl then h:=false;
         end;
      +1:begin    // new balancing
          p1:=p.right;
          if (p1.bal=+1) or ((p1.bal=0) and dl) then begin  // single rr rotation
            p.right:=p1.left; p1.left:=p;
            if not dl then p.bal:=0
                      else begin
                            if p1.bal=0 then begin
                              p.bal:=+1; p1.bal:=-1; h:=false;
                             end
                            else begin
                              p.bal:=0;  p1.bal:=0;
                              (* h:=false; *)
                             end;
                           end;
            p:=p1;
           end
          else begin  // double rl rotation
            p2:=p1.left;
            p1.left:=p2.right;
            p2.right:=p1;
            p.right:=p2.left;
            p2.left:=p;
            if p2.bal=+1 then p.bal:=-1 else p.bal:=0;
            if p2.bal=-1 then p1.bal:=+1 else p1.bal:=0;
            p:=p2;
            if dl then p2.bal:=0;
           end;
          if not dl then begin
            p.bal:=0;
            h:=false;
           end;
         end;
     end; // case
End;

procedure TBinTree.balanceLeft(var p:TBinTreeItem;var h:boolean;dl:boolean);
var p1,p2:TBinTreeItem;
Begin
    case p.bal of
     1:begin
        p.bal:=0;
        if not dl then h:=false;
       end;
     0:begin
        p.bal:=-1;
        if dl then  h:=false;
       end;
     -1:(* if (p.Left<>nil) or not dl then *)
        begin   // new balancing
         p1:=p.left;
         if (p1.bal=-1) or ((p1.bal=0) and dl) then begin   // single ll rotation
           p.left:=p1.right;p1.right:=p;
           if not dl then p.bal:=0
                     else begin
                           if p1.bal=0 then begin
                             p.bal:=-1;
                             p1.bal:=+1;
                             h:=false;
                            end
                           else begin
                             p.bal:=0;
                             p1.bal:=0;
                             (* h:=false; *)
                            end;
                          end;
           p:=p1;
          end
         else
          begin //double lr rotation
            p2:=p1.right;
            P1.Right:=p2.left;
            p2.left:=p1;
            p.left:=p2.right;
            p2.right:=p;
           if p2.bal=-1 then  p.bal:=+1 else p.bal:=0;
           if p2.bal=+1 then  p1.bal:=-1 else p1.bal:=0;
           p:=p2;if dl then p2.bal:=0;
          end;
           if not dl then begin
             p.bal:=0;
             h:=false;
            end;
        end; { -1 }
    end; { case }
End;


procedure TBinTree.Delete(item:TBinTreeItem;var p:TBinTreeItem;var h:boolean;var ok:boolean);
var q:TBinTreeItem; //h=false;

 procedure del(var r:TBinTreeItem;var h:boolean);
 begin //h=false
  if r.right <> nil then
   begin
    del(r.right,h);
    if h then balanceLeft(r,h,True);
   end
  else
   begin
    r.copy(q);  { q.key:=r.key; }
    q.count:=r.count;
    q:=r;
    r:=r.left;h:=true;
   end;
 end;

begin { main of delete }
 ok:=true;
 if (p=nil) then
  begin
   Ok:=false;h:=false;
  end
 else
  if (item.compare(p) > 0){(x < p^.key)} then
   begin
    delete(item,p.left,h,ok);
    if h then balanceRight(p,h,True);
   end
  else
   if (item.compare(p) < 0){(x > p^.key)}then
    begin
     delete(item,p.right,h,ok);
     if h then balanceLeft(p,h,True);
    end
   else
    begin // remove q
     q:=p;
     if q.right=nil then
      begin
       p:=q.left;h:=true;
      end
     else
      if (q.left=nil) then
       begin
        p:=q.right;h:=true;
       end
      else
       begin
        del(q.left,h);
        if h then balanceRight(p,h,True);
       end;
      q.free; {dispose(q)};
    end;
end; { delete }

//Returns true if the item was added, false if an identical one existed.
function TBinTree.Add(item:TBinTreeItem):boolean;
var h:boolean;
begin
  Result:=SearchAndInsert(item,root,h)=item;
end;

//Same, but returns the item found or added
function TBinTree.AddOrSearch(item:TBinTreeItem):TBinTreeItem;
var h:boolean;
begin
  Result:=SearchAndInsert(item,root,h);
end;

function TBinTree.Remove(item:TBinTreeItem):Boolean;
var h,ok:boolean;
begin
  Delete(item,root,h,ok);
  Result:=ok;
end;

function TBinTree.Search(item:TBinTreeItem):Boolean;
begin
  Result:=SearchItem(item)<>nil;
end;

end.
