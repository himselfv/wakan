unit JWBIntTip;
{ Universal highlight for most types of controls. Relies on JWBUnit/DrawReg
 which stores info about painted text.
 To enable support for a control:
 1. Paint text with JWBUnit/DrawUnicode.
 2. From event handlers call:
   IntTip.MouseMove
   IntTip.MouseDown
   IntTip.MouseUp
 Passes to JWBScreenTip to display actions panel for highlighted items.
}

interface
uses Types, Controls;

type
  THighlightHandler = class
  public
    function UpdateSelection(Control: TControl; DragStart, CursorPos:TPoint): string; virtual;
  end;

  CControl = class of TControl;
  CHighlightHandler = class of THighlightHandler;

  TIntTip = class
  protected
    FHighlightHandlers: array of record
      ControlClass: CControl;
      Handler: THighlightHandler;
    end;
    procedure UpdateSelection;
  public
   //Drag start control+point when drag-selecting, else nil.
    DragStartCtl: TControl;
    DragStartPt: TPoint;
   //Last selection-enabled control mouse was sighted over
    HoverCtl: TControl;
    HoverPt: TPoint;
   //Currently selected string/String under the mouse pointer right now
   //UpdateSelection changes this member
    StringUnderMouse: string;
    constructor Create;
    procedure MouseMove(c: TControl; x, y: integer; leftDown: boolean);
    procedure MouseDown(c: TControl; x, y: integer);
    procedure MouseUp;
    function IsDragging: boolean;
    procedure AbortDrag;
    procedure ResetHighlight;
    procedure RegisterHighlightHandler(AControlClass: CControl;
      AHandlerClass: CHighlightHandler);
  end;

var
  IntTip: TIntTip;

implementation
uses SysUtils, Grids, ExtCtrls, WakanPaintBox, JWBStrings, JWBUnit,
  JWBScreenTip, JWBWakanText, JWBEditor, JWBKanji, JWBRadical;

constructor TIntTip.Create;
begin
  inherited;
  DragStartCtl := nil;
  HoverCtl := nil;
end;

procedure TIntTip.RegisterHighlightHandler(AControlClass: CControl;
  AHandlerClass: CHighlightHandler);
var i: integer;
begin
  i := Length(FHighlightHandlers);
  SetLength(FHighlightHandlers, i+1);
  FHighlightHandlers[i].ControlClass := AControlClass;
  FHighlightHandlers[i].Handler := AHandlerClass.Create;
end;

{
IntTip*()
Various controls from all over the program call these on mouse events,
to support text selection and hint popups.
Minimal set you have to call from a control:
 IntTipControlOver(...) on mouse move
 IntTipMouseUp() on mouse up

Your control also has to be supported by UpdateSelection (see comments there).
}
procedure TIntTip.MouseMove(c:TControl;x,y:integer;leftDown:boolean);
begin
  if leftDown and (DragStartCtl<>c) then begin
    if DragStartCtl<>nil then AbortDrag; //at old control
    MouseDown(c,x,y); //auto-down
  end;
  if (not leftDown) and (DragStartCtl<>nil) then
   //auto-up: some controls send MouseOver(leftDown=false) first, and we'd lose
   //selected text if we just continued.
    MouseUp;
  HoverPt:=Point(x,y);
  HoverCtl:=c;
  UpdateSelection;
end;

procedure TIntTip.MouseDown(c:TControl;x,y:integer);
begin
 //Remember "drag start" point and control
  DragStartPt:=Point(x,y);
  DragStartCtl:=c;
end;

procedure TIntTip.MouseUp;
begin
 //Remove "drag start" point. Important - or we'll continue dragging
  DragStartCtl:=nil;
  ScreenTip.PopupImmediate(true);
end;

//Simply abort drag but show no popup
procedure TIntTip.AbortDrag;
begin
  DragStartCtl:=nil;
end;

procedure TIntTip.ResetHighlight;
begin
  AbortDrag;
  SetSelectionHighlight(0,0,0,0,nil);
end;

//True if we're in the process of highlighting a part of text
function TIntTip.IsDragging: boolean;
begin
  Result := DragStartCtl <> nil;
end;

//Updates text selection highlight and currently highlighted contents in intcurString
procedure TIntTip.UpdateSelection;
var s1: string;
  rx,ry: integer;
  wtt: TEvalCharType;
  gc: TGridCoord;
  rpos: TSourcePos;
  MouseControl: TControl; //control which receives the mouse events
  MousePos: TPoint; //mouse pos in that control coordinate system
begin
 //It is important that we route mouse events to the control which captured mouse,
 //else we'd get popup even when clicking in Text Editor, then dragging the mouse outside
  if DragStartCtl=nil then begin
   //mouse is free, hovering over intmo
    MouseControl:=HoverCtl;
    MousePos:=HoverPt;
  end else begin
   //mouse is captured by a different control
    MouseControl:=DragStartCtl;
    if DragStartCtl=HoverCtl then
      MousePos:=HoverPt
    else
      MousePos:=MouseControl.ScreenToClient(HoverCtl.ClientToScreen(HoverPt)); //convert to capture control coordinate system
  end;
 //Now MouseControl can either be DragStart control, MouseOver control or nil.

  if MouseControl=nil then begin
    s1 := '';
    SetSelectionHighlight(0,0,0,0,nil);
  end else

  if (fEditor<>nil) and (MouseControl=fEditor.EditorPaintBox) then
  begin
    rpos:=fEditor.TryGetExactLogicalPos(MousePos.x,MousePos.y);
    rx := rpos.x; ry := rpos.y;
    if (ry>=0) and (rx>=0) and (rx<=fEditor.doctr[ry].charcount) then
      s1:=fEditor.GetDocWord(rx,ry,wtt)
    else
      s1:='';
    SetSelectionHighlight(0,0,0,0,nil);
  end else

  if MouseControl is TPaintBox then
  begin
    if DragStartCtl<>nil then //dragging
      s1:=CanvasUpdateSelection(TPaintBox(MouseControl).Canvas,DragStartPt,MousePos)
    else //just hovering
      s1:=CanvasUpdateSelection(TPaintBox(MouseControl).Canvas,MousePos,MousePos);
  end else

 { TWakanPaintbox is mostly the same as TPaintBox with regard to custom painting,
  but it inherits from TCustomControl instead of TGraphicControl and has a different
  Canvas field }
  if MouseControl is TWakanPaintBox then
  begin
    if DragStartCtl<>nil then //dragging
      s1:=CanvasUpdateSelection(TWakanPaintBox(MouseControl).Canvas,DragStartPt,MousePos)
    else //just hovering
      s1:=CanvasUpdateSelection(TWakanPaintBox(MouseControl).Canvas,MousePos,MousePos);
  end else

 { Maybe I should have just taken the TGraphicControl/TCustomControl route instead of
  messing with descendants? And moved these clauses to the bottom, if nothing else
  works }

  if MouseControl is TCustomDrawGrid then
  begin
    gc:=TCustomDrawGrid(MouseControl).MouseCoord(MousePos.x,MousePos.y);
    if (fKanji<>nil) and (MouseControl=fKanji.DrawGrid1) then begin
      s1:=fKanji.GetKanji(gc.x,gc.y);
      SetSelectionHighlight(0,0,0,0,nil);
    end else
    if (fRadical<>nil) and (MouseControl=fRadical.DrawGrid) then begin
      s1:=fRadical.GetKanji(gc.x,gc.y);
      SetSelectionHighlight(0,0,0,0,nil);
    end else
    if DragStartCtl<>nil then //dragging
      s1:=DrawGridUpdateSelection(TCustomDrawGrid(MouseControl),DragStartPt,MousePos)
    else //just hovering
      s1:=DrawGridUpdateSelection(TCustomDrawGrid(MouseControl),MousePos,MousePos);
  end else

  begin
    s1 := ''; //invalid control in intmo
    SetSelectionHighlight(0,0,0,0,nil);
  end;

  StringUnderMouse:=s1;
end;


initialization
  IntTip := TIntTip.Create;

finalization
  FreeAndNil(IntTip);

end.
