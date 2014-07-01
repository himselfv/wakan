unit JWBIntTip;
{ Universal selection highlight for most types of controls. Relies on JWBUnit/
DrawReg which stores info about painted text.

To enable support for a control:
 1. Paint text with JWBUnit/DrawUnicode.
 2. From event handlers call:
   IntTip.MouseMove
   IntTip.MouseDown
   IntTip.MouseUp

If the control uses standard DrawUnicode routines, that's enough. Otherwise,
and for specialized cases, register a custom highlight handler:
  IntTip.RegisterHighlightHandler(TControlClass, HighlightProc);

To handle what happens with the selected text, set OnMouseUp.
}

interface
uses Types, Classes, Controls, Graphics, Grids, JWBStrings;

type
  THighlightContentProc = reference to function(Control: TControl;
    DragStart, MousePos: TPoint): string;
 { Controls can provide their own HighlightContent() for enhanced handling, but
  must use SelectionHighlight tooling below.
  If DragStart==MousePos then no drag takes places, just get text under mouse. }

  CControl = class of TControl;

  TIntTip = class
  protected
    FHighlightHandlers: array of record
      ControlClass: CControl;
      Handler: THighlightContentProc;
    end;
    FOnMouseUp: TNotifyEvent;
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
      AHandler: THighlightContentProc);
    property OnMouseUp: TNotifyEvent read FOnMouseUp write FOnMouseUp;
  end;

var
  IntTip: TIntTip;

{ Selection highlight -- used for controls which don't have their own highlight
 method.
 There can be only one span of highlight across the program }
procedure SetSelectionHighlight(x1,y1,x2,y2:integer;canvas:TCanvas);
procedure ClearSelectionHighlight;
procedure PaintSelectionHighlight(canv: TCanvas=nil; in_rect: PRect=nil);

implementation
uses SysUtils, Windows, ExtCtrls, WakanPaintBox, JWBUnit;

constructor TIntTip.Create;
begin
  inherited;
  DragStartCtl := nil;
  HoverCtl := nil;
end;

procedure TIntTip.RegisterHighlightHandler(AControlClass: CControl;
  AHandler: THighlightContentProc);
var i: integer;
begin
  i := Length(FHighlightHandlers);
  SetLength(FHighlightHandlers, i+1);
  FHighlightHandlers[i].ControlClass := AControlClass;
  FHighlightHandlers[i].Handler := AHandler;
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
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self);
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
  MouseControl: TControl; //control which receives the mouse events
  MousePos: TPoint; //mouse pos in that control coordinate system
  ADragStartPos: TPoint;
  CtlClass: TClass;
  i: integer;
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
  end else begin
    s1 := '';
    if DragStartCtl<>nil then //dragging
      ADragStartPos := DragStartPt
    else //just hovering
      ADragStartPos := MousePos;

   //Try highlight handlers, first for this class then for parent class and so on
    CtlClass := MouseControl.ClassType;
    while (s1='') and (CtlClass<>nil) do begin
      for i := 0 to Length(FHighlightHandlers)-1 do
        if FHighlightHandlers[i].ControlClass=CtlClass then begin
          s1 := FHighlightHandlers[i].Handler(MouseControl, ADragStartPos, MousePos);
          if s1<>'' then break;
        end;
      if s1='' then
        CtlClass := CtlClass.ClassParent;
    end;

    if s1='' then //unsupported control in intmo
      SetSelectionHighlight(0,0,0,0,nil);
  end;
  StringUnderMouse := s1;
end;


{ Text selection highlight.
 Remembers one block of pixels to be highlighted, in any control.
 Used by DrawUnicode-powered controls for their custom selection mechanics. }
var
  STB_canvas:TCanvas;
  STB:TRect; //ScreenTipBlock

{ Clears old one, and sets and paints new selection highlight. }
procedure SetSelectionHighlight(x1,y1,x2,y2:integer;canvas:TCanvas);
begin
  //No flicker please
  if (STB_canvas=canvas) and (STB.Left=x1) and (STB.Right=x2) and (STB.Top=y1)
    and (STB.Bottom=y2) then exit;
  PaintSelectionHighlight;
  STB := Rect(x1,y1,x2,y2);
  STB_canvas:=canvas;
  PaintSelectionHighlight;
end;

procedure ClearSelectionHighlight;
begin
  SetSelectionHighlight(0,0,0,0,nil);
end;

//Clips the rectangle to make it fit in in_rect
function ClipRect(const rgn: TRect; in_rect: PRect): TRect;
begin
  Result := rgn;
 //For top/left prefer left/top...
  if Result.Left>in_rect.Right then Result.Left:=in_rect.Right;
  if Result.Left<in_rect.Left then Result.Left:=in_rect.Left;
  if Result.Top>in_rect.Bottom then Result.Top:=in_rect.Bottom;
  if Result.Top<in_rect.Top then Result.Top:=in_rect.Top;
 //For right/bottom prefer right/bottom. This makes the region negative if
 //in_rect is negative (this is desired)
  if Result.Right>in_rect.Right then Result.Right:=in_rect.Right;
  if Result.Right<in_rect.Left then Result.Right:=in_rect.Left;
  if Result.Bottom>in_rect.Bottom then Result.Bottom:=in_rect.Bottom;
  if Result.Bottom<in_rect.Top then Result.Bottom:=in_rect.Top;
end;

{ Re-applies currently active selection highlight where it has to be applied.
Pass valid Canvas and Rect so we do proper clipping, otherwise we'll flip
highlight even on unrelated repaints (=> highlight state broken).
Only pass wildcard when the highlight has to be redrawn outside of repaint handlers
(such as when it changes). }
procedure PaintSelectionHighlight(canv: TCanvas; in_rect: PRect);
var oldR2:integer;
  rgn: TRect;
begin
  if STB_Canvas=nil then exit;
  if canv<>nil then if canv<>STB_canvas then exit;
{  if in_rect=nil then
    Log('PaintSelectionHighlight: nil',[])
  else
    Log('PaintSelectionHighlight: %d,%d -- %d,%d',[in_rect.Left, in_rect.Top, in_rect.Right, in_rect.Bottom]); }
  oldR2:=SetROP2(STB_Canvas.Handle,R2_NOT);

  if in_rect<>nil then
   { We don't use Windows HREGIONS because they're slower and were causing weird
    focus drawing problems with TStringGrid before. Better just do it this way }
    rgn := ClipRect(STB,in_rect)
  else
    rgn := STB;

  STB_Canvas.Rectangle(rgn.Left,rgn.Top,rgn.Right,rgn.Bottom);
  SetROP2(STB_Canvas.Handle,oldR2);
end;


{ DrawUnicode-powered text selection.
 See comments to AddDrawReg/FindDrawReg, also see TfMenu.UpdateSelection.
 Uses SelectionHighlight for highlighting blocks of pixels. }

{ Updates ScreenTipBox and returns the substring of one of the strings drawn
 with DrawUnicode, currently selected in PaintBox according to DragStart->CursorPos.
p: PaintBox which currently receives mouse events (the one mouse is over,
  or the one which captures it because of dragging)
If DragStart equals CursorPos, assumes no selection. }
function CanvasUpdateSelection(Canvas:TCanvas;DragStart,CursorPos:TPoint):FString;
var id1,id2:integer;
  x1,x2,y1,fs,fs2,x_tmp:integer;
  s2,s_tmp:string;
  fontface,fontface2:string;
begin
  if (DragStart.X=CursorPos.X) and (DragStart.Y=CursorPos.Y) then begin
   //No drag, mouse-over. Get text without char rounding (first half char also gives us this char)
    Result:=FindDrawReg(Canvas,CursorPos.x,CursorPos.y,[],id1,x1,y1,fontface,fs);
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  Result:=FindDrawReg(Canvas,CursorPos.x,CursorPos.y,[ffHalfCharRounding],id1,x1,y1,fontface,fs);
  s2:=FindDrawReg(Canvas,DragStart.x,DragStart.y,[ffHalfCharRounding],id2,x2,y1,fontface2,fs2);
  if id2<0 then begin //drag from dead point => no selection
    Result := '';
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  if id1<>id2 then begin //mouse over different control/line
   //Try again, with Y set to that of DragStart
    CursorPos.Y := DragStart.Y;
    Result:=FindDrawReg(Canvas,CursorPos.X,CursorPos.Y,[ffHalfCharRounding],id1,x1,y1,fontface,fs);
    if id1<>id2 then begin
     //Just set the endpoint to the start or the end of the capturing line
      if CursorPos.X>DragStart.X then begin
        Result:=s2;
        SetSelectionHighlight(x2,y1,x2+CalcStrWidth(Canvas,fontface2,fs2,s2),y1+fs2,Canvas);
        exit;
      end else begin
        Result:=GetDrawReg(id2,x1,y1,fontface,fs); //get whole line
        //and continue with normal handling
      end;
    end;
  end;

  if length(s2)>length(Result) then begin
   //Swap s1 and s2
    s_tmp:=s2; s2:=Result; Result:=s_tmp;
    x_tmp:=x2; x2:=x1; x1:=x_tmp;
  end;
  Result:=copy(Result,1,length(Result)-length(s2));
  SetSelectionHighlight(x1,y1,x2,y1+fs,Canvas);
end;

{ Some common highlighters for when more specific ones are unavailable. Rely
 on JWBUnit\FindWordReg }

{ Maybe I should have just taken the TGraphicControl/TCustomControl route instead of
 messing with descendants? And moved these clauses to the bottom, if nothing else
 works }
function PaintboxHighlightContent(Control: TControl; DragStart, MousePos: TPoint): string;
begin
  Result := CanvasUpdateSelection(TPaintBox(Control).Canvas,DragStart,MousePos);
end;

{ TWakanPaintbox is mostly the same as TPaintBox with regard to custom painting,
 but it inherits from TCustomControl instead of TGraphicControl and has a different
 Canvas field }
function WakanPaintboxHighlightContent(Control: TControl; DragStart, MousePos: TPoint): string;
begin
  Result := CanvasUpdateSelection(TWakanPaintBox(Control).Canvas,DragStart,MousePos);
end;

initialization
  IntTip := TIntTip.Create;
  IntTip.RegisterHighlightHandler(TPaintbox, PaintboxHighlightContent);
  IntTip.RegisterHighlightHandler(TWakanPaintbox, WakanPaintboxHighlightContent);
  STB_Canvas:=nil;

finalization
  FreeAndNil(IntTip);

end.
