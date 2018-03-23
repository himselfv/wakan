unit JWBEditorHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, WakanPaintbox;

{ Shows results from JWBDict.fDict dictionary lookup in another form,
 suited for giving the user hints as they type. }
{ Ought to be non-activable }

type
  TfEditorHint = class(TForm)
    PaintBox1: TWakanPaintbox;
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  public
    procedure ShowHint(pos: TPoint);
    procedure Hide; reintroduce;
  end;

implementation
uses JWBStrings, JWBUnit, JWBWordLookup, JWBSettings, Grids, JWBLegacyMarkup;

{$R *.DFM}

procedure TfEditorHint.CreateParams(var Params: TCreateParams);
begin
  inherited;
  //Do not activate by mouse click
  Params.ExStyle := Params.ExStyle + WS_EX_NOACTIVATE;
end;

procedure TfEditorHint.WMMouseActivate(var Message: TWMMouseActivate);
begin
  //Again, do not activate by mouse click
  Message.Result := MA_NOACTIVATE;
end;

{ If configured to, shows hint at the position determined by APos.
 To hide it, simply call Hide(). }
procedure TfEditorHint.ShowHint(pos: TPoint);
begin
  if not fSettings.cbShowEditorHint.Checked then begin
    Hide();
    exit;
  end;

  if pos.X+Self.Width>Screen.Width then pos.X:=Screen.Width-Self.Width;

  Self.Left:=pos.X;
  Self.Top:=pos.Y;

  if fSettings.cbHintMeaning.Checked then Self.Height:=44 else Self.Height:=22;

  //We do not want to be activated when shown. Do not call Show() use a trick:
  //ShowWindow(NOACTIVATE) + Visible
  ShowWindow(Self.Handle, SW_SHOWNOACTIVATE);
  Self.Visible := True;
  Self.Invalidate;
  PaintBox1.Invalidate; //because the form can be double buffered and consider buffer valid on redraw
end;

procedure TfEditorHint.Hide;
begin
  if Visible then inherited Hide;
end;

procedure TfEditorHint.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var StringGrid1: TStringGrid;
  kanjis:FString;
  i:integer;
  cw,cwl:integer;
  curk:string;
  fs,fsl:integer;
  rect:TRect;
begin
  StringGrid1 := fWordLookup.StringGrid; //faster access

  PaintBox1.Canvas.Brush.Color:=Col('Editor_HintBack');
  cw:=-1;
  cwl:=0;
  kanjis:='';
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    if kanjis<>'' then kanjis:=kanjis+UH_IDG_SPACE;
    curk:=remexcl(copy(StringGrid1.Cells[1,i],2,length(StringGrid1.Cells[1,i])-1));
    if StringGrid1.Row=i then
    begin
      cw:=flength(kanjis);
      cwl:=flength(curk);
    end;
    kanjis:=kanjis+curk;
  end;

  fs:=18;
  fsl:=PaintBox1.Width div fs;
  while flength(kanjis)>fsl do
  begin
    if cw>1 then
    begin
      while fcopy(kanjis,1,1)<>UH_IDG_SPACE do
      begin
        fdelete(kanjis,1,1);
        dec(cw,1);
      end;
      fdelete(kanjis,1,1);
      kanjis:=UH_ELLIPSIS+kanjis;
    end else
    begin
      while fcopy(kanjis,flength(kanjis)-1,1)<>UH_IDG_SPACE do fdelete(kanjis,flength(kanjis)-1,1);
      fdelete(kanjis,flength(kanjis)-1,1);
      kanjis:=kanjis+UH_ELLIPSIS;
    end;
  end;

//  PaintBox1.Canvas.Font.Style:=[];
  PaintBox1.Canvas.Font.Color:=Col('Editor_HintText');
  DrawUnicode(PaintBox1.Canvas,2,2,fs,fcopy(kanjis,1,cw),FontJapaneseGrid);
//  PaintBox1.Canvas.Font.Style:=[fsBold];
  PaintBox1.Canvas.Brush.Color:=Col('Editor_HintSelected');
  rect.Left:=2+cw*fs;
  rect.Top:=2;
  rect.Bottom:=fs+2;
  rect.Right:=2+cw*fs+cwl*fs;
  PaintBox1.Canvas.FillRect(rect);
  DrawUnicode(PaintBox1.Canvas,2+cw*fs,2,fs,fcopy(kanjis,cw+1,cwl),FontJapaneseGrid);
//  PaintBox1.Canvas.Font.Style:=[];
  PaintBox1.Canvas.Brush.Color:=Col('Editor_HintBack');
  DrawUnicode(PaintBox1.Canvas,2+cw*fs+cwl*fs,2,fs,fcopy(kanjis,cw+cwl+1,flength(kanjis)-cwl-cw),FontJapaneseGrid);
  if fSettings.cbHintMeaning.Checked then
  begin
    PaintBox1.Canvas.Font.Name:=FontEnglish;
    PaintBox1.Canvas.Font.Height:=fs-4;
    rect.top:=fs+2;
    rect.left:=2;
    rect.right:=PaintBox1.Width-4;
    rect.bottom:=fs*2;
    PaintBox1.Canvas.TextRect(rect,2,fs+2,remmark(remexcl(StringGrid1.Cells[2,fWordLookup.curword])));
  end;
end;


end.
