unit JWBTranslate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXCtrls, ExtCtrls, Buttons;

type
  TfTranslate = class(TForm)
    Shape10: TShape;
    PaintBox6: TPaintBox;
    ListBox1: TListBox;
    ScrollBar1: TScrollBar;
    Bevel1: TBevel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    Bevel2: TBevel;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    Label1: TLabel;
    Bevel3: TBevel;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    Label2: TLabel;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    Button1: TButton;
    SpeedButton21: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox6Click(Sender: TObject);
    procedure PaintBox6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox6Paint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton18Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure PaintBox6MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox6DblClick(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SpeedButton20Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure ListBox1Enter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Exit(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
  private
    { Private declarations }
  public
    CopyShort, CutShort, PasteShort, AllShort:TShortCut;
    { Public declarations }
  end;

var
  fTranslate: TfTranslate;

implementation

uses JWBUser, JWBMenu, JWBHint, JWBKanjiDetails, JWBKanji;

{$R *.DFM}

procedure TfTranslate.Button2Click(Sender: TObject);
begin
  fUser.Translate_Button2Click(sender);
end;

procedure TfTranslate.Button3Click(Sender: TObject);
begin
  fUser.Translate_Button3Click(sender);
end;

procedure TfTranslate.Button4Click(Sender: TObject);
begin
  fUser.Translate_Button4Click(sender);
end;

procedure TfTranslate.Button5Click(Sender: TObject);
begin
  fUser.Translate_Button5Click(sender);

end;

procedure TfTranslate.Button10Click(Sender: TObject);
begin
  fUser.Translate_Button10Click(sender);

end;

procedure TfTranslate.Button6Click(Sender: TObject);
begin
  fUser.Translate_Button2Click(sender);
end;

procedure TfTranslate.Button7Click(Sender: TObject);
begin
  fUser.Translate_Button7Click(sender);
end;

procedure TfTranslate.Button8Click(Sender: TObject);
begin
  fUser.Translate_Button8Click(sender);
end;

procedure TfTranslate.Button9Click(Sender: TObject);
begin
  fUser.Translate_Button9Click(sender);
end;

procedure TfTranslate.CheckBox1Click(Sender: TObject);
begin
  fUser.Translate_CheckBox1Click(sender);
end;

procedure TfTranslate.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  fUser.Translate_ListBox1KeyDown(sender,key,shift);
end;

procedure TfTranslate.PaintBox6Click(Sender: TObject);
begin
  fUser.Translate_PaintBox6Click(sender);
end;

procedure TfTranslate.PaintBox6MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fUser.Translate_PaintBox6MouseDown(sender,button,shift,x,y);
end;

procedure TfTranslate.PaintBox6Paint(Sender: TObject);
begin
  fUser.MakeEditorBitmap;
end;

procedure TfTranslate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton8.Down:=false;
  fMenu.aDictEditor.Checked:=false;
end;

procedure TfTranslate.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  fUser.InsertCharacter(key);
end;

procedure TfTranslate.SpeedButton8Click(Sender: TObject);
begin
  fMenu.aEditorReading.Checked:=SpeedButton8.Down;
  fUser.Translate_CheckBox1Click(sender);
end;

procedure TfTranslate.SpeedButton9Click(Sender: TObject);
begin
  fMenu.aEditorMeaning.Checked:=SpeedButton9.Down;
  fUser.Translate_CheckBox1Click(sender);
end;

procedure TfTranslate.SpeedButton10Click(Sender: TObject);
begin
  fUser.Translate_Button6Click(sender);
end;

procedure TfTranslate.SpeedButton11Click(Sender: TObject);
begin
  fUser.Translate_Button7Click(sender);
end;

procedure TfTranslate.SpeedButton12Click(Sender: TObject);
begin
  fUser.Translate_Button8Click(sender);
end;

procedure TfTranslate.SpeedButton13Click(Sender: TObject);
begin
  fUser.Translate_Button9Click(sender);
end;

procedure TfTranslate.SpeedButton1Click(Sender: TObject);
begin
  fUser.Translate_Button3Click(sender);
end;

procedure TfTranslate.SpeedButton2Click(Sender: TObject);
begin
  fUser.Translate_Button5Click(sender);
end;

procedure TfTranslate.SpeedButton3Click(Sender: TObject);
begin
  fUser.Translate_Button2Click(sender);
end;

procedure TfTranslate.SpeedButton4Click(Sender: TObject);
begin
  fUser.BlockOp(true,true);
end;

procedure TfTranslate.FormResize(Sender: TObject);
begin
  fUser.linl.Clear;
  Invalidate;
end;

procedure TfTranslate.SpeedButton14Click(Sender: TObject);
begin
  fUser.BlockOp(true,false);
end;

procedure TfTranslate.SpeedButton15Click(Sender: TObject);
begin
  fUser.PasteOp;
end;

procedure TfTranslate.SpeedButton6Click(Sender: TObject);
begin
  SpeedButton6.Down:=true;
  fMenu.aEditorKanjiMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfTranslate.SpeedButton7Click(Sender: TObject);
begin
  SpeedButton7.Down:=true;
  fMenu.aEditorKanaMode.Checked:=true;
  fMenu.aEditorKanjiMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfTranslate.SpeedButton5Click(Sender: TObject);
begin
  SpeedButton5.Down:=true;
  fMenu.aEditorASCIIMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorKanjiMode.Checked:=false;
end;

procedure TfTranslate.FormActivate(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfTranslate.SpeedButton16Click(Sender: TObject);
begin
  fMenu.aEditorSmallFont.Checked:=true;
  fMenu.aEditorLargeFont.Checked:=false;
  fMenu.aEditorMedFont.Checked:=false;
  fUser.RefreshLines;
//  fUser.Translate_CheckBox1Click(sender);
end;

procedure TfTranslate.SpeedButton17Click(Sender: TObject);
begin
  fMenu.aEditorSmallFont.Checked:=false;
  fMenu.aEditorLargeFont.Checked:=true;
  fMenu.aEditorMedFont.Checked:=false;
  fUser.RefreshLines;
//  fUser.Translate_CheckBox1Click(sender);
end;

procedure TfTranslate.SpeedButton18Click(Sender: TObject);
begin
  fMenu.aEditorSmallFont.Checked:=false;
  fMenu.aEditorLargeFont.Checked:=false;
  fMenu.aEditorMedFont.Checked:=true;
  fUser.RefreshLines;
//  fUser.Translate_CheckBox1Click(sender);
end;

procedure TfTranslate.FormShow(Sender: TObject);
begin
//  fUser.Look(false);
  fUser.ShowText(true);
  fTranslate.ListBox1.ItemIndex:=0;
  ListBox1.SetFocus;
end;

procedure TfTranslate.FormHide(Sender: TObject);
begin
  if dictmodeset=0 then fUser.SpeedButton1.Down:=true;
  if dictmodeset=1 then fUser.SpeedButton2.Down:=true;
  if dictmodeset=2 then fUser.SpeedButton3.Down:=true;
//  fUser.Look(false);
end;

procedure TfTranslate.ScrollBar1Change(Sender: TObject);
begin
  fUser.ScrollBar1Change(Sender);
end;

procedure TfTranslate.FormDeactivate(Sender: TObject);
begin
  if fHint.Visible then fUser.HideHint;
end;

procedure TfTranslate.PaintBox6MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fUser.Translate_PaintBox6MouseMove(sender,shift,x,y);
  fMenu.IntTipPaintOver(PaintBox6,x,y,false);
end;

procedure TfTranslate.PaintBox6DblClick(Sender: TObject);
begin
  if not fKanjiDetails.Visible then fMenu.aKanjiDetailsExecute(nil);
end;

procedure TfTranslate.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  fUser.HandleWheel(false);
  handled:=true;
end;

procedure TfTranslate.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  fUser.HandleWheel(true);
  handled:=true;
end;

procedure TfTranslate.SpeedButton20Click(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfTranslate.SpeedButton19Click(Sender: TObject);
begin
  fMenu.TabControl1Change(sender);
end;

procedure TfTranslate.ListBox1Enter(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=CopyShort;
  fMenu.aEditorCut.ShortCut:=CutShort;
  fMenu.aEditorPaste.ShortCut:=PasteShort;
  fMenu.aEditorSelectAll.ShortCut:=AllShort;
end;

procedure TfTranslate.FormCreate(Sender: TObject);
begin
  CopyShort:=fMenu.aEditorCopy.ShortCut;
  CutShort:=fMenu.aEditorCut.ShortCut;
  PasteShort:=fMenu.aEditorPaste.ShortCut;
  AllShort:=fMenu.aEditorSelectAll.ShortCut;
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;
end;

procedure TfTranslate.ListBox1Exit(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;
end;

procedure TfTranslate.SpeedButton21Click(Sender: TObject);
begin
  fMenu.aEditorCOlors.Execute;
end;

end.
