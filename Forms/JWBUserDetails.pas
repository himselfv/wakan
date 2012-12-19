unit JWBUserDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, RXCtrls, ExtCtrls;

type
  TfUserDetails = class(TForm)
    Label5: TLabel;
    Shape2: TShape;
    PaintBox1: TPaintBox;
    Shape5: TShape;
    Label6: TLabel;
    PaintBox6: TPaintBox;
    RxLabel3: TRxLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    btnMoveUpInCategory: TSpeedButton;
    btnMoveDownInCategory: TSpeedButton;
    btnDelete: TButton;
    Edit4: TEdit;
    Button3: TButton;
    GroupBox2: TGroupBox;
    btnSetProblematic: TButton;
    btnSetUnlearned: TButton;
    btnSetLearned: TButton;
    btnSetMastered: TButton;
    GroupBox3: TGroupBox;
    ComboBox2: TComboBox;
    Button4: TButton;
    ListBox2: TListBox;
    Button13: TButton;
    Bevel1: TBevel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox6Paint(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnSetProblematicClick(Sender: TObject);
    procedure btnSetUnlearnedClick(Sender: TObject);
    procedure btnSetLearnedClick(Sender: TObject);
    procedure btnSetMasteredClick(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpInCategoryClick(Sender: TObject);
    procedure btnMoveDownInCategoryClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBox6MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  public
    procedure SetWordControlsEnabled(Value: boolean);

  end;

var
  fUserDetails: TfUserDetails;

implementation

uses JWBWords, JWBMenu;

{$R *.DFM}

procedure TfUserDetails.PaintBox1Paint(Sender: TObject);
begin
  fWords.UserDetails_PaintBox1Paint(sender);
end;

procedure TfUserDetails.PaintBox6Paint(Sender: TObject);
begin
  fWords.UserDetails_PaintBox6Paint(sender);
end;

procedure TfUserDetails.Button4Click(Sender: TObject);
begin
  fWords.AddWordsToCategory(ComboBox2.text);
end;

procedure TfUserDetails.btnSetProblematicClick(Sender: TObject);
begin
  fWords.SetWordsLearnState(0);
end;

procedure TfUserDetails.btnSetUnlearnedClick(Sender: TObject);
begin
  fWords.SetWordsLearnState(1);
end;

procedure TfUserDetails.btnSetLearnedClick(Sender: TObject);
begin
  fWords.SetWordsLearnState(2);
end;

procedure TfUserDetails.btnSetMasteredClick(Sender: TObject);
begin
  fWords.SetWordsLearnState(3);
end;

procedure TfUserDetails.Button13Click(Sender: TObject);
begin
  fWords.UserDetails_Button13Click(sender);
end;

procedure TfUserDetails.btnDeleteClick(Sender: TObject);
begin
  fWords.DeleteWords();
end;

procedure TfUserDetails.btnMoveUpInCategoryClick(Sender: TObject);
begin
  fWords.MoveWordsInCategory(mdUp);
end;

procedure TfUserDetails.btnMoveDownInCategoryClick(Sender: TObject);
begin
  fWords.MoveWordsInCategory(mdDown);
end;

procedure TfUserDetails.Button3Click(Sender: TObject);
begin
  fWords.UserDetails_Button3Click(sender);
end;

procedure TfUserDetails.ComboBox2Change(Sender: TObject);
begin
  fWords.UserDetails_ComboBox2Change(sender);
end;

procedure TfUserDetails.Edit4Change(Sender: TObject);
begin
  fWords.UserDetails_Edit4Change(sender);
end;

procedure TfUserDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fWords.SpeedButton4.Down:=false;
  fMenu.aUserDetails.Checked:=false;
end;

procedure TfUserDetails.PaintBox6MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox6,x,y,ssLeft in Shift);
end;

procedure TfUserDetails.SetWordControlsEnabled(Value: boolean);
begin
  fUserDetails.btnDelete.Enabled:=Value;
  fUserDetails.btnSetProblematic.Enabled:=Value;
  fUserDetails.btnSetUnlearned.Enabled:=Value;
  fUserDetails.btnSetLearned.Enabled:=Value;
  fUserDetails.btnSetMastered.Enabled:=Value;
end;

end.
