unit UserDetails;

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
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Button2: TButton;
    Edit4: TEdit;
    Button3: TButton;
    GroupBox2: TGroupBox;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    GroupBox3: TGroupBox;
    ComboBox2: TComboBox;
    Button4: TButton;
    ListBox2: TListBox;
    Button13: TButton;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox6Paint(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserDetails: TfUserDetails;

implementation

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
  fWords.UserDetails_Button4Click(sender);
end;

procedure TfUserDetails.Button5Click(Sender: TObject);
begin
  fWords.UserDetails_Button5Click(sender);
end;

procedure TfUserDetails.Button6Click(Sender: TObject);
begin
  fWords.UserDetails_Button6Click(sender);
end;

procedure TfUserDetails.Button7Click(Sender: TObject);
begin
  fWords.UserDetails_Button7Click(sender);
end;

procedure TfUserDetails.Button8Click(Sender: TObject);
begin
  fWords.UserDetails_Button8Click(sender);
end;

procedure TfUserDetails.Button13Click(Sender: TObject);
begin
  fWords.UserDetails_Button13Click(sender);
end;

procedure TfUserDetails.Button2Click(Sender: TObject);
begin
  fWords.UserDetails_Button2Click(sender);
end;

procedure TfUserDetails.SpeedButton4Click(Sender: TObject);
begin
  fWords.UserDetails_SpeedButton4Click(sender);
end;

procedure TfUserDetails.SpeedButton5Click(Sender: TObject);
begin
  fWords.UserDetails_SpeedButton5Click(sender);
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

end.
