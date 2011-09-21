
{  ---------------------------------------------------------
   GradientFill Editor, written by William Yang.
   yang@btinternet.com    http://www.btinternet.com/~yang/
   ---------------------------------------------------------  }
   
unit GradEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  	Gradient, StdCtrls, ExtCtrls, ComCtrls, Buttons, IniFiles, StrMan;
type
  TfrmGradEditor = class(TForm)
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    Color1: TEdit;
    Color2: TEdit;
    Color3: TEdit;
    Color4: TEdit;
    Color5: TEdit;
    ColorDlg: TColorDialog;
    Label1: TLabel;
    Label2: TLabel;
    txtLinesize: TEdit;
    UpDown1: TUpDown;
    Label3: TLabel;
    txtColors: TEdit;
    UpDown2: TUpDown;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    diCombo: TComboBox;
    Label4: TLabel;
    lstGallary: TListBox;
    Label5: TLabel;
    btnUpdate: TButton;
    btnAdd: TButton;
    Button3: TButton;
    procedure Color1DblClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstGallaryDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnAddClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure txtLinesizeChange(Sender: TObject);
    procedure diComboChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure lstGallaryDblClick(Sender: TObject);
  private
    { Private declarations }
    fGradient: TGradient;
    function GetGradient: TGradient;
    procedure SetGradient(Grad: TGradient);
    procedure LoadIni;
    procedure SaveIni;
  public
    { Public declarations }
    function Execute(Grad: TGradient): Boolean;
  end;

function EditGradient(Grad: TGradient): Boolean;

implementation

{$R *.DFM}
const
	IntDi : array[TDirection] of Integer = (0, 1);
	DireInt : array[0..1] of TDirection = (diHorizonal, diVertical);
	StyleToInt : array[TGradientStyle] of Integer = (0,1,2,3);
	IntToStyle : array[0..3] of TGradientStyle=(gsLine, gsEllipse, gsRect, gsCustomRect);

function EditGradient(Grad: TGradient): Boolean;
begin
	with TfrmGradEditor.Create(Application) do
  begin
  	try
    	Result := Execute(Grad);
  	finally
    	Free;
    end;
  end;
end;

procedure TfrmGradEditor.LoadIni;
	procedure AddGallaryItem(Name: String; P: TStringList);
  var
  	Gradient: TGradient;
  begin
    Gradient := TGradient.Create;
    Gradient.Color1 := StrToInt(P[0]);
    Gradient.Color2 := StrToInt(P[1]);
    Gradient.Color3 := StrToInt(P[2]);
    Gradient.Color4 := StrToInt(P[3]);
    Gradient.Color5 := StrToInt(P[4]);
    Gradient.ColorCount := StrToInt(P[5]);
    Gradient.GradientStyle := IntToStyle[StrToInt(P[6])];
   	Gradient.LineDirection := DireInt[StrToInt(P[7])];
    lstGallary.Items.AddObject(Name, Gradient);
  end;
var
	Reg: TIniFile;
  Data, Params: TStringList;
  i : Integer;
begin
	Data := TStringList.Create;
  Params := TStringList.Create;
	Reg := TIniFile.Create('Gradient Fill Editor.ini');
  Reg.ReadSectionValues('Gallary', Data);
  for i := 0 to Data.Count - 1 do
  begin
  	Params.Clear;
    Split(Params, Data.Values[Data.Names[i]], ',');
    if Params.Count <> 0 then
    begin
    	AddGallaryItem(Data.Names[i], Params);
    end;
  end;
  Params.Free;
  Data.Free;
  Reg.Free;
end;

procedure TfrmGradEditor.SaveIni;

	function ReadGallaryItem(i: Integer): String;
  begin
    with TGradient(lstGallary.Items.Objects[i]) do
    Result := IntToStr(Color1) + ',' + IntToStr(Color2) + ',' +
    	IntToStr(Color3) + ',' + IntToStr(Color4) + ',' +
      IntToStr(Color5) + ',' + IntToStr(ColorCount) + ',' + IntToStr(StyleToInt[GradientStyle]) + ','
      + IntToStr(IntDi[LineDirection]);
  end;
var
	Reg: TIniFile;
  Data, Params: TStringList;
  i : Integer;
begin
	Reg := TIniFile.Create('Gradient Fill Editor.ini');
	Reg.EraseSection('Gallary');
  for i := 0 to lstGallary.Items.Count - 1 do
  begin
  	Reg.WriteString('Gallary', lstGallary.Items[i], ReadGallaryItem(i));
  end;
  Reg.Free;
end;

procedure TfrmGradEditor.SetGradient(Grad: TGradient);
begin
  Color1.Color := Grad.Color1;
  Color2.Color := Grad.Color2;
  Color3.Color := Grad.Color3;
  Color4.Color := Grad.Color4;
  Color5.Color := Grad.Color5;
  txtLinesize.Text := IntToStr(Grad.Linesize);
  txtColors.Text := IntToStr(Grad.ColorCount);
  diCombo.ItemIndex := IntDi[Grad.LineDirection];
  case Grad.GradientStyle of
  	gsLine: SpeedButton1.Down := True;
  	gsEllipse: SpeedButton2.Down := True;
  	gsRect: SpeedButton3.Down := True;
  end;
end;

function TfrmGradEditor.GetGradient: TGradient;
begin
	with fGradient do
  begin
  	Color1 := Self.Color1.Color;
		Color2 := Self.Color2.Color;
		Color3 := Self.Color3.Color;
		Color4 := Self.Color4.Color;
		Color5 := Self.Color5.Color;

    ColorCount := StrToInt(txtColors.Text);
    LineSize := StrToInt(txtLinesize.Text);
    LineDirection := DireInt[diCombo.ItemIndex];
  	if SpeedButton1.Down = True then
    	GradientStyle := gsLine
    else if SpeedButton2.Down = True then
    	GradientStyle := gsEllipse
  	else if SpeedButton3.Down = True then
    	GradientStyle := gsRect;
  end;
  Result := fGradient;
end;

function TfrmGradEditor.Execute(Grad: TGradient): Boolean;
begin
  SetGradient(Grad);
  if Showmodal = mrYes then
  begin
    Grad.Assign(GetGradient);
  	Result := True;
  end
  else
  	Result := False;
end;

procedure TfrmGradEditor.Color1DblClick(Sender: TObject);
begin
	ColorDlg.Color := TEdit(Sender).Color;
  if ColorDlg.Execute then
  begin
  	TEdit(Sender).Color := ColorDlg.Color;
		btnUpdateClick(Self);
  end;
end;

procedure TfrmGradEditor.btnUpdateClick(Sender: TObject);
begin
	Paintbox1.Canvas.Brush.Color := clBtnFace;
	Paintbox1.Canvas.FillRect(Paintbox1.ClientRect);
	PaintGradient(Paintbox1.Canvas, Paintbox1.ClientRect, GetGradient);
end;

procedure TfrmGradEditor.FormCreate(Sender: TObject);
begin
	fGradient := TGradient.Create;
	LoadIni;
end;

procedure TfrmGradEditor.FormDestroy(Sender: TObject);
begin
	SaveIni;
	fGradient.Free;
end;

procedure TfrmGradEditor.lstGallaryDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	Grad: TGradient;
  Cap: String;
  R : TRect;
  Bmp: TBitmap;
begin
	Grad := TGradient(lstGallary.Items.Objects[Index]);
  Cap := lstGallary.Items[Index];
  R := Bounds(Rect.Left+2, Rect.Top+2, 27, 27);
  lstGallary.Canvas.FillRect(Rect);
  Bmp := TBitmap.Create;
  Bmp.Width := 27;
  Bmp.Height := 27;
  Grad.Linesize := 1;
  PaintGradient(Bmp.Canvas, Classes.Rect(0,0,27,27), Grad);
	lstGallary.Canvas.Draw(Rect.Left+2, Rect.Top+2, Bmp);
  lstGallary.Canvas.Textout(34, Rect.Top+10, Cap);
  Bmp.Free;
  Grad := nil;
end;

procedure TfrmGradEditor.btnAddClick(Sender: TObject);
var
	New: TGradient;
  Inputname: String;
begin
	Inputname := Inputbox('New Gradient Fill', 'Please enter a name for the current Gradient Fill object.','');
  if Inputname = '' then Exit;
	New := TGradient.Create;
	New.Assign(GetGradient);
	lstGallary.Items.AddObject(Inputname, New);
end;

procedure TfrmGradEditor.Button3Click(Sender: TObject);
begin
	if MessageDlg('Do you really want to delete this object?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
	lstGallary.Items.Delete(lstGallary.ItemIndex);
end;

procedure TfrmGradEditor.SpeedButton1Click(Sender: TObject);
begin
	btnUpdateClick(Self);
end;

procedure TfrmGradEditor.txtLinesizeChange(Sender: TObject);
begin
	btnUpdateClick(Self);
end;

procedure TfrmGradEditor.diComboChange(Sender: TObject);
begin
	btnUpdateClick(Self);
end;

procedure TfrmGradEditor.PaintBox1Paint(Sender: TObject);
begin
	btnUpdateClick(Self);
end;

procedure TfrmGradEditor.lstGallaryDblClick(Sender: TObject);
begin
	SetGradient(TGradient(lstGallary.Items.Objects[lstGallary.ItemIndex]));
	btnUpdateClick(Self);  
end;

end.
