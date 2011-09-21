unit unPKGmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, MemSource,
  Grids, Outline, StdCtrls, ExtCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Panel2: TPanel;
    DirectoryListBox1: TDirectoryListBox;
    Panel3: TPanel;
    DriveComboBox1: TDriveComboBox;
    Button1: TButton;
    Panel4: TPanel;
    Outline1: TOutline;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  PS: TPackageSource;
  SL: TStringList;

implementation

uses unPKGwait, unPKGview;

{$R *.DFM}

procedure TForm1.FormShow(Sender: TObject);
var fn:string;
    hcs,ccs,fcs:string;
begin
  try
  SL:=TStringList.Create;
  if paramstr(2)='/c' then PKG_EnableCryptedHeader;
  if paramcount=0 then
  begin
    if OpenDialog1.Execute then fn:=OpenDialog1.FileName else
    begin
      Application.Terminate;
      exit;
    end;
  end else fn:=paramstr(1);
  hcs:='0'; ccs:='0'; fcs:='0';
  if (not InputQuery('Package code number','Enter header code:',hcs)) or
     (not InputQuery('Package code number','Enter file system code:',fcs)) or
     (not InputQuery('Package code number','Enter crypt code:',ccs)) then
  begin
    Application.Terminate;
    exit;
  end;
  unPKGWaitForm.Show;
  unPKGWaitForm.Update;
  PS:=TPackageSource.Create(fn,strtoint(hcs),strtoint(fcs),strtoint(ccs));
  PS.Tree.SaveToFile('tree');
  Outline1.LoadFromFile('tree');
  DeleteFile('tree');
  PS.FileList.SaveToFile('tree');
  SL.LoadFromFile('tree');
  DeleteFile('tree');
  SL.Sort;
  Label1.Caption:=PS.Name;
  Label3.Caption:='Memory used: '+inttostr(PS.MemoryUsed);
  Caption:='unPKG - '+PS.FileName;
  unPKGWaitForm.Hide;
  except
    if unPKGWaitForm.Visible then unPKGWaitForm.Hide;
    Application.MessageBox(pchar((ExceptObject as Exception).Message),
      'Error opening package',MB_ICONERROR+MB_OK);
    Application.Terminate;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  unPKGWaitForm.Show;
  unPKGWaitForm.Label1.Caption:='Extracting '+SL[Outline1.SelectedItem-1]+'...';
  unPKGWaitForm.Update;
//  showmessage(SL[Outline1.SelectedItem-1]);
  PS[SL[Outline1.SelectedItem-1]].WriteToDisk(DirectoryListBox1.Directory);
  unPKGWaitForm.Hide;
  Label3.Caption:='Memory used: '+inttostr(PS.MemoryUsed);
end;

procedure TForm1.Button2Click(Sender: TObject);
var ms:TMemoryStream;
begin
  unPKGWaitForm.Show;
  unPKGWaitForm.Label1.Caption:='Extracting '+SL[Outline1.SelectedItem-1]+'...';
  unPKGWaitForm.Update;
  ViewForm.Memo1.Lines.Clear;
  ms:=PS[SL[Outline1.SelectedItem-1]].Lock;
  if ms<>nil then ViewForm.Memo1.Lines.LoadFromStream(ms) else
  begin
    PS[SL[Outline1.SelectedItem-1]].Unlock;
    unPKGWaitForm.Hide;
    exit;
  end;
  PS[SL[Outline1.SelectedItem-1]].Unlock;
  unPKGWaitForm.Hide;
  Label3.Caption:='Memory used: '+inttostr(PS.MemoryUsed);
  ViewForm.Show;
end;

end.
