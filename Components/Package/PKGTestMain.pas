unit PKGTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MemSource, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  PS:TPackageSource;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var MS:TMemoryStream;
begin
  PS:=TPackageSource.Create('test.pkg',233);
//  PS.Tree.SaveToFile('tree');
//  TreeView1.LoadFromFile('tree');
//  DeleteFile('tree');
  ListBox1.Items:=PS.FileList;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  PS[ListBox1.Items[ListBox1.ItemIndex]].WriteToDisk('');
end;

end.
