unit TextTableBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, ComCtrls, TextTable;

type
  TfTextTableBrowser = class(TForm)
    Panel1: TPanel;
    btnClose: TButton;
    Grid: TStringGrid;
  protected
    FTable: TTextTable;
  public
    procedure SetTable(ATable: TTextTable);
    procedure Reload;
  end;

var
  fCharDataBrowser: TfTextTableBrowser;

implementation

{$R *.dfm}

procedure TfTextTableBrowser.SetTable(ATable: TTextTable);
begin
  FTable := ATable;
  Reload;
end;

procedure TfTextTableBrowser.Reload;
var i, sz, row: integer;
  CTable: TTextTableCursor;
begin
  Grid.ColCount := Length(FTable.Fields);
  Grid.RowCount := 1 + FTable.RecordCount;
  Grid.FixedCols := 0;
  Grid.FixedRows := 0;

  for i := 0 to Length(FTable.Fields)-1 do begin
    case FTable.Fields[i].DataType of
      'b': sz := 32;
      'w','i': sz := 48;
      'l': sz := 48;
      's','x': sz := 96;
    else
      sz := 48;
    end;
    Grid.ColWidths[i] := sz;
    Grid.Cells[i,0] := FTable.Fields[i].Name+': '+FTable.Fields[i].DataType;
  end;

  row := 1;
  CTable := FTable.NewCursor;
  CTable.First;
  while not CTable.EOF do begin
    for i := 0 to Length(FTable.Fields)-1 do
      Grid.Cells[i,row] := CTable.Str(i);
    Inc(row);
    CTable.Next();
  end;
end;

end.
