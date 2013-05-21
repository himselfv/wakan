unit JWBCharDataBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids;

type
  TfCharDataBrowser = class(TForm)
    Grid: TStringGrid;
    Panel1: TPanel;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
  public
    procedure Reload;
  end;

var
  fCharDataBrowser: TfCharDataBrowser;

implementation
uses TextTable, JWBCharData;

{$R *.dfm}

procedure TfCharDataBrowser.FormShow(Sender: TObject);
begin
  Reload;
end;

procedure TfCharDataBrowser.Reload;
var CCharProp: TTextTableCursor;
  row: integer;
begin
  Grid.ColCount := 6;
  Grid.FixedCols := 0;
  Grid.RowCount := 2;
  Grid.FixedRows := 1;

  Grid.ColWidths[0] := 32; //index
  Grid.ColWidths[1] := 32; //kanji
  Grid.ColWidths[2] := 64; //type
  Grid.ColWidths[3] := 128; //data
  Grid.ColWidths[4] := 32; //readdot
  Grid.ColWidths[5] := 32; //position

  Grid.Cells[0, 0] := 'Index';
  Grid.Cells[1, 0] := 'Kanji';
  Grid.Cells[2, 0] := 'Type';
  Grid.Cells[3, 0] := 'Data';
  Grid.Cells[4, 0] := 'Dot';
  Grid.Cells[5, 0] := 'Position';

  row := 1;
  CCharProp := TCharProp.NewCursor;
  CCharProp.First;
  while not CCharProp.EOF do begin
    Grid.Cells[0, row] := CCharProp.Str(TCharPropIndex);
    Grid.Cells[1, row] := CCharProp.Str(TCharPropKanji);
    Grid.Cells[2, row] := CCharProp.Str(TCharPropTypeId);
    Grid.Cells[3, row] := CCharProp.Str(TCharPropValue);
    Grid.Cells[4, row] := CCharProp.Str(TCharPropReadDot);
    Grid.Cells[5, row] := CCharProp.Str(TCharPropPosition);
    Inc(row);
    if Grid.RowCount<1+row then //required count
      Grid.RowCount := 1+row+1000; //preallocate
    CCharProp.Next;
  end;

  Grid.RowCount := 1+row;
end;

end.
