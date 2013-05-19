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
var CCharRead: TTextTableCursor;
  row: integer;
begin
  Grid.ColCount := 6;
  Grid.FixedCols := 0;
  Grid.RowCount := 2;
  Grid.FixedRows := 1;

  Grid.ColWidths[0] := 32; //kanji
  Grid.ColWidths[1] := 64; //type
  Grid.ColWidths[2] := 128; //data
  Grid.ColWidths[3] := 32; //index
  Grid.ColWidths[4] := 32; //readdot
  Grid.ColWidths[5] := 32; //position

  Grid.Cells[0, 0] := 'Kanji';
  Grid.Cells[1, 0] := 'Type';
  Grid.Cells[2, 0] := 'Data';
  Grid.Cells[3, 0] := 'Index';
  Grid.Cells[4, 0] := 'Dot';
  Grid.Cells[5, 0] := 'Position';

  row := 1;
  CCharRead := TCharRead.NewCursor;
  CCharRead.First;
  while not CCharRead.EOF do begin
    Grid.Cells[0, row] := CCharRead.Str(TCharReadKanji); //kanji
    Grid.Cells[1, row] := CCharRead.Str(TCharReadType); //type
    Grid.Cells[2, row] := CCharRead.Str(TCharReadReading); //reading (whatever we could read)
    Grid.Cells[3, row] := CCharRead.Str(TCharReadIndex);
    Grid.Cells[4, row] := CCharRead.Str(TCharReadReadDot);
    Grid.Cells[5, row] := CCharRead.Str(TCharReadPosition);
    Inc(row);
    Grid.RowCount := 1+row;
    CCharRead.Next;
  end;
end;

end.
