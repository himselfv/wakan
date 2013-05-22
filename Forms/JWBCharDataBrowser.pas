unit JWBCharDataBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, ComCtrls;

type
  TfCharDataBrowser = class(TForm)
    Panel1: TPanel;
    btnClose: TButton;
    tsTabs: TTabControl;
    Grid: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure tsTabsChange(Sender: TObject);
  protected
    procedure CharsReload;
    procedure PropsReload;
    procedure RadicalsReload;
  end;

var
  fCharDataBrowser: TfCharDataBrowser;

implementation
uses TextTable, JWBCharData;

{$R *.dfm}

procedure TfCharDataBrowser.FormShow(Sender: TObject);
begin
  tsTabs.TabIndex := 0;
  tsTabsChange(tsTabs);
end;

procedure TfCharDataBrowser.CharsReload;
var CChar: TTextTableCursor;
  row: integer;
begin
  Grid.ColCount := 9;
  Grid.FixedCols := 0;
  Grid.RowCount := 2;
  Grid.FixedRows := 1;

  Grid.ColWidths[0] := 32; //index
  Grid.ColWidths[1] := 48; //chinese
  Grid.ColWidths[2] := 32; //type
  Grid.ColWidths[3] := 48; //unicode
  Grid.ColWidths[4] := 48; //stroke #
  Grid.ColWidths[5] := 48; //jp stroke #
  Grid.ColWidths[6] := 48; //jp freq
  Grid.ColWidths[7] := 48; //ch freq
  Grid.ColWidths[8] := 48; //jouyou gr.

  Grid.Cells[0, 0] := 'Index';
  Grid.Cells[1, 0] := 'Chinese';
  Grid.Cells[2, 0] := 'Type';
  Grid.Cells[3, 0] := 'Unicode';
  Grid.Cells[4, 0] := 'Stroke #';
  Grid.Cells[5, 0] := 'Jp stroke #';
  Grid.Cells[6, 0] := 'Jp freq';
  Grid.Cells[7, 0] := 'Ch freq';
  Grid.Cells[8, 0] := 'Jouyou gr.';

  row := 1;
  CChar := TChar.NewCursor;
  CChar.First;
  while not CChar.EOF do begin
    Grid.Cells[0, row] := CChar.Str(TCharIndex);
    Grid.Cells[1, row] := CChar.Str(TCharChinese);
    Grid.Cells[2, row] := CChar.Str(TCharType);
    Grid.Cells[3, row] := CChar.Str(TCharUnicode);
    Grid.Cells[4, row] := CChar.Str(TCharStrokeCount);
    Grid.Cells[5, row] := CChar.Str(TCharJpStrokeCount);
    Grid.Cells[6, row] := CChar.Str(TCharJpFrequency);
    Grid.Cells[7, row] := CChar.Str(TCharChFrequency);
    Grid.Cells[8, row] := CChar.Str(TCharJouyouGrade);
    Inc(row);
    if Grid.RowCount<1+row then //required count
      Grid.RowCount := 1+row+1000; //preallocate
    CChar.Next;
  end;

  Grid.RowCount := 1+row;
end;

procedure TfCharDataBrowser.PropsReload;
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

procedure TfCharDataBrowser.RadicalsReload;
var CRadicals: TTextTableCursor;
  row: integer;
begin
  Grid.ColCount := 8;
  Grid.FixedCols := 0;
  Grid.RowCount := 2;
  Grid.FixedRows := 1;

  Grid.ColWidths[0] := 48;
  Grid.ColWidths[1] := 48;
  Grid.ColWidths[2] := 48;
  Grid.ColWidths[3] := 48;
  Grid.ColWidths[4] := 48;
  Grid.ColWidths[5] := 48;
  Grid.ColWidths[6] := 48;
  Grid.ColWidths[7] := 48;

  Grid.Cells[0, 0] := 'Number';
  Grid.Cells[1, 0] := 'Variant';
  Grid.Cells[2, 0] := 'Unicode';
  Grid.Cells[3, 0] := 'StrokeCount';
  Grid.Cells[4, 0] := 'UnicodeCount';
  Grid.Cells[5, 0] := 'BushuCount';
  Grid.Cells[6, 0] := 'JapaneseCount';
  Grid.Cells[7, 0] := 'KangXiCount';

  row := 1;
  CRadicals := TRadicals.NewCursor;
  CRadicals.First;
  while not CRadicals.EOF do begin
    Grid.Cells[0, row] := CRadicals.Str(TRadicalsNumber);
    Grid.Cells[1, row] := CRadicals.Str(TRadicalsVariant);
    Grid.Cells[2, row] := CRadicals.Str(TRadicalsUnicode);
    Grid.Cells[3, row] := CRadicals.Str(TRadicalsStrokeCount);
    Grid.Cells[4, row] := CRadicals.Str(TRadicalsUnicodeCount);
    Grid.Cells[5, row] := CRadicals.Str(TRadicalsBushuCount);
    Grid.Cells[6, row] := CRadicals.Str(TRadicalsJapaneseCount);
    Grid.Cells[7, row] := CRadicals.Str(TRadicalsKangXiCount);
    Inc(row);
    if Grid.RowCount<1+row then //required count
      Grid.RowCount := 1+row+1000; //preallocate
    CRadicals.Next;
  end;

  Grid.RowCount := 1+row;
end;

procedure TfCharDataBrowser.tsTabsChange(Sender: TObject);
begin
  case tsTabs.TabIndex of
    0: CharsReload;
    1: PropsReload;
    2: RadicalsReload;
  end;
end;

end.
