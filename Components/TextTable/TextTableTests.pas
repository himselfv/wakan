unit TextTableTests;

interface
uses TestFramework, TextTable;

type
  TTextTableCase = class(TTestCase)
  protected
    FTable: TTextTable;
    CTable: TTextTableCursor;
    procedure SetUp; override;
    procedure TearDown; override;
    function CreateTable: TTextTable; virtual; abstract;
  end;

 { Randomly populates integer index and then tests it }
  TIntIndexCase = class(TTextTableCase)
  protected
    function CreateTable: TTextTable; override;
    procedure BeforeFill; virtual;
    procedure AfterFill; virtual;
  public
    procedure RandomFill(const maxVal: integer; const count: integer);
  published
    procedure RandomFill10;
    procedure RandomFill1k;
    procedure RandomFill10k;
    procedure RandomFill100k;
  end;

 { Same -- with $RAWINDEX }
  TRawIntIndexCase = class(TIntIndexCase)
  protected
    function CreateTable: TTextTable; override;
  end;

 { Same in NoCommit mode, then Reindex and test it }
  TCommitIntIndexCase = class(TIntIndexCase)
  protected
    procedure BeforeFill; override;
    procedure AfterFill; override;
  end;

 { Same -- with $RAWINDEX }
  TCommitRawIntIndexCase = class(TRawIntIndexCase)
  protected
    procedure BeforeFill; override;
    procedure AfterFill; override;
  end;

 { Same but the field is declared as string; we test what happens when we
  Locate(integer) on a string field -- it should work, albeit slower,
  when values are less than 10.
  Bigger values will be sorted as strings. }
  TStrIntIndexCase = class(TIntIndexCase)
  protected
    function CreateTable: TTextTable; override;
  end;

  TRawStrIntIndexCase = class(TIntIndexCase)
  protected
    function CreateTable: TTextTable; override;
  end;

 { Randomly populates string index with UCS-2 characters and then tests it }
  TUStringIndexCase = class(TTextTableCase)
  protected
    function CreateTable: TTextTable; override;
    procedure BeforeFill; virtual;
    procedure AfterFill; virtual;
  public
    procedure RandomFill(const loChar, hiChar: WideChar; const count: integer);
  published
    procedure RandomFill10;
    procedure RandomFill1k;
    procedure RandomFill10k;
    procedure RandomFill100k;
  end;

 { Same -- with $RAWINDEX }
  TRawUStringIndexCase = class(TUStringIndexCase)
  protected
    function CreateTable: TTextTable; override;
  end;

 { Same -- in NoCommit mode }
  TCommitUStringIndexCase = class(TUStringIndexCase)
  protected
    procedure BeforeFill; override;
    procedure AfterFill; override;
  end;

 { Same -- with $RAWINDEX }
  TCommitRawUStringIndexCase = class(TRawUStringIndexCase)
  protected
    procedure BeforeFill; override;
    procedure AfterFill; override;
  end;

 { Table with several indexes }
  TMultiIndexCase = class(TTextTableCase)
  protected
   //Fields
    fIntValue,
    fAnsiValue,
    fWideValue: integer;
    soIntValue,
    soAnsiValue,
    soWideValue: TSeekObject;
    function CreateTable: TTextTable; override;
    procedure AttachFields;
    procedure BeforeFill; virtual;
    procedure AfterFill; virtual;
  protected
    maxIntVal: integer; //set
    loChar, hiChar: WideChar; //set
    IntValues: array of integer;
    AnsiValues: array of integer;
    WideValues: array of integer;
    procedure InitValues;
    function GetIntValue: integer;
    function GetAnsiValue: AnsiString;
    function GetWideValue: WideString;
    procedure CheckIntValues;
    procedure CheckAnsiValues;
    procedure CheckWideValues;
  public
    procedure RandomFill(const count: integer);
  published
    procedure RandomFill10;
    procedure RandomFill1k;
    procedure RandomFill10k;
    procedure RandomFill100k;
  end;

implementation
uses SysUtils;

procedure TTextTableCase.SetUp;
begin
  FTable := CreateTable;
  CTable := TTextTableCursor.Create(FTable);
end;

procedure TTextTableCase.TearDown;
begin
  FreeAndNil(CTable);
  FreeAndNil(FTable);
end;

function TIntIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$FIELDS',
    'iValue',
    '$ORDERS',
    'Value_Order',
    '$SEEKS',
    '0',
    'Value'
  ]);
end;

procedure TIntIndexCase.BeforeFill;
begin
end;

procedure TIntIndexCase.AfterFill;
begin
end;

procedure TIntIndexCase.RandomFill(const maxVal: integer; const count: integer);
var cells: array of integer;
  i, tmp, total: integer;
  fValue: integer;
  soValue: TSeekObject;
begin
  randomize;
  SetLength(cells, maxVal);
  for i := 0 to Length(cells) - 1 do
    cells[i] := 0;

  fValue := FTable.GetFieldIndex('Value');
  Check(fValue>=0, 'Field `Value` not found.');

  soValue := FTable.GetSeekObject('Value');
  Check(soValue.ind_i<>-1, 'Seek object `Value` not found.');

  BeforeFill();

  for i := 0 to count - 1 do begin
    tmp := random(maxVal);
    Inc(cells[tmp]);
    FTable.AddRecord([IntToStr(tmp)]);
  end;

  AfterFill();

  CTable.SetOrder('Value_Order');
  total := 0;
  for i := 0 to maxVal - 1 do
    if cells[i]<=0 then
      Check(not FTable.Locate(@soValue,i), 'Located value which shouldn''t be in a table')
    else begin
      total := total + cells[i];
      Check(CTable.Locate(@soValue,i), 'Cannot locate value which should be in a table');
      tmp := cells[i];
      while tmp>0 do begin
        Check(CTable.Int(fValue)=i, 'Cannot locate another copy of value which should be in a table ('+IntToStr(tmp)+' remains)');
        Dec(tmp);
        CTable.Next;
      end;
      Check(CTable.EOF or (CTable.Int(fValue)<>i), 'Located another copy of value where all should have been enumerated already.');
    end;

  Check(FTable.RecordCount=total,'Number of records in the table differs from '
    +'expected (generated '+IntToStr(total)+', found '+IntToStr(FTable.RecordCount)+')');
end;

procedure TIntIndexCase.RandomFill10;
begin
  RandomFill(2, 10);
end;

procedure TIntIndexCase.RandomFill1k;
begin
  RandomFill(10, 1000);
end;

procedure TIntIndexCase.RandomFill10k;
begin
  RandomFill(500, 10000);
end;

procedure TIntIndexCase.RandomFill100k;
begin
  RandomFill(5000, 100000)
end;

function TRawIntIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'iValue',
    '$ORDERS',
    'Value_Order',
    '$SEEKS',
    '0',
    'Value'
  ]);
end;

procedure TCommitIntIndexCase.BeforeFill;
begin
  FTable.NoCommitting := true;
end;

procedure TCommitIntIndexCase.AfterFill;
begin
  FTable.NoCommitting := false;
  FTable.Reindex;
end;

procedure TCommitRawIntIndexCase.BeforeFill;
begin
  FTable.NoCommitting := true;
end;

procedure TCommitRawIntIndexCase.AfterFill;
begin
  FTable.NoCommitting := false;
  FTable.Reindex;
end;

function TStrIntIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$FIELDS',
    'sValue',
    '$ORDERS',
    'Value_Order',
    '$SEEKS',
    '0',
    'Value'
  ]);
end;

function TRawStrIntIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'sValue',
    '$ORDERS',
    'Value_Order',
    '$SEEKS',
    '0',
    'Value'
  ]);
end;

function TUStringIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$FIELDS',
    'xValue',
    '$ORDERS',
    'Value_Order',
    '$SEEKS',
    '0',
    'Value'
  ]);
end;

procedure TUStringIndexCase.BeforeFill;
begin
end;

procedure TUStringIndexCase.AfterFill;
begin
end;

procedure TUStringIndexCase.RandomFill(const loChar, hiChar: WideChar; const count: integer);
var cells: array of integer;
  valBase, maxVal: integer;
  i, tmp, total: integer;
  fValue: integer;
  soValue: TSeekObject;
  val: WideChar;
begin
  randomize;
  valBase := Word(loChar);
  maxVal := Word(hiChar)-Word(loChar);
  SetLength(cells, maxVal);
  for i := 0 to Length(cells) - 1 do
    cells[i] := 0;

  fValue := FTable.GetFieldIndex('Value');
  Check(fValue>=0, 'Field `Value` not found.');

  soValue := FTable.GetSeekObject('Value');
  Check(soValue.ind_i<>-1, 'Seek object `Value` not found.');

  BeforeFill();

  for i := 0 to count - 1 do begin
    tmp := random(maxVal);
    Inc(cells[tmp]);
    val := WideChar(valBase + tmp);
    FTable.AddRecord([val]);
  end;

  AfterFill();

  CTable.SetOrder('Value_Order');
  total := 0;
  for i := 0 to maxVal - 1 do begin
    val := WideChar(valBase + i);
    if cells[i]<=0 then
      Check(not FTable.Locate(@soValue,val), 'Located value which shouldn''t be in a table')
    else begin
      total := total + cells[i];
      Check(CTable.Locate(@soValue,val), 'Cannot locate value which should be in a table');
      tmp := cells[i];
      while tmp>0 do begin
        Check(CTable.Str(fValue)=val, 'Cannot locate another copy of value which should be in a table ('+IntToStr(tmp)+' remains)');
        Dec(tmp);
        CTable.Next;
      end;
      Check(CTable.EOF or (CTable.Str(fValue)<>val), 'Located another copy of value where all should have been enumerated already.');
    end;
  end;

  Check(FTable.RecordCount=total,'Number of records in the table differs from '
    +'expected (generated '+IntToStr(total)+', found '+IntToStr(FTable.RecordCount)+')');
end;

procedure TUStringIndexCase.RandomFill10;
begin
  RandomFill(WideChar($4E00), WideChar($9FBF), 10);
end;

procedure TUStringIndexCase.RandomFill1k;
begin
  RandomFill(WideChar($4E00), WideChar($9FBF), 1000);
end;

procedure TUStringIndexCase.RandomFill10k;
begin
  RandomFill(WideChar($4E00), WideChar($9FBF), 10000);
end;

procedure TUStringIndexCase.RandomFill100k;
begin
  RandomFill(WideChar($4E00), WideChar($9FBF), 100000);
end;

function TRawUStringIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'xValue',
    '$ORDERS',
    'Value_Order',
    '$SEEKS',
    '0',
    'Value'
  ]);
end;

procedure TCommitUStringIndexCase.BeforeFill;
begin
  FTable.NoCommitting := true;
end;

procedure TCommitUStringIndexCase.AfterFill;
begin
  FTable.NoCommitting := false;
  FTable.Reindex;
end;

procedure TCommitRawUStringIndexCase.BeforeFill;
begin
  FTable.NoCommitting := true;
end;

procedure TCommitRawUStringIndexCase.AfterFill;
begin
  FTable.NoCommitting := false;
  FTable.Reindex;
end;

function TMultiIndexCase.CreateTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'iIntValue',
    'sAnsiValue',
    'xWideValue',
    '$ORDERS',
   //Orders go in different order just to catch index variable misplacements
    'AnsiValue_Order',
    'WideValue_Order',
    'IntValue_Order',
    '$SEEKS',
    '0',
    'AnsiValue',
    'WideValue',
    'IntValue'
  ]);
end;

procedure TMultiIndexCase.AttachFields;
begin
  fIntValue := FTable.GetFieldIndex('IntValue');
  Check(fIntValue>=0, 'Field `IntValue` not found.');
  fAnsiValue := FTable.GetFieldIndex('AnsiValue');
  Check(fAnsiValue>=0, 'Field `AnsiValue` not found.');
  fWideValue := FTable.GetFieldIndex('WideValue');
  Check(fWideValue>=0, 'Field `WideValue` not found.');

  soIntValue := FTable.GetSeekObject('IntValue');
  Check(soIntValue.ind_i<>-1, 'Seek object `IntValue` not found.');
  soAnsiValue := FTable.GetSeekObject('AnsiValue');
  Check(soAnsiValue.ind_i<>-1, 'Seek object `AnsiValue` not found.');
  soWideValue := FTable.GetSeekObject('WideValue');
  Check(soWideValue.ind_i<>-1, 'Seek object `WideValue` not found.');
end;

procedure TMultiIndexCase.BeforeFill;
begin
end;

procedure TMultiIndexCase.AfterFill;
begin
end;

procedure TMultiIndexCase.InitValues;
var maxVal: integer;
  i: integer;
begin
  SetLength(IntValues, maxIntVal);
  for i := 0 to Length(IntValues) - 1 do
    IntValues[i] := 0;

  SetLength(AnsiValues,255);
  for i := 0 to Length(AnsiValues) - 1 do
    AnsiValues[i] := 0;

  maxVal := Word(hiChar)-Word(loChar);
  SetLength(WideValues, maxVal);
  for i := 0 to Length(WideValues) - 1 do
    WideValues[i] := 0;
end;

function TMultiIndexCase.GetIntValue: integer;
begin
  Result := random(maxIntVal);
  Inc(IntValues[Result]);
end;

function TMultiIndexCase.GetAnsiValue: AnsiString;
var tmp: integer;
  ch: AnsiChar;
begin
 //We do not want symbols less than $0021 for various reasons
  tmp := random(Length(AnsiValues)-$0021)+$0021;
  ch := AnsiChar(Byte(tmp));
  ch := Upcase(ch); //or we'll hit lower/upper case collisions
  tmp := Byte(ch);
  Inc(AnsiValues[tmp]);
  Result := ch;
end;

function TMultiIndexCase.GetWideValue: WideString;
var tmp: integer;
begin
  tmp := random(Length(WideValues));
  Inc(WideValues[tmp]);
  Result := WideChar(Word(loChar) + tmp);
end;

procedure TMultiIndexCase.CheckIntValues;
var i, tmp, total: integer;
begin
  CTable.SetOrder('IntValue_Order');
  total := 0;
  for i := 0 to Length(IntValues) - 1 do
    if IntValues[i]<=0 then
      Check(not FTable.Locate(@soIntValue,i), 'Int: Located value which shouldn''t be in a table')
    else begin
      total := total + IntValues[i];
      Check(CTable.Locate(@soIntValue,i), 'Int: Cannot locate value which should be in a table');
      tmp := IntValues[i];
      while tmp>0 do begin
        Check(CTable.Int(fIntValue)=i, 'Int: Cannot locate another copy of value which should be in a table ('+IntToStr(tmp)+' remains)');
        Dec(tmp);
        CTable.Next;
      end;
      Check(CTable.EOF or (CTable.Int(fIntValue)<>i), 'Int: Located another copy of value where all should have been enumerated already.');
    end;

  Check(FTable.RecordCount=total,'Int: Number of records in the table differs from '
    +'expected (generated '+IntToStr(total)+', found '+IntToStr(FTable.RecordCount)+')');
end;

procedure TMultiIndexCase.CheckAnsiValues;
var i, tmp, total: integer;
  val: string;
  testRes: boolean;
begin
  CTable.SetOrder('AnsiValue_Order');
  total := 0;
  for i := 0 to Length(AnsiValues) - 1 do begin
    val := string(AnsiChar(Byte(i)));
    if AnsiValues[i]<=0 then begin
      tmp := Byte(upcase(AnsiChar(i)));
      if AnsiValues[tmp]=0 then //or we'd hit uppercase/lowercase collisions
        testRes := FTable.Locate(@soAnsiValue,val)
      else testRes := false; //can't check
      Check(not testRes, 'Ansi: Located value which shouldn''t be in a table')
    end else begin
      total := total + AnsiValues[i];
      testRes := CTable.Locate(@soAnsiValue,val);
      Check(testRes, 'Ansi: Cannot locate value which should be in a table');
      tmp := AnsiValues[i];
      while tmp>0 do begin
        Check(CTable.Str(fAnsiValue)=val, 'Ansi: Cannot locate another copy of value which should be in a table ('+IntToStr(tmp)+' remains)');
        Dec(tmp);
        CTable.Next;
      end;
      Check(CTable.EOF or (CTable.Str(fAnsiValue)<>val), 'Ansi: Located another copy of value where all should have been enumerated already.');
    end;
  end;

  Check(FTable.RecordCount=total,'Ansi: Number of records in the table differs from '
    +'expected (generated '+IntToStr(total)+', found '+IntToStr(FTable.RecordCount)+')');
end;

procedure TMultiIndexCase.CheckWideValues;
var i, tmp, total: integer;
  val: WideChar;
begin
  CTable.SetOrder('WideValue_Order');
  total := 0;
  for i := 0 to Length(WideValues) - 1 do begin
    val := WideChar(Word(loChar)+i);
    if WideValues[i]<=0 then
      Check(not FTable.Locate(@soWideValue,val), 'Wide: Located value which shouldn''t be in a table')
    else begin
      total := total + WideValues[i];
      Check(CTable.Locate(@soWideValue,val), 'Wide: Cannot locate value which should be in a table');
      tmp := WideValues[i];
      while tmp>0 do begin
        Check(CTable.Str(fWideValue)=val, 'Wide: Cannot locate another copy of value which should be in a table ('+IntToStr(tmp)+' remains)');
        Dec(tmp);
        CTable.Next;
      end;
      Check(CTable.EOF or (CTable.Str(fWideValue)<>val), 'Wide: Located another copy of value where all should have been enumerated already.');
    end;
  end;

  Check(FTable.RecordCount=total,'Wide: Number of records in the table differs from '
    +'expected (generated '+IntToStr(total)+', found '+IntToStr(FTable.RecordCount)+')');
end;

procedure TMultiIndexCase.RandomFill(const count: integer);
var i: integer;
begin
  AttachFields();

  randomize;

  maxIntVal := count div 10 + 2; //not less than 2
  loChar := WideChar($4E00);
  hiChar := WideChar($9FBF);
  InitValues();

  BeforeFill();

  for i := 0 to count - 1 do
    FTable.AddRecord([IntToStr(GetIntValue), string(GetAnsiValue), GetWideValue]);

  AfterFill();

  CheckIntValues();
  CheckAnsiValues();
  CheckWideValues();
end;

procedure TMultiIndexCase.RandomFill10;
begin
  RandomFill(10);
end;

procedure TMultiIndexCase.RandomFill1k;
begin
  RandomFill(1000);
end;

procedure TMultiIndexCase.RandomFill10k;
begin
  RandomFill(10000);
end;

procedure TMultiIndexCase.RandomFill100k;
begin
  RandomFill(100000);
end;

initialization
  RegisterTest(TIntIndexCase.Suite);
  RegisterTest(TRawIntIndexCase.Suite);
  RegisterTest(TCommitIntIndexCase.Suite);
  RegisterTest(TCommitRawIntIndexCase.Suite);
  RegisterTest(TStrIntIndexCase.Suite);
  RegisterTest(TRawStrIntIndexCase.Suite);
  RegisterTest(TUStringIndexCase.Suite);
  RegisterTest(TRawUStringIndexCase.Suite);
  RegisterTest(TCommitUStringIndexCase.Suite);
  RegisterTest(TCommitRawUStringIndexCase.Suite);
  RegisterTest(TMultiIndexCase.Suite);
end.
