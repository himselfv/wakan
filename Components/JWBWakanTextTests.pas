unit JWBWakanTextTests;

interface
uses TestFramework, JWBWakanText;

type
  TSourcePosTestCase = class(TTestCase)
  published
    procedure Initializers;
    procedure Arithmetics;
  end;

  TSourceBlockTestCase = class(TTestCase)
  published
    procedure Initializers;
    procedure Arithmetics;
  end;

  TWakanTextTestCase = class(TTestCase)
  protected
    text: TWakanText;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BaseState;
    procedure Clearing;
    procedure AddingLines;
    procedure SplittingLines;
    procedure DeletingCharacters;
    procedure DeletingLines;
    procedure DeletingBlocks;
    procedure LotsOfLines;
    procedure MiscSimple;
  end;

//TODO: Loading from examples (different encodings, Wakan text)
//TODO: Saving to temp then reloading.
//TODO: Pasting.
//TODO: Expanding Ruby

implementation
uses SysUtils, JWBStrings;

procedure TSourcePosTestCase.Initializers;
var p1: TSourcePos;
begin
  p1 := SourcePos(3,4);
  Check(p1.x=3);
  Check(p1.y=4);
end;

procedure TSourcePosTestCase.Arithmetics;
var p1, p2: TSourcePos;
begin
 //Equal
  p1 := SourcePos(4,3);
  p2 := SourcePos(4,3);
  Check(p1=p2,'Arithmetics 1-1');
  Check(not(p1>p2),'Arithmetics 1-2');
  Check(not(p1<p2),'Arithmetics 1-3');
  Check(not(p2>p1),'Arithmetics 1-4');
  Check(not(p2<p1),'Arithmetics 1-5');
  Check(not(p2<>p1),'Arithmetics 1-6');

 //Difference in x
  p1 := SourcePos(5,3);
  p2 := SourcePos(6,3);
  Check(not(p1=p2),'Arithmetics 2-1');
  Check(not(p1>p2),'Arithmetics 2-2');
  Check(p1<p2,'Arithmetics 2-3');
  Check(p2>p1,'Arithmetics 2-4');
  Check(not(p2<p1),'Arithmetics 2-5');
  Check(p2<>p1,'Arithmetics 2-6');

 //Difference in y
  p1 := SourcePos(5,3);
  p2 := SourcePos(5,4);
  Check(not(p1=p2),'Arithmetics 3-1');
  Check(not(p1>p2),'Arithmetics 3-2');
  Check(p1<p2,'Arithmetics 3-3');
  Check(p2>p1,'Arithmetics 3-4');
  Check(not(p2<p1),'Arithmetics 3-5');
  Check(p2<>p1,'Arithmetics 3-6');

 //Difference in both
  p1 := SourcePos(5,3);
  p2 := SourcePos(6,4);
  Check(not(p1=p2),'Arithmetics 4-1');
  Check(not(p1>p2),'Arithmetics 4-2');
  Check(p1<p2,'Arithmetics 4-3');
  Check(p2>p1,'Arithmetics 4-4');
  Check(not(p2<p1),'Arithmetics 4-5');
  Check(p2<>p1,'Arithmetics 4-6');

 //y smaller but x bigger (position priority check)
  p1 := SourcePos(4,4);
  p2 := SourcePos(5,3);
  Check(not(p1=p2),'Arithmetics 5-1');
  Check(p1>p2,'Arithmetics 5-2');
  Check(not(p1<p2),'Arithmetics 5-3');
  Check(not(p2>p1),'Arithmetics 5-4');
  Check(p2<p1,'Arithmetics 5-5');
  Check(p2<>p1,'Arithmetics 5-6');
end;

procedure TSourceBlockTestCase.Initializers;
var b1: TSourceBlock;
begin
  b1 := SourceBlock(3,4,5,6);
  Check(b1.fromy=3);
  Check(b1.fromx=4);
  Check(b1.toy=5);
  Check(b1.tox=6);
end;

procedure TSourceBlockTestCase.Arithmetics;
var b1, b2, b: TSourceBlock;
begin
 //Equal
  b1 := SourceBlock(3,4,5,6);
  b2 := SourceBlock(3,4,5,6);
  Check(b1=b2);
  Check(not(b1<>b2));

 //Difference in one point
  b1 := SourceBlock(3,4,5,6);
  b2 := SourceBlock(4,4,5,6);
  Check(not(b1=b2));
  Check(b1<>b2);

  b1 := SourceBlock(3,4,5,6);
  b2 := SourceBlock(3,5,5,6);
  Check(not(b1=b2));
  Check(b1<>b2);

  b1 := SourceBlock(3,4,5,6);
  b2 := SourceBlock(3,4,6,6);
  Check(not(b1=b2));
  Check(b1<>b2);

  b1 := SourceBlock(3,4,5,6);
  b2 := SourceBlock(3,4,5,7);
  Check(not(b1=b2));
  Check(b1<>b2);

 //Valid intersection by y
  b1 := SourceBlock(1,1,10,1);
  b2 := SourceBlock(8,1,12,1);
  b := b2 and b1;
  Check(b.fromy=8);
  Check(b.fromx=1);
  Check(b.toy=10);
  Check(b.tox=1);
  b := b1 and b2;
  Check(b.fromy=8);
  Check(b.fromx=1);
  Check(b.toy=10);
  Check(b.tox=1);

 //Valid intersection by x only
  b1 := SourceBlock(4,5,10,6);
  b2 := SourceBlock(4,6,10,5);
  b := b2 and b1;
  Check(b.fromy=4);
  Check(b.fromx=6);
  Check(b.toy=10);
  Check(b.tox=5);
  b := b1 and b2;
  Check(b.fromy=4);
  Check(b.fromx=6);
  Check(b.toy=10);
  Check(b.tox=5);

 //No intersection
  b1 := SourceBlock(4,1,6,1);
  b2 := SourceBlock(8,1,10,1);
  b := b1 and b2;
 //We don't specifically define the values, but the block must be empty
  Check((b.fromy>b.toy)or((b.fromy=b.toy)and(b.fromx>b.tox)));
end;

procedure TWakanTextTestCase.SetUp;
begin
  text := TWakanText.Create;
end;

procedure TWakanTextTestCase.TearDown;
begin
  FreeAndNil(text);
end;

procedure TWakanTextTestCase.BaseState;
begin
  Check(text.Lines<>nil);
  Check(text.PropertyLines<>nil);
  Check(text.Lines.Count=0);
  Check(text.PropertyLines.Count=0);
end;

procedure TWakanTextTestCase.Clearing;
begin
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Third line.');
  Check(text.Lines.Count=3);
  Check(text.PropertyLines.Count=3);

  text.Clear;
  Check(text.Lines<>nil);
  Check(text.PropertyLines<>nil);
  Check(text.Lines.Count=0);
  Check(text.PropertyLines.Count=0);

 //Again
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.Clear;
  Check(text.Lines.Count=0);
  Check(text.PropertyLines.Count=0);

 //Multiple times
  text.Clear;
  text.Clear;
  text.Clear;
  text.Clear;
  Check(text.Lines.Count=0);
  Check(text.PropertyLines.Count=0);
end;

procedure TWakanTextTestCase.AddingLines;
begin
 //Adding
  text.AddLine('Example text.');
  Check(text.Lines.Count=1);
  Check(text.PropertyLines.Count=1);
  Check(text.Lines[0]='Example text.');
  Check(text.PropertyLines[0].charcount=Length(text.Lines[0]));

  text.AddLine('Another line.');
  Check(text.Lines.Count=2);
  Check(text.PropertyLines.Count=2);
  Check(text.Lines[0]='Example text.');
  Check(text.PropertyLines[0].charcount=Length(text.Lines[0]));
  Check(text.Lines[1]='Another line.');
  Check(text.PropertyLines[1].charcount=Length(text.Lines[1]));
end;

procedure TWakanTextTestCase.SplittingLines;
begin
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Third line.');
  Check(text.Lines.Count=3, 'Initial line count is wrong.');

 //Splitting
  text.SplitLine(3,1); //2-nd line at 3-rd char
  Check(text.Lines.Count=4, 'Line count after splitting is wrong.');
  Check(text.PropertyLines.Count=4, 'Property line count after splitting is wrong.');
  Check(text.Lines[1]='Ano', 'First part of the line invalid after split: "'+text.Lines[1]+'"');
  Check(text.PropertyLines[1].charcount=Length(text.Lines[1]));
  Check(text.Lines[2]='ther line.', 'Second part of the line invalid after split: "'+text.Lines[2]+'"');
  Check(text.PropertyLines[2].charcount=Length(text.Lines[2]));
  Check(text.Lines[3]='Third line.');
  Check(text.PropertyLines[3].charcount=Length(text.Lines[3]));

  text.SplitLine(Length(text.Lines[3]), 3);
  Check(text.Lines.Count=5, 'Line count after splitting is wrong.');
  Check(text.Lines[4]='');

 //Joining
  text.JoinLine(3);
  Check(text.Lines.Count=4, 'Line count after joining is wrong.');
  Check(text.Lines[3]='Third line.');

  text.JoinLine(2);
  Check(text.Lines.Count=3);
  Check(text.Lines[2]='ther line.Third line.');
  Check(text.PropertyLines[2].charcount=Length(text.Lines[2]));

 //Splitting again
  text.SplitLine(0,0);
  Check(text.Lines.Count=4);
  Check(text.Lines[0]='');
  text.JoinLine(0);
  Check(text.Lines.Count=3);
  Check(text.Lines[0]='Example text.');
end;

procedure TWakanTextTestCase.DeletingCharacters;
begin
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Third line.');

  text.DeleteCharacter(1,0);
  Check(text.Lines[0]='Eample text.');
  Check(text.PropertyLines[0].charcount=Length(text.Lines[0]));
  text.DeleteCharacter(1,0);
  text.DeleteCharacter(1,0);
  Check(text.Lines[0]='Eple text.');
  text.DeleteCharacter(Length(text.Lines[0])-1,0);
  Check(text.Lines[0]='Eple text');
  text.DeleteCharacter(Length(text.Lines[0])-1,0);
  Check(text.Lines[0]='Eple tex');
  Check(text.PropertyLines[0].charcount=Length(text.Lines[0]));
  text.DeleteCharacter(0,0);
  Check(text.Lines[0]='ple tex');
  Check(text.PropertyLines[0].charcount=Length(text.Lines[0]));

 //Second line + SourcePos version
  text.DeleteCharacter(SourcePos(4,1));
  Check(text.Lines[1]='Anoter line.');
  text.DeleteCharacter(SourcePos(5,1));
  Check(text.Lines[1]='Anote line.');
  Check(text.PropertyLines[0].charcount=Length(text.Lines[0]));

 //Haven't we accidentally changed number of lines?
  Check(text.Lines.Count=3);
  Check(text.PropertyLines.Count=3);
end;

procedure TWakanTextTestCase.DeletingLines;
begin
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Third line.');
  text.AddLine('Fourth line.');
  text.AddLine('Fifth line.');

  text.DeleteLine(4);
  text.DeleteLine(0);
  text.DeleteLine(1);
  Check(text.Lines.Count=2, 'Line count wrong after deleting');
  Check(text.PropertyLines.Count=2);
  Check(text.Lines[0]='Another line.');
  Check(text.Lines[1]='Fourth line.');
end;

procedure TWakanTextTestCase.DeletingBlocks;
begin
 //Multiline block
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Third line.');
  text.DeleteBlock(SourceBlock(0,4,2,6));
  Check(text.Lines.Count=1);
  Check(text.PropertyLines.Count=1);
  Check(text.Lines[0]='Examline.', 'Multiline block');

 //Effectively empty block
  text.DeleteBlock(SourceBlock(0,3,0,2));
  text.DeleteBlock(SourceBlock(0,3,0,3));
  Check(text.Lines[0]='Examline.', 'Effectively empty block');

 //Block referencing a single invalid line
 //Single line is often a special case so have to check with invalid index here too
  text.DeleteBlock(SourceBlock(-100,2,-100,4));
  Check(text.Lines[0]='Examline.', 'Block referencing invalid line');

 //Single char
  text.DeleteBlock(SourceBlock(0,3,0,4));
  Check(text.Lines[0]='Exaline.', 'Single char');

 //Last char + lots more
  text.DeleteBlock(SourceBlock(0,Length(text.Lines[0])-1,0,Length(text.Lines[0])+100));
  Check(text.Lines[0]='Exaline', 'Last char + more');

 //Before first char + first char
  text.DeleteBlock(SourceBlock(-10,-1000,0,1));
  Check(text.Lines.Count=1);
  Check(text.PropertyLines.Count=1);
  Check(text.Lines[0]='xaline', 'Before first char + first char');

 //Everything that remains, but the line should be kept
  text.DeleteBlock(SourceBlock(0,-1000,0,+1000));
  Check(text.Lines.Count=1);
  Check(text.PropertyLines.Count=1);
  Check(text.Lines[0]='', 'Everything but keep the line');

 //Everything that remains AND the line (as being strictly inside the block)
  text.DeleteBlock(SourceBlock(-1,-1000,+1,+1000));
  Check(text.Lines.Count=0);
  Check(text.PropertyLines.Count=0);
end;

procedure TWakanTextTestCase.LotsOfLines;
var i: integer;
begin
  for i := 0 to 8000 do
    text.AddLine('Line '+IntToStr(i));
  Check(text.Lines.Count=8001);
  Check(text.PropertyLines.Count=8001);
  Check(text.Lines[655]='Line 655');

 //Deleting
  text.DeleteLine(0);
  text.DeleteLine(0);
  text.DeleteLine(0);
  text.DeleteLine(text.Lines.Count-1);
  text.DeleteLine(text.Lines.Count-1);
  text.DeleteLine(text.Lines.Count-1);
  Check(text.Lines.Count=8001-6);
  Check(text.Lines[0]='Line 3');
  Check(text.Lines[8000-6]='Line 7997');

  randomize();
  for i := text.Lines.Count-2 downto 0 do
    text.DeleteLine(random(text.Lines.Count));
  Check(text.Lines.Count=1);
  Check(text.PropertyLines.Count=1);
end;

procedure TWakanTextTestCase.MiscSimple;
begin
  text.AddLine('Example text.');
  text.AddLine('Another line.');
  text.AddLine('Third line.');

 //GetDoc
  Check(text.GetDoc(0,0)='E');
  Check(text.GetDoc(1,0)='x');
  Check(text.GetDoc(1000,0)=UH_ZERO);
  Check(text.GetDoc(0,1)='A');
  Check(text.GetDoc(2,1)='o');
  Check(text.GetDoc(1,2)='h');

 //GetDocChain
  text.PropertyLines[0].chars[1].wordstate := '<';
  text.PropertyLines[0].chars[2].wordstate := '<';
  text.PropertyLines[0].chars[3].wordstate := '<';
  Check(text.GetDocChain(0,0)='Exam');
  Check(text.GetDocChain(1,0)='xam');
  Check(text.GetDocChain(11,0)='t');
  Check(text.GetDocChain(12,0)='.');

  text.PropertyLines[0].chars[12].wordstate := '<';
  Check(text.GetDocChain(11,0)='t.');
  Check(text.GetDocChain(12,0)='.');

 //Positions
  Check(text.EndOfDocument=SourcePos(Length(text.Lines[text.Lines.Count-1]), text.Lines.Count-1));
  Check(text.EndOfLine(text.Lines.Count-1)=text.EndOfDocument);
  Check(text.EndOfLine(0)=SourcePos(Length(text.Lines[0]),0));

end;


function WakanTextTests: ITestSuite;
var ASuite: TTestSuite;
begin
  ASuite := TTestSuite.create('JWBWakanText');
  ASuite.addTest(TSourcePosTestCase.Suite);
  ASuite.addTest(TSourceBlockTestCase.Suite);
  ASuite.addTest(TWakanTextTestCase.Suite);
  Result := ASuite;
end;

initialization
  RegisterTest(WakanTextTests);

end.
