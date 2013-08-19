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
  end;

  TLinesTestCase = class(TWakanTextTestCase)
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

  TEncodingTestCase = class(TWakanTextTestCase)
  protected
    function GetTestFilename(const AFilename: string): string;
    procedure LoadFile(const AFilename: string; AEncoding: byte);
    procedure VerifyText(const AFilename: string; AEncoding: byte);
    procedure VerifyAscii(const AFilename: string; AEncoding: byte);
    procedure VerifyAcp(const AFilename: string; AEncoding: byte);
    procedure LoadSaveCompare(const AFilename: string; AEncoding: byte; ABom: boolean);
    function CompareFiles(const AFilename1, AFilename2: string): boolean;
  published
    procedure Ascii;
    procedure UTF8;
    procedure UTF8Sign;
    procedure UTF16LE;
    procedure UTF16LESign;
    procedure UTF16BE;
    procedure UTF16BESign;
    procedure EUC;
    procedure ShiftJis;
    procedure Acp;

    procedure GuessAscii;
    procedure GuessUTF8;
    procedure GuessUTF8Sign;
    procedure GuessUTF16LE;
    procedure GuessUTF16LESign;
    procedure GuessUTF16BE;
    procedure GuessUTF16BESign;
    procedure GuessEUC;
    procedure GuessShiftJis;

    procedure SaveAscii;
    procedure SaveUTF8;
    procedure SaveUTF8Sign;
    procedure SaveUTF16LE;
    procedure SaveUTF16LESign;
    procedure SaveUTF16BE;
    procedure SaveUTF16BESign;
    procedure SaveEUC;
    procedure SaveShiftJis;
    procedure SaveAcp;

  end;

  TFormatTestCase = class(TWakanTextTestCase)
  published
  end;

//TODO: Loading from examples (Wakan text, ruby text)
//TODO: Saving to temp then reloading of Wakan text, ruby text.
//TODO: Pasting.
//TODO: Expanding Ruby

implementation
uses SysUtils, Classes, JWBStrings, JWBConvert, StreamUtils;

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

procedure TLinesTestCase.BaseState;
begin
  Check(text.Lines<>nil);
  Check(text.PropertyLines<>nil);
  Check(text.Lines.Count=0);
  Check(text.PropertyLines.Count=0);
end;

procedure TLinesTestCase.Clearing;
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

procedure TLinesTestCase.AddingLines;
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

procedure TLinesTestCase.SplittingLines;
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

procedure TLinesTestCase.DeletingCharacters;
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

procedure TLinesTestCase.DeletingLines;
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

procedure TLinesTestCase.DeletingBlocks;
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

procedure TLinesTestCase.LotsOfLines;
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

procedure TLinesTestCase.MiscSimple;
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


function TEncodingTestcase.GetTestFilename(const AFilename: string): string;
begin
 //All encoding test files are stored in the same folder
  Result := 'Tests\encoding\'+AFilename;
end;

procedure TEncodingTestCase.LoadFile(const AFilename: string; AEncoding: byte);
begin
  if AEncoding=FILETYPE_UNKNOWN then
    Check(Conv_DetectTypeEx('Tests\encoding\'+AFilename, AEncoding)
      or (AEncoding<>FILETYPE_UNKNOWN),
      'Cannot guess file encoding.');

 //All encoding test files are stored in the same folder
  text.LoadText(GetTestFilename(AFilename), AEncoding, amNone);
end;

{ All unicode text files contain the same text, so we run the same series of tests
to verify that they were decoded properly.
Expand the text to include various corner cases. Do not cover Ruby or any extended
parsing here. }
procedure TEncodingTestCase.VerifyText(const AFilename: string; AEncoding: byte);
begin
  LoadFile(AFilename, AEncoding);
  Check(text.Lines.Count=3);
  Check(text.PropertyLines.Count=3);
  Check(text.Lines[0].StartsWith('世間体を気にするのは'));
  Check(text.Lines[2].EndsWith('女子中学生だ。'));
end;

{ Ascii text is different since it can't contain unicode }
procedure TEncodingTestCase.VerifyAscii(const AFilename: string; AEncoding: byte);
begin
  LoadFile(AFilename, AEncoding);
  Check(text.Lines.Count=3);
  Check(text.PropertyLines.Count=3);
  Check(text.Lines[0].StartsWith('Example ansi'));
  Check(text.Lines[2].EndsWith('other line.'));
end;

{ With ACP we cannot verify ACP text because the active codepage can be different
 on the PC where tests are run, but at least we check what we can }
procedure TEncodingTestCase.VerifyAcp(const AFilename: string; AEncoding: byte);
begin
  LoadFile(AFilename, AEncoding);
  Check(text.Lines.Count=4);
  Check(text.PropertyLines.Count=4);
  Check(text.Lines[0].StartsWith('Example ansi'));
  Check(text.Lines[2].EndsWith('other line.'));
end;

{ Load the file, save it in the same encoding to a temporary folder and then
 compare byte-by-byte to the original file.
 Realistically, there will be cases when some of the nuances are lost. If this
 happens another test might be needed to load the file back and compare as data }
procedure TEncodingTestCase.LoadSaveCompare(const AFilename: string;
  AEncoding: byte; ABom: boolean);
var tempDir: string;
  stream: TStream;
begin
  LoadFile(AFilename, AEncoding);
  tempDir := CreateRandomTempDir();
  try
    stream := TFileStream.Create(tempDir+'\'+AFilename, fmCreate);
    try
      text.SaveText(amDefault, TTextSaveFormat.Create(AEncoding, not ABom),
        stream);
    finally
      FreeAndNil(stream);
    end;
    Check(CompareFiles(GetTestFilename(AFilename), tempDir+'\'+AFilename));
  finally
    DeleteDirectory(tempDir);
  end;
end;

{ Binary comparison for files }
function TEncodingTestCase.CompareFiles(const AFilename1, AFilename2: string): boolean;
var f1, f2: TStream;
  b1, b2: byte;
  r1, r2: boolean;
begin
  f1 := nil;
  f2 := nil;
  try
    f1 := TStreamReader.Create(
      TFileStream.Create(AFilename1, fmOpenRead),
      true
    );
    f2 := TStreamReader.Create(
      TFileStream.Create(AFilename2, fmOpenRead),
      true
    );

   { Can be easily made somewhat faster by reading in dwords and comparing
    only the number of bytes read (e.g. case 1: 2: 3: 4: if (d1 & $000F) == etc.) }
    Result := true;
    while true do begin
      r1 := (f1.Read(b1,1)=1);
      r2 := (f2.Read(b2,1)=1);
      if r1 xor r2 then
        Result := false;
      if b1<>b2 then
        Result := false;
      if not r1 then
        break;
    end;

  finally
    FreeAndNil(f2);
    FreeAndNil(f1);
  end;
end;

{ Simple loading }
procedure TEncodingTestCase.Ascii;
begin
  VerifyAscii('ascii.txt', FILETYPE_ASCII);
end;

procedure TEncodingTestCase.UTF8;
begin
  VerifyText('utf8.txt', FILETYPE_UTF8);
end;

procedure TEncodingTestCase.UTF8Sign;
begin
  VerifyText('utf8sign.txt', FILETYPE_UTF8);
end;

procedure TEncodingTestCase.UTF16LE;
begin
  VerifyText('utf16le.txt', FILETYPE_UTF16LE);
end;

procedure TEncodingTestCase.UTF16LESign;
begin
  VerifyText('utf16lesign.txt', FILETYPE_UTF16LE);
end;

procedure TEncodingTestCase.UTF16BE;
begin
  VerifyText('utf16be.txt', FILETYPE_UTF16BE);
end;

procedure TEncodingTestCase.UTF16BESign;
begin
  VerifyText('utf16besign.txt', FILETYPE_UTF16BE);
end;

procedure TEncodingTestCase.EUC;
begin
  VerifyText('euc.txt', FILETYPE_EUC);
end;

procedure TEncodingTestCase.ShiftJis;
begin
  VerifyText('shiftjis.txt', FILETYPE_SJS);
end;

procedure TEncodingTestCase.Acp;
begin
  VerifyAcp('acp.txt', FILETYPE_ACP);
end;

{ Guess* versions make converter guess the encoding }
procedure TEncodingTestCase.GuessAscii;
begin
  VerifyAscii('ascii.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessUTF8;
begin
  VerifyText('utf8.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessUTF8Sign;
begin
  VerifyText('utf8sign.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessUTF16LE;
begin
  VerifyText('utf16le.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessUTF16LESign;
begin
  VerifyText('utf16lesign.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessUTF16BE;
begin
  VerifyText('utf16be.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessUTF16BESign;
begin
  VerifyText('utf16besign.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessEUC;
begin
  VerifyText('euc.txt', FILETYPE_UNKNOWN);
end;

procedure TEncodingTestCase.GuessShiftJis;
begin
  VerifyText('shiftjis.txt', FILETYPE_UNKNOWN);
end;

{ Save* to load, then save, then compare to original file }
procedure TEncodingTestCase.SaveAscii;
begin
  LoadSaveCompare('ascii.txt', FILETYPE_ASCII, false);
end;

procedure TEncodingTestCase.SaveUTF8;
begin
  LoadSaveCompare('utf8.txt', FILETYPE_UTF8, false);
end;

procedure TEncodingTestCase.SaveUTF8Sign;
begin
  LoadSaveCompare('utf8sign.txt', FILETYPE_UTF8, true);
end;

procedure TEncodingTestCase.SaveUTF16LE;
begin
  LoadSaveCompare('utf16le.txt', FILETYPE_UTF16LE, false);
end;

procedure TEncodingTestCase.SaveUTF16LESign;
begin
  LoadSaveCompare('utf16lesign.txt', FILETYPE_UTF16LE, true);
end;

procedure TEncodingTestCase.SaveUTF16BE;
begin
  LoadSaveCompare('utf16be.txt', FILETYPE_UTF16BE, false);
end;

procedure TEncodingTestCase.SaveUTF16BESign;
begin
  LoadSaveCompare('utf16besign.txt', FILETYPE_UTF16BE, true);
end;

procedure TEncodingTestCase.SaveEUC;
begin
  LoadSaveCompare('euc.txt', FILETYPE_EUC, false);
end;

procedure TEncodingTestCase.SaveShiftJis;
begin
  LoadSaveCompare('shiftjis.txt', FILETYPE_SJS, false);
end;

procedure TEncodingTestCase.SaveAcp;
begin
  LoadSaveCompare('acp.txt', FILETYPE_ACP, false);
end;



function WakanTextTests: ITestSuite;
var ASuite: TTestSuite;
begin
  ASuite := TTestSuite.create('JWBWakanText');
  ASuite.addTest(TSourcePosTestCase.Suite);
  ASuite.addTest(TSourceBlockTestCase.Suite);
  ASuite.addTest(TLinesTestCase.Suite);
  ASuite.addTest(TEncodingTestCase.Suite);
  Result := ASuite;
end;

initialization
  RegisterTest(WakanTextTests);

end.
