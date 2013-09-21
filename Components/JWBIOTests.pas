unit JWBIOTests;

interface
uses SysUtils, Classes, TestFramework, JWBIO;

type
 { Wakan supports reading and writing text in a range of encodings, with or
  without BOM. }
  TEncodingTestCase = class(TTestCase)
  protected
    FLines: TStringList;
    function GetTestFilename(const AFilename: string): string;
    procedure LoadFile(const AShortFilename: string; AEncoding: CEncoding);
    procedure SaveFile(const AFilename: string; AEncoding: CEncoding; AWriteBom: boolean);
    procedure VerifyText(const AFilename: string; AEncoding: CEncoding);
    procedure VerifyAscii(const AFilename: string; AEncoding: CEncoding);
    procedure VerifyAcp(const AFilename: string; AEncoding: CEncoding);
    procedure LoadSaveCompare(const AFilename: string; AEncoding: CEncoding;
      ABom: boolean; const ATestComment: string = '');
  public
    procedure SetUp; override;
    procedure TearDown; override;
    property Lines: TStringList read FLines;
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

 { Writes ~11Mb through the encoder, outputs the time to Status().
  You need console test runner to see it. }
  TReadWriteSpeedTestCase = class(TTestCase)
  protected
    procedure TestSpeed(AEncoding: CEncoding);
  published
    procedure Ascii;
    procedure UTF8;
    procedure UTF16LE;
    procedure UTF16BE;
    procedure EUC;
    procedure ShiftJis;
    procedure Jis;
    procedure OldJis;
    procedure NECJis;
    procedure GB;
    procedure Big5;
    procedure Acp;

  end;

implementation
uses Windows, JWBStrings;

procedure TEncodingTestCase.Setup;
begin
  inherited;
  FLines := TStringList.Create;
end;

procedure TEncodingTestCase.TearDown;
begin
  FreeAndNil(FLines);
  inherited;
end;

function TEncodingTestcase.GetTestFilename(const AFilename: string): string;
begin
 //All encoding test files are stored in the same folder
  Result := 'Tests\encoding\'+AFilename;
end;

procedure TEncodingTestCase.LoadFile(const AShortFilename: string; AEncoding: CEncoding);
var conv: TStreamDecoder;
  AFilename: string;
  ln: string;
begin
  AFilename := GetTestFilename(AShortFilename);
  if AEncoding=nil then
    Check(Conv_DetectType(AFilename, AEncoding) or (AEncoding<>nil),
      'Cannot guess file encoding.');

  Lines.Clear;
  conv := OpenTextFile(AFilename, AEncoding);
  try
    conv.TrySkipBom;
    while conv.ReadLn(ln) do
      Lines.Add(ln);
  finally
    FreeAndNil(conv);
  end;
end;

procedure TEncodingTestCase.SaveFile(const AFilename: string; AEncoding: CEncoding;
  AWriteBom: boolean);
var conv: TStreamEncoder;
  i: integer;
begin
  conv := CreateTextFile(AFilename, AEncoding);
  try
    if AWriteBom then
      conv.WriteBom();
    for i := 0 to Lines.Count-2 do
      conv.WriteLn(Lines[i]);
    if Lines.Count>0 then
      conv.Write(Lines[Lines.Count-1]); //last line always without CRLF
  finally
    FreeAndNil(conv);
  end;
end;

{ All unicode text files contain the same text, so we run the same series of tests
to verify that they were decoded properly.
Expand the text to include various corner cases. Do not cover Ruby or any extended
parsing here. }
procedure TEncodingTestCase.VerifyText(const AFilename: string; AEncoding: CEncoding);
begin
  LoadFile(AFilename, AEncoding);
  Check(Lines.Count=3);
  Check(Lines[0].StartsWith('世間体を気にするのは'));
  Check(Lines[2].EndsWith('女子中学生だ。'));
end;

{ Ascii text is different since it can't contain unicode }
procedure TEncodingTestCase.VerifyAscii(const AFilename: string; AEncoding: CEncoding);
begin
  LoadFile(AFilename, AEncoding);
  Check(Lines.Count=3);
  Check(Lines[0].StartsWith('Example ansi'));
  Check(Lines[2].EndsWith('other line.'));
end;

{ With ACP we cannot verify ACP text because the active codepage can be different
 on the PC where tests are run, but at least we check what we can }
procedure TEncodingTestCase.VerifyAcp(const AFilename: string; AEncoding: CEncoding);
begin
  LoadFile(AFilename, AEncoding);
  Check(Lines.Count=4);
  Check(Lines[0].StartsWith('Example ansi'));
  Check(Lines[2].EndsWith('other line.'));
end;

{ Load the file, save it in the same encoding to a temporary folder and then
 compare byte-by-byte to the original file.
 Realistically, there will be cases when some of the nuances are lost. If this
 happens another test might be needed to load the file back and compare as data }
procedure TEncodingTestCase.LoadSaveCompare(const AFilename: string;
  AEncoding: CEncoding; ABom: boolean; const ATestComment: string = '');
var tempDir: string;
begin
  LoadFile(AFilename, AEncoding);
  tempDir := CreateRandomTempDir();
  try
    SaveFile(tempDir+'\'+AFilename, AEncoding, ABom);
    Check(CompareFiles(GetTestFilename(AFilename), tempDir+'\'+AFilename));
  finally
    DeleteDirectory(tempDir);
  end;
end;

{ Simple loading }
procedure TEncodingTestCase.Ascii;
begin
  VerifyAscii('ascii.txt', TAsciiEncoding);
end;

procedure TEncodingTestCase.UTF8;
begin
  VerifyText('utf8.txt', TUTF8Encoding);
end;

procedure TEncodingTestCase.UTF8Sign;
begin
  VerifyText('utf8sign.txt', TUTF8Encoding);
end;

procedure TEncodingTestCase.UTF16LE;
begin
  VerifyText('utf16le.txt', TUTF16LEEncoding);
end;

procedure TEncodingTestCase.UTF16LESign;
begin
  VerifyText('utf16lesign.txt', TUTF16LEEncoding);
end;

procedure TEncodingTestCase.UTF16BE;
begin
  VerifyText('utf16be.txt', TUTF16BEEncoding);
end;

procedure TEncodingTestCase.UTF16BESign;
begin
  VerifyText('utf16besign.txt', TUTF16BEEncoding);
end;

procedure TEncodingTestCase.EUC;
begin
  VerifyText('euc.txt', TEUCEncoding);
end;

procedure TEncodingTestCase.ShiftJis;
begin
  VerifyText('shiftjis.txt', TSJISEncoding);
end;

procedure TEncodingTestCase.Acp;
begin
  VerifyAcp('acp.txt', TACPEncoding);
end;

{ Guess* versions make converter guess the encoding }
procedure TEncodingTestCase.GuessAscii;
begin
  VerifyAscii('ascii.txt', nil);
end;

procedure TEncodingTestCase.GuessUTF8;
begin
  VerifyText('utf8.txt', nil);
end;

procedure TEncodingTestCase.GuessUTF8Sign;
begin
  VerifyText('utf8sign.txt', nil);
end;

procedure TEncodingTestCase.GuessUTF16LE;
begin
  VerifyText('utf16le.txt', nil);
end;

procedure TEncodingTestCase.GuessUTF16LESign;
begin
  VerifyText('utf16lesign.txt', nil);
end;

procedure TEncodingTestCase.GuessUTF16BE;
begin
  VerifyText('utf16be.txt', nil);
end;

procedure TEncodingTestCase.GuessUTF16BESign;
begin
  VerifyText('utf16besign.txt', nil);
end;

procedure TEncodingTestCase.GuessEUC;
begin
  VerifyText('euc.txt', nil);
end;

procedure TEncodingTestCase.GuessShiftJis;
begin
  VerifyText('shiftjis.txt', nil);
end;

{ Save* to load, then save, then compare to original file }
procedure TEncodingTestCase.SaveAscii;
begin
  LoadSaveCompare('ascii.txt', TAsciiEncoding, false);
end;

procedure TEncodingTestCase.SaveUTF8;
begin
  LoadSaveCompare('utf8.txt', TUTF8Encoding, false);
end;

procedure TEncodingTestCase.SaveUTF8Sign;
begin
  LoadSaveCompare('utf8sign.txt', TUTF8Encoding, true);
end;

procedure TEncodingTestCase.SaveUTF16LE;
begin
  LoadSaveCompare('utf16le.txt', TUTF16LEEncoding, false);
end;

procedure TEncodingTestCase.SaveUTF16LESign;
begin
  LoadSaveCompare('utf16lesign.txt', TUTF16LEEncoding, true);
end;

procedure TEncodingTestCase.SaveUTF16BE;
begin
  LoadSaveCompare('utf16be.txt', TUTF16BEEncoding, false);
end;

procedure TEncodingTestCase.SaveUTF16BESign;
begin
  LoadSaveCompare('utf16besign.txt', TUTF16BEEncoding, true);
end;

procedure TEncodingTestCase.SaveEUC;
begin
  LoadSaveCompare('euc.txt', TEUCEncoding, false);
end;

procedure TEncodingTestCase.SaveShiftJis;
begin
  LoadSaveCompare('shiftjis.txt', TSJISEncoding, false);
end;

procedure TEncodingTestCase.SaveAcp;
begin
  LoadSaveCompare('acp.txt', TACPEncoding, false);
end;

procedure TReadWriteSpeedTestCase.TestSpeed(AEncoding: CEncoding);
var enc: TStreamEncoder;
  dec: TStreamDecoder;
  tempDir: string;
  tm: cardinal;
  i: integer;
  ln: string;
begin
  tempDir := CreateRandomTempDir();
  try
    tm := GetTickCount();
    enc := CreateTextFile(tempDir+'\test.txt', AEncoding);
    try
      enc.WriteBom;
      for i := 0 to 110000 do
        enc.WriteLn('私の家、とは、心の中ではあまり言いたくない羽川家のキッチンに'
          +'は、調理器具がとにかく多い。まな板は三枚あり、包丁も三本ある。');
    finally
      FreeAndNil(enc);
    end;
    tm := GetTickCount() - tm;
    Status('Write: '+IntToStr(tm)+' ticks.');

    tm := GetTickCount();
    dec := OpenTextFile(tempDir+'\test.txt', AEncoding);
    try
      i := 0;
      while dec.ReadLn(ln) do
        Inc(i);
      Check(i<>110001, 'Invalid number of files when reading back from file: '
        +IntToStr(i));
    finally
      FreeAndNil(dec);
    end;
    tm := GetTickCount() - tm;
    Status('Read: '+IntToStr(tm)+' ticks.');

  finally
    DeleteDirectory(tempDir);
  end;
end;

procedure TReadWriteSpeedTestCase.Ascii;
begin
  TestSpeed(TAsciiEncoding);
end;

procedure TReadWriteSpeedTestCase.UTF8;
begin
  TestSpeed(TUTF8Encoding);
end;

procedure TReadWriteSpeedTestCase.UTF16LE;
begin
  TestSpeed(TUTF16LEEncoding);
end;

procedure TReadWriteSpeedTestCase.UTF16BE;
begin
  TestSpeed(TUTF16BEEncoding);
end;

procedure TReadWriteSpeedTestCase.EUC;
begin
  TestSpeed(TEUCEncoding);
end;

procedure TReadWriteSpeedTestCase.ShiftJis;
begin
  TestSpeed(TSJISEncoding);
end;

procedure TReadWriteSpeedTestCase.Jis;
begin
  TestSpeed(TJISEncoding);
end;

procedure TReadWriteSpeedTestCase.OldJis;
begin
  TestSpeed(TOldJISEncoding);
end;

procedure TReadWriteSpeedTestCase.NECJis;
begin
  TestSpeed(TNecJISEncoding);
end;

procedure TReadWriteSpeedTestCase.GB;
begin
  TestSpeed(TGBEncoding);
end;

procedure TReadWriteSpeedTestCase.Big5;
begin
  TestSpeed(TBIG5Encoding);
end;

procedure TReadWriteSpeedTestCase.Acp;
begin
  TestSpeed(TACPEncoding);
end;

function JWBIOTestSuite: ITestSuite;
var ASuite: TTestSuite;
begin
  ASuite := TTestSuite.create('JWBIO Tests');
  ASuite.addTest(TEncodingTestCase.Suite);
  ASuite.addTest(TReadWriteSpeedTestCase.Suite);
  Result := ASuite;
end;

initialization
  RegisterTest(JWBIOTestSuite);

end.
