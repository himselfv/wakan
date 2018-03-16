unit JWBDicSearchTests;

interface
uses SysUtils, Classes, TestFramework, JWBDicSearch, JWBDictionaries, JWBDicTests;

type
 { Base class for cases which test dictionary search }
  TDicSearchTestCase = class(TTestCase)
  protected
    FSearch: TDicSearchRequest;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TAutoTranslateTestCase = class(TDicSearchTestCase)
  protected
    function GetTestFilename: string;
    procedure ProcessTestLine(line: string);
  published
    procedure AutoTranslate;
  end;


implementation
uses JWBStrings, StreamUtils, Windows, TestingCommon, JWBSettings, JWBUnit;

//WARNING WARNING WARNING modifies global state
//Do not use with parallel processing.
procedure TDicSearchTestCase.SetUp;
begin
  curlang := 'j';
  fSettings.cbPreferUserWords.Checked := false;
  fSettings.cbPreferNounsAndVerbs.Checked := true;
  fSettings.cbPreferPolite.Checked := true;
  fSettings.cbPreferPopular.Checked := true;

  FSearch := TDicSearchRequest.Create();
  FSearch.st := stJapanese;
  FSearch.MatchType := mtBestGuessLeft;
  FSearch.MaxWords := 0;
  FSearch.AutoDeflex := true;
  FSearch.DictGroup := 1;
  FSearch.Prepare;
end;

procedure TDicSearchTestCase.TearDown;
begin
  FreeAndNil(FSearch);
end;

{ Autotranslate }

function TAutoTranslateTestCase.GetTestFilename: string;
begin
  Result := TestCasesDir+'\autotl.txt';
end;

procedure TAutoTranslateTestCase.AutoTranslate;
var lines: TStringList;
  i, j_pos: integer;
  line: string;
begin
  lines := TStringList.Create;
  try
    lines.LoadFromFile(Self.GetTestFilename);
    for i := 0 to lines.Count-1 do begin
      line := lines[i];

      //Cut the comment part
      j_pos := pos('#', line);
      if j_pos > 0 then
        delete(line, j_pos, MaxInt);

      //Skip empty
      line := Trim(line);
      if line = '' then continue;

      ProcessTestLine(line);
    end;
  finally
    FreeAndNil(lines);
  end;
end;


function EatNextPart(var str: string): string;
var i_pos: integer;
begin
  i_pos := pos(' ', str);
  if i_pos <= 0 then begin
    Result := str;
    str := '';
    exit;
  end;

  Result := copy(str, 1, i_pos-1);

  Inc(i_pos);
  while (i_pos <= Length(str)) and (str[i_pos]=' ') do
    Inc(i_pos);
  if i_pos <= Length(str) then
    str := copy(str, i_pos, MaxInt)
  else
    str := '';
end;

{
Reads an expression and a list of deflexed words in kana, into which it should
be decoded.
Verifies that it
A. Can be decoded as such (at every step there's a dictionary match as listed)
B. Will probably be decoded as such (the dictionary match at every step is high enough in the list)
}
procedure TAutoTranslateTestCase.ProcessTestLine(line: string);
var expr, pattern: string;
  sl: TSearchResults;
  i: integer;
  match: PSearchResult;
begin
  expr := EatNextPart(line);
  Check(expr<>'', 'Bad test case: expression can''t be empty');

  sl := TSearchResults.Create;
  try
    while expr <> '' do begin
      //Eat next pattern to check against
      pattern := EatNextPart(line);
      Check(pattern<>'', 'Unmatched remainder of the expression: "'+expr+'"');

      //Use '~remainder' to indicate where you don't expect more results.
      //We won't check that there are none though.
      if pattern[1] = '~' then begin
        delete(pattern, 1, 1);
        Check(pattern=expr, 'Pattern "'+pattern+'" doesn''t match the remainder expression "'+expr+'"');
        expr := '';
        continue;
      end;

      //Locate + decode next chunk
      sl.Clear;
      FSearch.Search(expr, sl);

      //Verify that the given chunk is present in the results
      match := nil;
      for i := 0 to sl.Count-1 do
        if sl[i].kana=pattern then begin
          match := sl[i];
          break;
        end;
      Check(match<>nil, 'Pattern "'+pattern+'" does not match any of the results');

      //Remove the detected chunk
      Check(match.inflen <= Length(expr));
      delete(expr, 1, match.inflen);
    end;

  finally
    FreeAndNil(sl);
  end;

  //Nothing should remain in the verification part
  Check(line='', 'Unmatched patterns remain: "'+line+'"');
end;


function DicSearchTests: ITestSuite;
var ASuite: TTestSuite;
begin
  ASuite := TTestSuite.create('JWBDicSearch');
  ASuite.addTest(TAutoTranslateTestCase.Suite);
  Result := ASuite;
end;

initialization
  RegisterTest(DicSearchTests);

end.
