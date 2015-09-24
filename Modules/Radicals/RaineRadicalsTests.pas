unit RaineRadicalsTests;

interface
uses TestFramework, RaineRadicals;

type
  TRaineRadicalsTestCase = class(TTestCase)
  public
    FRadicals: TRaineRadicals;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadRadKFile;
  end;

implementation
uses SysUtils, TestingCommon;

procedure TRaineRadicalsTestCase.SetUp;
begin
  FRadicals := TRaineRadicals.Create;
end;

procedure TRaineRadicalsTestCase.TearDown;
begin
  FreeAndNil(FRadicals);
end;

procedure TRaineRadicalsTestCase.LoadRadKFile;
begin
  FRadicals.LoadFromRadKFile(CommonDataDir+'\RADKFILE');
  Check(FRadicals.Count > 0, 'No radicals loaded');
  Check(FRadicals.FindRadical('卜') >= 0);
  Check(FRadicals.FindRadical('月') >= 0);
  Check(FRadicals.FindRadical('門') >= 0);
  Check(FRadicals.GetCharRadicals('免') <> '');
  Check(FRadicals.GetCharRadicals('無') <> '');
  Check(FRadicals.GetContainingChars('化') <> '');
  Check(FRadicals.HasRadical('一', '一'));
  Check(FRadicals.HasRadical('髟', '髟'));
end;

initialization
  RegisterTest(TRaineRadicalsTestCase.Suite);

end.
