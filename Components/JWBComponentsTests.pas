unit JWBComponentsTests;

interface
uses TestFramework, JWBComponents;

type
  TComponentsTestCase = class(TTestCase)
  public
    FComponents: TAppComponents;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadComponentsRegistry;
  end;

implementation
uses SysUtils, TestingCommon;

procedure TComponentsTestCase.SetUp;
begin
  FComponents := TAppComponents.Create;
end;

procedure TComponentsTestCase.TearDown;
begin
  FreeAndNil(FComponents);
end;

procedure TComponentsTestCase.LoadComponentsRegistry;
begin
  FComponents.LoadFromFile(CommonDataDir+'\Components.ini');
  Check(FComponents.Count > 0, 'No components loaded');
  Check(FComponents.FindByName('EDICT') <> nil);
  Check(FComponents.FindByName('EDICT2') <> nil);
  Check(FComponents.FindByName('CCEDICT') <> nil);
  Check(FComponents.FindByName('KANJIDIC') <> nil);
  Check(FComponents.FindByName('Unihan') <> nil);
  Check(FComponents.FindByName('examples_j.pkg') <> nil);
  Check(FComponents.FindByName('RADKFILE') <> nil);
  Check(FComponents.FindByName('WORDFREQ_CK') <> nil);
end;


initialization
  RegisterTest(TComponentsTestCase.Suite);

end.
