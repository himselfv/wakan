program Jalet;
{
Define for the whole project:
  UNICODE -- defined by the compiler if the default string type is UnicodeString
  CLEAN_DEINIT -- delete and destroy everything properly, otherwise we cheat to speed up unloading (who cares!)

Disable ScaleMM2 when tracking a memory bug (default memory mgr is better at reporting those)
}

uses
  ScaleMM2 in 'Components\ScaleMM\ScaleMM2.pas',
  Forms,
  JWBStrings in 'Components\JWBStrings.pas',
  PackageCommon in 'Components\Package\PackageCommon.pas',
  TextTable in 'Components\TextTable\TextTable.pas',
  MemSource in 'Components\Package\MemSource.pas',
  PKGWrite in 'Components\Package\PKGWrite.pas' {PKGWriteForm},
  StdPrompt in 'Components\StdPrompt\StdPrompt.pas' {SMPromptForm},
  UnicodeFont in 'Forms\UnicodeFont.pas' {fSelectFont},
  JWBUnit in 'Components\JWBUnit.pas',
  JWBCategories in 'Components\JWBCategories.pas',
  JWBDicSearch in 'Components\JWBDicSearch.pas',
  JWBKanjiCard in 'Components\JWBKanjiCard.pas',
  JWBMenu in 'Forms\JWBMenu.pas' {fMenu},
  JWBKanji in 'Forms\JWBKanji.pas' {fKanji},
  JWBRadical in 'Forms\JWBRadical.pas' {fRadical},
  JWBWordLookup in 'Forms\JWBWordLookup.pas' {fWordLookup},
  JWBSettings in 'Forms\JWBSettings.pas' {fSettings},
  JWBSplash in 'Forms\JWBSplash.pas' {fSplash},
  JWBVocab in 'Forms\JWBVocab.pas' {fVocab},
  JWBVocabDetails in 'Forms\JWBVocabDetails.pas' {fVocabDetails},
  JWBVocabAdd in 'Forms\JWBVocabAdd.pas' {fVocabAdd},
  JWBVocabFilters in 'Forms\JWBVocabFilters.pas' {fVocabFilters},
  JWBNewCategory in 'Forms\JWBNewCategory.pas' {fNewCategory},
  JWBPrint in 'Forms\JWBPrint.pas' {fPrint},
  JWBStatistics in 'Forms\JWBStatistics.pas' {fStatistics},
  JWBWordList in 'Forms\JWBWordList.pas' {fWordList},
  JWBBitmap in 'Forms\JWBBitmap.pas' {fBitmap},
  JWBKanjiSearch in 'Forms\JWBKanjiSearch.pas' {fKanjiSearch},
  JWBKanjiCompounds in 'Forms\JWBKanjiCompounds.pas' {fKanjiCompounds},
  JWBKanjiDetails in 'Forms\JWBKanjiDetails.pas' {fKanjiDetails},
  JWBEditor in 'Forms\JWBEditor.pas' {fEditor},
  JWBWordKanji in 'Forms\JWBWordKanji.pas' {fWordKanji},
  JWBExamples in 'Forms\JWBExamples.pas' {fExamples},
  JWBDictMan in 'Forms\JWBDictMan.pas' {fDictMan},
  JWBDictImport in 'Forms\JWBDictImport.pas' {fDictImport},
  JWBDictCoding in 'Forms\JWBDictCoding.pas' {fDictCoding},
  JWBHint in 'Forms\JWBHint.pas' {fHint},
  JWBCharItem in 'Forms\JWBCharItem.pas' {fCharItem},
  JWBScreenTip in 'Forms\JWBScreenTip.pas' {fScreenTip},
  JWBInvalidator in 'Forms\JWBInvalidator.pas' {fInvalidator},
  JWBLanguage in 'Forms\JWBLanguage.pas' {fLanguage},
  JWBFileType in 'Forms\JWBFileType.pas' {fFileType},
  JWBWordsExpChoose in 'Forms\JWBWordsExpChoose.pas' {fWordsExpChoose},
  JWBMedia in 'Forms\JWBMedia.pas' {fMedia},
  JWBAnnotations in 'Components\JWBAnnotations.pas',
  JWBCommandLine in 'Components\JWBCommandLine.pas',
  JWBDic in 'Components\JWBDic.pas',
  JWBKanaConv in 'Components\JWBKanaConv.pas',
  JWBIndex in 'Components\JWBIndex.pas',
  JWBEdictMarkers in 'Components\Parsers\JWBEdictMarkers.pas',
  JWBEdictReader in 'Components\Parsers\JWBEdictReader.pas',
  JWBAutoImport in 'Components\JWBAutoImport.pas',
  JWBDownloader in 'Components\JWBDownloader.pas',
  JWBDownloadSources in 'Components\JWBDownloadSources.pas',
  JWBUserData in 'Components\JWBUserData.pas',
  JWBPortableMode in 'Forms\JWBPortableMode.pas' {fPortableMode},
  JWBCategoryMgr in 'Forms\JWBCategoryMgr.pas' {fCategoryMgr},
  JWBForms in 'Components\JWBForms.pas',
  JWBCharData in 'Components\JWBCharData.pas',
  JWBWakanText in 'Components\JWBWakanText.pas',
  JWBCharDataImport in 'Forms\JWBCharDataImport.pas' {fCharDataImport},
  RaineRadicals in 'Components\RaineRadicals.pas',
  JWBWordLookupBase in 'Forms\JWBWordLookupBase.pas' {fWordLookupBase};

{$R *.RES}


begin
  Application.Initialize;
  Application.Title := 'wakan';
  Application.HelpFile := 'wakan_en.chm';

{ Only static forms are auto-created. Dynamic forms are created when needed and
 destroyed after use.
 Some forms may be singletons and be created on the first use. }

  Application.CreateForm(TfMenu, fMenu);
  Application.CreateForm(TfKanji, fKanji);
  Application.CreateForm(TfWordLookup, fWordLookup);
  Application.CreateForm(TfSettings, fSettings);
  Application.CreateForm(TfVocab, fVocab);
  Application.CreateForm(TfKanjiSearch, fKanjiSearch);
  Application.CreateForm(TfWordLookupBase, fWordLookupBase);
  //this one
  Application.CreateForm(TfKanjiCompounds, fKanjiCompounds); //replace with wordlookup? //this one
  Application.CreateForm(TfKanjiDetails, fKanjiDetails);
  Application.CreateForm(TfEditor, fEditor);
  Application.CreateForm(TfWordKanji, fWordKanji);
  Application.CreateForm(TfExamples, fExamples);
  Application.CreateForm(TfVocabDetails, fVocabDetails);
  Application.CreateForm(TfVocabFilters, fVocabFilters); //this one
  Application.CreateForm(TfHint, fHint);

  fMenu.InitializeWakan;
  Application.Run;
end.
