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
  JWBKanjiCompounds in 'Forms\JWBKanjiCompounds.pas' {fKanjiCompounds},
  JWBKanjiDetails in 'Forms\JWBKanjiDetails.pas' {fKanjiDetails},
  JWBEditor in 'Forms\JWBEditor.pas' {fEditor},
  JWBEditorHint in 'Forms\JWBEditorHint.pas' {fEditorHint},
  JWBWordKanji in 'Forms\JWBWordKanji.pas' {fWordKanji},
  JWBExamples in 'Forms\JWBExamples.pas' {fExamples},
  JWBDictMan in 'Forms\JWBDictMan.pas' {fDictMan},
  JWBDictImport in 'Forms\JWBDictImport.pas' {fDictImport},
  JWBCharItem in 'Forms\JWBCharItem.pas' {fCharItem},
  JWBScreenTip in 'Forms\JWBScreenTip.pas' {fScreenTipForm},
  JWBLanguage in 'Forms\JWBLanguage.pas' {fLanguage},
  JWBFileType in 'Forms\JWBFileType.pas' {fFileType},
  JWBWordsExpChoose in 'Forms\JWBWordsExpChoose.pas' {fWordsExpChoose},
  JWBMedia in 'Forms\JWBMedia.pas' {fMedia},
  JWBPortableMode in 'Forms\JWBPortableMode.pas' {fPortableMode},
  JWBCategoryMgr in 'Forms\JWBCategoryMgr.pas' {fCategoryMgr},
  JWBCharDataImport in 'Forms\JWBCharDataImport.pas' {fCharDataImport},
  JWBWordLookupBase in 'Forms\JWBWordLookupBase.pas' {fWordLookupBase},
  JWBDownloader in 'Forms\JWBDownloader.pas' {fDownloader},
  JWBAnnotations in 'Components\JWBAnnotations.pas',
  JWBCommandLine in 'Components\JWBCommandLine.pas',
  JWBDic in 'Components\JWBDic.pas',
  JWBKanaConv in 'Components\JWBKanaConv.pas',
  JWBIndex in 'Components\JWBIndex.pas',
  JWBEdictMarkers in 'Components\JWBEdictMarkers.pas',
  JWBAutoImport in 'Components\JWBAutoImport.pas',
  JWBDownloaderCore in 'Components\JWBDownloaderCore.pas',
  JWBComponents in 'Components\JWBComponents.pas',
  JWBUserData in 'Components\JWBUserData.pas',
  JWBForms in 'Components\JWBForms.pas',
  JWBCharData in 'Components\JWBCharData.pas',
  JWBWakanText in 'Components\JWBWakanText.pas',
  RaineRadicals in 'Components\RaineRadicals.pas',
  JWBLegacyMarkup in 'Components\JWBLegacyMarkup.pas',
  JWBRefLinks in 'Components\JWBRefLinks.pas',
  JWBJobs in 'Components\JWBJobs.pas',
  JWBCore in 'Components\JWBCore.pas',
  JWBWordGrid in 'Components\JWBWordGrid.pas',
  JWBDicImportJob in 'Components\JWBDicImportJob.pas',
  JWBUnpackJob in 'Components\JWBUnpackJob.pas',
  TaskbarCtl in 'Components\TaskbarCtl.pas',
  JWBClipboard in 'Components\JWBClipboard.pas',
  JWBIntTip in 'Components\JWBIntTip.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.RES}
{$R WINXP.RES}

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
  Application.CreateForm(TfKanjiCompounds, fKanjiCompounds);
  Application.CreateForm(TfKanjiDetails, fKanjiDetails);
  Application.CreateForm(TfEditor, fEditor);
  Application.CreateForm(TfWordKanji, fWordKanji);
  Application.CreateForm(TfExamples, fExamples);
  Application.CreateForm(TfVocabDetails, fVocabDetails);
  Application.CreateForm(TfVocabFilters, fVocabFilters);
  Application.CreateForm(TfEditorHint, fEditorHint);
  fMenu.InitializeWakan;
  Application.Run;
end.
