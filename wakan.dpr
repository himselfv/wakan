program wakan;
{
Define for the whole project:
  UNICODE -- defined by the compiler if the default string type is UnicodeString
  CLEAN_DEINIT -- delete and destroy everything properly, otherwise we cheat to speed up unloading (who cares!)

Disable ScaleMM2 when tracking a memory bug (default memory mgr is better at reporting those)
}

uses
  ScaleMM2 in 'Modules\ScaleMM\ScaleMM2.pas',
  Forms,
  JWBStrings in 'Modules\JWBStrings.pas',
  TextTable in 'Modules\Package\TextTable.pas',
  PackageCommon in 'Modules\Package\PackageCommon.pas',
  MemSource in 'Modules\Package\MemSource.pas',
  PKGWrite in 'Modules\Package\PKGWrite.pas' {PKGWriteForm},
  JWBMenu in 'Modules\JWBMenu.pas' {fMenu},
  JWBSplash in 'Modules\JWBSplash.pas' {fSplash},
  JWBUnit in 'Modules\JWBUnit.pas',
  JWBCore in 'Modules\JWBCore.pas',
  TaskbarCtl in 'Modules\TaskbarCtl.pas',
  JWBClipboard in 'Modules\JWBClipboard.pas',
  JWBIntTip in 'Modules\JWBIntTip.pas',
  JWBStatistics in 'Modules\JWBStatistics.pas' {fStatistics},
  JWBWordKanji in 'Modules\JWBWordKanji.pas' {fWordKanji},
  JWBExamples in 'Modules\JWBExamples.pas' {fExamples},
  JWBScreenTip in 'Modules\JWBScreenTip.pas' {fScreenTipForm},
  JWBLanguage in 'Modules\JWBLanguage.pas' {fLanguage},
  JWBFileType in 'Modules\JWBFileType.pas' {fFileType},
  JWBCommandLine in 'Modules\JWBCommandLine.pas',
  JWBForms in 'Modules\JWBForms.pas',
  JWBRefLinks in 'Modules\JWBRefLinks.pas',
  AppData in 'Modules\LocalData\AppData.pas',
  JWBPortableMode in 'Modules\LocalData\JWBPortableMode.pas' {fPortableMode},
  PortabilitySettings in 'Modules\LocalData\PortabilitySettings.pas' {PortabilitySettingsPage},
  UpgradeFiles in 'Modules\LocalData\UpgradeFiles.pas' {fUpgradeFiles},
  JWBSettings in 'Modules\Settings\JWBSettings.pas' {fSettings},
  UnicodeFont in 'Modules\Settings\UnicodeFont.pas' {fSelectFont},
  JWBCharItem in 'Modules\Settings\JWBCharItem.pas' {fCharItem},
  JWBUserData in 'Modules\UserData\JWBUserData.pas',
  JWBCategories in 'Modules\UserData\JWBCategories.pas',
  JWBCategoryMgr in 'Modules\UserData\JWBCategoryMgr.pas' {fCategoryMgr},
  JWBNewCategory in 'Modules\UserData\JWBNewCategory.pas' {fNewCategory},
  JWBCharData in 'Modules\CharData\JWBCharData.pas',
  JWBCharDataImport in 'Modules\CharData\JWBCharDataImport.pas' {fCharDataImport},
  JWBKanjiList in 'Modules\KanjiList\JWBKanjiList.pas' {fKanji},
  JWBKanjiCard in 'Modules\KanjiList\JWBKanjiCard.pas',
  JWBKanjiDetails in 'Modules\KanjiList\JWBKanjiDetails.pas' {fKanjiDetails},
  StrokeOrder in 'Modules\KanjiList\StrokeOrder.pas',
  RaineRadicals in 'Modules\KanjiList\RaineRadicals.pas',
  JWBRadicalList in 'Modules\KanjiList\JWBRadicalList.pas' {fRadical},
  JWBEditor in 'Modules\Editor\JWBEditor.pas' {fEditor},
  JWBEditorHint in 'Modules\Editor\JWBEditorHint.pas' {fEditorHint},
  JWBWakanText in 'Modules\Editor\JWBWakanText.pas',
  JWBDictMan in 'Modules\Dictionary\JWBDictMan.pas' {fDictMan},
  JWBDictImport in 'Modules\Dictionary\JWBDictImport.pas' {fDictImport},
  JWBDicImportJob in 'Modules\Dictionary\JWBDicImportJob.pas',
  JWBDictionaries in 'Modules\Dictionary\JWBDictionaries.pas',
  JWBDic in 'Modules\Dictionary\JWBDic.pas',
  JWBIndex in 'Modules\Dictionary\JWBIndex.pas',
  JWBDicSearch in 'Modules\Dictionary\JWBDicSearch.pas',
  JWBEdictMarkers in 'Modules\Dictionary\JWBEdictMarkers.pas',
  JWBWordGrid in 'Modules\Dictionary\JWBWordGrid.pas',
  JWBWordLookupBase in 'Modules\Dictionary\JWBWordLookupBase.pas' {fWordLookupBase},
  JWBWordLookup in 'Modules\Dictionary\JWBWordLookup.pas' {fWordLookup},
  JWBKanjiCompounds in 'Modules\Dictionary\JWBKanjiCompounds.pas' {fKanjiCompounds},
  JWBVocab1 in 'Modules\Vocab\JWBVocab1.pas',
  JWBVocab in 'Modules\Vocab\JWBVocab.pas' {fVocab},
  JWBVocabDetails in 'Modules\Vocab\JWBVocabDetails.pas' {fVocabDetails},
  JWBVocabAdd in 'Modules\Vocab\JWBVocabAdd.pas' {fVocabAdd},
  JWBVocabFilters in 'Modules\Vocab\JWBVocabFilters.pas' {fVocabFilters},
  JWBWordsExpChoose in 'Modules\Vocab\JWBWordsExpChoose.pas' {fWordsExpChoose},
  JWBLegacyMarkup in 'Modules\Vocab\JWBLegacyMarkup.pas',
  JWBAutoImport in 'Modules\Components\JWBAutoImport.pas',
  JWBDownloader in 'Modules\Components\JWBDownloader.pas' {fDownloader},
  JWBDownloaderCore in 'Modules\Components\JWBDownloaderCore.pas',
  JWBComponents in 'Modules\Components\JWBComponents.pas',
  JWBJobs in 'Modules\Components\JWBJobs.pas',
  JWBUnpackJob in 'Modules\Components\JWBUnpackJob.pas',
  SevenZipUtils in 'Modules\Components\SevenZipUtils.pas',
  JWBPrint in 'Modules\Print\JWBPrint.pas' {fPrint},
  JWBBitmap in 'Modules\Print\JWBBitmap.pas' {fBitmap},
  JWBAnnotations in 'Modules\Annotations\JWBAnnotations.pas',
  JWBMedia in 'Modules\Annotations\JWBMedia.pas' {fMedia},
  AnnotationsSettings in 'Modules\Annotations\AnnotationsSettings.pas' {AnnotationsSettingsPage},
  JWBWordList in 'Modules\JWBWordList.pas' {fWordList},
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
  Application.CreateForm(TfSettings, fSettings);
  Application.CreateForm(TfKanji, fKanji);
  Application.CreateForm(TfKanjiDetails, fKanjiDetails);
  Application.CreateForm(TfWordLookup, fWordLookup);
  Application.CreateForm(TfKanjiCompounds, fKanjiCompounds);
  Application.CreateForm(TfWordKanji, fWordKanji);
  Application.CreateForm(TfVocab, fVocab);
  Application.CreateForm(TfVocabDetails, fVocabDetails);
  Application.CreateForm(TfVocabFilters, fVocabFilters);
  Application.CreateForm(TfEditor, fEditor);
  Application.CreateForm(TfExamples, fExamples);
  PortabilitySettings.Register;
  AnnotationsSettings.Register;

  fMenu.InitializeWakan;
  Application.Run;
end.
