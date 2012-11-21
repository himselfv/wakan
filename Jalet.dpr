program Jalet;

uses
  Forms,
  TextTable in 'Components\TextTable\TextTable.pas',
  MemSource in 'Components\Package\MemSource.pas',
  PKGWrite in 'Components\Package\PKGWrite.pas' {PKGWriteForm},
  StdPrompt in 'Components\StdPrompt\StdPrompt.pas' {SMPromptForm},
  UnicodeFont in 'UnicodeFont.pas' {fSelectFont},
  JWBMenu in 'JWBMenu.pas' {fMenu},
  JWBKanji in 'JWBKanji.pas' {fKanji},
  JWBRadical in 'JWBRadical.pas' {fRadical},
  JWBUnit in 'JWBUnit.pas',
  JWBUser in 'JWBUser.pas' {fUser},
  JWBSettings in 'JWBSettings.pas' {fSettings},
  JWBSplash in 'JWBSplash.pas' {fSplash},
  JWBAlphabet in 'JWBAlphabet.pas' {fAlphabet},
  JWBWords in 'JWBWords.pas' {fWords},
  JWBNewCategory in 'JWBNewCategory.pas' {fNewCategory},
  JWBPrint in 'JWBPrint.pas' {fPrint},
  JWBSizeCheck in 'JWBSizeCheck.pas' {fSizeCheck},
  JWBStatistics in 'JWBStatistics.pas' {fStatistics},
  JWBWordList in 'JWBWordList.pas' {fWordList},
  JWBWait in 'JWBWait.pas' {fWait},
  JWBBitmap in 'JWBBitmap.pas' {fBitmap},
  JWBClipboard in 'JWBClipboard.pas' {fClipboard},
  JWBKanjiSearch in 'JWBKanjiSearch.pas' {fKanjiSearch},
  JWBKanjiSort in 'JWBKanjiSort.pas' {fKanjiSort},
  JWBKanjiCompounds in 'JWBKanjiCompounds.pas' {fKanjiCompounds},
  JWBKanjiDetails in 'JWBKanjiDetails.pas' {fKanjiDetails},
  JWBTranslate in 'JWBTranslate.pas' {fTranslate},
  JWBWordDetails in 'JWBWordDetails.pas' {fWordDetails},
  JWBWordKanji in 'JWBWordKanji.pas' {fWordKanji},
  JWBWordAdd in 'JWBWordAdd.pas' {fWordAdd},
  JWBWordCategory in 'JWBWordCategory.pas' {fWordCategory},
  JWBUserDetails in 'JWBUserDetails.pas' {fUserDetails},
  JWBUserAdd in 'JWBUserAdd.pas' {fUserAdd},
  JWBUserFilters in 'JWBUserFilters.pas' {fUserFilters},
  JWBUserCategory in 'JWBUserCategory.pas' {fUserCategory},
  JWBLayout in 'JWBLayout.pas' {fLayout},
  JWBStrokeOrder in 'JWBStrokeOrder.pas' {fStrokeOrder},
  JWBDictMan in 'JWBDictMan.pas' {fDictMan},
  JWBDictImport in 'JWBDictImport.pas' {fDictImport},
  JWBDictCoding in 'JWBDictCoding.pas' {fDictCoding},
  JWBHint in 'JWBHint.pas' {fHint},
  JWBCharItem in 'JWBCharItem.pas' {fCharItem},
  JWBScreenTip in 'JWBScreenTip.pas' {fScreenTip},
  JWBInvalidator in 'JWBInvalidator.pas' {fInvalidator},
  JWBDicAdd in 'JWBDicAdd.pas' {fDicAdd},
  JWBLanguage in 'JWBLanguage.pas' {fLanguage},
  JWBPopupButton in 'JWBPopupButton.pas' {fPopupButton},
  JWBConvertTbl in 'JWBConvertTbl.pas',
  JWBConvert in 'JWBConvert.pas',
  JWBFileType in 'JWBFileType.pas' {fFileType},
  JWBWordsExpChoose in 'JWBWordsExpChoose.pas' {fWordsExpChoose},
  JWBMedia in 'JWBMedia.pas' {fMedia},
  JWBUtils in 'JWBUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'wakan';
  Application.HelpFile := 'C:\Projects\Jalet\wakan_en.chm';
  Application.CreateForm(TfMenu, fMenu);
  Application.CreateForm(TfKanji, fKanji);
  Application.CreateForm(TfRadical, fRadical);
  Application.CreateForm(TfUser, fUser);
  Application.CreateForm(TfSettings, fSettings);
  Application.CreateForm(TfSplash, fSplash);
  Application.CreateForm(TPKGWriteForm, PKGWriteForm);
  Application.CreateForm(TfAlphabet, fAlphabet);
  Application.CreateForm(TfSelectFont, fSelectFont);
  Application.CreateForm(TfWords, fWords);
  Application.CreateForm(TfNewCategory, fNewCategory);
  Application.CreateForm(TfPrint, fPrint);
  Application.CreateForm(TfSizeCheck, fSizeCheck);
  Application.CreateForm(TfStatistics, fStatistics);
  Application.CreateForm(TfWordList, fWordList);
  Application.CreateForm(TfWait, fWait);
  Application.CreateForm(TfBitmap, fBitmap);
  Application.CreateForm(TfClipboard, fClipboard);
  Application.CreateForm(TfKanjiSearch, fKanjiSearch);
  Application.CreateForm(TfKanjiSort, fKanjiSort);
  Application.CreateForm(TfKanjiCompounds, fKanjiCompounds);
  Application.CreateForm(TfKanjiDetails, fKanjiDetails);
  Application.CreateForm(TfTranslate, fTranslate);
  Application.CreateForm(TfWordDetails, fWordDetails);
  Application.CreateForm(TfWordKanji, fWordKanji);
  Application.CreateForm(TfWordAdd, fWordAdd);
  Application.CreateForm(TfWordCategory, fWordCategory);
  Application.CreateForm(TfUserDetails, fUserDetails);
  Application.CreateForm(TfUserAdd, fUserAdd);
  Application.CreateForm(TfUserFilters, fUserFilters);
  Application.CreateForm(TfUserCategory, fUserCategory);
  Application.CreateForm(TfLayout, fLayout);
  Application.CreateForm(TfStrokeOrder, fStrokeOrder);
  Application.CreateForm(TfDictMan, fDictMan);
  Application.CreateForm(TfDictImport, fDictImport);
  Application.CreateForm(TfDictCoding, fDictCoding);
  Application.CreateForm(TfHint, fHint);
  Application.CreateForm(TfCharItem, fCharItem);
  Application.CreateForm(TfInvalidator, fInvalidator);
  Application.CreateForm(TfDicAdd, fDicAdd);
  Application.CreateForm(TfLanguage, fLanguage);
  Application.CreateForm(TfFileType, fFileType);
  Application.CreateForm(TfWordsExpChoose, fWordsExpChoose);
  Application.CreateForm(TfMedia, fMedia);
  Application.Run;
end.
