program Jalet;

uses
  Forms,
  TextTable in 'Components\TextTable\TextTable.pas',
  MemSource in 'Components\Package\MemSource.pas',
  PKGWrite in 'Components\Package\PKGWrite.pas' {PKGWriteForm},
  StdPrompt in 'Components\StdPrompt\StdPrompt.pas' {SMPromptForm},
  UnicodeFont in 'Forms\UnicodeFont.pas' {fSelectFont}
  JWBUnit in 'Components\JWBUnit.pas',
  JWBConvertTbl in 'Components\JWBConvertTbl.pas',
  JWBConvert in 'Components\JWBConvert.pas',
  JWBUtils in 'Components\JWBUtils.pas',
  JWBDicSearch in 'Components\JWBDicSearch.pas',  
  JWBMenu in 'Forms\JWBMenu.pas' {fMenu},
  JWBKanji in 'Forms\JWBKanji.pas' {fKanji},
  JWBRadical in 'Forms\JWBRadical.pas' {fRadical},
  JWBUser in 'Forms\JWBUser.pas' {fUser},
  JWBSettings in 'Forms\JWBSettings.pas' {fSettings},
  JWBSplash in 'Forms\JWBSplash.pas' {fSplash},
  JWBAlphabet in 'Forms\JWBAlphabet.pas' {fAlphabet},
  JWBWords in 'Forms\JWBWords.pas' {fWords},
  JWBNewCategory in 'Forms\JWBNewCategory.pas' {fNewCategory},
  JWBPrint in 'Forms\JWBPrint.pas' {fPrint},
  JWBSizeCheck in 'Forms\JWBSizeCheck.pas' {fSizeCheck},
  JWBStatistics in 'Forms\JWBStatistics.pas' {fStatistics},
  JWBWordList in 'Forms\JWBWordList.pas' {fWordList},
  JWBWait in 'Forms\JWBWait.pas' {fWait},
  JWBBitmap in 'Forms\JWBBitmap.pas' {fBitmap},
  JWBClipboard in 'Forms\JWBClipboard.pas' {fClipboard},
  JWBKanjiSearch in 'Forms\JWBKanjiSearch.pas' {fKanjiSearch},
  JWBKanjiSort in 'Forms\JWBKanjiSort.pas' {fKanjiSort},
  JWBKanjiCompounds in 'Forms\JWBKanjiCompounds.pas' {fKanjiCompounds},
  JWBKanjiDetails in 'Forms\JWBKanjiDetails.pas' {fKanjiDetails},
  JWBTranslate in 'Forms\JWBTranslate.pas' {fTranslate},
  JWBWordDetails in 'Forms\JWBWordDetails.pas' {fWordDetails},
  JWBWordKanji in 'Forms\JWBWordKanji.pas' {fWordKanji},
  JWBWordAdd in 'Forms\JWBWordAdd.pas' {fWordAdd},
  JWBWordCategory in 'Forms\JWBWordCategory.pas' {fWordCategory},
  JWBUserDetails in 'Forms\JWBUserDetails.pas' {fUserDetails},
  JWBUserAdd in 'Forms\JWBUserAdd.pas' {fUserAdd},
  JWBUserFilters in 'Forms\JWBUserFilters.pas' {fUserFilters},
  JWBUserCategory in 'Forms\JWBUserCategory.pas' {fUserCategory},
  JWBLayout in 'Forms\JWBLayout.pas' {fLayout},
  JWBStrokeOrder in 'Forms\JWBStrokeOrder.pas' {fStrokeOrder},
  JWBDictMan in 'Forms\JWBDictMan.pas' {fDictMan},
  JWBDictImport in 'Forms\JWBDictImport.pas' {fDictImport},
  JWBDictCoding in 'Forms\JWBDictCoding.pas' {fDictCoding},
  JWBHint in 'Forms\JWBHint.pas' {fHint},
  JWBCharItem in 'Forms\JWBCharItem.pas' {fCharItem},
  JWBScreenTip in 'Forms\JWBScreenTip.pas' {fScreenTip},
  JWBInvalidator in 'Forms\JWBInvalidator.pas' {fInvalidator},
  JWBDicAdd in 'Forms\JWBDicAdd.pas' {fDicAdd},
  JWBLanguage in 'Forms\JWBLanguage.pas' {fLanguage},
  JWBPopupButton in 'Forms\JWBPopupButton.pas' {fPopupButton},
  JWBFileType in 'Forms\JWBFileType.pas' {fFileType},
  JWBWordsExpChoose in 'Forms\JWBWordsExpChoose.pas' {fWordsExpChoose},
  JWBMedia in 'Forms\JWBMedia.pas' {fMedia};

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
