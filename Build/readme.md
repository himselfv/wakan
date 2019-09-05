## PREREQUISITES

1. Unicode Delphi (Delphi 2010+). Ansi support is being deprecated.
2. Jp-tools (https://github.com/himselfv/jptools) which Wakan uses heavily.
3. Inno Setup 5.


## BUILDING

1. Configure paths in setupvars.cmd (and Settings -> Environment, if building from Delphi).
2. Build and install WakanControls.dpr.
3. Copy the dependencies (see below) to the Release folder.
4. Run build.cmd to generate:

    * Binary files in Release
    * Installer in Build/Output.


## AUTOMATED RELEASES

Needs Python 2.7 with pywin32 and oauth2client.

Run release.cmd and authorize Wakan Release Uploader, then wait.
Your authorization is stored in upload.credentials, do not commit or share it.

Target folder on Google Drive is set in setupvars.cmd. By default it's Wakan release folder,
you need authorization to write there.
You may specify your own folder in setupvars.cmd.


## OPTIMIZATION

1. Enable inlining (inline:on or inline:auto).
It's very important that some critical functions are inlined for speed in release builds.
Therefore if you're doing the building, READ THIS:
http://docwiki.embarcadero.com/RADStudio/XE/en/Calling_Procedures_and_Functions#Using_the_inline_Directive
Note the list of cases when the inlining is not done.

2. Disable "Compiling> String format checking" in Delphi Project Options:
http://www.micro-isv.asia/2008/10/needless-string-checks-with-ensureunicodestring/
They shouldn't be needed, and even if by some mistake we put Ansi chars into UnicodeString, we better crash and fix that instead of hiding the bug.
Enabling this option also effectively disables the "const s:string" optimizations since functions will get refcount management frames for strings anyway.


## MAINTENANCE

Has to be done regularly to keep Wakan up to date:

 * Up the version number.
 * Import new EDICT/ENAMDICT files
 * Update EDICT marker list in JWBUnit.pas (see http://www.csse.monash.edu.au/~jwb/edict_doc.html and http://www.csse.monash.edu.au/~jwb/edict_doc_old.html for older markers)
 * Update wakan.rad with new RADKFILEs (see dependencies)


## DEPENDENCIES

In the recent versions Wakan can download some of these from the Download / Update components.

1. UNICONV.exe - required for dictionary import.
http://ringtail.its.monash.edu.au/pub/nihongo/uniconv.zip
http://ringtail.its.monash.edu.au/pub/nihongo/uniconv.txt
Also see: http://www.autohotkey.com/board/topic/9831-uniconv-convert-unicode-cmd/

2. WORDFREQ_CK - required for adding frequency information to dictionaries
http://ftp.monash.edu.au/pub/nihongo/00INDEX.html
http://ftp.monash.edu.au/pub/nihongo/wordfreq_ck.gz
Also see: http://code.google.com/p/wakan/issues/detail?id=66

3. RADKFILE/RADKFILE2 - required for raine radicals search
http://www.csse.monash.edu.au/~jwb/kradinf.html
Download and place RADKFILE into Wakan release folder. You can import RADKFILE2 manually, but it's not yet supported.

4. 7z.dll -- 7zip library. Required for downloading packed dictionaries.

5. KANJIDIC - required for character data.

6. Unihan folder - required for character data.

7. EDICT2 - required for dictionary data (Wakan will work without it, but installer requires it).