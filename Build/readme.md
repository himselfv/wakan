++ PREREQUISITES

1. Unicode Delphi (Delphi 2010+). Ansi support is being deprecated.
2. Inno Setup 5.
3. Jp-tools (http://code.google.com/p/jp-tools/) which Wakan uses heavily.


++ BUILDING

1. Configure paths in setupvars.cmd (in Settings -> Environment, if building manually).

2. Build and install JaletControls.dpr.

3. Copy the dependencies (see below) to the Release folder.

4. Run build.cmd to generate:

    * Binary files in Release
    * Installer in Build/Output.


++ MANUAL BUILDING

1. Build Jalet.dpr.

2. Run Jalet.exe to generate:

    wakan.chr (if not present) -- character database
    wakan.usr -- empty user package
    wakan.sod -- stroke order package built from strokes.csv (included)
    wakan.rad -- raine radical search package built from RADKFILE/RADKFILE2 (see dependencies)

3. Import/update any dictionaries (EDICT2/CC-EDICT variations).

4. Pack these files into a distribution:
  jalet.exe -> wakan.exe
  wakan.cfg
  wakan.sod
  wakan.rad
  wakan.chr
  any dictionaries
  lng\*.*
  possibly: UNICONV.exe + related files
  possibly: wordfreq_ck for frequency generation


++ DEPENDENCIES

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

4. 7z.dll -- 7zip library
