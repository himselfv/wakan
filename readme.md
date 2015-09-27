== BUILDING

Go to /Build, read readme. Install prerequisites and run build.cmd to produce the app and the installer.


== OPTIMIZATION

1. Enable inlining (inline:on or inline:auto).
It's very important that some critical functions are inlined for speed in release builds.
Therefore if you're doing the building, READ THIS:
http://docwiki.embarcadero.com/RADStudio/XE/en/Calling_Procedures_and_Functions#Using_the_inline_Directive
Note the list of cases when the inlining is not done.

2. Disable "Compiling> String format checking" in Delphi Project Options:
http://www.micro-isv.asia/2008/10/needless-string-checks-with-ensureunicodestring/
They shouldn't be needed, and even if by some mistake we put Ansi chars into UnicodeString, we better crash and fix that instead of hiding the bug.
Enabling this option also effectively disables the "const s:string" optimizations since functions will get refcount management frames for strings anyway.


== MAINTENANCE

Stuff that has to be done regularly to keep Wakan up to date:
- Up the version number.
- Import new EDICT/ENAMDICT files
- Update EDICT marker list in JWBUnit.pas (see http://www.csse.monash.edu.au/~jwb/edict_doc.html and http://www.csse.monash.edu.au/~jwb/edict_doc_old.html for older markers)
- Update wakan.rad with new RADKFILEs (see dependencies)

