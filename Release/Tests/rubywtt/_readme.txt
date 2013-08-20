This folder contains data files for testing main Jalet formats (text with ruby, WTT).

rubytext.txt, norubytext.txt and rubytext.wtt must all have the same content, spare for format differences (norubytext.txt has valid ruby stripped).

Note that Wakan text files (wtt) are currently dictionary-version-dependent and therefore not portable.
When testing .wtt, do not access translations which will probably be garbage; just test that article numbers are loaded/saved properly.

Where encoding needs to be chosen, UTF16LE should be used.