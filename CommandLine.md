# Wakan 1.87+
```
wakan.exe <command or file> [/options option params]
```
Supported commands:

  * `<filename>` --- same as `open <filename>` except if your file name conflicts with one of the commands, in which case use explicit syntax
  * `open <filename>` --- opens the file in the editor
  * `makedic <dicname>` --- builds a dictionary
      * `[/include filename]` --- includes a edict-style file into the dictionary. At least one must be present.
      * `[/description text]`
      * `[/language <j|c>]` --- japanese or chinese
      * `[/addfrequencyinfo]` --- you'll need wordfreq\_ck.uni in the same dir
  * `makeexamples` --- rebuilds examples package
  * `makesod` --- rebuilds wakan.sod (stroke order package) from strokes.csv --- mostly for developers
  * `makerad [filename] [...]` --- rebuilds wakan.rad (raine radical tables) from RADKFILE-type files --- mostly for developers
      * if any `[filenames]` are specified, only those files are added, else RADKFILE is used
      * see [building notes](http://bitbucket.org/himselfv/wakan/src/tip/.building.txt) for details on RADKFILEs
  * `makechars [/kanjidic <filename>]` --- rebuilds wakan.chr (wakan character database) from sources. At this time can only update fields from KANJIDIC, English or non-English version.
  * `updatedics [dicname dicname ...]` --- forces updates to all dictionaries, or to a list of dictionaries (possibly from another folders)
  * `/fast` --- disables some integrity checks and speeds up startup. Do not use if you use different Wakan versions with the same wakan.usr (maybe on different PCs). See Issue #207.

# Wakan 1.85 and earlier
Similar to Wakan 1.87 but:

  * `makedic <dicfilename>` additionally takes:
      * `[/name dic_name]` --- different from dicfilename
      * `[/copyright text]`
      * `[/priority int]` --- from 0 to 5
      * `[/version text]` --- anything you like
      * `[/addwordindex]` --- just do it
      * `[/addcharacterindex]` --- just do it

# Wakan 1.67 and earlier
```
wakan.exe <command>
```
Supported commands:

  * `makeexamples` --- rebuilds examples package
  * `makedic <filename>` --- builds a dictionary, taking all params from `<filename>`. Someone should document it's format one day.