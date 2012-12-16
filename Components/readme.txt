COMPONENTS FOR WAKAN COMPILATION

1. Install RX controls
2. Install UrlLabel
3. Add to the project's search path:
  Components
  Components\Package
  Components\TextTable
  Components\UrlLabel
  Components\ScaleMM

NOTE:
1. You can install newer versions of common controls as per below. But don't come crying when it doesn't work.
2. When installing, install into separate folder outside of this repo. Do not install into or modify these folders:
  /PROLIB
  /RxInst
  /Graphic


INSTALLATION HOWTO
1. MadExcept (http://madshi.net/madExceptDescription.htm). Has an installer.
NOTE: Not required. Do not install.
Required by some versions, but be sure to disable it's auto-insertion functionality (Tools-> Mad Except Configuration-> Adjust project uses clause).

2. RX Library (http://sourceforge.net/projects/rxlib/)
The installation is pretty straightforward and documented in RxLib\Install_eng.txt

3. UrlLabel.
Add UrlLabel.pas to a package and install that.

4. ProLib (http://barabash.com/prolib/history.html)
NOTE: Not used anymore. Do not install.
To compile ProLibDB90.bpl you might have to add this to Pro.inc before {$IFDEF VCL_3_OR_HIGHER}:
{$IF CompilerVersion>17}
  {$DEFINE DELPHI_9}
  {$DEFINE VCL_9}
  {$DEFINE VCL_1_OR_HIGHER}
  {$DEFINE VCL_2_OR_HIGHER}
  {$DEFINE VCL_3_OR_HIGHER}
  {$DEFINE VCL_3_5_OR_HIGHER}
  {$DEFINE VCL_4_OR_HIGHER}
  {$DEFINE VCL_5_OR_HIGHER}
  {$DEFINE VCL_6_OR_HIGHER}
  {$DEFINE VCL_7_OR_HIGHER}
  {$DEFINE VCL_9_OR_HIGHER}
{$IFEND}
The rest is per readme.
After install:
* Add source folder to the shell's library path.

5. Dream Graphic Pack by William Yang (http://www.torry.net/authorsmore.php?id=667)
NOTE: Not used anymore. Do not install.
This one is tricky:
1. Create a new package and add all *.pas files except for ColorBtnEdit.pas.
2. When Delphi complains, add "Variants" and "Controls" to uses list where needed.
3. When Delphi complains, change "DsgnIntf" to "DesignIntf", "DsgnWindows" to "DesignWindows" and add "DesignEditors" to uses list where needed.
4. When Delphi complains, upgrade the function definition from DesignEditors.
Otherwise, build and install the package.

