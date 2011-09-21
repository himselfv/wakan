unit DGPReg;

interface

uses Classes, SysUtils, ExtCtrls, DsgnWnds, SImageBtn, ColorBtns, ClrPanel,
	ShadowButton, Backgnd, ArtLabel, DFAboutEditor, DsgnIntf, Gradient, GradEditor,
     {New} DFCtrls;
type

 	TDFEditor = class(TDefaultEditor)
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGradientProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TGradientEditor = class(TDFEditor)
  protected
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

{TDFEditor}
function TDFEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TDFEditor.GetVerb(Index: Integer): string;
begin
	case Index of
  	0: Result := '&About The Author';
	end;
end;

procedure TDFEditor.ExecuteVerb(Index: Integer);
begin
	case Index of
  0: ShowCmpAbout;
  end;
end;


{ TGradientProperty }

procedure TGradientProperty.Edit;
begin
	EditGradient(TGradient(GetOrdValue));
end;

function TGradientProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

{ TGradientEditor }
procedure TGradientEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'GRADIENT') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

function TGradientEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TGradientEditor.GetVerb(Index: Integer): string;
begin
	case Index of
  	1: Result := '&Edit Gradient Fill';
  else
  	Result := inherited GetVerb(Index);
	end;
end;

procedure TGradientEditor.ExecuteVerb(Index: Integer);
begin
	case Index of
  	1: 	if Component is TdfGradient then
    		begin
        	EditGradient(TdfGradient(Component).Gradient);
      	end;
  else
  	inherited ExecuteVerb(Index);
  end;
end;

procedure Register;
begin
{01}  	RegisterComponents('Grafix', [TApples]);
{03}  	RegisterComponents('Grafix', [TBackground]);
{04}  	RegisterComponents('Grafix', [TShadowButton]);
{05}  	RegisterComponents('Grafix', [TColorPanel]);
{06}  	RegisterComponents('Grafix', [TColor95Button]);
{07}  	RegisterComponents('Grafix', [TSImageBtn]);
{08}  	RegisterComponents('Grafix', [TBCBitmap]);
{09}  	RegisterComponents('Grafix', [TClrBevel]);
{10}  	RegisterComponents('Grafix', [TArtLabel]);
{11}  	RegisterComponents('Grafix', [TDfGradient]);
{12}  	RegisterComponents('Grafix', [TDFScroll]);

	RegisterComponentEditor(TDfGradient, TGradientEditor);
	RegisterComponentEditor(TApples, TDFEditor);
	RegisterComponentEditor(TBackground, TDFEditor);
	RegisterComponentEditor(TColorPanel, TDFEditor);
	RegisterComponentEditor(TShadowButton, TDFEditor);
	RegisterComponentEditor(TBCBitmap, TDFEditor);
	RegisterComponentEditor(TArtLabel, TDFEditor);
	RegisterComponentEditor(TSImageBtn, TDFEditor);
	RegisterComponentEditor(TClrBevel, TDFEditor);
	RegisterComponentEditor(TDFScroll, TDFEditor);

	RegisterPropertyEditor(TypeInfo(TGradient), nil, '', TGradientProperty);

end;


end.
