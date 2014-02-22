unit JWBDownloader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, Buttons, ComCtrls, JWBDownloadSources;

type
  TNdFileData = record
    Title: string; //if Source is not set
    Source: PDownloadSource;
  end;
  PNdFileData = ^TNdFileData;

  TfDownloader = class(TForm)
    btnCancel: TBitBtn;
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    PageControl: TPageControl;
    tsSelectFiles: TTabSheet;
    vtKnownFiles: TVirtualStringTree;
    mmFileDetails: TMemo;
    lblPageTitle: TLabel;
    lblPageDescription: TLabel;
    procedure PageControlChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vtKnownFilesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtKnownFilesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtKnownFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtKnownFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtKnownFilesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vtKnownFilesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
  protected
    procedure UpdatePageTitle;

  protected
    BaseCat: PVirtualNode;
    JpDicCat: PVirtualNode;
    ChDicCat: PVirtualNode;
    LanguageCat: PVirtualNode;
    FontCat: PVirtualNode;
    RomajiCat: PVirtualNode;
    procedure ReloadKnownFiles;
    function AddKnownCat(const AParent: PVirtualNode;
      const ATitle: string): PVirtualNode;
    function AddKnownFile(const AParent: PVirtualNode;
      const ASource: PDownloadSource): PVirtualNode;
  end;

var
  fDownloader: TfDownloader;

implementation
uses UITypes;

{$R *.dfm}

procedure TfDownloader.FormShow(Sender: TObject);
begin
  if PageControl.ActivePage=tsSelectFiles then
    PageControlChange(PageControl) //or it won't fire
  else
    PageControl.ActivePage := tsSelectFiles;
  ReloadKnownFiles;
end;

procedure TfDownloader.btnCancelClick(Sender: TObject);
begin
 //TODO: If downloading, ask for confirmation and cancel download jobs
  Close;
end;

procedure TfDownloader.PageControlChange(Sender: TObject);
begin
  UpdatePageTitle;
end;

//Updates common page title controls to reflect the title of active page
//Mostly called automatically on page change.
procedure TfDownloader.UpdatePageTitle;
begin
  lblPageTitle.Caption := PageControl.ActivePage.Caption;
  lblPageDescription.Caption := PageControl.ActivePage.Hint;
end;

procedure TfDownloader.ReloadKnownFiles;
var i: integer;
  cat: PVirtualNode;
begin
  vtKnownFiles.Clear;
  BaseCat := AddKnownCat(nil, 'Base');
  JpDicCat := AddKnownCat(nil, 'Japanese Dictionaries');
  ChDicCat := AddKnownCat(nil, 'Chinese Dictionaries');
  LanguageCat := AddKnownCat(nil, 'Interface Translations');
  FontCat := AddKnownCat(nil, 'Fonts');
  RomajiCat := AddKnownCat(nil, 'Romaji Schemes');
 //Or we can set all to nil and create as needed

  for i := 0 to DownloadSources.Count-1 do
    if (not DownloadSources[i].IsDeprecated)
    and (DownloadSources[i].URL <> '') then begin

      if (DownloadSources[i].Category='dic')
      and (DownloadSources[i].BaseLanguage='cn') then
        cat := ChDicCat
      else
      if (DownloadSources[i].Category='dic') then
        cat := JpDicCat
      else
      if DownloadSources[i].Category='language' then
        cat := LanguageCat
      else
      if DownloadSources[i].Category='font' then
        cat := FontCat
      else
      if DownloadSources[i].Category='romaji' then
        cat := RomajiCat
      else
        cat := BaseCat;

      AddKnownFile(cat, DownloadSources[i]);
    end;

  vtKnownFiles.FullExpand();
end;

function TfDownloader.AddKnownCat(const AParent: PVirtualNode;
  const ATitle: string): PVirtualNode;
var Data: PNdFileData;
begin
  Result := vtKnownFiles.AddChild(AParent);
  vtKnownFiles.ReinitNode(Result, false);
  Data := vtKnownFiles.GetNodeData(Result);
  Data.Title := ATitle;
end;

function TfDownloader.AddKnownFile(const AParent: PVirtualNode;
  const ASource: PDownloadSource): PVirtualNode;
var Data: PNdFileData;
begin
  Result := vtKnownFiles.AddChild(AParent);
  vtKnownFiles.ReinitNode(Result, false);
  Data := vtKnownFiles.GetNodeData(Result);
  Data.Source := ASource;
  vtKnownFiles.CheckType[Result] := ctCheckBox;
  if Data.Source.IsDefault then
    vtKnownFiles.CheckState[Result] := csCheckedNormal;
end;

procedure TfDownloader.vtKnownFilesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdFileData);
end;

procedure TfDownloader.vtKnownFilesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PNdFileData;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TfDownloader.vtKnownFilesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PNdFileData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TfDownloader.vtKnownFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: PNdFileData;
begin
  if TextType<>ttNormal then exit;
  Data := Sender.GetNodeData(Node);
  CellText := '';

  case Column of
    0, NoColumn:
      if Data.Source=nil then
        CellText := Data.Title
      else
        CellText := Data.Source.Name;
    1:
      if Data.Source<>nil then
        CellText := Data.Source.Language;
  end;
end;

procedure TfDownloader.vtKnownFilesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var Data: PNdFileData;
begin
  if TextType<>ttNormal then exit;
  Data := Sender.GetNodeData(Node);

  if (Data.Source=nil) or Data.Source.IsDefault then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
end;

procedure TfDownloader.vtKnownFilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var Data: PNdFileData;
begin
  if Node=nil then begin
    mmFileDetails.Text := '';
    exit;
  end;

  Data := Sender.GetNodeData(Node);
  if Data.Source=nil then
    mmFileDetails.Text := '' //TODO: Localized description for the category
  else
    mmFileDetails.Text := Data.Source.Description;
end;

end.
