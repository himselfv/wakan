unit JWBDownloader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, Buttons, ComCtrls, ImgList, JWBComponents,
  Generics.Collections, JWBJobs, Vcl.ExtCtrls;

type
  TNdFileData = record
    Source: PAppComponent;
    Index: integer;
    FlagImageIndex: integer;
    IsComponentPresent: boolean;
    Title: string; //if Source is not set
    Description: string;
  end;
  PNdFileData = ^TNdFileData;

  TSourceArray = array of PAppComponent;
  PSourceArray = ^TSourceArray;

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
    ilKnownFileImages: TImageList;
    tsReadyToDownload: TTabSheet;
    lbFilesToDownload: TListBox;
    Label1: TLabel;
    tsDownloading: TTabSheet;
    vtJobs: TVirtualStringTree;
    ilJobImages: TImageList;
    tmrJobUpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
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
    procedure vtKnownFilesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure vtKnownFilesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtJobsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtJobsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtJobsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtJobsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtJobsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure tmrJobUpdateTimerTimer(Sender: TObject);
    procedure vtKnownFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

  protected
    procedure UpdatePageTitle;
    procedure UpdatePrevNextButtons;

  protected
    Flags: TDictionary<string, integer>;
    function GetFlagImageIndex(const ALangCode: string): integer;

  protected
    FKnownCats: array of record
      Name: string;
      Title: string;
      Description: string;
      Cat: PVirtualNode;
    end;
    procedure ReloadKnownFiles;
    procedure AddKnownCat(const AName: string; const ATitle, ADescription: string);
    function GetKnownCat(const AName: string): PVirtualNode;
    function AddKnownFile(const AParent: PVirtualNode;
      const ASource: PAppComponent): PVirtualNode;
    function IsAnythingCheckedForDownload: boolean;
    procedure VtCountCheckedNodes(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetDownloadList: TSourceArray;
    procedure VtBuildDownloadList(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);

  protected
    procedure ReloadReadyToDownloadList;

  protected
    FWorkerThread: TWorkerThread;
    procedure StartDownloadJobs;
    procedure CancelDownloadJobs;
    function IsDownloadFinished: boolean;
    function AreAllJobsSuccessful: boolean;
    function AddJobNode(AJob: TJob): PVirtualNode;
    procedure UpdateJobNodes;
    procedure VtUpdateJobNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);

  end;

var
  fDownloader: TfDownloader;

implementation
uses UITypes, PngImage, JWBStrings, JWBDownloaderCore;

{$R *.dfm}

type
  TCustomDownloadJob = class(TDownloadJob)
  protected
    FName: string;
  public
    property Name: string read FName write FName;
  end;

procedure TfDownloader.FormCreate(Sender: TObject);
begin
  Flags := TDictionary<string, integer>.Create();
end;

procedure TfDownloader.FormDestroy(Sender: TObject);
begin
  CancelDownloadJobs;
  FreeAndNil(Flags);
end;

procedure TfDownloader.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsSelectFiles;
  PageControlChange(PageControl); //somehow it won't fire
  ReloadKnownFiles;
end;

procedure TfDownloader.btnCancelClick(Sender: TObject);
var ConfirmText: string;
begin
  if PageControl.ActivePage=tsReadyToDownload then
    ConfirmText := 'If you close the window now, you will not download files. '
      +'Do you want to close the window?' //TODO: Localize
  else
  if PageControl.ActivePage=tsDownloading then
    ConfirmText := 'Your files are still being downloaded. Some of them will not '
      +'be available. Do you want to cancel the operation?' //TODO: Localize
  else
    ConfirmText := '';

  if ConfirmText<>'' then
    if MessageBox(Self.Handle, PChar(ConfirmText), PChar(Self.Caption),
      MB_TASKMODAL or MB_YESNO)<>ID_YES then
      raise EAbort.Create('');

  CancelDownloadJobs;
  Close;
end;

procedure TfDownloader.PageControlChange(Sender: TObject);
begin
  UpdatePageTitle;
  UpdatePrevNextButtons;
end;

//Updates common page title controls to reflect the title of active page
//Mostly called automatically on page change.
procedure TfDownloader.UpdatePageTitle;
begin
  lblPageTitle.Caption := PageControl.ActivePage.Caption;
  lblPageDescription.Caption := PageControl.ActivePage.Hint;
end;

procedure TfDownloader.btnPrevClick(Sender: TObject);
begin
  if PageControl.ActivePage = tsReadyToDownload then begin
    PageControl.ActivePage := tsSelectFiles;
    PageControlChange(PageControl);
  end;
end;

procedure TfDownloader.btnNextClick(Sender: TObject);
begin
  if PageControl.ActivePage = tsSelectFiles then begin
    PageControl.ActivePage := tsReadyToDownload;
    PageControlChange(PageControl);
    ReloadReadyToDownloadList;
  end else
  if PageControl.ActivePage = tsReadyToDownload then begin
    PageControl.ActivePage := tsDownloading;
    PageControlChange(PageControl);
    StartDownloadJobs;
  end;
end;

procedure TfDownloader.UpdatePrevNextButtons;
begin
  btnNext.Visible := true;
  if PageControl.ActivePage=tsSelectFiles then begin
    btnPrev.Visible := false;
    btnNext.Enabled := IsAnythingCheckedForDownload()
  end else
  if PageControl.ActivePage=tsReadyToDownload then begin
    btnPrev.Visible := true;
    btnNext.Enabled := true;
  end else
  if PageControl.ActivePage=tsDownloading then begin
    btnPrev.Visible := false;
    btnNext.Enabled := IsDownloadFinished();
  end else begin
    btnPrev.Visible := true;
    btnNext.Enabled := false;
  end;

  if PageControl.ActivePage=tsReadyToDownload then
    btnNext.Caption := 'Download' //TODO: Localize
  else
    btnNext.Caption := 'Next'; //TODO: Localize
end;

procedure TfDownloader.ReloadKnownFiles;
var i: integer;
  cat: PVirtualNode;
begin
  SetLength(FKnownCats, 0);
  vtKnownFiles.Clear;

 //TODO: Localize all
  AddKnownCat('', 'Common files', 'Common files required for Wakan to function.');
  AddKnownCat('kanjidic', 'Kanji Dictionaries', 'Dictionaries with information about characters');
  AddKnownCat('dic-jp', 'Japanese Dictionaries', 'Dictionaries of Japanese words');
  AddKnownCat('dic-cn', 'Chinese Dictionaries', 'Dictionaries of Chinese words');
  AddKnownCat('language', 'Interface Translations', 'With these interface translations Wakan can be shown in your own language');
  AddKnownCat('font', 'Fonts', 'Recommended fonts if you don''t have appropriate fonts in your system.');
  AddKnownCat('romaji', 'Romaji Schemes', 'With different romaji schemes you can enter kana in a different way');
  AddKnownCat('group', 'Kanji Groups', 'Pre-populated sets of groups, or tags, for characters');

 //Or we can set all to nil and create as needed

  for i := 0 to AppComponents.Count-1 do
    if AppComponents[i].URL <> '' then begin

      if (AppComponents[i].Category='dic')
      and (AppComponents[i].BaseLanguage='cn') then
        cat := GetKnownCat('dic-cn')
      else
      if (AppComponents[i].Category='dic') then
        cat := GetKnownCat('dic-jp')
      else
        cat := GetKnownCat(AppComponents[i].Category); //same as in file
      if cat=nil then
        cat := GetKnownCat(''); //base
      AddKnownFile(cat, AppComponents[i]);
    end;

  vtKnownFiles.Sort(nil, NoColumn, sdAscending);
  vtKnownFiles.FullExpand();
end;

procedure TfDownloader.AddKnownCat(const AName: string; const ATitle, ADescription: string);
var i: integer;
begin
  i := Length(FKnownCats);
  SetLength(FKnownCats, i+1);
  FKnownCats[i].Name := AName;
  FKnownCats[i].Title := ATitle;
  FKnownCats[i].Description := ADescription;
  FKnownCats[i].Cat := nil;
end;

function TfDownloader.GetKnownCat(const AName: string): PVirtualNode;
var Data: PNdFileData;
  i, idx: integer;
begin
  idx := -1;
  for i := 0 to Length(FKnownCats)-1 do
    if FKnownCats[i].Name=AName then begin
      idx := i;
      break;
    end;

  if idx<0 then begin
    Result := nil; //no such category
    exit;
  end;

  Result := FKnownCats[idx].Cat;
  if Result<>nil then exit; //exists

  Result := vtKnownFiles.AddChild(nil);
  vtKnownFiles.ReinitNode(Result, false);
  Data := vtKnownFiles.GetNodeData(Result);
  Data.Title := FKnownCats[idx].Title;
  Data.Description := FKnownCats[idx].Description;
  Data.Index := idx;
  Data.FlagImageIndex := -1;
  FKnownCats[idx].Cat := Result;
end;

function TfDownloader.AddKnownFile(const AParent: PVirtualNode;
  const ASource: PAppComponent): PVirtualNode;
var Data: PNdFileData;
begin
  Result := vtKnownFiles.AddChild(AParent);
  vtKnownFiles.ReinitNode(Result, false);
  Data := vtKnownFiles.GetNodeData(Result);
  Data.Source := ASource;
  Data.IsComponentPresent := (Data.Source<>nil) and IsComponentPresent(Data.Source);
  vtKnownFiles.CheckType[Result] := ctCheckBox;
  if Data.Source.IsDefault or Data.IsComponentPresent then
    vtKnownFiles.CheckState[Result] := csCheckedNormal;
  if (Data.Source<>nil) and (Data.Source.Language<>'') then
    Data.FlagImageIndex := GetFlagImageIndex(Data.Source.Language)
  else
    Data.FlagImageIndex := -1;
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

  if (Data.Source=nil) or Data.IsComponentPresent then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
end;

procedure TfDownloader.vtKnownFilesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var Data: PNdFileData;
begin
  if not (Kind in [ikNormal, ikSelected]) then
    exit;

  Data := Sender.GetNodeData(Node);

  case Column of
    1: ImageIndex := Data.FlagImageIndex;
  end;
end;

procedure TfDownloader.vtKnownFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PNdFileData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  Result := Data1.Index-Data2.Index;
  if Result<>0 then exit;

  if (Data1.Source=nil) or (Data2.Source=nil) then
    Result := integer(Data1.Source=nil) - integer(Data2.Source=nil)
  else
  if (Data1.Source.Language=Data2.Source.Language) then
    Result := CompareText(Data1.Source.Name, Data2.Source.Name)
  else
  if (Data1.Source.Language='') or (Data2.Source.Language='') then
    Result := integer(Data1.Source.Language='') - integer(Data2.Source.Language='')
  else
  if (Data1.Source.Language='en') or (Data2.Source.Language='en') then
    Result := integer(Data1.Source.Language='en') - integer(Data2.Source.Language='en')
  else
    Result := CompareText(Data1.Source.Language, Data2.Source.Language);
end;

function Min(a,b: integer): integer;
begin
  if a<b then Result := a else Result := b;
end;

function Max(a,b: integer): integer;
begin
  if a>b then Result := a else Result := b;
end;

procedure ResizeBitmapCanvas(Bitmap: TBitmap; H, W: Integer; BackColor: TColor);
var
  Bmp: TBitmap;
  Source, Dest: TRect;
  Xshift, Yshift: Integer;
begin
  Xshift := (Bitmap.Width-W) div 2;
  Yshift := (Bitmap.Height-H) div 2;

  Source.Left := Max(0, Xshift);
  Source.Top := Max(0, Yshift);
  Source.Width := Min(W, Bitmap.Width);
  Source.Height := Min(H, Bitmap.Height);

  Dest.Left := Max(0, -Xshift);
  Dest.Top := Max(0, -Yshift);
  Dest.Width := Source.Width;
  Dest.Height := Source.Height;

  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(W, H);
    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Brush.Color := BackColor;
    Bmp.Canvas.FillRect(Rect(0, 0, W, H));
    Bmp.Canvas.CopyRect(Dest, Bitmap.Canvas, Source);
    Bitmap.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

function TfDownloader.GetFlagImageIndex(const ALangCode: string): integer;
var ACode: string;
  png: TPngImage;
  bmp: TBitmap;
begin
  ACode := AnsiLowerCase(ALangCode);
  if Flags.TryGetValue(ACode, Result) then
    exit;

  png := TPngImage.Create;
  try
    try
      png.LoadFromFile(AppFolder+'\Flags\'+ACode+'.png');
      bmp := TBitmap.Create;
      png.AssignTo(bmp);
      ResizeBitmapCanvas(bmp, 16, 16, clWhite); //resize and center
      bmp.TransparentColor := clWhite;
      bmp.Transparent := true;
      Result := ilKnownFileImages.Add(bmp, nil);
    except
      on EFOpenError do begin
        FreeAndNil(bmp);
        Result := -1;
      end;
      on Exception do begin
        FreeAndNil(bmp);
        raise;
      end;
    end;
  finally
    FreeAndNil(png);
  end;

  Flags.Add(ACode, Result);
end;

procedure TfDownloader.vtKnownFilesChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdatePrevNextButtons; //Next button could become enabled/disabled
end;

function TfDownloader.IsAnythingCheckedForDownload: boolean;
var CheckedNodeCount: integer;
begin
  CheckedNodeCount := 0;
  vtKnownFiles.IterateSubtree(nil, VtCountCheckedNodes, @CheckedNodeCount);
  Result := CheckedNodeCount > 0;
end;

procedure TfDownloader.VtCountCheckedNodes(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  if Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed] then
    Inc(PInteger(Data)^);
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
  if Data.Source<>nil then
    mmFileDetails.Text := Data.Source.Description
  else
    mmFileDetails.Text := Data.Description;
end;

function TfDownloader.GetDownloadList: TSourceArray;
begin
  SetLength(Result, 0);
  vtKnownFiles.IterateSubtree(nil, VtBuildDownloadList, @Result);
end;

procedure TfDownloader.VtBuildDownloadList(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var FileData: PNdFileData;
  AList: PSourceArray absolute Data;
begin
  if not (Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed]) then
    exit;

  FileData := Sender.GetNodeData(Node);
  if FileData.Source=nil then exit;

  SetLength(AList^, Length(AList^)+1);
  AList^[Length(AList^)-1] := FileData.Source;
end;


procedure TfDownloader.ReloadReadyToDownloadList;
var AList: TSourceArray;
  i: integer;
begin
  lbFilesToDownload.Clear;
  AList := GetDownloadList;
  for i := 0 to Length(AList)-1 do
    lbFilesToDownload.Items.Add(AList[i].Name);
end;


procedure TfDownloader.StartDownloadJobs;
var AList: TSourceArray;
  AJob: TCustomDownloadJob;
  ATempDir: string;
  AToFilename: string;
  ACheckPresent: string;
  AFileTime: TDatetime;
  i: integer;
begin
  CancelDownloadJobs; //just in case
  vtJobs.Clear;

  FWorkerThread := TWorkerThread.Create;
  AList := GetDownloadList;
  for i := 0 to Length(AList)-1 do begin
    ATempDir := CreateRandomTempDir();
    AToFilename := ATempDir+'\'+AList[i].TargetFilename;
    AJob := TCustomDownloadJob.Create(AList[i].URL, AToFilename);
    AJob.Name := AList[i].Name;

    ACheckPresent := AList[i].GetCheckPresentFilename;
    if ACheckPresent<>'' then begin
      ACheckPresent := AList[i].GetTargetDir + '\' + ACheckPresent;
      if FileAge(ACheckPresent, AFileTime) then
        AJob.IfModifiedSince := AFileTime;
    end;

    AddJobNode(AJob);
    FWorkerThread.AddJob(AJob);
  end;
  FWorkerThread.Start;

  tmrJobUpdateTimer.Enabled := true;
end;

procedure TfDownloader.CancelDownloadJobs;
begin
  tmrJobUpdateTimer.Enabled := false;
  FreeAndNil(FWorkerThread);
end;

function TfDownloader.IsDownloadFinished: boolean;
begin
  Result := (FWorkerThread<>nil) and FWorkerThread.Finished;
end;

function TfDownloader.AreAllJobsSuccessful: boolean;
var i: integer;
  AJob: TJob;
begin
  Result := true;
  if FWorkerThread=nil then begin
    Result := false; //wtf though
    exit;
  end;

  for i := 0 to FWorkerThread.Jobs.Count-1 do begin
    AJob := FWorkerThread.Jobs[i];
    if (AJob is TDownloadJob) and (TDownloadJob(AJob).Result=drError) then begin
      Result := false;
      break;
    end;
  end;
end;

procedure TfDownloader.tmrJobUpdateTimerTimer(Sender: TObject);
begin
  UpdateJobNodes;
  if (FWorkerThread<>nil) and FWorkerThread.Finished then begin
   //Jobs finished
    UpdatePrevNextButtons;
    if AreAllJobsSuccessful then
      lblPageDescription.Caption := 'All files has been downloaded.' //TODO: Localize
    else
      lblPageDescription.Caption := 'There were problems downloading some files'; //TODO: Localize
  end;
end;

procedure TfDownloader.vtJobsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TJob);
end;

procedure TfDownloader.vtJobsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
 //Nothing as of now
end;

procedure TfDownloader.vtJobsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
 //Nothing as of now
end;

procedure TfDownloader.vtJobsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var AJob: TJob;
begin
  if TextType<>ttNormal then exit;
  AJob := TJob(Sender.GetNodeData(Node)^);
  case Column of
    NoColumn, 0:
      if AJob is TCustomDownloadJob then
        CellText := TCustomDownloadJob(AJob).Name;

    1:
      case AJob.State of
        jsPending: CellText := '';
        jsWorking:
          if AJob.TotalSize>0 then
            CellText := 'Downloading ('+CurrToStr(100*AJob.Progress/AJob.TotalSize)+'%)' //TODO: Job action name, TODO: Percent bar
          else
            CellText := 'Downloading...';
        jsCompleted:
          if AJob is TCustomDownloadJob then
            case TCustomDownloadJob(AJob).Result of
              drDone: CellText := 'Done.';
              drUpToDate: CellText := 'Up to date.';
              drError: CellText := 'Cannot download: '+IntToStr(TCustomDownloadJob(AJob).ErrorCode);
            end;
      end;
  end;
end;

procedure TfDownloader.vtJobsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var AJob: TJob;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  AJob := TJob(Sender.GetNodeData(Node)^);
  case Column of
    1:
      case AJob.State of
        jsCompleted:
          if AJob is TDownloadJob then
            case TDownloadJob(AJob).Result of
              drDone, drUpToDate:
                ImageIndex := 0;
              drError: ImageIndex := 1;
            end;
      else
        ImageIndex := -1;
      end;
  end;
end;


function TfDownloader.AddJobNode(AJob: TJob): PVirtualNode;
var Data: PNdFileData;
begin
  Result := vtJobs.AddChild(nil);
  vtJobs.ReinitNode(Result, false);
  Data := vtJobs.GetNodeData(Result);
  PJob(Data)^ := AJob;
end;

procedure TfDownloader.UpdateJobNodes;
begin
  vtJobs.IterateSubtree(nil, VtUpdateJobNode, nil);
  vtJobs.Invalidate;
end;

procedure TfDownloader.VtUpdateJobNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
 //Nothing as of now. Everything is updated on paint.
end;

end.
