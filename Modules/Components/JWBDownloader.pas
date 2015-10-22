unit JWBDownloader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, Buttons, ComCtrls, ImgList, JWBComponents,
  Generics.Collections, JWBJobs, ExtCtrls, TaskbarCtl, JWBForms, JWBDownloaderCore;

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

  PNdJobData = ^TNdJobData;
  TNdJobData = record
    Job: TJob;
  end;

  TSourceArray = array of PAppComponent;
  PSourceArray = ^TSourceArray;

  TfDownloader = class(TJwbForm)
    btnClose: TBitBtn;
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
    Panel1: TPanel;
    cbCheckDownloadAll: TCheckBox;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
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
    procedure vtJobsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure cbCheckDownloadAllClick(Sender: TObject);

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
    FFilterCategory: string;
    FFilterBaseLanguage: char;
    procedure ReloadKnownFiles;
    procedure AddKnownCat(const AName: string; const ATitle, ADescription: string);
    function GetKnownCat(const AName: string): PVirtualNode;
    function AddKnownFile(const AParent: PVirtualNode;
      const ASource: PAppComponent): PVirtualNode;
    function IsAnythingCheckedForDownload: boolean;
    procedure vtCountCheckedNodes(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetDownloadList: TSourceArray;
    procedure vtBuildDownloadList(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure vtKnownFiles_CheckDefault(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure vtKnownFiles_UncheckAll(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);

  protected
    procedure ReloadReadyToDownloadList;

  protected
    taskbar: TTaskbarProgress;
    FWorkerThread: TWorkerThread;
    procedure StartDownloadJobs;
    procedure CancelDownloadJobs;
    procedure StopProgressUpdates;
    function IsDownloadFinished: boolean;
    function AreAllJobsSuccessful: boolean;
    function GetJobsTotalProgress: integer;
    procedure WorkerThreadException(Sender: TObject; Job: TJob; E: Exception;
      out StopJob: boolean);
    function AddJobNode(AJob: TJob): PVirtualNode;
    procedure UpdateJobNodes;
    procedure VtUpdateJobNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);

  end;

  TComponentDownloadJob = class(TJob)
  protected
    FComponent: PAppComponent;
    FDownloadJob: TDownloadJob;
    FImportJob: TJob;
    FFatalException: string;
    FStage: integer;
    FStageProgress: integer; //between 1 and 1000
    procedure StartOperation(const AOperation: string; AStage: integer); reintroduce;
    procedure ChildJobProgressChanged(Sender: TObject);
    procedure ChildYield(Sender: TObject);
    procedure SetStageProgress(const AValue: integer);
    procedure UpdateProgress;
    procedure ImportDictionary;
    procedure ImportKanjidic;
  public
    constructor Create(AComponent: PAppComponent);
    destructor Destroy; override;
    procedure ProcessChunk; override;
    property DownloadJob: TDownloadJob read FDownloadJob;
    property ImportJob: TJob read FImportJob;
    property FatalException: string read FFatalException write FFatalException;
  end;

function OpenDownloader(AOwner: TComponent): TModalResult; overload;
function OpenDownloader(AOwner: TComponent; ACategory: string;
  ABaseLanguage: char = #00): TModalResult; overload;

type
  EComponentNotFound = class(Exception);

function DownloadComponent(const AName: string): boolean;

implementation
uses UITypes, PngImage, JWBStrings, JWBUnpackJob, JWBDictionaries, JWBDicImportJob, JWBIO,
  JWBLanguage, StdPrompt;

{$R *.dfm}

function OpenDownloader(AOwner: TComponent): TModalResult;
var Instance: TfDownloader;
begin
  Instance := TfDownloader.Create(AOwner);
  try
    Result := Instance.ShowModal;
  finally
    FreeAndNil(Instance);
  end;
end;

function OpenDownloader(AOwner: TComponent; ACategory: string;
  ABaseLanguage: char): TModalResult;
var Instance: TfDownloader;
begin
  Instance := TfDownloader.Create(AOwner);
  try
    Instance.FFilterCategory := ACategory;
    Instance.FFilterBaseLanguage := ABaseLanguage;
    Result := Instance.ShowModal;
  finally
    FreeAndNil(Instance);
  end;
end;


constructor TComponentDownloadJob.Create(AComponent: PAppComponent);
begin
  inherited Create();
  FComponent := AComponent;
  FMaxProgress := 1000; //split between all subtasks
  FStage := 0;
end;

destructor TComponentDownloadJob.Destroy;
begin
  FreeAndNil(FImportJob);
  FreeAndNil(FDownloadJob);
  inherited;
end;

procedure TComponentDownloadJob.StartOperation(const AOperation: string;
  AStage: integer);
begin
  SetOperation(AOperation);
  if FStage<>AStage then begin
    FStage := AStage;
    FStageProgress := 0; //or there'd be a moment when it's show big
    UpdateProgress;
  end;
end;

procedure TComponentDownloadJob.ProcessChunk;
var
  ATempDir: string;
  ATempFilename: string; //temporary download filename
  ACheckPresent: string;
  AFileTime: TDatetime;
  AMoveJob: TJob;
  ATargetPath: string;
begin
  FState := jsWorking;

  ATempDir := CreateRandomTempDir();
  try
    StartOperation(_l('#01161^Starting download'), 0);

   //Download to temp folder
    FDownloadJob := TDownloadJob.Create(FComponent.URL, ATempDir+'\');
    ACheckPresent := FComponent.GetAbsoluteCheckPresent;
    if (ACheckPresent<>'') and FileAge(ACheckPresent, AFileTime) then
      FDownloadJob.IfModifiedSince := AFileTime;
    FDownloadJob.OnProgressChanged := ChildJobProgressChanged;
    FDownloadJob.OnYield := ChildYield;
    FDownloadJob.ProcessChunk; //first chunk establishes connection
    StartOperation(_l('#01162^Downloading'), 0);
    Run(FDownloadJob);
    if FDownloadJob.Result in [drUpToDate, drError] then begin
      FState := jsFinished;
      exit;
    end;
    ATempFilename := FDownloadJob.ToFilename; //could have been taken from server

   //Unpack or move
    ATargetPath := FComponent.GetAbsoluteTarget;
    if FComponent.URL_Unpack='' then begin
      StartOperation(_l('#01163^Moving'), 1);
      AMoveJob := TFileMoveJob.Create(ATempFilename, ATargetPath);
    end else begin
      StartOperation(_l('#01164^Unpacking'), 1);
      AMoveJob := TFileUnpackJob.Create(ATempFilename, ExtractFilePath(ATargetPath));
      TFileUnpackJob(AMoveJob).DefaultFilename := FComponent.Target;
      TFileUnpackJob(AMoveJob).ForceFormat := FComponent.URL_Unpack;
    end;
    try
      ForceDirectories(ExtractFilePath(ATargetPath));
      AMoveJob.OnProgressChanged := ChildJobProgressChanged;
      AMoveJob.OnYield := ChildYield;
      Run(AMoveJob);
    finally
      FreeAndNil(AMoveJob);
    end;

   //Install
    StartOperation(_l('#01165^Importing'), 2);
    if FComponent.Category='dic' then
      ImportDictionary
    else
    if FComponent.Category='kanjidic' then
      ImportKanjidic;

    FState := jsFinished;
  finally
    DeleteDirectory(ATempDir);
  end;
end;

procedure TComponentDownloadJob.ImportDictionary;
var AJob: TDicImportJob;
  AEncoding: CEncoding;
begin
  if FComponent.Format=sfWakan then exit; //no need to do anything
  if FComponent.Target='' then
    raise Exception.Create('Error in component definition file: Target filename'
      +' not specified');
   //We could have tried to import "the only file that was downloaded" or
   //"the only file that was unpacked", but whatever.
  AJob := TDicImportJob.Create;
  AJob.DicFilename := FComponent.GetBaseDir + '\' + FComponent.Name+'.dic';
  AJob.DicDescription := FComponent.Description;
  AJob.DicLanguage := FComponent.BaseLanguage;
  if AJob.DicLanguage=#00 then
    AJob.DicLanguage := 'j';
  if FComponent.Encoding<>'' then begin
    AEncoding := FindEncodingByName(FComponent.Encoding);
    if AEncoding=nil then
      raise Exception.Create('Unknown encoding: '+FComponent.Encoding);
  end else
    AEncoding := nil;
  AJob.OnProgressChanged := ChildJobProgressChanged;
  AJob.OnYield := ChildYield;
  AJob.AddSourceFile(FComponent.GetAbsoluteTarget, AEncoding);
  FImportJob := AJob;
  Run(AJob);
end;

procedure TComponentDownloadJob.ImportKanjidic;
begin
 //TODO: Kanjidic import
end;

procedure TComponentDownloadJob.ChildJobProgressChanged(Sender: TObject);
var AProg: int64;
  AMax: integer;
begin
  AProg := TJob(Sender).Progress;
  AMax := TJob(Sender).MaxProgress;
  if AMax = 0 then AProg := 0 else AProg := Trunc(AProg*1000 / AMax);
  Self.SetStageProgress(AProg);
end;

procedure TComponentDownloadJob.ChildYield(Sender: TObject);
begin
  Yield;
end;

procedure TComponentDownloadJob.SetStageProgress(const AValue: integer);
begin
  if FStageProgress=AValue then exit;
  FStageProgress := AValue;
  UpdateProgress;
end;

procedure TComponentDownloadJob.UpdateProgress;
begin
  case FStage of
    0: SetProgress(0 + Trunc(FStageProgress*500/1000));
    1: SetProgress(500 + Trunc(FStageProgress*100/1000));
    2: SetProgress(600 + Trunc(FStageProgress*400/1000));
  else
    SetProgress(1000);
  end;
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

procedure TfDownloader.btnCloseClick(Sender: TObject);
var ConfirmText: string;
begin
  if PageControl.ActivePage=tsReadyToDownload then
    ConfirmText := '#01166^If you close the window now, you will not download files. '
      +'Do you want to close the window?'
  else
  if (PageControl.ActivePage=tsDownloading) and not IsDownloadFinished then
    ConfirmText := '#01167^Your files are still being downloaded. Some of them will not '
      +'be available. Do you want to cancel the operation?'
  else
    ConfirmText := '';

  if ConfirmText<>'' then
    if MessageBox(Self.Handle, PChar(_l(ConfirmText)), PChar(Self.Caption),
      MB_TASKMODAL or MB_YESNO)<>ID_YES then
      raise EAbort.Create('');

  CancelDownloadJobs;
  dicts.Rescan(); //in case any were added
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
  end else
  if (PageControl.ActivePage = tsDownloading) and IsDownloadFinished then
    Close;
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
    btnNext.Visible := false;
  end else begin
    btnPrev.Visible := true;
    btnNext.Enabled := false;
  end;

  if PageControl.ActivePage=tsReadyToDownload then
    btnNext.Caption := _l('#01168^Download')
  else
    btnNext.Caption := _l('#01169^Next');

  if (PageControl.ActivePage=tsDownloading) and IsDownloadFinished then
    btnClose.Caption := _l('#01170^Close')
  else
    btnClose.Caption := _l('#01171^Cancel');
end;

procedure TfDownloader.ReloadKnownFiles;
var i: integer;
  cat: PVirtualNode;
  AFilterCategory: string;
begin
  SetLength(FKnownCats, 0);
  vtKnownFiles.Clear;

  AddKnownCat('', '#01179^Common files', '#01180^Common files required for Wakan to function.');
  AddKnownCat('kanjidic', '#01181^Kanji Dictionaries', '#01182^Dictionaries with information about characters');
  AddKnownCat('dic-jp', '#01183^Japanese Dictionaries', '#01184^Dictionaries of Japanese words');
  AddKnownCat('dic-cn', '#01185^Chinese Dictionaries', '#01186^Dictionaries of Chinese words');
{ Not yet supported:
  AddKnownCat('uilang', 'Interface Translations', 'With these interface translations Wakan can be shown in your own language');
  AddKnownCat('font', 'Fonts', 'Recommended fonts if you don''t have appropriate fonts in your system.');
  AddKnownCat('romaji', 'Romaji Schemes', 'With different romaji schemes you can enter kana in a different way');
  AddKnownCat('copyformat', 'Copy Formats', 'Text formats to use when copying dictionary results');
  AddKnownCat('kanjigroup', 'Kanji Groups', 'Pre-populated sets of groups, or tags, for characters'); }

 //Or we can set all to nil and create as needed

  AFilterCategory := FFilterCategory.ToLower;
  for i := 0 to AppComponents.Count-1 do begin
    if AppComponents[i].URL = '' then continue; //can't download, not interested
    if (AFilterCategory<>'') and (AppComponents[i].Category<>AFilterCategory) then
      continue;
    if (FFilterBaseLanguage<>#00) and (AppComponents[i].BaseLanguage<>FFilterBaseLanguage) then
      continue;

    if (AppComponents[i].Category='dic')
    and (AppComponents[i].BaseLanguage='c') then
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
  cbCheckDownloadAll.Checked := true;
end;

procedure TfDownloader.AddKnownCat(const AName: string; const ATitle, ADescription: string);
var i: integer;
begin
  i := Length(FKnownCats);
  SetLength(FKnownCats, i+1);
  FKnownCats[i].Name := AName;
  FKnownCats[i].Title := _l(ATitle);
  FKnownCats[i].Description := _l(ADescription);
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

procedure TfDownloader.vtCountCheckedNodes(Sender: TBaseVirtualTree;
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
  vtKnownFiles.IterateSubtree(nil, vtBuildDownloadList, @Result);
end;

procedure TfDownloader.vtBuildDownloadList(Sender: TBaseVirtualTree;
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

procedure TfDownloader.cbCheckDownloadAllClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    vtKnownFiles.IterateSubtree(nil, vtKnownFiles_CheckDefault, nil)
  else
    vtKnownFiles.IterateSubtree(nil, vtKnownFiles_UncheckAll, nil)
end;

procedure TfDownloader.vtKnownFiles_CheckDefault(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var NodeData: PNdFileData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData.IsComponentPresent then
    Sender.CheckState[Node] := csCheckedNormal
  else
  if (NodeData.Source<>nil) and NodeData.Source.IsDefault then
    Sender.CheckState[Node] := csCheckedNormal;
end;

procedure TfDownloader.vtKnownFiles_UncheckAll(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Sender.CheckState[Node] := csUncheckedNormal;
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
  AJob: TComponentDownloadJob;
  i: integer;
begin
  CancelDownloadJobs; //just in case
  vtJobs.Clear;

 //Unload dictionaries in case any needs to be overriden
  dicts.UnloadAll;

  FWorkerThread := TWorkerThread.Create;
  FWorkerThread.OnException := WorkerThreadException;
  AList := GetDownloadList;
  for i := 0 to Length(AList)-1 do begin
    AJob := TComponentDownloadJob.Create(AList[i]);
    AddJobNode(AJob);
    FWorkerThread.AddJob(AJob);
  end;
  FWorkerThread.Start;

  ProgressBar.State := pbsNormal;
  ProgressBar.Position := 0;
  ProgressBar.Max := Length(AList)*100; //100 percents for each job

  taskbar := TTaskbarProgress.Create;
  taskbar.State := tsNormal;
  taskbar.SetProgress(0, ProgressBar.Max);

  tmrJobUpdateTimer.Enabled := true;
end;

procedure TfDownloader.CancelDownloadJobs;
begin
  StopProgressUpdates;
  FreeAndNil(FWorkerThread); //does Terminate
end;

procedure TfDownloader.StopProgressUpdates;
begin
  tmrJobUpdateTimer.Enabled := false;
  FreeAndNil(taskbar);
  ProgressBar.State := pbsNormal;
  ProgressBar.Position := 1;
  ProgressBar.Max := 1;
end;

function TfDownloader.IsDownloadFinished: boolean;
begin
  Result := (FWorkerThread<>nil) and FWorkerThread.Finished;
end;

function TfDownloader.AreAllJobsSuccessful: boolean;
var i: integer;
  AJob: TComponentDownloadJob;
begin
  Result := true;
  if FWorkerThread=nil then begin
    Result := false; //wtf though
    exit;
  end;

  for i := 0 to FWorkerThread.Jobs.Count-1 do begin
    AJob := TComponentDownloadJob(FWorkerThread.Jobs[i]);
    if AJob.FatalException<>'' then begin
      Result := false;
      exit;
    end;
    if AJob.ImportJob<>nil then
      if (AJob.ImportJob is TDicImportJob)
      and (TDicImportJob(AJob.ImportJob).ProblemRecords>300) then begin
        Result := false;
        exit;
      end;
  end;
end;

{ Returns total progress of all jobs. Each job gets a chunk of 0..100, max
 progress is 100 * NumberOfJobs }
function TfDownloader.GetJobsTotalProgress: integer;
var i, tmpMax: integer;
begin
  Result := 0;
  for i := 0 to FWorkerThread.Jobs.Count-1 do
    case FWorkerThread.Jobs[i].State of
      jsWorking: begin
        tmpMax := FWorkerThread.Jobs[i].MaxProgress;
        if tmpMax>0 then
          Result := Result + Trunc(FWorkerThread.Jobs[i].Progress * 100 / tmpMax);
        //else can't do much until finished
      end;
      jsFinished: Result := Result + 100;
     //else it's jsPending and add nothing
    end;
end;

procedure TfDownloader.tmrJobUpdateTimerTimer(Sender: TObject);
begin
  UpdateJobNodes;

  ProgressBar.Position := GetJobsTotalProgress;
  taskbar.Progress := ProgressBar.Position;

  if (FWorkerThread<>nil) and FWorkerThread.Finished then begin
   //Jobs finished
    StopProgressUpdates; //but don't CancelDownloadJobs as we yet need Jobs
    UpdatePrevNextButtons;
    if AreAllJobsSuccessful then
      lblPageDescription.Caption := _l('#01187^All files has been downloaded.')
    else
      lblPageDescription.Caption := _l('#01188^There were problems downloading some files');
  end;
end;

procedure TfDownloader.WorkerThreadException(Sender: TObject; Job: TJob;
  E: Exception; out StopJob: boolean);
begin
  TComponentDownloadJob(Job).FatalException := E.Message;
  StopJob := true;
end;

procedure TfDownloader.vtJobsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TJob);
end;

procedure TfDownloader.vtJobsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PNdJobData;
begin
  Data := Sender.GetNodeData(Node);
  ZeroMemory(Data, SizeOf(Data^));
end;

procedure TfDownloader.vtJobsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
//var Data: PNdJobData;
begin
//  Data := Sender.GetNodeData(Node);
end;

function GetComponentJobResult(AJob: TComponentDownloadJob): string;
begin
  if AJob.FFatalException<>'' then begin
    Result := AJob.FatalException;
    exit;
  end;

  if AJob.DownloadJob.Result<>drDone then begin
    case AJob.DownloadJob.Result of
      drUpToDate: Result := _l('#01189^Up to date.');
      drError: Result := _l('#01190^Cannot download: %d', [AJob.DownloadJob.ErrorCode]);
    else Result := _l('#01191^Cannot download.');
    end;
    exit;
  end;

  Result := _l('#01192^Done.');
  if AJob<>nil then
    if AJob.ImportJob is TDicImportJob then
      if TDicImportJob(AJob.ImportJob).ProblemRecords=0 then
        Result := _l('#01192^Done.')
      else
        Result := _l('#01193^%d problem records', [TDicImportJob(AJob.ImportJob).ProblemRecords]);
end;

procedure TfDownloader.vtJobsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: PNdJobData;
  AJob: TComponentDownloadJob;
begin
  if TextType<>ttNormal then exit;
  Data := Sender.GetNodeData(Node);
  AJob := TComponentDownloadJob(Data.Job);
  case Column of
    NoColumn, 0:
      CellText := AJob.FComponent.Name;

    1:
      case AJob.State of
        jsPending: CellText := '';
        jsWorking:
          if AJob.MaxProgress>0 then
            CellText := AJob.Operation+Format(' (%.1f%%)', [100*AJob.Progress/AJob.MaxProgress])
          else
            CellText := AJob.Operation+'...';
        jsFinished:
          CellText := GetComponentJobResult(AJob);
      end;
  end;
end;

procedure TfDownloader.vtJobsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var Data: PNdJobData;
  AJob: TComponentDownloadJob;
  NodeRect: TRect;
begin
  Data := Sender.GetNodeData(Node);
  AJob := TComponentDownloadJob(Data.Job);
  case Column of
    1:
      case AJob.State of
        jsWorking: begin
          if Data.Job.MaxProgress>0 then begin
            NodeRect := CellRect;
            NodeRect.Width := Trunc(NodeRect.Width * (Data.Job.Progress / Data.Job.MaxProgress));
            TargetCanvas.Brush.Color := clHighlight;
            TargetCanvas.Brush.Style := bsSolid;
            TargetCanvas.FillRect(NodeRect);
          end;
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
        jsFinished:
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

type
  TCancelQueryHandler = class
  public
    procedure CancelQuery(ASender: TObject; var DoAbort: boolean);
  end;

procedure TCancelQueryHandler.CancelQuery(ASender: TObject; var DoAbort: Boolean);
begin
  DoAbort := MessageBox(Application.Handle,
    PChar(_l('01221^Do you really want to abort the operation?')),
    PChar(TSMPromptForm(ASender).Caption),
    MB_YESNO or MB_ICONQUESTION) = ID_YES;
end;

function DownloadComponent(const AName: string): boolean;
var AComponent: PAppComponent;
  prog: TSMPromptForm;
  job: TComponentDownloadJob;
  cq: TCancelQueryHandler;
begin
  AComponent := AppComponents.FindByName(AName);
  if AComponent=nil then
    raise EComponentNotFound.Create('Cannot find component information for "'+AName+'".');
  prog := SMProgressDlgCreate(
    _l('01223^Downloading %s...', [AName]),
    _l('01222^Downloading'),
    100,
    {canCancel=}true);
  job := TComponentDownloadJob.Create(AComponent);
  cq := TCancelQueryHandler.Create;
  try
    try
      prog.OnCancelQuery := cq.CancelQuery;
      prog.Show;
      prog.ExecuteJob(job);
      Result := true;
    except
      on EAbort do
        Result := false;
    end;
  finally
    FreeAndNil(cq);
    FreeAndNil(job);
    FreeAndNil(prog);
  end;
end;


end.
