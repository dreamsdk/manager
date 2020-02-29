unit RubyMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type

  { TRubySampleItem }
  TRubySampleItem = class(TObject)
  private
    fDirectoryName: TFileName;
    fFullPath: TFileName;
    fRepositoryURL: string;
  public
    property DirectoryName: TFileName read fDirectoryName;
    property FullPath: TFileName read fFullPath;
    property RepositoryURL: string read fRepositoryURL;
  end;

  { TRubySampleList }
  TRubySampleList = class(TObject)
  private
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TRubySampleItem;
  protected
    procedure Add(ADirectoryName: TFileName; ARepositoryURL: string);
    procedure Clear;
    procedure LoadReferential;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TRubySampleItem read GetItem; default;
  end;

  { TRubyManager }
  TRubyManager = class(TObject)
  private
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fRepository: TDreamcastSoftwareDevelopmentRepository;
    fSamples: TRubySampleList;

    function GetInstalled: Boolean;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;

    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Build(var BufferOutput: string): Boolean;
    function Uninstall: Boolean;

    property Installed: Boolean read GetInstalled;
    property Repository: TDreamcastSoftwareDevelopmentRepository
      read fRepository;
    property Samples: TRubySampleList read fSamples;
  end;

implementation

uses
  FileUtil, PostInst, FSTools, SysTools;

{ TRubySampleList }

function TRubySampleList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TRubySampleList.GetItem(Index: Integer): TRubySampleItem;
begin
  Result := TRubySampleItem(fList[Index]);
end;

procedure TRubySampleList.Add(ADirectoryName: TFileName; ARepositoryURL: string);
var
  Item: TRubySampleItem;

begin
  Item := TRubySampleItem.Create;
  with Item do
  begin
    fDirectoryName := ADirectoryName;
    fRepositoryURL := ARepositoryURL;
    fFullPath := fEnvironment.FileSystem.Ruby.SamplesDirectory + fDirectoryName;
  end;
  fList.Add(Item);
end;

procedure TRubySampleList.Clear;
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  fList.Clear;
end;

procedure TRubySampleList.LoadReferential;
var
  LibraryFileName,
  RepoDirName: TFileName;
  Buffer: TStringList;
  i: Integer;
  RepoUrl: string;

begin
  LibraryFileName := fEnvironment.FileSystem.Ruby.SamplesLibraryInformationFile;
  if FileExists(LibraryFileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(LibraryFileName);
        for i := 0 to Buffer.Count - 1 do
        begin
          RepoUrl := Right(ArraySeparator, Buffer[i]);
          RepoDirName := Left(ArraySeparator, Buffer[i]);
          Add(RepoDirName, RepoUrl);
        end;
    finally
      Buffer.Free;
    end;
  end;
end;

constructor TRubySampleList.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fList := TList.Create;
  LoadReferential;
end;

destructor TRubySampleList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

{ TRubyManager }

function TRubyManager.GetInstalled: Boolean;
begin
  Result := (not IsPostInstallMode) and
    DirectoryExists(Environment.FileSystem.Ruby.BaseDirectory);
end;

constructor TRubyManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fRepository := TDreamcastSoftwareDevelopmentRepository.Create(fEnvironment,
    fEnvironment.FileSystem.Ruby.BaseDirectory);
  fSamples := TRubySampleList.Create(fEnvironment);
end;

destructor TRubyManager.Destroy;
begin
  fSamples.Free;
  fRepository.Free;
  inherited Destroy;
end;

function TRubyManager.CloneRepository(var BufferOutput: string): Boolean;
const
  MRUBY_INSTALLATION_DIRECTORY = 'mruby';

var
  i: Integer;
  Dummy: string;
  Item: TRubySampleItem;

begin
  // mruby library
  Result := Environment.CloneRepository(
    Environment.Settings.Repositories.RubyURL,
    MRUBY_INSTALLATION_DIRECTORY,
    Environment.FileSystem.Ruby.BaseDirectory + '..\',
    BufferOutput
  );

  // samples: this is not critical
  Dummy := EmptyStr;
  for i := 0 to Samples.Count - 1 do
  begin
    Item := Samples[i];
    if not DirectoryExists(Item.FullPath) then
    begin
      Environment.CloneRepository(
        Item.RepositoryURL,
        Item.DirectoryName,
        Environment.FileSystem.Ruby.SamplesDirectory,
        Dummy
      );
    end;
  end;
end;

function TRubyManager.UpdateRepository(
  var BufferOutput: string): TUpdateOperationState;
begin
  Result := Environment.UpdateRepository(
    Environment.FileSystem.Ruby.BaseDirectory, BufferOutput);
end;

function TRubyManager.InitializeEnvironment: Boolean;
const
  SOURCE_FILE = 'examples\targets\build_config_dreamcast_shelf.rb';
  TARGET_FILE = 'build_config.rb';

var
  SourceFileName,
  TargetFileName: TFileName;

begin
  with Environment.FileSystem.Ruby do
  begin
    SourceFileName := BaseDirectory + SOURCE_FILE;
    TargetFileName := BaseDirectory + TARGET_FILE;
  end;

  // Remove the original file
  KillFile(TargetFileName);

  // Put the right sh-elf config file
  Result := CopyFile(SourceFileName, TargetFileName);
end;

function TRubyManager.Build(var BufferOutput: string): Boolean;
begin
  // Build mruby
  BufferOutput := Environment.ExecuteShellCommand('make',
    Environment.FileSystem.Ruby.BaseDirectory);

  // The result is OK if libkallisti is present, addons are optional...
  Result := FileExists(Environment.FileSystem.Ruby.RubyLibrary)
    and (not Environment.ShellCommandError);
end;

function TRubyManager.Uninstall: Boolean;
begin
  Result := KillDirectory(Environment.FileSystem.Ruby.BaseDirectory);
  Result := Result and KillDirectory(Environment.FileSystem.Ruby.SamplesDirectory);
end;

end.

