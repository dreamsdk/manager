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
    fRubyCompilerShellScriptFileName: TFileName;
    fForceNextRebuild: Boolean;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fRepository: TDreamcastSoftwareDevelopmentRepository;
    fSamples: TRubySampleList;

    function GetBuilt: Boolean;
    function GetInstalled: Boolean;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;

    function CloneRepository(var BufferOutput: string): Boolean;
    procedure ForceNextRebuild;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Build(var BufferOutput: string): Boolean;
    function Uninstall: Boolean;

    property Built: Boolean read GetBuilt;
    property Installed: Boolean read GetInstalled;
    property Repository: TDreamcastSoftwareDevelopmentRepository
      read fRepository;
    property Samples: TRubySampleList read fSamples;
  end;

implementation

uses
  FileUtil,
  FSTools,
  SysTools,
  StrTools;

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
  Result := Environment.IsComponentInstalled(Environment.FileSystem.Ruby
    .BaseDirectory);
end;

function TRubyManager.GetBuilt: Boolean;
begin
  Result := FileExists(Environment.FileSystem.Ruby.RubyLibrary) and (not fForceNextRebuild);
end;

constructor TRubyManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
const
  MRBC_SHELL_SCRIPT_LOCATION_FILE = 'bin\mrbc';

begin
  fForceNextRebuild := False;
  fEnvironment := AEnvironment;
  with Environment.FileSystem.Ruby do
    fRubyCompilerShellScriptFileName := BaseDirectory + MRBC_SHELL_SCRIPT_LOCATION_FILE;
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
  Sample: TRubySampleItem;

begin
  // mruby library
  Result := Environment.CloneRepository(
    Environment.Settings.Repositories.RubyURL,
    MRUBY_INSTALLATION_DIRECTORY,
    Environment.FileSystem.Ruby.BaseDirectory + '..\',
    BufferOutput
  );

  // Samples: this is not critical
  Dummy := EmptyStr;
  for i := 0 to Samples.Count - 1 do
  begin
    Sample := Samples[i];
    if not DirectoryExists(Sample.FullPath) then
      Environment.CloneRepository(
        Sample.RepositoryURL,
        Sample.DirectoryName,
        Environment.FileSystem.Ruby.SamplesDirectory,
        Dummy
      );
  end;
end;

procedure TRubyManager.ForceNextRebuild;
begin
  fForceNextRebuild := True;
end;

function TRubyManager.UpdateRepository(
  var BufferOutput: string): TUpdateOperationState;
var
  i: Integer;
  Dummy: string;
  Sample: TRubySampleItem;

begin
  // mruby library
  Result := Environment.UpdateRepository(
    Environment.FileSystem.Ruby.BaseDirectory, BufferOutput);

  // This is needed after a successful update
  if (Result = uosUpdateSuccess) then
  begin
    KillFile(Environment.FileSystem.Ruby.RubyLibrary);
    Environment.ExecuteShellCommand('make clean',
      Environment.FileSystem.Ruby.BaseDirectory);
  end;

  // Samples (try to update them if available)
  Dummy := EmptyStr;
  for i := 0 to Samples.Count - 1 do
  begin
    Sample := Samples[i];
    if DirectoryExists(Sample.FullPath) then
      Environment.UpdateRepository(Sample.FullPath, Dummy);
  end;
end;

function TRubyManager.InitializeEnvironment: Boolean;
const
  MRBC_CONTENT = '#!/usr/bin/env bash' + sLineBreak
    + '/opt/mruby/build/host/bin/mrbc "$@"';

begin
  Result := True;

  // Create "/opt/mruby/bin/mrbc"
  // As by default on Windows, mruby will create "mrbc.bat" which is useless for us.
  if not FileExists(fRubyCompilerShellScriptFileName) then
  begin
    ForceDirectories(ExtractFilePath(fRubyCompilerShellScriptFileName));
    SaveStringToFile(MRBC_CONTENT, fRubyCompilerShellScriptFileName);
  end;
end;

function TRubyManager.Build(var BufferOutput: string): Boolean;
begin
  // Handle force next rebuild if necessary
  // This is typically used when the toolchain has been changed (from GCC 4 to 9) in Manager
  if fForceNextRebuild then
  begin
    fForceNextRebuild := False;
    Environment.ExecuteShellCommand('make clean',
      Environment.FileSystem.Ruby.BaseDirectory);
    Environment.ExecuteShellCommand('make clean MRUBY_CONFIG=dreamcast_shelf',
      Environment.FileSystem.Ruby.BaseDirectory);
  end;

  // Build mruby
  BufferOutput := Environment.ExecuteShellCommand('make MRUBY_CONFIG=dreamcast_shelf',
    Environment.FileSystem.Ruby.BaseDirectory);

  // The result is OK if libmruby is present
  Result := FileExists(Environment.FileSystem.Ruby.RubyLibrary)
    and (not Environment.ShellCommandError);
end;

function TRubyManager.Uninstall: Boolean;
begin
  Result := KillDirectory(Environment.FileSystem.Ruby.BinariesDirectory);
  Result := Result and KillDirectory(Environment.FileSystem.Ruby.BuildDirectory);
end;

end.

