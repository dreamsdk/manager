unit KOSMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  { TKallistiManager }
  TKallistiManager = class(TObject)
  private
    fGenerateRomFileSystemBinaryFileName: TFileName;
    fGenerateRomFileSystemMakefileFileName: TFileName;
    fEnvironSampleShellScriptFileName: TFileName;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fRepository: TDreamcastSoftwareDevelopmentRepository;
    function GetBuilt: Boolean;
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
    function FixupHitachiNewlib(var BufferOutput: string): Boolean;
    property Built: Boolean read GetBuilt;
    property Installed: Boolean read GetInstalled;
    property Repository: TDreamcastSoftwareDevelopmentRepository
      read fRepository;
  end;

implementation

uses
  FileUtil, SysTools, FSTools;

{ TKallistiManager }

function TKallistiManager.GetInstalled: Boolean;
begin
  Result := Environment.IsComponentInstalled(Environment.FileSystem.Kallisti
    .KallistiDirectory);
end;

function TKallistiManager.GetBuilt: Boolean;
begin
  Result := FileExists(Environment.FileSystem.Kallisti.KallistiLibrary) and
    FileExists(Environment.FileSystem.Kallisti.KallistiConfigurationFileName);
end;

constructor TKallistiManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
const
  ENVIRON_SHELL_SCRIPT_SAMPLE_LOCATION_FILE = 'doc\environ.sh.sample';
  GENROMFS_BINARY_LOCATION_FILE = 'utils\genromfs\genromfs.exe';
  GENROMFS_MAKEFILE_LOCATION_FILE = 'utils\genromfs\Makefile';

begin
  fEnvironment := AEnvironment;
  with Environment.FileSystem.Kallisti do
  begin
    fEnvironSampleShellScriptFileName := KallistiDirectory
      + ENVIRON_SHELL_SCRIPT_SAMPLE_LOCATION_FILE;
    fGenerateRomFileSystemBinaryFileName := KallistiDirectory
      + GENROMFS_BINARY_LOCATION_FILE;
    fGenerateRomFileSystemMakefileFileName := KallistiDirectory
      + GENROMFS_MAKEFILE_LOCATION_FILE;
    fRepository := TDreamcastSoftwareDevelopmentRepository.Create(fEnvironment,
      KallistiDirectory);
  end;
end;

destructor TKallistiManager.Destroy;
begin
  fRepository.Free;
  inherited Destroy;
end;

function TKallistiManager.CloneRepository(var BufferOutput: string): Boolean;
const
  KALLISTI_INSTALLATION_DIRECTORY = 'kos';

begin
  Result := Environment.CloneRepository(Environment.Settings.Repositories.KallistiURL,
    KALLISTI_INSTALLATION_DIRECTORY,
    Environment.FileSystem.Kallisti.KallistiDirectory + '..\', BufferOutput);
end;

function TKallistiManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := Environment.UpdateRepository(
    Environment.FileSystem.Kallisti.KallistiDirectory, BufferOutput);
end;

function TKallistiManager.InitializeEnvironment: Boolean;
const
  GENROMFS_PACKAGE_FILENAME = DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY
    + 'genromfs-0.5.1-cygwin-bin.tar.xz';

var
  CommandLine: string;
  WorkingDirectory: TFileName;

begin
  Result := True;

  // environ.sh
  if not FileExists(Environment.FileSystem.Kallisti.KallistiConfigurationFileName) then
    Result := CopyFile(fEnvironSampleShellScriptFileName, Environment.FileSystem
      .Kallisti.KallistiConfigurationFileName);

  // genromfs
  if not FileExists(fGenerateRomFileSystemBinaryFileName) then
  begin
    CommandLine := Format('tar xf %s', [GENROMFS_PACKAGE_FILENAME]);
    WorkingDirectory := Environment.FileSystem.Kallisti.KallistiDirectory;
    Environment.ExecuteShellCommand(CommandLine, WorkingDirectory);
    PatchTextFile(
      fGenerateRomFileSystemMakefileFileName,
      'all: genromfs',
      'all:' + sLineBreak + TabStr + '@echo "(genromfs building is disabled)"'
    );
  end;
end;

function TKallistiManager.Build(var BufferOutput: string): Boolean;
begin
  // Build libkallisti
  BufferOutput := Environment.ExecuteShellCommand('make',
    Environment.FileSystem.Kallisti.KallistiDirectory);

  // The result is OK if libkallisti is present, addons are optional...
  Result := FileExists(Environment.FileSystem.Kallisti.KallistiLibrary)
    and (not Environment.ShellCommandError);
end;

function TKallistiManager.FixupHitachiNewlib(var BufferOutput: string): Boolean;
const
  FIXUP_SUPERH_NEWLIB = DREAMSDK_MSYS_INSTALL_HELPERS_DIRECTORY + 'fixup-sh4-newlib';
  SUCCESS_TAG = 'Done!';

var
  WorkingDirectory: TFileName;
  CommandLine: string;

begin
  WorkingDirectory := Environment.FileSystem.Shell.DreamSDKDirectory;
  CommandLine := Format('%s --verbose', [FIXUP_SUPERH_NEWLIB]);
  BufferOutput := Environment.ExecuteShellCommand(CommandLine, WorkingDirectory);
  Result := IsInString(SUCCESS_TAG, BufferOutput);
end;

end.

