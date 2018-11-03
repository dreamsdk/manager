unit KOSMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  { TKallistiManager }
  TKallistiManager = class(TObject)
  private
    fGenRomFSFileName: TFileName;
    fEnvironShellScriptFileName: TFileName;
    fEnvironSampleShellScriptFileName: TFileName;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetBuilt: Boolean;
    function GetInstalled: Boolean;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Build(var BufferOutput: string): Boolean;
    function FixupHitachiNewlib(var BufferOutput: string): Boolean;
    property Built: Boolean read GetBuilt;
    property Installed: Boolean read GetInstalled;
  end;

implementation

uses
  FileUtil, SysTools;

{ TKallistiManager }

function TKallistiManager.GetInstalled: Boolean;
begin
  Result := DirectoryExists(fEnvironment.FileSystem.Kallisti.KallistiDirectory);
end;

function TKallistiManager.GetBuilt: Boolean;
begin
  Result := FileExists(fEnvironment.FileSystem.Kallisti.KallistiLibrary);
end;

constructor TKallistiManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
const
  ENVIRON_SHELL_SCRIPT_FILENAME = 'environ.sh';
  ENVIRON_SHELL_SCRIPT_SAMPLE_FILE_LOCATION = 'doc\environ.sh.sample';
  GENROMFS_LOCATION_FILE = 'utils\genromfs.exe';

begin
  fEnvironment := AEnvironment;

  fEnvironShellScriptFileName := fEnvironment.FileSystem.Kallisti.KallistiDirectory
    + ENVIRON_SHELL_SCRIPT_FILENAME;
  fEnvironSampleShellScriptFileName := fEnvironment.FileSystem.Kallisti.KallistiDirectory
    + ENVIRON_SHELL_SCRIPT_SAMPLE_FILE_LOCATION;

  fGenRomFSFileName := fEnvironment.FileSystem.Kallisti.KallistiDirectory + GENROMFS_LOCATION_FILE;
end;

function TKallistiManager.CloneRepository(var BufferOutput: string): Boolean;
const
  KALLISTI_INSTALLATION_DIRECTORY = 'kos';

begin
  Result := fEnvironment.CloneRepository(fEnvironment.Repositories.KallistiURL,
    KALLISTI_INSTALLATION_DIRECTORY,
    fEnvironment.FileSystem.Kallisti.KallistiDirectory + '..\', BufferOutput);
end;

function TKallistiManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := fEnvironment.UpdateRepository(fEnvironment.FileSystem.Kallisti.KallistiDirectory, BufferOutput);
end;

function TKallistiManager.InitializeEnvironment: Boolean;
const
  GENROMFS_PACKAGE_FILENAME = DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY + 'genromfs-0.5.1-cygwin-bin.tar.xz';

var
  CommandLine: string;
  WorkingDirectory: TFileName;

begin
  Result := True;

  if not FileExists(fEnvironShellScriptFileName) then
    Result := CopyFile(fEnvironSampleShellScriptFileName, fEnvironShellScriptFileName);

  if not FileExists(fGenRomFSFileName) then
  begin
    CommandLine := Format('tar xf %s', [GENROMFS_PACKAGE_FILENAME]);
    WorkingDirectory := fEnvironment.FileSystem.Kallisti.KallistiDirectory;
    fEnvironment.ExecuteShellCommand(CommandLine, WorkingDirectory);
  end;
end;

function TKallistiManager.Build(var BufferOutput: string): Boolean;
begin
  BufferOutput := fEnvironment.ExecuteShellCommand('make',
    fEnvironment.FileSystem.Kallisti.KallistiDirectory);
  Result := FileExists(fEnvironment.FileSystem.Kallisti.KallistiLibrary);
end;

function TKallistiManager.FixupHitachiNewlib(var BufferOutput: string): Boolean;
const
  FIXUP_SUPERH_NEWLIB = DREAMSDK_MSYS_INSTALL_HELPERS_DIRECTORY + 'fixup-sh4-newlib';
  SUCCESS_TAG = 'Done!';

var
  WorkingDirectory: TFileName;
  CommandLine: string;

begin
  WorkingDirectory := fEnvironment.FileSystem.Shell.DreamSDKDirectory;
  CommandLine := Format('%s --verbose', [FIXUP_SUPERH_NEWLIB]);
  BufferOutput := fEnvironment.ExecuteShellCommand(CommandLine, WorkingDirectory);
  Result := IsInString(SUCCESS_TAG, BufferOutput);
end;

end.

