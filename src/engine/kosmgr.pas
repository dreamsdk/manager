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

resourcestring
  KallistiFileSystemInstallationDirectory = 'kos';
  EnvironShellScriptFileName = 'environ.sh';
  EnvironSampleShellScriptFileName = 'doc\environ.sh.sample';
  GenRomFSPackageFileName = '/opt/dcsdk/helpers/genromfs-0.5.1-cygwin-bin.tar.xz';
  GenRomFSFileName = 'utils\genromfs.exe';

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
begin
  fEnvironment := AEnvironment;

  fEnvironShellScriptFileName := fEnvironment.FileSystem.Kallisti.KallistiDirectory
    + EnvironShellScriptFileName;
  fEnvironSampleShellScriptFileName := fEnvironment.FileSystem.Kallisti.KallistiDirectory
    + EnvironSampleShellScriptFileName;

  fGenRomFSFileName := fEnvironment.FileSystem.Kallisti.KallistiDirectory + GenRomFSFileName;
end;

function TKallistiManager.CloneRepository(var BufferOutput: string): Boolean;
begin
  Result := fEnvironment.CloneRepository(fEnvironment.Repositories.KallistiURL,
    KallistiFileSystemInstallationDirectory,
    fEnvironment.FileSystem.Kallisti.KallistiDirectory + '..\', BufferOutput);
end;

function TKallistiManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := fEnvironment.UpdateRepository(fEnvironment.FileSystem.Kallisti.KallistiDirectory, BufferOutput);
end;

function TKallistiManager.InitializeEnvironment: Boolean;
var
  CommandLine: string;
  WorkingDirectory: TFileName;

begin
  Result := True;

  if not FileExists(fEnvironShellScriptFileName) then
    Result := CopyFile(fEnvironSampleShellScriptFileName, fEnvironShellScriptFileName);

  if not FileExists(fGenRomFSFileName) then
  begin
    CommandLine := Format('tar xf %s', [GenRomFSPackageFileName]);
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
  SUCCESS_TAG = 'Done!';

var
  WorkingDirectory: TFileName;
  CommandLine: string;

begin
  WorkingDirectory := ExtractFilePath(fEnvironment.FileSystem.ToolchainSuperH.FixupHitachiNewlibExecutable);
  CommandLine := Format('%s --verbose', [fEnvironment.FileSystem.ToolchainSuperH.FixupHitachiNewlibExecutable]);
  BufferOutput := fEnvironment.ExecuteShellCommand(CommandLine, WorkingDirectory);
  Result := IsInString(SUCCESS_TAG, BufferOutput);
end;

end.

