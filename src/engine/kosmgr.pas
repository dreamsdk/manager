unit KOSMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  { TKallistiManager }
  TKallistiManager = class(TObject)
  private
    fEnvironShellScriptFileName: TFileName;
    fEnvironSampleShellScriptFileName: TFileName;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetInstalled: Boolean;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironShellScript: Boolean;
    function BuildKallistiOS(var BufferOutput: string): Boolean;
    function FixupHitachiNewlib(var BufferOutput: string): Boolean;
    property Installed: Boolean read GetInstalled;
  end;

implementation

uses
  FileUtil, SysTools;

resourcestring
  KallistiFileSystemInstallationDirectory = 'kos';
  EnvironShellScriptFileName = 'environ.sh';
  EnvironSampleShellScriptFileName = 'doc/environ.sh.sample';

{ TKallistiManager }

function TKallistiManager.GetInstalled: Boolean;
begin
  Result := DirectoryExists(fEnvironment.FileSystem.KallistiDirectory);
end;

constructor TKallistiManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fEnvironShellScriptFileName := fEnvironment.FileSystem.KallistiDirectory
    + EnvironShellScriptFileName;
  fEnvironSampleShellScriptFileName := fEnvironment.FileSystem.KallistiDirectory
    + EnvironSampleShellScriptFileName;
end;

function TKallistiManager.CloneRepository(var BufferOutput: string): Boolean;
begin
  Result := fEnvironment.CloneRepository(fEnvironment.KallistiURL,
    KallistiFileSystemInstallationDirectory,
    fEnvironment.FileSystem.KallistiDirectory + '..\', BufferOutput);
end;

function TKallistiManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := fEnvironment.UpdateRepository(fEnvironment.FileSystem.KallistiDirectory,
    BufferOutput);
end;

function TKallistiManager.InitializeEnvironShellScript: Boolean;
begin
  Result := True;
  if not FileExists(fEnvironShellScriptFileName) then
    CopyFile(fEnvironSampleShellScriptFileName, fEnvironShellScriptFileName);
end;

function TKallistiManager.BuildKallistiOS(var BufferOutput: string): Boolean;
begin
  BufferOutput := fEnvironment.ExecuteShellCommand('make',
    fEnvironment.FileSystem.KallistiDirectory);
  Result := FileExists(fEnvironment.FileSystem.KallistiLibrary);
end;

function TKallistiManager.FixupHitachiNewlib(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Done!';

var
  WorkingDirectory: TFileName;

begin
  WorkingDirectory := ExtractFilePath(fEnvironment.FileSystem.FixupHitachiNewlibExecutable);
  BufferOutput := fEnvironment.ExecuteShellCommand(fEnvironment.FileSystem
    .FixupHitachiNewlibExecutable, WorkingDirectory);
  Result := IsInString(SUCCESS_TAG, BufferOutput);
end;

end.

