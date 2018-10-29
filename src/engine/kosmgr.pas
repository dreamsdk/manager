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
    property Installed: Boolean read GetInstalled;
  end;

implementation

uses
  FileUtil;

resourcestring
  KallistiFileSystemInstallationDirectory = 'kos';
  EnvironShellScriptFileName = 'environ.sh';
  EnvironSampleShellScriptFileName = 'doc/environ.sh.sample';

{ TKallistiManager }

function TKallistiManager.GetInstalled: Boolean;
begin
  Result := DirectoryExists(fEnvironment.FileSystem.KallistiOS);
end;

constructor TKallistiManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fEnvironShellScriptFileName := fEnvironment.FileSystem.KallistiOS
    + EnvironShellScriptFileName;
  fEnvironSampleShellScriptFileName := fEnvironment.FileSystem.KallistiOS
    + EnvironSampleShellScriptFileName;
end;

function TKallistiManager.CloneRepository(var BufferOutput: string): Boolean;
begin
  Result := fEnvironment.CloneRepository(fEnvironment.KallistiURL,
    KallistiFileSystemInstallationDirectory,
    fEnvironment.FileSystem.KallistiOS + '..\', BufferOutput);
end;

function TKallistiManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := fEnvironment.UpdateRepository(fEnvironment.FileSystem.KallistiOS,
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
    fEnvironment.FileSystem.KallistiOS);
  Result := True;//TODO
end;

end.

