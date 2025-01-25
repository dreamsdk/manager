unit KOSMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
  { TKallistiManager }
  TKallistiManager = class(TObject)
  private
    fForceNextRebuild: Boolean;
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
    function FixupHitachiNewlib: Boolean; overload;
    function FixupHitachiNewlib(var BufferOutput: string): Boolean; overload;
    procedure ForceNextRebuild;
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
    FileExists(Environment.FileSystem.Kallisti.KallistiConfigurationFileName) and (not fForceNextRebuild);
end;

constructor TKallistiManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
const
  ENVIRON_SHELL_SCRIPT_SAMPLE_LOCATION_FILE = 'doc\environ.sh.sample';

begin
  fForceNextRebuild := False;
  fEnvironment := AEnvironment;
  with Environment.FileSystem.Kallisti do
  begin
    fEnvironSampleShellScriptFileName := KallistiDirectory
      + ENVIRON_SHELL_SCRIPT_SAMPLE_LOCATION_FILE;
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

  // This is needed for KOS after a successful update
  if (Result = uosUpdateSuccess) then
  begin
    KillFile(Environment.FileSystem.Kallisti.KallistiLibrary);
    Environment.ExecuteShellCommand('make clean',
      Environment.FileSystem.Kallisti.KallistiDirectory);
  end;
end;

function TKallistiManager.InitializeEnvironment: Boolean;
begin
  Result := True;

{$IFDEF DEBUG}
  DebugLog(Format('InitializeEnvironment:' + sLineBreak +
    '  KallistiConfigurationFileName: "%s"' + sLineBreak +
    '  EnvironSampleShellScriptFileName: "%s"', [
      Environment.FileSystem.Kallisti.KallistiConfigurationFileName,
      fEnvironSampleShellScriptFileName
  ]));
{$ENDIF}

  // environ.sh
  if not FileExists(Environment.FileSystem.Kallisti.KallistiConfigurationFileName) then
    Result := CopyFile(fEnvironSampleShellScriptFileName, Environment.FileSystem
      .Kallisti.KallistiConfigurationFileName);
end;

function TKallistiManager.Build(var BufferOutput: string): Boolean;
begin
  // Handle force next rebuild if necessary
  // This is typically used when the toolchain has been changed (from GCC 4 to 9) in Manager
  if fForceNextRebuild then
  begin
    fForceNextRebuild := False;
    Environment.ExecuteShellCommand('make distclean',
      Environment.FileSystem.Kallisti.KallistiDirectory);
  end;

  // Build libkallisti
  BufferOutput := Environment.ExecuteShellCommand('make',
    Environment.FileSystem.Kallisti.KallistiDirectory);

  // The result is OK if libkallisti is present, addons are optional...
  Result := FileExists(Environment.FileSystem.Kallisti.KallistiLibrary)
    and (not Environment.ShellCommandError);
end;

function TKallistiManager.FixupHitachiNewlib: Boolean; overload;
var
  Dummy: string;

begin
  Dummy := EmptyStr;
  Result := FixupHitachiNewlib(Dummy);
end;

function TKallistiManager.FixupHitachiNewlib(var BufferOutput: string): Boolean; overload;
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

procedure TKallistiManager.ForceNextRebuild;
begin
  fForceNextRebuild := True;
end;

end.

