unit Environ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RunCmd, Settings;

const
  DCLOAD_IP_INSTALLATION_DIRECTORY = 'dcload-ip';
  DCLOAD_SERIAL_INSTALLATION_DIRECTORY = 'dcload-serial';

  DREAMSDK_RUNNER_EXECUTABLE = 'dreamsdk-runner.exe';
  DREAMSDK_LAUNCHER_EXECUTABLE = 'dreamsdk-shell.exe';
  DREAMSDK_HELP_FILE = 'dreamsdk.chm';
  DREAMSDK_MSYS_INSTALL_DIRECTORY = '/opt/dreamsdk/';
  DREAMSDK_MSYS_INSTALL_HELPERS_DIRECTORY = DREAMSDK_MSYS_INSTALL_DIRECTORY + 'helpers/';
  DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY = DREAMSDK_MSYS_INSTALL_DIRECTORY + 'packages/';

type
  EKallistiReferentialNotAvailable = class(Exception);

  TRepositoryKind = (
    rkUndefined,
    rkKallisti,
    rkKallistiPorts,
    rkDreamcastTool, // Both Serial and Internet Protocol
    rkDreamcastToolSerial,
    rkDreamcastToolInternetProtocol
  );

  TUpdateOperationState = (
    uosUndefined,
    uosUpdateSuccess,
    uosUpdateUseless,
    uosUpdateFailed
  );

  TToolchainKind = (
    tkSuperH,
    tkARM,
    tkWin32
  );

  { TDreamcastSoftwareDevelopmentFileSystemDreamcastTool }
  TDreamcastSoftwareDevelopmentFileSystemDreamcastTool = class(TObject)
  private
    fBaseDirectory: TFileName;
    fConfigurationFileName: TFileName;
    fInternetProtocolDirectory: TFileName;
    fInternetProtocolExecutable: TFileName;
    fSerialDirectory: TFileName;
    fSerialExecutable: TFileName;
  public
    function ResetRepository: Boolean;
    function ResetRepositorySerial: Boolean;
    function ResetRepositoryInternetProtocol: Boolean;
    property BaseDirectory: TFileName read fBaseDirectory;
    property ConfigurationFileName: TFileName read fConfigurationFileName;
    property InternetProtocolDirectory: TFileName read fInternetProtocolDirectory;
    property InternetProtocolExecutable: TFileName read fInternetProtocolExecutable;
    property SerialDirectory: TFileName read fSerialDirectory;
    property SerialExecutable: TFileName read fSerialExecutable;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemKallisti }
  TDreamcastSoftwareDevelopmentFileSystemKallisti = class(TObject)
  private
    fKallistiConfigurationFileName: TFileName;
    fKallistiDirectory: TFileName;
    fKallistiLibrary: TFileName;
    fKallistiChangeLogFile: TFileName;
    fKallistiPortsDirectory: TFileName;
    fKallistiPortsLibraryInformationFile: TFileName;
    fKallistiUtilitiesDirectory: TFileName;
  public
    function ResetRespository: Boolean;
    function ResetRepositoryKallisti: Boolean;
    function ResetRepositoryKallistiPorts: Boolean;
    property KallistiPortsDirectory: TFileName read fKallistiPortsDirectory;
    property KallistiPortsLibraryInformationFile: TFileName read fKallistiPortsLibraryInformationFile;
    property KallistiDirectory: TFileName read fKallistiDirectory;
    property KallistiUtilitiesDirectory: TFileName read fKallistiUtilitiesDirectory;
    property KallistiLibrary: TFileName read fKallistiLibrary;
    property KallistiChangeLogFile: TFileName read fKallistiChangeLogFile;
    property KallistiConfigurationFileName: TFileName read fKallistiConfigurationFileName;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemToolchain }
  TDreamcastSoftwareDevelopmentFileSystemToolchain = class(TObject)
  private
    fBinutilsExecutable: TFileName;
    fGCCExecutable: TFileName;
    fGDBExecutable: TFileName;
    fKind: TToolchainKind;
    fNewlibBinary: TFileName;
    fToolchainInstalled: Boolean;
  public
    constructor Create(AToolchainKind: TToolchainKind);
    property BinutilsExecutable: TFileName read fBinutilsExecutable;
    property GCCExecutable: TFileName read fGCCExecutable;
    property GDBExecutable: TFileName read fGDBExecutable;
    property NewlibBinary: TFileName read fNewlibBinary;
    property Installed: Boolean read fToolchainInstalled;
    property Kind: TToolchainKind read fKind;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemShell }
  TDreamcastSoftwareDevelopmentFileSystemShell = class(TObject)
  private
    fCodeBlocksPatcherExecutable: TFileName;
    fConfigurationDirectory: TFileName;
    fDreamSDKDirectory: TFileName;
    fDreamSDKExecutable: TFileName;
    fHelpFileName: TFileName;
    fHomeDirectory: TFileName;
    fIntegratedDevelopmentEnvironmentConfigurationFile: TFileName;
    fMinGWGetExecutable: TFileName;
    fRunnerExecutable: TFileName;
    fShellExecutable: TFileName;
  public
    property DreamSDKDirectory: TFileName read fDreamSDKDirectory;
    property HelpFileName: TFileName read fHelpFileName;
    property LauncherExecutable: TFileName read fDreamSDKExecutable;
    property RunnerExecutable: TFileName read fRunnerExecutable;
    property ConfigurationDirectory: TFileName read fConfigurationDirectory;
    property ShellExecutable: TFileName read fShellExecutable;
    property MinGWGetExecutable: TFileName read fMinGWGetExecutable;
    property HomeDirectory: TFileName read fHomeDirectory;
    property IntegratedDevelopmentEnvironmentConfigurationFile: TFileName
      read fIntegratedDevelopmentEnvironmentConfigurationFile;
    property CodeBlocksPatcherExecutable: TFileName
      read fCodeBlocksPatcherExecutable;
  end;

  { TDreamcastSoftwareDevelopmentFileSystem }
  TDreamcastSoftwareDevelopmentFileSystem = class(TObject)
  private
    fDreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool;
    fKallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti;
    fShell: TDreamcastSoftwareDevelopmentFileSystemShell;
    fToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain;
    fToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain;
    fToolchainWin32: TDreamcastSoftwareDevelopmentFileSystemToolchain;
  protected
    function GetReferentialDirectory: TFileName;
    procedure ComputeFileSystemObjectValues(InstallPath: TFileName);
  public
    constructor Create;
    destructor Destroy; override;
    function ResetRepository(const RepositoryKind: TRepositoryKind): Boolean;
    property DreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool
      read fDreamcastTool;
    property Kallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti
      read fKallisti;
    property Shell: TDreamcastSoftwareDevelopmentFileSystemShell read fShell;
    property ToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainARM;
    property ToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainSuperH;
    property ToolchainWin32: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainWin32;
  end;

  { TDreamcastSoftwareDevelopmentEnvironment }
  TDreamcastSoftwareDevelopmentEnvironment = class(TObject)
  private
    fShellCommandNewLine: TNewLineEvent;
    fShellCommandRunner: TRunCommand;
    fFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    fShellCommandBufferOutput: string;
  protected
    procedure LoadConfig;
    procedure HandleShellCommandRunnerNewLine(Sender: TObject; NewLine: string);
    procedure HandleShellCommandRunnerTerminate(Sender: TObject);
    function ExecuteShellCommandRunner(const CommandLine: string): string;
    function GetOfflineFileName(const WorkingDirectory: TFileName): TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AbortShellCommand;
    function ExecuteShellCommand(CommandLine: string;
      WorkingDirectory: TFileName): string;
    procedure PauseShellCommand;
    procedure ResumeShellCommand;
    function CloneRepository(const URL: string; const TargetDirectoryName,
      WorkingDirectory: TFileName; var BufferOutput: string): Boolean;
    function GetRepositoryVersion(const WorkingDirectory: TFileName): string;
    function IsOfflineRepository(const RepositoryDirectory: TFileName): Boolean;
    function IsRepositoryReady(const WorkingDirectory: TFileName): Boolean;
    function UpdateRepository(const WorkingDirectory: TFileName;
      var BufferOutput: string): TUpdateOperationState; overload;
    property FileSystem: TDreamcastSoftwareDevelopmentFileSystem read fFileSystem;
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
    property OnShellCommandNewLine: TNewLineEvent read fShellCommandNewLine
      write fShellCommandNewLine;
  end;

  { TDreamcastSoftwareDevelopmentRepository }
  TDreamcastSoftwareDevelopmentRepository = class(TObject)
  private
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fOfflineFileName: TFileName;
    fRepositoryDirectory: TFileName;
    function GetOffline: Boolean;
    function GetReady: Boolean;
    function GetURL: string;
    function GetVersion: string;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property OfflineFileName: TFileName read fOfflineFileName;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
      ARepositoryDirectory: TFileName);
    property Directory: TFileName read fRepositoryDirectory;
    property Ready: Boolean read GetReady;
    property Offline: Boolean read GetOffline;
    property Version: string read GetVersion;
    property URL: string read GetURL;
  end;

implementation

uses
  RefBase, SysTools, FSTools, RunTools;

{ TDreamcastSoftwareDevelopmentRepository }

function TDreamcastSoftwareDevelopmentRepository.GetOffline: Boolean;
begin
  Result := FileExists(OfflineFileName);
end;

function TDreamcastSoftwareDevelopmentRepository.GetReady: Boolean;
begin
  Result := Environment.IsRepositoryReady(Directory);
end;

function TDreamcastSoftwareDevelopmentRepository.GetURL: string;
begin
  Result := EmptyStr;
  if Ready then
    Result := Trim(Run('git', 'config --get remote.origin.url', fRepositoryDirectory));
end;

function TDreamcastSoftwareDevelopmentRepository.GetVersion: string;
begin
  Result := EmptyStr;
  if Ready then
    Result := Environment.GetRepositoryVersion(Directory);
end;

constructor TDreamcastSoftwareDevelopmentRepository.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  ARepositoryDirectory: TFileName);
begin
  fEnvironment := AEnvironment;
  fRepositoryDirectory := ARepositoryDirectory;
  fOfflineFileName := Environment.GetOfflineFileName(fRepositoryDirectory);
end;

{ TDreamcastSoftwareDevelopmentFileSystemKallisti }

function TDreamcastSoftwareDevelopmentFileSystemKallisti.ResetRespository: Boolean;
begin
  Result := ResetRepositoryKallisti and ResetRepositoryKallistiPorts;
end;

function TDreamcastSoftwareDevelopmentFileSystemKallisti.ResetRepositoryKallisti: Boolean;
begin
  Result := KillDirectory(KallistiDirectory);
end;

function TDreamcastSoftwareDevelopmentFileSystemKallisti.ResetRepositoryKallistiPorts: Boolean;
begin
  Result := KillDirectory(KallistiPortsDirectory);
end;

{ TDreamcastSoftwareDevelopmentFileSystemDreamcastTool }

function TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.ResetRepository: Boolean;
begin
  Result := ResetRepositorySerial and ResetRepositoryInternetProtocol;
end;

function TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.ResetRepositorySerial: Boolean;
begin
  Result := KillDirectory(SerialDirectory);
end;

function TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.ResetRepositoryInternetProtocol: Boolean;
begin
  Result := KillDirectory(InternetProtocolDirectory);
end;

{ TDreamcastSoftwareDevelopmentFileSystemToolchain }

constructor TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(
  AToolchainKind: TToolchainKind);
begin
  fKind := AToolchainKind;
end;

{ TDreamcastSoftwareDevelopmentFileSystem }

function TDreamcastSoftwareDevelopmentFileSystem.GetReferentialDirectory: TFileName;
begin
  Result := GetConfigurationDirectory + 'referentials' + DirectorySeparator;
end;

procedure TDreamcastSoftwareDevelopmentFileSystem.ComputeFileSystemObjectValues(
  InstallPath: TFileName);
var
  DreamSDKBase,
  MSYSBase,
  ToolchainBase,
  ToolchainBaseSuperH,
  ToolchainBaseARM,
  ToolchainBaseWin32: TFileName;

begin
  MSYSBase := GetMSysBaseDirectory;
  ToolchainBase := MSYSBase + 'opt\toolchains\dc\';

  // Translate DREAMSDK_MSYS_INSTALL_DIRECTORY to Windows location
  DreamSDKBase := MSYSBase + UnixPathToSystem(DREAMSDK_MSYS_INSTALL_DIRECTORY);

  with fShell do
  begin
    // MinGW/MSYS
    fMinGWGetExecutable := InstallPath + 'bin\mingw-get.exe';
    fShellExecutable := MSYSBase + 'bin\sh.exe';
    fHomeDirectory := MSYSBase + 'home\' + GetEnvironmentVariable('USERNAME')
      + DirectorySeparator;

    // DreamSDK
    fDreamSDKDirectory := DreamSDKBase;
    fDreamSDKExecutable := fDreamSDKDirectory + DREAMSDK_LAUNCHER_EXECUTABLE;
    fRunnerExecutable := fDreamSDKDirectory + DREAMSDK_RUNNER_EXECUTABLE;
    fConfigurationDirectory := GetConfigurationDirectory;
    fIntegratedDevelopmentEnvironmentConfigurationFile := GetConfigurationDirectory + 'ide.conf';
    fHelpFileName := fDreamSDKDirectory + DREAMSDK_HELP_FILE;
    fCodeBlocksPatcherExecutable := fDreamSDKDirectory +
      'packages\ide\codeblocks\codeblocks-patcher.exe';
  end;

  // Toolchain for Super-H (Hitachi SH-4)
  ToolchainBaseSuperH := ToolchainBase + 'sh-elf\';
  with fToolchainSuperH do
  begin
    fToolchainInstalled := DirectoryExists(ToolchainBaseSuperH);
    fBinutilsExecutable := ToolchainBaseSuperH + 'bin\sh-elf-ld.exe';
    fGCCExecutable := ToolchainBaseSuperH + 'bin\sh-elf-gcc.exe';
    fGDBExecutable := ToolchainBaseSuperH + 'bin\sh-elf-gdb.exe';
    fNewlibBinary := ToolchainBaseSuperH + 'sh-elf\lib\libnosys.a';
  end;

  // Toolchain for ARM
  ToolchainBaseARM := ToolchainBase + 'arm-eabi\';
  with fToolchainARM do
  begin
    fToolchainInstalled := DirectoryExists(ToolchainBaseARM);
    fBinutilsExecutable := ToolchainBaseARM + 'bin\arm-eabi-ld.exe';
    fGCCExecutable := ToolchainBaseARM + 'bin\arm-eabi-gcc.exe';
    fGDBExecutable := ''; // Not Applicable
    fNewlibBinary := ''; // Not Applicable
  end;

  // Toolchain for Win32 (Windows)
  ToolchainBaseWin32 := GetInstallationBaseDirectory;
  with fToolchainWin32 do
  begin
    fToolchainInstalled := DirectoryExists(ToolchainBaseWin32);
    fBinutilsExecutable := ToolchainBaseWin32 + 'bin\ld.exe';
    fGCCExecutable := ToolchainBaseWin32 + 'bin\gcc.exe';
    fGDBExecutable := ToolchainBaseWin32 + 'bin\gdb.exe';
    fNewlibBinary := ''; // Not Applicable
  end;

  // dcload/dc-tool
  with fDreamcastTool do
  begin
    fBaseDirectory := ToolchainBase + 'dcload\';
    fInternetProtocolDirectory := fBaseDirectory + DCLOAD_IP_INSTALLATION_DIRECTORY + '\';
    fSerialDirectory := fBaseDirectory + DCLOAD_SERIAL_INSTALLATION_DIRECTORY + '\';
    fSerialExecutable := ToolchainBase + 'bin\dc-tool-ser.exe';
    fInternetProtocolExecutable := ToolchainBase + 'bin\dc-tool-ip.exe';
    fConfigurationFileName := GetConfigurationDirectory + 'dc-tool.conf';
  end;

  // KallistiOS
  with fKallisti do
  begin
    fKallistiPortsDirectory := ToolchainBase + 'kos-ports\';
    fKallistiDirectory := ToolchainBase + 'kos\';
    fKallistiUtilitiesDirectory := fKallistiDirectory + 'utils\';
    fKallistiLibrary := KallistiDirectory + 'lib\dreamcast\libkallisti.a';
    fKallistiChangeLogFile := KallistiDirectory + 'doc\CHANGELOG';
    fKallistiConfigurationFileName := KallistiDirectory + 'environ.sh';
    fKallistiPortsLibraryInformationFile := GetReferentialDirectory + 'koslib.conf';
    if not FileExists(fKallistiPortsLibraryInformationFile) then
      raise EKallistiReferentialNotAvailable.CreateFmt(
        'The KallistiOS Ports library referential file was not found: %s', [fKallistiPortsLibraryInformationFile]);
  end;
end;

constructor TDreamcastSoftwareDevelopmentFileSystem.Create;
begin
  fDreamcastTool := TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.Create;
  fKallisti := TDreamcastSoftwareDevelopmentFileSystemKallisti.Create;
  fToolchainARM := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkARM);
  fToolchainSuperH := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkSuperH);
  fToolchainWin32 := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkWin32);
  fShell := TDreamcastSoftwareDevelopmentFileSystemShell.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystem.Destroy;
begin
  fDreamcastTool.Free;
  fKallisti.Free;
  fToolchainWin32.Free;
  fToolchainARM.Free;
  fToolchainSuperH.Free;
  fShell.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentFileSystem.ResetRepository(
  const RepositoryKind: TRepositoryKind): Boolean;
begin
  Result := False;
  case RepositoryKind of
    rkKallisti:
      Result := Kallisti.ResetRepositoryKallisti;
    rkKallistiPorts:
      Result := Kallisti.ResetRepositoryKallistiPorts;
    rkDreamcastTool:
      Result := DreamcastTool.ResetRepository;
    rkDreamcastToolSerial:
      Result := DreamcastTool.ResetRepositorySerial;
    rkDreamcastToolInternetProtocol:
      Result := DreamcastTool.ResetRepositoryInternetProtocol;
  end;
end;

{ TDreamcastSoftwareDevelopmentEnvironment }

procedure TDreamcastSoftwareDevelopmentEnvironment.LoadConfig;
begin
  Settings.LoadConfiguration;
  FileSystem.ComputeFileSystemObjectValues(Settings.InstallPath);
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.HandleShellCommandRunnerNewLine(
  Sender: TObject; NewLine: string);
begin
  if Assigned(fShellCommandNewLine) then
    fShellCommandNewLine(Self, NewLine);
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.HandleShellCommandRunnerTerminate
  (Sender: TObject);
begin
  fShellCommandBufferOutput := fShellCommandRunner.BufferOutput.Text;
end;

function TDreamcastSoftwareDevelopmentEnvironment.ExecuteShellCommandRunner(
  const CommandLine: string): string;
var
  i: Integer;

begin
  Result := '';
  AbortShellCommand;

  FreeAndNil(fShellCommandRunner);
  fShellCommandRunner := TRunCommand.Create(True);
  with fShellCommandRunner do
  begin
    Executable := fFileSystem.Shell.ShellExecutable;

    Parameters.Add('--login');
    Parameters.Add('-i');

    for i := 1 to GetEnvironmentVariableCount do
      Environment.Add(GetEnvironmentString(i));

    Environment.Add('_EXTERNAL_COMMAND=' + CommandLine);
    Environment.Add('_AUTOMATED_CALL=1');

    OnNewLine := @HandleShellCommandRunnerNewLine;
    OnTerminate := @HandleShellCommandRunnerTerminate;

    Start;
    WaitFor;

    Result := fShellCommandBufferOutput;
  end;
end;

function TDreamcastSoftwareDevelopmentEnvironment.GetOfflineFileName(
  const WorkingDirectory: TFileName): TFileName;
const
  OFFLINE_FILE = 'OFFLINE';

begin
  Result := IncludeTrailingPathDelimiter(WorkingDirectory)
    + OFFLINE_FILE;
end;

function TDreamcastSoftwareDevelopmentEnvironment.IsRepositoryReady(
  const WorkingDirectory: TFileName): Boolean;
const
  GIT_SYSTEM_DIRECTORY = '.git';

begin
  Result := DirectoryExists(WorkingDirectory + GIT_SYSTEM_DIRECTORY);
end;

constructor TDreamcastSoftwareDevelopmentEnvironment.Create;
begin
  fFileSystem := TDreamcastSoftwareDevelopmentFileSystem.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettings.Create;
  LoadConfig;
end;

destructor TDreamcastSoftwareDevelopmentEnvironment.Destroy;
begin
  AbortShellCommand;

  Settings.SaveConfiguration;

  fSettings.Free;
  fFileSystem.Free;

  FreeAndNil(fShellCommandRunner);

  inherited Destroy;
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.AbortShellCommand;
begin
{$IFDEF DEBUG}
  DebugLog('Aborting Shell Command!');
{$ENDIF}
  if Assigned(fShellCommandRunner) then
    fShellCommandRunner.Abort;
end;

function TDreamcastSoftwareDevelopmentEnvironment.ExecuteShellCommand(
  CommandLine: string; WorkingDirectory: TFileName): string;
var
  CurrentDir: TFileName;

begin
  CurrentDir := GetCurrentDir;
  SetCurrentDir(WorkingDirectory);

  Result := ExecuteShellCommandRunner(CommandLine);

  SetCurrentDir(CurrentDir);
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.PauseShellCommand;
begin
  if Assigned(fShellCommandRunner) then
    fShellCommandRunner.Pause;
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.ResumeShellCommand;
begin
  if Assigned(fShellCommandRunner) then
    fShellCommandRunner.Resume;
end;

function TDreamcastSoftwareDevelopmentEnvironment.CloneRepository(
  const URL: string; const TargetDirectoryName, WorkingDirectory: TFileName;
  var BufferOutput: string): Boolean;
const
  FAIL_TAG = 'fatal: ';

var
  CommandLine,
  TargetDirectoryFileName: TFileName;
{$IFDEF DEBUG}
  OfflineVersion: string;
{$ENDIF}

begin
  TargetDirectoryFileName := WorkingDirectory + TargetDirectoryName;

  if not IsOfflineRepository(TargetDirectoryFileName) then
  begin
    CommandLine := Format('git clone %s %s --progress', [URL, TargetDirectoryName]);
    BufferOutput := ExecuteShellCommand(CommandLine, WorkingDirectory);
    Result := not IsInString(FAIL_TAG, BufferOutput);
  end
  else
  begin
{$IFDEF DEBUG}
    BufferOutput := ExtractDirectoryName(TargetDirectoryFileName);
    OfflineVersion := GetRepositoryVersion(TargetDirectoryFileName);
    DebugLog('Offline Repository for ' + BufferOutput + ': ' + OfflineVersion);
{$ENDIF}
    // Offline (special case)
    Result := DirectoryExists(TargetDirectoryFileName);
  end;
end;

function TDreamcastSoftwareDevelopmentEnvironment.GetRepositoryVersion(
  const WorkingDirectory: TFileName): string;
begin
  Result := EmptyStr;
  if IsOfflineRepository(WorkingDirectory) then
    Result := LoadFileToString(GetOfflineFileName(WorkingDirectory))
  else
    Result := Run('git', 'describe --dirty --always', WorkingDirectory, False);
end;

function TDreamcastSoftwareDevelopmentEnvironment.IsOfflineRepository(
  const RepositoryDirectory: TFileName): Boolean;
begin
  Result := not IsRepositoryReady(RepositoryDirectory)
    and FileExists(GetOfflineFileName(RepositoryDirectory));
end;

function TDreamcastSoftwareDevelopmentEnvironment.UpdateRepository(
  const WorkingDirectory: TFileName; var BufferOutput: string): TUpdateOperationState;
const
  SUCCESS_TAG = 'Updating ';
  USELESS_TAG = 'Already up to date.';

var
  TempBuffer: string;
{$IFDEF DEBUG}
  OfflineVersion: string;
{$ENDIF}

begin
  Result := uosUpdateFailed;

  if not IsOfflineRepository(WorkingDirectory) then
  begin
    // Online (normal path)
    BufferOutput := ExecuteShellCommand('git pull', WorkingDirectory);
    TempBuffer := StringReplace(BufferOutput, '-', ' ', [rfReplaceAll]);

    if IsInString(USELESS_TAG, TempBuffer) then
      Result := uosUpdateUseless
    else if IsInString(SUCCESS_TAG, TempBuffer) then
      Result := uosUpdateSuccess;
  end
  else
  begin
{$IFDEF DEBUG}
    TempBuffer := ExtractDirectoryName(WorkingDirectory);
    OfflineVersion := GetRepositoryVersion(WorkingDirectory);
    DebugLog('Offline Repository for ' + TempBuffer + ': ' + OfflineVersion);
{$ENDIF}
    // Offline (special case)
    if DirectoryExists(WorkingDirectory) then
      Result := uosUpdateSuccess
    else
      Result := uosUpdateFailed;
  end;
end;

end.

