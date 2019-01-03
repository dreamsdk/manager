unit Environ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RunCmd, Settings;

const
  GIT_SYSTEM_DIRECTORY = '.git';

  DCLOAD_IP_INSTALLATION_DIRECTORY = 'dcload-ip';
  DCLOAD_SERIAL_INSTALLATION_DIRECTORY = 'dcload-serial';

  DREAMSDK_RUNNER_EXECUTABLE = 'dreamsdk-runner.exe';
  DREAMSDK_LAUNCHER_EXECUTABLE = 'dreamsdk.exe';
  DREAMSDK_MSYS_INSTALL_DIRECTORY = '/opt/dreamsdk/';
  DREAMSDK_MSYS_INSTALL_SCRIPTS_DIRECTORY = DREAMSDK_MSYS_INSTALL_DIRECTORY + 'scripts/';
  DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY = DREAMSDK_MSYS_INSTALL_DIRECTORY + 'packages/';

type
  TUpdateOperationState = (
    uosUndefined,
    uosUpdateSuccess,
    uosUpdateUseless,
    uosUpdateFailed
  );

  TToolchainKind = (
    tkSuperH,
    tkARM
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
  public
    property KallistiPortsDirectory: TFileName read fKallistiPortsDirectory;
    property KallistiDirectory: TFileName read fKallistiDirectory;
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
    fConfigurationDirectory: TFileName;
    fDreamSDKDirectory: TFileName;
    fDreamSDKExecutable: TFileName;
    fHomeDirectory: TFileName;
    fMinGWGetExecutable: TFileName;
    fRunnerExecutable: TFileName;
    fShellExecutable: TFileName;
  public
    property DreamSDKDirectory: TFileName read fDreamSDKDirectory;
    property LauncherExecutable: TFileName read fDreamSDKExecutable;
    property RunnerExecutable: TFileName read fRunnerExecutable;
    property ConfigurationDirectory: TFileName read fConfigurationDirectory;
    property ShellExecutable: TFileName read fShellExecutable;
    property MinGWGetExecutable: TFileName read fMinGWGetExecutable;
    property HomeDirectory: TFileName read fHomeDirectory;
  end;

  { TDreamcastSoftwareDevelopmentFileSystem }
  TDreamcastSoftwareDevelopmentFileSystem = class(TObject)
  private
    fDreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool;
    fKallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti;
    fShell: TDreamcastSoftwareDevelopmentFileSystemShell;
    fToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain;
    fToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain;
  protected
    procedure ComputeFileSystemObjectValues(InstallPath: TFileName);
  public
    constructor Create;
    destructor Destroy; override;
    property DreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool
      read fDreamcastTool;
    property Kallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti
      read fKallisti;
    property Shell: TDreamcastSoftwareDevelopmentFileSystemShell read fShell;
    property ToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainARM;
    property ToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainSuperH;
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
    function UpdateRepository(const WorkingDirectory: TFileName;
      var BufferOutput: string): TUpdateOperationState; overload;
    procedure PatchTextFile(const FileName: TFileName;
      OldValue, NewValue: string);
    property FileSystem: TDreamcastSoftwareDevelopmentFileSystem read fFileSystem;
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
    property OnShellCommandNewLine: TNewLineEvent read fShellCommandNewLine
      write fShellCommandNewLine;
  end;

implementation

uses
  SysTools;

{ TDreamcastSoftwareDevelopmentFileSystemToolchain }

constructor TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(
  AToolchainKind: TToolchainKind);
begin
  fKind := AToolchainKind;
end;

{ TDreamcastSoftwareDevelopmentFileSystem }

procedure TDreamcastSoftwareDevelopmentFileSystem.ComputeFileSystemObjectValues(
  InstallPath: TFileName);
var
  DreamSDKBase,
  MSYSBase,
  ToolchainBase,
  ToolchainBaseSuperH,
  ToolchainBaseARM: TFileName;

begin
  MSYSBase := InstallPath + 'msys\1.0\';
  ToolchainBase := MSYSBase + 'opt\toolchains\dc\';

  // Translate DREAMSDK_MSYS_INSTALL_DIRECTORY to Windows location
  DreamSDKBase := MSYSBase + UnixPathToSystem(DREAMSDK_MSYS_INSTALL_DIRECTORY);

  with fShell do
  begin
    // MinGW/MSYS
    fMinGWGetExecutable := InstallPath + 'bin\mingw-get.exe';
    fShellExecutable := MSYSBase + 'bin\sh.exe';
    fHomeDirectory := MSYSBase + 'home\' + GetEnvironmentVariable('USERNAME') + '\';

    // DreamSDK
    fDreamSDKDirectory := DreamSDKBase;
    fDreamSDKExecutable := fDreamSDKDirectory + DREAMSDK_LAUNCHER_EXECUTABLE;
    fRunnerExecutable := fDreamSDKDirectory + DREAMSDK_RUNNER_EXECUTABLE;
    fConfigurationDirectory := MSYSBase + UnixPathToSystem(SETTINGS_DIRECTORY);
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
    fKallistiLibrary := KallistiDirectory + 'lib\dreamcast\libkallisti.a';
    fKallistiChangeLogFile := KallistiDirectory + 'doc\CHANGELOG';
    fKallistiConfigurationFileName := KallistiDirectory + 'environ.sh';
  end;
end;

constructor TDreamcastSoftwareDevelopmentFileSystem.Create;
begin
  fDreamcastTool := TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.Create;
  fKallisti := TDreamcastSoftwareDevelopmentFileSystemKallisti.Create;
  fToolchainARM := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkARM);
  fToolchainSuperH := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkSuperH);
  fShell := TDreamcastSoftwareDevelopmentFileSystemShell.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystem.Destroy;
begin
  fDreamcastTool.Free;
  fKallisti.Free;
  fToolchainARM.Free;
  fToolchainSuperH.Free;
  fShell.Free;
  inherited Destroy;
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
  fShellCommandRunner := nil;
end;

function TDreamcastSoftwareDevelopmentEnvironment.ExecuteShellCommandRunner(
  const CommandLine: string): string;
var
  i: Integer;

begin
  Result := '';
  AbortShellCommand;

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

  inherited Destroy;
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.AbortShellCommand;
begin
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

  CommandLine := Format('%s', [CommandLine]);

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
  CommandLine: string;

begin
  CommandLine := Format('git clone %s %s --progress', [URL, TargetDirectoryName]);
  BufferOutput := ExecuteShellCommand(CommandLine, WorkingDirectory);
  Result := not IsInString(FAIL_TAG, BufferOutput);
end;

function TDreamcastSoftwareDevelopmentEnvironment.UpdateRepository(
  const WorkingDirectory: TFileName; var BufferOutput: string): TUpdateOperationState;
const
  SUCCESS_TAG = 'Updating ';
  USELESS_TAG = 'Already up to date.';

var
  TempBuffer: string;

begin
  Result := uosUpdateFailed;

  BufferOutput := ExecuteShellCommand('git pull', WorkingDirectory);
  TempBuffer := StringReplace(BufferOutput, '-', ' ', [rfReplaceAll]);

  if IsInString(USELESS_TAG, TempBuffer) then
    Result := uosUpdateUseless
  else if IsInString(SUCCESS_TAG, TempBuffer) then
    Result := uosUpdateSuccess;
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.PatchTextFile(
  const FileName: TFileName; OldValue, NewValue: string);
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(FileName);
    if IsInString(OldValue, Buffer.Text) then
    begin
      Buffer.Text := StringReplace(Buffer.Text, OldValue, NewValue, [rfReplaceAll]);
      Buffer.SaveToFile(FileName);
    end;
  finally
    Buffer.Free;
  end;
end;

end.

