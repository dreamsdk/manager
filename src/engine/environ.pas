unit Environ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RunCmd;

const
  DEFAULT_KALLISTI_URL = 'https://git.code.sf.net/p/cadcdev/kallistios';
  DEFAULT_KALLISTI_PORTS_URL = 'https://git.code.sf.net/p/cadcdev/kos-ports';
  DEFAULT_DREAMCAST_TOOL_SERIAL_URL = 'https://github.com/sizious/dcload-serial.git';
  DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL = 'https://github.com/sizious/dcload-ip.git';

type
  TUpdateOperationState = (
    uosUndefined,
    uosUpdateSuccess,
    uosUpdateUseless,
    uosUpdateFailed
  );

  { TDreamcastSoftwareDevelopmentFileSystemObject }

  TDreamcastSoftwareDevelopmentFileSystemObject = class(TObject)
  private
    fDCToolIPExecutable: TFileName;
    fDCToolSerialExecutable: TFileName;
    fDreamSDKDirectory: TFileName;
    fDreamSDKExecutable: TFileName;
    fFixupHitachiNewlibExecutable: TFileName;
    fGCCExecutable: TFileName;
    fGDBExecutable: TFileName;
    fKallistiDirectory: TFileName;
    fKallistiLibrary: TFileName;
    fKallistiChangeLogFile: TFileName;
    fKallistiPortsDirectory: TFileName;
    fNewlibBinary: TFileName;
    fMinGWGetExecutable: TFileName;
    fBinutilsExecutable: TFileName;
    fShellExecutable: TFileName;
    fToolchainInstalledARM: Boolean;
    fToolchainInstalledSH4: Boolean;
  protected
    procedure ComputeFileSystemObjectValues(InstallPath: TFileName);
  public
    property DreamSDKDirectory: TFileName read fDreamSDKDirectory;
    property DreamSDKExecutable: TFileName read fDreamSDKExecutable;
    property ShellExecutable: TFileName read fShellExecutable;
    property MinGWGetExecutable: TFileName read fMinGWGetExecutable;
    property BinutilsExecutable: TFileName read fBinutilsExecutable;
    property GCCExecutable: TFileName read fGCCExecutable;
    property GDBExecutable: TFileName read fGDBExecutable;
    property NewlibBinary: TFileName read fNewlibBinary;
    property DreamcastToolSerialExecutable: TFileName read fDCToolSerialExecutable;
    property DreamcastToolIPExecutable: TFileName read fDCToolIPExecutable;
    property FixupHitachiNewlibExecutable: TFileName read fFixupHitachiNewlibExecutable;
    property KallistiPortsDirectory: TFileName read fKallistiPortsDirectory;
    property KallistiDirectory: TFileName read fKallistiDirectory;
    property KallistiLibrary: TFileName read fKallistiLibrary;
    property KallistiChangeLogFile: TFileName read fKallistiChangeLogFile;
    property ToolchainInstalledARM: Boolean read fToolchainInstalledARM;
    property ToolchainInstalledSH4: Boolean read fToolchainInstalledSH4;
  end;

  { TDreamcastSoftwareDevelopmentRepositories }
  TDreamcastSoftwareDevelopmentRepositories = class(TObject)
  private
    fKallistiPortsURL: string;
    fKallistiURL: string;
    fDreamcastToolSerialURL: string;
    fDreamcastToolInternetProtocolURL: string;
  public
    property KallistiURL: string
      read fKallistiURL write fKallistiURL;
    property KallistiPortsURL: string
      read fKallistiPortsURL write fKallistiPortsURL;
    property DreamcastToolSerialURL: string
      read fDreamcastToolSerialURL write fDreamcastToolSerialURL;
    property DreamcastToolInternetProtocolURL: string
      read fDreamcastToolInternetProtocolURL write fDreamcastToolInternetProtocolURL;
  end;

  { TDreamcastSoftwareDevelopmentSettings }
  TDreamcastSoftwareDevelopmentSettings = class(TObject)
  private
    fInstallPath: TFileName;
    fUseMinTTY: Boolean;
  public
    property InstallPath: TFileName read fInstallPath;
    property UseMinTTY: Boolean read fUseMinTTY write fUseMinTTY;
  end;

  { TDreamcastSoftwareDevelopmentEnvironment }
  TDreamcastSoftwareDevelopmentEnvironment = class(TObject)
  private
    fRepositories: TDreamcastSoftwareDevelopmentRepositories;
    fShellCommandNewLine: TNewLineEvent;
    fShellCommandRunner: TRunCommand;
    fApplicationPath: TFileName;
    fFileSystem: TDreamcastSoftwareDevelopmentFileSystemObject;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    fShellCommandBufferOutput: string;
    function GetApplicationPath: TFileName;
    function GetConfigurationFileName: TFileName;
  protected
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
    procedure LoadConfig;
    procedure SaveConfig;
    function CloneRepository(const URL: string; const TargetDirectoryName,
      WorkingDirectory: TFileName; var BufferOutput: string): Boolean;
    function UpdateRepository(const WorkingDirectory: TFileName;
      var BufferOutput: string): TUpdateOperationState;
    property FileSystem: TDreamcastSoftwareDevelopmentFileSystemObject read fFileSystem;
    property Repositories: TDreamcastSoftwareDevelopmentRepositories read fRepositories;
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
    property OnShellCommandNewLine: TNewLineEvent read fShellCommandNewLine
      write fShellCommandNewLine;
  end;

implementation

uses
  IniFiles, SysTools;

const
  CONFIG_SETTINGS_SECTION_NAME = 'Settings';
  CONFIG_REPOSITORIES_SECTION_NAME = 'Repositories';

{ TDreamcastSoftwareDevelopmentFileSystemObject }

procedure TDreamcastSoftwareDevelopmentFileSystemObject.ComputeFileSystemObjectValues(
  InstallPath: TFileName);
var
  MSYSBase, ToolchainBase: TFileName;
begin
  MSYSBase := InstallPath + 'msys\1.0\';
  ToolchainBase := MSYSBase + 'opt\toolchains\dc\';

  // MinGW/MSYS
  fMinGWGetExecutable := InstallPath + 'bin\mingw-get.exe';
  fShellExecutable := MSYSBase + 'bin\sh.exe';

  // DreamSDK
  fDreamSDKDirectory := MSYSBase + 'opt\dcsdk\';
  fDreamSDKExecutable := fDreamSDKDirectory + 'dcsdk.exe';
  fFixupHitachiNewlibExecutable := MSYSBase + 'opt\dcsdk\helpers\fixup-sh4-newlib';

  // Toolchain
  fToolchainInstalledARM := DirectoryExists(ToolchainBase + 'arm-eabi');
  fToolchainInstalledSH4 := DirectoryExists(ToolchainBase + 'sh-elf');
  fBinutilsExecutable := ToolchainBase + 'sh-elf\bin\sh-elf-ld.exe';
  fGCCExecutable := ToolchainBase + 'sh-elf\bin\sh-elf-gcc.exe';
  fGDBExecutable := ToolchainBase + 'sh-elf\bin\sh-elf-gdb.exe';
  fNewlibBinary := ToolchainBase + 'sh-elf\sh-elf\lib\libnosys.a';

  // dcload/dc-tool
  fDCToolSerialExecutable := ToolchainBase + 'bin\dc-tool-ser.exe';
  fDCToolIPExecutable := ToolchainBase + 'bin\dc-tool-ip.exe';

  // KallistiOS
  fKallistiPortsDirectory := ToolchainBase + 'kos-ports\';
  fKallistiDirectory := ToolchainBase + 'kos\';
  fKallistiLibrary := KallistiDirectory + 'lib\dreamcast\libkallisti.a';
  fKallistiChangeLogFile := KallistiDirectory + 'doc\CHANGELOG';
end;

{ TDreamcastSoftwareDevelopmentEnvironment }

function TDreamcastSoftwareDevelopmentEnvironment.GetApplicationPath: TFileName;
var
  Path: TFileName;
{$IFDEF Darwin}
  i: Integer;
{$ENDIF}

begin
  if (fApplicationPath = '') then
  begin
    Path := ExtractFilePath(ParamStr(0));
{$IFDEF Darwin}
    i := Pos('.app', Path);
    if i > 0 then
    begin
      i := LastDelimiter('/', Copy(Path, 1, i));
      Path := Copy(Path, 1, i);
    end;
{$ENDIF}
    fApplicationPath := IncludeTrailingPathDelimiter(Path);
  end;
  Result := fApplicationPath;
end;

function TDreamcastSoftwareDevelopmentEnvironment.GetConfigurationFileName: TFileName;
begin
  Result := GetApplicationPath + 'dcsdk.conf';
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.LoadConfig;
var
  IniFile: TIniFile;
  DefaultInstallationPath: TFileName;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    // Settings
    DefaultInstallationPath := ExpandFileName(GetApplicationPath + '..\..\..\..\');
    fSettings.fInstallPath := IncludeTrailingPathDelimiter(
      IniFile.ReadString(
        CONFIG_SETTINGS_SECTION_NAME,
        'InstallPath',
        DefaultInstallationPath
      )
    );
    FileSystem.ComputeFileSystemObjectValues(fSettings.fInstallPath);

    fSettings.fUseMintty := IniFile.ReadBool(
      CONFIG_SETTINGS_SECTION_NAME,
      'UseMinTTY',
      False
    );

    // Repositories
    fRepositories.fKallistiURL := IniFile.ReadString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'KallistiOS',
      DEFAULT_KALLISTI_URL
    );
    fRepositories.fKallistiPortsURL := IniFile.ReadString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'KallistiPorts',
      DEFAULT_KALLISTI_PORTS_URL
    );
    fRepositories.fDreamcastToolSerialURL := IniFile.ReadString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'DreamcastToolSerial',
      DEFAULT_DREAMCAST_TOOL_SERIAL_URL
    );
    fRepositories.fDreamcastToolInternetProtocolURL := IniFile.ReadString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'DreamcastToolInternetProtocol',
      DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL
    );
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentEnvironment.SaveConfig;
var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(GetConfigurationFileName);
  try
    // Settings
    IniFile.WriteString(
      CONFIG_SETTINGS_SECTION_NAME,
      'InstallPath',
      fSettings.fInstallPath
    );
    IniFile.WriteBool(
      CONFIG_SETTINGS_SECTION_NAME,
      'UseMinTTY',
      fSettings.fUseMintty
    );

    // Repositories
    IniFile.WriteString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'KallistiOS',
      fRepositories.fKallistiURL
    );
    IniFile.WriteString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'KallistiPorts',
      fRepositories.fKallistiPortsURL
    );
    IniFile.WriteString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'DreamcastToolSerial',
      fRepositories.fDreamcastToolSerialURL
    );
    IniFile.WriteString(
      CONFIG_REPOSITORIES_SECTION_NAME,
      'DreamcastToolInternetProtocol',
      fRepositories.fDreamcastToolInternetProtocolURL
    );
  finally
    IniFile.Free;
  end;
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
    Executable := fFileSystem.ShellExecutable;

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
  fFileSystem := TDreamcastSoftwareDevelopmentFileSystemObject.Create;
  fRepositories := TDreamcastSoftwareDevelopmentRepositories.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettings.Create;
  LoadConfig;
end;

destructor TDreamcastSoftwareDevelopmentEnvironment.Destroy;
begin
  AbortShellCommand;
  SaveConfig;

  fSettings.Free;
  fRepositories.Free;
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

begin
  Result := uosUpdateFailed;

  BufferOutput := ExecuteShellCommand('git pull', WorkingDirectory);

  if IsInString(USELESS_TAG, BufferOutput) then
    Result := uosUpdateUseless
  else if IsInString(SUCCESS_TAG, BufferOutput) then
    Result := uosUpdateSuccess;
end;

end.

