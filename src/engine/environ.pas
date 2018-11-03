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
  TDreamcastToolKind = (
    dtkUndefined,
    dtkSerial,
    dtkInternetProtocol
  );

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
    fBaseExecutable: TFileName;
    fInternetProtocolDirectory: TFileName;
    fInternetProtocolExecutable: TFileName;
    fSerialDirectory: TFileName;
    fSerialExecutable: TFileName;
  public
    property BaseDirectory: TFileName read fBaseDirectory;
    property BaseExecutable: TFileName read fBaseExecutable;
    property InternetProtocolDirectory: TFileName read fInternetProtocolDirectory;
    property InternetProtocolExecutable: TFileName read fInternetProtocolExecutable;
    property SerialDirectory: TFileName read fSerialDirectory;
    property SerialExecutable: TFileName read fSerialExecutable;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemKallisti }
  TDreamcastSoftwareDevelopmentFileSystemKallisti = class(TObject)
  private
    fKallistiDirectory: TFileName;
    fKallistiLibrary: TFileName;
    fKallistiChangeLogFile: TFileName;
    fKallistiPortsDirectory: TFileName;
  public
    property KallistiPortsDirectory: TFileName read fKallistiPortsDirectory;
    property KallistiDirectory: TFileName read fKallistiDirectory;
    property KallistiLibrary: TFileName read fKallistiLibrary;
    property KallistiChangeLogFile: TFileName read fKallistiChangeLogFile;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemToolchain }
  TDreamcastSoftwareDevelopmentFileSystemToolchain = class(TObject)
  private
    fFixupNewlibExecutable: TFileName;
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
    property FixupHitachiNewlibExecutable: TFileName read fFixupNewlibExecutable;
    property Installed: Boolean read fToolchainInstalled;
    property Kind: TToolchainKind read fKind;
  end;

  { TDreamcastSoftwareDevelopmentFileSystem }
  TDreamcastSoftwareDevelopmentFileSystem = class(TObject)
  private
    fDreamSDKDirectory: TFileName;
    fDreamSDKExecutable: TFileName;

    fMinGWGetExecutable: TFileName;

    fShellExecutable: TFileName;
    fDreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool;
    fKallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti;
    fToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain;
    fToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain;
  protected
    procedure ComputeFileSystemObjectValues(InstallPath: TFileName);
  public
    constructor Create;
    destructor Destroy; override;
    property DreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool
      read fDreamcastTool;
    property DreamSDKDirectory: TFileName read fDreamSDKDirectory;
    property DreamSDKExecutable: TFileName read fDreamSDKExecutable;
    property ShellExecutable: TFileName read fShellExecutable;
    property MinGWGetExecutable: TFileName read fMinGWGetExecutable;
    property Kallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti
      read fKallisti;
    property ToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainARM;
    property ToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainSuperH;
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
    fDreamcastToolKind: TDreamcastToolKind;
    fInstallPath: TFileName;
    fUseMinTTY: Boolean;
  public
    property InstallPath: TFileName read fInstallPath;
    property DreamcastToolKind: TDreamcastToolKind read fDreamcastToolKind
      write fDreamcastToolKind;
    property UseMinTTY: Boolean read fUseMinTTY write fUseMinTTY;
  end;

  { TDreamcastSoftwareDevelopmentEnvironment }
  TDreamcastSoftwareDevelopmentEnvironment = class(TObject)
  private
    fRepositories: TDreamcastSoftwareDevelopmentRepositories;
    fShellCommandNewLine: TNewLineEvent;
    fShellCommandRunner: TRunCommand;
    fApplicationPath: TFileName;
    fFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
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
      var BufferOutput: string): TUpdateOperationState; overload;
    procedure PatchMakefile(const MakefileFileName: TFileName;
      OldValue, NewValue: string);
    property FileSystem: TDreamcastSoftwareDevelopmentFileSystem read fFileSystem;
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
  CONFIG_FILE_NAME = 'dcsdk.conf';

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
  MSYSBase,
  ToolchainBase,
  ToolchainBaseSuperH,
  ToolchainBaseARM: TFileName;

begin
  MSYSBase := InstallPath + 'msys\1.0\';
  ToolchainBase := MSYSBase + 'opt\toolchains\dc\';

  // MinGW/MSYS
  fMinGWGetExecutable := InstallPath + 'bin\mingw-get.exe';
  fShellExecutable := MSYSBase + 'bin\sh.exe';

  // DreamSDK
  fDreamSDKDirectory := MSYSBase + 'opt\dcsdk\';
  fDreamSDKExecutable := fDreamSDKDirectory + 'dcsdk.exe';

  // Toolchain for Super-H (Hitachi SH-4)
  ToolchainBaseSuperH := ToolchainBase + 'sh-elf\';
  with fToolchainSuperH do
  begin
    fToolchainInstalled := DirectoryExists(ToolchainBaseSuperH);
    fBinutilsExecutable := ToolchainBaseSuperH + 'bin\sh-elf-ld.exe';
    fGCCExecutable := ToolchainBaseSuperH + 'bin\sh-elf-gcc.exe';
    fGDBExecutable := ToolchainBaseSuperH + 'bin\sh-elf-gdb.exe';
    fNewlibBinary := ToolchainBaseSuperH + 'sh-elf\lib\libnosys.a';
    fFixupNewlibExecutable := fDreamSDKDirectory + 'helpers\fixup-sh4-newlib';
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
    fFixupNewlibExecutable := ''; // Not Applicable
  end;

  // dcload/dc-tool
  with fDreamcastTool do
  begin
    fBaseDirectory := ToolchainBase + 'dcload\';
    fInternetProtocolDirectory := fBaseDirectory + 'dcload-ip\';
    fSerialDirectory := fBaseDirectory + 'dcload-serial\';
    fSerialExecutable := ToolchainBase + 'bin\dc-tool-ser.exe';
    fInternetProtocolExecutable := ToolchainBase + 'bin\dc-tool-ip.exe';
    fBaseExecutable := ToolchainBase + 'bin\dc-tool.exe';
  end;

  // KallistiOS
  with fKallisti do
  begin
    fKallistiPortsDirectory := ToolchainBase + 'kos-ports\';
    fKallistiDirectory := ToolchainBase + 'kos\';
    fKallistiLibrary := KallistiDirectory + 'lib\dreamcast\libkallisti.a';
    fKallistiChangeLogFile := KallistiDirectory + 'doc\CHANGELOG';
  end;
end;

constructor TDreamcastSoftwareDevelopmentFileSystem.Create;
begin
  fDreamcastTool := TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.Create;
  fKallisti := TDreamcastSoftwareDevelopmentFileSystemKallisti.Create;
  fToolchainARM := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkARM);
  fToolchainSuperH := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(tkSuperH);
end;

destructor TDreamcastSoftwareDevelopmentFileSystem.Destroy;
begin
  fDreamcastTool.Free;
  fKallisti.Free;
  fToolchainARM.Free;
  fToolchainSuperH.Free;
  inherited Destroy;
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
  Result := GetApplicationPath + CONFIG_FILE_NAME;
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

    fSettings.fDreamcastToolKind := TDreamcastToolKind(IniFile.ReadInteger(
      CONFIG_SETTINGS_SECTION_NAME,
      'DreamcastToolKind',
      0
    ));

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
    IniFile.WriteInteger(
      CONFIG_SETTINGS_SECTION_NAME,
      'DreamcastToolKind',
      Integer(fSettings.fDreamcastToolKind)
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
  fFileSystem := TDreamcastSoftwareDevelopmentFileSystem.Create;
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

procedure TDreamcastSoftwareDevelopmentEnvironment.PatchMakefile(
  const MakefileFileName: TFileName; OldValue, NewValue: string);
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(MakefileFileName);
    if IsInString(OldValue, Buffer.Text) then
    begin
      Buffer.Text := StringReplace(Buffer.Text, OldValue, NewValue, [rfReplaceAll]);
      Buffer.SaveToFile(MakefileFileName);
    end;
  finally
    Buffer.Free;
  end;
end;

end.

