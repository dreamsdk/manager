unit Environ;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  RunCmd,
  RunCmdEx,
  Settings;

const
  DCLOAD_IP_INSTALLATION_DIRECTORY = 'dcload-ip';
  DCLOAD_SERIAL_INSTALLATION_DIRECTORY = 'dcload-serial';

  DREAMSDK_RUNNER_EXECUTABLE = 'dreamsdk-runner.exe';
  DREAMSDK_LAUNCHER_EXECUTABLE = 'dreamsdk-shell.exe';
  DREAMSDK_HELP_FILE = 'dreamsdk.chm';

  DREAMSDK_MSYS_MRUBY_INSTALL_DIRECTORY = '/opt/mruby/';
  DREAMSDK_MSYS_INSTALL_DIRECTORY = '/opt/dreamsdk/';
  DREAMSDK_MSYS_INSTALL_HELPERS_DIRECTORY = DREAMSDK_MSYS_INSTALL_DIRECTORY + 'helpers/';
  DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY = DREAMSDK_MSYS_INSTALL_DIRECTORY + 'packages/';
  DREAMSDK_MSYS_TOOLCHAINS_INSTALL_DIRECTORY = '/opt/toolchains/dc/';

  // Linked to TDebuggerVersionKind
  SUPPORTED_PYTHON_VERSIONS: array[0..9] of string = (
    '2.7',
    '3.3',
    '3.4',
    '3.5',
    '3.6',
    '3.7',
    '3.8',
    '3.9',
    '3.10',
    '3.11'
  );

  // Linked to TToolchainVersionKind
  SUPPORTED_GCC_VERSIONS: array[0..1] of string = (
    '4',
    '9'
  );

type
  EEnvironment = class(Exception);
  EKallistiReferentialNotAvailable = class(EEnvironment);
  EEmptyRepositoryUrl = class(EEnvironment);

  { TToolchainVersionKind }
  TToolchainVersionKind = (
    tvkUndefined,
    tvkStable,
    tvkExperimental
  );

  { TDebuggerVersionKind }
  TDebuggerVersionKind = (
    dvkUndefined,
    dvkPythonDisabled,
    dvkPython27,
    dvkPython33,
    dvkPython34,
    dvkPython35,
    dvkPython36,
    dvkPython37,
    dvkPython38,
    dvkPython39,
    dvkPython310,
    dvkPython311
  );

  TRepositoryKind = (
    rkUndefined,
    rkKallisti,
    rkKallistiPorts,
    rkDreamcastTool, // Both Serial and Internet Protocol
    rkDreamcastToolSerial,
    rkDreamcastToolInternetProtocol,
    rkRuby
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

  TDreamcastSoftwareDevelopmentEnvironment = class;

  { TDreamcastSoftwareDevelopmentFileSystemDreamcastToolPackages }
  TDreamcastSoftwareDevelopmentFileSystemDreamcastToolPackages = class(TObject)
  private
    fInternetProtocol: TFileName;
    fSerial: TFileName;
  public
    property InternetProtocol: TFileName read fInternetProtocol;
    property Serial: TFileName read fSerial;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemDreamcastTool }
  TDreamcastSoftwareDevelopmentFileSystemDreamcastTool = class(TObject)
  private
    fBaseDirectory: TFileName;
    fConfigurationFileName: TFileName;
    fInternetProtocolDirectory: TFileName;
    fInternetProtocolExecutable: TFileName;
    fPackages: TDreamcastSoftwareDevelopmentFileSystemDreamcastToolPackages;
    fSerialDirectory: TFileName;
    fSerialExecutable: TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    function ResetRepository: Boolean;
    function ResetRepositorySerial: Boolean;
    function ResetRepositoryInternetProtocol: Boolean;
    property BaseDirectory: TFileName read fBaseDirectory;
    property ConfigurationFileName: TFileName read fConfigurationFileName;
    property InternetProtocolDirectory: TFileName read fInternetProtocolDirectory;
    property InternetProtocolExecutable: TFileName read fInternetProtocolExecutable;
    property SerialDirectory: TFileName read fSerialDirectory;
    property SerialExecutable: TFileName read fSerialExecutable;
    property Packages: TDreamcastSoftwareDevelopmentFileSystemDreamcastToolPackages
      read fPackages;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemKallistiPackages }
  TDreamcastSoftwareDevelopmentFileSystemKallistiPackages = class(TObject)
  private
    fKallisti: TFileName;
    fKallistiPorts: TFileName;
  public
    property KallistiPorts: TFileName read fKallistiPorts;
    property Kallisti: TFileName read fKallisti;
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
    fPackages: TDreamcastSoftwareDevelopmentFileSystemKallistiPackages;
  public
    constructor Create;
    destructor Destroy; override;
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
    property Packages: TDreamcastSoftwareDevelopmentFileSystemKallistiPackages
      read fPackages;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemToolchainPackagesDebugger }
  TDreamcastSoftwareDevelopmentFileSystemToolchainPackagesDebugger = class(TObject)
  private
    fPythonDisabled: TFileName;
    fPython27: TFileName;
    fPython33: TFileName;
    fPython34: TFileName;
    fPython35: TFileName;
    fPython36: TFileName;
    fPython37: TFileName;
    fPython38: TFileName;
    fPython39: TFileName;
    fPython310: TFileName;
    fPython311: TFileName;
  public
    property PythonDisabled: TFileName read fPythonDisabled;
    property Python27: TFileName read fPython27;
    property Python33: TFileName read fPython33;
    property Python34: TFileName read fPython34;
    property Python35: TFileName read fPython35;
    property Python36: TFileName read fPython36;
    property Python37: TFileName read fPython37;
    property Python38: TFileName read fPython38;
    property Python39: TFileName read fPython39;
    property Python310: TFileName read fPython310;
    property Python311: TFileName read fPython311;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemToolchainPackages }
  TDreamcastSoftwareDevelopmentFileSystemToolchainPackages = class(TObject)
  private
    fDebugger: TDreamcastSoftwareDevelopmentFileSystemToolchainPackagesDebugger;
    fExperimental: TFileName;
    fStable: TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    property Debugger: TDreamcastSoftwareDevelopmentFileSystemToolchainPackagesDebugger
      read fDebugger;
    property Experimental: TFileName read fExperimental;
    property Stable: TFileName read fStable;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemToolchain }
  TDreamcastSoftwareDevelopmentFileSystemToolchain = class(TObject)
  private
    fBaseDirectory: TFileName;
    fBinutilsExecutable: TFileName;
    fGCCExecutable: TFileName;
    fGDBExecutable: TFileName;
    fKind: TToolchainKind;
    fNewlibBinary: TFileName;
    fPackages: TDreamcastSoftwareDevelopmentFileSystemToolchainPackages;
    fToolchainInstalled: Boolean;
  public
    constructor Create(AToolchainKind: TToolchainKind);
    destructor Destroy; override;
    function Reset: Boolean;
    property BaseDirectory: TFileName read fBaseDirectory;
    property BinutilsExecutable: TFileName read fBinutilsExecutable;
    property GCCExecutable: TFileName read fGCCExecutable;
    property GDBExecutable: TFileName read fGDBExecutable;
    property NewlibBinary: TFileName read fNewlibBinary;
    property Installed: Boolean read fToolchainInstalled;
    property Kind: TToolchainKind read fKind;
    property Packages: TDreamcastSoftwareDevelopmentFileSystemToolchainPackages
      read fPackages;
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

  { TDreamcastSoftwareDevelopmentFileSystemRubyPackages }
  TDreamcastSoftwareDevelopmentFileSystemRubyPackages = class(TObject)
  private
    fRubyLibrary: TFileName;
    fSamples: TFileName;
  public
    property RubyLibrary: TFileName read fRubyLibrary;
    property Samples: TFileName read fSamples;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemRuby }
  TDreamcastSoftwareDevelopmentFileSystemRuby = class(TObject)
  private
    fBinariesDirectory: TFileName;
    fBuildDirectory: TFileName;
    fPackages: TDreamcastSoftwareDevelopmentFileSystemRubyPackages;
    fRubyDirectory: TFileName;
    fRubyLibrary: TFileName;
    fSamplesDirectory: TFileName;
    fSamplesLibraryInformationFile: TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    function ResetRepository: Boolean;
    property BaseDirectory: TFileName read fRubyDirectory;
    property RubyLibrary: TFileName read fRubyLibrary;
    property BinariesDirectory: TFileName read fBinariesDirectory;
    property BuildDirectory: TFileName read fBuildDirectory;
    property SamplesDirectory: TFileName read fSamplesDirectory;
    property SamplesLibraryInformationFile: TFileName
      read fSamplesLibraryInformationFile;
    property Packages: TDreamcastSoftwareDevelopmentFileSystemRubyPackages
      read fPackages;
  end;

  { TDreamcastSoftwareDevelopmentFileSystem }
  TDreamcastSoftwareDevelopmentFileSystem = class(TObject)
  private
    fToolchainBase: TFileName;
    fDreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool;
    fKallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti;
    fRuby: TDreamcastSoftwareDevelopmentFileSystemRuby;
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
    property ToolchainBase: TFileName
      read fToolchainBase;
    property ToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainSuperH;
    property ToolchainWin32: TDreamcastSoftwareDevelopmentFileSystemToolchain
      read fToolchainWin32;
    property Ruby: TDreamcastSoftwareDevelopmentFileSystemRuby
      read fRuby;
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

  { TDreamcastSoftwareDevelopmentEnvironment }
  TDreamcastSoftwareDevelopmentEnvironment = class(TObject)
  private
    fShellCommandError: Boolean;
    fShellCommandNewLine: TNewLineEvent;
    fShellCommandRunner: TRunCommandEx;
    fFileSystem: TDreamcastSoftwareDevelopmentFileSystem;
    fSettings: TDreamcastSoftwareDevelopmentSettings;
    fShellCommandBufferOutput: string;
    fMsysBaseDirectoryReverse: TFileName;
    fMsysBaseDirectoryNormal: TFileName;
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
    function IsComponentInstalled(const WorkingDirectory: TFileName): Boolean;
    function IsOfflineRepository(const RepositoryDirectory: TFileName): Boolean;
    function IsRepositoryReady(const WorkingDirectory: TFileName): Boolean;
    function UpdateRepository(const WorkingDirectory: TFileName;
      var BufferOutput: string): TUpdateOperationState; overload;

    property FileSystem: TDreamcastSoftwareDevelopmentFileSystem read fFileSystem;
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
    property ShellCommandError: Boolean read fShellCommandError;
    property OnShellCommandNewLine: TNewLineEvent read fShellCommandNewLine
      write fShellCommandNewLine;
  end;

implementation

uses
  RefBase, SysTools, FSTools, RunTools{$IFDEF GUI}, PostInst{$ENDIF};

const
  FAIL_TAG = 'fatal: ';

{ TDreamcastSoftwareDevelopmentFileSystemToolchainPackages }

constructor TDreamcastSoftwareDevelopmentFileSystemToolchainPackages.Create;
begin
  fDebugger := TDreamcastSoftwareDevelopmentFileSystemToolchainPackagesDebugger.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystemToolchainPackages.Destroy;
begin
  fDebugger.Free;
  inherited Destroy;
end;

{ TDreamcastSoftwareDevelopmentFileSystemRuby }

constructor TDreamcastSoftwareDevelopmentFileSystemRuby.Create;
begin
  fPackages := TDreamcastSoftwareDevelopmentFileSystemRubyPackages.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystemRuby.Destroy;
begin
  fPackages.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentFileSystemRuby.ResetRepository: Boolean;
begin
  Result := KillDirectory(BaseDirectory);
  KillDirectory(SamplesDirectory); // not so critical, don't need to fail if this wasn't possible
end;

{ TDreamcastSoftwareDevelopmentRepository }

function TDreamcastSoftwareDevelopmentRepository.GetOffline: Boolean;
begin
  Result := FileExists(OfflineFileName);
end;

function TDreamcastSoftwareDevelopmentRepository.GetReady: Boolean;
begin
  Result := Environment.IsRepositoryReady(fRepositoryDirectory);
end;

function TDreamcastSoftwareDevelopmentRepository.GetURL: string;
begin
  Result := EmptyStr;
  if Ready then
	try
		Result := Trim(Run('git', 'config --get remote.origin.url', fRepositoryDirectory));
	except
		// Silent exception (not needed in that case)
	end;
end;

function TDreamcastSoftwareDevelopmentRepository.GetVersion: string;
begin
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

constructor TDreamcastSoftwareDevelopmentFileSystemKallisti.Create;
begin
  fPackages :=  TDreamcastSoftwareDevelopmentFileSystemKallistiPackages.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystemKallisti.Destroy;
begin
  fPackages.Free;
  inherited Destroy;
end;

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

constructor TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.Create;
begin
  fPackages := TDreamcastSoftwareDevelopmentFileSystemDreamcastToolPackages.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.Destroy;
begin
  fPackages.Free;
  inherited Destroy;
end;

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
  fPackages := TDreamcastSoftwareDevelopmentFileSystemToolchainPackages.Create;
  fKind := AToolchainKind;
end;

destructor TDreamcastSoftwareDevelopmentFileSystemToolchain.Destroy;
begin
  fPackages.Free;
  inherited Destroy;
end;

function TDreamcastSoftwareDevelopmentFileSystemToolchain.Reset: Boolean;
begin
  Result := False;
  if fKind <> tkWin32 then
  begin
    Result := KillDirectory(fBaseDirectory);
  end;
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
  ToolchainBaseSuperH,
  ToolchainBaseARM,
  ToolchainBaseWin32,
  PackagesBase: TFileName;

begin
  // Base
  MSYSBase := GetMSysBaseDirectory;
  fToolchainBase := MSYSBase + UnixPathToSystem(DREAMSDK_MSYS_TOOLCHAINS_INSTALL_DIRECTORY);
  PackagesBase := MSYSBase + UnixPathToSystem(DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY);

  // Translate DREAMSDK_MSYS_INSTALL_DIRECTORY to Windows location
  DreamSDKBase := IncludeTrailingPathDelimiter(MSYSBase
    + UnixPathToSystem(DREAMSDK_MSYS_INSTALL_DIRECTORY));

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
    fBaseDirectory := ToolchainBaseSuperH;
    fToolchainInstalled := DirectoryExists(ToolchainBaseSuperH);
    fBinutilsExecutable := ToolchainBaseSuperH + 'bin\sh-elf-ld.exe';
    fGCCExecutable := ToolchainBaseSuperH + 'bin\sh-elf-gcc.exe';
    fGDBExecutable := ToolchainBaseSuperH + 'bin\sh-elf-gdb.exe';
    fNewlibBinary := ToolchainBaseSuperH + 'sh-elf\lib\libnosys.a';
    with fPackages.fDebugger do
    begin
      fPythonDisabled := PackagesBase +'sh-elf-gdb-no-python-bin.7z';
      fPython27 := PackagesBase + 'sh-elf-gdb-python-2.7-bin.7z';
      fPython33 := PackagesBase + 'sh-elf-gdb-python-3.3-bin.7z';
      fPython34 := PackagesBase + 'sh-elf-gdb-python-3.4-bin.7z';
      fPython35 := PackagesBase + 'sh-elf-gdb-python-3.5-bin.7z';
      fPython36 := PackagesBase + 'sh-elf-gdb-python-3.6-bin.7z';
      fPython37 := PackagesBase + 'sh-elf-gdb-python-3.7-bin.7z';
      fPython38 := PackagesBase + 'sh-elf-gdb-python-3.8-bin.7z';
      fPython39 := PackagesBase + 'sh-elf-gdb-python-3.9-bin.7z';
      fPython310 := PackagesBase + 'sh-elf-gdb-python-3.10-bin.7z';
      fPython311 := PackagesBase + 'sh-elf-gdb-python-3.11-bin.7z';
    end;
    fPackages.fExperimental := PackagesBase + 'toolchain-experimental-sh-elf-bin.7z';
    fPackages.fStable := PackagesBase + 'toolchain-stable-sh-elf-bin.7z';
  end;

  // Toolchain for ARM
  ToolchainBaseARM := ToolchainBase + 'arm-eabi\';
  with fToolchainARM do
  begin
    fBaseDirectory := ToolchainBaseARM;
    fToolchainInstalled := DirectoryExists(ToolchainBaseARM);
    fBinutilsExecutable := ToolchainBaseARM + 'bin\arm-eabi-ld.exe';
    fGCCExecutable := ToolchainBaseARM + 'bin\arm-eabi-gcc.exe';
    fGDBExecutable := EmptyStr; // Not Applicable
    fNewlibBinary := EmptyStr; // Not Applicable
    fPackages.fExperimental := PackagesBase + 'toolchain-experimental-arm-eabi-bin.7z';
    fPackages.fStable := PackagesBase + 'toolchain-stable-arm-eabi-bin.7z';
  end;

  // Toolchain for Win32 (Windows)
  ToolchainBaseWin32 := GetInstallationBaseDirectory;
  with fToolchainWin32 do
  begin
    fBaseDirectory := ToolchainBaseWin32;
    fToolchainInstalled := DirectoryExists(ToolchainBaseWin32);
    fBinutilsExecutable := ToolchainBaseWin32 + 'bin\ld.exe';
    fGCCExecutable := ToolchainBaseWin32 + 'bin\gcc.exe';
    fGDBExecutable := ToolchainBaseWin32 + 'bin\gdb.exe';
    fNewlibBinary := EmptyStr; // Not Applicable
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
    fPackages.fInternetProtocol := PackagesBase + 'dcload-ip-offline-src.7z';
    fPackages.fSerial := PackagesBase + 'dcload-serial-offline-src.7z';
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
    fPackages.fKallisti := PackagesBase + 'kallisti-offline-src.7z';
    fPackages.fKallistiPorts := PackagesBase + 'kallisti-ports-offline-src.7z';
    fKallistiPortsLibraryInformationFile := GetReferentialDirectory + 'koslib.conf';
    if not FileExists(fKallistiPortsLibraryInformationFile) then
      raise EKallistiReferentialNotAvailable.CreateFmt(
        'The KallistiOS Ports library referential file was not found: %s', [fKallistiPortsLibraryInformationFile]);
  end;

  // Ruby
  with fRuby do
  begin
    fRubyDirectory := IncludeTrailingPathDelimiter(MSYSBase + UnixPathToSystem(DREAMSDK_MSYS_MRUBY_INSTALL_DIRECTORY));
    fBuildDirectory := fRubyDirectory + 'build\';
    fBinariesDirectory := fRubyDirectory + 'bin\';
    fRubyLibrary := fBuildDirectory + 'dreamcast\lib\libmruby.a';
    fSamplesDirectory := ToolchainBase + 'ruby\';
    fSamplesLibraryInformationFile := GetReferentialDirectory + 'mruby.conf';
    fPackages.fRubyLibrary := PackagesBase + 'ruby-offline-src.7z';
    fPackages.fSamples := PackagesBase + 'ruby-samples-offline-src.7z';
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
  fRuby := TDreamcastSoftwareDevelopmentFileSystemRuby.Create;
end;

destructor TDreamcastSoftwareDevelopmentFileSystem.Destroy;
begin
  fRuby.Free;
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
    rkRuby:
      Result := Ruby.ResetRepository;
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
const
  ERROR_KEYWORDS: array[0..1] of string = ('error:', 'fatal:');

var
  ProcessedNewLine: string;
  i: Integer;

begin
  ProcessedNewLine := StringReplace(NewLine, fMsysBaseDirectoryNormal,
    EmptyStr, [rfReplaceAll]);

  // Remove absolute DreamSDK installation path from lines
  ProcessedNewLine := StringReplace(ProcessedNewLine,
    fMsysBaseDirectoryReverse, EmptyStr, [rfReplaceAll]);

  // Send message event
  if Assigned(fShellCommandNewLine) and (not IsEmpty(ProcessedNewLine)) then
    fShellCommandNewLine(Self, ProcessedNewLine);

  // Detect if the line contains an error... not very smart but it works
  if not fShellCommandError then
  begin
    ProcessedNewLine := LowerCase(ProcessedNewLine); // only used for detecting errors
    for i := Low(ERROR_KEYWORDS) to High(ERROR_KEYWORDS) do
      fShellCommandError := IsInString(ERROR_KEYWORDS[i], ProcessedNewLine);
  end;
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

{$IFDEF DEBUG}
  DebugLog('ExecuteShellCommandRunner: ' + CommandLine + ' in: ' + GetCurrentDir);
{$ENDIF}

  FreeAndNil(fShellCommandRunner);
  fShellCommandRunner := TRunCommandEx.Create(True);
  with fShellCommandRunner do
  begin
    Executable := fFileSystem.Shell.ShellExecutable;

    Parameters.Add('--login');
    Parameters.Add('-i');

    for i := 1 to GetEnvironmentVariableCount do
    begin
      Environment.Add(GetEnvironmentString(i));
    end;

    Environment.Add('_EXTERNAL_COMMAND=' + CommandLine);
    Environment.Add('_AUTOMATED_CALL=1');

    OnNewLine := @HandleShellCommandRunnerNewLine;
    OnTerminate := @HandleShellCommandRunnerTerminate;

    Start;
    WaitFor;

    Result := fShellCommandBufferOutput;

{$IFDEF DEBUG}
    DebugLog('  ExitCode: ' + IntToStr(fShellCommandRunner.ExitCode));
{$ENDIF}
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

var
  AWorkingDirectory: TFileName;

begin
  AWorkingDirectory := IncludeTrailingPathDelimiter(WorkingDirectory);
  Result := DirectoryExists(AWorkingDirectory) and
    DirectoryExists(AWorkingDirectory + GIT_SYSTEM_DIRECTORY);
end;

constructor TDreamcastSoftwareDevelopmentEnvironment.Create;
begin
  fShellCommandError := False;
  fMsysBaseDirectoryNormal := ExcludeTrailingPathDelimiter(GetMSysBaseDirectory);
  fMsysBaseDirectoryReverse := StringReplace(fMsysBaseDirectoryNormal,
    DirectorySeparator, '/', [rfReplaceAll]);
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
  fShellCommandError := False;
  if Assigned(fShellCommandRunner) then
    fShellCommandRunner.Abort;
end;

function TDreamcastSoftwareDevelopmentEnvironment.ExecuteShellCommand(
  CommandLine: string; WorkingDirectory: TFileName): string;
var
  CurrentDir: TFileName;

begin
  CurrentDir := GetCurrentDir;

  if not DirectoryExists(WorkingDirectory) then
    ForceDirectories(WorkingDirectory);

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
{$IFDEF DEBUG}
    if IsEmpty(URL) then
      WriteLn('Warning: CloneRepository: URL is empty!');
{$ENDIF}
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
  if DirectoryExists(WorkingDirectory) then
    if IsOfflineRepository(WorkingDirectory) then
      Result := LoadFileToString(GetOfflineFileName(WorkingDirectory))
    else
	  begin
		  try
		    Result := Run('git', 'describe --dirty --always', WorkingDirectory, False);
		    if IsInString(FAIL_TAG, Result) then
			    Result := EmptyStr;
		  except
			  // Not needed in that cases
		  end;
	  end;
end;

function TDreamcastSoftwareDevelopmentEnvironment.IsComponentInstalled(
  const WorkingDirectory: TFileName): Boolean;
begin
  Result := {$IFDEF GUI}(not IsPostInstallMode) and {$ENDIF} DirectoryExists(WorkingDirectory)
    and (IsRepositoryReady(WorkingDirectory) or IsOfflineRepository(WorkingDirectory));
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
    if IsRepositoryReady(WorkingDirectory) then
    begin
      // Online (normal path)
      BufferOutput := ExecuteShellCommand('git pull', WorkingDirectory);
      TempBuffer := StringReplace(BufferOutput, '-', ' ', [rfReplaceAll]);

      if IsInString(USELESS_TAG, TempBuffer) then
        Result := uosUpdateUseless
      else if IsInString(SUCCESS_TAG, TempBuffer) then
        Result := uosUpdateSuccess;
    end;
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

