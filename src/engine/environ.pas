unit Environ;

{$mode objfpc}{$H+}

// Removing libdep bfd plugin on Windows
// See: https://sourceware.org/bugzilla/show_bug.cgi?id=27113
{$DEFINE SH_ELF_BFD_PLUGIN_LIBDEP_WORKAROUND}

interface

uses
  Classes,
  SysUtils,
  FGL,
  IniFiles,
  RunCmd,
  RunCmdEx,
  Settings,
  RefBase,
  PEUtils;

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

type
  EEnvironment = class(Exception);
  EKallistiReferentialNotAvailable = class(EEnvironment);
  EEmptyRepositoryUrl = class(EEnvironment);
  EUnsupportedToolchainKind = class(Exception); // Exception for unsupported toolchain kind

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

  // Toolchain architecture type
  TToolchainKind = (
    tkSuperH,
    tkARM,
    tkWin32
  );

  TDebuggerKind = (
    dkUndefined,
    dkPythonDisabled,
    dkPythonEnabled
  );

  TDreamcastSoftwareDevelopmentEnvironment = class;
  TDreamcastSoftwareDevelopmentFileSystem = class;

  { Dreamcast Tool / dc-tool }

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

  { KallistiOS }

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
    fKallistiPortsDirectory: TFileName;
    fKallistiPortsLibraryInformationFile: TFileName;
    fKallistiUtilitiesDirectory: TFileName;
    fPackages: TDreamcastSoftwareDevelopmentFileSystemKallistiPackages;
    function GetKallistiChangeLogFile: TFileName;
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
    property KallistiChangeLogFile: TFileName read GetKallistiChangeLogFile;
    property KallistiConfigurationFileName: TFileName read fKallistiConfigurationFileName;
    property Packages: TDreamcastSoftwareDevelopmentFileSystemKallistiPackages
      read fPackages;
  end;

  { TDreamcastSoftwareDevelopmentFileSystemToolchain }
  TDreamcastSoftwareDevelopmentFileSystemToolchain = class(TObject)
  private
    fOwner: TDreamcastSoftwareDevelopmentFileSystem;
    fBaseDirectory: TFileName;
    fBinutilsExecutable: TFileName;
    fGCCExecutable: TFileName;
    fGDBExecutable: TFileName;
    fKind: TToolchainKind;
    fNewlibBinary: TFileName;
    fToolchainInstalled: Boolean;
  public
    constructor Create(AOwner: TDreamcastSoftwareDevelopmentFileSystem;
      AToolchainKind: TToolchainKind);
    destructor Destroy; override;

    function GetGdbProfileKeyFromFileName(const FileName: TFileName): string;
    function GetToolchainProfileKeyFromFileName(const FileName: TFileName): string;
    function Reset: Boolean;

    property BaseDirectory: TFileName read fBaseDirectory;
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
    fOwner: TDreamcastSoftwareDevelopmentFileSystem;
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
    constructor Create(AOwner: TDreamcastSoftwareDevelopmentFileSystem);
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

  { TToolchainProfileInfo }
  // Class for toolchain profile information
  TToolchainProfileInfo = class(TObject)
  private
    fArmEabiWrapperPackage: TFileName;
    fBitness: TPortableExecutableBitness;
    fProfileKey: string;
    fName: string;
    fShElfWrapperPackage: TFileName;
    fVersion: string;
    fDescription: string;
    fArmEabiPackage: TFileName;
    fShElfPackage: TFileName;
    fChecksum: string;
  protected
    function GetPackageForToolchainKind(ToolchainKind: TToolchainKind): TFileName;
    property ArmEabiWrapperPackage: TFileName read fArmEabiWrapperPackage;
    property ShElfWrapperPackage: TFileName read fShElfWrapperPackage;
  public
    constructor Create;

    property ProfileKey: string read fProfileKey;
    property Name: string read fName;
    property Version: string read fVersion;
    property Description: string read fDescription;
    property ArmEabiPackage: TFileName read fArmEabiPackage;
    property ShElfPackage: TFileName read fShElfPackage;
    property Checksum: string read fChecksum;
    property Bitness: TPortableExecutableBitness read fBitness;
  end;

  { TGdbProfileInfo }
  // Class for GDB profile information
  TGdbProfileInfo = class(TObject)
  private
    fBitness: TPortableExecutableBitness;
    fProfileKey: string;
    fName: string;
    fPythonVersion: string;
    fVersion: string;
    fGdbPackage: string;
    fChecksum: string;
  public
    constructor Create;

    property ProfileKey: string read fProfileKey;
    property Name: string read fName;
    property Version: string read fVersion;
    property GdbPackage: string read fGdbPackage;
    property PythonVersion: string read fPythonVersion;
    property Checksum: string read fChecksum;
    property Bitness: TPortableExecutableBitness read fBitness;
  end;

  // Typed lists
  TToolchainProfileList = specialize TFPGObjectList<TToolchainProfileInfo>;
  TGdbProfileList = specialize TFPGObjectList<TGdbProfileInfo>;

  { TDreamcastSoftwareDevelopmentFileSystemPackages }
  TDreamcastSoftwareDevelopmentFileSystemPackages = class(TObject)
  private
    fOwner: TDreamcastSoftwareDevelopmentFileSystem;
    fToolchainProfiles: TToolchainProfileList;
    fGdbProfiles: TGdbProfileList;
    fAvailableToolchainProfiles: TStringList;
    fAvailableGdbProfiles: TStringList;

    procedure ParseConfigFile(IniFile: TMemIniFile);
    procedure LoadToolchainProfiles(IniFile: TMemIniFile);
    procedure LoadGdbProfiles(IniFile: TMemIniFile);
    procedure LoadChecksums(IniFile: TMemIniFile);
    procedure UpdateAvailableProfiles;
  protected
    // Loading methods
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const ConfigContent: string);
  public
    constructor Create(AOwner: TDreamcastSoftwareDevelopmentFileSystem;
      const AutoLoad: Boolean = True);
    destructor Destroy; override;

    // Data access properties
    property ToolchainProfiles: TToolchainProfileList read fToolchainProfiles;
    property GdbProfiles: TGdbProfileList read fGdbProfiles;

    // Search methods
    function GetToolchainProfileByKey(const ProfileKey: string): TToolchainProfileInfo;
    function GetGdbProfileByKey(const ProfileKey: string): TGdbProfileInfo;
    function GetGdbPackage(const ProfileKey: string): TFileName;
    function GetToolchainPackage(const ProfileKey: string;
      ToolchainKind: TToolchainKind): TFileName;
    function GetToolchainWrappersPackage(const ProfileKey: string;
      ToolchainKind: TToolchainKind): TFileName;

    // Utility methods
    function GetAvailableToolchainProfiles: TStringList;
    function GetAvailableGdbProfiles: TStringList;

    // Profile validation
    function IsValidToolchainProfile(const ProfileKey: string): Boolean;
    function IsValidGdbProfile(const ProfileKey: string): Boolean;

    // Checksum lookup methods
    function GetToolchainProfileKeyByChecksum(const Checksum: string): string;
    function GetGdbProfileKeyByChecksum(const Checksum: string): string;

    // Count properties
    function GetToolchainProfileCount: Integer;
    function GetGdbProfileCount: Integer;
  end;

  { TDreamcastSoftwareDevelopmentFileSystem }
  TDreamcastSoftwareDevelopmentFileSystem = class(TObject)
  private
    fOwner: TDreamcastSoftwareDevelopmentEnvironment;
    fPackages: TDreamcastSoftwareDevelopmentFileSystemPackages;
    fPackagesBase: TFileName;
    fToolchainBase: TFileName;
    fDreamcastTool: TDreamcastSoftwareDevelopmentFileSystemDreamcastTool;
    fKallisti: TDreamcastSoftwareDevelopmentFileSystemKallisti;
    fRuby: TDreamcastSoftwareDevelopmentFileSystemRuby;
    fShell: TDreamcastSoftwareDevelopmentFileSystemShell;
    fToolchainARM: TDreamcastSoftwareDevelopmentFileSystemToolchain;
    fToolchainSuperH: TDreamcastSoftwareDevelopmentFileSystemToolchain;
    fToolchainWin32: TDreamcastSoftwareDevelopmentFileSystemToolchain;

    function GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  protected
    procedure ComputeBaseDirectories;
    procedure ComputeFileSystemObjectValues(InstallPath: TFileName);
    function GetReferentialDirectory: TFileName;

    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read GetEnvironment;
    property PackagesBase: TFileName read fPackagesBase;
  public
    constructor Create(AOwner: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;

{$IFDEF DEBUG}
    procedure DebugPrintAllValues;
{$ENDIF}

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
    property Packages: TDreamcastSoftwareDevelopmentFileSystemPackages
      read fPackages;
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
    fAutoLoad: Boolean;
    fFoundationKind: TEnvironmentFoundationKind;
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
    function GetRepositoryFetchDateTime(const WorkingDirectory: TFileName): TDateTime;
  public
    constructor Create(const AutoLoad: Boolean);
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
    property FoundationKind: TEnvironmentFoundationKind
      read fFoundationKind;
    property Settings: TDreamcastSoftwareDevelopmentSettings read fSettings;
    property ShellCommandError: Boolean read fShellCommandError;
    property OnShellCommandNewLine: TNewLineEvent read fShellCommandNewLine
      write fShellCommandNewLine;
  end;

implementation

uses
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
  SysTools,
  StrTools,
  FSTools,
  RunTools,
  InetUtil
{$IFDEF GUI}
  , PostInst
{$ENDIF}
  ;

const
  FAIL_TAG = 'fatal: ';

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

function TDreamcastSoftwareDevelopmentFileSystemKallisti.GetKallistiChangeLogFile: TFileName;
begin
  Result := KallistiDirectory + 'doc\CHANGELOG.md';
  if not FileExists(Result) then
    Result := KallistiDirectory + 'doc\CHANGELOG';
end;

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

function TDreamcastSoftwareDevelopmentFileSystemToolchain.GetGdbProfileKeyFromFileName(
  const FileName: TFileName): string;
var
  CurrentHash: string;

begin
  CurrentHash := GetFileHash(FileName);
  Result := fOwner.Packages.GetGdbProfileKeyByChecksum(CurrentHash);
end;

function TDreamcastSoftwareDevelopmentFileSystemToolchain.GetToolchainProfileKeyFromFileName
  (const FileName: TFileName): string;
var
  CurrentHash: string;

begin
  CurrentHash := GetFileHash(FileName);
  Result := fOwner.Packages.GetToolchainProfileKeyByChecksum(CurrentHash);
end;

constructor TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(
  AOwner: TDreamcastSoftwareDevelopmentFileSystem;
  AToolchainKind: TToolchainKind);
begin
  fOwner := AOwner;
  fKind := AToolchainKind;
end;

destructor TDreamcastSoftwareDevelopmentFileSystemToolchain.Destroy;
begin
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

{ TDreamcastSoftwareDevelopmentFileSystemShell }

constructor TDreamcastSoftwareDevelopmentFileSystemShell.Create(
  AOwner: TDreamcastSoftwareDevelopmentFileSystem);
begin
  fOwner := AOwner;
end;

{ TDreamcastSoftwareDevelopmentFileSystem }

function TDreamcastSoftwareDevelopmentFileSystem.GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
begin
  Result := fOwner;
end;

procedure TDreamcastSoftwareDevelopmentFileSystem.ComputeBaseDirectories;
begin
  fPackagesBase := GetMSysBaseDirectory
    + UnixPathToSystem(DREAMSDK_MSYS_INSTALL_PACKAGES_DIRECTORY);
end;

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
  ToolchainBaseWin32: TFileName;
{$IFDEF SH_ELF_BFD_PLUGIN_LIBDEP_WORKAROUND}
  ToolchainBfdPluginsSuperH: TFileName;
{$ENDIF}

begin
  // Base
  MSYSBase := GetMSysBaseDirectory;
  fToolchainBase := MSYSBase + UnixPathToSystem(DREAMSDK_MSYS_TOOLCHAINS_INSTALL_DIRECTORY);

  // Translate DREAMSDK_MSYS_INSTALL_DIRECTORY to Windows location
  DreamSDKBase := IncludeTrailingPathDelimiter(MSYSBase
    + UnixPathToSystem(DREAMSDK_MSYS_INSTALL_DIRECTORY));

  with fShell do
  begin
    if Environment.FoundationKind = efkMinGW64MSYS2 then
    begin
      fMinGWGetExecutable := EmptyStr; // not available on MSYS2
      fShellExecutable := GetUserBinariesBaseDirectory + 'bash.exe';
    end
    else
    begin
      // Default for MSYS
      fMinGWGetExecutable := InstallPath + 'bin\mingw-get.exe';
      fShellExecutable := MSYSBase + 'bin\sh.exe';
    end;

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
  end;

  // Toolchain for Win32 (Windows)
  ToolchainBaseWin32 := GetWindowsToolchainBaseDirectory;
  with fToolchainWin32 do
  begin
    fBaseDirectory := ToolchainBaseWin32;
    fToolchainInstalled := DirectoryExists(ToolchainBaseWin32);
    fBinutilsExecutable := ToolchainBaseWin32 + 'ld.exe';
    fGCCExecutable := ToolchainBaseWin32 + 'gcc.exe';
    fGDBExecutable := ToolchainBaseWin32 + 'gdb.exe';
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

{$IFDEF SH_ELF_BFD_PLUGIN_LIBDEP_WORKAROUND}
  // The libdep plugin is incompatible with Windows.
  // See: https://sourceware.org/bugzilla/show_bug.cgi?id=27113
  ToolchainBfdPluginsSuperH := ToolchainBaseSuperH + 'lib\bfd-plugins\';

  KillFile(ToolchainBfdPluginsSuperH + 'libdep.a');
  KillFile(ToolchainBfdPluginsSuperH + 'libdep.la');
  KillFile(ToolchainBfdPluginsSuperH + 'libdep.dll.a');
{$ENDIF}
end;

constructor TDreamcastSoftwareDevelopmentFileSystem.Create(
  AOwner: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fOwner := AOwner;

  // Directories
  ComputeBaseDirectories;

  fPackages := TDreamcastSoftwareDevelopmentFileSystemPackages.Create(Self,
    fOwner.fAutoLoad);
  fDreamcastTool := TDreamcastSoftwareDevelopmentFileSystemDreamcastTool.Create;
  fKallisti := TDreamcastSoftwareDevelopmentFileSystemKallisti.Create;
  fToolchainARM := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(Self, tkARM);
  fToolchainSuperH := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(Self, tkSuperH);
  fToolchainWin32 := TDreamcastSoftwareDevelopmentFileSystemToolchain.Create(Self, tkWin32);
  fShell := TDreamcastSoftwareDevelopmentFileSystemShell.Create(Self);
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
  fPackages.Free;
  inherited Destroy;
end;

{$IFDEF DEBUG}

procedure TDreamcastSoftwareDevelopmentFileSystem.DebugPrintAllValues;
begin
  (*
  Kallisti
Shell
ToolchainARM
ToolchainBase
ToolchainSuperH
ToolchainWin32
Ruby*)

  DebugLog(
    Format(
      '#############################################################################' + sLineBreak
      + 'DreamcastTool:' + sLineBreak
      + '  BaseDirectory: "%s"' + sLineBreak
      + '  ConfigurationFileName: "%s"' + sLineBreak
      + 'Kallisti:' + sLineBreak
      + '  KallistiChangeLogFile: "%s"' + sLineBreak
      + '  KallistiConfigurationFileName: "%s"' + sLineBreak
      + '  KallistiDirectory: "%s"' + sLineBreak
      + '  KallistiLibrary: "%s"' + sLineBreak
      + '  KallistiPortsDirectory: "%s"' + sLineBreak
      + '  KallistiPortsLibraryInformationFile: "%s"' + sLineBreak
      + '  KallistiUtilitiesDirectory: "%s"' + sLineBreak
      + '  Packages.Kallisti: "%s"' + sLineBreak
      + '  Packages.KallistiPorts: "%s"' + sLineBreak
      + '#############################################################################'
    , [
      DreamcastTool.BaseDirectory,
      DreamcastTool.ConfigurationFileName,
      Kallisti.KallistiChangeLogFile,
      Kallisti.KallistiConfigurationFileName,
      Kallisti.KallistiDirectory,
      Kallisti.KallistiLibrary,
      Kallisti.KallistiPortsDirectory,
      Kallisti.KallistiPortsLibraryInformationFile,
      Kallisti.KallistiUtilitiesDirectory,
      Kallisti.Packages.Kallisti,
      Kallisti.Packages.KallistiPorts
    ])
  );
end;

{$ENDIF}

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
  fShellCommandBufferOutput := EmptyStr;
  if Assigned(fShellCommandRunner) then
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
    WorkingDirectory := GetCurrentDir;

    Parameters.Add('--login');

    for i := 1 to GetEnvironmentVariableCount do
    begin
      Environment.Add(GetEnvironmentString(i));
    end;

    Environment.Add('_EXTERNAL_COMMAND=' + CommandLine);
    Environment.Add('_WORKING_DIRECTORY=' + SystemToDreamSdkPath(WorkingDirectory));
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

function TDreamcastSoftwareDevelopmentEnvironment.GetRepositoryFetchDateTime(
  const WorkingDirectory: TFileName): TDateTime;
const
  GIT_CONTROL_DIRNAME = '.git';

var
  FetchFileName: TFileName;

begin
  Result := Default(TDateTime);
  if DirectoryExists(WorkingDirectory) then
  begin
    FetchFileName := IncludeTrailingPathDelimiter(WorkingDirectory) + GIT_CONTROL_DIRNAME + DirectorySeparator + 'FETCH_HEAD';
    if FileExists(FetchFileName) then
      Result := GetFileDate(FetchFileName)
    else
    begin
      FetchFileName := IncludeTrailingPathDelimiter(WorkingDirectory) + GIT_CONTROL_DIRNAME + DirectorySeparator + 'HEAD';
      if FileExists(FetchFileName) then
        Result := GetFileDate(FetchFileName);
    end;
  end;
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

constructor TDreamcastSoftwareDevelopmentEnvironment.Create(
  const AutoLoad: Boolean);
begin
  fAutoLoad := AutoLoad;
  fShellCommandError := False;
  fMsysBaseDirectoryNormal := ExcludeTrailingPathDelimiter(GetMSysBaseDirectory);
  fMsysBaseDirectoryReverse := StringReplace(fMsysBaseDirectoryNormal,
    DirectorySeparator, '/', [rfReplaceAll]);
  fFoundationKind := GetBaseEnvironmentFoundationKind;
  fFileSystem := TDreamcastSoftwareDevelopmentFileSystem.Create(Self);
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
{$IFDEF DEBUG}
  DebugLog('ExecuteShellCommand');
{$ENDIF}

  CurrentDir := GetCurrentDir;

{$IFDEF DEBUG}
  DebugLog(Format('  CurrentDir (1): "%s"', [GetCurrentDir]));
{$ENDIF}

  if not DirectoryExists(WorkingDirectory) then
    ForceDirectories(WorkingDirectory);

  SetCurrentDir(WorkingDirectory);

{$IFDEF DEBUG}
  DebugLog(Format('  CurrentDir (2): "%s"', [GetCurrentDir]));
{$ENDIF}

  Result := ExecuteShellCommandRunner(CommandLine);

  SetCurrentDir(CurrentDir);

{$IFDEF DEBUG}
  DebugLog(Format('  CurrentDir (3): "%s"', [GetCurrentDir]));
{$ENDIF}
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

{$IFDEF DEBUG}
  DebugLog(Format('*** CloneRepository: ' + sLineBreak
    + '  URL: %s' + sLineBreak
    + '  TargetDirectoryName: %s' + sLineBreak
    + '  WorkingDirectory: %s' + sLineBreak
    + '  TargetDirectoryFileName: %s' + sLineBreak, [
      URL,
      TargetDirectoryName,
      WorkingDirectory,
      TargetDirectoryFileName
    ])
  );
{$ENDIF}

  if not IsOfflineRepository(TargetDirectoryFileName) then
  begin
{$IFDEF DEBUG}
    if IsEmpty(URL) then
      DebugLog('Warning: CloneRepository: URL is empty!');
{$ENDIF}
    CommandLine := Format('git -C "%s" clone "%s" "%s" --progress', [
      SystemToDreamSdkPath(WorkingDirectory),
      URL,
      TargetDirectoryName
    ]);
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
var
  PullDate: TDateTime;
  RepositoryProvider,
  GitHash,
  StrPullDate: string;

begin
  Result := EmptyStr;
  if DirectoryExists(WorkingDirectory) then
    if IsOfflineRepository(WorkingDirectory) then
      Result := LoadFileToString(GetOfflineFileName(WorkingDirectory))
    else
	  begin
		  try
		    GitHash := Trim(Run('git', 'describe --always --long', WorkingDirectory, False));
		    if not IsInString(FAIL_TAG, GitHash) then
        begin
          // Get repository provider, usually 'github' or 'gitlab'
          RepositoryProvider := Run('git', 'config --get remote.origin.url', WorkingDirectory, False);
          if not IsEmpty(RepositoryProvider) then
            RepositoryProvider := GetHostFromUri(RepositoryProvider) + '-';

          // Get the git pull date
          StrPullDate := EmptyStr;
          PullDate := GetRepositoryFetchDateTime(WorkingDirectory);
          if PullDate <> Default(TDateTime) then
            StrPullDate := '-' + FormatDateTime('YYYY.MM.DD', PullDate);

          // Combine final repository version info
          Result := RepositoryProvider + GitHash + StrPullDate;
        end;
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
  TempBuffer,
  CommandLine: string;
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
      CommandLine := Format('git -C "%s" pull', [
        SystemToDreamSdkPath(WorkingDirectory)
      ]);
      BufferOutput := ExecuteShellCommand(CommandLine, WorkingDirectory);
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

{ TToolchainProfileInfo }

constructor TToolchainProfileInfo.Create;
begin
  inherited Create;
  fProfileKey := '';
  fName := '';
  fVersion := '';
  fDescription := '';
  fArmEabiPackage := '';
  fShElfPackage := '';
  fChecksum := '';
end;

function TToolchainProfileInfo.GetPackageForToolchainKind(ToolchainKind: TToolchainKind): TFileName;
begin
  case ToolchainKind of
    tkARM: Result := fArmEabiPackage;
    tkSuperH: Result := fShElfPackage;
    tkWin32: raise EUnsupportedToolchainKind.Create('Win32 toolchain kind is not supported in this context');
  else
    Result := '';
  end;
end;

{ TGdbProfileInfo }

constructor TGdbProfileInfo.Create;
begin
  inherited Create;
  fProfileKey := '';
  fName := '';
  fVersion := '';
  fGdbPackage := '';
  fChecksum := '';
end;

{ TDreamcastSoftwareDevelopmentFileSystemPackages }

constructor TDreamcastSoftwareDevelopmentFileSystemPackages.Create(
  AOwner: TDreamcastSoftwareDevelopmentFileSystem;
  const AutoLoad: Boolean);
const
  PACKAGES_FILE = 'packages.conf';

begin
  inherited Create;

  fOwner := AOwner;
  fToolchainProfiles := TToolchainProfileList.Create(True);
  fGdbProfiles := TGdbProfileList.Create(True);
  fAvailableToolchainProfiles := TStringList.Create;
  fAvailableGdbProfiles := TStringList.Create;

  if AutoLoad then
    LoadFromFile(GetConfigurationDirectory + PACKAGES_FILE);
end;

destructor TDreamcastSoftwareDevelopmentFileSystemPackages.Destroy;
begin
  fAvailableGdbProfiles.Free;
  fAvailableToolchainProfiles.Free;
  fGdbProfiles.Free;
  fToolchainProfiles.Free;
  inherited Destroy;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.LoadFromFile(const FileName: string);
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(FileName);
  try
    ParseConfigFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.LoadFromString(const ConfigContent: string);
var
  IniFile: TMemIniFile;
  TempFile: TStringList;
begin
  TempFile := TStringList.Create;
  IniFile := TMemIniFile.Create('');
  try
    TempFile.Text := ConfigContent;
    IniFile.SetStrings(TempFile);
    ParseConfigFile(IniFile);
  finally
    IniFile.Free;
    TempFile.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.ParseConfigFile(IniFile: TMemIniFile);
begin
  // Clear existing data
  fToolchainProfiles.Clear;
  fGdbProfiles.Clear;
  fAvailableToolchainProfiles.Clear;
  fAvailableGdbProfiles.Clear;

  // Load profiles
  LoadToolchainProfiles(IniFile);
  LoadGdbProfiles(IniFile);

  // Load checksums
  LoadChecksums(IniFile);

  // Update available profiles lists
  UpdateAvailableProfiles;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.LoadToolchainProfiles(IniFile: TMemIniFile);
var
  Sections: TStringList;
  i: Integer;
  SectionName,
  ProfileName,
  ProfileKeys,
  Bitness: string;
  Profile: TToolchainProfileInfo;

begin
  Sections := TStringList.Create;
  try
    ProfileKeys := IniFile.ReadString('Packages', 'ToolchainProfiles', EmptyStr);
    StringToStringList(ProfileKeys, ';', Sections);

{$IFDEF DEBUG}
    DebugLog(Format('LoadToolchainProfiles: %d entries', [Sections.Count]));
{$ENDIF}

    for i := 0 to Sections.Count - 1 do
    begin
      ProfileName := Sections[i];

{$IFDEF DEBUG}
      DebugLog('  ' + ProfileName + ':');
{$ENDIF}

      SectionName := 'ToolchainProfile_' + ProfileName;

      if IniFile.SectionExists(SectionName) then
      begin
        Profile := TToolchainProfileInfo.Create;
        Profile.fProfileKey := ProfileName;
        Profile.fName := IniFile.ReadString(SectionName, 'Name', '');
        Profile.fVersion := IniFile.ReadString(SectionName, 'Version', '');
        Profile.fDescription := IniFile.ReadString(SectionName, 'Description', '');

        // ARM
        Profile.fArmEabiPackage := fOwner.PackagesBase +
          IniFile.ReadString(SectionName, 'ArmEabiPackage', '');
        Profile.fArmEabiWrapperPackage := fOwner.PackagesBase
          + 'arm-eabi-wrappers-bin.7z';

        // Super-H
        Profile.fShElfPackage := fOwner.PackagesBase +
          IniFile.ReadString(SectionName, 'ShElfPackage', '');
        Profile.fShElfWrapperPackage := fOwner.PackagesBase
          + 'sh-elf-wrappers-bin.7z';

        // Handle Bitness
        Profile.fBitness := pebUnknown;
        Bitness := IniFile.ReadString(SectionName, 'Architecture', EmptyStr);
        if Bitness = '32' then
          Profile.fBitness := peb32
        else if Bitness = '64' then
          Profile.fBitness := peb64;

{$IFDEF DEBUG}
        Bitness := GetEnumName(TypeInfo(TPortableExecutableBitness), Ord(Profile.fBitness));
        DebugLog('    Bitness: ' + Bitness);
{$ENDIF}

{$IFDEF DEBUG}
        DebugLog('    Loaded: ' + Profile.Version );
{$ENDIF}

        fToolchainProfiles.Add(Profile);
      end;
    end;

  finally
    Sections.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.LoadGdbProfiles(IniFile: TMemIniFile);
const
  PYTHON_NAME_KEYWORD = 'Python';

var
  Sections: TStringList;
  i: Integer;
  SectionName,
  ProfileKey,
  ProfileKeys,
  Bitness: string;
  Profile: TGdbProfileInfo;

begin
  Sections := TStringList.Create;
  try
    ProfileKeys := IniFile.ReadString('Packages', 'GdbProfiles', EmptyStr);
    StringToStringList(ProfileKeys, ';', Sections);

{$IFDEF DEBUG}
    DebugLog(Format('LoadGdbProfiles: %d entries', [Sections.Count]));
{$ENDIF}

    for i := 0 to Sections.Count - 1 do
    begin
      ProfileKey := Sections[i];

{$IFDEF DEBUG}
      DebugLog('  ' + ProfileKey + ':');
{$ENDIF}

      SectionName := 'GdbProfile_' + ProfileKey;
      if IniFile.SectionExists(SectionName) then
      begin
        Profile := TGdbProfileInfo.Create;
        Profile.fProfileKey := ProfileKey;
        Profile.fName := IniFile.ReadString(SectionName, 'Name', '');
        Profile.fVersion := IniFile.ReadString(SectionName, 'Version', '');
        Profile.fGdbPackage := fOwner.PackagesBase
          + IniFile.ReadString(SectionName, 'GdbPackage', '');
        if StartsWith(PYTHON_NAME_KEYWORD, Profile.fName) then
          Profile.fPythonVersion := Trim(StringReplace(Profile.fName, PYTHON_NAME_KEYWORD, EmptyStr, []));

        // Handle Bitness
        Profile.fBitness := pebUnknown;
        Bitness := IniFile.ReadString(SectionName, 'Architecture', EmptyStr);
        if Bitness = '32' then
          Profile.fBitness := peb32
        else if Bitness = '64' then
          Profile.fBitness := peb64;

{$IFDEF DEBUG}
        Bitness := GetEnumName(TypeInfo(TPortableExecutableBitness), Ord(Profile.fBitness));
        DebugLog('    Bitness: ' + Bitness);
{$ENDIF}

{$IFDEF DEBUG}
        DebugLog('    Loaded: ' + Profile.Version );
{$ENDIF}

        fGdbProfiles.Add(Profile);
      end;
    end;

  finally
    Sections.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.LoadChecksums(IniFile: TMemIniFile);
var
  ChecksumKeys: TStringList;
  i: Integer;
  Checksum,
  ProfileKey: string;
  ToolchainProfile: TToolchainProfileInfo;
  GdbProfile: TGdbProfileInfo;

begin
  ChecksumKeys := TStringList.Create;
  try
    // Load toolchain profile checksums
    if IniFile.SectionExists('ToolchainProfilesChecksums') then
    begin
      IniFile.ReadSection('ToolchainProfilesChecksums', ChecksumKeys);

      for i := 0 to ChecksumKeys.Count - 1 do
      begin
        Checksum := ChecksumKeys[i];
        ProfileKey := IniFile.ReadString('ToolchainProfilesChecksums', Checksum, '');

        ToolchainProfile := GetToolchainProfileByKey(ProfileKey);
        if Assigned(ToolchainProfile) then
          ToolchainProfile.fChecksum := Checksum;
      end;
    end;

    // Clear buffer before dealing with GDB profiles
    ChecksumKeys.Clear;

    // Load GDB profile checksums
    if IniFile.SectionExists('GdbProfilesChecksums') then
    begin
      IniFile.ReadSection('GdbProfilesChecksums', ChecksumKeys);

      for i := 0 to ChecksumKeys.Count - 1 do
      begin
        Checksum := ChecksumKeys[i];
        ProfileKey := IniFile.ReadString('GdbProfilesChecksums', Checksum, '');

        GdbProfile := GetGdbProfileByKey(ProfileKey);
        if Assigned(GdbProfile) then
          GdbProfile.fChecksum := Checksum;
      end;
    end;

  finally
    ChecksumKeys.Free;
  end;
end;

procedure TDreamcastSoftwareDevelopmentFileSystemPackages.UpdateAvailableProfiles;
var
  i: Integer;
begin
  fAvailableToolchainProfiles.Clear;
  for i := 0 to fToolchainProfiles.Count - 1 do
    fAvailableToolchainProfiles.Add(fToolchainProfiles[i].ProfileKey);

  fAvailableGdbProfiles.Clear;
  for i := 0 to fGdbProfiles.Count - 1 do
    fAvailableGdbProfiles.Add(fGdbProfiles[i].ProfileKey);
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetToolchainProfileByKey(
  const ProfileKey: string): TToolchainProfileInfo;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to fToolchainProfiles.Count - 1 do
  begin
    if SameText(fToolchainProfiles[i].ProfileKey, ProfileKey) then
    begin
      Result := fToolchainProfiles[i];
      Break;
    end;
  end;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetGdbProfileByKey(
  const ProfileKey: string): TGdbProfileInfo;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to fGdbProfiles.Count - 1 do
  begin
    if SameText(fGdbProfiles[i].ProfileKey, ProfileKey) then
    begin
      Result := fGdbProfiles[i];
      Break;
    end;
  end;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetGdbPackage(
  const ProfileKey: string): TFileName;
var
  Profile: TGdbProfileInfo;

begin
  Result := EmptyStr;
  Profile := GetGdbProfileByKey(ProfileKey);
  if Assigned(Profile) then
    Result := Profile.GdbPackage;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetToolchainPackage(
  const ProfileKey: string; ToolchainKind: TToolchainKind): TFileName;
var
  Profile: TToolchainProfileInfo;

begin
  Result := EmptyStr;
  Profile := GetToolchainProfileByKey(ProfileKey);
  if Assigned(Profile) then
    Result := Profile.GetPackageForToolchainKind(ToolchainKind);
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetToolchainWrappersPackage(
  const ProfileKey: string; ToolchainKind: TToolchainKind): TFileName;
var
  Profile: TToolchainProfileInfo;

begin
  Result := EmptyStr;
  Profile := GetToolchainProfileByKey(ProfileKey);
  if Assigned(Profile) then
    case ToolchainKind of
      tkSuperH:
        Result := Profile.ShElfWrapperPackage;
      tkARM:
        Result := Profile.ArmEabiWrapperPackage;
    end;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetAvailableToolchainProfiles: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(fAvailableToolchainProfiles);
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetAvailableGdbProfiles: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(fAvailableGdbProfiles);
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.IsValidToolchainProfile(const ProfileKey: string): Boolean;
begin
  Result := fAvailableToolchainProfiles.IndexOf(ProfileKey) >= 0;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.IsValidGdbProfile(const ProfileKey: string): Boolean;
begin
  Result := fAvailableGdbProfiles.IndexOf(ProfileKey) >= 0;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetToolchainProfileCount: Integer;
begin
  Result := fToolchainProfiles.Count;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetGdbProfileCount: Integer;
begin
  Result := fGdbProfiles.Count;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetToolchainProfileKeyByChecksum(const Checksum: string): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to fToolchainProfiles.Count - 1 do
  begin
    if SameText(fToolchainProfiles[i].Checksum, Checksum) then
    begin
      Result := fToolchainProfiles[i].ProfileKey;
      Break;
    end;
  end;
end;

function TDreamcastSoftwareDevelopmentFileSystemPackages.GetGdbProfileKeyByChecksum(const Checksum: string): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to fGdbProfiles.Count - 1 do
  begin
    if SameText(fGdbProfiles[i].Checksum, Checksum) then
    begin
      Result := fGdbProfiles[i].ProfileKey;
      Break;
    end;
  end;
end;

end.

