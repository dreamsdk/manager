unit Main;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  CheckLst,
  MaskEdit,
  SysTools,
  Global,
  PortMgr,
  ShellThd,
  DCSDKMgr,
  Environ,
  StrRes,
  PkgMgr;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    apMain: TApplicationProperties;
    btnAllPortInstall: TButton;
    btnIdeCodeBlocksInstallDir: TButton;
    btnIdeRefresh: TButton;
    btnAllPortUninstall: TButton;
    btnIdeCodeBlocksInstallDetect: TButton;
    btnClose: TButton;
    btnCheckForUpdates: TButton;
    btnIdeInstall: TButton;
    btnIdeReinstall: TButton;
    btnIdeUninstall: TButton;
    btnComponentsApply: TButton;
    btnOfflineRuby: TButton;
    btnOfflineKallistiPorts: TButton;
    btnOfflineDreamcastToolSerial: TButton;
    btnOfflineDreamcastToolIP: TButton;
    btnOpenHelp: TButton;
    btnOpenHome: TButton;
    btnRubyOpenHome: TButton;
    btnOpenMinGWManager: TButton;
    btnOpenMSYS: TButton;
    btnRubyOpenMSYS: TButton;
    btnPortInstall: TButton;
    btnPortUninstall: TButton;
    btnPortUpdate: TButton;
    btnRestoreDefaults: TButton;
    btnUpdateKallistiOS: TButton;
    btnCredits: TButton;
    btnDreamcastToolCustomExecutable: TButton;
    btnUninstallMRuby: TButton;
    btnInstallMRuby: TButton;
    btnOptionsShortcut: TButton;
    btnUrlKallisti: TButton;
    btnBrowseHomeBaseDirectory: TButton;
    btnUrlRuby: TButton;
    btnUrlKallistiPorts: TButton;
    btnUrlDreamcastToolSerial: TButton;
    btnUrlDreamcastToolIP: TButton;
    btnUpdateMRuby: TButton;
    btnOfflineKallisti: TButton;
    btnIdeCodeBlocksUsersAvailableInitialize: TButton;
    btnIdeCodeBlocksUsersAvailableRefresh: TButton;
    btnWindowsTerminalInstall: TButton;
    btnWindowsTerminalUninstall: TButton;
    btnDebugEnvironmentFileSystemPrintAllValues: TButton;
    btnDebugRaiseException: TButton;
    btnDebugGetSelectedToolchain: TButton;
    btnDebugGetSelectedDebugger: TButton;
    cbxDreamcastToolSerialBaudrate: TComboBox;
    cbxDreamcastToolSerialPort: TComboBox;
    cbxToolchain: TComboBox;
    cbxUrlDreamcastToolIP: TComboBox;
    cbxUrlDreamcastToolSerial: TComboBox;
    cbxUrlKallisti: TComboBox;
    cbxUrlRuby: TComboBox;
    cbxUrlKallistiPorts: TComboBox;
    ckxDreamcastToolInternetProtocolUseARP: TCheckBox;
    ckxDreamcastToolAttachConsoleFileServer: TCheckBox;
    ckxDreamcastToolClearScreenBeforeDownload: TCheckBox;
    ckxDreamcastToolSerialAlternateBaudrate: TCheckBox;
    ckxDreamcastToolSerialDumbTerminal: TCheckBox;
    ckxDreamcastToolSerialExternalClock: TCheckBox;
    cbxModuleSelection: TComboBox;
    cbxDreamcastToolInternetProtocolNetworkAdapter: TComboBox;
    cbxDebugger: TComboBox;
    edtDreamcastToolSerialBaudrateCustom: TEdit;
    gbxRubyFolder: TGroupBox;
    gbxRubyRunShell: TGroupBox;
    gbxUrlRuby: TGroupBox;
    gbxDebugger: TGroupBox;
    gbxWindowsTerminal: TGroupBox;
    lblComponentsConfiguration: TLabel;
    lblIdeCodeBlocksUsersAvailable: TLabel;
    lblTextChangeLogKallistiOS: TLabel;
    lblTextOfflineStatusKallistiOS: TLabel;
    lblTextToolchainBuildDate: TLabel;
    lblTextToolchainBuildDateARM: TLabel;
    lblToolchain: TLabel;
    lblBuildDateMRuby: TLabel;
    lblHomeFolder1: TLabel;
    lblRubyShell: TLabel;
    lblIdeCodeBlocksUsersInstalled: TLabel;
    lblTextBuildDateMRuby: TLabel;
    lblTextMRuby: TLabel;
    lblTextRuby: TLabel;
    lblTextMeson: TLabel;
    lblTextVersionKallistiOS2: TLabel;
    lblDebugger: TLabel;
    lblBuildDateToolchain: TLabel;
    lblBuildDateToolchainARM: TLabel;
    lblOfflineStatusKallistiOS: TLabel;
    lblVersionChangeLogKallistiOS: TLabel;
    lblVersionMRuby: TLabel;
    lblVersionRuby: TLabel;
    lblVersionMeson: TLabel;
    lblVersionKallistiOS2: TLabel;
    lbxIdeCodeBlocksUsersAvailable: TListBox;
    memPortShortDescription: TEdit;
    edtIdeCodeBlocksInstallDir: TEdit;
    edtValueHomeBaseDir: TEdit;
    edtDreamcastToolCustomExecutable: TEdit;
    edtDreamcastToolCustomArguments: TEdit;
    edtDreamcastToolInternetProtocolAddress: TMaskEdit;
    edtDreamcastToolInternetProtocolMAC: TMaskEdit;
    edtModuleCompiledDate: TLabeledEdit;
    edtManagerCompilerInfo: TLabeledEdit;
    edtModuleFileVersion: TLabeledEdit;
    edtModuleProductVersion: TLabeledEdit;
    edtManagerLCLVersion: TLabeledEdit;
    edtManagerOS: TLabeledEdit;
    edtProductHelpVersion: TLabeledEdit;
    edtProductVersion: TLabeledEdit;
    edtManagerTargetInfo: TLabeledEdit;
    edtManagerWidgetSet: TLabeledEdit;
    edtPortLicense: TLabeledEdit;
    edtPortMaintainer: TLabeledEdit;
    edtPortURL: TLabeledEdit;
    edtPortVersion: TLabeledEdit;
    edtProductBuildDate: TLabeledEdit;
    gbxIdeCodeBlocksInstallDir: TGroupBox;
    gbxIdeList: TGroupBox;
    gbxIdeAll: TGroupBox;
    gbxVersionDreamcastToolSerial: TGroupBox;
    gbxEnvironmentContext: TGroupBox;
    gbxVersionDreamcastToolIP: TGroupBox;
    gbxVersionKallisti: TGroupBox;
    gbxModuleInfo: TGroupBox;
    gbxAvailablePorts: TGroupBox;
    gbxCompilerInfo: TGroupBox;
    gbxDependencies: TGroupBox;
    gbxDreamcastToolCommon: TGroupBox;
    gbxDreamcastToolInternetProtocol: TGroupBox;
    gbxDreamcastToolSerial: TGroupBox;
    gbxKallistiChangeLog: TGroupBox;
    gbxPortAll: TGroupBox;
    gbxPortDetails: TGroupBox;
    gbxToolchainSuperH: TGroupBox;
    gbxToolchainARM: TGroupBox;
    gbxUrlDreamcastToolIP: TGroupBox;
    gbxUrlDreamcastToolSerial: TGroupBox;
    gbxUrlKallistiOS: TGroupBox;
    gbxUrlKallistiPorts: TGroupBox;
    gbxHostEnvironment: TGroupBox;
    gbxPackageInfo: TGroupBox;
    gbxHomeRunShell: TGroupBox;
    gbxHomeFolder: TGroupBox;
    gbxHomeHelp: TGroupBox;
    gbxDreamcastToolCustomCommand: TGroupBox;
    gbxToolchainWin32: TGroupBox;
    gbxVersionKallistiPorts: TGroupBox;
    gbxIdeCodeBlocksUsersInstalled: TGroupBox;
    gbxIdeCodeBlocksUsersAvailable: TGroupBox;
    gbxIdeCodeBlocksActions: TGroupBox;
    lblDreamcastToolInternetProtocolNetworkAdapter: TLabel;
    lblComponentInformation: TLabel;
    lblDreamcastToolCustomArguments: TLabel;
    lblDreamcastToolInternetProtocolMAC: TLabel;
    lblDreamcastToolInternetProtocolInvalidMAC: TLabel;
    lblDreamcastToolCustomExecutable: TLabel;
    lblHomeHelp: TLabel;
    lblHomeShell: TLabel;
    lblHomeFolder: TLabel;
    lblIntroduction: TLabel;
    lblBuildDateKallistiOS: TLabel;
    lblDreamcastToolInternetProtocolAddress: TLabel;
    lblDreamcastToolSerialBaudrate: TLabel;
    lblDreamcastToolSerialPort: TLabel;
    lblDreamcastToolInternetProtocolInvalidAddress: TLabel;
    lblPortName: TLabel;
    lblTextBinutils: TLabel;
    lblTextBinutilsWin32: TLabel;
    lblTextBinutilsARM: TLabel;
    lblTextBuildDateKallistiOS: TLabel;
    lblTextGCC: TLabel;
    lblTextGCCWin32: TLabel;
    lblTextGCCARM: TLabel;
    lblTextGDB: TLabel;
    lblTextGDBWin32: TLabel;
    lblTextNewlib: TLabel;
    lblTextPythonGDB: TLabel;
    lblTextGit: TLabel;
    lblTextKallistiOS: TLabel;
    lblTextFoundation: TLabel;
    lblTextPython: TLabel;
    lblTextRepoKallistiOS: TLabel;
    lblTextVersionDreamcastToolSerial: TLabel;
    lblTextVersionDreamcastToolIP: TLabel;
    lblTextRepoKallistiPorts: TLabel;
    lblTextCMake: TLabel;
    lblTextRepoToolSerial: TLabel;
    lblTextRepoToolIP: TLabel;
    lblTitleAbout: TLabel;
    lblTitleHome: TLabel;
    lblVersionBinutils: TLabel;
    lblVersionBinutilsWin32: TLabel;
    lblVersionBinutilsARM: TLabel;
    lblVersionGCC: TLabel;
    lblVersionGCCWin32: TLabel;
    lblVersionGCCARM: TLabel;
    lblVersionGDB: TLabel;
    lblVersionGDBWin32: TLabel;
    lblVersionNewlib: TLabel;
    lblVersionPythonGDB: TLabel;
    lblVersionGit: TLabel;
    lblVersionKallistiOS: TLabel;
    lblVersionFoundation: TLabel;
    lblVersionPython: TLabel;
    lblVersionRepoKallistiOS: TLabel;
    lblVersionRepoKallistiPorts: TLabel;
    lblVersionCMake: TLabel;
    lblVersionRepoToolSerial: TLabel;
    lblVersionRepoToolIP: TLabel;
    lblVersionToolSerial: TLabel;
    lblVersionToolIP: TLabel;
    lbxPorts: TCheckListBox;
    lbxIdeList: TCheckListBox;
    lbxIdeCodeBlocksUsersInstalled: TListBox;
    memKallistiChangeLog: TMemo;
    memPortDescription: TMemo;
    opdDreamcastToolCustom: TOpenDialog;
    gbxIdeCodeBlocksUsersAvailableButtons: TPanel;
    pnlComponentsChange: TPanel;
    pnlRuby: TPanel;
    pnlAbout: TPanel;
    pcMain: TPageControl;
    pnlActions: TPanel;
    rbnComponentsChangeDebugger: TRadioButton;
    rbnComponentsChangeToolchain: TRadioButton;
    rbnComponentsNoChange: TRadioButton;
    rgxDreamcastTool: TRadioGroup;
    rgxTerminalOption: TRadioGroup;
    sddIdeCodeBlocks: TSelectDirectoryDialog;
    tsDebug: TTabSheet;
    tsRuby: TTabSheet;
    tsIDE: TTabSheet;
    tsComponents: TTabSheet;
    tsHome: TTabSheet;
    tmDisplayKallistiPorts: TTimer;
    tmrShellThreadTerminate: TTimer;
    tsAbout: TTabSheet;
    tsDreamcastTool: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    tsOptions: TTabSheet;
    procedure apMainException(Sender: TObject; E: Exception);
    procedure btnAllPortInstallClick(Sender: TObject);
    procedure btnAllPortUninstallClick(Sender: TObject);
    procedure btnBrowseHomeBaseDirectoryClick(Sender: TObject);
    procedure btnCheckForUpdatesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCreditsClick(Sender: TObject);
    procedure btnComponentsApplyClick(Sender: TObject);
    procedure btnDebugGetSelectedDebuggerClick(Sender: TObject);
    procedure btnDebugRaiseExceptionClick(Sender: TObject);
    procedure btnIdeCodeBlocksInstallDirClick(Sender: TObject);
    procedure btnInstallMRubyClick(Sender: TObject);
    procedure btnOfflineKallistiClick(Sender: TObject);
    procedure btnOpenHelpClick(Sender: TObject);
    procedure btnOpenHomeClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnOpenMSYSClick(Sender: TObject);
    procedure btnOptionsShortcutClick(Sender: TObject);
    procedure btnRubyOpenHomeClick(Sender: TObject);
    procedure btnRubyOpenMSYSClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure btnPortUninstallClick(Sender: TObject);
    procedure btnPortUpdateClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure btnUpdateKallistiOSClick(Sender: TObject);
    procedure btnDreamcastToolCustomExecutableClick(Sender: TObject);
    procedure btnUpdateMRubyClick(Sender: TObject);
    procedure btnUrlKallistiClick(Sender: TObject);
    procedure btnIdeInstallClick(Sender: TObject);
    procedure btnWindowsTerminalInstallClick(Sender: TObject);
    procedure btnWindowsTerminalUninstallClick(Sender: TObject);
    procedure btnDebugEnvironmentFileSystemPrintAllValuesClick(Sender: TObject);
    procedure btnDebugGetSelectedToolchainClick(Sender: TObject);
    procedure cbxDreamcastToolSerialBaudrateSelect(Sender: TObject);
    procedure cbxDreamcastToolSerialPortSelect(Sender: TObject);
    procedure cbxModuleSelectionChange(Sender: TObject);
    procedure cbxUrlDreamcastToolIPChange(Sender: TObject);
    procedure cbxUrlDreamcastToolSerialChange(Sender: TObject);
    procedure cbxUrlKallistiChange(Sender: TObject);
    procedure cbxUrlKallistiPortsChange(Sender: TObject);
    procedure cbxUrlRubyChange(Sender: TObject);
    procedure ckxDreamcastToolInternetProtocolUseARPChange(Sender: TObject);
    procedure edtDreamcastToolInternetProtocolAddressChange(Sender: TObject);
    procedure edtDreamcastToolInternetProtocolMACChange(Sender: TObject);
    procedure edtIdeCodeBlocksInstallDirChange(Sender: TObject);
    procedure edtPortMaintainerClick(Sender: TObject);
    procedure edtPortURLClick(Sender: TObject);
    procedure edtPortURLMouseEnter(Sender: TObject);
    procedure edtPortURLMouseLeave(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbxIdeAllClick(Sender: TObject);
    procedure lbxIdeListClickCheck(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure rbnComponentsNoChangeChange(Sender: TObject);
    procedure rgxDreamcastToolSelectionChanged(Sender: TObject);
    procedure rgxTerminalOptionClick(Sender: TObject);
    procedure tmDisplayKallistiPortsTimer(Sender: TObject);
    procedure tmrShellThreadTerminateTimer(Sender: TObject);
  private
    fPackageProfileKeysMapToolchains: TStringIntegerMap;
    fPackageProfileKeysMapGdb: TStringIntegerMap;
    fPackageManagerSelectedOfflinePackage: TPackageManagerRequestOffline;
    fPackageManagerOperation: TPackageManagerRequest;
    fLoadingConfiguration: Boolean;
    fKallistiPortsClearList: Boolean;
    fShellThreadExecutedAtLeastOnce: Boolean;
    fShellThreadSuccess: Boolean;
    fShellThreadInputRequest: TShellThreadInputRequest;
    fShellThreadOutputResult: TShellThreadOutputResponse;
    fShellThreadUpdateState: TUpdateOperationState;
    function CheckRepositoriesUrl: Boolean;
    function CheckComponentsChangeAllowRequestedOperation: Boolean;
    procedure DisplayEnvironmentComponentVersions;
    procedure DisplayKallistiPorts(ClearList: Boolean);
    procedure DoKallistiPortsClearList;
    procedure DoUpdateAll;
    function GetComponentSelectedOperation: TPackageManagerRequest;
    function GetSelectedDebugger: string;
    function GetSelectedKallistiPort: TKallistiPortItem;
    function GetSelectedKallistiPortItemIndex: Integer;
    function GetSelectedMediaAccessControlHostAddress: string;
    function GetSelectedSerialBaudrate: Integer;
    function GetSelectedSerialPort: Integer;
    function GetSelectedToolchain: string;
    procedure LoadConfiguration;
    function BooleanToCaption(Value: Boolean): string;
    function BooleanToCheckboxState(State: Boolean): TCheckBoxState;
    procedure ClearKallistiPortPanel;
    procedure DoComponentsClearLists;
    procedure SetSelectedSerialBaudrate(AValue: Integer);
    procedure UpdateKallistiPortControls;
    function IsVersionLabelValid(VersionLabel: TLabel): Boolean;
    procedure SetVersionLabelState(VersionLabel: TLabel; Erroneous: Boolean);
    procedure SetVersionLabel(VersionLabel: TLabel; Version: string);
    procedure UpdateComponentControls(const Initialize: Boolean = False);
    procedure UpdateDreamcastToolMediaAccessControlAddressControls;
    procedure UpdateDreamcastToolSerialOptionControls;
    procedure UpdateKallistiControls;
    procedure UpdateOptionsControls;
    procedure UpdateRepositories;
    procedure UpdateRubyControls;
    procedure UpdateWindowsTerminalControls;
    procedure InstallDreamcastTool;
    procedure HandleInvalidInternetProtocolAddress(const InvalidMaskFormat: Boolean);
    procedure HandleInvalidMediaAccessControlAddress(const InvalidMaskFormat: Boolean);
    procedure LoadRepositoriesSelectionList;
    procedure InitializeAboutScreen;
    procedure InitializeHomeScreen;
    procedure InitializeComponentsScreen;
    procedure InitializeIdeScreen;
    procedure InitializeOptionsScreen;
    procedure HandleAero;
    function GetMsgBoxWindowHandle: THandle;
    function GetAllKallistiPortsIcon(const Operation: TShellThreadInputRequest): TMsgDlgType;
    function GetAllKallistiPortsMessage(const Message: string): string;
    function CheckKallistiSinglePortPossibleInstallation: Boolean;
    procedure CreateNetworkAdapterList;
    procedure CreateSerialPortList;
    procedure FreeNetworkAdapterList;
    procedure FreeSerialPortList;
    function HostMacToItemIndex(const HostMediaAccessControlAddress: string): Integer;
    function SerialPortToItemIndex(const SerialPortIndex: Integer): Integer;
    function HasNetworkAdapters: Boolean;
    function HasSerialPorts: Boolean;
    procedure RefreshIdeScreen;
    procedure RunMSYS; overload;
    procedure RunMSYS(const WorkingDirectory: TFileName); overload;
    procedure AskForUpdate(const MandatoryAction: Boolean = False);
  protected
    function RunElevatedTask(const ATaskName: string): Boolean; overload;
    function RunElevatedTask(const ATaskName: string;
      AParameters: TStringList): Boolean; overload;
  public
    procedure RefreshViewDreamcastTool;
    procedure RefreshViewKallistiPorts(ForceRefresh: Boolean);
    procedure RefreshViewEnvironment(ForceRefresh: Boolean);
    procedure RefreshEverything(ForceRefresh: Boolean);
    procedure OnCommandTerminateThread(Sender: TObject;
      Request: TShellThreadInputRequest; Response: TShellThreadOutputResponse;
      Success: Boolean; UpdateState: TUpdateOperationState);
    procedure OnPackageManagerTerminate(Sender: TObject;
      const Success: Boolean; const Aborted: Boolean);
    function MsgBox(const aCaption: string; const aMsg: string;
      DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn): TModalResult; overload;
    function MsgBox(const aCaption: string; const aMsg: string;
      DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult; overload;
    property SelectedKallistiPortItemIndex: Integer
      read GetSelectedKallistiPortItemIndex;
    property SelectedKallistiPort: TKallistiPortItem
      read GetSelectedKallistiPort;
    property SelectedSerialPort: Integer
      read GetSelectedSerialPort;
    property SelectedSerialBaudrate: Integer
      read GetSelectedSerialBaudrate write SetSelectedSerialBaudrate;
    property SelectedHostMediaAccessControlAddress: string
      read GetSelectedMediaAccessControlHostAddress;
    property ComponentSelectedToolchain: string read
      GetSelectedToolchain;
    property ComponentSelectedDebugger: string read
      GetSelectedDebugger;
    property ComponentSelectedOperation: TPackageManagerRequest
      read GetComponentSelectedOperation;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
  LCLIntf,
  IniFiles,
  StrUtils,
  FPHttpClient,
  OpenSSLSockets,
  UITools,
  GetVer,
  PostInst,
  Settings,
  Version,
  VerIntf,
  About,
  UxTheme,
  MsgDlg,
  Progress,
  ModVer,
  InetUtil,
  RunTools,
  RefBase,
  Elevate,
  FSTools,
  Unpack,
  CBTools,
  EnumCom,
  WtTools,
  StrTools;

const
  KALLISTI_VERSION_FORMAT = '%s (%s)';
  UNKNOWN_VALUE = '(Unknown)';

  ELEVATED_TASK_CODEBLOCKS_IDE_INSTALL = 'elevated_task_cb_ide_install';
  ELEVATED_TASK_CODEBLOCKS_IDE_REINSTALL = 'elevated_task_cb_ide_reinstall';
  ELEVATED_TASK_CODEBLOCKS_IDE_UNINSTALL = 'elevated_task_cb_ide_uninstall';
  ELEVATED_TASK_CODEBLOCKS_IDE_REFRESH = 'elevated_task_cb_ide_refresh';
  ELEVATED_TASK_CODEBLOCKS_IDE_INITIALIZE_PROFILES = 'elevated_task_cb_ide_initialize';

type
  { TIntegerObject }
  TIntegerObject = class(TObject)
  private
    fValue: Integer;
  public
    property Value: Integer read fValue;
    constructor Create(AValue: Integer);
  end;

  { TNetworkAdapterListUserInterfaceItem }
  TNetworkAdapterListUserInterfaceItem = class(TObject)
  private
    fMacAddress: string;
  public
    constructor Create(AMacAddress: string);
    property MacAddress: string read fMacAddress;
  end;

  { TSerialPortListUserInterfaceItem }
  TSerialPortListUserInterfaceItem = class(TObject)
  private
    fSerialPortIndex: Integer;
  public
    constructor Create(ASerialPortIndex: Integer);
    property SerialPortIndex: Integer read fSerialPortIndex;
  end;

var
  HelpFileName: TFileName;
  ModuleVersionList: TModuleVersionList;
  FullVersionNumber: string;

function CreateModuleVersionList: TModuleVersionList;
var
  ModulesList: TStringList;

begin
  ModulesList := TStringList.Create;
  try
    ModulesList.Add(SELF_MODULE_REFERENCE);
    ModulesList.Add(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.LauncherExecutable);
    ModulesList.Add(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.RunnerExecutable);
    Result := TModuleVersionList.Create(ModulesList, UNKNOWN_VALUE);
  finally
    ModulesList.Free;
  end;
end;

{ TIntegerObject }

constructor TIntegerObject.Create(AValue: Integer);
begin
  fValue := AValue;
end;

{ TSerialPortListUserInterfaceItem }

constructor TSerialPortListUserInterfaceItem.Create(ASerialPortIndex: Integer);
begin
  fSerialPortIndex := ASerialPortIndex;
end;

{ TNetworkAdapterListUserInterfaceItem }

constructor TNetworkAdapterListUserInterfaceItem.Create(AMacAddress: string);
begin
  fMacAddress := AMacAddress;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IFDEF RELEASE}
  tsDebug.TabVisible := False;
{$ENDIF}

  (* Check if we are running this instance in elevated task mode... if yes,
     just exits all this UI code. *)
  if IsElevatedTaskRequested then
    Exit;

  // Initialize the main singleton objects
  fPackageProfileKeysMapToolchains := TStringIntegerMap.Create;
  fPackageProfileKeysMapGdb := TStringIntegerMap.Create;
  fShellThreadExecutedAtLeastOnce := False;
  GlobalInitialization;
  PackageManager.OnTerminate := @OnPackageManagerTerminate;
  HelpFileName := DreamcastSoftwareDevelopmentKitManager.Environment
    .FileSystem.Shell.HelpFileName;

  (* If we are not running in post-install mode (i.e., froml DreamSDK Setup)
     then get some environment information that will be used in the app *)
  if (not IsPostInstallMode) then
  begin
    ModuleVersionList := CreateModuleVersionList;
    CreateNetworkAdapterList;
    CreateSerialPortList;
  end;

  // Basic initialization of the UI
  DoubleBuffered := True;
  pcMain.TabIndex := 0;

  // Handle main window title
  if IsWindowsVistaOrGreater and IsElevated then
    Caption := Format(ElevatedCaption, [Caption]);
  Application.Title := Caption;

  // Properly handle Aero on Windows
  HandleAero;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fPackageProfileKeysMapGdb.Free;
  fPackageProfileKeysMapToolchains.Free;

  DoKallistiPortsClearList;
  DoComponentsClearLists;

  if not IsElevatedTaskRequested then
  begin
    if not fShellThreadExecutedAtLeastOnce then
    begin
      DreamcastSoftwareDevelopmentKitManager.KallistiPorts
        .GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
    end;

    if (not IsPostInstallMode) then
    begin
      FreeSerialPortList;
      FreeNetworkAdapterList;
      ModuleVersionList.Free;
    end;

    GlobalFinalization;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if not IsElevatedTaskRequested then
  begin
    Screen.Cursor := crHourGlass;
    InitializeHomeScreen;
    InitializeAboutScreen;
    InitializeComponentsScreen;
    InitializeOptionsScreen;
    InitializeIdeScreen;
    fLoadingConfiguration := True;
    LoadRepositoriesSelectionList;
    LoadConfiguration;
    RefreshEverything(False);
    DisplayKallistiPorts(True);
    fLoadingConfiguration := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.gbxIdeAllClick(Sender: TObject);
begin

end;

procedure TfrmMain.lbxIdeListClickCheck(Sender: TObject);
begin
  RefreshIdeScreen;
end;

procedure TfrmMain.lbxPortsClickCheck(Sender: TObject);
begin
  if Assigned(SelectedKallistiPort) then
    lbxPorts.State[lbxPorts.ItemIndex] :=
      BooleanToCheckboxState(SelectedKallistiPort.Installed);
end;

procedure TfrmMain.lbxPortsSelectionChange(Sender: TObject; User: Boolean);
begin
  if Assigned(SelectedKallistiPort) and User then
  begin
    lblPortName.Caption := SelectedKallistiPort.Name;
    edtPortVersion.Text := SelectedKallistiPort.Version;
    edtPortLicense.Text := SelectedKallistiPort.License;
    edtPortMaintainer.Text := SelectedKallistiPort.Maintainer;
    memPortShortDescription.Text := SelectedKallistiPort.ShortDescription;
    edtPortURL.Text := SelectedKallistiPort.URL;
    memPortDescription.Text := SelectedKallistiPort.Description;
    UpdateKallistiPortControls;
{$IFDEF DEBUG}
    DebugLog(
      sLineBreak +
      SelectedKallistiPort.Name + ': ' + sLineBreak +
      '  Installation Directory (Exists: ' + BoolToStr(DirectoryExists(SelectedKallistiPort.InstallDirectory), 'Yes', 'No') + '): '
        + SelectedKallistiPort.InstallDirectory + sLineBreak +
      '  Use SVN: ' + BoolToStr(SelectedKallistiPort.UseSubversion, 'Yes', 'No') + sLineBreak +
      '  Includes: ' + SelectedKallistiPort.Includes + sLineBreak +
      '  Include Directories: ' + SelectedKallistiPort.IncludeDirectory + sLineBreak +
      '  Libraries: ' + SelectedKallistiPort.Libraries + sLineBreak +
      '    Weights: ' + SelectedKallistiPort.LibraryWeights + sLineBreak +
      '  Usable in IDE: ' + BoolToStr(SelectedKallistiPort.UsableWithinIDE, 'Yes', 'No') + sLineBreak
    );
{$ENDIF}
  end;
end;

procedure TfrmMain.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsOptions then
    UpdateOptionsControls;
  if pcMain.ActivePage = tsKallistiOS then
    UpdateKallistiControls;

  if pcMain.ActivePage <> tsComponents then
    UpdateComponentControls;
end;

procedure TfrmMain.pcMainChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := CheckComponentsChangeAllowRequestedOperation;
end;

procedure TfrmMain.rbnComponentsNoChangeChange(Sender: TObject);
begin
  fPackageManagerOperation := TPackageManagerRequest((Sender as TRadioButton).Tag);

{$IFDEF DEBUG}
  WriteLn('Package Manager Operation: ', fPackageManagerOperation);
{$ENDIF}

  // Update controls
  btnComponentsApply.Enabled := fPackageManagerOperation <> pmrUndefined;
  lblToolchain.Enabled := fPackageManagerOperation = pmrToolchain;
  cbxToolchain.Enabled := lblToolchain.Enabled;
  lblDebugger.Enabled := fPackageManagerOperation <> pmrUndefined;
  cbxDebugger.Enabled := lblDebugger.Enabled;
end;

procedure TfrmMain.rgxDreamcastToolSelectionChanged(Sender: TObject);
begin
  InstallDreamcastTool;
  RefreshViewDreamcastTool;
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
  UpdateWindowsTerminalControls;
end;

procedure TfrmMain.tmDisplayKallistiPortsTimer(Sender: TObject);
var
  i, j, SavedIndex: Integer;
  PortInfo: TKallistiPortItem;
  HasKallistiPorts: Boolean;

begin
  tmDisplayKallistiPorts.Enabled := False;

  if fKallistiPortsClearList then
  begin
    // Reset the Ports view

    SavedIndex := lbxPorts.ItemIndex;

    // Clear all objects in lbxPorts
    DoKallistiPortsClearList;

    // Fill lbxPorts again
    for i := 0 to DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Count - 1 do
    begin
      PortInfo := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[i];
      if not PortInfo.Hidden then
      begin
        j := lbxPorts.Items.Add(PortInfo.Name);
        lbxPorts.Items.Objects[j] :=  TIntegerObject.Create(i);
        if PortInfo.Installed then
          lbxPorts.State[j] := cbGrayed;
      end;
      Application.ProcessMessages;
    end;

    HasKallistiPorts := (lbxPorts.Count > 0);
    if HasKallistiPorts then
    begin
      if (SavedIndex > -1) and (SavedIndex < lbxPorts.Items.Count - 1) then
        lbxPorts.ItemIndex := SavedIndex
      else
        lbxPorts.ItemIndex := 0;
      lbxPortsSelectionChange(Self, True);
    end
    else
      ClearKallistiPortPanel;

  end
  else
  begin
    // Just refresh the current view

    for i := 0 to lbxPorts.Items.Count - 1 do
    begin
      j := TIntegerObject(lbxPorts.Items.Objects[i]).Value;
      PortInfo := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[j];
      if PortInfo.Installed then
        lbxPorts.State[i] := cbGrayed
      else
        lbxPorts.State[i] := cbUnchecked;
      Application.ProcessMessages;
    end;

    UpdateKallistiPortControls;
  end;
end;

procedure TfrmMain.tmrShellThreadTerminateTimer(Sender: TObject);
var
  LogContext: TLogMessageContext;
  IsSinglePortRefreshOnly: Boolean;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    LogMessage(LogContext, Format('ShellThreadSuccess: %s, ShellThreadOutputResult: %d', [
      BoolToStr(fShellThreadSuccess, True),
      fShellThreadOutputResult
    ]));

    tmrShellThreadTerminate.Enabled := False;

    if fShellThreadSuccess then
    begin
      case fShellThreadOutputResult of
        // No action was done
        stoNothing:
          MsgBox(DialogInformationTitle, Format(UpdateProcessUpdateUselessText, [EverythingText]), mtInformation, [mbOk]);

        // KallistiOS was installed
        stoKallistiInstall:
          MsgBox(DialogInformationTitle, Format(UpdateProcessInstallSuccessText, [KallistiText]), mtInformation, [mbOk]);

        // KallistiOS, KallistiOS Ports, Dreamcast Tool or Ruby were updated
        stoKallistiUpdate:
          case fShellThreadUpdateState of
            uosUpdateSuccess:
              MsgBox(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [EverythingText]), mtInformation, [mbOk]);
          end;

        // All KallistiOS Ports were installed
        stoKallistiPortsInstall:
          MsgBox(DialogInformationTitle, UpdateProcessAllKallistiPortsInstalled, mtInformation, [mbOk]);

        // All KallistiOS Ports were uninstalled
        stoKallistiPortsUninstall:
          MsgBox(DialogInformationTitle, UpdateProcessAllKallistiPortsUninstalled, mtInformation, [mbOk]);

        // A single KallistiOS Port was updated
        stoKallistiSinglePortUpdate:
          case fShellThreadUpdateState of
            uosUpdateSuccess:
              MsgBox(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk]);
            uosUpdateUseless:
              MsgBox(DialogInformationTitle, Format(UpdateProcessUpdateUselessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk]);
          end;

        // A single KallistiOS Port was installed
        stoKallistiSinglePortInstall:
          if not DreamcastSoftwareDevelopmentKitManager.Environment.Settings.ProgressWindowAutoClose then
            MsgBox(DialogInformationTitle, Format(UpdateProcessInstallSuccessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk]);

        // A single KallistiOS Port was uninstalled
        stoKallistiSinglePortUninstall:
          if not DreamcastSoftwareDevelopmentKitManager.Environment.Settings.ProgressWindowAutoClose then
            MsgBox(DialogInformationTitle, Format(UpdateProcessUninstallSuccessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk]);
      end;

      // Handle IDE files
      DreamcastSoftwareDevelopmentKitManager.KallistiPorts
        .GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
    end;

    if not IsPostInstallMode then
    begin
      IsSinglePortRefreshOnly := (fShellThreadOutputResult = stoKallistiSinglePortInstall)
        or (fShellThreadOutputResult = stoKallistiSinglePortUpdate)
        or (fShellThreadOutputResult = stoKallistiSinglePortUninstall);

      if IsSinglePortRefreshOnly then
        RefreshViewKallistiPorts(False) // Single KallistiPorts change
      else
        RefreshEverything(True);
    end;

    Delay(250);
    Screen.Cursor := crDefault;
    fShellThreadExecutedAtLeastOnce := True;

  finally
    LogMessageExit(LogContext);
  end;
end;

function TfrmMain.CheckRepositoriesUrl: Boolean;

  function CheckRepositoryUrl(ComboBox: TComboBox): Boolean;
  begin
    Result := Assigned(ComboBox) and
      (not ComboBox.Enabled or (ComboBox.Enabled and not IsEmpty(ComboBox.Text)));
  end;

begin
  Result := True;

  // Normal repositories
  Result := CheckRepositoryUrl(cbxUrlKallisti)
    and CheckRepositoryUrl(cbxUrlKallistiPorts)
    and CheckRepositoryUrl(cbxUrlDreamcastToolSerial)
    and CheckRepositoryUrl(cbxUrlDreamcastToolIP);

  if not Result then
  begin
    MsgBox(DialogWarningTitle, PleaseVerifyRepositories, mtWarning, [mbOK]);
    pcMain.ActivePage := tsOptions;
  end;

  // Ruby
  if Result and DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Ruby.Enabled then
  begin
    Result := CheckRepositoryUrl(cbxUrlRuby);
    if not Result then
    begin
      MsgBox(DialogWarningTitle, PleaseVerifyRubyRepository, mtWarning, [mbOK]);
      pcMain.ActivePage := tsRuby;
    end;
  end;
end;

function TfrmMain.CheckComponentsChangeAllowRequestedOperation: Boolean;
begin
  Result := True;
  if (pcMain.ActivePage = tsComponents)
    and (fPackageManagerOperation <> pmrUndefined) then
  begin
    Result := MsgBox(DialogQuestionTitle,
      MsgBoxDlgTranslateString(ConfirmComponentsChangePendingCancel),
      mtConfirmation, mbOkCancel, mbCancel) = mrOk;
  end;
end;

procedure TfrmMain.DisplayEnvironmentComponentVersions;
var
  ComponentName: TComponentName;
  ComponentVersion,
  ComponentNameString: string;

  procedure SetLabelDate(LabelCtrl: TLabel; const FileName: TFileName;
    const FileDate: TDateTime; const DisplayTime: Boolean = True);
  var
    RequestedDateFormat: string;

  begin
    RequestedDateFormat := STRING_DATE_FORMAT;
    if not DisplayTime then
      RequestedDateFormat := STRING_DATE_FORMAT_SHORT;
    LabelCtrl.Caption := EmptyStr;
    if FileExists(FileName) then
      LabelCtrl.Caption := FormatDateTime(RequestedDateFormat, FileDate);
  end;

begin
  with DreamcastSoftwareDevelopmentKitManager do
  begin
    // Components versions
{$IFDEF DEBUG}
    DebugLog('Displaying Components version using Labels');
{$ENDIF}
    for ComponentName := Low(TComponentName) to High(TComponentName) do
    begin
      ComponentNameString := ComponentNameToString(ComponentName);
      ComponentVersion := Versions.GetComponentVersion(ComponentName);
      SetVersionLabel(FindComponent('lblVersion' + ComponentNameString) as TLabel,
        ComponentVersion);
    end;

    // KallistiOS version in the KallistiOS tab
    SetVersionLabel(lblVersionChangeLogKallistiOS, INVALID_VERSION);
    SetVersionLabel(lblVersionKallistiOS2, INVALID_VERSION);
    if KallistiOS.Built then
    begin
      // Update the version label in the Components tab
      SetVersionLabel(lblVersionKallistiOS2, lblVersionKallistiOS.Caption);

      // Display the version extracted from Change Log
      if IsVersionValid(Versions.KallistiChangeLog) then
        SetVersionLabel(lblVersionChangeLogKallistiOS, Versions.KallistiChangeLog);

      // Show the complete version in the KallistiOS tab
      ComponentVersion := Format(KALLISTI_VERSION_FORMAT, [
        lblVersionKallistiOS.Caption,
        KallistiOS.Repository.Version
      ]);
      SetVersionLabel(lblVersionKallistiOS, ComponentVersion);
    end;

    // KallistiOS build date
    SetLabelDate(lblBuildDateKallistiOS,
      Environment.FileSystem.Kallisti.KallistiLibrary,
      Versions.KallistiBuildDate);

    // KallistiOS changes log display
    memKallistiChangeLog.Lines.Clear;
    if FileExists(Environment.FileSystem.Kallisti.KallistiChangeLogFile) then
      memKallistiChangeLog.Lines.LoadFromFile(
        Environment.FileSystem.Kallisti.KallistiChangeLogFile);
    memKallistiChangeLog.Enabled := (memKallistiChangeLog.Lines.Count > 0);

    // KallistiOS
    SetVersionLabel(lblVersionRepoKallistiOS, KallistiOS.Repository.Version);

    // KallistiOS Ports
    SetVersionLabel(lblVersionRepoKallistiPorts, KallistiPorts.Repository.Version);

    // Dreamcast Tool Serial
    SetVersionLabel(lblVersionRepoToolSerial, DreamcastTool.RepositorySerial.Version);

    // Dreamcast Tool IP
    SetVersionLabel(lblVersionRepoToolIP, DreamcastTool.RepositoryInternetProtocol.Version);

    // Ruby
    SetVersionLabel(lblVersionMRuby, Ruby.Repository.Version);

    // Ruby build date
    SetLabelDate(lblBuildDateMRuby,
      Environment.FileSystem.Ruby.RubyLibrary,
      Versions.MRubyBuildDate);

    // SuperH toolchains build date
    SetLabelDate(lblBuildDateToolchain,
      Environment.FileSystem.ToolchainSuperH.GCCExecutable,
      Versions.ToolchainSuperH.BuildDate, False);

    // ARM toolchains build date
    SetLabelDate(lblBuildDateToolchainARM,
      Environment.FileSystem.ToolchainARM.GCCExecutable,
      Versions.ToolchainARM.BuildDate, False);
  end;
end;

procedure TfrmMain.DisplayKallistiPorts(ClearList: Boolean);
begin
  fKallistiPortsClearList := ClearList;
  tmDisplayKallistiPorts.Enabled := True;
end;

procedure TfrmMain.DoKallistiPortsClearList;
var
  i: Integer;

begin
  for i := 0 to lbxPorts.Items.Count - 1 do
    if Assigned(lbxPorts.Items.Objects[i]) then
      lbxPorts.Items.Objects[i].Free;
  lbxPorts.Clear;
end;

procedure TfrmMain.DoUpdateAll;
begin
  if CheckRepositoriesUrl then
    ExecuteThreadOperation(stiKallistiManage);
end;

function TfrmMain.GetComponentSelectedOperation: TPackageManagerRequest;
begin
  Result := fPackageManagerOperation;
end;

function TfrmMain.GetSelectedDebugger: string;
var
  ItemIndex: Integer;
  GdbProfile: TGdbProfileInfo;

begin
  Result := EmptyStr;

  ItemIndex := TIntegerObject(cbxDebugger.Items.Objects[cbxDebugger.ItemIndex]).Value;
  if cbxDebugger.ItemIndex <> -1 then
  begin
    GdbProfile := DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem
      .Packages.GdbProfiles[ItemIndex];
    if Assigned(GdbProfile) then
      Result := GdbProfile.ProfileKey;
  end;
end;

function TfrmMain.GetSelectedKallistiPort: TKallistiPortItem;
var
  Index: Integer;

begin
  Result := nil;
  Index := SelectedKallistiPortItemIndex;
  if Index <> -1 then
    Result := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[Index];
end;

function TfrmMain.GetSelectedKallistiPortItemIndex: Integer;
begin
  Result := lbxPorts.ItemIndex;
  if Result <> -1 then
    Result := TIntegerObject(lbxPorts.Items.Objects[Result]).Value;
end;

function TfrmMain.GetSelectedMediaAccessControlHostAddress: string;
var
  SelectedItem: TNetworkAdapterListUserInterfaceItem;

begin
  Result := EmptyStr;
  with cbxDreamcastToolInternetProtocolNetworkAdapter do
    if ItemIndex <> -1 then
    begin
      SelectedItem := TNetworkAdapterListUserInterfaceItem(Items.Objects[ItemIndex]);
      if Assigned(SelectedItem) then
        Result := SelectedItem.MacAddress;
    end;
end;

function TfrmMain.GetSelectedSerialBaudrate: Integer;
begin
  if cbxDreamcastToolSerialBaudrate.ItemIndex = 0 then
    Result := StrToIntDef(edtDreamcastToolSerialBaudrateCustom.Text, 0)
  else
    Result := StrToIntDef(cbxDreamcastToolSerialBaudrate.Text, 0);
end;

function TfrmMain.GetSelectedSerialPort: Integer;
var
  SelectedItem: TSerialPortListUserInterfaceItem;

begin
  Result := -1;
  with cbxDreamcastToolSerialPort do
    if ItemIndex <> -1 then
    begin
      SelectedItem := TSerialPortListUserInterfaceItem(Items.Objects[ItemIndex]);
      if Assigned(SelectedItem) then
        Result := SelectedItem.SerialPortIndex;
    end;
end;

function TfrmMain.GetSelectedToolchain: string;
var
  ItemIndex: Integer;
  ToolchainProfile: TToolchainProfileInfo;

begin
  Result := EmptyStr;

  ItemIndex := TIntegerObject(cbxToolchain.Items.Objects[cbxToolchain.ItemIndex]).Value;
  if cbxToolchain.ItemIndex <> -1 then
  begin
    ToolchainProfile := DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem
      .Packages.ToolchainProfiles[ItemIndex];
    if Assigned(ToolchainProfile) then
      Result := ToolchainProfile.ProfileKey;
  end;
end;

procedure TfrmMain.LoadConfiguration;
begin
  with DreamcastSoftwareDevelopmentKitManager.Environment.Settings do
  begin
    // Settings
    if UseMinTTY then
      rgxTerminalOption.ItemIndex := 1;

    // Dreamcast Tool
    cbxDreamcastToolSerialPort.ItemIndex := SerialPortToItemIndex(DreamcastTool.SerialPort);
    SelectedSerialBaudrate := DreamcastTool.SerialBaudrate;
    ckxDreamcastToolSerialAlternateBaudrate.Checked := DreamcastTool.SerialBaudrateAlternate;
    ckxDreamcastToolSerialExternalClock.Checked := DreamcastTool.SerialExternalClock;
    ckxDreamcastToolSerialDumbTerminal.Checked := DreamcastTool.SerialDumbTerminal;
    edtDreamcastToolInternetProtocolAddress.Text := DreamcastTool.InternetProtocolAddress;
    ckxDreamcastToolAttachConsoleFileServer.Checked := DreamcastTool.AttachConsoleFileserver;
    ckxDreamcastToolClearScreenBeforeDownload.Checked := DreamcastTool.ClearScreenBeforeDownload;
    ckxDreamcastToolInternetProtocolUseARP.Checked := DreamcastTool.MediaAccessControlEnabled;
    edtDreamcastToolInternetProtocolMAC.Text := DreamcastTool.MediaAccessControlAddress;
    cbxDreamcastToolInternetProtocolNetworkAdapter.ItemIndex :=
      HostMacToItemIndex(DreamcastTool.HostMediaAccessControlAddress);
    edtDreamcastToolCustomExecutable.Text := DreamcastTool.CustomExecutable;
    edtDreamcastToolCustomArguments.Text := DreamcastTool.CustomArguments;
    rgxDreamcastTool.ItemIndex := Integer(DreamcastTool.Kind);
  end;
end;

function TfrmMain.BooleanToCaption(Value: Boolean): string;
begin
  Result := UserInterfaceNotInstalledText;
  if Value then
    Result := UserInterfaceInstalledText;
end;

function TfrmMain.BooleanToCheckboxState(State: Boolean): TCheckBoxState;
begin
  Result := cbUnchecked;
  if State then
    Result := cbGrayed;
end;

procedure TfrmMain.ClearKallistiPortPanel;
begin
  lblPortName.Caption := '';
  edtPortVersion.Clear;
  edtPortLicense.Clear;
  edtPortMaintainer.Clear;
  memPortShortDescription.Clear;
  edtPortURL.Clear;
  memPortDescription.Clear;
  btnPortInstall.Enabled := False;
  btnPortUninstall.Enabled := False;
  btnPortUpdate.Enabled := False;
end;

procedure TfrmMain.DoComponentsClearLists;
var
  i: Integer;

begin
  // Clear Debugger Components
  for i := 0 to cbxDebugger.Items.Count - 1 do
    cbxDebugger.Items.Objects[i].Free;
  cbxDebugger.Clear;

  // Clear Toolchain Components
  for i := 0 to cbxToolchain.Items.Count - 1 do
    cbxToolchain.Items.Objects[i].Free;
  cbxToolchain.Clear;
end;

procedure TfrmMain.SetSelectedSerialBaudrate(AValue: Integer);
var
  ItemIndex: Integer;
  Baudrate: string;

begin
  // Set by default to 115200 if an invalid value has been passed...
  if (AValue < DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_MINIMAL_ALLOWED) then
    AValue := DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE_ALLOWED;

  // Select a value in the list if possible, if not, switch to Custom
  Baudrate := IntToStr(AValue);
  ItemIndex := cbxDreamcastToolSerialBaudrate.Items.IndexOf(Baudrate);
  if ItemIndex <> -1 then
    cbxDreamcastToolSerialBaudrate.ItemIndex := ItemIndex
  else
  begin
    cbxDreamcastToolSerialBaudrate.ItemIndex := 0;
    edtDreamcastToolSerialBaudrateCustom.Text := Baudrate;
  end;
end;

procedure TfrmMain.UpdateKallistiPortControls;
begin
  if Assigned(SelectedKallistiPort) then
  begin
    btnPortInstall.Enabled := not SelectedKallistiPort.Installed;
    btnPortUninstall.Enabled := SelectedKallistiPort.Installed;
    btnPortUpdate.Enabled := SelectedKallistiPort.Installed and
      (not DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Repository.Offline);
  end
  else
  begin
    btnPortInstall.Enabled := False;
    btnPortUninstall.Enabled := False;
    btnPortUpdate.Enabled := False;
  end;
end;

function TfrmMain.IsVersionLabelValid(VersionLabel: TLabel): Boolean;
begin
  Result := (VersionLabel.Caption <> UserInterfaceNotInstalledText)
    and (VersionLabel.Caption <> EmptyStr) and IsVersionValid(VersionLabel.Caption);
end;

procedure TfrmMain.SetVersionLabelState(VersionLabel: TLabel; Erroneous: Boolean);
const
  NOT_CRITICAL_COMPONENTS: array[0..6] of string = (
    'lblVersionPythonGDB',
    'lblVersionGit',
    'lblVersionPython',
    'lblVersionRuby',
    'lblVersionMRuby',
    'lblVersionCMake',
    'lblVersionMeson'
  );

var
  LabelFontColor: TColor;

begin
  if Erroneous then
  begin
    LabelFontColor := clRed;
    if IsInArray(NOT_CRITICAL_COMPONENTS, VersionLabel.Name) then
      LabelFontColor := clHighlight;
    VersionLabel.Font.Color := LabelFontColor;
    VersionLabel.Font.Style := [fsBold];
  end
  else
  begin
    VersionLabel.Font.Color := clDefault;
    VersionLabel.Font.Style := [];
  end;
end;

procedure TfrmMain.SetVersionLabel(VersionLabel: TLabel; Version: string);
var
  ValidVersion: Boolean;

begin
  if Assigned(VersionLabel) then
  begin
    ValidVersion := IsVersionValid(Version);
    if not ValidVersion then
      VersionLabel.Caption := BooleanToCaption(False)
    else
      VersionLabel.Caption := Version;
    SetVersionLabelState(VersionLabel, not ValidVersion);
  end;

{$IFDEF DEBUG}
  if not Assigned(VersionLabel) then
    DebugLog('  (NOT ASSIGNED UI LABEL): "' + Version + '"')
  else
    DebugLog('  ' + VersionLabel.Name + ': "' + VersionLabel.Caption + '"');
{$ENDIF}
end;

procedure TfrmMain.UpdateComponentControls(const Initialize: Boolean);
var
  i: Integer;

begin
  rbnComponentsNoChange.Checked := True;

  with DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem do
  begin
    // Fill the lists
    if Initialize then
    begin
      // Clear Debugger/Toolchain drop-down lists
      DoComponentsClearLists;

      // Debugger
      for i := 0 to Packages.GetGdbProfileCount - 1 do
        fPackageProfileKeysMapGdb.Add(Packages.GdbProfiles[i].ProfileKey,
          cbxDebugger.Items.AddObject(Packages.GdbProfiles[i].Name, TIntegerObject.Create(i))
        );

      // Toolchain
      for i := 0 to Packages.GetToolchainProfileCount - 1 do
        fPackageProfileKeysMapToolchains.Add(Packages.ToolchainProfiles[i].ProfileKey,
          cbxToolchain.Items.AddObject(Packages.ToolchainProfiles[i].Name, TIntegerObject.Create(i))
        );
    end;

    // Select the correct items in the drop downs

    // Debugger
    i := fPackageProfileKeysMapGdb.IndexOf(DreamcastSoftwareDevelopmentKitManager
      .Versions.ToolchainSuperH.PackageProfileGDB);
    cbxDebugger.ItemIndex := i;

    // Toolchain
    i := fPackageProfileKeysMapToolchains.IndexOf(DreamcastSoftwareDevelopmentKitManager
      .Versions.ToolchainSuperH.PackageProfileToolchain);
    cbxToolchain.ItemIndex := i;
  end;
end;

procedure TfrmMain.UpdateDreamcastToolMediaAccessControlAddressControls;
var
  EnabledControls: Boolean;

begin
  EnabledControls := (gbxDreamcastToolInternetProtocol.Enabled) and
    (ckxDreamcastToolInternetProtocolUseARP.Checked);
  edtDreamcastToolInternetProtocolMAC.Enabled := EnabledControls;
  lblDreamcastToolInternetProtocolInvalidMAC.Enabled := EnabledControls;
  lblDreamcastToolInternetProtocolMAC.Enabled := EnabledControls;
  if HasNetworkAdapters then
  begin
    cbxDreamcastToolInternetProtocolNetworkAdapter.Enabled := EnabledControls;
    lblDreamcastToolInternetProtocolNetworkAdapter.Enabled := EnabledControls;
  end;
end;

procedure TfrmMain.RefreshViewDreamcastTool;
var
  Kind: Integer;

begin
  Kind := rgxDreamcastTool.ItemIndex;
  gbxDreamcastToolSerial.Enabled := (Kind = 1);
  gbxDreamcastToolInternetProtocol.Enabled := (Kind = 2);
  gbxDreamcastToolCommon.Enabled := (Kind = 1) or (Kind = 2);
  gbxDreamcastToolCustomCommand.Enabled := (Kind = 3);
  UpdateDreamcastToolSerialOptionControls;
  UpdateDreamcastToolMediaAccessControlAddressControls;
end;

procedure TfrmMain.UpdateDreamcastToolSerialOptionControls;
begin
  edtDreamcastToolSerialBaudrateCustom.Enabled := (cbxDreamcastToolSerialBaudrate.ItemIndex = 0);
  ckxDreamcastToolSerialAlternateBaudrate.Enabled := (gbxDreamcastToolSerial.Enabled)
    and (
         (SelectedSerialBaudrate >= DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE_ALLOWED)
      or edtDreamcastToolSerialBaudrateCustom.Enabled
    );
  with edtDreamcastToolSerialBaudrateCustom do
    if not Focused and CanSetFocus then
    begin
      SetFocus;
      SelectAll;
    end;
end;

procedure TfrmMain.UpdateKallistiControls;
begin
  with DreamcastSoftwareDevelopmentKitManager do
  begin
    if KallistiOS.Repository.Offline then
    begin
      btnUpdateKallistiOS.Caption := KallistiMainButtonRebuild;
      lblOfflineStatusKallistiOS.Caption := RepositoryOfflineStatusOffline;
    end
    else
    begin
      btnUpdateKallistiOS.Caption := KallistiMainButtonUpdate;
      lblOfflineStatusKallistiOS.Caption := RepositoryOfflineStatusOnline;
    end;
  end;
end;

procedure TfrmMain.UpdateOptionsControls;
begin
  with DreamcastSoftwareDevelopmentKitManager do
  begin
    cbxUrlKallisti.Enabled := (not KallistiOS.Repository.Ready)
      and (not KallistiOS.Repository.Offline);
    btnUrlKallisti.Enabled := not cbxUrlKallisti.Enabled;
    btnOfflineKallisti.Enabled := not KallistiOS.Repository.Offline;
    if KallistiOS.Repository.Offline then
      cbxUrlKallisti.Text := EmptyStr;

    cbxUrlKallistiPorts.Enabled := (not KallistiPorts.Repository.Ready)
      and (not KallistiPorts.Repository.Offline);
    btnUrlKallistiPorts.Enabled := not cbxUrlKallistiPorts.Enabled;
    btnOfflineKallistiPorts.Enabled := not KallistiPorts.Repository.Offline;
    if KallistiPorts.Repository.Offline then
      cbxUrlKallistiPorts.Text := EmptyStr;

    cbxUrlDreamcastToolSerial.Enabled := (not DreamcastTool.RepositorySerial.Ready)
      and (not DreamcastTool.RepositorySerial.Offline);
    btnUrlDreamcastToolSerial.Enabled := not cbxUrlDreamcastToolSerial.Enabled;
    btnOfflineDreamcastToolSerial.Enabled := not DreamcastTool.RepositorySerial.Offline;
    if DreamcastTool.RepositorySerial.Offline then
      cbxUrlDreamcastToolSerial.Text := EmptyStr;

    cbxUrlDreamcastToolIP.Enabled := (not DreamcastTool.RepositoryInternetProtocol.Ready)
      and (not DreamcastTool.RepositoryInternetProtocol.Offline);
    btnUrlDreamcastToolIP.Enabled := not cbxUrlDreamcastToolIP.Enabled;
    btnOfflineDreamcastToolIP.Enabled := not DreamcastTool.RepositoryInternetProtocol.Offline;
    if DreamcastTool.RepositoryInternetProtocol.Offline then
      cbxUrlDreamcastToolIP.Text := EmptyStr;

    cbxUrlRuby.Enabled := (not Ruby.Repository.Ready)
      and (not Ruby.Repository.Offline);
    btnUrlRuby.Enabled := not cbxUrlRuby.Enabled;
    btnOfflineRuby.Enabled := not Ruby.Repository.Offline;
    if Ruby.Repository.Offline then
      cbxUrlRuby.Text := EmptyStr;
  end;
  UpdateWindowsTerminalControls;
end;

procedure TfrmMain.UpdateRepositories;
begin
  cbxUrlKallistiChange(Self);
  cbxUrlKallistiPortsChange(Self);
  cbxUrlDreamcastToolSerialChange(Self);
  cbxUrlDreamcastToolIPChange(Self);
  cbxUrlRubyChange(Self);
end;

procedure TfrmMain.UpdateRubyControls;
begin
  with DreamcastSoftwareDevelopmentKitManager do
  begin
    btnInstallMRuby.Enabled := not Ruby.Built;
    btnUpdateMRuby.Enabled := Ruby.Built;
    btnUninstallMRuby.Enabled := Ruby.Built;
    btnRubyOpenHome.Enabled := Ruby.Built;
    btnRubyOpenMSYS.Enabled := Ruby.Built;
  end;
end;

procedure TfrmMain.UpdateWindowsTerminalControls;
var
  TerminalVersion: string;

begin
  TerminalVersion := TerminalOptionsWindowsPrompt;
  if IsWindowsTerminalInstalled then
    TerminalVersion := TerminalOptionsWindowsTerminal;
  rgxTerminalOption.Items[0] := Format(rgxTerminalOption.Items[0], [TerminalVersion]);

  btnWindowsTerminalInstall.Enabled := IsWindowsTerminalInstalled
    and (not IsWindowsTerminalIntegrationInstalled);
  btnWindowsTerminalUninstall.Enabled := IsWindowsTerminalInstalled
    and IsWindowsTerminalIntegrationInstalled;
end;

procedure TfrmMain.InstallDreamcastTool;
begin
  if not fLoadingConfiguration then
  begin
    with DreamcastSoftwareDevelopmentKitManager.Environment.Settings.DreamcastTool do
    begin
      AttachConsoleFileserver := ckxDreamcastToolAttachConsoleFileServer.Checked;
      ClearScreenBeforeDownload := ckxDreamcastToolClearScreenBeforeDownload.Checked;
      Kind := TDreamcastToolKind(rgxDreamcastTool.ItemIndex);
      SerialBaudrate := SelectedSerialBaudrate;
      SerialBaudrateAlternate := ckxDreamcastToolSerialAlternateBaudrate.Checked;
      SerialDumbTerminal := ckxDreamcastToolSerialDumbTerminal.Checked;
      SerialPort := SelectedSerialPort;
      SerialExternalClock := ckxDreamcastToolSerialExternalClock.Checked;
      InternetProtocolAddress := edtDreamcastToolInternetProtocolAddress.Text;
      MediaAccessControlEnabled := ckxDreamcastToolInternetProtocolUseARP.Checked;
      MediaAccessControlAddress := SanitizeMediaAccessControlAddress(
        edtDreamcastToolInternetProtocolMAC.Text);
      HostMediaAccessControlAddress := SelectedHostMediaAccessControlAddress;
      CustomExecutable := edtDreamcastToolCustomExecutable.Text;
      CustomArguments := edtDreamcastToolCustomArguments.Text;
    end;
    DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
    DreamcastSoftwareDevelopmentKitManager.DreamcastTool.Install;
  end;
end;

procedure TfrmMain.HandleInvalidInternetProtocolAddress(const InvalidMaskFormat: Boolean);
var
  InvalidValue: Boolean;

begin
  InvalidValue := not IsValidInternetProtocolAddress(edtDreamcastToolInternetProtocolAddress.Text);
  if InvalidMaskFormat then
    lblDreamcastToolInternetProtocolInvalidAddress.Caption := InvalidInternetProtocolAddressFormat
  else
    lblDreamcastToolInternetProtocolInvalidAddress.Caption := InvalidInternetProtocolAddressValue;
  lblDreamcastToolInternetProtocolInvalidAddress.Visible := InvalidMaskFormat or InvalidValue;
end;

procedure TfrmMain.HandleInvalidMediaAccessControlAddress(
  const InvalidMaskFormat: Boolean);
var
  InvalidValue: Boolean;

begin
  InvalidValue := not IsValidMediaAccessControlAddress(edtDreamcastToolInternetProtocolMAC.Text);
  if InvalidMaskFormat then
    lblDreamcastToolInternetProtocolInvalidMAC.Caption := InvalidMediaAccessControlAddressFormat
  else
    lblDreamcastToolInternetProtocolInvalidMAC.Caption := InvalidMediaAccessControlAddressValue;
  lblDreamcastToolInternetProtocolInvalidMAC.Visible := InvalidMaskFormat or InvalidValue;
end;

procedure TfrmMain.LoadRepositoriesSelectionList;
const
  REPOSITORIES_DIRECTORY = 'repositories';
  REPOSITORY_CONFIG_KALLISTI = 'kallisti.conf';
  REPOSITORY_CONFIG_KALLISTI_PORTS = 'kallisti-ports.conf';
  REPOSITORY_CONFIG_DCLOAD_SERIAL = 'dcload-serial.conf';
  REPOSITORY_CONFIG_DCLOAD_IP = 'dcload-ip.conf';
  REPOSITORY_CONFIG_RUBY = 'mruby.conf';

var
  RepositoriesDirectory: TFileName;

  procedure LoadControl(Control: TComboBox; FileName: TFileName);
  begin
    if FileExists(RepositoriesDirectory + FileName) then
      Control.Items.LoadFromFile(RepositoriesDirectory + FileName);
  end;

begin
  RepositoriesDirectory := DreamcastSoftwareDevelopmentKitManager.Environment
    .FileSystem.Shell.ConfigurationDirectory + REPOSITORIES_DIRECTORY + '\';
  LoadControl(cbxUrlKallisti, REPOSITORY_CONFIG_KALLISTI);
  LoadControl(cbxUrlKallistiPorts, REPOSITORY_CONFIG_KALLISTI_PORTS);
  LoadControl(cbxUrlDreamcastToolSerial, REPOSITORY_CONFIG_DCLOAD_SERIAL);
  LoadControl(cbxUrlDreamcastToolIP, REPOSITORY_CONFIG_DCLOAD_IP);
  LoadControl(cbxUrlRuby, REPOSITORY_CONFIG_RUBY);
end;

procedure TfrmMain.InitializeAboutScreen;
var
  i: Integer;

  procedure DisplayProductInformation;
  const
    VERSION_FILE_NAME = 'VERSION';
    VERSION_SECTION_NAME = 'Version';

  var
    IniFile: TIniFile;

  begin
    IniFile := TIniFile.Create(DreamcastSoftwareDevelopmentKitManager.Environment
      .FileSystem.Shell.DreamSDKDirectory + VERSION_FILE_NAME);
    try
      FullVersionNumber := IniFile.ReadString(VERSION_SECTION_NAME, 'BuildNumber', UNKNOWN_VALUE);
      edtProductVersion.Text := Format('%s (%s)', [
        IniFile.ReadString(VERSION_SECTION_NAME, 'Release', UNKNOWN_VALUE),
        FullVersionNumber
      ]);
      edtProductBuildDate.Text := IniFile.ReadString(VERSION_SECTION_NAME, 'Date', UNKNOWN_VALUE);
    finally
      IniFile.Free;
    end;
  end;

  procedure DisplayHelpFileVersion;
  var
    HelpFileVersion,
    Temp: string;

  begin
    HelpFileVersion := GetRegisteredVersion(HelpFileName);
    if IsEmpty(HelpFileVersion) then
    begin
      HelpFileVersion := RetrieveVersionWithFind(HelpFileName, 'DreamSDK Help',
        sLineBreak, False);
      Temp := ExtractStr('Ver. ', #0, HelpFileVersion);
      if IsEmpty(Temp) then
        HelpFileVersion := Right('Ver. ', HelpFileVersion)
      else
        HelpFileVersion := Temp;
    end;

    if HelpFileVersion = EmptyStr then
      HelpFileVersion := UNKNOWN_VALUE
    else
      SetRegisteredVersion(HelpFileName, HelpFileVersion);

    edtProductHelpVersion.Text := HelpFileVersion;
  end;

begin
  btnCheckForUpdates.Caption := Format(btnCheckForUpdates.Caption, [GetProductName]);
  gbxPackageInfo.Caption := Format(gbxPackageInfo.Caption, [GetProductName]);
  lblTitleAbout.Caption := Format(lblTitleAbout.Caption, [GetProductName]);

  edtManagerCompilerInfo.Text := GetCompilerInfo;
  edtManagerTargetInfo.Text := GetTargetInfo;
  edtManagerOS.Text := GetOS;
  edtManagerLCLVersion.Text := GetLCLVersion;
  edtManagerWidgetSet.Text := GetWidgetSet;

  for i := 0 to ModuleVersionList.Count - 1 do
    cbxModuleSelection.Items.Add(ModuleVersionList[i].FileDescription);
  if cbxModuleSelection.Items.Count > 0 then
  begin
    cbxModuleSelection.ItemIndex := 0;
    cbxModuleSelectionChange(Self);
  end;

  DisplayProductInformation;
  DisplayHelpFileVersion;
end;

procedure TfrmMain.InitializeHomeScreen;
begin
  lblTitleHome.Caption := Format(lblTitleHome.Caption, [GetProductName]);
  lblIntroduction.Caption := Format(lblIntroduction.Caption, [GetProductName]);
  lblHomeShell.Caption := Format(lblHomeShell.Caption, [GetProductName]);
  lblHomeFolder.Caption := Format(lblHomeFolder.Caption, [GetProductName]);
  lblHomeHelp.Caption := Format(lblHomeHelp.Caption, [GetProductName]);
end;

procedure TfrmMain.InitializeComponentsScreen;
begin
  gbxEnvironmentContext.Caption := Format(gbxEnvironmentContext.Caption,
    [GetProductName]);
  edtValueHomeBaseDir.Caption := GetBaseInstallationHomeDirectory;
  UpdateComponentControls(True);
end;

procedure TfrmMain.InitializeIdeScreen;
begin
  gbxIdeCodeBlocksUsersInstalled.Caption :=
    Format(gbxIdeCodeBlocksUsersInstalled.Caption, [GetProductName]);
  lblIdeCodeBlocksUsersInstalled.Caption :=
    Format(lblIdeCodeBlocksUsersInstalled.Caption, [GetProductName]);
  lblIdeCodeBlocksUsersAvailable.Caption :=
    Format(lblIdeCodeBlocksUsersAvailable.Caption, [GetProductName]);

  SetButtonElevated(btnIdeRefresh.Handle);
  SetButtonElevated(btnIdeInstall.Handle);
  SetButtonElevated(btnIdeReinstall.Handle);
  SetButtonElevated(btnIdeUninstall.Handle);
  SetButtonElevated(btnIdeCodeBlocksUsersAvailableInitialize.Handle);
  SetButtonElevated(btnIdeCodeBlocksUsersAvailableRefresh.Handle);
  SetButtonElevated(btnIdeCodeBlocksInstallDetect.Handle);

  lbxIdeList.ItemIndex := 0;
  lbxIdeListClickCheck(Self);
end;

procedure TfrmMain.InitializeOptionsScreen;
begin
{$IFDEF DEBUG}
  DebugLog('InitializeOptionsScreen');
{$ENDIF}
  with DreamcastSoftwareDevelopmentKitManager do
  begin

    // Manage Repositories URL
    with Environment.Settings.Repositories do
    begin
      cbxUrlKallisti.Text := KallistiURL;
      cbxUrlKallistiPorts.Text := KallistiPortsURL;
      cbxUrlDreamcastToolSerial.Text := DreamcastToolSerialURL;
      cbxUrlDreamcastToolIP.Text := DreamcastToolInternetProtocolURL;
      cbxUrlRuby.Text := RubyURL;
{$IFDEF DEBUG}
      DebugLog(
        '  InitializeOptionsScreen, Loaded URLs:' + sLineBreak +
        '    * Kallisti: ' + cbxUrlKallisti.Text + sLineBreak +
        '    * Kallisti Ports: ' + cbxUrlKallistiPorts.Text + sLineBreak +
        '    * Dreamcast-Tool Serial: ' + cbxUrlDreamcastToolSerial.Text + sLineBreak +
        '    * Dreamcast-Tool IP: ' + cbxUrlDreamcastToolIP.Text + sLineBreak +
        '    * Ruby: ' + cbxUrlRuby.Text
      );
{$ENDIF}
    end;

    // Manage MSYS/MSYS2 environments
    btnOpenMinGWManager.Enabled := (Environment.FoundationKind = efkMinGWMSYS);
  end;
end;

procedure TfrmMain.HandleAero;
const
  LITTLE_BUTTON_HEIGHT = 21;

var
  i,
  ItemLabelExtraHeight,
  ItemButtonExtraHeight,
  ItemButtonExtraTop: Integer;
  ItemEdit: TLabeledEdit;
  ItemButton: TButton;
  ItemLabel: TLabel;

  procedure SetTransparent(Control: TControl);
  begin
    Control.ControlStyle := ItemEdit.ControlStyle - [csOpaque]
      + [csParentBackground];
    Control.Color := clNone;
  end;

begin
  if IsAeroEnabled or UseThemes then
  begin
{$IFDEF DEBUG}
    DebugLog('HandleAero: Start');
{$ENDIF}

    // Compute extra height for TLabels
    ItemLabelExtraHeight := ScaleY(2, Graphics.ScreenInfo.PixelsPerInchY);

    // Compute extra height and top for small TButtons
    ItemButtonExtraTop := ScaleY(1, Graphics.ScreenInfo.PixelsPerInchY) * -1;
    i := 2;
    if IsWindowsVistaOrGreater then
      Inc(i, 2);
    ItemButtonExtraHeight := ScaleY(i, Graphics.ScreenInfo.PixelsPerInchY);

    // Applying these new values
    for i := 0 to frmMain.ComponentCount - 1 do
    begin
{$IFDEF DEBUG}
      DebugLog('  Processing: ' + frmMain.Components[i].Name);
{$ENDIF}

      // TLabeledEdit
      if (frmMain.Components[i] is TLabeledEdit) then
      begin
        ItemEdit := (frmMain.Components[i] as TLabeledEdit);
{$IFDEF DEBUG}
        DebugLog('    TLabeledEdit: ' + ItemEdit.Name);
{$ENDIF}
        // Handle background color
        SetTransparent(ItemEdit);
        ItemEdit.ParentColor := False;
      end;

      // TLabel
      if (frmMain.Components[i] is TLabel) then
      begin
        ItemLabel := (frmMain.Components[i] as TLabel);
{$IFDEF DEBUG}
        DebugLog('    TLabel: ' + ItemLabel.Name);
{$ENDIF}
        if not ItemLabel.AutoSize then
          ItemLabel.Height := ItemLabel.Height + ItemLabelExtraHeight;
      end;

      // TButton
      if (frmMain.Components[i] is TButton) then
      begin
        ItemButton := (frmMain.Components[i] as TButton);
        if (ItemButton.Height = LITTLE_BUTTON_HEIGHT) then
        begin
{$IFDEF DEBUG}
          DebugLog('    TButton: ' + ItemButton.Name);
{$ENDIF}
          ItemButton.Height := ItemButton.Height + ItemButtonExtraHeight;
          ItemButton.Top := ItemButton.Top + ItemButtonExtraTop;
        end;
      end;
    end; // for

  end; // IsAeroEnabled / UseThemes
{$IFDEF DEBUG}
  DebugLog('HandleAero: End');
{$ENDIF}
end;

function TfrmMain.GetMsgBoxWindowHandle: THandle;
var
  IsProgressHandlePreferred,
  IsUnpackHandlePreferred: Boolean;

begin
  Result := Handle;

  IsProgressHandlePreferred := (not IsPostInstallMode and (not IsProgressAutoClose))
    and Assigned(frmProgress) and frmProgress.Visible;
  IsUnpackHandlePreferred := (not IsProgressHandlePreferred)
    and Assigned(frmUnpack) and frmUnpack.Visible;

  if IsPostInstallMode or IsProgressHandlePreferred then
    Result := frmProgress.Handle
  else if IsUnpackHandlePreferred then
    Result := frmUnpack.Handle;
end;

function TfrmMain.GetAllKallistiPortsIcon(
  const Operation: TShellThreadInputRequest): TMsgDlgType;
begin
  Result := mtWarning;
  if (Operation = stiKallistiPortsInstall) and
     (DreamcastSoftwareDevelopmentKitManager.Versions.CMakeInstalled) then
       Result := mtConfirmation;
end;

function TfrmMain.GetAllKallistiPortsMessage(const Message: string): string;
begin
  Result := Message;
  if (not DreamcastSoftwareDevelopmentKitManager.Versions.CMakeInstalled) then
     Result := Message + MsgBoxDlgWrapStr + UseSubversionAllKallistiPorts;
end;

function TfrmMain.CheckKallistiSinglePortPossibleInstallation: Boolean;
begin
  Result := True;
  if SelectedKallistiPort.UseSubversion
    and (not DreamcastSoftwareDevelopmentKitManager.Versions.CMakeInstalled) then
    begin
      Result := False;
      MsgBox(DialogWarningTitle, Format(UseSubversionKallistiSinglePort,
        [SelectedKallistiPort.Name]), mtWarning, [mbOK]);
    end;
end;

procedure TfrmMain.CreateNetworkAdapterList;
var
  NetworkCardAdapters: TNetworkCardAdapterList;
  i: Integer;
  ListEnabled: Boolean;

begin
  NetworkCardAdapters := Default(TNetworkCardAdapterList);
  GetNetworkCardAdapterList(NetworkCardAdapters);

  cbxDreamcastToolInternetProtocolNetworkAdapter.Clear;
  for i := Low(NetworkCardAdapters) to High(NetworkCardAdapters) do
  begin
    if Length(NetworkCardAdapters[i].IPv4Addresses) > 0 then
      with NetworkCardAdapters[i] do
        cbxDreamcastToolInternetProtocolNetworkAdapter.Items.AddObject(
          NetworkCardName, TNetworkAdapterListUserInterfaceItem.Create(MacAddress)
        );
  end;

  cbxDreamcastToolInternetProtocolNetworkAdapter.ItemIndex := -1;

  ListEnabled := HasNetworkAdapters;
  cbxDreamcastToolInternetProtocolNetworkAdapter.Enabled := ListEnabled;
  lblDreamcastToolInternetProtocolNetworkAdapter.Enabled := ListEnabled;
  if not ListEnabled then
  begin
    cbxDreamcastToolInternetProtocolNetworkAdapter.Hint := NoNetworkAdapterAvailable;
    lblDreamcastToolInternetProtocolNetworkAdapter.Hint := NoNetworkAdapterAvailable;
  end;
end;

procedure TfrmMain.CreateSerialPortList;
var
  SerialPorts: TSerialPortList;
  i: Integer;
  ListEnabled: Boolean;
  CustomFriendlyName: string;

begin
  SerialPorts := Default(TSerialPortList);
  GetSerialPortList(SerialPorts);

{$IFDEF DEBUG}
  DebugLog('*** LISTING COM PORTS ***');
{$ENDIF}

  cbxDreamcastToolSerialPort.Clear;
  for i := Low(SerialPorts) to High(SerialPorts) do
  begin
    with SerialPorts[i] do
    begin
      CustomFriendlyName := Format('%s (%s)', [Name, Description]);
{$IFDEF DEBUG}
      DebugLog('  ' + FriendlyName);
{$ENDIF}
      cbxDreamcastToolSerialPort.Items.AddObject(
        CustomFriendlyName, TSerialPortListUserInterfaceItem.Create(PortIndex)
      );
    end;
  end;

  cbxDreamcastToolSerialPort.ItemIndex := -1;

  ListEnabled := HasSerialPorts;
  cbxDreamcastToolSerialPort.Enabled := ListEnabled;
  lblDreamcastToolSerialPort.Enabled := ListEnabled;
  if not ListEnabled then
  begin
    cbxDreamcastToolSerialPort.Hint := NoSerialPortAvailable;
    lblDreamcastToolSerialPort.Hint := NoSerialPortAvailable;
  end;
end;

procedure TfrmMain.FreeNetworkAdapterList;
var
  i: Integer;

begin
  with cbxDreamcastToolInternetProtocolNetworkAdapter.Items do
  begin
    for i := Count - 1 downto 0 do
      Objects[i].Free;
    Clear;
  end;
end;

procedure TfrmMain.FreeSerialPortList;
var
  i: Integer;

begin
  with cbxDreamcastToolSerialPort.Items do
  begin
    for i := Count - 1 downto 0 do
      Objects[i].Free;
    Clear;
  end;
end;

function TfrmMain.HostMacToItemIndex(
  const HostMediaAccessControlAddress: string): Integer;
var
  i: Integer;
  CurrentItem: TNetworkAdapterListUserInterfaceItem;

begin
  Result := -1;
  with cbxDreamcastToolInternetProtocolNetworkAdapter.Items do
    for i := 0 to Count - 1 do
    begin
      CurrentItem := TNetworkAdapterListUserInterfaceItem(Objects[i]);
      if Assigned(CurrentItem) and
        (CurrentItem.MacAddress = HostMediaAccessControlAddress) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TfrmMain.SerialPortToItemIndex(
  const SerialPortIndex: Integer): Integer;
var
  i: Integer;
  CurrentItem: TSerialPortListUserInterfaceItem;

begin
  Result := -1;
  with cbxDreamcastToolSerialPort.Items do
    for i := 0 to Count - 1 do
    begin
      CurrentItem := TSerialPortListUserInterfaceItem(Objects[i]);
      if Assigned(CurrentItem) and
        (CurrentItem.SerialPortIndex = SerialPortIndex) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TfrmMain.HasNetworkAdapters: Boolean;
begin
  Result := cbxDreamcastToolInternetProtocolNetworkAdapter.Items.Count > 0;
end;

function TfrmMain.HasSerialPorts: Boolean;
begin
  Result := cbxDreamcastToolSerialPort.Items.Count > 0;
end;

procedure TfrmMain.RefreshIdeScreen;
begin
  with DreamcastSoftwareDevelopmentKitManager
    .IntegratedDevelopmentEnvironment.CodeBlocks do
  begin
    Refresh;
    gbxIdeCodeBlocksInstallDir.Enabled := not Installed;
    if Installed or (not Installed and IsEmpty(edtIdeCodeBlocksInstallDir.Text)) then
      edtIdeCodeBlocksInstallDir.Text := InstallationDirectory;
    lbxIdeCodeBlocksUsersAvailable.Items.Assign(AvailableUsers);
    lbxIdeCodeBlocksUsersInstalled.Items.Assign(InstalledUsers);
    btnIdeInstall.Enabled := not Installed;
    btnIdeCodeBlocksUsersAvailableInitialize.Enabled := not Installed;
    btnIdeReinstall.Enabled := Installed;
    btnIdeUninstall.Enabled := Installed;
    lbxIdeList.State[lbxIdeList.ItemIndex] := BooleanToCheckboxState(Installed);
  end;
end;

procedure TfrmMain.RunMSYS;
begin
  RunMSYS(EmptyStr);
end;

procedure TfrmMain.RunMSYS(const WorkingDirectory: TFileName);
var
  ShellExecutable: TFileName;

begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
  ShellExecutable := DreamcastSoftwareDevelopmentKitManager.Environment
    .FileSystem.Shell.LauncherExecutable;
  RunShellExecute(ShellExecutable, WorkingDirectory);
end;

procedure TfrmMain.AskForUpdate(const MandatoryAction: Boolean = False);
var
  Msg, Title: string;
  DlgIcon: TMsgDlgType;

begin
  Msg := Format(MsgBoxDlgTranslateString(ResetRepositoryConfirmUpdate), [KallistiText]);
  Title := DialogQuestionTitle;
  DlgIcon := mtConfirmation;

  if MandatoryAction then
  begin
    Msg := Format(MsgBoxDlgTranslateString(ResetRepositoryWarningUpdate), [KallistiText]);
    Title := DialogWarningTitle;
    DlgIcon := mtWarning;
  end;

  if (MsgBox(Title, Msg, DlgIcon, [mbYes, mbNo], mbNo) = mrYes) then
    ExecuteThreadOperation(stiKallistiManage);
end;

function TfrmMain.RunElevatedTask(const ATaskName: string): Boolean;
var
  WorkingParameters: TStringList;

begin
  WorkingParameters := TStringList.Create;
  try
    Result := RunElevatedTask(ATaskName, WorkingParameters);
  finally
    WorkingParameters.Free;
  end;
end;

function TfrmMain.RunElevatedTask(const ATaskName: string;
  AParameters: TStringList): Boolean;
var
  LogContext: TLogMessageContext;
  SwapExchangeFileName: TFileName;
  ElevatedParameters: string;
  RunElevatedResult: Cardinal;

  procedure StartWait;
  begin
    Cursor := crHourglass;
    Screen.Cursor := crHourglass;
    Application.ProcessMessages;
  end;

  procedure EndWait;
  begin
    Cursor := crDefault;
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;

begin
  Result := False;
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    SwapExchangeFileName := GetTemporaryFileName;

    LogMessage(LogContext, Format('SwapExchangeFileName: "%s"', [
      SwapExchangeFileName
    ]));

    StartWait;
    try
      AParameters.Insert(0, SwapExchangeFileName);
      if IsLogMessageEnabled then
        AParameters.Add(GetLogMessageCommandLineSwitch);
      ElevatedParameters := EncodeParameters(AParameters);

      RunElevatedResult := RunElevated(ATaskName, ElevatedParameters, Handle,
        @Application.ProcessMessages);

      LogMessage(LogContext, Format('RunElevatedResult: %d, ATaskName: %s, ElevatedParameters: %s', [
        RunElevatedResult,
        ATaskName,
        ElevatedParameters
      ]));

      SetLastError(RunElevatedResult);

      if IsRealOSError(GetLastError) then
        RaiseLastOSError
      else
      begin
        Result := (GetLastError = ERROR_SUCCESS);
        with DreamcastSoftwareDevelopmentKitManager
          .IntegratedDevelopmentEnvironment.CodeBlocks do
        begin
          UpdateStateFromElevatedTask(SwapExchangeFileName);
          KillFile(SwapExchangeFileName);
        end;
      end;
    finally
      EndWait;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TfrmMain.RefreshViewKallistiPorts(ForceRefresh: Boolean);

  procedure RefreshKallistiPortsControls;
  var
    i, PortsCount, PortsVisibleCount, InstalledPortsCount: Integer;
    CurrentPort: TKallistiPortItem;

  begin
    PortsCount := DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Count;
    PortsVisibleCount := DreamcastSoftwareDevelopmentKitManager.KallistiPorts.CountVisible;

    InstalledPortsCount := 0;
    for i := 0 to PortsCount - 1 do
    begin
      CurrentPort := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[i];
      if (not CurrentPort.Hidden) and (CurrentPort.Installed) then
        Inc(InstalledPortsCount);
    end;

    btnAllPortInstall.Enabled := (PortsVisibleCount <> InstalledPortsCount);
    btnAllPortUninstall.Enabled := (InstalledPortsCount > 0);

{$IFDEF DEBUG}
    with DreamcastSoftwareDevelopmentKitManager.KallistiPorts do
    begin
      WriteLn(
        'AllCount: ', Count,
        ', CountVisible: ', CountVisible,
        ', InstalledPortsCount: ', InstalledPortsCount
      );
    end;
{$ENDIF}
  end;

begin
  if ForceRefresh then
    DreamcastSoftwareDevelopmentKitManager.KallistiPorts.RetrieveAvailablePorts;

  DisplayKallistiPorts(ForceRefresh);
  RefreshKallistiPortsControls;
end;

procedure TfrmMain.RefreshViewEnvironment(ForceRefresh: Boolean);
begin
  if ForceRefresh then
    DreamcastSoftwareDevelopmentKitManager.Versions.RetrieveVersions;
  DisplayEnvironmentComponentVersions;
end;

procedure TfrmMain.RefreshEverything(ForceRefresh: Boolean);
begin
  Application.ProcessMessages;

  UpdateOptionsControls;
  UpdateRubyControls;
  RefreshViewDreamcastTool;
  RefreshViewEnvironment(ForceRefresh); // TODO: Slow function, need to be cached
  RefreshViewKallistiPorts(ForceRefresh);
end;

procedure TfrmMain.OnCommandTerminateThread(Sender: TObject;
  Request: TShellThreadInputRequest; Response: TShellThreadOutputResponse;
  Success: Boolean; UpdateState: TUpdateOperationState);
begin
  Screen.Cursor := crHourGlass;
  fShellThreadSuccess := Success;
  fShellThreadInputRequest := Request;
  fShellThreadOutputResult := Response;
  fShellThreadUpdateState := UpdateState;
  Delay(450);
  tmrShellThreadTerminate.Enabled := True;
end;

procedure TfrmMain.OnPackageManagerTerminate(Sender: TObject;
  const Success: Boolean; const Aborted: Boolean);
type
  TPackageManagerUpdateNeededType = (pmuntUseless, pmuntToolchain, pmuntOffline);

var
  PackageManagerUpdateNeededType: TPackageManagerUpdateNeededType;

begin
  // If Error/Aborted then warn the user and exit
  if (not Success) and (not Aborted) then
  begin
    MsgBox(DialogWarningTitle, UnableToInstallPackageText, mtWarning, [mbOK]);
    Exit;
  end;

  // Determine now (before UI changes) what kind of update we need (and if we need!)
  PackageManagerUpdateNeededType := pmuntUseless;
  if (ComponentSelectedOperation = pmrToolchain) then
    PackageManagerUpdateNeededType := pmuntToolchain
  else if (ComponentSelectedOperation = pmrOffline) and (fPackageManagerSelectedOfflinePackage <> pmroRuby) then
    PackageManagerUpdateNeededType := pmuntOffline;

  // Update UI (ComponentSelectedOperation will be reset!)
  DreamcastSoftwareDevelopmentKitManager.Versions.RetrieveVersions;
  DisplayEnvironmentComponentVersions;
  UpdateKallistiControls;
  UpdateComponentControls;
  UpdateOptionsControls;

  // Ask for update if needed
  if Success then
  begin
    case PackageManagerUpdateNeededType of
      pmuntOffline:
        begin
          // If an Offline button (but not for Ruby) was hit and everything is OK
          AskForUpdate;
        end;

      pmuntToolchain:
        begin
          // If toolchain was changed, libraries should be rebuilt!
          DreamcastSoftwareDevelopmentKitManager.ForceNextRebuild;

          // Then ask for update with different message
          AskForUpdate(True);
        end;
    end;
  end;

  // Clear flag
  fPackageManagerOperation := pmrUndefined;
end;

function TfrmMain.MsgBox(const aCaption: string; const aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn
  ): TModalResult;
begin
  Result := MsgBoxDlg(GetMsgBoxWindowHandle, aCaption, aMsg, DlgType, Buttons,
    DefaultButton);
end;

function TfrmMain.MsgBox(const aCaption: string; const aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult;
begin
  Result := MsgBoxDlg(GetMsgBoxWindowHandle, aCaption, aMsg, DlgType, Buttons);
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnCreditsClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnComponentsApplyClick(Sender: TObject);
var
  IsValidPythonVersionSelected: Boolean;
  AnywayText,
  PayAttentionText,
  MessageText,
  SelectedPythonVersion: string;
  ErrorMessages: TStringList;

begin
  ErrorMessages := TStringList.Create;
  try
    // Check if requested Python version is available
    SelectedPythonVersion := EmptyStr;
    IsValidPythonVersionSelected := IsDebuggerPythonVersionInstalled(
      ComponentSelectedDebugger, SelectedPythonVersion);

{$IFDEF DEBUG}
    WriteLn('Package Manager Operation: ', ComponentSelectedOperation);
    WriteLn('  Selected Toolchain: ', ComponentSelectedToolchain);
    WriteLn('  Selected Debugger: ', ComponentSelectedDebugger, ' [', IsValidPythonVersionSelected, ']');
{$ENDIF}

    // Added if Python 32-bit runtime was not found
    if not IsValidPythonVersionSelected then
    begin
      if (ErrorMessages.Count > 0) then
        ErrorMessages.Add(UnpackConfirmationAndKeywordText);
      ErrorMessages.Add(Format(UnpackConfirmationInvalidPythonText, [SelectedPythonVersion]));
    end;

    // Final message
    PayAttentionText := EmptyStr;
    AnywayText := EmptyStr;
    if ErrorMessages.Count > 0 then
    begin
      PayAttentionText := UnpackConfirmationPayAttentionText;
      AnywayText := UnpackConfirmationQuestionAnywayText;
    end;
    MessageText := MsgBoxDlgTranslateString(Format(UnpackConfirmationText, [
      PayAttentionText,
      StringListToString(ErrorMessages, ' '),
      AnywayText
    ]));

    // If user agrees, then we continue
    if (MsgBox(DialogWarningTitle, MessageText, mtWarning, [mbYes, mbNo], mbNo) = mrYes) then
      with PackageManager do
      begin
        DebuggerProfileKey := ComponentSelectedDebugger;
        ToolchainProfileKey := ComponentSelectedToolchain;
        Operation := ComponentSelectedOperation;
        Execute;
      end;
  finally
    ErrorMessages.Free;
  end;
end;

procedure TfrmMain.btnDebugGetSelectedDebuggerClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  ShowMessage(GetSelectedDebugger);
{$ENDIF}
end;

procedure TfrmMain.btnDebugRaiseExceptionClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  raise Exception.Create('Test Exception');
{$ENDIF}
end;

procedure TfrmMain.btnIdeCodeBlocksInstallDirClick(Sender: TObject);
begin
  with sddIdeCodeBlocks do
  begin
    if DirectoryExists(edtIdeCodeBlocksInstallDir.Text) then
      FileName := edtIdeCodeBlocksInstallDir.Text;
    if Execute then
      edtIdeCodeBlocksInstallDir.Text := FileName;
  end;
end;

procedure TfrmMain.btnInstallMRubyClick(Sender: TObject);
var
  InstallRequest: Boolean;

begin
  InstallRequest := ((Sender as TButton).Tag = 0);
{$IFDEF DEBUG}
  DebugLog('Ruby Install Request: ' + BoolToStr(InstallRequest));
{$ENDIF}

  if InstallRequest then
  begin
    // Check Ruby runtime
    if (not DreamcastSoftwareDevelopmentKitManager.Versions.RubyInstalled) or
      (not DreamcastSoftwareDevelopmentKitManager.Versions.MesonInstalled) then
    begin
      MsgBox(DialogWarningTitle, UnableToInstallRubyRuntimeText, mtWarning, [mbOK]);
      Exit;
    end;

    // Check Git availability (if needed)
    if not DreamcastSoftwareDevelopmentKitManager.Versions.GitInstalled and
      not DreamcastSoftwareDevelopmentKitManager.Ruby.Repository.Offline then
    begin
      MsgBox(DialogWarningTitle, UnableToInstallRubyGitText, mtWarning, [mbOK]);
      Exit;
    end;

    // Install can start!
    if MsgBox(DialogWarningTitle, InstallRubyText, mtWarning, [mbYes, mbNo]) = mrYes then
    begin
      DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Ruby.Enabled := True;
      DoUpdateAll;
    end;
  end
  else
  begin
    // Uninstall
    // This is just basically removing compiled mruby binaries/libraries
    if MsgBox(DialogWarningTitle, UninstallRubyText, mtWarning, [mbYes, mbNo], mbNo) = mrYes then
    begin
      DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Ruby.Enabled := False;
      if not DreamcastSoftwareDevelopmentKitManager.Ruby.Uninstall then
        MsgBox(DialogWarningTitle, UninstallRubyFailedText, mtWarning, [mbOK]);
      RefreshEverything(True);
    end;
  end;
end;

procedure TfrmMain.btnOfflineKallistiClick(Sender: TObject);
var
  Msg: string;

  function OfflinePackageToString: string;
  begin
    Result := EmptyStr;
    case fPackageManagerSelectedOfflinePackage of
      pmroKallisti:
        Result := KallistiText;
      pmroKallistiPorts:
        Result := KallistiPortsText;
      pmroDreamcastToolSerial:
        Result := DreamcastToolSerialText;
      pmroDreamcastToolInternetProtocol:
        Result := DreamcastToolInternetProtocolText;
      pmroRuby:
        Result := RubyText;
    end;
  end;

begin
  fPackageManagerOperation := pmrOffline;
  fPackageManagerSelectedOfflinePackage := TPackageManagerRequestOffline((Sender as TButton).Tag);

{$IFDEF DEBUG}
  WriteLn('Install Offline Package: ', fPackageManagerSelectedOfflinePackage);
{$ENDIF}

  Msg := MsgBoxDlgTranslateString(Format(UnpackOfflineQuestion, [OfflinePackageToString]));
  if (MsgBox(DialogWarningTitle, Msg, mtWarning, [mbYes, mbNo], mbNo) = mrYes) then
    with PackageManager do
    begin
      Operation := fPackageManagerOperation;
      OfflinePackage := fPackageManagerSelectedOfflinePackage;
      Execute;
    end;
end;

procedure TfrmMain.btnOpenHelpClick(Sender: TObject);
begin
  RunShellExecute(HelpFileName);
end;

procedure TfrmMain.btnOpenHomeClick(Sender: TObject);
begin
  RunShellExecute(DreamcastSoftwareDevelopmentKitManager.Environment
    .FileSystem.Shell.HomeDirectory);
end;

procedure TfrmMain.btnAllPortInstallClick(Sender: TObject);
begin
  if MsgBox(DialogQuestionTitle, GetAllKallistiPortsMessage(InstallAllKallistiPorts),
     GetAllKallistiPortsIcon(stiKallistiPortsInstall), [mbYes, mbNo]) = mrYes then
       ExecuteThreadOperation(stiKallistiPortsInstall);
end;

procedure TfrmMain.apMainException(Sender: TObject; E: Exception);
var
  SenderName: string;

begin
  if (E is EDBEditError) and (ActiveControl is TMaskEdit) then
  begin
    SenderName := (ActiveControl as TMaskEdit).Name;
    if SameText(SenderName, 'edtDreamcastToolInternetProtocolAddress') then
      HandleInvalidInternetProtocolAddress(True)
    else
      HandleInvalidMediaAccessControlAddress(True);
  end
  else
    Application.ShowException(E);
end;

procedure TfrmMain.btnAllPortUninstallClick(Sender: TObject);
begin
  if MsgBox(DialogWarningTitle, GetAllKallistiPortsMessage(UninstallAllKallistiPorts),
    GetAllKallistiPortsIcon(stiKallistiPortsUninstall), [mbYes, mbNo], mbNo) = mrYes then
      ExecuteThreadOperation(stiKallistiPortsUninstall);
end;

procedure TfrmMain.btnBrowseHomeBaseDirectoryClick(Sender: TObject);
begin
  RunShellExecute(edtValueHomeBaseDir.Text);
end;

procedure TfrmMain.btnCheckForUpdatesClick(Sender: TObject);
var
  RemoteVersion: string;

  function GetRemoteVersion: string;
  const
    UPDATE_URL = 'http://www.dreamsdk.org/.update/version.txt';

  var
    HTTPClient: TFPHTTPClient;

  begin
    try
      HTTPClient := TFPHTTPClient.Create(nil);
      try
        HTTPClient.AllowRedirect := True;
        Result := Trim(HTTPClient.Get(UPDATE_URL));
      finally
        HTTPClient.Free;
      end;
    except
      on E:Exception do
      begin
        Result := EmptyStr;
{$IFDEF DEBUG}
        DebugLog('GetRemoteVersion Exception: ' + E.Message);
{$ENDIF}
      end;
    end;
  end;

begin
  RemoteVersion := GetRemoteVersion;

{$IFDEF DEBUG}
  DebugLog('Version: ' + RemoteVersion);
{$ENDIF}

  if IsEmpty(RemoteVersion) then
    MsgBox(DialogWarningTitle, MsgBoxDlgTranslateString(Format(UnableToRetrieveRemotePackageVersion, [GetProductName])), mtWarning, [mbOk])
  else
  begin
    if CompareVersion(FullVersionNumber, RemoteVersion) > 0 then
    begin
      if MsgBox(DialogQuestionTitle, Format(PackageUpdateAvailable, [GetProductName, RemoteVersion]), mtConfirmation, [mbYes, mbNo]) = mrYes then
        OpenURL(GetComments);
    end
    else
      MsgBox(DialogInformationTitle, Format(PackageUpToDate, [GetProductName]), mtInformation, [mbOk]);
  end;
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  with DreamcastSoftwareDevelopmentKitManager do
  begin
    if (Environment.FoundationKind = efkMinGWMSYS) then
      RunNoWait(Environment.FileSystem.Shell.MinGWGetExecutable);
  end;
end;

procedure TfrmMain.btnOpenMSYSClick(Sender: TObject);
begin
  RunMSYS;
end;

procedure TfrmMain.btnOptionsShortcutClick(Sender: TObject);
begin
  pcMain.ActivePage := tsOptions;
end;

procedure TfrmMain.btnRubyOpenHomeClick(Sender: TObject);
begin
  with DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Ruby do
  begin
    if DirectoryExists(SamplesDirectory) then
      RunShellExecute(SamplesDirectory)
    else
      MsgBoxDlg(Handle, DialogWarningTitle,
        MsgBoxDlgTranslateString(RubySamplesNotInstalled), mtInformation, [mbOK]);
  end;
end;

procedure TfrmMain.btnRubyOpenMSYSClick(Sender: TObject);
begin
  with DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Ruby do
  begin
    if DirectoryExists(SamplesDirectory) then
      RunMSYS(SamplesDirectory)
    else
      MsgBoxDlg(Handle, DialogWarningTitle,
        MsgBoxDlgTranslateString(RubySamplesNotInstalled), mtInformation, [mbOK]);
  end;
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  if CheckKallistiSinglePortPossibleInstallation then
    ExecuteThreadOperation(stiKallistiSinglePortInstall);
end;

procedure TfrmMain.btnPortUninstallClick(Sender: TObject);
var
  Msg: string;

begin
  if Assigned(SelectedKallistiPort) then
  begin
    if CheckKallistiSinglePortPossibleInstallation then
    begin
      Msg := Format(UninstallKallistiSinglePort, [SelectedKallistiPort.Name]);
      if MsgBox(DialogQuestionTitle, Msg, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes then
        ExecuteThreadOperation(stiKallistiSinglePortUninstall);
    end;
  end;
end;

procedure TfrmMain.btnPortUpdateClick(Sender: TObject);
begin
  if CheckKallistiSinglePortPossibleInstallation then
    ExecuteThreadOperation(stiKallistiSinglePortUpdate);
end;

procedure TfrmMain.btnRestoreDefaultsClick(Sender: TObject);

  procedure ResetText(ComboBox: TComboBox; Value: string);
  begin
    if ComboBox.Enabled then
      ComboBox.Text := Value;
  end;

begin
  if MsgBox(DialogQuestionTitle, RestoreDefaultsText, mtConfirmation, [mbYes, mbNo]) = mrYes then
  begin
    rgxTerminalOption.ItemIndex := 0;
    rgxTerminalOptionClick(Self);

    // Repositories
    ResetText(cbxUrlKallisti, GetDefaultUrlKallisti);
    ResetText(cbxUrlKallistiPorts, GetDefaultUrlKallistiPorts);
    ResetText(cbxUrlDreamcastToolSerial, GetDefaultUrlDreamcastToolSerial);
    ResetText(cbxUrlDreamcastToolIP, GetDefaultUrlDreamcastToolInternetProtocol);
    ResetText(cbxUrlRuby, GetDefaultUrlRuby);

    // Dreamcast Tool (only options, not RS232 cable/IP settings...)
    rgxDreamcastTool.ItemIndex := DREAMCAST_TOOL_DEFAULT_KIND;
    rgxDreamcastToolSelectionChanged(Self);
    ckxDreamcastToolAttachConsoleFileServer.Checked := DREAMCAST_TOOL_DEFAULT_ATTACH_CONSOLE_FILESERVER;
    ckxDreamcastToolClearScreenBeforeDownload.Checked := DREAMCAST_TOOL_DEFAULT_CLEAR_SCREEN_BEFORE_DOWNLOAD;
    ckxDreamcastToolInternetProtocolUseARP.Checked := DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ENABLED;
    ckxDreamcastToolSerialDumbTerminal.Checked := DREAMCAST_TOOL_DEFAULT_SERIAL_DUMB_TERMINAL;
    ckxDreamcastToolSerialExternalClock.Checked := DREAMCAST_TOOL_DEFAULT_SERIAL_EXTERNAL_CLOCK;
    SelectedSerialBaudrate := DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE;
    ckxDreamcastToolSerialAlternateBaudrate.Checked := DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE;

    // Save changes
    DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
  end;
end;

procedure TfrmMain.btnUpdateKallistiOSClick(Sender: TObject);
begin
  DoUpdateAll;
end;

procedure TfrmMain.btnDreamcastToolCustomExecutableClick(Sender: TObject);
begin
  with opdDreamcastToolCustom do
    if Execute then
      edtDreamcastToolCustomExecutable.Text := opdDreamcastToolCustom.FileName;
end;

procedure TfrmMain.btnUpdateMRubyClick(Sender: TObject);
begin
  DoUpdateAll;
end;

procedure TfrmMain.btnUrlKallistiClick(Sender: TObject);
var
  Index: Integer;
  Msg: string;

  function TagToRepositoryKind: TRepositoryKind;
  begin
    Result := rkUndefined;
    case Index of
      0: Result := rkKallisti;
      1: Result := rkKallistiPorts;
      2: Result := rkDreamcastToolSerial;
      3: Result := rkDreamcastToolInternetProtocol;
      4: Result := rkRuby;
    end;
  end;

  function TagToString: string;
  begin
    Result := '(Undefined)';
    case Index of
      0: Result := KallistiText;
      1: Result := KallistiPortsText;
      2: Result := DreamcastToolSerialText;
      3: Result := DreamcastToolInternetProtocolText;
      4: Result := RubyText;
    end;
  end;

  function TagToComboBox: TComboBox;
  begin
    Result := nil;
    case Index of
      0: Result := cbxUrlKallisti;
      1: Result := cbxUrlKallistiPorts;
      2: Result := cbxUrlDreamcastToolSerial;
      3: Result := cbxUrlDreamcastToolIP;
      4: Result := cbxUrlRuby;
    end;
  end;

  function TagToRepository: TDreamcastSoftwareDevelopmentRepository;
  begin
    Result := nil;
    with DreamcastSoftwareDevelopmentKitManager do
      case Index of
        0: Result := KallistiOS.Repository;
        1: Result := KallistiPorts.Repository;
        2: Result := DreamcastTool.RepositorySerial;
        3: Result := DreamcastTool.RepositoryInternetProtocol;
        4: Result := Ruby.Repository;
      end;
  end;

begin
  Index := (Sender as TButton).Tag;

  if not DreamcastSoftwareDevelopmentKitManager.Versions.GitInstalled then
  begin
    MsgBox(DialogWarningTitle, GitNeeded, mtWarning, [mbOK]);
    Exit;
  end;

  if not IsInternetConnectionAvailable then
  begin
    MsgBox(DialogWarningTitle, InternetConnectionNeeded, mtWarning, [mbOK]);
    Exit;
  end;

  Msg := Format(MsgBoxDlgTranslateString(ResetRepositoryQuestion), [TagToString]);

  if MsgBox(DialogWarningTitle, Msg, mtWarning, [mbYes, mbNo], mbNo) = mrYes then
  begin
    if DreamcastSoftwareDevelopmentKitManager
      .Environment.FileSystem.ResetRepository(TagToRepositoryKind) then
    begin
      // Setting up the default repo
      if Assigned(TagToComboBox) and (TagToComboBox.Items.Count > 0) then
      begin
        TagToComboBox.ItemIndex := 0; // The first item in list is the default
        UpdateRepositories;
      end;

      // Refresh version numbers and build date
      RefreshEverything(True);

      // Ask if the user wants to update now
      if TagToRepositoryKind <> rkRuby then
        AskForUpdate;
    end
    else
      MsgBox(DialogWarningTitle, Format(FailedToResetRepository, [TagToString]),
        mtWarning, [mbOk])
  end;
end;

procedure TfrmMain.btnIdeInstallClick(Sender: TObject);
var
  CodeBlocksInstallationDirectory: TFileName;
  InstallTitle,
  InstallMessage: string;
  InstallIcon: TMsgDlgType;
  EncodedParameters: TStringList;
  CodeBlocksVersion: TCodeBlocksVersion;

  procedure SetCodeBlocksState(const State: Boolean);
  begin
    btnIdeRefresh.Enabled := State;
    if not State then
    begin
       Cursor := crHourGlass;
       btnIdeInstall.Enabled := False;
       btnIdeReinstall.Enabled := False;
       btnIdeUninstall.Enabled := False;
    end
    else
    begin
      RefreshIdeScreen;
      Cursor := crDefault;
    end;
  end;

  function GetLastErrorMessage(LastErrorMessage: string): string;
  begin
    LastErrorMessage := StringReplace(LastErrorMessage, sLineBreak, '\n', [rfReplaceAll]);
    Result := MsgBoxDlgTranslateString(LastErrorMessage);
  end;

begin
  CodeBlocksInstallationDirectory := EmptyStr;
  if not IsEmpty(edtIdeCodeBlocksInstallDir.Text) then
  begin
    CodeBlocksInstallationDirectory := IncludeTrailingPathDelimiter(
      edtIdeCodeBlocksInstallDir.Text);
  end;

  with DreamcastSoftwareDevelopmentKitManager.IntegratedDevelopmentEnvironment.CodeBlocks do
    case (Sender as TButton).Tag of
      0: // Install
        begin
          // Check C::B directory
          if not DirectoryExists(CodeBlocksInstallationDirectory) then
          begin
            MsgBox(DialogWarningTitle, CodeBlocksInstallationDirectoryNotExists,
              mtWarning, [mbOK]);
            Exit;
          end;

          // Checking installed C::B
          CodeBlocksVersion := GetCodeBlocksVersion(CodeBlocksInstallationDirectory);

          // Check if C::B is installed in this directory
          if CodeBlocksVersion = cbvUndefined then
          begin
            MsgBox(DialogWarningTitle, CodeBlocksInstallationDirectoryInvalid,
              mtWarning, [mbOK]);
            Exit;
          end;

          InstallTitle := DialogQuestionTitle;
          InstallMessage := Format(ConfirmCodeBlocksMessage, [ConfirmCodeBlocksInstallation]);
          InstallIcon := mtConfirmation;

          // Check if the hash is correct
          if CodeBlocksVersion = cbvUnknown then
          begin
            InstallTitle := DialogWarningTitle;
            InstallMessage := MsgBoxDlgTranslateString(CodeBlocksIncorrectHash);
            InstallIcon := mtWarning;
          end;

          // Let's go
          if MsgBox(InstallTitle, InstallMessage, InstallIcon, [mbYes, mbNo], mbNo) = mrYes then
          begin
            SetCodeBlocksState(False);
            EncodedParameters := TStringList.Create;
            try
              EncodedParameters.Add(CodeBlocksInstallationDirectory);
              if RunElevatedTask(ELEVATED_TASK_CODEBLOCKS_IDE_INSTALL, EncodedParameters) then
                if not LastOperationSuccess then
                  MsgBox(DialogWarningTitle, GetLastErrorMessage(LastErrorMessage), mtWarning, [mbOK]);
            finally
              EncodedParameters.Free;
            end;
          end;
        end;

      1: // Reinstall
        if MsgBox(DialogWarningTitle,
          MsgBoxDlgTranslateString(ConfirmCodeBlocksReinstall), mtWarning, [mbYes, mbNo]) = mrYes then
        begin
          SetCodeBlocksState(False);
          if RunElevatedTask(ELEVATED_TASK_CODEBLOCKS_IDE_REINSTALL) then
            if not LastOperationSuccess then
              MsgBox(DialogWarningTitle, GetLastErrorMessage(LastErrorMessage), mtWarning, [mbOK]);
        end;

      2: // Uninstall
        if MsgBox(DialogWarningTitle, Format(ConfirmCodeBlocksMessage,
          [ConfirmCodeBlocksUninstallation]), mtWarning, [mbYes, mbNo]) = mrYes then
        begin
          SetCodeBlocksState(False);
          if RunElevatedTask(ELEVATED_TASK_CODEBLOCKS_IDE_UNINSTALL) then
            if not LastOperationSuccess then
              MsgBox(DialogWarningTitle, GetLastErrorMessage(LastErrorMessage), mtWarning, [mbOK]);
        end;

      3: // Refresh
        begin
          SetCodeBlocksState(False);
          if RunElevatedTask(ELEVATED_TASK_CODEBLOCKS_IDE_REFRESH) then
            if not LastOperationSuccess then
              MsgBox(DialogErrorTitle, GetLastErrorMessage(LastErrorMessage), mtError, [mbOK]);
        end;

      4: // Initialize Profiles
        if MsgBox(DialogQuestionTitle,
          MsgBoxDlgTranslateString(CodeBlocksConfirmInitializeProfile), mtConfirmation, [mbYes, mbNo]) = mrYes then
        begin
          SetCodeBlocksState(False);
          if RunElevatedTask(ELEVATED_TASK_CODEBLOCKS_IDE_INITIALIZE_PROFILES) then
            if not LastOperationSuccess then
              MsgBox(DialogErrorTitle, GetLastErrorMessage(LastErrorMessage), mtError, [mbOK]);
        end;
    end;

  SetCodeBlocksState(True);
end;

procedure TfrmMain.btnWindowsTerminalInstallClick(Sender: TObject);
begin
  if MsgBox(DialogQuestionTitle, WindowsTerminalInstallText, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes then
    if not InstallWindowsTerminalIntegration then
      MsgBox(DialogWarningTitle, WindowsTerminalInstallFailedText, mtWarning, [mbOk]);
  UpdateWindowsTerminalControls;
end;

procedure TfrmMain.btnWindowsTerminalUninstallClick(Sender: TObject);
begin
  if MsgBox(DialogQuestionTitle, WindowsTerminalUninstallText, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes then
    if not UninstallWindowsTerminalIntegration then
      MsgBox(DialogWarningTitle, WindowsTerminalUninstallFailedText, mtWarning, [mbOk]);
  UpdateWindowsTerminalControls;
end;

procedure TfrmMain.btnDebugEnvironmentFileSystemPrintAllValuesClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  DreamcastSoftwareDevelopmentKitManager.Environment
    .FileSystem.DebugPrintAllValues;
{$ENDIF}
end;

procedure TfrmMain.btnDebugGetSelectedToolchainClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  ShowMessage(GetSelectedToolchain);
{$ENDIF}
end;

procedure TfrmMain.cbxDreamcastToolSerialBaudrateSelect(Sender: TObject);
begin
  UpdateDreamcastToolSerialOptionControls;
  InstallDreamcastTool;
end;

procedure TfrmMain.cbxDreamcastToolSerialPortSelect(Sender: TObject);
begin
  InstallDreamcastTool;
end;

procedure TfrmMain.cbxModuleSelectionChange(Sender: TObject);
var
  ModuleVersionItem: TModuleVersionItem;
  ValidItemIndex: Boolean;

begin
  edtModuleProductVersion.Text := EmptyStr;
  edtModuleCompiledDate.Text := EmptyStr;
  edtModuleFileVersion.Text := EmptyStr;

  ValidItemIndex := (cbxModuleSelection.ItemIndex > -1) and
    (cbxModuleSelection.ItemIndex < ModuleVersionList.Count);

  if ValidItemIndex then
  begin
    ModuleVersionItem := ModuleVersionList[cbxModuleSelection.ItemIndex];
    if Assigned(ModuleVersionItem) then
    begin
      edtModuleProductVersion.Text := ModuleVersionItem.ProductVersion;
      edtModuleCompiledDate.Text := ModuleVersionItem.BuildDateTime;
      edtModuleFileVersion.Text := ModuleVersionItem.FileVersion;
    end;
  end;
end;

procedure TfrmMain.cbxUrlDreamcastToolIPChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Repositories
    .DreamcastToolInternetProtocolURL := cbxUrlDreamcastToolIP.Text;
end;

procedure TfrmMain.cbxUrlDreamcastToolSerialChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Repositories
    .DreamcastToolSerialURL := cbxUrlDreamcastToolSerial.Text;
end;

procedure TfrmMain.cbxUrlKallistiChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Repositories
    .KallistiURL := cbxUrlKallisti.Text;
end;

procedure TfrmMain.cbxUrlKallistiPortsChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Repositories
    .KallistiPortsURL := cbxUrlKallistiPorts.Text;
end;

procedure TfrmMain.cbxUrlRubyChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Repositories
    .RubyURL := cbxUrlRuby.Text;
end;

procedure TfrmMain.ckxDreamcastToolInternetProtocolUseARPChange(Sender: TObject
  );
begin
  UpdateDreamcastToolMediaAccessControlAddressControls;
  InstallDreamcastTool;
end;

procedure TfrmMain.edtDreamcastToolInternetProtocolAddressChange(Sender: TObject
  );
begin
  HandleInvalidInternetProtocolAddress(False);
  InstallDreamcastTool;
end;

procedure TfrmMain.edtDreamcastToolInternetProtocolMACChange(Sender: TObject);
begin
  HandleInvalidMediaAccessControlAddress(False);
  InstallDreamcastTool;
end;

procedure TfrmMain.edtIdeCodeBlocksInstallDirChange(Sender: TObject);
{$IFDEF DEBUG}
var
  CodeBlocksVersion: string;

begin
  CodeBlocksVersion := CodeBlocksVersionToString(GetCodeBlocksVersion(edtIdeCodeBlocksInstallDir.Text));
  DebugLog('CodeBlocks version: ' + CodeBlocksVersion);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfrmMain.edtPortMaintainerClick(Sender: TObject);
const
  MAIL_TO_URL = 'mailto:%s?subject=%s';

var
  MailTo,
  Subject: string;

  function EncodeUrl(Value: string): string;
  begin
    Result := StringReplace(Value, WhiteSpaceStr, '%20', [rfReplaceAll]);
  end;

begin
  if IsInString('@', edtPortMaintainer.Text) then
  begin
    MailTo := EncodeUrl(edtPortMaintainer.Text);
    Subject := EncodeUrl(Format(MailToSubject, [SelectedKallistiPort.Name]));
    OpenURL(Format(MAIL_TO_URL, [MailTo, Subject]));
  end;
end;

procedure TfrmMain.edtPortURLClick(Sender: TObject);
var
  Url: string;

begin
  Url := '';

  if Sender is TEdit then
    Url := (Sender as TEdit).Text
  else if Sender is TLabeledEdit then
    Url := (Sender as TLabeledEdit).Text
  else
    Url := (Sender as TLabel).Caption;

  if IsInString('http', Url) then
    OpenURL(Url);
end;

procedure TfrmMain.edtPortURLMouseEnter(Sender: TObject);
begin
  with (Sender as TLabeledEdit) do
    if IsInString('@', Text) or IsInString('http', Text) then
    begin
      Font.Underline := True;
      Font.Color := clHotLight;
      Cursor := crHandPoint;
    end;
end;

procedure TfrmMain.edtPortURLMouseLeave(Sender: TObject);
begin
  with (Sender as TLabeledEdit) do
  begin
    Font.Underline := False;
    Font.Color := clDefault;
    Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  SetControlMultilineLabel(ckxDreamcastToolInternetProtocolUseARP);
  if not IsPostInstallMode and IsInstallOrUpdateRequired then
    if MsgBox(DialogQuestionTitle, InstallOrUpdateRequiredDoItNow, mtConfirmation, [mbYes, mbNo]) = mrYes then
      ExecuteThreadOperation(stiKallistiManage);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckComponentsChangeAllowRequestedOperation;
end;

(* This code is running in an elevated thread! *)
function DoElevatedTask(const ATaskName: string; AParameters: TStringList;
  ASourceWindowHandle: THandle): Cardinal;
type
  TElevatedTask = (
    etUnknown,
    etCodeBlocksPatchInstall,
    etCodeBlocksPatchUninstall,
    etCodeBlocksPatchReinstall,
    etCodeBlocksPatchRefresh,
    etCodeBlocksPatchInitializeProfiles
  );

var
  LogContext: TLogMessageContext;
  ElevatedDreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager;
  SwapExchangeFileName: TFileName;
  ParamInstallationDirectory: TFileName;
  LogMessageCommandLineSwitchItemIndex: Integer;

  function TaskNameToElavatedTask: TElevatedTask;
  begin
    Result := etUnknown;
    if (ATaskName = ELEVATED_TASK_CODEBLOCKS_IDE_INSTALL) then
      Result := etCodeBlocksPatchInstall
    else if (ATaskName = ELEVATED_TASK_CODEBLOCKS_IDE_UNINSTALL) then
      Result := etCodeBlocksPatchUninstall
    else if (ATaskName = ELEVATED_TASK_CODEBLOCKS_IDE_REINSTALL) then
      Result := etCodeBlocksPatchReinstall
    else if (ATaskName = ELEVATED_TASK_CODEBLOCKS_IDE_REFRESH) then
      Result := etCodeBlocksPatchRefresh
    else if (ATaskName = ELEVATED_TASK_CODEBLOCKS_IDE_INITIALIZE_PROFILES) then
      Result := etCodeBlocksPatchInitializeProfiles;
  end;

  procedure WriteLibraryInformation;
  begin
    // Handle IDE files
    ElevatedDreamcastSoftwareDevelopmentKitManager.KallistiPorts
      .GenerateIntegratedDevelopmentEnvironmentLibraryInformation;

    LogMessage(LogContext, 'GenerateIntegratedDevelopmentEnvironmentLibraryInformation called');
  end;

begin
  Result := ERROR_SUCCESS;
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try
    (* Remove the "--debug" switch from the AParameters, this already has been
       handled by SysTools, in the initialization section. *)
    LogMessageCommandLineSwitchItemIndex := AParameters.IndexOf(GetLogMessageCommandLineSwitch);
    if LogMessageCommandLineSwitchItemIndex >= 0 then
      AParameters.Delete(LogMessageCommandLineSwitchItemIndex);

    // Get the exchange files
    SwapExchangeFileName := AParameters[0];
    LogMessage(LogContext, Format('Swap File: "%s"', [
      SwapExchangeFileName
    ]));

    ElevatedDreamcastSoftwareDevelopmentKitManager :=
      TDreamcastSoftwareDevelopmentKitManager.Create(False);

    try
      with ElevatedDreamcastSoftwareDevelopmentKitManager
        .IntegratedDevelopmentEnvironment.CodeBlocks do
      begin
        // Load the KallistiOS Ports only as we need that for C::B DreamSDK Project Wizard...
        ElevatedDreamcastSoftwareDevelopmentKitManager.KallistiPorts
          .RetrieveAvailablePorts;

{$IFDEF DEBUG}
        LogMessage(LogContext, GetEnumName(TypeInfo(TElevatedTask),
          Ord(TaskNameToElavatedTask)));
{$ENDIF}

        // Execute the C::B Patcher
        case TaskNameToElavatedTask of
          etCodeBlocksPatchInstall:
            begin
              ParamInstallationDirectory := AParameters[1];
              Install(ParamInstallationDirectory);
              WriteLibraryInformation;
            end;

          etCodeBlocksPatchUninstall:
            Uninstall;

          etCodeBlocksPatchReinstall:
            begin
              Reinstall;
              WriteLibraryInformation;
            end;

          etCodeBlocksPatchRefresh:
            begin
              Refresh(True);
              WriteLibraryInformation;
            end;

          etCodeBlocksPatchInitializeProfiles:
            begin
              InitializeProfiles;
              WriteLibraryInformation;
            end;

          etUnknown:
            begin
              MsgBoxDlg(ASourceWindowHandle, sError, UnknownElevatedTask, mtError, [mbOK]);
              Result := ERROR_GEN_FAILURE;
            end;
        end;

        // Handle the output
        if not LastOperationSuccess then
          SaveStringToFile(Trim(LastErrorMessage), SwapExchangeFileName);
      end;
    finally
      ElevatedDreamcastSoftwareDevelopmentKitManager.Free;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

initialization
  OnElevateProc := @DoElevatedTask;
  CheckForElevatedTask;

end.

