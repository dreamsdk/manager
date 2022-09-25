unit Main;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst, MaskEdit, PortMgr, ShellThd, DCSDKMgr, Environ,
  StrRes, PkgMgr;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    apMain: TApplicationProperties;
    btnAllPortInstall: TButton;
    btnIdeRefresh: TButton;
    btnAllPortUninstall: TButton;
    btnIdeCodeBlocksInstallDir: TButton;
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
    gbxRubyFolder: TGroupBox;
    gbxRubyRunShell: TGroupBox;
    gbxUrlRuby: TGroupBox;
    gbxDebugger: TGroupBox;
    lblComponentsConfiguration: TLabel;
    lblIdeCodeBlocksUsersAvailable: TLabel;
    lblToolchain: TLabel;
    lblBuildDateMRuby: TLabel;
    lblHomeFolder1: TLabel;
    lblRubyShell: TLabel;
    lblIdeCodeBlocksUsersInstalled: TLabel;
    lblTextBuildDateMRuby: TLabel;
    lblTextMRuby: TLabel;
    lblTextRuby: TLabel;
    lblTextRake: TLabel;
    lblTextVersionKallistiOS2: TLabel;
    lblDebugger: TLabel;
    lblVersionMRuby: TLabel;
    lblVersionRuby: TLabel;
    lblVersionRake: TLabel;
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
    lblTextMinGW: TLabel;
    lblTextPython: TLabel;
    lblTextRepoKallistiOS: TLabel;
    lblTextVersionDreamcastToolSerial: TLabel;
    lblTextVersionDreamcastToolIP: TLabel;
    lblTextRepoKallistiPorts: TLabel;
    lblTextSVN: TLabel;
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
    lblVersionMinGW: TLabel;
    lblVersionPython: TLabel;
    lblVersionRepoKallistiOS: TLabel;
    lblVersionRepoKallistiPorts: TLabel;
    lblVersionSVN: TLabel;
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
    rgbDreamcastTool: TRadioGroup;
    rgxTerminalOption: TRadioGroup;
    sddIdeCodeBlocks: TSelectDirectoryDialog;
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
    procedure btnIdeCodeBlocksInstallDirClick(Sender: TObject);
    procedure btnInstallMRubyClick(Sender: TObject);
    procedure btnOfflineKallistiClick(Sender: TObject);
    procedure btnOpenHelpClick(Sender: TObject);
    procedure btnOpenHomeClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnOpenMSYSClick(Sender: TObject);
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbxIdeAllClick(Sender: TObject);
    procedure lbxIdeListClickCheck(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure rbnComponentsNoChangeChange(Sender: TObject);
    procedure rgbDreamcastToolSelectionChanged(Sender: TObject);
    procedure rgxTerminalOptionClick(Sender: TObject);
    procedure tmDisplayKallistiPortsTimer(Sender: TObject);
    procedure tmrShellThreadTerminateTimer(Sender: TObject);
  private
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
    procedure DisplayEnvironmentComponentVersions;
    procedure DisplayKallistiPorts(ClearList: Boolean);
    procedure DoUpdateAll;
    function GetComponentSelectedOperation: TPackageManagerRequest;
    function GetSelectedDebugger: TPackageManagerRequestDebugger;
    function GetSelectedKallistiPort: TKallistiPortItem;
    function GetSelectedKallistiPortItemIndex: Integer;
    function GetSelectedMediaAccessControlHostAddress: string;
    function GetSelectedToolchain: TPackageManagerRequestToolchain;
    procedure LoadConfiguration;
    function BooleanToCaption(Value: Boolean): string;
    function BooleanToCheckboxState(State: Boolean): TCheckBoxState;
    procedure ClearKallistiPortPanel;
    procedure UpdateKallistiPortControls;
    function IsVersionLabelValid(VersionLabel: TLabel): Boolean;
    procedure SetVersionLabelState(VersionLabel: TLabel; Erroneous: Boolean);
    procedure SetVersionLabel(VersionLabel: TLabel; Version: string);
    procedure UpdateComponentControls;
    procedure UpdateDreamcastToolMediaAccessControlAddressControls;
    procedure UpdateDreamcastToolAlternateCheckbox;
    procedure UpdateOptionsControls;
    procedure UpdateRepositories;
    procedure UpdateRubyControls;
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
    procedure FreeNetworkAdapterList;
    function HostMacToItemIndex(const HostMediaAccessControlAddress: string): Integer;
    function HasNetworkAdapters: Boolean;
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
    property SelectedHostMediaAccessControlAddress: string
      read GetSelectedMediaAccessControlHostAddress;
    property ComponentSelectedToolchain: TPackageManagerRequestToolchain read
      GetSelectedToolchain;
    property ComponentSelectedDebugger: TPackageManagerRequestDebugger read
      GetSelectedDebugger;
    property ComponentSelectedOperation: TPackageManagerRequest
      read GetComponentSelectedOperation;
  end;

var
  frmMain: TfrmMain;
  DreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager;
  PackageManager: TPackageManager;

implementation

{$R *.lfm}

uses
  LCLIntf, IniFiles, StrUtils, FPHttpClient, OpenSSLSockets,
  UITools, GetVer, SysTools, PostInst, Settings,
  Version, VerIntf, About, UxTheme, MsgDlg, Progress, ModVer, InetUtil,
  RunTools, RefBase, Elevate, FSTools, Unpack, CBTools;

const
  KALLISTI_VERSION_FORMAT = '%s (%s)';
  UNKNOWN_VALUE = '(Unknown)';

  ELEVATED_TASK_CODEBLOCKS_IDE_INSTALL = 'elevated_task_cb_ide_install';
  ELEVATED_TASK_CODEBLOCKS_IDE_REINSTALL = 'elevated_task_cb_ide_reinstall';
  ELEVATED_TASK_CODEBLOCKS_IDE_UNINSTALL = 'elevated_task_cb_ide_uninstall';
  ELEVATED_TASK_CODEBLOCKS_IDE_REFRESH = 'elevated_task_cb_ide_refresh';
  ELEVATED_TASK_CODEBLOCKS_IDE_INITIALIZE_PROFILES = 'elevated_task_cb_ide_initialize';

type
  { TNetworkAdapterListUserInterfaceItem }
  TNetworkAdapterListUserInterfaceItem = class(TObject)
  private
    fMacAddress: string;
  public
    constructor Create(AMacAddress: string);
    property MacAddress: string read fMacAddress;
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

{ TNetworkAdapterListUserInterfaceItem }

constructor TNetworkAdapterListUserInterfaceItem.Create(AMacAddress: string);
begin
  fMacAddress := AMacAddress;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if not IsElevatedTaskRequested then
  begin
    fShellThreadExecutedAtLeastOnce := False;
    DreamcastSoftwareDevelopmentKitManager :=
      TDreamcastSoftwareDevelopmentKitManager.Create(not IsPostInstallMode);
    PackageManager := TPackageManager.Create(DreamcastSoftwareDevelopmentKitManager);
    PackageManager.OnTerminate := @OnPackageManagerTerminate;
    HelpFileName := DreamcastSoftwareDevelopmentKitManager.Environment
      .FileSystem.Shell.HelpFileName;

    if (not IsPostInstallMode) then
    begin
      ModuleVersionList := CreateModuleVersionList;
      CreateNetworkAdapterList;
    end;

    DoubleBuffered := True;
    pcMain.TabIndex := 0;
    if IsWindowsVistaOrGreater and IsElevated then
      Caption := Format(ElevatedCaption, [Caption]);
    Application.Title := Caption;
    HandleAero;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if not IsElevatedTaskRequested then
  begin
    if not fShellThreadExecutedAtLeastOnce then
    begin
      DreamcastSoftwareDevelopmentKitManager.KallistiPorts
        .GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
    end;

    if (not IsPostInstallMode) then
    begin
      FreeNetworkAdapterList;
      ModuleVersionList.Free;
    end;

    PackageManager.Free;
    DreamcastSoftwareDevelopmentKitManager.Free;
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

  if pcMain.ActivePage <> tsComponents then
    UpdateComponentControls;
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

procedure TfrmMain.rgbDreamcastToolSelectionChanged(Sender: TObject);
begin
  InstallDreamcastTool;
  RefreshViewDreamcastTool;
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
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

    lbxPorts.Clear;
    for i := 0 to DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Count - 1 do
    begin
      PortInfo := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[i];
      if not PortInfo.Hidden then
      begin
        j := lbxPorts.Items.Add(PortInfo.Name);
        lbxPorts.Items.Objects[j] := TObject(i);
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
      j := Integer(lbxPorts.Items.Objects[i]);
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
  IsSinglePortRefreshOnly: Boolean;

begin
{$IFDEF DEBUG}
  WriteLn('[ShellThreadTerminateTimer::START] ShellThreadSuccess: ', fShellThreadSuccess,
    ', ShellThreadOutputResult: ', fShellThreadOutputResult);
{$ENDIF}

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

{$IFDEF DEBUG}
  DebugLog('[ShellThreadTerminateTimer::END]');
{$ENDIF}
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

procedure TfrmMain.DisplayEnvironmentComponentVersions;
var
  ComponentName: TComponentName;
  ComponentVersion, ComponentNameString: string;

  procedure SetLabelDate(LabelCtrl: TLabel; const FileName: TFileName;
    const FileDate: TDateTime);
  begin
    if FileExists(FileName) then
      LabelCtrl.Caption := FormatDateTime(STRING_DATE_FORMAT, FileDate);
  end;

begin
  with DreamcastSoftwareDevelopmentKitManager do
  begin
    // Components versions
    for ComponentName := Low(TComponentName) to High(TComponentName) do
    begin
      ComponentNameString := ComponentNameToString(ComponentName);
      ComponentVersion := Versions.GetComponentVersion(ComponentName);
      SetVersionLabel(FindComponent('lblVersion' + ComponentNameString) as TLabel,
        ComponentVersion);
    end;

    // KallistiOS ChangeLog version
    if KallistiOS.Built then
    begin
      SetVersionLabel(lblVersionKallistiOS, Format(KALLISTI_VERSION_FORMAT, [
        lblVersionKallistiOS.Caption,
        Versions.KallistiChangeLog
      ]));
      SetVersionLabel(lblVersionKallistiOS2, lblVersionKallistiOS.Caption);
    end
    else
    begin
      SetVersionLabel(lblVersionKallistiOS2, INVALID_VERSION);
    end;

    // KallistiOS build date
    lblBuildDateKallistiOS.Caption := EmptyStr;
    SetLabelDate(lblBuildDateKallistiOS,
      Environment.FileSystem.Kallisti.KallistiLibrary,
      Versions.KallistiBuildDate);

    // KallistiOS changes log display
    memKallistiChangeLog.Lines.Clear;
    if FileExists(Environment.FileSystem.Kallisti.KallistiChangeLogFile) then
      memKallistiChangeLog.Lines.LoadFromFile(
        Environment.FileSystem.Kallisti.KallistiChangeLogFile);

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
    lblBuildDateMRuby.Caption := EmptyStr;
    SetLabelDate(lblBuildDateMRuby,
      Environment.FileSystem.Ruby.RubyLibrary,
      Versions.MRubyBuildDate);
  end;
end;

procedure TfrmMain.DisplayKallistiPorts(ClearList: Boolean);
begin
  fKallistiPortsClearList := ClearList;
  tmDisplayKallistiPorts.Enabled := True;
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

function TfrmMain.GetSelectedDebugger: TPackageManagerRequestDebugger;
begin
  Result := TPackageManagerRequestDebugger(cbxDebugger.ItemIndex);
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
    Result := Integer(lbxPorts.Items.Objects[Result]);
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

function TfrmMain.GetSelectedToolchain: TPackageManagerRequestToolchain;
begin
  Result := TPackageManagerRequestToolchain(cbxToolchain.ItemIndex);
end;

procedure TfrmMain.LoadConfiguration;
begin
  with DreamcastSoftwareDevelopmentKitManager.Environment.Settings do
  begin
    // Settings
    if UseMinTTY then
      rgxTerminalOption.ItemIndex := 1;

    // Dreamcast Tool
    cbxDreamcastToolSerialPort.ItemIndex := Integer(DreamcastTool.SerialPort);
    cbxDreamcastToolSerialBaudrate.ItemIndex := Integer(DreamcastTool.SerialBaudrate);
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
    rgbDreamcastTool.ItemIndex := Integer(DreamcastTool.Kind);
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
begin
  if Erroneous then
  begin
    VersionLabel.Font.Color := clRed;
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
  ValidVersion := IsVersionValid(Version);
  if not ValidVersion then
    VersionLabel.Caption := BooleanToCaption(False)
  else
    VersionLabel.Caption := Version;
  SetVersionLabelState(VersionLabel, not ValidVersion);
{$IFDEF DEBUG}
  WriteLn(VersionLabel.Name, ': ', VersionLabel.Caption);
{$ENDIF}
end;

procedure TfrmMain.UpdateComponentControls;
var
  DebuggerPackage: TDebuggerVersionKind;
  ToolchainPackage: TToolchainVersionKind;

begin
  rbnComponentsNoChange.Checked := True;

  // Debugger Package
  DebuggerPackage := DreamcastSoftwareDevelopmentKitManager.Versions
    .ToolchainSuperH.PackageGDB;
  if DebuggerPackage <> dvkUndefined then
    cbxDebugger.ItemIndex := Integer(DebuggerPackage) - 1; // -1 for Undefined

  // Toolchain Package
  ToolchainPackage := DreamcastSoftwareDevelopmentKitManager.Versions
    .ToolchainSuperH.PackageToolchain;
  if ToolchainPackage <> tvkUndefined then
    cbxToolchain.ItemIndex := Integer(ToolchainPackage) - 1; // -1 for Undefined
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
  Kind := rgbDreamcastTool.ItemIndex;
  gbxDreamcastToolSerial.Enabled := (Kind = 1);
  gbxDreamcastToolInternetProtocol.Enabled := (Kind = 2);
  gbxDreamcastToolCommon.Enabled := (Kind = 1) or (Kind = 2);
  gbxDreamcastToolCustomCommand.Enabled := (Kind = 3);
  UpdateDreamcastToolAlternateCheckbox;
  UpdateDreamcastToolMediaAccessControlAddressControls;
end;

procedure TfrmMain.UpdateDreamcastToolAlternateCheckbox;
begin
  ckxDreamcastToolSerialAlternateBaudrate.Enabled := (gbxDreamcastToolSerial.Enabled)
    and (cbxDreamcastToolSerialBaudrate.ItemIndex = 8);
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

procedure TfrmMain.InstallDreamcastTool;
begin
  if not fLoadingConfiguration then
  begin
    with DreamcastSoftwareDevelopmentKitManager.Environment.Settings.DreamcastTool do
    begin
      AttachConsoleFileserver := ckxDreamcastToolAttachConsoleFileServer.Checked;
      ClearScreenBeforeDownload := ckxDreamcastToolClearScreenBeforeDownload.Checked;
      Kind := TDreamcastToolKind(rgbDreamcastTool.ItemIndex);
      SerialBaudrate := TDreamcastToolSerialBaudrate(cbxDreamcastToolSerialBaudrate.ItemIndex);
      SerialBaudrateAlternate := ckxDreamcastToolSerialAlternateBaudrate.Checked;
      SerialDumbTerminal := ckxDreamcastToolSerialDumbTerminal.Checked;
      SerialPort := TDreamcastToolSerialPort(cbxDreamcastToolSerialPort.ItemIndex);
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
  edtValueHomeBaseDir.Caption := GetInstallationBaseDirectory;
  UpdateComponentControls;
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

  lbxIdeList.ItemIndex := 0;
  lbxIdeListClickCheck(Self);
end;

procedure TfrmMain.InitializeOptionsScreen;
begin
{$IFDEF DEBUG}
  DebugLog('InitializeOptionsScreen');
{$ENDIF}
  with DreamcastSoftwareDevelopmentKitManager.Environment.Settings.Repositories do
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
     (DreamcastSoftwareDevelopmentKitManager.Versions.SubversionInstalled) then
       Result := mtConfirmation;
end;

function TfrmMain.GetAllKallistiPortsMessage(const Message: string): string;
begin
  Result := Message;
  if (not DreamcastSoftwareDevelopmentKitManager.Versions.SubversionInstalled) then
     Result := Message + MsgBoxDlgWrapStr + UseSubversionAllKallistiPorts;
end;

function TfrmMain.CheckKallistiSinglePortPossibleInstallation: Boolean;
begin
  Result := True;
  if SelectedKallistiPort.UseSubversion
    and (not DreamcastSoftwareDevelopmentKitManager.Versions.SubversionInstalled) then
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
          NetworkCardName, TNetworkAdapterListUserInterfaceItem.Create(MacAddress));
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

function TfrmMain.HasNetworkAdapters: Boolean;
begin
  Result := cbxDreamcastToolInternetProtocolNetworkAdapter.Items.Count > 0;
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
  SwapExchangeFileName: TFileName;
  ElevatedParameters: string;

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
  SwapExchangeFileName := GetTemporaryFileName;

  StartWait;
  try
    AParameters.Insert(0, SwapExchangeFileName);
    ElevatedParameters := EncodeParameters(AParameters);
    SetLastError(RunElevated(ATaskName, ElevatedParameters, Handle,
      @Application.ProcessMessages));

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
  SelectedPythonVersion,
  MessageText: string;

begin
  SelectedPythonVersion := EmptyStr;
  IsValidPythonVersionSelected := IsDebuggerPythonVersionInstalled(
    ComponentSelectedDebugger, SelectedPythonVersion);

{$IFDEF DEBUG}
  WriteLn('Package Manager Operation: ', ComponentSelectedOperation);
  WriteLn('  Selected Toolchain: ', ComponentSelectedToolchain);
  WriteLn('  Selected Debugger: ', ComponentSelectedDebugger, ' [', IsValidPythonVersionSelected, ']');
{$ENDIF}

  MessageText := UnpackConfirmationText;
  if not IsValidPythonVersionSelected then
    MessageText := Format(UnpackInvalidPythonConfirmationText, [SelectedPythonVersion]);

  if (MsgBox(DialogWarningTitle, MessageText, mtWarning, [mbYes, mbNo], mbNo) = mrYes) then
    with PackageManager do
    begin
      Debugger := ComponentSelectedDebugger;
      Toolchain := ComponentSelectedToolchain;
      Operation := ComponentSelectedOperation;
      Execute;
    end;
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
      (not DreamcastSoftwareDevelopmentKitManager.Versions.RakeInstalled) then
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
const
  VALID_PREFIX = 'http';

var
  RemoteVersion,
  Url: string;

  function GetRemoteVersion: string;
  const
    UPDATE_FILE_CHECK = '.update/version.txt';

  var
    HTTPClient: TFPHTTPClient;
    UpdateUrl: string;

  begin
    UpdateUrl := Concat(Url, UPDATE_FILE_CHECK);
    try
      HTTPClient := TFPHTTPClient.Create(nil);
      try
        HTTPClient.AllowRedirect := True;
        Result := Trim(HTTPClient.Get(UpdateUrl));
      finally
        HTTPClient.Free;
      end;
    except
      Result := EmptyStr;
    end;
  end;

begin
  Url := GetComments;
  if AnsiStartsStr(VALID_PREFIX, LowerCase(Url)) then
  begin
    RemoteVersion := GetRemoteVersion;

    if IsEmpty(RemoteVersion) then
      MsgBox(DialogWarningTitle, MsgBoxDlgTranslateString(Format(UnableToRetrieveRemotePackageVersion, [GetProductName])), mtWarning, [mbOk])
    else
    begin
      if CompareVersion(FullVersionNumber, RemoteVersion) > 0 then
      begin
        if MsgBox(DialogQuestionTitle, Format(PackageUpdateAvailable, [GetProductName, RemoteVersion]), mtConfirmation, [mbYes, mbNo]) = mrYes then
          OpenURL(Url);
      end
      else
        MsgBox(DialogInformationTitle, Format(PackageUpToDate, [GetProductName]), mtInformation, [mbOk]);
    end;
  end
  else
{$IFDEF DEBUG}
    raise Exception.Create('Invalid website in the File Comments!')
{$ELSE}
    MsgBox(DialogErrorTitle, ApplicationNotCorrectlyConfigured, mtError, [mbOk])
{$ENDIF}
  ;
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.MinGWGetExecutable);
end;

procedure TfrmMain.btnOpenMSYSClick(Sender: TObject);
begin
  RunMSYS;
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
    ResetText(cbxUrlKallisti, DEFAULT_KALLISTI_URL);
    ResetText(cbxUrlKallistiPorts, DEFAULT_KALLISTI_PORTS_URL);
    ResetText(cbxUrlDreamcastToolSerial, DEFAULT_DREAMCAST_TOOL_SERIAL_URL);
    ResetText(cbxUrlDreamcastToolIP, DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL);
    ResetText(cbxUrlRuby, DEFAULT_RUBY_URL);

    // Dreamcast Tool (only Options...)
    rgbDreamcastTool.ItemIndex := DREAMCAST_TOOL_DEFAULT_KIND;
    rgbDreamcastToolSelectionChanged(Self);
    ckxDreamcastToolAttachConsoleFileServer.Checked := DREAMCAST_TOOL_DEFAULT_ATTACH_CONSOLE_FILESERVER;
    ckxDreamcastToolClearScreenBeforeDownload.Checked := DREAMCAST_TOOL_DEFAULT_CLEAR_SCREEN_BEFORE_DOWNLOAD;
//    edtDreamcastToolInternetProtocolAddress.Text := DREAMCAST_TOOL_DEFAULT_INTERNET_PROTOCOL_ADDRESS;
    ckxDreamcastToolInternetProtocolUseARP.Checked := DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ENABLED;
//    edtDreamcastToolInternetProtocolMAC.Text := DREAMCAST_TOOL_DEFAULT_MEDIA_ACCESS_CONTROL_ADDRESS;
    ckxDreamcastToolSerialDumbTerminal.Checked := DREAMCAST_TOOL_DEFAULT_SERIAL_DUMB_TERMINAL;
    ckxDreamcastToolSerialExternalClock.Checked := DREAMCAST_TOOL_DEFAULT_SERIAL_EXTERNAL_CLOCK;
    cbxDreamcastToolSerialBaudrate.ItemIndex := DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE;
    ckxDreamcastToolSerialAlternateBaudrate.Checked := DREAMCAST_TOOL_DEFAULT_SERIAL_BAUDRATE_ALTERNATE;
//    cbxDreamcastToolSerialPort.ItemIndex := DREAMCAST_TOOL_DEFAULT_SERIAL_PORT;
//    edtDreamcastToolCustomExecutable.Text := DREAMCAST_TOOL_DEFAULT_CUSTOM_EXECUTABLE;
//    edtDreamcastToolCustomArguments.Text := DREAMCAST_TOOL_DEFAULT_CUSTOM_ARGUMENTS;

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

procedure TfrmMain.cbxDreamcastToolSerialBaudrateSelect(Sender: TObject);
begin
  UpdateDreamcastToolAlternateCheckbox;
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
  ElevatedDreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager;
  SwapExchangeFileName: TFileName;
  ParamInstallationDirectory: TFileName;

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
  end;

begin
  Result := ERROR_SUCCESS;
  SwapExchangeFileName := AParameters[0];
  ElevatedDreamcastSoftwareDevelopmentKitManager :=
    TDreamcastSoftwareDevelopmentKitManager.Create(False);
  try
    with ElevatedDreamcastSoftwareDevelopmentKitManager
      .IntegratedDevelopmentEnvironment.CodeBlocks do
    begin
      // Load the KallistiOS Ports only as we need that for C::B DreaSDK Project Wizard...
      ElevatedDreamcastSoftwareDevelopmentKitManager.KallistiPorts
        .RetrieveAvailablePorts;

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
end;

initialization
  OnElevateProc := @DoElevatedTask;
  CheckForElevatedTask;

end.

