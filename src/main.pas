unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst, MaskEdit, PortMgr, ShellThd, DCSDKMgr, Environ,
  StrRes;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    apMain: TApplicationProperties;
    btnAllPortInstall: TButton;
    btnAllPortUninstall: TButton;
    btnClose: TButton;
    btnCheckForUpdates: TButton;
    btnOpenHelp: TButton;
    btnOpenHome: TButton;
    btnOpenMinGWManager: TButton;
    btnOpenMSYS: TButton;
    btnPortInstall: TButton;
    btnPortUninstall: TButton;
    btnPortUpdate: TButton;
    btnRestoreDefaults: TButton;
    btnUpdateKallistiOS: TButton;
    btnCredits: TButton;
    btnDreamcastToolCustomExecutable: TButton;
    cbxDreamcastToolSerialBaudrate: TComboBox;
    cbxDreamcastToolSerialPort: TComboBox;
    cbxUrlDreamcastToolIP: TComboBox;
    cbxUrlDreamcastToolSerial: TComboBox;
    cbxUrlKallisti: TComboBox;
    cbxUrlKallistiPorts: TComboBox;
    ckxDreamcastToolInternetProtocolUseARP: TCheckBox;
    ckxDreamcastToolAttachConsoleFileServer: TCheckBox;
    ckxDreamcastToolClearScreenBeforeDownload: TCheckBox;
    ckxDreamcastToolSerialAlternateBaudrate: TCheckBox;
    ckxDreamcastToolSerialDumbTerminal: TCheckBox;
    ckxDreamcastToolSerialExternalClock: TCheckBox;
    cbxModuleSelection: TComboBox;
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
    gbxModuleInfo: TGroupBox;
    gbxAvailablePorts: TGroupBox;
    gbxCompilerInfo: TGroupBox;
    gbxDependencies: TGroupBox;
    gbxDreamcastTool: TGroupBox;
    gbxDreamcastToolCommon: TGroupBox;
    gbxDreamcastToolInternetProtocol: TGroupBox;
    gbxDreamcastToolSerial: TGroupBox;
    gbxKallistiChangeLog: TGroupBox;
    gbxPortAll: TGroupBox;
    gbxPortDetails: TGroupBox;
    gbxToolchain: TGroupBox;
    gbxToolchain1: TGroupBox;
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
    lblTextBinutilsARM: TLabel;
    lblTextBuildDateKallistiOS: TLabel;
    lblTextGCC: TLabel;
    lblTextGCCARM: TLabel;
    lblTextGDB: TLabel;
    lblTextGit: TLabel;
    lblTextKallistiOS: TLabel;
    lblTextMinGW: TLabel;
    lblTextNewlib: TLabel;
    lblTextPython: TLabel;
    lblTextSVN: TLabel;
    lblTextToolIP: TLabel;
    lblTextToolSerial: TLabel;
    lblTitleAbout: TLabel;
    lblTitleHome: TLabel;
    lblVersionBinutils: TLabel;
    lblVersionBinutilsARM: TLabel;
    lblVersionGCC: TLabel;
    lblVersionGCCARM: TLabel;
    lblVersionGDB: TLabel;
    lblVersionGit: TLabel;
    lblVersionKallistiOS: TLabel;
    lblVersionMinGW: TLabel;
    lblVersionNewlib: TLabel;
    lblVersionPython: TLabel;
    lblVersionSVN: TLabel;
    lblVersionToolIP: TLabel;
    lblVersionToolSerial: TLabel;
    lbxPorts: TCheckListBox;
    memKallistiChangeLog: TMemo;
    memPortDescription: TMemo;
    memPortShortDescription: TMemo;
    opdDreamcastToolCustom: TOpenDialog;
    pnlAbout: TPanel;
    pcMain: TPageControl;
    pnlActions: TPanel;
    rgbDreamcastTool: TRadioGroup;
    rgxTerminalOption: TRadioGroup;
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
    procedure btnCheckForUpdatesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCreditsClick(Sender: TObject);
    procedure btnOpenHelpClick(Sender: TObject);
    procedure btnOpenHomeClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnOpenMSYSClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure btnPortUninstallClick(Sender: TObject);
    procedure btnPortUpdateClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure btnUpdateKallistiOSClick(Sender: TObject);
    procedure btnDreamcastToolCustomExecutableClick(Sender: TObject);
    procedure cbxDreamcastToolSerialBaudrateSelect(Sender: TObject);
    procedure cbxDreamcastToolSerialPortSelect(Sender: TObject);
    procedure cbxModuleSelectionChange(Sender: TObject);
    procedure cbxUrlDreamcastToolIPChange(Sender: TObject);
    procedure cbxUrlDreamcastToolSerialChange(Sender: TObject);
    procedure cbxUrlKallistiChange(Sender: TObject);
    procedure cbxUrlKallistiPortsChange(Sender: TObject);
    procedure ckxDreamcastToolInternetProtocolUseARPChange(Sender: TObject);
    procedure edtDreamcastToolInternetProtocolAddressChange(Sender: TObject);
    procedure edtDreamcastToolInternetProtocolMACChange(Sender: TObject);
    procedure edtPortMaintainerClick(Sender: TObject);
    procedure edtPortURLClick(Sender: TObject);
    procedure edtPortURLMouseEnter(Sender: TObject);
    procedure edtPortURLMouseLeave(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure rgbDreamcastToolSelectionChanged(Sender: TObject);
    procedure rgxTerminalOptionClick(Sender: TObject);
    procedure tmDisplayKallistiPortsTimer(Sender: TObject);
    procedure tmrShellThreadTerminateTimer(Sender: TObject);
  private
    fLoadingConfiguration: Boolean;
    fKallistiPortsClearList: Boolean;
    fShellThreadSuccess: Boolean;
    fShellThreadInputRequest: TShellThreadInputRequest;
    fShellThreadOutputResult: TShellThreadOutputResponse;
    fShellThreadUpdateState: TUpdateOperationState;
    procedure DisplayEnvironmentComponentVersions;
    procedure DisplayKallistiPorts(ClearList: Boolean);
    function GetSelectedKallistiPort: TKallistiPortItem;
    function GetSelectedKallistiPortItemIndex: Integer;
    procedure LoadConfiguration;
    function BooleanToCaption(Value: Boolean): string;
    function BooleanToCheckboxState(State: Boolean): TCheckBoxState;
    procedure ClearKallistiPortPanel;
    procedure UpdateKallistiPortControls;
    procedure SetVersionLabelState(VersionLabel: TLabel; Erroneous: Boolean);
    procedure SetVersionLabel(VersionLabel: TLabel; Version: string);
    procedure UpdateDreamcastToolMediaAccessControlAddressControls;
    procedure UpdateDreamcastToolAlternateCheckbox;
    procedure UpdateOptionsControls;
    procedure InstallDreamcastTool;
    procedure HandleInvalidInternetProtocolAddress(const InvalidMaskFormat: Boolean);
    procedure HandleInvalidMediaAccessControlAddress(const InvalidMaskFormat: Boolean);
    procedure LoadRepositoriesSelectionList;
    procedure InitializeAboutScreen;
    procedure InitializeHomeScreen;
    procedure HandleAero;
    function GetMsgBoxWindowHandle: THandle;
  public
    procedure RefreshViewDreamcastTool;
    procedure RefreshViewKallistiPorts(ForceRefresh: Boolean);
    procedure RefreshViewEnvironment(ForceRefresh: Boolean);
    procedure RefreshEverything(ForceRefresh: Boolean);
    procedure OnCommandTerminateThread(Sender: TObject;
      Request: TShellThreadInputRequest; Response: TShellThreadOutputResponse;
      Success: Boolean; UpdateState: TUpdateOperationState);
    function MsgBox(const aCaption: string; const aMsg: string;
      DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn): TModalResult; overload;
    function MsgBox(const aCaption: string; const aMsg: string;
      DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult; overload;
    property SelectedKallistiPortItemIndex: Integer
      read GetSelectedKallistiPortItemIndex;
    property SelectedKallistiPort: TKallistiPortItem
      read GetSelectedKallistiPort;
  end;

var
  frmMain: TfrmMain;
  DreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager;

implementation

{$R *.lfm}

uses
  LCLIntf, IniFiles, StrUtils, UITools, GetVer, SysTools, PostInst, Settings,
  Version, VerIntf, About, UxTheme, MsgDlg, Progress, ModVer;

const
  HELPFILE = 'dreamsdk.chm';
  KALLISTI_VERSION_FORMAT = '%s (%s)';
  UNKNOWN_VALUE = '(Unknown)';

var
  HelpFileName: TFileName;
  ModuleVersionList: TModuleVersionList;

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

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager := TDreamcastSoftwareDevelopmentKitManager.Create;
  ModuleVersionList := CreateModuleVersionList;
  DoubleBuffered := True;
  pcMain.TabIndex := 0;
  Application.Title := Caption;
  HandleAero;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ModuleVersionList.Free;
  DreamcastSoftwareDevelopmentKitManager.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Cursor := crHourGlass;
  InitializeHomeScreen;
  InitializeAboutScreen;
  fLoadingConfiguration := True;
  LoadRepositoriesSelectionList;
  LoadConfiguration;
  RefreshEverything(False);
  DisplayKallistiPorts(True);
  fLoadingConfiguration := False;
  Cursor := crDefault;
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
      '  Includes: ' + SelectedKallistiPort.Includes + sLineBreak +
      '  Libraries: ' + SelectedKallistiPort.Libraries + sLineBreak +
      '    Weights: ' + SelectedKallistiPort.LibraryWeights + sLineBreak
    );
{$ENDIF}
  end;
end;

procedure TfrmMain.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsOptions then
    UpdateOptionsControls;
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
  IsGlobalRefreshViewNeeded: Boolean;

begin
  tmrShellThreadTerminate.Enabled := False;

  if fShellThreadSuccess then
  begin
    case fShellThreadOutputResult of
      // No action was done
      stoNothing:
        MsgBox(DialogInformationTitle, UpdateProcessEverythingUpdate, mtInformation, [mbOk]);

      // KallistiOS was installed
      stoKallistiInstall:
        MsgBox(DialogInformationTitle, Format(UpdateProcessInstallSuccessText, [KallistiText]), mtInformation, [mbOk]);

      // KallistiOS, KallistiOS Ports or Dreamcast Tool were updated
      stoKallistiUpdate:
        case fShellThreadUpdateState of
          uosUpdateSuccess:
            MsgBox(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [KallistiText]), mtInformation, [mbOk]);
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
    end;

    // Handle IDE files
    DreamcastSoftwareDevelopmentKitManager.KallistiPorts
      .GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
  end;

  if not IsPostInstallMode then
  begin
    IsGlobalRefreshViewNeeded := (fShellThreadOutputResult = stoKallistiInstall)
      or (fShellThreadOutputResult = stoKallistiUpdate)
      or (fShellThreadOutputResult = stoKallistiPortsInstall)
      or (fShellThreadOutputResult = stoKallistiPortsUninstall);

    if IsGlobalRefreshViewNeeded then
      RefreshEverything(True)
    else
      RefreshViewKallistiPorts(False); // Single KallistiPorts change
  end;

  Cursor := crDefault;
end;

procedure TfrmMain.DisplayEnvironmentComponentVersions;
var
  ComponentName: TComponentName;
  ComponentVersion, ComponentNameString: string;

begin
  // Components versions
  for ComponentName := Low(TComponentName) to High(TComponentName) do
  begin
    ComponentNameString := ComponentNameToString(ComponentName);
    ComponentVersion := DreamcastSoftwareDevelopmentKitManager.Versions
      .GetComponentVersion(ComponentName);
    SetVersionLabel(FindComponent('lblVersion' + ComponentNameString) as TLabel,
      ComponentVersion);
  end;

  // KallistiOS ChangeLog version
  if DreamcastSoftwareDevelopmentKitManager.KallistiOS.Built then
  begin
    lblVersionKallistiOS.Caption := Format(KALLISTI_VERSION_FORMAT,
      [lblVersionKallistiOS.Caption,
      DreamcastSoftwareDevelopmentKitManager.Versions.KallistiChangeLog]);
  end;

  // KallistiOS build date
  lblBuildDateKallistiOS.Caption := '';
  if FileExists(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Kallisti.KallistiLibrary) then
    lblBuildDateKallistiOS.Caption := FormatDateTime(STRING_DATE_FORMAT,
      DreamcastSoftwareDevelopmentKitManager.Versions.KallistiBuildDate);

  // KallistiOS changes log display
  memKallistiChangeLog.Lines.Clear;
  if FileExists(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Kallisti.KallistiChangeLogFile) then
    memKallistiChangeLog.Lines.LoadFromFile(DreamcastSoftwareDevelopmentKitManager
      .Environment.FileSystem.Kallisti.KallistiChangeLogFile);
end;

procedure TfrmMain.DisplayKallistiPorts(ClearList: Boolean);
begin
  fKallistiPortsClearList := ClearList;
  tmDisplayKallistiPorts.Enabled := True;
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

procedure TfrmMain.LoadConfiguration;
begin
  with DreamcastSoftwareDevelopmentKitManager.Environment.Settings do
  begin
    // Settings
    if UseMinTTY then
      rgxTerminalOption.ItemIndex := 1;

    // Repositories
    cbxUrlKallisti.Text := Repositories.KallistiURL;
    cbxUrlKallistiPorts.Text := Repositories.KallistiPortsURL;
    cbxUrlDreamcastToolSerial.Text := Repositories.DreamcastToolSerialURL;
    cbxUrlDreamcastToolIP.Text := Repositories.DreamcastToolInternetProtocolURL;

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
    btnPortUpdate.Enabled := SelectedKallistiPort.Installed;
  end
  else
  begin
    btnPortInstall.Enabled := False;
    btnPortUninstall.Enabled := False;
    btnPortUpdate.Enabled := False;
  end;
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
    cbxUrlKallisti.Enabled := not KallistiOS.RepositoryReady;
    cbxUrlKallistiPorts.Enabled := not KallistiPorts.RepositoryReady;
    cbxUrlDreamcastToolSerial.Enabled := not DreamcastTool.RepositoryReadySerial;
    cbxUrlDreamcastToolIP.Enabled := not DreamcastTool.RepositoryReadyInternetProtocol;
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
      MediaAccessControlAddress := edtDreamcastToolInternetProtocolMAC.Text;
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
      edtProductVersion.Text := IniFile.ReadString(VERSION_SECTION_NAME, 'Release', UNKNOWN_VALUE);
      edtProductBuildDate.Text := IniFile.ReadString(VERSION_SECTION_NAME, 'Date', UNKNOWN_VALUE);
    finally
      IniFile.Free;
    end;
  end;

  procedure DisplayHelpFileVersion;
  var
    HelpFileVersion: string;

  begin
    HelpFileVersion := RetrieveVersionWithFind(HelpFileName, 'DreamSDK Help', sLineBreak);
    HelpFileVersion := Right('Ver. ', HelpFileVersion);
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

procedure TfrmMain.HandleAero;
var
  i: Integer;

begin
  if IsAeroEnabled or UseThemes then
  begin
    for i := 0 to frmMain.ComponentCount - 1 do
      if (frmMain.Components[i] is TLabeledEdit) then
        (frmMain.Components[i] as TLabeledEdit).Color := clDefault;
    memPortShortDescription.Color := clDefault;
  end;
end;

function TfrmMain.GetMsgBoxWindowHandle: THandle;
begin
  Result := Handle;
  if IsPostInstallMode then
    Result := frmProgress.Handle;
end;

procedure TfrmMain.RefreshViewKallistiPorts(ForceRefresh: Boolean);

  procedure RefreshKallistiPortsControls;
  var
    i, PortsCount, InstalledPortsCount: Integer;

  begin
    PortsCount := DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Count;

    InstalledPortsCount := 0;
    for i := 0 to PortsCount - 1 do
      if DreamcastSoftwareDevelopmentKitManager.KallistiPorts[i].Installed then
        Inc(InstalledPortsCount);

    btnAllPortInstall.Enabled := (PortsCount <> InstalledPortsCount);
    btnAllPortUninstall.Enabled := (InstalledPortsCount > 0);
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

  RefreshViewEnvironment(ForceRefresh);
  RefreshViewKallistiPorts(ForceRefresh);
  RefreshViewDreamcastTool;
end;

procedure TfrmMain.OnCommandTerminateThread(Sender: TObject;
  Request: TShellThreadInputRequest; Response: TShellThreadOutputResponse;
  Success: Boolean; UpdateState: TUpdateOperationState);
begin
  Cursor := crHourGlass;
  Application.ProcessMessages;
  fShellThreadSuccess := Success;
  fShellThreadInputRequest := Request;
  fShellThreadOutputResult := Response;
  fShellThreadUpdateState := UpdateState;
  tmrShellThreadTerminate.Enabled := True;
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

procedure TfrmMain.btnOpenHelpClick(Sender: TObject);
begin
  RunShellExecute(HelpFileName);
end;

procedure TfrmMain.btnOpenHomeClick(Sender: TObject);
begin
  RunShellExecute(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.HomeDirectory);
end;

procedure TfrmMain.btnAllPortInstallClick(Sender: TObject);
begin
  if MsgBox(DialogQuestionTitle, InstallAllKallistiPorts, mtConfirmation, [mbYes, mbNo]) = mrYes then
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
  end;
end;

procedure TfrmMain.btnAllPortUninstallClick(Sender: TObject);
begin
  if MsgBox(DialogWarningTitle, UninstallAllKallistiPorts, mtWarning, [mbYes, mbNo], mbNo) = mrYes then
    ExecuteThreadOperation(stiKallistiPortsUninstall);
end;

procedure TfrmMain.btnCheckForUpdatesClick(Sender: TObject);
const
  VALID_PREFIX = 'http';

var
  Url: string;

begin
  Url := GetComments;
  if AnsiStartsStr(VALID_PREFIX, LowerCase(Url)) then
    OpenURL(GetComments)
{$IFDEF DEBUG}
  else
    raise Exception.Create('Invalid website in the File Comments!')
{$ENDIF}
  ;
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.MinGWGetExecutable);
end;

procedure TfrmMain.btnOpenMSYSClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.LauncherExecutable);
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  ExecuteThreadOperation(stiKallistiSinglePortInstall);
end;

procedure TfrmMain.btnPortUninstallClick(Sender: TObject);
var
  Msg: string;

begin
  if Assigned(SelectedKallistiPort) then
  begin
    Msg := Format(UninstallKallistiSinglePort, [SelectedKallistiPort.Name]);
    if MsgBox(DialogQuestionTitle, Msg, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes then
      ExecuteThreadOperation(stiKallistiSinglePortUninstall);
  end;
end;

procedure TfrmMain.btnPortUpdateClick(Sender: TObject);
begin
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
  ExecuteThreadOperation(stiKallistiManage);
end;

procedure TfrmMain.btnDreamcastToolCustomExecutableClick(Sender: TObject);
begin
  with opdDreamcastToolCustom do
    if Execute then
      edtDreamcastToolCustomExecutable.Text := opdDreamcastToolCustom.FileName;
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

procedure TfrmMain.edtPortMaintainerClick(Sender: TObject);
const
  MAIL_TO_URL = 'mailto:%s?subject=%s';

var
  MailTo,
  Subject: string;

  function EncodeUrl(Value: string): string;
  begin
    Result := StringReplace(Value, ' ', '%20', [rfReplaceAll]);
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

initialization
  HelpFileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + HELPFILE;

end.

