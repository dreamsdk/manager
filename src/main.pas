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
    btnClose: TButton;
    btnOpenMinGWManager: TButton;
    btnOpenMSYS: TButton;
    btnPortInstall: TButton;
    btnPortUninstall: TButton;
    btnPortUpdate: TButton;
    btnUpdateKallistiOS: TButton;
    btnRestoreDefaults: TButton;
    btnAllPortInstall: TButton;
    btnAllPortUninstall: TButton;
    ckxDreamcastToolAttachConsoleFileServer: TCheckBox;
    ckxDreamcastToolClearScreenBeforeDownload: TCheckBox;
    ckxDreamcastToolAlwaysStartGDB: TCheckBox;
    ckxDreamcastToolSerialAlternateBaudrate: TCheckBox;
    ckxDreamcastToolSerialExternalClock: TCheckBox;
    ckxDreamcastToolSerialDumbTerminal: TCheckBox;
    cbxDreamcastToolSerialBaudrate: TComboBox;
    cbxDreamcastToolSerialPort: TComboBox;
    cbxUrlKallisti: TComboBox;
    cbxUrlKallistiPorts: TComboBox;
    cbxUrlDreamcastToolSerial: TComboBox;
    cbxUrlDreamcastToolIP: TComboBox;
    edtPortLicense: TLabeledEdit;
    edtPortMaintainer: TLabeledEdit;
    edtPortURL: TLabeledEdit;
    edtPortVersion: TLabeledEdit;
    gbxToolchain1: TGroupBox;
    gbxKallistiChangeLog: TGroupBox;
    gbxUrlKallistiOS: TGroupBox;
    gbxUrlDreamcastToolSerial: TGroupBox;
    gbxUrlDreamcastToolIP: TGroupBox;
    gbxUrlKallistiPorts: TGroupBox;
    gbxPortAll: TGroupBox;
    gbxPortDetails: TGroupBox;
    gbxDreamcastToolSerial: TGroupBox;
    gbxDreamcastToolInternetProtocol: TGroupBox;
    gbxDreamcastToolCommon: TGroupBox;
    lblInvalidlInternetProtocolAddress: TLabel;
    lblDreamcastToolInternetProtocolAddress: TLabel;
    lblDreamcastToolSerialPort: TLabel;
    lblDreamcastToolSerialBaudrate: TLabel;
    lblBuildDateKallistiOS: TLabel;
    lblPortName: TLabel;
    lblTextBinutilsARM: TLabel;
    lblTextGCCARM: TLabel;
    lblTitleAbout: TLabel;
    lblTextKallistiOS: TLabel;
    lblTextBuildDateKallistiOS: TLabel;
    lblVersionBinutilsARM: TLabel;
    lblVersionGCCARM: TLabel;
    lbxPorts: TCheckListBox;
    gbxAvailablePorts: TGroupBox;
    gbxToolchain: TGroupBox;
    gbxDependencies: TGroupBox;
    gbxDreamcastTool: TGroupBox;
    lblTextBinutils: TLabel;
    lblTextGit: TLabel;
    lblTextPython: TLabel;
    lblTextSVN: TLabel;
    lblTextMinGW: TLabel;
    lblTextToolSerial: TLabel;
    lblTextGCC: TLabel;
    lblTextNewlib: TLabel;
    lblTextGDB: TLabel;
    lblTextToolIP: TLabel;
    lblVersionToolSerial: TLabel;
    lblVersionToolIP: TLabel;
    lblVersionKallistiOS: TLabel;
    lblVersionBinutils: TLabel;
    lblVersionGCC: TLabel;
    lblVersionGDB: TLabel;
    lblVersionGit: TLabel;
    lblVersionNewlib: TLabel;
    lblVersionPython: TLabel;
    lblVersionMinGW: TLabel;
    lblVersionSVN: TLabel;
    edtDreamcastToolInternetProtocolAddress: TMaskEdit;
    memKallistiChangeLog: TMemo;
    memPortDescription: TMemo;
    memPortShortDescription: TMemo;
    pcMain: TPageControl;
    pnlActions: TPanel;
    rgbDreamcastTool: TRadioGroup;
    rgxTerminalOption: TRadioGroup;
    tmDisplayKallistiPorts: TTimer;
    tsDreamcastTool: TTabSheet;
    tmrShellThreadTerminate: TTimer;
    tsAbout: TTabSheet;
    tsOptions: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    procedure apMainException(Sender: TObject; E: Exception);
    procedure btnAllPortInstallClick(Sender: TObject);
    procedure btnAllPortUninstallClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnOpenMSYSClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure btnPortUninstallClick(Sender: TObject);
    procedure btnPortUpdateClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure btnUpdateKallistiOSClick(Sender: TObject);
    procedure cbxDreamcastToolSerialBaudrateSelect(Sender: TObject);
    procedure cbxDreamcastToolSerialPortSelect(Sender: TObject);
    procedure cbxUrlDreamcastToolIPChange(Sender: TObject);
    procedure cbxUrlDreamcastToolSerialChange(Sender: TObject);
    procedure cbxUrlKallistiChange(Sender: TObject);
    procedure cbxUrlKallistiPortsChange(Sender: TObject);
    procedure edtDreamcastToolInternetProtocolAddressChange(Sender: TObject);
    procedure edtPortMaintainerClick(Sender: TObject);
    procedure edtPortURLClick(Sender: TObject);
    procedure edtPortURLMouseEnter(Sender: TObject);
    procedure edtPortURLMouseLeave(Sender: TObject);
    procedure edtUrlDreamcastToolIPChange(Sender: TObject);
    procedure edtUrlDreamcastToolSerialChange(Sender: TObject);
    procedure edtUrlKallistiOSChange(Sender: TObject);
    procedure edtUrlKallistiPortsChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: Boolean);
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
    procedure RefreshKallistiPortsControls;
    procedure UpdateDreamcastToolScreen;
    procedure UpdateDreamcastToolAlternateCheckbox;
    procedure InstallDreamcastTool;
    procedure HandleInvalidInternetProtocolAddress(const InvalidMaskFormat: Boolean);
    procedure LoadRepositoriesSelectionList;
  public
    procedure UpdateDisplay(ForceRefresh: Boolean);
    procedure OnCommandTerminateThread(Request: TShellThreadInputRequest;
    Response: TShellThreadOutputResponse;
    Success: Boolean;
    UpdateState: TUpdateOperationState);
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
  LCLIntf, GetVer, SysTools, PostInst, Settings;

const
  KALLISTI_BUILD_DATE_FORMAT  = 'YYYY-MM-DD @ HH:mm:ss';
  KALLISTI_VERSION_FORMAT     = '%s (%s)';

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pcMain.TabIndex := 0;
  Application.Title := Caption;
  lblTitleAbout.Caption := Format(lblTitleAbout.Caption, [Caption]);
  fLoadingConfiguration := True;
  LoadRepositoriesSelectionList;
  LoadConfiguration;
  UpdateDisplay(True);
  fLoadingConfiguration := False;
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
  end;
end;

procedure TfrmMain.rgbDreamcastToolSelectionChanged(Sender: TObject);
begin
  InstallDreamcastTool;
  UpdateDreamcastToolScreen;
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
end;

procedure TfrmMain.tmDisplayKallistiPortsTimer(Sender: TObject);
var
  i, j: Integer;
  PortInfo: TKallistiPortItem;
  HasKallistiPorts: Boolean;

begin
  tmDisplayKallistiPorts.Enabled := False;

  if fKallistiPortsClearList then
  begin
    // Reset the Ports view

    lbxPorts.Clear;
    for i := 0 to DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Count - 1 do
    begin
      PortInfo := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[i];
      j := lbxPorts.Items.Add(PortInfo.Name);
      lbxPorts.Items.Objects[j] := TObject(i);
      if PortInfo.Installed then
        lbxPorts.State[j] := cbGrayed;
      Application.ProcessMessages;
    end;

    HasKallistiPorts := (lbxPorts.Count > 0);
    if HasKallistiPorts then
    begin
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

  RefreshKallistiPortsControls;
end;

procedure TfrmMain.tmrShellThreadTerminateTimer(Sender: TObject);
var
  IsGlobalRefreshViewNeeded,
  IsKallistiPortsRefreshViewNeeded: Boolean;

begin
  tmrShellThreadTerminate.Enabled := False;

  if fShellThreadSuccess then
    case fShellThreadOutputResult of
      // No action was done
      stoNothing:
        MessageDlg(DialogInformationTitle, UpdateProcessEverythingUpdate, mtInformation, [mbOk], 0);

      // KallistiOS was installed
      stoKallistiInstall:
        MessageDlg(DialogInformationTitle, Format(UpdateProcessInstallSuccessText, [KallistiText]), mtInformation, [mbOk], 0);

      // KallistiOS, KallistiOS Ports or Dreamcast Tool were updated
      stoKallistiUpdate:
        case fShellThreadUpdateState of
          uosUpdateSuccess:
            MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [KallistiText]), mtInformation, [mbOk], 0);
        end;

      // All KallistiOS Ports were installed
      stoKallistiPortsInstall:
        MessageDlg(DialogInformationTitle, UpdateProcessAllKallistiPortsInstalled, mtInformation, [mbOk], 0);

      // All KallistiOS Ports were uninstalled
      stoKallistiPortsUninstall:
        MessageDlg(DialogInformationTitle, UpdateProcessAllKallistiPortsUninstalled, mtInformation, [mbOk], 0);

      // A single KallistiOS Port was installed
      stoKallistiPortInstall:
        DisplayKallistiPorts(False);

      // A single KallistiOS Port was updated
      stoKallistiPortUpdate:
        case fShellThreadUpdateState of
          uosUpdateSuccess:
            MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk], 0);
          uosUpdateUseless:
            MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateUselessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk], 0);
        end;

      // A single KallistiOS Port was uninstalled
      stoKallistiPortUninstall:
        DisplayKallistiPorts(False);
    end;

  IsGlobalRefreshViewNeeded := (fShellThreadOutputResult = stoKallistiInstall)
    or (fShellThreadOutputResult = stoKallistiUpdate)
    or (fShellThreadOutputResult = stoKallistiPortsInstall)
    or (fShellThreadOutputResult = stoKallistiPortsUninstall);

  if IsGlobalRefreshViewNeeded then
    UpdateDisplay(True);

  IsKallistiPortsRefreshViewNeeded := (fShellThreadOutputResult = stoKallistiPortInstall)
    or (fShellThreadOutputResult = stoKallistiPortUninstall);

  if IsKallistiPortsRefreshViewNeeded then
    RefreshKallistiPortsControls;
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
    lblBuildDateKallistiOS.Caption := FormatDateTime(KALLISTI_BUILD_DATE_FORMAT,
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
    ckxDreamcastToolAlwaysStartGDB.Checked := DreamcastTool.AlwaysStartDebugger;
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
  btnPortInstall.Enabled := not SelectedKallistiPort.Installed;
  btnPortUninstall.Enabled := SelectedKallistiPort.Installed;
  btnPortUpdate.Enabled := SelectedKallistiPort.Installed;
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

procedure TfrmMain.RefreshKallistiPortsControls;
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

procedure TfrmMain.UpdateDreamcastToolScreen;
begin
  gbxDreamcastToolSerial.Enabled := (rgbDreamcastTool.ItemIndex = 1);
  gbxDreamcastToolInternetProtocol.Enabled := (rgbDreamcastTool.ItemIndex = 2);
  gbxDreamcastToolCommon.Enabled := (rgbDreamcastTool.ItemIndex <> 0);
  UpdateDreamcastToolAlternateCheckbox;
end;

procedure TfrmMain.UpdateDreamcastToolAlternateCheckbox;
begin
  ckxDreamcastToolSerialAlternateBaudrate.Enabled := (gbxDreamcastToolSerial.Enabled)
    and (cbxDreamcastToolSerialBaudrate.ItemIndex = 8);
end;

procedure TfrmMain.InstallDreamcastTool;
begin
  if not fLoadingConfiguration then
  begin
    with DreamcastSoftwareDevelopmentKitManager.Environment.Settings.DreamcastTool do
    begin
      AlwaysStartDebugger := ckxDreamcastToolAlwaysStartGDB.Checked;
      AttachConsoleFileserver := ckxDreamcastToolAttachConsoleFileServer.Checked;
      ClearScreenBeforeDownload := ckxDreamcastToolClearScreenBeforeDownload.Checked;
      Kind := TDreamcastToolKind(rgbDreamcastTool.ItemIndex);
      SerialBaudrate := TDreamcastToolSerialBaudrate(cbxDreamcastToolSerialBaudrate.ItemIndex);
      SerialBaudrateAlternate := ckxDreamcastToolSerialAlternateBaudrate.Checked;
      SerialDumbTerminal := ckxDreamcastToolSerialDumbTerminal.Checked;
      SerialPort := TDreamcastToolSerialPort(cbxDreamcastToolSerialPort.ItemIndex);
      SerialExternalClock := ckxDreamcastToolSerialExternalClock.Checked;
      InternetProtocolAddress := edtDreamcastToolInternetProtocolAddress.Text;
    end;
    DreamcastSoftwareDevelopmentKitManager.DreamcastTool.Install;
  end;
end;

procedure TfrmMain.HandleInvalidInternetProtocolAddress(const InvalidMaskFormat: Boolean);
var
  InvalidValue: Boolean;

begin
  InvalidValue := not IsValidInternetProtocolAddress(edtDreamcastToolInternetProtocolAddress.Text);
  if InvalidMaskFormat then
    lblInvalidlInternetProtocolAddress.Caption := InvalidInternetProtocolAddressFormat
  else
    lblInvalidlInternetProtocolAddress.Caption := InvalidInternetProtocolAddressValue;
  lblInvalidlInternetProtocolAddress.Visible := InvalidMaskFormat or InvalidValue;
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

procedure TfrmMain.UpdateDisplay(ForceRefresh: Boolean);
begin
  Cursor := crHourGlass;
  Application.ProcessMessages;

  if ForceRefresh then
  begin
    DreamcastSoftwareDevelopmentKitManager.KallistiPorts.RetrieveAvailablePorts;
    DreamcastSoftwareDevelopmentKitManager.Versions.RetrieveVersions;
  end;

  DisplayEnvironmentComponentVersions;
  DisplayKallistiPorts(ForceRefresh);
  UpdateDreamcastToolScreen;
  Cursor := crDefault;
end;

procedure TfrmMain.OnCommandTerminateThread(Request: TShellThreadInputRequest;
  Response: TShellThreadOutputResponse; Success: Boolean;
  UpdateState: TUpdateOperationState);
begin
  Application.ProcessMessages;
  fShellThreadSuccess := Success;
  fShellThreadInputRequest := Request;
  fShellThreadOutputResult := Response;
  fShellThreadUpdateState := UpdateState;
  tmrShellThreadTerminate.Enabled := True;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnAllPortInstallClick(Sender: TObject);
begin
  if MessageDlg(DialogQuestionTitle, InstallAllKallistiPorts, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ExecuteThreadOperation(stiKallistiPortsInstall);
end;

procedure TfrmMain.apMainException(Sender: TObject; E: Exception);
begin
  if (E is EDBEditError) then
    HandleInvalidInternetProtocolAddress(True);
end;

procedure TfrmMain.btnAllPortUninstallClick(Sender: TObject);
begin
  if MessageDlg(DialogQuestionTitle, UninstallAllKallistiPorts, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ExecuteThreadOperation(stiKallistiPortsUninstall);
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.MinGWGetExecutable);
end;

procedure TfrmMain.btnOpenMSYSClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.Shell.DreamSDKExecutable);
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  ExecuteThreadOperation(stiKallistiPortInstall);
end;

procedure TfrmMain.btnPortUninstallClick(Sender: TObject);
begin
  ExecuteThreadOperation(stiKallistiPortUninstall);
end;

procedure TfrmMain.btnPortUpdateClick(Sender: TObject);
begin
  ExecuteThreadOperation(stiKallistiPortUpdate);
end;

procedure TfrmMain.btnRestoreDefaultsClick(Sender: TObject);
begin
  if MessageDlg(DialogQuestionTitle, RestoreDefaultsText, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    rgxTerminalOption.ItemIndex := 0;
    rgxTerminalOptionClick(Self);
    cbxUrlKallisti.Text := DEFAULT_KALLISTI_URL;
    cbxUrlKallistiPorts.Text := DEFAULT_KALLISTI_PORTS_URL;
    cbxUrlDreamcastToolSerial.Text := DEFAULT_DREAMCAST_TOOL_SERIAL_URL;
    cbxUrlDreamcastToolIP.Text := DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL;
    DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
  end;
end;

procedure TfrmMain.btnUpdateKallistiOSClick(Sender: TObject);
begin
  ExecuteThreadOperation(stiKallistiManage);
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

procedure TfrmMain.edtDreamcastToolInternetProtocolAddressChange(Sender: TObject
  );
begin
  HandleInvalidInternetProtocolAddress(False);
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

procedure TfrmMain.edtUrlDreamcastToolIPChange(Sender: TObject);
begin

end;

procedure TfrmMain.edtUrlDreamcastToolSerialChange(Sender: TObject);
begin

end;

procedure TfrmMain.edtUrlKallistiOSChange(Sender: TObject);
begin

end;

procedure TfrmMain.edtUrlKallistiPortsChange(Sender: TObject);
begin

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if not IsPostInstallMode and IsInstallOrUpdateRequired then
    ExecuteThreadOperation(stiKallistiManage);
end;

initialization
  DreamcastSoftwareDevelopmentKitManager := TDreamcastSoftwareDevelopmentKitManager.Create;

finalization
  DreamcastSoftwareDevelopmentKitManager.Free;

end.

