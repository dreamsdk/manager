unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst, PortMgr, ShellThd, DCSDKMgr, Environ, StrRes;

type
  { TfrmMain }
  TfrmMain = class(TForm)
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
    btnAllPortUpdate: TButton;
    edtPortLicense: TLabeledEdit;
    edtPortMaintainer: TLabeledEdit;
    edtPortURL: TLabeledEdit;
    edtPortVersion: TLabeledEdit;
    edtUrlKallistiOS: TEdit;
    edtUrlDreamcastToolSerial: TEdit;
    edtUrlDreamcastToolIP: TEdit;
    edtUrlKallistiPorts: TEdit;
    gbxToolchainInstalled: TGroupBox;
    gbxKallistiChangeLog: TGroupBox;
    gbxUrlKallistiOS: TGroupBox;
    gbxUrlDreamcastToolSerial: TGroupBox;
    gbxUrlDreamcastToolIP: TGroupBox;
    gbxUrlKallistiPorts: TGroupBox;
    gbxPortAll: TGroupBox;
    gbxPortDetails: TGroupBox;
    lblBuildDateKallistiOS: TLabel;
    lblPortName: TLabel;
    lblTitleAbout: TLabel;
    lblTextKallistiOS: TLabel;
    lblTextBuildDateKallistiOS: TLabel;
    lblTextToolchainInstalledSH4: TLabel;
    lblTextToolchainInstalledARM: TLabel;
    lblToolchainInstalledSH4: TLabel;
    lblToolchainInstalledARM: TLabel;
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
    memKallistiChangeLog: TMemo;
    memPortDescription: TMemo;
    memPortShortDescription: TMemo;
    pcMain: TPageControl;
    pnlActions: TPanel;
    rgbDreamcastTool: TRadioGroup;
    rgxTerminalOption: TRadioGroup;
    tsHome: TTabSheet;
    tsDreamcastTool: TTabSheet;
    tmrShellThreadTerminate: TTimer;
    tsAbout: TTabSheet;
    tsOptions: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnOpenMSYSClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure btnPortUninstallClick(Sender: TObject);
    procedure btnPortUpdateClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure btnUpdateKallistiOSClick(Sender: TObject);
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
    procedure rgbDreamcastToolClick(Sender: TObject);
    procedure rgxTerminalOptionClick(Sender: TObject);
    procedure tmrShellThreadTerminateTimer(Sender: TObject);
  private
    fShellThreadOutputResult: TShellThreadOutputResponse;
    fShellThreadUpdateState: TUpdateOperationState;
    procedure DisplayEnvironmentToolchainStatus;
    procedure DisplayEnvironmentComponentVersions;
    procedure DisplayKallistiPorts;
    function GetSelectedKallistiPort: TKallistiPortItem;
    function GetSelectedKallistiPortItemIndex: Integer;
    procedure LoadConfiguration;
    function BooleanToCaption(Value: Boolean): string;
    function BooleanToCheckboxState(State: Boolean): TCheckBoxState;
    procedure ClearKallistiPortPanel;
    procedure UpdateKallistiPortControls;
    procedure SetVersionLabelState(VersionLabel: TLabel; Erroneous: Boolean);
    procedure SetVersionLabel(VersionLabel: TLabel; Version: string);
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
  LCLIntf, GetVer, SysTools, PostInst;

const
  KALLISTI_BUILD_DATE_FORMAT  = 'YYYY-MM-DD @ HH:mm:ss';
  KALLISTI_VERSION_FORMAT     = '%s (%s)';

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pcMain.TabIndex := 0;
  Application.Title := Caption;
  lblTitleAbout.Caption := Format(lblTitleAbout.Caption, [Caption]);
  DisplayEnvironmentToolchainStatus;
  UpdateDisplay(False);
  LoadConfiguration;
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

procedure TfrmMain.rgbDreamcastToolClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.DreamcastToolKind := TDreamcastToolKind(rgbDreamcastTool.ItemIndex);
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
end;

procedure TfrmMain.tmrShellThreadTerminateTimer(Sender: TObject);

  procedure KallistiPortUpdateView;
  begin
    lbxPorts.State[lbxPorts.ItemIndex] :=
      BooleanToCheckboxState(fShellThreadOutputResult = stoKallistiPortInstall);
    UpdateKallistiPortControls;
  end;

begin
  tmrShellThreadTerminate.Enabled := False;

  case fShellThreadOutputResult of
    stoNothing:
      MessageDlg(DialogInformationTitle, UpdateProcessEverythingUpdate, mtInformation, [mbOk], 0);

    stoKallistiInstall:
      begin
        MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [KallistiText]), mtInformation, [mbOk], 0);
        UpdateDisplay(True);
      end;

    stoKallistiUpdate:
      case fShellThreadUpdateState of
        uosUpdateSuccess:
          begin
            MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [KallistiText]), mtInformation, [mbOk], 0);
            UpdateDisplay(True);
          end;
      end;

    stoKallistiPortInstall:
      KallistiPortUpdateView;

    stoKallistiPortUpdate:
      case fShellThreadUpdateState of
        uosUpdateSuccess:
          MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateSuccessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk], 0);
        uosUpdateUseless:
          MessageDlg(DialogInformationTitle, Format(UpdateProcessUpdateUselessText, [SelectedKallistiPort.Name]), mtInformation, [mbOk], 0);
      end;

    stoKallistiPortUninstall:
      KallistiPortUpdateView;

  end;
end;

procedure TfrmMain.DisplayEnvironmentToolchainStatus;

  procedure SetToolchainInstalled(ToolchainLabel: TLabel; Installed: Boolean);
  begin
    ToolchainLabel.Caption := BooleanToCaption(Installed);
    SetVersionLabelState(ToolchainLabel, not Installed);
  end;

begin
  SetToolchainInstalled(lblToolchainInstalledARM,
    DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.ToolchainInstalledARM);
  SetToolchainInstalled(lblToolchainInstalledSH4,
    DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.ToolchainInstalledSH4);
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
  if FileExists(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.KallistiLibrary) then
    lblBuildDateKallistiOS.Caption := FormatDateTime(KALLISTI_BUILD_DATE_FORMAT,
      DreamcastSoftwareDevelopmentKitManager.Versions.KallistiBuildDate);

  // KallistiOS changes log display
  memKallistiChangeLog.Lines.Clear;
  if FileExists(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.KallistiChangeLogFile) then
    memKallistiChangeLog.Lines.LoadFromFile(DreamcastSoftwareDevelopmentKitManager
      .Environment.FileSystem.KallistiChangeLogFile);
end;

procedure TfrmMain.DisplayKallistiPorts;
var
  i, j: Integer;
  PortInfo: TKallistiPortItem;

begin
  lbxPorts.Clear;
  for i := 0 to DreamcastSoftwareDevelopmentKitManager.KallistiPorts.Count - 1 do
  begin
    PortInfo := DreamcastSoftwareDevelopmentKitManager.KallistiPorts[i];
    j := lbxPorts.Items.Add(PortInfo.Name);
    lbxPorts.Items.Objects[j] := TObject(i);
    if PortInfo.Installed then
      lbxPorts.State[j] := cbGrayed;
  end;

  if lbxPorts.Count > 0 then
  begin
    lbxPorts.ItemIndex := 0;
    lbxPortsSelectionChange(Self, True);
  end
  else
    ClearKallistiPortPanel;
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
  if DreamcastSoftwareDevelopmentKitManager.Environment.Settings.UseMinTTY then
    rgxTerminalOption.ItemIndex := 1;
  edtUrlKallistiOS.Text := DreamcastSoftwareDevelopmentKitManager.Environment.Repositories.KallistiURL;
  edtUrlKallistiPorts.Text := DreamcastSoftwareDevelopmentKitManager.Environment.Repositories.KallistiPortsURL;
  edtUrlDreamcastToolSerial.Text := DreamcastSoftwareDevelopmentKitManager.Environment.Repositories.DreamcastToolSerialURL;
  edtUrlDreamcastToolIP.Text := DreamcastSoftwareDevelopmentKitManager.Environment.Repositories.DreamcastToolInternetProtocolURL;
  rgbDreamcastTool.ItemIndex := Integer(DreamcastSoftwareDevelopmentKitManager.Environment.Settings.DreamcastToolKind);
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
  DisplayKallistiPorts;
  Cursor := crDefault;
end;

procedure TfrmMain.OnCommandTerminateThread(Request: TShellThreadInputRequest;
  Response: TShellThreadOutputResponse; Success: Boolean;
  UpdateState: TUpdateOperationState);
begin
  Application.ProcessMessages;
  if Success then
  begin
    fShellThreadOutputResult := Response;
    fShellThreadUpdateState := UpdateState;
    tmrShellThreadTerminate.Enabled := True;
  end;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.MinGWGetExecutable);
end;

procedure TfrmMain.btnOpenMSYSClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.SaveConfig;
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.DreamSDKExecutable);
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
  if MessageDlg(RestoreDefaultsTitle, RestoreDefaultsText, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    rgxTerminalOption.ItemIndex := 0;
    rgxTerminalOptionClick(Self);
    edtUrlKallistiOS.Text := DEFAULT_KALLISTI_URL;
    edtUrlKallistiPorts.Text := DEFAULT_KALLISTI_PORTS_URL;
    edtUrlDreamcastToolSerial.Text := DEFAULT_DREAMCAST_TOOL_SERIAL_URL;
    edtUrlDreamcastToolIP.Text := DEFAULT_DREAMCAST_TOOL_INTERNET_PROTOCOL_URL;
    DreamcastSoftwareDevelopmentKitManager.Environment.SaveConfig;
  end;
end;

procedure TfrmMain.btnUpdateKallistiOSClick(Sender: TObject);
begin
  ExecuteThreadOperation(stiKallistiManage);
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
  DreamcastSoftwareDevelopmentKitManager.Environment.Repositories
    .DreamcastToolInternetProtocolURL := edtUrlDreamcastToolIP.Text;
end;

procedure TfrmMain.edtUrlDreamcastToolSerialChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Repositories
    .DreamcastToolSerialURL := edtUrlDreamcastToolSerial.Text;
end;

procedure TfrmMain.edtUrlKallistiOSChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Repositories
    .KallistiURL := edtUrlKallistiOS.Text;
end;

procedure TfrmMain.edtUrlKallistiPortsChange(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Repositories
    .KallistiPortsURL := edtUrlKallistiPorts.Text;
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

