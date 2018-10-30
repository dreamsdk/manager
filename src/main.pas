unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst, PortMgr, ShellThd, DCSDKMgr, Environ;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnClose: TButton;
    btnOpenMinGWManager: TButton;
    btnOpenMSYS: TButton;
    btnPortInstall: TButton;
    btnPortUninstall: TButton;
    btnUpdateKallistiOS: TButton;
    btnPortUpdate: TButton;
    edtPortLicense: TLabeledEdit;
    edtPortURL: TLabeledEdit;
    edtPortMaintainer: TLabeledEdit;
    edtPortVersion: TLabeledEdit;
    gbxToolchainInstalled: TGroupBox;
    lblPortName: TLabel;
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
    memPortDescription: TMemo;
    memPortShortDescription: TMemo;
    PageControl1: TPageControl;
    pcPortDetails: TPageControl;
    pnlActions: TPanel;
    pnlActions1: TPanel;
    rgxTerminalOption: TRadioGroup;
    tmrShellThreadTerminate: TTimer;
    tsPortInformation: TTabSheet;
    tsPortDescription: TTabSheet;
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
    procedure btnUpdateKallistiOSClick(Sender: TObject);
    procedure edtPortMaintainerClick(Sender: TObject);
    procedure edtPortURLClick(Sender: TObject);
    procedure edtPortURLMouseEnter(Sender: TObject);
    procedure edtPortURLMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: Boolean);
    procedure rgxTerminalOptionClick(Sender: TObject);
    procedure tmrShellThreadTerminateStartTimer(Sender: TObject);
    procedure tmrShellThreadTerminateTimer(Sender: TObject);
  private
    fShellThreadOperation: TShellThreadOperation;
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
  public
    procedure UpdateDisplay(ForceRefresh: Boolean);
    procedure OnCommandTerminateThread(Operation: TShellThreadOperation;
      Success: Boolean; UpdateState: TUpdateOperationState);
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
  LCLIntf, GetVer, SysTools;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
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

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
end;

procedure TfrmMain.tmrShellThreadTerminateStartTimer(Sender: TObject);
begin

end;

procedure TfrmMain.tmrShellThreadTerminateTimer(Sender: TObject);

  procedure KallistiPortUpdateView;
  begin
    lbxPorts.State[lbxPorts.ItemIndex] := BooleanToCheckboxState(fShellThreadOperation = stoPortInstall);
    UpdateKallistiPortControls;
  end;

begin
  tmrShellThreadTerminate.Enabled := False;

  case fShellThreadOperation of

    stoKallistiInstall:
      begin
        MessageDlg('Information', 'KallistiOS was successfully installed.', mtInformation, [mbOk], 0);
        UpdateDisplay(True);
      end;

    stoKallistiUpdate:
      case fShellThreadUpdateState of
        uosUpdateSuccess:
          MessageDlg('Information', 'KallistiOS was successfully updated.', mtInformation, [mbOk], 0);
        uosUpdateUseless:
          MessageDlg('Information', 'KallistiOS is already installed and up-to-date.', mtInformation, [mbOk], 0);
      end;

    stoPortInstall:
      KallistiPortUpdateView;

    stoPortUpdate:
      case fShellThreadUpdateState of
        uosUpdateSuccess:
          MessageDlg('Information', Format('%s was successfully updated.', [SelectedKallistiPort.Name]), mtInformation, [mbOk], 0);
        uosUpdateUseless:
          MessageDlg('Information', Format('%s doesn''t need to be updated.', [SelectedKallistiPort.Name]), mtInformation, [mbOk], 0);
      end;

    stoPortUninstall:
      KallistiPortUpdateView;

  end;

end;

procedure TfrmMain.DisplayEnvironmentToolchainStatus;
begin
  lblToolchainInstalledARM.Caption := BooleanToCaption(
    DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.ToolchainInstalledARM);
  lblToolchainInstalledSH4.Caption := BooleanToCaption(
    DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.ToolchainInstalledSH4);
end;

procedure TfrmMain.DisplayEnvironmentComponentVersions;
var
  ComponentName: TComponentName;
  ComponentNameString: string;

begin
  for ComponentName := Low(TComponentName) to High(TComponentName) do
  begin
    ComponentNameString := ComponentNameToString(ComponentName);
    (FindComponent('lblVersion' + ComponentNameString) as TLabel).Caption :=
      DreamcastSoftwareDevelopmentKitManager.Versions.GetComponentVersion(ComponentName);
  end;
end;

procedure TfrmMain.DisplayKallistiPorts;
var
  i, j: Integer;
  PortInfo: TKallistiPortItem;

begin
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
  if DreamcastSoftwareDevelopmentKitManager.Environment.UseMinTTY then
    rgxTerminalOption.ItemIndex := 1;
end;

function TfrmMain.BooleanToCaption(Value: Boolean): string;
begin
  Result := 'Not Installed';
  if Value then
    Result := 'Installed';
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

procedure TfrmMain.UpdateDisplay(ForceRefresh: Boolean);
begin
  if ForceRefresh then
  begin
    DreamcastSoftwareDevelopmentKitManager.KallistiPorts.RetrieveAvailablePorts;
    DreamcastSoftwareDevelopmentKitManager.Versions.RetrieveVersions;
  end;
  DisplayEnvironmentComponentVersions;
  DisplayKallistiPorts;
end;

procedure TfrmMain.OnCommandTerminateThread(Operation: TShellThreadOperation;
  Success: Boolean; UpdateState: TUpdateOperationState);

begin
  Application.ProcessMessages;
  if Success then
  begin
    fShellThreadOperation := Operation;
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
  DreamcastSoftwareDevelopmentKitManager.Environment.RefreshConfig;
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.DreamSDKExecutable);
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  ExecuteThreadOperation(stoPortInstall);
end;

procedure TfrmMain.btnPortUninstallClick(Sender: TObject);
begin
  ExecuteThreadOperation(stoPortUninstall);
end;

procedure TfrmMain.btnPortUpdateClick(Sender: TObject);
begin
  ExecuteThreadOperation(stoPortUpdate);
end;

procedure TfrmMain.btnUpdateKallistiOSClick(Sender: TObject);
begin
  ExecuteThreadOperation(stoKallistiManage);
end;

procedure TfrmMain.edtPortMaintainerClick(Sender: TObject);
var
  MailTo, Subject: string;

begin
  if IsInString('@', edtPortMaintainer.Text) then
  begin
    MailTo := StringReplace(edtPortMaintainer.Text, ' ', '%20', [rfReplaceAll]);
    Subject := '[KallistiOS]%20Question%20about%20the%20'
      + SelectedKallistiPort.Name + '%20KallistiOS%20port';
    OpenURL('mailto:' + MailTo + '?subject=' + Subject);
  end;
end;

procedure TfrmMain.edtPortURLClick(Sender: TObject);
begin
  if IsInString('http', edtPortURL.Text) then
    OpenURL(edtPortURL.Text);
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

initialization
  DreamcastSoftwareDevelopmentKitManager := TDreamcastSoftwareDevelopmentKitManager.Create;

finalization
  DreamcastSoftwareDevelopmentKitManager.Free;

end.

