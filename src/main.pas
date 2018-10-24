unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst, PortMgr;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClose: TButton;
    btnOpenMinGWManager: TButton;
    btnPortUninstall: TButton;
    btnUpdateKallistiOS: TButton;
    btnPortInstall: TButton;
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
    rgxTerminalOption: TRadioGroup;
    tsPortInformation: TTabSheet;
    tsPortDescription: TTabSheet;
    tsAbout: TTabSheet;
    tsOptions: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure edtPortMaintainerClick(Sender: TObject);
    procedure edtPortURLClick(Sender: TObject);
    procedure edtPortURLMouseEnter(Sender: TObject);
    procedure edtPortURLMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: Boolean);
    procedure rgxTerminalOptionClick(Sender: TObject);
  private
    procedure DisplayEnvironmentToolchainStatus;
    procedure DisplayEnvironmentComponentVersions;
    procedure DisplayKallistiPorts;
    function GetSelectedKallistiPort: TKallistiPortItem;
    function GetSelectedKallistiPortItemIndex: Integer;
    procedure LoadConfiguration;
    function BooleanToCaption(Value: Boolean): string;
  public
    property SelectedKallistiPortItemIndex: Integer
      read GetSelectedKallistiPortItemIndex;
    property SelectedKallistiPort: TKallistiPortItem
      read GetSelectedKallistiPort;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  LCLIntf, DCSDKMgr, GetVer, SysTools;

var
  DreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  DisplayEnvironmentToolchainStatus;
  DisplayEnvironmentComponentVersions;
  DisplayKallistiPorts;
  LoadConfiguration;
end;

procedure TfrmMain.lbxPortsClickCheck(Sender: TObject);
var
  State: TCheckBoxState;

begin
  if Assigned(SelectedKallistiPort) then
  begin
    State := cbUnchecked;
    if SelectedKallistiPort.Installed then
      State := cbGrayed;
    lbxPorts.State[lbxPorts.ItemIndex] := State;
  end;
end;

procedure TfrmMain.lbxPortsSelectionChange(Sender: TObject; User: Boolean);
begin
  if Assigned(SelectedKallistiPort) and User then
  begin
    lblPortName.Caption := SelectedKallistiPort.Name;
    edtPortVersion.Caption := SelectedKallistiPort.Version;
    edtPortLicense.Text := SelectedKallistiPort.License;
    edtPortMaintainer.Caption := SelectedKallistiPort.Maintainer;
    memPortShortDescription.Caption := SelectedKallistiPort.ShortDescription;
    edtPortURL.Caption := SelectedKallistiPort.URL;
    memPortDescription.Text := SelectedKallistiPort.Description;
  end;
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
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
  for i := 0 to DreamcastSoftwareDevelopmentKitManager.Ports.Count - 1 do
  begin
    PortInfo := DreamcastSoftwareDevelopmentKitManager.Ports[i];
    j := lbxPorts.Items.Add(PortInfo.Name);
    lbxPorts.Items.Objects[j] := TObject(i);
    if PortInfo.Installed then
      lbxPorts.State[j] := cbGrayed;
  end;
end;

function TfrmMain.GetSelectedKallistiPort: TKallistiPortItem;
var
  Index: Integer;

begin
  Result := nil;
  Index := SelectedKallistiPortItemIndex;
  if Index <> -1 then
    Result := DreamcastSoftwareDevelopmentKitManager.Ports[Index];
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

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.MinGWGetExecutable);
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  ShowMessage(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.KallistiPorts);
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

