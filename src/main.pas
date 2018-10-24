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
    btnAbout: TButton;
    gbxToolchainInstalled: TGroupBox;
    lblTextPortVersion: TLabel;
    lblTextPortMaintainer: TLabel;
    lblPortMaintainer: TLabel;
    lblPortName: TLabel;
    lblPortShortDescription: TLabel;
    lblPortURL: TLabel;
    lblPortVersion: TLabel;
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
    memPortLicense: TMemo;
    PageControl1: TPageControl;
    pcPortDetails: TPageControl;
    pnlActions: TPanel;
    rgxTerminalOption: TRadioGroup;
    tsPortLicense: TTabSheet;
    tsPortInformation: TTabSheet;
    tsPortDescription: TTabSheet;
    tsAbout: TTabSheet;
    tsOptions: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure lbxPortsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxPortsClickCheck(Sender: TObject);
    procedure lbxPortsSelectionChange(Sender: TObject; User: boolean);
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
  DCSDKMgr, GetVer, SysTools;

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

procedure TfrmMain.lbxPortsSelectionChange(Sender: TObject; User: boolean);
begin
  if Assigned(SelectedKallistiPort) then
  begin
    lblPortName.Caption := SelectedKallistiPort.Name;
    lblPortVersion.Caption := SelectedKallistiPort.Version;
    memPortLicense.Text := SelectedKallistiPort.License;
    lblPortMaintainer.Caption := SelectedKallistiPort.Maintainer;
    lblPortShortDescription.Caption := SelectedKallistiPort.ShortDescription;
    lblPortURL.Caption := SelectedKallistiPort.URL;
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

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin

end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.MinGWGetExecutable);
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  ShowMessage(DreamcastSoftwareDevelopmentKitManager.Environment.FileSystem.KallistiPorts);
end;

procedure TfrmMain.lbxPortsClick(Sender: TObject);
begin

end;

initialization
  DreamcastSoftwareDevelopmentKitManager := TDreamcastSoftwareDevelopmentKitManager.Create;

finalization
  DreamcastSoftwareDevelopmentKitManager.Free;

end.

