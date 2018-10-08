unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClose: TButton;
    btnOpenMinGWManager: TButton;
    btnPortUninstall: TButton;
    btnUpdateKallistiOS: TButton;
    btnPortInstall: TButton;
    btnAbout: TButton;
    gbxAvailablePorts: TGroupBox;
    gbxPortInformation: TGroupBox;
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
    lbxPorts: TListBox;
    PageControl1: TPageControl;
    pnlActions: TPanel;
    rgxTerminalOption: TRadioGroup;
    tsAbout: TTabSheet;
    tsOptions: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure btnPortInstallClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgxTerminalOptionClick(Sender: TObject);
  private
    procedure DisplayEnvironmentComponentVersions;
    procedure LoadConfiguration;
  public
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
  DisplayEnvironmentComponentVersions;
  LoadConfiguration;
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.UseMinTTY := (rgxTerminalOption.ItemIndex = 1);
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

procedure TfrmMain.LoadConfiguration;
begin
  if DreamcastSoftwareDevelopmentKitManager.Environment.UseMinTTY then
    rgxTerminalOption.ItemIndex := 1;
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
  RunNoWait(DreamcastSoftwareDevelopmentKitManager.Environment.MinGWGetExecutable);
end;

procedure TfrmMain.btnPortInstallClick(Sender: TObject);
begin
  ShowMessage(DreamcastSoftwareDevelopmentKitManager.Environment.KallistiPorts);
end;

initialization
  DreamcastSoftwareDevelopmentKitManager := TDreamcastSoftwareDevelopmentKitManager.Create;

finalization
  DreamcastSoftwareDevelopmentKitManager.Free;

end.

