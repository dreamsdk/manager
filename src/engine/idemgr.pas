unit IDEMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Environ,
  FSTools,
  Settings;

type
  TIntegratedDevelopmentEnvironment = class;

  { TCodeBlocksIntegratedDevelopmentEnvironment }
  TCodeBlocksIntegratedDevelopmentEnvironment = class(TObject)
  private
    fSettings: TDreamcastSoftwareDevelopmentCodeBlocksSettings;
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
    fLastErrorMessage: string;
    fLastOperationSuccess: Boolean;
    fPatchState: string;
    fAvailableUsers: TStringList;
    fInstalledUsers: TStringList;
    function GetInstallationDirectory: TFileName;
    function GetInstalled: Boolean;
    function GetAvailableUsers: TStringList;
    function GetInstalledUsers: TStringList;
    procedure RetrievePatchState;
    function RunCodeBlocksPatcher(const Operation: string): string;
  public
    constructor Create(AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment);
    destructor Destroy; override;
    function Install(const CodeBlocksInstallationDirectory: TFileName): Boolean;
    function Uninstall: Boolean;
    function Reinstall: Boolean;
    procedure Refresh; overload;
    procedure Refresh(const Force: Boolean);
    property AvailableUsers: TStringList read GetAvailableUsers;
    property Installed: Boolean read GetInstalled;
    property InstallationDirectory: TFileName read GetInstallationDirectory;
    property InstalledUsers: TStringList read GetInstalledUsers;
    property LastOperationSuccess: Boolean read fLastOperationSuccess
      write fLastOperationSuccess;
    property LastErrorMessage: string read fLastErrorMessage
      write fLastErrorMessage;
    property Settings: TDreamcastSoftwareDevelopmentCodeBlocksSettings
      read fSettings;
  end;

  { TIntegratedDevelopmentEnvironment }
  TIntegratedDevelopmentEnvironment = class(TObject)
  private
    fCodeBlocks: TCodeBlocksIntegratedDevelopmentEnvironment;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    property CodeBlocks: TCodeBlocksIntegratedDevelopmentEnvironment
      read fCodeBlocks;
  end;

implementation

uses
  SysTools,
  RunTools,
  RefBase;

const
  CODEBLOCKS_SUCCESSFUL_STATE = 'Code::Blocks is now';

{ TCodeBlocksIntegratedDevelopmentEnvironment }

function TCodeBlocksIntegratedDevelopmentEnvironment.GetAvailableUsers: TStringList;
begin
  Result := fAvailableUsers;
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.RetrievePatchState;
begin
  fPatchState := RunCodeBlocksPatcher('internal-refresh');
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstallationDirectory: TFileName;
begin
  Result := Settings.InstallationDirectory;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstalled: Boolean;
begin
  Result := Settings.Installed;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstalledUsers: TStringList;
begin
  Result := fInstalledUsers;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.RunCodeBlocksPatcher(
  const Operation: string): string;
var
  CmdLine: string;

begin
  CmdLine := Format('--operation=%s --home-dir="%s" --no-logo --show-splash', [
    Operation, GetInstallationBaseDirectory]);
//  ShowMessage(CmdLine);
  Result := Run(fIntegratedDevelopmentEnvironment.Environment.FileSystem.Shell
    .CodeBlocksPatcherExecutable, CmdLine);

  fLastErrorMessage := Trim(Right('Error: ', Result));
  fLastOperationSuccess := IsEmpty(fLastErrorMessage);
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.Refresh;
begin
  Refresh(False);
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.Refresh(
  const Force: Boolean);
begin
  if Force then
    RetrievePatchState;

  if Settings.LoadConfiguration then
  begin
    // Available Users
    fAvailableUsers.Clear;
    fAvailableUsers.Assign(Settings.AvailableUsers);

    // Installed Users
    fInstalledUsers.Clear;
    if Settings.Installed then
      fInstalledUsers.Assign(Settings.InstalledUsers);
  end;
end;

constructor TCodeBlocksIntegratedDevelopmentEnvironment.Create(
  AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment);
begin
  fIntegratedDevelopmentEnvironment := AIntegratedDevelopmentEnvironment;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentCodeBlocksSettings.Create;
end;

destructor TCodeBlocksIntegratedDevelopmentEnvironment.Destroy;
begin
  fSettings.Free;
  fInstalledUsers.Free;
  fAvailableUsers.Free;
  inherited Destroy;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Install(
  const CodeBlocksInstallationDirectory: TFileName): Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher(Format('install --install-dir="%s" ', [
      CodeBlocksInstallationDirectory])));
  Refresh;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Uninstall: Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher('uninstall'));
  Refresh;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Reinstall: Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher('reinstall'));
  Refresh;
end;

{ TIntegratedDevelopmentEnvironment }

constructor TIntegratedDevelopmentEnvironment.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fCodeBlocks := TCodeBlocksIntegratedDevelopmentEnvironment.Create(Self);
  fEnvironment := AEnvironment;
end;

destructor TIntegratedDevelopmentEnvironment.Destroy;
begin
  fCodeBlocks.Free;
  inherited Destroy;
end;

end.

