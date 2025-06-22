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
    fSettings: TDreamcastSoftwareDevelopmentSettingsCodeBlocks;
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

    function InitializeProfiles: Boolean;
    function Install(const CodeBlocksInstallationDirectory: TFileName): Boolean;
    function Uninstall: Boolean;
    function Reinstall: Boolean;
    procedure Refresh; overload;
    procedure Refresh(const Force: Boolean);
    procedure UpdateStateFromElevatedTask(const SwapExchangeFileName: TFileName);

    property AvailableUsers: TStringList read GetAvailableUsers;
    property Installed: Boolean read GetInstalled;
    property InstallationDirectory: TFileName read GetInstallationDirectory;
    property InstalledUsers: TStringList read GetInstalledUsers;
    property LastOperationSuccess: Boolean read fLastOperationSuccess;
    property LastErrorMessage: string read fLastErrorMessage;
    property Settings: TDreamcastSoftwareDevelopmentSettingsCodeBlocks
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
  StrTools,
  RunTools,
  RefBase,
  CBTools;

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
  CmdLine := Format('--operation=%s --home-dir="%s" --internal-integration', [
    Operation, GetBaseInstallationHomeDirectory]);
{$IFDEF DEBUG}
  DebugLog('RunCodeBlocksPatcher: ' + CmdLine);
{$ENDIF}
  Result := Run(fIntegratedDevelopmentEnvironment.Environment.FileSystem.Shell
    .CodeBlocksPatcherExecutable, CmdLine);

  fLastErrorMessage := Right(CODEBLOCKS_PATCHER_ERROR_TAG, Result);
  fLastErrorMessage := Trim(StringReplace(fLastErrorMessage,
    CODEBLOCKS_PATCHER_ERROR_SEPARATOR, sLineBreak, [rfReplaceAll]));
  fLastOperationSuccess := IsEmpty(fLastErrorMessage);
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.Refresh;
begin
  Refresh(False);
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.Refresh(
  const Force: Boolean);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    if Force then
    begin
      RetrievePatchState;
      Settings.ClearCachedAvailableUsers;
    end;

    LogMessage(LogContext, Format('Force: "%s"', [
      BoolToStr(Force, True)
    ]));

    if Settings.LoadConfiguration(Force) then
    begin
      // Save updated configuration
      if Force then
        Settings.SaveConfiguration;

      // Available Users
      fAvailableUsers.Clear;
      fAvailableUsers.Assign(Settings.AvailableUsers);

      // Installed Users
      fInstalledUsers.Clear;
      if Settings.Installed then
        fInstalledUsers.Assign(Settings.InstalledUsers);
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.UpdateStateFromElevatedTask(
  const SwapExchangeFileName: TFileName);
begin
  fLastOperationSuccess := not FileExists(SwapExchangeFileName);
  if not LastOperationSuccess then
    fLastErrorMessage := LoadFileToString(SwapExchangeFileName);
end;

constructor TCodeBlocksIntegratedDevelopmentEnvironment.Create(
  AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment);
begin
  fIntegratedDevelopmentEnvironment := AIntegratedDevelopmentEnvironment;
  fAvailableUsers := TStringList.Create;
  fInstalledUsers := TStringList.Create;
  fSettings := TDreamcastSoftwareDevelopmentSettingsCodeBlocks.Create;
end;

destructor TCodeBlocksIntegratedDevelopmentEnvironment.Destroy;
begin
  fSettings.Free;
  fInstalledUsers.Free;
  fAvailableUsers.Free;
  inherited Destroy;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.InitializeProfiles: Boolean;
const
  SUCCESS = 'Profiles initialization finished!';

var
  ProfilesInitializationState: string;

begin
  ProfilesInitializationState := RunCodeBlocksPatcher('internal-initialize-profiles');
  Result := IsInString(SUCCESS, ProfilesInitializationState);
  Refresh(True);
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

