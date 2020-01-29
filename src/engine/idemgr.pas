unit IDEMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Environ,
  FSTools;

type
  TIntegratedDevelopmentEnvironmentKind = (ideNone, ideCodeBlocks);

  TIntegratedDevelopmentEnvironment = class;

  { TCodeBlocksIntegratedDevelopmentEnvironment }
  TCodeBlocksIntegratedDevelopmentEnvironment = class(TObject)
  private
    fLastErrorMessage: string;
    fPatchState: string;
    fIDE: TIntegratedDevelopmentEnvironment;
    fAvailableUsers: TStringList;
    fInstalled: Boolean;
    fInstallationDirectory: TFileName;
    fInstalledUsers: TStringList;
    fConfigurationFileNames: TFileList;
    function GetAvailableUsers: TStringList;
    function GetConfigurationFileNames: TFileList;
    function GetInstalledUsers: TStringList;
    function GetPatcherValue(const Key: string): string;
    procedure RetrievePatchState;
    function RunCodeBlocksPatcher(const Operation: string): string;
  public
    constructor Create(AIDE: TIntegratedDevelopmentEnvironment);
    destructor Destroy; override;
    function Install(const CodeBlocksInstallationDirectory: TFileName): Boolean;
    function Uninstall: Boolean;
    function Reinstall: Boolean;
    procedure RefreshStatus;
    property AvailableUsers: TStringList read GetAvailableUsers;
    property ConfigurationFileNames: TFileList read GetConfigurationFileNames;
    property Installed: Boolean read fInstalled;
    property InstallationDirectory: TFileName read fInstallationDirectory;
    property InstalledUsers: TStringList read GetInstalledUsers;
    property LastErrorMessage: string read fLastErrorMessage;
  end;

  { TIntegratedDevelopmentEnvironment }
  TIntegratedDevelopmentEnvironment = class(TObject)
  private
    fCodeBlocks: TCodeBlocksIntegratedDevelopmentEnvironment;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fConfigurationFileName: TFileName;
    fExportLibraryInformation: Boolean;
    fExportLibraryInformationPath: TFileName;
    fKind: TIntegratedDevelopmentEnvironmentKind;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    procedure ProcessConfigurationFile;
    property Kind: TIntegratedDevelopmentEnvironmentKind read fKind;
    property ExportLibraryInformation: Boolean read fExportLibraryInformation;
    property ExportLibraryInformationPath: TFileName read fExportLibraryInformationPath;
    property CodeBlocks: TCodeBlocksIntegratedDevelopmentEnvironment
      read fCodeBlocks;
  end;

implementation

uses
  IniFiles,
  SysTools,
  RunTools,
  RefBase;

const
  CODEBLOCKS_PATCHER_SEPARATOR = ';';
  CODEBLOCKS_SUCCESSFUL_STATE = 'Code::Blocks is now';

{ TCodeBlocksIntegratedDevelopmentEnvironment }

function TCodeBlocksIntegratedDevelopmentEnvironment.GetInstalledUsers: TStringList;
begin
  Result := fInstalledUsers;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetAvailableUsers: TStringList;
begin
  Result := fAvailableUsers;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetConfigurationFileNames: TFileList;
begin
  Result := fConfigurationFileNames;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.GetPatcherValue(
  const Key: string): string;
var
  Buffer: TStringList;
  i: Integer;

begin
  Buffer := TStringList.Create;
  try
    Buffer.Text := fPatchState;
    i := StringListSubstringIndexOf(Buffer, Key + '=');
    if (i <> -1) then
      Result := Trim(Right('=', Buffer[i]));
  finally
    Buffer.Free;
  end;
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.RetrievePatchState;
begin
  fPatchState := RunCodeBlocksPatcher('internal-status');
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.RunCodeBlocksPatcher(
  const Operation: string): string;
begin
  Result := Run(fIDE.Environment.FileSystem.Shell.CodeBlocksPatcherExecutable,
    Format('--operation=%s --home-dir="%s" --no-logo --show-splash', [
      Operation, GetInstallationBaseDirectory]));

  if Operation <> 'internal-status' then
    fLastErrorMessage := Trim(Right('Error: ', Result));
end;

procedure TCodeBlocksIntegratedDevelopmentEnvironment.RefreshStatus;
begin
  RetrievePatchState;

  fInstalled := UpperCase(GetPatcherValue('IsPatchInstalled')) = 'TRUE';
  fInstallationDirectory := GetPatcherValue('InstallationDirectory');

  fConfigurationFileNames.SetItems(GetPatcherValue('ConfigurationFileNames'),
    CODEBLOCKS_PATCHER_SEPARATOR);

  fInstalledUsers.Clear;
  StringToStringList(GetPatcherValue('InstalledUsers'),
    CODEBLOCKS_PATCHER_SEPARATOR, fInstalledUsers);

  fAvailableUsers.Clear;
  StringToStringList(GetPatcherValue('AvailableUsers'),
    CODEBLOCKS_PATCHER_SEPARATOR, fAvailableUsers);
end;

constructor TCodeBlocksIntegratedDevelopmentEnvironment.Create(
  AIDE: TIntegratedDevelopmentEnvironment);
begin
  fIDE := AIDE;
  fAvailableUsers := TStringList.Create;
  fConfigurationFileNames := TFileList.Create;
  fInstalledUsers := TStringList.Create;
end;

destructor TCodeBlocksIntegratedDevelopmentEnvironment.Destroy;
begin
  fInstalledUsers.Free;
  fConfigurationFileNames.Free;
  fAvailableUsers.Free;
  inherited Destroy;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Install(
  const CodeBlocksInstallationDirectory: TFileName): Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher(Format('install --install-dir="%s"', [
      CodeBlocksInstallationDirectory])));
  RefreshStatus;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Uninstall: Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher('uninstall'));
  RefreshStatus;
end;

function TCodeBlocksIntegratedDevelopmentEnvironment.Reinstall: Boolean;
begin
  Result := IsInString(CODEBLOCKS_SUCCESSFUL_STATE,
    RunCodeBlocksPatcher('reinstall'));
  RefreshStatus;
end;

{ TIntegratedDevelopmentEnvironment }

constructor TIntegratedDevelopmentEnvironment.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fCodeBlocks := TCodeBlocksIntegratedDevelopmentEnvironment.Create(Self);
  fEnvironment := AEnvironment;
  fConfigurationFileName := fEnvironment.FileSystem.Shell.IntegratedDevelopmentEnvironmentConfigurationFile;
  ProcessConfigurationFile;
end;

destructor TIntegratedDevelopmentEnvironment.Destroy;
begin
  fCodeBlocks.Free;
  inherited Destroy;
end;

procedure TIntegratedDevelopmentEnvironment.ProcessConfigurationFile;
var
  IniFile: TIniFile;

begin
  if FileExists(fConfigurationFileName) then
  begin
    IniFile := TIniFile.Create(fConfigurationFileName);
    try
      fKind := TIntegratedDevelopmentEnvironmentKind(IniFile.ReadInteger('IDE', 'Kind', 0));
      fExportLibraryInformation := IniFile.ReadBool('IDE', 'ExportLibraryInformation', False);
      fExportLibraryInformationPath := IniFile.ReadString('IDE', 'ExportLibraryInformationPath', EmptyStr);
      if not SameText(fExportLibraryInformationPath, EmptyStr) then
      begin
        fExportLibraryInformationPath := IncludeTrailingPathDelimiter(fExportLibraryInformationPath);
        ForceDirectories(fExportLibraryInformationPath);
        SetDirectoryRights(fExportLibraryInformationPath, GetEveryoneName, ACL_RIGHT_FULL);
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

end.

