unit ToolMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ, Settings;

type
  { TDreamcastToolManager }
  TDreamcastToolManager = class(TObject)
  private
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fRepositoryInternetProtocol: TDreamcastSoftwareDevelopmentRepository;
    fRepositorySerial: TDreamcastSoftwareDevelopmentRepository;
    function GetBuilt: Boolean;
    function GetInstalled: Boolean;
    function GetRepositoryReady: Boolean;
    function GetSettings: TDreamcastSoftwareDevelopmentSettingsDreamcastTool;
  protected
    function DoRepositoryOperation(Kind: TDreamcastToolKind;
      var UpdateOperationState: TUpdateOperationState;
      var BufferOutput: string): Boolean;

    function GetDreamcastToolExecutableFileName: TFileName;
    function GenerateDreamcastToolCommandLine: string;

    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property Settings: TDreamcastSoftwareDevelopmentSettingsDreamcastTool
      read GetSettings;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;

    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Build(var BufferOutput: string): Boolean;
    function Install: Boolean;

    property Built: Boolean read GetBuilt;
    property Installed: Boolean read GetInstalled;

    property RepositoryReady: Boolean read GetRepositoryReady;
    property RepositorySerial: TDreamcastSoftwareDevelopmentRepository
      read fRepositorySerial;
    property RepositoryInternetProtocol: TDreamcastSoftwareDevelopmentRepository
      read fRepositoryInternetProtocol;
  end;

implementation

uses
  FSTools, FileUtil, IniFiles;

{ TDreamcastToolManager }

function TDreamcastToolManager.GetBuilt: Boolean;
begin
  Result := FileExists(Environment.FileSystem.DreamcastTool.InternetProtocolExecutable)
    and FileExists(Environment.FileSystem.DreamcastTool.SerialExecutable);
end;

function TDreamcastToolManager.GetInstalled: Boolean;
begin
  Result := Environment.IsComponentInstalled(Environment.FileSystem.DreamcastTool.SerialDirectory)
    and Environment.IsComponentInstalled(Environment.FileSystem.DreamcastTool.InternetProtocolDirectory);
end;

function TDreamcastToolManager.GetRepositoryReady: Boolean;
begin
  Result := RepositorySerial.Ready and RepositoryInternetProtocol.Ready;
end;

function TDreamcastToolManager.GetSettings: TDreamcastSoftwareDevelopmentSettingsDreamcastTool;
begin
  Result := Environment.Settings.DreamcastTool;
end;

function TDreamcastToolManager.DoRepositoryOperation(Kind: TDreamcastToolKind;
  var UpdateOperationState: TUpdateOperationState;
  var BufferOutput: string): Boolean;
var
  Url: string;
  InstallationDirectoryName,
  InstallationDirectoryPath: TFileName;
  IsRepositoryReady: Boolean;

begin
  Result := True;
  IsRepositoryReady := True;

  if not DirectoryExists(Environment.FileSystem.DreamcastTool.BaseDirectory) then
    ForceDirectories(Environment.FileSystem.DreamcastTool.BaseDirectory);

  Url := EmptyStr;
  InstallationDirectoryName := EmptyStr;
  InstallationDirectoryPath := EmptyStr;

  case Kind of
    dtkSerial:
      begin
        Url := Environment.Settings.Repositories.DreamcastToolSerialURL;
        InstallationDirectoryName := DCLOAD_SERIAL_INSTALLATION_DIRECTORY;
        InstallationDirectoryPath := Environment.FileSystem.DreamcastTool.SerialDirectory;
        IsRepositoryReady := RepositorySerial.Ready;
      end;
    dtkInternetProtocol:
      begin
        Url := Environment.Settings.Repositories.DreamcastToolInternetProtocolURL;
        InstallationDirectoryName := DCLOAD_IP_INSTALLATION_DIRECTORY;
        InstallationDirectoryPath := Environment.FileSystem.DreamcastTool.InternetProtocolDirectory;
        IsRepositoryReady := RepositoryInternetProtocol.Ready;
      end;
    dtkUndefined:
      begin
        Result := False;
        Exit;
      end;
  end;

  if Result then
    if IsRepositoryReady then
    begin
      // Update the repository if it already exists
      UpdateOperationState := Environment.UpdateRepository(
        InstallationDirectoryPath, BufferOutput);
      Result := (UpdateOperationState <> uosUpdateFailed);
    end
    else
    begin
      // Initialize the repository
      Result := Environment.CloneRepository(Url, InstallationDirectoryName,
        Environment.FileSystem.DreamcastTool.BaseDirectory, BufferOutput);
    end;
end;

function TDreamcastToolManager.GetDreamcastToolExecutableFileName: TFileName;

  function _Parse(FileName: TFileName): TFileName;
  begin
    Result := ExtractFileName(ChangeFileExt(FileName, ''));
  end;

begin
  Result := '';
  if Settings.Kind = dtkCustom then
    Result := Settings.CustomExecutable
  else
    with Environment.FileSystem.DreamcastTool do
    begin
      case Settings.Kind of
        dtkSerial: Result := _Parse(SerialExecutable);
        dtkInternetProtocol: Result := _Parse(InternetProtocolExecutable);
      end;
    end;
end;

function TDreamcastToolManager.GenerateDreamcastToolCommandLine: string;
var
  CommandLine: string;

  procedure Concat(Param: string);
  begin
    CommandLine := CommandLine + ' ' + Param;
  end;

begin
  CommandLine := '';

  if (Settings.Kind = dtkSerial) or (Settings.Kind = dtkInternetProtocol) then
  begin
    with Settings do
    begin

      case Kind of
        dtkSerial:
          begin
            Concat(Format('-t %s', [SerialPortToString(SerialPort)]));
            Concat(Format('-b %s', [SerialBaudrateToString(SerialBaudrate)]));
            if (SerialBaudrate = dtb115200) and SerialBaudrateAlternate then
              Concat('-e');
            if SerialDumbTerminal then
              Concat('-p');
            if SerialExternalClock then
              Concat('-E');
          end;

        dtkInternetProtocol:
          Concat(Format('-t %s', [InternetProtocolAddress]));
      end;

      if not AttachConsoleFileserver then
        Concat('-n');
      if not ClearScreenBeforeDownload then
        Concat('-q');
    end;
  end
  else
    CommandLine := Settings.CustomArguments;

  Result := Trim(CommandLine);
end;

constructor TDreamcastToolManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;

  fRepositorySerial := TDreamcastSoftwareDevelopmentRepository.Create(
    fEnvironment, Environment.FileSystem.DreamcastTool.SerialDirectory);

  fRepositoryInternetProtocol := TDreamcastSoftwareDevelopmentRepository.Create(
    fEnvironment, Environment.FileSystem.DreamcastTool.InternetProtocolDirectory);
end;

destructor TDreamcastToolManager.Destroy;
begin
  fRepositorySerial.Free;
  fRepositoryInternetProtocol.Free;
  inherited Destroy;
end;

function TDreamcastToolManager.CloneRepository(var BufferOutput: string): Boolean;
var
  TempBuffer: string;

  function DoCloneRepo(Kind: TDreamcastToolKind): Boolean;
  var
    UnusedCrap: TUpdateOperationState;

  begin
    UnusedCrap := uosUndefined;
    Result := DoRepositoryOperation(Kind, UnusedCrap, TempBuffer);
    BufferOutput := BufferOutput + sLineBreak + TempBuffer;
  end;

begin
  Result := True;
  BufferOutput := '';
  Result := Result and DoCloneRepo(dtkSerial);
  Result := Result and DoCloneRepo(dtkInternetProtocol);
end;

function TDreamcastToolManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
var
  TempBuffer: string;
  ResultInternetProtocol,
  ResultSerial: TUpdateOperationState;

  function DoUpdateRepo(Kind: TDreamcastToolKind): TUpdateOperationState;
  begin
    Result := uosUndefined;
    DoRepositoryOperation(Kind, Result, TempBuffer);
    BufferOutput := BufferOutput + sLineBreak + TempBuffer;
  end;

  function Combine(Result1, Result2: TUpdateOperationState): TUpdateOperationState;
  begin
    Result := uosUndefined;
    if Result1 = Result2 then
      Result := Result1
    else if (Result1 = uosUpdateFailed) or (Result2 = uosUpdateFailed) then // worst case
      Result := uosUpdateFailed
    else if (Result1 = uosUpdateSuccess) or (Result2 = uosUpdateSuccess) then
      Result := uosUpdateSuccess;
  end;

begin
  Result := uosUndefined;
  BufferOutput := '';
  ResultSerial := DoUpdateRepo(dtkSerial);
  ResultInternetProtocol := DoUpdateRepo(dtkInternetProtocol);
  Result := Combine(ResultInternetProtocol, ResultSerial);
end;

function TDreamcastToolManager.InitializeEnvironment: Boolean;

  procedure PatchMakefile(const FileName: TFileName);
  begin
    PatchTextFile(FileName, '#STANDALONE_BINARY', 'STANDALONE_BINARY');
  end;

begin
  Result := True;
  PatchMakefile(Environment.FileSystem.DreamcastTool.InternetProtocolDirectory + 'Makefile.cfg');
  PatchMakefile(Environment.FileSystem.DreamcastTool.SerialDirectory + 'Makefile.cfg');
end;

function TDreamcastToolManager.Build(var BufferOutput: string): Boolean;
var
  TempBuffer: string;

  function DoBuild(WorkingDirectory, TargetFileName: TFileName): Boolean;
  begin
    // Build
    TempBuffer := Environment.ExecuteShellCommand('make', WorkingDirectory);
    BufferOutput := BufferOutput + sLineBreak + TempBuffer;

    // Install
    TempBuffer := Environment.ExecuteShellCommand('make install', WorkingDirectory);
    BufferOutput := BufferOutput + sLineBreak + TempBuffer;

    // Check
    Result := FileExists(TargetFileName) and (not Environment.ShellCommandError);
  end;

begin
  Result := True;
  Result := Result and DoBuild(Environment.FileSystem.DreamcastTool
    .InternetProtocolDirectory, Environment.FileSystem.DreamcastTool.InternetProtocolExecutable);
  Result := Result and DoBuild(Environment.FileSystem.DreamcastTool
    .SerialDirectory, Environment.FileSystem.DreamcastTool.SerialExecutable);
end;

function TDreamcastToolManager.Install: Boolean;
const
  SECTION_NAME = 'DreamcastTool';

var
  IniFile: TIniFile;

begin
  IniFile := TIniFile.Create(Environment.FileSystem.DreamcastTool.ConfigurationFileName);
  try
    IniFile.WriteInteger(SECTION_NAME, 'Kind', Integer(Settings.Kind));
    IniFile.WriteString(SECTION_NAME, 'Executable', GetDreamcastToolExecutableFileName);
    IniFile.WriteString(SECTION_NAME, 'CommandLine', GenerateDreamcastToolCommandLine);
    IniFile.WriteString(SECTION_NAME, 'InternetProtocolAddress', Settings.InternetProtocolAddress);
    IniFile.WriteBool(SECTION_NAME, 'MediaAccessControlEnabled', Settings.MediaAccessControlEnabled);
    IniFile.WriteString(SECTION_NAME, 'MediaAccessControlAddress', Settings.MediaAccessControlAddress);
    IniFile.WriteString(SECTION_NAME, 'HostMediaAccessControlAddress', Settings.HostMediaAccessControlAddress);
  finally
    IniFile.Free;
  end;
  Result := FileExists(Environment.FileSystem.DreamcastTool.ConfigurationFileName);
end;

end.

