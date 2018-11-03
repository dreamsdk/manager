unit ToolMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type

  { TDreamcastToolManager }

  TDreamcastToolManager = class(TObject)
  private
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetBuilt: Boolean;
    function GetInstalled: Boolean;
  protected
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Build(var BufferOutput: string): Boolean;
    function Install: Boolean;
    property Built: Boolean read GetBuilt;
    property Installed: Boolean read GetInstalled;
  end;

implementation

uses
  FileUtil;

{ TDreamcastToolManager }

function TDreamcastToolManager.GetBuilt: Boolean;
begin
  Result := FileExists(Environment.FileSystem.DreamcastTool.InternetProtocolExecutable)
    and FileExists(Environment.FileSystem.DreamcastTool.SerialExecutable);
end;

function TDreamcastToolManager.GetInstalled: Boolean;
begin
  Result := DirectoryExists(Environment.FileSystem.DreamcastTool.SerialDirectory)
    and DirectoryExists(Environment.FileSystem.DreamcastTool.InternetProtocolDirectory);
end;

constructor TDreamcastToolManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
end;

function TDreamcastToolManager.CloneRepository(var BufferOutput: string): Boolean;
const
  DCLOAD_IP_INSTALLATION_DIRECTORY = 'dcload-ip';
  DCLOAD_SERIAL_INSTALLATION_DIRECTORY = 'dcload-serial';
var
  TempBuffer: string;

  function DoCloneRepo(Url: string; InstallationDirectory: TFileName): Boolean;
  begin
    Result := Environment.CloneRepository(Url, InstallationDirectory,
      Environment.FileSystem.DreamcastTool.BaseDirectory, TempBuffer);
    BufferOutput := BufferOutput + sLineBreak + TempBuffer;
  end;

begin
  Result := True;
  BufferOutput := '';

  if not DirectoryExists(Environment.FileSystem.DreamcastTool.BaseDirectory) then
    ForceDirectories(Environment.FileSystem.DreamcastTool.BaseDirectory);

  Result := Result and DoCloneRepo(Environment.Repositories.DreamcastToolInternetProtocolURL, DCLOAD_IP_INSTALLATION_DIRECTORY);
  Result := Result and DoCloneRepo(Environment.Repositories.DreamcastToolSerialURL, DCLOAD_SERIAL_INSTALLATION_DIRECTORY);
end;

function TDreamcastToolManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
var
  TempBuffer: string;
  ResultInternetProtocol,
  ResultSerial: TUpdateOperationState;

  function DoUpdateRepo(InstallationDirectory: TFileName): TUpdateOperationState;
  begin
    Result := Environment.UpdateRepository(InstallationDirectory, TempBuffer);
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

  ResultInternetProtocol := DoUpdateRepo(Environment.FileSystem.DreamcastTool.InternetProtocolDirectory);
  ResultSerial := DoUpdateRepo(Environment.FileSystem.DreamcastTool.SerialDirectory);
  Result := Combine(ResultInternetProtocol, ResultSerial);
end;

function TDreamcastToolManager.InitializeEnvironment: Boolean;

  procedure PatchMakefile(const FileName: TFileName);
  begin
    Environment.PatchMakefile(FileName, '#STANDALONE_BINARY', 'STANDALONE_BINARY');
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
    Result := FileExists(TargetFileName);
  end;

begin
  Result := True;
  Result := Result and DoBuild(Environment.FileSystem.DreamcastTool.InternetProtocolDirectory, Environment.FileSystem.DreamcastTool.InternetProtocolExecutable);
  Result := Result and DoBuild(Environment.FileSystem.DreamcastTool.SerialDirectory, Environment.FileSystem.DreamcastTool.SerialExecutable);
end;

function TDreamcastToolManager.Install: Boolean;
var
  SourceBinary: TFileName;

begin
  Result := True;
  if not FileExists(Environment.FileSystem.DreamcastTool.BaseExecutable) then
  begin
    SourceBinary := '';
    case Environment.Settings.DreamcastToolKind of
      dtkInternetProtocol:
        SourceBinary := Environment.FileSystem.DreamcastTool.InternetProtocolExecutable;
      dtkSerial:
        SourceBinary := Environment.FileSystem.DreamcastTool.SerialExecutable;
    end;
    if FileExists(SourceBinary) then
    begin
      Result := CopyFile(SourceBinary, Environment.FileSystem.DreamcastTool.BaseExecutable, True);
    end;
  end;
end;

end.

