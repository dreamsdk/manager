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
    function GetBuilt: Boolean;
    function GetInstalled: Boolean;
    function GetSettings: TDreamcastSoftwareDevelopmentSettingsDreamcastTool;
  protected
    function GenerateDreamcastToolCommandLine: string;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property Settings: TDreamcastSoftwareDevelopmentSettingsDreamcastTool
      read GetSettings;
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
  SysTools, FileUtil;

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

function TDreamcastToolManager.GetSettings: TDreamcastSoftwareDevelopmentSettingsDreamcastTool;
begin
  Result := Environment.Settings.DreamcastTool;
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

  if Settings.Kind <> dtkUndefined then
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
      if AlwaysStartDebugger then
        Concat('-g');
    end;

  Result := Trim(CommandLine);
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

  Result := Result and DoCloneRepo(Environment.Settings.Repositories.DreamcastToolInternetProtocolURL, DCLOAD_IP_INSTALLATION_DIRECTORY);
  Result := Result and DoCloneRepo(Environment.Settings.Repositories.DreamcastToolSerialURL, DCLOAD_SERIAL_INSTALLATION_DIRECTORY);
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
    Environment.PatchTextFile(FileName, '#STANDALONE_BINARY', 'STANDALONE_BINARY');
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
const
  COMMAND_LINE_TAG = 'dreamcast_tool_parameters=';

var
  Buffer: TStringList;
  EnvironShellScriptFileName: TFileName;
  i: Integer;
  SourceLine: string;

  function IsCommented(const SourceLine: string): Boolean;
  begin
    Result := Copy(SourceLine, 1, 1) = '#';
  end;

  function MakeLine(const SourceLine, NewCommandLine: string): string;
  var
    RightStr: string;

  begin
    RightStr := ExtremeRight('"', SourceLine);
    Result := Format('%s"%s"%s', [COMMAND_LINE_TAG, NewCommandLine, RightStr]);
  end;

begin
  Result := False;
  EnvironShellScriptFileName := Environment.FileSystem.DreamcastTool.BaseShellScriptExecutable;
  if FileExists(EnvironShellScriptFileName) then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.LoadFromFile(EnvironShellScriptFileName);
      for i := 0 to Buffer.Count - 1 do
      begin
        SourceLine := Buffer[i];
        if (IsInString(COMMAND_LINE_TAG, SourceLine)) and (not IsCommented(SourceLine)) then
        begin
          Buffer[i] := MakeLine(SourceLine, GenerateDreamcastToolCommandLine);
          Result := True;
        end;
      end;
      Buffer.SaveToFile(EnvironShellScriptFileName);
    finally
      Buffer.Free;
    end;
  end;
end;

end.

