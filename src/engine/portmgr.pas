unit PortMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Environ, SysTools;

type
  TOverridenInformationKind = (oikLibrary, oikInclude);
  TKallistiPortManager = class;

  { TKallistiPortItem }
  TKallistiPortItem = class(TObject)
  private
    fOwner: TKallistiPortManager;
    fListIndex: Integer;
    fSourceDirectory: TFileName;
    fInstallDirectory: TFileName;
    fDescription: string;
    fLicense: string;
    fMaintainer: string;
    fName: string;
    fShortDescription: string;
    fURL: string;
    fVersion: string;
    fSelfIncludes: string;
    fSelfLibraries: string;
    fDeclaredDependenciesNameList: TStringList;
    fFullComputedDependenciesNameList: TStringList;
    fFullComputedDependenciesLibraries: string;
    function GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetLibraries: string;
    function IsPortInstalled: Boolean;
    function DeleteInstallPortDirectoryIfNeeded: Boolean;
    property Index: Integer read fListIndex;
  protected
    function ExecuteShellCommand(const CommandLine: string): string;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read GetEnvironment;
  public
    constructor Create(AOwner: TKallistiPortManager);
    destructor Destroy; override;
    function Install(var BufferOutput: string): Boolean;
    function Uninstall(var BufferOutput: string): Boolean;
    function Update(var BufferOutput: string): TUpdateOperationState;
    property Name: string read fName;
    property Description: string read fDescription;
    property SourceDirectory: TFileName read fSourceDirectory;
    property Includes: string read fSelfIncludes;
    property InstallDirectory: TFileName read fInstallDirectory;
    property Installed: Boolean read IsPortInstalled;
    property Maintainer: string read fMaintainer;
    property Libraries: string read GetLibraries;
    property License: string read fLicense;
    property ShortDescription: string read fShortDescription;
    property URL: string read fURL;
    property Version: string read fVersion;
  end;

  { TKallistiPortManager }
  TKallistiPortManager = class(TObject)
  private
    fList: TList;
    fPortsMap: TStringIntegerMap;
    fPortsWithDependencies: TIntegerList;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fOverrideInformation: TIniFile;
    fProcessDependenciesCurrentPortInfo: TKallistiPortItem;
    fProcessDependenciesCallCount: LongWord;
    function GetCount: Integer;
    function GetInstalled: Boolean;
    function GetItem(Index: Integer): TKallistiPortItem;
    function Add: TKallistiPortItem;
    procedure Clear;
    function GetRepositoryReady: Boolean;
    function GetUtilityDirectory: TFileName;
    procedure ProcessPort(const PackagingDescriptionFilename: TFileName);
    procedure ProcessPortsDependencies;
    function GetPortLibrary(const PortName: string): string;
    function GetPortWeight(const PortName: string): Integer;
    function GetOverridenInformation(InformationKind: TOverridenInformationKind;
      PortName: string): string;
    procedure ProcessDependenciesInitialize(PortInfo: TKallistiPortItem);
    function ProcessPortDependencies(PortInfo: TKallistiPortItem): string;
  protected
    property UtilityDirectory: TFileName read GetUtilityDirectory;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
    destructor Destroy; override;
    function CloneRepository(var BufferOutput: string): Boolean;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Install(var OutputBuffer: string): Boolean;
    function Uninstall(var OutputBuffer: string): Boolean;
    procedure RetrieveAvailablePorts;
    property Count: Integer read GetCount;
    property Installed: Boolean read GetInstalled;
    property Items[Index: Integer]: TKallistiPortItem read GetItem; default;
    property RepositoryReady: Boolean read GetRepositoryReady;
  end;

implementation

uses
  FileUtil;

const
  KALLISTI_PORTS_DIRECTORY = '/opt/toolchains/dc/kos-ports/';
  KALLISTI_PORTS_UTILS_DIRECTORY = KALLISTI_PORTS_DIRECTORY + 'utils/';
  KALLISTI_PORTS_ALL_BUILD = KALLISTI_PORTS_UTILS_DIRECTORY + 'build-all.sh';
  KALLISTI_PORTS_ALL_CLEAN = KALLISTI_PORTS_UTILS_DIRECTORY + 'clean-all.sh';
  KALLISTI_PORTS_ALL_UNINSTALL = KALLISTI_PORTS_UTILS_DIRECTORY + 'uninstall-all.sh';
  KALLISTI_PORTS_RESET = DREAMSDK_MSYS_INSTALL_SCRIPTS_DIRECTORY + 'kos-ports-reset';

  PROCESS_DEPENDENCIES_MAX_DEPTH = 128;
  PROCESS_DEPENDENCIES_CIRCULAR_REFERENCE_ERROR = '#CIRCULAR_REFERENCE_ERROR#';

{ TKallistiPortItem }

function TKallistiPortItem.IsPortInstalled: Boolean;
begin
  Result := FileExists(fSourceDirectory + '..\lib\.kos-ports\' + Name);
end;

function TKallistiPortItem.DeleteInstallPortDirectoryIfNeeded: Boolean;
begin
  Result := False;
  if DirectoryExists(InstallDirectory) then
    Result := DeleteDirectory(InstallDirectory, False);
end;

function TKallistiPortItem.ExecuteShellCommand(const CommandLine: string): string;
begin
  Result := Environment.ExecuteShellCommand(CommandLine, SourceDirectory);
end;

function TKallistiPortItem.GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
begin
  Result := fOwner.fEnvironment;
end;

function TKallistiPortItem.GetLibraries: string;
begin
  Result := fSelfLibraries;
  if fFullComputedDependenciesNameList.Count > 0 then
    Result := fFullComputedDependenciesLibraries;
end;

constructor TKallistiPortItem.Create(AOwner: TKallistiPortManager);
begin
  fOwner := AOwner;
  fDeclaredDependenciesNameList := TStringList.Create;
  fFullComputedDependenciesNameList := TStringList.Create;
end;

destructor TKallistiPortItem.Destroy;
begin
  fDeclaredDependenciesNameList.Free;
  fFullComputedDependenciesNameList.Free;
  inherited Destroy;
end;

function TKallistiPortItem.Install(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Marking %s %s as installed.';

var
  SuccessTag: string;

begin
  DeleteInstallPortDirectoryIfNeeded;
  SuccessTag := Format(SUCCESS_TAG, [Name, Version]);
  BufferOutput := ExecuteShellCommand('make install');
  ExecuteShellCommand('make clean');
  Result := IsInString(SuccessTag, BufferOutput);
end;

function TKallistiPortItem.Uninstall(var BufferOutput: string): Boolean;
const
  SUCCESS_TAG = 'Uninstalled %s.';

var
  SuccessTag: string;

begin
  SuccessTag := Format(SUCCESS_TAG, [Name]);
  BufferOutput := ExecuteShellCommand('make uninstall');
  Result := IsInString(SuccessTag, BufferOutput);
  DeleteInstallPortDirectoryIfNeeded;
end;

function TKallistiPortItem.Update(var BufferOutput: string): TUpdateOperationState;
const
  SUCCESS_TAG = '%s is already installed';

var
  SuccessTag: string;

begin
  Result := uosUndefined;

  SuccessTag := Format(SUCCESS_TAG, [Name]);

  if Install(BufferOutput) then
    Result := uosUpdateSuccess
  else
    if IsInString(SuccessTag, BufferOutput) then
      Result := uosUpdateUseless
    else
      Result := uosUpdateFailed;
end;

{ TKallistiPortManager }

procedure TKallistiPortManager.RetrieveAvailablePorts;
const
  KALLISTI_PORTS_PACKAGE_DESCRIPTION = 'pkg-descr';

var
  PortsAvailable: TStringList;
  i: Integer;

begin
  Clear;
  PortsAvailable := TStringList.Create;
  try
    FindAllFiles(PortsAvailable, Environment.FileSystem.Kallisti.KallistiPortsDirectory,
      KALLISTI_PORTS_PACKAGE_DESCRIPTION, True);
    for i := 0 to PortsAvailable.Count - 1 do
      ProcessPort(PortsAvailable[i]);
    ProcessPortsDependencies;
  finally
    PortsAvailable.Free;
  end;
end;

function TKallistiPortManager.GetItem(Index: Integer): TKallistiPortItem;
begin
  Result := TKallistiPortItem(fList[Index]);
end;

function TKallistiPortManager.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TKallistiPortManager.GetInstalled: Boolean;
begin
  Result := DirectoryExists(Environment.FileSystem.Kallisti.KallistiPortsDirectory);
end;

function TKallistiPortManager.Add: TKallistiPortItem;
begin
  Result := TKallistiPortItem.Create(Self);
  Result.fListIndex := fList.Add(Result);
end;

procedure TKallistiPortManager.Clear;
var
  i: Integer;

begin
  for i := 0 to fList.Count - 1 do
    TKallistiPortItem(fList[i]).Free;
  fList.Clear;
  fPortsWithDependencies.Clear;
  fPortsMap.Clear;
end;

function TKallistiPortManager.GetRepositoryReady: Boolean;
begin
  Result := DirectoryExists(Environment.FileSystem.Kallisti.KallistiPortsDirectory
    + GIT_SYSTEM_DIRECTORY);
end;

function TKallistiPortManager.GetUtilityDirectory: TFileName;
begin
  Result := Environment.FileSystem.Kallisti.KallistiPortsDirectory + 'utils\';
end;

procedure TKallistiPortManager.ProcessPort(const PackagingDescriptionFilename: TFileName);
const
  MAKEFILE_FILE_NAME = 'Makefile';

var
  MakefileContent: TStringList;
  PortName, MakefileContentText, ExtractedURL: string;
  PortDirectory: TFileName;

  function GetPackageString(const Key: string): string;
  var
    S, Line: string;
    i: Integer;
    Buffer: TStringList;

  begin
    Result := EmptyStr;
    S := Right(Key, MakefileContentText);
    if S <> EmptyStr then
    begin
      // Handle simple case
      Result := Trim(ExtractStr('=', #10, S));

      // Handle multilines ('\')
      if EndsWith('\', Result) then
      begin
        Buffer := TStringList.Create;
        try
          Buffer.Text := Trim(Right(Result, MakefileContentText));
          Result := Trim(Left('\', Result));
          for i := 0 to Buffer.Count - 1 do
          begin
            Line := Buffer[i];
            if EndsWith('\', Line) then
              // There is another lines to proceed
              Result := Result + ' ' + Trim(Left('\', Line))
            else
            begin
              // This is the last line, so break
              Result := Result + ' ' + Trim(Line);
              Break;
            end;
          end;
        finally
          Buffer.Free;
        end;
      end;
    end;
  end;

  function SanitizeText(Text: string): string;
  begin
    Text := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
    Text := StringReplace(Text, #13, #10, [rfReplaceAll]);
    Text := StringReplace(Text, #9, '', [rfReplaceAll]);
    Result := Text;
  end;

  function GetPackageDescription(var URL: string): string;
  const
    URL_TAG = ' URL: ';

  var
    DescriptionContent: TStringList;

  begin
    Result := '';
    DescriptionContent := TStringList.Create;
    try
      if FileExists(PackagingDescriptionFilename) then
      begin
        DescriptionContent.LoadFromFile(PackagingDescriptionFilename);
        Result := SanitizeText(DescriptionContent.Text);
        Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
        Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
        if IsInString(URL_TAG, Result) then
        begin
          URL := Trim(Right(URL_TAG, Result));
          Result := Left(URL_TAG, Result);
        end;
        Result := Trim(Result);
      end;
    finally
      DescriptionContent.Free;
    end;
  end;

  function GetInstallDirectory: TFileName;
  begin
    Result := GetPackageString('HDR_INSTDIR');
    if Result <> '' then
      Result := PortDirectory + '..\include\' + Result;
  end;

  function GetPackageIncludes: string;
  var
    IncludeDirectory, IncludeFiles, IncludeFile: string;
    InputBuffer, OutputBuffer: TStringList;
    i: Integer;
    IsOverriden: Boolean;

  begin
    Result := EmptyStr;

    IncludeDirectory := EmptyStr;
    IncludeFiles := GetOverridenInformation(oikInclude, PortName);
    IsOverriden := not SameText(IncludeFiles, EmptyStr);

    if not IsOverriden then
    begin
      // Get the include directory
      IncludeDirectory := GetPackageString('HDR_INSTDIR');
      if SameText(IncludeDirectory, EmptyStr) then
        IncludeDirectory := PortName;
      IncludeDirectory := IncludeDirectory + '/';

      // Get all includes files
      IncludeFiles := Trim(StringReplace(GetPackageString('INSTALLED_HDRS'),
        'include/' + IncludeDirectory, EmptyStr, [rfReplaceAll]));
      IncludeFiles := Trim(StringReplace(IncludeFiles, 'include/', EmptyStr,
        [rfReplaceAll]));
    end;

    // Combine everything
    InputBuffer := TStringList.Create;
    OutputBuffer := TStringList.Create;
    try
      InputBuffer.Text := StringReplace(IncludeFiles, ' ', sLineBreak, [rfReplaceAll]);
      for i := 0 to InputBuffer.Count - 1 do
      begin
        IncludeFile := IncludeDirectory + Trim(InputBuffer[i]);
        if not SameText(IncludeFile, EmptyStr) then
          OutputBuffer.Add(Format('#include <%s>', [IncludeFile]));
      end;
      Result := StringListToString(OutputBuffer, '|');
    finally
      OutputBuffer.Free;
      InputBuffer.Free;
    end;
  end;

  function GetPackageLibraries: string;
  begin
    Result := GetOverridenInformation(oikLibrary, PortName);
    if SameText(Result, EmptyStr) then
    begin
      Result := ChangeFileExt(GetPackageString('TARGET'), EmptyStr);
      Result := Copy(Result, 4, Length(Result));
    end
    else
      Result := StringReplace(Trim(Result), ' ', '|', [rfReplaceAll]);
  end;

  function GetPackageDependencies: string;
  begin
    Result := GetPackageString('DEPENDENCIES');
    if not SameText(Result, EmptyStr) then
      Result := StringReplace(SuppressUselessWhiteSpaces(Result), WhiteSpaceStr,
        sLineBreak, [rfReplaceAll]);
  end;

begin
  PortDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(PackagingDescriptionFilename));
  MakefileContent := TStringList.Create;
  try
    MakefileContent.LoadFromFile(PortDirectory + MAKEFILE_FILE_NAME);
    MakefileContentText := SanitizeText(MakefileContent.Text);

    PortName := GetPackageString('PORTNAME');
    with Add do
    begin
      fSourceDirectory := PortDirectory;
      fName := PortName;
      fVersion := GetPackageString('PORTVERSION');
      fMaintainer := GetPackageString('MAINTAINER');
      fLicense := GetPackageString('LICENSE');
      fShortDescription := GetPackageString('SHORT_DESC');
      fDescription := GetPackageDescription(ExtractedURL);
      fURL := ExtractedURL;
      fInstallDirectory := GetInstallDirectory;
      fSelfIncludes := GetPackageIncludes;
      fSelfLibraries := GetPackageLibraries;
      fDeclaredDependenciesNameList.Text := GetPackageDependencies;
      if not SameText(fDeclaredDependenciesNameList.Text, EmptyStr) then
        fPortsWithDependencies.Add(Index);
      fPortsMap.Add(PortName, Index);
    end;

  finally
    MakefileContent.Free;
  end;
end;

procedure TKallistiPortManager.ProcessPortsDependencies;
var
  i, j: Integer;
  PortInfo: TKallistiPortItem;
  PortDependenciesStr, DependencyPortName, DependencyPortLibrary: string;
  PortOrderingBuffer: TStringList;

begin
{$IFDEF DEBUG}
  DebugLog('*** Process Ports Dependencies: Start ***');
{$ENDIF}

  for i := 0 to fPortsWithDependencies.Count - 1 do
  begin
    PortInfo := Items[fPortsWithDependencies[i]];
{$IFDEF DEBUG}
    DebugLog('  Processing: ' + PortInfo.Name);
{$ENDIF}
    ProcessDependenciesInitialize(PortInfo);
    PortDependenciesStr := ProcessPortDependencies(PortInfo);
    if not IsInString(PROCESS_DEPENDENCIES_CIRCULAR_REFERENCE_ERROR, PortDependenciesStr) then
    begin
      PortInfo.fFullComputedDependenciesNameList.Text := PortDependenciesStr;
      PortOrderingBuffer := TStringList.Create;
      try
        // Sort by Port weights
        for j := 0 to PortInfo.fFullComputedDependenciesNameList.Count - 1 do
        begin
          DependencyPortName := PortInfo.fFullComputedDependenciesNameList[j];
          DependencyPortLibrary := GetPortLibrary(DependencyPortName);
          PortOrderingBuffer.Add(Format('%0.8d:%s', [GetPortWeight(DependencyPortName), DependencyPortLibrary]));
        end;
        PortOrderingBuffer.Sort;

{$IFDEF DEBUG}
        DebugLog(StringListToString(PortOrderingBuffer, ' + '));
{$ENDIF}

        // Remove all weight information
        for j := 0 to PortOrderingBuffer.Count - 1 do
          PortOrderingBuffer[j] := Right(':', PortOrderingBuffer[j]);

        // Store all the dependencies libraries, ordered in the good way!
        // This is the final result of all of this...
        PortInfo.fFullComputedDependenciesLibraries := StringListToString(PortOrderingBuffer, '|');
      finally
        PortOrderingBuffer.Free;
      end;
    end;
  end;

{$IFDEF DEBUG}
  DebugLog(sLineBreak + 'Results:');
  for i := 0 to fPortsWithDependencies.Count - 1 do
  begin
    PortInfo := Items[fPortsWithDependencies[i]];
    DebugLog('  ' + PortInfo.Name + ': "' +
      StringListToString(PortInfo.fFullComputedDependenciesNameList, ' + ') + '"' +
      ' (vs. "' + StringListToString(PortInfo.fDeclaredDependenciesNameList, ' + ') + '")');
  end;

  DebugLog('*** Process Ports Dependencies: End ***');
{$ENDIF}
end;

function TKallistiPortManager.GetPortLibrary(const PortName: string): string;
var
  Index: Integer;

begin
  Result := EmptyStr;
  Index := fPortsMap.IndexOf(PortName);
  if Index <> -1 then
    Result := Items[Index].fSelfLibraries;
end;

function TKallistiPortManager.GetPortWeight(const PortName: string): Integer;
begin
  Result := fOverrideInformation.ReadInteger('Weights', PortName, 0);
end;

function TKallistiPortManager.GetOverridenInformation(
  InformationKind: TOverridenInformationKind; PortName: string): string;
var
  SectionName: string;

begin
  SectionName := 'Libraries';
  if InformationKind = oikInclude then
    SectionName := 'Includes';
  Result := fOverrideInformation.ReadString(SectionName, PortName, EmptyStr);
end;

procedure TKallistiPortManager.ProcessDependenciesInitialize(
  PortInfo: TKallistiPortItem);
begin
  fProcessDependenciesCurrentPortInfo := PortInfo;
  fProcessDependenciesCallCount := 0;
end;

function TKallistiPortManager.ProcessPortDependencies(PortInfo: TKallistiPortItem): string;
var
  j, DependencyPortIndex: Integer;
  SubPortInfo: TKallistiPortItem;

begin
  // Fail-safe for circular references detection...
  Inc(fProcessDependenciesCallCount);
{$IFDEF DEBUG}
  DebugLog(Format('ProcessPortDependencies for %s: %d',
    [fProcessDependenciesCurrentPortInfo.Name, fProcessDependenciesCallCount]
  ));
{$ENDIF}
  if fProcessDependenciesCallCount >= PROCESS_DEPENDENCIES_MAX_DEPTH then
  begin
    Result := PROCESS_DEPENDENCIES_CIRCULAR_REFERENCE_ERROR;
    Exit;
  end;

  // Recursive code
  if PortInfo.fDeclaredDependenciesNameList.Count = 0 then
  begin
    Result := PortInfo.Name;
  end
  else
  begin
    Result := PortInfo.Name;
    for j := 0 to PortInfo.fDeclaredDependenciesNameList.Count - 1 do
    begin
      DependencyPortIndex := fPortsMap.IndexOf(PortInfo.fDeclaredDependenciesNameList[j]);
      if DependencyPortIndex <> -1 then
      begin
        SubPortInfo := Items[DependencyPortIndex];
        Result := Result + sLineBreak + ProcessPortDependencies(SubPortInfo);
      end;
    end;
  end;
end;

constructor TKallistiPortManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment);
begin
  fEnvironment := AEnvironment;
  fList := TList.Create;
  fPortsWithDependencies := TIntegerList.Create;
  fPortsMap := TStringIntegerMap.Create;
  fOverrideInformation := TIniFile.Create(Environment.FileSystem.Kallisti.KallistiPortsOverrideFile);
  RetrieveAvailablePorts;
end;

destructor TKallistiPortManager.Destroy;
begin
  Clear;
  fPortsWithDependencies.Free;
  fList.Free;
  fPortsMap.Free;
  fOverrideInformation.Free;
  inherited Destroy;
end;

function TKallistiPortManager.CloneRepository(var BufferOutput: string): Boolean;
const
  KALLISTI_PORTS_INSTALLATION_DIRECTORY = 'kos-ports';

begin
  Result := Environment.CloneRepository(Environment.Settings.Repositories.KallistiPortsURL,
    KALLISTI_PORTS_INSTALLATION_DIRECTORY,
    Environment.FileSystem.Kallisti.KallistiPortsDirectory + '..\', BufferOutput);
end;

function TKallistiPortManager.UpdateRepository(var BufferOutput: string): TUpdateOperationState;
begin
  Result := Environment.UpdateRepository(
    Environment.FileSystem.Kallisti.KallistiPortsDirectory, BufferOutput);
end;

function TKallistiPortManager.InitializeEnvironment: Boolean;
const
  CONFIG_MAKEFILE = 'config.mk';
  BUILD_MAKEFILE = 'scripts\build.mk';
  DOWNLOAD_MAKEFILE = 'scripts\download.mk';

var
  ConfigFileName,
  BuildFileName,
  DownloadFileName: TFileName;

begin
  ConfigFileName := Environment.FileSystem.Kallisti.KallistiPortsDirectory + CONFIG_MAKEFILE;
  BuildFileName := Environment.FileSystem.Kallisti.KallistiPortsDirectory + BUILD_MAKEFILE;
  DownloadFileName := Environment.FileSystem.Kallisti.KallistiPortsDirectory + DOWNLOAD_MAKEFILE;

  Result := FileExists(ConfigFileName) and FileExists(BuildFileName);
  if Result then
  begin
    Environment.PatchTextFile(
      ConfigFileName,
      '#FETCH_CMD = wget',
      'FETCH_CMD = wget --no-check-certificate'
    );

    Environment.PatchTextFile(
      ConfigFileName,
      'FETCH_CMD = curl',
      '#FETCH_CMD = curl'
    );

    Environment.PatchTextFile(
      DownloadFileName,
      'svn checkout',
      'svn checkout --non-interactive --trust-server-cert'
    );

    // ln doesn't work very well under MinGW/MSYS...
    Environment.PatchTextFile(
      BuildFileName,
      'ln -s',
      'cp -r'
    );
  end;
end;

function TKallistiPortManager.Install(var OutputBuffer: string): Boolean;
const
  FAILED_TAG = 'Bailing out.';

begin
  Result := False;
  if Installed then
  begin
    OutputBuffer := Environment.ExecuteShellCommand(KALLISTI_PORTS_ALL_BUILD, UtilityDirectory);
    Environment.ExecuteShellCommand(KALLISTI_PORTS_ALL_CLEAN, UtilityDirectory);
    Result := not IsInString(FAILED_TAG, OutputBuffer);
  end;
end;

function TKallistiPortManager.Uninstall(var OutputBuffer: string): Boolean;
begin
  if Installed then
  begin
    OutputBuffer := Environment.ExecuteShellCommand(KALLISTI_PORTS_ALL_UNINSTALL, UtilityDirectory);
    Environment.ExecuteShellCommand(KALLISTI_PORTS_RESET, UtilityDirectory);
  end;
  Result := Installed;
end;

end.

