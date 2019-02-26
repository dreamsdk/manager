unit PortMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, SysTools, Environ, IDEMgr;

type
  TLibraryLanguageKind = (llkAll, llkC, llkCPP);
  TOverridenInformationKind = (oikLibrary, oikInclude);

  TKallistiPortManager = class;

  { TKallistiPortItem }
  TKallistiPortItem = class(TObject)
  private
    fOwner: TKallistiPortManager;
    fVirtualAddon: Boolean;
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
    fFullComputedDependenciesLibraryWeights: string;
    fLibraryLanguageKind: TLibraryLanguageKind;
    function GetEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    function GetLibraries: string;
    function IsPortInstalled: Boolean;
    function DeleteInstallPortDirectoryIfNeeded: Boolean;
  protected
    function DoInstallOrUpdate: string;
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
    property Hidden: Boolean read fVirtualAddon;
    property LibraryWeights: string read fFullComputedDependenciesLibraryWeights;
  end;

  { TKallistiPortManager }
  TKallistiPortManager = class(TObject)
  private
    fOnlyVisibleListCount: Integer;
    fList: TList;
    fPortsMap: TStringIntegerMap;
    fPortsWithDependencies: TIntegerList;
    fPortsWithoutDependencies: TIntegerList;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
    fKallistiLibraryInformation: TIniFile;
    fProcessDependenciesCurrentPortInfo: TKallistiPortItem;
    fProcessDependenciesCallCount: LongWord;
    fAddonsId: TStringList;
    fAddonsIncludes: TStringList;
    fAddonsLibraries: TStringList;
    function GetCount: Integer;
    function GetInstalled: Boolean;
    function GetItem(Index: Integer): TKallistiPortItem;
    function Add: TKallistiPortItem;
    procedure Clear;
    function GetRepositoryReady: Boolean;
    function GetUtilityDirectory: TFileName;
    procedure ProcessPort(const PortDirectoryBaseName: TFileName);
    procedure ProcessPortsDependencies;
    procedure ProcessPortsWithoutDependencies;
    function GetPortLibrary(const PortName: string): string;
    function GetPortWeight(const PortName: string): string;
    function GetPortLanguageKind(const PortName: string): TLibraryLanguageKind;
    function GetOverridenInformation(InformationKind: TOverridenInformationKind;
      PortName: string): string;
    procedure ProcessDependenciesInitialize(PortInfo: TKallistiPortItem);
    function ProcessPortDependencies(PortInfo: TKallistiPortItem): string;
    function GenerateIncludeHeader(const IncludeFiles: string;
      const IncludeDirectory: TFileName): string;
    function GetPortIndex(const PortName: string): Integer;
    procedure HandleLibrary(OutputBuffer: TStringList; PortName: string);
    procedure SetPortAdditionalInformation(PortInfo: TKallistiPortItem;
      ProcessDependencies: Boolean);
  protected
    procedure GenerateIntegratedDevelopmentEnvironmentLibraryInformation(
      const LibraryLanguageKind: TLibraryLanguageKind);
    function GetAddonIncludes(const AddonIndex: Integer): string;
    function GetAddonIndex(const AddonName: string): Integer;
    procedure RetrieveAvailableAddons;
    property UtilityDirectory: TFileName read GetUtilityDirectory;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property IntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment
      read fIntegratedDevelopmentEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
      AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment);
    destructor Destroy; override;
    function CloneRepository(var BufferOutput: string): Boolean;
    procedure GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
    function UpdateRepository(var BufferOutput: string): TUpdateOperationState;
    function InitializeEnvironment: Boolean;
    function Install(var OutputBuffer: string): Boolean;
    function Uninstall(var OutputBuffer: string): Boolean;
    procedure RetrieveAvailablePorts;
    property Count: Integer read GetCount;
    property CountVisible: Integer read fOnlyVisibleListCount;
    property Installed: Boolean read GetInstalled;
    property Items[Index: Integer]: TKallistiPortItem read GetItem; default;
    property RepositoryReady: Boolean read GetRepositoryReady;
  end;

implementation

uses
  FileUtil;

const
  KALLISTI_PORTS_PACKAGE_DESCRIPTION = 'pkg-descr';
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
  Result := FileExists(fSourceDirectory + '..\lib\.kos-ports\' + Name)
    or fVirtualAddon;
end;

function TKallistiPortItem.DeleteInstallPortDirectoryIfNeeded: Boolean;
begin
  Result := False;
  if DirectoryExists(InstallDirectory) then
    Result := DeleteDirectory(InstallDirectory, False);
end;

function TKallistiPortItem.DoInstallOrUpdate: string;
begin
  Result := ExecuteShellCommand('make install');
  ExecuteShellCommand('make clean');
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
  BufferOutput := DoInstallOrUpdate;
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
  USELESS_TAG = '%s is already installed';
  SUCCESS_TAG = 'Marking %s %s as installed.';

var
  SuccessTag,
  UselessTag: string;

begin
  Result := uosUndefined;

  SuccessTag := Format(SUCCESS_TAG, [Name, Version]);
  UselessTag := Format(USELESS_TAG, [Name]);

  BufferOutput := DoInstallOrUpdate;
  if IsInString(SuccessTag, BufferOutput) then
    Result := uosUpdateSuccess
  else
    if IsInString(UselessTag, BufferOutput) then
      Result := uosUpdateUseless
    else
      Result := uosUpdateFailed;
end;

{ TKallistiPortManager }

procedure TKallistiPortManager.RetrieveAvailablePorts;
var
  PortsAvailable: TStringList;
  i: Integer;

begin
  Clear;
  RetrieveAvailableAddons;

  PortsAvailable := TStringList.Create;
  try
    FindAllFiles(PortsAvailable, Environment.FileSystem.Kallisti.KallistiPortsDirectory,
      KALLISTI_PORTS_PACKAGE_DESCRIPTION, True);

    // Keeping only PortName information...
    for i := 0 to PortsAvailable.Count - 1 do
      PortsAvailable[i] :=
        ExtractStr(
          Environment.FileSystem.Kallisti.KallistiPortsDirectory,
          DirectorySeparator + KALLISTI_PORTS_PACKAGE_DESCRIPTION,
          PortsAvailable[i]
        );

    // Adding addons
    PortsAvailable.AddStrings(fAddonsId);

    // Sorting everything
    PortsAvailable.Sort;

{$IFDEF DEBUG}
    DebugLog(StringListToString(PortsAvailable, sLineBreak));
{$ENDIF}

    for i := 0 to PortsAvailable.Count - 1 do
      ProcessPort(PortsAvailable[i]);

    ProcessPortsDependencies;

    ProcessPortsWithoutDependencies;
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
  fPortsWithoutDependencies.Clear;
  fPortsMap.Clear;
  fOnlyVisibleListCount := 0;
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

procedure TKallistiPortManager.ProcessPort(const PortDirectoryBaseName: TFileName);
const
  MAKEFILE_FILE_NAME = 'Makefile';

var
  MakefileContent: TStringList;
  PortName, MakefileContentText, ExtractedURL: string;
  PortDirectory, Makefile, PortDescriptionFileName: TFileName;
  AddonIndex: Integer;

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
      PortDescriptionFileName := PortDirectory + KALLISTI_PORTS_PACKAGE_DESCRIPTION;
      if FileExists(PortDescriptionFileName) then
      begin
        DescriptionContent.LoadFromFile(PortDescriptionFileName);
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
    IncludeDirectory, IncludeFiles: string;
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
      begin
        IncludeDirectory := GetPackageString('HDR_COMDIR');
        if SameText(IncludeDirectory, EmptyStr) then
          IncludeDirectory := PortName;
      end;
      IncludeDirectory := IncludeDirectory + '/';

      // Get all includes files
      IncludeFiles := Trim(StringReplace(GetPackageString('INSTALLED_HDRS'),
        'include/' + IncludeDirectory, EmptyStr, [rfReplaceAll]));
      IncludeFiles := Trim(StringReplace(IncludeFiles, 'include/', EmptyStr,
        [rfReplaceAll]));
      IncludeFiles := Trim(StringReplace(IncludeFiles, 'src/', EmptyStr,
        [rfReplaceAll]));
    end;

    // Combine everything
    Result := GenerateIncludeHeader(IncludeFiles, IncludeDirectory);
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
      Result := StringReplace(Trim(Result), ' ', ArraySeparator, [rfReplaceAll]);
  end;

  function GetPackageDependencies: string;
  begin
    Result := GetPackageString('DEPENDENCIES');
    if not SameText(Result, EmptyStr) then
      Result := StringReplace(SuppressUselessWhiteSpaces(Result), WhiteSpaceStr,
        sLineBreak, [rfReplaceAll]);
  end;

begin
  PortDirectory := IncludeTrailingPathDelimiter(
    Environment.FileSystem.Kallisti.KallistiPortsDirectory + PortDirectoryBaseName);
  Makefile := PortDirectory + MAKEFILE_FILE_NAME;
  if FileExists(Makefile) then
  begin
    // Normal KallistiOS Port
    MakefileContent := TStringList.Create;
    try
      MakefileContent.LoadFromFile(Makefile);
      MakefileContentText := SanitizeText(MakefileContent.Text);

      ExtractedURL := EmptyStr;
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
          fPortsWithDependencies.Add(fListIndex)
        else
          fPortsWithoutDependencies.Add(fListIndex);
        fPortsMap.Add(PortName, fListIndex);
        fLibraryLanguageKind := GetPortLanguageKind(PortName);
        fVirtualAddon := False;
      end;

      Inc(fOnlyVisibleListCount);
    finally
      MakefileContent.Free;
    end;
  end
  else
  begin
    // KallistiOS Addon
    AddonIndex := GetAddonIndex(PortDirectoryBaseName);
    with Add do
    begin
      fName := PortDirectoryBaseName;
      fSelfIncludes := GetAddonIncludes(AddonIndex);
      fSelfLibraries := fAddonsLibraries[AddonIndex];
      fFullComputedDependenciesLibraryWeights := GetPortWeight(fName);
      fVirtualAddon := True;
    end;
  end;
end;

procedure TKallistiPortManager.ProcessPortsDependencies;
var
  i: Integer;
  PortInfo: TKallistiPortItem;
  PortDependenciesStr: string;

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
      SetPortAdditionalInformation(PortInfo, True);
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

procedure TKallistiPortManager.ProcessPortsWithoutDependencies;
var
  i: Integer;
  PortInfo: TKallistiPortItem;

begin
  for i := 0 to fPortsWithoutDependencies.Count - 1 do
  begin
    PortInfo := Items[fPortsWithoutDependencies[i]];
    SetPortAdditionalInformation(PortInfo, False);
  end;
end;

function TKallistiPortManager.GetPortLibrary(const PortName: string): string;
var
  Index: Integer;

begin
  Result := EmptyStr;
  Index := GetPortIndex(PortName);
  if Index <> -1 then
    Result := Items[Index].fSelfLibraries;
end;

function TKallistiPortManager.GetPortWeight(const PortName: string): string;
var
  Weight: Integer;

  function LibraryNameToPortName(const LibraryName: string): string;
  begin
    Result := fKallistiLibraryInformation.ReadString('Reverse', LibraryName, LibraryName);
  end;

begin
  Weight := fKallistiLibraryInformation.ReadInteger('Weights',
    LibraryNameToPortName(PortName), 0);

  if Weight = 0 then
    Weight := fKallistiLibraryInformation.ReadInteger('Weights',
      LibraryNameToPortName('lib' + PortName), 0);

  Result := Format('%0.8d', [Weight]);
end;

function TKallistiPortManager.GetPortLanguageKind(const PortName: string
  ): TLibraryLanguageKind;
begin
  Result := TLibraryLanguageKind(
    fKallistiLibraryInformation.ReadInteger('Languages', PortName, 0));
end;

function TKallistiPortManager.GetOverridenInformation(
  InformationKind: TOverridenInformationKind; PortName: string): string;
var
  SectionName: string;

begin
  SectionName := 'Libraries';
  if InformationKind = oikInclude then
    SectionName := 'Includes';
  Result := fKallistiLibraryInformation.ReadString(SectionName, PortName, EmptyStr);
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
      DependencyPortIndex := GetPortIndex(PortInfo.fDeclaredDependenciesNameList[j]);
      if DependencyPortIndex <> -1 then
      begin
        SubPortInfo := Items[DependencyPortIndex];
        Result := Result + sLineBreak + ProcessPortDependencies(SubPortInfo);
      end;
    end;
  end;
end;

function TKallistiPortManager.GenerateIncludeHeader(const IncludeFiles: string;
  const IncludeDirectory: TFileName): string;
var
  InputBuffer,
  OutputBuffer: TStringList;
  i: Integer;
  IncludeFile: string;

begin
  InputBuffer := TStringList.Create;
  OutputBuffer := TStringList.Create;
  try
    InputBuffer.Text := StringReplace(IncludeFiles, ' ', sLineBreak, [rfReplaceAll]);
    for i := 0 to InputBuffer.Count - 1 do
    begin
      IncludeFile := Trim(InputBuffer[i]);
      if not SameText(IncludeFile, EmptyStr) then
      begin
        IncludeFile := IncludeDirectory + IncludeFile;
        if not SameText(IncludeFile, EmptyStr) then
          OutputBuffer.Add(IncludeFile);
      end;
    end;
    Result := StringListToString(OutputBuffer, ArraySeparator);
  finally
    OutputBuffer.Free;
    InputBuffer.Free;
  end;
end;

function TKallistiPortManager.GetPortIndex(const PortName: string): Integer;
var
  KeyIndex: Integer;

begin
  Result := -1;
  KeyIndex := fPortsMap.IndexOf(PortName);
  if KeyIndex <> -1 then
    Result := fPortsMap.Data[KeyIndex];
end;

procedure TKallistiPortManager.HandleLibrary(OutputBuffer: TStringList;
  PortName: string);
var
  DependencyPortLibrary: string;

  function GenerateLine(const LibraryCode: string): string;
  begin
    Result := Format('%s:%s', [GetPortWeight(LibraryCode), LibraryCode]);
  end;

  function HandleMultipleLibrary(OutputBuffer: TStringList;
    MultipleLibraryStr: string): Boolean;
  var
    Buffer: TStringList;
    k: Integer;
    SinglePortName: string;

  begin
    Result := False;
    Buffer := TStringList.Create;
    try
      StringToStringList(MultipleLibraryStr, ArraySeparator, Buffer);
      for k := 0 to Buffer.Count - 1 do
      begin
        SinglePortName := Buffer[k];
        OutputBuffer.Add(GenerateLine(SinglePortName));
      end;
      Result := True;
    finally
      Buffer.Free;
    end;
  end;

begin
  if Assigned(OutputBuffer) then
  begin
    DependencyPortLibrary := GetPortLibrary(PortName);
    if IsInString(ArraySeparator, DependencyPortLibrary) then
      HandleMultipleLibrary(OutputBuffer, DependencyPortLibrary)
    else
    begin
      OutputBuffer.Add(GenerateLine(DependencyPortLibrary));
    end;
  end;
end;

procedure TKallistiPortManager.SetPortAdditionalInformation(
  PortInfo: TKallistiPortItem; ProcessDependencies: Boolean);
var
  j: Integer;
  DependencyPortName, DependencyPortLibrary, DependencyPortWeight: string;
  PortOrderingBuffer, WeightsBuffer: TStringList;

begin
  PortOrderingBuffer := TStringList.Create;
  try
    // Populate PortOrderingBuffer to sort by weight
    if ProcessDependencies then
    begin
      for j := 0 to PortInfo.fFullComputedDependenciesNameList.Count - 1 do
      begin
        DependencyPortName := PortInfo.fFullComputedDependenciesNameList[j];
        HandleLibrary(PortOrderingBuffer, DependencyPortName);
      end;
    end
    else
      HandleLibrary(PortOrderingBuffer, PortInfo.Name);

    // Sort by Port weights
    PortOrderingBuffer.Sort;

{$IFDEF DEBUG}
    DebugLog(StringListToString(PortOrderingBuffer, ' + '));
{$ENDIF}

    // Remove all weight information from the libraries names
    // Save the weight in a separate list
    WeightsBuffer := TStringList.Create;
    try
      for j := 0 to PortOrderingBuffer.Count - 1 do
      begin
        DependencyPortLibrary := Right(':', PortOrderingBuffer[j]);
        DependencyPortWeight := Left(':', PortOrderingBuffer[j]);

        // Saving only library name (without weight)
        PortOrderingBuffer[j] := DependencyPortLibrary;

        // Saving the libraries weight
        WeightsBuffer.Add(DependencyPortWeight);
      end;

      // Save the weights!
      PortInfo.fFullComputedDependenciesLibraryWeights :=
        StringListToString(WeightsBuffer, ArraySeparator);
    finally
      WeightsBuffer.Free;
    end;

    // Store all the dependencies libraries, ordered in the good way!
    // This is the final result of all of this...
    PortInfo.fFullComputedDependenciesLibraries :=
      StringListToString(PortOrderingBuffer, ArraySeparator);
  finally
    PortOrderingBuffer.Free;
  end;
end;

procedure TKallistiPortManager.GenerateIntegratedDevelopmentEnvironmentLibraryInformation
  (const LibraryLanguageKind: TLibraryLanguageKind);
const
  LIBINFO_PATH_C = 'c';
  LIBINFO_PATH_CPP = 'cpp';
  LIBINFO_ID = 'id.dat';
  LIBINFO_INC = 'inc.dat';
  LIBINFO_LIB = 'lib.dat';
  LIBINFO_SORT = 'sort.dat';

var
  OutputDirectory: TFileName;
  BufferId,
  BufferIncludes,
  BufferLibraries,
  BufferSort: TStringList;
  i: Integer;
  PortInfo: TKallistiPortItem;

  function LibraryLanguageKindToDirectory: TFileName;
  begin
    Result := LIBINFO_PATH_C;
    if LibraryLanguageKind = llkCPP then
      Result := LIBINFO_PATH_CPP;
  end;

  function IsValidPort: Boolean;
  begin
    Result := (PortInfo.fLibraryLanguageKind = llkAll)
      or (PortInfo.fLibraryLanguageKind = LibraryLanguageKind);
  end;

begin
  OutputDirectory := IntegratedDevelopmentEnvironment.ExportLibraryInformationPath
    + LibraryLanguageKindToDirectory + DirectorySeparator;
  ForceDirectories(OutputDirectory);

  BufferId := TStringList.Create;
  BufferIncludes := TStringList.Create;
  BufferLibraries := TStringList.Create;
  BufferSort := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      PortInfo := Items[i];
      if PortInfo.Installed and IsValidPort then
      begin
        BufferId.Add(PortInfo.Name);
        BufferIncludes.Add(PortInfo.Includes);
        BufferLibraries.Add(PortInfo.Libraries);
        BufferSort.Add(PortInfo.LibraryWeights);
      end;
    end;

    SaveStringToFile(StringListToString(BufferId, ';'), OutputDirectory + LIBINFO_ID);
    SaveStringToFile(StringListToString(BufferIncludes, ';'), OutputDirectory + LIBINFO_INC);
    SaveStringToFile(StringListToString(BufferLibraries, ';'), OutputDirectory + LIBINFO_LIB);
    SaveStringToFile(StringListToString(BufferSort, ';'), OutputDirectory + LIBINFO_SORT);
  finally
    BufferId.Free;
    BufferIncludes.Free;
    BufferLibraries.Free;
    BufferSort.Free;
  end;
end;

function TKallistiPortManager.GetAddonIncludes(const AddonIndex: Integer): string;
var
  IncludeFiles: string;

begin
  IncludeFiles := StringReplace(fAddonsIncludes[AddonIndex],
    ArraySeparator, WhiteSpaceStr, [rfReplaceAll]);
  Result := GenerateIncludeHeader(IncludeFiles , EmptyStr);
end;

function TKallistiPortManager.GetAddonIndex(const AddonName: string): Integer;
begin
  Result := fAddonsId.IndexOf(AddonName);
end;

procedure TKallistiPortManager.RetrieveAvailableAddons;
begin
  StringToStringList(fKallistiLibraryInformation.ReadString('Addons', 'Id', EmptyStr), ';', fAddonsId);
  StringToStringList(fKallistiLibraryInformation.ReadString('Addons', 'Includes', EmptyStr), ';', fAddonsIncludes);
  StringToStringList(fKallistiLibraryInformation.ReadString('Addons', 'Libraries', EmptyStr), ';', fAddonsLibraries);
end;

constructor TKallistiPortManager.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  AIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment
);
begin
  fEnvironment := AEnvironment;
  fIntegratedDevelopmentEnvironment := AIntegratedDevelopmentEnvironment;
  fList := TList.Create;
  fPortsWithDependencies := TIntegerList.Create;
  fPortsWithoutDependencies := TIntegerList.Create;
  fPortsMap := TStringIntegerMap.Create;
  fKallistiLibraryInformation := TIniFile.Create(Environment.FileSystem.Kallisti.KallistiPortsLibraryInformationFile);
  fAddonsId := TStringList.Create;
  fAddonsIncludes := TStringList.Create;
  fAddonsLibraries := TStringList.Create;
  RetrieveAvailablePorts;
end;

destructor TKallistiPortManager.Destroy;
begin
  Clear;
  fPortsWithDependencies.Free;
  fPortsWithoutDependencies.Free;
  fList.Free;
  fPortsMap.Free;
  fKallistiLibraryInformation.Free;
  fAddonsId.Free;
  fAddonsIncludes.Free;
  fAddonsLibraries.Free;
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

procedure TKallistiPortManager.GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
begin
  if IntegratedDevelopmentEnvironment.ExportLibraryInformation then
  begin
    GenerateIntegratedDevelopmentEnvironmentLibraryInformation(llkC);
    GenerateIntegratedDevelopmentEnvironmentLibraryInformation(llkCPP);
  end;
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
    PatchTextFile(
      ConfigFileName,
      '#FETCH_CMD = wget',
      'FETCH_CMD = wget --no-check-certificate'
    );

    PatchTextFile(
      ConfigFileName,
      'FETCH_CMD = curl',
      '#FETCH_CMD = curl'
    );

    PatchTextFile(
      DownloadFileName,
      'svn checkout',
      'svn checkout --non-interactive --trust-server-cert'
    );

    // ln doesn't work very well under MinGW/MSYS...
    PatchTextFile(
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

