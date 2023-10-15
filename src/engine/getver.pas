unit GetVer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Environ;

type
  TComponentVersion = class;

  { TComponentName }
  TComponentName = (
    cnGit,
    cnSVN,
    cnPython,
    cnMinGW,
    cnBinutils,
    cnGCC,
    cnGDB,
    cnPythonGDB,
    cnNewlib,
    cnToolSerial,
    cnToolIP,
    cnKallistiOS,
    cnBinutilsARM,
    cnGCCARM,
    cnBinutilsWin32,
    cnGCCWin32,
    cnGDBWin32,
    cnRuby,
    cnRake
  );

  { TToolchainVersion }
  TToolchainVersion = class(TObject)
  private
    fBuildDate: TDateTime;
    fKind: TToolchainKind;
    fPackageGDB: TDebuggerVersionKind;
    fPackageToolchain: TToolchainVersionKind;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGDB: string;
    fVersionNewlib: string;
    fVersionPythonGDB: string;
    fOwner: TComponentVersion;
  protected
    function GetStringVersionToDebuggerVersionKind(
      const Version: string): TDebuggerVersionKind;
    function GetStringVersionToPackageToolchainKind(
      const Version: string): TToolchainVersionKind;
  public
    constructor Create(AOwner: TComponentVersion; ToolchainKind: TToolchainKind);
    property BuildDate: TDateTime read fBuildDate;
    property Binutils: string read fVersionBinutils;
    property GCC: string read fVersionGCC;
    property GDB: string read fVersionGDB;
    property PackageGDB: TDebuggerVersionKind read fPackageGDB;
    property PackageToolchain: TToolchainVersionKind read fPackageToolchain;
    property PythonGDB: string read fVersionPythonGDB;
    property Newlib: string read fVersionNewlib;
    property Kind: TToolchainKind read fKind;
    property Owner: TComponentVersion read fOwner;
  end;

  { TComponentVersion }
  TComponentVersion = class(TObject)
  private
    fBuildDateKallistiOS: TDateTime;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fPythonInstalled: Boolean;
	  fGitInstalled: Boolean;
    fMRubyBuildDate: TDateTime;
    fRakeInstalled: Boolean;
    fRubyInstalled: Boolean;
    fSubversionInstalled: Boolean;
    fToolchainVersionARM: TToolchainVersion;
    fToolchainVersionSuperH: TToolchainVersion;
    fToolchainVersionWin32: TToolchainVersion;
    fVersionGit: string;
    fVersionKallistiOS: string;
    fChangeLogKallistiOS: string;
    fVersionMinGW: string;
    fVersionPython: string;
    fVersionSVN: string;
    fVersionToolIP: string;
    fVersionToolSerial: string;
    fVersionRuby: string;
    fVersionRake: string;
  protected
    function IsValidVersion(const Version: string): Boolean;
    function RetrieveKallistiVersion: string;
    function RetrieveKallistiBuildDate: TDateTime;
    function RetrieveMRubyBuildDate: TDateTime;
    procedure RetrieveKallistiInformation;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
  public
    constructor Create(AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
      const AutoLoad: Boolean);
    destructor Destroy; override;

    function GetComponentVersion(const ComponentName: TComponentName): string;
    procedure RetrieveVersions;

    property Git: string read fVersionGit;
	  property GitInstalled: Boolean read fGitInstalled;
    property MinGW: string read fVersionMinGW;
    property Subversion: string read fVersionSVN;
    property SubversionInstalled: Boolean read fSubversionInstalled;
    property Python: string read fVersionPython;
    property PythonInstalled: Boolean read fPythonInstalled;
    property ToolSerial: string read fVersionToolSerial;
    property ToolIP: string read fVersionToolIP;
    property KallistiOS: string read fVersionKallistiOS;
    property KallistiBuildDate: TDateTime read fBuildDateKallistiOS;
    property KallistiChangeLog: string read fChangeLogKallistiOS;
    property ToolchainSuperH: TToolchainVersion read fToolchainVersionSuperH;
    property ToolchainARM: TToolchainVersion read fToolchainVersionARM;
    property ToolchainWin32: TToolchainVersion read fToolchainVersionWin32;
    property Ruby: string read fVersionRuby;
    property RubyInstalled: Boolean read fRubyInstalled;
    property Rake: string read fVersionRake;
    property RakeInstalled: Boolean read fRakeInstalled;
    property MRubyBuildDate: TDateTime read fMRubyBuildDate;
  end;

function ComponentNameToString(const ComponentName: TComponentName): string;

implementation

uses
{$IFDEF GUI}
  Forms,
{$ENDIF}
  SysTools,
  VerIntf,
  FSTools,
  PEUtils;

const
  DEVELOPMENT_VERSION_SUFFIX = '-dev';

function ComponentNameToString(const ComponentName: TComponentName): string;
const
  COMPONENTS_NAME: array[0..18] of string = (
    'Git',
    'SVN',
    'Python',
    'MinGW',
    'Binutils',
    'GCC',
    'GDB',
    'PythonGDB',
    'Newlib',
    'ToolSerial',
    'ToolIP',
    'KallistiOS',
    'BinutilsARM',
    'GCCARM',
    'BinutilsWin32',
    'GCCWin32',
    'GDBWin32',
    'Ruby',
    'Rake'
  );

begin
  Result := COMPONENTS_NAME[Integer(ComponentName)];
end;

{ TToolchainVersion }

function TToolchainVersion.GetStringVersionToDebuggerVersionKind(
  const Version: string): TDebuggerVersionKind;
var
  i: Integer;

begin
  Result := dvkUndefined;
  if fOwner.IsValidVersion(fVersionGDB) then
  begin
    Result := dvkPythonDisabled;
    if fOwner.IsValidVersion(Version) then
    begin
      for i := Low(SUPPORTED_PYTHON_VERSIONS) to High(SUPPORTED_PYTHON_VERSIONS) do
      begin
        if SUPPORTED_PYTHON_VERSIONS[i] = Version then
        begin
          Result := TDebuggerVersionKind(i + 2); // 2 for Undefined+PythonDisabled
          Break;
        end;
      end;
    end;
  end;
end;

function TToolchainVersion.GetStringVersionToPackageToolchainKind(
  const Version: string): TToolchainVersionKind;
var
  i: Integer;

begin
  Result := tvkUndefined;
  if fOwner.IsValidVersion(Version) then
  begin
    for i := Low(SUPPORTED_GCC_VERSIONS) to High(SUPPORTED_GCC_VERSIONS) do
    begin
      if StartsWith(SUPPORTED_GCC_VERSIONS[i] + '.', Version) then
      begin
        Result := TToolchainVersionKind(i + 1); // 1 for Undefined
        Break;
      end;
    end;
  end;
end;

constructor TToolchainVersion.Create(AOwner: TComponentVersion;
  ToolchainKind: TToolchainKind);
begin
  fOwner := AOwner;
  fKind := ToolchainKind;
end;

{ TComponentVersion }

function TComponentVersion.IsValidVersion(const Version: string): Boolean;
begin
  Result := not SameText(Version, INVALID_VERSION);
end;

function TComponentVersion.RetrieveKallistiVersion: string;
const
  GIT_REVISION_TAG = 'Git revision ';
  START_TAG1 = 'KallistiOS ';
  START_TAG2 = 'offline:';
  END_TAG = ':';

var
  TargetFileName:  TFileName;

begin
  TargetFileName := Environment.FileSystem.Kallisti.KallistiLibrary;
  Result := GetRegisteredVersion(TargetFileName);

  if IsEmpty(Result) then
  begin
    Result := RetrieveVersionWithFind(TargetFileName, START_TAG1, END_TAG, False);
    if IsInString(START_TAG1, Result) then
    begin
      // Online, Git version
      Result := Right(START_TAG1, Result);
      Result := Right(GIT_REVISION_TAG, Result);

      // Offline version (failback)
      if IsEmpty(Result) then
      begin
        Result := RetrieveVersionWithFind(TargetFileName, START_TAG2, False);
        Result := ExtractStr(START_TAG1, END_TAG, Result);
      end;

      SetRegisteredVersion(TargetFileName, Result);
    end;
  end;

  if IsEmpty(Result) then
    Result := INVALID_VERSION;
end;

function TComponentVersion.RetrieveKallistiBuildDate: TDateTime;
begin
  Result := GetFileDate(Environment.FileSystem.Kallisti.KallistiLibrary);
end;

function TComponentVersion.RetrieveMRubyBuildDate: TDateTime;
begin
  Result := GetFileDate(Environment.FileSystem.Ruby.RubyLibrary);
end;

procedure TComponentVersion.RetrieveKallistiInformation;
begin
  with Environment.FileSystem.Kallisti do
  begin
    fVersionKallistiOS := RetrieveKallistiVersion;
    fBuildDateKallistiOS := RetrieveKallistiBuildDate;
    fChangeLogKallistiOS := INVALID_VERSION;
    if FileExists(KallistiLibrary) then
    begin
      fChangeLogKallistiOS := RetrieveVersionWithFind(KallistiChangeLogFile,
        'KallistiOS version ', ' -----');
      if IsVersionValid(fChangeLogKallistiOS) then
        fChangeLogKallistiOS := fChangeLogKallistiOS + DEVELOPMENT_VERSION_SUFFIX;
    end;
  end;
end;

procedure TComponentVersion.RetrieveVersions;

  function RetrievePythonGdb(const GdbExecutable: TFileName): string;
  var
    Buffer: TStringList;
    i: Integer;

  begin
    Result := EmptyStr;
    Buffer := TStringList.Create;
    try
      GetPortableExecutableModules(GdbExecutable, Buffer);
{$IFDEF DEBUG}
      DebugLog('* GDB Imports: ' + sLineBreak + Buffer.Text);
{$ENDIF}
      i := StringListSubstringIndexOf(Buffer, 'python', False);
      if i <> -1 then
      begin
        Result := ExtractStr('python', '.dll', LowerCase(Buffer[i]));
        if Length(Result) > 1 then
          Result := Copy(Result, 1, 1) + '.' + Copy(Result, 2, Length(Result) - 1);
      end;
      if SameText(Result, EmptyStr) then
        Result := INVALID_VERSION;
    finally
      Buffer.Free;
    end;
  end;

  procedure RetrieveVersionToolchain(var AVersion: TToolchainVersion;
    AEnvironment: TDreamcastSoftwareDevelopmentFileSystemToolchain);
  begin
    if Assigned(AVersion) then
    begin
      AVersion.fBuildDate := GetFileDate(AEnvironment.GCCExecutable);

      AVersion.fVersionBinutils := RetrieveVersion(AEnvironment.BinutilsExecutable,
        '--version', ' (GNU Binutils)', sLineBreak);
      AVersion.fVersionGCC := RetrieveVersion(AEnvironment.GCCExecutable,
        '--version', ') ', sLineBreak);

      if AEnvironment.Kind <> tkARM then
      begin
        // Super-H and Win32
        AVersion.fVersionGDB := RetrieveVersion(AEnvironment.GDBExecutable,
          '--version', ' (GDB)', sLineBreak);
      end;

      if AEnvironment.Kind = tkSuperH then
      begin
        // Super-H only
        AVersion.fVersionPythonGDB := RetrievePythonGdb(AEnvironment.GDBExecutable);
        AVersion.fPackageGDB := AVersion.GetStringVersionToDebuggerVersionKind(
          AVersion.fVersionPythonGDB);
        AVersion.fVersionNewlib := RetrieveVersionWithFind(AEnvironment.NewlibBinary,
          '/dc-chain/newlib-', '/newlib/libc/');
      end;

      if AEnvironment.Kind <> tkWin32 then
      begin
        // Super-H and ARM
        AVersion.fPackageToolchain := AVersion.GetStringVersionToPackageToolchainKind(
          AVersion.fVersionGCC);
      end;
    end;
  end;

begin
  with Environment.FileSystem do
  begin
    fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
	  fGitInstalled := IsValidVersion(fVersionGit);
    fVersionSVN := RetrieveVersion('svn', '--version', 'svn, version', sLineBreak);
    fSubversionInstalled := IsValidVersion(fVersionSVN);
    fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
    fPythonInstalled := IsValidVersion(fVersionPython);
    fVersionMinGW := RetrieveVersion(Shell.MinGWGetExecutable,
      '--version', 'mingw-get version', sLineBreak);
    fVersionRuby := RetrieveVersion('ruby', '--version', 'ruby ', WhiteSpaceStr);
    fRubyInstalled := IsValidVersion(fVersionRuby);
    fVersionRake := RetrieveVersion('rake', '--version', 'version ', sLineBreak, False);
    fRakeInstalled := IsValidVersion(fVersionRake);

    RetrieveVersionToolchain(fToolchainVersionSuperH, ToolchainSuperH);
    RetrieveVersionToolchain(fToolchainVersionARM, ToolchainARM);
    RetrieveVersionToolchain(fToolchainVersionWin32, ToolchainWin32);

    fVersionToolSerial := RetrieveVersion(DreamcastTool.SerialExecutable,
      '-h', 'dc-tool', 'by ');
    fVersionToolIP := RetrieveVersion(DreamcastTool.InternetProtocolExecutable,
      '-h', 'dc-tool-ip', 'by ');

    RetrieveKallistiInformation;

    fMRubyBuildDate := RetrieveMRubyBuildDate;
  end;
end;

constructor TComponentVersion.Create(
  AEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  const AutoLoad: Boolean);
begin
  fEnvironment := AEnvironment;
  fToolchainVersionSuperH := TToolchainVersion.Create(Self, tkSuperH);
  fToolchainVersionARM := TToolchainVersion.Create(Self, tkARM);
  fToolchainVersionWin32 := TToolchainVersion.Create(Self, tkWin32);
  if AutoLoad then
    RetrieveVersions;
end;

destructor TComponentVersion.Destroy;
begin
  fToolchainVersionWin32.Free;
  fToolchainVersionSuperH.Free;
  fToolchainVersionARM.Free;
  inherited Destroy;
end;

function TComponentVersion.GetComponentVersion(
  const ComponentName: TComponentName): string;
begin
  Result := '';
  case ComponentName of
    cnGit:
      Result := fVersionGit;
    cnSVN:
      Result := fVersionSVN;
    cnPython:
      Result := fVersionPython;
    cnMinGW:
      Result := fVersionMinGW;
    cnBinutils:
      Result := fToolchainVersionSuperH.fVersionBinutils;
    cnGCC:
      Result := fToolchainVersionSuperH.fVersionGCC;
    cnGDB:
      Result := fToolchainVersionSuperH.fVersionGDB;
    cnPythonGDB:
      Result := fToolchainVersionSuperH.fVersionPythonGDB;
    cnNewlib:
      Result := fToolchainVersionSuperH.fVersionNewlib;
    cnToolSerial:
      Result := fVersionToolSerial;
    cnToolIP:
      Result := fVersionToolIP;
    cnKallistiOS:
      Result := fVersionKallistiOS;
    cnBinutilsARM:
      Result := fToolchainVersionARM.fVersionBinutils;
    cnGCCARM:
      Result := fToolchainVersionARM.fVersionGCC;
    cnBinutilsWin32:
      Result := fToolchainVersionWin32.fVersionBinutils;
    cnGCCWin32:
      Result := fToolchainVersionWin32.fVersionGCC;
    cnGDBWin32:
      Result := fToolchainVersionWin32.fVersionGDB;
    cnRuby:
      Result := fVersionRuby;
    cnRake:
      Result := fVersionRake;
  end;
end;

end.

