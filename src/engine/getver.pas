unit GetVer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Environ;

type
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
    fKind: TToolchainKind;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGDB: string;
    fVersionNewlib: string;
    fVersionPythonGDB: string;
  public
    constructor Create(ToolchainKind: TToolchainKind);
    property Binutils: string read fVersionBinutils;
    property GCC: string read fVersionGCC;
    property GDB: string read fVersionGDB;
    property PythonGDB: string read fVersionPythonGDB;
    property Newlib: string read fVersionNewlib;
    property Kind: TToolchainKind read fKind;
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
  Forms,
  StrUtils,
  SysTools,
  VerIntf,
  FSTools;

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

constructor TToolchainVersion.Create(ToolchainKind: TToolchainKind);
begin
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
  begin
    Result := RetrieveVersion(GdbExecutable, '--configuration',
      '--with-python=c:/python/', '/x86', False); // Major.Minor.Build
    Result := Right('.', ReverseString(Result)); // Minor.Major
    Result := ReverseString(Result); // Major.Minor
    if SameText(Result, EmptyStr) then
      Result := INVALID_VERSION;
  end;

  procedure RetrieveVersionToolchain(var AVersion: TToolchainVersion;
    AEnvironment: TDreamcastSoftwareDevelopmentFileSystemToolchain);
  begin
    if Assigned(AVersion) then
    begin
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
        AVersion.fVersionNewlib := RetrieveVersionWithFind(AEnvironment.NewlibBinary,
          '/dc-chain/newlib-', '/newlib/libc/');
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
    fVersionRake := RetrieveVersion(ParseInputFileSystemObject('%ComSpec%'),
      '/C "rake --version"', 'version ', sLineBreak, False);
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
  fToolchainVersionSuperH := TToolchainVersion.Create(tkSuperH);
  fToolchainVersionARM := TToolchainVersion.Create(tkARM);
  fToolchainVersionWin32 := TToolchainVersion.Create(tkWin32);
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

