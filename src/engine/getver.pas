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
    cnFoundation,
    cnGit,
    cnPython,
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
    cnCMake,
    cnMeson
  );

  { TToolchainVersion }
  TToolchainVersion = class(TObject)
  private
    fBuildDate: TDateTime;
    fKind: TToolchainKind;
    fPackageProfileGDB: string;
    fPackageProfileToolchain: string;
    fVersionBinutils: string;
    fVersionGCC: string;
    fVersionGDB: string;
    fVersionNewlib: string;
    fVersionPythonGDB: string;
    fOwner: TComponentVersion;
  protected
    (*function GetStringVersionToDebuggerVersionKind(
      const Version: string): TDebuggerKind;
    function GetStringVersionToPackageToolchainKind(
      const Version: string): TToolchainVersionKind;*)
  public
    constructor Create(AOwner: TComponentVersion; ToolchainKind: TToolchainKind);

    property BuildDate: TDateTime read fBuildDate;
    property Binutils: string read fVersionBinutils;
    property GCC: string read fVersionGCC;
    property GDB: string read fVersionGDB;
    property PackageProfileGDB: string read fPackageProfileGDB;
    property PackageProfileToolchain: string read fPackageProfileToolchain;
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
    fMesonInstalled: Boolean;
    fRubyInstalled: Boolean;
    fCMakeInstalled: Boolean;
    fToolchainVersionARM: TToolchainVersion;
    fToolchainVersionSuperH: TToolchainVersion;
    fToolchainVersionWin32: TToolchainVersion;
    fVersionGit: string;
    fVersionKallistiOS: string;
    fChangeLogKallistiOS: string;
    fVersionPython: string;
    fVersionCMake: string;
    fVersionToolIP: string;
    fVersionToolSerial: string;
    fVersionRuby: string;
    fVersionMeson: string;
    fVersionFoundation: string;
  protected
    function IsValidVersion(const Version: string): Boolean;
    function RetrieveKallistiChangeLogVersion: string;
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
    property CMake: string read fVersionCMake;
    property CMakeInstalled: Boolean read fCMakeInstalled;
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
    property Meson: string read fVersionMeson;
    property MesonInstalled: Boolean read fMesonInstalled;
    property MRubyBuildDate: TDateTime read fMRubyBuildDate;
    property Foundation: string read fVersionFoundation;
  end;

function ComponentNameToString(const ComponentName: TComponentName): string;

implementation

uses
  TypInfo,
{$IFDEF GUI}
  Forms,
{$ENDIF}
  SysTools,
  StrTools,
  VerIntf,
  FSTools,
  PEUtils,
  RefBase;

function ComponentNameToString(const ComponentName: TComponentName): string;
var
  ComponentRadicalName: string;

begin
  // Get 'cnXXXXX'
  ComponentRadicalName := GetEnumName(TypeInfo(TComponentName), Ord(ComponentName));

  // Remove the 'cn' prefix
  ComponentRadicalName := Copy(ComponentRadicalName, 3, Length(ComponentRadicalName) - 2);

  // Return 'XXXXX'
  Result := ComponentRadicalName;
end;

{ TToolchainVersion }

(*function TToolchainVersion.GetStringVersionToDebuggerVersionKind(
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
end;*)

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

function TComponentVersion.RetrieveKallistiChangeLogVersion: string;
var
  TargetFileName: TFileName;

begin
  Result := INVALID_VERSION;
  TargetFileName := Environment.FileSystem.Kallisti.KallistiChangeLogFile;
  if FileExists(TargetFileName) then
  begin
    Result := Trim(StringReplace(RetrieveVersionWithFind(TargetFileName,
      'KallistiOS version ', sLineBreak), '-', EmptyStr, [rfReplaceAll]));
  end;
end;

function TComponentVersion.RetrieveKallistiVersion: string;
var
  TargetFileName: TFileName;

begin
  TargetFileName := Environment.FileSystem.Kallisti.KallistiLibrary;
  Result := RetrieveVersionKallisti(TargetFileName, True);
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
      fChangeLogKallistiOS := RetrieveKallistiChangeLogVersion;
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
        AVersion.fPackageProfileGDB := AEnvironment.GetGdbProfileKeyFromFileName(
          AEnvironment.GDBExecutable);
{$IFDEF DEBUG}
        DebugLog(Format('[%s] GDB PackageProfile: "%s"', [
          GetEnumName(TypeInfo(TToolchainKind), Ord(AEnvironment.Kind)),
          AVersion.fPackageProfileGDB
        ]));
{$ENDIF}
        AVersion.fVersionNewlib := RetrieveVersionWithFind(AEnvironment.NewlibBinary,
          '/dc-chain/newlib-', '/newlib/libc/');
      end;

      if AEnvironment.Kind = tkSuperH then
      begin
        // Super-H
        AVersion.fPackageProfileToolchain := AEnvironment
          .GetToolchainProfileKeyFromFileName(AEnvironment.GCCExecutable);
{$IFDEF DEBUG}
        DebugLog(Format('[%s] Toolchain PackageProfile: "%s"', [
          GetEnumName(TypeInfo(TToolchainKind), Ord(AEnvironment.Kind)),
          AVersion.fPackageProfileToolchain
        ]));
{$ENDIF}
      end;
    end;
  end;

begin
  // Retrieve environment flavour
  fVersionFoundation := INVALID_VERSION;
  case Environment.FoundationKind of
    efkMinGWMSYS:
      fVersionFoundation := 'MinGW/MSYS';
    efkMinGW64MSYS2:
      fVersionFoundation := 'MinGW-w64/MSYS2';
  end;

  // Retrieve components version
  with Environment.FileSystem do
  begin
    fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak);
	  fGitInstalled := IsValidVersion(fVersionGit);

    fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak);
    fPythonInstalled := IsValidVersion(fVersionPython);

    fVersionRuby := RetrieveVersion('ruby', '--version', 'ruby ', WhiteSpaceStr);
    fRubyInstalled := IsValidVersion(fVersionRuby);

    fVersionCMake := RetrieveVersion('cmake', '--version', 'version ', sLineBreak);
    fCMakeInstalled := IsValidVersion(fVersionCMake);

    fVersionMeson := RetrieveVersion('meson', '--version', True);
    fMesonInstalled := IsValidVersion(fVersionMeson);

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
    cnFoundation:
      Result := fVersionFoundation;
    cnGit:
      Result := fVersionGit;
    cnPython:
      Result := fVersionPython;
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
    cnCMake:
      Result := fVersionCMake;
    cnMeson:
      Result := fVersionMeson;
  end;
end;

end.

