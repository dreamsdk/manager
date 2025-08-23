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
    fCMakePath: TFileName;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fGitPath: TFileName;
    fMesonPath: TFileName;
    fPythonInstalled: Boolean;
	  fGitInstalled: Boolean;
    fMesonInstalled: Boolean;
    fPythonPath: TFileName;
    fRubyInstalled: Boolean;
    fCMakeInstalled: Boolean;
    fRubyPath: TFileName;
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
    procedure RetrieveKallistiInformation;
    procedure RetrievePaths;
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
    property GitPath: TFileName read fGitPath;
    property CMake: string read fVersionCMake;
    property CMakeInstalled: Boolean read fCMakeInstalled;
    property CMakePath: TFileName read fCMakePath;
    property Foundation: string read fVersionFoundation;
    property Python: string read fVersionPython;
    property PythonInstalled: Boolean read fPythonInstalled;
    property PythonPath: TFileName read fPythonPath;
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
    property RubyPath: TFileName read fRubyPath;
    property Meson: string read fVersionMeson;
    property MesonInstalled: Boolean read fMesonInstalled;
    property MesonPath: TFileName read fMesonPath;
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
  RefBase,
  Runner;

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

procedure TComponentVersion.RetrievePaths;
const
  COMMAND = 'executables=("git" "cmake" "python" "ruby" "meson");' +
    'for exec in "${executables[@]}";' +
    'do path=$(command -v "$exec" 2>/dev/null);' +
    'if [ -n "$path" ];' +
    'then echo "$exec=FOUND:$path";' +
    'else echo "$exec=NOT_FOUND:";' +
    'fi; done';

var
  ShellRunner: TDreamcastSoftwareDevelopmentKitRunner;
  Buffer: string;

  function _GetCommandPath(const Command: string): TFileName;
  var
    CommandOutput: string;

  begin
    Result := EmptyStr;
    CommandOutput := ExtractStr(Concat(Command, '='), sLineBreak, Buffer);
    if Left(':', CommandOutput) = 'FOUND' then
      Result := Trim(Right(':', CommandOutput));
  end;

begin
  Buffer := Default(string);
  ShellRunner := TDreamcastSoftwareDevelopmentKitRunner.Create(True);
  try
    // Execute the script to get all components
    ShellRunner.StartShellCommand(COMMAND, Buffer);

    // Parse the Buffer output
    fGitPath := _GetCommandPath('git');
    fCMakePath := _GetCommandPath('cmake');
    fPythonPath := _GetCommandPath('python');
    fRubyPath := _GetCommandPath('ruby');
    fMesonPath := _GetCommandPath('meson');

{$IFDEF DEBUG}
    DebugLog(Format('---' + sLineBreak +
      'RetrievePaths:' + sLineBreak +
      '---' + sLineBreak +
      'Git: "%s"' + sLineBreak +
      'CMake: "%s"' + sLineBreak +
      'Python: "%s"' + sLineBreak +
      'Ruby: "%s"' + sLineBreak +
      'Meson: "%s"' + sLineBreak +
      '---', [
        fGitPath,
        fCMakePath,
        fPythonPath,
        fRubyPath,
        fMesonPath
      ])
    );
{$ENDIF}
  finally
    ShellRunner.Free;
  end;
end;

procedure TComponentVersion.RetrieveVersions;

  function _RetrievePythonGdb(const GdbExecutable: TFileName): string;
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

  procedure _RetrieveVersionToolchain(var AVersion: TToolchainVersion;
    AEnvironment: TDreamcastSoftwareDevelopmentFileSystemToolchain);
  begin
    if Assigned(AVersion) then
    begin
      AVersion.fBuildDate := GetFileDate(AEnvironment.GCCExecutable);

      AVersion.fVersionBinutils := RetrieveVersion(AEnvironment.BinutilsExecutable,
        '--version', ' (GNU Binutils)', sLineBreak, [rvfRegister]);
      AVersion.fVersionGCC := RetrieveVersion(AEnvironment.GCCExecutable,
        '--version', ') ', sLineBreak, [rvfRegister]);

      if AEnvironment.Kind <> tkARM then
      begin
        // Super-H and Win32
        AVersion.fVersionGDB := RetrieveVersion(AEnvironment.GDBExecutable,
          '--version', ' (GDB)', sLineBreak, [rvfRegister]);
      end;

      if AEnvironment.Kind = tkSuperH then
      begin
        // Super-H only
        AVersion.fVersionPythonGDB := _RetrievePythonGdb(AEnvironment.GDBExecutable);
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
    fVersionGit := RetrieveVersion('git', '--version', 'git version', sLineBreak, [
      rvfRegister,
      rvfUseShellRunner
    ]);
	  fGitInstalled := IsValidVersion(fVersionGit);

    fVersionPython := RetrieveVersion('python', '--version', 'Python', sLineBreak, [
      rvfRegister,
      rvfUseShellRunner
    ]);
    fPythonInstalled := IsValidVersion(fVersionPython);

    fVersionRuby := RetrieveVersion('ruby', '--version', 'ruby ', WhiteSpaceStr, [
      rvfRegister,
      rvfUseShellRunner
    ]);
    fRubyInstalled := IsValidVersion(fVersionRuby);

    fVersionCMake := RetrieveVersion('cmake', '--version', 'version ', sLineBreak, [
      rvfRegister,
      rvfUseShellRunner
    ]);
    fCMakeInstalled := IsValidVersion(fVersionCMake);

    fVersionMeson := RetrieveVersion('meson', '--version', [
      rvfRegister,
      rvfUseShellRunner
    ]);
    fMesonInstalled := IsValidVersion(fVersionMeson);

    _RetrieveVersionToolchain(fToolchainVersionSuperH, ToolchainSuperH);
    _RetrieveVersionToolchain(fToolchainVersionARM, ToolchainARM);
    _RetrieveVersionToolchain(fToolchainVersionWin32, ToolchainWin32);

    fVersionToolSerial := RetrieveVersion(DreamcastTool.SerialExecutable,
      '-h', 'dc-tool', 'by ', [rvfRegister]);
    fVersionToolIP := RetrieveVersion(DreamcastTool.InternetProtocolExecutable,
      '-h', 'dc-tool-ip', 'by ', [rvfRegister]);

    RetrieveKallistiInformation;

    RetrievePaths;
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

