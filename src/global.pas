unit Global;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DCSDKMgr,
  PkgMgr;

var
  DreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager = nil;
  PackageManager: TPackageManager = nil;

procedure GlobalInitialization;
procedure GlobalFinalization;

implementation

uses
  PostInst;

procedure GlobalInitialization;
begin
  DreamcastSoftwareDevelopmentKitManager :=
    TDreamcastSoftwareDevelopmentKitManager.Create(not IsPostInstallMode);
  PackageManager := TPackageManager.Create(DreamcastSoftwareDevelopmentKitManager);
end;

procedure GlobalFinalization;
begin
  PackageManager.Free;
  DreamcastSoftwareDevelopmentKitManager.Free;
end;

end.

