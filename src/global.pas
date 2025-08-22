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

procedure GlobalInitialization;
begin
  DreamcastSoftwareDevelopmentKitManager :=
    TDreamcastSoftwareDevelopmentKitManager.Create;
  PackageManager := TPackageManager.Create(DreamcastSoftwareDevelopmentKitManager);
end;

procedure GlobalFinalization;
begin
  PackageManager.Free;
  DreamcastSoftwareDevelopmentKitManager.Free;
end;

end.

