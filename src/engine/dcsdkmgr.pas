unit DCSDKMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetVer, Environ, KOSMgr, PortMgr;

type
  { TDreamcastSoftwareDevelopmentKitManager }
  TDreamcastSoftwareDevelopmentKitManager = class(TObject)
  private
    fKallistiManager: TKallistiManager;
    fVersionRetriever: TVersionRetriever;
    fKallistiPortsManager: TKallistiPortManager;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property KallistiOS: TKallistiManager read fKallistiManager;
    property KallistiPorts: TKallistiPortManager read fKallistiPortsManager;
    property Versions: TVersionRetriever read fVersionRetriever;
  end;

implementation

{ TDreamcastSoftwareDevelopmentKitManager }

constructor TDreamcastSoftwareDevelopmentKitManager.Create;
begin
  fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  fVersionRetriever := TVersionRetriever.Create(fEnvironment);
  fKallistiPortsManager := TKallistiPortManager.Create(fEnvironment);
  fKallistiManager := TKallistiManager.Create(fEnvironment);
end;

destructor TDreamcastSoftwareDevelopmentKitManager.Destroy;
begin
  fVersionRetriever.Free;
  fEnvironment.Free;
  fKallistiPortsManager.Free;
  fKallistiManager.Free;
  inherited Destroy;
end;

end.

