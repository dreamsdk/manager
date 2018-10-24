unit DCSDKMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetVer, Environ, PortMgr;

type
  { TDreamcastSoftwareDevelopmentKitManager }
  TDreamcastSoftwareDevelopmentKitManager = class(TObject)
  private
    fVersionRetriever: TVersionRetriever;
    fPortsManager: TKallistiPortsManager;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property Ports: TKallistiPortsManager read fPortsManager;
    property Versions: TVersionRetriever
      read fVersionRetriever;
  end;

implementation

uses
  SysTools;

{ TDreamcastSoftwareDevelopmentKitManager }

constructor TDreamcastSoftwareDevelopmentKitManager.Create;
begin
  fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  fVersionRetriever := TVersionRetriever.Create(fEnvironment);
  fPortsManager := TKallistiPortsManager.Create(fEnvironment);
end;

destructor TDreamcastSoftwareDevelopmentKitManager.Destroy;
begin
  fVersionRetriever.Free;
  fEnvironment.Free;
  fPortsManager.Free;
  inherited Destroy;
end;

end.

