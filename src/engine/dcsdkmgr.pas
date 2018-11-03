unit DCSDKMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetVer, Environ, KOSMgr, PortMgr, ToolMgr;

type
  { TDreamcastSoftwareDevelopmentKitManager }
  TDreamcastSoftwareDevelopmentKitManager = class(TObject)
  private
    fDreamcastTool: TDreamcastToolManager;
    fKallistiManager: TKallistiManager;
    fVersionRetriever: TComponentVersion;
    fKallistiPortsManager: TKallistiPortManager;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property DreamcastTool: TDreamcastToolManager read fDreamcastTool;
    property KallistiOS: TKallistiManager read fKallistiManager;
    property KallistiPorts: TKallistiPortManager read fKallistiPortsManager;
    property Versions: TComponentVersion read fVersionRetriever;
  end;

implementation

{ TDreamcastSoftwareDevelopmentKitManager }

constructor TDreamcastSoftwareDevelopmentKitManager.Create;
begin
  fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  fVersionRetriever := TComponentVersion.Create(fEnvironment);
  fKallistiPortsManager := TKallistiPortManager.Create(fEnvironment);
  fKallistiManager := TKallistiManager.Create(fEnvironment);
  fDreamcastTool := TDreamcastToolManager.Create(fEnvironment);
end;

destructor TDreamcastSoftwareDevelopmentKitManager.Destroy;
begin
  fDreamcastTool.Free;
  fVersionRetriever.Free;
  fEnvironment.Free;
  fKallistiPortsManager.Free;
  fKallistiManager.Free;
  inherited Destroy;
end;

end.

