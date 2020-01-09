unit DCSDKMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetVer, Environ, KOSMgr, PortMgr, ToolMgr, IDEMgr;

type
  { TDreamcastSoftwareDevelopmentKitManager }
  TDreamcastSoftwareDevelopmentKitManager = class(TObject)
  private
    fDreamcastTool: TDreamcastToolManager;
    fKallistiManager: TKallistiManager;
    fVersionRetriever: TComponentVersion;
    fKallistiPortsManager: TKallistiPortManager;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property DreamcastTool: TDreamcastToolManager read fDreamcastTool;
    property IntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment
      read fIntegratedDevelopmentEnvironment;
    property KallistiOS: TKallistiManager read fKallistiManager;
    property KallistiPorts: TKallistiPortManager read fKallistiPortsManager;
    property Versions: TComponentVersion read fVersionRetriever;
  end;

implementation

{ TDreamcastSoftwareDevelopmentKitManager }

constructor TDreamcastSoftwareDevelopmentKitManager.Create;
begin
  fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  fIntegratedDevelopmentEnvironment := TIntegratedDevelopmentEnvironment.Create(fEnvironment);
  fVersionRetriever := TComponentVersion.Create(fEnvironment);
  fKallistiPortsManager := TKallistiPortManager.Create(fEnvironment,
    fIntegratedDevelopmentEnvironment, fVersionRetriever);
  fKallistiManager := TKallistiManager.Create(fEnvironment);
  fDreamcastTool := TDreamcastToolManager.Create(fEnvironment);
end;

destructor TDreamcastSoftwareDevelopmentKitManager.Destroy;
begin
  fDreamcastTool.Free;
  fKallistiManager.Free;
  fKallistiPortsManager.Free;
  fVersionRetriever.Free;
  fIntegratedDevelopmentEnvironment.Free;
  fEnvironment.Free;
  inherited Destroy;
end;

end.

