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
    procedure UpdateRepositoriesURL;
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

procedure TDreamcastSoftwareDevelopmentKitManager.UpdateRepositoriesURL;
begin
  with Environment.Settings.Repositories do
  begin
    KallistiURL := KallistiOS.Repository.URL;
    KallistiPortsURL := KallistiPorts.Repository.URL;
    DreamcastToolSerialURL := DreamcastTool.RepositorySerial.URL;
    DreamcastToolInternetProtocolURL := DreamcastTool.RepositoryInternetProtocol.URL;
  end;
end;

constructor TDreamcastSoftwareDevelopmentKitManager.Create;
begin
  fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  fIntegratedDevelopmentEnvironment := TIntegratedDevelopmentEnvironment.Create(fEnvironment);
  fVersionRetriever := TComponentVersion.Create(fEnvironment);
  fKallistiPortsManager := TKallistiPortManager.Create(fEnvironment,
    fIntegratedDevelopmentEnvironment, fVersionRetriever);
  fKallistiManager := TKallistiManager.Create(fEnvironment);
  fDreamcastTool := TDreamcastToolManager.Create(fEnvironment);
  UpdateRepositoriesURL;
  fIntegratedDevelopmentEnvironment.CodeBlocks.RefreshStatus;
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

