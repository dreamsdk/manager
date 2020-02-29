unit DCSDKMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GetVer,
  Environ,
  KOSMgr,
  PortMgr,
  ToolMgr,
  IDEMgr,
  RubyMgr;

type
  { TDreamcastSoftwareDevelopmentKitManager }
  TDreamcastSoftwareDevelopmentKitManager = class(TObject)
  private
    fRubyManager: TRubyManager;
    fDreamcastTool: TDreamcastToolManager;
    fKallistiManager: TKallistiManager;
    fVersionRetriever: TComponentVersion;
    fKallistiPortsManager: TKallistiPortManager;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
    fIntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment;
    procedure UpdateRepositoriesURL;
    procedure InitializeObject(const AutoLoad: Boolean);
  public
    constructor Create; overload;
    constructor Create(const AutoLoad: Boolean); overload;
    destructor Destroy; override;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
    property DreamcastTool: TDreamcastToolManager read fDreamcastTool;
    property IntegratedDevelopmentEnvironment: TIntegratedDevelopmentEnvironment
      read fIntegratedDevelopmentEnvironment;
    property KallistiOS: TKallistiManager read fKallistiManager;
    property KallistiPorts: TKallistiPortManager read fKallistiPortsManager;
    property Ruby: TRubyManager read fRubyManager;
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

procedure TDreamcastSoftwareDevelopmentKitManager.InitializeObject(
  const AutoLoad: Boolean);
begin
  fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  fIntegratedDevelopmentEnvironment := TIntegratedDevelopmentEnvironment
    .Create(fEnvironment);
  fVersionRetriever := TComponentVersion.Create(fEnvironment, AutoLoad);
  fKallistiPortsManager := TKallistiPortManager.Create(fEnvironment,
    fIntegratedDevelopmentEnvironment, fVersionRetriever, AutoLoad);
  fKallistiManager := TKallistiManager.Create(fEnvironment);
  fDreamcastTool := TDreamcastToolManager.Create(fEnvironment);
  fRubyManager := TRubyManager.Create(fEnvironment);
  if AutoLoad then
    UpdateRepositoriesURL;
  fIntegratedDevelopmentEnvironment.CodeBlocks.Refresh;
end;

constructor TDreamcastSoftwareDevelopmentKitManager.Create;
begin
  InitializeObject(True);
end;

constructor TDreamcastSoftwareDevelopmentKitManager.Create(
  const AutoLoad: Boolean);
begin
  InitializeObject(AutoLoad);
end;

destructor TDreamcastSoftwareDevelopmentKitManager.Destroy;
begin
  fDreamcastTool.Free;
  fKallistiManager.Free;
  fKallistiPortsManager.Free;
  fVersionRetriever.Free;
  fIntegratedDevelopmentEnvironment.Free;
  fEnvironment.Free;
  fRubyManager.Free;
  inherited Destroy;
end;

end.

