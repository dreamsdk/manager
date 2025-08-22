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
  IDEMgr;

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
    procedure InitializeObject(const AutoLoad: Boolean);
  public
    constructor Create; overload;
    constructor Create(const AutoLoad: Boolean); overload;
    destructor Destroy; override;
    procedure ForceNextRebuild;
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

uses
  SysTools;

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
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    fEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create(AutoLoad);
    fIntegratedDevelopmentEnvironment := TIntegratedDevelopmentEnvironment
      .Create(fEnvironment);
    fVersionRetriever := TComponentVersion.Create(fEnvironment, AutoLoad);
    fKallistiPortsManager := TKallistiPortManager.Create(fEnvironment,
      fIntegratedDevelopmentEnvironment, fVersionRetriever, AutoLoad);
    fKallistiManager := TKallistiManager.Create(fEnvironment);
    fDreamcastTool := TDreamcastToolManager.Create(fEnvironment);
    if AutoLoad then
      UpdateRepositoriesURL;
    fIntegratedDevelopmentEnvironment.CodeBlocks.Refresh;

  finally
    LogMessageExit(LogContext);
  end;
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
  inherited Destroy;
end;

procedure TDreamcastSoftwareDevelopmentKitManager.ForceNextRebuild;
begin
  KallistiOS.ForceNextRebuild;
end;

end.

