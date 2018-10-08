unit DCSDKMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetVer, Environ;

type
  { TDreamcastSoftwareDevelopmentKitManager }
  TDreamcastSoftwareDevelopmentKitManager = class(TObject)
  private
    fVersionRetriever: TVersionRetriever;
    fEnvironment: TDreamcastSoftwareDevelopmentEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    property Environment: TDreamcastSoftwareDevelopmentEnvironment
      read fEnvironment;
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
end;

destructor TDreamcastSoftwareDevelopmentKitManager.Destroy;
begin
  fVersionRetriever.Free;
  fEnvironment.Free;
  inherited Destroy;
end;

end.

