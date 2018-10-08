unit PortMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TKallistiPortsManager }
  TKallistiPortsManager = class(TObject)
  private
    procedure RetrieveAvailablePorts;
  public
  end;

implementation

uses
  FileUtil;

{ TKallistiPortsManager }

procedure TKallistiPortsManager.RetrieveAvailablePorts;
var
  PascalFiles: TStringList;

begin
  PascalFiles := TStringList.Create;
  try
    FindAllFiles(PascalFiles, '', '*.pas;*.pp;*.p;*.inc', True);
  finally
    PascalFiles.Free;
  end;
end;

end.

