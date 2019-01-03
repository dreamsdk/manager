unit ModVer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VerIntf;

const
  SELF_MODULE_REFERENCE = ':SELF';

type
  { TModuleVersionItem }
  TModuleVersionItem = class(TObject)
  private
    fBuildDateTime: string;
    fFileDescription: string;
    fFileVersion: string;
    fProductVersion: string;
  public
    constructor Create(Item: TModuleVersion);
    property FileDescription: string read fFileDescription;
    property FileVersion: string read fFileVersion;
    property BuildDateTime: string read fBuildDateTime;
    property ProductVersion: string read fProductVersion;
  end;

  { TModuleVersionList }
  TModuleVersionList = class(TObject)
  private
    fList: TList;
    fUnknownString: string;
    procedure Add(AItem: TModuleVersion);
    procedure Clear;
    function GetCount: Integer;
    function GetItem(Index: Integer): TModuleVersionItem;
  public
    constructor Create(var AModuleFileList: TStringList;
      const AUnknownString: string);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TModuleVersionItem read GetItem; default;
    property UnknownString: string read fUnknownString;
  end;

implementation

{ TModuleVersionItem }

constructor TModuleVersionItem.Create(Item: TModuleVersion);
begin
  fFileDescription := Item.FileDescription;
  fFileVersion := Item.FileVersion;
  fProductVersion := Item.ProductVersion;
  fBuildDateTime := Item.BuildDateTime;
end;

{ TModuleVersionList }

function TModuleVersionList.GetItem(Index: Integer): TModuleVersionItem;
begin
  Result := TModuleVersionItem(fList[Index]);
end;

procedure TModuleVersionList.Add(AItem: TModuleVersion);
begin
  fList.Add(TModuleVersionItem.Create(AItem));
end;

procedure TModuleVersionList.Clear;
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  fList.Clear;
end;

function TModuleVersionList.GetCount: Integer;
begin
  Result := fList.Count;
end;

constructor TModuleVersionList.Create(var AModuleFileList: TStringList;
  const AUnknownString: string);
var
  ModuleExecutable: TFileName;
  i: Integer;

begin
  fUnknownString := AUnknownString;
  fList := TList.Create;
  if Assigned(AModuleFileList) then
    for i := 0 to AModuleFileList.Count - 1 do
    begin
      ModuleExecutable := AModuleFileList[i];
      if SameText(ModuleExecutable, SELF_MODULE_REFERENCE) then
        Add(ExtractCurrentModuleVersion(UnknownString))
      else
        Add(ExtractModuleVersion(ModuleExecutable, UnknownString));
    end;
end;

destructor TModuleVersionList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;

end.

