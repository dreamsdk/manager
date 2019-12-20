unit Splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  { TfrmSplash }
  TfrmSplash = class(TForm)
    imgSplash: TImage;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  frmSplash: TfrmSplash;

procedure SplashInitialize;
procedure SplashFinalize;

implementation

{$R *.lfm}

uses
  PostInst, SysTools, UITools;

function IsSplashActive: Boolean;
begin
  Result := (not IsPostInstallMode);
end;

procedure SplashInitialize;
begin
  if IsSplashActive then
  begin
    frmSplash := TfrmSplash.Create(Application);
{$IFDEF RELEASE}
    frmSplash.Show;
    frmSplash.Update;
    Delay(1500);
{$ENDIF}
  end;
end;

procedure SplashFinalize;
begin
  if IsSplashActive then
  begin
    frmSplash.Close;
    frmSplash.Release;
  end;
end;

{ TfrmSplash }

procedure TfrmSplash.FormCreate(Sender: TObject);
begin
  ImageToForm(Handle, imgSplash, clFuchsia);
end;

end.

