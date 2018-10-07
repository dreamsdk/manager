unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClose: TButton;
    btnOpenMinGWManager: TButton;
    btnPortUninstall: TButton;
    btnUpdateKallistiOS: TButton;
    btnPortInstall: TButton;
    gbxAvailablePorts: TGroupBox;
    gbxPortInformation: TGroupBox;
    gbxToolchain: TGroupBox;
    gbxDependencies: TGroupBox;
    lblKallistiOS: TLabel;
    lblVersionBinutils: TLabel;
    lblVersionGCC: TLabel;
    lblVersionGDB: TLabel;
    lblVersionGit: TLabel;
    lblVersionNewlib: TLabel;
    lblVersionPython: TLabel;
    lblVersionMinGW: TLabel;
    lblVersionSVN: TLabel;
    lbxPorts: TListBox;
    PageControl1: TPageControl;
    rgxTerminalOption: TRadioGroup;
    tsAbout: TTabSheet;
    tsOptions: TTabSheet;
    tsEnvironment: TTabSheet;
    tsKallistiOS: TTabSheet;
    tsKallistiPorts: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DisplayEnvironmentComponentVersions;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  GetVer;

var
  VersionRetriever: TVersionRetriever;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  DisplayEnvironmentComponentVersions;
end;

procedure TfrmMain.DisplayEnvironmentComponentVersions;
begin
  lblVersionGit.Caption := VersionRetriever.Git;
  lblVersionSVN.Caption := VersionRetriever.SVN;
  lblVersionPython.Caption := VersionRetriever.Python;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  VersionRetriever := TVersionRetriever.Create;

finalization
  VersionRetriever.Free;

end.

