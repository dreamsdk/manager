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
    procedure btnOpenMinGWManagerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgxTerminalOptionClick(Sender: TObject);
  private
    procedure DisplayEnvironmentComponentVersions;
    procedure LoadConfiguration;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  GetVer, Environ;

var
  VersionRetriever: TVersionRetriever;
  DreamcastSoftwareDevelopmentEnvironment: TDreamcastSoftwareDevelopmentEnvironment;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  DisplayEnvironmentComponentVersions;
  LoadConfiguration;
end;

procedure TfrmMain.rgxTerminalOptionClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentEnvironment.UseMintty := (rgxTerminalOption.ItemIndex = 1);
end;

procedure TfrmMain.DisplayEnvironmentComponentVersions;
begin
  lblVersionGit.Caption := VersionRetriever.Git;
  lblVersionSVN.Caption := VersionRetriever.SVN;
  lblVersionPython.Caption := VersionRetriever.Python;
  lblVersionMinGW.Caption := VersionRetriever.MinGW;
  lblVersionBinutils.Caption := VersionRetriever.Binutils;
  lblVersionGCC.Caption := VersionRetriever.GCC;
  lblVersionGDB.Caption := VersionRetriever.GDB;
  lblVersionNewlib.Caption := VersionRetriever.Newlib;
end;

procedure TfrmMain.LoadConfiguration;
begin
  if DreamcastSoftwareDevelopmentEnvironment.UseMintty then
    rgxTerminalOption.ItemIndex := 1;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnOpenMinGWManagerClick(Sender: TObject);
begin

end;

initialization
  DreamcastSoftwareDevelopmentEnvironment := TDreamcastSoftwareDevelopmentEnvironment.Create;
  VersionRetriever := TVersionRetriever.Create(DreamcastSoftwareDevelopmentEnvironment);

finalization
  VersionRetriever.Free;
  DreamcastSoftwareDevelopmentEnvironment.Free;

end.

