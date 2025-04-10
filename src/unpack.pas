unit Unpack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TfrmUnpack }

  TfrmUnpack = class(TForm)
    btnCancel: TButton;
    lblUnpack: TLabel;
    pnlCenter: TPanel;
    pnlTop: TPanel;
    pgbUnpack: TProgressBar;
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    fFinished: Boolean;
    function GetProgressText: string;
    function GetProgressValue: Integer;
    procedure SetProgressText(AValue: string);
    procedure SetProgressValue(AValue: Integer);
  public
    property ProgressText: string read GetProgressText write SetProgressText;
    property ProgressValue: Integer read GetProgressValue write SetProgressValue;
    property Finished: Boolean read fFinished write fFinished;
  end;

var
  frmUnpack: TfrmUnpack;

implementation

{$R *.lfm}

uses
  SysTools,
  StrTools,
  PkgMgr,
  MsgDlg,
  StrRes,
  Main;

{ TfrmUnpack }

procedure TfrmUnpack.FormCreate(Sender: TObject);
begin
  ProgressText := EmptyStr;
  Finished := False;
end;

procedure TfrmUnpack.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not Finished then
  begin
    PackageManager.Pause;
    if MsgBoxDlg(Handle, CancelDialogCaption, CancelDialogText, mtWarning, [mbYes, mbNo], mbNo) = mrYes then
      PackageManager.Abort
    else
    begin
      CloseAction := caNone;
      PackageManager.Resume;
    end;
  end;
end;

procedure TfrmUnpack.btnCancelClick(Sender: TObject);
begin
  Close;
end;

function TfrmUnpack.GetProgressText: string;
begin
  Result := lblUnpack.Caption;
end;

function TfrmUnpack.GetProgressValue: Integer;
begin
  Result := pgbUnpack.Position;
end;

procedure TfrmUnpack.SetProgressText(AValue: string);
begin
  lblUnpack.Caption := AValue;
  if IsEmpty(AValue) then
    lblUnpack.Caption := UnpackingText;
end;

procedure TfrmUnpack.SetProgressValue(AValue: Integer);
begin
  pgbUnpack.Position := AValue;
end;

end.

