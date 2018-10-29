unit Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TfrmProgress }

  TfrmProgress = class(TForm)
    btnAbort: TButton;
    lblProgressStep: TLabel;
    memBufferOutput: TMemo;
    pgbOperationProgress: TProgressBar;
    procedure btnAbortClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    fFinished: Boolean;
    function GetAbortOperation: Boolean;
    procedure SetIdleState(State: Boolean);
  public
    property Finished: Boolean read fFinished write fFinished;
    procedure SetTerminateErrorState(Aborted: Boolean);
    procedure SetProgressText(const Message: string);
    property AbortOperation: Boolean read GetAbortOperation;
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.lfm}

uses
  ShellThd;

{ TfrmProgress }

procedure TfrmProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfrmProgress.btnAbortClick(Sender: TObject);
begin
  if AbortOperation then
    AbortThreadOperation
  else
    Close;
end;

procedure TfrmProgress.FormShow(Sender: TObject);
begin
  lblProgressStep.Caption := '';
  memBufferOutput.Clear;
  SetIdleState(False);
  Finished := False;
end;

procedure TfrmProgress.SetIdleState(State: Boolean);
begin
  if State then
  begin
    btnAbort.Caption := '&Close';
    btnAbort.Tag := 1000;
    pgbOperationProgress.Style := pbstNormal;
  end
  else
  begin
    btnAbort.Caption := '&Cancel';
    btnAbort.Tag := 0;
    pgbOperationProgress.Style := pbstMarquee;
  end;
end;

function TfrmProgress.GetAbortOperation: Boolean;
begin
  Result := btnAbort.Tag <> 1000;
end;

procedure TfrmProgress.SetTerminateErrorState(Aborted: Boolean);
var
  Message: string;

begin
  SetIdleState(True);
  if Aborted then
    Message := 'Operation aborted.'
  else
    Message := 'Operation done with errors.';

  memBufferOutput.Lines.Add('*** ' + Message);
  lblProgressStep.Caption := Message;
end;

procedure TfrmProgress.SetProgressText(const Message: string);
begin
  lblProgressStep.Caption := Message;
  frmProgress.memBufferOutput.Lines.Add(Message);
end;

end.

