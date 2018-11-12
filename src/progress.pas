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
    procedure SetCloseButtonState(State: Boolean);
  public
    property Finished: Boolean read fFinished write fFinished;
    procedure SetTerminateState(Success: Boolean; Aborted: Boolean);
    procedure SetProgressText(const Message: string);
    procedure AddNewLine(const Message: string);
    property AbortOperation: Boolean read GetAbortOperation;
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.lfm}

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  ShellThd,
  SysTools,
  Main,
  PostInst,
  StrRes;

{ TfrmProgress }

procedure TfrmProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not Finished and AbortOperation then
  begin
    CloseAction := caNone;
    PauseThreadOperation;
    if MessageDlg(CancelDialogCaption, CancelDialogText, mtWarning, [mbYes, mbNo], 0, mbNo) = mrYes then
    begin
      AbortThreadOperation;
      SetCloseButtonState(False);
      memBufferOutput.Lines.Add(SendingCancelSignal);
      lblProgressStep.Caption := SendingCancelSignal;
    end
    else
      ResumeThreadOperation;
  end
  else if Finished and IsPostInstallMode then
    Application.Terminate;
end;

procedure TfrmProgress.btnAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmProgress.FormShow(Sender: TObject);
begin
  lblProgressStep.Caption := '';
  memBufferOutput.Clear;
  SetIdleState(False);
  SetCloseButtonState(True);
  Finished := False;
end;

procedure TfrmProgress.SetIdleState(State: Boolean);
begin
  if State then
  begin
    btnAbort.Caption := CloseButtonCaption;
    btnAbort.Tag := 1000;
    pgbOperationProgress.Style := pbstNormal;
    SetCloseButtonState(True);
  end
  else
  begin
    btnAbort.Caption := CancelButtonCaption;
    btnAbort.Tag := 0;
    pgbOperationProgress.Style := pbstMarquee;
  end;
end;

// Thanks: https://www.tek-tips.com/faqs.cfm?fid=7515
procedure TfrmProgress.SetCloseButtonState(State: Boolean);
{$IFDEF Windows}
var
  hSysMenu: HMENU;

begin
  hSysMenu := GetSystemMenu(Self.Handle, False);
  if hSysMenu <> 0 then
  begin
    if State then
      EnableMenuItem(hSysMenu, SC_CLOSE, MF_BYCOMMAND or MF_ENABLED)
    else
      EnableMenuItem(hSysMenu, SC_CLOSE, MF_BYCOMMAND or MF_GRAYED);
    DrawMenuBar(Self.Handle);
  end;
{$ENDIF}
  btnAbort.Enabled := State;
end;

function TfrmProgress.GetAbortOperation: Boolean;
begin
  Result := btnAbort.Tag <> 1000;
end;

procedure TfrmProgress.SetTerminateState(Success: Boolean; Aborted: Boolean);
var
  Message: string;

begin
  SetIdleState(True);

  if not Success then
  begin
    if Aborted then
      Message := OperationAborted
    else
      Message := OperationDoneWithErrors;

    memBufferOutput.Lines.Add(Format(OperationErrorMemoText, [Message]));
    lblProgressStep.Caption := Message;
  end
  else
  begin
    lblProgressStep.Caption := OperationSuccessfullyTerminated;
    if not IsPostInstallMode then
      Close;
  end;
end;

procedure TfrmProgress.SetProgressText(const Message: string);
begin
  lblProgressStep.Caption := Message;
  frmProgress.memBufferOutput.Lines.Add(Message);
end;

procedure TfrmProgress.AddNewLine(const Message: string);
const
  PROGRESS_LINE = '% ';

var
  StrExtractedValue: string;
  Value: Integer;

  procedure PrintLine;
  begin
    pgbOperationProgress.Position := 0;
    pgbOperationProgress.Style := pbstMarquee;
    memBufferOutput.Lines.Add(Message);
  end;

  function ParseValue(LeftStr, RightStr, Str: string): Integer;
  begin
    StrExtractedValue := Trim(ExtractStr(LeftStr, RightStr, Str));
    Result := StrToIntDef(StrExtractedValue, -1);
  end;

  function ExtractValue: Integer;
  const
    FULL = '100%';

  var
    i: Integer;
    Buffer: string;

  begin
    Result := ParseValue(':', PROGRESS_LINE + ' (', Message); // for git
    if Result = -1 then
    begin
      i := Pos(PROGRESS_LINE, Message);
      Buffer := Copy(Message, i - 4, i - 1);
      Result := ParseValue(' ', PROGRESS_LINE, Buffer);  // for wget
      if (Result = -1) and (IsInString(FULL, Buffer)) then
        Result := 100;
    end;
  end;

begin
  if not IsInString(PROGRESS_LINE, Message) then
    PrintLine
  else
  begin
    Value := ExtractValue;
    if Value <> -1 then
    begin
      pgbOperationProgress.Position := Value;
      pgbOperationProgress.Style := pbstNormal;
    end
    else
      PrintLine;
  end;
end;

end.

