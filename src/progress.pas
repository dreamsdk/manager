unit Progress;

{$mode objfpc}{$H+}

(* Enable me if you want to test the abort system *)
// {$DEFINE DEBUG_PROGRESS_SIMULATE_ABORT_ERROR}

(* Enable me if you don't want the window Stay On Top even in RELEASE mode *)
// {$DEFINE DEBUG_PROGRESS_AVOID_STAY_ON_TOP}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls;

type
  { TfrmProgress }
  TfrmProgress = class(TForm)
    btnAbort: TButton;
    cbxAutocloseWindow: TCheckBox;
    lblProgressStep: TLabel;
    memBufferOutput: TMemo;
    pnlBottom: TPanel;
    pgbOperationProgress: TProgressBar;
    pnlTop: TPanel;
    tmrAbortFailSafe: TTimer;
    procedure btnAbortClick(Sender: TObject);
    procedure cbxAutocloseWindowClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrAbortFailSafeStartTimer(Sender: TObject);
    procedure tmrAbortFailSafeStopTimer(Sender: TObject);
    procedure tmrAbortFailSafeTimer(Sender: TObject);
  private
    fFinished: Boolean;
    function GetAbortOperation: Boolean;
    function GetAutoCloseState: Boolean;
    procedure SetIdleState(State: Boolean);
    procedure StartSafeAbort;
    procedure StopSafeAbort;
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

function IsProgressAutoClose: Boolean;

implementation

{$R *.lfm}

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  ShellThd,
  SysTools,
  StrTools,
  PostInst,
  StrRes,
  MsgDlg,
  Main;

const
  CLOSE_TAG_VALUE = 1000;
  ABORT_SAFE_MAX_TRIES = 5;

{ TfrmProgress }

procedure TfrmProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  LogMessageEnter('TfrmProgress.FormClose');
  try
    try

      if not Finished and AbortOperation then
      begin
        LogMessage('TfrmProgress.FormClose::PauseThreadOperation');
        CloseAction := caNone;
        PauseThreadOperation;
        if MsgBoxDlg(Handle, CancelDialogCaption, CancelDialogText, mtWarning, [mbYes, mbNo], mbNo) = mrYes then
        begin
          LogMessage('TfrmProgress.FormClose::StartSafeAbort');
          StartSafeAbort
        end
        else
        begin
          LogMessage('TfrmProgress.FormClose::ResumeThreadOperation');
          ResumeThreadOperation;
        end;
      end
      else if Finished and IsPostInstallMode then
      begin
        LogMessage('TfrmProgress.FormClose::Application.Terminate called');
        Application.Terminate;
      end;

    except
      raise;
    end;
  finally
    LogMessageExit('TfrmProgress.FormClose');
  end;
end;

procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  if IsPostInstallMode then
    cbxAutocloseWindow.Visible := False;
end;

procedure TfrmProgress.btnAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmProgress.cbxAutocloseWindowClick(Sender: TObject);
begin
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings
    .ProgressWindowAutoClose := cbxAutocloseWindow.Checked;
  DreamcastSoftwareDevelopmentKitManager.Environment.Settings.SaveConfiguration;
end;

procedure TfrmProgress.FormShow(Sender: TObject);
begin
  pgbOperationProgress.Max := 100;
  lblProgressStep.Caption := EmptyStr;
  memBufferOutput.Clear;
  SetIdleState(False);
  SetCloseButtonState(True);
  Finished := False;

  if IsPostInstallMode then
  begin
{$IF DEFINED(RELEASE) AND NOT DEFINED(DEBUG_PROGRESS_AVOID_STAY_ON_TOP)}
    FormStyle := fsSystemStayOnTop;
{$ENDIF}
  end
  else
  begin
    // Check the box to reflect the config
    cbxAutocloseWindow.Checked := DreamcastSoftwareDevelopmentKitManager
      .Environment.Settings.ProgressWindowAutoClose;
  end;
end;

procedure TfrmProgress.tmrAbortFailSafeStartTimer(Sender: TObject);
begin
  LogMessageEnter('TfrmProgress.tmrAbortFailSafeStartTimer');
  try
    try

      // Abort all the processes!
      AbortThreadOperation;

{$IFDEF DEBUG_PROGRESS_SIMULATE_ABORT_ERROR}
      ShowMessage('This message will causes issues in Abort process.');
{$ENDIF}

      // Updating the progress UI
      if not Finished then
      begin
        SetCloseButtonState(False);
        memBufferOutput.Lines.Add(SendingCancelSignal);
        lblProgressStep.Caption := SendingCancelSignal;
      end;

    except
      raise;
    end;
  finally
    LogMessageExit('TfrmProgress.tmrAbortFailSafeStartTimer');
  end;
end;

procedure TfrmProgress.tmrAbortFailSafeStopTimer(Sender: TObject);
begin
  LogMessageExit('TfrmProgress.tmrAbortFailSafeStopTimer');
  try
    try

      pgbOperationProgress.Position := 0;
      Sleep(500);
      SetIdleState(True);
      lblProgressStep.Caption := OperationAborted;
      Application.ProcessMessages;

    except
      raise;
    end;
  finally
    LogMessageExit('TfrmProgress.tmrAbortFailSafeStopTimer');
  end;
end;

procedure TfrmProgress.tmrAbortFailSafeTimer(Sender: TObject);
begin
  LogMessageExit('TfrmProgress.tmrAbortFailSafeTimer');
  try
    try

      pgbOperationProgress.Position := pgbOperationProgress.Position + 1;

      if (pgbOperationProgress.Position >= pgbOperationProgress.Max) then
        StopSafeAbort;

    except
      raise;
    end;
  finally
    LogMessageExit('TfrmProgress.tmrAbortFailSafeTimer');
  end;
end;

procedure TfrmProgress.SetIdleState(State: Boolean);
begin
  if State then
  begin
    btnAbort.Caption := CloseButtonCaption;
    btnAbort.Tag := CLOSE_TAG_VALUE;
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

procedure TfrmProgress.StartSafeAbort;
begin
  // Start the fail-safe timer...
  pgbOperationProgress.Position := 0;
  pgbOperationProgress.Max := ABORT_SAFE_MAX_TRIES;
  pgbOperationProgress.Style := pbstNormal;
  tmrAbortFailSafe.Enabled := True;
end;

procedure TfrmProgress.StopSafeAbort;
begin
  tmrAbortFailSafe.Enabled := False;
end;

// Thanks: https://www.tek-tips.com/faqs.cfm?fid=7515
procedure TfrmProgress.SetCloseButtonState(State: Boolean);
{$IFDEF WINDOWS}
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
{$ELSE}
begin
{$ENDIF}
  btnAbort.Enabled := State;

  (* If for some reason, the Close button has been disabled again, restart the
     the fail-safe timer... *)
  if not State then
    StartSafeAbort;
end;

function TfrmProgress.GetAbortOperation: Boolean;
begin
  Result := btnAbort.Tag <> CLOSE_TAG_VALUE;
end;

function TfrmProgress.GetAutoCloseState: Boolean;
begin
  Result := cbxAutocloseWindow.Checked;
end;

procedure TfrmProgress.SetTerminateState(Success: Boolean; Aborted: Boolean);
var
  Message,
  LongMessage: string;

begin
  LogMessageEnter('TfrmProgress.SetTerminateState');
  try
    try

      StopSafeAbort;
      SetIdleState(True);

      if not Success then
      begin
        LongMessage := EmptyStr;

        if Aborted then
          Message := OperationAborted
        else
        begin
          Message := OperationDoneWithErrors;
          if IsPostInstallMode then
            LongMessage := Concat(Message, sLineBreak, OperationDoneWithErrorsPostInstall);
        end;

        if IsEmpty(LongMessage) then
          LongMessage := Message;

        memBufferOutput.Lines.Add(Format(OperationErrorMemoText, [LongMessage]));
        lblProgressStep.Caption := Message;
      end
      else
      begin
        SetProgressText(OperationSuccessfullyTerminated);
        if (not IsPostInstallMode) and IsProgressAutoClose then
          Close;
      end;

    except
      raise;
    end;
  finally
    LogMessageExit('TfrmProgress.SetTerminateState');
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
    Buffer := Default(string);

    // for Git
    Result := ParseValue(':', PROGRESS_LINE + ' (', Message);

    // for Wget and Package Manager
    if (Result = -1) then
    begin
      i := Pos(PROGRESS_LINE, Message);
      Buffer := Copy(Message, i - 4, i - 1);
      Result := ParseValue(' ', PROGRESS_LINE, Buffer);
    end;

    // If we detect "100%" then the value is 100...
    if (Result = -1) and (IsInString(FULL, Buffer)) then
      Result := 100;
  end;

begin
  if not IsInString(PROGRESS_LINE, Message) then
    PrintLine
  else
  begin
    // Handle percent steps (0% to 100%)
    Value := ExtractValue;
    if (Value <> -1) and (pgbOperationProgress.Max = 100) then
    begin
      pgbOperationProgress.Position := Value;
      pgbOperationProgress.Style := pbstNormal;
    end
    else
      PrintLine;
  end;
end;

function IsProgressAutoClose: Boolean;
begin
  Result := DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.ProgressWindowAutoClose;
end;

end.

