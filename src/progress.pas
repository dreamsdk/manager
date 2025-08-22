unit Progress;

{$mode objfpc}{$H+}

(* Enable me if you want to test the abort system *)
// {$DEFINE DEBUG_PROGRESS_SIMULATE_ABORT_ERROR}

(* Enable me if you don't want the window Stay On Top even in RELEASE mode *)
// {$DEFINE DEBUG_PROGRESS_AVOID_STAY_ON_TOP}

(* Use this if you want to debug the AddNewLine output *)
// {$DEFINE DEBUG_PROGRESS_ADD_NEW_LINE}

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
  TProgressType = (ptNoPercentage, ptPercentageSingle, ptPercentagePersistent);

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
    fProgressType: TProgressType;
    function GetAbortOperation: Boolean;
    function GetAutoCloseState: Boolean;
    function IsSafeAbortRequested: Boolean;
    procedure SetFinished(AValue: Boolean);
    procedure SetIdleState(State: Boolean);
    procedure StartSafeAbort;
    procedure StopSafeAbort;
    procedure SetCloseButtonState(State: Boolean);
  protected
    function IsProgressLine(const Message: string): Boolean;
    procedure PrintLine(const Message: string);
    function ExtractValueFromLine(const Message: string): Integer;
  public
    property Finished: Boolean read fFinished write SetFinished;
    procedure SetTerminateState(Success: Boolean; Aborted: Boolean;
      ForcedAbort: Boolean = False);
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
  Global;

const
  CLOSE_TAG_VALUE = 1000;
  ABORT_SAFE_MAX_TRIES = 5;

  PROGRESS_LINE1 = '% ';
  PROGRESS_LINE2 = '%] ';
  FULL = '100%';

{ TfrmProgress }

procedure TfrmProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    if not Finished then
    begin
      LogMessage(LogContext, 'User tried to close frmProgress, but process is running');    
    
      if AbortOperation then
      begin
        LogMessage(LogContext, 'AbortOperation is requested; pausing thread');      
        PauseThreadOperation;
        if MsgBoxDlg(Handle, CancelDialogCaption, CancelDialogText, mtWarning, [mbYes, mbNo], mbNo) = mrYes then
        begin
          LogMessage(LogContext, 'User want to abort the current process; StartSafeAbort called');
          StartSafeAbort;
        end
        else
        begin
          LogMessage(LogContext, 'User finally changed his/her mind; ResumeThreadOperation called');
          ResumeThreadOperation;
        end;    
      end;
    
      // Abort closing the window
      CloseAction := caNone;  
      Exit;
    end;

    LogMessage(LogContext, 'frmProgress will close now, as process is finished');
  
    if IsPostInstallMode then
    begin
      LogMessage(LogContext, 'Closing frmProgress on post-install mode: calling Application.Terminate');
      Application.Terminate;
    end;

  finally
    LogMessageExit(LogContext);
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
  fProgressType := ptNoPercentage;
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
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
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

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TfrmProgress.tmrAbortFailSafeStopTimer(Sender: TObject);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    pgbOperationProgress.Position := 0;
    Sleep(500);
    SetTerminateState(False, True, True);
    Application.ProcessMessages;

  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TfrmProgress.tmrAbortFailSafeTimer(Sender: TObject);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try

    pgbOperationProgress.Position := pgbOperationProgress.Position + 1;
    if (pgbOperationProgress.Position >= pgbOperationProgress.Max) then
      StopSafeAbort;

  finally
    LogMessageExit(LogContext);
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

function TfrmProgress.IsProgressLine(const Message: string): Boolean;
begin
  Result := (IsInString(PROGRESS_LINE1, Message))
    or (IsInString(PROGRESS_LINE2, Message));
end;

procedure TfrmProgress.PrintLine(const Message: string);
begin
  if (fProgressType <> ptPercentageSingle) then
    memBufferOutput.Lines.Add(Message);

  if (not IsSafeAbortRequested) and (fProgressType = ptNoPercentage) then
  begin
    pgbOperationProgress.Position := 0;
    pgbOperationProgress.Style := pbstMarquee;
  end;

{$IF DEFINED(DEBUG) AND DEFINED(DEBUG_PROGRESS_ADD_NEW_LINE)}
  DebugLog(Format('ProgressType: %d, Message: "%s"', [
    Ord(fProgressType),
    Message
  ]));
{$ENDIF}
end;

function TfrmProgress.ExtractValueFromLine(const Message: string): Integer;
const
  MAX_PERCENT = 100;

  function ParseValue(LeftStr, RightStr, Str: string): Integer;
  var
    StrExtractedValue: string;

  begin
    StrExtractedValue := Trim(ExtractStr(LeftStr, RightStr, Str));
    Result := StrToIntDef(StrExtractedValue, -1);
  end;

var
  i: Integer;
  Buffer: string;
  IsPersistentPercentageMode: Boolean;

begin
  Result := -1;
  IsPersistentPercentageMode := False;
  Buffer := Default(string);

  // For Git
  Result := ParseValue(':', PROGRESS_LINE1 + ' (', Message);

  // For Wget and Package Manager
  if (Result = -1) then
  begin
    i := Pos(PROGRESS_LINE1, Message);
    Buffer := Copy(Message, i - 4, i - 1);
    Result := ParseValue(' ', PROGRESS_LINE1, Buffer);
  end;

  // If we detect "100%" then the value is 100...
  if (Result = -1) and (IsInString(FULL, Buffer)) then
    Result := MAX_PERCENT;

  // For CMake
  if (Result = -1) then
  begin
    Result := ParseValue('[', PROGRESS_LINE2, Message);
    IsPersistentPercentageMode := (Result <> -1);
  end;

  // There is a value here to display in the progress bar...
  if (Result <> -1) then
  begin
    fProgressType := ptPercentageSingle;
    if IsPersistentPercentageMode and (Result < MAX_PERCENT) then
      fProgressType := ptPercentagePersistent;
  end;
end;

function TfrmProgress.GetAbortOperation: Boolean;
begin
  Result := btnAbort.Tag <> CLOSE_TAG_VALUE;
end;

function TfrmProgress.GetAutoCloseState: Boolean;
begin
  Result := cbxAutocloseWindow.Checked;
end;

function TfrmProgress.IsSafeAbortRequested: Boolean;
begin
  Result := tmrAbortFailSafe.Enabled;
end;

procedure TfrmProgress.SetFinished(AValue: Boolean);
var
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    if fFinished = AValue then
      Exit;

    LogMessage(LogContext, Format('Marking frmProgress.Finished: %s', [
      BoolToStr(AValue, True)
    ]));
    fFinished := AValue;
  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TfrmProgress.SetTerminateState(Success: Boolean; Aborted: Boolean;
  ForcedAbort: Boolean);
var
  Message,
  LongMessage: string;
  LogContext: TLogMessageContext;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%}, ClassName);
  try
    LogMessage(LogContext, Format('Success: "%s", Aborted: "%s", ForcedAbort: "%s"', [
      BoolToStr(Success),
      BoolToStr(Aborted),
      BoolToStr(ForcedAbort)
    ]));

    Finished := True;

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

      if not ForcedAbort then
        memBufferOutput.Lines.Add(Format(OperationErrorMemoText, [LongMessage]));
      lblProgressStep.Caption := Message;
    end
    else
    begin
      SetProgressText(OperationSuccessfullyTerminated);
      if (not IsPostInstallMode) and IsProgressAutoClose then
        Close;
    end;

    LogMessage(LogContext, Format('Message: "%s", LongMessage: "%s"', [
      Message,
      LongMessage
    ]));
  finally
    LogMessageExit(LogContext);
  end;
end;

procedure TfrmProgress.SetProgressText(const Message: string);
begin
  lblProgressStep.Caption := Message;
  frmProgress.memBufferOutput.Lines.Add(Message);
end;

procedure TfrmProgress.AddNewLine(const Message: string);
var
  Value: Integer;

begin
  // Handle percent steps (0% to 100%)
  case IsProgressLine(Message) of
    // The current line contains a percentage. We will extract it and handle it
    True:
      begin
        Value := ExtractValueFromLine(Message);
        if (not IsSafeAbortRequested) and (Value <> -1) then
        begin
          pgbOperationProgress.Position := Value;
          pgbOperationProgress.Style := pbstNormal;
        end;
      end;
    // The current line don't have any percentage.
    False:
      begin
        if (fProgressType <> ptPercentagePersistent) then
          // We don't have any percentage currently, so reset the flag.
          fProgressType := ptNoPercentage;
      end;
  end;

  // Display the line
  PrintLine(Message);
end;

function IsProgressAutoClose: Boolean;
begin
  Result := DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.ProgressWindowAutoClose;
end;

end.

