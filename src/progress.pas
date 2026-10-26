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
  ExtCtrls,
  ProgLine;

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
    fProgressLineParser: TProgressLineParser;
    function GetAbortOperation: Boolean;
    function GetAutoCloseState: Boolean;
    function IsSafeAbortRequested: Boolean;
    procedure SetFinished(AValue: Boolean);
    procedure SetIdleState(State: Boolean);
    procedure StartSafeAbort;
    procedure StopSafeAbort;
    procedure SetCloseButtonState(State: Boolean);
    procedure AddOutputLine(const Message: string);
  public
    destructor Destroy; override;
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
  InstMode,
  StrRes,
  MsgDlg,
  Global;

const
  CLOSE_TAG_VALUE = 1000;
  ABORT_SAFE_MAX_TRIES = 5;

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
  fProgressLineParser := TProgressLineParser.Create;
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
  fProgressLineParser.Reset;
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
      AddOutputLine(SendingCancelSignal);
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

// The output is followed only when the memo is scrolled to the bottom, so the
// user can scroll up (or right) to read the output without being disturbed
procedure TfrmProgress.AddOutputLine(const Message: string);
{$IFDEF WINDOWS}
var
  OutputHandle: HWND;
  FollowOutput: Boolean;
  LastVisibleLine,
  LastLine,
  FirstVisibleLine,
  HorizontalPosition,
  SelectionStart,
  SelectionLength,
  TextLength: Integer;

begin
  OutputHandle := memBufferOutput.Handle;

  // The memo is scrolled to the bottom when its last line is visible
  LastVisibleLine := HiWord(SendMessage(OutputHandle, EM_CHARFROMPOS, 0,
    MakeLParam(0, memBufferOutput.ClientHeight - 1)));

  // Lines.Add ends the text with a line break, so the last line is empty (and
  // holds the caret when following the output). EM_CHARFROMPOS never returns
  // this line as there is no character on it, so the line before is used.
  LastLine := SendMessage(OutputHandle, EM_GETLINECOUNT, 0, 0) - 1;
  if (LastLine > 0) and (SendMessage(OutputHandle, EM_LINELENGTH,
    SendMessage(OutputHandle, EM_LINEINDEX, LastLine, 0), 0) = 0) then
    Dec(LastLine);
  FollowOutput := LastVisibleLine >= LastLine;

  FirstVisibleLine := SendMessage(OutputHandle, EM_GETFIRSTVISIBLELINE, 0, 0);
  HorizontalPosition := GetScrollPos(OutputHandle, SB_HORZ);
  SelectionStart := memBufferOutput.SelStart;
  SelectionLength := memBufferOutput.SelLength;

  // Adding a line moves the caret and scrolls the memo, so the redraw is
  // disabled until the scroll position is restored
  SendMessage(OutputHandle, WM_SETREDRAW, 0, 0);
  try
    memBufferOutput.Lines.Add(Message);

    if FollowOutput then
    begin
      TextLength := SendMessage(OutputHandle, WM_GETTEXTLENGTH, 0, 0);
      SendMessage(OutputHandle, EM_SETSEL, TextLength, TextLength);
      SendMessage(OutputHandle, EM_SCROLLCARET, 0, 0);
    end
    else
    begin
      // Keep the selection and the visible lines where the user left them
      memBufferOutput.SelStart := SelectionStart;
      memBufferOutput.SelLength := SelectionLength;
      SendMessage(OutputHandle, EM_LINESCROLL, 0, FirstVisibleLine
        - SendMessage(OutputHandle, EM_GETFIRSTVISIBLELINE, 0, 0));
    end;

    // The horizontal position is always kept
    SendMessage(OutputHandle, WM_HSCROLL,
      MakeWParam(SB_THUMBPOSITION, HorizontalPosition), 0);
  finally
    SendMessage(OutputHandle, WM_SETREDRAW, 1, 0);
    RedrawWindow(OutputHandle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
  end;
{$ELSE}
begin
  memBufferOutput.Lines.Add(Message);
{$ENDIF}
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
        AddOutputLine(Format(OperationErrorMemoText, [LongMessage]));
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
  AddOutputLine(Message);
end;

destructor TfrmProgress.Destroy;
begin
  fProgressLineParser.Free;
  inherited Destroy;
end;

procedure TfrmProgress.AddNewLine(const Message: string);
var
  ProgressPosition: Integer;

begin
  // Display the line, except if it contains a single percentage (e.g., Git)
  if fProgressLineParser.Parse(Message, ProgressPosition) then
    AddOutputLine(Message);

  // Handle percent steps (0% to 100%)
  if (not IsSafeAbortRequested) then
    case ProgressPosition of
      PROGRESS_POSITION_UNCHANGED:
        ;
      PROGRESS_POSITION_MARQUEE:
        begin
          pgbOperationProgress.Position := 0;
          pgbOperationProgress.Style := pbstMarquee;
        end;
      else
        begin
          pgbOperationProgress.Position := ProgressPosition;
          pgbOperationProgress.Style := pbstNormal;
        end;
    end;

{$IF DEFINED(DEBUG) AND DEFINED(DEBUG_PROGRESS_ADD_NEW_LINE)}
  DebugLog(Format('ProgressType: %d, ProgressPosition: %d, Message: "%s"', [
    Ord(fProgressLineParser.ProgressType),
    ProgressPosition,
    Message
  ]));
{$ENDIF}
end;

function IsProgressAutoClose: Boolean;
begin
  Result := DreamcastSoftwareDevelopmentKitManager.Environment
    .Settings.ProgressWindowAutoClose;
end;

end.

