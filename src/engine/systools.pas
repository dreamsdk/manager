unit SysTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
function ExtremeRight(SubStr: string ; S: string): string;
function GetSubStrCount(SubStr, S: string): Integer;
function Left(SubStr: string; S: string): string;
function LeftNRight(SubStr, S: string; N: Integer): string;
function Right(SubStr: string; S: string): string;
function RunNoWait(Executable: string): Boolean; overload;
function RunNoWait(Executable, CommandLine: string): Boolean; overload;

implementation

uses
  Process
{$IF Defined(Unix) OR Defined(Darwin)}
  , UTF8Process
{$ENDIF}
  ;

// Thanks Michel (Phidels.com)
function GetSubStrCount(SubStr, S: string): Integer;
begin
  result:=0;
  while pos(substr,s)<>0 do
  begin
    S:=Right(substr,s);
    inc(result);
  end;
end;

// Thanks Michel (Phidels.com)
function LeftNRight(SubStr, S: string; N: Integer): string;
var i:integer;
begin
  S:=S+substr;
  for i:=1 to n do
  begin
    S:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
  end;
  result:=copy(s, 1, pos(substr, s)-1);
end;

// Thanks Michel (Phidels.com)
function Right(SubStr: string; S: string): string;
begin
  if pos(substr,s)=0 then result:='' else
    result:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
end;

// Thanks Michel (Phidels.com)
function Left(SubStr: string; S: string): string;
begin
  result:=copy(s, 1, pos(substr, s)-1);
end;

// Thanks Michel (Phidels.com)
function ExtractStr(LeftSubStr, RightSubStr, S: string): string;
begin
  Result := Left(RightSubStr, Right(LeftSubStr, S));
end;

// Thanks Michel (Phidels.com)
function ExtremeRight(SubStr: string ; S: string): string;
begin
  Repeat
    S:= Right(substr,s);
  until pos(substr,s)=0;
  result:=S;
end;

function RunNoWait(Executable: string): Boolean;
begin
  Result := RunNoWait(Executable, '');
end;

function RunNoWait(Executable, CommandLine: string): Boolean;
var
  OurProcess: {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF};

begin
  OurProcess := {$IFDEF Windows}TProcess{$ELSE}TProcessUTF8{$ENDIF}.Create(nil);
  try
    OurProcess.Executable := Executable;
    if CommandLine <> '' then
      OurProcess.Parameters.Add(CommandLine);
    OurProcess.ShowWindow := swoHide;
    OurProcess.Execute;
    Result := (OurProcess.ExitCode = 0);
  finally
    OurProcess.Free;
  end;
end;

end.

