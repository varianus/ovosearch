unit ProcessThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pipes, process;

type

  TRunnerMessageKind= (ieInfo, ieCommand, ieError);

  TRunnerMessage = procedure (Sender: TObject; const MessageKind: TRunnerMessageKind; const Message: string) of object;

  { TProcessThread }

  TProcessThread = class(TThread)
  private
    FOnRunnerMessage: TRunnerMessage;
    fProcess: TProcess;
    fLines: TStringList;
    procedure Elabora;
    procedure SetOnRunnerMessage(AValue: TRunnerMessage);
  protected
    procedure Execute; override;
  public
    property Process: TProcess read fProcess;
    property OnRunnerMessage : TRunnerMessage read FOnRunnerMessage write SetOnRunnerMessage;
    Constructor Create;
    destructor Destroy; override;


  end;

implementation
uses
  uMainForm, math;
{ TProcessThread }

procedure TProcessThread.Elabora;
var
  n: Integer;
begin
  for n := 0 to fLines.Count - 1 do
    begin
      if Assigned(FOnRunnerMessage)   then
         FOnRunnerMessage(Self, ieInfo, Flines[n]);
    end;
  fLines.Clear;
end;

procedure TProcessThread.SetOnRunnerMessage(AValue: TRunnerMessage);
begin
  if FOnRunnerMessage=AValue then Exit;
  FOnRunnerMessage:=AValue;
end;

procedure TProcessThread.Execute;
var
  Buf: string = '';
  HasOutput: boolean;
  OutputLine: String = '';
  StdErrLine: String = '';

  function ReadInputPipe(aStream: TInputPipeStream; var LineBuf: string;  IsStdErr: boolean): boolean;
  // true if some bytes have been read
  var
    Count: DWord;
    StartPos: Integer;
    i: DWord;
  begin
    Result:=false;
    if aStream=nil then exit;
    Count:=aStream.NumBytesAvailable;
    if Count=0 then exit;
    Count:=aStream.Read(Buf[1],Min(length(Buf),Count));
    if Count=0 then exit;
    Result:=true;
    StartPos:=1;
    i:=1;
    while i<=Count do
      begin
        if Buf[i] in [#10,#13] then
          begin
            LineBuf:=LineBuf+copy(Buf,StartPos,i-StartPos);
            if IsStdErr then
              fLines.AddObject(LineBuf,fLines)
            else
              fLines.Add(LineBuf);

            LineBuf:='';
            if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
              inc(i);

            StartPos:=i+1;
          end;
        inc(i);
      end;
    LineBuf:=LineBuf+copy(Buf,StartPos,Count-StartPos+1);
  end;

begin
  SetLength(Buf,4096);
  fLines:=TStringList.Create;

  fProcess.Execute;

  while (not Terminated) do // and (FProcess.Running)  do
    begin
      HasOutput := ReadInputPipe(FProcess.Output,OutputLine,false);
      Synchronize(@Elabora);
      if (not HasOutput) then
        if not FProcess.Running then
          break
        else
          Sleep(100);
    end;

  if (OutputLine<>'') then
    fLines.Add(OutputLine);
  Synchronize(@Elabora);

  fProcess.Terminate(100);

  Synchronize(@Elabora);
  fLines.free;

end;

constructor TProcessThread.Create;
begin
  inherited Create(true);
  fProcess := TProcess.Create(Nil);
  FProcess.Options:= [poUsePipes{$IFDEF Windows},poStderrToOutPut{$ENDIF}];
  FProcess.ShowWindow := swoHide;

end;

destructor TProcessThread.Destroy;
begin
  if fProcess.Running then
    fProcess.Terminate(100);
  fProcess.Free;
  inherited Destroy;
end;

end.

