unit ProcessThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pipes, process;

type

  TMessageLineKind= (ieInfo, ieCommand, ieError);

  TMessageLineEvent = procedure (Sender: TObject; const MessageKind: TMessageLineKind; const Message: string) of object;
  TMessageListEvent = procedure (Sender: TObject; Out MessageList: TStrings) of object;

  { TProcessThread }

  TProcessThread = class(TThread)
  private
    FOnMessageLine: TMessageLineEvent;
    FOnMessageList: TMessageListEvent;
    fProcess: TProcess;
    fLines: TStringList;
    procedure SendResults;
    procedure SetOnMessageLine(AValue: TMessageLineEvent);
    procedure SetOnMessageList(AValue: TMessageListEvent);
  protected
    procedure Execute; override;
  public
    property Process: TProcess read fProcess;
    property OnMessageLine : TMessageLineEvent read FOnMessageLine write SetOnMessageLine;
    property OnMessageList : TMessageListEvent read FOnMessageList write SetOnMessageList;
    Constructor Create;
    destructor Destroy; override;


  end;

implementation
uses
  math, LazLoggerBase;
{ TProcessThread }

procedure TProcessThread.SendResults;
var
  n: Integer;
begin
  for n := 0 to fLines.Count - 1 do
    begin
      if Assigned(FOnMessageLine)   then
        begin
          if Assigned(fLines.Objects[n]) then
            FOnMessageLine(Self, ieError, Flines[n])
          else
            FOnMessageLine(Self, ieInfo, Flines[n]);
        end;
    end;

  if Assigned(FOnMessageList) then
     FOnMessageList(Self,  FLines);

  fLines.Clear;
end;

procedure TProcessThread.SetOnMessageLine(AValue: TMessageLineEvent);
begin
  if FOnMessageLine=AValue then Exit;
  FOnMessageLine:=AValue;
end;

procedure TProcessThread.SetOnMessageList(AValue: TMessageListEvent);
begin
  if FOnMessageList=AValue then Exit;
  FOnMessageList:=AValue;
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
            debugln(linebuf);
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
  SetLength(Buf,16*1024);
  fLines:=TStringList.Create;

  fProcess.Execute;

  while (not Terminated) do // and (FProcess.Running)  do
    begin
      HasOutput := ReadInputPipe(FProcess.Output,OutputLine,false) or ReadInputPipe(FProcess.Stderr,StdErrLine,True);
      SendResults;
      if (not HasOutput) then
        if not FProcess.Running then
          break
        else
          Sleep(100);
    end;

  if (OutputLine<>'') then
    fLines.Add(OutputLine);
  if (StdErrLine<>'') then
    fLines.AddObject(OutputLine, self);

  SendResults;

  fProcess.Terminate(100);

  fLines.free;

end;

constructor TProcessThread.Create;
begin
  inherited Create(true);
  fProcess := TProcess.Create(Nil);
  FProcess.Options:= [poUsePipes{,poStderrToOutPut}];
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

