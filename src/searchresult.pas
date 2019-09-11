unit searchresult;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, FilesFunctions;

type

  { TThreadQueue }
  TThreadQueue<T> = class (TQueue<T>)
  Private
    fLock : TRTLCriticalSection;
  protected
    function GetCount: SizeInt; override;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Enqueue(constref AValue: T); reintroduce;
    function Dequeue: T;  reintroduce;
    procedure Clear;  reintroduce;
    function LockList: TThreadQueue<T>;
    procedure UnlockList; inline;
  end;

  { TFoundLine }
  TSubMatch = record
    Start, _End: integer;
  end;

  TSubMatches = array of TSubMatch;

  TFoundLine = class
  private
    FColum: integer;
    FLine: string;
    FRow: integer;
    procedure SetColum(AValue: integer);
    procedure SetLine(AValue: string);
    procedure SetRow(AValue: integer);
  public
    SubMatches: TSubMatches;
    property Row: integer read FRow write SetRow;
    property Colum: integer read FColum write SetColum;
    property Line: string read FLine write SetLine;
  end;

  TFoundLines = class(TObjectList<TFoundLine>)

  end;

  { TFoundFile }
  TFoundFile = class
  private
    FFileInfo: TFileInfo;
    FFileName: TFileName;
    FFoundLines: TFoundLines;
    FMatchedLine: integer;
    FMatches: Integer;
    procedure SetFileName(AValue: TFileName);
    procedure SetMatchedLine(AValue: integer);
    procedure SetMatches(AValue: Integer);
  public
    Property FileName: TFileName read FFileName write SetFileName;
    property FoundLines: TFoundLines read FFoundLines;
    property Matches: Integer read FMatches write SetMatches;
    property MatchedLine: integer read FMatchedLine write SetMatchedLine;
    property FileInfo: TFileInfo read FFileInfo write FFileInfo;
    Constructor Create;
    Destructor Destroy; override;
  end;

  { TFoundFiles }

  TFoundFiles = class(TObjectList<TFoundFile>)
  public
  end;


implementation

{ TThreadQueue }

function TThreadQueue<T>.GetCount: SizeInt;
begin
  EnterCriticalSection(fLock);
  try
    Result := inherited GetCount;
  finally
    LeaveCriticalSection(fLock);
  end;

end;

constructor TThreadQueue<T>.Create;
begin
  inherited Create;
  InitCriticalSection(fLock);
end;

destructor TThreadQueue<T>.Destroy;
begin
  Clear;
  inherited Destroy;
  DoneCriticalSection(fLock);
end;

procedure TThreadQueue<T>.Enqueue(constref AValue: T);
begin
  EnterCriticalSection(fLock);
  try
    inherited Enqueue(AValue);
  finally
    LeaveCriticalSection(fLock);
  end;

end;

function TThreadQueue<T>.Dequeue: T;
begin
  EnterCriticalSection(fLock);
  try
    Result := inherited Dequeue;
  finally
    LeaveCriticalSection(fLock);
  end;

end;

procedure TThreadQueue<T>.Clear;
begin
  EnterCriticalSection(fLock);
  try
    inherited Clear;
  finally
    LeaveCriticalSection(fLock);
  end;

end;

function TThreadQueue<T>.LockList: TThreadQueue<T>;
begin
  EnterCriticalSection(fLock);
  Result := self;
end;

procedure TThreadQueue<T>.UnlockList;
begin
  LeaveCriticalSection(fLock);
end;

{ TFoundFile }

procedure TFoundFile.SetFileName(AValue: TFileName);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TFoundFile.SetMatchedLine(AValue: integer);
begin
  if FMatchedLine = AValue then Exit;
  FMatchedLine := AValue;
end;

procedure TFoundFile.SetMatches(AValue: Integer);
begin
  if FMatches = AValue then Exit;
  FMatches := AValue;
end;

constructor TFoundFile.Create;
begin
  inherited Create;
  FFoundLines:= TFoundLines.Create;
end;

destructor TFoundFile.Destroy;
begin
  FFoundLines.Free;

  inherited Destroy;
end;

{ TFoundLine }

procedure TFoundLine.SetColum(AValue: integer);
begin
  if FColum=AValue then Exit;
  FColum:=AValue;
end;

procedure TFoundLine.SetLine(AValue: string);
begin
  if FLine=AValue then Exit;
  FLine:=AValue;
end;

procedure TFoundLine.SetRow(AValue: integer);
begin
  if FRow=AValue then Exit;
  FRow:=AValue;
end;

end.

