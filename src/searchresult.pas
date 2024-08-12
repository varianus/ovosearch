unit searchresult;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, FilesFunctions;

type
  TResultField = (rfFileName, rfMAtchesCount, rfPath, rfSize, rfDate, rfNone);
  TSortDirection = (sdAscending, sdDescending);


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
    FPath: TFileName;
    function GetByColumn(idx: integer): string;
    function GetFullName: TFileName;
    procedure SetFileName(AValue: TFileName);
    procedure SetFullName(AValue: TFileName);
    procedure SetMatchedLine(AValue: integer);
    procedure SetMatches(AValue: Integer);
    procedure SetPath(AValue: TFileName);
  public
    Property FileName: TFileName read FFileName write SetFileName;
    property Path: TFileName read FPath write SetPath;
    property FullName: TFileName read GetFullName write SetFullName;
    property FoundLines: TFoundLines read FFoundLines;
    property Matches: Integer read FMatches write SetMatches;
    property MatchedLine: integer read FMatchedLine write SetMatchedLine;
    property FileInfo: TFileInfo read FFileInfo write FFileInfo;
    property ByColumn[idx:integer]: string read GetByColumn;
    Constructor Create;
    Destructor Destroy; override;
  end;

  { TFoundFiles }

  TFoundFiles = class(TObjectList<TFoundFile>)
  private
    FSortColumn: TResultField;
    FSortDirection: TSortDirection;
    procedure SetSortColumn(AValue: TResultField);
    procedure SetSortDirection(AValue: TSortDirection);
  public
    property SortColumn: TResultField read FSortColumn write SetSortColumn;
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection;
    Procedure SortbyColumn(Field: TResultField; Direction: TSortDirection);
    function Add(const AValue: TFoundFile): SizeInt; override;
    constructor Create;
  end;


implementation
uses Generics.Defaults, DateUtils, math;

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

{ TFoundFiles }

function CompareFileNameAsc(constref A,B:TFoundFile): integer;
begin
  Result := CompareText(A.FFileName, b.FFileName);
end;
function CompareFileNameDesc(constref A,B:TFoundFile): integer;
begin
  Result := - CompareText(A.FFileName, b.FFileName);
end;
function ComparePathAsc(constref A,B:TFoundFile): integer;
begin
  Result := CompareText(A.Path, b.Path);
  if Result = 0 then
    Result := CompareFileNameAsc(a,b);
end;
function ComparePathDesc(constref A,B:TFoundFile): integer;
begin
  Result := - CompareText(A.Path, b.Path);
  if Result = 0 then
    Result := CompareFileNameDesc(a,b);

end;

function CompareMatchesAsc(constref A,B:TFoundFile): integer;
begin
  Result := CompareValue(A.Matches, b.Matches);
end;
function CompareMatchesDesc(constref A,B:TFoundFile): integer;
begin
  Result := - CompareValue(A.Matches, b.Matches);
end;

function CompareSizeAsc(constref A,B:TFoundFile): integer;
begin
  Result := CompareValue(A.FFileInfo.Size, b.FFileInfo.Size);
end;
function CompareSizeDesc(constref A,B:TFoundFile): integer;
begin
  Result := - CompareValue(A.FFileInfo.Size, b.FFileInfo.Size);
end;

function CompareDateAsc(constref A,B:TFoundFile): integer;
begin
  Result := CompareDate(A.FFileInfo.ModifyDate, b.FFileInfo.ModifyDate);
end;
function CompareDateDesc(constref A,B:TFoundFile): integer;
begin
  Result := - CompareDate(A.FFileInfo.ModifyDate, b.FFileInfo.ModifyDate);
end;

procedure TFoundFile.SetPath(AValue: TFileName);
begin
  if FPath=AValue then Exit;
  FPath:=AValue;
end;

procedure TFoundFiles.SetSortColumn(AValue: TResultField);
begin
  if FSortColumn=AValue then Exit;
   FSortColumn:=AValue;
   if FSortColumn <> rfNone then
     SortbyColumn(FSortColumn, FSortDirection);
end;

procedure TFoundFiles.SetSortDirection(AValue: TSortDirection);
begin
  if FSortDirection=AValue then Exit;
  FSortDirection:=AValue;
  if FSortColumn <> rfNone then
    SortbyColumn(FSortColumn, FSortDirection);

end;

procedure TFoundFiles.SortbyColumn(Field: TResultField; Direction: TSortDirection);
var
  Compare: TComparisonFunc<TFoundFile>;
begin
  case Field of
    rfFileName: if Direction = sdAscending then Compare:= @CompareFileNameAsc else Compare:= @CompareFileNameDesc;
    rfMAtchesCount: if Direction = sdAscending then Compare:= @CompareMatchesAsc else Compare:= @CompareMatchesDesc;
    rfPath: if Direction = sdAscending then Compare:= @ComparePathAsc else Compare:= @ComparePathDesc;
    rfSize: if Direction = sdAscending then Compare:= @CompareSizeAsc else Compare:= @CompareSizeDesc;
    rfDate: if Direction = sdAscending then Compare:= @CompareDateAsc else Compare:= @CompareDateDesc;
  end;
  fSortColumn := Field;
  FSortDirection:=Direction;
  Sort(TComparer<TFoundFile>.Construct(Compare));
end;

function TFoundFiles.Add(const AValue: TFoundFile): SizeInt;
var
  LSearchResult: TBinarySearchResult;
begin
  if FSortColumn = rfNone then
    Result:=inherited Add(AValue)
  else
    begin
      if TArrayHelperBugHack.BinarySearch(FItems, AValue, LSearchResult, FComparer, 0, Count) then
        Result := LSearchResult.FoundIndex
      else
        begin
         if LSearchResult.CandidateIndex = -1 then
           Result := 0
         else
           if LSearchResult.CompareResult > 0 then
             Result := LSearchResult.CandidateIndex
           else
            Result := LSearchResult.CandidateIndex + 1;
       end;
     InternalInsert(Result, AValue);
  end;
end;

constructor TFoundFiles.Create;
begin
  inherited Create(True);
  FSortColumn:=rfNone;
  FSortDirection:=sdAscending;


end;

{ TFoundFile }

procedure TFoundFile.SetFileName(AValue: TFileName);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

function TFoundFile.GetFullName: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(FPath)+FFileName;
end;

function TFoundFile.GetByColumn(idx: integer): string;
begin
  case idx of
    0:
      Result := FFileName;
    1:
      Result := (IntToStr(FMatches));
    2:
      Result := FPath;
    3:
      Result := (strByteSize(FFileInfo.Size));
    4:
      Result := (DateTimeToStr(FFileInfo.ModifyDate));
  end;
end;

procedure TFoundFile.SetFullName(AValue: TFileName);
begin
  FFileName:=ExtractFileName(AValue);
  FPath:=ExtractFilePath(AValue);
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

