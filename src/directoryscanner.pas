unit DirectoryScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FilesFunctions;

type

  { TFilters }

  TFilters = class
  private
    FCheckContent: boolean;
    FCheckMaxDate: boolean;
    FCheckMaxSize: boolean;
    FCheckMinDate: boolean;
    FCheckMinSize: boolean;
    FContent: string;
    FMask: string;
    FMaxDate: TDateTime;
    FMaxSize: int64;
    FMinDate: TDateTime;
    FMinSize: Int64;
    procedure SetCheckContent(AValue: boolean);
    procedure SetCheckMaxDate(AValue: boolean);
    procedure SetCheckMaxSize(AValue: boolean);
    procedure SetCheckMinDate(AValue: boolean);
    procedure SetCheckMinSize(AValue: boolean);
    procedure SetContent(AValue: string);
    procedure SetMask(AValue: string);
    procedure SetMaxDate(AValue: TDateTime);
    procedure SetMaxSize(AValue: int64);
    procedure SetMinDate(AValue: TDateTime);
    procedure SetMinSize(AValue: Int64);
  public
    property CheckContent: boolean read FCheckContent write SetCheckContent;
    property CheckMinSize: boolean read FCheckMinSize write SetCheckMinSize;
    property CheckMaxSize: boolean read FCheckMaxSize write SetCheckMaxSize;
    property CheckMinDate: boolean read FCheckMinDate write SetCheckMinDate;
    property CheckMaxDate: boolean read FCheckMaxDate write SetCheckMaxDate;
    property Content: string read FContent write SetContent;
    property MinSize: Int64 read FMinSize write SetMinSize;
    property MaxSize: int64 read FMaxSize write SetMaxSize;
    property MinDate: TDateTime read FMinDate write SetMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    Property Mask: string read FMask write SetMask;

  end;

  { TDirectoryScanner }

  TDirectoryScanner = class(TThread)
  private
    FileList: TStringList;
    CurrentPaths: TStringList;
    CurrFileName: TFileName;
    CurrInfo: TFileInfoObject;
    FFilters: TFilters;
    function BuildFileList(const Path: string; const Attr: integer; Recurring: boolean; Filter: TFilters): boolean;
    procedure CallBack; register;
  protected
    procedure Execute; override;
  public
    constructor CreateScanner(Paths: TStrings; Filters: TFilters);
    destructor Destroy;  override;
  end;

implementation
uses lclproc;

{ TFilters }

procedure TFilters.SetCheckContent(AValue: boolean);
begin
  if FCheckContent=AValue then Exit;
  FCheckContent:=AValue;
end;

procedure TFilters.SetCheckMaxDate(AValue: boolean);
begin
  if FCheckMaxDate=AValue then Exit;
  FCheckMaxDate:=AValue;
end;

procedure TFilters.SetCheckMaxSize(AValue: boolean);
begin
  if FCheckMaxSize=AValue then Exit;
  FCheckMaxSize:=AValue;
end;

procedure TFilters.SetCheckMinDate(AValue: boolean);
begin
  if FCheckMinDate=AValue then Exit;
  FCheckMinDate:=AValue;
end;

procedure TFilters.SetCheckMinSize(AValue: boolean);
begin
  if FCheckMinSize=AValue then Exit;
  FCheckMinSize:=AValue;
end;

procedure TFilters.SetContent(AValue: string);
begin
  if FContent=AValue then Exit;
  FContent:=AValue;
end;

procedure TFilters.SetMask(AValue: string);
begin
  if FMask=AValue then Exit;
  FMask:=AValue;
end;

procedure TFilters.SetMaxDate(AValue: TDateTime);
begin
  if FMaxDate=AValue then Exit;
  FMaxDate:=AValue;
end;

procedure TFilters.SetMaxSize(AValue: int64);
begin
  if FMaxSize=AValue then Exit;
  FMaxSize:=AValue;
end;

procedure TFilters.SetMinDate(AValue: TDateTime);
begin
  if FMinDate=AValue then Exit;
  FMinDate:=AValue;
end;

procedure TFilters.SetMinSize(AValue: Int64);
begin
  if FMinSize=AValue then Exit;
  FMinSize:=AValue;
end;

constructor TDirectoryScanner.CreateScanner(Paths: TStrings; Filters: TFilters);
begin
  inherited Create(True);
  Priority     := tpIdle;
  CurrentPaths := TStringList.create;
  CurrentPaths.Assign(Paths);
  FFilters:= Filters;
  FreeOnTerminate := True;

end;

destructor TDirectoryScanner.Destroy;
begin

  CurrentPaths.Free;
  Inherited Destroy;
end;

procedure TDirectoryScanner.CallBack;
begin

end;

function TDirectoryScanner.BuildFileList(const Path: string; const Attr: integer; Recurring: boolean; Filter:TFilters): boolean;
var
  SearchRec: TSearchRec;
  IndexMask: integer;
  MaskList: TStringList;
  Masks, Directory: string;
  info: TFileInfoObject;
  Passed: Boolean;
begin
  MaskList := TStringList.Create;
  try
    {* extract the Directory *}
    Directory := ExtractFileDir(Path);

    {* files can be searched in the current directory *}
    if Directory <> '' then
    begin
      Directory := IncludeTrailingPathDelimiter(Directory);
      {* extract the FileMasks portion out of Path *}
    end;

    {* put the Masks into TStringlist *}
    StrToStrings(FFilters.Mask, ';', MaskList, False);

    {* search all files in the directory *}
    Result := FindFirst(Directory + AllFilesMask, faAnyFile, SearchRec) = 0;

    try
      while Result do
      begin
        if Terminated then exit;

        {* if the filename matches any mask then it is added to the list *}
        if Recurring and ((searchrec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and
          (SearchRec.Name <> '..') then
          BuildFileList(IncludeTrailingPathDelimiter(Directory + SearchRec.Name) + masks,
            Attr, Recurring, filter);

        for IndexMask := 0 to MaskList.Count - 1 do
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
            ((SearchRec.Attr and Attr) = (SearchRec.Attr and faAnyFile)) and
            IsFileNameMatch(SearchRec.Name, MaskList.Strings[IndexMask], False) then
          begin
            Passed := true;
            if Passed and FFilters.CheckMinSize and (SearchRec.Size < ffilters.MinSize) then Passed := false;
            if Passed and FFilters.CheckMaxSize and (SearchRec.Size > ffilters.MaxSize) then Passed := false;
            if Passed and FFilters.CheckMinDate and (SearchRec.TimeStamp < ffilters.MinDate) then Passed := false;
            if Passed and FFilters.CheckMaxDate and (SearchRec.TimeStamp > ffilters.MaxDate) then Passed := false;
            if Passed then
              begin
                info := TFileInfoObject.Create;
                info.info.Size := SearchRec.Size;
                info.info.ModifyDate := SearchRec.TimeStamp;
                //List.AddObject(SysToUTF8(Directory + SearchRec.Name), info);
                //
              end;
            Break;
          end;

        case FindNext(SearchRec) of
          0: ;
          2: //ERROR_NO_MORE_FILES:
            Break;
          else
            Result := False;
        end;
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
    MaskList.Free;
  end;
end;

procedure TDirectoryScanner.Execute;
var
  i: integer;
begin

  for i:= 0 to CurrentPaths.Count -1 do
    begin
      BuildFileList(IncludeTrailingPathDelimiter(CurrentPaths[i]),
                    faAnyFile, True, Ffilters);
    end;
//
//  for I := 0 to FileList.Count - 1 do
//    begin
//      try
//      CurrFileName := FileList[i];
//      CurrInfo := TFileInfoObject(FileList.Objects[i]);
//      Synchronize(@Callback);
//      Except
//        DebugLn('Error reading ', FileList[i]);
//      end;
//    end;
//  FileList.free;
end;


end.

