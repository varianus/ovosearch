unit DirectoryScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FilesFunctions;

type

  { TDirectoryScanner }

  TDirectoryScanner = class(TThread)
  private
    FileList: TStringList;
    CurrentPaths: TStringList;
    CurrFileName: TFileName;
    CurrInfo: TFileInfoObject;
    FMasks: String;
    function BuildFileList(const Path: string; const Attr: integer; Recurring: boolean; Filter: TFilters): boolean;
    procedure CallBack; register;
  protected
    procedure Execute; override;
  public
    constructor CreateScanner(Paths: TStrings; Masks:String);
    destructor Destroy;  override;
  end;

implementation
uses lclproc;

constructor TDirectoryScanner.CreateScanner(Paths: TStrings; Masks:String);
begin
  inherited Create(True);
  Priority     := tpIdle;
  CurrentPaths := TStringList.create;
  CurrentPaths.Assign(Paths);
  FMasks:= Masks;
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
      Masks := copy(Path, Length(Directory) + 1, Length(Path));
    end
    else
      Masks := Path;

    {* put the Masks into TStringlist *}
    StrToStrings(Masks, ';', MaskList, False);

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
            if Passed and Filter.CheckMinSize and (SearchRec.Size < filter.MinSize) then Passed := false;
            if Passed and Filter.CheckMaxSize and (SearchRec.Size > filter.MaxSize) then Passed := false;
            if Passed and Filter.CheckMinDate and (FileDateToDateTime(SearchRec.Time) < filter.MinDate) then Passed := false;
            if Passed and Filter.CheckMaxDate and (FileDateToDateTime(SearchRec.Time) > filter.MaxDate) then Passed := false;
            if Passed then
              begin
                info := TFileInfoObject.Create;
                info.info.Size := SearchRec.Size;
                info.info.ModifyDate := FileDateToDateTime(SearchRec.Time);
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
  Filter: TFilters;
begin

  for i:= 0 to CurrentPaths.Count -1 do
    begin
      BuildFileList(IncludeTrailingPathDelimiter(CurrentPaths[i]) + FMasks,
                    faAnyFile, True, filter);
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

