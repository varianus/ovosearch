unit DirectoryScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FilesFunctions;

type

  TDirectoryScanner = class(TThread)
  private
    FileList: TStringList;
    CurrentPaths: TStringList;
    CurrFileName: TFileName;
    CurrInfo: TFileInfoObject;
    FMasks: String;
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

procedure TDirectoryScanner.Execute;
var
  i: integer;

begin
  FileList := TStringList.Create;
  FileList.OwnsObjects := true;

  for i:= 0 to CurrentPaths.Count -1 do
    begin
      BuildFileList(IncludeTrailingPathDelimiter(CurrentPaths[i]) + FMasks,
                    faAnyFile, FileList, True);
    end;

  for I := 0 to FileList.Count - 1 do
    begin
      try
      CurrFileName := FileList[i];
      CurrInfo := TFileInfoObject(FileList.Objects[i]);
      Synchronize(@Callback);
      Except
        DebugLn('Error reading ', FileList[i]);
      end;
    end;
  FileList.free;
end;


end.

