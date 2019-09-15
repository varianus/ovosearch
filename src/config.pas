unit Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, JsonTools, typinfo;

type
  { TEnum }

 generic TEnum<T> = class(TObject)
  public
    class function ToString(const aEnumValue: T): string; reintroduce;
    class function FromString(const aEnumString: string; const aDefault: T): T;
  end;

{ TConfig }

  TConfig = class
  private
    FConfigFile: string;
    fConfigDir: string;
    ResourcesPath: string;
    fConfigHolder: TJsonNode;
  public
    constructor Create;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure WriteStrings(APath: string; Values: TStrings);
    function ReadStrings(APath: string; Values: TStrings): integer;
    procedure WriteString(APath: string; Value: String);
    function ReadString(APath: string; ADefault: String): string;
    function GetResourcesPath: string;
    procedure WriteBoolean(APath: string; Value: Boolean);
    function ReadBoolean(APath: string; ADefault: Boolean): Boolean;
    procedure WriteInteger(APath: string; Value: Integer);
    function ReadInteger(APath: string; ADefault: Integer): Integer;

    procedure Flush;
    destructor Destroy; override;
    // -- //
    property ConfigDir: string read fConfigDir;
    property ConfigFile: string read FConfigFile;
  end;

  { TSimpleHistory }

  TSimpleHistory = class
  private
    FMax: Integer;
    IntList: TStringList;
    function GetCount: integer;
    procedure SetMax(AValue: Integer);
  public
    function Add(const S: string): Integer;
    Constructor Create;
    Destructor Destroy; override;
    Procedure SetList(List: TStrings);

    Procedure LoadFromConfig(Config: TConfig; APath: string);
    Procedure WriteToConfig(Config: TConfig; APath: string);
    Property Max: Integer read FMax write SetMax;
    Property Count: integer read GetCount;
  end;

function ConfigObj: TConfig;

implementation

{ TConfig }
uses
  Fileutil
  // only for default font !
{$ifdef Darwin}
  , MacOSAll
{$endif}  ;

var
  FConfigObj: TConfig;

const
  SectionUnix = 'UNIX';
  IdentResourcesPath = 'ResourcesPath';
  ResourceSubDirectory = 'Resources';

const
 {$ifdef UNIX}
  DefaultDirectory = '/usr/share/ovosearch/';
  {$DEFINE NEEDCFGSUBDIR}
 {$endif}

 {$ifdef DARWIN}
  BundleResourcesDirectory = '/Contents/Resources/';
 {$endif}

  SectionGeneral = 'General';


function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelim: Char): string;
var
  TokStart: Integer;
begin
  repeat
    if SeekPos > Length(s) then begin Result := ''; Exit end;
    if S[SeekPos] = TokenDelim then Inc(SeekPos) else Break;
  until false;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not(S[SeekPos] = TokenDelim) do Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  result := Copy(s, TokStart, SeekPos-TokStart);

  { We don't have to do Inc(seekPos) below. But it's obvious that searching
    for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
  Inc(SeekPos);
end;

function GetConfigDir: string;
var
  Path: string;
begin
  Path := GetAppConfigDir(False);
  ForceDirectories(Path);
  Result := IncludeTrailingPathDelimiter(Path);

end;

function ConfigObj: TConfig;
begin
  if not Assigned(FConfigObj) then
    FConfigObj := TConfig.Create;
  Result := FConfigObj;
end;

{ TEnum }

class function TEnum.ToString(const aEnumValue: T): string;
begin
  WriteStr(Result, aEnumValue);
end;

class function TEnum.FromString(const aEnumString: string; const aDefault: T): T;
var
  OrdValue: Integer;
begin
  OrdValue := GetEnumValue(TypeInfo(T), aEnumString);
  if OrdValue < 0 then
    Result := aDefault
  else
    Result := T(OrdValue);
end;

{ TSimpleHistory }

procedure TSimpleHistory.SetMax(AValue: Integer);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;

  while IntList.Count > FMax do
    IntList.Delete(IntList.Count-1);           // -1 since its 0 indexed

end;

function TSimpleHistory.GetCount: integer;
begin
  Result := IntList.count;
end;

function TSimpleHistory.Add(const S: string): Integer;
var
   i : integer;
begin
   i := IntList.IndexOf(S);
   if i<>-1 then
     IntList.Delete(i);

   IntList.Insert(0, S);

   // Trim the oldest files if more than NumFiles
   while IntList.Count > FMax do
     IntList.Delete(IntList.Count-1);           // -1 since its 0 indexed

end;

constructor TSimpleHistory.Create;
begin
  IntList := TStringList.Create;
end;

destructor TSimpleHistory.Destroy;
begin
  FreeAndNil(IntList);
  inherited Destroy;
end;

procedure TSimpleHistory.SetList(List: TStrings);
begin
  List.Assign(IntList);
end;

procedure TSimpleHistory.LoadFromConfig(Config: TConfig; APath: string);
begin
  Config.ReadStrings(APath, IntList);
end;

procedure TSimpleHistory.WriteToConfig(Config: TConfig; APath: string);
begin
  Config.WriteStrings(APath, IntList);
end;

constructor TConfig.Create;
begin
  FConfigFile := GetAppConfigFile(False
{$ifdef NEEDCFGSUBDIR}
    , True
{$ENDIF}
    );
  fConfigDir := GetConfigDir;
  fConfigHolder := TJsonNode.Create;
  ReadConfig;

end;

destructor TConfig.Destroy;
begin
  SaveConfig;
  fConfigHolder.Free;
  inherited Destroy;
end;

procedure TConfig.SaveConfig;
begin
  fConfigHolder.SaveToFile(FConfigFile);
end;

procedure TConfig.ReadConfig;
begin

  fConfigHolder.LoadFromFile(FConfigFile);
{$ifdef WINDOWS}
  ResourcesPath := ReadString(SectionUnix + '/' + IdentResourcesPath,
    ExtractFilePath(ExtractFilePath(ParamStr(0))));
{$else}
  {$ifndef DARWIN}
  ResourcesPath := ReadString(SectionUnix + '/' + IdentResourcesPath, DefaultDirectory);
  {$endif}
{$endif}

end;

procedure TConfig.WriteStrings(APath: string; Values: TStrings);
var
  Node: TJsonNode;
  i: Integer;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    begin
     Node.Clear;
     for i := 0 to Values.Count -1 do
       node.Add('',Values[i]);
    end
  else
    begin
      Node := fConfigHolder.find(APath, true);  // fConfigHolder.Add(APath, nkArray);
      node.Kind:=nkArray;
      for i := 0 to Values.Count -1 do
        node.Add('',Values[i]);

    end;
end;

function TConfig.ReadStrings(APath: string; Values: TStrings): integer;
var
  Node: TJsonNode;
  i: Integer;
begin
  Values.Clear;
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
    begin
      for i := 0 to node.Count -1 do
       Values.Add(Node.Child(i).AsString);
    end
  else

  Result := Values.Count;
end;

procedure TConfig.WriteString(APath: string; Value: String);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Node.AsString := Value
  else
     begin
       fConfigHolder.find(APath, true).AsString := Value;
     end;

end;

function TConfig.ReadString(APath: string; ADefault: String): string;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Result :=  Node.AsString
  else
     Result :=  ADefault;
end;

procedure TConfig.WriteBoolean(APath: string; Value: Boolean);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Node.AsBoolean := Value
  else
     fConfigHolder.find(APath, true).AsBoolean := Value;

end;

function TConfig.ReadBoolean(APath: string; ADefault: Boolean): Boolean;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath, true);
  if Assigned(Node) then
     Result :=  Node.AsBoolean
  else
    Result :=  ADefault;
end;

procedure TConfig.WriteInteger(APath: string; Value: Integer);
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Node.AsInteger := Value
  else
     begin
       fConfigHolder.find(APath, true).AsInteger := Value;
     end;

end;

function TConfig.ReadInteger(APath: string; ADefault: Integer): Integer;
var
  Node: TJsonNode;
begin
  Node := fConfigHolder.find(APath);
  if Assigned(Node) then
     Result :=  Node.AsInteger
  else
     Result :=  ADefault;
end;

procedure TConfig.Flush;
begin
  fConfigHolder.SaveToFile(FConfigFile);
end;

function TConfig.GetResourcesPath: string;
{$ifdef DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := ResourcesPath;
{$endif}
{$endif}

{$ifdef WINDOWS}
  Result := ExtractFilePath(ExtractFilePath(ParamStr(0))) + ResourceSubDirectory + PathDelim;
{$endif}

end;

initialization
  FConfigObj := nil;

finalization
  if Assigned(FConfigObj) then
  begin
    FConfigObj.SaveConfig;
    FConfigObj.Free;
  end;


end.
