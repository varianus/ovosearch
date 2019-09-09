unit searchresult;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  { TFoundLine }

  TFoundLine = class
  private
    FColum: integer;
    FLine: string;
    FRow: integer;
    procedure SetColum(AValue: integer);
    procedure SetLine(AValue: string);
    procedure SetRow(AValue: integer);
  public
    property Row: integer read FRow write SetRow;
    property Colum: integer read FColum write SetColum;
    property Line: string read FLine write SetLine;
  end;

  TFoundLines = class(TObjectList<TFoundLine>)

  end;

  { TFoundFile }
  TFoundFile = class
  private
    FFileName: TFileName;
    FLines: TFoundLines;
    procedure SetFileName(AValue: TFileName);
  public
    Property FileName: TFileName read FFileName write SetFileName;
    property Lines: TFoundLines read FLines;
    Constructor Create;
    Destructor Destroy; override;
  end;

  { TFoundFiles }

  TFoundFiles = class(TObjectList<TFoundFile>)
  public
  end;


implementation

{ TFoundFile }

procedure TFoundFile.SetFileName(AValue: TFileName);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

constructor TFoundFile.Create;
begin
  inherited Create;
  FLines:= TFoundLines.Create;
end;

destructor TFoundFile.Destroy;
begin
  FLines.Free;

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

