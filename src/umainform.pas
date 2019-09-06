unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, AnchorDockPanel, searchresult, ProcessThread, JsonTools;

type

  { TForm1 }
  TSearchState = (ssFile, ssLine, ssNone);

  TForm1 = class(TForm)
    bSearch: TButton;
    lbContaining: TLabeledEdit;
    lbFiles: TLabeledEdit;
    lbPath: TLabeledEdit;
    lvFiles: TListView;
    Memo1: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure bSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvFilesData(Sender: TObject; Item: TListItem);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FoundFiles: TFoundFiles;
    SearchState : TSearchState;
    Node: TJsonNode;
    CurrObj : TfoundFile;
    procedure GotMessage(Sender: TObject;
      const MessageKind: TRunnerMessageKind; const Message: string);
    procedure UpdateForm;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.UpdateForm;
begin

end;

procedure TForm1.GotMessage(Sender: TObject;
  const MessageKind: TRunnerMessageKind; const Message: string);
var
  Line: TFoundItem;

begin
  if (MessageKind <> ieinfo) or (message = '') then
    exit;

  Node.Free;
  Node := TJsonNode.Create;
  node.Parse(Message);
  if Node.Find('type').Value = '"begin"'   then
    begin
      CurrObj := TFoundFile.Create;
      CurrObj.FileName:=Node.Find('data/path/text').AsString;
      FoundFiles.Add(CurrObj);
      lvFiles.AddItem(ExtractFileName(CurrObj.FileName), CurrObj);
    end;
  if Node.Find('type').Value = '"match"'   then
    begin
      Line := TFoundItem.Create;
      line.Line:= node.find('data/lines/text').AsString;
      line.Row:= trunc(node.Find('data/line_number').AsNumber);
      Line.Colum:= trunc(Node.Find('data/submatches').Child(0).find('start').AsNumber);
      CurrObj.Lines.Add(line);
    end;
  if Node.Find('type').Value = '"end"'   then
    begin
    end;

end;

procedure TForm1.bSearchClick(Sender: TObject);
var
  pr: TProcessThread;
begin

  FoundFiles.Clear;
  pr := TProcessThread.Create;
  pr.FreeOnTerminate:=true;
  pr.Process.Executable:= '/usr/bin/rg';
//  pr.Parameters.add('--trace');
//  pr.Process.Parameters.Add('--color');  pr.Process.Parameters.add('never');
//  pr.Process.Parameters.add('--column');
//  pr.Process.Parameters.add('--line-buffered');
  pr.Process.Parameters.add('-n');
  pr.Process.Parameters.add('-i');
  pr.Process.Parameters.add('--json');
  pr.Process.Parameters.add('-e');  pr.Process.Parameters.add(lbContaining.Text);
  pr.Process.Parameters.add('-g');  pr.Process.Parameters.add(lbFiles.Text);
  pr.Process.Parameters.add(lbPath.Text);

  pr.OnRunnerMessage:=@GotMessage;
  pr.Start;

  //  /usr/bin/rg --color never --column -n -i -e sccsILEdtAddMoreResolutions -g "*.{pp,pas,inc}" /home/varianus/development/lazarus/

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FoundFiles := TFoundFiles.Create;
  Node:= TJsonNode.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Node.Free;
  FoundFiles.Free;
end;

procedure TForm1.lvFilesData(Sender: TObject; Item: TListItem);
var
  obj : TFoundFile;
begin
  obj := TFoundFile(item.Data);
end;

procedure TForm1.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  obj : TFoundFile;
  match: TFoundItem;
begin
  if not selected then exit;
  obj := TFoundFile(item.Data);
  memo1.lines.clear;
  memo1.Lines.Add(obj.FileName);
  for Match in obj.Lines do
    begin
      Memo1.Lines.Add(format('%5.5d: %s',[match.Row,match.Line]));
    end;


end;

end.

