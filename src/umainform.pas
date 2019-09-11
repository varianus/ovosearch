unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Grids, searchresult, ProcessThread, JsonTools,
  FilesFunctions, Generics.Collections, Types, LazLoggerBase, laz.VirtualTrees;

type

  { TfMainForm }
  TSearchState = (ssFile, ssLine, ssNone);

  TfMainForm = class(TForm)
    bSearch: TButton;
    bStop: TButton;
    grdDetails: TDrawGrid;
    vtvResults: TLazVirtualStringTree;
    lbContaining: TLabeledEdit;
    lbFiles: TLabeledEdit;
    lbPath: TLabeledEdit;
    lvFiles: TListView;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    tmrParseResult: TTimer;
    procedure bSearchClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure grdDetailsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtvResultsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtvResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure lvFilesData(Sender: TObject; Item: TListItem);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tmrParseResultStartTimer(Sender: TObject);
    procedure tmrParseResultStopTimer(Sender: TObject);
    procedure tmrParseResultTimer(Sender: TObject);
  private
    FoundFiles: TFoundFiles;
    CurrObj : TFoundFile;
    pr: TProcessThread;
    MessageQueue: specialize TThreadQueue<string>;
    procedure GotMessage(Sender: TObject;
      const MessageKind: TMessageLineKind; const Message: string);
    procedure ParseMessages;
    Procedure RenderLine(aCanvas:TCanvas; aRect:TRect; Obj: TFoundLine);
    procedure StopTimer(Sender: TObject);
    procedure UpdateForm;
  public

  end;

  PNodeData = ^TNodeData;
  TNodeData = record
    Data: pointer;
  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.lfm}

{ TfMainForm }

Procedure TfMainForm.UpdateForm;
begin

end;

procedure TfMainForm.GotMessage(Sender: TObject;
  const MessageKind: TMessageLineKind; const Message: string);
begin
  if (MessageKind <> ieinfo) or (message = '') then
    exit;
  MessageQueue.Enqueue(Message);
end;

procedure TfMainForm.ParseMessages;
var
  Line: TFoundLine;
  Node: TJsonNode;
  TreeNode : PVirtualNode;
  tmpData: PNodeData;
  tmpNode: TJsonNode;
  Message: string;
//  Item:TListItem;
  i, J: integer;

begin
  while MessageQueue.Count > 0 do
    begin
      Message := MessageQueue.Dequeue;
      Node := TJsonNode.Create;
      node.Parse(Message);
      if Node.Find('type').Value = '"begin"'   then
        begin
          CurrObj := TFoundFile.Create;
          CurrObj.FileName:=Node.Find('data/path/text').AsString;
          FoundFiles.Add(CurrObj);
          CurrObj.FileInfo := GetFileInfo(CurrObj.fileName);
    //      item := lvFiles.Items.Add;
    //      item.Data := CurrObj;
         TreeNode := vtvResults.AddChild(nil);
         tmpData := vtvResults.GetNodeData(TreeNode);
         tmpData^.Data := CurrObj;

        end;
      if Node.Find('type').Value = '"match"'   then
        begin
          Line := TFoundLine.Create;
          line.Line:= node.find('data/lines/text').AsString;
          line.Row:= trunc(node.Find('data/line_number').AsNumber);
          CurrObj.FoundLines.Add(line);
          j := Node.Find('data/submatches').Count;
          SetLength(line.SubMatches, j);
          for i:= 0 to j -1 do
            begin
              tmpNode := Node.Find('data/submatches').Child(i);
              Line.SubMatches[i].Start := trunc(tmpnode.find('start').AsInteger);
              Line.SubMatches[i]._End := trunc(tmpnode.find('end').AsInteger);
            end;

        end;
      if Node.Find('type').Value = '"end"'   then
        begin
          CurrObj.MatchedLine := node.Find('data/stats/matched_lines').AsInteger;
          CurrObj.Matches := node.Find('data/stats/matches').AsInteger;
        end;

      if Node.Find('type').Value = '"summary"'   then
        begin
        end;

      Node.free;
    end;
end;

procedure TfMainForm.StopTimer(Sender: TObject);
begin
  tmrParseResult.Enabled := false;
end;


function ReplaceLineEndings(const s:string; NewLineEnds: AnsiChar): string;
var
  p: Integer;
  r: integer;
  Max:integer;
begin
  Max:= Length(S);
  SetLength(Result, Max);

  p:=1;
  r:=1;
  while (p<=Max) do
    begin
      if S[p] in [#10,#13] then
        begin
          Result[r]:=NewLineEnds;
          inc(r);
          inc(p);
          While (p<=Max) and (s[P] in [#10,#13])  do
            inc(p);
        end
    else
      begin
        Result[r]:=S[p];
        Inc(r);
        Inc(p);
      end;
    end;
  SetLength(Result, R-1);
end;


procedure TfMainForm.bSearchClick(Sender: TObject);
begin
  lvFiles.Clear;
  FoundFiles.Clear;

  pr := TProcessThread.Create;
  pr.FreeOnTerminate:=true;
//  pr.Process.Executable:= '/usr/bin/rg';

  pr.Process.Executable:= 'C:\-\rg\rg.exe';

//  pr.Parameters.add('--trace');
//  pr.Process.Parameters.Add('--color');  pr.Process.Parameters.add('never');
//  pr.Process.Parameters.add('--column');

  pr.Process.Parameters.add('--line-buffered');
  pr.Process.Parameters.add('-n');
  pr.Process.Parameters.add('-i');
  pr.Process.Parameters.add('--json');

  pr.Process.Parameters.add('-e');  pr.Process.Parameters.add(lbContaining.Text);
  pr.Process.Parameters.add('-g');  pr.Process.Parameters.add(lbFiles.Text);
  pr.Process.Parameters.add(lbPath.Text);
  debugLn(pr.Process.Executable + ' '+ ReplaceLineEndings(pr.Process.Parameters.text, ' '));
  pr.OnTerminate := @StopTimer;

  pr.OnMessageLine:=@GotMessage;
  tmrParseResult.Enabled := true;
  pr.Start;

  //  /usr/bin/rg --color never --column -n -i -e sccsILEdtAddMoreResolutions -g "*.{pp,pas,inc}" /home/varianus/development/lazarus/

end;

procedure TfMainForm.bStopClick(Sender: TObject);
begin
  if pr.Process.Running then
    pr.Terminate;
end;

Procedure TfMainForm.RenderLine(aCanvas:TCanvas; aRect:TRect; Obj: TFoundLine);
var
 i: integer;
 StartPos: integer;
 x: integer;
 aText:string;
begin
  StartPos := 1;
  x:= aRect.Left;
  for i := 0 to Length(Obj.SubMatches) -1 do
    begin
      aText := Copy(Obj.Line,StartPos, Obj.SubMatches[i].Start+1 - StartPos);
      aCanvas.Font.Color := clWindowText;
      aCanvas.brush.Color := clWindow;
      aCanvas.TextOut(x, aRect.Top, aText);
      inc(x, aCanvas.GetTextWidth(atext));
      aText := Copy(obj.Line, Obj.SubMatches[i].Start +1, Obj.SubMatches[i]._end - Obj.SubMatches[i].Start);
      aCanvas.Font.Color := clHighlightText;
      aCanvas.brush.Color := clHighlight;
      aCanvas.TextOut(x, aRect.Top, aText);
      inc(x, aCanvas.GetTextWidth(atext)+2);
      StartPos := Obj.SubMatches[i]._end;
    end;
  if StartPos < Length(Obj.Line) then
    begin
      aText := copy (obj.line, StartPos,Length(Obj.Line) -StartPos);
      aCanvas.Font.Color := clWindowText;
      aCanvas.brush.Color := clWindow;
      aCanvas.TextOut(x, aRect.Top, aText);
      inc(x, aCanvas.GetTextWidth(atext));
    end;

end;


procedure TfMainForm.grdDetailsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Match: TFoundLine;
  ts: TTextStyle;
begin
  if not Assigned(CurrObj) then exit;
  if aRow >= CurrObj.FoundLines.Count then exit;

  Match := CurrObj.FoundLines[aRow];
  grdDetails.Canvas.FillRect(arect);

  InflateRect(aRect,-3,-3);
  ts := Default(TTextStyle);
  case aCol of
    0 : begin
          ts := grdDetails.Canvas.TextStyle;
          ts.Alignment := taRightJustify;
          grdDetails.Canvas.TextStyle := ts;
          grdDetails.Canvas.TextOut(aRect.Left, arect.Top, IntToStr(Match.Row));
        end;
    1 : begin
          ts := grdDetails.Canvas.TextStyle;
          ts.Alignment := taLeftJustify;
          grdDetails.Canvas.TextStyle := ts;
          RenderLine(grdDetails.Canvas, Arect, Match);
        end;
  end;



end;

procedure TfMainForm.FormCreate(Sender: TObject);
begin
  FoundFiles := TFoundFiles.Create;
  MessageQueue := specialize TThreadQueue<string>.Create;
  vtvResults.NodeDataSize := SizeOf(TNodeData);
end;

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
  FoundFiles.Free;
  tmrParseResult.Enabled := false;
  MessageQueue.free;
end;

procedure TfMainForm.vtvResultsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if not assigned(Node) then exit;
  CurrObj := FoundFiles[Node^.Index]; // TFoundFile(PNodeData(vtvResults.GetNodeData(Node))^.Data);

  //Currobj := TFoundFile(item.Data);
  grdDetails.RowCount:=CurrObj.FoundLines.Count;
  grdDetails.invalidate;
end;

procedure TfMainForm.vtvResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
 Data: TFoundFile;
begin
  Data := FoundFiles[Node^.Index]; // TFoundFile(PNodeData(vtvResults.GetNodeData(Node))^.Data);
  case column of
    0: CellText:=ExtractFileName(data.FileName);
    1: CellText:=(IntToStr(data.Matches));
    2: CellText:=(ExtractFilePath(Data.FileName));
    3: CellText:=(strByteSize(Data.FileInfo.Size));
    4: CellText:=(DateTimeToStr(Data.FileInfo.ModifyDate));

  end;

end;

procedure TfMainForm.lvFilesData(Sender: TObject; Item: TListItem);
var
  Data: TFoundFile;
begin
  Data := TFoundFile(Item.Data);
  Item.Caption:=ExtractFileName(data.FileName);
  item.SubItems.Add(IntToStr(data.Matches));
  item.SubItems.Add(ExtractFilePath(Data.FileName));
  item.SubItems.Add(strByteSize(Data.FileInfo.Size));
  item.SubItems.Add(DateTimeToStr(Data.FileInfo.ModifyDate));

end;

procedure TfMainForm.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);

begin
  if not selected then exit;
  Currobj := TFoundFile(item.Data);
  grdDetails.RowCount:=CurrObj.FoundLines.Count;
  grdDetails.invalidate;

end;

procedure TfMainForm.tmrParseResultStartTimer(Sender: TObject);
begin
  ProgressBar1.Visible := true;
  ProgressBar1.Style := pbstMarquee;
end;

procedure TfMainForm.tmrParseResultStopTimer(Sender: TObject);
begin
  ProgressBar1.Visible := False;
  ProgressBar1.Style := pbstNormal;

end;

procedure TfMainForm.tmrParseResultTimer(Sender: TObject);
begin
  tmrParseResult.Enabled := false;
  ParseMessages;
  tmrParseResult.Enabled := true;


end;

end.

