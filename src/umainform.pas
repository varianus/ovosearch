unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Grids, searchresult, ProcessThread, JsonTools, LCLType, Buttons,
  FilesFunctions, Config, Generics.Collections, base64, Types, LazLoggerBase, laz.VirtualTrees,
  LCLIntf,LazUTF8;

type

  { TfMainForm }
  TSearchState = (ssFile, ssLine, ssNone);

  { TComboBox }

  TfMainForm = class(TForm)
    bSearch: TButton;
    bStop: TButton;
    cbFileNames: TComboBox;
    cbContent: TComboBox;
    cbHidden: TCheckBox;
    cbBinary: TCheckBox;
    cbRecursive: TCheckBox;
    grdDetails: TDrawGrid;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbContaining: TComboBox;
    lbFiles: TComboBox;
    lbPath: TComboBox;
    memoLog: TMemo;
    PageControl1: TPageControl;
    pcDetails: TPageControl;
    rbCaseSensitiveText: TSpeedButton;
    rbCaseSensitiveFile: TSpeedButton;
    SelectDirectory: TSelectDirectoryDialog;
    bSelectPath: TSpeedButton;
    tsSearch: TTabSheet;
    tsAdvanced: TTabSheet;
    tsDetails: TTabSheet;
    tsLog: TTabSheet;
    vtvResults: TLazVirtualStringTree;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    tmrParseResult: TTimer;

    procedure bSearchClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure grdDetailsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bSelectPathClick(Sender: TObject);
    procedure vtvResultsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtvResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure tmrParseResultStartTimer(Sender: TObject);
    procedure tmrParseResultStopTimer(Sender: TObject);
    procedure tmrParseResultTimer(Sender: TObject);
    procedure vtvResultsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo
      );
    procedure vtvResultsNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    FoundFiles: TFoundFiles;
    CurrObj : TFoundFile;
    pr: TProcessThread;
    RipGrepExecutable: String;
    MessageQueue: specialize TThreadQueue<string>;
    procedure GotMessage(Sender: TObject;
      const MessageKind: TMessageLineKind; const Message: string);
    procedure ParseMessages;
    procedure ProcessFileNames;
    Procedure RenderLine(aCanvas:TCanvas; aRect:TRect; Obj: TFoundLine);
    procedure StopTimer(Sender: TObject);
    procedure UpdateForm;
  public

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
  if (message = '') then
    exit;
  if (MessageKind <> ieinfo) then
    begin
      memoLog.Lines.Add(Message);
      exit;
    end;
  MessageQueue.Enqueue(Message);
end;

procedure TfMainForm.ParseMessages;
var
  Line: TFoundLine;
  Node: TJsonNode;
  tmpNode: TJsonNode;
  Message: string;
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
          tmpNode := Node.Find('data/path/text');
          if Assigned(tmpNode) then
            CurrObj.FullName:= tmpNode.AsString
          else
            begin
              tmpNode := Node.Find('data/path/bytes');
              if Assigned(tmpNode) then
                begin
                 CurrObj.FullName:= StrippedOfNonAscii(DecodeStringBase64(tmpNode.AsString));
                end;
            end;
          FoundFiles.Add(CurrObj);
          CurrObj.FileInfo := GetFileInfo(CurrObj.FullName);
          vtvResults.AddChild(nil);
        end;

      if Node.Find('type').Value = '"match"'   then
        begin
          Line := TFoundLine.Create;
          tmpnode := node.find('data/lines/text');
          if Assigned(tmpNode) then
             line.Line:=tmpNode.AsString
          else
            begin
              tmpNode := Node.Find('data/lines/bytes');
              if Assigned(tmpNode) then
                begin
                 line.Line:=StrippedOfNonAscii(DecodeStringBase64(tmpNode.AsString));
                end;

            end;

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

Procedure TfMainForm.ProcessFileNames;
var
  List: TStringList;
  i: integer;
  Glob : string;
begin
  if rbCaseSensitiveFile.Down then
    Glob := '--glob'
  else
    Glob := '--iglob';

  if cbFileNames.ItemIndex = 1 then
    begin
      pr.Process.Parameters.add(Glob);  pr.Process.Parameters.add(lbFiles.Text);
    end
  else
  if cbFileNames.ItemIndex = 0 then
    begin
      List := TStringList.Create;
      List.StrictDelimiter := true;
      List.Delimiter := ';';
      List.DelimitedText := lbFiles.Text;
      for i := 0 to list.Count -1 do
        begin
          pr.Process.Parameters.add(Glob);  pr.Process.Parameters.add(List[i]);
        end;
      List.free;
    end;

end;

procedure TfMainForm.bSearchClick(Sender: TObject);
begin
  lbFiles.AddHistoryItem(lbFiles.Text, 10, true,true);
  lbPath.AddHistoryItem(lbPath.Text,10, true,true);
  lbContaining.AddHistoryItem(lbContaining.Text,10, true,true);

  vtvResults.Clear;
  memoLog.Clear;
  FoundFiles.Clear;
  MessageQueue.Clear;

  pr := TProcessThread.Create;
  pr.FreeOnTerminate:=true;

  if trim(RipGrepExecutable) = EmptyStr then
     begin
       // try some default
       // message to user
       // Open config
     end;
  pr.Process.Executable:= RipGrepExecutable;

  pr.Process.Parameters.add('--line-buffered');
  pr.Process.Parameters.add('-n');

  if not cbRecursive.Checked then
     begin
       pr.Process.Parameters.add('--max-depth');  pr.Process.Parameters.add('1');
     end;

  if not rbCaseSensitiveText.Down then
    pr.Process.Parameters.add('-i');

  pr.Process.Parameters.add('--json');

  if cbContent.ItemIndex = 0 then
    pr.Process.Parameters.add('-F'); // search as literal string

  if cbHidden.Checked then
    pr.Process.Parameters.add('--hidden');

  if cbBinary.Checked then
    begin
      pr.Process.Parameters.add('--text');
    //  pr.Process.Parameters.add('--null-data');
    end;

//  pr.Process.Parameters.add('--no-ignore');   pr.Process.Parameters.add('--no-ignore-global');

  ProcessFileNames;

  pr.Process.Parameters.add('-e');  pr.Process.Parameters.add(lbContaining.Text);
  pr.Process.Parameters.add(lbPath.Text);

  debugLn(pr.Process.Executable + ' '+ ReplaceLineEndings(pr.Process.Parameters.text, ' '));

  pr.OnTerminate := @StopTimer;
  pr.OnMessageLine:=@GotMessage;
  tmrParseResult.Enabled := true;
  pr.Start;


end;

procedure TfMainForm.bStopClick(Sender: TObject);
begin
  if assigned(pr) and pr.Process.Running then
    begin
     pr.Terminate;
     StatusBar1.Panels[0].Text := 'Interrupted by user';
    end;
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
      //inc(x, aCanvas.GetTextWidth(atext));
    end;

end;


procedure TfMainForm.grdDetailsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Match: TFoundLine;
  ts: TTextStyle;
begin
  if not Assigned(CurrObj) then
    exit;

  if aRow >= CurrObj.FoundLines.Count then
    exit;

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
  rbCaseSensitiveText.Down := ConfigObj.ReadBoolean('search/casesensitive_text', false);
  rbCaseSensitiveFile.Down := ConfigObj.ReadBoolean('search/casesensitive_file', false);
  cbRecursive.Checked := ConfigObj.ReadBoolean('search/recursive', true);
  RipGrepExecutable:= ConfigObj.ReadString('ripgrep/executable', '');

  ConfigObj.ReadStrings('history/files', lbFiles.Items);
  lbFiles.ItemIndex:=0;

  ConfigObj.ReadStrings('history/paths', lbPath.Items);
  lbPath.ItemIndex:=0;

  ConfigObj.ReadStrings('history/contents', lbContaining.Items);
  lbContaining.ItemIndex:=0;

  cbFileNames.ItemIndex:= ConfigObj.ReadInteger('search/filenames', {$IFDEF windows}0{$ELSE}1{$ENDIF});
  cbHidden.Checked := ConfigObj.ReadBoolean('search/hidden', False);
  cbBinary.Checked := ConfigObj.ReadBoolean('search/binary', False);

end;

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
  ConfigObj.writeBoolean('search/casesensitive_text', rbCaseSensitiveText.Down);
  ConfigObj.writeBoolean('search/casesensitive_file', rbCaseSensitiveFile.Down);
  ConfigObj.writeBoolean('search/recursive', cbRecursive.Checked );
  ConfigObj.WriteString('ripgrep/executable', RipGrepExecutable);
  FoundFiles.Free;
  tmrParseResult.Enabled := false;
  MessageQueue.free;

  ConfigObj.WriteStrings('history/files', lbFiles.Items);
  ConfigObj.WriteStrings('history/paths', lbPath.Items);
  ConfigObj.WriteStrings('history/contents', lbContaining.Items);

  ConfigObj.WriteInteger('search/filenames', cbFileNames.ItemIndex);
  ConfigObj.writeBoolean('search/hidden', cbHidden.Checked);
  ConfigObj.writeBoolean('search/binary', cbBinary.Checked);
end;

procedure TfMainForm.bSelectPathClick(Sender: TObject);
begin
  SelectDirectory.FileName := lbPath.Text;
  if SelectDirectory.Execute then
    lbPath.Text := SelectDirectory.FileName;
end;

procedure TfMainForm.vtvResultsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if not assigned(Node) then exit;
  CurrObj := FoundFiles[Node^.Index]; // TFoundFile(PNodeData(vtvResults.GetNodeData(Node))^.Data);
  DebugLn('>>'+CurrObj.FoundLines[0].Line+'<<');
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
    0: CellText := data.FileName;
    1: CellText := (IntToStr(data.Matches));
    2: CellText := Data.Path;
    3: CellText := (strByteSize(Data.FileInfo.Size));
    4: CellText := (DateTimeToStr(Data.FileInfo.ModifyDate));
  end;

end;

procedure TfMainForm.tmrParseResultStartTimer(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Searching';
  Application.ProcessMessages;
end;

procedure TfMainForm.tmrParseResultStopTimer(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
  Application.ProcessMessages;

end;

procedure TfMainForm.tmrParseResultTimer(Sender: TObject);
begin
  tmrParseResult.Enabled := false;
  ParseMessages;
  tmrParseResult.Enabled := true;

end;

procedure TfMainForm.vtvResultsHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if TResultField(HitInfo.Column) <> FoundFiles.SortColumn then
    Sender.SortDirection:=sdDescending;

  Sender.SortDirection:= TSortDirection(abs(ord(Sender.SortDirection)-1));
  FoundFiles.SortbyColumn(TResultField(HitInfo.Column), searchresult.TSortDirection(Sender.SortDirection));
  Sender.SortColumn:=HitInfo.Column;

end;

procedure TfMainForm.vtvResultsNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
 Data: TFoundFile;
begin
  Data := FoundFiles[HitInfo.HitNode^.Index];
  OpenDocument(Data.FullName);

end;

end.

