unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, AnchorDockPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    bSearch: TButton;
    lbContaining: TLabeledEdit;
    lbFiles: TLabeledEdit;
    lbPath: TLabeledEdit;
    lvFiles: TListView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

