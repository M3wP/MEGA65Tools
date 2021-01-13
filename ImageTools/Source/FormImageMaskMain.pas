unit FormImageMaskMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    ActionList1: TActionList;
    ActSourceOpen: TAction;
    ActSourceClear: TAction;
    ActSourceProcess: TAction;
    ActOutputSave: TAction;
    SaveDialog1: TSaveDialog;
    Label5: TLabel;
    Button5: TButton;
    ActMaskOpen: TAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
