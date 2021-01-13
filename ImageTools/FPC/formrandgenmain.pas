unit FormRandGenMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TForm1 }

    TForm1 = class(TForm)
        Button1: TButton;
        Memo1: TMemo;
        procedure Button1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
    var
    dat: array[0..255] of Byte;
    dst: set of Byte;
    i,
    j: Integer;
    v: Byte;
    s: string;

    begin
    dst:= [];

    for i:= 0 to 255 do
    	begin
        repeat
        	v:= Random(256);

        	until not (v in dst);

        dat[i]:= v;
        Include(dst, v);
        end;

   	Memo1.Lines.Clear;
    for i:= 0 to 15 do
        begin
        s:= #9'.byte' + #9'$' + IntToHex(dat[i * 16 + 0], 2);

        for j:= 1 to 15 do
            s:= s + ', $' + IntToHex(dat[i * 16 + j], 2);

        Memo1.Lines.Add(s);
        end;
    end;

procedure TForm1.FormCreate(Sender: TObject);
	begin
    Randomize;
	end;

end.

