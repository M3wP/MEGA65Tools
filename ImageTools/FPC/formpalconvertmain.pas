unit FormPalConvertMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TForm1 }

    TForm1 = class(TForm)
        Button1: TButton;
        OpenDialog1: TOpenDialog;
        SaveDialog1: TSaveDialog;
        procedure Button1Click(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

function  DoSwapNybbles(AData: Byte): Byte;
	begin
	Result:= (AData and $F0) shr 4;
	Result:= Result or ((AData and $0F) shl 4);
	end;

{ TForm1 }
procedure TForm1.Button1Click(Sender: TObject);
    var
    fi,
    fo: TFileStream;
    mr: TMemoryStream;
    b: Byte;
    s: AnsiString;

	begin
    if  OpenDialog1.Execute then
    	begin
        fi:= TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
     	mr:= TMemoryStream.Create;
        try
            s:= 'GIMP Palette'#10;
            mr.Write(s[1], Length(s));

            s:= 'Name: ' + AnsiString(ExtractFileName(OpenDialog1.FileName)) +
            		#10;
            mr.Write(s[1], Length(s));

            s:= '#'#10;
            mr.Write(s[1], Length(s));

            while fi.Position < fi.Size do
            	begin
                s:= '';

                fi.Read(b, 1);
                b:= DoSwapNybbles(b);
                s:= s + IntToStr(b) + ' ';

                fi.Read(b, 1);
                b:= DoSwapNybbles(b);
                s:= s + IntToStr(b) + ' ';

                fi.Read(b, 1);
                b:= DoSwapNybbles(b);
                s:= s + IntToStr(b) + ' ';

                s:= s + #10;

                mr.Write(s[1], Length(s));
                end;

            if  SaveDialog1.Execute then
                begin
                fo:= TFileStream.Create(SaveDialog1.Filename, fmCreate);
                try
                    mr.Position:= 0;

                    fo.CopyFrom(mr, mr.Size);

                	finally
                    fo.Free;
                    end;
                end;

        	finally
            mr.Free;
            fi.Free;
            end;
        end;
    end;

end.

