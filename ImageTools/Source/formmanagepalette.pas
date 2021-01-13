unit FormManagePalette;

{$mode delphi}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
    ColorBox, StdCtrls, ImageToTilesTypes, Types;

type

    { TManagePaletteForm }

    TManagePaletteForm = class(TForm)
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        ColorBox1: TColorBox;
        ColorBox2: TColorBox;
        DrawGrid1: TDrawGrid;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure ColorBox1GetColors(Sender: TCustomColorBox; Items: TStrings);
        procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
            aRect: TRect; aState: TGridDrawState);
        procedure DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
            var CanSelect: Boolean);
    private
        FPaletteMap: PPaletteMap;
        FBackground: TRGBPixel;
        FCount: Integer;
        FIndex: Integer;

    public
        procedure ManagePalette(APalette: PPaletteMap; ABackground: TRGBPixel);
    end;

var
    ManagePaletteForm: TManagePaletteForm;

implementation

{$R *.lfm}

{ TManagePaletteForm }

procedure TManagePaletteForm.ColorBox1GetColors(Sender: TCustomColorBox;
        Items: TStrings);
    var
    i: Integer;
    c: TColourRec;

    begin
    for i:= 0 to High(FPaletteMap^) do
        begin
        c.A:= 0;
        c.R:= FPaletteMap^[i].RGB.R;
        c.G:= FPaletteMap^[i].RGB.G;
        c.B:= FPaletteMap^[i].RGB.B;

        Items.AddObject(Format('%3.3d', [i + 1]), TObject(PtrInt(c.C)));
        end;
    end;

procedure TManagePaletteForm.Button1Click(Sender: TObject);
    var
    t: TPaletteEntry;
    b: Boolean;

    begin
    if  ColorBox1.ItemIndex = -1 then
        Exit;

    if  ColorBox1.ItemIndex = FIndex - 1 then
        Exit;

    if  FIndex = 0 then
        Exit;

    t:= FPaletteMap^[FIndex - 1];
    FPaletteMap^[FIndex - 1]:= FPaletteMap^[ColorBox1.ItemIndex];
    FPaletteMap^[ColorBox1.ItemIndex]:= t;

    ColorBox1.Style:= [];
    ColorBox2.Style:= [];

    ColorBox1.Style:= [cbCustomColors];
    ColorBox2.Style:= [cbCustomColors];

    DrawGrid1.Invalidate;
    DrawGrid1SelectCell(Sender, DrawGrid1.Col, DrawGrid1.Row, b);
    end;

procedure TManagePaletteForm.Button2Click(Sender: TObject);
    var
    i,
    j: Integer;
    t: TPaletteEntry;
    b: Boolean;

    begin
    if  ColorBox2.ItemIndex = -1 then
        Exit;

    i:= FIndex - 1;

    if  ColorBox2.ItemIndex = i then
        Exit;

    if  FIndex = 0 then
        Exit;

    if  ColorBox2.ItemIndex > i then
        begin
        t:= FPaletteMap^[i];
        for j:= i + 1 to ColorBox2.ItemIndex do
            FPaletteMap^[j - 1]:= FPaletteMap^[j];
        FPaletteMap^[ColorBox2.ItemIndex]:= t;
        end
    else
        begin
        t:= FPaletteMap^[i];
        for j:= i downto ColorBox2.ItemIndex + 1 do
            FPaletteMap^[j]:= FPaletteMap^[j - 1];
        FPaletteMap^[ColorBox2.ItemIndex]:= t;
        end;

    ColorBox1.Style:= [];
    ColorBox2.Style:= [];

    ColorBox1.Style:= [cbCustomColors];
    ColorBox2.Style:= [cbCustomColors];

    DrawGrid1.Invalidate;
    DrawGrid1SelectCell(Sender, DrawGrid1.Col, DrawGrid1.Row, b);
    end;

procedure TManagePaletteForm.Button3Click(Sender: TObject);
    begin
    Close;
    end;

procedure TManagePaletteForm.Button4Click(Sender: TObject);
    var
    s: string;
    c1,
    c2: Char;
    f: Boolean;
    v: TRGBPixel;
    r,
    c,
    i: Integer;

    begin
    s:= InputBox('Enter Web Colour String...', 'Colour:', '');
    s:= UpperCase(s);

    f:= False;
    if  Length(s) = 6 then
        begin
        f:= True;
        i:= Low(string);

        c1:= s[i];
        Inc(i);
        c2:= s[i];
        Inc(i);
        v.R:= 0;
        if c1 in ['0'..'9'] then
        	v.R:= (Ord(c1) - Ord('0')) shl 8
        else if c1 in ['A'..'Z'] then
        	v.R:= (Ord(c1) - Ord('A')) shl 8
        else
           	f:= False;
        if c2 in ['0'..'9'] then
        	v.R:= v.R or (Ord(c2) - Ord('0'))
        else if c2 in ['A'..'Z'] then
        	v.R:= v.R or (Ord(c2) - Ord('A'))
        else
           	f:= False;

        c1:= s[i];
        Inc(i);
        c2:= s[i];
        Inc(i);
        v.G:= 0;
        if c1 in ['0'..'9'] then
        	v.G:= (Ord(c1) - Ord('0')) shl 8
        else if c1 in ['A'..'Z'] then
        	v.G:= (Ord(c1) - Ord('A')) shl 8
        else
           	f:= False;
        if c2 in ['0'..'9'] then
        	v.G:= v.R or (Ord(c2) - Ord('0'))
        else if c2 in ['A'..'Z'] then
        	v.G:= v.R or (Ord(c2) - Ord('A'))
        else
           	f:= False;

                c1:= s[i];
        Inc(i);
        c2:= s[i];
        Inc(i);
        v.B:= 0;
        if c1 in ['0'..'9'] then
        	v.B:= (Ord(c1) - Ord('0')) shl 8
        else if c1 in ['A'..'Z'] then
        	v.B:= (Ord(c1) - Ord('A')) shl 8
        else
           	f:= False;
        if c2 in ['0'..'9'] then
        	v.B:= v.R or (Ord(c2) - Ord('0'))
        else if c2 in ['A'..'Z'] then
        	v.B:= v.R or (Ord(c2) - Ord('A'))
        else
           	f:= False;

        if  f then
            begin
            for i:= 0 to High(FPaletteMap^) do
                if  (v.R = FPaletteMap^[i].RGB.R)
                and (v.G = FPaletteMap^[i].RGB.G)
                and (v.B = FPaletteMap^[i].RGB.B) then
                    begin
                    r:= i div 16;
                    c:= i - (r * 16);

                    ShowMessage('Row: ' + IntToStr(r) + ', Col: ' + IntToStr(c));

//                  DrawGrid1.Selection.Top:= r;
//                  DrawGrid1.Selection.Left:= c;

                    Break;
                    end;
            end;
        end;
    end;

procedure TManagePaletteForm.DrawGrid1DrawCell(Sender: TObject; ACol,
        ARow: Integer; ARect: TRect; AState: TGridDrawState);
    var
    i: Integer;
    p: TRGBPixel;
    c: TColourRec;

    begin
    if  ARow > 0 then
        if ACol > 0 then
            begin
            i:= (ARow - 1) * 16 + (ACol - 1);

            if  i >= FCount then
                Exit;

            if  i = 0 then
                p:= FBackground
            else
                begin
                Dec(i);
                p:= FPaletteMap^[i].RGB;
                end;

            if  gdSelected in AState then
                begin
                c.A:= 0;
                c.R:= FBackground.R;
                c.G:= FBackground.G;
                c.B:= FBackground.B;

                DrawGrid1.Canvas.Brush.Color:= c.C;
                DrawGrid1.Canvas.Brush.Style:= bsSolid;
                DrawGrid1.Canvas.FillRect(ARect);

                InflateRect(ARect, -2, -2);
                end;

            c.A:= 0;
            c.R:= p.R;
            c.G:= p.G;
            c.B:= p.B;

            DrawGrid1.Canvas.Brush.Color:= c.C;
            DrawGrid1.Canvas.Brush.Style:= bsSolid;
            DrawGrid1.Canvas.FillRect(ARect);
            end;
    end;

procedure TManagePaletteForm.DrawGrid1SelectCell(Sender: TObject; ACol,
        ARow: Integer; var CanSelect: Boolean);
    var
    i: Integer;
    p: TRGBPixel;
    c: Integer;

    begin
    i:= (ARow - 1) * 16 + (ACol - 1);

    if  i >= FCount then
        begin
        CanSelect:= False;
        Exit;
        end;

    if  i = 0 then
        begin
        p:= FBackground;
        c:= -1;
        end
    else
        begin
        p:= FPaletteMap^[i - 1].RGB;
        c:= FPaletteMap^[i - 1].Count;
        end;

    if  c = -1 then
        Label11.Caption:= '----'
    else
        Label11.Caption:= IntToStr(c);

    Label8.Caption:= '$' + IntToHex(p.R, 2);
    Label9.Caption:= '$' + IntToHex(p.G, 2);
    Label10.Caption:= '$' + IntToHex(p.B, 2);

    FIndex:= i;

    if  FIndex = 0 then
        begin
        Button1.Enabled:= False;
        Button2.Enabled:= False;
        end
    else
        begin
        Button1.Enabled:= True;
        Button2.Enabled:= True;
        end;
    end;

procedure TManagePaletteForm.ManagePalette(APalette: PPaletteMap;
        ABackground: TRGBPixel);
    begin
    FPaletteMap:= APalette;
    FBackground:= ABackground;

    FCount:= Length(FPaletteMap^) + 1;

    ColorBox1.Style:= [cbCustomColors];
    ColorBox2.Style:= [cbCustomColors];

    ShowModal;

    ColorBox1.Style:= [];
    ColorBox2.Style:= [];
    end;

end.

