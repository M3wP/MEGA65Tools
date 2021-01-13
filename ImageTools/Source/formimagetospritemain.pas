unit FormImageToSpriteMain;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls, ActnList, ImageToSpriteTypes;

type

    { TForm1 }

    TForm1 = class(TForm)
        ActOutputPalette: TAction;
        ActionList1: TActionList;
        ActOutputSave: TAction;
        ActSourceClear: TAction;
        ActSourceOpen: TAction;
        ActSourceProcess: TAction;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Edit1: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        OpenDialog1: TOpenDialog;
        ProgressBar1: TProgressBar;
        SaveDialog1: TSaveDialog;
        procedure ActOutputPaletteExecute(Sender: TObject);
        procedure ActOutputSaveExecute(Sender: TObject);
        procedure ActSourceOpenExecute(Sender: TObject);
        procedure ActSourceProcessExecute(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        FPicture: TPicture;
        FBitmap: TBitmap;
        FSpriteHeight: Integer;
        FSpriteSize: Integer;
        FSprites: TSpriteList;
        FPaletteMap: TPaletteMap;

        procedure DoProcessTileData(AData: TTileRGBData; ACX, ACY: Integer;
		        var ACurrIndex: Integer);
   public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

uses
    Math, GraphType;


{ TForm1 }

procedure TForm1.ActSourceOpenExecute(Sender: TObject);
    begin
  	if  OpenDialog1.Execute then
  		begin
  		FPicture.LoadFromFile(OpenDialog1.FileName);

  		ActSourceOpen.Enabled:= False;
  		ActSourceClear.Enabled:= True;
  		ActSourceProcess.Enabled:= True;
  		end;
    end;

procedure TForm1.ActOutputSaveExecute(Sender: TObject);
    var
	f: TFileStream;
    actSize: Integer;

	procedure DoWriteHeader;
		var
		header: TM65SpriteHeader;

		begin
		FillChar(header, SizeOf(TM65SpriteHeader), 0);

//		header.Magic:= LIT_TOK_V4TLIMG_MAGICI;

        header.SpriteHeight:= FSpriteHeight;
        header.SpriteCount:= FSprites.Count;
        header.DataSize:= actSize div 2;

		f.Write(header, SizeOf(TM65SpriteHeader));
		end;

	function  DoSwapNybbles(AData: Byte): Byte;
		begin
		Result:= (AData and $F0) shr 4;
		Result:= Result or ((AData and $0F) shl 4);
		end;

	procedure DoWriteColourR(AColour: TRGBPixel);
		var
		b: Byte;

		begin
		AColour.R:= AColour.R and $FE;
		b:= DoSwapNybbles(AColour.R);
		f.Write(b, 1);
		end;

	procedure DoWriteColourG(AColour: TRGBPixel);
		var
		b: Byte;

		begin
		b:= DoSwapNybbles(AColour.G);
		f.Write(b, 1);
		end;

	procedure DoWriteColourB(AColour: TRGBPixel);
		var
		b: Byte;

		begin
		b:= DoSwapNybbles(AColour.B);
		f.Write(b, 1);
		end;

	procedure DoWritePalette;
		var
		i: Integer;

		begin
		for i:= 0 to High(FPaletteMap) do
			DoWriteColourR(FPaletteMap[i].RGB);
		for i:= 0 to High(FPaletteMap) do
			DoWriteColourG(FPaletteMap[i].RGB);
		for i:= 0 to High(FPaletteMap) do
			DoWriteColourB(FPaletteMap[i].RGB);
		end;

    procedure DoWriteTiles;
        var
        i,
        j: Integer;
        b: Byte;

        begin
        for i:= 0 to FSprites.Count - 1 do
            begin
            j:= 0;
            while j < Length(FSprites[i].PalData) - 1 do
                begin
                b:= (FSprites[i].PalData[j] shl 4) or FSprites[i].PalData[j + 1];
                f.Write(b, 1);

                Inc(j, 2);
                end;

            j:= Length(FSprites[i].PalData);
            while j < actSize - 1 do
                begin
                b:= 0;
                f.Write(b, 1);
                Inc(j, 2);
                end;
            end;
        end;

    begin
	if  SaveDialog1.Execute then
		begin
        actSize:= Ceil(FSpriteSize / 64) * 64;

    	f:= TFileStream.Create(SaveDialog1.FileName, fmCreate);
	    try
		    DoWriteHeader;
		    DoWritePalette;
		    DoWriteTiles;

		    finally
		    f.Free;
		    end;
        end;
    end;

procedure TForm1.ActOutputPaletteExecute(Sender: TObject);
	begin
//
	end;

procedure TForm1.ActSourceProcessExecute(Sender: TObject);
    var
    i,
    j,
    k,
    x,
    y: Integer;
    l: PRGBPixelArr;
    cw,
    ch,
    ow,
    oh: Integer;
    d: TTileRGBData;
    n: Integer;
{$IFDEF FPC}
    h: PRGBPixel;
    PixelPtr: PInteger;
    PixelRowPtr: PInteger;
    RawImage: TRawImage;
    BytePerPixel: Integer;
{$ENDIF}

    begin
    ActOutputSave.Enabled:= True;

    FSpriteHeight:= StrToInt(Edit1.Text);
    FSpriteSize:= FSpriteHeight * VAL_SIZ_SPRTPIX_WIDTH;

    SetLength(d, FSpriteSize * 3);

    FBitmap:= TBitmap.Create;
    FBitmap.PixelFormat:= pf24bit;
    FBitmap.Width:= FPicture.Width;
    FBitmap.Height:= FPicture.Height;
    FBitmap.Canvas.Draw(0, 0, FPicture.Graphic);

    cw:= FBitmap.Width div VAL_SIZ_SPRTPIX_WIDTH;
    ch:= FBitmap.Height div FSpriteHeight;

	for i:= 0 to ch - 1 do
		begin
		oh:= i * FSpriteHeight;

		for j:= 0 to cw - 1 do
			begin
			ow:= j * VAL_SIZ_SPRTPIX_WIDTH;

			k:= 0;
			for y:= oh to oh + FSpriteHeight - 1 do
				begin
				RawImage := FBitmap.RawImage;
				PixelRowPtr := PInteger(RawImage.Data);
				Inc(PByte(PixelRowPtr), RawImage.Description.BytesPerLine * y);
				BytePerPixel := RawImage.Description.BitsPerPixel div 8;
				PixelPtr := PixelRowPtr;
				Inc(PByte(PixelPtr), BytePerPixel * ow);
				for x:= ow to ow + VAL_SIZ_SPRTPIX_WIDTH - 1 do
					begin
					h:= PRGBPixel(PixelPtr);

					d[k]:= h^.B;
					Inc(k);
					d[k]:= h^.G;
					Inc(k);
					d[k]:= h^.R;
					Inc(k);

					Inc(PByte(PixelPtr), BytePerPixel);
					end;
				end;

			DoProcessTileData(d, j, i, n);
			end;
		end;

    end;

procedure TForm1.FormCreate(Sender: TObject);
    begin
    FPicture:= TPicture.Create;
    FSprites:= TSpriteList.Create;

    end;

procedure TForm1.DoProcessTileData(AData: TTileRGBData; ACX, ACY: Integer;
        var ACurrIndex: Integer);
    procedure DoUpdatePaletteMap(AData: TTileRGBData);
  	    var
      	p: PRGBPixelArr;
  	    i,
      	j: Integer;
  	    f: Boolean;

      	begin
  	    p:= @AData[0];
  	    for i:= 0 to FSpriteSize - 1 do
	    	begin
		    f:= False;

            for j:= 0 to High(FPaletteMap) do
//	    	   	if  p^[i] = FPaletteMap[j].RGB then
  		    	if  (p^[i].R = FPaletteMap[j].RGB.R)
  			    and (p^[i].G = FPaletteMap[j].RGB.G)
      			and (p^[i].B = FPaletteMap[j].RGB.B) then
  	    			begin
  		    		f:= True;
  			   		Break;
  			    	end;

            if  not f then
  	    		begin
  		    	SetLength(FPaletteMap, Length(FPaletteMap) + 1);
  			   	FPaletteMap[High(FPaletteMap)].RGB:= p^[i];
  			    end;
      		end;
  	    end;

    procedure DoConvertPaletteData(AData: TTileRGBData;
            var APalData: TTilePalData);
    	var
    	p: PRGBPixelArr;
    	i,
    	j: Integer;

    	begin
    	p:= @AData[0];
    	for i:= 0 to FSpriteSize - 1 do
   			for j:= 0 to High(FPaletteMap) do
   				if  (p^[i].R = FPaletteMap[j].RGB.R)
   				and (p^[i].G = FPaletteMap[j].RGB.G)
   				and (p^[i].B = FPaletteMap[j].RGB.B) then
   					begin
   					APalData[i]:= j;

   					Break;
   					end;
   			end;

	procedure DoAddTileMapTile(AData: TTileRGBData; var AIndex: Integer);
		var
		t: TSprite;

        begin
		t:= TSprite.Create;

        SetLength(t.RGBData, Length(AData));
        Move(AData[0], t.RGBData[0], Length(AData));

        SetLength(t.PalData, FSpriteSize);
        DoConvertPaletteData(t.RGBData, t.PalData);

        t.Index:= AIndex;

        FSprites.Add(t);
        end;

    begin
	DoUpdatePaletteMap(AData);
	DoAddTileMapTile(AData, ACurrIndex);
    end;

end.

