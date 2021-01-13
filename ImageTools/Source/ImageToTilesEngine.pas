unit ImageToTilesEngine;

{$IFDEF FPC}
	{$MODE Delphi}
	{$POINTERMATH ON}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
	SysUtils, Classes, Graphics, ImageToTilesTypes;
{$ELSE}
	SysUtils, Classes, GraphType, Graphics, ImageToTilesTypes;
{$ENDIF}

type
	TImageToTilesProgress = class
		procedure InitProgress(AMax: Integer); virtual; abstract;
		procedure SetStage(ACaption: string); virtual; abstract;
		procedure SetDetail(ACaption: string); virtual; abstract;
		procedure IncrementProgress; virtual; abstract;
		procedure DrawTileData(AData: TTileRGBData;
				APosition: Integer); virtual; abstract;
	end;

    { TImageToTilesCore }

 TImageToTilesCore = class
	private
		FProgress: TImageToTilesProgress;
        FLastCX,
        FLastCY: Integer;

        procedure DoAddTileMapTile(AData: TTileRGBData; ACX, ACY, AColCount: Integer;
    			var AIndex: Integer);
    	procedure DoSetTileMapTile(ATile: TTile; ACX, ACY: Integer; AFlipX,
    			AFlipY: Boolean; AColour: TPaletteColour);

	protected
		procedure DoConvertPaletteData(AData: TTileRGBData;
				var APalData: TTilePalData);
		procedure DoExtractPaletteInfo(AData: TTileRGBData;
				var AInfo: TTilePaletteInfo);
		procedure DoProcessTileData(AData, AData2: TTileRGBData; ACX, ACY: Integer;
				var ACurrIndex: Integer);
        procedure DoUpdatePaletteMap(AData: TTileRGBData);

        procedure DoWriteScreenMap(AFile: TFileStream);
        procedure DoWriteColourMap(AFile: TFileStream);
        procedure DoWritePalette(AFile: TFileStream);

	public
		Picture: TPicture;
		Bitmap: TBitmap;

        ColCount: Integer;
        RowCount: Integer;

		TileList: TTileList;
		TileDict: TTileDictionary;
		TileMap: TTileMap;
		PaletteMap: TPaletteMap;
		Background: TRGBPixel;
        AllUnique: Boolean;
        NoFlip: Boolean;
        Mode: Byte;
        Vertical: Boolean;
        ExtraLeading: Boolean;
        PaletteBank: Byte;
        PaletteOffset: Byte;

        MemMap: TMemTileSegs;
        BufferPtr: Cardinal;
        ScreenPtr: Cardinal;
        ColourPtr: Cardinal;
        PalettePtr: Cardinal;
        RelocSourcePtr: Cardinal;
        RelocDestPtr: Cardinal;
        RelocSize: Word;

		constructor Create;
		destructor  Destroy; override;

		procedure Clear;
        procedure PerformImportImage;
		procedure PerformProcessImage(AProgress: TImageToTilesProgress);
        procedure PerformMapTiles(AMemorySegs: TMemModuleSegs;
                ABufferSeg, AScreenSeg, AColourSeg, APaletteSeg,
                ARelocSource, ARelocDest, ARelocSize: Integer);
		procedure PerformOutputFile(AFile: string);
		procedure PerformOutputScreen(AFile: string);
		procedure PerformOutputColour(AFile: string);
		procedure PerformOutputPalette(AFile: string);
	end;


procedure HandleCommandLineInvocation;


implementation

uses
    Generics.Defaults;

type
	TCmdLineProgress = class(TImageToTilesProgress)
		procedure InitProgress(AMax: Integer); override;
		procedure SetStage(ACaption: string); override;
		procedure SetDetail(ACaption: string); override;
		procedure IncrementProgress; override;
		procedure DrawTileData(AData: TTileRGBData; APosition: Integer); override;
	end;


procedure HandleCommandLineInvocation;
	var
	i: Integer;
	progress: TCmdLineProgress;
	core: TImageToTilesCore;
	s: AnsiString;
	b: array[0..1] of Char;

	begin
	progress:= TCmdLineProgress.Create;
	core:= TImageToTilesCore.Create;
	try
		if  ParamCount < 2 then
			Exit;

		i:= 1;
		if  ParamStr(1)[1] = '-' then
			begin
			if  ParamCount < 3 then
				Exit;
			if  Length(ParamStr(1)) < 9 then
				Exit;
			if  ParamStr(1)[2] <> 'b' then
				Exit;
			if  ParamStr(1)[3] <> '#' then
				Exit;

			s:= AnsiString(Copy(ParamStr(1), 4, 2));
			HexToBin(PAnsiChar(@s[1]), PAnsiChar(@b[0]), 1);
			core.Background.R:= Byte(b[0]);
			s:= AnsiString(Copy(ParamStr(1), 6, 2));
			HexToBin(PAnsiChar(@s[1]), PAnsiChar(@b[0]), 1);
			core.Background.G:= Byte(b[0]);
			s:= AnsiString(Copy(ParamStr(1), 8, 2));
			HexToBin(PAnsiChar(@s[1]), PAnsiChar(@b[0]), 1);
			core.Background.B:= Byte(b[0]);

			Inc(i);
			end
		else
			begin
			core.Background.R:= 0;
			core.Background.G:= 0;
			core.Background.B:= 0;
			end;

		core.Picture.LoadFromFile(ParamStr(i));
		core.PerformProcessImage(progress);
		core.PerformOutputFile(ParamStr(i + 1));

		finally
		core.Free;
		progress.Free;
		end;
	end;


procedure TImageToTilesCore.Clear;
	var
	i: Integer;
	l: TTileList;
	enum: TTileDictionary.TKeyEnumerator;
	md5: TMD5;

	begin
	if  TileDict.Keys.Count > 0 then
		begin
//dengland Need a cast for the FPC compiler to be happy.
		enum:= TTileDictionary.TKeyEnumerator(TileDict.Keys.GetEnumerator);
		repeat
			md5:= enum.Current;

//dengland  I don't know why we get an error on the first key if we do this.  Worse
//					yet, I don't know why it doesn't fail below on the first key.  Do
//                  we have to MoveNext in order to get the first value??
//			l:= FTiles[md5];
			if  TileDict.TryGetValue(md5, l) then
				begin
				for i:= l.Count - 1 downto 0 do
					begin
					l[i].Free;
					l.Delete(i);
					end;

				l.Free;
				end;
//			else
//				Label3.Caption:= 'Warning!';

//dengland  Why doesn't this fail on the first one if the get line above fails??
			TileDict.Remove(md5);
			until not enum.MoveNext;

		TileDict.Clear;
		end;

	TileList.Clear;

	for i:= 0 to High(TileMap) do
		SetLength(TileMap[i], 0);

	SetLength(TileMap, 0);

	SetLength(PaletteMap, 0);

	if  Assigned(Bitmap) then
		begin
		Bitmap.Free;
		Bitmap:= nil;
		end;
	end;

procedure TImageToTilesCore.PerformImportImage;
	var
	i,
	j,
	k,
	x,
	y: Integer;
{$IFNDEF FPC}
	l: PRGBPixelArr;
{$ENDIF}
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
	Bitmap:= TBitmap.Create;
	Bitmap.PixelFormat:= pf24bit;
	Bitmap.Width:= Picture.Width;
	Bitmap.Height:= Picture.Height;
	Bitmap.Canvas.Draw(0, 0, Picture.Graphic);

	cw:= Bitmap.Width div VAL_SIZ_TILEPIX_LENGTH;
	ch:= Bitmap.Height div VAL_SIZ_TILEPIX_LENGTH;

    ColCount:= cw;
    RowCount:= ch;

	for i:= 0 to ch - 1 do
		begin
		oh:= i * VAL_SIZ_TILEPIX_LENGTH;

		for j:= 0 to cw - 1 do
			begin
			ow:= j * VAL_SIZ_TILEPIX_LENGTH;

			k:= 0;
			for y:= oh to oh + VAL_SIZ_TILEPIX_LENGTH - 1 do
				begin
{$IFNDEF FPC}
				l:= PRGBPixelArr(Bitmap.ScanLine[y]);
				for x:= ow to ow + VAL_SIZ_TILEPIX_LENGTH - 1 do
					begin
					d[k]:= l^[x].B;
					Inc(k);
					d[k]:= l^[x].G;
					Inc(k);
					d[k]:= l^[x].R;
					Inc(k);
					end;
{$ELSE}
				RawImage := Bitmap.RawImage;
				PixelRowPtr := PInteger(RawImage.Data);
				Inc(PByte(PixelRowPtr), RawImage.Description.BytesPerLine * y);
				BytePerPixel := RawImage.Description.BitsPerPixel div 8;
				PixelPtr := PixelRowPtr;
				Inc(PByte(PixelPtr), BytePerPixel * ow);
				for x:= ow to ow + VAL_SIZ_TILEPIX_LENGTH - 1 do
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
{$ENDIF}
				end;

			DoUpdatePaletteMap(d);
			end;
		end;

    SortPaletteMap(PaletteMap);
    end;

constructor TImageToTilesCore.Create;
	begin
	inherited Create;

	Picture:= TPicture.Create;
	TileDict:= TTileDictionary.Create;
	TileList:= TTileList.Create;
	end;

destructor TImageToTilesCore.Destroy;
	begin
	Clear;

	TileList.Free;
	TileDict.Free;
	Picture.Free;

	inherited;
	end;

procedure TImageToTilesCore.DoConvertPaletteData(AData: TTileRGBData;
		var APalData: TTilePalData);
	var
	p: PRGBPixelArr;
	i,
	j: Integer;

	begin
	p:= @AData[0];
	for i:= 0 to VAL_SIZ_TILEPIX_TLPALD - 1 do
		if  not ((p^[i].R = Background.R)
		and (p^[i].G = Background.G)
		and (p^[i].B = Background.B)) then
			begin
			for j:= 0 to High(PaletteMap) do
				if  (p^[i].R = PaletteMap[j].RGB.R)
				and (p^[i].G = PaletteMap[j].RGB.G)
				and (p^[i].B = PaletteMap[j].RGB.B) then
					begin
					APalData[i]:= j;

					Break;
					end;
			end
		else
			APalData[i]:= VAL_CLR_TILEPAL_BACKGD;
	end;

procedure TImageToTilesCore.DoExtractPaletteInfo(AData: TTileRGBData;
		var AInfo: TTilePaletteInfo);
	var
	p: PRGBPixelArr;
	i,
	j,
	k: Integer;

	begin
    if  Mode = 2 then
        begin
        AInfo[0].Colour:= VAL_CLR_TILEPAL_BACKGD;
		AInfo[0].Count:= 1;
        end
    else
		begin
        AInfo[0].Colour:= VAL_CLR_TILEPAL_UNKNWN;
		AInfo[0].Count:= 0;
        end;

    for k:= 1 to High(AInfo) do
		begin
		AInfo[k].Colour:= VAL_CLR_TILEPAL_UNKNWN;
		AInfo[k].Count:= 0;
		end;

	p:= @AData[0];
	for i:= 0 to VAL_SIZ_TILEPIX_TLPALD - 1 do
		if  not ((p^[i].R = Background.R)
		and  (p^[i].G = Background.G)
		and  (p^[i].B = Background.B)) then
			for j:= 0 to High(PaletteMap) do
				if  (p^[i].R = PaletteMap[j].RGB.R)
				and (p^[i].G = PaletteMap[j].RGB.G)
				and (p^[i].B = PaletteMap[j].RGB.B) then
					begin
					for k:= 0 to High(AInfo) do
						if  (AInfo[k].Colour = j)
						or  (AInfo[k].Colour = VAL_CLR_TILEPAL_UNKNWN) then
							begin
							AInfo[k].Colour:= j;
							Inc(AInfo[k].Count);
							Break;
							end;

					Break;
					end;
	end;


procedure TImageToTilesCore.DoSetTileMapTile(ATile: TTile; ACX, ACY: Integer; AFlipX,
		AFlipY: Boolean; AColour: TPaletteColour);
	var
	m: PTileMapCell;

	begin
	m:= @TileMap[ACY, ACX];
	m^.Tile:= ATile;
	m^.FlipX:= AFlipX;
	m^.FlipY:= AFlipY;
	m^.ForeGnd:= AColour;
	end;

procedure TImageToTilesCore.DoAddTileMapTile(AData: TTileRGBData; ACX, ACY, AColCount: Integer;
		var AIndex: Integer);
	var
	l: TTileList;
	t: TTile;
	md5: TMD5;

	begin
//      Going to need a new tile somewhere.
	t:= TTile.Create;
	t.RGBData:= AData;
    t.Index:= AIndex;
	t.Count:= 1;
	t.Foreground:= VAL_CLR_TILEPAL_UNKNWN;
	t.ColCount:= AColCount;

//dengland This ends up being called twice for this data but I can't help it without
//				some awful looking code.
	ComputeTileMD5(AData, md5);

//      Check if we need to start a new list (for MD5 collisions)
	if  not TileDict.TryGetValue(md5, l) then
		begin
		l:= TTileList.Create;
		TileDict.Add(md5, l);
		end;
	l.Add(t);

	TileList.Add(t);

//      Update tile map
	DoSetTileMapTile(t, ACX, ACY, False, False, VAL_CLR_TILEPAL_UNKNWN);

	FProgress.SetDetail('Added index: ' + IntToStr(AIndex));

	Inc(AIndex);
	end;


procedure TImageToTilesCore.DoProcessTileData(AData, AData2: TTileRGBData; ACX,
        ACY: Integer; var ACurrIndex: Integer);
	var
	r: TTileRGBData;
	t: TTile;
	fx,
	fy: Boolean;
	c: TPaletteColour;
	n: Integer;
    e,
    u: Boolean;


    function  DoFindTileInDictionary(AData: TTileRGBData): TTile;
		var
		md5: TMD5;
		i: Integer;
		l: TTileList;

		begin
		Result:= nil;
		ComputeTileMD5(AData, md5);

//      Search for the tile's MD5
		if  TileDict.TryGetValue(md5, l) then
			begin
//          If found, check for existing tile
			for i:= 0 to l.Count - 1 do
				if  CompareMem(@AData[0], @l[i].RGBData[0],
						SizeOf(TTileRGBData)) then
					begin
					Result:= l[i];
					Exit;
					end;
			end;
		end;

	procedure DoGeneratePalSwapData(APalData: TTilePalData; var AData: TTileRGBData;
			AColFrom, AColTo: TPaletteColour);
		var
		i: Integer;
		p: PRGBPixelArr;

		begin
		p:= @AData[0];

		for i:= 0 to High(APalData) do
			if  APalData[i] = VAL_CLR_TILEPAL_BACKGD then
				p[i]:= Background
			else if (APalData[i] = VAL_CLR_TILEPAL_FOREGD)
			or  (APalData[i] = AColFrom) then
				p[i]:= PaletteMap[AColTo].RGB
			else
				p[i]:= PaletteMap[APalData[i]].RGB;
		end;

	function  DoCheckForTileData(AData: TTileRGBData;
			var AForeGnd: TPaletteColour; var AColCount: Integer): TTile;
		var
		i: Integer;
		r: TTileRGBData;
		pi: TTilePaletteInfo;
		pd: TTilePalData;
		f,
		t: TPaletteColour;
        maxPal: Integer;

		begin
        maxPal:= High(PaletteMap);

        if  Mode = 2 then
            maxPal:= -1
        else if Mode = 1 then
            if  maxPal > 14 then
                maxPal:= 14;

		DoExtractPaletteInfo(AData, pi);

		AForeGnd:= VAL_CLR_TILEPAL_UNKNWN;
		AColCount:= High(TTilePaletteInfo);
		for i:= 0 to High(TTilePaletteInfo) do
			if  pi[i].Colour = VAL_CLR_TILEPAL_UNKNWN then
				begin
				AColCount:= i;
				Break;
				end;

//		Debugging
		FProgress.DrawTileData(AData, 1);

		Result:= DoFindTileInDictionary(AData);
		if  Assigned(Result) then
			begin
			FProgress.SetDetail('Found index: ' + IntToStr(Result.Index));
			Inc(Result.Count);
			end
		else
			begin
			DoConvertPaletteData(AData, pd);

			i:= 0;
			while pi[i].Colour >= 0 do
				begin
				f:= pi[i].Colour;
				for t:= 0 to maxPal do
					if  t <> f then
						begin
						DoGeneratePalSwapData(pd, r, f, t);

						Result:= DoFindTileInDictionary(r);
						if  Assigned(Result) then
							begin
							if  Result.ColCount <> AColCount then
								begin
								FProgress.SetDetail('Skipped index: ' +
										IntToStr(Result.Index));
								Result:= nil;
								Continue;
								end;

							if  Result.Foreground <> VAL_CLR_TILEPAL_UNKNWN then
								begin
								if  Result.Foreground <> t then
									begin
									FProgress.SetDetail('Skipped index: ' +
											IntToStr(Result.Index));
									Result:= nil;
									end;
								end
							else
								Result.Foreground:= t;
							end;

						if  Assigned(Result) then
							begin
							FProgress.SetDetail('Found index: ' +
									IntToStr(Result.Index));
							Inc(Result.Count);
							AForeGnd:= f;
							Exit;
							end;
						end;

				Inc(i);
				end;
			end;
		end;

	procedure DoSetTileMapTile(ATile: TTile; ACX, ACY: Integer; AFlipX,
			AFlipY: Boolean; AColour: TPaletteColour);
		var
		m: PTileMapCell;

		begin
		m:= @TileMap[ACY, ACX];
		m^.Tile:= ATile;
		m^.FlipX:= AFlipX;
		m^.FlipY:= AFlipY;
		m^.ForeGnd:= AColour;
		end;

	procedure DoFlipDataX(var AData: TTileRGBData);
		var
		i,
		j: Integer;
		t: TRGBPixel;
		c,
		s: PRGBPixel;

		begin
//      Flip data on X axis (vertically)

		for i:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
			begin
			c:= PRGBPixel(@AData[i * VAL_SIZ_TILEPIX_LENGTH * 3]);
			s:= PRGBPixel(@AData[(i + 1) * VAL_SIZ_TILEPIX_LENGTH * 3 - 3]);

			for j:= 0 to (VAL_SIZ_TILEPIX_LENGTH div 2) - 1 do
				begin
				t:= s^;

				s^:= c^;
				c^:= t;

				Inc(c);
				Dec(s);
				end;
			end;
		end;

	procedure DoFlipDataY(var AData: TTileRGBData);
		var
		i,
		j,
		k: Integer;
		t: TRGBPixel;
		c,
		s: PRGBPixel;

		begin
//      Flip data on Y axis (horizontally)

		for i:= 0 to (VAL_SIZ_TILEPIX_LENGTH div 2) - 1 do
			begin
			for j:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
				begin
				k:= i * VAL_SIZ_TILEPIX_LENGTH * 3 + j * 3;
				c:= PRGBPixel(@AData[k]);

				k:= ((VAL_SIZ_TILEPIX_LENGTH - 1) - i) * VAL_SIZ_TILEPIX_LENGTH * 3 +
						j * 3;
				s:= PRGBPixel(@AData[k]);

				t:= s^;

				s^:= c^;
				c^:= t;
				end;
			end;
		end;

	begin
    if  ExtraLeading
    and (TileList.Count > 0) then
        begin
		Move(AData2[0], r[0], SizeOf(TTileRGBData));
    	t:= DoCheckForTileData(r, c, n);
	    u:= (not Assigned(t)) or (t.Index <> 0);
        end
   	else
    	u:= False;

//  Check normal orientation
	fx:= False;
	fy:= False;
	Move(AData[0], r[0], SizeOf(TTileRGBData));

//	Debugging
	FProgress.DrawTileData(r, 0);

    e:= False;

    if  AllUnique
    or  u then
        t:= nil
    else
        begin
	    t:= DoCheckForTileData(r, c, n);

        if  NoFlip then
            begin

            end
        else if  not Assigned(t) then
		    begin
    //		Else check x (vert) flip
		    fx:= True;
		    DoFlipDataX(r);

		    t:= DoCheckForTileData(r, c, n);
		    if  not Assigned(t) then
			    begin
    //			Else check x, y (vert, horz) flip
			    fy:= True;
			    DoFlipDataY(r);

			    t:= DoCheckForTileData(r, c, n);
			    if  not Assigned(t) then
				    begin
    //				Else check y (horz) flip
				    fx:= False;
				    Move(AData[0], r[0], SizeOf(TTileRGBData));
				    DoFlipDataY(r);

				    t:= DoCheckForTileData(r, c, n);
				    end;
			    end;
		    end;
        end;

    e:= False;

    if  ExtraLeading
    and Assigned(t)
    and (t.Index = 0) then
        if  (ACY > 0)
        and ((TileMap[ACY- 1, ACX].Tile.Index <> 0)
        and  (not TileMap[ACY - 1, ACX].Tile.Padding)) then
            begin
            e:= True;
            t:= nil;
            end;

	if  Assigned(t) then
//		Found
		DoSetTileMapTile(t, ACX, ACY, fx, fy, c)
	else
        begin
//		Else need to add as a new tile
		DoAddTileMapTile(AData, ACX, ACY, n, ACurrIndex);

        FLastCX:= ACX;
        FLastCY:= ACY;

        TileList[ACurrIndex - 1].Padding:= e;
        end;
    end;


procedure TImageToTilesCore.DoUpdatePaletteMap(AData: TTileRGBData);
	var
	p: PRGBPixelArr;
	i,
	j: Integer;
	f: Boolean;

	begin
	p:= @AData[0];
	for i:= 0 to VAL_SIZ_TILEPIX_TLPALD - 1 do
		if  not ((p^[i].R = Background.R)
		and  (p^[i].G = Background.G)
		and  (p^[i].B = Background.B)) then
			begin
			f:= False;

			for j:= 0 to High(PaletteMap) do
//					if  p^[i] = FPaletteMap[j].RGB then
				if  (p^[i].R = PaletteMap[j].RGB.R)
				and (p^[i].G = PaletteMap[j].RGB.G)
				and (p^[i].B = PaletteMap[j].RGB.B) then
					begin
					f:= True;
                    Inc(PaletteMap[j].Count);

					Break;
					end;

			if  not f then
				begin
				SetLength(PaletteMap, Length(PaletteMap) + 1);
				PaletteMap[High(PaletteMap)].RGB:= p^[i];
                PaletteMap[High(PaletteMap)].Count:= 1;
				end;
			end;
	end;

procedure TImageToTilesCore.DoWriteScreenMap(AFile: TFileStream);
	var
	i,
	j: Integer;
	w: Word;

    begin
	for i:= Low(TileMap) to High(TileMap) do
		for j:= Low(TileMap[i]) to High(TileMap[i]) do
			begin
            if  TileMap[i, j].Tile.EndMarker then
                begin
				w:= TileMap[0, 0].Tile.SegTile;
	            end
           	else
            	begin
				Assert((TileMap[i, j].Tile.SegTile >= 0) and
						(TileMap[i, j].Tile.SegTile <= $1FFF));

				w:= TileMap[i, j].Tile.SegTile;
                end;

//dengland  This must be little endian.  The highest 3 bits should be masked.
			w:= w and $1FFF;
			AFile.Write(w, 2);
			end;
    end;

procedure TImageToTilesCore.DoWriteColourMap(AFile: TFileStream);
	var
	i,
	j: Integer;
	fg: Byte;
    dat: Byte;

    begin
	for i:= Low(TileMap) to High(TileMap) do
		for j:= Low(TileMap[i]) to High(TileMap[i]) do
			begin
			dat:= $00;
			if  TileMap[i, j].FlipX then
				dat:= dat or $40;
			if  TileMap[i, j].FlipY then
				dat:= dat or $80;

			AFile.Write(dat, 1);


//          If the screen location uses its own foreground, use it
			if  TileMap[i, j].ForeGnd >= 0 then
				fg:= TileMap[i, j].ForeGnd + 1
//          Otherwise, use the tile's foreground.  All tiles must have been
//			    assigned a foreground by now for this to work.
            else
				fg:= TileMap[i, j].Tile.Foreground + 1;

            fg:= fg + PaletteOffset;

            if Mode = 2 then
                dat:= 0
            else if  Mode = 1 then
                begin
                dat:= (fg and $0F);
//              dat:= dat or ((fg and $10) shl 2);
                end
            else
                dat:= fg;

			AFile.Write(dat, 1);
			end;
    end;

procedure TImageToTilesCore.DoWritePalette(AFile: TFileStream);
    var
	i: Integer;

	function  DoSwapNybbles(AData: Byte): Byte;
		begin
		Result:= (AData and $F0) shr 4;
		Result:= Result or ((AData and $0F) shl 4);
		end;

	procedure DoWriteColour(AColour: TRGBPixel);
		var
		b: Byte;

		begin
		AColour.R:= AColour.R and $FE;
		b:= DoSwapNybbles(AColour.R);
		AFile.Write(b, 1);

		b:= DoSwapNybbles(AColour.G);
		AFile.Write(b, 1);

		b:= DoSwapNybbles(AColour.B);
		AFile.Write(b, 1);
		end;

	begin
	DoWriteColour(Background);

	for i:= 0 to High(PaletteMap) do
		DoWriteColour(PaletteMap[i].RGB);
	end;


procedure TImageToTilesCore.PerformOutputFile(AFile: string);
	var
	f: TFileStream;

	procedure DoWriteHeader;
		var
		header: TV4TImgHeader;
        i: Integer;

		begin
		FillChar(header, SizeOf(TV4TImgHeader), 0);

		header.Magic:= LIT_TOK_V4TLIMG_MAGICI;
//dengland Little endian
		header.Version:= $0203;
		header.Mode:= Mode;
//dengland Little endian
		header.ColCount:= ColCount;
		header.RowCount:= RowCount;

        header.LoadBufferAddr:= BufferPtr;
        header.ScreenRAMAddr:= ScreenPtr;
        header.ColourRAMAddr:= ColourPtr;

        header.PaletteAddr:= PalettePtr;
        header.PaletteBank:= PaletteBank;
        header.PaletteCount:= Length(PaletteMap);
        header.PaletteOffset:= PaletteOffset;

        for i:= 0 to High(MemMap) do
            begin
            if  MemMap[i].Segment = -1 then
                Break;

            header.TileCounts[MemMap[i].Segment]:= MemMap[i].Count;
            end;

        header.RelocSource:= RelocSourcePtr;
        header.RelocDest:= RelocDestPtr;
        header.RelocSize:= RelocSize;

		f.Write(header, SizeOf(TV4TImgHeader));
		end;

	procedure DoWriteTiles;
		var
		i,
		j,
		k: Integer;
		pd: TTilePalData;
		pi: TTilePaletteInfo;
		pr: TTilePalDataRaw;
		fg: TPaletteColour;
		m: Integer;

		begin
		for i:= 0 to TileList.Count - 1 do
			begin
//          Get the tile's data as palette colours
			DoConvertPaletteData(TileList[i].RGBData, pd);

//          If we have already determined a foregound...
			if  TileList[i].Foreground > 0 then
//              then use it
				fg:= TileList[i].Foreground
			else
				begin
//              otherwise, get a list of used colours
				DoExtractPaletteInfo(TileList[i].RGBData, pi);

//              and determine the most common
				m:= 0;
				j:= 0;
				k:= 0;
				while (j < Length(pi)) and (pi[j].Colour > 0) do
					begin
					if  pi[j].Count > m then
						begin
						m:= pi[j].Count;
						k:= j;
						end;

					Inc(j);
					end;

//              Use the most common as a foreground
				fg:= pi[k].Colour;

//              ...but only if its within the first 31 (zero based).
                if  Mode = 1 then
                    if  fg > 30 then
                        fg:= VAL_CLR_TILEPAL_UNKNWN;

//              Make sure every tile has a foreground colour
				TileList[i].Foreground:= fg;
				end;

//          Convert the palette colours into actual palette data
			for j:= 0 to High(TTilePalData) do
//              Background/unknown must be $00
				if  (pd[j] = VAL_CLR_TILEPAL_BACKGD)
				or  (pd[j] = VAL_CLR_TILEPAL_UNKNWN) then
					pr[j]:= $00
//              Foreground must be $FF
				else if (pd[j] = fg)
				or  (pd[j] = VAL_CLR_TILEPAL_FOREGD) then
					pr[j]:= $FF
//              Otherwise, the data is one off (zero based but must be one based)
				else
					pr[j]:= pd[j] + 1 + PaletteOffset;

//          Write the data
			f.Write(pr[0], SizeOf(TTilePalDataRaw));
			end;
		end;

	begin
	f:= TFileStream.Create(AFile, fmCreate);
	try
		DoWriteHeader;
		DoWriteTiles;
        if  ScreenPtr > 0 then
            begin
		    DoWriteScreenMap(f);
		    DoWriteColourMap(f);
            end;

        if  PaletteBank < $FF then
            DoWritePalette(f);

		finally
		f.Free;
		end;
	end;

procedure TImageToTilesCore.PerformOutputScreen(AFile: string);
    var
    f: TFileStream;

    begin
	f:= TFileStream.Create(AFile, fmCreate);
	try
        DoWriteScreenMap(f);

        finally
        f.Free
        end;
    end;

procedure TImageToTilesCore.PerformOutputColour(AFile: string);
    var
    f: TFileStream;

    begin
	f:= TFileStream.Create(AFile, fmCreate);
	try
        DoWriteColourMap(f);

        finally
        f.Free
        end;
    end;

procedure TImageToTilesCore.PerformOutputPalette(AFile: string);
    var
    f: TFileStream;

    begin
	f:= TFileStream.Create(AFile, fmCreate);
	try
        DoWritePalette(f);

        finally
        f.Free
        end;
    end;

procedure TImageToTilesCore.PerformProcessImage(AProgress: TImageToTilesProgress);
	var
	i,
	j,
	k: Integer;
 {$IFNDEF FPC}
	l: PRGBPixelArr;
 {$ENDIF}
	cw,
	ch,
	ow,
	oh: Integer;
	d,
    d2: TTileRGBData;
	n: Integer;
{$IFDEF FPC}
	h: PRGBPixel;
	PixelPtr: PInteger;
	PixelRowPtr: PInteger;
	RawImage: TRawImage;
	BytePerPixel: Integer;
{$ENDIF}

	procedure DoPrepareTileData;
        var
    	x,
    	y: Integer;

        begin
		k:= 0;
		for y:= oh to oh + VAL_SIZ_TILEPIX_LENGTH - 1 do
			begin
{$IFNDEF FPC}
			l:= PRGBPixelArr(Bitmap.ScanLine[y]);
			for x:= ow to ow + VAL_SIZ_TILEPIX_LENGTH - 1 do
				begin
				d[k]:= l^[x].B;
				Inc(k);
				d[k]:= l^[x].G;
				Inc(k);
				d[k]:= l^[x].R;
				Inc(k);
				end;
{$ELSE}
			RawImage := Bitmap.RawImage;
			PixelRowPtr := PInteger(RawImage.Data);
			Inc(PByte(PixelRowPtr), RawImage.Description.BytesPerLine * y);
			BytePerPixel := RawImage.Description.BitsPerPixel div 8;
			PixelPtr := PixelRowPtr;
			Inc(PByte(PixelPtr), BytePerPixel * ow);
			for x:= ow to ow + VAL_SIZ_TILEPIX_LENGTH - 1 do
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
{$ENDIF}
			end;

		FProgress.SetStage('X: ' + IntToStr(j + 1) + '; Y: ' +
				IntToStr(i + 1));

		if  ExtraLeading
        and (i < (ch - 1)) then
			begin
			k:= 0;
			for y:= oh + VAL_SIZ_TILEPIX_LENGTH to oh + (VAL_SIZ_TILEPIX_LENGTH * 2) - 1 do
				begin
{$IFNDEF FPC}
				l:= PRGBPixelArr(Bitmap.ScanLine[y]);
				for x:= ow to ow + VAL_SIZ_TILEPIX_LENGTH - 1 do
					begin
					d2[k]:= l^[x].B;
					Inc(k);
					d2[k]:= l^[x].G;
					Inc(k);
					d2[k]:= l^[x].R;
					Inc(k);
					end;
{$ELSE}
				RawImage := Bitmap.RawImage;
				PixelRowPtr := PInteger(RawImage.Data);
				Inc(PByte(PixelRowPtr), RawImage.Description.BytesPerLine * y);
				BytePerPixel := RawImage.Description.BitsPerPixel div 8;
				PixelPtr := PixelRowPtr;
				Inc(PByte(PixelPtr), BytePerPixel * ow);
				for x:= ow to ow + VAL_SIZ_TILEPIX_LENGTH - 1 do
					begin
					h:= PRGBPixel(PixelPtr);

					d2[k]:= h^.B;
					Inc(k);
					d2[k]:= h^.G;
					Inc(k);
					d2[k]:= h^.R;
					Inc(k);

					Inc(PByte(PixelPtr), BytePerPixel);
					end;
{$ENDIF}
   				end;
            end
        else
        	begin
            if  TileList.Count > 0 then
            	Move(TileList[0].RGBData[0], d2[0], SizeOf(TTileRGBData))
            else
                Move(d[0], d2[0],SizeOf(TTileRGBData));
            end;
        end;

	begin
	FProgress:= AProgress;

	cw:= Bitmap.Width div VAL_SIZ_TILEPIX_LENGTH;
	ch:= Bitmap.Height div VAL_SIZ_TILEPIX_LENGTH;

	n:= 0;
	SetLength(TileMap, ch);
	for i:= 0 to ch - 1 do
		SetLength(TileMap[i], cw);

	FProgress.InitProgress(cw * ch);

    if  Vertical then
	    for j:= 0 to cw - 1 do
		    begin
		    ow:= j * VAL_SIZ_TILEPIX_LENGTH;

            for i:= 0 to ch - 1 do
		        begin
		        oh:= i * VAL_SIZ_TILEPIX_LENGTH;


                DoPrepareTileData;

			    DoProcessTileData(d, d2, j, i, n);

			    FProgress.IncrementProgress;
			    end;
		    end
   	else
        for i:= 0 to ch - 1 do
         	begin
	        oh:= i * VAL_SIZ_TILEPIX_LENGTH;

            for j:= 0 to cw - 1 do
    	        begin
    	        ow:= j * VAL_SIZ_TILEPIX_LENGTH;

                DoPrepareTileData;

		        DoProcessTileData(d, d2, j, i, n);

		        FProgress.IncrementProgress;
		        end;
	        end;

    if  ExtraLeading
    and (FLastCY < (cw - 1)) then
        begin
        i:= 1;
        j:= TileList.Count;
        DoAddTileMapTile(TileList[0].RGBData, FLastCX, FLastCY + 1, i, j);

        TileList[TileList.Count - 1].EndMarker:= True;
        end;


    FProgress.SetStage('Total Tiles: ' + IntToStr(n));
	FProgress.SetDetail('Colours: ' + IntToStr(Length(PaletteMap)));
	end;

procedure TImageToTilesCore.PerformMapTiles(AMemorySegs: TMemModuleSegs;
        ABufferSeg, AScreenSeg, AColourSeg, APaletteSeg,
        ARelocSource, ARelocDest, ARelocSize: Integer);
    var
    t,
    c,
    i,
    m,
    s: Integer;
    ts: IComparer<TTile>;

    begin
    for i:= 0 to High(MemMap) do
        begin
        MemMap[i].Count:= 0;
        MemMap[i].Segment:= -1;
        end;

    m:= 0;
    s:= 0;
    t:= TileList.Count;
    while t > 0 do
        begin
        c:= VAL_SZE_MEMMOD_SEG div (VAL_SIZ_TILEPIX_LENGTH * VAL_SIZ_TILEPIX_LENGTH);

        if  c > t then
            c:= t;

        for i:= s to High(AMemorySegs) do
            if  AMemorySegs[i].Kind in [mmkFree, mmkRelocDest] then
                begin
                s:= i;
                Break;
                end;

        MemMap[m].Segment:= s;
        MemMap[m].Count:= c;

        Inc(m);
        Inc(s);

        Dec(t, c);
        end;

    i:= 0;
    m:= 0;
    while i < TileList.Count do
        begin
        Assert(MemMap[m].Segment > -1, 'Logic error in mapping.');

        for t:= 0 to MemMap[m].Count - 1 do
            begin
            s:= (MemMap[m].Segment * VAL_SZE_MEMMOD_SEG) div
                    (VAL_SIZ_TILEPIX_LENGTH * VAL_SIZ_TILEPIX_LENGTH) + t;

            TileList[i].SegTile:= s;

            if  AMemorySegs[MemMap[m].Segment].Kind = mmkRelocDest then
                TileList[i].Segment:= MemMap[m].Segment - ARelocDest +
                        ARelocSource
            else
                TileList[i].Segment:= MemMap[m].Segment;

            Inc(i);
            end;

        Inc(m);
        end;

    ts:= TTileSort.Create;
    TileList.Sort(ts);

    if  (ARelocSource > -1)
    and (ARelocDest > -1)
    and (ARelocSize > 0) then
        begin
        m:= 0;
        for i:= 0 to High(MemMap) do
            if  MemMap[i].Segment > -1 then
                if  AMemorySegs[MemMap[i].Segment].Kind = mmkRelocDest then
                    begin
                    MemMap[i].Segment:= ARelocSource + m;
                    Inc(m);
                    end;
        end;

    BufferPtr:= VAL_SZE_MEMMOD_SEG * ABufferSeg;

    if  AScreenSeg = -1 then
        begin
        ScreenPtr:= 0;
        ColourPtr:= 0;
        end
    else
        begin
        ScreenPtr:= VAL_SZE_MEMMOD_SEG * AScreenSeg;

        if  AColourSeg = -1 then
            ColourPtr:= 0
        else
            ColourPtr:= VAL_SZE_MEMMOD_SEG * AColourSeg;
        end;

    if  APaletteSeg = -1 then
        PalettePtr:= 0
    else
        PalettePtr:= VAL_SZE_MEMMOD_SEG * APaletteSeg;

    if  (ARelocSource = -1)
    or  (ARelocDest = -1)
    or  (ARelocSize = 0) then
        begin
        RelocSourcePtr:= 0;
        RelocDestPtr:= 0;
        RelocSize:= 0;
        end
    else
        begin
        RelocSourcePtr:= VAL_SZE_MEMMOD_SEG * ARelocSource;
        RelocDestPtr:= VAL_SZE_MEMMOD_SEG * ARelocDest;
        RelocSize:= VAL_SZE_MEMMOD_SEG * ARelocSize;
        end;
    end;


{ TCmdLineProgress }

procedure TCmdLineProgress.DrawTileData(AData: TTileRGBData; APosition: Integer);
	begin
	end;

procedure TCmdLineProgress.IncrementProgress;
	begin
	end;

procedure TCmdLineProgress.InitProgress(AMax: Integer);
	begin
	end;

procedure TCmdLineProgress.SetDetail(ACaption: string);
	begin
	end;

procedure TCmdLineProgress.SetStage(ACaption: string);
	begin
	end;

end.
