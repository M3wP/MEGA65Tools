unit ImageToTilesTypes;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
	Vcl.Graphics, System.Generics.Collections;
{$ELSE}
	Graphics, Generics.Collections, Generics.Defaults;
{$ENDIF}

const
    LIT_TOK_V4TLIMG_MAGICI = 'V4TI';

	VAL_SIZ_TILEPIX_LENGTH = 8;
	VAL_SIZ_TILEPIX_TLRGBD = VAL_SIZ_TILEPIX_LENGTH * VAL_SIZ_TILEPIX_LENGTH * 3;
	VAL_SIZ_TILEPIX_TLPALD = VAL_SIZ_TILEPIX_LENGTH * VAL_SIZ_TILEPIX_LENGTH;

	VAL_CLR_TILEPAL_BACKGD = -1;
	VAL_CLR_TILEPAL_FOREGD = -2;
	VAL_CLR_TILEPAL_UNKNWN = -3;

type
	TColourRec = packed record
		case Boolean of
			True: (
				R, G, B, A: Byte
				);
			False: (
				C: TColor)
	end;

	TMD5 = array[0..15] of Byte;

	PRGBPixel = ^TRGBPixel;
	TRGBPixel = packed record
		B, G, R: Byte;
	end;

//dengland I'm making this an integer so that foreground and background can be
//		explicitly signalled.  Must be SmallInt because we also need the value $FF.
    PPaletteColour = ^TPaletteColour;
	TPaletteColour = SmallInt;

	TPaletteInfo = record
		Colour: TPaletteColour;
		Count: Integer;
	end;

	PRGBPixelArr = ^TRGBPixelArr;
	TRGBPixelArr = array[0..4095] of TRGBPixel;

	TTileRGBData = array[0..VAL_SIZ_TILEPIX_TLRGBD - 1] of Byte;
	TTileMaskData = array[0..VAL_SIZ_TILEPIX_TLPALD - 1] of Byte;
	TTilePalData = array[0..VAL_SIZ_TILEPIX_TLPALD - 1] of TPaletteColour;
	TTilePalDataRaw = array[0..VAL_SIZ_TILEPIX_TLPALD - 1] of Byte;
	TTilePaletteInfo = array[0..VAL_SIZ_TILEPIX_TLPALD - 1] of TPaletteInfo;

	TTile = class
	public
		Index: Integer;
		Count: Integer;

        Padding: Boolean;
        EndMarker: Boolean;

        SegTile: Integer;
        Segment: Integer;

		RGBData: TTileRGBData;
		MaskData: TTileMaskData;
//fixme I should carry this data in processing logic.  I don't.
		PalData: TTilePalData;
		Foreground: TPaletteColour;
        ColCount: Integer;
	end;

	TTileList = TList<TTile>;

    { TTileSort }

    TTileSort = class(TComparer<TTile>)
    public
      function Compare(constref ALeft, ARight: TTile): Integer; override;
    end;

    PTileMapCell = ^TTileMapCell;
	TTileMapCell = record
	public
		Tile: TTile;
		FlipX,
		FlipY: Boolean;
        ForeGnd: TPaletteColour;
	end;

	TTileMapRow = array of TTileMapCell;
	TTileMap = array of TTileMapRow;

	TTileDictionary = TDictionary<TMD5, TTileList>;

	TPaletteEntry = record
		RGB: TRGBPixel;
//		Tiles: TTileList;
        Count: Integer;
	end;

    PPaletteMap = ^TPaletteMap;
	TPaletteMap = array of TPaletteEntry;

//  254 bytes + 2 bytes sector pointer is one disk sector.
	TV4TImgHeader = packed record
		Magic: array[0..3] of AnsiChar;             //0
		Version: Word;                              //4
		Mode: Byte;                                 //5
        Reserved0: Byte;                            //6
		ColCount: Byte;                             //7
		RowCount: Byte;                             //8
        LoadBufferAddr: Cardinal;                   //9
        ScreenRAMAddr: Cardinal;                    //13    0
        ColourRAMAddr: Cardinal;                    //17    4
        PaletteAddr: Cardinal;                      //21    8
        PaletteBank: Byte;                          //25    12
        PaletteCount: Byte;                         //26    13
        PaletteOffset: Byte;                        //27    14
        TileCounts: array[0..191] of Byte;          //28    15
        RelocSource: Cardinal;
        RelocDest: Cardinal;
        RelocSize: Word;
        Reserved1: array[0..22] of Byte;
	end;


    TMemAddress = $00000..$2FFFF;

    TMemModKind = (mmkFree, mmkSystem, mmkSysColourRAM, mmkScreenRAM,
            mmkColourRAM, mmkPalette, mmkBuffer, mmkReserved, mmkUnavailable,
            mmkRelocSource, mmkRelocDest);
    TMemModKinds = set of TMemModKind;

    TMemModule = record
        Name: string;
        Kind:  TMemModKind;
        Start: TMemAddress;
        Segments: Word;
    end;

    TMemMapTiles = record
        Segment: Integer;
        Count: Integer;
    end;

    TMemModuleSegs = array[0..191] of TMemModule;
    TMemTileSegs = array[0..191] of TMemMapTiles;

const
    VAL_SZE_MEMMOD_SEG = $0400;

    REC_MEMMOD_SYSTEMLO: TMemModule = (
            Name: 'System Lo'; Kind: mmkSystem;
            Start: $00000; Segments: 1);
    REC_MEMMOD_SYSTEMHI: TMemModule = (
            Name: 'System Hi'; Kind: mmkSystem;
            Start: $0FC00; Segments: 1);
    REC_MEMMOD_SYSTEMIO: TMemModule = (
            Name: 'System IO'; Kind: mmkSystem;
            Start: $0D000; Segments: 4);
    REC_MEMMOD_SYSCLRRM: TMemModule = (
            Name: 'System Colour RAM'; Kind: mmkSysColourRAM;
            Start: $1F800; Segments: 2);
    REC_MEMMOD_SCREENRM: TMemModule = (
            Name: 'Screen RAM'; Kind: mmkScreenRAM;
            Start: $02000; Segments: 2);
    REC_MEMMOD_LDBUFFER: TMemModule = (
            Name: 'Load Buffer'; Kind: mmkBuffer;
            Start: $01800; Segments: 2);

    LIT_TOK_MEMMOD_CLR = 'Colour RAM';
    LIT_TOK_MEMMOD_RSV = 'Reserved';
    LIT_TOK_MEMMOD_FRE = 'Free';
    LIT_TOK_MEMMOD_PAL = 'Palette Buffer';
    LIT_TOK_MEMMOD_UNV = 'Unavailable to Tiles';
    LIT_TOK_MEMMOD_RLS = 'Relocation Source';
    LIT_TOK_MEMMOD_RLD = 'Relocation Destination';



//dengland
//      What on earth was I doing with the Mask?  I cannot remember.
function  ComputeTileMD5(AData: TTileRGBData; {AMask: TTileMaskData;}
		var AMD5: TMD5): Boolean;

procedure SortPaletteMap(var APalette: TPaletteMap);


implementation

type
	TMD5Context = record
	private
		FTotal: array[0..1] of Cardinal;
		FState: array[0..3] of Cardinal;
		FBuffer: array[0..63] of Byte;

		procedure Process(ABuf: PByte);

	public
		procedure Start;
		procedure Update(ABuf: PByte; ACount: Cardinal);
		procedure Finish(var AMD5: TMD5);
	end;

const
	ARR_VAL_TILEMMD5_PADDING: array[0..63] of Byte = (
			$80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

function  READ_LE_UINT32(ptr: Pointer): Cardinal; inline;
	begin
	Result:= PCardinal(ptr)^;
	end;

procedure WRITE_LE_UINT32(ptr: Pointer; value: Cardinal); inline;
	begin
	PCardinal(ptr)^:= value;
	end;

procedure GET_UINT32(var val: Cardinal; base: PByte; offs: Integer); inline;
	begin
	val:= READ_LE_UINT32(base + offs);
	end;

procedure PUT_UINT32(val: Cardinal; base: PByte; offs: Integer); inline;
	begin
	WRITE_LE_UINT32(base + offs, val);
	end;

{ TTileSort }

function TTileSort.Compare(constref ALeft, ARight: TTile): Integer;
    begin
    Result:= ALeft.Segment - ARight.Segment;
    if  Result = 0 then
        Result:= ALeft.SegTile - ARight.SegTile;
    end;


procedure TMD5Context.Finish(var AMD5: TMD5);
	var
	last,
	padn: Cardinal;
	high,
	low: Cardinal;
	msglen: array[0..7] of Byte;

	begin
	high:= (FTotal[0] shr 29) or (FTotal[1] shl 3);
	low:= FTotal[0] shl  3;

	PUT_UINT32(low,  @msglen[0], 0);
	PUT_UINT32(high, @msglen[0], 4);

	last:= FTotal[0] and $3F;
	if  last < 56 then
		padn:= 56 - last
	else
		padn:= 120 - last;

	Update(@ARR_VAL_TILEMMD5_PADDING[0], padn);
	Update(@msglen[0], 8);

	PUT_UINT32(FState[0], @AMD5[0],  0);
	PUT_UINT32(FState[1], @AMD5[0],  4);
	PUT_UINT32(FState[2], @AMD5[0],  8);
	PUT_UINT32(FState[3], @AMD5[0], 12);
	end;

//This is pretty nasty.  I hope there are no artefacts from the conversion and
//		that I haven't otherwise broken the logic.  For some reason the F
//		routines don't match my pascal reference.  I guess someone has
//		determined that these versions are likely to be better??
procedure TMD5Context.Process(ABuf: PByte);
//define S(x, n) ((x << n) | ((x & 0xFFFFFFFF) >> (32 - n)))
	function S(AX: Cardinal; AN: Byte): Cardinal; inline;
		begin
		Result:= ((AX shl AN) or ((AX and $FFFFFFFF) shr (32 - AN)));
		end;

//define P(a, b, c, d, k, s, t)
//	{
//		a += F(b,c,d) + X[k] + t; a = S(a,s) + b;
//	}

//define F(x, y, z) (z ^ (x & (y ^ z)))
	procedure P1(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AD xor (AB and (AC xor AD))) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

//define F(x, y, z) (y ^ (z & (x ^ y)))
	procedure P2(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AC xor (AD and (AB xor AC))) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

//define F(x, y, z) (x ^ y ^ z)
	procedure P3(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AB xor AC xor AD) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

//define F(x, y, z) (y ^ (x | ~z))
	procedure P4(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AC xor (AB or (not AD))) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

	var
	X: array[0..15] of Cardinal;
	A,
	B,
	C,
	D: Cardinal;

	begin
	GET_UINT32(X[0],  ABuf,  0);
	GET_UINT32(X[1],  ABuf,  4);
	GET_UINT32(X[2],  ABuf,  8);
	GET_UINT32(X[3],  ABuf, 12);
	GET_UINT32(X[4],  ABuf, 16);
	GET_UINT32(X[5],  ABuf, 20);
	GET_UINT32(X[6],  ABuf, 24);
	GET_UINT32(X[7],  ABuf, 28);
	GET_UINT32(X[8],  ABuf, 32);
	GET_UINT32(X[9],  ABuf, 36);
	GET_UINT32(X[10], ABuf, 40);
	GET_UINT32(X[11], ABuf, 44);
	GET_UINT32(X[12], ABuf, 48);
	GET_UINT32(X[13], ABuf, 52);
	GET_UINT32(X[14], ABuf, 56);
	GET_UINT32(X[15], ABuf, 60);

	A:= FState[0];
	B:= FState[1];
	C:= FState[2];
	D:= FState[3];

	P1(A, B, C, D, X[  0],  7, $D76AA478);
	P1(D, A, B, C, X[  1], 12, $E8C7B756);
	P1(C, D, A, B, X[  2], 17, $242070DB);
	P1(B, C, D, A, X[  3], 22, $C1BDCEEE);
	P1(A, B, C, D, X[  4],  7, $F57C0FAF);
	P1(D, A, B, C, X[  5], 12, $4787C62A);
	P1(C, D, A, B, X[  6], 17, $A8304613);
	P1(B, C, D, A, X[  7], 22, $FD469501);
	P1(A, B, C, D, X[  8],  7, $698098D8);
	P1(D, A, B, C, X[  9], 12, $8B44F7AF);
	P1(C, D, A, B, X[ 10], 17, $FFFF5BB1);
	P1(B, C, D, A, X[ 11], 22, $895CD7BE);
	P1(A, B, C, D, X[ 12],  7, $6B901122);
	P1(D, A, B, C, X[ 13], 12, $FD987193);
	P1(C, D, A, B, X[ 14], 17, $A679438E);
	P1(B, C, D, A, X[ 15], 22, $49B40821);

	P2(A, B, C, D, X[  1],  5, $F61E2562);
	P2(D, A, B, C, X[  6],  9, $C040B340);
	P2(C, D, A, B, X[ 11], 14, $265E5A51);
	P2(B, C, D, A, X[  0], 20, $E9B6C7AA);
	P2(A, B, C, D, X[  5],  5, $D62F105D);
	P2(D, A, B, C, X[ 10],  9, $02441453);
	P2(C, D, A, B, X[ 15], 14, $D8A1E681);
	P2(B, C, D, A, X[  4], 20, $E7D3FBC8);
	P2(A, B, C, D, X[  9],  5, $21E1CDE6);
	P2(D, A, B, C, X[ 14],  9, $C33707D6);
	P2(C, D, A, B, X[  3], 14, $F4D50D87);
	P2(B, C, D, A, X[  8], 20, $455A14ED);
	P2(A, B, C, D, X[ 13],  5, $A9E3E905);
	P2(D, A, B, C, X[  2],  9, $FCEFA3F8);
	P2(C, D, A, B, X[  7], 14, $676F02D9);
	P2(B, C, D, A, X[ 12], 20, $8D2A4C8A);

	P3(A, B, C, D, X[  5],  4, $FFFA3942);
	P3(D, A, B, C, X[  8], 11, $8771F681);
	P3(C, D, A, B, X[ 11], 16, $6D9D6122);
	P3(B, C, D, A, X[ 14], 23, $FDE5380C);
	P3(A, B, C, D, X[  1],  4, $A4BEEA44);
	P3(D, A, B, C, X[  4], 11, $4BDECFA9);
	P3(C, D, A, B, X[  7], 16, $F6BB4B60);
	P3(B, C, D, A, X[ 10], 23, $BEBFBC70);
	P3(A, B, C, D, X[ 13],  4, $289B7EC6);
	P3(D, A, B, C, X[  0], 11, $EAA127FA);
	P3(C, D, A, B, X[  3], 16, $D4EF3085);
	P3(B, C, D, A, X[  6], 23, $04881D05);
	P3(A, B, C, D, X[  9],  4, $D9D4D039);
	P3(D, A, B, C, X[ 12], 11, $E6DB99E5);
	P3(C, D, A, B, X[ 15], 16, $1FA27CF8);
	P3(B, C, D, A, X[  2], 23, $C4AC5665);

	P4(A, B, C, D, X[  0],  6, $F4292244);
	P4(D, A, B, C, X[  7], 10, $432AFF97);
	P4(C, D, A, B, X[ 14], 15, $AB9423A7);
	P4(B, C, D, A, X[  5], 21, $FC93A039);
	P4(A, B, C, D, X[ 12],  6, $655B59C3);
	P4(D, A, B, C, X[  3], 10, $8F0CCC92);
	P4(C, D, A, B, X[ 10], 15, $FFEFF47D);
	P4(B, C, D, A, X[  1], 21, $85845DD1);
	P4(A, B, C, D, X[  8],  6, $6FA87E4F);
	P4(D, A, B, C, X[ 15], 10, $FE2CE6E0);
	P4(C, D, A, B, X[  6], 15, $A3014314);
	P4(B, C, D, A, X[ 13], 21, $4E0811A1);
	P4(A, B, C, D, X[  4],  6, $F7537E82);
	P4(D, A, B, C, X[ 11], 10, $BD3AF235);
	P4(C, D, A, B, X[  2], 15, $2AD7D2BB);
	P4(B, C, D, A, X[  9], 21, $EB86D391);

	Inc(FState[0], A);
	Inc(FState[1], B);
	Inc(FState[2], C);
	Inc(FState[3], D);
	end;

procedure TMD5Context.Start;
	begin
	FTotal[0]:= 0;
	FTotal[1]:= 0;

	FState[0]:= $67452301;
	FState[1]:= $EFCDAB89;
	FState[2]:= $98BADCFE;
	FState[3]:= $10325476;
	end;

procedure TMD5Context.Update(ABuf: PByte; ACount: Cardinal);
	var
	left,
	fill: Cardinal;
	len: Cardinal;
	input: PByte;

	begin
	len:= ACount;
	input:= ABuf;

	if  len = 0 then
		Exit;

	left:= FTotal[0] and $3F;
	fill:= 64 - left;

	Inc(FTotal[0], len);
	FTotal[0]:= FTotal[0] and $FFFFFFFF;

	if  FTotal[0] < len then
		Inc(FTotal[1]);

	if  (left <> 0) and (len >= fill) then
		begin
//		memcpy((void *)(ctx->buffer + left), (const void *)input, fill);
		Move(input^, FBuffer[left], fill);

		Process(@FBuffer[0]);
		Dec(len, fill);
		Inc(input, fill);
		left:= 0;
		end;

	while len >= 64 do
		begin
		Process(input);
		Dec(len, 64);
		Inc(input, 64);
		end;

	if  len > 0 then
//		memcpy((void *)(ctx->buffer + left), (const void *)input, length);
		Move(input^, FBuffer[left], len);
	end;

function  ComputeTileMD5(AData: TTileRGBData; {AMask: TTileMaskData;}
		var AMD5: TMD5): Boolean;
	var
	ctx: TMD5Context;
	i: Integer;
//	j,
//	k: Integer;
//	buf: array[0..191] of Byte;
//	restricted: Boolean;
//	readlen: Cardinal;
//	len: Cardinal;

	begin
	FillChar(AMD5, SizeOf(TMD5), 0);

//	restricted:= (len <> 0);

//	if  (not restricted) or (SizeOf(buf) <= len) then
//		readlen:= SizeOf(buf)
//	else
//		readlen:= len;

	ctx.Start;

//	i:= readlen;
	i:= SizeOf(TTileRGBData);

//	while i > 0 do
//		begin
//		ctx.Update(@buf[0], i);
	ctx.Update(@AData[0], i);

	//i:= SizeOf(TTileMaskData);
	//ctx.Update(@AMask[0], i);

//		if  restricted then
//			begin
//			Dec(len, i);
//			if  len = 0 then
//				Break;
//
//			if  SizeOf(buf) > len then
//				readlen:= len;
//			end;
//
//		i:= AStream.Read(buf, readlen);
//		end;

	ctx.Finish(AMD5);
	Result:= True;
	end;

function ComparePalette(var APalette: TPaletteMap; L, R: Integer): Integer;
    begin
//  descending
    Result:= APalette[R].Count - APalette[L].Count;
    end;

procedure ExchangePaletteProc(var APalette: TPaletteMap; L, R: Integer);
    var
    temp: TPaletteEntry;

    begin
    temp:= APalette[L];
    APalette[L]:= APalette[R];
    APalette[R]:= temp;
    end;


procedure QuickSortPalette(var APalette: TPaletteMap; L, R: Integer);
    var
    Pivot,
    vL,
    vR: Integer;

    begin
//  a little bit of time saver
    if  R - L <= 1 then
        begin
        if L < R then
            if  ComparePalette(APalette, L, R) > 0 then
                ExchangePaletteProc(APalette, L, R);

        Exit;
        end;

    vL:= L;
    vR:= R;

//  they say random is best
    Pivot:= L + Random(R - L);

    while vL < vR do
        begin
        while (vL < Pivot) and (ComparePalette(APalette, vL, Pivot) <= 0) do
            Inc(vL);

        while (vR > Pivot) and (ComparePalette(APalette, vR, Pivot) > 0) do
            Dec(vR);

        ExchangePaletteProc(APalette, vL, vR);

//      swap pivot if we just hit it from one side
        if  Pivot = vL then
            Pivot := vR
        else if Pivot = vR then
            Pivot := vL;
        end;

    if  Pivot - 1 >= L then
        QuickSortPalette(APalette, L, Pivot - 1);
    if  Pivot + 1 <= R then
        QuickSortPalette(APalette, Pivot + 1, R);
    end;


procedure SortPaletteMap(var APalette: TPaletteMap);
    begin
    QuickSortPalette(APalette, 0, Length(APalette) - 1);
    end;

end.
