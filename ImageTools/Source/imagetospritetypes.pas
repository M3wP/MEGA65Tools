unit ImageToSpriteTypes;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

{$H+}

interface

uses
    Classes, SysUtils, Generics.Collections;

const
    VAL_SIZ_SPRTPIX_WIDTH = 16;

type
	PRGBPixel = ^TRGBPixel;
	TRGBPixel = packed record
		B, G, R: Byte;
	end;

    PRGBPixelArr = ^TRGBPixelArr;
	TRGBPixelArr = array[0..4095] of TRGBPixel;

    TPaletteEntry = record
		RGB: TRGBPixel;
//		Tiles: TTileList;
	end;

	TPaletteMap = array of TPaletteEntry;

	TTileRGBData = array of Byte;
	TTilePalData = array of Byte;

	TSprite = class
	public
		Index: Integer;

		RGBData: TTileRGBData;
		PalData: TTilePalData;
	end;

	TSpriteList = TList<TSprite>;

	TM65SpriteHeader = packed record
		Magic: array[0..10] of AnsiChar;
        Unknown: Byte;
        SpriteHeight: Byte;
		SpriteCount: Byte;
        DataSize: Word;
	end;


implementation

end.

