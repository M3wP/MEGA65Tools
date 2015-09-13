unit FormImageToTilesMapView;

{$IFDEF FPC}
	{$MODE Delphi}
	{$POINTERMATH ON}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	ImageToTilesTypes, Vcl.Grids;
{$ELSE}
	SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
	ImageToTilesTypes, Grids, GraphType;
{$ENDIF}

type
	TImageToTilesMapViewForm = class(TForm)
		DrawGrid1: TDrawGrid;
		procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
				State: TGridDrawState);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
	private
		FTempBM: TBitmap;
		FBitmap: TBitmap;
		FTileMap: TTileMap;

	public
		procedure SetBitmap(ABitmap: TBitmap);
		procedure SetTileMap(ATileMap: TTileMap);
	end;

var
	ImageToTilesMapViewForm: TImageToTilesMapViewForm;

implementation

{$IFNDEF FPC}
	{$R *.dfm}
{$ELSE}
	{$R *.lfm}
{$ENDIF}

{ TImageToTilesMapViewForm }

procedure TImageToTilesMapViewForm.DrawGrid1DrawCell(Sender: TObject; ACol,
		ARow: Integer; Rect: TRect; State: TGridDrawState);
	var
	d,
	r: TRect;
	i,
	j: Integer;
	v: Word;
	p: PRGBPixel;
	h: TRGBPixel;
	s: string;
	t: TTile;
	rb,
	gbb: Byte;
{$IFDEF FPC}
	PixelPtr: PInteger;
	PixelRowPtr: PInteger;
	RawImage: TRawImage;
	BytePerPixel: Integer;
    style: TTextStyle;
{$ENDIF}

	begin
	if  (ACol > 0)
	and (ARow > 0) then
		if  Assigned(FBitmap) then
			begin
			t:= FTileMap[ARow - 1, ACol - 1].Tile;

			rb:= $FF;
			if  t.Count = 1 then
				gbb:= $80
			else
				gbb:= $FF;

			d.Top:= 0;
			d.Left:= 0;
			d.Bottom:= VAL_SIZ_TILEPIX_LENGTH;
			d.Right:= VAL_SIZ_TILEPIX_LENGTH;

			r.Top:= (ARow - 1) * VAL_SIZ_TILEPIX_LENGTH;
			r.Bottom:= r.Top + VAL_SIZ_TILEPIX_LENGTH;
			r.Left:= (ACol - 1) * VAL_SIZ_TILEPIX_LENGTH;
			r.Right:= r.Left + VAL_SIZ_TILEPIX_LENGTH;

			FTempBM.Canvas.CopyRect(d, FBitmap.Canvas, r);

{$IFNDEF FPC}
			for i:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
				begin
				p:= PRGBPixel(FTempBM.ScanLine[i]);
				for j:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
					begin
					h:= p^;

					v:= h.R + $FF + rb;
					h.R:= v div 3;
					v:= h.G + $FF + gbb;
					h.G:= v div 3;
					v:= h.B + $FF + gbb;
					h.B:= v div 3;

					p^:= h;

					Inc(p);
					end;
				end;
{$ELSE}
			FTempBM.BeginUpdate(False);
			try
				RawImage := FTempBM.RawImage;
				PixelRowPtr := PInteger(RawImage.Data);
				BytePerPixel := RawImage.Description.BitsPerPixel div 8;
				for i:= 0 to FTempBM.Height - 1 do
					begin
					PixelPtr := PixelRowPtr;
					for j:= 0 to FTempBM.Width - 1 do
						begin
						h:= PRGBPixel(PixelPtr)^;

						v:= h.R + $FF + rb;
						h.R:= v div 3;
						v:= h.G + $FF + gbb;
						h.G:= v div 3;
						v:= h.B + $FF + gbb;
						h.B:= v div 3;

						PRGBPixel(PixelPtr)^:= h;

						Inc(PByte(PixelPtr), BytePerPixel);
						end;
					Inc(PByte(PixelRowPtr), RawImage.Description.BytesPerLine);
					end;

				finally
				FTempBM.EndUpdate(False);
				end;
{$ENDIF}

			DrawGrid1.Canvas.StretchDraw(Rect, FTempBM);

			DrawGrid1.Canvas.Brush.Color:= clNone;
			DrawGrid1.Canvas.Brush.Style:= bsClear;
			DrawGrid1.Canvas.Pen.Color:= clBlack;
			DrawGrid1.Canvas.Pen.Style:= psSolid;

			r:= Rect;

{$IFNDEF FPC}
			InflateRect(r, -4, -4);
{$ELSE}
			r.Top:= r.Top + 4;
			r.Bottom:= r.Bottom - 4;
			r.Left:= r.Left + 4;
			r.Right:= r.Right - 4;
{$ENDIF}

			s:= IntToStr(t.Index);
{$IFNDEF FPC}
			DrawGrid1.Canvas.TextRect(r, s, [tfCenter, tfVerticalCenter]);
{$ELSE}
			FillChar(style, SizeOf(TTextStyle), 0);
			style.Alignment:= taCenter;
			style.Layout:= tlTop;
			style.SingleLine:= True;

			d.Right:= Rect.Right - Rect.Left;
			d.Bottom:= Rect.Bottom - Rect.Top;

			DrawGrid1.Canvas.TextRect(r, r.Left, r.Top, s, style);
{$ENDIF}

			if  t.Count = 1 then
				begin
{$IFNDEF FPC}
				InflateRect(r, 3, 3);
//              Don't know why I need these but if I don't, there is a border
				Inc(r.Right);
				Inc(r.Bottom);
{$ELSE}
				r.Top:= r.Top - 3;
				r.Bottom:= r.Bottom + 3;
				r.Left:= r.Left - 3;
				r.Right:= r.Right + 3;
{$ENDIF}

				DrawGrid1.Canvas.Pen.Color:= clRed;
				DrawGrid1.Canvas.Pen.Width:= 2;
				DrawGrid1.Canvas.Rectangle(r);
				end;
			end;
	end;

procedure TImageToTilesMapViewForm.FormCreate(Sender: TObject);
	begin
	FTempBM:= TBitmap.Create;
	FTempBM.PixelFormat:= pf24bit;
	FTempBM.Width:= VAL_SIZ_TILEPIX_LENGTH;
	FTempBM.Height:= VAL_SIZ_TILEPIX_LENGTH;

    FBitmap:= TBitmap.Create;
    FBitmap.PixelFormat:= pf24bit;
	end;

procedure TImageToTilesMapViewForm.FormDestroy(Sender: TObject);
	begin
	FBitmap.Free;
    FTempBM.Free;
	end;

procedure TImageToTilesMapViewForm.SetBitmap(ABitmap: TBitmap);
	begin
	FBitmap.Width:= ABitmap.Width;
    FBitmap.Height:= ABitmap.Height;

    FBitmap.Canvas.Draw(0, 0, ABitmap);

	DrawGrid1.RowCount:= FBitmap.Height div VAL_SIZ_TILEPIX_LENGTH + 1;
	DrawGrid1.ColCount:= FBitmap.Width div VAL_SIZ_TILEPIX_LENGTH + 1;
	end;

procedure TImageToTilesMapViewForm.SetTileMap(ATileMap: TTileMap);
	begin
	FTileMap:= ATileMap;
	end;

end.
