unit FormImageToTilesMain;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, System.Actions, System.Generics.Collections,
	Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
	Vcl.Imaging.pngimage, Vcl.ActnList, ImageToTilesTypes, FormImageToTilesImgView,
	FormImageToTilesMapView, Vcl.ExtCtrls, ImageToTilesEngine;
{$ELSE}
	SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
	ComCtrls, ActnList, ImageToTilesTypes, FormImageToTilesImgView,
	FormImageToTilesMapView, ExtCtrls, ColorBox, Buttons, ImageToTilesEngine;
{$ENDIF}

type
	TImageToTilesMainForm = class;

	TGUIProgress = class(TImageToTilesProgress)
	private
		FForm: TImageToTilesMainForm;

	public
		procedure InitProgress(AMax: Integer); override;
		procedure SetStage(ACaption: string); override;
		procedure SetDetail(ACaption: string); override;
		procedure IncrementProgress; override;
		procedure DrawTileData(AData: TTileRGBData; APosition: Integer); override;

		constructor Create(AOwner: TImageToTilesMainForm);
	end;

    { TImageToTilesMainForm }

 TImageToTilesMainForm = class(TForm)
        ActExportScreen: TAction;
        ActExportColour: TAction;
        ActExportPalette: TAction;
        ActPaletteManage: TAction;
        ActOutputDebug: TAction;
        ActSourceMap: TAction;
		Button1: TButton;
        Button10: TButton;
        Button5: TButton;
        Button6: TButton;
        Button7: TButton;
        Button8: TButton;
        Button9: TButton;
        CheckBox1: TCheckBox;
        CheckBox2: TCheckBox;
        CheckBox3: TCheckBox;
        CheckBox4: TCheckBox;
        CheckBox5: TCheckBox;
        ComboBox1: TComboBox;
		Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
		OpenDialog1: TOpenDialog;
        PaintBox1: TPaintBox;
		ProgressBar1: TProgressBar;
		ActionList1: TActionList;
		ActSourceOpen: TAction;
		Button2: TButton;
		ActSourceClear: TAction;
		Button3: TButton;
		ActSourceProcess: TAction;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		ColorBox1: TColorBox;
		Button4: TButton;
		Label6: TLabel;
		ActOutputSave: TAction;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        RadioButton3: TRadioButton;
		SaveDialog1: TSaveDialog;
        SaveDialog2: TSaveDialog;
        SaveDialog3: TSaveDialog;
        SaveDialog4: TSaveDialog;
        TrackBar1: TTrackBar;
        procedure ActExportColourExecute(Sender: TObject);
        procedure ActExportPaletteExecute(Sender: TObject);
        procedure ActExportScreenExecute(Sender: TObject);
        procedure ActOutputDebugExecute(Sender: TObject);
        procedure ActPaletteManageExecute(Sender: TObject);
        procedure ActSourceMapExecute(Sender: TObject);
        procedure ActSourceOpenExecute(Sender: TObject);
		procedure ActSourceClearExecute(Sender: TObject);
		procedure ActSourceProcessExecute(Sender: TObject);
        procedure CheckBox3Change(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure ActOutputSaveExecute(Sender: TObject);
        procedure PaintBox1Paint(Sender: TObject);
        procedure TrackBar1Change(Sender: TObject);
	private
		FSourceView: TImageToTilesImgViewForm;
		FMapView: TImageToTilesMapViewForm;

		FProgress: TGUIProgress;
		FCore: TImageToTilesCore;

        FPixData: TTileRGBData;

	protected
		procedure DoDrawTileData(AData: TTileRGBData; APX, APY: Integer);

	public
		{ Public declarations }
	end;

var
	ImageToTilesMainForm: TImageToTilesMainForm;

implementation

{$IFNDEF FPC}
	{$R *.dfm}
{$ELSE}
	{$R *.lfm}
{$ENDIF}

uses
    Math, FormSegmentAllocEdit, FormManagePalette, FormDebugOutput;


procedure TImageToTilesMainForm.ActOutputSaveExecute(Sender: TObject);
	begin
	if  SaveDialog1.Execute then
		try
			FCore.PerformOutputFile(SaveDialog1.FileName);

			except
			ShowMessage('File save failed!');
			end;
	end;

procedure TImageToTilesMainForm.PaintBox1Paint(Sender: TObject);
	var
	c: TColourRec;
	i,
	j: Integer;
    p: PRGBPixel;

	begin
    p:= @FPixData[0];

	for i:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
		for j:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
			begin
			c.A:= $00;
			c.B:= p^.B;
			c.G:= p^.G;
			c.R:= p^.R;

			PaintBox1.Canvas.Pixels[j, i]:= c.C;

			Inc(p);
			end;
    end;

procedure TImageToTilesMainForm.TrackBar1Change(Sender: TObject);
    begin
    Label13.Caption:= IntToStr(TrackBar1.Position);
    FCore.PaletteOffset:= TrackBar1.Position;
    end;

procedure TImageToTilesMainForm.ActSourceClearExecute(Sender: TObject);
	begin
	if  Assigned(FSourceView) then
		begin
		FSourceView.Hide;
		FSourceView.Release;
		FSourceView:= nil;
		end;

	if  Assigned(FMapView) then
		begin
		FMapView.Hide;
		FMapView.Release;
		FMapView:= nil;
		end;

	FCore.Clear;

	ActSourceOpen.Enabled:= True;
	ActSourceClear.Enabled:= False;
	ActSourceProcess.Enabled:= False;
    ActSourceMap.Enabled:= False;
	ActOutputSave.Enabled:= False;
	end;

procedure TImageToTilesMainForm.ActSourceOpenExecute(Sender: TObject);
    var
	cr: TColourRec;

    begin
	if  OpenDialog1.Execute then
		begin
//fixme We should insist that the image is a correct size.

		FCore.Picture.LoadFromFile(OpenDialog1.FileName);

		FSourceView:= TImageToTilesImgViewForm.Create(Self);
		FSourceView.Image1.Picture.Assign(FCore.Picture);

		FSourceView.Show;
        BringToFront;

    	cr.C:= ColorBox1.Selected;
    	FCore.Background.R:= cr.R;
    	FCore.Background.G:= cr.G;
    	FCore.Background.B:= cr.B;

        FCore.PerformImportImage;

        TrackBar1.Position:= 0;
        TrackBar1.Max:= 256 - (Length(FCore.PaletteMap) + 1);
        FCore.PaletteOffset:= 0;

		ActSourceOpen.Enabled:= False;
		ActSourceClear.Enabled:= True;
		ActSourceProcess.Enabled:= True;
        ActPaletteManage.Enabled:= True;
		end;
	end;

procedure TImageToTilesMainForm.ActSourceMapExecute(Sender: TObject);
    begin
    SegmentAllocEditForm.ScreenReqd:= CheckBox3.Checked;

    if  SegmentAllocEditForm.EditMapping then
        begin
        FCore.PerformMapTiles(SegmentAllocEditForm.MemorySegs,
                SegmentAllocEditForm.Buffer, SegmentAllocEditForm.ScreenRAM,
                SegmentAllocEditForm.ColourRAM, SegmentAllocEditForm.Palette,
                SegmentAllocEditForm.RelocSource,
                SegmentAllocEditForm.RelocDest,
                SegmentAllocEditForm.RelocSourceSz);

        ActOutputSave.Enabled:= True;
        ActOutputDebug.Enabled:= True;
        end
    else
        ActOutputSave.Enabled:= False;
    end;

procedure TImageToTilesMainForm.ActOutputDebugExecute(Sender: TObject);
    var
    s: string;
    i: Integer;
    x: Integer;
    a: Cardinal;

    begin
    if  not Assigned(DebugOutputForm) then
        Application.CreateForm(TDebugOutputForm, DebugOutputForm);

    DebugOutputForm.Memo1.Lines.Clear;

    s:= IntToStr(FCore.TileList.Count) + ' tiles; ' +
            IntToStr(Length(FCore.PaletteMap)) + ' colours.';
    DebugOutputForm.Memo1.Lines.Add(s);
    DebugOutputForm.Memo1.Lines.Add('');

    s:= 'Load buffer:  ' + Format('segment# %3.3d; address $%6.6x',
            [SegmentAllocEditForm.Buffer, FCore.BufferPtr]);
    DebugOutputForm.Memo1.Lines.Add(s);
    s:= 'Screen RAM:  ' + Format('segment# %3.3d; address $%6.6x',
            [SegmentAllocEditForm.ScreenRAM, FCore.ScreenPtr]);
    DebugOutputForm.Memo1.Lines.Add(s);
    s:= 'Colour RAM:  ' + Format('segment# %3.3d; address $%6.6x',
            [SegmentAllocEditForm.ColourRAM, FCore.ColourPtr]) + ' --or-- $01FB80';
    DebugOutputForm.Memo1.Lines.Add(s);
    DebugOutputForm.Memo1.Lines.Add('');

    DebugOutputForm.Memo1.Lines.Add('Tile#  Mapped#  Segment#  Address');
    DebugOutputForm.Memo1.Lines.Add('---------------------------------');

    for i:= 0 to FCore.TileList.Count - 1 do
        begin
        a:= FCore.TileList[i].SegTile * 64;
        x:= FCore.TileList[i].Segment;

        s:= Format('%-5.5d  %-7.7d  %-8.8d  $%6.6x', [
                FCore.TileList[i].Index, FCore.TileList[i].SegTile, x, a]);
        DebugOutputForm.Memo1.Lines.Add(s);
        end;

    DebugOutputForm.Visible:= True;
    end;

procedure TImageToTilesMainForm.ActExportScreenExecute(Sender: TObject);
    begin
	if  SaveDialog2.Execute then
		try
			FCore.PerformOutputScreen(SaveDialog2.FileName);

			except
			ShowMessage('File save failed!');
			end;
    end;

procedure TImageToTilesMainForm.ActExportColourExecute(Sender: TObject);
    begin
	if  SaveDialog3.Execute then
		try
			FCore.PerformOutputColour(SaveDialog3.FileName);

			except
			ShowMessage('File save failed!');
			end;
    end;

procedure TImageToTilesMainForm.ActExportPaletteExecute(Sender: TObject);
    begin
	if  SaveDialog4.Execute then
		try
			FCore.PerformOutputPalette(SaveDialog4.FileName);

			except
			ShowMessage('File save failed!');
			end;
    end;

procedure TImageToTilesMainForm.ActPaletteManageExecute(Sender: TObject);
    begin
    ManagePaletteForm.ManagePalette(@FCore.PaletteMap, FCore.Background);
    end;

procedure TImageToTilesMainForm.ActSourceProcessExecute(Sender: TObject);
	var
    c: Cardinal;

	begin
    FCore.AllUnique:= CheckBox1.Checked;
    FCore.NoFlip:= CheckBox2.Checked;

    if RadioButton3.Checked then
        FCore.Mode:= 2
    else if RadioButton2.Checked then
        FCore.Mode:= 1
    else
        FCore.Mode:= 0;

    FCore.Vertical:= CheckBox4.Checked;
    FCore.ExtraLeading:= CheckBox5.Checked;

	FCore.PerformProcessImage(FProgress);

	FMapView:= TImageToTilesMapViewForm.Create(Self);
	FMapView.SetBitmap(FCore.Bitmap);
	FMapView.SetTileMap(FCore.TileMap);
	FMapView.Show;

    BringToFront;

    c:= Ceil((FCore.ColCount * FCore.RowCount * 2) / VAL_SZE_MEMMOD_SEG);
    SegmentAllocEditForm.ScreenSegs:= c;

    c:= Ceil((FCore.TileList.Count * VAL_SIZ_TILEPIX_LENGTH * VAL_SIZ_TILEPIX_LENGTH) /
            VAL_SZE_MEMMOD_SEG);
    SegmentAllocEditForm.TileSegs:= c;

    SegmentAllocEditForm.ScreenReqd:= CheckBox3.Checked;
    SegmentAllocEditForm.InitSegmentMap;

    Label15.Caption:= IntToStr(c);

	ActSourceProcess.Enabled:= False;
    ActPaletteManage.Enabled:= False;
	ActSourceMap.Enabled:= True;
	end;

procedure TImageToTilesMainForm.CheckBox3Change(Sender: TObject);
    begin

    end;

procedure TImageToTilesMainForm.ComboBox1Change(Sender: TObject);
    begin
    if  ComboBox1.ItemIndex = 4 then
        FCore.PaletteBank:= $FF
    else
        FCore.PaletteBank:= ComboBox1.ItemIndex;
    end;

procedure TImageToTilesMainForm.DoDrawTileData(AData: TTileRGBData;
		APX, APY: Integer);
//	var
//	c: TColourRec;
//	i,
//	j: Integer;
//	p: PRGBPixel;

	begin
    FPixData:= AData;
    PaintBox1.Repaint;

 //   p:= @(AData[0]);
	//for i:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
	//	for j:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
	//		begin
	//		c.A:= $00;
	//		c.B:= p^.B;
	//		c.G:= p^.G;
	//		c.R:= p^.R;
 //
	//		Canvas.Pixels[APX + j, APY + i]:= c.C;
 //
	//		Inc(p);
	//		end;

 	Application.ProcessMessages;
//	Sleep(200);
	end;

procedure TImageToTilesMainForm.FormCreate(Sender: TObject);
	begin
    FProgress:= TGUIProgress.Create(Self);
	FCore:= TImageToTilesCore.Create;
	end;

procedure TImageToTilesMainForm.FormDestroy(Sender: TObject);
	begin
	ActSourceClearExecute(Self);

	FCore.Free;
	FProgress.Free;
	end;

{ TGUIProgress }

constructor TGUIProgress.Create(AOwner: TImageToTilesMainForm);
	begin
	inherited Create;

	FForm:= AOwner;
	end;

procedure TGUIProgress.DrawTileData(AData: TTileRGBData; APosition: Integer);
	var
	x: Integer;

	begin
	x:= 346 + APosition * VAL_SIZ_TILEPIX_LENGTH;
	FForm.DoDrawTileData(AData, x, 130);
    FForm.Repaint;
	end;

procedure TGUIProgress.IncrementProgress;
	begin
	FForm.ProgressBar1.Position:= FForm.ProgressBar1.Position + 1;
	end;

procedure TGUIProgress.InitProgress(AMax: Integer);
	begin
	FForm.ProgressBar1.Max:= AMax;
	FForm.ProgressBar1.Position:= 0;
	end;

procedure TGUIProgress.SetDetail(ACaption: string);
	begin
	FForm.Label4.Caption:= ACaption;
	end;

procedure TGUIProgress.SetStage(ACaption: string);
	begin
	FForm.Label3.Caption:= ACaption;
	end;

end.
