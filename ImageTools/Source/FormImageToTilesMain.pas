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
	FormImageToTilesMapView, ExtCtrls, ColorBox, ImageToTilesEngine;
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

	TImageToTilesMainForm = class(TForm)
		Button1: TButton;
		Label1: TLabel;
		OpenDialog1: TOpenDialog;
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
		SaveDialog1: TSaveDialog;
		procedure ActSourceOpenExecute(Sender: TObject);
		procedure ActSourceClearExecute(Sender: TObject);
		procedure ActSourceProcessExecute(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure ActOutputSaveExecute(Sender: TObject);
	private
		FSourceView: TImageToTilesImgViewForm;
		FMapView: TImageToTilesMapViewForm;

		FProgress: TGUIProgress;
		FCore: TImageToTilesCore;

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

procedure TImageToTilesMainForm.ActOutputSaveExecute(Sender: TObject);
	begin
	if  SaveDialog1.Execute then
		try
			FCore.PerformOutputFile(SaveDialog1.FileName);

			except
			ShowMessage('File save failed!');
			end;
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
	ActOutputSave.Enabled:= False;
	end;

procedure TImageToTilesMainForm.ActSourceOpenExecute(Sender: TObject);
	begin
	if  OpenDialog1.Execute then
		begin
		FCore.Picture.LoadFromFile(OpenDialog1.FileName);

		FSourceView:= TImageToTilesImgViewForm.Create(Self);
		FSourceView.Image1.Picture.Assign(FCore.Picture);

//fixme We should insist that the image is a correct size.

		FSourceView.Show;

		ActSourceOpen.Enabled:= False;
		ActSourceClear.Enabled:= True;
		ActSourceProcess.Enabled:= True;
		end;
	end;

procedure TImageToTilesMainForm.ActSourceProcessExecute(Sender: TObject);
	var
	cr: TColourRec;

	begin
	cr.C:= ColorBox1.Selected;
	FCore.Background.R:= cr.R;
	FCore.Background.G:= cr.G;
	FCore.Background.B:= cr.B;

	FCore.PerformProcessImage(FProgress);

	FMapView:= TImageToTilesMapViewForm.Create(Self);
	FMapView.SetBitmap(FCore.Bitmap);
	FMapView.SetTileMap(FCore.TileMap);
	FMapView.Show;

	ActSourceProcess.Enabled:= False;
	ActOutputSave.Enabled:= True;
	end;

procedure TImageToTilesMainForm.DoDrawTileData(AData: TTileRGBData;
		APX, APY: Integer);
	var
	c: TColourRec;
	i,
	j: Integer;
	p: PRGBPixel;

	begin
	p:= @(AData[0]);
	for i:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
		for j:= 0 to VAL_SIZ_TILEPIX_LENGTH - 1 do
			begin
			c.A:= $00;
			c.B:= p^.B;
			c.G:= p^.G;
			c.R:= p^.R;

			Canvas.Pixels[APX + j, APY + i]:= c.C;

			Inc(p);
			end;

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
