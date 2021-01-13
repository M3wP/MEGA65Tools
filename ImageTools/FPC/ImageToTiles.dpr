program ImageToTiles;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  FormImageToTilesMain in '..\Source\FormImageToTilesMain.pas' {ImageToTilesMainForm},
  FormImageToTilesImgView in '..\Source\FormImageToTilesImgView.pas' {ImageToTilesImgViewForm},
  FormImageToTilesMapView in '..\Source\FormImageToTilesMapView.pas' {ImageToTilesMapViewForm},
  FormSegmentAllocEdit in '..\Source\FormSegmentAllocEdit.pas' {SegmentAllocEditForm},
  FormDebugOutput in '..\Source\FormDebugOutput.pas' {DebugOutputForm},
  FormManagePalette in '..\Source\FormManagePalette.pas' {ManagePaletteForm},
  ImageToTilesTypes in '..\Source\ImageToTilesTypes.pas',
  ImageToTilesEngine in '..\Source\ImageToTilesEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageToTilesMainForm, ImageToTilesMainForm);
  Application.CreateForm(TSegmentAllocEditForm, SegmentAllocEditForm);
  Application.CreateForm(TManagePaletteForm, ManagePaletteForm);
  Application.Run;
end.
