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
  ImageToTilesTypes in '..\Source\ImageToTilesTypes.pas',
  ImageToTilesEngine in '..\Source\ImageToTilesEngine.pas';

{$R *.res}

begin
if  ParamCount > 0 then
	begin
	HandleCommandLineInvocation;
	Exit;
	end;

  Application.Initialize;
  Application.CreateForm(TImageToTilesMainForm, ImageToTilesMainForm);
  Application.Run;
end.
