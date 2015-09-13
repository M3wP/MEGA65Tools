program ImageView;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  FormImageViewMain in '..\Source\FormImageViewMain.pas' {Form1},
  ImageToTilesTypes in '..\Source\ImageToTilesTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageViewMainForm, ImageViewMainForm);
  Application.Run;
end.
