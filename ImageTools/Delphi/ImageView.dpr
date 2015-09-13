program ImageView;

uses
  Vcl.Forms,
  FormImageViewMain in '..\Source\FormImageViewMain.pas' {ImageViewMainForm},
  ImageToTilesTypes in '..\Source\ImageToTilesTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TImageViewMainForm, ImageViewMainForm);
  Application.Run;
end.
