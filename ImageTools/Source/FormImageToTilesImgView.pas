unit FormImageToTilesImgView;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;
{$ELSE}
	SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls;
{$ENDIF}

type
	TImageToTilesImgViewForm = class(TForm)
		ScrollBox1: TScrollBox;
		Image1: TImage;
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	ImageToTilesImgViewForm: TImageToTilesImgViewForm;

implementation

{$IFNDEF FPC}
	{$R *.dfm}
{$ELSE}
	{$R *.lfm}
{$ENDIF}


end.
