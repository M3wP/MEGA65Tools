unit FormDebugOutput;

{$mode delphi}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TDebugOutputForm }

    TDebugOutputForm = class(TForm)
        Memo1: TMemo;
    private

    public

    end;

var
    DebugOutputForm: TDebugOutputForm;

implementation

{$R *.lfm}

end.

