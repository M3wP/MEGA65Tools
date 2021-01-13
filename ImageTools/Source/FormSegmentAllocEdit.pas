unit FormSegmentAllocEdit;

{$MODE DELPHI}
{$H+}

interface

uses
    Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics,
    Dialogs, StdCtrls, ImageToTilesTypes;

type
    { TSegmentAllocEditForm }

    TSegmentAllocEditForm = class(TForm)
        Button1: TButton;
        Button10: TButton;
        Button11: TButton;
        Button12: TButton;
        Button14: TButton;
        Button15: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Button6: TButton;
        Button7: TButton;
        Button8: TButton;
        Button9: TButton;
        Label1: TLabel;
        VirtualStringTree1: TVirtualStringTree;
        procedure Button10Click(Sender: TObject);
        procedure Button11Click(Sender: TObject);
        procedure Button12Click(Sender: TObject);
        procedure Button14Click(Sender: TObject);
        procedure Button15Click(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure Button6Click(Sender: TObject);
        procedure Button7Click(Sender: TObject);
        procedure Button8Click(Sender: TObject);
        procedure Button9Click(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
            var CellText: String);
        procedure VirtualStringTree1KeyPress(Sender: TObject; var Key: char);
    private
        FTryOK: Boolean;
        FIsOK: Boolean;
        FScreenReqd: Boolean;

        function  GetScreenReqd: Boolean;
        procedure SetScreenReqd(const AValue: Boolean);

    public
        MemorySegs: TMemModuleSegs;
        ScreenSegs: Integer;
        TileSegs: Integer;
        ScreenRAM: Integer;
        ColourRAM: Integer;
        Buffer: Integer;
        Palette: Integer;
        RelocSource,
        RelocSourceSz,
        RelocDest,
        RelocDestSz: Integer;

        procedure InitSegmentMap;
        function  EditMapping: Boolean;

        property  ScreenReqd: Boolean read GetScreenReqd write SetScreenReqd;
    end;

var
    SegmentAllocEditForm: TSegmentAllocEditForm;

implementation

{$R *.lfm}

{ TSegmentAllocEditForm }

procedure TSegmentAllocEditForm.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
        Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
        var CellText: String);
    begin
    if  Column = 0 then
        CellText:= Format('$%4.4X', [Node^.Index * $0400])
    else
        CellText:= MemorySegs[Node^.Index].Name;
    end;

procedure TSegmentAllocEditForm.VirtualStringTree1KeyPress(Sender: TObject; var Key: char);
    begin
    if  Key in ['F', 'f'] then
        Button4Click(Sender)
    else if Key in ['R', 'r'] then
        Button5Click(Sender)
    else if Key in ['S', 's'] then
        Button6Click(Sender)
    else if Key in ['C', 'c'] then
        Button9Click(Sender)
    else if Key in ['B', 'b'] then
        Button7Click(Sender)
    else if Key in ['P', 'p'] then
        Button12Click(Sender)
    else if Key in ['L', 'l'] then
        Button14Click(Sender)
    else if Key in ['D', 'd'] then
        Button15Click(Sender);
    end;

function TSegmentAllocEditForm.GetScreenReqd: Boolean;
    begin
    Result:= FScreenReqd;
    end;

procedure TSegmentAllocEditForm.SetScreenReqd(const AValue: Boolean);
    var
    i: Integer;

    begin
    if  not AValue then
        for i:= 0 to High(MemorySegs) do
            if  MemorySegs[i].Kind in [mmkScreenRAM, mmkColourRAM] then
                begin
                MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
                MemorySegs[i].Kind:= mmkFree;
                MemorySegs[i].Start:= i * $0400;
                MemorySegs[i].Segments:= 1;
                end;

    Button6.Enabled:= AValue;
    Button9.Enabled:= AValue;

    FScreenReqd:= AValue;
    end;

procedure TSegmentAllocEditForm.InitSegmentMap;
    var
    i: Integer;

    begin
    for i:= 0 to 127 do
        begin
        MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
        MemorySegs[i].Kind:= mmkFree;
        MemorySegs[i].Start:= i * $0400;
        MemorySegs[i].Segments:= 1;
        end;

    for i:= 128 to 191 do
        begin
        MemorySegs[i].Name:= LIT_TOK_MEMMOD_UNV;
        MemorySegs[i].Kind:= mmkUnavailable;
        MemorySegs[i].Start:= i * $0400;
        MemorySegs[i].Segments:= 1;
        end;

    MemorySegs[REC_MEMMOD_SYSTEMLO.Start div $0400]:= REC_MEMMOD_SYSTEMLO;
    MemorySegs[REC_MEMMOD_SYSTEMHI.Start div $0400]:= REC_MEMMOD_SYSTEMHI;

//  for i:= 0 to REC_MEMMOD_SYSTEMIO.Segments - 1 do
//      MemorySegs[REC_MEMMOD_SYSTEMIO.Start div $0400 + i]:= REC_MEMMOD_SYSTEMIO;

    for i:= 0 to REC_MEMMOD_SYSCLRRM.Segments - 1 do
        MemorySegs[REC_MEMMOD_SYSCLRRM.Start div $0400 + i]:= REC_MEMMOD_SYSCLRRM;

    if  ScreenReqd then
        begin
        for i:= 0 to ScreenSegs - 1 do
            MemorySegs[REC_MEMMOD_SCREENRM.Start div $0400 + i]:= REC_MEMMOD_SCREENRM;

        ScreenRAM:= REC_MEMMOD_SCREENRM.Start div $0400;
        end
    else
        begin
        ScreenRAM:= -1;
//      ScreenSegs:= 0;
        end;

    for i:= 0 to REC_MEMMOD_LDBUFFER.Segments - 1 do
        MemorySegs[REC_MEMMOD_SCREENRM.Start div $0400 + ScreenSegs + i]:= REC_MEMMOD_LDBUFFER;

    Buffer:= REC_MEMMOD_SCREENRM.Start div $0400 + ScreenSegs;

    VirtualStringTree1.RootNodeCount:= Length(MemorySegs);
    end;

function TSegmentAllocEditForm.EditMapping: Boolean;
    begin
    ShowModal;

    Result:= FIsOK;
    end;

procedure TSegmentAllocEditForm.FormCreate(Sender: TObject);
    begin
    ScreenSegs:= 2;
    ColourRAM:= -1;
    Palette:= -1;
    RelocSource:= -1;
    RelocSourceSz:= 0;
    RelocDest:= -1;
    RelocDestSz:= 0;

    InitSegmentMap;
    end;

procedure TSegmentAllocEditForm.Button1Click(Sender: TObject);
    var
    i: Integer;

    begin
    for i:= 2 to 39 do
        if  MemorySegs[i].Kind = mmkFree then
            begin
            MemorySegs[i].Name:= LIT_TOK_MEMMOD_RSV;
            MemorySegs[i].Kind:= mmkReserved;
            MemorySegs[i].Start:= i * $0400;
            MemorySegs[i].Segments:= 1;
            end;

    VirtualStringTree1.Invalidate;
    end;

procedure TSegmentAllocEditForm.Button10Click(Sender: TObject);
    begin
    FTryOK:= True;
    Close;
    end;

procedure TSegmentAllocEditForm.Button11Click(Sender: TObject);
    begin
    FTryOK:= False;
    FIsOK:= False;
    Close;
    end;

procedure TSegmentAllocEditForm.Button12Click(Sender: TObject);
    var
    i,
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        begin
        if  not (MemorySegs[s].Kind in [mmkFree, mmkUnavailable]) then
            Exit;

        for i:= 0 to High(MemorySegs) do
            if  MemorySegs[i].Kind = mmkPalette then
                if  i < 128 then
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
                    MemorySegs[i].Kind:= mmkFree;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end
                else
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_UNV;
                    MemorySegs[i].Kind:= mmkUnavailable;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end;

        MemorySegs[s].Name:= LIT_TOK_MEMMOD_PAL;
        MemorySegs[s].Kind:= mmkPalette;
        MemorySegs[s].Start:= i * $0400;
        MemorySegs[s].Segments:= 1;

        Palette:= s;

        VirtualStringTree1.Invalidate;
        end;
    end;

procedure TSegmentAllocEditForm.Button14Click(Sender: TObject);
    var
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        begin
        if  not (MemorySegs[s].Kind in [mmkFree, mmkUnavailable]) then
            Exit;

        if  RelocSource > -1 then
            begin
            if  s > (RelocSource + RelocSourceSz) then
                Exit;

            if  s < (RelocSource - 1) then
                Exit;

            if  s < RelocSource then
                RelocSource:= s;

            Inc(RelocSourceSz);
            end
        else
            begin
            RelocSource:= s;
            RelocSourceSz:= 1;
            end;

        MemorySegs[s].Kind:= mmkRelocSource;
        MemorySegs[s].Name:= LIT_TOK_MEMMOD_RLS;
        MemorySegs[s].Start:= s * $0400;
        MemorySegs[s].Segments:= 1;

        VirtualStringTree1.Invalidate;
        end;
    end;

procedure TSegmentAllocEditForm.Button15Click(Sender: TObject);
    var
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        begin
        if  not (MemorySegs[s].Kind in [mmkFree, mmkUnavailable]) then
            Exit;

        if  RelocDest > -1 then
            begin
            if  s > (RelocDest + RelocDestSz) then
                Exit;

            if  s < (RelocDest - 1) then
                Exit;

            if  s < RelocDest then
                RelocDest:= s;

            Inc(RelocDestSz);
            end
        else
            begin
            RelocDest:= s;
            RelocDestSz:= 1;
            end;

        MemorySegs[s].Kind:= mmkRelocDest;
        MemorySegs[s].Name:= LIT_TOK_MEMMOD_RLD;
        MemorySegs[s].Start:= s * $0400;
        MemorySegs[s].Segments:= 1;

        VirtualStringTree1.Invalidate;
        end;
    end;

procedure TSegmentAllocEditForm.Button2Click(Sender: TObject);
    var
    i: Integer;

    begin
    for i:= 48 to 51 do
        if  MemorySegs[i].Kind = mmkFree then
            begin
            MemorySegs[i].Name:= LIT_TOK_MEMMOD_RSV;
            MemorySegs[i].Kind:= mmkReserved;
            MemorySegs[i].Start:= i * $0400;
            MemorySegs[i].Segments:= 1;
            end;

    VirtualStringTree1.Invalidate;
    end;

procedure TSegmentAllocEditForm.Button3Click(Sender: TObject);
    var
    i: Integer;

    begin
    for i:= 0 to 127 do
        begin
        MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
        MemorySegs[i].Kind:= mmkFree;
        MemorySegs[i].Start:= i * $0400;
        MemorySegs[i].Segments:= 1;
        end;
    for i:= 128 to 191 do
        begin
        MemorySegs[i].Name:= LIT_TOK_MEMMOD_UNV;
        MemorySegs[i].Kind:= mmkUnavailable;
        MemorySegs[i].Start:= i * $0400;
        MemorySegs[i].Segments:= 1;
        end;

    MemorySegs[REC_MEMMOD_SYSTEMLO.Start div $0400]:= REC_MEMMOD_SYSTEMLO;
    MemorySegs[REC_MEMMOD_SYSTEMHI.Start div $0400]:= REC_MEMMOD_SYSTEMHI;

//  for i:= 0 to REC_MEMMOD_SYSTEMIO.Segments - 1 do
//      MemorySegs[REC_MEMMOD_SYSTEMIO.Start div $0400 + i]:= REC_MEMMOD_SYSTEMIO;

    for i:= 0 to REC_MEMMOD_SYSCLRRM.Segments - 1 do
        MemorySegs[REC_MEMMOD_SYSCLRRM.Start div $0400 + i]:= REC_MEMMOD_SYSCLRRM;

    ScreenRAM:= -1;
    ColourRAM:= -1;
    Buffer:= -1;
    RelocSource:= -1;
    RelocSourceSz:= 0;
    RelocDest:= -1;
    RelocDestSz:= 0;

    VirtualStringTree1.Invalidate;
    end;

procedure TSegmentAllocEditForm.Button4Click(Sender: TObject);
    var
    s: Integer;
    n: PVirtualNode;

    procedure DoClearKind(const AKind: TMemModKind);
        var
        i: Integer;

        begin
        for i:= 0 to High(MemorySegs) do
            if  MemorySegs[i].Kind = AKind then
                if  i < 128 then
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
                    MemorySegs[i].Kind:= mmkFree;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end
                else
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_UNV;
                    MemorySegs[i].Kind:= mmkUnavailable;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end;
        end;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        case MemorySegs[s].Kind of
            mmkReserved:
                begin
                MemorySegs[s].Name:= LIT_TOK_MEMMOD_FRE;
                MemorySegs[s].Kind:= mmkFree;
                MemorySegs[s].Start:= s * $0400;
                MemorySegs[s].Segments:= 1;
                end;
            mmkColourRAM:
                begin
                DoClearKind(mmkColourRAM);
                ColourRAM:= -1;
                end;
            mmkScreenRAM:
                begin
                DoClearKind(mmkScreenRAM);
                ScreenRAM:= -1;
                end;
            mmkBuffer:
                begin
                DoClearKind(mmkBuffer);
                Buffer:= -1;
                end;
            mmkPalette:
                begin
                DoClearKind(mmkPalette);
                Palette:= -1;
                end;
            mmkRelocSource:
                begin
                DoClearKind(mmkRelocSource);
                RelocSource:= -1;
                RelocSourceSz:= 0;
                end;
            mmkRelocDest:
                begin
                DoClearKind(mmkRelocDest);
                RelocDest:= -1;
                RelocDestSz:= 0;
                end;
            end;

    VirtualStringTree1.Invalidate;
    end;

procedure TSegmentAllocEditForm.Button5Click(Sender: TObject);
    var
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        if  MemorySegs[s].Kind = mmkFree then
            begin
            MemorySegs[s].Name:= LIT_TOK_MEMMOD_RSV;
            MemorySegs[s].Kind:= mmkReserved;
            MemorySegs[s].Start:= s * $0400;
            MemorySegs[s].Segments:= 1;
            end;

    VirtualStringTree1.Invalidate;
    end;

procedure TSegmentAllocEditForm.Button6Click(Sender: TObject);
    var
    i,
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        begin
        for i:= s to s + ScreenSegs - 1 do
            if  not (MemorySegs[i].Kind in [mmkScreenRAM, mmkFree, mmkUnavailable]) then
                Exit;

        for i:= 0 to High(MemorySegs) do
            if  MemorySegs[i].Kind = mmkScreenRAM then
                if  i < 128 then
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
                    MemorySegs[i].Kind:= mmkFree;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end
                else
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_UNV;
                    MemorySegs[i].Kind:= mmkUnavailable;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end;

        for i:= s to s + ScreenSegs - 1 do
                MemorySegs[i]:= REC_MEMMOD_SCREENRM;

        ScreenRAM:= s;

        VirtualStringTree1.Invalidate;
        end;
    end;

procedure TSegmentAllocEditForm.Button7Click(Sender: TObject);
    var
    i,
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  (s > -1)
    and (s < 52) then
        begin
        for i:= s to s + REC_MEMMOD_LDBUFFER.Segments - 1 do
            if  not (MemorySegs[i].Kind in [mmkBuffer, mmkFree]) then
                Exit;

        for i:= 0 to 127 do
            if  MemorySegs[i].Kind = mmkBuffer then
                begin
                MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
                MemorySegs[i].Kind:= mmkFree;
                MemorySegs[i].Start:= i * $0400;
                MemorySegs[i].Segments:= 1;
                end;

        for i:= s to s + REC_MEMMOD_LDBUFFER.Segments - 1 do
            MemorySegs[i]:= REC_MEMMOD_LDBUFFER;

        Buffer:= s;

        VirtualStringTree1.Invalidate;
        end
    else
        ShowMessage('Load Buffer cannot be placed in System IO or Kernel areas '+
                'and must be in the first 64kB of RAM!');
    end;

procedure TSegmentAllocEditForm.Button8Click(Sender: TObject);
    var
    i: Integer;

    begin
    for i:= 63 to 71 do
        if  MemorySegs[i].Kind = mmkFree then
            begin
            MemorySegs[i].Name:= LIT_TOK_MEMMOD_RSV;
            MemorySegs[i].Kind:= mmkReserved;
            MemorySegs[i].Start:= i * $0400;
            MemorySegs[i].Segments:= 1;
            end;

    VirtualStringTree1.Invalidate;
    end;

procedure TSegmentAllocEditForm.Button9Click(Sender: TObject);
    var
    i,
    s: Integer;
    n: PVirtualNode;

    begin
    s:= -1;
    n:= VirtualStringTree1.GetFirst;
    while  Assigned(n) do
        begin
        if  VirtualStringTree1.Selected[n] then
            begin
            s:= n^.Index;
            Break;
            end;

        n:= VirtualStringTree1.GetNext(n);
        end;

    if  s > -1 then
        begin
        for i:= s to s + ScreenSegs - 1 do
            if  not (MemorySegs[i].Kind in [mmkColourRAM, mmkFree, mmkUnavailable]) then
                Exit;

        for i:= 0 to High(MemorySegs) do
            if  MemorySegs[i].Kind = mmkColourRAM then
                if  i < 128 then
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_FRE;
                    MemorySegs[i].Kind:= mmkFree;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end
                else
                    begin
                    MemorySegs[i].Name:= LIT_TOK_MEMMOD_UNV;
                    MemorySegs[i].Kind:= mmkUnavailable;
                    MemorySegs[i].Start:= i * $0400;
                    MemorySegs[i].Segments:= 1;
                    end;

        for i:= s to s + ScreenSegs - 1 do
            begin
            MemorySegs[i].Name:= LIT_TOK_MEMMOD_CLR;
            MemorySegs[i].Kind:= mmkColourRAM;
            MemorySegs[i].Start:= i * $0400;
            MemorySegs[i].Segments:= ScreenSegs;
            end;

        ColourRAM:= s;

        VirtualStringTree1.Invalidate;
        end;
    end;

procedure TSegmentAllocEditForm.FormCloseQuery(Sender: TObject;
        var CanClose: boolean);
    var
    i,
    c: Integer;

    begin
    CanClose:= True;

    if  FTryOK then
        begin
        if  FScreenReqd then
            if  (SegmentAllocEditForm.ScreenRAM = -1) then
                begin
                ShowMessage('The Screen RAM must be mapped!');
                CanClose:= False;
                end;

        if  (SegmentAllocEditForm.Buffer = -1) then
            begin
            ShowMessage('The Load Buffer must be mapped!');
            CanClose:= False;
            end;

        c:= 0;
        for i:= 0 to High(MemorySegs) do
            if  MemorySegs[i].Kind in [mmkFree, mmkRelocDest] then
                Inc(c);

        if  c < TileSegs then
            begin
            ShowMessage('There are not enough free segments for the tiles!');
            CanClose:= False;
            end;

        if  FScreenReqd
        and (ScreenSegs > 4)
        and (ColourRAM = -1) then
            begin
            ShowMessage('The temporary Colour RAM buffer must be mapped!');
            CanClose:= False;
            end;

        if  RelocSource > -1 then
            if  (RelocDest = -1)
            or  (RelocSourceSz <> RelocDestSz) then
                begin
                ShowMessage('The Relocation Source and Destination areas must be the same size!');
                CanClose:= False;
                end;

        FIsOK:= CanClose;
        end;
    end;

end.

