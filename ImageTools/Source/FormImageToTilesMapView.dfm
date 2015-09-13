object ImageToTilesMapViewForm: TImageToTilesMapViewForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Tile Map'
  ClientHeight = 737
  ClientWidth = 1116
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DrawGrid1: TDrawGrid
    Left = 0
    Top = 0
    Width = 1116
    Height = 737
    Align = alClient
    DefaultColWidth = 40
    DefaultRowHeight = 40
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    OnDrawCell = DrawGrid1DrawCell
  end
end
