object ImageToTilesImgViewForm: TImageToTilesImgViewForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Image'
  ClientHeight = 216
  ClientWidth = 795
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 795
    Height = 216
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
      Proportional = True
    end
  end
end
