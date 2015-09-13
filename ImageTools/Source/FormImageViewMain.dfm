object ImageViewMainForm: TImageViewMainForm
  Left = 0
  Top = 0
  Caption = 'MEGA65 Image Viewer'
  ClientHeight = 597
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 637
    Height = 597
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
  object MainMenu1: TMainMenu
    Left = 552
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = ActFileOpen
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ActFileExit
      end
    end
    object View1: TMenuItem
      Caption = '&View'
    end
    object Help1: TMenuItem
      Caption = '&Help'
    end
  end
  object ActionList1: TActionList
    Left = 552
    Top = 52
    object ActFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = ActFileOpenExecute
    end
    object ActFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      OnExecute = ActFileExitExecute
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.v4t'
    Filter = 'VIC-IV Tile Images (*.v4t)|*.v4t|Any File (*.*)|*.*'
    Options = [ofReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Open Image...'
    Left = 552
    Top = 100
  end
end
