object ImageToTilesMainForm: TImageToTilesMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Image To Tiles'
  ClientHeight = 325
  ClientWidth = 370
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
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 70
    Height = 13
    Caption = 'Source Image:'
  end
  object Label2: TLabel
    Left = 8
    Top = 109
    Width = 46
    Height = 13
    Caption = 'Progress:'
  end
  object Label3: TLabel
    Left = 100
    Top = 130
    Width = 12
    Height = 13
    Caption = '---'
  end
  object Label4: TLabel
    Left = 202
    Top = 130
    Width = 12
    Height = 13
    Caption = '---'
  end
  object Label5: TLabel
    Left = 100
    Top = 13
    Width = 94
    Height = 13
    Caption = 'Background Colour:'
  end
  object Label6: TLabel
    Left = 8
    Top = 181
    Width = 38
    Height = 13
    Caption = 'Output:'
  end
  object Button1: TButton
    Left = 100
    Top = 46
    Width = 75
    Height = 25
    Action = ActSourceOpen
    TabOrder = 0
  end
  object ProgressBar1: TProgressBar
    Left = 99
    Top = 107
    Width = 263
    Height = 17
    Smooth = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 181
    Top = 46
    Width = 75
    Height = 25
    Action = ActSourceClear
    TabOrder = 2
  end
  object Button3: TButton
    Left = 287
    Top = 46
    Width = 75
    Height = 25
    Action = ActSourceProcess
    TabOrder = 3
  end
  object ColorBox1: TColorBox
    Left = 217
    Top = 10
    Width = 145
    Height = 22
    Style = [cbStandardColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    TabOrder = 4
  end
  object Button4: TButton
    Left = 100
    Top = 176
    Width = 75
    Height = 25
    Action = ActOutputSave
    TabOrder = 5
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'png'
    Filter = 
      'PNG Images (*.png)|*.png|Bitmap Images (*.bmp)|*.bmp|JPEG Images' +
      ' (*.jpg)|*.jpg|Any File (*.*)|*.*'
    Options = [ofReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select Source Image...'
    Left = 332
    Top = 36
  end
  object ActionList1: TActionList
    Left = 332
    Top = 4
    object ActSourceOpen: TAction
      Category = 'Source'
      Caption = 'Open...'
      OnExecute = ActSourceOpenExecute
    end
    object ActSourceClear: TAction
      Category = 'Source'
      Caption = 'Clear'
      Enabled = False
      OnExecute = ActSourceClearExecute
    end
    object ActSourceProcess: TAction
      Category = 'Source'
      Caption = 'Process'
      Enabled = False
      OnExecute = ActSourceProcessExecute
    end
    object ActOutputSave: TAction
      Category = 'Output'
      Caption = 'Save...'
      Enabled = False
      OnExecute = ActOutputSaveExecute
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.v4t'
    Filter = 'VIC-IV Tile Image (*.v4t)|*.v4t|Any File (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofExtensionDifferent, ofPathMustExist, ofEnableSizing]
    Left = 332
    Top = 148
  end
end
