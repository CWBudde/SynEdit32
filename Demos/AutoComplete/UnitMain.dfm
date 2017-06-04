object FormAutoComplete: TFormAutoComplete
  Left = 66
  Top = 78
  Caption = 'Autocompletion demo'
  ClientHeight = 316
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit32: TSynEdit32
    Left = 0
    Top = 0
    Width = 576
    Height = 316
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    FontSmoothing = fsmNone
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn
    Lines.Strings = (
      '// Press <Ctrl+J> to invoke autocompletion, and <Ctrl+Z> to undo'
      ''
      'arrayc'
      'classf'
      'pro')
  end
  object MainMenu: TMainMenu
    Left = 276
    Top = 56
    object MenuItemNewForm: TMenuItem
      Caption = '&New form'
      OnClick = MenuItemNewFormClick
    end
  end
  object SynPasSyn: TSynEdit32HighlighterPas
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 278
    Top = 104
  end
end
