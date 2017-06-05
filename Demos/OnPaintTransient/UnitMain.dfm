object FormMain: TFormMain
  Left = 385
  Top = 196
  Caption = 'OnPaintTransient Demo'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonOpen: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Open'
      TabOrder = 0
      OnClick = ButtonOpenClick
    end
    object Button1: TButton
      Left = 152
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Toggle Enabled'
      TabOrder = 1
      Visible = False
      OnClick = Button1Click
    end
  end
  object Editor: TSynEdit32
    Left = 0
    Top = 42
    Width = 688
    Height = 411
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
    Highlighter = SynJavaSyn
    OnPaintTransient = EditorPaintTransient
  end
  object OpenDialog: TOpenDialog
    Filter = 'Java files|*.java'
    Left = 320
    Top = 16
  end
  object SynJavaSyn: TSynEdit32HighlighterJava
    Enabled = False
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 192
    Top = 88
  end
end
