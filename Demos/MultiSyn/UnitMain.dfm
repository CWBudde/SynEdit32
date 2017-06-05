object frmMain: TfrmMain
  Left = 192
  Top = 107
  Caption = 'MultiHighlight HTML Demo'
  ClientHeight = 349
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit: TSynEdit32
    Left = 185
    Top = 0
    Width = 371
    Height = 330
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    FontSmoothing = fsmNone
    TabOrder = 0
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Width = 10
    Highlighter = SynMultiSyn
    Options = [eoAutoIndent, eoKeepCaretX, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
    WantTabs = True
    OnStatusChange = SynEditStatusChange
    RemovedKeystrokes = <
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <>
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 330
    Width = 556
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 330
    Align = alLeft
    TabOrder = 2
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 183
      Height = 81
      Align = alTop
      AutoSize = False
      Caption = 
        'Use CTRL+SPACE to bring up the Completion Proposal. Use SHIFT+SP' +
        'ACE to invoke the AutoComplete for one of the keywords below.'
      WordWrap = True
    end
    object ListBox1: TListBox
      Left = 1
      Top = 82
      Width = 183
      Height = 247
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        '!--'
        '!doct'
        'a'
        'applet'
        'frame'
        'head'
        'img'
        'link'
        'script'
        'style'
        'table'
        'td'
        'th'
        'tr'
        'ul'
        '')
      TabOrder = 0
      OnDblClick = ListBox1DblClick
    end
  end
  object MainMenu: TMainMenu
    Left = 208
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 272
    Top = 32
  end
  object SaveDialog: TSaveDialog
    Left = 344
    Top = 32
  end
  object SynCompletionProposal: TSynEdit32CompletionProposal
    NbLinesInWindow = 6
    ClSelect = clInactiveCaption
    Width = 262
    EndOfTokenChr = '()[].'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ShortCut = 16416
    Editor = SynEdit
    Left = 232
    Top = 144
  end
  object SynAutoComplete: TSynEdit32AutoComplete
    EndOfTokenChr = '()[].'
    Editor = SynEdit
    ShortCut = 8224
    Options = []
    Left = 232
    Top = 192
  end
  object SynMultiSyn: TSynEdit32HighlighterMulti
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Schemes = <
      item
        StartExpr = '<style type="text/css">'
        EndExpr = '</style>'
        Highlighter = SynCssSyn
        SchemeName = 'CSS'
      end
      item
        StartExpr = '<script language="JavaScript" type="text/javascript">'
        EndExpr = '</script>'
        Highlighter = SynJScriptSyn
        SchemeName = 'JavaScript'
      end>
    DefaultHighlighter = SynHTMLSyn
    Left = 408
    Top = 32
  end
  object SynHTMLSyn: TSynEdit32HighlighterHTML
    DefaultFilter = 'HTML Document (*.htm,*.html)|*.htm;*.html'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 408
    Top = 96
  end
  object SynCssSyn: TSynEdit32HighlighterCss
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 408
    Top = 144
  end
  object SynJScriptSyn: TSynEdit32HighlighterJScript
    DefaultFilter = 'JavaScript files (*.js)|*.js'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 408
    Top = 192
  end
end
