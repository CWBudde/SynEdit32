object EditorForm: TEditorForm
  Left = 338
  Top = 199
  Caption = 'Editor'
  ClientHeight = 299
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SynEditor: TSynEdit32
    Left = 0
    Top = 0
    Width = 462
    Height = 299
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    FontSmoothing = fsmNone
    PopupMenu = PopupMenuEditor
    TabOrder = 0
    OnEnter = SynEditorEnter
    OnExit = SynEditorExit
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    SearchEngine = SynEditSearch
    OnChange = SynEditorChange
    OnReplaceText = SynEditorReplaceText
    OnStatusChange = SynEditorStatusChange
  end
  object PopupMenuEditor: TPopupMenu
    Left = 92
    Top = 28
    object MenuItemEditUndo: TMenuItem
      Action = CommandsDataModule.ActionEditUndo
    end
    object MenuItemEditRedo: TMenuItem
      Action = CommandsDataModule.ActionEditRedo
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItemEditCut: TMenuItem
      Action = CommandsDataModule.ActionEditCut
    end
    object MenuItemEditCopy: TMenuItem
      Action = CommandsDataModule.ActionEditCopy
    end
    object MenuItemEditPaste: TMenuItem
      Action = CommandsDataModule.ActionEditPaste
    end
    object MenuItemEditDelete: TMenuItem
      Action = CommandsDataModule.ActionEditDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItemEditSelectAll: TMenuItem
      Action = CommandsDataModule.ActionEditSelectAll
    end
  end
  object SynEditSearch: TSynEdit32Search
    Left = 92
    Top = 88
  end
end
