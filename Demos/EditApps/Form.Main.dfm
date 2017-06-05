object MainForm: TMainForm
  Left = 186
  Top = 133
  ClientHeight = 300
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 281
    Width = 506
    Height = 19
    Action = ActionUpdateStatusBarPanels
    Panels = <
      item
        Alignment = taCenter
        Width = 84
      end
      item
        Alignment = taCenter
        Width = 72
      end
      item
        Alignment = taCenter
        Width = 84
      end
      item
        Width = 50
      end>
  end
  object MainMenu: TMainMenu
    Left = 28
    Top = 32
    object MenuItemFile: TMenuItem
      Caption = '&File'
      OnClick = MenuItemFileClick
      object MenuItemFileNew: TMenuItem
        Action = ActionFileNew
      end
      object MenuItemFileOpen: TMenuItem
        Action = ActionFileOpen
      end
      object MenuItemRecentFiles: TMenuItem
        Caption = '&Recent Files'
        OnClick = MenuItemRecentFilesClick
        object MenuItemFileMRU1: TMenuItem
          Caption = '[MRU1]'
          OnClick = OnOpenMRUFile
        end
        object MenuItemFileMRU2: TMenuItem
          Caption = '[MRU2]'
          OnClick = OnOpenMRUFile
        end
        object MenuItemFileMRU3: TMenuItem
          Caption = '[MRU3]'
          OnClick = OnOpenMRUFile
        end
        object MenuItemFileMRU4: TMenuItem
          Caption = '[MRU4]'
          OnClick = OnOpenMRUFile
        end
        object MenuItemFileMRU5: TMenuItem
          Caption = '[MRU5]'
          OnClick = OnOpenMRUFile
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuItemFileSave: TMenuItem
        Action = CommandsDataModule.ActionFileSave
      end
      object MenuItemFileSaveAs: TMenuItem
        Action = CommandsDataModule.ActionFileSaveAs
      end
      object MenuItemFileClose: TMenuItem
        Action = CommandsDataModule.ActionFileClose
      end
      object MenuItemFileCloseAll: TMenuItem
        Action = ActionFileCloseAll
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuItemFilePrint: TMenuItem
        Action = CommandsDataModule.ActionFilePrint
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemFileExit: TMenuItem
        Action = ActionFileExit
      end
    end
    object MenuItemEdit: TMenuItem
      Caption = '&Edit'
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
      object MenuItemEditSelectAll: TMenuItem
        Action = CommandsDataModule.ActionEditSelectAll
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuItemEditFind: TMenuItem
        Action = CommandsDataModule.ActionSearchFind
      end
      object MenuItemEditFindNext: TMenuItem
        Action = CommandsDataModule.ActionSearchFindNext
      end
      object MenuItemEditFindPrev: TMenuItem
        Action = CommandsDataModule.ActionSearchFindPrev
      end
      object MenuItemEditReplace: TMenuItem
        Action = CommandsDataModule.ActionSearchReplace
      end
    end
    object MenuItemView: TMenuItem
      Caption = '&View'
      object MenuItemViewStatusbar: TMenuItem
        Action = ActionViewStatusbar
      end
    end
  end
  object ActionListStandard: TActionList
    Left = 28
    Top = 108
    object ActionFileNew: TAction
      Category = 'File'
      Caption = '&New'
      ShortCut = 16462
      OnExecute = ActionFileNewExecute
      OnUpdate = ActionFileNewOrOpenUpdate
    end
    object ActionFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = ActionFileOpenExecute
      OnUpdate = ActionFileNewOrOpenUpdate
    end
    object ActionFileCloseAll: TAction
      Category = 'File'
      Caption = 'Close All Fi&les'
      Enabled = False
      ShortCut = 24691
      OnExecute = ActionFileCloseAllExecute
      OnUpdate = ActionFileCloseAllUpdate
    end
    object ActionFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      ShortCut = 32883
      OnExecute = ActionFileExitExecute
    end
    object ActionViewStatusbar: TAction
      Category = 'View'
      Caption = '&Status Bar'
      OnExecute = ActionViewStatusbarExecute
      OnUpdate = ActionViewStatusbarUpdate
    end
    object ActionUpdateStatusBarPanels: TAction
      Caption = 'actUpdateStatusBarPanels'
      OnUpdate = ActionUpdateStatusBarPanelsUpdate
    end
  end
end
