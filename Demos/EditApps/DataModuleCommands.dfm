object CommandsDataModule: TCommandsDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 276
  Width = 420
  object DialogFileOpen: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 20
    Top = 16
  end
  object ActionListMain: TActionList
    Left = 20
    Top = 168
    object ActionFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      Enabled = False
      ShortCut = 16467
      OnExecute = ActionFileSaveExecute
      OnUpdate = ActionFileSaveUpdate
    end
    object ActionFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save &As...'
      Enabled = False
      OnExecute = ActionFileSaveAsExecute
      OnUpdate = ActionFileSaveAsUpdate
    end
    object ActionFileClose: TAction
      Category = 'File'
      Caption = '&Close'
      Enabled = False
      ShortCut = 16499
      OnExecute = ActionFileCloseExecute
      OnUpdate = ActionFileCloseUpdate
    end
    object ActionFilePrint: TAction
      Category = 'File'
      Caption = '&Print...'
      Enabled = False
      OnExecute = ActionFilePrintExecute
      OnUpdate = ActionFilePrintUpdate
    end
    object ActionEditCut: TAction
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      ShortCut = 16472
      OnExecute = ActionEditCutExecute
      OnUpdate = ActionEditCutUpdate
    end
    object ActionEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      ShortCut = 16451
      OnExecute = ActionEditCopyExecute
      OnUpdate = ActionEditCopyUpdate
    end
    object ActionEditPaste: TAction
      Category = 'Edit'
      Caption = '&Paste'
      Enabled = False
      ShortCut = 16470
      OnExecute = ActionEditPasteExecute
      OnUpdate = ActionEditPasteUpdate
    end
    object ActionEditDelete: TAction
      Category = 'Edit'
      Caption = 'De&lete'
      Enabled = False
      OnExecute = ActionEditDeleteExecute
      OnUpdate = ActionEditDeleteUpdate
    end
    object ActionEditUndo: TAction
      Category = 'Edit'
      Caption = '&Undo'
      Enabled = False
      ShortCut = 16474
      OnExecute = ActionEditUndoExecute
      OnUpdate = ActionEditUndoUpdate
    end
    object ActionEditRedo: TAction
      Category = 'Edit'
      Caption = '&Redo'
      Enabled = False
      ShortCut = 24666
      OnExecute = ActionEditRedoExecute
      OnUpdate = ActionEditRedoUpdate
    end
    object ActionEditSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select &All'
      Enabled = False
      ShortCut = 16449
      OnExecute = ActionEditSelectAllExecute
      OnUpdate = ActionEditSelectAllUpdate
    end
    object ActionSearchFind: TAction
      Category = 'Search'
      Caption = '&Find...'
      Enabled = False
      ShortCut = 16454
      OnExecute = ActionSearchFindExecute
      OnUpdate = ActionSearchFindUpdate
    end
    object ActionSearchFindNext: TAction
      Category = 'Search'
      Caption = 'Find &Next'
      Enabled = False
      ShortCut = 114
      OnExecute = ActionSearchFindNextExecute
      OnUpdate = ActionSearchFindNextUpdate
    end
    object ActionSearchFindPrev: TAction
      Category = 'Search'
      Caption = 'Find &Previous'
      Enabled = False
      ShortCut = 8306
      OnExecute = ActionSearchFindPrevExecute
      OnUpdate = ActionSearchFindPrevUpdate
    end
    object ActionSearchReplace: TAction
      Category = 'Search'
      Caption = '&Replace...'
      Enabled = False
      ShortCut = 16456
      OnExecute = ActionSearchReplaceExecute
      OnUpdate = ActionSearchReplaceUpdate
    end
  end
  object DialogFileSave: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 20
    Top = 68
  end
  object SynCppSyn: TSynEdit32HighlighterCpp
    DefaultFilter = 'C++ files (*.cpp,*.h,*.hpp)|*.cpp;*.h;*.hpp'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 172
    Top = 12
  end
  object SynPasSyn: TSynEdit32HighlighterPas
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 252
    Top = 12
  end
  object SynSQLSyn: TSynEdit32HighlighterSQL
    DefaultFilter = 'SQL files (*.sql)|*.sql'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    SQLDialect = sqlSybase
    Left = 328
    Top = 12
  end
end
