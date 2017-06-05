inherited SDIMainForm: TSDIMainForm
  Caption = 'Single Document Edit Demo'
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited mnuMain: TMainMenu
    inherited MenuItemFile: TMenuItem
      inherited miFileClose: TMenuItem
        Visible = False
      end
      inherited MenuItemFileCloseAll: TMenuItem
        Visible = False
      end
    end
  end
end
