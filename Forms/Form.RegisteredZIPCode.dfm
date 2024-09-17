object FormRegisteredZIPCode: TFormRegisteredZIPCode
  Left = 0
  Top = 0
  Caption = 'Cadastro de CEP'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object dbgCEP: TDBGrid
    Left = 0
    Top = 0
    Width = 628
    Height = 442
    Align = alClient
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    OnCellClick = dbgCEPCellClick
    OnDrawColumnCell = dbgCEPDrawColumnCell
    OnMouseMove = dbgCEPMouseMove
    Columns = <
      item
        Expanded = False
        FieldName = 'CEP'
        Width = 60
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LOGRADOURO'
        Title.Caption = 'Logradouro'
        Width = 130
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COMPLEMENTO'
        Title.Caption = 'Complemento'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BAIRRO'
        Title.Caption = 'Bairro'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'UF'
        Width = 30
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LOCALIDADE'
        Title.Caption = 'Localidade'
        Width = 100
        Visible = True
      end
      item
        Alignment = taCenter
        Color = 5197823
        Expanded = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        Title.Caption = 'A'#231#227'o'
        Width = 60
        Visible = True
      end>
  end
end
