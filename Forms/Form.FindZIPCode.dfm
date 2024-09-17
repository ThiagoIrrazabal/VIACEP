object FormFindZIPCode: TFormFindZIPCode
  Left = 0
  Top = 0
  Caption = 'Buscar CEP'
  ClientHeight = 438
  ClientWidth = 612
  Color = clWhite
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 612
    Height = 120
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 3
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 608
    object pnlCEP: TPanel
      Left = 3
      Top = 0
      Width = 609
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 0
      ExplicitWidth = 605
      object lblCEP: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 9
        Width = 110
        Height = 18
        Margins.Top = 9
        Align = alLeft
        AutoSize = False
        Caption = 'CEP.................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitTop = 8
        ExplicitHeight = 19
      end
      object edtCEP: TEdit
        AlignWithMargins = True
        Left = 119
        Top = 4
        Width = 66
        Height = 23
        Margins.Top = 4
        Align = alLeft
        MaxLength = 8
        NumbersOnly = True
        TabOrder = 0
        OnChange = edtCEPChange
      end
    end
    object pnlLogradouro: TPanel
      Left = 3
      Top = 30
      Width = 609
      Height = 90
      Align = alClient
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 1
      ExplicitWidth = 605
      object pnlUF: TPanel
        Left = 0
        Top = 0
        Width = 609
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 605
        object lblUF: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 9
          Width = 110
          Height = 18
          Margins.Top = 9
          Align = alLeft
          AutoSize = False
          Caption = 'UF...................................'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitLeft = 4
          ExplicitTop = 10
          ExplicitHeight = 27
        end
        object cbxUF: TComboBox
          AlignWithMargins = True
          Left = 119
          Top = 4
          Width = 66
          Height = 23
          Margins.Top = 4
          Align = alLeft
          Style = csDropDownList
          CharCase = ecUpperCase
          TabOrder = 0
          OnCloseUp = cbxUFCloseUp
          OnKeyDown = cbxUFKeyDown
          Items.Strings = (
            'AC'
            'AL'
            'AP'
            'AM'
            'BA'
            'CE'
            'DF'
            'ES'
            'GO'
            'MA'
            'MT'
            'MS'
            'MG'
            'PA'
            'PB'
            'PR'
            'PE'
            'PI'
            'RJ'
            'RN'
            'RS'
            'RO'
            'RR'
            'SC'
            'SP'
            'SE'
            'TO')
        end
      end
      object pnlCidade: TPanel
        Left = 0
        Top = 30
        Width = 609
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        ExplicitWidth = 605
        object lblCidade: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 9
          Width = 110
          Height = 18
          Margins.Top = 9
          Align = alLeft
          AutoSize = False
          Caption = 'Cidade.................................'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitTop = 8
          ExplicitHeight = 19
        end
        object edtCidade: TEdit
          AlignWithMargins = True
          Left = 119
          Top = 4
          Width = 483
          Height = 23
          Margins.Top = 4
          Align = alLeft
          Anchors = [akLeft, akTop, akRight, akBottom]
          CharCase = ecUpperCase
          TabOrder = 0
          OnChange = edtCidadeChange
          ExplicitWidth = 479
        end
      end
      object pnlEndereco: TPanel
        Left = 0
        Top = 60
        Width = 609
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 2
        ExplicitWidth = 605
        object lblEndereco: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 9
          Width = 110
          Height = 18
          Margins.Top = 9
          Align = alLeft
          AutoSize = False
          Caption = 'Logradouro.................................'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitTop = 8
          ExplicitHeight = 19
        end
        object edtEndereco: TEdit
          AlignWithMargins = True
          Left = 119
          Top = 4
          Width = 483
          Height = 23
          Margins.Top = 4
          Align = alLeft
          Anchors = [akLeft, akTop, akRight, akBottom]
          CharCase = ecUpperCase
          TabOrder = 0
          OnChange = edtEnderecoChange
          ExplicitWidth = 479
        end
      end
    end
  end
  object dbgCEP: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 164
    Width = 606
    Height = 271
    Align = alClient
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ReadOnly = True
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'CEP'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LOGRADOURO'
        Title.Caption = 'Logradouro'
        Width = 150
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
        Width = 35
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LOCALIDADE'
        Title.Caption = 'Localidade'
        Width = 125
        Visible = True
      end>
  end
  object pnlBotoes: TPanel
    Left = 0
    Top = 120
    Width = 612
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 3
    TabOrder = 1
    ExplicitWidth = 608
    object btnPesquisar: TSpeedButton
      AlignWithMargins = True
      Left = 498
      Top = 9
      Width = 111
      Height = 29
      Cursor = crHandPoint
      Margins.Top = 9
      Align = alRight
      Caption = 'Pesquisar'
      OnClick = btnPesquisarClick
      ExplicitLeft = 515
    end
    object rdbMetodo: TRadioGroup
      AlignWithMargins = True
      Left = 6
      Top = 3
      Width = 193
      Height = 35
      Align = alLeft
      Caption = '  M'#233'todo de Busca  '
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'JSON'
        'XML')
      TabOrder = 0
    end
  end
end
