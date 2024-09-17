# VIACEP (Delphi 11)
Projeto de Integração FireBird e API de CEP para Gestão de Logradouros

# Projeto de Integração FireBird e API de CEP para Gestão de Logradouros

## Descrição
Este projeto visa integrar uma aplicação com um banco de dados **FireBird** e uma **API de CEP**, permitindo a busca e gestão de dados de logradouros. O sistema permite buscar endereços por **CEP** ou pelo conjunto de informações **UF, Cidade e Endereço**, armazenando ou atualizando esses dados no banco de dados local.

## Funcionalidades
- **Busca por CEP**: Consulta dados de logradouros através da API utilizando o CEP como critério.
- **Busca por UF, Cidade e Endereço**: Permite consultar logradouros utilizando um conjunto de informações detalhadas.
- **Validação de entradas**:
  - Valida CEP com exatamente 8 caracteres.
  - Valida que UF, Cidade e Endereço têm ao menos 3 caracteres (exceto UF).
- **Armazenamento e Atualização no Banco de Dados**:
  - Se o logradouro já existe no banco, ele é atualizado.
  - Se não existir, o novo registro é inserido.
- **Exibição e Gerenciamento de Registros**: 
  - Interface para visualização de todos os logradouros cadastrados.
  - Exclusão de registros individuais.

## Tecnologias Utilizadas
- **Linguagem**: Delphi
- **Banco de Dados**: FireBird
- **API Externa**: API de CEP
- **Componentes**: 
  - `TFDQuery` para manipulação de dados
  - `IXMLDocument` para tratamento de XML

## Instalação

### Pré-requisitos
- Delphi 11 instalado (com suporte a FireDAC para conexão com FireBird).
- Banco de dados FireBird configurado.
- Acesso à API de CEP (configurar endpoint).

### Passos para Configuração
1. Clone este repositório:
   ```bash
   git clone https://github.com/seu-usuario/nome-do-projeto.git
