# VIACEP (Delphi 11)
Projeto de Integração FireBird e API de CEP para Gestão de Logradouros (Teste aplicado em entrevista)

[![GitHub Contributors](https://img.shields.io/github/contributors/ThiagoIrrazabal/VIACEP)](https://github.com/ThiagoIrrazabal/VIACEP/graphs/contributors)
[![GitHub Forks](https://img.shields.io/github/forks/ThiagoIrrazabal/VIACEP?style=social)](https://github.com/ThiagoIrrazabal/VIACEP/network/members)
[![GitHub Issues](https://img.shields.io/github/issues/ThiagoIrrazabal/VIACEP)](https://github.com/ThiagoIrrazabal/VIACEP/issues)
[![GitHub Stars](https://img.shields.io/github/stars/ThiagoIrrazabal/VIACEP?style=social)](https://github.com/ThiagoIrrazabal/VIACEP/stargazers)
[![GitHub License](https://img.shields.io/github/license/ThiagoIrrazabal/VIACEP)](https://github.com/ThiagoIrrazabal/VIACEP/blob/main/LICENSE)

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
  - `TJSONObject` para tratamento de JSON 

## Instalação

### Pré-requisitos
- Delphi 11 instalado (com suporte a FireDAC para conexão com FireBird).
- Banco de dados FireBird configurado.
- Acesso à API de CEP (configurar endpoint).

### Passos para Configuração
1. Clone este repositório:
   ```bash
   git clone https://github.com/ThiagoIrrazabal/VIACEP.git
