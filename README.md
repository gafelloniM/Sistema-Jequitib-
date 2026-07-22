# Sistema Jequitibá - Sistema de Gestão Ambiental (SGA) 🌳

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21498244.svg)](https://doi.org/10.5281/zenodo.21498244)


O **Sistema Jequitibá** é uma aplicação analítica e espacial portátil desenvolvida em R/Shiny para otimizar, automatizar e auditar processos de licenciamento ambiental. O foco principal da ferramenta é a gestão de inventários florestais georreferenciados e o cálculo automatizado de compensação de passivos, operando em estrita conformidade com a **Resolução SEMIL nº 02/2024** do Estado de São Paulo.

## 📌 Funcionalidades Principais

* **Gestão de Portfólio:** Controle centralizado de múltiplos projetos, propriedades e polígonos de supressão vegetal.
* **Cálculo de Volumetria (Cubagem):** Inserção manual ou importação em lote (via planilha `.xlsx`) de dados de campo (DAP, CAP, Altura). O sistema processa automaticamente a conversão de circunferência para diâmetro e calcula o volume de madeira (m³) utilizando Fator de Forma customizável.
* **Mapeamento Interativo:** Geração de mapas cartográficos georreferenciados (Leaflet) para visualização espacial dos indivíduos arbóreos, destacando visualmente espécies exóticas e ameaçadas de extinção.
* **Simulador Legal (Compliance):** Motor de auditoria que cruza a área de supressão solicitada com as características da vegetação (estágio sucessional e prioridade da bacia) para calcular o **déficit compensatório de área equivalente**, aplicando atenuantes e agravantes matemáticos.
* **Emissão de Laudos:** Geração automática de relatórios técnicos exportáveis nos formatos PDF (via HTML) e DOCX.
* **Persistência de Dados Local:** Sistema integrado de salvamento de sessões (`.rds`), protegendo contra perdas de dados e permitindo backup estruturado de todo o banco relacional.

## 🚀 Requisitos e Execução Local

O Sistema Jequitibá foi encapsulado utilizando a arquitetura **R Portable Framework**. Isso significa que a aplicação é autossuficiente e não exige a instalação prévia da linguagem R, do RStudio ou de bibliotecas por parte do usuário final.

### Como Executar (Ambiente Windows)
1. Faça o download ou extração da pasta raiz do sistema em sua máquina local.
2. Não altere a estrutura de pastas internas (onde o motor do R Portable está alocado).
3. Dê um duplo clique no arquivo executável `Iniciar_Sistema.bat` (ou atalho correspondente na raiz do projeto).
4. O servidor local será iniciado em segundo plano e a interface gráfica (UI) do sistema será aberta automaticamente em seu navegador padrão.

### Execução em Ambiente de Desenvolvimento (Linux/Mac)
Caso deseje rodar o código-fonte cru via terminal, utilize o script executável:
```bash
chmod +x sga_jequitiba.R
./sga_jequitiba.R
🏗️ Arquitetura de Dados
O sistema utiliza armazenamento relacional local persistido no arquivo sga_dados_locais.rds. Esse modelo foi projetado para operações "single-user" e garante autonomia offline ao operador técnico em campo. Operações de deploy em servidores web (nuvem) com múltiplos acessos simultâneos exigirão a migração desta camada lógica para um SGBD robusto (ex: PostgreSQL).

📚 Como Citar
Se você utilizar este software em relatórios técnicos, laudos ou trabalhos acadêmicos, por favor, referencie-o através do DOI oficial fixado no topo deste documento ou consulte os metadados bibliográficos estruturados no arquivo CITATION.cff incluído na raiz deste repositório.

👥 Autoria e Desenvolvimento
Este sistema foi concebido e estruturado pela equipe da Scientia Ambiental:

Gabriel F. M. do Rosário - Engenharia de Dados & Especialista Ambiental

Daiane R. de Barros - Consultoria & Compliance

Rafael Caracho - Consultoria & Operações
