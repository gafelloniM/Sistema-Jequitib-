# Sistema Jequitibá - Sistema de Gestão Ambiental (SGA) 🌳
[![DOI](https://zenodo.org/badge/1309079055.svg)](https://doi.org/10.5281/zenodo.21498029)


O **Sistema Jequitibá** é uma aplicação analítica e espacial desenvolvida em R/Shiny para otimizar, automatizar e auditar processos de licenciamento ambiental. O foco principal da ferramenta é a gestão de inventários florestais georreferenciados e o cálculo automatizado de compensação de passivos, operando em estrita conformidade com a **Resolução SEMIL nº 02/2024** do Estado de São Paulo.

## 📌 Funcionalidades Principais

* **Gestão de Portfólio:** Controle centralizado de múltiplos projetos, propriedades e polígonos de supressão vegetal.
* **Cálculo de Volumetria (Cubagem):** Inserção manual ou importação em lote (via planilha `.xlsx`) de dados de campo (DAP, CAP, Altura). O sistema processa automaticamente a conversão de circunferência para diâmetro e calcula o volume de madeira (m³) utilizando Fator de Forma customizável.
* **Mapeamento Interativo:** Geração de mapas cartográficos georreferenciados para visualização espacial dos indivíduos arbóreos, destacando visualmente espécies exóticas e ameaçadas de extinção.
* **Simulador Legal (Compliance):** Motor de auditoria que cruza a área de supressão solicitada com as características da vegetação (estágio sucessional e prioridade da bacia) para calcular o **déficit compensatório de área equivalente**, aplicando atenuantes e agravantes matemáticos.
* **Emissão de Laudos:** Geração automática de relatórios técnicos exportáveis nos formatos PDF (via HTML) e DOCX.
* **Persistência de Dados Local:** Sistema integrado de salvamento de sessões (`.rds`), protegendo contra perdas de dados e permitindo backup estruturado de todo o banco relacional.

## 🚀 Requisitos e Execução Local

O Sistema Jequitibá foi encapsulado utilizando a arquitetura **R Portable Framework**. Isso significa que a aplicação é autossuficiente e não exige a instalação prévia da linguagem R, do RStudio ou de bibliotecas por parte do usuário final.

### Como Executar (Ambiente Windows):

1. Faça o download/extração da pasta raiz do sistema em sua máquina local.
2. Não altere a estrutura de pastas internas (onde o motor do R Portable está alocado).
3. Dê um duplo clique no arquivo executável `Iniciar_Sistema.bat` (ou `.vbs` / `.exe` correspondente configurado).
4. O servidor local será iniciado em segundo plano e a interface gráfica (UI) do sistema será aberta automaticamente em seu navegador padrão.

### Execução em Ambiente de Desenvolvimento (Linux/Mac):
Caso deseje rodar ou debugar o código-fonte cru via terminal, utilize o script executável:
```bash
chmod +x sga_jequitiba.R
./sga_jequitiba.R

### 1. Dependências

Certifique-se de ter o R instalado. Execute o comando abaixo para instalar todos os pacotes necessários:

```R
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "dplyr", "tidyr", "DT", "readxl", "writexl", "rmarkdown", "leaflet"))

🏗️ Arquitetura de Dados
O sistema utiliza armazenamento relacional local persistido no arquivo sga_dados_locais.rds. Esse modelo foi projetado para operações "single-user" e garante autonomia offline ao operador técnico. Operações de deploy em servidores web (nuvem) com múltiplos acessos simultâneos exigirão a migração desta camada lógica para um SGBD robusto (ex: PostgreSQL).

👥 Autoria e Desenvolvimento
Este sistema foi concebido e estruturado pela equipe da Scientia Ambiental:

Gabriel F. M. do Rosário - Engenharia de Dados & Especialista Ambiental

Daiane R. de Barros - Consultoria & Compliance

Rafael Caracho - Consultoria & Operações

⚠️ Aviso Legal
Os resultados matemáticos e espaciais gerados por este sistema servem exclusivamente como ferramentas de auditoria prévia (Due Diligence). A aprovação final de volumes de supressão, passivos ambientais e multiplicadores de área é de competência exclusiva do órgão ambiental avaliador (CETESB / SEMIL / Prefeituras). O uso das informações geradas é de inteira responsabilidade dos técnicos assinantes do respectivo laudo (ART/TRT).

📄 Licença e Direitos Autorais
© 2026 Scientia Ambiental. Todos os direitos reservados.

Este software e seu código-fonte são de propriedade intelectual da Scientia Ambiental. A cópia, distribuição, modificação ou uso comercial não autorizado deste material é estritamente proibida sem o consentimento prévio e por escrito dos autores.
(Nota: Se o repositório for Open Source, substitua este texto por "Distribuído sob a licença MIT. Veja o arquivo LICENSE para mais detalhes.")
