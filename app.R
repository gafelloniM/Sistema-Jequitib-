#!/usr/bin/env Rscript
# Sistema de Gestão Ambiental (SGA) - Inventário Florestal e Compensação SEMIL

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(DT)
library(readxl)
library(writexl)
library(rmarkdown)
library(leaflet)

# ============================================================================
# PERSISTÊNCIA LOCAL (AUTO-SAVE CONTRA F5)
# ============================================================================
arquivo_bd_local <- "sga_dados_locais.rds"

carregar_banco <- function() {
  if (file.exists(arquivo_bd_local)) {
    return(readRDS(arquivo_bd_local))
  } else {
    return(list(
      projetos = data.frame(ID = character(), Nome = character(), Cliente = character(), Municipio = character(), Area_Total = numeric(), Area_Sup = numeric(), Estagio = character(), Prio_Sup = character(), APP = logical(), Tipologia = character(), Obs = character(), stringsAsFactors = FALSE),
      inventario = data.frame(ID_Arvore = character(), ID_Projeto = character(), Data = character(), Foto = character(), Especie = character(), CAP = numeric(), DAP = numeric(), Altura = numeric(), Ff = numeric(), Volume = numeric(), Lat = numeric(), Lon = numeric(), Ameacada = logical(), Exotica = logical(), Em_APP = logical(), stringsAsFactors = FALSE)
    ))
  }
}

salvar_banco <- function(dados_projetos, dados_inventario) {
  saveRDS(list(projetos = dados_projetos, inventario = dados_inventario), arquivo_bd_local)
}

# ============================================================================
# DICIONÁRIOS E CONSTANTES (IBGE + SEMIL 02/2024)
# ============================================================================

tipologias_ibge <- c("Não se aplica", "Floresta Ombrófila Densa", "Floresta Ombrófila Aberta", "Floresta Ombrófila Mista", "Floresta Estacional Sempre-Verde", "Floresta Estacional Semidecidual", "Floresta Estacional Decidual", "Floresta Paludosa (Especial SEMIL)", "Campinarana", "Savana", "Savana Gramíneo-Lenhosa (Cerrado Campestre)", "Savana-Estépica", "Estepe", "Restinga", "Manguezal", "Palmeiral", "Refúgios Vegetacionais", "Áreas de Tensão Ecológica (Contatos)", "Vegetação Secundária", "Floresta Plantada")

criterios_vegetacao <- data.frame(
  tipo_vegetacao = c(rep("Inicial", 4), rep("Médio", 4), rep("Avançado", 4)),
  prioridade = rep(c("Baixa", "Média", "Alta", "Muito Alta"), 3),
  fator_compensacao = c(1.25, 1.5, 1.8, 2.0, 1.5, 2.0, 2.5, 3.0, 2.0, 3.0, 5.0, 6.0)
)

tipologias_especiais <- data.frame(
  tipologia = c("Floresta Paludosa (Especial SEMIL)", "Manguezal", "Savana Gramíneo-Lenhosa (Cerrado Campestre)"),
  fator_fixo = c(6, 6, 3)
)

# ============================================================================
# MOTOR MATEMÁTICO (SEMIL 02/2024)
# ============================================================================

calcular_compensacao <- function(area_suprimida, tipo_vegetacao, prioridade_supressao, prioridade_compensacao = NULL, aplicacao_app = FALSE, arvores_isoladas = 0, tipologia_especial = NULL) {
  if (is.null(prioridade_compensacao)) prioridade_compensacao <- prioridade_supressao
  nivel_prioridade <- function(prio) { switch(prio, "Baixa" = 1, "Média" = 2, "Alta" = 3, "Muito Alta" = 4, 1) }
  
  fator_basico <- criterios_vegetacao %>% filter(tipo_vegetacao == !!tipo_vegetacao & prioridade == !!prioridade_supressao) %>% pull(fator_compensacao)
  if (length(fator_basico) == 0) fator_basico <- 1.5 
  
  area_compensacao_basica <- area_suprimida * fator_basico
  diferenca_niveis <- nivel_prioridade(prioridade_compensacao) - nivel_prioridade(prioridade_supressao)
  
  fator_ajuste <- 1.0; ajuste_aplicado <- "Nenhum"
  if (diferenca_niveis > 0) {
    if (diferenca_niveis == 1) { fator_ajuste <- 0.80; ajuste_aplicado <- "Redução 20%" }
    else if (diferenca_niveis == 2) { fator_ajuste <- 0.70; ajuste_aplicado <- "Redução 30%" }
    else if (diferenca_niveis >= 3) { fator_ajuste <- 0.50; ajuste_aplicado <- "Redução 50%" }
  } else if (diferenca_niveis < 0) {
    if (diferenca_niveis == -1) { fator_ajuste <- 1.25; ajuste_aplicado <- "Aumento 25%" }
    else if (diferenca_niveis == -2) { fator_ajuste <- 1.45; ajuste_aplicado <- "Aumento 45%" }
    else if (diferenca_niveis <= -3) { fator_ajuste <- 2.00; ajuste_aplicado <- "Aumento 100%" }
  }
  
  area_com_ajuste <- area_compensacao_basica * fator_ajuste
  area_extra_app <- ifelse(aplicacao_app, area_suprimida, 0)
  area_arvores <- ifelse(arvores_isoladas > 0, (arvores_isoladas / 1000), 0)
  
  fator_tipologia <- 1.0; tipologia_descricao <- "Padrão"
  if (!is.null(tipologia_especial) && tipologia_especial != "Não se aplica") {
    info_tipologia <- tipologias_especiais %>% filter(tipologia == !!tipologia_especial)
    if (nrow(info_tipologia) > 0) { fator_tipologia <- info_tipologia$fator_fixo[1] / fator_basico; tipologia_descricao <- tipologia_especial }
  }
  
  area_final <- (area_com_ajuste + area_extra_app) * fator_tipologia + area_arvores
  return(list(fator_basico = round(fator_basico, 3), ajuste_descricao = ajuste_aplicado, area_final_compensacao = round(area_final, 4)))
}

# ============================================================================
# INTERFACE DO USUÁRIO (UI)
# ============================================================================

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Sistema Jequitibá", titleWidth = 500),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(id = "tabs",
                menuItem("1. Painel de Controle", tabName = "dashboard", icon = icon("chart-bar")),
                menuItem("2. Gestão de Projetos", tabName = "projetos", icon = icon("folder-open")),
                menuItem("3. Entrada de Dados / Cubagem", tabName = "insercao", icon = icon("keyboard")),
                menuItem("4. Matriz do Inventário", tabName = "matriz", icon = icon("table")),
                menuItem("5. Mapa Georreferenciado", tabName = "mapa", icon = icon("map-marked-alt")),
                menuItem("6. Simulador Legal (SEMIL)", tabName = "calculo", icon = icon("balance-scale")),
                menuItem("7. Relatórios e Laudos", tabName = "relatorio", icon = icon("file-pdf")),
                menuItem("8. Banco de Dados (Backup)", tabName = "banco", icon = icon("database")),
                menuItem("9. Sobre o Sistema", tabName = "sobre", icon = icon("info-circle")),
                
                # Botão de segurança para fechar
                br(), hr(),
                actionButton("btn_sair", "Desligar Sistema Seguramente", icon = icon("power-off"), 
                             style = "color: #fff; background-color: #d9534f; border-color: #d43f3a; width: 90%; margin-left: 15px; font-weight: bold;")
    )
  ),
  
  dashboardBody(
    tags$head(tags$script(HTML("
      window.onbeforeunload = function() {
          return 'O sistema realiza autosalvamento, mas certifique-se de que exportou seu backup antes de sair.';
      };
    "))),
    tags$head(tags$style(HTML(".box { box-shadow: 0 2px 4px rgba(0,0,0,0.1); } .leaflet-popup-content { font-size: 14px; }"))),
    
    tabItems(
      # TAB 1: DASHBOARD
      tabItem(tabName = "dashboard",
              fluidRow(column(12, h2("Visão Geral do Portfólio", icon = icon("chart-bar")))),
              fluidRow(
                infoBoxOutput("dash_projetos", width = 4),
                infoBoxOutput("dash_arvores", width = 4),
                infoBoxOutput("dash_volume", width = 4)
              ),
              fluidRow(
                column(12, box(title = "Gerenciador de Projetos", status = "primary", solidHeader = TRUE, width = 12,
                               p("Selecione um projeto na tabela e clique em Abrir para carregar as informações da propriedade:"),
                               DT::dataTableOutput("tabela_dash_projetos"),
                               hr(),
                               fluidRow(
                                 column(6, actionButton("btn_abrir_projeto", "Abrir Projeto Selecionado", class = "btn-success", icon = icon("folder-open"), width = "100%", style = "font-weight:bold; font-size: 16px; padding: 10px;")),
                                 column(6, actionButton("btn_excluir_projeto", "Excluir Projeto Permanentemente", class = "btn-danger", icon = icon("trash"), width = "100%", style = "padding: 10px;"))
                               )
                ))
              )
      ),
      
      # TAB 2: PROJETOS
      tabItem(tabName = "projetos",
              fluidRow(column(12, h2("Cadastro e Edição de Projetos", icon = icon("folder-open")))),
              
              fluidRow(column(12, uiOutput("status_modo_projeto"))),
              
              fluidRow(
                column(6,
                       box(title = "Dados Administrativos", status = "primary", solidHeader = TRUE, width = 12,
                           p(style="color:gray; font-style:italic;", "O Código/ID do projeto será gerado automaticamente (Nome + Cliente)."),
                           textInput("proj_nome", "Nome do Projeto / Propriedade", placeholder = "Ex: 3 Irmãs"),
                           textInput("proj_cliente", "Cliente / Proprietário", placeholder = "Ex: Roberto"),
                           textInput("proj_municipio", "Município", placeholder = "Ex: Araraquara/SP"),
                           numericInput("proj_area_total", "Área Total do Imóvel (ha)", value = 0, min = 0, step = 0.1),
                           textAreaInput("proj_obs", "Observações e Pendências", rows = 4)
                       )
                ),
                column(6,
                       box(title = "Dados Base para Supressão de Área", status = "warning", solidHeader = TRUE, width = 12,
                           numericInput("proj_area_sup", "Área de Supressão Prevista (m²)", value = 0, min = 0, step = 1),
                           selectInput("proj_estagio", "Estágio Sucessional", choices = c("Inicial", "Médio", "Avançado")),
                           selectInput("proj_prio_sup", "Prioridade Local (Mapa SEMIL)", choices = c("Baixa", "Média", "Alta", "Muito Alta")),
                           selectInput("proj_tipologia", "Tipologia Botânica (IBGE)", choices = tipologias_ibge),
                           checkboxInput("proj_app", "O polígono de supressão cruza APP?", FALSE),
                           hr(),
                           actionButton("btn_salvar_projeto", "Salvar Dados no Projeto Atual", class = "btn-success", width = "100%", icon = icon("save"), style="font-weight:bold; padding: 10px;"),
                           br(), br(),
                           actionButton("btn_limpar_form", "➕ Criar Novo Projeto (Zerar Tela)", class = "btn-primary", width = "100%", style="font-weight:bold;")
                       )
                )
              )
      ),
      
      # TAB 3: INSERÇÃO E CUBAGEM
      tabItem(tabName = "insercao",
              fluidRow(column(12, h2("Cálculo de Cubagem e Entrada de Árvores", icon = icon("keyboard")))),
              fluidRow(column(12, box(title = "Projeto Ativo", status = "info", solidHeader = TRUE, width = 12, uiOutput("header_projeto_ativo_insercao")))),
              
              fluidRow(
                column(8,
                       box(title = "Entrada Manual Individual", status = "success", solidHeader = TRUE, width = 12,
                           fluidRow(
                             column(4, textInput("cub_especie", "🌳 Espécie", placeholder = "Ex: Ipê Amarelo")),
                             column(4, dateInput("cub_data", "📅 Data Coleta", value = Sys.Date(), format = "dd/mm/yyyy")),
                             column(4, textInput("cub_foto", "📷 Ref. Foto", placeholder = "Ex: DSC001"))
                           ),
                           hr(),
                           fluidRow(
                             column(3, numericInput("cub_dap", "📏 DAP (cm)", value = NA)),
                             column(3, numericInput("cub_cap", "📐 CAP (cm)", value = NA)),
                             column(3, numericInput("cub_altura", "🌲 Altura (m)", value = NA)),
                             column(3, numericInput("cub_ff", "Fator Forma", value = 0.7, step = 0.05))
                           ),
                           fluidRow(
                             column(6, numericInput("cub_lat", "📍 Latitude (Y)", value = NA, step = 0.000001)),
                             column(6, numericInput("cub_lon", "📍 Longitude (X)", value = NA, step = 0.000001))
                           ),
                           hr(),
                           fluidRow(
                             column(4, checkboxInput("cub_exotica", "🌎 Espécie Exótica", value = FALSE)),
                             column(4, checkboxInput("cub_ameacada", "⚠️ Ameaçada de Extinção", value = FALSE)),
                             column(4, checkboxInput("cub_app", "🌿 Localizada em APP", value = FALSE))
                           ),
                           hr(),
                           actionButton("btn_add_cubagem", "Calcular Cubagem (m³) e Salvar Árvore", class = "btn-success", width = "100%", icon = icon("save"), style = "font-weight:bold; font-size:16px; padding:10px;")
                       )
                ),
                column(4,
                       box(title = "Importação em Lote (Excel)", status = "warning", solidHeader = TRUE, width = 12,
                           p("Mesmo se a sua planilha contiver APENAS Espécie, Altura e DAP/CAP, o sistema calculará as volumetrias automaticamente."),
                           downloadButton("btn_baixar_modelo", "1. Baixar Planilha Modelo", class = "btn-default", style = "width: 100%; margin-bottom: 10px;"),
                           fileInput("upload_inventario", "2. Importar Planilha (Calculadora Auto)", accept = c(".xlsx"))
                       )
                )
              )
      ),
      
      # TAB 4: MATRIZ DO INVENTÁRIO
      tabItem(tabName = "matriz",
              fluidRow(column(12, h2("Matriz de Dados do Inventário", icon = icon("table")))),
              fluidRow(column(12, box(title = "Projeto Ativo", status = "info", solidHeader = TRUE, width = 12, uiOutput("header_projeto_ativo_matriz")))),
              fluidRow(
                column(12,
                       box(title = "Banco de Dados Físico das Árvores", status = "primary", solidHeader = TRUE, width = 12,
                           downloadButton("btn_exportar_inventario", "📈 Exportar Matriz (Excel/CSV)", class = "btn-success", style = "margin-bottom: 15px;"),
                           DT::dataTableOutput("tabela_inventario")
                       )
                )
              )
      ),
      
      # TAB 5: MAPA GPS
      tabItem(tabName = "mapa",
              fluidRow(column(12, h2("Mapa Georreferenciado", icon = icon("map-marked-alt")))),
              fluidRow(column(12, box(title = "Projeto Ativo", status = "info", solidHeader = TRUE, width = 12, uiOutput("header_projeto_ativo_mapa")))),
              fluidRow(
                column(12,
                       box(title = "Visão Cartográfica dos Indivíduos", status = "primary", solidHeader = TRUE, width = 12,
                           p("As árvores cadastradas com Latitude e Longitude (GPS) aparecerão aqui automaticamente. Clique no pino para abrir o Fichário Digital da árvore."),
                           leafletOutput("mapa_arvores", height = 550)
                       )
                )
              )
      ),
      
      # TAB 6: SIMULADOR SEMIL
      tabItem(tabName = "calculo",
              fluidRow(column(12, h2("Simulador de Compliance Ambiental", icon = icon("balance-scale")))),
              fluidRow(column(12, box(title = "Projeto Ativo", status = "info", solidHeader = TRUE, width = 12, uiOutput("header_projeto_ativo_calc")))),
              fluidRow(
                column(6,
                       box(title = "Auditoria de Supressão (Agravantes)", status = "primary", solidHeader = TRUE, width = 12,
                           actionButton("btn_puxar_projeto", "Cruzar com Dados do Inventário Atual", icon = icon("sync"), class = "btn-info", style = "margin-bottom: 15px; width: 100%; font-weight: bold;"),
                           numericInput("calc_area_sup", "Área Suprimida de Polígono (m²)", value = 0, min = 0),
                           selectInput("calc_tipo_veg", "Estágio Sucessional Identificado", choices = c("Inicial", "Médio", "Avançado")),
                           selectInput("calc_prio_sup", "Prioridade SEMIL (Local da Supressão)", choices = c("Baixa", "Média", "Alta", "Muito Alta")),
                           checkboxInput("calc_app", "Polígono incide sobre APP", FALSE),
                           hr(),
                           h5(strong("Filtro do Inventário:")),
                           numericInput("calc_arvores_nativas", "Total de Indivíduos NATIVOS (Filtrado)", value = 0, min = 0),
                           p(style="color:gray; font-size:12px;", "Obs: A auditoria matemática cruza as métricas automaticamente e exclui árvores exóticas do cálculo de passivo.")
                       )
                ),
                column(6,
                       box(title = "Estratégia de Compensação (Atenuantes)", status = "warning", solidHeader = TRUE, width = 12,
                           selectInput("calc_prio_comp", "Prioridade SEMIL (Local do Destino)", choices = c("Baixa", "Média", "Alta", "Muito Alta")),
                           selectInput("calc_tipologia", "Tipologia Botânica (IBGE)", choices = tipologias_ibge),
                           hr(),
                           actionButton("btn_calcular", "Executar Auditoria de Passivo", class = "btn-danger", width = "100%", icon = icon("cogs"))
                       )
                )
              ),
              fluidRow(
                column(12, box(title = "Parecer Oficial", status = "success", solidHeader = TRUE, width = 12, uiOutput("resultado_calculo")))
              )
      ),
      
      # TAB 7: RELATÓRIO
      tabItem(tabName = "relatorio",
              fluidRow(column(12, h2("Exportação de Laudos", icon = icon("file-pdf")))),
              fluidRow(
                column(12, box(title = "Configuração do Relatório Técnico", status = "primary", solidHeader = TRUE, width = 12,
                               p("Gera o caderno PDF contendo as métricas do projeto e o Fichário Digital das Árvores."),
                               textInput("rel_responsavel", "Responsável Técnico", placeholder = "Nome do Engenheiro/Biólogo"),
                               textInput("rel_art", "Registro (ART/TRT)", placeholder = "Nº do Conselho"),
                               selectInput("rel_formato", "Formato de Arquivo", choices = c("HTML (Pronto para gerar PDF)", "DOCX (Word)")),
                               hr(),
                               downloadButton("btn_gerar_relatorio", "📄 Gerar Laudo Técnico", class = "btn-success", style = "width: 100%; font-size: 16px; font-weight: bold; padding: 10px;")
                ))
              )
      ),
      
      # TAB 8: BANCO DE DADOS
      tabItem(tabName = "banco",
              fluidRow(column(12, h2("Persistência e Backup", icon = icon("database")))),
              fluidRow(
                column(6,
                       box(title = "Salvar Dados", status = "success", solidHeader = TRUE, width = 12,
                           p("Faça o download estruturado de todo o banco de dados do aplicativo."),
                           downloadButton("btn_exportar_bd", "Exportar Arquivo (.rds)", class = "btn-success", style = "width: 100%;")
                       )
                ),
                column(6,
                       box(title = "Restaurar Dados", status = "warning", solidHeader = TRUE, width = 12,
                           p("Carregue um banco anterior (Substituirá a sessão atual)."),
                           fileInput("upload_bd", "Selecione o arquivo (.rds)", accept = c(".rds")),
                           actionButton("btn_importar_bd", "Restaurar", class = "btn-warning", width = "100%", icon = icon("upload"))
                       )
                )
              )
      ),
      
      # TAB 9: SOBRE O SISTEMA
      tabItem(tabName = "sobre",
              fluidRow(column(12, h2("Sobre o Sistema", icon = icon("info-circle")))),
              fluidRow(
                column(6,
                       box(title = "Sistema Jequitibá", status = "primary", solidHeader = TRUE, width = 12,
                           h3(strong("Scientia Ambiental"), style = "color: #2e7d32; margin-top: 0;"),
                           p(strong("Versão:"), " 1.0.0 (Build 2026)"),
                           p("Este software foi desenvolvido para otimizar e auditar processos de licenciamento ambiental, com foco em inventários florestais georreferenciados e cálculo de compensação de passivos em conformidade com a legislação do Estado de São Paulo (Resolução SEMIL nº 02/2024)."),
                           hr(),
                           h4(strong("Arquitetura e Desenvolvimento:")),
                           tags$ul(
                             tags$li(strong("Dr. Gabriel F. M. do Rosário"), " - Engenharia de Dados & Especialista Ambiental"),
                             tags$li(strong("Dra. Daiane R. de Barros"), " - Consultoria & Compliance"),
                             tags$li(strong("Me. Rafael Caracho"), " - Consultoria & Operações")
                           )
                       )
                ),
                column(6,
                       box(title = "Tecnologia & Termos de Uso", status = "success", solidHeader = TRUE, width = 12,
                           p(strong("Motor Analítico:"), " R / Shiny (Portable Framework)"),
                           p(strong("Geoprocessamento:"), " Leaflet Engine"),
                           p(strong("Banco de Dados:"), " SGBD Local Relacional"),
                           hr(),
                           p(style = "color: gray; font-size: 13px; text-align: justify;", 
                             strong("Aviso Legal:"), " Os resultados matemáticos e espaciais gerados por este sistema servem exclusivamente como ferramentas de auditoria prévia (Due Diligence). A aprovação final de volumes de supressão, passivos ambientais e multiplicadores de área é de competência exclusiva do órgão ambiental avaliador (CETESB / SEMIL / Prefeituras). O uso das informações geradas é de inteira responsabilidade dos técnicos assinantes do laudo.")
                       )
                )
              )
      )
    )
  )
)

# ============================================================================
# LÓGICA DO SERVIDOR (SERVER)
# ============================================================================

server <- function(input, output, session) {
  
  dados_iniciais <- carregar_banco()
  
  db <- reactiveValues(
    projetos = dados_iniciais$projetos,
    inventario = dados_iniciais$inventario,
    projeto_ativo = NULL, ultimo_calculo = NULL
  )
  
  gerar_id_arvore <- function() { paste0("ARV-", sample(1000:9999, 1)) }
  
  observeEvent(input$btn_sair, {
    salvar_banco(db$projetos, db$inventario)
    stopApp()
  })
  
  # ========== TAB 1: DASHBOARD ==========
  output$dash_projetos <- renderInfoBox({ infoBox("Projetos Abertos", nrow(db$projetos), icon = icon("folder"), color = "blue") })
  output$dash_arvores <- renderInfoBox({ infoBox("Fichas de Árvore", nrow(db$inventario), icon = icon("tree"), color = "green") })
  output$dash_volume <- renderInfoBox({ infoBox("Vol. Total (m³)", round(sum(db$inventario$Volume, na.rm=TRUE), 2), icon = icon("cube"), color = "yellow") })
  
  output$tabela_dash_projetos <- DT::renderDataTable({
    if(nrow(db$projetos) == 0) return(data.frame(Status = "Banco vazio."))
    datatable(db$projetos %>% select(ID, Nome, Cliente, Municipio), selection = 'single', options = list(pageLength = 5))
  })
  
  observeEvent(input$btn_abrir_projeto, {
    linha <- input$tabela_dash_projetos_rows_selected
    if(is.null(linha)) { showNotification("Selecione um projeto na tabela primeiro.", type = "warning"); return() }
    
    id_sel <- db$projetos$ID[linha]
    db$projeto_ativo <- id_sel
    
    prj <- db$projetos %>% filter(ID == id_sel)
    updateTextInput(session, "proj_nome", value = prj$Nome)
    updateTextInput(session, "proj_cliente", value = prj$Cliente)
    updateTextInput(session, "proj_municipio", value = prj$Municipio)
    updateNumericInput(session, "proj_area_total", value = prj$Area_Total)
    updateTextAreaInput(session, "proj_obs", value = prj$Obs)
    updateNumericInput(session, "proj_area_sup", value = prj$Area_Sup)
    updateSelectInput(session, "proj_estagio", selected = prj$Estagio)
    updateSelectInput(session, "proj_prio_sup", selected = prj$Prio_Sup)
    updateSelectInput(session, "proj_tipologia", selected = prj$Tipologia)
    updateCheckboxInput(session, "proj_app", value = prj$APP)
    
    db$ultimo_calculo <- NULL
    
    updateTabItems(session, "tabs", "projetos") 
    showNotification(paste("PROJETO", prj$Nome, "CARREGADO COM SUCESSO! Todos os dados foram resgatados."), type = "message", duration = 8)
  })
  
  observeEvent(input$btn_excluir_projeto, {
    linha <- input$tabela_dash_projetos_rows_selected
    if(is.null(linha)) { showNotification("Selecione um projeto para excluir.", type = "warning"); return() }
    
    id_sel <- db$projetos$ID[linha]
    db$projetos <- subset(db$projetos, ID != id_sel)
    db$inventario <- subset(db$inventario, ID_Projeto != id_sel)
    
    if(!is.null(db$projeto_ativo) && db$projeto_ativo == id_sel) { db$projeto_ativo <- NULL }
    
    salvar_banco(db$projetos, db$inventario)
    showNotification("Projeto e seu inventário foram excluídos permanentemente.", type = "error")
  })
  
  # ========== TAB 2: GESTÃO DE PROJETOS ==========
  output$status_modo_projeto <- renderUI({
    if(is.null(db$projeto_ativo)) {
      div(style = "background-color: #337ab7; color: white; padding: 15px; text-align: center; border-radius: 5px; margin-bottom: 20px; font-size: 18px; font-weight: bold;", 
          icon("plus-circle"), " MODO DE CRIAÇÃO: VOCÊ ESTÁ INICIANDO UM NOVO PROJETO")
    } else {
      prj_nome_atual <- db$projetos$Nome[db$projetos$ID == db$projeto_ativo]
      div(style = "background-color: #f39c12; color: white; padding: 15px; text-align: center; border-radius: 5px; margin-bottom: 20px; font-size: 18px; font-weight: bold;", 
          icon("edit"), paste(" MODO DE EDIÇÃO: ATUALIZANDO DADOS DO PROJETO '", prj_nome_atual, "'"))
    }
  })
  
  observeEvent(input$btn_salvar_projeto, {
    tryCatch({
      nome_proj <- as.character(trimws(input$proj_nome))
      cliente_proj <- as.character(trimws(input$proj_cliente))
      if(is.null(nome_proj) || nome_proj == "") { stop("O campo 'Nome do Projeto' é obrigatório.") }
      
      id_nome <- toupper(gsub("\\s+", "_", nome_proj))
      id_cliente <- toupper(gsub("\\s+", "_", cliente_proj))
      novo_id <- id_nome
      if(cliente_proj != "") { novo_id <- paste0(novo_id, "-", id_cliente) }
      
      id_antigo <- db$projeto_ativo
      
      if (is.null(id_antigo) && (novo_id %in% db$projetos$ID)) {
        stop("Já existe um projeto cadastrado com este Nome e Cliente exatos.")
      }
      
      if (!is.null(id_antigo) && id_antigo != novo_id) {
        if (novo_id %in% db$projetos$ID) { stop("A alteração foi bloqueada pois já existe outro projeto com este mesmo Nome e Cliente.") }
        if (nrow(db$inventario) > 0) { db$inventario$ID_Projeto[db$inventario$ID_Projeto == id_antigo] <- novo_id }
        db$projetos <- subset(db$projetos, ID != id_antigo)
      }
      
      nova_linha <- data.frame(
        ID = novo_id, Nome = nome_proj, Cliente = as.character(ifelse(is.null(input$proj_cliente), "", input$proj_cliente)), 
        Municipio = as.character(ifelse(is.null(input$proj_municipio), "", input$proj_municipio)),
        Area_Total = ifelse(is.na(as.numeric(input$proj_area_total)), 0, as.numeric(input$proj_area_total)), 
        Area_Sup = ifelse(is.na(as.numeric(input$proj_area_sup)), 0, as.numeric(input$proj_area_sup)), 
        Estagio = as.character(input$proj_estagio), Prio_Sup = as.character(input$proj_prio_sup), 
        APP = as.logical(input$proj_app), Tipologia = as.character(input$proj_tipologia), 
        Obs = as.character(ifelse(is.null(input$proj_obs), "", input$proj_obs)), stringsAsFactors = FALSE
      )
      
      db$projetos <- subset(db$projetos, ID != novo_id) 
      db$projetos <- rbind(db$projetos, nova_linha)    
      db$projeto_ativo <- novo_id 
      salvar_banco(db$projetos, db$inventario) 
      showNotification("Projeto salvo e gravado no banco com sucesso!", type = "message")
      
    }, error = function(e) { showNotification(e$message, type = "error") })
  })
  
  observeEvent(input$btn_limpar_form, {
    db$projeto_ativo <- NULL
    updateTextInput(session, "proj_nome", value = "")
    updateTextInput(session, "proj_cliente", value = "")
    updateTextInput(session, "proj_municipio", value = "")
    updateNumericInput(session, "proj_area_total", value = 0)
    updateTextAreaInput(session, "proj_obs", value = "")
    updateNumericInput(session, "proj_area_sup", value = 0)
    updateCheckboxInput(session, "proj_app", value = FALSE)
    showNotification("Memória limpa! Você pode começar a digitar os dados do Novo Projeto.", type = "message", duration = 5)
  })
  
  msg_vazio <- h4(style="color:red;", "NENHUM PROJETO ABERTO. Vá ao Painel de Controle e clique em 'Abrir Projeto'.")
  msg_ativo <- function(prefixo) { h4(style="color:green; font-weight:bold;", paste(prefixo, db$projetos$Nome[db$projetos$ID == db$projeto_ativo], "(", db$projeto_ativo, ")")) }
  
  output$header_projeto_ativo_insercao <- renderUI({ if(is.null(db$projeto_ativo)) msg_vazio else msg_ativo("Operando em:") })
  output$header_projeto_ativo_matriz <- renderUI({ if(is.null(db$projeto_ativo)) msg_vazio else msg_ativo("Operando em:") })
  output$header_projeto_ativo_mapa <- renderUI({ if(is.null(db$projeto_ativo)) msg_vazio else msg_ativo("Operando em:") })
  output$header_projeto_ativo_calc <- renderUI({ if(is.null(db$projeto_ativo)) msg_vazio else msg_ativo("Auditoria de:") })
  
  # ========== TAB 3: INSERÇÃO E CUBAGEM ==========
  observeEvent(input$cub_cap, { if (!is.na(input$cub_cap)) { dap_c <- round(input$cub_cap / pi, 2); if (is.na(input$cub_dap) || abs(input$cub_dap - dap_c) > 0.01) updateNumericInput(session, "cub_dap", value = dap_c) } }, ignoreInit = TRUE)
  observeEvent(input$cub_dap, { if (!is.na(input$cub_dap)) { cap_c <- round(input$cub_dap * pi, 2); if (is.na(input$cub_cap) || abs(input$cub_cap - cap_c) > 0.01) updateNumericInput(session, "cub_cap", value = cap_c) } }, ignoreInit = TRUE)
  
  observeEvent(input$btn_add_cubagem, {
    if(is.null(db$projeto_ativo)) { showNotification("Abra um projeto primeiro.", type="warning"); return() }
    if(is.na(input$cub_dap) || is.na(input$cub_altura)) { showNotification("DAP e Altura são obrigatórios.", type="error"); return() }
    
    vol <- ((pi * (input$cub_dap / 100)^2) / 4) * input$cub_altura * input$cub_ff
    nova_arvore <- data.frame(
      ID_Arvore = gerar_id_arvore(), ID_Projeto = db$projeto_ativo, Data = as.character(input$cub_data), Foto = input$cub_foto, Especie = input$cub_especie, CAP = input$cub_cap, DAP = input$cub_dap, Altura = input$cub_altura, Ff = input$cub_ff, Volume = round(vol, 4), Lat = input$cub_lat, Lon = input$cub_lon, Ameacada = input$cub_ameacada, Exotica = input$cub_exotica, Em_APP = input$cub_app, stringsAsFactors = FALSE
    )
    db$inventario <- rbind(db$inventario, nova_arvore)
    salvar_banco(db$projetos, db$inventario)
    showNotification("Árvore inserida e gravada.", type="message")
    updateNumericInput(session, "cub_cap", value = NA); updateNumericInput(session, "cub_dap", value = NA)
    updateNumericInput(session, "cub_altura", value = NA); updateTextInput(session, "cub_foto", value = "")
  })
  
  output$btn_baixar_modelo <- downloadHandler(
    filename = function() { "Matriz_Excel_Fichario.xlsx" },
    content = function(file) { write_xlsx(data.frame(Data = "15/07/2026", Foto = "DSC001", Especie = "Ipê Amarelo", CAP = 142, DAP = 45.2, Altura = 18, Ff = 0.7, Lat = -22.345, Lon = -47.123, Ameacada = "Não", Exotica = "Não", Em_APP = "Sim"), file) }
  )
  
  observeEvent(input$upload_inventario, {
    if(is.null(db$projeto_ativo)) { showNotification("Abra um projeto primeiro.", type="warning"); return() }
    tryCatch({
      df <- read_excel(input$upload_inventario$datapath)
      cols <- colnames(df)
      if(any(grepl("Especie|Espécie", cols, ignore.case=T))) colnames(df)[grepl("Especie|Espécie", cols, ignore.case=T)[1]] <- "Especie"
      if(any(grepl("CAP", cols, ignore.case=T))) colnames(df)[grepl("CAP", cols, ignore.case=T)[1]] <- "CAP"
      if(any(grepl("DAP", cols, ignore.case=T))) colnames(df)[grepl("DAP", cols, ignore.case=T)[1]] <- "DAP"
      if(any(grepl("Altura", cols, ignore.case=T))) colnames(df)[grepl("Altura", cols, ignore.case=T)[1]] <- "Altura"
      if(any(grepl("Ff|Fator", cols, ignore.case=T))) colnames(df)[grepl("Ff|Fator", cols, ignore.case=T)[1]] <- "Ff"
      if(any(grepl("Lat", cols, ignore.case=T))) colnames(df)[grepl("Lat", cols, ignore.case=T)[1]] <- "Lat"
      if(any(grepl("Lon", cols, ignore.case=T))) colnames(df)[grepl("Lon", cols, ignore.case=T)[1]] <- "Lon"
      if(any(grepl("Foto", cols, ignore.case=T))) colnames(df)[grepl("Foto", cols, ignore.case=T)[1]] <- "Foto"
      if(any(grepl("Ameaca|Ameaça", cols, ignore.case=T))) colnames(df)[grepl("Ameaca|Ameaça", cols, ignore.case=T)[1]] <- "Ameacada"
      if(any(grepl("Exotica|Exótica", cols, ignore.case=T))) colnames(df)[grepl("Exotica|Exótica", cols, ignore.case=T)[1]] <- "Exotica"
      if(any(grepl("APP", cols, ignore.case=T))) colnames(df)[grepl("APP", cols, ignore.case=T)[1]] <- "Em_APP"
      
      if(!"Especie" %in% colnames(df)) df$Especie <- "sp."
      if(!"CAP" %in% colnames(df)) df$CAP <- NA
      if(!"DAP" %in% colnames(df)) df$DAP <- NA
      if(!"Altura" %in% colnames(df)) df$Altura <- NA
      if(!"Ff" %in% colnames(df)) df$Ff <- 0.7
      if(!"Lat" %in% colnames(df)) df$Lat <- NA
      if(!"Lon" %in% colnames(df)) df$Lon <- NA
      if(!"Foto" %in% colnames(df)) df$Foto <- ""
      if(!"Ameacada" %in% colnames(df)) df$Ameacada <- "Não"
      if(!"Exotica" %in% colnames(df)) df$Exotica <- "Não"
      if(!"Em_APP" %in% colnames(df)) df$Em_APP <- "Não"
      
      df <- df %>% mutate(CAP = suppressWarnings(as.numeric(CAP)), DAP = suppressWarnings(as.numeric(DAP)), Altura = suppressWarnings(as.numeric(Altura)), Ff = suppressWarnings(as.numeric(Ff)), Ff = ifelse(is.na(Ff), 0.7, Ff), DAP = ifelse(is.na(DAP) & !is.na(CAP), CAP / pi, DAP), CAP = ifelse(is.na(CAP) & !is.na(DAP), DAP * pi, CAP)) %>% filter(!is.na(DAP) & !is.na(Altura))
      
      n <- nrow(df)
      if(n > 0) {
        novas <- data.frame(
          ID_Arvore = paste0("ARV-", sample(10000:99999, n, replace=T)), ID_Projeto = db$projeto_ativo, Data = as.character(Sys.Date()), 
          Foto = as.character(ifelse(is.na(df$Foto), "", df$Foto)), Especie = as.character(ifelse(is.na(df$Especie) | df$Especie=="", "sp.", df$Especie)),
          CAP = round(df$CAP, 2), DAP = round(df$DAP, 2), Altura = round(df$Altura, 2), Ff = df$Ff,
          Volume = round(((pi*(df$DAP/100)^2)/4) * df$Altura * df$Ff, 4), Lat = as.numeric(df$Lat), Lon = as.numeric(df$Lon),
          Ameacada = grepl("Sim|S|true", as.character(df$Ameacada), ignore.case=T), Exotica = grepl("Sim|S|true", as.character(df$Exotica), ignore.case=T), Em_APP = grepl("Sim|S|true", as.character(df$Em_APP), ignore.case=T), stringsAsFactors = FALSE
        )
        db$inventario <- rbind(db$inventario, novas)
        salvar_banco(db$projetos, db$inventario)
        showNotification(paste(n, "árvores importadas e calculadas com sucesso."), type="message", duration=8)
      } else { showNotification("A planilha não continha árvores com DAP/CAP e Altura válidos.", type="warning") }
    }, error = function(e) { showNotification(paste("Erro na leitura:", e$message), type="error", duration=10) })
  })
  
  # ========== TAB 4: MATRIZ DE DADOS ==========
  inventario_filtrado <- reactive({ req(db$projeto_ativo); db$inventario %>% filter(ID_Projeto == db$projeto_ativo) })
  output$tabela_inventario <- DT::renderDataTable({
    if(is.null(db$projeto_ativo)) return(data.frame(Aviso="Abra um projeto no Painel de Controle."))
    datatable(inventario_filtrado() %>% select(-ID_Projeto), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$btn_exportar_inventario <- downloadHandler(filename = function() { paste0("Inventario_", Sys.Date(), ".csv") }, content = function(file) { write.csv(inventario_filtrado(), file, row.names = FALSE) })
  
  # ========== TAB 5: MAPA GPS ==========
  output$mapa_arvores <- renderLeaflet({
    mapa <- leaflet() %>% addTiles() %>% setView(lng = -48.1, lat = -21.7, zoom = 6)
    if(!is.null(db$projeto_ativo)) {
      dados_mapa <- inventario_filtrado() %>% filter(!is.na(Lat) & !is.na(Lon))
      if(nrow(dados_mapa) > 0) {
        popups <- paste0(
          "<div style='font-family: Arial; padding: 5px;'><h4 style='margin:0; color:#2e7d32;'>🌳 ", dados_mapa$Especie, "</h4><hr style='margin: 5px 0;'>",
          "<b>📏 DAP:</b> ", dados_mapa$DAP, " cm<br><b>📐 CAP:</b> ", dados_mapa$CAP, " cm<br><b>🌲 Altura:</b> ", dados_mapa$Altura, " m<br><b>🪵 Volume:</b> ", dados_mapa$Volume, " m³<br><br>",
          "<b>📷 Foto:</b> ", ifelse(dados_mapa$Foto=="", "N/A", dados_mapa$Foto), "<br><b>⚠️ Ameaçada:</b> ", ifelse(dados_mapa$Ameacada, "Sim", "Não"), "<br>",
          "<b>🌎 Exótica:</b> ", ifelse(dados_mapa$Exotica, "Sim", "Não"), "<br><b>🌿 APP:</b> ", ifelse(dados_mapa$Em_APP, "Sim", "Não"), "<br>",
          "<hr style='margin: 5px 0;'><small><i>", dados_mapa$Data, " (", dados_mapa$ID_Arvore, ")</i></small></div>"
        )
        cores <- ifelse(dados_mapa$Ameacada, "red", ifelse(dados_mapa$Exotica, "purple", "green"))
        mapa <- mapa %>% clearMarkers() %>% addCircleMarkers(data = dados_mapa, lng = ~Lon, lat = ~Lat, popup = popups, color = cores, fillOpacity = 0.8, radius = 6, weight=1) %>% fitBounds(min(dados_mapa$Lon), min(dados_mapa$Lat), max(dados_mapa$Lon), max(dados_mapa$Lat))
      }
    }
    mapa
  })
  
  # ========== TAB 6: SIMULADOR SEMIL ==========
  observeEvent(input$btn_puxar_projeto, {
    if(is.null(db$projeto_ativo)) { showNotification("Nenhum projeto aberto.", type = "warning"); return() }
    prj <- db$projetos %>% filter(ID == db$projeto_ativo)
    nativas <- db$inventario %>% filter(ID_Projeto == db$projeto_ativo & Exotica == FALSE)
    updateNumericInput(session, "calc_area_sup", value = prj$Area_Sup)
    updateSelectInput(session, "calc_tipo_veg", selected = prj$Estagio)
    updateSelectInput(session, "calc_prio_sup", selected = prj$Prio_Sup)
    updateCheckboxInput(session, "calc_app", value = prj$APP)
    updateNumericInput(session, "calc_arvores_nativas", value = nrow(nativas))
    updateSelectInput(session, "calc_tipologia", selected = prj$Tipologia)
    showNotification("Auditoria: Dados puxados com sucesso.", type = "message")
  })
  
  observeEvent(input$btn_calcular, {
    res <- calcular_compensacao(area_suprimida = input$calc_area_sup / 10000, tipo_vegetacao = input$calc_tipo_veg, prioridade_supressao = input$calc_prio_sup, prioridade_compensacao = input$calc_prio_comp, aplicacao_app = input$calc_app, arvores_isoladas = input$calc_arvores_nativas, tipologia_especial = input$calc_tipologia)
    db$ultimo_calculo <- res
  })
  
  output$resultado_calculo <- renderUI({
    if(is.null(db$ultimo_calculo)) return(p(style="color:gray;", "Execute a auditoria matemática."))
    res <- db$ultimo_calculo
    tagList(
      fluidRow(column(6, h4("Base de Supressão (Auditada)"), p("Área do Polígono: ", paste(input$calc_area_sup / 10000, "ha")), p("Indivíduos Nativos: ", input$calc_arvores_nativas)), column(6, h4("Multiplicadores (SEMIL)"), p("Fator Categoria Botânica: ", res$fator_basico, "x"), p("Ajuste de Deslocamento de Bacia: ", res$ajuste_descricao))), hr(),
      h3(style = "color: #c9302c; text-align: center; font-weight: bold;", "DÉFICIT COMPENSATÓRIO (AREA EQUIVALENTE): ", res$area_final_compensacao, " ha")
    )
  })
  
  # ========== TAB 7 & 8: RELATÓRIO E BD ==========
  output$btn_gerar_relatorio <- downloadHandler(
    filename = function() { ext <- switch(input$rel_formato, "HTML (Pronto para gerar PDF)"="html", "DOCX (Word)"="docx"); paste0("Laudo_Auditoria_", Sys.Date(), ".", ext) },
    content = function(file) {
      if(is.null(db$projeto_ativo)) stop("Selecione um projeto.")
      prj <- db$projetos %>% filter(ID == db$projeto_ativo)
      inv <- db$inventario %>% filter(ID_Projeto == db$projeto_ativo)
      vol_nativo <- sum(inv$Volume[inv$Exotica == FALSE], na.rm=TRUE)
      vol_exotico <- sum(inv$Volume[inv$Exotica == TRUE], na.rm=TRUE)
      
      txt <- c("---", paste0("title: 'Laudo de Compliance Ambiental'"), paste0("subtitle: 'Projeto: ", prj$ID, "'"), "---",
               "## 1. Dados do Projeto", paste("- Cliente:", prj$Cliente), paste("- Total da Propriedade:", prj$Area_Total, "ha"),
               "## 2. Balanço Geral do Fichário Florestal", 
               paste("- Total de Indivíduos Inventariados:", nrow(inv)), paste("- Indivíduos Exóticos:", nrow(inv %>% filter(Exotica == TRUE))), paste("- Espécies Ameaçadas Identificadas:", nrow(inv %>% filter(Ameacada == TRUE))),
               paste("- Volume Total (DOF):", round(sum(inv$Volume, na.rm=T), 4), "m³"), paste("  - Madeira Nativa:", round(vol_nativo, 4), "m³"), paste("  - Madeira Exótica:", round(vol_exotico, 4), "m³")
      )
      tmp <- file.path(tempdir(), "laudo.Rmd")
      writeLines(txt, tmp)
      rmarkdown::render(tmp, output_format = switch(input$rel_formato, "HTML (Pronto para gerar PDF)"="html_document", "DOCX (Word)"="word_document"), output_file = file, quiet = TRUE)
    }
  )
  
  output$btn_exportar_bd <- downloadHandler(filename = function(){ "SGA_Backup.rds" }, content = function(f){ saveRDS(list(projetos=db$projetos, inventario=db$inventario), f) })
  observeEvent(input$btn_importar_bd, { req(input$upload_bd); pac <- readRDS(input$upload_bd$datapath); db$projetos <- pac$projetos; db$inventario <- pac$inventario; salvar_banco(db$projetos, db$inventario); showNotification("Banco restaurado.", type="message") })
}

shinyApp(ui = ui, server = server)