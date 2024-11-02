library(tidyverse)
library(janitor)
library(DBI)
library(pool)
library(RPostgres)
library(stringr)
library(collapse)
library(gsubfn)
library(glue)
library(splitstackshape)
library(lubridate)
library(ggplot2)
library(highcharter)
library(bs4Dash)
library(bslib)
library(DT)
library(flextable)
library(fresh)
library(memoise)
library(openssl)
library(scales)
library(shinyalert)
library(shinycssloaders)
library(shinyFeedback)
library(shinyjs)
library(shinymanager)
library(shinyvalidate)
library(shinyWidgets)
library(sodium)
library(sp)
library(shadowtext)
library(ggtext)
library(data.table)
library(shiny)
library(RColorBrewer)
library(waiter)
shinyOptions(cache = cachem::cache_disk("./bind-cache"))
set.seed(123)
options(scipen = 9999)
options(device.ask.default = FALSE)

set_labels(
  language = "en",
  "Please authenticate" = "",
  "Username:" = "Usuário:",
  "Password:" = "Senha:",
  "Login" = "Aceder"
)
  
  {
    db2 <- 'semea'
    host_db2 <- "semea.choww6kimn1s.eu-north-1.rds.amazonaws.com"
    db_port2 <- '5432'
    db_user2 <- "postgres"
    db_password2 <- "FAR;2030,"
    far_pool <- dbPool(RPostgres::Postgres(), dbname = db2, host=host_db2, port=db_port2, user=db_user2, password=db_password2)
  } ### credentials
  
  BLUE <- "#076fa2"
  RED <- "#E3120B"
  BLACK <- "#202020"
  GREY <- "grey50"  

{
title <- tags$a(href='https://finance.far.org',tags$img(src="FARFPNEW.png", height = '150', width = '240'), '', target="_blank")
user_image <- "https://img.icons8.com/color/96/000000/circled-user-male-skin-type-6--v1.png"
provincias  <- c('CABO DELGADO', 'GAZA', 'INHAMBANE', 'MANICA', 'MAPUTO', 'MAPUTO CIDADE', 'NAMPULA', 'NIASSA', 'SOFALA', 'TETE', 'ZAMBÉZIA')
opcoes <- dbGetQuery(far_pool, SQL(paste0("SELECT DISTINCT proposta FROM vistas.listados")))
} ########### LOAD DATASETS
{
two_decimals <- scales::label_comma(accuracy = .2, big.mark = ".", decimal.mark = ",")
zero_decimals <- function (numero){prettyNum(numero, big.mark = ",")}
grafico_barras <- function(data, categoria, valor, meta){
  data <- data %>% mutate(valores := {{valor}}) %>% 
    mutate(categoria := as.character({{categoria}}),
           valor := as.numeric({{valor}}),
           categoria := fct_reorder({{categoria}}, {{valor}}),
           fill := ifelse(valor == max({{valor}}), "Dark", "Light"))
  
  no_y_grid_plot <- ggplot(data, aes(valor, categoria)) +
    geom_col(aes(x = valor, y=categoria, fill  = fill))+
    theme_minimal(base_size = 14)+
    geom_text(
      data = data,
      mapping = aes(x = valor, y = categoria, label = valor),
      hjust = 1,
      nudge_x = -0.1,
      color = 'white',
      fontface = 'bold',
      size = 4.5
    ) +
    scale_fill_manual(values = c('#008000', '#A2AD9C'))+
    geom_text(
      data = data,
      mapping = aes(x = 0, y = categoria, label = categoria),
      hjust = 0,
      nudge_x = 0.25,
      color = 'white',
      fontface = 'bold',
      size = 4.5
    )+
    # geom_vline(xintercept=40,col = "red", lty=2) +
    # geom_vline(xintercept = 0) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(breaks = NULL) +
    labs(x = element_blank(), y = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = "none")
  no_y_grid_plot
}

box_title_js <- '
  Shiny.addCustomMessageHandler("box_title", function(title) {
  if(title.includes("mpg")){
    colour = "red"
  } else {
    colour = "blue"
  }
    $("#box_plot h3").html(title)
    $("#box_plot .card-body").css("background-color", colour)
  });
'

initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#228B22', 'color': '#fff'});",
  "$(this.api().table().body()).css({'font-weight': 'normal'});",
  "}"
) 

farfpdatatable <- function(dataset){
  datatable(dataset, rownames= FALSE, 
            extensions = 'Buttons',
            selection = "none", 
            options = list(
              initComplete = initComplete,
              scrollX = TRUE,
              autoWidth = FALSE,
              dom = 'Blrtip',
              buttons = list(
                I('colvis'), 'copy', 'print',
                list(
                  extend = 'collection',
                  buttons = list(list(extend = "csv", filename = "page", exportOptions = list(columns = ":visible", modifier = list(page = "current"))),
                                 list(extend = 'excel', filename = "page", title = NULL, exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                  text = 'Baixar dados visíveis'),
                list(
                  extend = 'collection',
                  buttons = list(
                    list(extend = "csv", filename = "data",exportOptions = list(columns = ":visible",modifier = list(page = "all"))),
                    list(extend = 'excel', filename = "data", title = NULL, exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                  text = 'Baixar todos os dados')), lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All'))
            ),
            class = "display"
  )
} ########### FARFP STYLE DATATABLE

}########### FUNCTIONS
  
{
N <- 20

x <- cumsum(rnorm(N)) + 0.5 * cumsum(runif(N))
x <- round(200*x)
  
df <- data.frame(x = sort(as.Date(Sys.time() - lubridate::days(1:N))), y = abs(x))

hc_theme_sparkline_vb <- function(...) {
 theme <- list(chart = list( backgroundColor = NULL,margins = c(0, 0, 0, 0),spacingTop = 0,spacingRight = 0, spacingBottom = 0, spacingLeft = 0, plotBorderWidth = 0, borderWidth = 0, style = list(overflow = "visible")),
    xAxis = list(visible = FALSE, endOnTick = FALSE, startOnTick = FALSE),
    yAxis = list(visible = FALSE, endOnTick = FALSE,  startOnTick = FALSE),
    tooltip = list(outside = FALSE, shadow = FALSE, borderColor = "transparent", botderWidth = 0, backgroundColor = "transparent", style = list(textOutline = "5px white")),
    plotOptions = list(series = list(marker = list(enabled = FALSE),
        lineWidth = 2, shadow = FALSE, fillOpacity = 0.25, color = "#FFFFFFBF",
        fillColor = list(linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0), stops = list(list(0.00, "#FFFFFF00"), list(0.50, "#FFFFFF7F"), list(1.00, "#FFFFFFFF"))))),
    credits = list(enabled = FALSE, text = ""))
  
  theme <- structure(theme, class = "hc_theme")
  if (length(list(...)) > 0) {theme <- hc_theme_merge(theme, hc_theme(...))}
  theme
}

bullet_chart <- function(respondentes){
  realizado <- respondentes %>% group_by(categoria = distrito) %>% summarize(realizado = fndistinct(codigo))
  targeted <- meta %>% group_by(categoria = distrito)  %>% summarize(meta = fsum(as.numeric(metas)))
  realizacoes <- head(realizado,15) %>% left_join(targeted, by = "categoria")
  
  realizacoes %>%  mutate(categoria := fct_reorder(categoria, realizado)) %>% 
    ggplot(aes(x = realizado, y = categoria)) +
    geom_col(aes(x = meta), fill = "gray") +
    geom_col(width = 0.6, fill = "#008000") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())+
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_text(size=15),
          plot.title = element_markdown(lineheight = 1.1),
          legend.text = element_markdown(size = 15))+
          # size = rel(1))+
    coord_cartesian(ylim = c(10, max(realizacoes$meta)+10)) +
    coord_cartesian(xlim = c(10,  max(realizacoes$meta))) +
    geom_shadowtext(
      data = subset(realizacoes, meta < 8),
      aes(meta, y = categoria, label = categoria),
      hjust = 0, nudge_x = 0.3, colour = BLUE,
      bg.colour = "white", bg.r = 0.2)+
    # geom_text(data = subset(realizacoes, meta >= 8), aes(0, y = categoria, label = categoria), nudge_x = 1.0, colour = "white", size = 7)+
    geom_text(data = realizacoes, mapping = aes(x = realizado, y = categoria, label = realizado), hjust = 1.5, nudge_x = -0.2, color = 'lightgreen', fontface = 'bold', size = 7)+
    geom_text(data = realizacoes, mapping = aes(x = meta, y = categoria, label = meta), hjust = 1.5, nudge_x = -0.2, color = 'white', fontface = 'bold', size =7)
}  ########### CHART FUNCTION

bullet_chart4 <- function(respondentes){
  total <- total %>% mutate(label_meta = ifelse(meta*0.95 <= realizado, NA, meta))
  
  total %>%  mutate(categoria := fct_reorder(categoria, realizado)) %>% 
    mutate(meta := ifelse(situacao == 'Alcance', 0, meta)) %>% 
    ggplot(aes(x = realizado, y = categoria)) +
    geom_col(aes(x = meta), fill = "gray") +
    geom_col(width = 0.6, fill = "#008000") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())+
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          legend.text = element_markdown(size = 15))+
    labs(
      title = " <span style='font-size:18pt'> **Inquérito de Avaliação de Resultados (COI): Estudo de Base do PROCAVA** <br> </span>
    <span style='font-size:18pt'> Agregados familiares
    <span style='color:#008000;'>entrevistados </span>em comparação com os
    <span style='color:#708090;'>planificados</span>
    </span>",
      caption = "Fonte: FAR-FP | DPMA")+
    coord_cartesian(ylim = c(10, max(total$meta)+10)) +
    coord_cartesian(xlim = c(10,  max(total$meta))) +
    geom_shadowtext(
      data = subset(total, meta < 8),
      aes(meta, y = categoria, label = categoria),
      hjust = 0, nudge_x = 0.3, colour = BLUE,
      bg.colour = "white", bg.r = 0.2)+
    geom_text(data = subset(total, meta >= 8), aes(0, y = categoria, label = categoria), hjust = -0.4, nudge_x = 0.0, colour = "white", size = 7)+
    geom_text(data = total, mapping = aes(x = realizado, y = categoria, label = realizado), hjust = 1.5, nudge_x = -0.2, color = 'white', fontface = 'bold', size = 7)+
    geom_text(data = total, mapping = aes(x = meta, y = categoria, label = label_meta), hjust = 1.5, nudge_x = -0.2, color = 'white', fontface = 'bold', size =7)
}  ########### CHART FUNCTION

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, icon = NULL, color = "aqua", width = 3, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-sm icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

hc <- hchart(df, "area", hcaes(x, y), name = "Participantes")  %>% hc_size(height = 20) %>% hc_credits(enabled = TRUE) %>% hc_add_theme(hc_theme_sparkline_vb()) 
hc2 <- hchart(df, "line", hcaes(x, y), name = "Beneficiários")  %>% hc_size(height = 20) %>% hc_credits(enabled = FALSE) %>% hc_add_theme(hc_theme_sparkline_vb()) 
hc3 <- hchart(df, "column", hcaes(x, y), name = "Agregados familiares")  %>% hc_size(height = 20) %>% hc_credits(enabled = FALSE) %>% hc_add_theme(hc_theme_sparkline_vb()) 



} ############ VALUE BOX FUNCTIONS

ui <- dashboardPage(

  dashboardHeader(title =  title, rightUi = userOutput("user_names")),
  dashboardSidebar(disable = FALSE, minified = F, collapsed = TRUE, sidebarMenu(
    menuItem("ASSISTÊNCIA", startExpanded = FALSE, icon = icon("motorcycle"),
             menuSubItem("Famílias", tabName = "farfp_outreach", icon = icon("arrows-down-to-people")),
             menuSubItem("Grupos", tabName = "op_msme", icon = icon("users-between-lines"))
             ),
    menuItem("INVESTIMENTOS", startExpanded = FALSE, icon = icon("industry"),
             menuSubItem("Infraestruturas", tabName = "technicians_trainings", icon = icon("warehouse")),
             menuSubItem("Equipamentos", tabName = "beneficiaries_trainings", icon = icon("tractor"))
                                 ))),
  dashboardBody(
    autoWaiter(),
    tags$head(tags$style(HTML('.btn{background-color:green}'))),
                tags$head(tags$style(".modal-dialog{ width:2000px}")),
                tags$head(tags$style(HTML(".selectize-input {padding-left:4px;padding-right:4px;  font-size: 12pt; width: 90%;}"))),
                tags$style(type='text/css', "#button {padding: 10px 20px; border-radius: 5px; display: inline-block; margin: 5px;}"),

    tabItems(
      tabItem("farfp_outreach", 
              fluidRow(
                valueBoxOutput("vbox", width = 2),
                valueBoxOutput("vbox2", width = 2),
                valueBoxOutput("vbox3", width = 2),
                valueBoxOutput("vbox4", width = 2),
                valueBoxOutput("vbox5", width = 2),
                valueBoxOutput("vbox6", width = 2)
                ),
              
              fluidRow(
                box(title = "AGREGADOS FAMILIARES CADASTRADOS POR TÉCNICO", closable = TRUE, maximizable = TRUE, width = 6, 
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    plotOutput("familias_assistidas", height=700)),
                box(title = "AGREGADOS FAMILIARES CADASTRADOS POR DISTRITO", closable = TRUE, maximizable = TRUE, width = 6, 
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    plotOutput("participantes", height=700)),
                box(title = "AGENTES PROPOSTOS", closable = TRUE, maximizable = TRUE, width = 12, collapsed = TRUE,
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    DTOutput("agentespropostos"))

                 )
              )
  
  )),
  
  footer = bs4DashFooter(left = a(href = "www.farfp.com", target = "_blank", "Direitos reservados ao FAR-FP"), right = paste0("Actualização: ", format(Sys.Date(), "%b"), " ", format(Sys.Date(), "%Y"))),
  controlbar = dashboardControlbar(
    tags$head(tags$style(HTML(".selectize-input {margin: 12px 12px 12px 12px; font-size: 12pt; width: 95%;}"))),
    tags$style(type='text/css', "#button {margin: 12px 12px 12px 12px; border-radius: 0px; display: inline-block;}"),
    tags$head(tags$style(type="text/css",".control-label {margin: 12px 12px 12px 12px; width: 100%; width = 95%;}")),
    id = "controlbar", collapsed = TRUE, overlay = TRUE,
    selectizeInput("selected_province", label = h5("Províncias"), c("Todas", provincias), selected = "Todas"),
    selectizeInput("intervencao_selecionada", "Intervenções", choices = c("Todas", opcoes), selected = 'PAC'),
    actionButton("dados_detalhados", "Obter Dados", icon = icon("play-circle"), style="margin:0 auto; width: 200px; margin: 12px 12px 12px 12px; display: center-align; color: #fff; background-color:#008000")
    )
)

ui <- secure_app(
  ui = ui,
  tags_top = 
    tags$div(
      tags$h4("", style = "align:center"),
      tags$a(href='https://farfp.com/assistencia',
             tags$img(src="FARFPLONG.png", height = '100.0', width = '475'),
             '', target="_blank")
    ),

  tags_bottom = tags$div(
    tags$p("", tags$a(href = "mailto:info@dpma.gov.mz?Subject=Shiny%20aManager",target="_top", "Solicitar Assistência")),
    tags$p("", tags$a(href = " https://farfp.shinyapps.io/user_registration", target="_top", "Actualizar Credenciais"))
  )
)


server <- function(input, output, session) {
  
  
      observeEvent(input$dados_detalhados, {
        showModal(modalDialog(title = h1("DADOS DETALHADOS"),
          DT::dataTableOutput('dadosdetalhado'),
          size = c("xl"),
          fade = TRUE, position = c("centered"), transparent = FALSE,
          easyClose = TRUE,
          footer = modalButton("Fechar")
        ))
      })

  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  credentials <-  DBI::dbGetQuery(far_pool, "SELECT * FROM vistas.utente WHERE cargo NOT IN ('Técnico de Campo')")
  creds_reactive <- reactive({reactiveValuesToList(res_auth)})

  output$user_names <- renderUser({
    dashboardUser(
      name = creds_reactive()$nome,
      image = user_image, 
      title = creds_reactive()$cargo,
      subtitle = NULL,
      footer = NULL
    )
  })
  
  output$vbox <- renderValueBox({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    valor <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT person_id) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND data between '2024-10-10' AND CURRENT_DATE"))
    valor_24 <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT person_id) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito  WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND data BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()"))

    vb <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      title = toupper("Famílias cadastradas"), sparkobj = hc2,
      info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
      icon = icon("people-roof"), width = 2, color = "green", href = NULL)
    vb})

  output$vbox2 <- renderValueBox({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    valor <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT tecnico) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito  WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND data between '2024-10-10' AND CURRENT_DATE"))
    valor_24 <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT tecnico) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND data BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()"))
    vb <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      title = toupper("EXTENSIONISTAS ENVOLVIDOS"), sparkobj = hc2,
      info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
      icon = icon("person-biking"), width = 2, color = "orange", href = NULL)
    vb})
    
  output$vbox3 <- renderValueBox({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    valor <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT P.person_id) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id left join familias.sexo S ON S.person_id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND  sexo = 'Mulheres' AND data between '2024-10-10' AND CURRENT_DATE"))
    valor_24 <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT P.person_id) FROM familias.propostas P  LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id left join familias.sexo S ON S.person_id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND sexo = 'Mulheres' AND data BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()"))

    vb3 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"), valor_24, " nas últimas 24 horas"),
      
                         title = toupper("Mulheres cadastradas"), sparkobj = hc3,
                         info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
                         icon = icon("venus"), width = 2, color = "yellow", href = NULL)
    vb3})
  
  output$vbox4 <- renderValueBox({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    valor <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT I.person_id) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id LEFT JOIN familias.idades I  ON P.person_id = I.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito  WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND data between '2024-10-10' AND CURRENT_DATE AND ano  > 1989"))
    valor_24 <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT( DISTINCT I.person_id) FROM familias.propostas P  LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id LEFT JOIN familias.idades I  ON P.person_id = I.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito  WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND data BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW() AND ano  > 1989"))
    
    vb4 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      title = toupper("Jovens cadastrados"), sparkobj = hc3,
      info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
      icon = icon("children"), width = 2, color = "purple", href = NULL)
    
    vb4})
  
  output$vbox5 <- renderValueBox({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    valor <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT (DISTINCT person_id) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND (proposta LIKE '%PAC%' OR proposta LIKE '%PCC%' OR proposta LIKE '%PRON%' OR proposta LIKE '%VCO%' OR proposta LIKE '%DIF%' OR PROPOSTA LIKE '%multiplicacao_sementes%')"))
    valor_24 <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT (DISTINCT person_id) FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND (proposta LIKE '%PAC%' OR proposta LIKE '%PCC%' OR proposta LIKE '%PRON%' OR proposta LIKE '%VCO%' OR proposta LIKE '%DIF%' OR proposta LIKE '%multiplicacao_sementes%') AND data BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()"))
    
    vb5 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
                         title = toupper("Agentes locais (PAC, PCC, VCO, PRON, DIF, MS)"),
                         sparkobj = hc3,
                         info =     tags$span(tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "message")),
                         icon = icon("person-chalkboard"),
                         width = 2,
                         color = "fuchsia",
                         href = NULL)
  vb5})
  
  output$vbox6 <- renderValueBox({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    valor <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT(DISTINCT members) FROM (SELECT P.person_id::text || C.member::text AS members FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id LEFT JOIN familias.pwd C ON C.people_id = F.id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "')AND (data BETWEEN '2024-10-10' AND CURRENT_DATE))"))
    valor_24 <- DBI::dbGetQuery(far_pool, paste0("SELECT COUNT(DISTINCT members) FROM (SELECT P.person_id::text || C.member::text AS members FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id LEFT JOIN familias.pwd C ON C.people_id = F.id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "')AND (data BETWEEN NOW() - INTERVAL '24 HOURS' AND NOW()))"))
    
    vb6 <- valueBoxSpark(
      value =  zero_decimals(valor),
      subtitle = tagList(HTML("&uarr;"),  "", valor_24, " nas últimas 24 horas"),
      
      title = toupper("Pessoas com Deficiência"),
      sparkobj = hc3,
      info = tags$span("Fixed ratio", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "message")),
      icon = icon("wheelchair"),
      width = 2, color = "olive", href = NULL)

    vb6})

  output$familias_assistidas <- renderPlot({
    admin_id <- as.data.frame(locais())
    provinciaselecionada = dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    intervencao = dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    
    agregados_familiares <- DBI::dbGetQuery(far_pool, paste0("SELECT nuit, E.nome || ' ' || E.apelido As nome, COUNT(distinct person_id) AS agregados FROM familias.propostas P LEFT JOIN odk.rotulos R ON R.name = P.proposta LEFT JOIN familias.pessoas F ON F.id = P.person_id  LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito LEFT JOIN pessoal.extensao E ON E.nuit::numeric = P.tecnico::numeric  WHERE   F.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND E.nome NOTNULL AND rotulo IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') GROUP BY nuit, E.nome || ' ' || E.apelido ORDER BY COUNT(distinct person_id) DESC LIMIT 15")) %>% 
      mutate(
        color = case_when(
          row_number() == 1 ~ "#008000",
          row_number() == 2 ~ "#32CD32",
          row_number() == 3 ~ "#7CFC00",
          row_number() >= 9 ~ "#FFC300",
          TRUE ~ "gray70"))

    agregados_familiares %>%  mutate(nome = fct_reorder(nome, agregados)) %>% 
      ggplot(aes(x = nome, y = agregados, fill = color)) +
      geom_col() +
      coord_flip()+
      geom_text(aes(label = agregados),  size = 8, hjust = 1.5,  vjust = 0.5) +
      scale_fill_identity(guide = "none") +
      theme_void()+
      theme(axis.text.y = element_text(size = 15, angle = 0, vjust=0, hjust=0))
  })
  
  output$participantes <- renderPlot({
    withProgress(message = 'Preparando os seus dados. ', detail = 'Aguarde, por favor...', value = 0, {for (i in 1:15) {incProgress(1/15)
      Sys.sleep(0.25)}})
    admin_id <- as.data.frame(locais())
    provinciaselecionada = dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    intervencao = dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    
    agregados_familiares <- DBI::dbGetQuery(far_pool, paste0("SELECT distrito, COUNT(distinct person_id) AS agregados FROM familias.propostas S  LEFT JOIN odk.rotulos R ON R.name = S.proposta LEFT JOIN familias.pessoas P ON P.id = S.person_id LEFT JOIN moz.localidades L ON L.id = P.admin_id LEFT JOIN  moz.distritos D ON D.id = L.distrito WHERE rotulo IN ('",intervencao , "') AND P.admin_id IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND provincia IN ('", provinciaselecionada, "') AND distrito NOTNULL GROUP BY distrito ORDER BY COUNT(distinct person_id) DESC LIMIT 15")) %>% 
      mutate(
        color = case_when(
          row_number() == 1 ~ "#008000",
          row_number() == 2 ~ "#32CD32",
          row_number() == 3 ~ "#7CFC00",
          row_number() >= 9 ~ "#FFC300",
          TRUE ~ "gray70"))
    
    agregados_familiares %>% mutate(distrito = fct_reorder(distrito, agregados)) %>% 
      ggplot(aes(x = distrito, y = agregados, fill = color)) +
      geom_col() +
      coord_flip()+
      geom_text(aes(label = agregados),  size = 8, hjust = 1.5,  vjust = 0.5) +
      scale_fill_identity(guide = "none") +
      theme_void()+
      theme(axis.text.y = element_text(size = 15, angle = 0, vjust=0, hjust=0))
  })
  
  output$dadosdetalhado <- renderDT({
    admin_id <- as.data.frame(locais())
    intervencao <- dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    provinciaselecionada <- dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    
    listagem <- dbGetQuery(far_pool, SQL(paste0("SELECT * FROM vistas.listados WHERE proposta IN ('",intervencao , "') AND provincia IN ('", provinciaselecionada, "') AND (proposta LIKE '%PAC%' OR proposta LIKE '%MLS%' OR proposta LIKE '%PRON%' OR proposta LIKE '%VCO%' OR proposta LIKE '%PCC%' OR proposta LIKE '%DIF%') AND codlocalidade IN (", glue_sql_collapse(admin_id$admin_id, sep = ", "), ") AND (proposta LIKE '%PAC%' OR proposta LIKE '%PCC%' OR proposta LIKE '%VCO%' OR proposta LIKE '%multiplic%' OR proposta LIKE '%PRON%')"))) %>% dplyr::select(-codlocalidade)
    listagem <- listagem %>%  distinct_all() %>% pivot_wider(names_from = proposta, values_from = quantidade)
    listas <- listagem %>% dplyr::select(codbeneficiario:ncol(listagem)) 
    listas[] <- Map(paste, names(listas),listas, sep = ' ') 
    listas[]<-  Map(paste, listas, "%", sep = '')
    
    listas <- listas %>% mutate(dplyr::across(.cols = everything(), .fns = ~ dplyr::if_else(stringr::str_detect(.x, "(NA)"), NA, .x)))
    listas$codbeneficiario <- gsub("codbeneficiario ", "", listas$codbeneficiario, fixed = TRUE)
    listas$codbeneficiario <- gsub('%','',listas$codbeneficiario, fixed = TRUE)
    listas_text <- listas %>%  unite('Propostas', 2:ncol(listas), sep = ', ', na.rm = TRUE)
    listed <- listagem %>% select(provincia:codbeneficiario) %>% left_join(listas_text, by = "codbeneficiario") %>% 
      dplyr::select(codigo=codbeneficiario, everything()) %>% 
      mutate_all(funs(str_replace(., "1%", ""))) %>% 
      mutate_all(funs(str_replace(., "PAC 1%", "PAC"))) %>%
      mutate_all(funs(str_replace(., "VCO 1%", "VCO"))) %>%
      mutate_all(funs(str_replace(., "PRON 1%", "PRON"))) %>%
      mutate_all(funs(str_replace(., "MLS 1%", "MLS"))) %>%
      mutate_all(funs(str_replace(., "PCC 1%", "PCC")))
    farfpdatatable(listed)
  })
  
  output$agentespropostos <- renderDT({

    admin_id <- as.data.frame(locais())
    provinciaselecionada = dplyr::case_when(input$selected_province== "Todas" ~ glue_sql_collapse(c("MAPUTO", "GAZA", "INHAMBANE", "MAPUTO CIDADE", "MANICA", "SOFALA", "TETE", "ZAMBÉZIA", "NAMPULA", "CABO DELGADO", "NIASSA"), sep = "', '"), TRUE ~ glue_sql_collapse(input$selected_province))
    intervencao = dplyr::case_when(input$intervencao_selecionada == "Todas" ~ glue_sql_collapse(opcoes$proposta, sep = "', '"), TRUE ~ glue_sql_collapse(input$intervencao_selecionada))
    
    query <- glue_sql_collapse(paste0("SELECT  CASE WHEN proposta = 'multiplicacao_sementes' THEN 'MS' ELSE proposta END AS proposta, distrito, COUNT(DISTINCT F.id) AS agentes FROM familias.propostas P LEFT JOIN familias.pessoas F ON F.id = P.person_id LEFT JOIN familias.sexo S ON S.person_id = F.id LEFT JOIN familias.idades I ON I.person_id = F.id LEFT JOIN moz.localidades L ON L.id = F.admin_id LEFT JOIN moz.distritos D ON D.id = L.distrito WHERE proposta IN ('",intervencao , "') AND (proposta LIKE '%PAC%' OR proposta LIKE '%MLSC%' OR proposta LIKE '%PRON%' OR proposta LIKE '%VCO%'  OR proposta LIKE '%PCC%'  OR proposta LIKE '%DIF%') AND F.admin_id IN (", glue_sql_collapse(locais()$admin_id, sep = ", "), ") AND provincia IN ('", provinciaselecionada, "') AND distrito NOTNULL AND (proposta LIKE '%PAC%' OR proposta LIKE '%PCC%' OR proposta LIKE '%PRON%' OR proposta LIKE '%VCO%' OR PROPOSTA LIKE '%DIF%' OR PROPOSTA LIKE '%multiplicacao_sementes%') GROUP BY proposta, distrito"))
    agentes <- DBI::dbGetQuery(far_pool, query)
    
    agentes <- agentes %>% pivot_wider(names_from = "proposta", values_from = "agentes") %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% mutate_all(as.character)
    agentes[,2:ncol(agentes)] <- sapply(agentes[,2:ncol(agentes)],as.numeric)
    agentes <- agentes %>% adorn_totals(c("col")) %>% arrange(-Total) %>% adorn_totals("row")
    
    farfpdatatable(agentes)
  })
  
  locais <- reactive({
    selected_province <-  "Todas"
    selected_district <-  "Todos"
    area_influence <-  creds_reactive()$area_influence
    computed_district <-  creds_reactive()$distrito
    computed_province <-  creds_reactive()$provincia
    nivel_acesso <-  creds_reactive()$privilegio
    
    geografia <-  dbGetQuery(far_pool, SQL(paste0("SELECT L.id AS admin_id, posto_id, L.distrito, localidade, provincia FROM moz.localidades L LEFT JOIN moz.distritos D ON D.id = L.distrito LEFT JOIN moz.postos P ON P.id = L.posto_id")))
    if(area_influence == "URGPS" & nivel_acesso < 12){geografia <- fsubset(geografia, provincia %in% c("MAPUTO", "MAPUTO CIDADE", "GAZA", "INHAMBANE"))}
    if(area_influence =="URGPC" & nivel_acesso < 12){geografia <- fsubset(geografia, provincia %in% c("MANICA", "SOFALA", "TETE"))}
    if(area_influence == "URGPN" & nivel_acesso < 12){geografia <- fsubset(geografia, provincia %in% c("ZAMBÉZIA", "NAMPULA", "CABO DELGADO"))}
    if(area_influence == "UPGPN" & nivel_acesso < 12){geografia <- fsubset(geografia, provincia %in% c("NIASSA"))}
    if(nivel_acesso < 10){geografia <- fsubset(geografia, provincia == computed_province)}
    if(nivel_acesso < 8){geografia <- fsubset(geografia, distrito == computed_district)}
    if(nivel_acesso < 4){geografia <- fsubset(geografia, admin_id == computed_admin_id)}
    if(selected_province != "Todas" & !is.na(selected_province)){geografia <- fsubset(geografia, provincia %in% selected_province)}
    if(selected_district != "Todos" & !is.na(selected_district)){geografia <- fsubset(geografia, distrito %in% selected_district)}
    geografia
  })
  
}

shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
