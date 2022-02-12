#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(sf)
library(stringr)


arg_dep <- readRDS("argentina_covid_dep.RDS")
arg_prov <- readRDS("argentina_covid_prov.RDS")
arg_data <- readRDS("argentina_data.RDS")

arg_date_confir <- arg_data %>% select(date, NAME_1, NAME_2, confirmed, pro.confir, 
                                       population, administrative_area_level_1)

arg_date_falle <- arg_data %>% select(date, NAME_1, NAME_2, deaths, pro.muertes, 
                                       population, administrative_area_level_1)

arg_date_test <- arg_data %>% select(date, NAME_1, NAME_2, tests, pro.test, 
                                       population, administrative_area_level_1)

arg_date_vacu <- arg_data %>% select(date, NAME_1, NAME_2, people_vaccinated, pro.gente.vacu, 
                                       population, administrative_area_level_1)

ui <- dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "Covid-19 en Argentina", 
                  tags$li(class="dropdown", tags$a(href="https://twitter.com/Vidal_Lucas_", icon("twitter"), "Mi perfil", target="_blanck")), 
                 tags$li(class="dropdown", tags$a(href="https://github.com/LucasDavid12/covid_19", icon("github"), "Código", target="_blanck"))),
  dashboardSidebar(
    selectInput("fecha", 
                "Seleccione un periodo:", 
                choices = unique(arg_dep$date)),
    div(style="display:inline-block;width:32%;text-align: center;", actionButton("amba", label = NULL, style = "width: 60px; height: 60px;
background: url('https://lh4.googleusercontent.com/s8zT3gNEk0o6pOfcpVVUG2ddodc4-_Sgzs8tJ1gCn3StJHQn99YrqxZwL6F9JCUquQ_YMY9Zdb3UsqcM51c1osc0UxDhU0kyGxe967HsIm0x46k9F69w32dRsH_bLqH2nBI5LzP5');  background-size: cover; background-position: center;")),
    div(style="display:inline-block;width:32%;text-align: center;", actionButton("nor", label = NULL, style = "width: 60px; height: 60px;
background: url('https://lh5.googleusercontent.com/Ou00Rsq1r9q9pRwVhNmiTtt00A2bWYoNO2ag4Gds6fm6p7Z05XNm4qM8B8WySdMqw6zXKmz9m-VFMtxP0LbspYYuTciFw6pamicrBPFLM4n0Y_Gc6BHxkijgjTkUUXh3I_q0i9eA');  background-size: cover; background-position: center;")),
    div(style="display:inline-block;width:32%;text-align: center;", actionButton("centro", label = NULL, style = "width: 60px; height: 60px;
background: url('https://lh4.googleusercontent.com/VZkG62RHYOD0HFeFrFzz8b6qhRZLkV1di-qsprplUJPMUFZUkudKq25nR_4FXlOpL-s5OeP9yCDdQJegUI1lHGnnPqG5eoXmMJYwHDyyOl7KQ_xYQlfZqN7GnXEsZ9WqG7XV4rOx');  background-size: cover; background-position: center;")), 
    div(style="display:inline-block;width:32%;text-align: center;", actionButton("sur", label = NULL, style = "width: 60px; height: 60px;
background: url('https://lh3.googleusercontent.com/IclLBPF7PLc9tSXCT7if3yN_y7Q-Pc9ARxUhZjiZMNg06FzdQROk2eVtrb6GBAhC7KujUEABMUwcEAXslDq_0hRk_kEJbS3Kfgsk9-x21sjY06myxRQsxfwNIdj2aKsZHXoEoElf');  background-size: cover; background-position: center;")),
    h5("Este entorno fue desarrollado a través de ", 
       tags$a(href = "https://covid19datahub.io/", "COVID-19 Data Hub"), ", el cual proporciona un conjunto de datos unificados
       y detallados de todo el mundo, útil para una mejor comprensión del COVID-19", align = "center")),
  dashboardBody(
    tabsetPanel( 
      tabPanel("Confirmados", icon = icon("virus"), leafletOutput("Confirmados"), box(title = strong("Provincias con más casos confirmados"), status= "warning", solidHeader = T, collapsible = T, plotOutput(outputId = "Gra_Confi")),
               box(title =  strong("Departamentos con más casos confirmados"), status = "warning", solidHeader = T, collapsible = T, plotOutput("Gra_Confi_dpto")),
               htmlOutput(outputId = "total_confi"), downloadButton("downloadDataConfir", "Descargar info. sobre los casos confirmados")),
      tabPanel("Test", icon = icon("vial"), leafletOutput("test"), box(title = strong("Provincias con más testeos"), status= "info", solidHeader = T, collapsible = T, plotOutput(outputId = "Gra_test")),
               box(title =  strong("Departamentos con más testeos"), status = "info", solidHeader = T, collapsible = T, plotOutput("Gra_test_dpto")),
               htmlOutput(outputId = "total_test"), downloadButton("downloadDataTest", "Descargar info. sobre los tests")),
      tabPanel("Fallecimientos", icon = icon("times"), leafletOutput("Fallecimientos"), box(title = strong("Provincias con más decesos por Covid-19"), status= "danger", solidHeader = T, collapsible = T, plotOutput(outputId = "Gra_falle")),
               box(title =  strong("Departamentos con más decesos por Covid-19"), status = "danger", solidHeader = T, collapsible = T, plotOutput("Gra_falle_dpto")),
               htmlOutput(outputId = "total_falle"), downloadButton("downloadDataFalle", "Descargar info. sobre los fallecimientos")),
      tabPanel("Personas Vacunadas", icon = icon("syringe"), leafletOutput("Vacunados"), box(title = strong("Provincias con más personas vacunadas"), status = "success", solidHeader = T, collapsible = T, plotOutput("Gra_gen_vacu")), 
               box(title = strong("Departamentos con más personas vacunadas"), status = "success", solidHeader = T,  collapsible = T, plotOutput("Gra_gen_vacu_dpto")), 
               htmlOutput("total_gen_vacu"), downloadButton("downloadDataVacu", "Descargar info. sobre las Vacunas")), 
      tags$div(id = "cite", "Data compilada por: ", tags$em("Guidotti, E., Ardia, D., (2020), 'COVID-19 Data Hub', Journal of Open Source Software 5(51):2376, doi: 10.21105/joss.02376."))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  fecha_covid <- reactive({
    lv <- arg_dep %>% filter(date == input$fecha)
    return(lv)
  })
  
  fecha_covid2 <- reactive({
    lv <- arg_prov %>% filter(date == input$fecha)
    return(lv)
  })  
  
  output$total_confi <- renderUI({
    
    num_conf <- sum(fecha_covid2()$confirmed)
    
    num_conf <- format(num_conf, big.mark = ".")
    
    h4(strong("Casos confirmados a nivel nacional en el mes: ", num_conf))
    
  })
  output$Confirmados <- renderLeaflet({
    
    bins_conf <- c(0, 10, 50, 200, 450, 950, 2500, 4500, 9500)
  
    icono_covid = "https://cdn.pixabay.com/photo/2020/05/22/19/50/corona-5206900_1280.png"
    
    pal_confir <- colorBin(palette = "OrRd", domain = arg_prov$pro.confir, bins = bins_conf)
    
    leaflet() %>% 
      addTiles('http://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG:3857@png/{z}/{x}/{-y}.png', 
           attribution = "Argenmap v2 - Instituto Geográfico Nacional") %>%
      addPolygons(data = fecha_covid2(), weight = 1, color = "black", label = 
                    paste0(as.character(fecha_covid2()$NAME_1),
                           " | Cantidad de casos: ", as.character(fecha_covid2()$confirmed)),
                  fillColor = ~pal_confir(fecha_covid2()$pro.confir), 
                  fillOpacity = 0.8, 
                  highlight = highlightOptions(weight = 3, 
                                               color = "black", 
                                               bringToFront = T)) %>%
      addLegend(position = "bottomright", 
                pal = pal_confir,
                values = arg_prov$pro.confir,
                title = "Casos cada 100.000 pers.") %>% 
      addMarkers(lat = fecha_covid()$latitude, lng = fecha_covid()$longitude,
                 label = 
                   paste0(as.character(fecha_covid()$NAME_2),
                          " | Cantidad de casos: ", as.character(fecha_covid()$confirmed)), 
                 icon = list(
                   iconUrl = icono_covid, 
                   iconSize = c(20, 20)
                 ), 
                group = "Departamentos", 
                clusterOptions = markerClusterOptions()) %>% 
    addLayersControl(overlayGroups = "Departamentos", 
                    options = layersControlOptions(collapsed = F)) %>% 
  hideGroup("Departamentos")
    
  })
  output$Gra_Confi <- renderPlot({
    
    
    pal_plot_confir <- c('#800026','#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c',
                         '#feb24c', '#fed976', '#ffeda0','#ffffcc')
    
    confir <- fecha_covid2() %>%
      arrange(desc(pro.confir)) %>% head(9)
    
    ggplot(data = confir, mapping = aes(x = reorder(NAME_1, pro.confir), 
                                        y = pro.confir)) +
      geom_bar(stat = "identity", fill = pal_plot_confir, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Casos confirmados cada 100mil personas")
    
    
  })
  output$Gra_Confi_dpto <- renderPlot({
    
    pal_plot_confir <- c('#800026','#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c',
                         '#feb24c', '#fed976', '#ffeda0','#ffffcc')
    
    
    confir_2 <- fecha_covid() %>% arrange(desc(pro.confir)) %>% head(9) %>% 
      unite(NAME, NAME_1, NAME_2, sep = " - ")
    
    ggplot(data = confir_2, mapping = aes(x = reorder(NAME, pro.confir), 
                                          y = pro.confir)) +
      geom_bar(stat = "identity", fill = pal_plot_confir, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Casos confirmados cada 100mil personas") 
    
  })
  output$total_test <- renderUI({
    
    num_test <- sum(fecha_covid2()$tests)
    
    num_test <- format(num_test, big.mark = ".")
    
    h4(strong("Testeos a nivel nacional en el mes: ", num_test))
    
  })
  output$test <- renderLeaflet({
    
    bins_conf <- c(0, 10, 50, 100, 500, 1000, 5000, 15000, 33000)
    
    icono_test = "https://www.rizzola.it/images/COVID-19-DIAGNOSI_TEST_SERIOLOGICO.png"
    
    pal_test <- colorBin(palette = "PuBuGn", domain = arg_prov$pro.test, bins = bins_conf)
    
    leaflet() %>% 
      addTiles('http://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG:3857@png/{z}/{x}/{-y}.png', 
           attribution = "Argenmap v2 - Instituto Geográfico Nacional") %>%
      addPolygons(data = fecha_covid2(), weight = 1, color = "black", label = 
                    paste0(as.character(fecha_covid2()$NAME_1),
                           " | Cantidad de testeos: ", as.character(fecha_covid2()$tests)),
                  fillColor = ~pal_test(fecha_covid2()$pro.test), 
                  fillOpacity = 0.8) %>%
      addLegend(position = "bottomright", 
                pal = pal_test,
                values = arg_prov$pro.test,
                title = "Casos cada 100.000 pers.") %>% 
      addMarkers(lat = fecha_covid()$latitude, lng = fecha_covid()$longitude,
                 label = 
                   paste0(as.character(fecha_covid()$NAME_2),
                          " | Cantidad de testeos: ", as.character(fecha_covid()$tests)), 
                 icon = list(
                   iconUrl = icono_test, 
                   iconSize = c(25, 25)
                 ), 
                group = "Departamentos", 
                clusterOptions = markerClusterOptions()) %>%
    addLayersControl(overlayGroups = "Departamentos", 
                    options = layersControlOptions(collapsed = F)) %>% 
  hideGroup("Departamentos")
  })
  output$Gra_test <- renderPlot({
    
    
    pal_plot_test <- c('#014636','#016c59', '#02818a', '#3690c0', '#67a9cf',
                         '#a6bddb', '#d0d1e6', '#ece2f0','#fff7fb')
    
    
    test <- fecha_covid2() %>%
      arrange(desc(pro.test)) %>% head(9)
    
    ggplot(data = test, mapping = aes(x = reorder(NAME_1, pro.test), 
                                        y = pro.test)) +
      geom_bar(stat = "identity", fill = pal_plot_test, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Casos confirmados cada 100mil personas")
    
    
  })
  output$Gra_test_dpto <- renderPlot({
    
    pal_plot_test <- c('#014636','#016c59', '#02818a', '#3690c0', '#67a9cf',
                       '#a6bddb', '#d0d1e6', '#ece2f0','#fff7fb')
    
    
    test_2 <- fecha_covid() %>% arrange(desc(pro.test)) %>% head(9) %>% 
      unite(NAME, NAME_1, NAME_2, sep = " - ")
    
    ggplot(data = test_2, mapping = aes(x = reorder(NAME, pro.test), 
                                          y = pro.test)) +
      geom_bar(stat = "identity", fill = pal_plot_test, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Casos confirmados cada 100mil personas") 
    
  })
  output$total_falle <- renderUI({
    
    num_falle <- sum(fecha_covid2()$deaths)
    
    num_falle <- format(num_falle, big.mark = ".")
    
   h4(strong("Decesos por covid-19 a nivel nacional en el mes: ", num_falle))
    
  })
  output$Fallecimientos <- renderLeaflet({
    
    
    bins_falle <- c(0, 1, 3, 5, 10, 15, 30, 50, 75)
    
    pal_falle <- colorBin(palette = "Reds", domain = arg_prov$pro.muertes, bins = bins_falle)
    ico_falle = "https://cdn-0.emojis.wiki/emoji-pics/mozilla/cross-mark-mozilla.png"
    
    
    leaflet() %>% 
      addTiles('http://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG:3857@png/{z}/{x}/{-y}.png', 
           attribution = "Argenmap v2 - Instituto Geográfico Nacional") %>%
      addPolygons(data = fecha_covid2(), weight = 1, color = "black", label = 
                    paste0(as.character(fecha_covid2()$NAME_1),
                           " | Cantidad de fallecimientos: ", as.character(fecha_covid2()$deaths)),
                  fillColor = ~pal_falle(fecha_covid2()$pro.muertes), 
                  fillOpacity = 0.8, 
                  highlight = highlightOptions(weight = 3, 
                                               color = "black", 
                                               bringToFront = T)) %>% 
      addLegend(position = "bottomright", 
                pal = pal_falle,
                values = arg_prov$pro.muertes,
                title = "Decesos cada 100.000 pers.") %>% 
      addMarkers(lat = fecha_covid()$latitude, lng = fecha_covid()$longitude,
                 label = 
                   paste0(as.character(fecha_covid()$NAME_2),
                          " | Cantidad de fallecimientos: ", as.character(fecha_covid()$deaths)), 
                 icon = list(
                   iconUrl = ico_falle, 
                   iconSize = c(20, 20)
                 ), 
                group = "Departamentos", 
                clusterOptions = markerClusterOptions()) %>% 
    addLayersControl(overlayGroups = "Departamentos", 
                    options = layersControlOptions(collapsed = F)) %>% 
  hideGroup("Departamentos")
  })
  output$Gra_falle <- renderPlot({
    
    
    pal_plot_falle <- c('#67000d','#a50f15', '#cb181d', '#ef3b2c', '#fb6a4a',
                        '#fc9272', '#fcbba1', '#fee0d2','#fff5f0')
    
    falle <- fecha_covid() %>% group_by(NAME_1) %>% mutate(deaths = sum(deaths)) %>%
      mutate(population = sum(population)) %>%
      distinct(date, NAME_1, deaths, population) %>% 
      mutate(pro.falle = (deaths/population)*100000) %>%
      arrange(desc(pro.falle)) %>% head(9)
    
    ggplot(data = falle, mapping = aes(x = reorder(NAME_1, pro.falle), 
                                       y = pro.falle)) +
      geom_bar(stat = "identity", fill = pal_plot_falle, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Decesos cada 100mil personas")
    
    
  })
  output$Gra_falle_dpto <- renderPlot({
    
    pal_plot_falle <- c('#67000d','#a50f15', '#cb181d', '#ef3b2c', '#fb6a4a',
                        '#fc9272', '#fcbba1', '#fee0d2','#fff5f0')
    
    falle_2 <- fecha_covid() %>% arrange(desc(pro.muertes)) %>% head(9) %>% 
      unite(NAME, NAME_1, NAME_2, sep = " - ")
    
    ggplot(data = falle_2, mapping = aes(x = reorder(NAME, pro.muertes), 
                                         y = pro.muertes)) +
      geom_bar(stat = "identity", fill = pal_plot_falle, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Decesos cada 100mil personas") 
    
  })
  output$total_gen_vacu <- renderUI({
    
    num_gen_vacu <- sum(fecha_covid2()$people_vaccinated)
    
    num_gen_vacu <- format(num_gen_vacu, big.mark = ".")
    
    h4(strong("Cantidad de personas vacunadas a nivel nacional en el mes: ", num_gen_vacu))
    
  })
  output$Vacunados <- renderLeaflet({
    
    bins_gen <- c(0, 10, 50, 100, 500, 1000, 5000, 10000, 30000)
    
    pal_gen_vacu <- colorBin(palette = "BuGn", domain = arg_prov$pro.gente.vacu, bins = bins_gen)
    
    ico_vacu = "https://i.pinimg.com/originals/8a/1f/32/8a1f3226401038e0dacec4237151127f.png"
    
    leaflet() %>% 
      addTiles('http://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG:3857@png/{z}/{x}/{-y}.png', 
           attribution = "Argenmap v2 - Instituto Geográfico Nacional") %>% 
      addPolygons(data = fecha_covid2(), weight = 1, color = "black", label = 
                    paste0(as.character(fecha_covid2()$NAME_1),
                           " | Cantidad de vacunados: ", as.character(fecha_covid2()$people_vaccinated)),
                  fillColor = ~pal_gen_vacu(fecha_covid2()$pro.gente.vacu), 
                  fillOpacity = 0.8, 
                  highlight = highlightOptions(weight = 3, 
                                               color = "black", 
                                               bringToFront = T)) %>% 
      addLegend(position = "bottomright", 
                pal = pal_gen_vacu,
                values = arg_prov$pro.gente.vacu,
                title = "Casos cada 100.000 pers.") %>% 
      addMarkers(lat = fecha_covid()$latitude, lng = fecha_covid()$longitude,
                 label = 
                   paste0(as.character(fecha_covid()$NAME_2),
                          " | Cantidad de vacunados: ", as.character(fecha_covid()$people_vaccinated)), 
                 icon = list(
                   iconUrl = ico_vacu, 
                   iconSize = c(20, 20)
                 ), 
                group = "Departamentos", 
                clusterOptions = markerClusterOptions()) %>%
    addLayersControl(overlayGroups = "Departamentos", 
                    options = layersControlOptions(collapsed = F)) %>% 
  hideGroup("Departamentos")
    
  })
  output$Gra_gen_vacu <- renderPlot({
    
    
    pal_plot_gen_vacu <- c('#084081','#0868ac', '#2b8cbe', '#4eb3d3', '#7bccc4',
                           '#a8ddb5', '#ccebc5', '#e0f3db','#f7fcf0')
    
    
    gen_vacu <- fecha_covid() %>% group_by(NAME_1) %>% mutate(people_vaccinated = sum(people_vaccinated)) %>%
      mutate(population = sum(population)) %>%
      distinct(date, NAME_1, people_vaccinated, population) %>% 
      mutate(pro.gen.vacu = (people_vaccinated/population)*100000) %>%
      arrange(desc(pro.gen.vacu)) %>% head(9)
    
    ggplot(data = gen_vacu, mapping = aes(x = reorder(NAME_1, pro.gen.vacu), 
                                          y = pro.gen.vacu)) +
      geom_bar(stat = "identity", fill = pal_plot_gen_vacu, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Vacunas aplicadas cada 100mil personas")
    
    
  })
  output$Gra_gen_vacu_dpto <- renderPlot({
    
    pal_plot_gen_vacu <- c('#084081','#0868ac', '#2b8cbe', '#4eb3d3', '#7bccc4',
                           '#a8ddb5', '#ccebc5', '#e0f3db','#f7fcf0')
    
    
    gen_vacu2 <- fecha_covid() %>% arrange(desc(pro.gente.vacu)) %>% head(9) %>% 
      unite(NAME, NAME_1, NAME_2, sep = " - ")
    
    ggplot(data = gen_vacu2, mapping = aes(x = reorder(NAME, pro.gente.vacu), 
                                           y = pro.gente.vacu)) +
      geom_bar(stat = "identity", fill = pal_plot_gen_vacu, colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Vacunas aplicadas cada 100mil personas")
    
  })
  observeEvent(input$amba, {
    
      leafletProxy("Confirmados") %>% 
      setView(lat = -34.622064, lng = -58.43552, zoom = 9,5)
  })
  observeEvent(input$amba, {
    
    leafletProxy("test") %>% 
      setView(lat = -34.622064, lng = -58.43552, zoom = 9,5)
  })
  observeEvent(input$amba, {
    
    leafletProxy("Fallecimientos") %>% 
      setView(lat = -34.622064, lng = -58.43552, zoom = 9,5)
  })
  observeEvent(input$amba, {
    
    leafletProxy("Vacunados") %>% 
      setView(lat = -34.622064, lng = -58.43552, zoom = 9,5)
  })
  observeEvent(input$nor, {
    
    leafletProxy("Confirmados") %>% 
      setView(lat = -27.79511, lng = -64.26149, zoom = 5)
  })
  observeEvent(input$nor, {
    
    leafletProxy("test") %>% 
      setView(lat = -27.79511, lng = -64.26149, zoom = 5)
  })
  observeEvent(input$nor, {
    
    leafletProxy("Fallecimientos") %>% 
      setView(lat = -27.79511, lng = -64.26149, zoom = 5)
  })
  observeEvent(input$nor, {
    
    leafletProxy("Vacunados") %>% 
      setView(lat = -27.79511, lng = -64.26149, zoom = 5)
  })
  observeEvent(input$centro, {
    
    leafletProxy("Confirmados") %>% 
      setView(lat = -36.61667, lng = -64.28333, zoom = 5)
  })
  observeEvent(input$centro, {
    
    leafletProxy("test") %>% 
      setView(lat = -36.61667, lng = -64.28333, zoom = 5)
  })
  observeEvent(input$centro, {
    
    leafletProxy("Fallecimientos") %>% 
      setView(lat = -36.61667, lng = -64.28333, zoom = 5)
  })
  observeEvent(input$centro, {
    
    leafletProxy("Vacunados") %>% 
      setView(lat = -36.61667, lng = -64.28333, zoom = 5)
  })
  observeEvent(input$sur, {
    
    leafletProxy("Confirmados") %>% 
      setView(lat = -43.3, lng = -65.1, zoom = 5)
  })
  observeEvent(input$sur, {
    
    leafletProxy("test") %>% 
      setView(lat = -43.3, lng = -65.1, zoom = 5)
  })
  observeEvent(input$sur, {
    
    leafletProxy("Fallecimientos") %>% 
      setView(lat = -43.3, lng = -65.1, zoom = 5)
  })
  observeEvent(input$sur, {
    
    leafletProxy("Vacunados") %>% 
      setView(lat = -43.3, lng = -65.1, zoom = 5)
  })

    output$downloadDataConfir <- downloadHandler(
    filename = function(){ 
      paste("arg_date_confir",".csv",sep=",")
    },
    content = function(file) {
      write.csv(arg_date_confir,file)
    })
  output$downloadDataFalle <- downloadHandler(
    filename = function(){ 
      paste("arg_date_falle",".csv",sep=",")
    },
    content = function(file) {
      write.csv(arg_date_falle,file)
    })
  output$downloadDataTest <- downloadHandler(
    filename = function(){ 
      paste("arg_date_test",".csv",sep=",")
    },
    content = function(file) {
      write.csv(arg_date_test,file)
    }) 
  output$downloadDataVacu <- downloadHandler(
    filename = function(){ 
      paste("arg_date_vacu",".csv",sep=",")
    },
    content = function(file) {
      write.csv(arg_date_vacu,file)
    }) 
  
  
  
  
}    
# Run the application 
shinyApp(ui = ui, server = server)

