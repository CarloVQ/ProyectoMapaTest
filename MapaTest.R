setwd("~/Ensayo/EnsayoMapa/mapas")

#############################################################################3
req_inst_libr_faltantes <- function(librerias) {
  paqu_faltantes <- librerias[!(librerias %in% installed.packages()[, "Package"])]
  if (length(paqu_faltantes)) {
    install.packages(paqu_faltantes, dependencies = TRUE)
  }
  sapply(librerias, require, character.only = TRUE)
}

# Lista de paquetes necesarios para visualizar y transformar datos
paquetes <-
  c("rgdal", "leaflet", "leaflet.extras", "widgetframe", "RColorBrewer",
    "readxl", "htmltools", "sf", "rgdal", "maptools", "rgeos", "shiny")

# Aplicacion de la funcion creada antes
req_inst_libr_faltantes(paquetes)

######################################

MapaNivel1 <-
  rgdal::readOGR(dsn = getwd(), layer = "BAS_LIM_DEPARTAMENTO")

# Segundo nivel: Por Provincia:


MapaNivel2 <-
  rgdal::readOGR(dsn = getwd(), layer = "PROVINCIAS")

# Tercer nivel: Por Distritos:

MapaNivel3 <-
  rgdal::readOGR(dsn = getwd(), layer = "BAS_LIM_DISTRITOS")

# Datos a plotear

datos_mapa <- readxl::read_excel("mapas.xlsx")

###########################################################################

a1 <-
  MapaNivel1 %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  sf::st_as_sf()

nombresdep <- unique(a1$NOMBDEP)

a2 <-
  MapaNivel2 %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  sf::st_as_sf() %>%
  dplyr::rename(NOMBDEP = DEPARTAMEN) %>% 
  dplyr::left_join(datos_mapa, by = 'PROVINCIA')

nombresprov <- unique(a2$PROVINCIA)

a3 <-
  MapaNivel3 %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  sf::st_as_sf()

########################################################################################

## los modificare despues, es solo una prueba

Divisiones1 <- c(14000, 3000000, 5096126, 38000000, Inf)

Colores1 <- colorBin("YlOrRd", domain = a1$HECTARES, bins = Divisiones1)

###

Divisiones2 <- c(14000, 3000000, 5096126, 38000000, Inf)

Colores2 <- colorBin("YlOrRd", domain = a2$HECTARES, bins = Divisiones2)

###

Divisiones3 <- c(14000, 3000000, 5096126, 38000000, Inf)

Colores3 <- colorBin("YlOrRd", domain = a3$HECTARES, bins = Divisiones3)

### Ui y Server ----

# Queda pendiente por aniadir boton de descarga del grafico como html

ui <- 
  bootstrapPage(
    tags$style(type = "text/css", "html, body { width:100%; height:100% }"),
    leafletOutput("Mapa", width = "100%", height = "100%"), 
    absolutePanel(top = 10, 
                  right = 10, 
                  draggable = T, 
                  selectInput(
                    inputId = "Departamento", 
                    label = "Departamento", 
                    choices = c( "Todos", sort( nombresdep ) ), 
                    selected = "Todos"
                  ),
                  conditionalPanel(condition = "input.Departamento != 'Todos'", 
                                   selectInput(inputId = "Provincia", 
                                               label = "Provincia", 
                                               choices = c( "Todos", sort( nombresprov ) ), 
                                               selected = "Todos")
                  )
    )
  )

# hr(),
# downloadButton(
#   outputId = "download_epicurve",
#   label = "Download plot"
# )

server <- function(input, output, session) {
  
  label1 <- # Funcion HTML que servira para las etiquetas que se mostraran
    paste(
      "Cuenta: ", unique(a1$COUNT), "<br/>", 
      "Area: ", unique(a1$HECTARES), "<br/>", 
      nombresdep, "<br/>", 
      sep = ""
    ) %>% 
    lapply(htmltools::HTML)
  
  MapaDepartamentos <- a1 %>% sf::as_Spatial()
  
  observeEvent(input$Departamento,{
    updateSelectInput(session, "Provincia", 
                      choices = c("Todos", unique(a2$PROVINCIA[a2$NOMBDEP == input$Departamento])))
  })
  
  Data2 <- reactive({
    data2 <- a2 %>% dplyr::filter(NOMBDEP %in% input$Departamento) %>% sf::as_Spatial()
    data2
  })
  
  label2 <-  reactive({
    # Funcion HTML que servira para las etiquetas que se mostraran
    paste(unique(Data2()$PROVINCIA), "<br/>", 
          # "Area: ", unique(Data3()$AREA_MINAM), "<br/>", 
          sep = "") %>% 
      lapply(htmltools::HTML)
    
  })
  
  Data3 <- reactive({
    data3 <- a3 %>% dplyr::filter(NOMBPROV %in% input$Provincia) %>% sf::as_Spatial()
    data3
  })
  
  label3 <-  reactive({
    # Funcion HTML que servira para las etiquetas que se mostraran
    paste(unique(Data3()$NOMBDIST), "<br/>", 
          "Area: ", unique(Data3()$AREA_MINAM), "<br/>", 
          sep = "") %>% 
      lapply(htmltools::HTML)
    
  })
  
  output$Mapa <- renderLeaflet({
    
    if (input$Departamento == "Todos" && input$Provincia == "Todos") {
      
      m <- leaflet()
      m %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% # Tipo de mapa a usar, hay muchos mas.
        addPolygons(
          data = MapaDepartamentos, # Data para ser ploteada
          fillColor = ~ Colores1(HECTARES), # variable sujeta a cambio
          stroke = T, # Para mostrar los bordes territoriales
          label = ~label1, # Funcion HTML para la etiqueta
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", # Tipo de letra de la etiqueta
                         padding = "2px 2px", # Ajuste de la letra con el cuadro
                         color = "blue"), # Color de la letra
            textsize = "12px", # Tamanio del texto del cuadro de etiqueta
            direction = "top"), # Direccion del cuadro de texto
          color = "black", # Color de los bordes territoriales
          fillOpacity = 1, # Transparencia del color del poligono
          weight = 4, # Grosor del borde territorial
          highlight = highlightOptions(
            weight = 5, # Grosor del poligono cuando pasas el mouse encima
            color = "red", # Color del poligono cuando pasas el mouse encima
            fillOpacity = 0.4, # Transparencia del color del poligono cuando pasas el mouse encima
            bringToFront = TRUE)) %>% 
        leaflet::addLegend(data = MapaDepartamentos, # Data del mapa
                           pal = Colores1, # Nombre de la funcion de la paleta de colores
                           values = ~HECTARES, # Variable sobre la cual se pinto
                           opacity = 0.7, # Transparencia de la leyenda
                           title = "Ha", # Titulo de la leyenda
                           position = "bottomright" # Posicion de la leyenda
        )
      
    } else if (input$Departamento != "Todos" && input$Provincia == "Todos") {
      
      m <- leaflet()
      m %>% addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          data = Data2(), 
          fillColor = "#2BF028", # Pendiente por eliminar
          # fillColor = ~Colores2(HECTARES), # Pendiente por activar
          stroke = T, 
          label = label2(), 
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", 
                         padding = "2px 2px", 
                         color = "blue"), 
            textsize = "12px", 
            direction = "top"), 
          color = "black", # Color de los bordes territoriales
          fillOpacity = 1, # Transparencia del color del poligono
          weight = 4, # Grosor del borde territorial
          highlight = highlightOptions(
            weight = 5, # Grosor del poligono cuando pasas el mouse encima
            color = "red", # Color del poligono cuando pasas el mouse encima
            fillOpacity = 0.4, # Transparencia del color del poligono cuando pasas el mouse encima
            bringToFront = TRUE))
      
    } else {
      
      m <- leaflet()
      m %>% addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          data = Data3(), 
          fillColor = "#97C8D5", # Pendiente por eliminar
          # fillColor = ~Colores3(HECTARES), # Pendiente por activar
          stroke = T,
          label = label3(), 
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", 
                         padding = "2px 2px", 
                         color = "blue"), 
            textsize = "12px", 
            direction = "top"), 
          color = "black", # Color de los bordes territoriales
          fillOpacity = 1, # Transparencia del color del poligono
          weight = 4, # Grosor del borde territorial
          highlight = highlightOptions(
            weight = 5, # Grosos del poligono cuando pasas el mouse encima
            color = "red", # Color del poligono cuando pasas el mouse encima
            fillOpacity = 0.4, # Transparencia del color del poligono cuando pasas el mouse encima
            bringToFront = TRUE))
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)