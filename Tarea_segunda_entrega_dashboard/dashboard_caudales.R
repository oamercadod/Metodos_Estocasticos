# Lista de paquetes necesarios
packages <- c("rgdal", "plyr", "lubridate", "shiny", "ggplot2", "forecast", "readr", 
              "dplyr", "Spectrum", "moments", "EMD", "trend", "leaflet", "maptools", 
              "plotly", "readxl", "DT")

# Verificar e instalar paquetes faltantes
installed_packages <- installed.packages()
for (package in packages) {
  if (!(package %in% installed_packages)) {
    install.packages(package)
  }
}

# Cargar paquetes
lapply(packages, require, character.only = TRUE)


setwd("C:/Users/,metodos_estocasticos")

datos<-read.fwf("nq_all.txt",widths=c(1,8,2,2,2,3,7,2,3,7,2,3,7,2,3,7,2,3,7))
aforos<-read.fwf("af_all.txt",widths=c(8,2,8,2,4,10,10,10,10,8,8,5))
colnames(aforos)<-c("codigo","tipo","fecha","num","Nivel[cm]","Ancho[mt]","Area[m2]","Permoj[m]","Caudal[m3/sg]","Fact_geom","Fact_conv","V12")
Estaciones_caudales <- read_excel("Estaciones_caudales.xlsx")
Estaciones_caudales$CODIGO<-as.character(Estaciones_caudales$CODIGO)
Estaciones_caudales<-data.frame(Estaciones_caudales)
Caudales_Registro_1981_2014 <- read_excel("Caudales_Registro_1981_2014.xlsx")
Caudales_Registro_1981_2014$FECHA <- as.Date(paste(Caudales_Registro_1981_2014$ANO, Caudales_Registro_1981_2014$MES, Caudales_Registro_1981_2014$DIA, sep = "-"))
Caudales_Registro_1981_2014 <- Caudales_Registro_1981_2014 %>% arrange(FECHA)
mapa_base<-readShapePoly("Pomcas/POMCAS_noviembre_2023.shp")


#####
#codit<-levels(factor(datos$V2))
codit<-levels(factor(Estaciones_caudales$CODIGO))

for (g in 1:length(codit))##length(codit)
{
  nq1<-subset(datos,datos$V2==codit[g])
  #nq1<-subset(datos,datos$V2==28017101)
  nq<-nq1[-1,]
  nq<-nq[,c(-5,-8,-11,-14,-17)]
  #nq<-as.numeric(as.character(nq))
  nqa<-data.frame(1:length(nq$V2),2)
  colnames(nqa)<-c("nivel(cm)","caudal")
  nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
  nqa$caudal<-as.numeric(nqa$caudal)
  if (NROW(nq)>0)
  {
   for (r in 5:14)
  {
    #r=5 
    f<-nq[,r]
    if (is.factor(f)) 
    {nq[,r]<-as.numeric(levels(f))[f]}
  }
  
  for (t in 1:length(nq$V2))
  {
    
    nqa[1+5*(t-1),1:2]<-nq[1+(t-1),5:6]
    nqa[2+5*(t-1),1:2]<-nq[1+(t-1),7:8]
    nqa[3+5*(t-1),1:2]<-nq[1+(t-1),9:10]
    nqa[4+5*(t-1),1:2]<-nq[1+(t-1),11:12]
    nqa[5+5*(t-1),1:2]<-nq[1+(t-1),13:14]
  }
  }
}

#metodo<-as.factor(c("","Theis_Libre","Theis_Confinado","Hantush-Jacob",
#                    "Hantush","Solucion_numerica_Libre","Solucion_numerica_Confinado"))
#Variables<-as.factor(c("","Transmisividad(T)","Potencia(b)","Conductividad(K)","Almacenamiento(S)"))

runApp(list(ui = pageWithSidebar(
  headerPanel("UNAL - MÉTODOS ESTOCÁSTICO"),
  sidebarPanel(
    h3("REGISTRO CAUDALES"),
    br(),
    h5("Seleccione el codigo de la estacion que quiere analizar"),
    ##SELECCION DEL CODIGO DE LA ESTACION
    selectInput("codit", "Seleccione un codigo:", 
                choices = levels(as.factor(Estaciones_caudales$CODIGO)))
    
    ),#cierro sidebar
  mainPanel(
    tabsetPanel(
      tabPanel('Mapa Estaciones',
               leafletOutput("mapa",height=400),
               br(),
               #h5("Los triangulos rojos muestran los aforos realizados posteriores a la vigencia de la curva, los circulos aforos previos."),
               br(),
               h4("INFORMACION DE ESTACIONES"),
               dataTableOutput("resumen6")
               
      ),#cierro tabpanel
      tabPanel('Grafica Aforos',
               plotOutput("plot",height=400),
               br(),
               #h5("Los triangulos rojos muestran los aforos realizados posteriores a la vigencia de la curva, los circulos aforos previos."),
               br(),
               h4("TABLA DE RESUMEN DE AFOROS"),
               verbatimTextOutput("resumen4")
               
      ),#cierro tabpanel
      tabPanel('Calculo de Caudal',
               numericInput("tryu",label = h3("Ingrese el nivel"),value = 1,min=min(nqa[,1]),max=max(nqa[,1])),  
               br(),
               h5("Ingrese un valor dentro de los limites de la curva"),
               br(),
               h4("Caudal en función de la curva vigente"),
               verbatimTextOutput("calculo"),
               
               br(),
               br(),
               br(),
               h4("TABLA DE RELACION NIVEL CAUDAL VIGENTE"),
               verbatimTextOutput("resumen3")
      ),#cierro tabpanel
      tabPanel('Serie de caudales',
               plotlyOutput("plot2",height=400),
               br(),
               #h5("Los triangulos rojos muestran los aforos realizados posteriores a la vigencia de la curva, los circulos aforos previos."),
               br(),
               plotOutput("histograma",height=300),
               plotOutput("box",height=300),
               plotOutput("ACF",height=250),
               plotOutput("PACF",height=250),
               plotOutput("PSD",height=250)
      ),#cierro tabpanel
      tabPanel('Pruebas y Estadisticos',
               br(),
               h4("TABLA DE RESUMEN DE ESTADISTICOS"),
               verbatimTextOutput("resumen5"),
               h5("PRUEBAS ESTADISTICAS"),
               h5("Test de Von Newman:"),
               verbatimTextOutput("VN"),
               h5("Test de Mann -Kendall:"),
               verbatimTextOutput("MK"),
               h5("Test de Wilcox:"),
               verbatimTextOutput("wilcox"),
               h5("Test de Kruskal:"),
               verbatimTextOutput("kruskal"),
               h5("Test t student:"),
               verbatimTextOutput("student"),
               h5("Valores Atipicos:"),
               verbatimTextOutput("atipicos"),
               
      )#cierro tabpanel
    )#cierro tabsetpanel
  )#cierro mainpanel
),

server = function(input, output) {
  
  ##CURVA NIVEL CAUDAL
  output$plot <- renderPlot({
    #a<-'2000-06-20'
    nq1<-subset(datos,datos$V2==input$codit)
    #nq1<-subset(datos,datos$V2==28017101)
    nq<-nq1[-1,]
    nq<-nq[,c(-5,-8,-11,-14,-17)]
    #nq<-as.numeric(as.character(nq))
    nqa<-data.frame(1:length(nq$V2),2)
    colnames(nqa)<-c("nivel(cm)","caudal")
    nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
    nqa$caudal<-as.numeric(nqa$caudal)
    for (r in 5:14)
    {
      #r=5 
      f<-nq[,r]
      if (is.factor(f)) 
      {nq[,r]<-as.numeric(levels(f))[f]}
    }
    
    for (t in 1:length(nq$V2))
    {
      
      nqa[1+5*(t-1),1:2]<-nq[1+(t-1),5:6]
      nqa[2+5*(t-1),1:2]<-nq[1+(t-1),7:8]
      nqa[3+5*(t-1),1:2]<-nq[1+(t-1),9:10]
      nqa[4+5*(t-1),1:2]<-nq[1+(t-1),11:12]
      nqa[5+5*(t-1),1:2]<-nq[1+(t-1),13:14]
    }
    nqa <- nqa[complete.cases(nqa),]
    for (j in 2:length(nqa[,1]))
      
    {
      if (nqa[j-1,1]>nqa[j,1])
        nqa[j,1]=1000+nqa[j,1]
      nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
      nqa$caudal<-as.numeric(nqa$caudal)
    }
    
    af<-subset(aforos,aforos$codigo==input$codit)
    
    for (r in 5:11)
    {
      #r=5 
      f<-af[,r]
      if (is.factor(f)) 
      {af[,r]<-as.numeric(levels(f))[f]}
    }
    
    an<-as.numeric((nq1[1,5]))
    if (nq1[1,6]<100) an<-an*10
    act<-paste(an,nq1[1,6],nq1[1,7],sep="")
    act<-ymd(act)
    
    af$fecha<-ymd(af$fecha)
    c_vig<-af[which(af$fecha>act),]
    c_pas<-af[which(af$fecha<act),]
    
    
    plot(nqa[,2],nqa[,1],type="o",col="blue",ylab="Nivel[cm]",xlab="Caudal",main=paste("Curva N-Q ",input$codit,"  Fecha: ",act,sep=""))
    points(c_vig[,9],c_vig[,5],col="red",pch=24)
    points(c_pas[,9],c_pas[,5],col="brown")
   
    })
  ##plot de series de tiempo
  output$plot2 <- renderPlotly({
    if (input$codit %in% colnames(Caudales_Registro_1981_2014)) {
      # Ordenar los datos por fecha
      caudales_ordenados <- Caudales_Registro_1981_2014 %>% arrange(FECHA)
      
      # Crear el gráfico interactivo con plot_ly
      p <- plot_ly(data = caudales_ordenados, x = ~FECHA, y = ~get(input$codit), type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
        layout(xaxis = list(title = "Fecha"),
               yaxis = list(title = "Caudal"),
               hovermode = 'closest')
      
      # Mostrar el gráfico interactivo
      p
    }
  })
  
  ##plot de histograma
  output$histograma <- renderPlot({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    hist(valores, ylab = "Frecuencia",xlab = "caudal", main = "Histograma del caudal diario")
  
  })
  ##plot de caja de bigotes
  output$box <- renderPlot({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    boxplot(valores, main = "Diagrama de caja del caudal diario")
    
    
  })
  ##plot de caja de bigotes
  output$atipicos <- renderPrint({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    boxplot_A<- boxplot(valores, main = "Diagrama de caja del caudal diario")
    valores_atipicos <- boxplot_A$out
    valores_atipicos
    
    })
  
   ##plot de acf
  output$ACF <- renderPlot({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    
    acf_caudal <- forecast::Acf(valores, na.rm = TRUE, lag.max = 20, plot = F)
    plot(acf_caudal, main = "Correlograma")

      })
  ##plot de Pacf
  output$PACF <- renderPlot({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    
    pacf_caudal <- forecast::Pacf(valores, na.rm = TRUE, lag.max = 20, plot = F)
    plot(pacf_caudal, main = "Correlograma Parcial")
    
    })
  
  ##plot de psd
  output$PSD <- renderPlot({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    
    # Calcular la función de densidad espectral
    psd_caudal <- spec.pgram(na.omit(valores), na.rm = TRUE, spans = c(4,4), taper = 0, plot = F)
    plot(psd_caudal, main = "Densidad Espectral")
  })
  
  ##plot de mapa con estaciones
  output$mapa <- renderLeaflet({
    mapa <- leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = mapa_base, fillOpacity = 0.2, color = "black", weight = 1)
    
    # Establecer el centro y el zoom manualmente
    mapa <- mapa %>%
      setView(lng = -74.0, lat = 4.0, zoom = 5) # Establece el centro y el zoom según tus datos
    
    # Agregar las estaciones al mapa
    mapa <- mapa %>%
      addCircleMarkers(data = Estaciones_caudales, lng = ~LONGITUD, lat = ~LATITUD, radius = 5, color = "red",
                       popup = ~paste("CODIGO:", CODIGO, "<br>",
                                      "NOMBRE:", NOMBRE, "<br>",
                                      "CATEGORIA:", CAT, "<br>",
                                      "LATITUD:", LATITUD, "<br>",
                                      "LONGITUD:", LONGITUD))
    mapa
    })
  
  ##Calculo caudal desde nivel
  output$calculo <- renderPrint({
    
    nq1<-subset(datos,datos$V2==input$codit)
    #nq1<-subset(datos,datos$V2==11017010)
    nq<-nq1[-1,]
    nq<-nq[,c(-5,-8,-11,-14,-17)]
    #nq<-as.numeric(as.character(nq))
    nqa<-data.frame(1:(length(nq$V2)*5),2)
    colnames(nqa)<-c("nivel(cm)","caudal")
    nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
    nqa$caudal<-as.numeric(nqa$caudal)
    for (r in 5:14)
    {
      #r=5 
      f<-nq[,r]
      if (is.factor(f)) 
      {nq[,r]<-as.numeric(levels(f))[f]}
    }
    
    for (t in 1:length(nq$V2))
    {
      
      nqa[1+5*(t-1),1:2]<-nq[1+(t-1),5:6]
      nqa[2+5*(t-1),1:2]<-nq[1+(t-1),7:8]
      nqa[3+5*(t-1),1:2]<-nq[1+(t-1),9:10]
      nqa[4+5*(t-1),1:2]<-nq[1+(t-1),11:12]
      nqa[5+5*(t-1),1:2]<-nq[1+(t-1),13:14]
    }
    
    nqa <- nqa[complete.cases(nqa),]
    for (j in 2:length(nqa[,1]))
      
    {
      if (nqa[j-1,1]>nqa[j,1])
        nqa[j,1]=1000+nqa[j,1]
      
      nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
      nqa$caudal<-as.numeric(nqa$caudal)  
    }
    
    tryu=input$tryu
    #tryu=140.2
    
    if (tryu>=min(nqa[,1])&tryu<=max(nqa[,1])){up<-which(abs(nqa[,1]-tryu)==min(abs(nqa[,1]-tryu),na.rm=TRUE))
    if (up==1){up=2}
    dw<-up-1
    lev<-nqa[up,1]-nqa[dw,1]
    qa<-nqa[up,2]-nqa[dw,2]
    nwq<-((tryu-nqa[dw,1])*qa/lev)+nqa[dw,2]
    nwq[1]}
    
  })
  
  output$resumen3 <- renderPrint({
    nq1<-subset(datos,datos$V2==input$codit)
    #nq1<-subset(datos,datos$V2==25027330)
    nq<-nq1[-1,]
    nq<-nq[,c(-5,-8,-11,-14,-17)]
    #nq<-as.numeric(as.character(nq))
    nqa<-data.frame(1:(length(nq$V2)*5),2)
    colnames(nqa)<-c("nivel(cm)","caudal")
    nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
    nqa$caudal<-as.numeric(nqa$caudal)
    for (r in 5:14)
    {
      #r=5 
      f<-nq[,r]
      if (is.factor(f)) 
      {nq[,r]<-as.numeric(levels(f))[f]}
    }
    
    for (t in 1:length(nq$V2))
    {
      nqa[1+5*(t-1),1:2]<-nq[1+(t-1),5:6]
      nqa[2+5*(t-1),1:2]<-nq[1+(t-1),7:8]
      nqa[3+5*(t-1),1:2]<-nq[1+(t-1),9:10]
      nqa[4+5*(t-1),1:2]<-nq[1+(t-1),11:12]
      nqa[5+5*(t-1),1:2]<-nq[1+(t-1),13:14]
    }
    nqa <- nqa[complete.cases(nqa),]
    nqa$`nivel(cm)`<-as.numeric(nqa$`nivel(cm)`)
    nqa$caudal<-as.numeric(nqa$caudal)
    
    for (j in 2:length(nqa[,1]))
    {
      if (nqa[j-1,1]>nqa[j,1])
        nqa[j,1]=1000+nqa[j,1]
    }
    
    nqa
  })
  
  output$resumen4 <- renderPrint({

    af<-subset(aforos,aforos$codigo==input$codit)
    
    for (r in 5:11)
    {
      #r=5 
      f<-af[,r]
      if (is.factor(f)) 
      {af[,r]<-as.numeric(levels(f))[f]}
    }
    an<-as.numeric(as.character(nq1[1,5]))
    if (nq1[1,6]<100) an<-an*10
    act<-paste(an,nq1[1,6],nq1[1,7],sep="")
    act<-ymd(act)
    
    af$fecha<-ymd(af$fecha)
    c_vig<-af[which(af$fecha>act),]
    c_pas<-af[which(af$fecha<act),]
    
    af[,c(3,5,6,7,8,9,10)]
    
  })
  
  output$resumen6 <- renderDataTable({
    Estaciones_caudales<-data.frame(Estaciones_caudales)
    datatable(Estaciones_caudales)
  })
  
  output$VN <- renderPrint({
  ##
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    Caudal <- Caudales_Registro_1981_2014[[input$codit]]
    #### Realizamos la prueba Von Neuman para medir la aleoteriedad e independencia 
    
    Caudal <- Caudal[!is.na(Caudal)]
    d <- diff(Caudal)
    n <- length(Caudal)
    mx <- mean(Caudal)
    
    # para los datos el valor de Von Neuman se calcula:
    
    VN <- sum(d^2)/sum((Caudal - mx)^2)
    VN

    ##
  })
  output$MK <- renderPrint({
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    Caudal <- Caudales_Registro_1981_2014[[input$codit]]
  #Pruebe si se puede detectar una tendencia.
  
  ### Realizamos la prueba de Mann-kendall para probar si hay tendencias 
  Mann <- mk.test(na.omit(Caudal))
  Mann

  })
  
  ###
  output$wilcox <- renderPrint({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    Caudal <- Caudales_Registro_1981_2014[[input$codit]]
    
    # Prueba de Suma de Rangos (Wilcoxon)
    wilcox_test <- wilcox.test(Caudal, mu = median(na.omit(Caudal)))
    
    wilcox_test
    
      })
  
  output$kruskal <- renderPrint({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    Caudal <- Caudales_Registro_1981_2014[[input$codit]]
      
    # Prueba de Kruskal-Wallis
    kruskal_test <- kruskal.test(Caudal ~ seq_along(Caudal))
    
    kruskal_test
    
      })
  
  output$student <- renderPrint({
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    Caudal <- Caudales_Registro_1981_2014[[input$codit]]
    
    # Prueba t de Student
    t_student <- t.test(Caudal)
    
    t_student
  })
  
  output$resumen5 <- renderPrint({
    
    ##### Estimación de los estadísticos o momentos
    # Crear un data frame para la serie
    serie <- data.frame(
      datos = Caudales_Registro_1981_2014[[input$codit]],
      fecha = Caudales_Registro_1981_2014$FECHA
    )
    # Remover filas con NA en los datos
    serie <- na.omit(serie)
    
    # Contar los datos disponibles
    datos_disponibles <- nrow(serie)
    
    # Definir las fechas de inicio y fin del periodo
    fecha_inicio <-as.Date(min(serie$fecha))
    fecha_fin <-as.Date(max(serie$fecha))
    
    # Calcular el número de días en el periodo
    Total_dias <- as.numeric(fecha_fin - fecha_inicio)
    
    # Comparar con datos disponibles
    dias_faltantes <- Total_dias - datos_disponibles
    
    # Obtener el nombre de la columna
    nombre_columna <- paste0("Caudales_Registro_1981_2014$", input$codit)
    
    # Obtener los valores de la columna
    valores <- Caudales_Registro_1981_2014[[input$codit]]
    
    resultados <- data.frame(
      Estadístico = c("Media","Maximo","Minimo", "Varianza", "Desviación estándar", "Coeficiente de variación",
                      "Coeficiente de asimetría", "Coeficiente de curtosis", "Covarianza",
                      "# dias disponibles","# dias faltantes"),
      Valor = c(
        mean(valores, na.rm = TRUE),
        max(valores, na.rm = TRUE),
        min(valores, na.rm = TRUE),
        var(valores, na.rm = TRUE),
        sd(valores, na.rm = TRUE),
        sd(valores, na.rm = TRUE) / mean(valores, na.rm = TRUE),
        moments::skewness(valores, na.rm = TRUE),
        moments::kurtosis(valores, na.rm = TRUE),
        cov(valores, valores, use = "complete.obs"),
        datos_disponibles,
        dias_faltantes
      )
    )
    options(scipen = 999)
    # Mostrar la tabla de resultados
    print(resultados)
    
    # Calcular la función de densidad espectral
    #psd_caudal <- spec.pgram(datos_caudal$caudal, spans = c(4,4), taper = 0, plot = FALSE)
    
    
  })
  
}
)
)

shinyApp(ui = ui, server = server)
