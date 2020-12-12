#librerias
library(YieldCurve)
library(ustyc)
library(Quandl)
library(shiny)
library(futile.paradigm)
library(jrvFinance)
library(ggplot2)
library(lpSolve)
Quandl.api_key('jmXuysufE_QdsR3b_kTj')






data <- Quandl('USTREASURY/YIELD', type = 'xts',
                from = Sys.Date())


#Sensibilidades proximas emisiones
for(i in 1:ncol(z)){
   
   precios[i] <- bondprc(100, 0.0425, madurez[i], z[,i]-0.001)
   precios2[i] <- bondprc(100, 0.0425, madurez[i], z[,i]+0.001)
DV01[i] <- (precios[i] - precios2[i])/(10000*(z[,i]+0.001-(z[,i]-0.001)))
}
View(abs(DV01[,1]))


#Creacion de los TES

vencimientos <- c("2022-05-04", "2024-07-24","2025-11-26", "2026-08-26", "2027-11-03","2028-04-28","2030-09-18","2032-06-30", "2034-10-18","2050-10-11")
cupones <- c(0.07, 0.1,0.0625,0.075,0.0575,0.06,0.0775,0.07,0.0725,0.0725)
tasas <- c(2.487,3.586,4.069,4.354,4.814,4.921,5.444,5.869,6.118,6.782)
durations<- jrvFinance::bond.durations(Sys.Date(),mature = vencimientos,coupon = cupones,modified = TRUE,redemption_value = 100,freq = 2, yield = tasas/100,convention = "ACT/360")
preciosSucios <- jrvFinance::bond.prices(Sys.Date(),mature = vencimientos,coupon = cupones,freq = 2,convention = "ACT/360",redemption_value = 100,yield = tasas/100)

DV01TES <- durations*preciosSucios*100

#Portafolio TES

#modelo de optimización 
modeloTES <- function(yield, capital){
   
   
   Restriccion2 <- tasas
   Restriccion1 <- t(c(rep(1,length(tasas))))
   sens <- t(DV01TES)
   #matriz de restricciones.
   
   
   A <- rbind(Restriccion1,Restriccion2)
   A
   #igualdades
   
   b <- c(1,yield)
   dir <- rep("=",2)
   
   #Funcion objetivo
   FO <- c(sens)
   solucion <- lp('min', FO,A,dir,b)
   
   xoptimo <- solucion$solution
   
   xoptimo
   cantidad <- round(c(capital*xoptimo),3)
   sensibilidad <-round(cantidad*DV01TES/100,3) 

      tabla1 <- data.frame("Vencimiento" = vencimientos, "Posicion(MM COP)" = cantidad, "Sensibilidad(COP)" = sensibilidad, "Duración(Años)" = round(durations*xoptimo,3))

      totales <- c("Total",round(sum(cantidad),3),round(sum(sensibilidad),3),round(sum(durations*xoptimo),3))
      tablafinal <<- rbind(tabla1,totales)
      return(tablafinal)   
}

modeloTES(4.3,100)
#funcion para agregar un bono 
portaTES <- data.frame("Vencimiento" = vencimientos, "Cupón(%)" = cupones*100, "YTM(%)" = tasas, "DV01(COP)" =round(DV01TES,2), "Duración(Años)" =round(durations,2)) 
portaTES
agregarbono <-function(vencimiento,cupon,tasa){
   duracionNuevo<- jrvFinance::bond.duration(Sys.Date(),mature = vencimiento,coupon = cupon/100,modified = TRUE,redemption_value = 100,freq = 2, yield = tasa/100,convention = "ACT/360")
   preciosSucioNuevo <- jrvFinance::bond.price(Sys.Date(),mature = vencimiento,coupon = cupon/100,freq = 2,convention = "ACT/360",redemption_value = 100,yield = tasa/100)
   
   DV01Nuevo <- round(duracionNuevo*preciosSucioNuevo*100,2)
   nuevo <- c(vencimiento,cupon,tasa,DV01Nuevo, round(duracionNuevo,2))
    portaTES <<- rbind(portaTES,nuevo)
    tasas <<- c(tasas,tasa)
    vencimientos <<- c(vencimientos,vencimiento)
    cupones <<- c(cupones,cupon)
    DV01TES <<-round(c(DV01TES, DV01Nuevo),2)
    durations <<- c(durations,duracionNuevo)
   return(portaTES)
}
quitarbono <- function(vencimiento){
   
   
   portaTES<<- portaTES[!as.character(portaTES$Vencimiento) %in% vencimiento,]
   tasas <<- as.numeric(portaTES$YTM)
   DV01TES  <<- as.numeric(portaTES$DV01)
   cupones <<- as.numeric(portaTES$Cupón)
   durations <<- as.numeric(portaTES$Duración)
   vencimientos <<- as.character(portaTES$Vencimiento)
   return(portaTES)
}
modificarprecio <- function(vencimiento, YTM){
   tasas <<- replace(tasas,tasas == portaTES$YTM[portaTES$Vencimiento==vencimiento],YTM)
   cupon <- as.numeric(portaTES$Cupón[portaTES$Vencimiento==vencimiento])
   tasa <- portaTES$YTM[portaTES$Vencimiento==vencimiento]
   duracionNuevo<- jrvFinance::bond.duration(Sys.Date(),mature = vencimiento,coupon = cupon/100,modified = TRUE,redemption_value = 100,freq = 2, yield = YTM/100,convention = "ACT/360")
   preciosSucioNuevo <- jrvFinance::bond.price(Sys.Date(),mature = vencimiento,coupon = cupon/100,freq = 2,convention = "ACT/360",redemption_value = 100,yield = YTM/100)
   
   DV01Nuevo <- round(duracionNuevo*preciosSucioNuevo*100,2)
   
   durations <<- replace(durations,durations == portaTES$Duración[portaTES$Vencimiento==vencimiento],duracionNuevo)
   DV01TES <<- replace(DV01TES,DV01TES == portaTES$DV01[portaTES$Vencimiento==vencimiento],DV01Nuevo)
   portaTES$YTM[portaTES$Vencimiento==vencimiento] <<- YTM
   portaTES$Duración[portaTES$Vencimiento==vencimiento] <<- round(duracionNuevo,2)
   portaTES$DV01[portaTES$Vencimiento==vencimiento] <<- round(DV01Nuevo,2)
   return(portaTES)
   
}
portaTES
seguimientoMarktoMarket <- function(archivo){
   
   datos <- readxl::read_excel(archivo)
   
    cambio <- (as.numeric(portaTES$YTM) - as.numeric(datos$YTM))
    
    PnL <- cambio*as.numeric(tablafinal$Posicion[tablafinal$Vencimiento!="Total"])*as.numeric(portaTES$DV01)
    PnL[length(PnL)+1] <- sum(PnL)
    PnL <- PnL[PnL!=0]
    tablapnl <- cbind(tablafinal[!as.character(tablafinal$Posicion) %in% "0",],PnL)
    
    return(tablapnl)
   
   
   
}
#APLICACION


library(shiny)
library(xtable)

ui <- navbarPage(title = "Optimización de Portafolios",tabPanel(title="Mercado",

                                                                sidebarLayout(
   
   sidebarPanel(wellPanel( titlePanel(title = "Agregar bono"),
      dateInput(inputId = "vencimientoNuevo",label = "Vencimiento (AAAA-MM-DD):",value = Sys.Date()),
               
               numericInput(inputId = "cupon", label = "cupón (%):", value = 7.25, min = 0, max = 100),
               numericInput(inputId = "YTM", label = "YTM (%):", value =2.56, min = 0, max =100),
               actionButton(inputId = "AgregarBono", label = "Agregar")),
   wellPanel(titlePanel(title = "Eliminar bono"),
             
             dateInput(inputId = "vencimientoEliminar", label = "Vencimiento (AAAA-MM-DD)", value = Sys.Date()),
             actionButton(inputId = "Eliminar", label = "Eliminar")
      
   ),wellPanel(titlePanel(title = "Modificar YTM"),
               
               textInput(inputId = "vencimientoModificar", label = "vencimiento (AAAA-MM-DD)", value = Sys.Date()),
               numericInput(inputId = "YTM2",label = "YTM (%)", value = 2.34,min = 0,max = 100),
               actionButton(inputId = "Modificar", label = "Modificar"))
   
,width = 3),

mainPanel(
   
          #grafica de bonos
          dataTableOutput(outputId = "TES")
),)),

tabPanel(title = "Invertir",
         
         sidebarLayout(
            sidebarPanel(
               titlePanel(title = "Invertir"),
               
               #cantidad a invetir
               numericInput(inputId = "cantidad", label = "Cantidad a invetir (MM COP)", value = 1, min = 1, max = 100000000000),
               #yield deseada
               numericInput(inputId = "yield", label= "Yield (%)",value = mean(TIRs), min=0, max = max(tasas),
                            #boton de invertir
               ), actionButton(inputId = "invertir", label = "Invertir")                                                        
               
            ),
            
            mainPanel(dataTableOutput(outputId = "optimo"))
         )),
tabPanel(title = "Seguimiento",
         
         sidebarLayout(
            sidebarPanel(titlePanel(title = "Subir precios"),
                         fileInput(inputId = "Archivo", label = "seleccionar archivo (.xlsx)"),
                         actionButton(inputId = "Actualizar", label = "Actualizar PnL")),
            
            mainPanel(
               
               dataTableOutput("pnl")
            )
         ))
                       
         )
    
         






   
   

server <- function(input, output, session) {
   
 model <- reactive(modeloTES(input$yield,input$cantidad)) 
 agregar <- reactive(agregarbono(input$vencimientoNuevo,input$cupon,input$YTM)) 

 fechas <- reactive(
    validate(input$Archivo$type!="/xlsx", "Tiene que ser un archivo de excel")
    
 )
   observeEvent(input$invertir, output$optimo <- renderDataTable({
     iso <- isolate({model()})
    
 }) )
                 
   output$TES <- renderDataTable(portaTES)
   
   observeEvent(input$AgregarBono, output$TES <- renderDataTable({
      
      dm<- isolate({agregarbono(as.character(input$vencimientoNuevo),input$cupon,input$YTM)})
      
      
    })
    )
 observeEvent(input$Actualizar, output$pnl <- renderDataTable({
    
    dm5 <- isolate({seguimientoMarktoMarket(as.character(input$Archivo$datapath))})
 })) 
   observeEvent(input$Modificar, output$TES <- renderDataTable({
      
      dm3 <- isolate({modificarprecio(as.character(input$vencimientoModificar),as.numeric(input$YTM2))})
   }))
   
observeEvent(input$Eliminar, output$TES <- renderDataTable({
   
   dm2 <- isolate({quitarbono(as.character(input$vencimientoEliminar))})
})

)

        }

   
      

   


shinyApp(ui =ui, server=server)

