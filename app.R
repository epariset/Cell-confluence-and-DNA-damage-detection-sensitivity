# Load packages ----
library(shiny)
library(shinyjs)

#create dataframe with T values 
p1 <- c(6.31, 2.92, 2.35, 2.13, 2.01, 1.94, 1.89, 1.86, 1.83, 1.81, 1.80, 1.78, 1.77, 1.76, 1.75, 1.75, 1.74, 1.73, 1.73, 1.71, 1.70, 1.69, 1.69, 1.68, 1.66, 1.65, 1.65, 1.65, 1.65)
p2 <- c(31.82, 6.96, 4.56, 3.75, 3.36, 3.14, 3, 2.9, 2.82, 2.76, 2.72, 2.68, 2.65, 2.62, 2.6, 2.58, 2.57, 2.55, 2.54, 2.49, 2.46, 2.44, 2.43, 2.4, 2.36, 2.34, 2.34, 2.34, 2.33)
p3 <- c(318.31, 22.33, 10.92, 7.21, 5.9, 5.21, 4.79, 4.5, 4.3, 4.14, 4.02, 3.93, 3.85, 3.79, 3.73, 3.69, 3.65, 3.61, 3.58, 3.47, 3.4, 3.35, 3.31, 3.26, 3.17, 3.13, 3.12, 3.11, 3.11)
N_vect <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20-24", "25-29", "30-34", "35-39", "40-49", "50-99", "100-199", "200-299", "300-399", "400-499" , "500+")
T_table <- data.frame(p1, p2, p3)
colnames(T_table) <- c("0.05", "0.01", "0.001")
rownames(T_table) <- N_vect

#create dataframe with mouse strain and t1/2
strain <- c("Average across strains", "B6C3", "BALBC", "C3H", "C57", "CBA", "CC002", "CC011", "CC013", "CC019", "CC032", "CC037", "CC040", "CC042", "CC051","CC061")
t_strain <- c(4.5, 5.3, 6.6, 4.7, 5.0, 4.8, 3.1, 3.9, 3.3, 4.6, 4.3, 4.6, 4.5, 4.3, 4.5, 3.8)
strain_table <- data.frame(strain, t_strain)
colnames(strain_table) <- c("strain", "t_half")

# User interface ----
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

  titlePanel("Detection of Significant RIF Induction"),
  
  #Required fields
  column(5,style='background:#ECEFF3;margin-right:20px;padding-bottom:179px',
         fluidRow(column(12, HTML("<h4>Please fill in the following fields:</h4>"))),
         #N
         fluidRow(column(6, HTML("<h5>Number of replicates per condition (<i> N </i>): </h5>"))),
         fluidRow(column(3, numericInput("N", label = NULL, value = NULL, min=2, max=100))),
         #p
         fluidRow(column(6, HTML("<h5>p-value for significance (<i> p </i>): </h5>"))),
         fluidRow(column(6, radioButtons("p", label = NULL, choices = c("0.05", "0.01", "0.001"), inline = TRUE))),
         #t
         fluidRow(column(6, HTML("<h5>Time post-irradiation (<i> t </i>): </h5>"))),
         fluidRow(column(3, style='padding-right:0px', numericInput("t", label=NULL, value = NULL, min = 0)),
                  column(2, style='padding-left:2px;padding-top:10px', HTML("hr"))),
         #C
         fluidRow(column(6, HTML("<h5>Confluence (<i> C </i>): </h5>")),),
         fluidRow(column(6, style='padding-right:0px', sliderInput("c_slider", label=NULL, min = 0, max = 100, value = 50, round = TRUE)),
                  column(2, style='padding-left:2px;padding-right:2px', numericInput("c_numeric", label=NULL, 50)),
                  column(1, style='padding-left:0px;padding-top:8px', HTML("%"))),
         #D
         fluidRow(column(6, HTML("<h5>Dose (<i> D </i>): </h5>"))),
         fluidRow(column(6, style='padding-right:0px', sliderInput("d_slider", label=NULL, min = 0, max = 0.5, value=0.25, round = TRUE)),
                  column(2, style='padding-left:0px;padding-right:0px; margin-right:0px; margin-left:0px', numericInput("d_numeric", label=NULL, value = 0.25)),
                  column(1, style='padding-left:0px;padding-top:8px; margin-right:0px', HTML("Gy"))),
         #Extra empty space to match optional fields column 
         conditionalPanel(condition = "input.t_half_radio == 'Chose by mouse strain'", fluidRow(column(12, HTML("<p>  <br></br></p>"))))
  ),#required fields column
  
  #Optional fields
  column(5, style='background:#EDECF3;padding-bottom:8px',
         fluidRow(column(12, style='padding-bottom:0px; margin-bottom:0px', HTML("<div style='padding-bottom:0px; margin-bottom:0px'><h4>The following fields are optional:</h4><p>If left empty a default value will be used.</div>"))),
         #formula 1
         fluidRow(column(12, style='font-weight:bold; text-align:center', HTML("Number of RIF/cell (&mu;) induced by dose D at confluence C and time t post-irradiation:<br><i>&mu; ( D, C, t )= &mu;<sub>0</sub> ( C ) + &mu;<sub>D</sub> ( t )*D</i>"))),
         hr(),
         #formula 2
         fluidRow(column(12, style='text-align:center', HTML("<i>&mu;<sub>0</sub> ( C ) = D<sub>C</sub> ( C - 100 )<sup>2</sup> + &mu;<sub>0</sub><sup>100</sup></i>"))),
         #Dc
         fluidRow(column(4, HTML("<h5>Confluence decay ( <i>D<sub>C</sub></i> ):</h5>"))),
         fluidRow(column(3, style='padding-right:0px', numericInput("Dc", label = NULL, NULL))),
         #mu_100
         fluidRow(column(6, HTML("<h5>Background number of foci/cell at 100% confluence ( &mu;<sub>0</sub><sup>100</sup> ):</h5>"))),
         fluidRow(column(3,  style='padding-right:0px', numericInput("mu_100", label = NULL, NULL))),
         hr(),
         #formula 3
         fluidRow(column(12, style='text-align:center',HTML("<i>&mu;<sub>D</sub> ( t ) = &mu;<sub>D</sub><sup>0</sup> * exp<sup>-ln2 t/t<sub>1/2</sub></sup> + &mu;<sub>D</sub><sup>&infin;</sup></i>"))),
         #t_half
         fluidRow(column(6, HTML("<h5>RIF repair half-life ( <i>t<sub>1/2</sub></i> ): </h5>"))),
         fluidRow(column(12, style='padding-bottom:0px;margin-bottom:0px;padding-left:10px;margin-left:10px', 
                             radioButtons("t_half_radio", label = NULL, width = '800px', selected = "Chose by mouse strain",
                                          choiceNames = list( HTML("1.4 hr (Human breast cells MCF10A)<br><p style='font-size:12px;padding-bottom:0px;margin-bottom:0px'><a href='https://www.pnas.org/content/109/2/443#disp-formula-1' target='_blank'>https://www.pnas.org/content/109/2/443#disp-formula-1</a></p>"), 
                                                              HTML("Mouse skin fibrobalsts<p style='font-size:12px;padding-bottom:0px;margin-bottom:0px'><a >Reference coming soon</a></p>")),
                                          choiceValues = list("1.4hr", "Chose by mouse strain")))),
         conditionalPanel(
           condition = "input.t_half_radio == 'Chose by mouse strain'",
           fluidRow(column(2, style='text-align:right;padding-left:0px;margin-left:0px;padding-right:10px;margin-right:10px;padding-top:8px;', htmlOutput("mouse_strain_thalf")),
                    column(4, style='padding-left:0px; margin-left:0px; padding-top:0px;margin-top:0px', selectInput("mouse_strain", label=NULL, choices = strain_table$strain)))
         ),#conditional panel
         fluidRow(HTML("<p style='margin-left:10px;padding-left:10px;font-size:12px;font-style:italic'>*Please contact us if you would like your kinetics to be added: <a href='mailto:sylvain.v.costes@nasa.gov'>sylvain.v.costes@nasa.gov</a></p>")),
         #mu_0 and mu_inf in same row
         fluidRow(column(6, style='padding-right:0px;margin-right:0px',HTML("<h5>Slope of foci/cell over dose at time 0 ( &mu;<sub>D</sub><sup>0</sup> ):</h5>")),
                  column(6, style='padding-left;0px;margin-left;0px',HTML("<h5>Slope of foci/cell over dose at infinite time (&mu;<sub>D</sub><sup>&infin;</sup>):</h5>"))),
         fluidRow(column(3, style='padding-right:0px', numericInput("mu_0", label = NULL, NULL)),
                  column(2, style='padding-left:4px;padding-top:10px', HTML("RIF/cell/Gy")),
                  column(3, offset = 1, style='padding-right:0px', numericInput("mu_inf", label = NULL, NULL)),
                  column(2, style='padding-left:4px;padding-top:10px', HTML("RIF/cell/Gy")))
  ),#optional fields column
  
  #Result display
  column(12,
         #GO button
         fluidRow(column(12, offset=4, style='margin-left:500px;margin-top:10px;margin-botton:10px', actionButton("go", label="GO", width = "300px"))),
         #label
         fluidRow(column(8, offset = 1, style='margin-top:10px;margin-left:350px',h4("Minimum number of cells required for detection of significant RIF induction:"))),
         #formula
         withMathJax(),
         fluidRow(column(8, offset = 1, "$$n' = T'^2\\left(\\frac{2(D_C(C-100)^2 + \\mu_0^{100})}{D( \\mu_D^0 \\cdot exp^{-(ln2)\\cdot t /t_{1/2}} + \\mu_D^\\infty)} \\left( \\frac{D_C(C-100)^2 + \\mu_0^{100}}{D( \\mu_D^0 \\cdot exp^{-(ln2)\\cdot t /t_{1/2}} + \\mu_D^\\infty) + 1}\\right) + 1\\right)$$")),
         #result
         fluidRow(column(4, style='font-size:30px;border-style:solid;text-align:center;border-color:#DCDAE5;margin-top:10px;margin-bottom:100px', offset = 3, textOutput("n")))
  )#result display
  
)# -----UI

# Server ----
server <- function(input, output, session) {
  
  #Modal to dislpay error if N < 2
  dataModal_NError <- function(failed = FALSE) {
    modalDialog(title = "Error",
                easyClose = TRUE,
                fluidRow(column(12, h5("The number of replicates per condition (N) must be greater or equal to 2."))),
                footer = tagList(modalButton("Ok"))
    )
  }
  
  #Modal to display error if required fields are empty
  dataModal_emptyVal <- function(failed = FALSE) {
    modalDialog(title = "Error",
                easyClose = TRUE,
                fluidRow(column(12, h5("The required fields can not be empty."))),
                footer = tagList(modalButton("Ok"))
    )
  }
  
  #Modal to display error if D == 0
  dataModal_Dcero <- function(failed = FALSE) {
    modalDialog(title = "Error",
                easyClose = TRUE,
                fluidRow(column(12, h5("The Dose (D) must be greater than cero."))),
                footer = tagList(modalButton("Ok"))
    )
  }
  
  #Update c slider and numeric input to match each other
  observeEvent(input$c_slider, {updateNumericInput(session, "c_numeric", label=NULL, input$c_slider)})
  observeEvent(input$c_numeric, {updateSliderInput(session, "c_slider", label=NULL, min = 0, max = 100, value = input$c_numeric)})
  
  #Update d slider and numeric input to match each other
  observeEvent(input$d_slider, {updateNumericInput(session, "d_numeric", label=NULL, input$d_slider)})
  observeEvent(input$d_numeric, {updateSliderInput(session, "d_slider", label=NULL, min = 0, max = 0.5, value = input$d_numeric)})
  
  #Render t_half value to display hr
  output$mouse_strain_thalf <- renderText({HTML( strain_table$t_half[strain_table$strain == input$mouse_strain], "hr")})
  
  #n (result)
  n_reactive <- eventReactive(input$go, {
    N <- input$N
    p <- input$p
    C <- input$c_numeric
    D <- input$d_numeric
    t <- input$t
    mu_0 <- input$mu_0
    mu_inf <- input$mu_inf
    Dc <- input$Dc
    mu_100 <- input$mu_100
    t1 <- NULL
    
    #Get N value from data frame
    if(isTruthy(input$N)){
      if (N<20){
        t1 <- T_table[toString(N), toString(p)]
      }else if(N >= 20 && N<=24){
        t1 <- T_table["20-24", toString(p)]
      }else if(N >= 25 && N<=29){
        t1 <- T_table["25-29", toString(p)]
      }else if(N >= 30 && N<=34){
        t1 <- T_table["30-34", toString(p)]
      }else if(N >= 35 && N<=39){
        t1 <- T_table["35-39", toString(p)]
      }else if(N >= 40 && N<=49){
        t1 <- T_table["40-49", toString(p)]
      }else if(N >= 50 && N<=99){
        t1 <- T_table["50-99", toString(p)]
      }else if(N >= 100 && N<=199){
        t1 <- T_table["100-199", toString(p)]
      }else if(N >= 200 && N<=299){
        t1 <- T_table["200-299", toString(p)]
      }else if(N >= 300 && N<=399){
        t1 <- T_table["300-399", toString(p)]
      }else if(N >= 400 && N<=499){
        t1 <- T_table["400-499", toString(p)]
      }else if(N >= 500 ){
        t1 <- T_table["500+", toString(p)]
      }
    }
    
    #Get t_half value
    t_half <- switch(input$t_half_radio, 
                     "1.4hr" = 1.4,
                     "Chose by mouse strain" = strain_table$t_half[strain_table$strain == input$mouse_strain]
    )
    
    #If optional fields are empty assign default value and update UI
    if(is.na(Dc)){
      updateNumericInput(session, "Dc", label = NULL, formatC(0.00032, format = "e", digits = 2))  
      Dc <- 0.00032
    }
    
    if(is.na(mu_0)){
      updateNumericInput(session, "mu_0", label = NULL, 8.2)  
      mu_0 <- 8.2
    }
      
    if(is.na(mu_inf)){
      updateNumericInput(session, "mu_inf", label = NULL, 0.48)  
      mu_inf <- 0.48
    }
      
    if(is.na(mu_100)){
      updateNumericInput(session, "mu_100", label = NULL, 0.50)  
      mu_100 <- 0.50
    }
      
    #Validations
    if ( !(is.na(N)) && !(is.na(t)) && !(is.na(C)) && !(is.na(D)) ){
      if( N<2 ){
        showModal(dataModal_NError())
        n <- "   .   "
      }else{
        if( D==0) {
          showModal(dataModal_Dcero())
          n <- "   .   "
        }else{
          #***************************************************
          #CALCULTAE RESULT
          shinyjs::runjs("window.scrollTo(0, 500)")
          a <- Dc*((C-100)^2) + mu_100
          b <- D*( mu_0*( exp( -log(2)*t/t_half  )) + mu_inf )
          n <- (t1^2)*(((2*a)/b)*((a/b)+1)+1)
          n <-round(n)
          #***************************************************
        }
      }
    }else{
      showModal(dataModal_emptyVal()) 
      n <- "    .    "
    }
      
  })

  #Render result
  output$n <- renderText({ n_reactive() })

}# ----Server

# Run the app
shinyApp(ui, server)