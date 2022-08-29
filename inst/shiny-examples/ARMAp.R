#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages ----
library(shiny)
library(forecast)
library(ggplot2)
library(shinythemes)
library(BLRPM)

# Load data ----
# HiFlow <- as.matrix(data(NRFAData)) # import all PeakFlow sites and catchment descriptors

# Source functions
# source("U:/Rstuff/ARMA/ARMA_sim_5AR.R")

# Tp calculation based off ReFH

ui <- fluidPage(theme = shinytheme("yeti"),
    titlePanel("ARMA Parameter Testing Hub"),
    navbarPage("ARMAp",
               tabPanel(icon("home"),
                        sidebarLayout(
                            sidebarPanel(
                                h2("Guidance"),
                                p("Please refer to the guidance note ", strong("(upcoming)"), "on the proper useage of this application and the underlying therory on ARMA updating"),
                                # code('install.packages("shiny")'),
                                br(),
                                br(),
                                img(src = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/organisation/logo/199/environment-agency-logo-480w.png", height = 90, width = 250),
                                br(),
                                "ARMAp is a product of the",
                                span("Environment Agency.", style = "color:darkblue"),
                                br(),
                                "Please contact ", a("Jonathan Payne", href = "mailto:jon.payne@environment-agency.gov.uk?
                                body=''&subject=ARMAp Query"),
                                " or ", a("Mike Vaughan", href = "mailto:michael.vaughan@environment-agency.gov.uk?
                                body=''&subject=ARMAp Query"), " for further details."),
                            mainPanel(
                                h1("Introducing ARMAp"),
                                p("ARMAp is a new application developed by the Environment Agency to make it ",
                                  em("easy "),
                                  "to test the stability of ARMA parameters prior to them being used in IMFS"),
                                br(),
                                p("For an introduction and live examples, please refer to the forecasting ",
                                  a("sharepoint site.",
                                    href = "http://ea.sharepoint.com")),
                                br(),
                                h2("Current Features"),
                                p("- View the characteristic decay of AR parameters."),
                                p("- Assess the performance of ARMA parameters using the characteristic roots up to a level of ",
                                  strong("ARMA(5,2)")),
                                p("- Develop rainfall series using a ", strong("Bartlett-Lewis model"),"."),
                                br(),
                                h2("Upcoming Features"),
                                p("- Import your own error series and derive suitable ARMA parameters for it."),
                                p("- Use catchment descriptors to inform interim ARMA parameters."),
                                p("- Simulate a rainfall runoff model (PDM) using the stochastic rainfall tool. Outputs include quasi-observed and simulated error series"),

                            )
                        )
               ),

               tabPanel("AR Suitability",
                        fluidPage(
                          h1("AR Parameter Testing"),
                          fluidRow(
                            column(2,
                                   br(),
                                   selectInput("Sign1", h4("Sign convention"),
                                               choices = list("Deltares" = 1, "Standard" = 2),
                                               selected = 1),
                                   numericInput("Length_AR",
                                                h4("Length"),
                                                value = 100)
                            ),
                            column(2,
                                   br(),
                                   numericInput("AR1",
                                                h4("AR1"),
                                                value = 0.01),
                                   numericInput("Lag1",
                                                h4("Lag 1"),
                                                value = 1)
                            ),
                            column(2,
                                   br(),
                                   numericInput("AR2",
                                                h4("AR2"),
                                                value = 0),
                                   numericInput("Lag2",
                                                h4("Lag 2"),
                                                value = 1)
                            ),
                            column(2,
                                   br(),
                                   numericInput("AR3",
                                                h4("AR3"),
                                                value = 0),
                                   numericInput("Lag3",
                                                h4("Lag 3"),
                                                value = 1)
                            ),
                            column(2,
                                   br(),
                                   numericInput("AR4",
                                                h4("AR4"),
                                                value = 0),
                                   numericInput("Lag4",
                                                h4("Lag 4"),
                                                value = 1)

                            ),
                            column(2,
                                   br(),
                                   numericInput("AR5",
                                                h4("AR5"),
                                                value = 0),
                                   numericInput("Lag5",
                                                h4("Lag 5"),
                                                value = 1)
                            ),
                          ),

                          plotOutput("AR_Plot"),
                          htmlOutput("AR_tests"),
                          DT::dataTableOutput("AR_roots"),
                          plotOutput("AR_Roots")

                        )
               ),

               tabPanel("MA Suitability",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Input your MA(",em("q"),") parameters below"),
                                selectInput("Sign2", h3("Sign convention"),
                                            choices = list("Deltares" = 1, "Standard" = 2),
                                            selected = 1),
                                numericInput("MA1",
                                             h3("MA1"),
                                             value = 0.01),
                                numericInput("MA2",
                                             h3("MA2"),
                                             value = 0)),
                            mainPanel(
                                # tableOutput("table")
                                # textOutput("selectedData"),
                                # plotOutput("AR_Plot"),
                                plotOutput("MA_Roots")
                            )
                        )
               ),
               # tabPanel("Data Import",
               #          sidebarLayout(
               #            sidebarPanel(
               #              # Input: Select a file ----
               #              fileInput("file1", "Choose CSV File",
               #                        multiple = FALSE,
               #                        accept = c("text/csv",
               #                                   "text/comma-separated-values,text/plain",
               #                                   ".csv")),
               #              # Horizontal line ----
               #              tags$hr(),
               #              # Input: Checkbox if file has header ----
               #              checkboxInput("header", "Header", TRUE),
               #              # Input: Select separator ----
               #              radioButtons("sep", "Separator",
               #                           choices = c(Comma = ",",
               #                                       Semicolon = ";",
               #                                       Tab = "\t"),
               #                           selected = ","),
               #              # Input: Select quotes ----
               #              radioButtons("quote", "Quote",
               #                           choices = c(None = "",
               #                                       "Double Quote" = '"',
               #                                       "Single Quote" = "'"),
               #                           selected = '"'),
               #              # Horizontal line ----
               #              tags$hr(),
               #              # Input: Select number of rows to display ----
               #              radioButtons("disp", "Display",
               #                           choices = c(Head = "head",
               #                                       All = "all"),
               #                           selected = "head")
               #
               #            ),
               #
               #            # Main panel for displaying outputs ----
               #            mainPanel(
               #
               #              # Output: Data file ----
               #              tableOutput("contents")
               #            )
               #          )
               # ),
               # tabPanel("Catchment Descriptors",
               #          titlePanel("NRFA Catchment Descriptors"),
               #
               #            # Create a new Row in the UI for selectInputs
               #            # fluidRow(
               #            #   column(4,
               #            #          selectInput("ID",
               #            #                      "Gauge:",
               #            #                      c("All",
               #            #                        unique(as.character(row.names(HiFlow)))))
               #            #   ),
               #            #   column(4,
               #            #          selectInput("AREA",
               #            #                      "Catchment Area:",
               #            #                      c("All",
               #            #                        unique(as.character(HiFlow$AREA))))
               #            #   ),
               #            #   column(4,
               #            #          selectInput("BFIHOST",
               #            #                      "BFI Host:",
               #            #                      c("All",
               #            #                        unique(as.character(HiFlow$BFIHOST))))
               #            #   )
               #            # ),
               #            # Create a new row for the table.
               #
               #            DT::dataTableOutput("table")
               #
               # ),
               tabPanel("Rainfall generator",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Input your Bartlet Lewis parameters below"),
                            numericInput("Lambda",
                                         h3("Lambda"),
                                         value = 16/240),
                            numericInput("Gamma",
                                         h3("Gamma"),
                                         value = 2/10),
                            numericInput("Beta",
                                         h3("Beta"),
                                         value = 0.3),
                            numericInput("Eta",
                                         h3("Lambda"),
                                         value = 2),
                            numericInput("Mux",
                                         h3("Mux"),
                                         value = 4),
                            numericInput("t.sim",
                                         h3("t.sim"),
                                         value = 1000),
                            numericInput("Interval",
                                         h3("Interval"),
                                         value = 1),
                            numericInput("Offset",
                                         h3("Offset"),
                                         value = 0)),
                          mainPanel(
                            # tableOutput("table")
                            # textOutput("selectedData"),
                            # plotOutput("AR_Plot"),
                            plotOutput("rainfall", width = 1000, height = 1000)
                          )
                        )
               ),
               tabPanel("NRFA Map",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Input your Easting and Northing references here"),
                            numericInput("Easting",
                                         h3("Easting"),
                                         value = 460000),
                            numericInput("Northing",
                                         h3("Northing"),
                                         value = 440000)
                          ),
                          mainPanel(
                            titlePanel("ARMA sites"),
                            plotOutput("Map", width = 750, height = 1000)
                          )
               )
               )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(
  function(input, output) {
    output$AR_Plot <- renderPlot({
      len <- input$Length_AR +5
      value <- rep(1, len)
      value[5] <- input$Lag1
      value[4] <- input$Lag2
      value[3] <- input$Lag3
      value[2] <- input$Lag4
      value[1] <- input$Lag5
      loop <- length(value)-5
      if(input$Sign1 == 2){
        for(i in 1:loop)
        {
          value[i+5] <- -input$AR1*value[i+4] - input$AR2*value[i+3] - input$AR3*value[i+2] - input$AR4*value[i+1] - input$AR5*value[i]
        }
      } else {
        for(i in 1:loop)
        {
          value[i+5] <- -(input$AR1*-1)*value[i+4] - (input$AR2*-1)*value[i+3] - (input$AR3*-1)*value[i+2] - (input$AR4*-1)*value[i+1] - (input$AR5*-1)*value[i]
        }
      }
      value <- as.data.frame(value)
      value$x <- c(-4:loop)
      ggplot(value, aes(x = x, y = value)) +
        geom_line(color="blue", size=1) +
        geom_point(color="red", size=1) +
        geom_hline(yintercept =  0, linetype = "dashed", size = 0.5)+
        labs(title = "AR decay of the selected parameters") +
        xlab("Time step") +
        ylab("AR decay")
    })
    output$AR_Roots <- renderPlot({
      if(input$Sign1 == 2){
        params <- data.frame(input$AR1, input$AR2, input$AR3, input$AR4, input$AR5)*-1
      } else {
        params <- data.frame(input$AR1, input$AR2, input$AR3, input$AR4, input$AR5)
      }
      abs_params <- abs(params)
      parvec <- as.numeric(params[1:5])
      last.nonzero <- max(which(abs(parvec) > 1e-08))
      roots <- structure(list(
        roots=polyroot(c(1,-parvec[1:last.nonzero])),
        type="AR"),
        class='armaroots')
      oldpar <- par(pty='s')
      on.exit(par(oldpar))
      plot(c(-2,2), c(-2,2),
           xlab="Real", ylab="Imaginary",
           type="n", bty="n", xaxt="n", yaxt="n",
           main=paste("Inverse roots of", roots$type,"characteristic polynomial"))
      axis(1, at=c(-2,-1,0,1,2), line=0.5, tck=-0.025)
      axis(2, at=c(-2,-1,0,1,2), label=c("-2i","-i","0","i", "2i"),
           line=0.5, tck=-0.025)
      circx <- seq(-1,1,l=501)
      circy <- sqrt(1-circx^2)
      lines(c(circx,circx), c(circy,-circy), col="grey", lwd = 2)
      lines(c(-2,2), c(0,0), col="grey", lwd = 2)
      lines(c(0,0), c(-2,2), col="grey", lwd = 2)
      lines(c(1,0), c(0,0), col = "#00af41", lwd = 3)
      if(length(roots) > 0){
        inside <- as.logical((abs(roots$roots) > 1) * (abs(round(Im(roots$roots), digits = 6))==0))
        points(1/roots$roots[inside], pch=19, col="black", cex = 1.5)
        if(sum(!inside) > 0)
          points(1/roots$roots[!inside], pch=19, col="red", cex = 1.5)
      }
    })
    output$MA_Roots <- renderPlot({
      params <- data.frame(input$MA1, input$MA2)
      abs_params <- abs(params)
      parvec <- as.numeric(params[1:2])
      last.nonzero <- max(which(abs(parvec) > 1e-08))
      roots <- structure(list(
        roots=polyroot(c(1,-parvec[1:last.nonzero])),
        type="MA"),
        class='armaroots')
      oldpar <- par(pty='s')
      on.exit(par(oldpar))
      plot(c(-1,1), c(-1,1),
           xlab="Real", ylab="Imaginary",
           type="n", bty="n", xaxt="n", yaxt="n",
           main=paste("Inverse roots of", roots$type,"characteristic polynomial"))
      axis(1, at=c(-1,0,1), line=0.5, tck=-0.025)
      axis(2, at=c(-1,0,1), label=c("-i","0","i"),
           line=0.5, tck=-0.025)
      circx <- seq(-1,1,l=501)
      circy <- sqrt(1-circx^2)
      lines(c(circx,circx), c(circy,-circy), col="grey", lwd = 2)
      lines(c(-2,2), c(0,0), col="black", lwd = 2)
      lines(c(0,0), c(-2,2), col="black", lwd = 2)
      if(length(roots$roots) > 0)
      {
        inside <- abs(roots$roots) > 1
        points(1/roots$roots[inside], pch=19, col="black", cex = 1.5)
        if(sum(!inside) > 0)
          points(1/roots$roots[!inside], pch=19, col="red", cex = 1.5)
      }
    })
    output$AR_tests <- renderText({
      if(input$Sign1 == 2){
        conv = -1
      } else {
        conv = 1
      }
      if(input$AR1 * conv < 0){
        out1 <- paste("The AR1 parameter magnitude (", input$AR1*conv, ") is too low, this could result in an unstable error decay. Please check sign convention input.")
      } else {
        out1 <- paste("AR1 parameter (", input$AR1*conv, ") is suitable.")
      }
      if(input$AR2 * conv >= 0){
        out2 <- "The AR2 parameter is positive, this may result in an unstable error decay. Investigate by extending length to >= 500"
      } else {
        out2 <- "The AR2 paramater is negative, this is normaly suitable"
      }
      if((abs(input$AR1) >= abs(input$AR2))&
         (abs(input$AR2) >= abs(input$AR3))&
         (abs(input$AR3) >= abs(input$AR4))&
         (abs(input$AR4) >= abs(input$AR5))==TRUE){
        out3 <- "Magnitudes of parameters decrease from the higher order AR parameters"
      } else {
        out3 <- "Magnitude of AR parameters are not exhibiting a gradual decline, this could lead to instabilities in AR decay"
      }

      HTML(paste(out1,out2, out3, sep="<br/>"))
    })
    output$AR_roots <- DT::renderDataTable({
      if(input$Sign1 == 2){
        conv = -1
      } else {
        conv = 1
      }
      params <- data.frame(input$AR1, input$AR2, input$AR3, input$AR4, input$AR5)*conv
      parvec <- as.numeric(params[1:5])
      last.nonzero <- max(which(abs(parvec) > 1e-08))
      roots_raw <- structure(list(
        roots=polyroot(c(1,-parvec[1:last.nonzero])),
        type="AR"),
        class="armaroots")
      root <- unlist(roots_raw$roots)
      Roots <- print(root)
      Real_Unit <- round(Re(Roots), digits = 6)
      Imaginary_Unit <- round(Im(Roots), digits = 6)
      Details <- rep(0, length(Roots))
      for(i in 1:length(Roots)){
        if(Imaginary_Unit[i] == 0 & Real_Unit[i] > 1){
          Details[i] <- "Characteristic roots imply that component is suitable for forecasting purposes."
        } else{
          if(Imaginary_Unit[i] == 0 & Real_Unit[i] < -1){
            Details[i] <- "The characteristic root of the real component is negative and sits outside the unit circle, this is generally not suitable for forecasting. However, if it's magnitude is lower than the other parameters check the AR decay plot under various initial conditions for stability."
          } else{
          if((Imaginary_Unit[i] > 1 | Imaginary_Unit[i]< -1)*(Real_Unit[i] > 1 |Real_Unit[i] < -1)){
            Details[i] <- "Though the AR parameters are stable, the characteristics of the polynomial imply that there could be oscillatory behaviours in the error decay."
          } else {
            if(Real_Unit[i] <= 1 | Real_Unit[i] >= -1){
              Details[i] <- "Characteristic roots imply that component is not suitable for forecasting purposes, real component sits within unit circle."
            } else {
              Details[i] <- "Characteristic roots imply that component is not suitable for forecasting purposes."
            }
            }
          }
        }
      }
      Inverse <- print(1/root)
      Real_Inverse <- round(Re(Inverse), digits = 6)
      Imaginary_Inverse <- round(Im(Inverse), digits = 6)
      Inverted_Details <- rep(0, length(Roots))
      for(i in 1:length(Roots)){
        if((Imaginary_Inverse[i] == 0 & Real_Inverse[i] < 1 & Real_Inverse[i] > 0)){
          Inverted_Details[i] <- "Inverted characteristic roots imply that component is suitable for forecasting purposes, real element sits within unit circle."
          } else{
          if((Imaginary_Inverse[i] == 0 & Real_Inverse[i] < 0 & Real_Inverse[i] > -1)){
            Inverted_Details[i] <- "The inverted characteristic root of the real component is negative but sits within the unit circle, this is generally not suitable for forecasting. However, if it's magnitude is lower than the other parameters check the AR decay plot under various initial conditions for stability."
            } else {
          if((Imaginary_Inverse[i] < 1 & Imaginary_Unit[i] > -1)*(Real_Inverse[i] < 1 & Real_Inverse[i] > -1)){
            Inverted_Details[i] <- "Though the AR parameters are stable, the inverted characteristics of the polynomial imply that there could be oscillatory behaviours in the error decay."
            } else {
            if(Real_Inverse[i] >= 1 | Real_Inverse[i] <= -1){
              Inverted_Details[i] <- "Inverted characteristic roots imply that component is not suitable for forecasting purposes, real component sits outside unit circle."
              } else {
              Inverted_Details[i] <- "Inverted characteristic roots imply that component is not suitable for forecasting purposes."
              }
            }
          }
        }
      }
      df <- data.frame(Real_Unit, Imaginary_Unit, Details, Real_Inverse, Imaginary_Inverse, Inverted_Details)
      DT::datatable(df)

    })
    output$rainfall <- renderPlot({
      sim_rain <- BLRPM.sim(input$Lambda,input$Gamma,input$Beta,input$Eta,input$Mux,input$t.sim,t.acc=input$t.sim,input$Interval,input$Offset)
      plot(sim_rain)
    })
    output$contents <- renderTable({

      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.

      req(input$file1)

      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
    })
    output$table <- DT::renderDataTable(DT::datatable({
      data <- data.frame(NRFAData)
      # if (input$ID != "All") {
      #   data <- data[input$ID,]
      # }
      # if (input$AREA != "All") {
      #   data <- data[data$AREA == input$AREA,]
      # }
      # if (input$BFIHOST!= "All") {
      #   data <- data[data$BFIHOST == input$BFIHOST,]
      # }
      data
    })
    )
    output$Map <- renderPlot({
      plot(UKOutline, pch = 19, cex = 0.25, xlab = "Easting",
           ylab = "Northing", xlim = c(25272, 650000))
      QMED.Pool <- QMEDData[21:22]
      points(QMED.Pool, pch = 19, col = "red")
      points(input$Easting, input$Northing, pch = 24, cex = 3, col = "blue", bg = "red", lwd = 2)
    })
  }
)




# Run the application
shinyApp(ui = ui, server = server)
