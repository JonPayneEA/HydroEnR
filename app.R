#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("PDM Application"),
                navbarPage("PDMapp",
                           tabPanel(icon("home"),
                                    sidebarLayout(
                                        sidebarPanel(
                                            h2("Guidance"),
                                            p("Please refer to the guidance note XXXX on the proper useage of this application"),
                                            # code('install.packages("shiny")'),
                                            br(),
                                            br(),
                                            img(src = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/organisation/logo/199/EA_logo_354.jpg", height = 90, width = 250),
                                            br(),
                                            "PDMapp is a product of the", 
                                            span("Environment Agency", style = "color:blue")
                                        ),
                                        mainPanel(
                                            h1("Introducing PDMapp"),
                                            p("PDMapp is a new application developed by the Environment Agency to make the process of reviewing ", 
                                              em("PDM "), 
                                              "models easy"),
                                            br(),
                                            p("For an introduction and live examples, please refer to the forecasting ",
                                              a("sharepoint site.", 
                                                href = "http://ea.sharepoint.com")),
                                            br(),
                                            h2("Current Features"),
                                            p("- See how the parameters affect their conceptual buckets"),
                                            p("- View how these changes affect the PDM forecast"),
                                            br(),
                                            h2("Upcoming Features"),
                                            p("- Calibrate PDM models"),
                                            p("- Generate PDM model simulations"),
                                            p("- Generate probablistic PDM models"),
                                            p("- Use extended sets and alternate PDM paramatisations"),
                                        )
                                    )
                           ),
                           tabPanel("Soil Store",
                                    fluidPage(
                                        h1("Probability-distributed store"),
                                        fluidRow(
                                            column(2,
                                                   br(),
                                                   numericInput("Cmin",
                                                                h4("Cmin"),
                                                                value = 30)
                                            ),
                                            column(2,
                                                   br(),
                                                   numericInput("Cmax",
                                                                h4("Cmax"),
                                                                value = 200)
                                            ),
                                            column(2,
                                                   br(),
                                                   numericInput("b",
                                                                h4("b"),
                                                                value = 1)
                                            ),
                                            column(2,
                                                   br(),
                                                   numericInput("rain",
                                                                h4("Precipitation"),
                                                                value = 25)
                                            ),
                                            column(2,
                                                   br(),
                                                   numericInput("S",
                                                                h4("Initial Store (%)"),
                                                                value = 25)
                                            ),
                                            
                                            plotOutput("runoff_Plot", brush = "plot_brush"),
                                            # htmlOutput("AR_tests"),
                                            # DT::dataTableOutput("AR_roots"),
                                            # plotOutput("AR_Roots")
                                            
                                        )
                                    )
                           ),
                           tabPanel("Evaporation",
                                    sidebarLayout(
                                        sidebarPanel(
                                            h2("AE exponent"),
                                            # numericInput("be",
                                            #              h3(HTML(paste0("b",tags$sub("e")))),
                                            #              value = 1),
                                            sliderInput("be", h4(HTML(paste0("b",tags$sub("e")))),
                                                        min = 0, max = 5,
                                                        value = 1, step = 0.2)
                                        ),
                                        mainPanel(
                                            # tableOutput("table")
                                            # textOutput("selectedData"),
                                            plotOutput("AEPE"),
                                            # plotOutput("MA_Roots")
                                        )
                                    )
                           ),
                           tabPanel("Drainage",
                                    sidebarLayout(
                                        sidebarPanel(
                                            h2("Recharge functions"),
                                            numericInput("kg",
                                                         h4(HTML(paste0("k",tags$sub("g")))),
                                                         value = 100000),
                                            sliderInput("bg", h4(HTML(paste0("b",tags$sub("g")))),
                                                        min = 0, max = 5,
                                                        value = 1, step = 0.2),
                                            numericInput("St",
                                                 h4(HTML(paste0("S",tags$sub("t")))),
                                                 value = 0)
                                    ),
                                        
                                        # Main panel for displaying outputs ----
                                        mainPanel(
                                            plotOutput("drainage", brush = "plot_brush"),                                        
                                            )
                                    )
                           ),
                           tabPanel("Surface routing",
                                    sidebarLayout(
                                        sidebarPanel(
                                            h2("Time cascade"),
                                            numericInput("k1",
                                                         h4(HTML(paste0("K",tags$sub("1")))),
                                                         value = 1),
                                            numericInput("k2",
                                                         h4(HTML(paste0("K",tags$sub("2")))),
                                                         value = 0)
                                        ),
                                        
                                        # Main panel for displaying outputs ----
                                        mainPanel(
                                            plotOutput("linear_store", brush = "plot_brush"),
                                        )
                                    )
                           ),
                           tabPanel("Cubic Store",
                                    sidebarLayout(
                                        sidebarPanel(
                                            h2("Groundwater storage routing"),
                                            numericInput("kb",
                                                         h4(HTML(paste0("K",tags$sub("b")))),
                                                         value = 100),
                                        numericInput("baseflow_s",
                                                     h4("Baseflow start"),
                                                     value = 1)
                                    ),
                                        
                                        # Main panel for displaying outputs ----
                                        mainPanel(
                                            plotOutput("cubic_store", brush = "plot_brush"),
                                        )
                                    )
                           ),
                           tabPanel("PDM Forecast",
                                    sidebarLayout(
                                        sidebarPanel(
                                            h2("Rainfall parameters"),
                                            numericInput("td",
                                                         h4("Time Delay"),
                                                         value = 0),
                                            numericInput("fc",
                                                         h4("Rainfall factor"),
                                                         value = 1)
                                        ),
                                        
                                        # Main panel for displaying outputs ----
                                        mainPanel(
                                            plotOutput("PDM_forecast", brush = "plot_brush"),
                                        )
                                    )
                           )
                )
)

                                            
                                  


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$runoff_Plot <- renderPlot({
        if(input$Cmax <= 0){
            warning("Cmax value is too low")
        } else{
            if(input$b < 0){
                warning("Parameter b must be greater than or equal to 0")
            } else{
                if(input$Cmin <0){
                    warning("Cmin must be greater than or equal to 0")
                } else {
                    if(input$Cmin > input$Cmax){
                        warning("The Cmin value must be less than the Cmax value")
                    }
                }
            }
        }
        Smax <- (input$b * input$Cmin + input$Cmax) / (input$b + 1)
        perc_full <- seq(input$Cmin/Smax, 1, length.out = 100)
        storage <- perc_full * Smax
        start_storage <- ifelse(storage - Smax*(input$S/100)<0, 0,
                                storage - Smax*(input$S/100))
        rain_storage <- start_storage - input$rain
        C_star <-input$ Cmin + (input$Cmax - input$Cmin) * (1 - ((Smax - storage) / (Smax - input$Cmin)) ^ (1/(input$b + 1)))
        stuff <- tibble::tibble(perc_full, storage, C_star)
        C_star_point <- ifelse(max(which(start_storage==0)) == -Inf, 1, max(which(start_storage==0)))
        R_star_point <- min(which(rain_storage > 0))
        ggplot(stuff, aes(x = C_star, y = storage)) +
            scale_y_reverse() +
            geom_ribbon(aes(ymin = start_storage-input$rain, ymax = storage), fill ="lightblue") +
            geom_ribbon(aes(ymin = start_storage-input$rain, ymax = start_storage), fill ="cornflowerblue") +
            geom_line(size = 1) +
            geom_line(aes(x = C_star, y = start_storage-input$rain), size = 1, linetype = "dashed") +
            geom_line(aes(x = C_star, y = start_storage), size = 1) +
            geom_hline(yintercept = 0, size = 1.5, colour = "black")  +
            geom_segment(aes(x = C_star[C_star_point], xend = C_star[C_star_point], y = storage[C_star_point], yend = 0),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_segment(aes(x = C_star[C_star_point], xend = C_star[C_star_point], y = 0, yend = storage[C_star_point]),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_label(label = "C*", aes(x = C_star[C_star_point] + 5, y = storage[C_star_point]/2)) +
            geom_segment(aes(x = C_star[R_star_point], xend = C_star[R_star_point], y = storage[R_star_point], yend = 0),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_segment(aes(x = C_star[R_star_point], xend = C_star[R_star_point], y = 0, yend = storage[R_star_point]),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_label(label = expression("C* + " ~ pi * Delta ~"t"), aes(x = C_star[R_star_point] + 10, y = storage[R_star_point]/2)) +
            # geom_segment(aes(x = C_star[1]-5, xend = C_star[1]-5, y = 0,  yend = 0 - input$rain),
            #             arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_segment(aes(x = C_star[1]-5, xend = C_star[1]-5, y = 0 -input$rain,  yend = 0),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_label(label = expression("P (" ~ pi * Delta ~"t)"), aes(x = C_star[1]-15, y = (0 -input$rain)/2)) +
            geom_label(label = "Direct runoff", aes(x = C_star[R_star_point], y = (0 -input$rain)/2)) +
            geom_segment(aes(x = C_star[100]+ 2, xend = C_star[100]+ 2, y = 0 + input$Cmin,  yend = 0),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_segment(aes(x = C_star[100]+ 2, xend = C_star[100]+ 2, y = 0,  yend =  0 + input$Cmin),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_label(label = "Cmin", aes(x = C_star[100]-10, y = input$Cmin/2)) +
            geom_segment(aes(x = C_star[100]+ 12, xend = C_star[100]+ 12, y = 0 + Smax,  yend = 0),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_segment(aes(x = C_star[100]+ 12, xend = C_star[100]+ 12, y = 0,  yend =  0 + Smax),
                         arrow = arrow(length = unit(0.5, "cm")), size = 1) +
            geom_label(label = "Cmax", aes(x = C_star[100], y = Smax/2)) +
            theme_void()
        
    })
    output$AEPE <- renderPlot({
        if(input$be <0){
            warning("The be parameter must be greater than 0")
        } else {
            if(input$be >= 0 & input$be < 1){
                message("The be parameter is normally greater than 1")
            }
        }
        soil_fullness <- c(0:100)
        evaporation <- 1 - (1 - (soil_fullness/100)) ^ input$be
        calculated_AE <- 1 - (1 - (input$S/100)) ^ input$be
        evap_table <- tibble::tibble(soil_fullness, evaporation)
        ggplot(evap_table, aes(x = soil_fullness, y = evaporation)) +
            geom_line(size = 1) +
            labs(x = "Soil Store Fullness (%)", y = "Proportion evaporation") +
            geom_segment(aes(x = input$S, xend = input$S, y = 0,  yend = calculated_AE), 
                         colour = "red", size = 1, linetype = "dashed") +
            geom_segment(aes(x = 0, xend = input$S, y = calculated_AE,  yend = calculated_AE), 
                         colour = "red", size = 1, linetype = "dashed") +
            geom_point(aes(x = input$S, y = calculated_AE),
                       fill = "red", size = 4, shape = 23)
            
    })
    output$drainage <- renderPlot({
        Smax <- (input$b * input$Cmin + input$Cmax) / (input$b + 1)
        if(input$kg <= 0){
            warning("The parameter kg must be greater than 0")
        } else { 
            if(input$bg <= 0){
                warning("The b_g parameter must be greater than 0")
            } else {
                if(input$St < 0){
                    warning("The St parameter must be greater than or equal to 0")
                } else {
                    if(input$St >= Smax) {
                        warning("The paramaetr St must be less than the Smax")
                    }
                }
            }
        }
        k <- 1/input$kg
        perc_full <- seq(0, 1, length.out = 100)
        storage <- perc_full * 100
        drainage <- ifelse(storage < input$St, 0, k * (storage - input$St) ^ input$bg) 
        calculated_drain <- k * (input$S - input$St) ^ input$bg
        drainage_table <- tibble::tibble(storage, perc_full, drainage)
        ggplot(drainage_table, aes(x = storage, y = drainage)) +
            geom_line(size = 1) +
            labs(x = "Soil Store Fullness (%)", y = expression("Drainage (mm /"~Delta~"t)")) +
            geom_segment(aes(x = input$S, xend = input$S, y = min(drainage),  yend = calculated_drain),
                         colour = "red", size = 1, linetype = "dashed") +
            geom_segment(aes(x = min(storage), xend = input$S, y = calculated_drain,  yend = calculated_drain),
                         colour = "red", size = 1, linetype = "dashed") +
            geom_point(aes(x = input$S, y = calculated_drain),
                       fill = "red", size = 4, shape = 23)
        
    })
    output$linear_store <- renderPlot({
        Dt <- 0.25
        if(input$k1 <=0){
            warning("The k1 parameter must be greater than 0")
        } else {
            if(input$k2 < 0){
                warning("The k2 parameter must be equal to or greater than zero. If k2 = 0 then k2 will be coereced to k1")
            } else {
                if(input$k2 == 0){
                    k2 <- input$k1
                    message("k2 has been coerced to the same parameter as k1")
                } else {
                    if(input$k2 != input$k1)
                        k2 <- input$k2 
                }
            }
        }
        output <- 100
        s1_alpha <- -exp(-Dt/input$k1)
        s1_beta <- 1 + s1_alpha
        s2_alpha <- -exp(-Dt/k2)
        s2_beta <- 1 + s2_alpha
        b0 <- s1_beta * s2_beta
        a1 <- s1_alpha + s2_alpha
        a2 <- s1_alpha * s2_alpha
        if(length(input$rain) >= output){
            precip <- c(0, input$rain, 0)
            precip <- rain[1:output+2]
        } else{
            precip <- c(0, input$rain, rep(0, output-(length(input$rain))+1))
        }
        flow <- rep(0,length(precip))
        for(i in 1:(length(precip)-2)){
            flow[i+2] <- -a1*flow[i+1] - a2 * flow [i] + b0 * precip[i+1]
        }
        ts <- seq_along(flow)
        ls <- tibble::tibble(flow, ts)
        ggplot(ls, aes(x = ts, y = flow)) +
            geom_line(size = 1) +
            labs(x =  expression("Timestep (hours)"), y = expression("Quick Flow (mm /"*Delta*"t)"))
        
    })
    output$cubic_store <- renderPlot({
        Dt <- 0.25
        output <- 100
        drainage <- (1/input$kg) * (input$S - input$St) ^ input$bg
        k <- 1/(input$kb^3)
        drainage <- c(drainage, rep(0, output))
        baseflow <-c(input$baseflow_s, rep(0, output))
        storage <- (baseflow[1]/k)^(1/3)
        storage <- c(storage, rep(0, output))
        for(i in 1:output){
        storage[i+1] <- storage[i]-(1/(3*k*(storage[i]^2)))*(exp(-3*k*(storage[i]^2)*Dt)-1)*(drainage[i+1] - k*(storage[i] ^3))
            baseflow[i+1] <- k*(storage[i+1]^3)
        }
        index <- seq_along(baseflow)
        cubic <- tibble::tibble(storage, baseflow, index)
        ggplot(cubic,aes(x = index, y = baseflow)) +
            geom_line(size = 1) +
            labs(x =  expression("Timestep (hours)"), y = expression("Baseflow (mm /"*Delta*"t)"))
        
    })
    output$PDM_forecast <- renderPlot({
        PE <- rep(0.0153, 100)
        Area <- 1
        Qconst <- 0
        dt <- 0.25  # Unit of time 15 mins
        output <- 100
        Smax <- (input$b * input$Cmin + input$Cmax) / (input$b + 1)     # Smax calculation
        precip <- rep(0, output+1)
        precipitation <- c(rep(0, (input$td/dt)), input$rain, precip)*input$fc    # Adjust pure time delay
        if(input$td !=0){
            PE <- c(rep(PE[1], (input$td/dt)), PE)
        }
        if(input$Cmax <= 0){
            warning("Cmax value is too low")
        } else{
            if(input$b <= 0){
                warning("Parameter b must be greater than 0")
            } else{
                if(input$Cmin <0){
                    warning("Cmin must be greater than or equal to 0")
                } else {
                    if(input$Cmin > input$Cmax){
                        warning("The Cmin value must be less than the Cmax value")
                    } else {
                        if(input$be <0){
                            warning("The be parameter must be greater than 0")
                        } else {
                            if(input$be >= 0 & input$be < 1){
                                message("The be parameter is normally greater than 1")
                            } else {
                                if(input$kg <= 0){
                                    warning("The parameter kg must be greater than 0")
                                } else {
                                    if(input$bg <= 0){
                                        warning("The b_g parameter must be greater than 0")
                                    } else {
                                        if(input$St < 0){
                                            warning("The St parameter must be greater than or equal to 0")
                                        } else {
                                            if(input$St >= Smax){
                                                warning("The paramaetr St must be less than the Smax")
                                            } else {
                                                if(input$k1 <=0){
                                                    warning("The k1 parameter must be greater than 0")
                                                } else {
                                                    if(input$k2 < 0){
                                                        warning("The k2 parameter must be equal to or greater than zero. If k2 = 0 then k2 will be coereced to k1")
                                                    } else {
                                                        if(input$k2 == 0){
                                                            k2 <- input$k1
                                                            message("k2 has been coerced to the same parameter as k1")
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        storage <- c(input$S, rep(0, length(precipitation))) # Initial storage
        store_fullness <- c(storage[1]/Smax, rep(0, length(precipitation))) # Initial proportion of full storage
        AE_PE <- c((1 - (1 - store_fullness[1]) ^ input$be), rep(0, length(precipitation)))    # Initial evaporation
        evaporation <- rep(0, (length(precipitation)+1))
        C_star <- c(input$Cmin + (input$Cmax - input$Cmin) * (1 - ((Smax - storage[1]) / (Smax - input$Cmin)) ^ (1/(input$b + 1))),
                    rep(0, length(precipitation)))    # Initial C*star dependent on S parameter
        prop_runoff <- c(max(0, 1 - ((input$Cmax - C_star[1])/(input$Cmax-input$Cmin))^input$b), rep(0, length(precipitation))) # Initial proportion of run-off
        runoff <- c(0, rep(0, length(precipitation)))   # Initial run-off
        k <- 1/input$kg # k value
        drainage <- c(ifelse(storage[1] <= input$St, 0, k * (storage[1] - input$St) ^ input$bg), rep(0, length(precipitation)))   # Initial drainage, relates to storage S value
        print(precipitation)
        for(i in seq_along(precipitation)){
            drainage[i] <- ifelse(storage[i] <= input$St, 0, k * (storage[i] - input$St) ^ input$bg)
            AE_PE[i] <- 1 - (1 - store_fullness[i]) ^ input$be
            evaporation[i] <- PE[i] * AE_PE[i]
            # if((storage[i] + (precipitation[i]-(precipitation[i]*prop_runoff[i])) - drainage[i] - evaporation[i]) >= Smax){
            #     storage[i+1] <- Smax
            # } else {
            #     if(storage[i] + (precipitation[i]-(precipitation[i]*prop_runoff[i])) - drainage[i] - evaporation[i] < 0){
            #         storage[i+1] <- 0
            #     } else {
                    # if(storage[i] + (precipitation[i]-(precipitation[i]*prop_runoff[i])) - drainage[i] - evaporation[i] < Smax &&
                    # storage[i] + (precipitation[i]-(precipitation[i]*prop_runoff[i])) - drainage[i] - evaporation[i] >= 0){
                    storage[i+1] <- storage[i] + (precipitation[i]-(precipitation[i] * prop_runoff[i])) - drainage[i] - evaporation[i]
                #     }
                # }
            # } # Nested ifs to set S relative to Smax
            store_fullness[i+1] <- storage[i+1]/Smax # Proportion of soil store full
            C_star[i+1] <- input$Cmin + (input$Cmax - input$Cmin) * (1 - ((Smax - storage[i+1]) / (Smax -input$ Cmin)) ^ (1/(input$b + 1))) # Cstar
            prop_runoff[i+1] <- max(0, ifelse(storage[i] == Smax, 1, 1 - ((input$Cmax - C_star[i+1])/(input$Cmax-input$Cmin))^input$b)) # Proportion of store generating runoff
            runoff[i+1] <-  precipitation[i] * prop_runoff[i+1] # Effective rain as run off
        }
        runoff_table <- tibble::tibble(store_fullness, storage, C_star, prop_runoff, runoff, drainage, AE_PE, evaporation)
        # Linear Store
        if(input$k2 == 0){
            k2 <- input$k1
        } else {
            k2 <- input$k2
        }
        s1_alpha <- -exp(-dt/input$k1)
        s1_beta <- 1 + s1_alpha
        s2_alpha <- -exp(-dt/k2)
        s2_beta <- 1 + s2_alpha
        b0 <- s1_beta * s2_beta
        a1 <- s1_alpha + s2_alpha
        a2 <- s1_alpha * s2_alpha
        rain <- c(0, runoff_table$runoff, rep(0, output+1))
        flow <- rep(0,length(rain))
        for(i in 1:(length(rain)-2)){
            flow[i+2] <- -a1*flow[i+1] - a2 * flow [i] + b0 * rain[i+1] # Loop to calculate rapid run off
        }
        # Cubic store
        cubic_k <- 1/(input$kb^3) # k value for cubic store
        drainage_to_cubic <- c(runoff_table$drainage, rep(0, output +1)) # Drainage data
        initial_store <- input$baseflow_s # Arbitrary number, initial storage value
        baseflow <- cubic_k*(initial_store[1]^3) # Initial baseflow
        baseflow <-c(baseflow, rep(0, length(drainage_to_cubic))) # Dummmy baseflow series, rows 2 onwards will be over written
        cubic_store <- c(initial_store, rep(0, output)) # Dummy baseflow series, rows 2 onwards will be overwritten
        for(i in seq_along(drainage_to_cubic)){
            cubic_store[i+1] <- cubic_store[i]-(1/(3*cubic_k*(cubic_store[i]^2)))*(exp(-3*cubic_k*(cubic_store[i]^2)*dt)-1)*(drainage_to_cubic[i+1] - cubic_k*(cubic_store[i] ^3))
            cubic_store[i+1] <- ifelse(cubic_store[i+1] < 0, 0, cubic_store[i+1]) # Minimises cubic store to zero, prevents negative baseflow
            baseflow[i+1] <- cubic_k*(cubic_store[i+1]^3) + Qconst # Addition of Qconst to baseflow
        } # Loop to generate baseflows
        # print (paste("Smax = ", Smax))
        # print(flow)
        # print(baseflow)
        pred_Q <- (baseflow + flow) * Area
        timestep <- seq_along(pred_Q)
        flows <- tibble::tibble(baseflow* Area, flow* Area, pred_Q, timestep)
        print(runoff_table)
        ggplot(flows, aes(x = timestep, y = pred_Q)) +
            geom_line(colour = "blue", size = 1) +
            geom_line(aes(x = timestep, y = flow),colour = "black", size = 1) +
            geom_line(aes(x = timestep, y = baseflow), colour = "red", size = 1) +
            labs(x =  expression("Timestep (15 mins)"), y = expression("Flow (mm /"*Delta*"t)")) +
            scale_colour_manual(name = "Variables", 
                                values =c("black"="black","red"="red", "blue" = "blue"), labels = c("Quick Flow","Baseflow", "Combined")) +
            xlim(0, 105)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
