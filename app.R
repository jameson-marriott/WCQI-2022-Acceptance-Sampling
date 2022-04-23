# Notes for improvement
# reduce the number of packages that the app loads
# make an option for a reference OC curve
# make additional tabs to specify OC curves different ways?
# test the load of 75 people accessing it all at the same time
# make sure that n cannot be larger than N
# make sure that c cannot be larger than n

library(shiny)
library(shinydashboard)
library(AcceptanceSampling)
library(AQLSchemes)
library(tidyverse) # only grab used packages
#library(patchwork)
theme_set(theme_bw())

# Define UI
ui <- dashboardPage(# Application title
  dashboardHeader(title = "Acceptance Sampling"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(
      tags$style(
        HTML(
          '.content {background-color: white;}
          .alert-success, .bg-green, .callout.callout-success, .label-success, .modal-success .modal-body {
          background-color: #27B9A5!important;}
          .alert-danger, .alert-error, .bg-red, .callout.callout-danger, .label-danger, .modal-danger .modal-body {
          background-color: #A4343A!important;}'
        )
      )
    ),
    
    fluidRow(
      column(4, 
             wellPanel(
               sliderInput(
                 "p",
                 "Percent Conforming:",
                 min = 1,
                 max = 100,
                 value = 90
               ),
               sliderInput("N",
                           "Lot Size:",
                           500,
                           min = 10,
                           max = 5000,
                           value = 500,
                           step = 5),
               numericInput("n",
                            "Sample Size:",
                            25,
                            step = 1),
               numericInput("c",
                            "Acceptable Non-Conformances",
                            5,
                            step = 1),
               actionButton("run_simulation", # refresh the simulated sample
                            "Run Simulation")
             ),
             valueBoxOutput("pass_fail_box", 12),
             valueBoxOutput("NC_in_n", 12),
             valueBoxOutput("good_in_n", 12)
      ),
      column(8,
             fluidRow(
               column(6,
                      plotOutput("simulation_plot")
               ),
               column(6,
                      plotOutput("OC_plot")
               )
             )
      ),
      column(8,
             fluidRow(
               column(6,
                      plotOutput("AOQL_plot")
               ),
               column(6,
                      plotOutput("ATI_plot")
               )
             )
      ),
      column(12,
             p("Source code is available on ",
               a(href = "https://github.com/jameson-marriott/WCQI-2022-Acceptance-Sampling",
                 "GitHub."))
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  prob_NC <- reactive({if (1 - (input$p / 100)  > .5) {(1 - (input$p / 100))} else {(.5)}})
  side_length <- reactive({ceiling(sqrt(input$N))})
  
  simulation_data <- reactive({
    input$run_simulation # this makes the simulation plot dependent on the run button
    tibble(
      ID = 1:input$N,
      y = rep(1:side_length(), length.out = input$N),
      x = rep(1:side_length(), each = side_length())[1:input$N]
    ) %>%
      left_join(tibble(
        ID = sample(1:input$N, round(input$N * (
          1 - (input$p / 100)
        )), replace = FALSE),
        defect = "Non-Conforming"
      ),
      by = "ID") %>%
      left_join(tibble(
        ID = sample(1:input$N, input$n, replace = FALSE),
        sample = "Selected"
      ),
      by = "ID") %>%
      mutate(
        sample = case_when(is.na(sample) ~ "Unselected",
                           TRUE ~ sample),
        defect = case_when(is.na(defect) ~ "Conforming",
                           TRUE ~ defect)
      )
  })
  
  non_conformances <- reactive({
    simulation_data() %>%
      filter(sample == "Selected") %>%
      filter(defect == "Non-Conforming") %>%
      summarise(n()) %>%
      unlist()
  })
  
  pass <- reactive({non_conformances() <= input$c
  })
  
  output$simulation_plot <- renderPlot({
    
    simulation_data() %>%
      ggplot(aes(xmin = x, 
                 xmax = x + 1, 
                 ymin = y, 
                 ymax = y + 1, 
                 fill = defect)) +
      geom_rect(color = "white") +
      geom_rect(data = simulation_data() %>% filter(sample == "Selected"),
                aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1), 
                color = "black") +
      scale_fill_manual(values = c(# ASQ palette
        "#27B9A5",
        "#A4343A")) +
      coord_fixed(ratio = 1) +
      #ggtitle(paste0("Accept Lot: ", pass)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none"
      )
  })
  
  OC_test_plan <- reactive({
    OC2c(
      n = input$n,
      c = input$c,
      r = input$c + 1,
      type = "hypergeom",
      N = input$N,
      pd = seq(0, prob_NC(), .01)
    )
  })  
  
  plan_data <- reactive({
    tibble(pa = attr(OC_test_plan(), "paccept"),
           pd = attr(OC_test_plan(), "pd"))
  })
  
  AOQ_data <- reactive({
    tibble(p = seq(.01, prob_NC(), .005)) %>%
      mutate(
        Pa = pbinom(input$c, input$n, p, lower.tail = TRUE),
        AOQ = (Pa * p * (input$N - input$n)) / input$N,
        ATI = input$n + (1 - Pa) * (input$N - input$n)
      )
  })
  
  output$OC_plot <- renderPlot({
    ggplot(plan_data(), aes(x = pd, y = pa)) +
      geom_line() +
      #geom_point() +
      #geom_smooth(color = "blue", se = FALSE) +
      geom_vline(xintercept = (1 - (input$p / 100))) +
      ylab("Prob. of Acceptance") +
      xlab("Percent Defectives") +
      ggtitle("Operating Characteristic (OC) Curve")
  })
  
  output$AOQL_plot <- renderPlot({
    
    AOQL <- max(AOQ_data()$AOQ)
    
    ggplot(AOQ_data(), aes(x = p, y = AOQ)) +
      geom_line() +
      geom_hline(yintercept = AOQL) +
      geom_vline(xintercept = (1 - (input$p / 100))) +
      geom_label(aes(x = .025, y = AOQL, label = "AOQ Limit")) +
      ylab("AOQ") +
      xlab("Incoming Percent Non-Conforming") +
      ggtitle("Average Outgoing Quality Limit")
  })
  
  output$ATI_plot <- renderPlot({
    
    AOQ_data() %>%
      ggplot(aes(x = p, y = ATI)) +
      geom_line() +
      geom_vline(xintercept = (1 - (input$p / 100))) +
      ylab("ATI") +
      xlab("Incoming Percent Non-Conforming") +
      ggtitle("Average Total Inspection")
  })
  
  pass_color <- reactive({if (pass() == TRUE) return("green") else ("red")})
  
  pass_text <- reactive({if (pass() == TRUE) return ("Pass") else ("Fail")})
  
  output$pass_fail_box <- renderValueBox(
    valueBox(
      pass_text(),
      "Lot Acceptance",
      color = pass_color()
    )
  )
  
  output$NC_in_n <- renderValueBox(
    valueBox(
      paste0(non_conformances(), "/", input$n),
      "Non-conforming",
      color = "red"
    )
  )
  
  output$good_in_n <- renderValueBox(
    valueBox(
      paste0(input$n - non_conformances(), "/", input$n),
      "Conforming",
      color = "green"
    )
  )
}

# Run the application
shinyApp(ui = ui, server = server)
