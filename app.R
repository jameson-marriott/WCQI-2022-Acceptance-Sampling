# Notes for improvement
# only load AOQ data once 
# maybe use patchwork to combine plots?
# fix rounding errors on OC curve so that is is always smooth
# reduce the number of packages that the app loads
# make an option for a reference OC curve
# make additional tabs to specify OC curves different ways
# add a refresh button to the top graph

library(shiny)
library(AcceptanceSampling)
library(AQLSchemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Acceptance Sampling"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("p",
                        "Percent Conforming:",
                        min = 1,
                        max = 100,
                        value = 90),
            numericInput("N",
                         "Lot Size:",
                         500,
                         step = 1),
            numericInput("n",
                         "Sample Size:",
                         25,
                         step = 1),
            numericInput("c",
                         "Acceptable Non-Conformances",
                         5,
                         step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("simulation_plot"),
           plotOutput("OCcurve"),
           plotOutput("AOQLplot"),
           plotOutput("ATI")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
theme_set(theme_bw())
    output$simulation_plot <- renderPlot({
        side_length = ceiling(sqrt(input$N))
        
        simulation_data <- tibble(ID = 1:input$N,
                                  y = rep(1:side_length, length.out = input$N),
                                  x = rep(1:side_length, each = side_length)[1:input$N]) %>%
          left_join(tibble(ID = sample(1:input$N, round(input$N * (1 - (input$p / 100))), replace = FALSE),
                           defect = "Non-Conforming"),
                    by = "ID") %>%
          left_join(tibble(ID = sample(1:input$N, input$n, replace = FALSE),
                           sample = "Selected"),
                    by = "ID") %>%
          mutate(sample = case_when(is.na(sample) ~ "Unselected",
                                    TRUE ~ sample),
                 defect = case_when(is.na(defect) ~ "Conforming",
                                    TRUE ~ defect))
        
        pass <- simulation_data %>%
          filter(sample == "Selected") %>%
          filter(defect == "Non-Conforming") %>%
          summarise(n()) %>%
          unlist() <= input$c
        
        simulation_data %>%
          ggplot(aes(x = x, y = y, color = defect)) +
          geom_point(data = simulation_data %>% filter(sample == "Selected"),
                     aes(x = x, y = y), color = "black", fill = NA, size = 3) +
          geom_point() +
          scale_color_manual(
            values = c(
              # ASQ palette
              "#27B9A5",
              "#A4343A"
            )
          ) +
          coord_fixed(ratio = 1) +
          ggtitle(paste0("Accept Lot: ", pass), paste0(100 - input$p, "% defective")) +
          theme(axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.title = element_blank(),
                panel.grid = element_blank(),
                panel.border = element_blank())
    })
    
    output$OCcurve <- renderPlot({
      OC_test_plan <-
        OC2c(
          n = input$n,
          c = input$c,
          r = input$c + 1,
          type = "hypergeom",
          N = input$N,
          pd = seq(0, .25, .01)
        )
      
      plan_data <- tibble(pa = attr(OC_test_plan, "paccept"),
                          pd = attr(OC_test_plan, "pd"))
      
      ggplot(plan_data, aes(x = pd, y = pa)) +
        geom_line() +
        geom_vline(xintercept = (1 - (input$p / 100))) +
        ylab("Prob. of Acceptance") +
        xlab("Percent Defectives") +
        ggtitle("Operating Characteristic (OC) Curve")
    })
    
    output$AOQLplot <- renderPlot({
      AOQ_data <- tibble(p = seq(.01, .25, .005)) %>%
        mutate(Pa = pbinom(input$c, input$n, p, lower.tail = TRUE),
               AOQ = (Pa * p * (input$N - input$n)) / input$N,
               ATI = input$n + (1 - Pa) * (input$N - input$n))
      AOQL <- max(AOQ_data$AOQ)
      
      ggplot(AOQ_data, aes(x = p, y = AOQ)) +
        geom_line() +
        geom_hline(yintercept = AOQL) +
        geom_vline(xintercept = (1 - (input$p / 100))) +
        geom_label(aes(x = .025, y = AOQL, label = "AOQ Limit")) +
        ylab("AOQ") +
        xlab("Incoming Percent Non-Conforming") +
        ggtitle("Average Outgoing Quality Limit")
    })
    
    output$ATI <- renderPlot({
      #generating this table twice - need to make it available to AOQL and ATI plots
      AOQ_data <- tibble(p = seq(.01, .25, .005)) %>%
        mutate(Pa = pbinom(input$c, input$n, p, lower.tail = TRUE),
               AOQ = (Pa * p * (input$N - input$n)) / input$N,
               ATI = input$n + (1 - Pa) * (input$N - input$n))
      
      AOQ_data %>%
        ggplot(aes(x = p, y = ATI)) +
        geom_line() +
        geom_vline(xintercept = (1 - (input$p / 100))) +
        ylab("ATI") +
        xlab("Incoming Percent Non-Conforming") +
        ggtitle("Average Total Inspection")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
