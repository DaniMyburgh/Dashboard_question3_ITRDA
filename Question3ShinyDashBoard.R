library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

# Here is where I have loaded the code 
grad_survey_cleaned <-read.csv("/Users/Dani-myburgh/Downloads/graduate_survey_dashboard.csv")
 

# Here is where I process and prepare the data for visualization
prepare_top_tools <- function(df, column) {
  df %>%
    separate_rows(!!sym(column), sep = ";") %>%
    count(!!sym(column), sort = TRUE) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    slice_head(n = 20)
}

# i created the datasets for visualization
Prog_lang_count <- prepare_top_tools(grad_survey_cleaned, "ProgLang")
Databases_count <- prepare_top_tools(grad_survey_cleaned, "Databases")
AISearch_count <- prepare_top_tools(grad_survey_cleaned, "AISearch")
AITool_count <- prepare_top_tools(grad_survey_cleaned, "AITool")
WebFramework_count <- prepare_top_tools(grad_survey_cleaned, "WebFramework")
Platform_count <- prepare_top_tools(grad_survey_cleaned, "Platform")

# Here i have defined the user interface
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Graudates top tools Dashboard", titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Programming Languages", tabName = "prog_lang"),
      menuItem("Databases", tabName = "databases"),
      menuItem("AI Search Tools", tabName = "aisearch"),
      menuItem("AI Tools", tabName = "aitools"),
      menuItem("Web Frameworks", tabName = "webframeworks"),
      menuItem("Platforms", tabName = "platforms")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "prog_lang",
              fluidRow(
                box(title = "Top Programming Languages", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("plot_prog_lang", height = "500px"))
              )
      ),
      tabItem(tabName = "databases",
              fluidRow(
                box(title = "The top databases", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("plot_databases", height = "500px"))
              )
      ),
      tabItem(tabName = "aisearch",
              fluidRow(
                box(title = "The top AI Search Tools", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("plot_aisearch", height = "500px"))
              )
      ),
      tabItem(tabName = "aitools",
              fluidRow(
                box(title = "The top AI Tools", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("plot_aitools", height = "500px"))
              )
      ),
      tabItem(tabName = "webframeworks",
              fluidRow(
                box(title = "The top Web Frameworks", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("plot_webframeworks", height = "500px"))
              )
      ),
      tabItem(tabName = "platforms",
              fluidRow(
                box(title = "The top Platforms", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("plot_platforms", height = "500px"))
              )
      )
    )
  )
)

# here i am just defining the server logic and also added a function to make bar plots with percentages
server <- function(input, output) {
  create_bar_plot <- function(data, x_var, fill_color, title, x_label) {
    ggplot(data, aes(x = reorder(!!sym(x_var), n), y = n, label = paste0(round(percentage, 1), "%"))) +
      geom_bar(stat = "identity", fill = fill_color, alpha = 0.8) +
      geom_text(hjust = -0.1) +
      coord_flip() +
      labs(title = title, x = x_label, y = "Count") +
      theme_minimal()
  }
  
  output$plot_prog_lang <- renderPlot({
    create_bar_plot(Prog_lang_count, "ProgLang", "blue", "Top Programming Languages", "Language")
  })
  
  output$plot_databases <- renderPlot({
    create_bar_plot(Databases_count, "Databases", "green", "Top Databases", "Database")
  })
  
  output$plot_aisearch <- renderPlot({
    create_bar_plot(AISearch_count, "AISearch", "purple", "Top AI Search Tools", "AI Search Tool")
  })
  
  output$plot_aitools <- renderPlot({
    create_bar_plot(AITool_count, "AITool", "pink", "Top AI Tools", "AI Tool")
  })
  
  output$plot_webframeworks <- renderPlot({
    create_bar_plot(WebFramework_count, "WebFramework", "red", "Top Web Frameworks", "Web Framework")
  })
  
  output$plot_platforms <- renderPlot({
    create_bar_plot(Platform_count, "Platform", "orange", "Top Platforms", "Platform")
  })
}

# here is the function to launch the shiny application
shinyApp(ui = ui, server = server)