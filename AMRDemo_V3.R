library(shiny)
library(tidyverse)
library(readr)
library(janitor)
library(DT)

# Load synthetic data at startup
synthetic_data <- read_csv("/Users/tavpriteshsethi/Documents/ESCMID_Workshop_2025/synthetic_data_2_10k_wide.csv", show_col_types = FALSE) %>% clean_names()

ui <- fluidPage(
  titlePanel("Data Examination, Preprocessing & Transformation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("wideFile", "Upload Your Own CSV:", accept = ".csv"),
      uiOutput("columnSelectUI"),
      sliderInput("topN", "Top N Categories for Bar Plot:", min = 1, max = 50, value = 15),
      hr(),
      h4("Identifier Columns (manual selection always available):"),
      uiOutput("idColsUI"),
      actionButton("guessBtn", "Guess My Identifiers"),
      actionButton("convertBtn", "Toggle Wide ↔ Long"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Source", verbatimTextOutput("dataSource")),
        tabPanel("Column Overview", DTOutput("colInfo")),
        tabPanel("Current Data Preview", DTOutput("dataPreview")),
        tabPanel("Unique Values", DTOutput("uniqueValues")),
        tabPanel("Bar Plot", plotOutput("barPlot")),
        tabPanel("Numeric Plots", plotOutput("histPlot"),
                 plotOutput("boxPlot"), plotOutput("densityPlot")),
        tabPanel("Guessed Identifiers", DTOutput("guessedIds")),
        tabPanel("Conversion Status", verbatimTextOutput("conversionStatus"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveValues(
    current = synthetic_data,
    wide = synthetic_data,
    long = NULL,
    mode = "wide",
    source = "✅ Preloaded synthetic dataset (10k rows)"
  )
  
  # Handle file upload
  observeEvent(input$wideFile, {
    req(input$wideFile)
    df <- read_csv(input$wideFile$datapath, show_col_types = FALSE) %>% clean_names()
    data$wide <- df
    data$current <- df
    data$long <- NULL
    data$mode <- "wide"
    data$source <- paste0("✅ User-uploaded file: ", input$wideFile$name)
    
    output$idColsUI <- renderUI({
      checkboxGroupInput("idCols", "Select Identifier Columns:", choices = names(df))
    })
    
    output$columnSelectUI <- renderUI({
      selectInput("examineColumn", "Select Column:", choices = names(df))
    })
    
    output$dataPreview <- renderDT({
      datatable(head(data$current, 10))
    })
  })
  
  # Show current data source
  output$dataSource <- renderText({ data$source })
  
  # Initial widgets for synthetic data
  output$idColsUI <- renderUI({
    checkboxGroupInput("idCols", "Select Identifier Columns:", choices = names(synthetic_data))
  })
  output$columnSelectUI <- renderUI({
    selectInput("examineColumn", "Select Column:", choices = names(synthetic_data))
  })
  output$colInfo <- renderDT({
    tibble(
      Column = names(data$current),
      Type = sapply(data$current, function(x) class(x)[1]),
      Missing = sapply(data$current, function(x) sum(is.na(x))),
      Missing_Pct = round(sapply(data$current, function(x) mean(is.na(x))) * 100, 2)
    ) %>% datatable()
  })
  output$dataPreview <- renderDT({
    datatable(head(data$current, 10))
  })
  
  # Identifier guessing
  observeEvent(input$guessBtn, {
    df <- data$wide
    n_rows <- nrow(df)
    id_guesses <- tibble(
      column = names(df),
      unique_vals = sapply(df, n_distinct),
      type = sapply(df, function(x) class(x)[1])
    ) %>%
      mutate(
        pct_unique = unique_vals / n_rows * 100,
        likely_identifier = case_when(
          str_detect(column, "id|sample|patient|date") ~ TRUE,
          pct_unique == 100 & type %in% c("character", "factor", "numeric", "integer") ~ TRUE,
          type %in% c("character", "factor") & unique_vals < (0.05 * n_rows) ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>%
      filter(likely_identifier)
    
    output$guessedIds <- renderDT({
      datatable(id_guesses %>% select(column, unique_vals, pct_unique, type))
    })
    
    if (nrow(id_guesses) == 0) {
      showModal(modalDialog(
        title = "No Identifiers Found",
        "No clear identifier columns were guessed. Please manually select at least one identifier column for proper pivoting.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      updateCheckboxGroupInput(session, "idCols", selected = id_guesses$column)
    }
  })
  
  # Toggle Wide ↔ Long
  observeEvent(input$convertBtn, {
    req(data$current)
    
    id_cols <- if (length(input$idCols) == 0) names(data$current)[1] else input$idCols
    pivot_cols <- setdiff(names(data$current), id_cols)
    
    if (data$mode == "wide") {
      tryCatch({
        df_pivot <- data$current
        df_pivot <- df_pivot %>% mutate(across(all_of(pivot_cols), as.character))
        
        data$long <- df_pivot %>%
          pivot_longer(
            cols = all_of(pivot_cols),
            names_to = "antibiotic",
            values_to = "category"
          )
        data$current <- data$long
        data$mode <- "long"
        
        output$dataPreview <- renderDT({
          datatable(head(data$current, 10))
        })
        output$conversionStatus <- renderText("✅ Converted to LONG format.")
      }, error = function(e) {
        output$conversionStatus <- renderText(paste("❌ Conversion error:", e$message))
      })
    } else if (data$mode == "long") {
      tryCatch({
        data$current <- data$current %>%
          pivot_wider(
            names_from = antibiotic,
            values_from = category,
            values_fn = list(category = ~ paste(unique(.), collapse = "; "))
          )
        data$mode <- "wide"
        
        output$dataPreview <- renderDT({
          datatable(head(data$current, 10))
        })
        output$conversionStatus <- renderText("✅ Converted back to WIDE format.")
      }, error = function(e) {
        output$conversionStatus <- renderText(paste("❌ Conversion error:", e$message))
      })
    }
  })
  
  output$uniqueValues <- renderDT({
    req(data$current, input$examineColumn)
    unique_vals <- unique(data$current[[input$examineColumn]])
    datatable(data.frame(Unique_Values = unique_vals))
  })
  
  output$barPlot <- renderPlot({
    req(data$current, input$examineColumn)
    col <- data$current[[input$examineColumn]]
    if (!is.numeric(col)) {
      data$current %>%
        count(.data[[input$examineColumn]]) %>%
        slice_max(n, n = input$topN) %>%
        ggplot(aes(x = reorder(.data[[input$examineColumn]], -n), y = n)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = paste("Top", input$topN, input$examineColumn),
             x = input$examineColumn, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$histPlot <- renderPlot({
    req(data$current, input$examineColumn)
    col <- data$current[[input$examineColumn]]
    if (is.numeric(col)) {
      ggplot(data$current, aes(x = .data[[input$examineColumn]])) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = paste("Histogram of", input$examineColumn))
    }
  })
  
  output$boxPlot <- renderPlot({
    req(data$current, input$examineColumn)
    col <- data$current[[input$examineColumn]]
    if (is.numeric(col)) {
      ggplot(data$current, aes(y = .data[[input$examineColumn]])) +
        geom_boxplot(fill = "tomato") +
        theme_minimal() +
        labs(title = paste("Boxplot of", input$examineColumn))
    }
  })
  
  output$densityPlot <- renderPlot({
    req(data$current, input$examineColumn)
    col <- data$current[[input$examineColumn]]
    if (is.numeric(col)) {
      ggplot(data$current, aes(x = .data[[input$examineColumn]])) +
        geom_density(fill = "lightgreen", alpha = 0.6) +
        theme_minimal() +
        labs(title = paste("Density Plot of", input$examineColumn))
    }
  })
}

shinyApp(ui, server)
