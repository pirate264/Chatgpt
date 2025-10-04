library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)
library(scales)
library(viridis)

# Load data
df <- read.csv("C:/Users/gaura/Downloads/chatgpt.csv")
df <- df %>% select(-matches("Timestamp|Email"))

# UI
ui <- navbarPage(
  title = div(icon("brain"), "ChatGPT Analytics"),
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');
      
      * {
        font-family: 'Poppins', sans-serif;
      }
      
      .navbar-default {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        border: none;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: 600;
      }
      
      body {
        background: #f5f7fa;
      }
      
      .hero-section {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 40px 30px;
        border-radius: 15px;
        margin-bottom: 30px;
        box-shadow: 0 10px 30px rgba(102, 126, 234, 0.3);
        animation: fadeIn 0.6s ease-in;
      }
      
      .hero-section h1 {
        margin: 0;
        font-size: 2.8em;
        font-weight: 700;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      
      .hero-section p {
        margin: 15px 0 0 0;
        font-size: 1.2em;
        opacity: 0.95;
      }
      
      .metric-card {
        background: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
        border-left: 4px solid #667eea;
        animation: slideInUp 0.5s ease-out;
      }
      
      .metric-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      
      .metric-value {
        font-size: 2.5em;
        font-weight: 700;
        color: #667eea;
        margin: 10px 0;
      }
      
      .metric-label {
        font-size: 0.95em;
        color: #6c757d;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 600;
      }
      
      .metric-icon {
        font-size: 2.5em;
        color: #667eea;
        opacity: 0.2;
        position: absolute;
        right: 20px;
        top: 20px;
      }
      
      .control-panel {
        background: white;
        padding: 30px;
        border-radius: 12px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        animation: slideInLeft 0.6s ease-out;
      }
      
      .control-section {
        margin-bottom: 25px;
      }
      
      .control-label {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 10px;
        font-size: 0.95em;
      }
      
      .plot-card {
        background: white;
        padding: 30px;
        border-radius: 12px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        min-height: 650px;
        animation: fadeInUp 0.7s ease-out;
      }
      
      .nav-pills > li > a {
        border-radius: 8px;
        margin-right: 5px;
        font-weight: 600;
        transition: all 0.3s;
      }
      
      .nav-pills > li.active > a {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      }
      
      .btn-analyze {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        padding: 12px 30px;
        border-radius: 8px;
        font-weight: 600;
        font-size: 1em;
        width: 100%;
        transition: all 0.3s;
        box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
      }
      
      .btn-analyze:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
        background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
      }
      
      .btn-download {
        background: white;
        color: #667eea;
        border: 2px solid #667eea;
        padding: 10px 25px;
        border-radius: 8px;
        font-weight: 600;
        transition: all 0.3s;
        margin-top: 15px;
      }
      
      .btn-download:hover {
        background: #667eea;
        color: white;
      }
      
      .insight-box {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-top: 20px;
        box-shadow: 0 4px 15px rgba(240, 147, 251, 0.3);
      }
      
      .insight-box h4 {
        margin: 0 0 10px 0;
        font-weight: 600;
      }
      
      .filter-chip {
        display: inline-block;
        background: #e8eaf6;
        color: #667eea;
        padding: 5px 15px;
        border-radius: 20px;
        margin: 5px;
        font-size: 0.85em;
        font-weight: 600;
      }
      
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(255,255,255,0.9);
        z-index: 9999;
        display: none;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      
      @keyframes slideInUp {
        from {
          opacity: 0;
          transform: translateY(30px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      @keyframes slideInLeft {
        from {
          opacity: 0;
          transform: translateX(-30px);
        }
        to {
          opacity: 1;
          transform: translateX(0);
        }
      }
      
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .dataTables_wrapper {
        padding: 20px 0;
      }
      
      table.dataTable thead th {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white !important;
        font-weight: 600;
      }
      
      .progress-section {
        margin-top: 20px;
        padding: 15px;
        background: #f8f9fa;
        border-radius: 8px;
      }
    "))
  ),
  
  # Dashboard Tab
  tabPanel("Dashboard",
           icon = icon("dashboard"),
           
           fluidRow(
             column(12,
                    div(class = "hero-section",
                        h1(icon("chart-line"), " Survey Analytics Dashboard"),
                        p("Real-time insights and interactive visualizations of ChatGPT survey responses")
                    )
             )
           ),
           
           fluidRow(
             # Metrics
             column(3,
                    div(class = "metric-card",
                        icon("users", class = "metric-icon"),
                        div(class = "metric-label", "Total Responses"),
                        div(class = "metric-value", textOutput("totalResponses", inline = TRUE))
                    )
             ),
             column(3,
                    div(class = "metric-card",
                        icon("chart-bar", class = "metric-icon"),
                        div(class = "metric-label", "Variables Tracked"),
                        div(class = "metric-value", textOutput("totalVariables", inline = TRUE))
                    )
             ),
             column(3,
                    div(class = "metric-card",
                        icon("percentage", class = "metric-icon"),
                        div(class = "metric-label", "Completion Rate"),
                        div(class = "metric-value", textOutput("completionRate", inline = TRUE))
                    )
             ),
             column(3,
                    div(class = "metric-card",
                        icon("clock", class = "metric-icon"),
                        div(class = "metric-label", "Last Updated"),
                        div(class = "metric-value", style = "font-size: 1.3em;", 
                            textOutput("lastUpdated", inline = TRUE))
                    )
             )
           ),
           
           fluidRow(
             # Control Panel
             column(4,
                    div(class = "control-panel",
                        h3(icon("sliders-h"), " Visualization Controls"),
                        hr(),
                        
                        div(class = "control-section",
                            div(class = "control-label", "Primary Variable"),
                            pickerInput(
                              "variable",
                              NULL,
                              choices = colnames(df),
                              options = list(
                                `live-search` = TRUE,
                                `style` = "btn-outline-primary"
                              )
                            )
                        ),
                        
                        div(class = "control-section",
                            div(class = "control-label", "Visualization Type"),
                            radioGroupButtons(
                              "plotType",
                              NULL,
                              choices = c(
                                `<i class='fa fa-bar-chart'></i> Bar` = "Histogram",
                                `<i class='fa fa-pie-chart'></i> Pie` = "Pie Chart",
                                `<i class='fa fa-box'></i> Box` = "Boxplot",
                                `<i class='fa fa-th'></i> Heat` = "Heatmap"
                              ),
                              justified = TRUE,
                              status = "primary"
                            )
                        ),
                        
                        uiOutput("secondVarUI"),
                        
                        div(class = "control-section",
                            div(class = "control-label", "Color Scheme"),
                            pickerInput(
                              "colorScheme",
                              NULL,
                              choices = c("Viridis", "Magma", "Plasma", "Inferno", "Cividis"),
                              selected = "Viridis"
                            )
                        ),
                        
                        actionButton("applyBtn", 
                                     HTML("<i class='fa fa-play'></i> Generate Visualization"),
                                     class = "btn-analyze"),
                        
                        downloadButton("downloadPlot", "Download Plot", 
                                       class = "btn-download")
                    ),
                    
                    # Quick Insights
                    div(class = "insight-box",
                        h4(icon("lightbulb"), " Quick Insights"),
                        uiOutput("quickInsights")
                    )
             ),
             
             # Visualization Panel
             column(8,
                    div(class = "plot-card",
                        tabsetPanel(
                          id = "mainTabs",
                          type = "pills",
                          
                          tabPanel("Visualization",
                                   icon = icon("chart-area"),
                                   br(),
                                   plotlyOutput("plot", height = "500px"),
                                   br(),
                                   uiOutput("plotStats")
                          ),
                          
                          tabPanel("Statistics",
                                   icon = icon("calculator"),
                                   br(),
                                   verbatimTextOutput("summary"),
                                   plotlyOutput("distributionPlot", height = "300px")
                          ),
                          
                          tabPanel("Correlations",
                                   icon = icon("project-diagram"),
                                   br(),
                                   plotlyOutput("correlationPlot", height = "500px")
                          )
                        )
                    )
             )
           )
  ),
  
  # Data Explorer Tab
  tabPanel("Data Explorer",
           icon = icon("table"),
           br(),
           fluidRow(
             column(12,
                    div(class = "plot-card",
                        h3(icon("database"), " Complete Dataset"),
                        hr(),
                        DTOutput("dataTable")
                    )
             )
           )
  ),
  
  # Insights Tab
  tabPanel("AI Insights",
           icon = icon("brain"),
           br(),
           fluidRow(
             column(12,
                    div(class = "plot-card",
                        h3(icon("chart-line"), " Automated Analysis"),
                        hr(),
                        uiOutput("aiInsights")
                    )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Metrics
  output$totalResponses <- renderText({
    format(nrow(df), big.mark = ",")
  })
  
  output$totalVariables <- renderText({
    format(ncol(df), big.mark = ",")
  })
  
  output$completionRate <- renderText({
    complete <- sum(complete.cases(df))
    paste0(round(complete/nrow(df) * 100, 1), "%")
  })
  
  output$lastUpdated <- renderText({
    format(Sys.Date(), "%b %d")
  })
  
  # Second variable UI
  output$secondVarUI <- renderUI({
    if(input$plotType == "Heatmap") {
      div(class = "control-section",
          div(class = "control-label", "Secondary Variable"),
          pickerInput(
            "variable2",
            NULL,
            choices = colnames(df),
            selected = colnames(df)[min(2, length(colnames(df)))],
            options = list(`live-search` = TRUE, `style` = "btn-outline-primary")
          )
      )
    }
  })
  
  # Reactive plot data
  plotData <- eventReactive(input$applyBtn, {
    list(
      variable = input$variable,
      plotType = input$plotType,
      variable2 = input$variable2,
      colorScheme = tolower(input$colorScheme)
    )
  }, ignoreNULL = FALSE)
  
  # Main plot
  output$plot <- renderPlotly({
    data <- df
    params <- plotData()
    colors <- switch(params$colorScheme,
                     "viridis" = viridis(10),
                     "magma" = magma(10),
                     "plasma" = plasma(10),
                     "inferno" = inferno(10),
                     "cividis" = cividis(10))
    
    if(params$plotType == "Histogram") {
      p <- ggplot(data, aes_string(x = params$variable)) +
        geom_bar(fill = colors[5], alpha = 0.8, color = colors[8]) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", size = 18),
          panel.grid.major = element_line(color = "#e0e0e0"),
          panel.grid.minor = element_blank()
        ) +
        labs(title = paste("Distribution of", params$variable), 
             y = "Count", x = params$variable)
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hoverlabel = list(bgcolor = "white"))
    }
    
    else if(params$plotType == "Pie Chart") {
      df_count <- data %>% count(!!sym(params$variable))
      plot_ly(df_count, 
              labels = ~get(params$variable), 
              values = ~n, 
              type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              marker = list(colors = colors,
                            line = list(color = 'white', width = 2)),
              hole = 0.4) %>%
        layout(title = list(text = paste("Distribution of", params$variable),
                            font = list(size = 18, family = "Poppins")),
               showlegend = TRUE,
               legend = list(orientation = "v"))
    }
    
    else if(params$plotType == "Boxplot") {
      if(is.numeric(data[[params$variable]])) {
        p <- ggplot(data, aes_string(y = params$variable)) +
          geom_boxplot(fill = colors[5], alpha = 0.6, outlier.color = colors[8], 
                       color = colors[8], outlier.size = 3) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold", size = 18)) +
          labs(title = paste("Distribution of", params$variable))
        ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
      } else {
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        if(length(numeric_cols) > 0) {
          p <- ggplot(data, aes_string(x = params$variable, y = numeric_cols[1])) +
            geom_boxplot(fill = colors[5], alpha = 0.6, outlier.color = colors[8]) +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(face = "bold", size = 18)) +
            labs(title = paste(numeric_cols[1], "by", params$variable),
                 x = params$variable, y = numeric_cols[1])
          ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
        }
      }
    }
    
    else if(params$plotType == "Heatmap") {
      req(params$variable2)
      df_heat <- table(data[[params$variable]], data[[params$variable2]])
      df_melt <- melt(df_heat)
      
      plot_ly(x = df_melt$Var2, y = df_melt$Var1, z = df_melt$value,
              type = "heatmap",
              colorscale = list(c(0, "white"), c(1, colors[8])),
              text = df_melt$value,
              hovertemplate = paste('<b>%{y}</b><br>',
                                    '%{x}<br>',
                                    'Count: %{z}<extra></extra>')) %>%
        layout(title = list(text = paste("Relationship:", params$variable, "vs", params$variable2),
                            font = list(size = 18)),
               xaxis = list(title = params$variable2),
               yaxis = list(title = params$variable))
    }
  })
  
  # Quick insights
  output$quickInsights <- renderUI({
    var_data <- df[[input$variable]]
    
    if(is.numeric(var_data)) {
      HTML(paste0(
        "<p><strong>Mean:</strong> ", round(mean(var_data, na.rm = TRUE), 2), "</p>",
        "<p><strong>Median:</strong> ", round(median(var_data, na.rm = TRUE), 2), "</p>",
        "<p><strong>Std Dev:</strong> ", round(sd(var_data, na.rm = TRUE), 2), "</p>"
      ))
    } else {
      top_val <- names(sort(table(var_data), decreasing = TRUE))[1]
      top_count <- max(table(var_data))
      HTML(paste0(
        "<p><strong>Most Common:</strong> ", top_val, "</p>",
        "<p><strong>Frequency:</strong> ", top_count, " (", 
        round(top_count/length(var_data)*100, 1), "%)</p>",
        "<p><strong>Unique Values:</strong> ", length(unique(var_data)), "</p>"
      ))
    }
  })
  
  # Summary statistics
  output$summary <- renderPrint({
    var_data <- df[[input$variable]]
    
    if(is.numeric(var_data)) {
      cat("=== NUMERICAL SUMMARY ===\n\n")
      print(summary(var_data))
      cat("\n\nAdditional Statistics:\n")
      cat("Standard Deviation:", round(sd(var_data, na.rm = TRUE), 4), "\n")
      cat("Variance:", round(var(var_data, na.rm = TRUE), 4), "\n")
      cat("IQR:", round(IQR(var_data, na.rm = TRUE), 4), "\n")
      cat("Missing Values:", sum(is.na(var_data)), "\n")
    } else {
      cat("=== CATEGORICAL SUMMARY ===\n\n")
      freq_table <- table(var_data, useNA = "ifany")
      freq_df <- data.frame(
        Category = names(freq_table),
        Count = as.numeric(freq_table),
        Percentage = round(as.numeric(freq_table) / sum(freq_table) * 100, 2)
      )
      print(freq_df)
      cat("\n\nTotal Categories:", length(unique(var_data)), "\n")
      cat("Most Common:", names(which.max(freq_table)), "\n")
      cat("Missing Values:", sum(is.na(var_data)), "\n")
    }
  })
  
  # Distribution plot for numeric variables
  output$distributionPlot <- renderPlotly({
    var_data <- df[[input$variable]]
    
    if(is.numeric(var_data)) {
      # Remove NA values for plotting
      var_data_clean <- var_data[!is.na(var_data)]
      
      if(length(var_data_clean) > 0) {
        p <- ggplot(data.frame(x = var_data_clean), aes(x = x)) +
          geom_density(fill = viridis(5)[3], alpha = 0.6, color = viridis(5)[5], size = 1.2) +
          geom_rug(alpha = 0.3, color = viridis(5)[5]) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(face = "bold", size = 14),
            panel.grid.minor = element_blank()
          ) +
          labs(title = "Density Distribution", x = input$variable, y = "Density")
        ggplotly(p) %>%
          layout(hoverlabel = list(bgcolor = "white"))
      }
    } else {
      # For categorical variables, show a frequency bar chart
      freq_table <- as.data.frame(table(var_data))
      names(freq_table) <- c("Category", "Frequency")
      
      plot_ly(freq_table, x = ~Category, y = ~Frequency, type = 'bar',
              marker = list(color = viridis(nrow(freq_table))),
              text = ~Frequency,
              textposition = 'auto',
              hovertemplate = '<b>%{x}</b><br>Count: %{y}<extra></extra>') %>%
        layout(
          title = "Frequency Distribution",
          xaxis = list(title = input$variable),
          yaxis = list(title = "Count"),
          showlegend = FALSE
        )
    }
  })
  
  # Correlation plot
  output$correlationPlot <- renderPlotly({
    # Get numeric columns
    numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
    
    # If no numeric columns, try to convert categorical to numeric
    if(ncol(numeric_df) < 2) {
      # Try to create numeric representations of categorical variables
      converted_df <- df
      for(col in names(converted_df)) {
        if(is.character(converted_df[[col]]) || is.factor(converted_df[[col]])) {
          # Convert to factor then to numeric
          converted_df[[col]] <- as.numeric(as.factor(converted_df[[col]]))
        }
      }
      numeric_df <- converted_df[, sapply(converted_df, is.numeric), drop = FALSE]
    }
    
    # Check if we have enough numeric variables
    if(ncol(numeric_df) >= 2) {
      # Remove columns with zero variance or all NA
      numeric_df <- numeric_df[, sapply(numeric_df, function(x) {
        length(unique(na.omit(x))) > 1 && sum(!is.na(x)) > 1
      }), drop = FALSE]
      
      if(ncol(numeric_df) >= 2) {
        # Calculate correlation matrix
        cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
        
        # Check if correlation matrix is valid
        if(!any(is.na(cor_matrix)) || sum(!is.na(cor_matrix)) > 0) {
          cor_melt <- melt(cor_matrix)
          
          # Create interactive heatmap
          plot_ly(x = cor_melt$Var2, y = cor_melt$Var1, z = cor_melt$value,
                  type = "heatmap",
                  colorscale = list(
                    c(0, "#053061"), c(0.25, "#2166ac"), c(0.5, "#f7f7f7"),
                    c(0.75, "#b2182b"), c(1, "#67001f")
                  ),
                  zmid = 0,
                  zmin = -1,
                  zmax = 1,
                  text = round(cor_melt$value, 3),
                  hovertemplate = '<b>%{y}</b> vs <b>%{x}</b><br>Correlation: %{text}<extra></extra>',
                  colorbar = list(title = "Correlation")) %>%
            layout(
              title = list(text = "Correlation Matrix", 
                           font = list(size = 16, family = "Poppins")),
              xaxis = list(title = "", tickangle = -45, tickfont = list(size = 10)),
              yaxis = list(title = "", tickfont = list(size = 10)),
              margin = list(l = 150, b = 150, r = 50, t = 80)
            )
        } else {
          # Correlation matrix has issues
          plot_ly() %>%
            layout(
              title = list(text = "Correlation Analysis", font = list(size = 16)),
              annotations = list(
                x = 0.5, y = 0.5,
                text = paste0("<b>Unable to calculate correlations</b><br><br>",
                              "The data may have insufficient variation or too many missing values."),
                showarrow = FALSE,
                font = list(size = 14, color = "#666"),
                xref = "paper", yref = "paper"
              ),
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        }
      } else {
        # After filtering, not enough variables
        plot_ly() %>%
          layout(
            title = list(text = "Correlation Analysis", font = list(size = 16)),
            annotations = list(
              x = 0.5, y = 0.5,
              text = paste0("<b>Insufficient data for correlation analysis</b><br><br>",
                            "Variables need sufficient variation to calculate correlations."),
              showarrow = FALSE,
              font = list(size = 14, color = "#666"),
              xref = "paper", yref = "paper"
            ),
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          )
      }
    } else {
      # No numeric variables even after conversion
      plot_ly() %>%
        layout(
          title = list(text = "Correlation Analysis", font = list(size = 16)),
          annotations = list(
            x = 0.5, y = 0.5,
            text = paste0("<b>Converting categorical variables to numeric</b><br><br>",
                          "Categorical variables have been encoded numerically.<br>",
                          "However, insufficient variables available for correlation analysis.<br><br>",
                          "<i>Tip: Ensure your dataset has at least 2 variables with variation.</i>"),
            showarrow = FALSE,
            font = list(size = 14, color = "#666"),
            xref = "paper", yref = "paper"
          ),
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
    }
  })
  
  # Data table
  output$dataTable <- renderDT({
    datatable(df,
              extensions = 'Buttons',
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend = 'copy', className = 'btn-primary'),
                  list(extend = 'csv', className = 'btn-primary'),
                  list(extend = 'excel', className = 'btn-primary'),
                  list(extend = 'pdf', className = 'btn-primary')
                ),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#667eea', 'color': '#fff'});",
                  "}"
                )
              ),
              class = 'stripe hover row-border')
  })
  
  # AI Insights
  output$aiInsights <- renderUI({
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    categorical_vars <- names(df)[!sapply(df, is.numeric)]
    
    HTML(paste0(
      "<div class='row'>",
      "<div class='col-md-6'>",
      "<div class='metric-card'>",
      "<h4><i class='fa fa-chart-bar'></i> Dataset Overview</h4>",
      "<p><strong>Numeric Variables:</strong> ", length(numeric_vars), "</p>",
      "<p><strong>Categorical Variables:</strong> ", length(categorical_vars), "</p>",
      "<p><strong>Missing Data:</strong> ", round(sum(is.na(df))/(nrow(df)*ncol(df))*100, 2), "%</p>",
      "</div></div>",
      "<div class='col-md-6'>",
      "<div class='metric-card'>",
      "<h4><i class='fa fa-lightbulb'></i> Key Findings</h4>",
      "<p>• Dataset contains ", nrow(df), " complete survey responses</p>",
      "<p>• ", ncol(df), " variables tracked across all responses</p>",
      "<p>• Data quality score: ", round(sum(complete.cases(df))/nrow(df)*100, 1), "%</p>",
      "</div></div>",
      "</div>"
    ))
  })
  
  # Download handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 12, height = 8, dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)