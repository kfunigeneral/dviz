library(shiny)
library(ggplot2)
library(DT)
library(tidyr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)

rsconnect::setAccountInfo(name='kfunigen', token='0D9B2F413DC0D340F64A3D405958B582', secret='W4X6acFQZRSpXHaUC0YZNQvHJOC3DAbhmHg9xMzw')

# Load and prepare data
cards <- read.csv("dataset.csv", stringsAsFactors = FALSE)

# Clean up column names and data
colnames(cards) <- gsub("X\\.\\.", "", colnames(cards))
cards$Picked <- as.numeric(gsub(",", "", cards$Picked))
cards$Seen <- as.numeric(gsub(",", "", cards$Seen))

colors <- unique(cards$Color)
rarities <- unique(cards$Rarity)

# we use shinydashboard for better layout
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$div("⚔️", style = "font-size: 24px; margin-right: 10px;"),
      "MTGInspect"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Card Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("Performance Analysis", tabName = "performance", icon = icon("chart-line")),
      menuItem("Comparative View", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    # Include Tailwind and custom CSS
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"),
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f8fafc; }
        .main-header .logo { font-weight: bold; color: #4f46e5 !important; }
        .skin-blue .main-header .navbar { background-color: #4f46e5; }
        .skin-blue .main-header .logo { background-color: #4338ca; }
        .box { box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1); }
        .selected-card { background-color: #e0e7ff; border-left: 4px solid #4f46e5; }
      "))
    ),
    
    tabItems(
      # Card Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "🔍 Card Filters", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("color", "Color", choices = c("All", colors), selected = "All"),
                  checkboxGroupInput("rarity", "Rarity", choices = rarities, selected = rarities),
                  sliderInput("alsa", "ALSA Range", min = min(cards$ALSA, na.rm = TRUE), 
                              max = max(cards$ALSA, na.rm = TRUE), 
                              value = c(min(cards$ALSA, na.rm = TRUE), max(cards$ALSA, na.rm = TRUE))),
                  sliderInput("pick_range", "Pick Count Range", 
                              min = 0, max = max(cards$Picked, na.rm = TRUE),
                              value = c(0, max(cards$Picked, na.rm = TRUE))),
                  actionButton("reset_filters", "Reset Filters", class = "btn-warning")
                ),
                
                box(
                  title = "📊 Card Statistics", status = "info", solidHeader = TRUE, width = 9,
                  valueBoxOutput("total_cards", width = 3),
                  valueBoxOutput("avg_winrate", width = 3),
                  valueBoxOutput("top_card", width = 3),
                  valueBoxOutput("avg_alsa", width = 3)
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Interactive Card Table", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("card_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 ALSA vs Win Rate (Click to Select)", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("alsa_scatter")
                ),
                box(
                  title = "🎯 Selected Card Details", status = "warning", solidHeader = TRUE, width = 6,
                  uiOutput("selected_card_info")
                )
              )
      ),
      
      # Performance Analysis Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "⚙️ Analysis Controls", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("analysis_metric", "Primary Metric", 
                              choices = c("GP.WR" = "GP.WR", "OH.WR" = "OH.WR", 
                                          "GD.WR" = "GD.WR", "GIH.WR" = "GIH.WR")),
                  numericInput("top_n", "Top N Cards", value = 15, min = 5, max = 50),
                  radioButtons("chart_type", "Chart Type",
                               choices = c("Bar Chart" = "bar", "Lollipop" = "lollipop")),
                  checkboxInput("show_rarity", "Color by Rarity", value = TRUE)
                ),
                
                box(
                  title = "🏆 Top Performers", status = "success", solidHeader = TRUE, width = 9,
                  plotlyOutput("performance_chart")
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Win Rate Distribution", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("winrate_histogram")
                ),
                box(
                  title = "🎲 Rarity Performance", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("rarity_boxplot")
                )
              )
      ),
      
      # Comparative View Tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "🔄 Comparison Controls", status = "primary", solidHeader = TRUE, width = 3,
                  selectizeInput("compare_cards", "Select Cards to Compare",
                                 choices = NULL, multiple = TRUE, options = list(maxItems = 6)),
                  radioButtons("comparison_view", "View Type",
                               choices = c("Win Rate Breakdown" = "winrate", 
                                           "Performance Radar" = "radar",
                                           "Pick vs Performance" = "scatter")),
                  actionButton("add_random", "Add Random Card", class = "btn-info")
                ),
                
                box(
                  title = "⚖️ Card Comparison", status = "success", solidHeader = TRUE, width = 9,
                  plotlyOutput("comparison_chart")
                )
              ),
              
              fluidRow(
                box(
                  title = "🌈 Color Performance Matrix", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("color_heatmap")
                ),
                box(
                  title = "📈 ALSA Trends", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("alsa_trends")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About MTGInspect", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "p-4",
                      h3("Purpose", class = "text-xl font-bold mb-3"),
                      p("MTGInspect is an interactive dashboard for analyzing Magic: The Gathering card performance in draft formats. It provides comprehensive insights into card power levels, pick rates, and win rates across different game contexts."),
                      
                      h3("Features", class = "text-xl font-bold mb-3 mt-4"),
                      tags$ul(
                        tags$li("Interactive card filtering and exploration"),
                        tags$li("Dynamic performance visualizations"),
                        tags$li("Card-to-card comparison tools"),
                        tags$li("Click-to-select chart interactions"),
                        tags$li("Sortable and searchable data tables"),
                        tags$li("Color and rarity-based analysis")
                      ),
                      
                      h3("Metrics Explained", class = "text-xl font-bold mb-3 mt-4"),
                      tags$ul(
                        tags$li("ALSA (Average Last Seen At): Indicates card power level based on draft pick position"),
                        tags$li("GP.WR (Games Played Win Rate): Win rate in games where the card was played"),
                        tags$li("OH.WR (Opening Hand Win Rate): Win rate when the card was in the opening hand"),
                        tags$li("GD.WR (Games Drawn Win Rate): Win rate when the card was drawn"),
                        tags$li("GIH.WR (Games in Hand Win Rate): Win rate when the card was held in hand"),
                        tags$li("GNS.WR (Games Not Seen Win Rate): Win rate in games where the card was not seen")
                      ),
                      
                      h3("How to Use", class = "text-xl font-bold mb-3 mt-4"),
                      tags$ol(
                        tags$li("Use the Card Explorer to filter and examine individual cards"),
                        tags$li("Click on scatter plot points to view detailed card data"),
                        tags$li("Analyze top-performing cards in the Performance Analysis tab"),
                        tags$li("Compare cards directly using the Comparative View"),
                        tags$li("Hover over charts to view detailed tooltips")
                      ),
                      
                      div(class = "mt-6 text-center",
                          p("Created by Korvin Felcser", class = "text-gray-600 italic")
                      )
                  )
                )
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  # Update card choices for comparison
  observe({
    updateSelectizeInput(session, "compare_cards",
                         choices = cards$Name,
                         server = TRUE)
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- cards
    if (input$color != "All") df <- df[df$Color == input$color, ]
    df <- df[df$Rarity %in% input$rarity, ]
    df <- df[df$ALSA >= input$alsa[1] & df$ALSA <= input$alsa[2], ]
    df <- df[!is.na(df$Picked) & df$Picked >= input$pick_range[1] & df$Picked <= input$pick_range[2], ]
    df
  })
  
  # Selected card reactive value
  selected_card <- reactiveVal(NULL)
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "color", selected = "All")
    updateCheckboxGroupInput(session, "rarity", selected = rarities)
    updateSliderInput(session, "alsa", value = c(min(cards$ALSA, na.rm = TRUE), max(cards$ALSA, na.rm = TRUE)))
    updateSliderInput(session, "pick_range", value = c(0, max(cards$Picked, na.rm = TRUE)))
  })
  
  # Value boxes
  output$total_cards <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Cards",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$avg_winrate <- renderValueBox({
    df <- filtered_data()
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))  # Clean + convert
    
    avg_wr <- round(mean(df$`GP.WR`, na.rm = TRUE), 1)
    
    valueBox(
      value = if (!is.na(avg_wr)) paste0(avg_wr, "%") else "N/A",
      subtitle = "Avg Win Rate",
      icon = icon("trophy"),
      color = "green"
    )
  })
  
  
  output$top_card <- renderValueBox({
    df <- filtered_data()
    
    # Clean and convert GP.WR
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))
    
    # Get index of top card safely
    if (nrow(df) == 0 || all(is.na(df$`GP.WR`))) {
      top <- NA
    } else {
      top_idx <- which.max(df$`GP.WR`)
      top <- df$Name[top_idx]
    }
    
    valueBox(
      value = tags$span(title = top, substr(top, 1, 15)),
      subtitle = "Top Card",
      icon = icon("star"),
      color = "yellow"
    )
    
  })
  
  
  output$avg_alsa <- renderValueBox({
    avg_alsa <- round(mean(filtered_data()$ALSA, na.rm = TRUE), 2)
    valueBox(
      value = avg_alsa,
      subtitle = "Avg ALSA",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  # Interactive card table
  output$card_table <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              selection = 'single') %>%
      formatRound(columns = c('ALSA', 'GP.WR', 'OH.WR', 'GD.WR', 'GIH.WR', 'GNS.WR'), digits = 1)
  })
  
  # Table selection updates selected card
  observeEvent(input$card_table_rows_selected, {
    if(length(input$card_table_rows_selected) > 0) {
      selected_card(filtered_data()[input$card_table_rows_selected, ])
    }
  })
  
  # Interactive ALSA scatter plot
  output$alsa_scatter <- renderPlotly({
    df <- filtered_data()
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))  # Ensure numeric
    
    p <- ggplot(df, aes(x = ALSA, y = `GP.WR`, color = Rarity, text = Name)) +
      geom_point(alpha = 0.7, size = 2) +
      theme_minimal() +
      scale_y_continuous(
        name = "GP Win Rate (%)",
        breaks = seq(40, 60, by = 5),
        limits = c(40, 60)
      ) +
      labs(title = "ALSA vs GP Win Rate", x = "ALSA")
    
    ggplotly(p, tooltip = c("text", "x", "y", "colour"))
  })
  
  
  # Handle scatter plot clicks
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    if(!is.null(click_data)) {
      point_index <- click_data$pointNumber + 1
      selected_card(filtered_data()[point_index, ])
    }
  })
  
  # Selected card information
  output$selected_card_info <- renderUI({
    card <- selected_card()
    if(is.null(card)) {
      return(div(class = "text-center p-4", 
                 h4("No card selected"), 
                 p("Click on a point in the scatter plot or select a row in the table")))
    }
    
    div(class = "selected-card p-4",
        h4(card$Name, class = "font-bold text-lg"),
        p(strong("Color: "), card$Color),
        p(strong("Rarity: "), card$Rarity),
        p(strong("ALSA: "), round(card$ALSA, 2)),
        p(strong("Times Picked: "), format(card$Picked, big.mark = ",")),
        p(strong("GP Win Rate: "), paste0(card$GP.WR, "%")),
        p(strong("Opening Hand WR: "), paste0(card$OH.WR, "%")),
        p(strong("Games Drawn WR: "), paste0(card$GD.WR, "%"))
    )
  })
  
  # Performance chart
  output$performance_chart <- renderPlotly({
    df <- filtered_data()
    
    # Clean the selected metric column — remove % and convert to numeric
    metric <- input$analysis_metric
    df[[metric]] <- as.numeric(gsub("%", "", df[[metric]]))
    
    top_df <- df[order(-df[[metric]]), ][1:min(input$top_n, nrow(df)), ]
    
    if(input$chart_type == "bar") {
      p <- ggplot(top_df, aes_string(
        x = sprintf("reorder(Name, -`%s`)", metric), 
        y = metric,
        fill = if(input$show_rarity) "Rarity" else NULL
      )) +
        geom_col() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Card Name", y = paste(metric, "(%)"))
      
    } else {
      p <- ggplot(top_df, aes_string(
        x = sprintf("reorder(Name, -`%s`)", metric), 
        y = metric,
        color = if(input$show_rarity) "Rarity" else NULL
      )) +
        geom_segment(aes_string(xend = sprintf("reorder(Name, -`%s`)", metric), yend = 0)) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Card Name", y = paste(metric, "(%)"))
    }
    
    ggplotly(p)
  })
  
  
  # Win rate histogram
  output$winrate_histogram <- renderPlotly({
    df <- filtered_data()
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))  # Clean "%"
    
    p <- ggplot(df, aes(x = `GP.WR`, fill = Rarity)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Win Rate Distribution", x = "GP Win Rate (%)", y = "Count")
    
    ggplotly(p)
  })
  
  # Rarity boxplot
  output$rarity_boxplot <- renderPlotly({
    df <- filtered_data()
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))
    
    p <- ggplot(df, aes(x = Rarity, y = `GP.WR`, fill = Rarity)) +
      geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
      theme_minimal() +
      scale_y_continuous(
        name = "GP Win Rate (%)",
        limits = c(40, 60),                   # Focused range
        breaks = seq(40, 60, by = 5)          # Clean tick steps: 40, 45, 50, 55, 60
      ) +
      labs(title = "Win Rate by Rarity")
    
    ggplotly(p)
  })
  
  
  # Add random card to comparison
  observeEvent(input$add_random, {
    random_card <- sample(cards$Name, 1)
    current_selection <- input$compare_cards
    if(length(current_selection) < 6) {
      updateSelectizeInput(session, "compare_cards", 
                           selected = c(current_selection, random_card))
    }
  })
  
  # Comparison chart
  output$comparison_chart <- renderPlotly({
    if(is.null(input$compare_cards) || length(input$compare_cards) == 0) {
      return(plotly_empty() %>% layout(title = "Select cards to compare"))
    }
    
    compare_df <- cards[cards$Name %in% input$compare_cards, ]
    
    if(input$comparison_view == "winrate") {
      wr_cols <- c("OH.WR", "GD.WR", "GIH.WR", "GNS.WR")
      long_df <- pivot_longer(compare_df, cols = all_of(wr_cols), 
                              names_to = "Context", values_to = "WinRate")
      long_df$Context <- gsub("\\.WR", "", long_df$Context)
      
      p <- ggplot(long_df, aes(x = Context, y = WinRate, fill = Name)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(title = "Win Rate Comparison", y = "Win Rate (%)")
    } else if(input$comparison_view == "scatter") {
      p <- ggplot(compare_df, aes(x = Picked, y = GP.WR, color = Name, text = Name)) +
        geom_point(size = 4) +
        theme_minimal() +
        labs(title = "Pick Count vs Performance", x = "Times Picked", y = "GP Win Rate (%)")
    } else if (input$comparison_view == "radar") {
      radar_df <- cards[cards$Name %in% input$compare_cards, ]
      
      radar_df <- radar_df[, c("Name", "GP.WR", "OH.WR", "GD.WR", "GIH.WR", "GNS.WR")]
      
      # Clean percentage strings: remove "%" and convert to numeric
      for (col in names(radar_df)[-1]) {
        radar_df[[col]] <- as.numeric(gsub("%", "", radar_df[[col]]))
      }
      
      # Drop rows with all NA values
      radar_df <- radar_df[rowSums(is.na(radar_df[,-1])) < length(names(radar_df)) - 1, ]
      
      # Plotly radar chart
      fig <- plot_ly(type = 'scatterpolar', fill = 'toself')
      
      for (i in 1:nrow(radar_df)) {
        fig <- fig %>%
          add_trace(
            r = unlist(radar_df[i, -1], use.names = FALSE),
            theta = c("GP", "OH", "GD", "GIH", "GNS"),
            name = radar_df$Name[i],
            mode = 'lines+markers'
          )
      }
      
      fig <- fig %>% layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 100))
        ),
        showlegend = TRUE,
        title = "Performance Radar"
      )
      
      return(fig)
    }
    
    ggplotly(p)
  })
  
  # Color performance heatmap
  output$color_heatmap <- renderPlotly({
    df <- filtered_data()
    
    # Clean GP.WR column
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))
    
    # Drop NAs for this chart
    df <- df[!is.na(df$`GP.WR`) & !is.na(df$Color) & !is.na(df$Rarity), ]
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for current filters."))
    }
    
    # Aggregate average win rate by Color and Rarity
    color_stats <- aggregate(`GP.WR` ~ Color + Rarity, data = df, FUN = mean)
    
    # Plot heatmap
    p <- ggplot(color_stats, aes(x = Color, y = Rarity, fill = `GP.WR`,
                                 text = paste("Color:", Color, "<br>Rarity:", Rarity, "<br>WR:", round(`GP.WR`, 1), "%"))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "red", high = "green") +
      theme_minimal() +
      labs(title = "Average Win Rate by Color and Rarity", fill = "Win Rate (%)")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ALSA trends
  output$alsa_trends <- renderPlotly({
    df <- filtered_data()
    
    # Ensure ALSA and GP.WR are numeric
    df$ALSA <- as.numeric(df$ALSA)
    df$`GP.WR` <- as.numeric(gsub("%", "", df$`GP.WR`))
    
    # Filter out NAs
    df <- df[!is.na(df$ALSA) & !is.na(df$`GP.WR`), ]
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for current filters."))
    }
    
    # Bin ALSA into ranges
    df$ALSA_range <- cut(df$ALSA, breaks = 10)
    
    # Aggregate win rates by ALSA bin
    trend_data <- aggregate(`GP.WR` ~ ALSA_range, data = df, FUN = mean, na.rm = TRUE)
    
    # Plot line chart
    p <- ggplot(trend_data, aes(x = ALSA_range, y = `GP.WR`, group = 1)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Win Rate Trends by ALSA Range", x = "ALSA Range", y = "Average Win Rate (%)")
    
    ggplotly(p)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
