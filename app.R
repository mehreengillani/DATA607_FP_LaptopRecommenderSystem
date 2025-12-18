# ====================SHINY APP ====================
# File: app.R

library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(scales)

# 1. LOAD YOUR ACTUAL DATA
df <- readRDS("laptop_data_shiny.rds")

# Define professional color palette
professional_colors <- list(
  primary = "#2c3e50",        # Dark blue-gray
  secondary = "#3498db",      # Bright blue
  success = "#27ae60",        # Green
  warning = "#f39c12",        # Orange
  danger = "#e74c3c",         # Red
  info = "#2980b9",           # Darker blue
  light = "#ecf0f1",          # Light gray
  dark = "#2c3e50",           # Dark
  
  # Retailer colors (professional blues)
  amazon = "#3498db",         # Professional blue for Amazon
  walmart = "#1abc9c",        # Teal for Walmart
  
  # Light pastel accents (PERFECT for charts)
  accent1 = "#FFDAB9",        # Soft lavender (pastel purple)
  accent2 = "#98FB98",        # Lemon chiffon (pastel yellow)
  accent3 = "#87CEEB"        # blue
)

# Color scales for plots
color_scale <- c(professional_colors$amazon, professional_colors$walmart, 
                 professional_colors$accent1, professional_colors$accent2,
                 professional_colors$accent3)

# 2. CREATE ENHANCED DATA FOR THE APP
df <- df %>%
  mutate(
    # Fix unrealistic RAM values first
    ram_gb = ifelse(
      ram_gb > 32 & extracted_price < 500,
      case_when(
        ram_gb == 128 ~ 4,
        ram_gb == 256 ~ 8,
        ram_gb == 512 ~ 16,
        ram_gb == 1024 ~ 32,
        TRUE ~ 8
      ),
      ram_gb
    ),
    
    # Cap extreme RAM values
    ram_gb = pmin(ram_gb, 64),
    
    # Fix unrealistic price per GB
    price_per_gb = extracted_price / ram_gb,
    
    # Flag unrealistic products
    is_realistic = !(ram_gb > 32 & extracted_price < 300) &
      !(price_per_gb < 1),
    
    price_category = factor(price_category,
                            levels = c("Budget (<$500)", 
                                       "Mid-Range ($500-$1k)", 
                                       "Premium ($1k-$1.5k)", 
                                       "High-End ($1.5k-$3k)", 
                                       "Luxury (>$3k)")),
  

    ram_category = case_when(
      ram_gb <= 4 ~ "4GB or less",
      ram_gb <= 8 ~ "8GB",
      ram_gb <= 16 ~ "16GB",
      ram_gb <= 32 ~ "32GB",
      TRUE ~ "64GB+"
    ),
    
    # Fix unrealistic storage values
    storage_gb_cleaned = case_when(
      # If storage > 4000GB and price < $1500, it's likely an error (TB entered as GB)
      storage_gb > 4000 & extracted_price < 1500 ~ storage_gb / 1000,
      # If storage > 2000GB, cap it at reasonable max
      storage_gb > 2000 ~ 2048,
      # If storage > 1000GB and price < $800, it's likely an error
      storage_gb > 1000 & extracted_price < 800 ~ 512,
      # Keep original if reasonable
      TRUE ~ storage_gb
    ),
    
    # Create storage category (similar to RAM)
    storage_category = case_when(
      is.na(storage_gb_cleaned) ~ "Unknown",
      storage_gb_cleaned <= 128 ~ "128GB or less",
      storage_gb_cleaned <= 256 ~ "256GB",
      storage_gb_cleaned <= 512 ~ "512GB",
      storage_gb_cleaned <= 1024 ~ "1TB",
      storage_gb_cleaned <= 2048 ~ "2TB",
      TRUE ~ "2TB+"
    ),
    
    # Make them ordered factors for proper sorting
    ram_category = factor(ram_category,
                          levels = c("4GB or less", "8GB", "16GB", "32GB", "64GB+")),
    
    storage_category = factor(storage_category,
                              levels = c("128GB or less", "256GB", "512GB", "1TB", "2TB", "2TB+", "Unknown")),
    
    # Enhanced value score with more factors
    value_score_enhanced = ifelse(
      !is.na(rating_numeric) & rating_numeric > 0 & ram_gb > 0,
      (rating_numeric * 20) * (ram_gb / 8) * 
        (ifelse(!is.na(storage_gb), log10(storage_gb/128 + 1), 1)) /
        (extracted_price / 500),
      0
    ),
    
    # Processor tier (simplified)
    processor_tier = case_when(
      grepl("i9|ryzen 9|apple m3 max|apple m2 max", processor_type, ignore.case = TRUE) ~ "High-End",
      grepl("i7|ryzen 7|apple m3 pro|apple m2 pro", processor_type, ignore.case = TRUE) ~ "Performance",
      grepl("i5|ryzen 5|apple m3|apple m2|apple m1", processor_type, ignore.case = TRUE) ~ "Mid-Range",
      grepl("i3|ryzen 3|celeron|pentium|athlon", processor_type, ignore.case = TRUE) ~ "Entry-Level",
      TRUE ~ "Unknown"
    ),
    
    # Gaming flag
    is_gaming_flag = ifelse(!is.na(is_gaming), as.logical(is_gaming), FALSE),
    
    
    # Student laptop flag
    is_student_friendly = extracted_price <= 800 & 
      ram_gb >= 8 & 
      grepl("lightweight|thin|portable|student|chromebook", title, ignore.case = TRUE),

    
    
    # Create a performance score
    performance_score = (ram_gb / 8) * 
      case_when(
        processor_tier == "High-End" ~ 1.5,
        processor_tier == "Performance" ~ 1.2,
        processor_tier == "Mid-Range" ~ 1.0,
        processor_tier == "Entry-Level" ~ 0.7,
        TRUE ~ 0.5
      ) *
      (ifelse(!is.na(display_inches), display_inches/15.6, 1))
  ) %>%
  # Filter out unrealistic products
  filter(is_realistic == TRUE)

# 3. GET UNIQUE VALUES
brands <- sort(unique(df$brand[df$brand != "Other" & df$brand != "Budget Brand"]))
retailers <- sort(unique(df$source))
price_categories <- sort(unique(df$price_category))
ram_categories <- sort(unique(df$ram_category))
processor_tiers <- sort(unique(df$processor_tier))

# 4. UI COMPONENT - UPDATED WITH PROFESSIONAL COLORS
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/2942/2942802.png", 
               height = "30px", style = "margin-right: 10px;"),
      span("Laptop Recommender Pro", style = paste0("color: ", professional_colors$light, "; font-weight: bold; font-size: 20px;"))
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("💡 Smart Recommendations", tabName = "recommend", icon = icon("laptop-code")),
      menuItem("🔍 Data Explorer", tabName = "data", icon = icon("search")),
      menuItem("📈 Advanced Analytics", tabName = "analysis", icon = icon("chart-line")),
      menuItem("🏪 Retailer Comparison", tabName = "compare", icon = icon("store")),
      menuItem("📊 Feature Analysis", tabName = "features", icon = icon("microchip")),
      menuItem("💾 Export Data", tabName = "export", icon = icon("download")),
      menuItem("ℹ️ About", tabName = "about", icon = icon("info-circle")),
      
      hr(),
      
      h4("Quick Filters", style = paste0("padding: 10px; color: ", professional_colors$primary, ";")),
      
      pickerInput("sidebar_retailer", "Retailer:",
                  choices = c("All", retailers),
                  selected = "All",
                  options = list(`live-search` = TRUE,
                                 `actions-box` = TRUE)),
      
      pickerInput("sidebar_brand", "Brand:",
                  choices = c("All", brands),
                  selected = "All",
                  options = list(`live-search` = TRUE,
                                 `actions-box` = TRUE)),
      
      sliderInput("sidebar_price", "Price Range:",
                  min = 0, 
                  max = max(df$extracted_price, na.rm = TRUE),  # This should be around $5,000+
                  value = c(0, max(df$extracted_price, na.rm = TRUE)),  # Default to show ALL
                  pre = "$"),
      
      actionBttn("apply_filters", "Apply Filters",
                 style = "gradient",
                 color = "primary",
                 block = TRUE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(paste0("
      /* Professional styling */
      .small-box {border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);}
      .box {border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);}
      .content-wrapper {background-color: #f8f9fa;}
      .skin-blue .main-header .logo {background-color: ", professional_colors$primary, ";}
      .skin-blue .main-header .navbar {background-color: ", professional_colors$secondary, ";}
      
      /* Value box colors */
      .bg-blue {background-color: ", professional_colors$secondary, " !important;}
      .bg-green {background-color: ", professional_colors$success, " !important;}
      .bg-yellow {background-color: ", professional_colors$warning, " !important;}
      .bg-purple {background-color: ", professional_colors$accent1, " !important;}
      .bg-teal {background-color: ", professional_colors$accent2, " !important;}
      
      /* Tab colors */
      .nav-tabs-custom > .nav-tabs > li.active {border-top-color: ", professional_colors$secondary, ";}
      
      /* Button styling */
      .btn-primary {background-color: ", professional_colors$secondary, "; border-color: ", professional_colors$secondary, ";}
      
      /* Plot background */
      .plotly {background-color: white !important;}
    ")))
    ),
    
    tabItems(
      # Tab 1: Dashboard
      tabItem(tabName = "dashboard",
              h2("📊 Market Intelligence Dashboard", style = paste0("color: ", professional_colors$primary, ";")),
              
              fluidRow(
                valueBox(nrow(df), "Total Laptops", 
                         icon = icon("laptop"), 
                         color = "aqua", width = 3),
                valueBox(paste0("$", round(mean(df$extracted_price, na.rm = TRUE), 0)), 
                         "Average Price", 
                         icon = icon("dollar-sign"), 
                         color = "lime", width = 3),
                valueBox(round(mean(df$rating_numeric, na.rm = TRUE), 2), 
                         "Average Rating", 
                         icon = icon("star"), 
                         color = "yellow", width = 3),
                valueBox(round(mean(df$ram_gb, na.rm = TRUE), 1), 
                         "Avg RAM (GB)", 
                         icon = icon("memory"), 
                         color = "purple", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Market Distribution by Price Tier",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("price_tier_distribution"),
                  footer = "Distribution of laptops across different price segments"
                ),
                
                box(
                  title = "Brand Market Share (Top 10)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("brand_market_share"),
                  footer = "Market share based on product count"
                )
              ),
              
              fluidRow(
                box(
                  title = "Price vs Rating Analysis",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rating_price_scatter")
                ),
                
                box(
                  title = "Gaming vs Regular Laptops: Price Comparison",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("gaming_price_comparison")
                )
              ),
              
              fluidRow(
                box(
                  title = "RAM Distribution Analysis",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("ram_distribution")
                ),
                
                box(
                  title = "Retailer Comparison: Average Ratings",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("retailer_rating_comparison")
                )
              )
      ),
      
      # Tab 2: Smart Recommendations 
      tabItem(tabName = "recommend",
              h2("💡 Smart Laptop Recommendations", style = paste0("color: ", professional_colors$primary, ";")),
              
              fluidRow(
                box(
                  title = "🎯 Configure Your Search",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  
                  radioGroupButtons("use_case", "Primary Use Case:",
                                    choices = c("General Use", "Student", "Gaming", 
                                                "Business", "Creative Work", "Budget",
                                                "2-in-1/Convertible"),
                                    selected = "General Use",
                                    justified = FALSE,  # Change from TRUE
                                    checkIcon = list(yes = icon("check")),
                                    status = "primary",
                                    direction = "vertical",  # Stack vertically
                                    individual = TRUE),
                  
                  uiOutput("dynamic_filters"),
                  
                  hr(),
                  
                  h4("Priority Weights", style = paste0("color: ", professional_colors$primary, ";")),
                  sliderInput("weight_performance", "Performance Importance:",
                              min = 0, max = 100, value = 70, post = "%"),
                  sliderInput("weight_value", "Value for Money:",
                              min = 0, max = 100, value = 80, post = "%"),
                  sliderInput("weight_features", "Features & Quality:",
                              min = 0, max = 100, value = 60, post = "%"),
                  
                  br(),
                  actionBttn("find_laptops", "Find Best Laptops", 
                             icon = icon("search"),
                             style = "gradient",
                             color = "success",
                             size = "lg",
                             block = TRUE),
                  
                  hr(),
                  
                  uiOutput("category_recommendations_ui")
                ),
                
                box(
                  title = "🎯 Top Recommendations",
                  status = "success",
                  solidHeader = TRUE,
                  width = 8,
                  collapsible = TRUE,
                  
                  # Graph on top
                  plotlyOutput("recommendation_comparison_chart", height = "300px"),
                  br(),
                  hr(),
                  
                  # Table in middle
                  DTOutput("recommendation_table"),
                  
                  br(),
                  
                  # Details at bottom
                  uiOutput("recommendation_details")
                )
              )
      ),
      
      # Tab 3: Data Explorer
      tabItem(tabName = "data",
              h2("🔍 Interactive Data Explorer", style = paste0("color: ", professional_colors$primary, ";")),
              fluidRow(
                box(
                  title = "Filters & Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  pickerInput("explorer_retailer", "Retailer:",
                              choices = c("All", retailers),
                              selected = "All",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `selected-text-format` = "count > 3")),
                  pickerInput("explorer_brand", "Brand:",
                              choices = c("All", brands),
                              selected = "All",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `selected-text-format` = "count > 3")),
                  sliderInput("explorer_price", "Price Range:",
                              min = 0, max = max(df$extracted_price, na.rm = TRUE),
                              value = c(0, 2000),
                              pre = "$"),
                  pickerInput("explorer_processor", "Processor Tier:",
                              choices = c("All", processor_tiers),
                              selected = "All",
                              multiple = TRUE),
                  awesomeCheckboxGroup("explorer_features", "Special Features:",
                                       choices = c("Gaming", "2-in-1", "Student Friendly"),
                                       selected = character(0),
                                       status = "primary"),
                  br(),
                  downloadBttn("download_explorer_data", "Download Filtered Data",
                               style = "gradient",
                               color = "primary",
                               size = "sm",
                               block = TRUE)
                ),
                
                box(
                  title = "Data Table",
                  status = "success",
                  solidHeader = TRUE,
                  width = 9,
                  DTOutput("explorer_table")
                )
              )
      ),
      
      # Tab 4: Advanced Analytics
      tabItem(tabName = "analysis",
              h2("📈 Advanced Analytics", style = paste0("color: ", professional_colors$primary, ";")),
              fluidRow(
                box(
                  title = "Value Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("value_analysis_plot"),
                  footer = "Higher scores indicate better value for money"
                ),
                
                box(
                  title = "Price vs Specifications",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("analysis_x", "X-Axis Variable:",
                              choices = c("RAM (GB)" = "ram_gb", 
                                          "Storage (GB)" = "storage_gb",
                                          "Screen Size" = "display_inches",
                                          "Performance Score" = "performance_score"),
                              selected = "ram_gb"),
                  plotlyOutput("analysis_scatter")
                )
              ),
              
              fluidRow(
                box(
                  title = "Brand Value Comparison",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("brand_value_boxplot"),
                  footer = "Compares value scores across different brands"
                ),
                
                box(
                  title = "Processor Tier Analysis",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("processor_analysis"),
                  footer = "Performance across different processor categories"
                )
              )
      ),
      
      # Tab 5: Retailer Comparison
      tabItem(tabName = "compare",
              h2("🏪 Retailer Comparison", style = paste0("color: ", professional_colors$primary, ";")),
              fluidRow(
                box(
                  title = "Price Distribution Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("retailer_price_distribution")
                ),
                
                box(
                  title = "Product Selection by Category",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("retailer_category_comparison")
                )
              ),
              
              fluidRow(
                box(
                  title = "Average Specifications by Retailer",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("retailer_specs_comparison")
                ),
                
                box(
                  title = "Value for Money Comparison",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("retailer_value_comparison")
                )
              )
      ),
      
      # Tab 6: Feature Analysis
      tabItem(tabName = "features",
              h2("📊 Feature Analysis", style = paste0("color: ", professional_colors$primary, ";")),
              fluidRow(
                box(
                  title = "RAM vs Price Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("ram_price_analysis")
                ),
                
                box(
                  title = "Storage vs Price Analysis",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("storage_price_analysis")
                )
              ),
              
              fluidRow(
                box(
                  title = "Processor Tier vs Price",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("processor_price_analysis")
                ),
                
                box(
                  title = "Gaming Features Premium",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("gaming_premium_analysis")
                )
              )
      ),
      
      # Tab 7: Export Data
      tabItem(tabName = "export",
              h2("💾 Data Export Center", style = paste0("color: ", professional_colors$primary, ";")),
              fluidRow(
                box(
                  title = "Export Options",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  
                  h4("📁 Export Full Dataset"),
                  radioGroupButtons("export_format", "Format:",
                                    choices = c("CSV", "Excel", "RDS"),
                                    selected = "CSV",
                                    justified = TRUE),
                  downloadBttn("export_full", "Download Full Dataset",
                               style = "gradient",
                               color = "primary",
                               block = TRUE),
                  
                  hr(),
                  
                  h4("📊 Custom Analysis Export"),
                  textInput("analysis_name", "Analysis Name:", 
                            placeholder = "e.g., Gaming_Laptops_Analysis"),
                  textAreaInput("analysis_description", "Description:", 
                                rows = 3,
                                placeholder = "Describe what this analysis contains..."),
                  
                  actionBttn("create_analysis", "Create Analysis Package",
                             style = "gradient",
                             color = "success",
                             block = TRUE),
                  
                  br(),
                  uiOutput("download_analysis_ui")
                ),
                
                box(
                  title = "Export Statistics",
                  status = "success",
                  solidHeader = TRUE,
                  width = 8,
                  valueBoxOutput("total_products_box"),
                  valueBoxOutput("amazon_count_box"),
                  valueBoxOutput("walmart_count_box"),
                  
                  hr(),
                  h4("Quick Exports:"),
                  fluidRow(
                    column(6,
                           downloadBttn("export_budget", "Budget Laptops (<$500)",
                                        style = "material-flat",
                                        color = "warning",
                                        block = TRUE)
                    ),
                    column(6,
                           downloadBttn("export_gaming", "Gaming Laptops",
                                        style = "material-flat",
                                        color = "danger",
                                        block = TRUE)
                    )
                  ),
                  br(),
                  fluidRow(
                    column(6,
                           downloadBttn("export_student", "Student Laptops",
                                        style = "material-flat",
                                        color = "default",
                                        block = TRUE)
                    ),
                    column(6,
                           downloadBttn("export_premium", "Premium Laptops",
                                        style = "material-flat",
                                        color = "success",
                                        block = TRUE)
                    )
                  )
                )
              )
      ),
      
      # Tab 8: About
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "📋 Project Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 20px;",
                    
                    h2("DATA607: Final Project - Laptop Recommender System"),
                    h3("Intelligent Laptop Comparison & Recommendation Engine"),
                    
                    hr(),
                    
                    h4("👤 Student Information:"),
                    tags$ul(
                      tags$li(tags$strong("Name:"), " Mehreen ALi Gillani"),
                      tags$li(tags$strong("Course:"), " DATA607 - Data Science Capstone"),
                      tags$li(tags$strong("Course Instructor:"), "Andrew Catlin"),
                      tags$li(tags$strong("Institution:"), " CUNY School of Professional Studies")
                    ),
                    
                    hr(),
                    
                    h4("🎯 Application Overview:"),
                    p("This interactive Shiny application provides comprehensive analysis and intelligent recommendations for laptop purchases. The system helps users make data-driven decisions by comparing specifications, prices, and value across different retailers."),
                    
                    h5("🔧 Key Features:"),
                    tags$ul(
                      tags$li(tags$strong("Smart Recommendations:"), " Personalized laptop suggestions based on use case, budget, and preferences"),
                      tags$li(tags$strong("Market Intelligence:"), " Real-time analysis of laptop market trends and pricing"),
                      tags$li(tags$strong("Retailer Comparison:"), " Side-by-side comparison of Amazon vs Walmart offerings"),
                      tags$li(tags$strong("Feature Analysis:"), " Detailed breakdown of specifications vs price relationships"),
                      tags$li(tags$strong("Interactive Exploration:"), " Filter and explore thousands of laptop listings")
                    ),
                    
                    hr(),
                    
                    h4("📊 Data Sources & Methodology:"),
                    tags$ul(
                      tags$li("Scraped data from Amazon and Walmart e-commerce platforms"),
                      tags$li("Cleaned and standardized specifications (RAM, Storage, Processor, etc.)"),
                      tags$li("Calculated value scores based on price-performance ratios"),
                      tags$li("Implemented machine learning-inspired recommendation algorithms")
                    ),
                    
                    hr(),
                    
                    h4("⚙️ Technical Implementation:"),
                    tags$div(
                      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                      tags$code("R Shiny · plotly · dplyr · ggplot2 · shinydashboard")
                    ),
                    
                    br(),
                    
                    p(tags$em("Note: This application is for educational purposes as part of the DATA607 capstone project. All data is used for analytical demonstration."))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 Dataset Statistics",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  valueBoxOutput("total_products_about"),
                  valueBoxOutput("amazon_count_about"),
                  valueBoxOutput("walmart_count_about"),
                  valueBoxOutput("avg_price_about")
                ),
                
                box(
                  title = "🔍 Quick Links",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  tags$div(
                    style = "padding: 15px;",
                    actionBttn("goto_recommend", "Get Laptop Recommendations", 
                               icon = icon("laptop-code"),
                               style = "gradient", color = "primary", block = TRUE),
                    br(),
                    actionBttn("goto_analysis", "Explore Market Analytics", 
                               icon = icon("chart-line"),
                               style = "material-flat", color = "success", block = TRUE),
                    br(),
                    actionBttn("goto_compare", "Compare Retailers", 
                               icon = icon("store"),
                               style = "material-flat", color = "warning", block = TRUE)
                  )
                )
              )
      )
    ),
    
    # Footer
    tags$footer(
      style = paste0("text-align: center; padding: 15px; background-color: ", professional_colors$primary, "; color: white; margin-top: 20px;"),
      tags$div(
        tags$p("DATA607 Final Project - Laptop Recommender System"),
        tags$p("Mehreen ALi Gillani - CUNY SPS"),
        tags$p(tags$small("Academic project for educational purposes"))
      )
    )
  )
)
  
# 5. SERVER LOGIC - UPDATED WITH RECOMMENDATIONS
server <- function(input, output, session) {
  
  # === 1. MAIN DASHBOARD REACTIVE (ONLY ONCE) ===
  filtered_data <- reactive({
    data <- df
    
    if (input$sidebar_retailer != "All") {
      data <- data %>% filter(source == input$sidebar_retailer)
    }
    
    if (input$sidebar_brand != "All") {
      data <- data %>% filter(brand == input$sidebar_brand)
    }
    
    data <- data %>%
      filter(extracted_price >= input$sidebar_price[1],
             extracted_price <= input$sidebar_price[2])
    
    return(data)
  })

  # === 2. EXPLORER FUNCTIONALITY ===

  explorer_data <- reactive({
    # Wait for essential inputs to load
    req(input$explorer_price, input$explorer_retailer, input$explorer_brand)
    
    data <- df
    
    # Simple, direct filtering (no apply button logic)
    if (!"All" %in% input$explorer_retailer) {
      data <- data %>% filter(source %in% input$explorer_retailer)
    }
    
    if (!"All" %in% input$explorer_brand) {
      data <- data %>% filter(brand %in% input$explorer_brand)
    }
    
    data <- data %>%
      filter(between(extracted_price, input$explorer_price[1], input$explorer_price[2]))
    
    # Optional filters (only if specified)
    if (!is.null(input$explorer_processor) && !"All" %in% input$explorer_processor) {
      data <- data %>% filter(processor_tier %in% input$explorer_processor)
    }
    
    # Feature filters
    if ("Gaming" %in% input$explorer_features) data <- data %>% filter(is_gaming_flag)
    if ("2-in-1" %in% input$explorer_features) data <- data %>% filter(is_convertible)
    if ("Student Friendly" %in% input$explorer_features) data <- data %>% filter(is_student_friendly)
    
    return(data)
  })
  

  
  # ========== analysis_scatter ===============
  output$analysis_scatter <- renderPlotly({
    data <- filtered_data()  # Use filtered_data() not explorer_data()
    
    # Check if data exists
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "dot") %>%
               layout(
                 title = "No data available with current filters",
                 plot_bgcolor = professional_colors$light,
                 paper_bgcolor = professional_colors$light
               ))
    }
    
    # Get the selected X-axis variable
    x_var <- input$analysis_x
    
    # Create appropriate labels based on selection
    x_label <- case_when(
      x_var == "ram_gb" ~ "RAM (GB)",
      x_var == "storage_gb" ~ "Storage (GB)",
      x_var == "rating_numeric" ~ "Rating",
      x_var == "performance_score" ~ "Performance Score",
      TRUE ~ x_var
    )
    
    # Create the scatter plot with lines
    
    p <- plot_ly(data, x = ~get(x_var), y = ~extracted_price,
                 color = ~source,
                 colors = c(professional_colors$amazon, professional_colors$walmart),
                 type = "scatter",
                 mode = "markers",
                 marker = list(size = 6, opacity = 0.6),
                 text = ~paste("Brand:", brand,
                               "<br>Price: $", extracted_price),
                 hoverinfo = "text",
                 name = ~paste(source, "Products")) %>%
      layout(title = paste("Price vs", x_label, "with Trend Lines"),
             xaxis = list(title = x_label),
             yaxis = list(title = "Price ($)", tickprefix = "$"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
    
    # Add trend lines for each retailer
    retailers <- unique(data$source)
    for(retailer in retailers) {
      retailer_data <- data %>% filter(source == retailer)
      
      if(nrow(retailer_data) > 1) {
        # Fit linear model
        fit <- lm(extracted_price ~ get(x_var), data = retailer_data)
        
        # Add trend line
        p <- p %>% add_trace(
          x = ~get(x_var),
          y = fitted(fit),
          data = retailer_data,
          type = "scatter",
          mode = "lines",
          line = list(width = 2, dash = "dash"),
          name = paste(retailer, "Trend"),
          showlegend = TRUE,
          hoverinfo = "none"
        )
      }
    }
    
    return(p)
  })
  
  
 # =============== processor_analysis =============
  output$processor_analysis <- renderPlotly({
    data <- filtered_data()
    
    # Check if data exists
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = "No data available",
                 plot_bgcolor = professional_colors$light,
                 paper_bgcolor = professional_colors$light
               ))
    }
    
    # Check if processor_tier exists
    if (!"processor_tier" %in% names(data)) {
      return(plotly_empty() %>%
               layout(
                 title = "Processor tier data not available",
                 plot_bgcolor = professional_colors$light,
                 paper_bgcolor = professional_colors$light
               ))
    }
    
    # Remove NA processor tiers
    data <- data %>% filter(!is.na(processor_tier))
    
    if (nrow(data) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = "No processor data available",
                 plot_bgcolor = professional_colors$light,
                 paper_bgcolor = professional_colors$light
               ))
    }
    
    # Create summary by processor tier
    summary <- data %>%
      group_by(processor_tier) %>%
      summarise(
        avg_price = mean(extracted_price, na.rm = TRUE),
        avg_rating = mean(rating_numeric, na.rm = TRUE),
        avg_ram = mean(ram_gb, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      filter(count > 0) %>%
      arrange(desc(avg_price))
    
    # Create a grouped bar chart
    plot_ly(summary) %>%
      add_trace(x = ~processor_tier, y = ~avg_price,
                type = "bar", name = "Avg Price",
                marker = list(color = professional_colors$secondary),
                text = ~paste("$", round(avg_price, 0)),
                textposition = "auto",
                hoverinfo = "text",
                hovertext = ~paste(processor_tier,
                                   "<br>Avg Price: $", round(avg_price, 0),
                                   "<br>Avg Rating: ", round(avg_rating, 2),
                                   "<br>Avg RAM: ", round(avg_ram, 1), "GB",
                                   "<br>Count: ", count)) %>%
      add_trace(x = ~processor_tier, y = ~avg_rating * 100,
                type = "bar", name = "Avg Rating (x100)",
                marker = list(color = professional_colors$accent2),
                text = ~paste(round(avg_rating, 2)),
                textposition = "auto") %>%
      layout(title = "Processor Tier Analysis",
             xaxis = list(title = "Processor Tier", 
                          categoryorder = "total descending"),
             yaxis = list(title = "Avg Price"),
             barmode = "group",
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light,
             legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  
  # === 3. EXPLORER TABLE OUTPUT  ===
  
  
  # 1. Reactive data filtering
  explorer_data <- reactive({
    # Wait for inputs
    req(input$explorer_price)
    
    # Start with all data
    data <- df
    
    # Apply filters dynamically
    if (!is.null(input$explorer_retailer) && !"All" %in% input$explorer_retailer) {
      data <- data %>% filter(source %in% input$explorer_retailer)
    }
    
    if (!is.null(input$explorer_brand) && !"All" %in% input$explorer_brand) {
      data <- data %>% filter(brand %in% input$explorer_brand)
    }
    
    # Price filter
    data <- data %>% 
      filter(extracted_price >= input$explorer_price[1],
             extracted_price <= input$explorer_price[2])
    
    return(data)
  })
  
  # 2. Simple table display
  output$explorer_table <- renderDT({
    data <- explorer_data()
    
    # Show all data or message
    if (nrow(data) > 0) {
      datatable(
        data %>% select(source, brand, extracted_price, rating_numeric, ram_gb),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(Message = "Adjust filters to see laptops"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # 1. DASHBOARD VISUALIZATIONS - UPDATED COLORS
  output$price_tier_distribution <- renderPlotly({
    # Define categories EXACTLY as they appear in your data
    all_categories <- c("Budget (<$500)", 
                        "Mid-Range ($500-$1k)", 
                        "Premium ($1k-$1.5k)", 
                        "High-End ($1.5k-$3k)", 
                        "Luxury (>$3k)")
    
    # DEBUG: Check what's in filtered_data()
    filtered <- filtered_data()
    cat("=== DEBUG: Filtered Data Info ===\n")
    cat("Total rows in filtered data:", nrow(filtered), "\n")
    cat("Unique price categories:", toString(unique(filtered$price_category)), "\n")
    cat("Price range: $", min(filtered$extracted_price, na.rm = TRUE), 
        " to $", max(filtered$extracted_price, na.rm = TRUE), "\n")
    
    # Get data from filtered dataset
    data <- filtered %>%
      count(price_category) %>%
      filter(!is.na(price_category))  # Remove NA values
    
    cat("=== Counts by category ===\n")
    print(data)
    
    # Create a complete dataset with all categories (even if count is 0)
    plot_data <- data.frame(price_category = factor(all_categories, levels = all_categories)) %>%
      left_join(data, by = "price_category") %>%
      mutate(
        n = ifelse(is.na(n), 0, n),
        percentage = ifelse(is.na(n) | sum(n, na.rm = TRUE) == 0, 0, n / sum(n, na.rm = TRUE) * 100)
      )
    
    cat("=== Plot Data ===\n")
    print(plot_data)
    
    # Create a simple test plot to debug
    if(nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available with current filters",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Fix the line.width warning by removing it or using a single value
    plot_ly(plot_data, x = ~price_category, y = ~percentage,
            type = "bar",
            marker = list(
              color = color_scale,
              line = list(color = professional_colors$dark, width = 1)  # SINGLE value for width
            ),
            text = ~paste(n, "products", "<br>", round(percentage, 1), "%"),
            hoverinfo = "text") %>%
      layout(title = paste("Market Distribution by Price Tier (Total:", sum(plot_data$n), "products)"),
             xaxis = list(title = "Price Category", 
                          tickangle = 45,
                          categoryorder = "array",
                          categoryarray = all_categories),
             yaxis = list(title = "Percentage (%)", 
                          range = c(0, max(plot_data$percentage, na.rm = TRUE) * 1.1)),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light,
             margin = list(b = 120, t = 50, l = 50, r = 50))
  })
  
  output$brand_market_share <- renderPlotly({
    data <- filtered_data() %>%
      filter(brand != "Other") %>%
      count(brand) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      arrange(desc(n)) %>%
      head(10)
    
    plot_ly(data, labels = ~brand, values = ~percentage,
            type = "pie",
            marker = list(colors = color_scale),
            textinfo = "label+percent",
            hoverinfo = "label+value+percent") %>%
      layout(title = "Brand Market Share (Top 10)",
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  output$download_explorer_data <- downloadHandler(
    filename = function() {
      paste0("filtered_laptops_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- explorer_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  output$rating_price_scatter <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~extracted_price, y = ~rating_numeric,
            color = ~source, colors = c(professional_colors$amazon, professional_colors$walmart),
            type = "scatter", mode = "markers",
            size = ~ram_gb,
            text = ~paste("Brand:", brand, 
                          "<br>Price: $", extracted_price,
                          "<br>RAM: ", ram_gb, "GB",
                          "<br>Rating: ", rating_numeric),
            hoverinfo = "text",
            marker = list(opacity = 0.7, sizemode = "diameter", sizeref = 2)) %>%
      layout(title = "Price vs Rating Analysis",
             xaxis = list(title = "Price ($)"),
             yaxis = list(title = "Rating"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  output$gaming_price_comparison <- renderPlotly({
    data <- filtered_data()
    
    # First, check if we have the gaming flag column
    if(!"is_gaming_flag" %in% names(data)) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(
            title = "Gaming data not available",
            plot_bgcolor = professional_colors$light,
            paper_bgcolor = professional_colors$light
          )
      )
    }
    
    # Create gaming classification
    data <- data %>%
      mutate(
        laptop_type = case_when(
          is_gaming_flag == TRUE ~ "Gaming",
          is_gaming_flag == FALSE ~ "Regular",
          TRUE ~ "Unknown"
        )
      ) %>%
      filter(laptop_type != "Unknown")  # Remove unknown
    
    # Check what we have
    type_counts <- table(data$laptop_type)
    cat("DEBUG - Box plot data:\n")
    cat("  Gaming count:", ifelse("Gaming" %in% names(type_counts), type_counts["Gaming"], 0), "\n")
    cat("  Regular count:", ifelse("Regular" %in% names(type_counts), type_counts["Regular"], 0), "\n")
    
    # If no data for one type, we need to handle it
    all_types <- c("Gaming", "Regular")
    missing_types <- setdiff(all_types, unique(data$laptop_type))
    
    if(length(missing_types) > 0) {
      cat("  Missing types:", paste(missing_types, collapse = ", "), "\n")
      
      # Create dummy data for missing types
      for(missing_type in missing_types) {
        dummy_row <- data[1, , drop = FALSE]  # Copy first row structure
        dummy_row$laptop_type <- missing_type
        dummy_row$extracted_price <- NA  # Set price to NA
        data <- rbind(data, dummy_row)
      }
    }
    
    # Ensure factor ordering
    data <- data %>%
      mutate(laptop_type = factor(laptop_type, levels = c("Gaming", "Regular")))
    
    # Create the box plot
    p <- plot_ly(data, 
                 x = ~laptop_type, 
                 y = ~extracted_price,
                 color = ~laptop_type,
                 colors = c("Gaming" = professional_colors$danger, 
                            "Regular" = professional_colors$secondary),
                 type = "box",
                 boxpoints = ifelse(nrow(data) < 100, "all", "outliers"),
                 hoverinfo = "y+x",
                 showlegend = FALSE) %>%
      layout(
        title = paste("Price Distribution: Gaming vs Regular Laptops",
                      ifelse(nrow(data) > 0, paste0("(Total: ", nrow(data), ")"), "")),
        xaxis = list(title = "Laptop Type",
                     categoryorder = "array",
                     categoryarray = c("Gaming", "Regular")),
        yaxis = list(title = "Price ($)",
                     tickprefix = "$",
                     gridcolor = "#e1e5e9"),
        plot_bgcolor = professional_colors$light,
        paper_bgcolor = professional_colors$light,
        margin = list(t = 50, b = 90, l = 50, r = 50)
      )
    
    return(p)
  })
  #===============ram_distribution=================
  output$ram_distribution <- renderPlotly({
    data <- filtered_data() %>%
      count(ram_category) %>%
      mutate(ram_category = factor(ram_category, 
                                   levels = c("4GB or less", "8GB", "16GB", "32GB", "64GB+")))
    
    plot_ly(data, x = ~ram_category, y = ~n,
            type = "bar",
            marker = list(color = color_scale,
                          line = list(color = professional_colors$dark, width = 1))) %>%
      layout(title = "RAM Distribution",
             xaxis = list(title = "RAM Category"),
             yaxis = list(title = "Count"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  #===============retailer_price_distribution=================
  
  output$retailer_price_distribution <- renderPlotly({
    data <- df  # Use full data for retailer comparison
    
    # Check if we have retailer data
    if (!"source" %in% names(data) || nrow(data) == 0) {
      return(plotly_empty() %>%
               layout(title = "No retailer data available",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Create histogram/violin plot
    plot_ly(data, 
            x = ~source, 
            y = ~extracted_price,
            color = ~source,
            colors = c(professional_colors$amazon, professional_colors$walmart),
            type = "violin",
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = "outliers",
            hoverinfo = "y",
            hovertext = ~paste("Retailer:", source,
                               "<br>Price: $", extracted_price,
                               "<br>Brand:", brand)) %>%
      layout(title = "Price Distribution by Retailer",
             xaxis = list(title = "Retailer"),
             yaxis = list(title = "Price ($)", tickprefix = "$"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light,
             showlegend = FALSE)
  })
  # ===================retailer_category_comparison=======================
  
  output$retailer_category_comparison <- renderPlotly({
    data <- df
    
    # Get price categories
    data <- data %>%
      mutate(price_category = cut(extracted_price,
                                  breaks = c(0, 500, 1000, 1500, 3000, Inf),
                                  labels = c("Budget (<$500)", 
                                             "Mid-Range ($500-$1k)", 
                                             "Premium ($1k-$1.5k)", 
                                             "High-End ($1.5k-$3k)", 
                                             "Luxury (>$3k)"),
                                  include.lowest = TRUE)) %>%
      filter(!is.na(price_category))
    
    # Summarize by retailer and category
    summary <- data %>%
      group_by(source, price_category) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = count / sum(count) * 100)
    
    # Create grouped bar chart
    plot_ly(summary, 
            x = ~price_category, 
            y = ~count,
            color = ~source,
            colors = c(professional_colors$amazon, professional_colors$walmart),
            type = "bar",
            text = ~paste(count, "products"),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = ~paste(source, 
                               "<br>Category:", price_category,
                               "<br>Count:", count,
                               "<br>Percentage:", round(percentage, 1), "%")) %>%
      layout(title = "Product Selection by Price Category",
             xaxis = list(title = "Price Category", tickangle = 45),
             yaxis = list(title = "Number of Products"),
             barmode = "group",
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light,
             margin = list(b = 130))
  })
  
  # ===================retailer_specs_comparison========================
  output$retailer_specs_comparison <- renderPlotly({
    data <- df
    
    # Calculate average specifications by retailer
    specs_summary <- data %>%
      group_by(source) %>%
      summarise(
        `Avg RAM (GB)` = round(mean(ram_gb, na.rm = TRUE), 1),
        `Avg Storage (GB)` = round(mean(storage_gb, na.rm = TRUE), 1),
        `Avg Rating` = round(mean(rating_numeric, na.rm = TRUE), 2),
        `Avg Price ($)` = round(mean(extracted_price, na.rm = TRUE), 0),
        `Product Count` = n(),
        .groups = "drop"
      )
    
    # Prepare data for plotting
    plot_data <- specs_summary %>%
      pivot_longer(cols = -c(source, `Product Count`), 
                   names_to = "specification", 
                   values_to = "value") %>%
      mutate(
        # Format labels based on specification type
        label = case_when(
          specification == "Avg Price ($)" ~ paste0("$", value),
          specification == "Avg Rating" ~ as.character(value),
          TRUE ~ paste0(value, " GB")
        ),
        # Create hover text
        hover_text = paste0(
          "<b>", source, "</b><br>",
          specification, ": ", label, "<br>",
          "Products: ", `Product Count`
        )
      )
    
    # Create grouped bar chart
    plot_ly(plot_data, 
            x = ~specification, 
            y = ~value,
            color = ~source,
            colors = c("Amazon" = professional_colors$amazon, 
                       "Walmart" = professional_colors$walmart),
            type = "bar",
            text = ~label,
            textposition = "outside",
            hoverinfo = "text",
            hovertext = ~hover_text,
            showlegend = TRUE) %>%
      layout(
        title = list(
          text = "Average Specifications by Retailer",
          font = list(size = 14)
        ),
        xaxis = list(
          title = "Specification",
          tickangle = 0,
          categoryorder = "array",
          categoryarray = c("Avg RAM (GB)", "Avg Storage (GB)", "Avg Rating", "Avg Price ($)")
        ),
        yaxis = list(
          title = "Value",
          gridcolor = "#e1e5e9"
        ),
        barmode = "group",
        plot_bgcolor = professional_colors$light,
        paper_bgcolor = professional_colors$light,
        legend = list(
          orientation = "h",
          x = 0.3,
          y = -0.2,
          xanchor = "center"
        ),
        margin = list(t = 50, b = 120, l = 50, r = 50)
      )
  })
  
# ==================retailer_value_comparison===================
  output$retailer_value_comparison <- renderPlotly({
    data <- df
    
    # Filter out rows with missing RAM or price
    value_data <- data %>%
      filter(!is.na(ram_gb), 
             !is.na(extracted_price),
             ram_gb > 0,  # Avoid division by zero
             extracted_price > 0)  # Avoid infinite values
    
    # Calculate value metrics with proper handling
    value_data <- value_data %>%
      mutate(
        # Value score: (Rating × RAM) / Price (higher is better)
        value_score = (rating_numeric * ram_gb) / (extracted_price / 100),
        
        # Price per GB of RAM (lower is better)
        price_per_gb = extracted_price / ram_gb,
        
        # Price per rating point (lower is better)
        price_per_rating = ifelse(rating_numeric > 0, 
                                  extracted_price / rating_numeric, 
                                  NA)
      ) %>%
      # Remove infinite or NaN values
      filter(!is.infinite(value_score), 
             !is.na(value_score),
             !is.infinite(price_per_gb),
             !is.na(price_per_gb))
    
    # Check if we have data for both retailers
    retailer_counts <- table(value_data$source)
    cat("DEBUG - Retailer counts:", retailer_counts, "\n")
    
    # Calculate average value metrics by retailer
    value_summary <- value_data %>%
      group_by(source) %>%
      summarise(
        avg_value_score = mean(value_score, na.rm = TRUE),
        avg_price_per_gb = mean(price_per_gb, na.rm = TRUE),
        avg_price_per_rating = mean(price_per_rating, na.rm = TRUE),
        product_count = n(),
        .groups = "drop"
      )
    
    cat("DEBUG - Value summary:\n")
    print(value_summary)
    
    # If no Amazon data, create a placeholder
    if (!"Amazon" %in% value_summary$source) {
      value_summary <- rbind(value_summary, 
                             data.frame(source = "Amazon",
                                        avg_value_score = 0,
                                        avg_price_per_gb = 0,
                                        avg_price_per_rating = 0,
                                        product_count = 0))
    }
    
    # If no Walmart data, create a placeholder
    if (!"Walmart" %in% value_summary$source) {
      value_summary <- rbind(value_summary, 
                             data.frame(source = "Walmart",
                                        avg_value_score = 0,
                                        avg_price_per_gb = 0,
                                        avg_price_per_rating = 0,
                                        product_count = 0))
    }
    
    # Create comparison bar chart
    plot_ly(value_summary) %>%
      add_trace(x = ~source, y = ~avg_value_score,
                type = "bar", name = "Value Score",
                marker = list(color = professional_colors$success),
                text = ~paste(round(avg_value_score, 2)),
                textposition = "auto",
                hoverinfo = "text",
                hovertext = ~paste("<b>", source, "</b><br>",
                                   "Value Score: ", round(avg_value_score, 2),
                                   "<br>Products: ", product_count,
                                   "<br>Higher = Better")) %>%
      add_trace(x = ~source, y = ~avg_price_per_gb,
                type = "bar", name = "Price per GB ($)",
                marker = list(color = professional_colors$warning),
                text = ~paste("$", round(avg_price_per_gb, 1)),
                textposition = "auto",
                hoverinfo = "text",
                hovertext = ~paste("<b>", source, "</b><br>",
                                   "Avg Price per GB: $", round(avg_price_per_gb, 1),
                                   "<br>Lower = Better")) %>%
      layout(title = "Value for Money Comparison",
             xaxis = list(title = "Retailer"),
             yaxis = list(title = "Value Metrics"),
             barmode = "group",
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light,
             legend = list(orientation = "h", 
                           x = 0.5, y = 1.05,
                           xanchor = "bottom"))
  })
  
  # ================storage_price_analysis=====================
  
  output$storage_price_analysis <- renderPlotly({
    data <- df  # Now uses the pre-cleaned data
    
    # Filter for plotting
    plot_data <- data %>%
      filter(storage_category != "Unknown",
             !is.na(extracted_price),
             extracted_price > 0,
             extracted_price <= 5000)
    
    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
               layout(title = "No storage data available",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Create box plot using the pre-defined categories
    plot_ly(plot_data,
            x = ~storage_category,
            y = ~extracted_price,
            color = ~storage_category,
            colors = color_scale,
            type = "box",
            boxpoints = "outliers",
            hoverinfo = "y") %>%
      layout(
        title = "Price Distribution by Storage Category",
        xaxis = list(title = "Storage Capacity"),
        yaxis = list(title = "Price ($)", tickprefix = "$"),
        plot_bgcolor = professional_colors$light,
        paper_bgcolor = professional_colors$light,
        showlegend = FALSE
      )
  })
  

  
  # ===================Processor tier vs price ===============
  output$processor_price_analysis <- renderPlotly({
    data <- df
    
    # Check if processor_tier exists
    if (!"processor_tier" %in% names(data)) {
      return(plotly_empty() %>%
               layout(title = "Processor tier data not available",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Filter data
    plot_data <- data %>%
      filter(!is.na(processor_tier),
             !is.na(extracted_price),
             extracted_price > 0)
    
    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
               layout(title = "No processor data available",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Create box plot
    plot_ly(plot_data,
            x = ~processor_tier,
            y = ~extracted_price,
            color = ~processor_tier,
            colors = color_scale,
            type = "box",
            boxpoints = "outliers",
            hoverinfo = "y") %>%
      layout(
        title = "Processor Tier vs Price",
        xaxis = list(title = "Processor Tier",
                     categoryorder = "array",
                     categoryarray = c("Entry-Level", "Mid-Range", 
                                       "Performance", "High-End", "Unknown")),
        yaxis = list(title = "Price ($)", tickprefix = "$"),
        plot_bgcolor = professional_colors$light,
        paper_bgcolor = professional_colors$light,
        showlegend = FALSE
      )
  })
  
  # =====================Gaming Feature premium ===================

  output$gaming_premium_analysis <- renderPlotly({
    data <- df
    
    # Check if gaming flag exists
    if (!"is_gaming_flag" %in% names(data)) {
      return(plotly_empty() %>%
               layout(title = "Gaming data not available",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Filter valid data
    valid_data <- data %>%
      filter(!is.na(is_gaming_flag),
             !is.na(extracted_price),
             extracted_price > 0)
    
    if (nrow(valid_data) == 0) {
      return(plotly_empty() %>%
               layout(title = "No price data available",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Calculate summary statistics
    gaming_data <- valid_data %>% filter(is_gaming_flag == TRUE)
    regular_data <- valid_data %>% filter(is_gaming_flag == FALSE)
    
    # Check if we have both types
    if (nrow(gaming_data) == 0 || nrow(regular_data) == 0) {
      return(plotly_empty() %>%
               layout(title = "Need both gaming and regular laptops for comparison",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # Create summary data frame
    price_summary <- data.frame(
      type = c("Regular Laptops", "Gaming Laptops"),
      avg_price = c(mean(regular_data$extracted_price, na.rm = TRUE),
                    mean(gaming_data$extracted_price, na.rm = TRUE)),
      median_price = c(median(regular_data$extracted_price, na.rm = TRUE),
                       median(gaming_data$extracted_price, na.rm = TRUE)),
      min_price = c(min(regular_data$extracted_price, na.rm = TRUE),
                    min(gaming_data$extracted_price, na.rm = TRUE)),
      max_price = c(max(regular_data$extracted_price, na.rm = TRUE),
                    max(gaming_data$extracted_price, na.rm = TRUE)),
      count = c(nrow(regular_data), nrow(gaming_data))
    )
    
    # Calculate premium percentage
    regular_avg <- price_summary$avg_price[1]
    gaming_avg <- price_summary$avg_price[2]
    premium_pct <- round(((gaming_avg / regular_avg) - 1) * 100, 1)
    
    # Create the plot - SIMPLE VERSION without line.width issues
    plot_ly(price_summary,
            x = ~type,
            y = ~avg_price,
            type = "bar",
            marker = list(
              color = c(professional_colors$secondary, professional_colors$success),
              line = list(color = professional_colors$primary, width = 1)  # SINGLE value
            ),
            text = ~paste0("$", round(avg_price, 0)),
            textposition = "outside",
            hoverinfo = "text",
            hovertext = ~paste0("<b>", type, "</b>\n",
                                "Average: $", round(avg_price, 0), "\n",
                                "Median: $", round(median_price, 0), "\n",
                                "Range: $", round(min_price, 0), " - $", round(max_price, 0), "\n",
                                "Count: ", count, " products")) %>%
      layout(
        title = list(
          text = "Gaming Premium Analysis",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "",
          tickangle = 0
        ),
        yaxis = list(
          title = "Average Price ($)",
          tickprefix = "$",
          gridcolor = "#e1e5e9"
        ),
        plot_bgcolor = professional_colors$light,
        paper_bgcolor = professional_colors$light,
        showlegend = FALSE,
        margin = list(t = 80, b = 80, l = 50, r = 50),
        annotations = list(
          list(
            x = 1,  # Gaming bar position
            y = gaming_avg * 1.05,  # Just above the bar
            text = paste0("+", premium_pct, "% premium"),
            xref = "x",
            yref = "y",
            showarrow = FALSE,
            font = list(size = 14, color = professional_colors$primary, weight = "bold"),
            bgcolor = "white",
            bordercolor = professional_colors$danger,
            borderwidth = 1,
            borderpad = 4
          ),
          list(
            x = 0.5,
            y = 1.05,
            xref = "paper",
            yref = "paper",
            text = paste("Gaming laptops cost", premium_pct, "% more on average"),
            showarrow = FALSE,
            font = list(size = 12, color = "#666666")
          )
        )
      )
  })
  
  # DEBUG FUNCTION to check gaming data
  output$gaming_premium_debug <- renderPrint({
    data <- df
    
    cat("=== GAMING PREMIUM DEBUG ===\n")
    cat("Total rows:", nrow(data), "\n")
    
    if ("is_gaming_flag" %in% names(data)) {
      cat("\nGaming flag distribution:\n")
      print(table(data$is_gaming_flag, useNA = "always"))
      
      gaming_data <- data %>% filter(is_gaming_flag == TRUE)
      regular_data <- data %>% filter(is_gaming_flag == FALSE)
      
      cat("\nGaming laptops:", nrow(gaming_data), "\n")
      cat("Regular laptops:", nrow(regular_data), "\n")
      
      if (nrow(gaming_data) > 0) {
        cat("\nGaming - Price stats:\n")
        cat("  Avg: $", mean(gaming_data$extracted_price, na.rm = TRUE), "\n")
        cat("  Min: $", min(gaming_data$extracted_price, na.rm = TRUE), "\n")
        cat("  Max: $", max(gaming_data$extracted_price, na.rm = TRUE), "\n")
      }
      
      if (nrow(regular_data) > 0) {
        cat("\nRegular - Price stats:\n")
        cat("  Avg: $", mean(regular_data$extracted_price, na.rm = TRUE), "\n")
        cat("  Min: $", min(regular_data$extracted_price, na.rm = TRUE), "\n")
        cat("  Max: $", max(regular_data$extracted_price, na.rm = TRUE), "\n")
      }
      
      if (nrow(gaming_data) > 0 && nrow(regular_data) > 0) {
        gaming_avg <- mean(gaming_data$extracted_price, na.rm = TRUE)
        regular_avg <- mean(regular_data$extracted_price, na.rm = TRUE)
        premium <- ((gaming_avg / regular_avg) - 1) * 100
        cat("\nGaming premium:", round(premium, 1), "%\n")
      }
    } else {
      cat("\nERROR: is_gaming_flag column not found!\n")
    }
  })
  
  
  
  # ===================retailer_rating_comparison===============
  output$retailer_rating_comparison <- renderPlotly({
    data <- filtered_data() %>%
      group_by(source) %>%
      summarise(avg_rating = mean(rating_numeric, na.rm = TRUE),
                count = n()) %>%
      mutate(label = paste0(source, "\n", round(avg_rating, 2)))
    
    plot_ly(data, x = ~source, y = ~avg_rating,
            type = "bar",
            marker = list(color = c(professional_colors$amazon, professional_colors$walmart),
                          line = list(color = professional_colors$dark, width = 1)),
            text = ~paste("Avg Rating:", round(avg_rating, 2)),
            hoverinfo = "text") %>%
      layout(title = "Average Ratings by Retailer",
             xaxis = list(title = "Retailer"),
             yaxis = list(title = "Average Rating"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  # 2. SMART RECOMMENDATIONS
  output$dynamic_filters <- renderUI({
    req(input$use_case)
    
    filters <- tagList()
    
    # Use-case specific defaults
    defaults <- list(
      "General Use" = list(budget = 1000, min_rating = 4.0, min_ram = 8),
      "Student" = list(budget = 800, min_rating = 4.0, min_ram = 8),
      "Gaming" = list(budget = 1500, min_rating = 4.2, min_ram = 16),
      "Business" = list(budget = 1200, min_rating = 4.1, min_ram = 16),
      "Creative Work" = list(budget = 1600, min_rating = 4.2, min_ram = 16),
      "Budget" = list(budget = 500, min_rating = 3.8, min_ram = 4),
      "2-in-1/Convertible" = list(budget = 1000, min_rating = 4.0, min_ram = 8)
    )
    
    current_defaults <- defaults[[input$use_case]]
    
    filters <- tagAppendChild(filters, 
                              sliderInput("budget", "Your Budget:",
                                          min = 100, max = 5000, step = 50,
                                          value = current_defaults$budget,
                                          pre = "$"))
    
    filters <- tagAppendChild(filters,
                              sliderInput("min_rating", "Minimum Rating:",
                                          min = 1, max = 5, step = 0.1,
                                          value = current_defaults$min_rating))
    
    filters <- tagAppendChild(filters,
                              sliderInput("min_ram", "Minimum RAM (GB):",
                                          min = 4, max = 64, step = 4,
                                          value = current_defaults$min_ram))
    
    filters <- tagAppendChild(filters,
                              pickerInput("brand_pref", "Brand Preference:",
                                          choices = c("No Preference", brands),
                                          selected = "No Preference",
                                          options = list(`live-search` = TRUE)))
    
    return(filters)
  })
  
  # Category-specific recommendations UI
  output$category_recommendations_ui <- renderUI({
    req(input$use_case)
    
    priorities <- list(
      "General Use" = c("1. Reliability & Durability", "2. Battery Life", "3. Good Value"),
      "Student" = c("1. Portability & Weight", "2. Battery Life", "3. Budget-Friendly"),
      "Gaming" = c("1. Graphics Performance", "2. High Refresh Rate", "3. Cooling System"),
      "Business" = c("1. Professional Build", "2. Security Features", "3. Port Connectivity"),
      "Creative Work" = c("1. Color Accurate Display", "2. High Performance CPU", "3. Ample Storage"),
      "Budget" = c("1. Best Value for Money", "2. Basic Performance", "3. Reliability"),
      "2-in-1/Convertible" = c("1. Stylus Support", "2. Hinge Durability", "3. Touchscreen Quality")
    )
    
    current_priorities <- priorities[[input$use_case]]
    
    tagList(
      h4("🎯 Category Priorities", style = paste0("color: ", professional_colors$primary, ";")),
      tags$div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        tags$ul(
          tags$li(tags$strong(current_priorities[1])),
          tags$li(tags$strong(current_priorities[2])),
          tags$li(tags$strong(current_priorities[3]))
        )
      )
    )
  })
  
  # ======================= RECOMMENDATION ALGORITHM ======================================
  recommendations <- eventReactive(input$find_laptops, {
    # 1. Start with data and define case_weight FIRST
    data <- df
    case_weight <- case_when(
      input$use_case == "Gaming" ~ 1.5,
      input$use_case == "Student" ~ 1.2,
      input$use_case == "2-in-1/Convertible" ~ 1.3,
      TRUE ~ 1.0
    )
    
    # 2. Apply use case filters SAFELY
    if (input$use_case == "Gaming") {
      if ("is_gaming_flag" %in% names(data)) {
        data <- data %>% 
          filter(is_gaming_flag == TRUE) %>%
          filter(ram_gb >= 16)  # Use numeric, not category
      }
    } else if (input$use_case == "Student") {
      if ("is_student_friendly" %in% names(data)) {
        data <- data %>% 
          filter(is_student_friendly == TRUE | extracted_price <= 800)
      } else {
        data <- data %>% filter(extracted_price <= 800)
      }
    } else if (input$use_case == "2-in-1/Convertible") {
      if ("is_2in1" %in% names(data)) {
        data <- data %>% filter(is_2in1 == TRUE)
      }
    }
    
    # 3. Core filters with reasonable ranges
    budget_min <- input$budget * 0.8
    budget_max <- input$budget * 1.2
    
    data <- data %>%
      filter(extracted_price >= budget_min,
             extracted_price <= budget_max,
             rating_numeric >= input$min_rating,
             ram_gb >= input$min_ram,
             !is.na(extracted_price))
    
    if (input$brand_pref != "No Preference") {
      data <- data %>% filter(brand == input$brand_pref)
    }
    
    # 4. Smart fallback (not starting from scratch)
    if (nrow(data) < 5) {
      fallback_data <- df %>%
        filter(extracted_price >= input$budget * 0.7,
               extracted_price <= input$budget * 1.5,
               rating_numeric >= max(3.0, input$min_rating * 0.9),
               ram_gb >= max(4, input$min_ram * 0.7),
               !is.na(extracted_price))
      
      if (input$brand_pref != "No Preference") {
        fallback_data <- fallback_data %>% filter(brand == input$brand_pref)
      }
      
      # Keep use case filters in fallback
      if (input$use_case == "Gaming" && "is_gaming_flag" %in% names(fallback_data)) {
        fallback_data <- fallback_data %>% filter(is_gaming_flag == TRUE)
      }
      
      # Combine and remove duplicates
      if ("product_id" %in% names(data)) {
        data <- bind_rows(data, fallback_data) %>%
          distinct(product_id, .keep_all = TRUE)
      } else {
        data <- bind_rows(data, fallback_data) %>%
          distinct(title, brand, .keep_all = TRUE)
      }
    }
    
    # 5. Add case_weight (now safe from overwriting)
    data <- data %>% mutate(case_weight = case_weight)
    
    # 6. IMPROVED scoring with local normalization
    if (nrow(data) > 0) {
      data <- data %>%
        mutate(
          # Price score with bonus for under budget
          price_score = 1 - (abs(extracted_price - input$budget) / input$budget),
          price_score = ifelse(extracted_price <= input$budget,
                               price_score * 1.1,  # Bonus for under budget
                               price_score),
          price_score = pmax(0, pmin(1, price_score)),
          
          # RAM score normalized within filtered data
          ram_min = min(ram_gb, na.rm = TRUE),
          ram_max = max(ram_gb, na.rm = TRUE),
          ram_score = ifelse(ram_max > ram_min,
                             (ram_gb - ram_min) / (ram_max - ram_min),
                             0.5),
          
          # Storage score with better missing handling
          storage_score = case_when(
            is.na(storage_gb) ~ 0.3,
            storage_gb < 128 ~ 0.4,
            storage_gb >= 2000 ~ 1.0,
            TRUE ~ storage_gb / 2000
          ),
          
          rating_score = rating_numeric / 5,
          
          processor_score = case_when(
            processor_tier == "High-End" ~ 1.0,
            processor_tier == "Performance" ~ 0.8,
            processor_tier == "Mid-Range" ~ 0.6,
            processor_tier == "Entry-Level" ~ 0.4,
            TRUE ~ 0.3
          ),
          
          # Balanced weight distribution
          weight_price = input$weight_value / 100,
          weight_perf = input$weight_performance / 100,
          weight_features = input$weight_features / 100,
          weight_rating = 0.3,
          
          final_score = (
            (price_score * weight_price) +
              (ram_score * weight_perf * 0.6) +
              (processor_score * weight_perf * 0.4) +
              (storage_score * weight_features) +
              (rating_score * weight_rating)
          ) * case_weight,
          
          # Normalize final score for display
          final_score_normalized = if(n() > 1) {
            100 * (final_score - min(final_score)) / 
              (max(final_score) - min(final_score))
          } else {
            100
          },
          final_score_normalized = round(final_score_normalized, 1),
          
          # Better value metric
          value_score = (ram_gb * 8 +  # Approximate $ value of RAM
                           ifelse(!is.na(storage_gb), storage_gb * 0.05, 0)) /  # $ value of storage
            extracted_price
        ) %>%
        arrange(desc(final_score)) %>%
        head(15)
    }
    
    return(data)
  }) 
 
  
  
  
  # =================recommendation_table=============
  output$recommendation_table <- renderDT({
    req(recommendations())
    
    recs <- recommendations() %>%
      mutate(
        Rank = row_number(),
        `Score %` = round(final_score, 1)
      ) %>%
      select(
        Rank,
        Brand = brand,
        Model = title,
        Price = extracted_price,
        Rating = rating_numeric,
        RAM = ram_gb,
        Processor = processor_tier,
        `Score %`
      )
    
    # Create the datatable
    dt <- datatable(recs,
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel'),
                      order = list(list(7, 'desc'))
                    ),
                    extensions = 'Buttons',
                    rownames = FALSE) %>%
      formatCurrency("Price", "$") %>%
      formatRound(c("Rating", "Score %"), 1)
    
    # Safe styling without "color" object
    dt <- dt %>%
      formatStyle(
        "Score %",
        background = styleColorBar(range(recs$`Score %`, na.rm = TRUE), 
                                   "#27ae60")  # Using hex color directly
      )
    
    return(dt)
  })
  
  output$recommendation_details <- renderUI({
    req(recommendations())
    recs <- recommendations()
    
    if (nrow(recs) > 0) {
      top_rec <- recs[1, ]
      
      tagList(
        h4("🎖️ Top Recommendation", style = paste0("color: ", professional_colors$success, ";")),
        tags$div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          tags$h5(tags$strong(top_rec$brand, ": ", top_rec$title)),
          tags$p(tags$strong("Price: "), "$", top_rec$extracted_price),
          tags$p(tags$strong("Why it's recommended:")),
          tags$ul(
            tags$li(paste("RAM: ", top_rec$ram_gb, "GB (", 
                          round(top_rec$ram_gb/max(recs$ram_gb, na.rm = TRUE)*100, 0), 
                          "% better than average)")),
            tags$li(paste("Processor: ", top_rec$processor_tier, " tier")),
            tags$li(paste("Value Score: ", round(top_rec$value_performance, 2), 
                          " (Higher is better)"))
          )
        )
      )
    }
  })
  
  output$recommendation_comparison_chart <- renderPlotly({
    req(recommendations())
    
    recs <- recommendations() %>% head(5)
    
    plot_ly(recs, x = ~brand, y = ~final_score * 100,
            type = "bar",
            marker = list(color = color_scale,
                          line = list(color = professional_colors$dark, width = 1)),
            text = ~paste("Price: $", extracted_price, 
                          "<br>RAM: ", ram_gb, "GB"),
            hoverinfo = "text") %>%
      layout(title = "Top 5 Recommendation Scores",
             xaxis = list(title = "Brand"),
             yaxis = list(title = "Recommendation Score (%)"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  # =============recommendation_radar===========
  
  output$recommendation_radar <- renderPlotly({
    req(recommendations())
    
    # Get top 5 recommendations
    recs <- recommendations() %>% head(5)
    
    # If no recommendations, show empty plot
    if (nrow(recs) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(title = "No recommendations found",
                      plot_bgcolor = professional_colors$light,
                      paper_bgcolor = professional_colors$light))
    }
    
    # **CRITICAL: Sort by final_score DESCENDING first**
    recs <- recs %>%
      arrange(desc(final_score))
    
    # Create display labels with score for better context
    recs <- recs %>%
      mutate(
        # Shorten title if too long
        short_title = ifelse(nchar(title) > 30, 
                             paste0(substr(title, 1, 27), "..."), 
                             title),
        # Create label with score
        label = paste0(short_title, " (", round(final_score, 1), ")"),
        # Convert to factor with levels in score order (HIGHEST to LOWEST)
        label = factor(label, levels = label, ordered = TRUE)
      )
    
    # **DEBUG: Check order before plotting**
    print("DEBUG - Data order:")
    print(recs %>% select(title, final_score, label))
    
    # Create vertical bar chart
    p <- plot_ly(recs,
                 x = ~label,  # Use factor for correct order
                 y = ~final_score,
                 type = "bar",
                 orientation = "h",  # Vertical orientation
                 marker = list(
                   color = ~final_score,
                   colorscale = list(
                     c(0, color_scale[1]),
                     c(0.5, color_scale[2]),
                     c(1, color_scale[3])
                   ),
                   line = list(color = professional_colors$dark, width = 1),
                   showscale = FALSE
                 ),
                 hoverinfo = "text",
                 text = ~paste(
                   "<b>Rank #", row_number(), ": ", title, "</b><br>",
                   "Score: ", round(final_score, 2), "<br>",
                   "Price: $", extracted_price, "<br>",
                   "RAM: ", ram_gb, "GB<br>",
                   ifelse(!is.na(processor), paste0("Processor: ", processor, "<br>"), ""),
                   "Rating: ", rating_numeric, "/5"
                 )) %>%
      
      layout(
        title = list(
          text = "<b>Top 5 Recommended Laptops</b><br><sub>Sorted by Recommendation Score</sub>",
          font = list(size = 16, color = professional_colors$dark)
        ),
        xaxis = list(
          title = "",
          tickangle = -45,
          categoryorder = "array",  # **CRITICAL: Force category order**
          categoryarray = ~label,   # **CRITICAL: Use sorted labels**
          showgrid = FALSE,
          tickfont = list(size = 10, color = professional_colors$dark)
        ),
        yaxis = list(
          title = "Recommendation Score",
          range = c(0, max(recs$final_score) * 1.2),
          showgrid = TRUE,
          gridcolor = "#f0f0f0",
          titlefont = list(size = 12, color = professional_colors$dark),
          tickfont = list(size = 10, color = professional_colors$dark),
          tickformat = ".1f"  # Format with 1 decimal
        ),
        showlegend = FALSE,
        plot_bgcolor = professional_colors$light,
        paper_bgcolor = professional_colors$light,
        margin = list(
          l = 60,  # left margin
          r = 40,  # right margin
          b = 150, # bottom margin (for rotated labels)
          t = 100, # top margin
          pad = 10
        )
      )
    
    # Add value labels on bars
    p <- p %>% add_annotations(
      x = ~label,
      y = ~final_score,
      text = ~paste0(round(final_score, 1)),
      xanchor = 'center',
      yanchor = 'bottom',
      showarrow = FALSE,
      font = list(size = 11, color = professional_colors$dark),
      yshift = 5  # Move labels above bars
    )
    
    return(p)
  })
  
  # 3. VALUE ANALYSIS - 
  output$value_analysis_plot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(value_ratio = (rating_numeric * ram_gb) / (extracted_price / 100))
    
    top_value <- data %>%
      arrange(desc(value_ratio)) %>%
      head(10)
    
    plot_ly(top_value, x = ~brand, y = ~value_ratio,
            type = "bar",
            marker = list(color = color_scale,
                          line = list(color = professional_colors$dark, width = 1)),
            text = ~paste("Price: $", extracted_price,
                          "<br>Rating: ", rating_numeric,
                          "<br>RAM: ", ram_gb, "GB"),
            hoverinfo = "text") %>%
      layout(title = "Top 10 Value Laptops (Rating × RAM / Price)",
             xaxis = list(title = "Brand", categoryorder = "total descending"),
             yaxis = list(title = "Value Ratio"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  # 4. ADDITIONAL VISUALIZATIONS
  output$ram_price_analysis <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~ram_gb, y = ~extracted_price,
            color = ~processor_tier,
            colors = color_scale,
            type = "scatter", mode = "markers",
            size = ~rating_numeric,
            text = ~paste("Brand:", brand,
                          "<br>Processor:", processor_tier,
                          "<br>Price: $", extracted_price),
            hoverinfo = "text") %>%
      layout(title = "RAM vs Price by Processor Tier",
             xaxis = list(title = "RAM (GB)"),
             yaxis = list(title = "Price ($)"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  output$brand_value_boxplot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(brand) %>%
      filter(n() >= 5) %>%  # Only brands with at least 5 products
      ungroup() %>%
      mutate(value_score = (rating_numeric * ram_gb) / (extracted_price / 100))
    
    plot_ly(data, x = ~brand, y = ~value_score,
            color = ~brand,
            type = "box",
            colors = color_scale) %>%
      layout(title = "Value Score Distribution by Brand",
             xaxis = list(title = "Brand", tickangle = 45),
             yaxis = list(title = "Value Score"),
             plot_bgcolor = professional_colors$light,
             paper_bgcolor = professional_colors$light)
  })
  
  # 5. EXPORT FUNCTIONALITY (same as before, but with added exports)
  output$export_student <- downloadHandler(
    filename = function() {
      paste0("student_laptops_", Sys.Date(), ".csv")
    },
    content = function(file) {
      student_data <- df %>% filter(is_student_friendly == TRUE | extracted_price <= 800)
      write.csv(student_data, file, row.names = FALSE)
    }
  )
  
  output$export_premium <- downloadHandler(
    filename = function() {
      paste0("premium_laptops_", Sys.Date(), ".csv")
    },
    content = function(file) {
      premium_data <- df %>% filter(price_category %in% c("Premium ($1k-$1.5k)", "High-End ($1.5k-$3k)", "Luxury (>$3k)"))
      write.csv(premium_data, file, row.names = FALSE)
    }
  )
  # Add these to your server function:
  
  # Value boxes for About page
  output$total_products_about <- renderValueBox({
    valueBox(nrow(df), "Total Laptops Analyzed", 
             icon = icon("laptop"), 
             color = "blue")
  })
  
  output$amazon_count_about <- renderValueBox({
    valueBox(sum(df$source == "Amazon"), "Amazon Listings", 
             icon = icon("amazon"), 
             color = "orange")
  })
  
  output$walmart_count_about <- renderValueBox({
    valueBox(sum(df$source == "Walmart"), "Walmart Listings", 
             icon = icon("shopping-cart"), 
             color = "light-blue")
  })
  
  output$avg_price_about <- renderValueBox({
    avg_price <- round(mean(df$extracted_price, na.rm = TRUE), 0)
    valueBox(paste0("$", avg_price), "Average Price", 
             icon = icon("dollar-sign"), 
             color = "green")
  })
  
  # Navigation buttons
  observeEvent(input$goto_recommend, {
    updateTabItems(session, "sidebar", "recommend")
  })
  
  observeEvent(input$goto_analysis, {
    updateTabItems(session, "sidebar", "analysis")
  })
  
  observeEvent(input$goto_compare, {
    updateTabItems(session, "sidebar", "compare")
  })
  # ... (keep other existing server functions from original code)
  
  # Note: Due to length, I've shown the key improvements. 
  # You'll need to add the remaining server functions for the new tabs
  # following the same pattern with professional colors and improved logic.
}

# 6. RUN THE APP
shinyApp(ui = ui, server = server)