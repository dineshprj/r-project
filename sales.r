library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(DT)
library(tidyr)
library(lubridate)
library(plotly)

# Sample sales data
sales_data <- data.frame(
  Product = c("Product A", "Product B", "Product C"),
  Sales = c(1000, 500, 1200),
  Region = c("North", "South", "East"),
  Date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
)

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Product Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Add Product", tabName = "add_product", icon = icon("plus-circle")),
      menuItem("Sales Data", tabName = "sales_data", icon = icon("table")),
      menuItem("Export Data", tabName = "export_data", icon = icon("file-export")),
      menuItem("User Settings", tabName = "user_settings", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales", width = 3),
                valueBoxOutput("total_products", width = 3),
                valueBoxOutput("total_regions", width = 3),
                valueBoxOutput("forecasted_sales", width = 3)
              ),
              fluidRow(
                box(title = "Sales by Product", width = 12, status = "primary", solidHeader = TRUE, 
                    plotOutput("product_sales_plot"))
              ),
              fluidRow(
                box(title = "Sales Trends", width = 12, status = "primary", solidHeader = TRUE, 
                    plotlyOutput("sales_trend_plot"))
              )
      ),
      tabItem(tabName = "add_product",
              fluidRow(
                box(title = "Add or Edit Product", status = "primary", width = 12, solidHeader = TRUE,
                    textInput("product_name", "Product Name"),
                    numericInput("product_sales", "Sales Value", value = 0, min = 0),
                    selectInput("product_region", "Region", choices = c("North", "South", "East", "West")),
                    dateInput("product_date", "Date", value = Sys.Date()),
                    actionButton("save_product_button", "Save Product", class = "btn-success"))
              )
      ),
      tabItem(tabName = "sales_data",
              fluidRow(
                DTOutput("sales_table"),
                actionButton("edit_product_button", "Edit Selected Product", class = "btn-primary"),
                actionButton("delete_product_button", "Delete Selected Product", class = "btn-danger")
              )
      ),
      tabItem(tabName = "export_data",
              fluidRow(
                downloadButton("download_data", "Download Sales Data (CSV)"),
                downloadButton("download_pdf", "Download Dashboard as PDF")
              )
      ),
      tabItem(tabName = "user_settings",
              fluidRow(
                box(title = "User Preferences", width = 12, status = "warning", solidHeader = TRUE,
                    textInput("user_name", "Enter your name:", value = "Dinesh Prajapati"),  # Pre-fill with name
                    actionButton("update_greeting", "Update Greeting"),
                    checkboxInput("dark_mode", "Enable Dark Mode", value = FALSE)
                ),
                box(title = "Welcome", width = 12, status = "success", solidHeader = TRUE,
                    textOutput("greeting_message")  # Display personalized greeting message
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive data frame to store product sales data
  sales_data_reactive <- reactiveVal(sales_data)
  
  # Render total sales value
  output$total_sales <- renderValueBox({
    total_sales <- sum(sales_data_reactive()$Sales)
    valueBox(format(total_sales, big.mark = ","), "Total Sales", icon = icon("usd"), color = "green")
  })
  
  # Render total number of products
  output$total_products <- renderValueBox({
    total_products <- nrow(sales_data_reactive())
    valueBox(total_products, "Products Sold", icon = icon("cube"), color = "blue")
  })
  
  # Render total number of regions
  output$total_regions <- renderValueBox({
    total_regions <- length(unique(sales_data_reactive()$Region))
    valueBox(total_regions, "Regions Covered", icon = icon("map"), color = "purple")
  })
  
  # Render forecasted sales (basic prediction example)
  output$forecasted_sales <- renderValueBox({
    forecasted_sales <- sum(sales_data_reactive()$Sales) * 1.1  # simple 10% increase forecast
    valueBox(format(forecasted_sales, big.mark = ","), "Forecasted Sales", icon = icon("chart-line"), color = "orange")
  })
  
  # Render Product Sales Plot
  output$product_sales_plot <- renderPlot({
    sales_data_reactive() %>%
      group_by(Product) %>%
      summarise(Total_Sales = sum(Sales)) %>%
      ggplot(aes(x = Product, y = Total_Sales, fill = Product)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Sales by Product", x = "Product", y = "Total Sales") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render Sales Trends Plot with professional enhancements
  output$sales_trend_plot <- renderPlotly({
    sales_data_reactive() %>%
      ggplot(aes(x = Date, y = Sales, color = Product, text = paste("Product: ", Product, "<br>Sales: ", Sales))) +
      geom_point(size = 3, alpha = 0.7) +  # Scatter plot for better point visibility
      geom_smooth(method = "loess", se = FALSE, aes(color = Product), size = 1.5, linetype = "solid") +  # Trend line (LOESS smooth)
      scale_color_manual(values = c("Product A" = "#FF5733", "Product B" = "#33FF57", "Product C" = "#3357FF")) +  # Custom color palette
      labs(title = "Sales Trends Over Time", x = "Date", y = "Sales") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      ) +
      theme(legend.title = element_blank(), legend.position = "bottom") -> p
    
    # Convert ggplot to plotly for interactive tooltip
    ggplotly(p, tooltip = "text")
  })
  
  # Add or Edit product to the dataset
  observeEvent(input$save_product_button, {
    # Get selected row if editing
    selected_row <- input$sales_table_rows_selected
    
    # Create new product data
    new_product <- data.frame(
      Product = input$product_name,
      Sales = input$product_sales,
      Region = input$product_region,
      Date = input$product_date
    )
    
    if (length(selected_row) > 0) {
      # Edit the selected row
      updated_data <- sales_data_reactive()
      updated_data[selected_row, ] <- new_product
      sales_data_reactive(updated_data)
    } else {
      # Add a new product if no row is selected
      updated_data <- rbind(sales_data_reactive(), new_product)
      sales_data_reactive(updated_data)
    }
  })
  
  # Render sales data table with edit and delete options
  output$sales_table <- renderDT({
    datatable(sales_data_reactive(), selection = 'single')
  })
  
  # Edit the selected product
  observeEvent(input$edit_product_button, {
    selected_row <- input$sales_table_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- sales_data_reactive()[selected_row, ]
      updateTextInput(session, "product_name", value = selected_data$Product)
      updateNumericInput(session, "product_sales", value = selected_data$Sales)
      updateSelectInput(session, "product_region", selected = selected_data$Region)
      updateDateInput(session, "product_date", value = selected_data$Date)
    } else {
      shinyjs::alert("Please select a product to edit")
    }
  })
  
  # Delete the selected product
  observeEvent(input$delete_product_button, {
    selected_row <- input$sales_table_rows_selected
    if (length(selected_row) > 0) {
      updated_data <- sales_data_reactive()[-selected_row, ]
      sales_data_reactive(updated_data)
    } else {
      shinyjs::alert("Please select a product to delete")
    }
  })
  
  # Download CSV data
  output$download_data <- downloadHandler(
    filename = function() { paste("sales_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(sales_data_reactive(), file, row.names = FALSE)
    }
  )
  
  # Download PDF of the dashboard (simple example, can be enhanced further)
  output$download_pdf <- downloadHandler(
    filename = function() { paste("sales_dashboard_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      pdf(file)
      plot(sales_data_reactive()$Sales, type = "b", main = "Sales Data Overview")
      dev.off()
    }
  )
  
  # Render personalized greeting message
  observeEvent(input$update_greeting, {
    updated_name <- input$user_name
    output$greeting_message <- renderText({
      paste("Hello,", updated_name, "Welcome to the Product Sales Dashboard!")
    })
  })
  
}

# Run the application
shinyApp(ui, server)
