
library(rsconnect)
rsconnect::setAccountInfo(name='nikithabalaji', token='D4FC68DF31928EA753F996DD91A37624', secret='nQUkWr+HBsh6G4F73t9Xu3Uy1BXr97bdY2+ZgURA')
# Load necessary libraries
library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr)
library(mice)
library(caret)

data<- read_excel("Window_Manufacturing.xlsx")

realBlanks<- c("Window Type", "Glass Supplier", "Glass Supplier Location")
for (field in realBlanks) {
  data[[field]][is.na(data[[field]])] <- "NA"
}

#coercing columns
data$`Window Type`<- as.factor(data$`Window Type`)
data$`Glass Supplier`<- as.factor(data$`Glass Supplier`)
data$`Glass Supplier Location`<- as.factor(data$`Glass Supplier Location`)

colnames(data)<- c("Breakage_Rate", "Window_Size", "Glass_Thickness","Ambient_Temp", "Cut_Speed", "Edge_Deletion_Rate", 
                   "Spacer_Distance", "Window_Color", "Window_Type", "Glass_Supplier", "Silicon_Viscosity", "Glass_Supplier_Location")

#fill in empty values
imputedValues <- mice(data=data
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)

# impute the missing values in our tr data.frame
data <- mice::complete(imputedValues,1) # completely fills in the missing

window_data<- data

# dummy variables
dummies <- dummyVars(Breakage_Rate ~ ., data )  # create dummies for Xs
d_encoded <- data.frame(predict(dummies, newdata = data)) # actually creates the dummies
names(d_encoded) <- gsub("\\.", "", names(d_encoded))          # removes dots from col names
data<- cbind(data$Breakage_Rate, d_encoded)       
names(data)[1]<- "Breakage_Rate"

# Compute the correlation matrix
corr_matrix <- cor(data, use = "pairwise.complete.obs")

#Find correlated variables with correlation higher than 80% (or lower than -80%)
high_corr <- findCorrelation(corr_matrix, cutoff = 0.8)
high_corr
# Remove the highly correlated variables from the dataset
data <- data[, -high_corr]

#remove linear combos
Breakage_Rate <- data$Breakage_Rate

# create a column of 1s. This will help identify all the right linear combos
data <- cbind(rep(1, nrow(data)), data[2:ncol(data)])
names(data)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(data)
comboInfo

# remove columns identified that led to linear combos
data <- data[, -comboInfo$remove]

# remove the "ones" column in the first column
data <- data[, c(2:ncol(data))]

# Add the target variable back to our data.frame
data <- cbind(Breakage_Rate, data)

nzv <- nearZeroVar(data, saveMetrics = TRUE)
head(nzv, 20)
data$Window_TypeNA<-NULL
data$Glass_Supplier_LocationNA<- NULL

pre_proc_d <- preProcess(data, method = c("range"))
normalized_data <- predict(pre_proc_d, newdata = data)
not_significant= c("Spacer_Distance", "Glass_SupplierSupplierD", "Glass_Supplier_LocationIowa", "Glass_Supplier_LocationMichigan", "Glass_Supplier_LocationMinnesota")
normalized_data <- normalized_data[, !colnames(normalized_data) %in% not_significant]

inTrain <- createDataPartition(y = normalized_data$Breakage_Rate,   # outcome variable
                               p = .80,   # % of training data you want
                               list = F)
# create your partitions
train <- normalized_data[inTrain,]  # training data set
test <- normalized_data[-inTrain,]  # test data set

lm_model <- lm(Breakage_Rate ~ ., data = train)
summary(lm_model)
predicted_values <- predict(lm_model, newdata = train)

# Root Mean Square Error (RMSE) calculation
rmse_calculation <- function(observed, forecasted) {
  sqrt(mean((observed - forecasted)^2))
}

normalize <- function(data, min_val, max_val) {
  return((data - min_val) / (max_val - min_val))
}

denormalize <- function(normalized_value, min_value, max_value) {
  original_value <- normalized_value * (max_value - min_value) + min_value
  return(original_value)
}


# Enhanced UI for the Shiny app
ui <- dashboardPage(
  skin = "blue", # Adjust the theme color
  dashboardHeader(title = "Window Manufacturing Optimization", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Descriptive Analytics", tabName = "descriptive", icon = icon("bar-chart")),
      menuItem("Predictive Analytics", tabName = "predictive", icon = icon("chart-line")),
      menuItem("Prescriptive Analytics", tabName = "prescriptive", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box { height: 120px; }
        .content-wrapper { background-color: #f7f7f7; }
        .box-title { font-size: 20px; font-weight: bold; }
      "))
    ),
    
    tabItems(
      
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Business Problem: Reducing Window Breakage Rate in Manufacturing Process", status = "primary", solidHeader = TRUE, width = 12,
                  p("The business problem revolves around a manufacturing process where glass windows are produced for 
                    commercial purposes. In recent times, the company has experienced a significant increase in window 
                    breakage rates. 
                    The breakages are suspected to be caused by multiple factors, including variations in machine performance, 
                    raw material quality, and temperature conditions. The management team is seeking a solution to minimize these 
                    breakages and improve overall production efficiency."
                    )
                ),
                box(
                  title = "Key Stakeholders", status = "info", solidHeader = TRUE, width = 12,
                  p("The primary stakeholder for this decision-support system (DSS) is the person 
                    who is responsible for maintaining high production standards and ensuring product quality.")
                ),
                box(
                  title = "Refinement of the Problem", status = "info", solidHeader = TRUE, width = 12,
                  p("Upon deeper analysis, the problem was refined to focus specifically on the breakage rate where window size, window type, cut speed, edge deletion rate, silicon viscosity, temperature, and glass thickness are 
                  known to play a key role. This refinement was guided by initial data analysis, which indicated that these 
                  variables had the highest correlation with breakage events. Given the constraints, such as limited 
                    availability of real-time data and resource allocation for new equipment, the solution assumes that 
                    existing machines will remain in operation and that any optimizations will come from process adjustments 
                    and predictive analytics.")
                ),
                box(
                  title = "Initial Set of Business Benefits", status = "info", solidHeader = TRUE, width = 12,
                  p("- The decision-support system is expected to reduce the window breakage rate by 15% by providing real-time recommendations for process adjustments."),
                  p("- Improved operational efficiency by enabling the company to monitor key performance indicators and make data-driven decisions."),
                  p("- Enhanced product quality and customer satisfaction by consistently producing defect-free windows.")
                ),
                box(
                  title = "Analytics Problem Framing: Reformulated Problem Statement", status = "primary", solidHeader = TRUE, width = 12,
                  p("The core analytics problem is to predict the window breakage rate based on various operational factors. This is treated as a regression-type problem, specifically utilizing linear regression to model the relationship between breakage rates and key performance indicators (KPIs) such as surrounding temperature, machine functioning inputs, and raw material quality. The caret library will be employed to facilitate model training and validation, allowing us to estimate coefficients that best fit the data.")
                  ),
                box(
                  title = "Proposed Set of Drivers and Relationships", status = "info", solidHeader = TRUE, width = 12,
                  p("A set of potential drivers influencing the window breakage rate was identified, including:"),
                  p("- Surrounding Temperature: Higher temperature fluctuations during the cooling phase correlate with increased breakage rates."),
                  p("- Machine Functioning Inputs: Variable values without proper optimisation, causing fractures."),
                  p("- Raw Material Quality: Variability in the quality of raw materials, such as inconsistencies in glass thickness, may affect the overall durability of the product."),
                  p("From prior analyses, the end-user can expect to see that as glass thickness increases, the window breakage rate decreases by a certain percentage, demonstrating a direct negative relationship between these two variables."),
                  ),
                box(
                  title = "Key Metrics of Success", status = "info", solidHeader = TRUE, width = 12,
                  p("- Reduction in Window Breakage Rate"),
                  p("- Prediction Accuracy"),
                  p("- Operational Efficiency Gains")
                ),
                box(
                  title = "Data Dictionary", status = "warning", solidHeader = TRUE, width = 12,
                  dataTableOutput("dataDict")
                )
              )
      ),
      
      # Updated UI for Descriptive Analytics
      tabItem(tabName = "descriptive",
              fluidRow(
                box(
                  title = "Filter Data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                  selectInput("glass_supplier", "Select Glass Supplier:", choices = NULL),
                  selectInput("window_type", "Select Window Type:", choices = NULL),
                  numericInput("window_size", "Window Size (inches):", value = 60, min = 50, max = 80),
                  numericInput("glass_thickness", "Glass Thickness (inches):", value = 0.5, min = 0.4, max = 0.6),
                  numericInput("cut_speed", "Cut Speed (m/min):", value = 2.0, min = 0.2, max = 5),
                  numericInput("edge_deletion_rate", "Edge Deletion Rate (m/min):", value = 15.0, min = 13, max = 18),
                  numericInput("silicon_viscosity", "Silicon Viscosity:", value = 10, min = 7, max = 17),
                  sliderInput("ambient_temp", "Ambient Temperature (°C):", min = 5, max = 25, value = 20)
                ),
                box(
                  title = "Breakage Data Summary", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 8,
                  # Summary Table displayed first
                  dataTableOutput("summary_table"),
                  
                  # 2x2 Grid for charts
                  fluidRow(
                    column(width = 6, plotOutput("breakage_dist")),
                    column(width = 6, plotOutput("cut_speed_plot"))
                  ),
                  fluidRow(
                    column(width = 6, plotOutput("heat_matrix")),
                    column(width = 6, plotOutput("supplier_barplot"))
                  )
                )
              )
      ),
      
      # Predictive Analytics tab
      tabItem(tabName = "predictive",
              fluidRow(
                box(
                  title = "Prediction Model", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  dataTableOutput("model_coef"),
                  valueBoxOutput("breakage_pred"),
                  valueBoxOutput("train_rmse_box"),
                  valueBoxOutput("test_rmse_box"),
                  actionButton("predict_btn", "Predict Breakage", class = "btn-primary btn-lg")
                )
              )
      ),
      
      # Prescriptive Analytics tab
      tabItem(tabName = "prescriptive",
              fluidRow(
                box(
                  title = "Optimal Parameters", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  valueBoxOutput("optimal_cut_speed"),
                  valueBoxOutput("optimal_edge_deletion_rate"),
                  valueBoxOutput("optimal_silicon_viscosity"),
                  valueBoxOutput("optimal_breakage"),
                  actionButton("opt_cut_speed_btn", "Find Optimal Values", class = "btn-success btn-lg")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Load and clean data
  
  # Update input options for supplier and window type
  updateSelectInput(session, "glass_supplier", choices = sort(unique(window_data$Glass_Supplier)))
  updateSelectInput(session, "window_type", choices = sort(unique(window_data$Window_Type)))
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$glass_supplier, input$window_type)
    window_data %>% filter(Glass_Supplier == input$glass_supplier, Window_Type == input$window_type)
  })
  
  # Output: Data dictionary
  output$dataDict <- renderDataTable({
    data.frame(
      Variable = c("Glass_Supplier", "Window_Type", "Glass_Supplier_Location", "Breakage_Rate", "Window_Size", 
                   "Glass_thickness", "Ambient_Temp", "Cut_Speed","Edge Deletion rate",	"Spacer Distance",	"Window color",
                   "Silicon_Viscosity"),
      Type = c("Categorical", "Categorical", "Categorical", "Numeric", "Numeric", "Numeric", "Numeric", "Numeric",
               "Numeric", "Numeric", "Numeric", "Numeric"),
      Description = c("Supplier of glass", "Type of window", "Location of the supplier", "Rate of breakage", "Size of the window", 
                      "Thickness of glass", "Ambient temperature", "Cutting speed", "Glass edge finishing speed",
                      "Gap between spacers", "Visible light transmission", "Adhesive thickness measurement")
    )
  })
  
  # Output: Data summary
  output$summary_table <- renderDataTable({
    filtered_data() %>%
      group_by(Glass_Supplier, Window_Type) %>%
      summarize(Average_Breakage = mean(Breakage_Rate, na.rm = TRUE),
                Average_Speed = mean(Cut_Speed, na.rm = TRUE)) %>%
      datatable()
  })
  
  # Output: Breakage rate distribution
  output$breakage_dist <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Breakage_Rate)) +
      geom_histogram(fill = "purple", alpha = 0.6, binwidth = 1) +
      labs(x = "Breakage Rate", y = "Frequency") +
      theme_minimal()
  })
  
  # Output: Breakage vs Cut Speed plot
  output$cut_speed_plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Cut_Speed, y = Breakage_Rate)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
      labs(x = "Cut Speed (m/min)", y = "Breakage Rate") +
      theme_minimal()
  })
  
  # Output: Heat Matrix
  output$heat_matrix <- renderPlot({
    library(reshape2)
    
    cor_matrix <- cor(window_data[, sapply(window_data, is.numeric)])
    melted_cor <- melt(cor_matrix)
    
    ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      labs(title = "Correlation Matrix Heatmap",
           x = "Variables",
           y = "Variables") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Output: Barplot for Breakage by Supplier
  output$supplier_barplot <- renderPlot({
    ggplot(window_data %>%
             group_by(Glass_Supplier) %>%
             summarize(Average_Breakage = mean(Breakage_Rate, na.rm = TRUE)), aes(x = Glass_Supplier, y = Average_Breakage)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(x = "Supplier", y = "Average Breakage Rate") +
      theme_minimal()
  })
  
  
  
  # Output: Model coefficients
  output$model_coef <- renderDataTable({
    summary(lm_model)$coefficients %>%
      as.data.frame() %>%
      datatable()
  })
  
  # Predictive analytics: Predict breakage rate based on input values
  observeEvent(input$predict_btn, {
    new_data <- data.frame(
      Window_Size = normalize(input$window_size, min(data$Window_Size), max(data$Window_Size)),
      Glass_Thickness = normalize(input$glass_thickness,min(data$Glass_Thickness), max(data$Glass_Thickness)),
      Cut_Speed = normalize(input$cut_speed,min(data$Cut_Speed), max(data$Cut_Speed)),
      Ambient_Temp = normalize(input$ambient_temp, min(data$Ambient_Temp), max(data$Ambient_Temp)),
      Edge_Deletion_Rate= normalize(input$edge_deletion_rate,min(data$Edge_Deletion_Rate), max(data$Edge_Deletion_Rate)),
      Silicon_Viscosity= normalize(input$silicon_viscosity, min(data$Silicon_Viscosity), max(data$Silicon_Viscosity))
    )
    
    selected_window_type <- input$window_type
    
    # Create dummy variables based on the selected Window Type
    if (selected_window_type == "Aluminum") {
      new_data$Window_TypeAluminum <- 1
      new_data$Window_TypeVinyl <- 0
    } else if (selected_window_type == "Vinyl") {
      new_data$Window_TypeAluminum <- 0
      new_data$Window_TypeVinyl <- 1
    } else {
      new_data$Window_TypeAluminum <- 0
      new_data$Window_TypeVinyl <- 0
    } 
    
    predicted_breakage <- predict(lm_model, newdata = new_data)
    
    output$breakage_pred <- renderValueBox({
      valueBox(
        round(denormalize(predicted_breakage, min(data$Breakage_Rate), max(data$Breakage_Rate)), 2), "Predicted Breakage Rate", color = "blue"
      )
    })
    
    train_predictions <- predict(lm_model, newdata = train)
    test_predictions <- predict(lm_model, newdata = test)
    train_rmse<- rmse_calculation(train$Breakage_Rate, train_predictions)
    test_rmse<- rmse_calculation(test$Breakage_Rate, test_predictions)
    
    
    output$train_rmse_box <- renderValueBox({
      valueBox(round(train_rmse, 2), "Training RMSE", color = "blue")
    })
    
    output$test_rmse_box <- renderValueBox({
      valueBox(round(test_rmse, 2), "Testing RMSE", color = "green")
    })
    
  })
  
  
  # Optimize cut speed for minimal breakage
  observeEvent(input$opt_cut_speed_btn, {
    new_data <- data.frame(
      Window_Size = normalize(input$window_size, min(data$Window_Size), max(data$Window_Size)),
      Glass_Thickness = normalize(input$glass_thickness, min(data$Glass_Thickness), max(data$Glass_Thickness)),
      Ambient_Temp = normalize(input$ambient_temp, min(data$Ambient_Temp), max(data$Ambient_Temp))
    )
    
    selected_window_type <- input$window_type
    
    # Create dummy variables based on the selected Window Type
    if (selected_window_type == "Aluminum") {
      new_data$Window_TypeAluminum <- 1
      new_data$Window_TypeVinyl <- 0
    } else if (selected_window_type == "Vinyl") {
      new_data$Window_TypeAluminum <- 0
      new_data$Window_TypeVinyl <- 1
    } else {
      new_data$Window_TypeAluminum <- 0
      new_data$Window_TypeVinyl <- 0
    } 
    
    # Define the optimization function for multiple parameters
    optimize_params <- function(params) {
      new_data$Cut_Speed <- params[1]
      new_data$Edge_Deletion_Rate <- params[2]
      new_data$Silicon_Viscosity <- params[3]
      
      # Return the predicted breakage rate (you may want to negate this if you're minimizing)
      return(predict(lm_model, newdata = new_data))
    }
    
    # Initial guesses for Cut_speed, Edge_Deletion_Rate, and Silicon_Viscosity
    initial_values <- c(0.4, 0.26, 0.6)  # Example initial values
    
    # Set bounds for each parameter (replace with your actual min and max)
    lower_bounds <- c(0, 0, 0)  # Replace with actual minimums
    upper_bounds <- c(1, 1, 1)         # Replace with actual maximums
    
    # Use optim() to find the optimal values
    optimal_result <- optim(
      par = initial_values,
      fn = optimize_params,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds
    )
    
    # Extract optimal values
    optimal_cut_speed <- optimal_result$par[1]
    optimal_edge_deletion_rate <- optimal_result$par[2]
    optimal_silicon_viscosity <- optimal_result$par[3]
    
    optimal_breakage <- optimize_params(optimal_result$par)  # Predicted breakage rate
    
    # Display results
    output$optimal_cut_speed <- renderValueBox({
      valueBox(round(denormalize(optimal_cut_speed, min(data$Cut_Speed), max(data$Cut_Speed)), 2), "Optimal Cut Speed", color = "green")
    })
    
    output$optimal_edge_deletion_rate <- renderValueBox({
      valueBox(round(denormalize(optimal_edge_deletion_rate, min(data$Edge_Deletion_Rate), max(data$Edge_Deletion_Rate)), 2), "Optimal Edge Deletion Rate", color = "orange")
    })
    
    output$optimal_silicon_viscosity <- renderValueBox({
      valueBox(round(denormalize(optimal_silicon_viscosity, min(data$Silicon_Viscosity), max(data$Silicon_Viscosity)), 2), "Optimal Silicon Viscosity", color = "blue")
    })
    
    output$optimal_breakage <- renderValueBox({
      valueBox(round(denormalize(optimal_breakage, min(data$Breakage_Rate), max(data$Breakage_Rate)), 2), "Minimum Predicted Breakage Rate", color = "red")
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


