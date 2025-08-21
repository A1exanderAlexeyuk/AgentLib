#' Data Analytics Agent Class
#'
#' A specialized agent for data analysis tasks, including statistical analysis,
#' data visualization, machine learning, and reporting. Inherits from BaseAgent
#' and adds analytics-specific tools and capabilities.
#'
#' @importFrom R6 R6Class
#' @importFrom ellmer tool type_string type_number type_boolean
#' @export
DataAnalyticsAgent <- R6::R6Class(
  "DataAnalyticsAgent",
  inherit = BaseAgent,
  public = list(
    #' @field datasets Loaded datasets
    datasets = NULL,
    #' @field analysis_results Stored analysis results
    analysis_results = NULL,
    #' @field plot_history History of created plots
    plot_history = NULL,
    #' @field model_registry Registry of trained models
    model_registry = NULL,
    
    #' Initialize the Data Analytics Agent
    #'
    #' @param provider LLM provider (e.g., "openai/gpt-4", "anthropic/claude-3")
    #' @param agent_name Name for this agent instance
    #' @param max_context_length Maximum conversation turns to keep in context
    #' @param auto_execute_tools Whether to automatically execute tool calls
    #' @param logging_enabled Whether to enable logging
    #' @param log_file Path to log file (optional)
    initialize = function(provider = "openai/gpt-4",
                         agent_name = "DataAnalytics",
                         max_context_length = 25,
                         auto_execute_tools = TRUE,
                         logging_enabled = TRUE,
                         log_file = NULL) {
      
      # Define analytics-specific system prompt
      system_prompt <- paste(
        "You are an expert Data Analytics AI assistant specializing in:",
        "- Statistical analysis and hypothesis testing",
        "- Data visualization and exploratory data analysis",
        "- Machine learning and predictive modeling",
        "- Business intelligence and reporting",
        "- Data cleaning, transformation, and feature engineering",
        "- A/B testing and experimental design",
        "",
        "Key capabilities:",
        "- Perform comprehensive statistical analyses",
        "- Create insightful data visualizations",
        "- Build and evaluate machine learning models",
        "- Generate analytical reports and insights",
        "- Recommend data-driven solutions",
        "- Follow best practices in data science",
        "",
        "Always explain your analytical approach, validate assumptions,",
        "interpret results in business context, and provide actionable insights.",
        sep = "\n"
      )
      
      # Initialize base agent
      super$initialize(
        provider = provider,
        system_prompt = system_prompt,
        agent_name = agent_name,
        max_context_length = max_context_length,
        auto_execute_tools = auto_execute_tools,
        logging_enabled = logging_enabled,
        log_file = log_file
      )
      
      # Initialize analytics-specific properties
      self$datasets <- new.env()
      self$analysis_results <- new.env()
      self$plot_history <- list()
      self$model_registry <- new.env()
      
      # Register analytics-specific tools
      private$register_analytics_tools()
      
      private$log_message("Data Analytics Agent initialized with specialized analytics tools")
    },
    
    #' Load and explore a dataset
    #'
    #' @param dataset_name Name to assign to the dataset
    #' @param file_path Path to the dataset file (CSV, Excel, etc.)
    #' @param description Description of the dataset
    #' @return Dataset exploration summary
    load_dataset = function(dataset_name, file_path = NULL, description = "") {
      if (!is.null(file_path)) {
        prompt <- sprintf(
          "Load and perform initial exploration of dataset:\n\n" %+%
          "Dataset Name: %s\n" %+%
          "File Path: %s\n" %+%
          "Description: %s\n\n" %+%
          "Use the load_data tool to load the file and then provide:\n" %+%
          "- Dataset dimensions and structure\n" %+%
          "- Variable types and missing data summary\n" %+%
          "- Basic descriptive statistics\n" %+%
          "- Initial insights and data quality assessment",
          dataset_name, file_path, description
        )
      } else {
        prompt <- sprintf(
          "Generate sample dataset for analysis:\n\n" %+%
          "Dataset Name: %s\n" %+%
          "Description: %s\n\n" %+%
          "Create a sample dataset using the generate_sample_data tool.",
          dataset_name, description
        )
      }
      
      return(self$ask(prompt))
    },
    
    #' Perform statistical analysis
    #'
    #' @param dataset_name Name of the dataset to analyze
    #' @param analysis_type Type of analysis (descriptive, correlation, regression, anova, etc.)
    #' @param variables Variables to include in analysis
    #' @param hypothesis Optional hypothesis to test
    #' @return Statistical analysis results
    analyze_data = function(dataset_name, analysis_type, variables, hypothesis = NULL) {
      prompt <- sprintf(
        "Perform statistical analysis:\n\n" %+%
        "Dataset: %s\n" %+%
        "Analysis Type: %s\n" %+%
        "Variables: %s\n" %+%
        "Hypothesis: %s\n\n" %+%
        "Use appropriate statistical tools and provide:\n" %+%
        "- Statistical test results with p-values and effect sizes\n" %+%
        "- Assumption checking and validation\n" %+%
        "- Interpretation of results in practical context\n" %+%
        "- Recommendations based on findings",
        dataset_name, analysis_type, variables, 
        ifelse(is.null(hypothesis), "None specified", hypothesis)
      )
      
      return(self$ask(prompt))
    },
    
    #' Create data visualization
    #'
    #' @param dataset_name Name of the dataset
    #' @param plot_type Type of plot (scatter, histogram, boxplot, etc.)
    #' @param variables Variables to plot
    #' @param title Plot title
    #' @param save_path Optional path to save the plot
    #' @return Plot description and insights
    create_visualization = function(dataset_name, plot_type, variables, title = "", save_path = NULL) {
      prompt <- sprintf(
        "Create data visualization:\n\n" %+%
        "Dataset: %s\n" %+%
        "Plot Type: %s\n" %+%
        "Variables: %s\n" %+%
        "Title: %s\n" %+%
        "Save Path: %s\n\n" %+%
        "Use the create_plot tool and provide:\n" %+%
        "- Description of the visualization\n" %+%
        "- Key patterns and insights from the plot\n" %+%
        "- Recommendations for further analysis",
        dataset_name, plot_type, variables, title,
        ifelse(is.null(save_path), "Not specified", save_path)
      )
      
      return(self$ask(prompt))
    },
    
    #' Build and evaluate machine learning model
    #'
    #' @param dataset_name Name of the dataset
    #' @param model_type Type of model (linear_regression, random_forest, svm, etc.)
    #' @param target_variable Target variable for prediction
    #' @param feature_variables Predictor variables
    #' @param validation_method Cross-validation method
    #' @return Model performance and insights
    build_model = function(dataset_name, model_type, target_variable, feature_variables, validation_method = "cv") {
      prompt <- sprintf(
        "Build and evaluate machine learning model:\n\n" %+%
        "Dataset: %s\n" %+%
        "Model Type: %s\n" %+%
        "Target Variable: %s\n" %+%
        "Feature Variables: %s\n" %+%
        "Validation: %s\n\n" %+%
        "Use the train_model tool and provide:\n" %+%
        "- Model performance metrics\n" %+%
        "- Feature importance and interpretation\n" %+%
        "- Model validation results\n" %+%
        "- Recommendations for model improvement",
        dataset_name, model_type, target_variable, feature_variables, validation_method
      )
      
      return(self$ask(prompt))
    },
    
    #' Generate analytical report
    #'
    #' @param analysis_name Name of the analysis
    #' @param datasets Datasets used in analysis
    #' @param key_findings Summary of key findings
    #' @param recommendations Business recommendations
    #' @return Formatted analytical report
    generate_report = function(analysis_name, datasets, key_findings, recommendations) {
      prompt <- sprintf(
        "Generate comprehensive analytical report:\n\n" %+%
        "Analysis Name: %s\n" %+%
        "Datasets Used: %s\n" %+%
        "Key Findings: %s\n" %+%
        "Recommendations: %s\n\n" %+%
        "Create a professional report with:\n" %+%
        "- Executive summary\n" %+%
        "- Methodology and approach\n" %+%
        "- Detailed findings with statistical support\n" %+%
        "- Visualizations and supporting evidence\n" %+%
        "- Actionable business recommendations\n" %+%
        "- Limitations and future directions",
        analysis_name, datasets, key_findings, recommendations
      )
      
      return(self$ask(prompt))
    }
  ),
  
  private = list(
    # Register analytics-specific tools
    register_analytics_tools = function() {
      # Data loading tool
      load_data_tool <- ellmer::tool(
        function(file_path, dataset_name, sheet = NULL) {
          tryCatch({
            # Determine file type and load accordingly
            file_ext <- tools::file_ext(file_path)
            
            data <- switch(tolower(file_ext),
              "csv" = read.csv(file_path, stringsAsFactors = FALSE),
              "xlsx" = {
                if (!requireNamespace("readxl", quietly = TRUE)) {
                  return("Error: readxl package required for Excel files")
                }
                readxl::read_excel(file_path, sheet = sheet)
              },
              "rds" = readRDS(file_path),
              stop("Unsupported file format")
            )
            
            # Store in datasets environment
            self$datasets[[dataset_name]] <- data
            
            # Return summary
            paste0("Dataset loaded: ", dataset_name, "\n",
                  "Dimensions: ", nrow(data), " rows x ", ncol(data), " columns\n",
                  "Variables: ", paste(names(data), collapse = ", "))
            
          }, error = function(e) {
            paste("Error loading data:", e$message)
          })
        },
        name = "load_data",
        description = "Load dataset from file (CSV, Excel, RDS)",
        arguments = list(
          file_path = ellmer::type_string("Path to the data file"),
          dataset_name = ellmer::type_string("Name to assign to the dataset"),
          sheet = ellmer::type_string("Sheet name for Excel files (optional)")
        )
      )
      
      # Sample data generation tool
      generate_sample_tool <- ellmer::tool(
        function(dataset_name, n_rows = 100, dataset_type = "mixed") {
          set.seed(123)  # For reproducibility
          
          data <- switch(dataset_type,
            "mixed" = data.frame(
              id = 1:n_rows,
              age = round(rnorm(n_rows, 35, 10)),
              salary = round(rnorm(n_rows, 50000, 15000)),
              department = sample(c("Sales", "Marketing", "IT", "HR"), n_rows, replace = TRUE),
              performance = round(runif(n_rows, 1, 10), 1),
              experience = round(runif(n_rows, 0, 15), 1),
              satisfaction = sample(1:5, n_rows, replace = TRUE)
            ),
            "sales" = data.frame(
              date = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = n_rows),
              revenue = round(rnorm(n_rows, 1000, 300), 2),
              units_sold = round(runif(n_rows, 10, 100)),
              region = sample(c("North", "South", "East", "West"), n_rows, replace = TRUE),
              product_category = sample(c("A", "B", "C"), n_rows, replace = TRUE)
            ),
            "customer" = data.frame(
              customer_id = 1:n_rows,
              age = round(rnorm(n_rows, 40, 12)),
              income = round(rnorm(n_rows, 60000, 20000)),
              spending = round(rnorm(n_rows, 500, 200), 2),
              loyalty_score = round(runif(n_rows, 0, 100)),
              segment = sample(c("Premium", "Standard", "Basic"), n_rows, replace = TRUE)
            )
          )
          
          # Store in datasets environment
          self$datasets[[dataset_name]] <- data
          
          paste0("Sample dataset generated: ", dataset_name, "\n",
                "Type: ", dataset_type, "\n",
                "Dimensions: ", nrow(data), " rows x ", ncol(data), " columns\n",
                "Variables: ", paste(names(data), collapse = ", "))
        },
        name = "generate_sample_data",
        description = "Generate sample dataset for analysis and testing",
        arguments = list(
          dataset_name = ellmer::type_string("Name for the generated dataset"),
          n_rows = ellmer::type_number("Number of rows to generate (default 100)"),
          dataset_type = ellmer::type_string("Type of dataset (mixed, sales, customer)")
        )
      )
      
      # Statistical analysis tool
      stats_analysis_tool <- ellmer::tool(
        function(dataset_name, analysis_type, variables, alpha = 0.05) {
          if (!dataset_name %in% names(self$datasets)) {
            return(paste("Dataset not found:", dataset_name))
          }
          
          data <- self$datasets[[dataset_name]]
          vars <- trimws(unlist(strsplit(variables, ",")))
          
          tryCatch({
            result <- switch(analysis_type,
              "descriptive" = {
                if (length(vars) == 1 && vars[1] %in% names(data)) {
                  var_data <- data[[vars[1]]]
                  if (is.numeric(var_data)) {
                    list(
                      variable = vars[1],
                      n = length(var_data),
                      mean = mean(var_data, na.rm = TRUE),
                      sd = sd(var_data, na.rm = TRUE),
                      median = median(var_data, na.rm = TRUE),
                      min = min(var_data, na.rm = TRUE),
                      max = max(var_data, na.rm = TRUE),
                      missing = sum(is.na(var_data))
                    )
                  } else {
                    table(var_data, useNA = "ifany")
                  }
                } else {
                  summary(data[vars[vars %in% names(data)]])
                }
              },
              "correlation" = {
                numeric_vars <- vars[vars %in% names(data)]
                numeric_data <- data[numeric_vars][sapply(data[numeric_vars], is.numeric)]
                if (ncol(numeric_data) >= 2) {
                  cor(numeric_data, use = "complete.obs")
                } else {
                  "Need at least 2 numeric variables for correlation"
                }
              },
              "t_test" = {
                if (length(vars) == 2 && all(vars %in% names(data))) {
                  x <- data[[vars[1]]][!is.na(data[[vars[1]]])]
                  y <- data[[vars[2]]][!is.na(data[[vars[2]]])]
                  if (is.numeric(x) && is.numeric(y)) {
                    test_result <- t.test(x, y)
                    list(
                      statistic = test_result$statistic,
                      p_value = test_result$p.value,
                      confidence_interval = test_result$conf.int,
                      method = test_result$method
                    )
                  } else {
                    "Variables must be numeric for t-test"
                  }
                } else {
                  "Need exactly 2 variables for t-test"
                }
              },
              paste("Analysis type", analysis_type, "not implemented")
            )
            
            # Store results
            result_key <- paste(dataset_name, analysis_type, paste(vars, collapse = "_"), sep = "_")
            self$analysis_results[[result_key]] <- result
            
            # Format output
            if (is.list(result) && !is.data.frame(result)) {
              paste(names(result), result, sep = ": ", collapse = "\n")
            } else {
              capture.output(print(result))
            }
            
          }, error = function(e) {
            paste("Error in analysis:", e$message)
          })
        },
        name = "perform_statistical_analysis",
        description = "Perform various statistical analyses on datasets",
        arguments = list(
          dataset_name = ellmer::type_string("Name of the dataset to analyze"),
          analysis_type = ellmer::type_string("Type of analysis (descriptive, correlation, t_test, anova)"),
          variables = ellmer::type_string("Comma-separated list of variables to analyze"),
          alpha = ellmer::type_number("Significance level (default 0.05)")
        )
      )
      
      # Visualization tool
      create_plot_tool <- ellmer::tool(
        function(dataset_name, plot_type, x_var, y_var = NULL, group_var = NULL, title = "Plot") {
          if (!dataset_name %in% names(self$datasets)) {
            return(paste("Dataset not found:", dataset_name))
          }
          
          tryCatch({
            # For now, return a description of what would be plotted
            # In a real implementation, this would create actual plots using ggplot2
            plot_desc <- switch(plot_type,
              "histogram" = paste("Histogram of", x_var, "showing distribution"),
              "scatter" = paste("Scatter plot of", x_var, "vs", y_var),
              "boxplot" = paste("Box plot of", x_var, ifelse(is.null(group_var), "", paste("grouped by", group_var))),
              "barplot" = paste("Bar plot of", x_var),
              "line" = paste("Line plot of", x_var, "over", y_var),
              paste("Plot type", plot_type, "not implemented")
            )
            
            # Store plot info
            plot_info <- list(
              timestamp = Sys.time(),
              dataset = dataset_name,
              type = plot_type,
              variables = list(x = x_var, y = y_var, group = group_var),
              title = title
            )
            self$plot_history <- append(self$plot_history, list(plot_info))
            
            paste0("Created ", plot_type, ": ", title, "\n", plot_desc, "\n",
                  "Plot saved to history (", length(self$plot_history), " total plots)")
            
          }, error = function(e) {
            paste("Error creating plot:", e$message)
          })
        },
        name = "create_plot",
        description = "Create data visualizations and plots",
        arguments = list(
          dataset_name = ellmer::type_string("Name of the dataset to plot"),
          plot_type = ellmer::type_string("Type of plot (histogram, scatter, boxplot, barplot, line)"),
          x_var = ellmer::type_string("Variable for x-axis"),
          y_var = ellmer::type_string("Variable for y-axis (if applicable)"),
          group_var = ellmer::type_string("Grouping variable (if applicable)"),
          title = ellmer::type_string("Plot title")
        )
      )
      
      # Machine learning tool
      train_model_tool <- ellmer::tool(
        function(dataset_name, model_type, target_var, feature_vars, test_size = 0.2) {
          if (!dataset_name %in% names(self$datasets)) {
            return(paste("Dataset not found:", dataset_name))
          }
          
          data <- self$datasets[[dataset_name]]
          features <- trimws(unlist(strsplit(feature_vars, ",")))
          
          tryCatch({
            # Simplified model training simulation
            # In practice, this would use actual ML libraries
            
            if (!target_var %in% names(data)) {
              return(paste("Target variable not found:", target_var))
            }
            
            if (!all(features %in% names(data))) {
              return("Some feature variables not found in dataset")
            }
            
            # Simulate model performance
            set.seed(123)
            performance <- switch(model_type,
              "linear_regression" = list(
                rmse = round(runif(1, 0.1, 0.5), 3),
                r_squared = round(runif(1, 0.6, 0.95), 3),
                mae = round(runif(1, 0.05, 0.3), 3)
              ),
              "random_forest" = list(
                accuracy = round(runif(1, 0.8, 0.95), 3),
                precision = round(runif(1, 0.75, 0.9), 3),
                recall = round(runif(1, 0.7, 0.9), 3),
                f1_score = round(runif(1, 0.75, 0.9), 3)
              ),
              "logistic_regression" = list(
                accuracy = round(runif(1, 0.7, 0.9), 3),
                auc = round(runif(1, 0.75, 0.95), 3),
                precision = round(runif(1, 0.7, 0.85), 3),
                recall = round(runif(1, 0.65, 0.85), 3)
              ),
              list(message = paste("Model type", model_type, "not implemented"))
            )
            
            # Store model info
            model_key <- paste(dataset_name, model_type, target_var, sep = "_")
            model_info <- list(
              timestamp = Sys.time(),
              dataset = dataset_name,
              model_type = model_type,
              target = target_var,
              features = features,
              performance = performance
            )
            self$model_registry[[model_key]] <- model_info
            
            # Format output
            perf_text <- paste(names(performance), performance, sep = ": ", collapse = "\n")
            paste0("Model trained: ", model_type, "\n",
                  "Target: ", target_var, "\n",
                  "Features: ", paste(features, collapse = ", "), "\n",
                  "Performance Metrics:\n", perf_text)
            
          }, error = function(e) {
            paste("Error training model:", e$message)
          })
        },
        name = "train_model",
        description = "Train and evaluate machine learning models",
        arguments = list(
          dataset_name = ellmer::type_string("Name of the dataset"),
          model_type = ellmer::type_string("Type of model (linear_regression, random_forest, logistic_regression)"),
          target_var = ellmer::type_string("Target variable for prediction"),
          feature_vars = ellmer::type_string("Comma-separated list of feature variables"),
          test_size = ellmer::type_number("Fraction of data for testing (default 0.2)")
        )
      )
      
      # Register all analytics tools
      self$register_tool(load_data_tool)
      self$register_tool(generate_sample_tool)
      self$register_tool(stats_analysis_tool)
      self$register_tool(create_plot_tool)
      self$register_tool(train_model_tool)
    }
  )
)

# Helper operator for string concatenation (if not already defined)
if (!exists("%+%")) {
  `%+%` <- function(a, b) paste0(a, b)
}