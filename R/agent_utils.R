#' Agent Utility Functions
#'
#' Collection of utility functions to support agent functionality
#' and provide common operations across different agent types.
#'
#' @importFrom ellmer tool type_string type_number type_boolean

#' Create a new agent factory
#'
#' Factory function to create different types of agents with consistent configuration
#'
#' @param agent_type Type of agent to create ("base", "medical", "analytics")
#' @param provider LLM provider (e.g., "openai/gpt-4", "anthropic/claude-3")
#' @param agent_name Custom name for the agent
#' @param ... Additional parameters passed to the agent constructor
#' @return Agent instance
#' @export
create_agent <- function(agent_type = "base", provider = "openai/gpt-4", agent_name = NULL, ...) {
  # Set default agent name if not provided
  if (is.null(agent_name)) {
    agent_name <- switch(agent_type,
      "base" = "BaseAgent",
      "medical" = "MedicalWriter",
      "analytics" = "DataAnalytics",
      paste0("Agent_", agent_type)
    )
  }
  
  # Create appropriate agent type
  agent <- switch(agent_type,
    "base" = BaseAgent$new(provider = provider, agent_name = agent_name, ...),
    "medical" = MedicalWriterAgent$new(provider = provider, agent_name = agent_name, ...),
    "analytics" = DataAnalyticsAgent$new(provider = provider, agent_name = agent_name, ...),
    stop("Unknown agent type: ", agent_type)
  )
  
  return(agent)
}

#' Batch process multiple prompts
#'
#' Process multiple prompts with an agent and collect results
#'
#' @param agent Agent instance
#' @param prompts Vector of prompts to process
#' @param delay_seconds Delay between prompts (default 1 second)
#' @param save_results Whether to save results to file
#' @param output_file File to save results to
#' @return List of results
#' @export
batch_process <- function(agent, prompts, delay_seconds = 1, save_results = FALSE, output_file = NULL) {
  if (!inherits(agent, "BaseAgent")) {
    stop("agent must be a BaseAgent or descendant")
  }
  
  results <- list()
  
  for (i in seq_along(prompts)) {
    cat(sprintf("Processing prompt %d of %d...\n", i, length(prompts)))
    
    tryCatch({
      result <- agent$ask(prompts[i])
      results[[i]] <- list(
        prompt = prompts[i],
        response = result,
        timestamp = Sys.time(),
        tokens = agent$get_tokens(),
        cost = agent$get_cost()
      )
    }, error = function(e) {
      results[[i]] <- list(
        prompt = prompts[i],
        response = paste("Error:", e$message),
        timestamp = Sys.time(),
        error = TRUE
      )
    })
    
    # Add delay between requests
    if (i < length(prompts)) {
      Sys.sleep(delay_seconds)
    }
  }
  
  # Save results if requested
  if (save_results) {
    if (is.null(output_file)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      output_file <- sprintf("batch_results_%s_%s.rds", agent$agent_name, timestamp)
    }
    
    batch_data <- list(
      agent_name = agent$agent_name,
      timestamp = Sys.time(),
      prompts = prompts,
      results = results,
      total_tokens = agent$get_tokens(),
      total_cost = agent$get_cost()
    )
    
    saveRDS(batch_data, output_file)
    cat(sprintf("Results saved to: %s\n", output_file))
  }
  
  return(results)
}

#' Compare agent responses
#'
#' Compare how different agents respond to the same prompt
#'
#' @param agents List of agent instances
#' @param prompt Prompt to test
#' @param agent_names Optional names for the agents
#' @return Comparison results
#' @export
compare_agents <- function(agents, prompt, agent_names = NULL) {
  if (!is.list(agents)) {
    stop("agents must be a list of agent instances")
  }
  
  if (is.null(agent_names)) {
    agent_names <- sapply(agents, function(x) x$agent_name)
  }
  
  results <- list()
  
  for (i in seq_along(agents)) {
    agent <- agents[[i]]
    name <- agent_names[i]
    
    cat(sprintf("Getting response from %s...\n", name))
    
    start_time <- Sys.time()
    tryCatch({
      response <- agent$ask(prompt)
      end_time <- Sys.time()
      
      results[[name]] <- list(
        response = response,
        response_time = as.numeric(end_time - start_time, units = "secs"),
        tokens = agent$get_tokens(),
        cost = agent$get_cost(),
        success = TRUE
      )
    }, error = function(e) {
      end_time <- Sys.time()
      results[[name]] <- list(
        response = paste("Error:", e$message),
        response_time = as.numeric(end_time - start_time, units = "secs"),
        success = FALSE
      )
    })
  }
  
  # Create summary
  comparison <- list(
    prompt = prompt,
    timestamp = Sys.time(),
    results = results,
    summary = list(
      num_agents = length(agents),
      successful_responses = sum(sapply(results, function(x) x$success)),
      avg_response_time = mean(sapply(results, function(x) x$response_time)),
      total_tokens = sum(sapply(results, function(x) x$tokens %||% 0)),
      total_cost = sum(sapply(results, function(x) x$cost %||% 0))
    )
  )
  
  return(comparison)
}

#' Agent performance monitor
#'
#' Monitor agent performance and resource usage
#'
#' @param agent Agent instance to monitor
#' @param duration_minutes How long to monitor (default 60 minutes)
#' @param check_interval_seconds How often to check status (default 300 seconds)
#' @return Monitoring results
#' @export
monitor_agent <- function(agent, duration_minutes = 60, check_interval_seconds = 300) {
  if (!inherits(agent, "BaseAgent")) {
    stop("agent must be a BaseAgent or descendant")
  }
  
  start_time <- Sys.time()
  end_time <- start_time + duration_minutes * 60
  
  monitoring_data <- list()
  check_count <- 1
  
  cat(sprintf("Starting monitoring of %s for %d minutes...\n", 
              agent$agent_name, duration_minutes))
  
  while (Sys.time() < end_time) {
    timestamp <- Sys.time()
    status <- agent$get_status()
    
    monitoring_data[[check_count]] <- list(
      timestamp = timestamp,
      status = status,
      memory_usage = object.size(agent),
      system_memory = if (requireNamespace("pryr", quietly = TRUE)) pryr::mem_used() else NA
    )
    
    cat(sprintf("[%s] Check %d: %d conversations, %s tokens used\n",
                format(timestamp, "%H:%M:%S"), check_count,
                status$conversation_length, status$tokens_used %||% "unknown"))
    
    check_count <- check_count + 1
    
    # Wait for next check
    Sys.sleep(check_interval_seconds)
  }
  
  cat("Monitoring completed.\n")
  
  return(list(
    agent_name = agent$agent_name,
    monitoring_period = list(start = start_time, end = Sys.time()),
    data = monitoring_data,
    summary = list(
      total_checks = length(monitoring_data),
      final_status = agent$get_status()
    )
  ))
}

#' Export agent configuration
#'
#' Export agent configuration for reproducibility
#'
#' @param agent Agent instance
#' @param include_history Whether to include conversation history
#' @param include_tools Whether to include tool definitions
#' @return Configuration list
#' @export
export_agent_config <- function(agent, include_history = FALSE, include_tools = TRUE) {
  if (!inherits(agent, "BaseAgent")) {
    stop("agent must be a BaseAgent or descendant")
  }
  
  config <- list(
    agent_class = class(agent)[1],
    agent_name = agent$agent_name,
    max_context_length = agent$max_context_length,
    auto_execute_tools = agent$auto_execute_tools,
    logging_enabled = agent$logging_enabled,
    timestamp = Sys.time()
  )
  
  if (include_history) {
    config$conversation_history <- agent$get_history()
  }
  
  if (include_tools) {
    config$tools <- names(agent$tools)
  }
  
  # Add class-specific configuration
  if (inherits(agent, "MedicalWriterAgent")) {
    config$medical_standards <- agent$medical_standards
    config$document_templates <- names(agent$document_templates)
  }
  
  if (inherits(agent, "DataAnalyticsAgent")) {
    config$datasets <- names(agent$datasets)
    config$models <- names(agent$model_registry)
    config$plots_created <- length(agent$plot_history)
  }
  
  return(config)
}

#' Validate agent setup
#'
#' Validate that an agent is properly configured and functional
#'
#' @param agent Agent instance to validate
#' @param run_tests Whether to run functional tests
#' @return Validation results
#' @export
validate_agent <- function(agent, run_tests = TRUE) {
  if (!inherits(agent, "BaseAgent")) {
    stop("agent must be a BaseAgent or descendant")
  }
  
  validation <- list(
    agent_name = agent$agent_name,
    timestamp = Sys.time(),
    checks = list(),
    issues = list(),
    success = TRUE
  )
  
  # Check basic configuration
  validation$checks$basic_config <- list(
    agent_name_set = !is.null(agent$agent_name) && nchar(agent$agent_name) > 0,
    chat_initialized = !is.null(agent$chat),
    tools_registered = length(agent$tools) > 0,
    logging_setup = is.logical(agent$logging_enabled)
  )
  
  # Check for any failed basic checks
  failed_basic <- !all(unlist(validation$checks$basic_config))
  if (failed_basic) {
    validation$issues <- append(validation$issues, "Basic configuration checks failed")
    validation$success <- FALSE
  }
  
  # Run functional tests if requested
  if (run_tests && !failed_basic) {
    cat("Running functional tests...\n")
    
    tryCatch({
      # Test basic conversation
      test_response <- agent$ask("Hello, can you confirm you're working properly?", 
                               return_full_response = FALSE)
      validation$checks$conversation_test <- !is.null(test_response) && nchar(test_response) > 0
      
      # Test tool availability
      validation$checks$tools_available <- length(agent$tools) >= 4  # Should have default tools
      
      # Test status retrieval
      status <- agent$get_status()
      validation$checks$status_retrieval <- is.list(status) && "agent_name" %in% names(status)
      
    }, error = function(e) {
      validation$issues <- append(validation$issues, paste("Functional test error:", e$message))
      validation$success <- FALSE
    })
  }
  
  # Overall validation result
  if (run_tests) {
    all_checks_passed <- all(unlist(validation$checks))
    validation$success <- validation$success && all_checks_passed
  }
  
  validation$summary <- sprintf(
    "Agent %s validation %s. %d checks performed, %d issues found.",
    agent$agent_name,
    if (validation$success) "PASSED" else "FAILED",
    length(unlist(validation$checks)),
    length(validation$issues)
  )
  
  cat(validation$summary, "\n")
  
  return(validation)
}

# Null-default operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}