#' Base Agent Class
#'
#' A comprehensive R6 class that provides the foundation for building AI agents
#' using ellmer. This class encapsulates chat functionality, tool management,
#' context handling, logging, and cost tracking.
#'
#' @importFrom R6 R6Class
#' @importFrom ellmer chat tool type_string type_number type_boolean
#' @export
BaseAgent <- R6::R6Class(
  "BaseAgent",
  public = list(
    #' @field chat The ellmer chat object
    chat = NULL,
    #' @field tools List of registered tools
    tools = NULL,
    #' @field conversation_history Full conversation history
    conversation_history = NULL,
    #' @field agent_name Name of the agent
    agent_name = NULL,
    #' @field max_context_length Maximum context length to maintain
    max_context_length = NULL,
    #' @field auto_execute_tools Whether to automatically execute tool calls
    auto_execute_tools = NULL,
    #' @field logging_enabled Whether logging is enabled
    logging_enabled = NULL,
    #' @field log_file Path to log file
    log_file = NULL,
    
    #' Initialize the BaseAgent
    #'
    #' @param provider LLM provider (e.g., "openai/gpt-4", "anthropic/claude-3")
    #' @param system_prompt System prompt for the agent
    #' @param agent_name Name for this agent instance
    #' @param max_context_length Maximum conversation turns to keep in context
    #' @param auto_execute_tools Whether to automatically execute tool calls
    #' @param logging_enabled Whether to enable logging
    #' @param log_file Path to log file (optional)
    initialize = function(provider = "openai/gpt-4", 
                         system_prompt = "You are a helpful AI assistant.",
                         agent_name = "BaseAgent",
                         max_context_length = 20,
                         auto_execute_tools = TRUE,
                         logging_enabled = TRUE,
                         log_file = NULL) {
      
      # Initialize ellmer chat
      self$chat <- ellmer::chat(provider, system_prompt = system_prompt)
      
      # Initialize properties
      self$tools <- list()
      self$conversation_history <- list()
      self$agent_name <- agent_name
      self$max_context_length <- max_context_length
      self$auto_execute_tools <- auto_execute_tools
      self$logging_enabled <- logging_enabled
      self$log_file <- log_file
      
      # Set up logging
      if (self$logging_enabled) {
        private$setup_logging()
      }
      
      # Register default tools
      private$register_default_tools()
      
      # Log initialization
      private$log_message(sprintf("Agent '%s' initialized with provider '%s'", 
                                 self$agent_name, provider))
    },
    
    #' Register a tool with the agent
    #'
    #' @param tool_definition Tool definition created with ellmer::tool()
    #' @param tool_name Optional name for the tool (uses tool's name if not provided)
    register_tool = function(tool_definition, tool_name = NULL) {
      if (is.null(tool_name)) {
        tool_name <- tool_definition$name
      }
      
      # Register with ellmer chat
      self$chat$register_tool(tool_definition)
      
      # Store in our tools registry
      self$tools[[tool_name]] <- tool_definition
      
      private$log_message(sprintf("Tool '%s' registered", tool_name))
    },
    
    #' Send a message to the agent
    #'
    #' @param message The message to send
    #' @param return_full_response Whether to return the full response object
    #' @return Agent's response
    ask = function(message, return_full_response = FALSE) {
      private$log_message(sprintf("User: %s", message))
      
      # Manage context length
      private$manage_context()
      
      # Get initial response
      result <- self$chat$chat(message)
      
      # Handle tool calls if auto-execution is enabled
      if (self$auto_execute_tools) {
        result <- private$handle_tool_calls(result)
      }
      
      # Store in conversation history
      private$add_to_history("user", message)
      private$add_to_history("assistant", as.character(result))
      
      private$log_message(sprintf("Agent: %s", as.character(result)))
      
      if (return_full_response) {
        return(result)
      } else {
        return(as.character(result))
      }
    },
    
    #' Execute a tool call manually
    #'
    #' @param tool_call Tool call object from ellmer
    #' @return Result of tool execution
    execute_tool = function(tool_call) {
      if (!inherits(tool_call, "ToolCall")) {
        stop("Input must be a ToolCall object")
      }
      
      tool_name <- tool_call$tool_name
      args <- tool_call$args
      
      if (!tool_name %in% names(self$tools)) {
        stop(sprintf("Tool '%s' not found", tool_name))
      }
      
      private$log_message(sprintf("Executing tool: %s(%s)", 
                                 tool_name, 
                                 paste(names(args), args, sep = "=", collapse = ", ")))
      
      tool_fun <- self$tools[[tool_name]]$fun
      result <- do.call(tool_fun, args)
      
      private$log_message(sprintf("Tool result: %s", as.character(result)))
      
      return(result)
    },
    
    #' Get conversation turns from ellmer chat
    #'
    #' @return List of conversation turns
    get_turns = function() {
      return(self$chat$get_turns())
    },
    
    #' Get token usage statistics
    #'
    #' @return Token usage information
    get_tokens = function() {
      return(self$chat$get_tokens())
    },
    
    #' Get cost information
    #'
    #' @return Cost information
    get_cost = function() {
      return(self$chat$get_cost())
    },
    
    #' Clear conversation history
    clear_history = function() {
      self$conversation_history <- list()
      private$log_message("Conversation history cleared")
    },
    
    #' Get full conversation history
    #'
    #' @return List of conversation history
    get_history = function() {
      return(self$conversation_history)
    },
    
    #' Save conversation to file
    #'
    #' @param file_path Path to save the conversation
    save_conversation = function(file_path) {
      conversation_data <- list(
        agent_name = self$agent_name,
        timestamp = Sys.time(),
        turns = self$get_turns(),
        history = self$conversation_history,
        tokens = self$get_tokens(),
        cost = self$get_cost()
      )
      
      saveRDS(conversation_data, file_path)
      private$log_message(sprintf("Conversation saved to %s", file_path))
    },
    
    #' Load conversation from file
    #'
    #' @param file_path Path to load the conversation from
    load_conversation = function(file_path) {
      if (!file.exists(file_path)) {
        stop(sprintf("File not found: %s", file_path))
      }
      
      conversation_data <- readRDS(file_path)
      self$conversation_history <- conversation_data$history
      
      private$log_message(sprintf("Conversation loaded from %s", file_path))
    },
    
    #' Get agent status and statistics
    #'
    #' @return List with agent status information
    get_status = function() {
      list(
        agent_name = self$agent_name,
        tools_registered = length(self$tools),
        conversation_length = length(self$conversation_history),
        tokens_used = self$get_tokens(),
        estimated_cost = self$get_cost(),
        auto_execute_tools = self$auto_execute_tools,
        logging_enabled = self$logging_enabled
      )
    }
  ),
  
  private = list(
    # Set up logging system
    setup_logging = function() {
      if (is.null(self$log_file)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        self$log_file <- sprintf("agent_%s_%s.log", self$agent_name, timestamp)
      }
      
      # Create log directory if it doesn't exist
      log_dir <- dirname(self$log_file)
      if (log_dir != "." && !dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }
      
      # Write initial log entry
      cat(sprintf("[%s] Agent '%s' logging started\n", 
                  Sys.time(), self$agent_name), 
          file = self$log_file, append = TRUE)
    },
    
    # Log a message
    log_message = function(message) {
      if (!self$logging_enabled) return()
      
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- sprintf("[%s] %s\n", timestamp, message)
      
      # Write to console
      cat(log_entry)
      
      # Write to file if specified
      if (!is.null(self$log_file)) {
        cat(log_entry, file = self$log_file, append = TRUE)
      }
    },
    
    # Add entry to conversation history
    add_to_history = function(role, content) {
      entry <- list(
        timestamp = Sys.time(),
        role = role,
        content = content
      )
      
      self$conversation_history <- append(self$conversation_history, list(entry))
    },
    
    # Manage context length
    manage_context = function() {
      if (length(self$conversation_history) > self$max_context_length) {
        # Keep the most recent conversations
        keep_from <- length(self$conversation_history) - self$max_context_length + 1
        self$conversation_history <- self$conversation_history[keep_from:length(self$conversation_history)]
        
        private$log_message("Context trimmed to maintain length limit")
      }
    },
    
    # Handle tool calls automatically
    handle_tool_calls = function(result) {
      iteration_count <- 0
      max_iterations <- 10  # Prevent infinite loops
      
      while (inherits(result, "ToolCall") && iteration_count < max_iterations) {
        iteration_count <- iteration_count + 1
        
        # Execute the tool
        tool_result <- self$execute_tool(result)
        
        # Send result back to chat
        result <- self$chat$chat(tool_result)
      }
      
      if (iteration_count >= max_iterations) {
        private$log_message("Warning: Maximum tool call iterations reached")
      }
      
      return(result)
    },
    
    # Register default tools that all agents should have
    register_default_tools = function() {
      # Get current time tool
      get_time_tool <- ellmer::tool(
        function(timezone = "UTC") {
          format(Sys.time(), tz = timezone, usetz = TRUE)
        },
        name = "get_current_time",
        description = "Get the current date and time in the specified timezone",
        arguments = list(
          timezone = ellmer::type_string("Timezone (e.g., 'UTC', 'America/New_York', 'Europe/London')")
        )
      )
      
      # Calculate tool
      calculate_tool <- ellmer::tool(
        function(expression) {
          tryCatch({
            result <- eval(parse(text = expression))
            as.character(result)
          }, error = function(e) {
            paste("Error:", e$message)
          })
        },
        name = "calculate",
        description = "Evaluate mathematical expressions and calculations",
        arguments = list(
          expression = ellmer::type_string("Mathematical expression to evaluate (e.g., '2+2', 'sqrt(16)', 'mean(c(1,2,3))')")
        )
      )
      
      # Memory tool for important information
      memory_store <- new.env()
      
      remember_tool <- ellmer::tool(
        function(key, value) {
          memory_store[[key]] <- value
          paste("Remembered:", key, "=", value)
        },
        name = "remember",
        description = "Store important information for later use",
        arguments = list(
          key = ellmer::type_string("Key to store the information under"),
          value = ellmer::type_string("Value to remember")
        )
      )
      
      recall_tool <- ellmer::tool(
        function(key) {
          if (exists(key, envir = memory_store)) {
            memory_store[[key]]
          } else {
            paste("No memory found for key:", key)
          }
        },
        name = "recall",
        description = "Retrieve previously stored information",
        arguments = list(
          key = ellmer::type_string("Key to retrieve information for")
        )
      )
      
      # Register all default tools
      self$register_tool(get_time_tool)
      self$register_tool(calculate_tool)
      self$register_tool(remember_tool)
      self$register_tool(recall_tool)
    }
  )
)