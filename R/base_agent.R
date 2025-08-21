#' Base Agent Class with Multi-Provider Support
#'
#' @description
#' A flexible base class for creating AI agents using ellmer with support for
#' multiple LLM providers including OpenAI, Anthropic, Google Gemini, and AWS Bedrock.
#'
#' @import R6
#' @import ellmer
#' @export
BaseAgent <- R6::R6Class(
  "BaseAgent",
  public = list(
    #' @field chat The ellmer chat object
    chat = NULL,
    
    #' @field provider The LLM provider and model
    provider = NULL,
    
    #' @field tools List of registered tools
    tools = NULL,
    
    #' @field context_manager Context management object
    context_manager = NULL,
    
    #' @field logger Logger object
    logger = NULL,
    
    #' @field metadata Agent metadata
    metadata = NULL,
    
    #' @field conversation_history Full conversation history
    conversation_history = NULL,
    
    #' @description
    #' Initialize a new BaseAgent
    #' @param provider Character string specifying provider/model or chat object
    #' @param system_prompt System prompt for the agent
    #' @param temperature Temperature for generation (0-2)
    #' @param max_tokens Maximum tokens to generate
    #' @param auto_execute_tools Whether to automatically execute tool calls
    #' @param log_interactions Whether to log all interactions
    #' @param ... Additional arguments passed to chat provider
    #' @return A new BaseAgent object
    initialize = function(provider = "openai/gpt-4", 
                         system_prompt = "You are a helpful AI assistant.",
                         temperature = 0.7,
                         max_tokens = NULL,
                         auto_execute_tools = TRUE,
                         log_interactions = TRUE,
                         ...) {
      
      # Initialize provider
      self$provider <- provider
      self$chat <- private$create_chat_object(
        provider = provider,
        system_prompt = system_prompt,
        temperature = temperature,
        max_tokens = max_tokens,
        ...
      )
      
      # Initialize components
      self$tools <- list()
      self$conversation_history <- list()
      self$context_manager <- private$create_context_manager()
      self$logger <- private$create_logger(log_interactions)
      
      # Set metadata
      self$metadata <- list(
        agent_id = private$generate_agent_id(),
        created_at = Sys.time(),
        provider = provider,
        auto_execute_tools = auto_execute_tools,
        total_tokens = 0,
        total_cost = 0
      )
      
      # Register default tools
      private$register_default_tools()
      
      # Log initialization
      if (log_interactions) {
        self$logger$info(sprintf("Initialized %s agent with provider: %s", 
                                class(self)[1], provider))
      }
    },
    
    #' @description
    #' Send a message to the agent and get response
    #' @param message The message to send
    #' @param context Additional context to include
    #' @param tools_enabled Whether to enable tool use for this interaction
    #' @return The agent's response
    ask = function(message, context = NULL, tools_enabled = TRUE) {
      # Add context if provided
      if (!is.null(context)) {
        self$context_manager$add_context(context)
        full_message <- paste(context, "\n\n", message)
      } else {
        full_message <- message
      }
      
      # Log the interaction
      self$logger$info(sprintf("User: %s", message))
      
      # Disable tools temporarily if requested
      if (!tools_enabled && length(self$tools) > 0) {
        original_tools <- self$chat$tools
        self$chat$tools <- list()
      }
      
      # Get response
      result <- self$chat$chat(full_message)
      
      # Handle tool calls if auto-execution is enabled
      if (self$metadata$auto_execute_tools) {
        while (inherits(result, "ToolCall")) {
          tool_result <- private$execute_tool_call(result)
          result <- self$chat$chat(tool_result)
        }
      }
      
      # Restore tools if they were disabled
      if (!tools_enabled && exists("original_tools")) {
        self$chat$tools <- original_tools
      }
      
      # Update conversation history
      self$conversation_history <- append(
        self$conversation_history,
        list(list(
          timestamp = Sys.time(),
          user = message,
          assistant = result,
          context = context
        ))
      )
      
      # Update token usage
      private$update_token_usage()
      
      # Log response
      self$logger$info(sprintf("Assistant: %s", 
                              substr(as.character(result), 1, 100)))
      
      return(result)
    },
    
    #' @description
    #' Register a new tool
    #' @param tool_def Tool definition created with ellmer::tool()
    register_tool = function(tool_def) {
      tool_name <- tool_def$name
      self$tools[[tool_name]] <- tool_def
      self$chat$register_tool(tool_def)
      self$logger$info(sprintf("Registered tool: %s", tool_name))
    },
    
    #' @description
    #' Remove a registered tool
    #' @param tool_name Name of the tool to remove
    remove_tool = function(tool_name) {
      if (tool_name %in% names(self$tools)) {
        self$tools[[tool_name]] <- NULL
        # Recreate chat tools list without the removed tool
        self$chat$tools <- self$tools
        self$logger$info(sprintf("Removed tool: %s", tool_name))
      }
    },
    
    #' @description
    #' Clear conversation history
    clear_history = function() {
      self$conversation_history <- list()
      # Recreate chat object to clear internal history
      self$chat <- private$create_chat_object(
        provider = self$provider,
        system_prompt = self$chat$system_prompt,
        temperature = self$chat$temperature,
        max_tokens = self$chat$max_tokens
      )
      # Re-register tools
      for (tool in self$tools) {
        self$chat$register_tool(tool)
      }
      self$logger$info("Cleared conversation history")
    },
    
    #' @description
    #' Get conversation summary
    #' @param last_n Number of recent interactions to include
    #' @return Character string with conversation summary
    get_summary = function(last_n = NULL) {
      history <- self$conversation_history
      if (!is.null(last_n) && length(history) > last_n) {
        history <- tail(history, last_n)
      }
      
      summary_prompt <- paste(
        "Please provide a concise summary of the following conversation:",
        paste(sapply(history, function(h) {
          paste("User:", h$user, "\nAssistant:", h$assistant)
        }), collapse = "\n\n")
      )
      
      # Temporarily disable tools for summary
      original_tools <- self$chat$tools
      self$chat$tools <- list()
      
      summary <- self$chat$chat(summary_prompt)
      
      # Restore tools
      self$chat$tools <- original_tools
      
      return(summary)
    },
    
    #' @description
    #' Get token usage statistics
    #' @return List with token counts and costs
    get_stats = function() {
      tokens <- self$chat$get_tokens()
      cost <- self$chat$get_cost()
      
      list(
        total_interactions = length(self$conversation_history),
        tokens = tokens,
        cost = cost,
        provider = self$provider,
        tools_registered = length(self$tools),
        agent_uptime = difftime(Sys.time(), self$metadata$created_at, units = "hours")
      )
    },
    
    #' @description
    #' Export agent configuration
    #' @param include_history Whether to include conversation history
    #' @return List with agent configuration
    export_config = function(include_history = FALSE) {
      config <- list(
        provider = self$provider,
        system_prompt = self$chat$system_prompt,
        temperature = self$chat$temperature,
        tools = names(self$tools),
        metadata = self$metadata
      )
      
      if (include_history) {
        config$conversation_history <- self$conversation_history
      }
      
      return(config)
    },
    
    #' @description
    #' Set a new system prompt
    #' @param system_prompt New system prompt
    set_system_prompt = function(system_prompt) {
      self$chat$system_prompt <- system_prompt
      self$logger$info("Updated system prompt")
    },
    
    #' @description
    #' Print method for the agent
    print = function() {
      cat(sprintf(
        "<%s>\n  Provider: %s\n  Tools: %d registered\n  Interactions: %d\n  Status: Active\n",
        class(self)[1],
        self$provider,
        length(self$tools),
        length(self$conversation_history)
      ))
    }
  ),
  
  private = list(
    #' @description
    #' Create chat object based on provider
    create_chat_object = function(provider, system_prompt, temperature, max_tokens, ...) {
      # Parse provider string
      provider_parts <- strsplit(provider, "/")[[1]]
      provider_name <- tolower(provider_parts[1])
      model <- if (length(provider_parts) > 1) provider_parts[2] else NULL
      
      # Create appropriate chat object
      chat_obj <- switch(
        provider_name,
        "openai" = ellmer::chat_openai(
          model = model %||% "gpt-4",
          system_prompt = system_prompt,
          temperature = temperature,
          max_tokens = max_tokens,
          ...
        ),
        "anthropic" = ellmer::chat_anthropic(
          model = model %||% "claude-3-sonnet-20240229",
          system_prompt = system_prompt,
          temperature = temperature,
          max_tokens = max_tokens,
          ...
        ),
        "gemini" = ellmer::chat_gemini(
          model = model %||% "gemini-pro",
          system_prompt = system_prompt,
          temperature = temperature,
          max_tokens = max_tokens,
          ...
        ),
        "bedrock" = ellmer::chat_bedrock(
          model = model %||% "anthropic.claude-v2",
          system_prompt = system_prompt,
          temperature = temperature,
          max_tokens = max_tokens,
          ...
        ),
        # Default to generic chat() which auto-detects
        ellmer::chat(
          provider = provider,
          system_prompt = system_prompt,
          temperature = temperature,
          max_tokens = max_tokens,
          ...
        )
      )
      
      return(chat_obj)
    },
    
    #' @description
    #' Execute a tool call
    execute_tool_call = function(tool_call) {
      tool_name <- tool_call$tool_name
      args <- tool_call$args
      
      self$logger$info(sprintf("Executing tool: %s", tool_name))
      
      if (tool_name %in% names(self$tools)) {
        tool_def <- self$tools[[tool_name]]
        result <- tryCatch({
          do.call(tool_def$fun, args)
        }, error = function(e) {
          self$logger$error(sprintf("Tool execution error: %s", e$message))
          paste("Error executing tool:", e$message)
        })
        
        return(result)
      } else {
        self$logger$error(sprintf("Tool not found: %s", tool_name))
        return(paste("Error: Tool", tool_name, "not found"))
      }
    },
    
    #' @description
    #' Register default tools
    register_default_tools = function() {
      # Current time tool
      time_tool <- ellmer::tool(
        function(timezone = "UTC") {
          format(Sys.time(), tz = timezone, usetz = TRUE)
        },
        name = "get_current_time",
        description = "Get the current time in a specified timezone",
        arguments = list(
          timezone = ellmer::type_string("Timezone (e.g., 'America/New_York')")
        )
      )
      
      # Calculator tool
      calc_tool <- ellmer::tool(
        function(expression) {
          tryCatch({
            result <- eval(parse(text = expression))
            as.character(result)
          }, error = function(e) {
            paste("Calculation error:", e$message)
          })
        },
        name = "calculator",
        description = "Evaluate a mathematical expression",
        arguments = list(
          expression = ellmer::type_string("Mathematical expression to evaluate")
        )
      )
      
      # Memory tool
      memory_tool <- ellmer::tool(
        function(action = "get", key = NULL, value = NULL) {
          if (action == "set" && !is.null(key) && !is.null(value)) {
            private$memory_store[[key]] <- value
            return(paste("Stored:", key))
          } else if (action == "get" && !is.null(key)) {
            return(private$memory_store[[key]] %||% "Key not found")
          } else if (action == "list") {
            return(paste("Keys:", paste(names(private$memory_store), collapse = ", ")))
          } else {
            return("Invalid memory operation")
          }
        },
        name = "memory",
        description = "Store and retrieve information",
        arguments = list(
          action = ellmer::type_string("Action: 'get', 'set', or 'list'"),
          key = ellmer::type_string("Key for storage", required = FALSE),
          value = ellmer::type_string("Value to store", required = FALSE)
        )
      )
      
      # Register the default tools
      self$register_tool(time_tool)
      self$register_tool(calc_tool)
      self$register_tool(memory_tool)
    },
    
    #' @field memory_store Internal memory storage
    memory_store = list(),
    
    #' @description
    #' Create context manager
    create_context_manager = function() {
      list(
        contexts = list(),
        add_context = function(context) {
          self$contexts <- append(self$contexts, list(context))
        },
        get_context = function() {
          paste(self$contexts, collapse = "\n")
        },
        clear_context = function() {
          self$contexts <- list()
        }
      )
    },
    
    #' @description
    #' Create logger
    create_logger = function(enabled = TRUE) {
      list(
        enabled = enabled,
        logs = list(),
        info = function(message) {
          if (self$enabled) {
            entry <- list(
              timestamp = Sys.time(),
              level = "INFO",
              message = message
            )
            self$logs <- append(self$logs, list(entry))
            if (interactive()) {
              cat(sprintf("[%s] INFO: %s\n", 
                         format(entry$timestamp, "%H:%M:%S"), 
                         message))
            }
          }
        },
        error = function(message) {
          if (self$enabled) {
            entry <- list(
              timestamp = Sys.time(),
              level = "ERROR",
              message = message
            )
            self$logs <- append(self$logs, list(entry))
            if (interactive()) {
              cat(sprintf("[%s] ERROR: %s\n", 
                         format(entry$timestamp, "%H:%M:%S"), 
                         message))
            }
          }
        },
        get_logs = function(level = NULL) {
          if (is.null(level)) {
            return(self$logs)
          }
          Filter(function(log) log$level == level, self$logs)
        }
      )
    },
    
    #' @description
    #' Generate unique agent ID
    generate_agent_id = function() {
      paste0(
        substr(class(self)[1], 1, 3),
        "_",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "_",
        substr(digest::digest(runif(1)), 1, 6)
      )
    },
    
    #' @description
    #' Update token usage
    update_token_usage = function() {
      current_tokens <- self$chat$get_tokens()
      self$metadata$total_tokens <- sum(unlist(current_tokens))
      self$metadata$total_cost <- self$chat$get_cost()
    }
  )
)

# Helper for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x