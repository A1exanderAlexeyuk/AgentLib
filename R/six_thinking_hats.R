#' Six Thinking Hats Discussion Framework
#'
#' @description
#' Implements Edward de Bono's Six Thinking Hats method for structured thinking
#' and discussion using multiple AI agents with different perspectives.
#'
#' @import R6
#' @export
SixThinkingHats <- R6::R6Class(
  "SixThinkingHats",
  public = list(
    #' @field hats List of hat agents
    hats = NULL,
    
    #' @field topic Current discussion topic
    topic = NULL,
    
    #' @field discussion_history History of the discussion
    discussion_history = NULL,
    
    #' @field facilitator Optional facilitator agent
    facilitator = NULL,
    
    #' @field provider Default LLM provider for all hats
    provider = NULL,
    
    #' @description
    #' Initialize Six Thinking Hats discussion
    #' @param provider LLM provider to use (can be different per hat)
    #' @param temperature Temperature for generation
    #' @param use_facilitator Whether to use a facilitator agent
    #' @return A new SixThinkingHats object
    initialize = function(provider = "openai/gpt-4", 
                         temperature = 0.7,
                         use_facilitator = TRUE) {
      
      self$provider <- provider
      self$hats <- list()
      self$discussion_history <- list()
      
      # Initialize the six hats with their specific prompts
      private$initialize_hats(provider, temperature)
      
      # Initialize facilitator if requested
      if (use_facilitator) {
        self$facilitator <- BaseAgent$new(
          provider = provider,
          system_prompt = private$get_facilitator_prompt(),
          temperature = temperature,
          auto_execute_tools = FALSE,
          log_interactions = FALSE
        )
      }
      
      message("Six Thinking Hats framework initialized")
    },
    
    #' @description
    #' Start a discussion on a topic
    #' @param topic The topic to discuss
    #' @param rounds Number of discussion rounds
    #' @param hat_order Custom order of hats (default: white, red, black, yellow, green, blue)
    #' @return Discussion results
    discuss = function(topic, rounds = 1, hat_order = NULL) {
      self$topic <- topic
      
      # Default hat order if not specified
      if (is.null(hat_order)) {
        hat_order <- c("white", "red", "black", "yellow", "green", "blue")
      }
      
      # Clear previous discussion
      self$discussion_history <- list()
      
      # Conduct discussion rounds
      for (round in 1:rounds) {
        cat(sprintf("\n=== Round %d of %d ===\n", round, rounds))
        
        round_responses <- list()
        
        # Each hat provides their perspective
        for (hat_name in hat_order) {
          if (hat_name %in% names(self$hats)) {
            response <- private$get_hat_response(hat_name, topic, round)
            round_responses[[hat_name]] <- response
            
            # Display response
            cat(sprintf("\n%s Hat (%s):\n%s\n", 
                       private$get_hat_emoji(hat_name),
                       toupper(hat_name), 
                       response))
            cat(rep("-", 50), "\n", sep = "")
          }
        }
        
        # Store round in history
        self$discussion_history[[round]] <- round_responses
        
        # Get facilitator summary if available
        if (!is.null(self$facilitator) && round == rounds) {
          summary <- private$get_facilitator_summary()
          cat("\n📋 FACILITATOR SUMMARY:\n", summary, "\n")
          self$discussion_history[["facilitator_summary"]] <- summary
        }
      }
      
      return(invisible(self$discussion_history))
    },
    
    #' @description
    #' Conduct a focused discussion with specific hats
    #' @param topic The topic to discuss
    #' @param hats Vector of hat names to include
    #' @return Discussion results
    focused_discussion = function(topic, hats = c("white", "black", "yellow")) {
      self$discuss(topic, rounds = 1, hat_order = hats)
    },
    
    #' @description
    #' Get consensus from all hats
    #' @param topic The topic to get consensus on
    #' @return Consensus statement
    get_consensus = function(topic = NULL) {
      if (is.null(topic)) topic <- self$topic
      
      # First get all perspectives
      self$discuss(topic, rounds = 1)
      
      # Ask for consensus
      consensus_prompt <- sprintf(
        "Based on all the Six Thinking Hats perspectives on '%s', what is the consensus view? 
        Synthesize the key points from each hat into a balanced conclusion.",
        topic
      )
      
      if (!is.null(self$facilitator)) {
        consensus <- self$facilitator$ask(consensus_prompt, tools_enabled = FALSE)
      } else {
        # Use blue hat for synthesis
        consensus <- self$hats$blue$ask(consensus_prompt, tools_enabled = FALSE)
      }
      
      return(consensus)
    },
    
    #' @description
    #' Brainstorm solutions using green hat with input from others
    #' @param problem Problem statement
    #' @param constraints Optional constraints
    #' @return List of creative solutions
    brainstorm = function(problem, constraints = NULL) {
      # First get critical analysis
      black_analysis <- self$hats$black$ask(
        sprintf("What are the key challenges with: %s", problem),
        tools_enabled = FALSE
      )
      
      # Get emotional perspective
      red_perspective <- self$hats$red$ask(
        sprintf("What are the emotional aspects of: %s", problem),
        tools_enabled = FALSE
      )
      
      # Generate solutions with green hat
      green_prompt <- sprintf(
        "Problem: %s\n\nChallenges identified: %s\n\nEmotional aspects: %s\n\n%s\n\n
        Generate 5 creative and innovative solutions.",
        problem,
        black_analysis,
        red_perspective,
        if (!is.null(constraints)) paste("Constraints:", constraints) else ""
      )
      
      solutions <- self$hats$green$ask(green_prompt, tools_enabled = FALSE)
      
      return(list(
        problem = problem,
        challenges = black_analysis,
        emotional_aspects = red_perspective,
        solutions = solutions
      ))
    },
    
    #' @description
    #' Analyze a decision from all perspectives
    #' @param decision The decision to analyze
    #' @param context Additional context
    #' @return Analysis from all hats
    analyze_decision = function(decision, context = NULL) {
      analysis_topic <- sprintf(
        "Analyze this decision: %s%s",
        decision,
        if (!is.null(context)) paste("\n\nContext:", context) else ""
      )
      
      # Get structured analysis
      result <- self$discuss(analysis_topic, rounds = 1)
      
      # Add decision matrix
      matrix <- private$create_decision_matrix(result[[1]])
      result$decision_matrix <- matrix
      
      return(result)
    },
    
    #' @description
    #' Export discussion to markdown
    #' @param filename Output filename
    #' @param include_metadata Whether to include metadata
    export_discussion = function(filename = NULL, include_metadata = TRUE) {
      if (is.null(filename)) {
        filename <- sprintf("six_hats_discussion_%s.md", 
                           format(Sys.time(), "%Y%m%d_%H%M%S"))
      }
      
      content <- c(
        sprintf("# Six Thinking Hats Discussion: %s", self$topic),
        sprintf("\nDate: %s", Sys.Date())
      )
      
      if (include_metadata) {
        content <- c(content,
                    sprintf("Provider: %s", self$provider),
                    sprintf("Rounds: %d", length(self$discussion_history) - 1))
      }
      
      content <- c(content, "\n## Discussion\n")
      
      # Add each round
      for (i in seq_along(self$discussion_history)) {
        if (names(self$discussion_history)[i] != "facilitator_summary") {
          content <- c(content, sprintf("\n### Round %d\n", i))
          
          for (hat in names(self$discussion_history[[i]])) {
            content <- c(content,
                        sprintf("\n#### %s %s Hat\n", 
                                private$get_hat_emoji(hat), 
                                toupper(hat)),
                        self$discussion_history[[i]][[hat]],
                        "\n")
          }
        }
      }
      
      # Add facilitator summary if present
      if ("facilitator_summary" %in% names(self$discussion_history)) {
        content <- c(content,
                    "\n## Facilitator Summary\n",
                    self$discussion_history$facilitator_summary)
      }
      
      writeLines(content, filename)
      message(sprintf("Discussion exported to %s", filename))
    },
    
    #' @description
    #' Set a custom prompt for a specific hat
    #' @param hat_name Name of the hat
    #' @param custom_prompt Custom system prompt
    set_hat_prompt = function(hat_name, custom_prompt) {
      if (hat_name %in% names(self$hats)) {
        self$hats[[hat_name]]$set_system_prompt(custom_prompt)
        message(sprintf("Updated %s hat prompt", hat_name))
      }
    },
    
    #' @description
    #' Get statistics about the discussion
    #' @return List of statistics
    get_stats = function() {
      stats <- list(
        topic = self$topic,
        rounds = length(self$discussion_history) - 1,
        total_responses = sum(sapply(self$discussion_history, length)),
        hats_used = unique(unlist(lapply(self$discussion_history, names)))
      )
      
      # Add token usage per hat
      token_usage <- list()
      for (hat_name in names(self$hats)) {
        token_usage[[hat_name]] <- self$hats[[hat_name]]$get_stats()
      }
      stats$token_usage <- token_usage
      
      return(stats)
    },
    
    #' @description
    #' Print method
    print = function() {
      cat("Six Thinking Hats Discussion Framework\n")
      cat(sprintf("Provider: %s\n", self$provider))
      cat(sprintf("Hats initialized: %s\n", 
                 paste(names(self$hats), collapse = ", ")))
      cat(sprintf("Current topic: %s\n", 
                 if (!is.null(self$topic)) self$topic else "None"))
      cat(sprintf("Discussion rounds: %d\n", 
                 length(self$discussion_history)))
    }
  ),
  
  private = list(
    #' @description
    #' Initialize all six hats with their specific prompts
    initialize_hats = function(provider, temperature) {
      # White Hat - Facts and Information
      self$hats$white <- BaseAgent$new(
        provider = provider,
        system_prompt = private$get_white_hat_prompt(),
        temperature = temperature * 0.5,  # Lower temperature for factual
        auto_execute_tools = TRUE,
        log_interactions = FALSE
      )
      
      # Red Hat - Emotions and Intuition
      self$hats$red <- BaseAgent$new(
        provider = provider,
        system_prompt = private$get_red_hat_prompt(),
        temperature = temperature * 1.2,  # Higher temperature for creativity
        auto_execute_tools = FALSE,
        log_interactions = FALSE
      )
      
      # Black Hat - Caution and Critical Thinking
      self$hats$black <- BaseAgent$new(
        provider = provider,
        system_prompt = private$get_black_hat_prompt(),
        temperature = temperature * 0.7,
        auto_execute_tools = TRUE,
        log_interactions = FALSE
      )
      
      # Yellow Hat - Optimism and Benefits
      self$hats$yellow <- BaseAgent$new(
        provider = provider,
        system_prompt = private$get_yellow_hat_prompt(),
        temperature = temperature * 0.9,
        auto_execute_tools = FALSE,
        log_interactions = FALSE
      )
      
      # Green Hat - Creativity and Innovation
      self$hats$green <- BaseAgent$new(
        provider = provider,
        system_prompt = private$get_green_hat_prompt(),
        temperature = temperature * 1.5,  # Highest temperature for creativity
        auto_execute_tools = FALSE,
        log_interactions = FALSE
      )
      
      # Blue Hat - Process Control and Organization
      self$hats$blue <- BaseAgent$new(
        provider = provider,
        system_prompt = private$get_blue_hat_prompt(),
        temperature = temperature * 0.8,
        auto_execute_tools = TRUE,
        log_interactions = FALSE
      )
    },
    
    #' @description
    #' Get response from a specific hat
    get_hat_response = function(hat_name, topic, round) {
      context <- NULL
      
      # Add previous responses as context for later hats
      if (round == 1 && length(self$discussion_history) == 0) {
        # First response in first round
        context <- sprintf("Topic: %s", topic)
      } else {
        # Build context from previous responses
        previous_responses <- c()
        
        # Get responses from current round
        if (round <= length(self$discussion_history)) {
          for (prev_hat in names(self$discussion_history[[round]])) {
            if (prev_hat != hat_name) {
              previous_responses <- c(previous_responses,
                                    sprintf("%s hat: %s", 
                                           toupper(prev_hat), 
                                           substr(self$discussion_history[[round]][[prev_hat]], 1, 200)))
            }
          }
        }
        
        if (length(previous_responses) > 0) {
          context <- sprintf("Topic: %s\n\nPrevious perspectives:\n%s",
                           topic,
                           paste(previous_responses, collapse = "\n"))
        } else {
          context <- sprintf("Topic: %s", topic)
        }
      }
      
      # Get response from hat
      prompt <- sprintf("Provide your perspective on: %s", topic)
      response <- self$hats[[hat_name]]$ask(prompt, context = context, tools_enabled = FALSE)
      
      return(as.character(response))
    },
    
    #' @description
    #' Get facilitator summary
    get_facilitator_summary = function() {
      # Build comprehensive context
      all_responses <- c()
      for (round in self$discussion_history) {
        if (!is.null(names(round))) {
          for (hat in names(round)) {
            all_responses <- c(all_responses,
                             sprintf("%s hat: %s", toupper(hat), round[[hat]]))
          }
        }
      }
      
      summary_prompt <- sprintf(
        "As a facilitator, summarize the Six Thinking Hats discussion on '%s'. 
        Include key insights from each hat, areas of agreement and disagreement, 
        and provide a balanced conclusion with actionable recommendations.
        
        Discussion:\n%s",
        self$topic,
        paste(all_responses, collapse = "\n\n")
      )
      
      summary <- self$facilitator$ask(summary_prompt, tools_enabled = FALSE)
      return(as.character(summary))
    },
    
    #' @description
    #' Create decision matrix from responses
    create_decision_matrix = function(responses) {
      matrix <- data.frame(
        Hat = c("White", "Red", "Black", "Yellow", "Green", "Blue"),
        Perspective = c("Facts", "Emotions", "Risks", "Benefits", "Innovation", "Process"),
        Key_Points = sapply(names(responses), function(hat) {
          # Extract first sentence or key point
          text <- responses[[hat]]
          sentences <- strsplit(text, "\\. ")[[1]]
          if (length(sentences) > 0) sentences[1] else text
        }),
        stringsAsFactors = FALSE
      )
      
      return(matrix)
    },
    
    #' @description
    #' Get emoji for each hat
    get_hat_emoji = function(hat_name) {
      emojis <- list(
        white = "⚪",
        red = "❤️",
        black = "⚫",
        yellow = "💛",
        green = "💚",
        blue = "💙"
      )
      return(emojis[[hat_name]] %||% "🎩")
    },
    
    # System prompts for each hat
    get_white_hat_prompt = function() {
      "You are wearing the WHITE HAT in a Six Thinking Hats discussion. 
      Focus ONLY on facts, data, and objective information. 
      Present verified information, statistics, and evidence.
      Avoid interpretations, opinions, or judgments.
      Ask questions about what information is needed or missing.
      Be neutral and objective."
    },
    
    get_red_hat_prompt = function() {
      "You are wearing the RED HAT in a Six Thinking Hats discussion.
      Express emotions, feelings, hunches, and intuitions.
      Share gut reactions without needing to justify them.
      Consider how others might feel emotionally about the topic.
      Be honest about fears, likes, dislikes, and instincts.
      No need for logical reasoning - feelings are valid as they are."
    },
    
    get_black_hat_prompt = function() {
      "You are wearing the BLACK HAT in a Six Thinking Hats discussion.
      Focus on caution, critical thinking, and identifying problems.
      Point out risks, weaknesses, and potential failures.
      Be logical and realistic about what could go wrong.
      Identify obstacles and challenges that need to be addressed.
      Play devil's advocate constructively."
    },
    
    get_yellow_hat_prompt = function() {
      "You are wearing the YELLOW HAT in a Six Thinking Hats discussion.
      Focus on optimism, benefits, and positive outcomes.
      Identify opportunities and advantages.
      Look for the value and benefits in ideas.
      Be constructive and find ways to make things work.
      Maintain logical positivity based on potential benefits."
    },
    
    get_green_hat_prompt = function() {
      "You are wearing the GREEN HAT in a Six Thinking Hats discussion.
      Focus on creativity, innovation, and new ideas.
      Generate alternatives and novel solutions.
      Think outside the box and challenge assumptions.
      Build on ideas and explore possibilities.
      Use lateral thinking and avoid judgment of ideas."
    },
    
    get_blue_hat_prompt = function() {
      "You are wearing the BLUE HAT in a Six Thinking Hats discussion.
      Focus on process control and organization of thinking.
      Summarize conclusions and decisions.
      Define the problem and set thinking objectives.
      Manage the thinking process and ensure all aspects are covered.
      Provide overview and metacognitive perspective."
    },
    
    get_facilitator_prompt = function() {
      "You are a skilled facilitator for Six Thinking Hats discussions.
      Your role is to synthesize perspectives from all hats objectively.
      Identify common themes, conflicts, and key insights.
      Provide balanced summaries that honor each perspective.
      Suggest actionable next steps based on the discussion.
      Maintain neutrality while guiding toward productive outcomes."
    }
  )
)