# Advanced Examples for AgentLib
#
# This script demonstrates advanced usage patterns, custom tool creation,
# and complex workflows using the AgentLib package.

library(AgentLib)

# =============================================================================
# 1. Custom Tool Creation and Registration
# =============================================================================

cat("=== Creating Custom Tools ===\n")

# Create a custom tool for text analysis
text_analysis_tool <- ellmer::tool(
  function(text, analysis_type = "sentiment") {
    # Simplified text analysis (in practice would use proper NLP libraries)
    word_count <- length(unlist(strsplit(text, "\\s+")))
    char_count <- nchar(text)
    
    result <- switch(analysis_type,
      "sentiment" = {
        positive_words <- c("good", "great", "excellent", "amazing", "wonderful", "fantastic")
        negative_words <- c("bad", "terrible", "awful", "horrible", "disappointing")
        
        text_lower <- tolower(text)
        pos_count <- sum(sapply(positive_words, function(w) grepl(w, text_lower)))
        neg_count <- sum(sapply(negative_words, function(w) grepl(w, text_lower)))
        
        sentiment <- if (pos_count > neg_count) "Positive" else if (neg_count > pos_count) "Negative" else "Neutral"
        list(sentiment = sentiment, positive_words = pos_count, negative_words = neg_count)
      },
      "readability" = {
        sentences <- length(unlist(strsplit(text, "[.!?]+")))
        avg_words_per_sentence <- word_count / max(sentences, 1)
        complexity <- if (avg_words_per_sentence > 20) "Complex" else if (avg_words_per_sentence > 15) "Moderate" else "Simple"
        list(complexity = complexity, avg_words_per_sentence = round(avg_words_per_sentence, 1))
      },
      "basic" = list(word_count = word_count, character_count = char_count)
    )
    
    return(paste(names(result), result, sep = ": ", collapse = ", "))
  },
  name = "analyze_text",
  description = "Analyze text for sentiment, readability, or basic statistics",
  arguments = list(
    text = ellmer::type_string("Text to analyze"),
    analysis_type = ellmer::type_string("Type of analysis: sentiment, readability, or basic")
  )
)

# Create custom web search simulation tool
web_search_tool <- ellmer::tool(
  function(query, num_results = 5) {
    # Simulate web search results
    sample_results <- list(
      "machine learning" = c(
        "Introduction to Machine Learning - MIT OpenCourseWare",
        "Machine Learning Course - Coursera",
        "Scikit-learn: Machine Learning in Python",
        "Google's Machine Learning Crash Course",
        "Machine Learning Mastery - Blog and Tutorials"
      ),
      "clinical trials" = c(
        "ClinicalTrials.gov - NIH Database",
        "FDA Guidance for Industry - Clinical Trials",
        "Good Clinical Practice Guidelines - ICH",
        "Clinical Trial Design and Management",
        "Phases of Clinical Trials - Cancer.gov"
      ),
      "data visualization" = c(
        "ggplot2: Elegant Graphics for Data Analysis",
        "D3.js - Data-Driven Documents",
        "Tableau - Business Intelligence and Analytics",
        "Python Matplotlib Tutorial",
        "R Shiny for Interactive Visualizations"
      )
    )
    
    # Find matching results
    query_lower <- tolower(query)
    matching_key <- names(sample_results)[sapply(names(sample_results), function(k) grepl(k, query_lower))]
    
    if (length(matching_key) > 0) {
      results <- sample_results[[matching_key[1]]][1:min(num_results, length(sample_results[[matching_key[1]]]))]
    } else {
      results <- paste("Sample result", 1:num_results, "for query:", query)
    }
    
    return(paste("Search Results for '", query, "':\n", paste(1:length(results), ". ", results, collapse = "\n"), sep = ""))
  },
  name = "web_search",
  description = "Search the web for information on a given topic",
  arguments = list(
    query = ellmer::type_string("Search query"),
    num_results = ellmer::type_number("Number of results to return (default 5)")
  )
)

# Create enhanced agent with custom tools
enhanced_agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "EnhancedAgent",
  system_prompt = paste(
    "You are an advanced AI assistant with text analysis and web search capabilities.",
    "Use your tools to provide comprehensive and well-researched responses.",
    "Always cite sources when using web search results."
  ),
  logging_enabled = TRUE
)

# Register custom tools
enhanced_agent$register_tool(text_analysis_tool)
enhanced_agent$register_tool(web_search_tool)

# Test custom tools
text_analysis_result <- enhanced_agent$ask(
  "Analyze this text for sentiment: 'This is an amazing product that exceeded my expectations. The quality is fantastic and I'm very satisfied with my purchase.'"
)
cat("Text analysis result:\n", text_analysis_result, "\n\n")

search_result <- enhanced_agent$ask("Search for information about machine learning best practices")
cat("Search result:\n", search_result, "\n\n")

# =============================================================================
# 2. Multi-Agent Collaboration Workflow
# =============================================================================

cat("=== Multi-Agent Collaboration ===\n")

# Create specialized agents for collaboration
research_agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "Researcher",
  system_prompt = "You are a research specialist focused on gathering and summarizing information."
)

writing_agent <- MedicalWriterAgent$new(
  provider = "openai/gpt-4",
  agent_name = "Writer"
)

analysis_agent <- DataAnalyticsAgent$new(
  provider = "openai/gpt-4",  
  agent_name = "Analyst"
)

# Add web search to research agent
research_agent$register_tool(web_search_tool)

# Collaborative workflow: Research -> Analysis -> Writing
topic <- "Impact of AI in Healthcare"

# Step 1: Research phase
cat("Step 1: Research Phase\n")
research_prompt <- paste("Research the topic:", topic, 
                        "Provide key findings, recent developments, and important statistics.")
research_findings <- research_agent$ask(research_prompt)
cat("Research findings:\n", substr(research_findings, 1, 200), "...\n\n")

# Step 2: Analysis phase  
cat("Step 2: Analysis Phase\n")
analysis_prompt <- paste("Based on these research findings about", topic, ":\n", 
                        research_findings, "\n\nGenerate sample data and perform analysis to support key claims.")
analysis_results <- analysis_agent$ask(analysis_prompt)
cat("Analysis results:\n", substr(analysis_results, 1, 200), "...\n\n")

# Step 3: Writing phase
cat("Step 3: Writing Phase\n")
writing_prompt <- paste("Create a professional summary document about", topic, 
                       "using these research findings:\n", research_findings,
                       "\n\nAnd these analysis results:\n", analysis_results)
final_document <- writing_agent$ask(writing_prompt)
cat("Final document:\n", substr(final_document, 1, 300), "...\n\n")

# =============================================================================
# 3. Advanced Analytics Workflow
# =============================================================================

cat("=== Advanced Analytics Workflow ===\n")

# Create advanced analytics agent
advanced_analytics <- DataAnalyticsAgent$new(
  provider = "openai/gpt-4",
  agent_name = "AdvancedAnalytics",
  logging_enabled = TRUE
)

# Multi-step analytics workflow
cat("Creating customer segmentation analysis...\n")

# Step 1: Generate customer data
customer_data <- advanced_analytics$load_dataset(
  dataset_name = "customers",
  description = "Customer data for segmentation analysis"
)

# Step 2: Exploratory data analysis
eda_result <- advanced_analytics$analyze_data(
  dataset_name = "customers",
  analysis_type = "descriptive",
  variables = "age, income, spending, loyalty_score"
)

# Step 3: Create visualizations
viz1 <- advanced_analytics$create_visualization(
  dataset_name = "customers",
  plot_type = "scatter",
  variables = "income, spending",
  title = "Income vs Spending Pattern"
)

viz2 <- advanced_analytics$create_visualization(
  dataset_name = "customers", 
  plot_type = "histogram",
  variables = "loyalty_score",
  title = "Customer Loyalty Distribution"
)

# Step 4: Build segmentation model
segmentation_model <- advanced_analytics$build_model(
  dataset_name = "customers",
  model_type = "random_forest",
  target_variable = "segment",
  feature_variables = "age, income, spending, loyalty_score"
)

# Step 5: Generate comprehensive report
report <- advanced_analytics$generate_report(
  analysis_name = "Customer Segmentation Analysis",
  datasets = "customers",
  key_findings = "Three distinct customer segments identified with different spending patterns",
  recommendations = "Develop targeted marketing strategies for each segment"
)

cat("Analytics workflow completed. Report generated.\n\n")

# =============================================================================
# 4. Medical Research Pipeline
# =============================================================================

cat("=== Medical Research Pipeline ===\n")

# Create comprehensive medical agent
medical_researcher <- MedicalWriterAgent$new(
  provider = "openai/gpt-4",
  agent_name = "MedicalResearcher",
  logging_enabled = TRUE
)

# Add custom medical tools
drug_interaction_tool <- ellmer::tool(
  function(drug1, drug2) {
    # Simplified drug interaction checker
    interactions <- list(
      "warfarin" = c("aspirin", "ibuprofen", "amoxicillin"),
      "metformin" = c("alcohol", "contrast_dye"),
      "lisinopril" = c("potassium", "nsaids")
    )
    
    drug1_lower <- tolower(drug1)
    drug2_lower <- tolower(drug2)
    
    interaction_found <- FALSE
    if (drug1_lower %in% names(interactions) && drug2_lower %in% interactions[[drug1_lower]]) {
      interaction_found <- TRUE
    } else if (drug2_lower %in% names(interactions) && drug1_lower %in% interactions[[drug2_lower]]) {
      interaction_found <- TRUE
    }
    
    if (interaction_found) {
      return(paste("CAUTION: Potential interaction between", drug1, "and", drug2, 
                  "- Consult clinical references for detailed information"))
    } else {
      return(paste("No known major interactions between", drug1, "and", drug2, 
                  "in simplified database"))
    }
  },
  name = "check_drug_interaction",
  description = "Check for potential drug interactions between two medications",
  arguments = list(
    drug1 = ellmer::type_string("First medication name"),
    drug2 = ellmer::type_string("Second medication name")
  )
)

medical_researcher$register_tool(drug_interaction_tool)

# Medical research workflow
study_design <- medical_researcher$create_protocol(
  study_title = "Comparative Effectiveness of Digital Health Interventions in Diabetes Management",
  indication = "Type 2 Diabetes Mellitus",
  study_design = "Pragmatic Randomized Controlled Trial",
  primary_endpoint = "Change in HbA1c from baseline at 6 months",
  sample_size = "600 patients"
)

safety_check <- medical_researcher$ask("Check for interactions between metformin and lisinopril")

manuscript <- medical_researcher$create_abstract(
  title = "Digital Health Interventions for Diabetes: A Pragmatic Trial",
  study_type = "Randomized Controlled Trial",
  results_summary = "Significant improvement in glycemic control with digital intervention",
  conclusion = "Digital health tools show promise for improving diabetes outcomes",
  word_limit = 300
)

cat("Medical research pipeline completed.\n\n")

# =============================================================================
# 5. Agent Performance Monitoring and Optimization
# =============================================================================

cat("=== Performance Monitoring ===\n")

# Monitor agent performance
monitoring_agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "MonitoringTest",
  max_context_length = 10,
  logging_enabled = TRUE
)

# Test performance with multiple interactions
test_prompts <- c(
  "What is artificial intelligence?",
  "Explain machine learning briefly",
  "What are neural networks?",
  "How does deep learning work?",
  "What is natural language processing?"
)

start_time <- Sys.time()
for (i in seq_along(test_prompts)) {
  response <- monitoring_agent$ask(test_prompts[i])
  cat(sprintf("Query %d processed (length: %d chars)\n", i, nchar(response)))
}
end_time <- Sys.time()

# Performance metrics
total_time <- as.numeric(end_time - start_time, units = "secs")
status <- monitoring_agent$get_status()

cat(sprintf("Performance Summary:\n"))
cat(sprintf("- Total time: %.2f seconds\n", total_time))
cat(sprintf("- Average time per query: %.2f seconds\n", total_time / length(test_prompts)))
cat(sprintf("- Conversations processed: %d\n", status$conversation_length))
cat(sprintf("- Tools available: %d\n", status$tools_registered))

# =============================================================================
# 6. Error Handling and Robustness Testing
# =============================================================================

cat("\n=== Error Handling Testing ===\n")

# Test error handling
robust_agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "RobustTest",
  logging_enabled = TRUE
)

# Test with various edge cases
test_cases <- list(
  empty_prompt = "",
  very_long_prompt = paste(rep("This is a very long prompt.", 100), collapse = " "),
  special_chars = "Test with special characters: @#$%^&*(){}[]|\\:;\"'<>?/~`",
  mixed_languages = "Hello, こんにちは, Bonjour, Hola, Guten Tag",
  mathematical = "Calculate: ∫(x²+2x+1)dx from 0 to 5, and find lim(x→∞) (x²+1)/(2x²-3)"
)

for (test_name in names(test_cases)) {
  cat(sprintf("Testing: %s\n", test_name))
  tryCatch({
    response <- robust_agent$ask(test_cases[[test_name]])
    cat(sprintf("  Success: Response length %d characters\n", nchar(response)))
  }, error = function(e) {
    cat(sprintf("  Error: %s\n", e$message))
  })
}

# =============================================================================
# 7. Advanced Configuration and Customization
# =============================================================================

cat("\n=== Advanced Configuration ===\n")

# Create highly customized agent
custom_agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "CustomConfigured",
  system_prompt = paste(
    "You are a specialized assistant with the following characteristics:",
    "- Provide detailed, technical responses",
    "- Always include confidence levels in your answers",  
    "- Focus on practical, actionable advice",
    "- Cite sources when possible",
    "- Ask clarifying questions when needed"
  ),
  max_context_length = 15,
  auto_execute_tools = TRUE,
  logging_enabled = TRUE,
  log_file = "logs/custom_agent.log"  
)

# Test custom configuration
custom_response <- custom_agent$ask(
  "What are the best practices for implementing machine learning in production environments?"
)
cat("Custom agent response:\n", substr(custom_response, 1, 200), "...\n\n")

# Export configuration for reuse
config <- export_agent_config(custom_agent, include_tools = TRUE, include_history = FALSE)
cat("Configuration exported for reuse:\n")
str(config, max.level = 2)

# =============================================================================
# 8. Cleanup and Final Summary
# =============================================================================

cat("\n=== Advanced Examples Summary ===\n")

# Collect final statistics
all_agents <- list(
  enhanced_agent, research_agent, writing_agent, analysis_agent,
  advanced_analytics, medical_researcher, monitoring_agent,
  robust_agent, custom_agent
)

total_conversations <- sum(sapply(all_agents, function(a) a$get_status()$conversation_length))
total_tools <- sum(sapply(all_agents, function(a) a$get_status()$tools_registered))
unique_agent_types <- length(unique(sapply(all_agents, function(a) class(a)[1])))

cat(sprintf("Advanced examples completed successfully!\n"))
cat(sprintf("- Total agents created: %d\n", length(all_agents)))
cat(sprintf("- Unique agent types: %d\n", unique_agent_types))
cat(sprintf("- Total conversations: %d\n", total_conversations))
cat(sprintf("- Total tools registered: %d\n", total_tools))
cat(sprintf("- Custom tools created: 3\n"))
cat(sprintf("- Workflows demonstrated: 4\n"))

cat("\nCheck log files for detailed interaction histories.\n")
cat("All advanced features have been successfully demonstrated!\n")