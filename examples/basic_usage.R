# Basic Usage Examples for AgentLib
# 
# This script demonstrates how to use the different agent classes
# provided by the AgentLib package.

library(AgentLib)

# =============================================================================
# 1. Basic Agent Usage
# =============================================================================

cat("=== Creating Base Agent ===\n")

# Create a basic agent
base_agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "MyAssistant",
  system_prompt = "You are a helpful assistant that provides clear and concise answers.",
  logging_enabled = TRUE
)

# Basic conversation
response1 <- base_agent$ask("What is the capital of France?")
cat("Response:", response1, "\n\n")

# Using built-in tools
response2 <- base_agent$ask("What time is it in New York?")
cat("Response:", response2, "\n\n")

# Mathematical calculation
response3 <- base_agent$ask("Calculate the square root of 144")
cat("Response:", response3, "\n\n")

# Check agent status
status <- base_agent$get_status()
print(status)

# =============================================================================
# 2. Medical Writer Agent Usage
# =============================================================================

cat("\n=== Creating Medical Writer Agent ===\n")

# Create medical writer agent
medical_agent <- MedicalWriterAgent$new(
  provider = "openai/gpt-4",
  agent_name = "MedicalExpert",
  logging_enabled = TRUE
)

# Create a clinical protocol outline
protocol <- medical_agent$create_protocol(
  study_title = "Efficacy and Safety of Novel Antihypertensive Drug XYZ",
  indication = "Essential Hypertension",
  study_design = "Randomized, Double-blind, Placebo-controlled Trial",
  primary_endpoint = "Change in systolic blood pressure from baseline at 12 weeks",
  sample_size = "300 patients"
)
cat("Protocol outline:\n", protocol, "\n\n")

# Generate regulatory summary
reg_summary <- medical_agent$create_regulatory_summary(
  drug_name = "Cardioprotect-XYZ",
  indication = "Hypertension",
  submission_type = "NDA",
  key_findings = "Significant reduction in blood pressure with favorable safety profile"
)
cat("Regulatory summary:\n", reg_summary, "\n\n")

# Create medical abstract
abstract <- medical_agent$create_abstract(
  title = "Efficacy of Novel ACE Inhibitor in Hypertensive Patients",
  study_type = "Randomized Controlled Trial",
  results_summary = "30% reduction in systolic BP, 25% reduction in diastolic BP",
  conclusion = "Novel ACE inhibitor demonstrates superior efficacy with good tolerability",
  word_limit = 250
)
cat("Abstract:\n", abstract, "\n\n")

# =============================================================================
# 3. Data Analytics Agent Usage
# =============================================================================

cat("\n=== Creating Data Analytics Agent ===\n")

# Create data analytics agent
analytics_agent <- DataAnalyticsAgent$new(
  provider = "openai/gpt-4",
  agent_name = "DataScientist",
  logging_enabled = TRUE
)

# Generate sample data for analysis
sample_data <- analytics_agent$load_dataset(
  dataset_name = "sales_data",
  description = "Monthly sales data for analysis"
)
cat("Dataset loaded:\n", sample_data, "\n\n")

# Perform statistical analysis
analysis <- analytics_agent$analyze_data(
  dataset_name = "sales_data",
  analysis_type = "descriptive",
  variables = "revenue, units_sold",
  hypothesis = "Revenue correlates with units sold"
)
cat("Analysis results:\n", analysis, "\n\n")

# Create visualization
plot_result <- analytics_agent$create_visualization(
  dataset_name = "sales_data",
  plot_type = "scatter",
  variables = "units_sold, revenue",
  title = "Sales Performance Analysis"
)
cat("Visualization created:\n", plot_result, "\n\n")

# Build predictive model
model_result <- analytics_agent$build_model(
  dataset_name = "sales_data",
  model_type = "linear_regression",
  target_variable = "revenue",
  feature_variables = "units_sold, region",
  validation_method = "cv"
)
cat("Model results:\n", model_result, "\n\n")

# =============================================================================
# 4. Advanced Features
# =============================================================================

cat("\n=== Advanced Features ===\n")

# Save conversation
base_agent$save_conversation("conversation_backup.rds")
cat("Conversation saved\n")

# Export agent configuration
config <- export_agent_config(medical_agent, include_tools = TRUE)
print("Agent configuration:")
str(config)

# Validate agent setup
validation <- validate_agent(analytics_agent, run_tests = TRUE)
print("Validation results:")
str(validation$summary)

# =============================================================================
# 5. Batch Processing Example
# =============================================================================

cat("\n=== Batch Processing ===\n")

# Prepare multiple prompts
prompts <- c(
  "Explain the concept of statistical significance",
  "What is the difference between Type I and Type II errors?",
  "How do you interpret a p-value?",
  "What is the purpose of a confidence interval?"
)

# Process prompts in batch
batch_results <- batch_process(
  agent = base_agent,
  prompts = prompts,
  delay_seconds = 1,
  save_results = TRUE
)

cat("Batch processing completed. Results for", length(batch_results), "prompts.\n")

# =============================================================================
# 6. Agent Comparison
# =============================================================================

cat("\n=== Agent Comparison ===\n")

# Compare how different agents handle the same medical question
medical_question <- "What are the key considerations for designing a clinical trial?"

comparison <- compare_agents(
  agents = list(base_agent, medical_agent),
  prompt = medical_question,
  agent_names = c("BaseAgent", "MedicalAgent")
)

cat("Comparison completed:\n")
cat("Prompt:", comparison$prompt, "\n")
cat("Successful responses:", comparison$summary$successful_responses, "\n")
cat("Average response time:", round(comparison$summary$avg_response_time, 2), "seconds\n")

# =============================================================================
# 7. Using the Agent Factory
# =============================================================================

cat("\n=== Using Agent Factory ===\n")

# Create agents using the factory function
quick_medical <- create_agent("medical", provider = "openai/gpt-4", agent_name = "QuickMedical")
quick_analytics <- create_agent("analytics", provider = "openai/gpt-4", agent_name = "QuickAnalytics")

# Test the factory-created agents
medical_response <- quick_medical$ask("What is pharmacovigilance?")
analytics_response <- quick_analytics$ask("What is the purpose of cross-validation in machine learning?")

cat("Medical agent response:", medical_response, "\n")
cat("Analytics agent response:", analytics_response, "\n")

# =============================================================================
# 8. Cleanup and Summary
# =============================================================================

cat("\n=== Session Summary ===\n")

# Get final status of all agents
agents <- list(
  "Base" = base_agent,
  "Medical" = medical_agent,
  "Analytics" = analytics_agent,
  "QuickMedical" = quick_medical,
  "QuickAnalytics" = quick_analytics
)

for (name in names(agents)) {
  agent <- agents[[name]]
  status <- agent$get_status()
  cat(sprintf("%s Agent: %d conversations, %s tokens used\n", 
              name, status$conversation_length, status$tokens_used %||% "unknown"))
}

cat("\nBasic usage examples completed successfully!\n")
cat("Check the log files for detailed interaction history.\n")