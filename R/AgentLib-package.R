#' AgentLib: Advanced AI Agent Framework for R
#'
#' @description
#' AgentLib provides a comprehensive framework for building AI agents in R using
#' the ellmer package. It includes base agent functionality and specialized agents
#' for medical writing and data analytics, along with utilities for tool management,
#' conversation handling, and performance monitoring.
#'
#' @section Main Classes:
#' \describe{
#'   \item{\code{\link{BaseAgent}}}{Core agent class with chat, tools, logging, and context management}
#'   \item{\code{\link{MedicalWriterAgent}}}{Specialized agent for medical writing and clinical documentation}
#'   \item{\code{\link{DataAnalyticsAgent}}}{Specialized agent for data analysis and machine learning tasks}
#' }
#'
#' @section Key Features:
#' \itemize{
#'   \item Unified interface to multiple LLM providers via ellmer
#'   \item Automatic tool execution and management
#'   \item Conversation history and context management
#'   \item Comprehensive logging and monitoring
#'   \item Cost tracking and token usage statistics
#'   \item Extensible architecture for custom agents
#'   \item Built-in tools for common tasks
#'   \item Support for batch processing and agent comparison
#' }
#'
#' @section Getting Started:
#' 
#' Create a basic agent:
#' \preformatted{
#' library(AgentLib)
#' 
#' # Create a basic agent
#' agent <- BaseAgent$new(
#'   provider = "openai/gpt-4",
#'   agent_name = "MyAssistant"
#' )
#' 
#' # Have a conversation
#' response <- agent$ask("Hello, how can you help me?")
#' }
#' 
#' Create specialized agents:
#' \preformatted{
#' # Medical writing agent
#' medical_agent <- MedicalWriterAgent$new()
#' protocol <- medical_agent$create_protocol(
#'   study_title = "My Clinical Study",
#'   indication = "Hypertension",
#'   study_design = "RCT",
#'   primary_endpoint = "Blood pressure reduction",
#'   sample_size = "200 patients"
#' )
#' 
#' # Data analytics agent  
#' analytics_agent <- DataAnalyticsAgent$new()
#' data_summary <- analytics_agent$load_dataset(
#'   dataset_name = "my_data",
#'   description = "Sales data for analysis"
#' )
#' }
#'
#' @section Agent Factory:
#' Use the agent factory for quick agent creation:
#' \preformatted{
#' # Create agents using factory
#' base_agent <- create_agent("base")
#' medical_agent <- create_agent("medical") 
#' analytics_agent <- create_agent("analytics")
#' }
#'
#' @section Custom Tools:
#' Extend agents with custom tools:
#' \preformatted{
#' # Create custom tool
#' my_tool <- ellmer::tool(
#'   function(input) {
#'     # Tool implementation
#'     return(paste("Processed:", input))
#'   },
#'   name = "my_custom_tool",
#'   description = "A custom tool for processing input",
#'   arguments = list(
#'     input = ellmer::type_string("Input to process")
#'   )
#' )
#' 
#' # Register with agent
#' agent$register_tool(my_tool)
#' }
#'
#' @section Monitoring and Utilities:
#' \itemize{
#'   \item \code{\link{batch_process}()}: Process multiple prompts
#'   \item \code{\link{compare_agents}()}: Compare agent responses
#'   \item \code{\link{monitor_agent}()}: Monitor agent performance
#'   \item \code{\link{validate_agent}()}: Validate agent configuration
#'   \item \code{\link{export_agent_config}()}: Export agent settings
#' }
#'
#' @author Alexander Alexeyuk
#' @docType package
#' @name AgentLib-package
#' @aliases AgentLib
#' @keywords package
#' 
#' @import R6
#' @importFrom ellmer chat tool type_string type_number type_boolean
#' 
#' @examples
#' \dontrun{
#' # Basic usage
#' library(AgentLib)
#' 
#' # Create and use a base agent
#' agent <- BaseAgent$new(provider = "openai/gpt-4")
#' response <- agent$ask("What is machine learning?")
#' 
#' # Create specialized agents
#' medical <- create_agent("medical")
#' analytics <- create_agent("analytics")
#' 
#' # Batch processing
#' prompts <- c("Question 1", "Question 2", "Question 3")
#' results <- batch_process(agent, prompts)
#' 
#' # Monitor performance
#' status <- agent$get_status()
#' validation <- validate_agent(agent)
#' }
NULL