# AgentLib: Advanced AI Agent Framework for R

[![R-CMD-check](https://github.com/A1exanderAlexeyuk/AgentLib/workflows/R-CMD-check/badge.svg)](https://github.com/A1exanderAlexeyuk/AgentLib/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/AgentLib)](https://CRAN.R-project.org/package=AgentLib)

AgentLib is a comprehensive R package for building advanced AI agents using the [ellmer](https://ellmer.tidyverse.org) framework. It provides a robust foundation for creating conversational AI systems with specialized capabilities for medical writing, data analytics, and custom applications.

## Features

- **🤖 Multiple Agent Types**: Base, Medical Writer, and Data Analytics agents
- **🔧 Tool Management**: Built-in tools with support for custom tool creation
- **💬 Conversation Handling**: Automatic context management and conversation history
- **📊 Performance Monitoring**: Cost tracking, token usage, and performance metrics
- **🔄 Batch Processing**: Process multiple prompts efficiently
- **📝 Comprehensive Logging**: Detailed interaction logs and debugging support
- **🏭 Agent Factory**: Quick agent creation with pre-configured settings
- **⚡ Auto-execution**: Automatic tool call execution and response handling

## Installation

```r
# Install from GitHub (development version)
# install.packages("devtools")
devtools::install_github("A1exanderAlexeyuk/AgentLib")

# Load the package
library(AgentLib)
```

## Quick Start

### Basic Agent

```r
library(AgentLib)

# Create a basic agent
agent <- BaseAgent$new(
  provider = "openai/gpt-4",
  agent_name = "MyAssistant",
  system_prompt = "You are a helpful AI assistant."
)

# Have a conversation
response <- agent$ask("What is machine learning?")
print(response)

# Check agent status
status <- agent$get_status()
print(status)
```

### Medical Writer Agent

```r
# Create medical writing agent
medical_agent <- MedicalWriterAgent$new(
  provider = "openai/gpt-4",
  agent_name = "MedicalExpert"
)

# Create a clinical protocol
protocol <- medical_agent$create_protocol(
  study_title = "Efficacy of Novel Drug XYZ in Hypertension",
  indication = "Essential Hypertension", 
  study_design = "Randomized, Double-blind, Placebo-controlled Trial",
  primary_endpoint = "Change in systolic BP from baseline at 12 weeks",
  sample_size = "300 patients"
)

# Generate regulatory summary
reg_summary <- medical_agent$create_regulatory_summary(
  drug_name = "DrugXYZ",
  indication = "Hypertension", 
  submission_type = "NDA",
  key_findings = "Significant BP reduction with good safety profile"
)
```

### Data Analytics Agent

```r
# Create analytics agent
analytics_agent <- DataAnalyticsAgent$new(
  provider = "openai/gpt-4",
  agent_name = "DataScientist"
)

# Load and analyze data
data_summary <- analytics_agent$load_dataset(
  dataset_name = "sales_data",
  description = "Monthly sales performance data"
)

# Perform statistical analysis
analysis <- analytics_agent$analyze_data(
  dataset_name = "sales_data",
  analysis_type = "descriptive", 
  variables = "revenue, units_sold",
  hypothesis = "Revenue correlates with units sold"
)

# Create visualizations
plot_result <- analytics_agent$create_visualization(
  dataset_name = "sales_data",
  plot_type = "scatter",
  variables = "units_sold, revenue", 
  title = "Sales Performance Analysis"
)

# Build predictive model
model <- analytics_agent$build_model(
  dataset_name = "sales_data",
  model_type = "linear_regression",
  target_variable = "revenue",
  feature_variables = "units_sold, region"
)
```

## Agent Factory

Use the agent factory for quick agent creation:

```r
# Create agents using factory function
base_agent <- create_agent("base", provider = "openai/gpt-4")
medical_agent <- create_agent("medical", provider = "anthropic/claude-3")
analytics_agent <- create_agent("analytics", provider = "openai/gpt-4")

# All agents are ready to use immediately
response1 <- base_agent$ask("Hello!")
response2 <- medical_agent$ask("What is pharmacovigilance?") 
response3 <- analytics_agent$ask("Explain cross-validation")
```

## Custom Tools

Extend agents with custom functionality:

```r
# Create custom tool
sentiment_tool <- ellmer::tool(
  function(text) {
    # Simple sentiment analysis
    positive_words <- c("good", "great", "excellent", "amazing")
    negative_words <- c("bad", "terrible", "awful", "horrible")
    
    text_lower <- tolower(text)
    pos_count <- sum(sapply(positive_words, function(w) grepl(w, text_lower)))
    neg_count <- sum(sapply(negative_words, function(w) grepl(w, text_lower)))
    
    if (pos_count > neg_count) return("Positive")
    if (neg_count > pos_count) return("Negative") 
    return("Neutral")
  },
  name = "analyze_sentiment",
  description = "Analyze the sentiment of text",
  arguments = list(
    text = ellmer::type_string("Text to analyze for sentiment")
  )
)

# Register with agent
agent$register_tool(sentiment_tool)

# Use the custom tool
result <- agent$ask("Analyze the sentiment: 'This product is amazing and I love it!'")
```

## Advanced Features

### Batch Processing

```r
# Process multiple prompts
prompts <- c(
  "What is artificial intelligence?",
  "Explain machine learning", 
  "What are neural networks?"
)

results <- batch_process(
  agent = agent,
  prompts = prompts,
  delay_seconds = 1,
  save_results = TRUE
)
```

### Agent Comparison

```r
# Compare responses from different agents
comparison <- compare_agents(
  agents = list(base_agent, medical_agent),
  prompt = "What are clinical trials?",
  agent_names = c("BaseAgent", "MedicalAgent")
)
```

### Performance Monitoring

```r
# Monitor agent performance
monitoring <- monitor_agent(
  agent = agent,
  duration_minutes = 30,
  check_interval_seconds = 300
)

# Validate agent configuration
validation <- validate_agent(agent, run_tests = TRUE)

# Export configuration
config <- export_agent_config(agent, include_tools = TRUE)
```

### Conversation Management

```r
# Save and load conversations
agent$save_conversation("my_session.rds")
agent$load_conversation("my_session.rds")

# Clear history
agent$clear_history()

# Get conversation statistics
turns <- agent$get_turns()
tokens <- agent$get_tokens()
cost <- agent$get_cost()
```

## Built-in Tools

All agents come with default tools:

- **get_current_time**: Get current date/time in any timezone
- **calculate**: Evaluate mathematical expressions  
- **remember/recall**: Store and retrieve information
- **Medical agents add**: Medical terminology lookup, citation formatting, compliance checking
- **Analytics agents add**: Data loading, statistical analysis, visualization, machine learning

## Architecture

AgentLib is built on the ellmer framework and follows R6 object-oriented patterns:

```
BaseAgent (core functionality)
├── MedicalWriterAgent (medical writing tools)
└── DataAnalyticsAgent (analytics tools)
```

Each agent includes:
- Chat interface via ellmer
- Tool registry and management
- Conversation history and context
- Logging and monitoring
- Cost tracking
- Error handling

## Examples

Comprehensive examples are available:

```r
# View basic usage examples
file.edit(system.file("examples", "basic_usage.R", package = "AgentLib"))

# View advanced examples
file.edit(system.file("examples", "advanced_examples.R", package = "AgentLib"))
```

## Configuration

### Environment Variables

Set your API keys:

```bash
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
```

### Provider Options

Supported providers (via ellmer):
- OpenAI: `"openai/gpt-4"`, `"openai/gpt-3.5-turbo"`
- Anthropic: `"anthropic/claude-3-opus"`, `"anthropic/claude-3-sonnet"`  
- And more via ellmer

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Requirements

- R (>= 4.0)
- ellmer package
- R6 package
- Internet connection for LLM API calls

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use AgentLib in your research, please cite:

```bibtex
@software{agentlib2024,
  author = {Alexander Alexeyuk},
  title = {AgentLib: Advanced AI Agent Framework for R},
  year = {2024},
  url = {https://github.com/A1exanderAlexeyuk/AgentLib}
}
```

## Support

- 📖 Documentation: See function help pages and examples
- 🐛 Bug Reports: [GitHub Issues](https://github.com/A1exanderAlexeyuk/AgentLib/issues)  
- 💡 Feature Requests: [GitHub Issues](https://github.com/A1exanderAlexeyuk/AgentLib/issues)
- 📧 Questions: Create a discussion on GitHub

## Roadmap

- [ ] Additional specialized agents (Financial, Legal, etc.)
- [ ] Integration with more LLM providers
- [ ] Async processing capabilities
- [ ] Web interface via Shiny
- [ ] Advanced workflow orchestration
- [ ] Multi-modal capabilities (images, audio)
- [ ] Integration with vector databases
- [ ] Advanced memory systems

---

Built with ❤️ using [ellmer](https://ellmer.tidyverse.org) and R6.