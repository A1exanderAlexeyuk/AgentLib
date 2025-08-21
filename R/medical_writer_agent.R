#' Medical Writer Agent Class
#'
#' A specialized agent for medical writing tasks, including clinical documentation,
#' research summaries, regulatory submissions, and medical communications.
#' Inherits from BaseAgent and adds medical-specific tools and capabilities.
#'
#' @importFrom R6 R6Class
#' @importFrom ellmer tool type_string type_number type_boolean
#' @export
MedicalWriterAgent <- R6::R6Class(
  "MedicalWriterAgent",
  inherit = BaseAgent,
  public = list(
    #' @field medical_standards Medical writing standards and guidelines
    medical_standards = NULL,
    #' @field document_templates Available document templates
    document_templates = NULL,
    #' @field reference_database Medical reference database
    reference_database = NULL,
    
    #' Initialize the Medical Writer Agent
    #'
    #' @param provider LLM provider (e.g., "openai/gpt-4", "anthropic/claude-3")
    #' @param agent_name Name for this agent instance
    #' @param max_context_length Maximum conversation turns to keep in context
    #' @param auto_execute_tools Whether to automatically execute tool calls
    #' @param logging_enabled Whether to enable logging
    #' @param log_file Path to log file (optional)
    #' @param medical_standards List of medical writing standards to follow
    initialize = function(provider = "openai/gpt-4",
                         agent_name = "MedicalWriter",
                         max_context_length = 30,
                         auto_execute_tools = TRUE,
                         logging_enabled = TRUE,
                         log_file = NULL,
                         medical_standards = NULL) {
      
      # Define medical-specific system prompt
      system_prompt <- paste(
        "You are an expert Medical Writer AI assistant specializing in:",
        "- Clinical research documentation (protocols, CSRs, regulatory submissions)",
        "- Medical communications (manuscripts, abstracts, presentations)",
        "- Regulatory writing (CTDs, INDs, NDAs, marketing applications)",
        "- Medical education materials and training content",
        "- Healthcare policy and procedure documentation",
        "",
        "Key principles:",
        "- Follow ICH-GCP, FDA, EMA, and other regulatory guidelines",
        "- Ensure scientific accuracy and clinical relevance",
        "- Use appropriate medical terminology and formatting",
        "- Maintain objectivity and evidence-based writing",
        "- Consider target audience (clinicians, regulators, patients)",
        "- Follow medical writing style guides (AMA, AMWA, etc.)",
        "",
        "Always verify medical facts, use proper citations, and ensure",
        "compliance with relevant medical writing standards.",
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
      
      # Initialize medical-specific properties
      self$medical_standards <- medical_standards %||% private$get_default_standards()
      self$document_templates <- private$get_document_templates()
      self$reference_database <- new.env()
      
      # Register medical-specific tools
      private$register_medical_tools()
      
      private$log_message("Medical Writer Agent initialized with specialized medical tools")
    },
    
    #' Create a clinical study protocol
    #'
    #' @param study_title Title of the study
    #' @param indication Medical condition being studied
    #' @param study_design Type of study design (e.g., "randomized controlled trial")
    #' @param primary_endpoint Primary endpoint of the study
    #' @param sample_size Expected sample size
    #' @return Generated protocol outline
    create_protocol = function(study_title, indication, study_design, primary_endpoint, sample_size) {
      prompt <- sprintf(
        "Create a clinical study protocol outline for:\n\n" %+%
        "Study Title: %s\n" %+%
        "Indication: %s\n" %+%
        "Study Design: %s\n" %+%
        "Primary Endpoint: %s\n" %+%
        "Sample Size: %s\n\n" %+%
        "Include all essential sections according to ICH-GCP guidelines.",
        study_title, indication, study_design, primary_endpoint, sample_size
      )
      
      return(self$ask(prompt))
    },
    
    #' Generate regulatory submission summary
    #'
    #' @param drug_name Name of the drug/device
    #' @param indication Target indication
    #' @param submission_type Type of submission (IND, NDA, BLA, etc.)
    #' @param key_findings Summary of key clinical findings
    #' @return Regulatory submission summary
    create_regulatory_summary = function(drug_name, indication, submission_type, key_findings) {
      prompt <- sprintf(
        "Create a regulatory submission summary for:\n\n" %+%
        "Drug/Device: %s\n" %+%
        "Indication: %s\n" %+%
        "Submission Type: %s\n" %+%
        "Key Findings: %s\n\n" %+%
        "Follow FDA/EMA regulatory writing guidelines and include appropriate sections.",
        drug_name, indication, submission_type, key_findings
      )
      
      return(self$ask(prompt))
    },
    
    #' Generate medical manuscript abstract
    #'
    #' @param title Manuscript title
    #' @param study_type Type of study
    #' @param results_summary Summary of key results
    #' @param conclusion Main conclusion
    #' @param word_limit Word limit for abstract
    #' @return Structured abstract
    create_abstract = function(title, study_type, results_summary, conclusion, word_limit = 250) {
      prompt <- sprintf(
        "Create a structured medical abstract for:\n\n" %+%
        "Title: %s\n" %+%
        "Study Type: %s\n" %+%
        "Results: %s\n" %+%
        "Conclusion: %s\n" %+%
        "Word Limit: %d words\n\n" %+%
        "Use structured format with Background, Methods, Results, Conclusions sections.",
        title, study_type, results_summary, conclusion, word_limit
      )
      
      return(self$ask(prompt))
    },
    
    #' Validate medical content for accuracy and compliance
    #'
    #' @param content Medical content to validate
    #' @param document_type Type of document being validated
    #' @return Validation report with recommendations
    validate_content = function(content, document_type) {
      prompt <- sprintf(
        "Review and validate the following medical content for:\n" %+%
        "Document Type: %s\n\n" %+%
        "Content:\n%s\n\n" %+%
        "Check for:\n" %+%
        "- Medical accuracy and terminology\n" %+%
        "- Regulatory compliance\n" %+%
        "- Writing clarity and style\n" %+%
        "- Missing citations or references\n" %+%
        "- Adherence to medical writing standards\n\n" %+%
        "Provide specific recommendations for improvement.",
        document_type, content
      )
      
      return(self$ask(prompt))
    }
  ),
  
  private = list(
    # Get default medical writing standards
    get_default_standards = function() {
      list(
        "ICH-GCP" = "International Conference on Harmonisation Good Clinical Practice",
        "FDA_Guidance" = "FDA Guidance for Industry documents",
        "EMA_Guidelines" = "European Medicines Agency Guidelines",
        "CONSORT" = "Consolidated Standards of Reporting Trials",
        "STROBE" = "Strengthening the Reporting of Observational Studies in Epidemiology",
        "AMA_Style" = "American Medical Association Manual of Style",
        "AMWA" = "American Medical Writers Association standards"
      )
    },
    
    # Get document templates
    get_document_templates = function() {
      list(
        "clinical_protocol" = list(
          sections = c("Title Page", "Synopsis", "Table of Contents", "Abbreviations",
                      "Background", "Objectives", "Study Design", "Subject Selection",
                      "Treatment", "Assessments", "Statistics", "Ethics", "References"),
          format = "ICH-GCP compliant"
        ),
        "clinical_study_report" = list(
          sections = c("Title Page", "Synopsis", "Table of Contents", "Abbreviations",
                      "Ethics", "Investigators", "Introduction", "Study Objectives",
                      "Methods", "Results", "Discussion", "Conclusions", "References"),
          format = "ICH-E3 compliant"
        ),
        "regulatory_summary" = list(
          sections = c("Executive Summary", "Product Information", "Clinical Development",
                      "Efficacy Summary", "Safety Summary", "Benefit-Risk Assessment",
                      "Conclusions", "References"),
          format = "CTD Module 2 format"
        ),
        "manuscript" = list(
          sections = c("Title", "Abstract", "Keywords", "Introduction", "Methods",
                      "Results", "Discussion", "Conclusions", "Acknowledgments", "References"),
          format = "ICMJE guidelines"
        )
      )
    },
    
    # Register medical-specific tools
    register_medical_tools = function() {
      # Medical terminology lookup tool
      medical_lookup_tool <- ellmer::tool(
        function(term, context = "general") {
          # Simplified medical term lookup (in practice, would use medical databases)
          medical_terms <- list(
            "efficacy" = "The ability of a drug to produce a desired therapeutic effect",
            "safety" = "The extent to which a drug does not cause harmful effects",
            "adverse_event" = "Any untoward medical occurrence in a patient administered a drug",
            "primary_endpoint" = "The main result measured to determine treatment effect",
            "secondary_endpoint" = "Additional outcomes measured in a clinical trial",
            "placebo" = "An inactive substance used as a control in clinical trials",
            "randomization" = "Process of assigning participants to treatment groups by chance"
          )
          
          term_lower <- tolower(term)
          if (term_lower %in% names(medical_terms)) {
            return(paste("Term:", term, "\nDefinition:", medical_terms[[term_lower]]))
          } else {
            return(paste("Medical term not found in database:", term))
          }
        },
        name = "lookup_medical_term",
        description = "Look up medical terminology and definitions",
        arguments = list(
          term = ellmer::type_string("Medical term to look up"),
          context = ellmer::type_string("Context for the term (e.g., 'clinical_trial', 'regulatory', 'general')")
        )
      )
      
      # Citation formatter tool
      citation_tool <- ellmer::tool(
        function(authors, title, journal, year, volume = NULL, pages = NULL, doi = NULL, style = "AMA") {
          if (style == "AMA") {
            citation <- paste0(authors, ". ", title, ". ", journal, ". ", year)
            if (!is.null(volume)) citation <- paste0(citation, ";", volume)
            if (!is.null(pages)) citation <- paste0(citation, ":", pages)
            if (!is.null(doi)) citation <- paste0(citation, ". doi:", doi)
            citation <- paste0(citation, ".")
          } else {
            citation <- paste("Citation formatted in", style, "style:", 
                            authors, title, journal, year)
          }
          return(citation)
        },
        name = "format_citation",
        description = "Format medical literature citations in various styles",
        arguments = list(
          authors = ellmer::type_string("Author names"),
          title = ellmer::type_string("Article title"),
          journal = ellmer::type_string("Journal name"),
          year = ellmer::type_string("Publication year"),
          volume = ellmer::type_string("Volume number (optional)"),
          pages = ellmer::type_string("Page numbers (optional)"),
          doi = ellmer::type_string("DOI (optional)"),
          style = ellmer::type_string("Citation style (AMA, APA, Vancouver)")
        )
      )
      
      # Regulatory compliance checker
      compliance_tool <- ellmer::tool(
        function(document_type, content_section, regulation = "FDA") {
          compliance_checks <- list(
            "FDA" = list(
              "clinical_protocol" = c("IRB approval statement", "Informed consent process", 
                                    "Safety reporting procedures"),
              "regulatory_summary" = c("Risk-benefit assessment", "Dosing rationale", 
                                     "Patient population definition")
            ),
            "EMA" = list(
              "clinical_protocol" = c("Ethics committee approval", "GDPR compliance", 
                                    "Pharmacovigilance plan"),
              "regulatory_summary" = c("Quality documentation", "Risk management plan", 
                                     "Pediatric investigation plan")
            )
          )
          
          if (regulation %in% names(compliance_checks) && 
              document_type %in% names(compliance_checks[[regulation]])) {
            requirements <- compliance_checks[[regulation]][[document_type]]
            return(paste("Compliance requirements for", document_type, "under", regulation, ":\n",
                        paste("-", requirements, collapse = "\n")))
          } else {
            return("Compliance requirements not found for specified document type and regulation")
          }
        },
        name = "check_regulatory_compliance",
        description = "Check regulatory compliance requirements for medical documents",
        arguments = list(
          document_type = ellmer::type_string("Type of document (clinical_protocol, regulatory_summary, etc.)"),
          content_section = ellmer::type_string("Section of document to check"),
          regulation = ellmer::type_string("Regulatory authority (FDA, EMA, ICH)")
        )
      )
      
      # Medical statistics calculator
      stats_tool <- ellmer::tool(
        function(calculation_type, data_values, confidence_level = 0.95) {
          tryCatch({
            values <- as.numeric(unlist(strsplit(data_values, ",")))
            
            result <- switch(calculation_type,
              "mean_ci" = {
                mean_val <- mean(values, na.rm = TRUE)
                se <- sd(values, na.rm = TRUE) / sqrt(length(values))
                margin <- qt((1 + confidence_level) / 2, df = length(values) - 1) * se
                paste0("Mean: ", round(mean_val, 3), 
                      " (95% CI: ", round(mean_val - margin, 3), 
                      " to ", round(mean_val + margin, 3), ")")
              },
              "descriptive" = {
                paste0("N: ", length(values),
                      ", Mean: ", round(mean(values, na.rm = TRUE), 3),
                      ", SD: ", round(sd(values, na.rm = TRUE), 3),
                      ", Median: ", round(median(values, na.rm = TRUE), 3),
                      ", Range: ", round(min(values, na.rm = TRUE), 3), 
                      " to ", round(max(values, na.rm = TRUE), 3))
              },
              "Unknown calculation type"
            )
            return(result)
          }, error = function(e) {
            return(paste("Error in calculation:", e$message))
          })
        },
        name = "calculate_medical_stats",
        description = "Calculate medical statistics for clinical data",
        arguments = list(
          calculation_type = ellmer::type_string("Type of calculation (mean_ci, descriptive, t_test)"),
          data_values = ellmer::type_string("Comma-separated numeric data values"),
          confidence_level = ellmer::type_number("Confidence level (default 0.95)")
        )
      )
      
      # Register all medical tools
      self$register_tool(medical_lookup_tool)
      self$register_tool(citation_tool)
      self$register_tool(compliance_tool)
      self$register_tool(stats_tool)
    }
  )
)

# Helper operator for string concatenation
`%+%` <- function(a, b) paste0(a, b)

# Null-default operator
`%||%` <- function(a, b) if (is.null(a)) b else a