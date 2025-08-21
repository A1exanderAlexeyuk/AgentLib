#' Medical Writer Agent
#'
#' @description
#' Specialized agent for medical writing tasks including clinical protocols,
#' regulatory documents, and scientific abstracts.
#'
#' @import R6
#' @export
MedicalWriterAgent <- R6::R6Class(
  "MedicalWriterAgent",
  inherit = BaseAgent,
  
  public = list(
    #' @description
    #' Initialize Medical Writer Agent
    #' @param provider LLM provider (supports OpenAI, Anthropic, Gemini, Bedrock)
    #' @param temperature Temperature for generation
    #' @param ... Additional arguments passed to BaseAgent
    initialize = function(provider = "anthropic/claude-3-opus-20240229",
                         temperature = 0.3,
                         ...) {
      
      # Medical writer system prompt
      system_prompt <- "You are an expert medical writer with extensive experience in:
      - Clinical trial protocols and amendments
      - Regulatory submissions (FDA, EMA)
      - Scientific manuscripts and abstracts
      - Patient information materials
      - Medical education content
      
      You follow ICH-GCP guidelines, regulatory requirements, and medical writing best practices.
      You use clear, precise medical terminology and ensure accuracy in all content.
      You are familiar with CONSORT, STROBE, and other reporting guidelines."
      
      # Initialize parent
      super$initialize(
        provider = provider,
        system_prompt = system_prompt,
        temperature = temperature,
        ...
      )
      
      # Register medical writing specific tools
      private$register_medical_tools()
    },
    
    #' @description
    #' Create a clinical trial protocol outline
    #' @param study_title Title of the study
    #' @param indication Medical indication
    #' @param study_design Study design (e.g., "Phase 3 RCT")
    #' @param primary_endpoint Primary endpoint
    #' @param sample_size Target sample size
    #' @return Protocol outline
    create_protocol = function(study_title, indication, study_design, 
                             primary_endpoint, sample_size) {
      
      prompt <- sprintf(
        "Create a detailed clinical trial protocol outline for:
        Title: %s
        Indication: %s
        Study Design: %s
        Primary Endpoint: %s
        Sample Size: %s
        
        Include all standard protocol sections following ICH-GCP guidelines.",
        study_title, indication, study_design, primary_endpoint, sample_size
      )
      
      self$ask(prompt, tools_enabled = FALSE)
    },
    
    #' @description
    #' Generate regulatory submission summary
    #' @param drug_name Name of the drug/device
    #' @param indication Indication
    #' @param submission_type Type of submission (e.g., "IND", "NDA", "510k")
    #' @param key_data Key efficacy and safety data
    #' @return Regulatory summary
    regulatory_summary = function(drug_name, indication, submission_type, key_data) {
      
      prompt <- sprintf(
        "Create an executive summary for a %s submission:
        Drug/Device: %s
        Indication: %s
        Key Data: %s
        
        Include benefit-risk assessment and regulatory strategy.",
        submission_type, drug_name, indication, key_data
      )
      
      self$ask(prompt, tools_enabled = FALSE)
    },
    
    #' @description
    #' Write a scientific abstract
    #' @param title Study title
    #' @param background Background information
    #' @param methods Study methods
    #' @param results Key results
    #' @param conclusions Study conclusions
    #' @param word_limit Word limit for abstract
    #' @return Formatted abstract
    write_abstract = function(title, background, methods, results, 
                            conclusions, word_limit = 250) {
      
      prompt <- sprintf(
        "Write a structured scientific abstract (%d words) with:
        Title: %s
        Background: %s
        Methods: %s
        Results: %s
        Conclusions: %s
        
        Use standard IMRAD format.",
        word_limit, title, background, methods, results, conclusions
      )
      
      self$ask(prompt, tools_enabled = FALSE)
    },
    
    #' @description
    #' Create patient information leaflet
    #' @param drug_name Drug name
    #' @param indication What the drug is for
    #' @param key_points Important information points
    #' @param reading_level Target reading level (default: 8th grade)
    #' @return Patient-friendly information
    patient_information = function(drug_name, indication, key_points, 
                                 reading_level = "8th grade") {
      
      prompt <- sprintf(
        "Create a patient information leaflet for %s:
        Used for: %s
        Key information: %s
        
        Write at %s reading level. Use clear, simple language.
        Include sections: What is this medicine, How to take it, 
        Side effects, Warnings.",
        drug_name, indication, key_points, reading_level
      )
      
      self$ask(prompt, tools_enabled = FALSE)
    },
    
    #' @description
    #' Review document for medical accuracy
    #' @param document Document text to review
    #' @param document_type Type of document
    #' @return Review comments and suggestions
    review_document = function(document, document_type = "general") {
      
      prompt <- sprintf(
        "Review this %s medical document for:
        - Medical accuracy
        - Regulatory compliance
        - Clarity and readability
        - Completeness
        
        Document:
        %s
        
        Provide specific feedback and suggestions.",
        document_type, document
      )
      
      self$ask(prompt, tools_enabled = FALSE)
    },
    
    #' @description
    #' Generate adverse event narrative
    #' @param patient_id Patient identifier
    #' @param event_description Description of adverse event
    #' @param timeline Timeline of events
    #' @param outcome Event outcome
    #' @param causality Causality assessment
    #' @return AE narrative
    ae_narrative = function(patient_id, event_description, timeline, 
                          outcome, causality) {
      
      prompt <- sprintf(
        "Write a regulatory-compliant adverse event narrative:
        Patient: %s
        Event: %s
        Timeline: %s
        Outcome: %s
        Causality: %s
        
        Follow ICH guidelines for safety reporting.",
        patient_id, event_description, timeline, outcome, causality
      )
      
      self$ask(prompt, tools_enabled = FALSE)
    }
  ),
  
  private = list(
    #' @description
    #' Register medical writing specific tools
    register_medical_tools = function() {
      
      # Medical abbreviation expander
      abbrev_tool <- ellmer::tool(
        function(abbreviation) {
          common_abbrevs <- list(
            "AE" = "Adverse Event",
            "SAE" = "Serious Adverse Event", 
            "ICF" = "Informed Consent Form",
            "CRF" = "Case Report Form",
            "GCP" = "Good Clinical Practice",
            "FDA" = "Food and Drug Administration",
            "EMA" = "European Medicines Agency",
            "ICH" = "International Council for Harmonisation",
            "RCT" = "Randomized Controlled Trial",
            "ITT" = "Intention to Treat",
            "PP" = "Per Protocol",
            "CI" = "Confidence Interval",
            "HR" = "Hazard Ratio",
            "OR" = "Odds Ratio",
            "NNT" = "Number Needed to Treat"
          )
          
          result <- common_abbrevs[[toupper(abbreviation)]]
          if (is.null(result)) {
            return(paste("Unknown abbreviation:", abbreviation))
          }
          return(result)
        },
        name = "medical_abbreviation",
        description = "Expand common medical abbreviations",
        arguments = list(
          abbreviation = ellmer::type_string("Medical abbreviation to expand")
        )
      )
      
      # ICD-10 code lookup (simplified)
      icd10_tool <- ellmer::tool(
        function(condition) {
          # Simplified lookup - in practice would connect to database
          common_codes <- list(
            "hypertension" = "I10 - Essential (primary) hypertension",
            "diabetes" = "E11 - Type 2 diabetes mellitus",
            "pneumonia" = "J18.9 - Pneumonia, unspecified organism",
            "depression" = "F32.9 - Major depressive disorder, single episode",
            "asthma" = "J45.9 - Asthma, unspecified"
          )
          
          result <- common_codes[[tolower(condition)]]
          if (is.null(result)) {
            return(paste("ICD-10 code not found for:", condition))
          }
          return(result)
        },
        name = "icd10_lookup",
        description = "Look up ICD-10 codes for medical conditions",
        arguments = list(
          condition = ellmer::type_string("Medical condition to look up")
        )
      )
      
      # Medical statistics calculator
      stats_tool <- ellmer::tool(
        function(calculation_type, values) {
          values <- as.numeric(strsplit(values, ",")[[1]])
          
          result <- switch(
            calculation_type,
            "mean" = mean(values, na.rm = TRUE),
            "median" = median(values, na.rm = TRUE),
            "sd" = sd(values, na.rm = TRUE),
            "ci95" = {
              m <- mean(values, na.rm = TRUE)
              se <- sd(values, na.rm = TRUE) / sqrt(length(values))
              ci <- m + c(-1.96, 1.96) * se
              sprintf("95%% CI: [%.2f, %.2f]", ci[1], ci[2])
            },
            "Invalid calculation type"
          )
          
          return(as.character(result))
        },
        name = "medical_stats",
        description = "Calculate medical statistics",
        arguments = list(
          calculation_type = ellmer::type_enum(
            "Type of calculation",
            values = c("mean", "median", "sd", "ci95")
          ),
          values = ellmer::type_string("Comma-separated numeric values")
        )
      )
      
      # Reference formatter
      reference_tool <- ellmer::tool(
        function(authors, year, title, journal, volume, pages) {
          # Vancouver style formatting
          sprintf("%s. %s. %s. %s;%s:%s.",
                 authors, title, journal, year, volume, pages)
        },
        name = "format_reference",
        description = "Format medical references in Vancouver style",
        arguments = list(
          authors = ellmer::type_string("Authors (Last FM, Last FM)"),
          year = ellmer::type_string("Publication year"),
          title = ellmer::type_string("Article title"),
          journal = ellmer::type_string("Journal name abbreviated"),
          volume = ellmer::type_string("Volume number"),
          pages = ellmer::type_string("Page numbers (e.g., 123-45)")
        )
      )
      
      # Register all medical tools
      self$register_tool(abbrev_tool)
      self$register_tool(icd10_tool)
      self$register_tool(stats_tool)
      self$register_tool(reference_tool)
    }
  )
)