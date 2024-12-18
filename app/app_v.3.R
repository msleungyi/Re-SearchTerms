##############################
## RE-SEARCHTERMS SHINY APP ##
##############################

# Load libraries
library(shiny)
library(DT)
library(visNetwork)
library(plotly)
library(dplyr)
library(bslib)
library(tidytext)
library(widyr)

# Load datasets
df_bydefinition <- read.csv("df_bydefinition.csv")
pairwise_cosine_similarity <- read.csv("pairwise_cosine_similarity.csv")
df_byterm <- read.csv("df_byterm.csv") 
df_umap <- read.csv("umap_df.csv")

# Preprocess datasets
df_bydefinition <- df_bydefinition %>%
  left_join(df_byterm %>% select(term, cluster_name), by = "term") %>%  # Add cluster_name
  mutate(
    source = case_when(
      source == "forrt" ~ "FORRT",
      source == "wiktionary" ~ "Wiktionary",
      TRUE ~ source
    )
  )

#################################
## Define UI for the Shiny app ##
#################################

ui <- fluidPage(
  
  # Add logo above the navigation bar
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; padding: 10px; background-color: #f9f9f9; border-bottom: 1px solid #ddd;",
    
    # Empty space for alignment
    tags$div(style = "flex: 1;"),
    
    # Add logos on the right
    tags$div(
      style = "flex: 0 0 auto; display: flex; align-items: center;",
      tags$img(
        src = "metarep.jpg",
        height = "50px",
        alt = "MetaRep Logo",
        style = "margin-left: 10px;"  # Spacing between logos
      ),
      tags$img(
        src = "dfg.jpg",
        height = "50px",
        alt = "DFG Logo",
        style = "margin-left: 10px;"  # Adjust spacing if needed
      )
    )
  ),
  
  navbarPage(
    
    title = "Re-SearchTerms",
    theme = bs_theme(
      bootswatch = "minty",
      primary = "#28a745",
      font_scale = 1.1
    ),
    
    # Custom CSS to adjust the font size
    tags$head(
      tags$style(HTML("
      h1 { font-size: 32px !important; }  /* Reduce the font size of the main title */
      h2 { font-size: 24px !important; }  /* Reduce the font size of subtitles */
    "))
    ),
    
    # Custom CSS to reduce row height
    tags$head(
      tags$style(HTML("
    table.dataTable tr {
      height: auto !important; /* Allow automatic adjustment */
    }
    table.dataTable td, table.dataTable th {
      padding: 2px !important; /* Reduce padding for both header and cells */
      vertical-align: middle !important; /* Ensure content is vertically aligned */
    }
    table.dataTable {
      font-size: 12px !important; /* Optional: Reduce font size to make rows compact */
    }
  "))
    ),
    
    # Home Tab
    tabPanel("Home",
             div(style = "padding: 20px;",
                 h2("Welcome to Re-SearchTerms: A Shiny App for Navigating Open Scholarship Terminology!"),
                 p("This Shiny app helps you explore open-scholarship and research terms, definitions, and their relationships."),
                 p("Key Features:"),
                 tags$ul(
                   tags$li("Dataset: Explore and download the dataset."),
                   tags$li("Term-Level Analysis: Analyse relationships between terms."),
                   tags$li("Definition-Level Analysis: Investigate relationships between definitions."),
                   tags$li("Word-Level Analysis: Explore the wording of a term's definitions.")
                 ),
                 p("Use the navigation bar on the top to explore.")
             )
    ),
    
    # Dataset Tab
    tabPanel("Dataset",
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Dataset"),
                 p("Use the filters below to explore the dataset:"),
                 tags$ul(
                   tags$li("Filter by cluster, term, or source."),
                   tags$li("Reset filters to view the entire dataset."),
                   tags$li("Download the filtered dataset for offline use.")
                 ),
                 hr(),  # Add a horizontal line for visual separation
                 selectInput("filterCluster", "Cluster Name:", choices = NULL, multiple = TRUE),
                 selectInput("filterTerm", "Term:", choices = NULL, multiple = TRUE),
                 selectInput("filterSource", "Source:", choices = NULL, multiple = TRUE),
                 actionButton("resetFilters", "Reset Filters", icon = icon("redo")),
                 br(), br(),
                 downloadButton("downloadDatasetFiltered", "Download Filtered Dataset"),
                 width = 3
               ),
               mainPanel(
                 h4("Dataset Table"),
                 div(
                   DTOutput("dataTable", width = "100%"),
                   style = "height: 400px; overflow-y: auto;"  # Enable vertical scrolling
                 ),
                 tags$style(HTML("
               table.dataTable {
                 font-size: 12px !important; /* Reduce font size */
               }
               table.dataTable td, table.dataTable th {
                 padding: 4px !important; /* Reduce cell padding */
               }
             ")),
                 width = 9  # Increased main panel width
               )
             )
    ),
    
    # Word-Level Analysis Tab
    tabPanel(
      "Word-Level Analysis",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Word-Level Analysis"),
          p("Explore the word-level patterns across definitions of a term."),
          tags$ul(
            tags$li("Select a term from the dropdown."),
            tags$li("Adjust the minimum co-occurrence frequency slider to filter the word co-occurrence network."),
            tags$li("Visualisations:"),
            tags$ul(
              tags$li("Terms with only one definition: Word frequency bar chart."),
              tags$li("Terms with multiple definitions: Word co-occurrence network graph.")
            ),
          ),
          br(),
          selectInput("selectedTermWord", "Select Term:", choices = NULL),  # Dynamically populated
          sliderInput(
            "frequencyThreshold",
            "Minimum Co-Occurrence Frequency:",
            min = 1,
            max = 20,
            value = 3  # Default value
          ),
          helpText("If no graph is displayed, it could be due to a high minimum frequency threshold. Try lowering the slider value to include more connections. For large word networks, increasing the slider value can simplify the graph but may take longer to render.")
        ),
        mainPanel(
          width = 9,
          h4("Word Co-Occurrence Visualisation"),
          uiOutput("wordCooccurrencePlot", height = "600px")
        )
      )
    ),
    
    # Definition-Level Analysis Tab
    tabPanel("Definition-Level Analysis",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Definition-Level Analysis"),
                 
                 # Instructions
                 div(
                   tags$ul(
                     tags$li("Use the dropdown menu to select a specific term. The graph will dynamically update to show the relationships."),
                     tags$li("The network graph shows nodes (definitions) and edges (relationships based on cosine similarity). The thicker the edges, the higher the cosine similarity."),
                     tags$li("Click on a node in the graph to view the full definition below."),
                     tags$li("Hover over nodes to see additional information about the definition or source.")
                   )
                 ),
                 
                 # Term selection dropdown
                 selectInput("selectedDefinitionTerm", "Select Term:", choices = unique(df_bydefinition$term))
               ),
               
               mainPanel(
                 width = 9,
                 h4("Definition Relationship Network"),
                 visNetworkOutput("definitionNetwork", height = "500px"),
                 h5("Selected Definition:"),
                 textOutput("nodeDefinition")
               )
             )
    ),
    
    # Term-Level Analysis Tab
    tabPanel("Term-Level Analysis",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar", # Add a class to apply the custom styles
                 h4("Term-Level Analysis"),
                 selectInput("termAnalysisMode", "Analysis Mode:",
                             choices = c("Word Co-Occurrence Network", 
                                         "Clustering (Sentence Embeddings)", 
                                         "Types & Tokens Analysis")),
                 conditionalPanel(
                   condition = "input.termAnalysisMode == 'Word Co-Occurrence Network'",
                   tags$ul(
                     tags$li("Adjust the 'Minimum Co-Occurrence' slider to filter term connections."),
                     tags$li("Select a term from the dropdown to highlight it on the network.")
                   ),
                   sliderInput("cooccurrenceThreshold", "Minimum Co-Occurrence:", min = 1, max = 50, value = 25),
                   selectInput("selectedTerm", "Highlight Term:", choices = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.termAnalysisMode == 'Clustering (Sentence Embeddings)'",
                   tags$ul(
                     tags$li("Select a term to highlight its cluster and nearby terms.")
                   ),
                   selectizeInput("highlightedTerm", "Highlight Term:", choices = list("(Select a term)" = ""), selected = NULL),
                   helpText("Note: The clustering graph visualises terms in a reduced 2D space, which is optimised for layout and ease of understanding. 
   The closest terms table is based on cosine similarity in the original high-dimensional embedding space, providing a more accurate measure of similarity.")
                 ), 
                 hr(),
                 p("After interacting with the network or clustering visualisation, scroll down to view the table:"),
                 conditionalPanel(
                   condition = "input.termAnalysisMode == 'Types & Tokens Analysis'",
                   tags$ul(
                     tags$li("Use sliders to filter data by types, tokens, and their ratios.")
                   ),
                   sliderInput("typesFilter", "Filter by Types:", min = 1, max = 100, value = c(1, 100)),
                   sliderInput("tokensFilter", "Filter by Tokens:", min = 1, max = 1000, value = c(1, 1000)),
                   sliderInput("ttrFilter", "Filter by Type-to-Token Ratio:", 
                               min = 0, max = 1, step = 0.01, value = c(0, 1)),
                   actionButton("generateTable", "Generate Table", icon = icon("table"))
                 ),
                 tags$ul(
                   tags$li("The table dynamically updates based on your selections and filters."),
                   tags$li("It displays terms, clusters, and other relevant information.")
                 ),
                 width = 3
               ),
               mainPanel(
                 class = "main-panel",
                 conditionalPanel(
                   condition = "input.termAnalysisMode == 'Word Co-Occurrence Network'",
                   h4("Word Co-Occurrence Network"),
                   visNetworkOutput("termNetwork", height = "500px"),
                   br(),
                   uiOutput("tableTitle"),  # Dynamic table title
                   DTOutput("cooccurrenceTable", height = "400px")  # Table to display co-occurring terms
                 ),
                 conditionalPanel(
                   condition = "input.termAnalysisMode == 'Clustering (Sentence Embeddings)'",
                   h4("Clustering Visualisation"),
                   plotlyOutput("clusteringPlot", height = "400px"),
                   uiOutput("closestTermsTableTitle", height = "400px"),  # Dynamic table title
                   DTOutput("closestTermsTable", height = "400px")
                 ),
                 conditionalPanel(
                   condition = "input.termAnalysisMode == 'Types & Tokens Analysis'",
                   h4("Types & Tokens Analysis Visualisation"),
                   p("This 3D scatter plot visualises the relationship between types, tokens, and their ratios."),
                   
                   # 3D Scatter Plot
                   plotlyOutput("typesTokensPlot", height = "400px"),
                   hr(),  # Add a horizontal line
                   
                   # Data Table Section
                   h4("Filtered data table"),
                   p("The table below shows the filtered data based on your inputs."),
                   DTOutput("filteredTable")
                 ),
                 width = 9
               )
             )
    ),
    
    # About Tab
    tabPanel("About",
             div(style = "padding: 20px;",
                 h2("About Re-SearchTerms"),
                 p("Re-SearchTerms is a Shiny app designed to explore the variability in open-scholarship and research term definitions."),
                 p("It is supported by the priority programme, A Meta-Scientific Programme to Analyse and Optimise Replicability in the Behavioural, Social, and Cognitive Sciences ", 
                   a("(META-REP)", href = "https://www.meta-rep.uni-muenchen.de/index.html", target = "_blank"),
                   ", funded by the German Research Foundation (DFG)."),
                 br(),
                 br(),
                 h2("Team"),
                 p(a("Anna Yi Leung", href = "https://msleungyi.github.io/mywebsite/", target = "blank"), "(Ludwig-Maximilians-University of Munich, Germany)"),
                 p(a("Dr. Daniel Kristanto", href = "https://uol.de/psychologie/statistik/daniel-kristanto", target = "blank"), "(Carl von Ossietzky Universität Oldenburg, Germany)"),
                 p(a("Dr. habil. Xenia Schmalz", href = "https://www.xenia-schmalz.net/", target = "blank"), "(Ludwig-Maximilians-University of Munich, Germany)")
             )
    )
  )
)

#########################
## DEFINE SERVER LOGIC ##
#########################

server <- function(input, output, session) {
  
  # Define a global color palette for clusters
  cluster_colors <- c(
    "Data sharing, reproducibility, and computational tools" = "red",
    "Replication and research challenges" = "blue",
    "Research design, validity, and theoretical frameworks" = "green",
    "Research credit" = "orange",
    "Statistical analysis and hypothesis testing" = "purple",
    "Open science and collaborative practices" = "cyan"
  )
    
    #################
    ## DATASET TAB ##
    #################
    
    # Reactive Dataset for Filtering
    filtered_data <- reactive({
      data <- df_bydefinition %>%
        select(-def_clean, -source_type) %>%
        rename(
          "Definition ID" = def_ID,
          "ID" = ID,
          "Term" = term,
          "Source" = source,
          "Cluster Name" = cluster_name,
          "Retrieval Date" = retrieval_date,
          "Hyperlink" = hyperlink,
          "Definition" = definition
        )
      
      # Apply Cluster Name Filter
      if (!is.null(input$filterCluster)) {
        data <- data %>% filter(`Cluster Name` %in% input$filterCluster)
      }
      
      # Apply Term Filter
      if (!is.null(input$filterTerm)) {
        data <- data %>% filter(`Term` %in% input$filterTerm)
      }
      
      # Apply Source Filter
      if (!is.null(input$filterSource)) {
        data <- data %>% filter(`Source` %in% input$filterSource)
      }
      
      return(data)
    })
    
    # Populate Filter Choices Dynamically
    observe({
      updateSelectInput(
        session, "filterCluster", 
        choices = unique(df_bydefinition$cluster_name), 
        selected = NULL
      )
    })
    
    observe({
      filtered_cluster <- df_bydefinition
      if (!is.null(input$filterCluster)) {
        filtered_cluster <- filtered_cluster %>%
          filter(cluster_name %in% input$filterCluster)
      }
      updateSelectInput(
        session, "filterTerm", 
        choices = unique(filtered_cluster$term), 
        selected = NULL
      )
    })
    
    observe({
      filtered_source <- df_bydefinition
      if (!is.null(input$filterCluster)) {
        filtered_source <- filtered_source %>%
          filter(cluster_name %in% input$filterCluster)
      }
      if (!is.null(input$filterTerm)) {
        filtered_source <- filtered_source %>%
          filter(term %in% input$filterTerm)
      }
      updateSelectInput(
        session, "filterSource", 
        choices = unique(filtered_source$source), 
        selected = NULL
      )
    })
    
    # Reset Filters when Reset Button is clicked
    observeEvent(input$resetFilters, {
      updateSelectInput(session, "filterCluster", selected = NULL)
      updateSelectInput(session, "filterTerm", selected = NULL)
      updateSelectInput(session, "filterSource", selected = NULL)
      
      # Refresh dropdown choices
      updateSelectInput(session, "filterCluster", choices = unique(df_bydefinition$cluster_name), selected = NULL)
      updateSelectInput(session, "filterTerm", choices = unique(df_bydefinition$term), selected = NULL)
      updateSelectInput(session, "filterSource", choices = unique(df_bydefinition$source), selected = NULL)
    })
    
    # Render Filtered Data Table
    output$dataTable <- renderDT({
      datatable(
        filtered_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(list(targets = "_all", className = "dt-center"))
        ),
        rownames = FALSE
      )
    })
    
    # Allow Download of Filtered Data
    output$downloadDatasetFiltered <- downloadHandler(
      filename = function() {
        paste0("filtered_dataset_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
    
    ################################
    ## WORD CO-OCCURRENCE DIAGRAM ##
    ################################
    
    # Reactive term pairs for full co-occurrence data
    term_pairs_full <- reactive({
      term_words <- df_bydefinition %>%
        group_by(term) %>%
        summarise(words = list(unique(unlist(strsplit(def_clean, "\\s+"))))) %>%
        ungroup()
      
      term_pairs <- expand.grid(term_words$term, term_words$term, stringsAsFactors = FALSE)
      colnames(term_pairs) <- c("from", "to")
      
      # Remove self-loops
      term_pairs <- term_pairs[term_pairs$from != term_pairs$to,]
      
      # Sort terms in each pair to ensure uniqueness
      term_pairs <- term_pairs %>%
        rowwise() %>%
        mutate(pair_id = paste(sort(c(from, to)), collapse = "_")) %>%
        distinct(pair_id, .keep_all = TRUE) %>%  # Keep unique pairs
        select(-pair_id)  # Drop the helper column
      
      # Compute edge weight based on shared words
      term_pairs$weight <- mapply(function(x, y) {
        length(intersect(
          term_words$words[[which(term_words$term == x)]],
          term_words$words[[which(term_words$term == y)]]
        ))
      }, term_pairs$from, term_pairs$to)
      
      term_pairs
    })
    
    # Reactive term pairs filtered for visualization
    term_pairs_filtered <- reactive({
      term_pairs_full() %>%
        filter(weight >= input$cooccurrenceThreshold)
    })
    
    getClosestTerms <- function(selected_term, umap_data, n = 20) {
      # Normalize vectors to calculate cosine similarity
      normalize <- function(x) sqrt(sum(x^2))
      umap_data <- umap_data %>%
        mutate(norm = sqrt(UMAP1^2 + UMAP2^2))
      
      # Get the vector for the selected term
      selected_row <- umap_data %>%
        filter(term == selected_term) %>%
        select(UMAP1, UMAP2, norm)
      
      # Calculate cosine similarity
      umap_data <- umap_data %>%
        mutate(
          dot_product = (UMAP1 * selected_row$UMAP1 + UMAP2 * selected_row$UMAP2),
          cosine_similarity = dot_product / (norm * selected_row$norm)
        )
      
      # Return the closest terms sorted by cosine similarity
      closest_terms <- umap_data %>%
        filter(term != selected_term) %>%
        arrange(desc(cosine_similarity)) %>%
        head(n)
      
      return(closest_terms)
    }
    
    # Populate the term selection dropdown with hierarchical structure
    observe({
      cluster_terms <- df_byterm %>%
        group_by(cluster_name) %>%
        summarise(terms = list(term)) %>%
        ungroup()
      
      # Create a list of choices with hierarchical structure
      choices_list <- list()
      choices_list[["(Select a term)"]] <- ""  # Add placeholder for default selection
      
      for (i in seq_len(nrow(cluster_terms))) {
        cluster_name <- cluster_terms$cluster_name[i]
        terms <- cluster_terms$terms[[i]]
        # Group terms under each cluster
        choices_list[[cluster_name]] <- terms
      }
      
      # Update selectInput with hierarchical choices
      updateSelectInput(
        session, "selectedTerm",
        choices = choices_list,
        selected = NULL  # Make sure no term is selected by default
      )
      
      # Update the dropdown menu with hierarchical choices
      updateSelectInput(
        session, "selectedDefinitionTerm",
        choices = choices_list,
        selected = NULL  # No term selected by default
      )
    })
    
    # Render the term network
    output$termNetwork <- renderVisNetwork({
      edges <- term_pairs_filtered()
      
      nodes <- df_byterm %>%
        select(term, cluster_name) %>%
        distinct() %>%
        rename(id = term) %>%
        mutate(
          label = id,
          group = cluster_name,
          color = cluster_colors[cluster_name],
          size = 15
        )
      
      visNetwork(nodes, edges, width = "100%", height = "800px") %>%
        visNodes(
          shape = "dot",
          font = list(size = 18),
          color = list(highlight = "orange")
        ) %>%
        visEdges(
          smooth = FALSE,
          color = list(highlight = "orange")
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
        )
    })
    
    # Highlight selected term and display co-occurring terms
    observeEvent(input$selectedTerm, {
      req(input$selectedTerm)  # Ensure a term is selected
      
      # Filter co-occurrence data for the selected term
      selected_edges <- term_pairs_full() %>%
        filter(from == input$selectedTerm | to == input$selectedTerm) %>%
        mutate(
          connected_term = ifelse(from == input$selectedTerm, to, from)
        ) %>%
        filter(!is.na(connected_term) & weight > 0) %>%
        arrange(desc(weight))
      
      # Add cluster information to the data
      cooccurrence_data <- selected_edges %>%
        left_join(df_byterm, by = c("connected_term" = "term")) %>%
        select(
          Term = connected_term,
          `Cluster Name` = cluster_name,
          `No. of Co-Occurring Words` = weight
        )
      
      # Highlight and zoom in on the selected node
      visNetworkProxy("termNetwork") %>%
        visSelectNodes(id = input$selectedTerm) %>%
        visFocus(id = input$selectedTerm, scale = 1.5)  # Zoom in on the selected node
      
      # Render the table dynamically
      output$cooccurrenceTable <- renderDT({
        req(nrow(cooccurrence_data) > 0)  # Render only if data exists
        datatable(
          cooccurrence_data,
          options = list(
            dom = 't',              # Display only the table (no search bar or pagination)
            scrollX = TRUE,         # Allow horizontal scrolling
            scrollY = "400px",      # Set vertical scrolling with fixed height
            paging = FALSE,         # Disable pagination
            columnDefs = list(list(targets = "_all", className = "dt-center"))
          ),
          rownames = FALSE
        )
      })
    })
    
    # Dynamic title for the table
    output$tableTitle <- renderUI({
      req(input$selectedTerm)  # Ensure a term is selected
      h3(paste("Terms with co-occurring words with", input$selectedTerm, " (frequency in descending order):"))
    })
    
    ######################
    ## CLUSTERING GRAPH ##
    ######################
    
    # Dynamically update the dropdown menu based on terms in df_umap
    observe({
      req(df_umap)  # Ensure df_umap is available
      
      # Create a nested list with clusters as group headers
      dropdown_choices <- split(df_umap$term, df_umap$cluster_name)
      dropdown_choices <- c("(Select a term)" = "", dropdown_choices) # Add a placeholder as the first option
      
      # Populate dropdown with unique terms
      updateSelectizeInput(
        session,
        inputId = "highlightedTerm",
        choices = dropdown_choices,
        selected = "" # No default selection
      )
    })
    
    # Reactive data with highlighting
    filtered_umap <- reactive({
      req(input$highlightedTerm)
      data <- df_umap %>%
        mutate(is_selected = term == input$highlightedTerm)
      data
    })
    
    # Render clustering plot
    output$clusteringPlot <- renderPlotly({
      req(filtered_umap())
      
      data <- filtered_umap()
      selected_data <- data %>% filter(is_selected)
      non_selected_data <- data %>% filter(!is_selected)
      
      plot_ly() %>%
        # Layer for non-selected points (Note: This is to ensure the selected terms are correctly rendered.)
        add_trace(
          data = non_selected_data,
          x = ~UMAP1,
          y = ~UMAP2,
          color = ~cluster_name,
          colors = 'Set1',
          text = ~paste("Term: ", term, "<br>Cluster: ", cluster_name),
          hoverinfo = 'text',
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = 10,
            opacity = 0.7,
            line = list(width = 0)
          )
        ) %>%
        # Layer for selected point
        add_trace(
          data = selected_data,
          x = ~UMAP1,
          y = ~UMAP2,
          color = ~cluster_name,
          colors = 'Set1',
          text = ~paste("Term: ", term, "<br>Cluster: ", cluster_name),
          hoverinfo = 'text',
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = 15,
            opacity = 1,
            line = list(width = 3, color = 'black')
          )
        ) %>%
        layout(
          title = "Clustering Graph of Terms (Dimensionality Reduced Using UMAP)",
          xaxis = list(title = "UMAP Dimension 1"),
          yaxis = list(title = "UMAP Dimension 2"),
          legend = list(title = list(text = "Cluster"))
        )
    })
    
    # Function to calculate closest terms
    getClosestTerms <- function(highlightedTerm, data) {
      req(highlightedTerm)  # Ensure a term is provided
      req(data)  # Ensure data is available
      
      # Simulate closest terms
      data <- data %>%
        mutate(cosine_similarity = runif(n())) %>%  # Random similarity
        filter(term != highlightedTerm) %>%  # Exclude the highlighted term itself
        arrange(desc(cosine_similarity)) %>%  # Sort by similarity
        
        return(data)
    }
    
    # Render the closest terms table
    output$closestTermsTable <- renderDT({
      req(input$highlightedTerm)  # Ensure a term is selected
      
      # Calculate the closest terms
      closest_terms <- getClosestTerms(input$highlightedTerm, df_umap)
      
      # Render the datatable
      datatable(
        closest_terms %>%
          select(term, cluster_name, cosine_similarity),
        colnames = c("Term", "Cluster", "Cosine Similarity"),
        options = list(
          dom = 't',  # Only show the table (no search bar or pagination)
          scrollX = TRUE,  # Enable horizontal scrolling
          scrollY = "400px", # Enable vertical scrolling with a fixed height
          paging = FALSE  # Disable pagination
        ),
        rownames = FALSE  # Hide row numbers
      )
    })
    
    # Add a dynamic table title
    output$closestTermsTableTitle <- renderUI({
      req(input$highlightedTerm)  # Ensure a term is selected
      term <- input$highlightedTerm
      
      # Return a dynamic title
      if (term == "") {
        h4("No term selected")  # Default title when no term is selected
      } else {
        h4(paste("Closest terms to:", term, "(ordered by cosine similarity)"))
      }
    })
    
    ####################
    ## TYPES & TOKENS ##
    ####################
    
    # Reactive filtering based on user inputs
    filtered3DData <- reactive({
      df_byterm %>%
        filter(
          types >= input$typesFilter[1], types <= input$typesFilter[2],
          tokens >= input$tokensFilter[1], tokens <= input$tokensFilter[2],
          type_to_token_ratio >= input$ttrFilter[1], type_to_token_ratio <= input$ttrFilter[2]
        )
    })
    
    # Render the 3D scatter plot
    output$typesTokensPlot <- renderPlotly({
      req(filtered3DData())  # Ensure filtered data is available
      data <- filtered3DData()
      
      plot_ly(
        data = data,
        x = ~tokens,
        y = ~types,
        z = ~type_to_token_ratio,
        color = ~cluster_name,
        colors = "Set1",
        sizes = c(15, 40),  # Marker size range
        text = ~paste(
          "Term: ", term,
          "<br>Cluster: ", cluster_name,
          "<br>No. of Definitions: ", total_definitions,
          "<br>Tokens: ", tokens,
          "<br>Types: ", types,
          "<br>Type-to-Token Ratio: ", round(type_to_token_ratio, 2),
          "<br>Average Cosine Similarity: ", round(avg_similarity, 2)
        ),
        hoverinfo = 'text',
        type = 'scatter3d',
        mode = 'markers',
        marker = list(
          opacity = 0.8,
          line = list(width = 2, color = "black")
        )
      ) %>%
        layout(
          title = "3D Plot: Types, Tokens & Type-to-Token Ratio",
          scene = list(
            xaxis = list(title = "Tokens"),
            yaxis = list(title = "Types"),
            zaxis = list(title = "Type-to-Token Ratio")
          )
        )
    })
    
    # Observe maximum values for sliders dynamically
    observe({
      max_types <- max(df_byterm$types, na.rm = TRUE)
      max_tokens <- max(df_byterm$tokens, na.rm = TRUE)
      max_ttr <- max(df_byterm$type_to_token_ratio, na.rm = TRUE)
      
      # Update sliders dynamically
      updateSliderInput(
        session, "typesFilter",
        min = 1, max = max_types,
        value = c(1, max_types)
      )
      updateSliderInput(
        session, "tokensFilter",
        min = 1, max = max_tokens,
        value = c(1, max_tokens)
      )
      updateSliderInput(
        session, "ttrFilter",
        min = 0, max = max_ttr,
        value = c(0, max_ttr)
      )
    })
    
    # Render the 3D scatter plot for "Types & Tokens Analysis"
    output$typesTokensPlot <- renderPlotly({
      data <- filtered3DData()  # Use the filtered dataset
      
      plot_ly(
        data = data,
        x = ~tokens,
        y = ~types,
        z = ~type_to_token_ratio,
        color = ~cluster_name,
        colors = "Set1",
        sizes = c(15, 40),  # Increased marker size range
        text = ~paste("Term: ", term, 
                      "<br>Cluster: ", cluster_name, 
                      "<br>No. of Definitions: ", total_definitions, 
                      "<br>Tokens: ", tokens, 
                      "<br>Types: ", types, 
                      "<br>Type-to-Token Ratio: ", round(type_to_token_ratio, 2),
                      "<br>Average Cosine Similarity: ", round(avg_similarity, 2)),
        hoverinfo = 'text',
        type = 'scatter3d',
        mode = 'markers',
        marker = list(opacity = 0.8,
                      line = list(width = 2,
                                  color = "black"
                      )
        )
      ) %>%
        layout(
          title = "Scatter Plot of Types, Tokens & Type-to-Token Ratio of Terms",
          scene = list(
            xaxis = list(title = "Tokens"),
            yaxis = list(title = "Types"),
            zaxis = list(title = "Type-to-Token Ratio")
          )
        )
    })
    
    # Render the table when the button is clicked
    observeEvent(input$generateTable, {
      filtered_data <- filtered3DData()  # Get filtered data
      
      output$filteredTable <- renderDT({
        req(nrow(filtered_data) > 0)  # Ensure the table is rendered only if data exists
        
        datatable(
          filtered_data %>%
            select(term, cluster_name, total_definitions, types, tokens, type_to_token_ratio, avg_similarity),
          colnames = c("Term", "Cluster Name", "Total Definitions", "Types", "Tokens", 
                       "Type-to-Token Ratio", "Average Cosine Similarity"),
          options = list(
            dom = 't',  # Only show the table (no search bar or pagination)
            scrollX = TRUE,  # Enable horizontal scrolling
            scrollY = "400px",  # Enable vertical scrolling with a fixed height
            paging = FALSE,  # Disable pagination
            columnDefs = list(list(targets = "_all", className = "dt-center"))  # Center align columns
          ),
          rownames = FALSE  # Disable row numbers
        )
      })
    })
    
    ########################
    ## DEFINITION NETWORK ##
    ########################
    
    # Reactive to filter the pairwise cosine similarity data for the selected term
    filteredDefinitions <- reactive({
      req(input$selectedDefinitionTerm)
      
      # Filter pairwise cosine similarity data for the selected term's definitions
      pairwise_filtered <- pairwise_cosine_similarity %>%
        filter(
          def_ID1 %in% df_bydefinition$def_ID[df_bydefinition$term == input$selectedDefinitionTerm] |
            def_ID2 %in% df_bydefinition$def_ID[df_bydefinition$term == input$selectedDefinitionTerm]
        )
      
      # Enrich pairwise_filtered with definition and cluster information
      enriched_definitions <- pairwise_filtered %>%
        
        # Add term and cluster_name for def_ID1
        left_join(
          df_bydefinition %>%
            select(def_ID, term) %>%
            left_join(df_byterm %>% select(term, cluster_name), by = "term"),
          by = c("def_ID1" = "def_ID")
        ) %>%
        rename(term1 = term, cluster_name1 = cluster_name) %>%
        
        # Add term and cluster_name for def_ID2
        left_join(
          df_bydefinition %>%
            select(def_ID, term) %>%
            left_join(df_byterm %>% select(term, cluster_name), by = "term"),
          by = c("def_ID2" = "def_ID")
        ) %>%
        rename(term2 = term, cluster_name2 = cluster_name)
      
      # Ensure variables are matching by converting the strings into lowercase
      df_bydefinition <- df_bydefinition %>%
        mutate(def_ID = trimws(def_ID))  # Remove extra spaces
      
      pairwise_cosine_similarity <- pairwise_cosine_similarity %>%
        mutate(def_ID1 = trimws(def_ID1), def_ID2 = trimws(def_ID2))  # Remove extra spaces
      
      df_bydefinition <- df_bydefinition %>%
        mutate(def_ID = tolower(def_ID))  # Convert to lowercase
      
      # Rename concept to term
      pairwise_cosine_similarity <- pairwise_cosine_similarity %>% rename(term = concept)
      
      # Return enriched definitions, or handle cases with no connections
      if (nrow(enriched_definitions) == 0) {
        # If no pairwise connections exist, return a placeholder with a single definition
        single_definition <- df_bydefinition %>%
          filter(term == input$selectedDefinitionTerm) %>%
          select(def_ID, term, cluster_name)
        
        if (nrow(single_definition) == 1) {
          return(data.frame(
            def_ID1 = single_definition$def_ID,
            def_ID2 = single_definition$def_ID,
            cosine_similarity = NA,
            term1 = single_definition$term,
            term2 = single_definition$term,
            cluster_name1 = single_definition$cluster_name,
            cluster_name2 = single_definition$cluster_name
          ))
        }
      }
      
      enriched_definitions
    })
    
    # Reactive value to track node clicks
    clickedNode <- reactiveVal(NULL)
    
    # Render the definition network graph
    output$definitionNetwork <- renderVisNetwork({
      req(filteredDefinitions())  # Ensure filtered data is available
      
      # Edges: Cosine similarity
      edges <- filteredDefinitions() %>%
        select(from = def_ID1, to = def_ID2, weight = cosine_similarity)
      
      # Handle cases with NA weights and ensure valid scaling
      if (nrow(edges) > 0) {
        # Remove rows with NA weights
        edges <- edges %>%
          filter(!is.na(weight))
        
        # Handle cases where there are no valid weights left
        if (nrow(edges) > 0) {
          edges <- edges %>%
            mutate(
              width = 2 + (weight - min(weight, na.rm = TRUE)) / 
                (max(weight, na.rm = TRUE) - min(weight, na.rm = TRUE)) * 15
            )
        } else {
          # If all rows were removed, create an empty edges data frame
          edges <- data.frame(from = character(0), to = character(0), weight = numeric(0), width = numeric(0))
        }
      } else {
        # If edges were initially empty, create an empty data frame
        edges <- data.frame(from = character(0), to = character(0), weight = numeric(0), width = numeric(0))
      }
      
      # Nodes: Definitions
      nodes <- df_bydefinition %>%
        filter(def_ID %in% c(edges$from, edges$to) | term == input$selectedDefinitionTerm) %>%
        select(id = def_ID, label = def_ID, title = source, cluster_name) %>%
        mutate(
          color = cluster_colors[cluster_name]
        )
      
      # Create visNetwork graph
      visNetwork(nodes, edges, width = "100%", height = "500px") %>%
        visNodes(
          shape = "dot",
          size = 20,
          font = list(size = 16),
          color = ~color
        ) %>%
        visEdges(
          color = list(color = "gray", highlight = "pink"),
          smooth = TRUE,
          width = ~width
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
        ) %>%
        visInteraction(
          hover = TRUE, tooltipDelay = 0, dragNodes = FALSE
        ) %>%
        visPhysics(
          stabilization = TRUE,
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -200, centralGravity = 0.01)
        ) %>%
        visEvents(
          click = "function(nodes) {
      Shiny.onInputChange('clicked_node', nodes.nodes[0]);
    }"
        )
    })
    
    # Update reactive value whenever a node is clicked
    observeEvent(input$clicked_node, {
      clickedNode(input$clicked_node)
      
      # Update node color to orange
      visNetworkProxy("definitionNetwork") %>%
        visUpdateNodes(
          nodes = data.frame(id = input$clicked_node, color = "orange")
        )
    })
    
    # Render the clicked node's definition
    output$nodeDefinition <- renderText({
      req(clickedNode())
      selected_definition <- df_bydefinition %>%
        filter(def_ID == clickedNode()) %>%
        pull(definition)
      if (length(selected_definition) == 0) {
        return("No definition available.")
      }
      selected_definition
    })
    
    ################################
    ## WORD CO-OCCURRENCE NETWORK ##
    ################################
    
    observe({
      # Create hierarchical list for dropdown
      cluster_terms <- df_bydefinition %>%
        filter(!is.na(cluster_name)) %>%
        group_by(cluster_name) %>%
        summarise(terms = list(unique(term)), .groups = "drop")  # Ensure unique terms
      
      # Create choices list with a placeholder
      choices_list <- list("(Select a term)" = "")  # Add placeholder
      for (i in seq_len(nrow(cluster_terms))) {
        cluster_name <- cluster_terms$cluster_name[i]
        terms <- cluster_terms$terms[[i]]
        choices_list[[cluster_name]] <- terms
      }
      
      # Update dropdown with hierarchical choices
      updateSelectizeInput(
        session,
        "selectedTermWord",
        label = "Select Term",
        choices = choices_list,
        selected = "",  # No term selected by default
        server = TRUE   # Enable server-side selectize
      )
    })
    
    # Reactive function to process co-occurrence data for the selected term
    wordCooccurrence <- reactive({
      req(input$selectedTermWord)  # Ensure a term is selected
      
      # Filter definitions for the selected term
      selected_definitions <- df_bydefinition %>%
        filter(term == input$selectedTermWord) %>%
        pull(def_clean)  # Extract the clean definitions
      
      # Tokenize definitions into words
      tokens <- tibble(id = seq_along(selected_definitions), text = selected_definitions) %>%
        unnest_tokens(word, text)
      
      # Check the number of definitions for the selected term
      if (length(selected_definitions) == 1) {
        # Return word frequencies for a single definition
        frequencies <- tokens %>%
          count(word, sort = TRUE)
        return(list(type = "frequency", data = frequencies))
      } else {
        # Calculate word co-occurrences for multiple definitions
        cooccurrences <- tokens %>%
          pairwise_count(word, id, sort = TRUE) %>%
          filter(n >= input$frequencyThreshold)  # Filter edges based on frequency
        return(list(type = "network", data = cooccurrences))
      }
    })
    
    # Visualisation
    output$frequencyPlot <- renderPlotly({
      result <- wordCooccurrence()
      req(result$type == "frequency")  # Ensure it's a frequency plot
      
      # Get the cluster of the selected term
      selected_cluster <- df_bydefinition %>%
        filter(term == input$selectedTermWord) %>%
        pull(cluster_name) %>%
        unique()
      
      # Assign color for the bars
      bar_color <- ifelse(
        length(selected_cluster) > 0 && selected_cluster %in% names(cluster_colors),
        cluster_colors[selected_cluster],  # Use the color of the selected cluster
        "gray"  # Default color for unmatched clusters
      )
      
      # Create the bar chart
      plot_ly(
        data = result$data,
        x = ~reorder(word, -n),
        y = ~n,
        type = "bar",
        text = ~paste("Frequency: ", n),
        hoverinfo = "text",
        marker = list(color = bar_color)  # Apply the color dynamically
      ) %>%
        layout(
          title = paste("Word Frequency for Term:", input$selectedTermWord),
          xaxis = list(title = "Words"),
          yaxis = list(title = "Frequency", tickformat = ",d")
        )
    })
    
    # Render the word co-occurrence network for multiple definitions
    output$cooccurrenceNetwork <- renderVisNetwork({
      result <- wordCooccurrence()
      req(result$type == "network")  # Ensure it's a network plot
      
      cooccurrences <- result$data
      
      # Define nodes
      nodes <- tibble(id = unique(c(cooccurrences$item1, cooccurrences$item2))) %>%
        mutate(label = id, size = 10)
      
      # Get the cluster of the selected term
      selected_cluster <- df_bydefinition %>%
        filter(term == input$selectedTermWord) %>%
        pull(cluster_name) %>%
        unique()
      
      # Assign colors based on the selected term's cluster
      nodes <- nodes %>%
        mutate(
          color = ifelse(
            length(selected_cluster) > 0 && selected_cluster %in% names(cluster_colors),
            cluster_colors[selected_cluster],  # Use the color of the selected cluster
            "gray"  # Default color for unmatched clusters
          )
        )
      
      # Define edges
      edges <- cooccurrences %>%
        rename(from = item1, to = item2, width = n)  # Use 'n' as edge width
      
      # Ensure edges and nodes are not empty
      if (nrow(nodes) == 0 || nrow(edges) == 0) {
        print("No nodes or edges to display.")
        return(NULL)  # Exit rendering if no data is available
      }
      
      # Create the visNetwork graph
      visNetwork(nodes, edges) %>%
        visNodes(
          shape = "dot",
          color = list(background = nodes$color, border = "black"),  # Apply colors from nodes
          font = list(size = 14)
        ) %>%
        visEdges(
          smooth = TRUE,
          color = list(color = "gray", highlight = "orange")
        ) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
        visInteraction(navigationButtons = TRUE)
    })
    
    # Dynamic UI to show the appropriate plot
    output$wordCooccurrencePlot <- renderUI({
      result <- wordCooccurrence()
      
      if (result$type == "frequency") {
        plotlyOutput("frequencyPlot", height = "600px")
      } else if (result$type == "network") {
        visNetworkOutput("cooccurrenceNetwork", height = "600px")
      }
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
  