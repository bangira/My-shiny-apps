library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(DT)

ui <- navbarPage(
  "",
  tabPanel(
    "Nursery rows fieldbook generator",
    fluidPage(
      titlePanel(""),
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Input Excel File", accept = c(".xlsx")),
          numericInput("start_row", "Enter Starting Row Number (typically ends with 1)", value = 1001, min = 1),
          numericInput("tier_total", "Total number of rows in each tier", value = 100, min = 1),
          div(style = "margin-top: 20px;",
              downloadButton("download_fieldbook", "Download nursery row fieldbook", 
                             style = "background-color: #505050; color: white; margin-bottom: 10px;")
          ),
          div(style = "margin-top: 10px;",
              downloadButton("download_summary", "Download nursery row summary", 
                             style = "background-color: #505050; color: white; margin-bottom: 10px;")
          ),
          div(style = "margin-top: 10px;",
              downloadButton("download_breedbase", "Download new accession template for Breedbase", 
                             style = "background-color: #505050; color: white; margin-bottom: 10px;")
          ),
          div(style = "margin-top: 10px;",
              downloadButton("download_template", "Input Template example", 
                             style = "background-color: #4CAF50; color: white;")
          ),
          
          htmlOutput("validation_result"),
          tags$hr(),
          h4("Notes:"),
          p("It will generate an accession template that will be used to upload new accessions to Breedbase.")
        ),
        mainPanel(
          DTOutput("table")
        )
      )
    )
  ),
  tabPanel(
    "Nursery rows set fieldbook file combiner",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          fileInput("files", "Choose files to combine:", multiple = TRUE, accept = c(".csv", ".xlsx")),
          downloadButton("downloadButton", "Download Combined Data (Excel)"),
          tags$hr(),
          h4("Notes:"),
          p("This tab allows you to combine multiple fieldbook files (.csv or .xlsx) into one Excel file.")
        ),
        mainPanel(
          tableOutput("combinedTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  processed_data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    
    required_columns <- c("Loading unit", "Existing BB accession name", "Loading Unit Total Rows", 
                          "Number of Purple rows", "Number of Blank Rows",  
                          "Existing BB Family_ID", "Generation", 
                          "Desired Fam Accession suffix", "Tier", "Year (2 digit)", "HT Set ID",
                          "herbicide_class", "grain_class", "grain_type", "Pedigree",
                          "purdy_pedigree", "Pop Native Traits", "Material", "MAS NT info", 
                          "Available Panicles", "Overall Set ID", "MAS Source", "WGP Source")
    missing_columns <- setdiff(required_columns, names(df))
    
    if (length(missing_columns) > 0) {
      output$validation_result <- renderUI({
        HTML(paste0('<div style="color: red; font-weight: bold;">',
                    "ERROR: Missing required columns: ", 
                    paste(missing_columns, collapse = ", "),
                    '</div>'))
      })
      return(NULL)
    }
    
    first_row_year <- df[1, "Year (2 digit)"]
    generation_col_name <- paste0(first_row_year, "-Generation")
    names(df)[names(df) == "Generation"] <- generation_col_name
    
    tier_checks <- df %>%
      group_by(Tier) %>%
      summarise(
        total = sum(`Loading Unit Total Rows` + `Number of Purple rows` + `Number of Blank Rows`, na.rm = TRUE),
        .groups = "drop"
      )
    
    invalid_tiers <- tier_checks %>% 
      filter(total != input$tier_total) %>% 
      pull(Tier)
    
    if (length(invalid_tiers) > 0) {
      output$validation_result <- renderUI({
        HTML(paste0('<div style="color: red; font-weight: bold;">',
                    "ERROR: The following tier rows don't sum to ", input$tier_total, ": ", 
                    paste(invalid_tiers, collapse = ", "),
                    '</div>'))
      })
      return(NULL)
    } else {
      output$validation_result <- renderUI({
        HTML(paste0('<div style="color: green; font-weight: bold;">Validation passed: Each tier rows sum to ', input$tier_total, '</div>'))
      })
    }
    
    expand_rows <- function(df) {
      result_list <- list()
      
      for (i in seq_len(nrow(df))) {
        row <- df[i, ]
        expanded_rows <- row[rep(1, row$`Loading Unit Total Rows`), ]
        
        current_year <- row$`Year (2 digit)`
        current_overall_set <- row$`Overall Set ID`
        current_ht_set <- row$`HT Set ID`
        
        purple_leaf_rows <- if (row$`Number of Purple rows` > 0) {
          empty_row <- as.data.frame(matrix(NA, nrow = row$`Number of Purple rows`, ncol = ncol(df)))
          colnames(empty_row) <- colnames(df)
          empty_row$`Loading unit` <- "Purple_row"
          empty_row$`Year (2 digit)` <- current_year
          empty_row$`Overall Set ID` <- current_overall_set
          empty_row$`HT Set ID` <- current_ht_set
          empty_row
        } else NULL
        
        blank_rows <- if (row$`Number of Blank Rows` > 0) {
          empty_row <- as.data.frame(matrix(NA, nrow = row$`Number of Blank Rows`, ncol = ncol(df)))
          colnames(empty_row) <- colnames(df)
          empty_row$`Loading unit` <- "Blank"
          empty_row$`Year (2 digit)` <- current_year
          empty_row$`Overall Set ID` <- current_overall_set
          empty_row$`HT Set ID` <- current_ht_set
          empty_row
        } else NULL
        
        result_list[[i]] <- bind_rows(expanded_rows, purple_leaf_rows, blank_rows)
      }
      
      bind_rows(result_list)
    }
    
    final_df <- expand_rows(df) %>%
      mutate(`Row number` = seq(input$start_row, length.out = n(), by = 1)) %>%
      mutate(`Row number` = paste0(`Year (2 digit)`, "-", `Row number`))
    
    existing_accessions <- df %>% 
      filter(!is.na(`Existing BB accession name`) & `Existing BB accession name` != "") %>% 
      pull(`Existing BB accession name`) %>% 
      unique()
    
    final_df <- final_df %>%
      mutate(
        `Family accession name` = case_when(
          `Loading unit` %in% c("Purple_row", "Blank") ~ `Loading unit`,
          !is.na(`Existing BB accession name`) & `Existing BB accession name` != "" ~ "",
          (is.na(`Existing BB Family_ID`) | `Existing BB Family_ID` == "") & 
            (is.na(`Existing BB accession name`) | `Existing BB accession name` == "") ~ `Loading unit`,
          TRUE ~ paste(`Existing BB Family_ID`, !!sym(generation_col_name), `Year (2 digit)`, `Desired Fam Accession suffix`, sep = "_")
        ),
        `Breedbase accession name` = case_when(
          !is.na(`Family accession name`) & `Family accession name` != "" ~ `Family accession name`,
          !is.na(`Existing BB accession name`) & `Existing BB accession name` != "" ~ `Existing BB accession name`,
          TRUE ~ `Loading unit`
        )
      )
    
    total_rows <- nrow(final_df)
    final_df$Row <- rep(1:ceiling(total_rows/input$tier_total), each = input$tier_total, length.out = total_rows)
    
    column_pattern <- unlist(lapply(1:ceiling(total_rows/input$tier_total), function(x) {
      if (x %% 2 == 1) 1:input$tier_total else input$tier_total:1
    }))
    final_df$Column <- column_pattern[1:total_rows]
    
    fieldbook_output <- final_df %>%
      select(
        `Row number`, `Breedbase accession name`, `Overall Set ID`, `HT Set ID`, Row, Column,
        `Pedigree`, `purdy_pedigree`,
        !!sym(generation_col_name), 
        `herbicide_class`, `grain_class`, `grain_type`, `Material`, `MAS NT info`, `MAS Source`, `WGP Source`, `Pop Native Traits`
      )
    
    # Create summary first to use for synonyms
    summary_output <- fieldbook_output %>%
      filter(!(`Breedbase accession name` %in% c("Purple_row", "Blank"))) %>%
      group_by(`Breedbase accession name`) %>%
      summarise(
        `Start-Stop` = {
          year_part <- sub("^(\\d{2}).*$", "\\1", dplyr::first(`Row number`))
          start_num <- sub("^\\d{2}-(\\d+)$", "\\1", dplyr::first(`Row number`))
          end_num <- sub("^\\d{2}-(\\d+)$", "\\1", dplyr::last(`Row number`))
          paste0(year_part, "-", start_num, "-", end_num)
        },
        Count = n(),
        `Overall Set ID` = first(`Overall Set ID`),
        `HT Set ID` = first(`HT Set ID`),
        Pedigree = first(Pedigree),
        purdy_pedigree = first(purdy_pedigree),
        !!sym(generation_col_name) := first(!!sym(generation_col_name)),
        herbicide_class = first(herbicide_class),
        grain_class = first(grain_class),
        grain_type = first(grain_type),
        Material = first(Material),
        `MAS NT info` = first(`MAS NT info`),
        `MAS Source` = first(`MAS Source`),
        `WGP Source` = first(`WGP Source`),
        `Pop Native Traits` = first(`Pop Native Traits`),
        .groups = "drop"
      ) %>%
      mutate(Start_Num = as.numeric(sub("^\\d{2}-(\\d+)-.*$", "\\1", `Start-Stop`))) %>%
      arrange(Start_Num) %>%
      select(-Start_Num) %>%
      select(`Breedbase accession name`, `Start-Stop`, Count, everything())
    

    
    
    # Create breedbase output with synonyms from summary and new description column
    breedbase_output <- final_df %>%
      filter(`Loading unit` %in% c("Purple_row", "Blank") == FALSE) %>%
      filter(`Breedbase accession name` != "" & !(`Breedbase accession name` %in% existing_accessions)) %>%
      distinct(`Breedbase accession name`, .keep_all = TRUE) %>%
      left_join(summary_output %>% select(`Breedbase accession name`, `Start-Stop`), 
                by = "Breedbase accession name") %>%
      mutate(
        accession_name = `Breedbase accession name`,
        species_name = "Oryza sativa",
        population_name = "",
        organization_name = "LSU Rice",
        synonym = `Start-Stop`,  # Add Start-Stop as synonym
        description = "",  # New empty description column
        accession_number = "",
        population_type = "",
        ru_number = "",
        variety = ""
      ) %>%
      select(
        accession_name, species_name, population_name, organization_name,
        synonym, description, accession_number, grain_class, grain_type,
        herbicide_class, population_type, purdy_pedigree,
        ru_number, variety
      )
    
    first_row <- df[1, ]
    fieldbook_filename <<- paste0(first_row$`Year (2 digit)`, "_",
                                  first_row$`Desired Fam Accession suffix`, "_",
                                  first_row$`HT Set ID`, ".xlsx")
    
    list(fieldbook = fieldbook_output, 
         summary = summary_output,
         breedbase = breedbase_output, 
         generation_col_name = generation_col_name)
  })
 
  
  
  
  
  
  
  
  
  
  
  
  
   
  output$table <- renderDT({
    data <- processed_data()
    req(data)
    datatable(data$fieldbook, options = list(scrollX = TRUE, pageLength = 100))
  })
  
  output$download_fieldbook <- downloadHandler(
    filename = function() {
      if (exists("fieldbook_filename")) fieldbook_filename else "Row_Fieldbook.xlsx"
    },
    content = function(file) {
      data <- processed_data()
      write_xlsx(data$fieldbook, file)
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() {
      data <- processed_data()
      ht_set_id <- data$summary$`HT Set ID`[1]
      paste0("Nursery_row_summary-", ht_set_id, ".xlsx")
    },
    content = function(file) {
      data <- processed_data()
      write_xlsx(data$summary, file)
    }
  )
  
  output$download_breedbase <- downloadHandler(
    filename = function() {
      data <- processed_data()
      ht_set_id <- data$summary$`HT Set ID`[1]
      paste0("Accession_template_for_Breedbase-", ht_set_id, ".xlsx")
    },
    content = function(file) {
      data <- processed_data()
      req(data)
      write_xlsx(data$breedbase, file)
    }
  )
  
  output$download_template <- downloadHandler(
    filename = function() {
      "template-input file.xlsx"
    },
    content = function(file) {
      file.copy("template-input file.xlsx", file)
    }
  )
  
  # Combiner
  combined_df <- reactiveVal(NULL)
  
  observeEvent(input$files, {
    req(input$files)
    file_paths <- input$files$datapath
    file_names <- input$files$name
    
    df_list <- lapply(seq_along(file_paths), function(i) {
      ext <- tools::file_ext(file_paths[i])
      if (tolower(ext) %in% c("xlsx", "xls")) {
        original_names <- names(read_excel(file_paths[i], n_max = 0))
        df <- read_excel(file_paths[i], col_names = FALSE, skip = 1)
        names(df) <- original_names
      } else if (tolower(ext) == "csv") {
        original_names <- names(read.csv(file_paths[i], check.names = FALSE, nrows = 0))
        df <- read.csv(file_paths[i], check.names = FALSE, stringsAsFactors = FALSE)
        names(df) <- original_names
      } else return(NULL)
      df$File_name <- file_names[i]
      df %>% mutate(across(everything(), as.character))
    })
    
    df_list <- Filter(Negate(is.null), df_list)
    if (length(df_list) == 0) return(NULL)
    
    all_cols <- unique(unlist(lapply(df_list, names)))
    df_list <- lapply(df_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      if (length(missing_cols) > 0) df[missing_cols] <- NA
      df[all_cols]
    })
    
    combined_df(bind_rows(df_list) %>% select(File_name, everything()))
  })
  
  output$combinedTable <- renderTable({
    req(combined_df())
    combined_df()
  }, sanitize.text.function = identity)
  
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0("combined_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_xlsx(combined_df(), file)
    }
  )
}

shinyApp(ui, server)