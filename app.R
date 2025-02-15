library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(glue)
library(tmtyro)

ui <- page_navbar(
  title = "tmtyro Builder",
  id = "nav",
  sidebar = sidebar(
    accordion(
      id = "workflow_accordion",
      multiple = FALSE,
      accordion_panel(
        "Load",
        icon = icon("book"),
        selectInput(
          "source_type", "Source Type",
          choices = c("Local ZIP File", "Local Folder", "Gutenberg IDs", "URLs"),
          selected = "Gutenberg IDs"),
        conditionalPanel(
          "input.source_type == 'Local ZIP File'",
          fileInput("zip_file", "Upload ZIP file")
        ),
        conditionalPanel(
          "input.source_type == 'Local Folder'",
          textInput("folder_path", "Folder Path")
        ),
        conditionalPanel(
          "input.source_type == 'Gutenberg IDs'",
          textInput(
            "gutenberg_ids",
            "Enter Gutenberg IDs separated by commas",
            value = 2814)
        ),
        conditionalPanel(
          "input.source_type == 'URLs'",
          textAreaInput("urls", "Enter URLs (one per line)")
        ),
        tags$details(
          tags$summary(
            style = "margin-bottom: 1em;",
            "Customize"),
            checkboxInput("lowercase", "Lowercase", value = TRUE),
            checkboxInput("remove_names", "Remove names", value = FALSE),
            checkboxInput("keep_original", "Keep original", value = FALSE),
            checkboxInput("parts_of_speech", "Parts of speech", value = FALSE),
            checkboxInput("lemmatize", "Lemmatize", value = FALSE),
            radioButtons("tokenization", "Tokenization",
                         choices = c("word", "other")),
            conditionalPanel(
              "input.tokenization == 'other'",
              textInput("custom_token", "Custom tokenization pattern")
            )
          ),
        actionButton("load", "Load", class = "btn-primary")
      ),
      accordion_panel(
        "Adjust",
        icon = icon("sliders"),
        checkboxInput("move_header", "Move header to text"),
        conditionalPanel(
          "input.move_header == true",
          uiOutput("header_column_select")
        ),
        checkboxInput("identify_by", "Identify by column"),
        conditionalPanel(
          "input.identify_by == true",
          selectInput("identify_column", NULL, choices = "")
        ),
        checkboxInput("standardize_titles", "Standardize titles"),
        checkboxInput("drop_cols", "Remove columns"),
        conditionalPanel(
          "input.drop_cols == true",
          selectInput("identify_drops", NULL,
                      choices = "",
                      multiple = TRUE)
        ),
      ),
      accordion_panel(
        "Study",
        icon = icon("magnifying-glass"),
        checkboxGroupInput(
          "study_options", "",
          choices = c(
            "Add frequency",
            "Add vocabulary",
            "Add sentiment",
            "Add ngrams",
            "Add tf-idf",
            "Drop NA",
            "Drop stopwords")),
        conditionalPanel(
          "input.study_options.includes('Add sentiment')",
          selectInput(
            "sentiment_lexicon", "Sentiment Lexicon",
            choices = c("afinn", "bing", "nrc", "custom"),
            selected = "bing"),
          conditionalPanel(
            "input.sentiment_lexicon == 'custom'",
            fileInput("custom_lexicon", "Upload custom lexicon")
          )
        ),
        selectInput(
          "feature_col", "Feature column:",
          choices = "word",
          selected = "word"
          )
      ),
    )
  ),
  # nav_spacer(),
  nav_panel(
    "Build",
    icon = icon("arrow-pointer"),
    layout_columns(
      navset_card_pill(
        nav_panel(
          "Data",
          DTOutput("data_table")),
        nav_panel(
          "contextualize()",
          icon = icon("glasses"),
          textInput("search_term", "Search Term"),
          numericInput("context_window", "Context Window Size", value = 5),
          uiOutput("context"))
        ),
      card(
          card_header(
            icon("table"),
            "tabulize()"),
          uiOutput("tabulize"),
          full_screen = TRUE),
        card(
          card_header(
            icon("chart-simple"),
            "visualize()"),
          plotOutput("visualize"),
          full_screen = TRUE),
      col_widths = c(12, 4, 8),
      row_heights = c(1,1))),
  nav_panel(
    "Code",
    icon = icon("code"),
    verbatimTextOutput("code_output")))

server <- function(input, output, session) {
  the_corpus <- reactive({
    message(input$gutenberg_ids)
    print(input$gutenberg_ids)

    if (input$source_type == "Gutenberg IDs") {
      df_corpus <- input$gutenberg_ids |>
        strsplit(",") |>
        unlist() |>
        as.numeric() |>
        get_gutenberg_corpus()
    }

    params <- list(
      src = df_corpus,
      to_lower = input$lowercase,
      remove_names = input$remove_names,
      keep_original = input$keep_original,
      pos = input$parts_of_speech,
      lemma = input$lemmatize
    )

    result <- rlang::exec(load_texts, !!!params)
    if ("integer" %in% class(result$doc_id)) {
      result$doc_id <- factor(result$doc_id)
    }
    result
  }) |>
    bindEvent(input$load)

  the_corpus2 <- reactive({
    df_corpus <- the_corpus()
    if (input$move_header && !is.null(input$header_column)) {
      df_corpus <- df_corpus |>
        move_header_to_text(column = input$header_column)
    }

    if (input$identify_by && !is.null(input$identify_column)) {
      df_corpus <- df_corpus |>
        identify_by(input$identify_column)
    }

    if (input$drop_cols && !is.null(input$identify_drops)) {
      df_corpus <- df_corpus |>
        dplyr::select(-all_of(input$identify_drops))
    }

    if (input$standardize_titles) {
      df_corpus <- df_corpus |>
        standardize_titles()
    }

    if ("Add frequency" %in% input$study_options) {
      df_corpus <- df_corpus |>
        add_frequency()
        # add_frequency(feature = !!sym(input$feature_col))
    }

    if ("Add vocabulary" %in% input$study_options) {
      df_corpus <- df_corpus |>
        add_vocabulary()
        # add_vocabulary(feature = !!rlang::sym(input$feature_col))
    }

    if ("Add sentiment" %in% input$study_options) {
      df_corpus <- df_corpus |>
        add_sentiment(lexicon = input$sentiment_lexicon)
        # add_sentiment(lexicon = input$sentiment_lexicon, feature = !!rlang::sym(input$feature_col))
    }

    if ("Add ngrams" %in% input$study_options) {
      df_corpus <- df_corpus |>
        add_ngrams()
        # add_ngrams(feature = !!rlang::sym(input$feature_col))
    }

    if ("Add tf-idf" %in% input$study_options) {
      df_corpus <- df_corpus |>
        add_tf_idf()
        # add_tf_idf(feature = !!rlang::sym(input$feature_col))
    }

    if ("Drop NA" %in% input$study_options) {
      df_corpus <- df_corpus |>
        drop_na()
    }

    if ("Drop stopwords" %in% input$study_options) {
      df_corpus <- df_corpus |>
        drop_stopwords()
    }

    df_corpus
  })

  observe({
    input$load

    updateSelectInput(
      session, "identify_column",
      choices = the_corpus() |>
        colnames(),
      selected = "doc_id"
      )

    updateSelectInput(
      session, "identify_drops",
      choices = the_corpus() |>
        colnames() |>
        stringr::str_subset("doc_id", negate = TRUE)
        )
  })

  # Reactive value to store the code steps
  code_steps <- reactiveVal(character())

  output$data_table <- renderDT(
    the_corpus2()
  )

  output$context <- renderUI(
    the_corpus2() |>
      contextualize(
        input$search_term,
        window = input$context_window,
        html = TRUE)
  )

  output$tabulize <- renderUI(
    the_corpus2() |>
      tabulize()
      # this needs work
      # tabulize(feature = input$feature_col)
  )

  output$visualize <- renderPlot(
    the_corpus2() |>
      visualize()
      # this needs work
      # visualize(feature = input$feature_col)
  )

  # Helper function to update code steps
  update_code_steps <- function() {
    steps <- character()

    # Initial step based on source type
    if (!is.null(input$source_type)) {
      if (input$source_type == "Gutenberg IDs") {
        steps <- c(steps, glue('get_gutenberg_corpus(c({paste0(input$gutenberg_ids, collapse=",")}))'))
      } else if (input$source_type == "Local ZIP File") {
        steps <- c(steps, 'load_texts("uploaded_zip")')
      } else if (input$source_type == "Local Folder") {
        steps <- c(steps, glue('load_texts("{input$folder_path}")'))
      } else if (input$source_type == "URLs") {
        steps <- c(steps, glue('download_once(c({paste0(input$urls, collapse = ",")})) |>\n  load_texts()'))
      }
    }

    # pause for code to work
    req(input$tokenization)

      # only add code for changes from default
      val_lowercase <- if_else(input$lowercase, "", "\n    lowercase = FALSE,")
      val_remove_names <- if_else(input$remove_names, "\n    remove_names = TRUE,", "")
      val_keep_original <- if_else(input$keep_original, "\n    keep_original = TRUE,", "")
      val_pos <- if_else(input$parts_of_speech, "\n    pos = TRUE,", "")
      val_lemma <- if_else(input$lemmatize, "\n    lemmatize = TRUE,", "")
      val_token <- if_else(!is.null(input$tokenization) && input$tokenization == "other", glue(',\n    token = "{input$custom_token}"'), "")

      load_text_params <- glue('load_texts({val_lowercase}{val_remove_names}{val_keep_original}{val_pos}{val_lemma}{val_token})') |>
        stringr::str_replace_all(",\\)", ")")
      steps <- c(steps, load_text_params)


    # Tidy operations
    if (input$move_header && !is.null(input$header_column)) {
      steps <- c(steps, glue('move_header_to_text({input$header_column})'))
    }
    if (input$identify_by && !is.null(input$identify_column)) {
      steps <- c(steps, glue('identify_by({input$identify_column})'))
    }
    if (input$standardize_titles) {
      steps <- c(steps, 'standardize_titles()')
    }

    # Study operations
    if (!is.null(input$study_options)) {
      for (option in input$study_options) {
        if (option == "Add frequency") {
          steps <- c(steps, 'add_frequency()')
        } else if (option == "Add vocabulary") {
          steps <- c(steps, 'add_vocabulary()')
        } else if (option == "Add sentiment") {
          lexicon <- if(input$sentiment_lexicon == "custom") "custom_lex" else input$sentiment_lexicon
          steps <- c(steps, glue('add_sentiment("{lexicon}")'))
        } else if (option == "Add ngrams") {
          steps <- c(steps, 'add_ngrams()')
        } else if (option == "Add tf-idf") {
          steps <- c(steps, 'add_tf_idf()')
        } else if (option == "Drop NA") {
          steps <- c(steps, 'drop_na()')
        } else if (option == "Drop stopwords") {
          steps <- c(steps, 'drop_stopwords()')
        }
      }
    }

    # Combine all steps into a single pipeline
    if (length(steps) > 0) {
      workflow <- paste0("my_data <- ", steps[1])
      if (length(steps) > 1) {
        for (i in 2:length(steps)) {
          workflow <- paste0(workflow, " |>\n  ", steps[i])
        }
      }

      # Add the final output step if specified
      if (!is.null(input$output_type)) {
        if (input$output_type == "Contextualize" && !is.null(input$search_term)) {
          workflow <- paste0(workflow, glue('\n\nresult <- my_data |>\n  contextualize(\n    pattern = "{input$search_term}",\n    window = {input$context_window}\n  )'))
        } else if (input$output_type == "Tabulize") {
          workflow <- paste0(workflow, '\n\nresult <- my_data |>\n  tabulize()')
        } else if (input$output_type == "Visualize") {
          workflow <- paste0(workflow, '\n\nresult <- my_data |>\n  visualize()')
        }
      }

      code_steps(workflow)
    }
  }

  # Observe all inputs that should trigger code updates
  observe({
    input$source_type
    input$folder_path
    input$tokenization
    input$custom_token
    input$lowercase
    input$remove_names
    input$keep_original
    input$parts_of_speech
    input$lemmatize
    input$move_header
    input$header_column
    input$identify_by
    input$identify_column
    input$standardize_titles
    input$study_options
    input$sentiment_lexicon
    input$output_type
    input$search_term
    input$context_window

    update_code_steps()
  })

  observe({
    updateSelectInput(
      session, "feature_col",
      choices = the_corpus() |>
        colnames() |>
        stringr::str_subset("doc_id", negate = TRUE),
      selected = "word"
    )
  })

  # Update the code output
  output$code_output <- renderText({
    req(code_steps())
    paste("# tmtyro workflow\nlibrary(tmtyro)\n\n",
          "# prepare the data","\n",
          code_steps(),
          sep = "") |>
      paste0(
        "\n\n",
        "# make a table", "\n",
        "tabulize(my_data)",
        "\n\n",
        "# make a chart", "\n",
        "visualize(my_data)"
        )
  })
}

shinyApp(ui, server)
