# load in packages
library(DBI)

# load in ds packages
library(shinypanels)
library(shinyinvoer)
library(hgchmagic)
library(homodatum)
library(shi18ny)
library(DT)

## Setup DB

# data
col <- read_csv("data/prep/col.csv")
gbr <- read_csv("data/prep/gbr.csv")
prt <- read_csv("data/prep/prt.csv")

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "col", col)
dbWriteTable(con, "gbr", gbr)
dbWriteTable(con, "prt", prt)

# read queries file
queries <- read_csv("data/prep/queries.csv")

# possible viz
frtypes_doc_viz <- suppressWarnings(yaml::read_yaml("conf/frtypes_viz.yaml"))

# Define UI
ui <- panelsPage(useShi18ny(),
                 langSelectorInput("lang", position = "fixed"),
                 panel(id = "panel_data",
                       title = ui_("query_data"),
                       width = 350,
                       body = div(
                         div(
                           uiOutput("select_country"),
                           div(class="title-data-select", ui_("select_question")),
                           br(),
                           DT::dataTableOutput("select_question")
                         )
                       )),
                 panel(id = "panel_dic",
                       width = 200,
                       title = ui_("ocds_fields"),
                       can_collapse = TRUE,
                       collapsed = TRUE,
                       body = uiOutput("fields"),
                       footer = NULL),
                 panel(id = "panel_viz",
                       title = ui_("viz_data"),
                       # title_plugin = uiOutput("download"),
                       can_collapse = FALSE,
                       body = highchartOutput("viz"),
                       footer = uiOutput("viz_icons"))
                 )

# Define Server
server <- function(input, output, session) {

  i18n <- list(defaultLang = "en",
               availableLangs = c("en", "es", "pt"))

  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)

  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })

  output$select_country <- renderUI({
    choices <- c("col", "gbr", "prt")
    names(choices) <- i_(c("col", "gbr", "prt"), lang = lang())
    selectInput("selected_country",
                div(class="title-data-select", i_("select_country", lang())),
                choices = choices
                )
  })

  output$select_question <- DT::renderDataTable({

    # queries %>% filter(db == input$selected_country) %>% select(questions),
    dt <- queries %>% select_(paste0("questions_", lang()))
    datatable(dt,
              rownames = FALSE,
              selection = "single",
              colnames = NULL,
              options = list(
                dom = 't',
                # pageLength = 5,
                #language = list(search = 'Buscar:'),
                autoWidth = TRUE,
                #scrollX = TRUE,
                scrollY = TRUE
                #columnDefs = list(list(width = '200px'))
              ))

  },server = FALSE, escape = FALSE)

  query <- reactive({
    req(input$select_question_rows_selected)
    row_selected <- input$select_question_rows_selected
    queries %>% slice(row_selected) %>% pull(sql_query)

  })

  inputData <- reactive({
    if (is.null(query())) return ()
    query <- gsub("col", input$selected_country, query())
    tbl(con, sql(query)) %>% collect()
  })

  data_fringe <- reactive({
    fringe(inputData())
  })

  data_load <- reactive({
    data_fringe()$data
  })

  dic_load <- reactive({
    data_fringe()$dic
  })


  # keep _draw reactives in here in case we want to add column select input
  data_draw <- reactive({
    # var_select <- input$var_order
    dic <- dic_draw()
    # if (is.null(var_select)) return()
    # d <- data_load()[var_select]
    d <- data_load()
    names(d) <- dic$label
    d
  })

  dic_draw <- reactive({
    # var_select <- input$var_order
    # if (is.null(var_select)) return()
    # dic_load()[match(input$var_order, dic_load()$id),]
    dic_load()
  })

  ftype_draw <- reactive({
    if (is.null(dic_draw())) return()
    paste0(dic_draw()$hdType, collapse = "-")

  })

  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    frtypes_doc_viz[[ftype_draw()]]
  })

  actual_but <- reactiveValues(active_viz = 'bar')

  observe({
    viz_rec <- possible_viz()
    if (is.null(viz_rec)) return()
    if (is.null(input$viz_selection)) return()
    if (!( input$viz_selection %in% viz_rec)) {
      actual_but$active_viz <- viz_rec[1]
    } else {
      actual_but$active_viz <- input$viz_selection
    }
  })

  output$viz_icons <- renderUI({
    path <- 'img/svg/viz/'
    active <- actual_but$active_viz
    buttonImageInput('viz_selection',
                     "Visualization type",
                     images = possible_viz(),
                     # checkmarkColor = "#df5c33",
                     path = path,
                     format = 'svg',
                     active = active)
  })

  viz_name <- reactive({
    if (is.null(ftype_draw())) return()
    if (ftype_draw() == "") return()
    ctype <- gsub("-", "", ftype_draw())
    gtype <- actual_but$active_viz
    if (is.null(gtype)) return()
    typeV <- paste0('hgch_', gtype, '_', ctype)
    typeV
  })

  hgch_viz <- reactive({
    if (is.null(viz_name())) return()
    data <- data_draw()
    viz_name <- viz_name()
    dataLabels_show <- FALSE
    if(actual_but$active_viz %in% c("treemap", "bubbles")){
      dataLabels_show <- TRUE
    }
    opts <- dsvizopts::merge_dsviz_options(label_wrap = 30,
                                           legend_y_position = 10,
                                           legend_verticalAlign = 'top',
                                           dataLabels_show = dataLabels_show)
    viz <- do.call(viz_name, c(list(data = data, opts = opts
    )))

    viz
  })

  output$viz <- renderHighchart({
    viz <- hgch_viz()
    if (is.null(viz)) return()
    suppressWarnings(
      viz
    )
  })


  output$fields <- renderUI({

    p("HERE GO THE FIELDS AND DESCRIPTIONS IN THE CORRESPONDING LANGUAGE")

  })


}

shinyApp(ui, server)





