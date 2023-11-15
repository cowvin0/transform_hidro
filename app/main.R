box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags,
        uiOutput, sidebarLayout, sidebarPanel, fileInput, downloadButton,
        tableOutput, renderTable, numericInput, req, reactive, downloadHandler],
  utils[head],
  tidyr[pivot_longer],
  dplyr[starts_with, mutate, select],
  stringr[str_replace, str_split],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    fileInput(ns("upload"), NULL, buttonLabel = "Faça Upload do arquivo", accept = ".csv"),
    downloadButton(ns("download"), buttonLabel = "Faça o download do arquivo"),
    tableOutput(ns("preview"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$upload)

      ext <- tools::file_ext(input$upload$name)
      switch(ext,
        csv = vroom::vroom(input$upload$datapath),
        validate("Arquivo Inválido: Faça o Upload de um arquivo .csv")
      )
    })

    new_data  <- reactive({
      splitting_date <- data()$Data |> str_split("/")

      data() |>
        mutate(
          Mês = unlist(splitting_date)[2] |> as.integer(),
          Ano = unlist(splitting_date)[3] |> as.integer()
        ) |>
        pivot_longer(
          cols = starts_with("Chuva"),
          names_to = "Dia",
          values_to = "Chuva"
        ) |>
        mutate(
          Dia = str_replace(Dia, "Chuva", "") |> as.integer()
        )
    })

    output$preview <- renderTable({
      new_data() |>
        head()
    })

    output$download <- downloadHandler(
      filename = \() base::paste0(input$upload$name, ".csv"),
      content = \(file) vroom::vroom_write(data(), file)
    )
  })
}
