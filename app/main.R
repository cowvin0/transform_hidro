box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags,
        uiOutput, sidebarLayout, sidebarPanel, fileInput, downloadButton,
        tableOutput, renderTable, numericInput, req, reactive, downloadHandler],
  utils[head],
  tidyr[pivot_longer],
  dplyr[starts_with]
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

    output$preview <- renderTable({
      data() |>
        pivot_longer(
          cols = starts_with("Chuva"),
          names_to = "Chuva",
          values_to = "Dia"
        ) #|>
        #head()
    })

    output$download <- downloadHandler(
      filename = \() base::paste0(input$upload$name, ".csv"),
      content = \(file) vroom::vroom_write(data(), file)
    )
  })
}
