dataSummaryUI <- function(id) {
  tagList(
    textOutput(NS(id, "text")),
    verbatimTextOutput(NS(id, "data_size")),
    verbatimTextOutput(NS(id, "data_summary")),
    textOutput(NS(id, "exp_text")),
    verbatimTextOutput(NS(id, "exp_summary"))
  )
}

dataSummaryServer <- function(id, data_set, exp_names) {
  stopifnot(is.reactive(data_set))
  stopifnot(is.reactive(exp_names))
  
  moduleServer(id, function(input, output, session) {
    # Display data summary
    output$text <- renderText("Resumen de los datos:")
    
    output$data_size <- renderPrint({
      cat(
        "Tamaño de los datos (dentro del ambiente de R):", 
        object.size(data_set())/1000, "Kb", "\n"
      )
      cat("Número de of renglones:", nrow(data_set()), "\n") 
      cat("Número de columnas:", ncol(data_set()))
    })
    
    output$data_summary <- renderPrint({
      summary(data_set())
    })
    
    # Display summary for explanatory variable
    output$exp_text <- renderText("Resumen para la variable explicativa:")
    
    exp_table <- reactive(table(exp_names()))
    
    output$exp_summary <- renderPrint({
      cat("Cuenta total:", sum(as.data.frame(exp_table())$Freq), "\n")
      cat("Tabla de frecuencias:")
      exp_table()
    })
  })
}