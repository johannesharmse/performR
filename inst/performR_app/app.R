library(shiny)
library(tidyverse)

log_stats <- function(file = getwd(), logfile = paste0(getwd(), '/logfile.csv'), period = 'hour', period_count = 1){

  if (file.exists(logfile)){
    last_entry <- unlist(strsplit(tail(readLines(logfile, warn = FALSE), n = 1), split = ','))
    last_time <- as.POSIXct(as.numeric(last_entry[length(last_entry)]), origin = "1970-01-01")
  }else{
    last_time <- NA
  }

  if (period == 'minute'){
    period_pos <- 1*60
  }else if(period == 'hour'){
    period_pos <- 60*60
  }
  else if (period == 'day'){
    period_pos <- 24*60*60
  }else if(period == 'week'){
    period_pos <- 7*24*60*60
  }

  period_pos <- period_pos*period_count

  if (is.na(last_time) || (Sys.time() > (last_time + period_pos))){
    code_file <- readLines(file, warn = FALSE)
    lines_num <- length(code_file)
    char_num <- sum(nchar(code_file))

    stats <- list('lines' = lines_num, 'chars' = char_num, 'date_time' = Sys.time())

    write(paste0(unlist(stats), collapse = ','), file = logfile, append=TRUE)
    message('Stats have been added successfully to log file')
    return(TRUE)

    # return(stats)

  }else{
    message(paste0(period_count, ' ', period, '(s) has not yet passed since last log (', as.character(last_time), ')'))
    return(FALSE)
  }


}

ui <- fluidPage(

   titlePanel("Testing"),

   sidebarLayout(
      sidebarPanel(
        fileInput("log_dir",
                     h6("Select log file location"),
                    accept = c("text/csv",
                               "csv", ".csv")
           ),
           fileInput("file_dir",
                     h6("Select file to monitor")),

        selectizeInput('period', label = h6('Period'), choices = c('minute', 'hour', 'day', 'week'),
                       selected = "hour", multiple = FALSE),
         numericInput("period_count", h6("Period Units"), value = 1,
                       min = 1, max = 1000),
          actionButton("log", "Start Log")
           #)
      ),

      mainPanel(
         plotOutput("progress")
      )
   )
)


server <- function(input, output) {

  timer_init <- reactiveValues(period_pos = NULL, wait = NULL)
  plot_log <- reactiveValues(plotted = FALSE, count = 0, log_plot = ggplot())

  observeEvent(input$log > 0, {
    # timer <- input$period*input$period_count
    logfile <- input$log_dir$datapath
    if (file.exists(logfile)){
      last_entry <- unlist(strsplit(tail(readLines(logfile, warn = FALSE), n = 1), split = ','))
      last_time <- as.POSIXct(as.numeric(last_entry[length(last_entry)]), origin = "1970-01-01")
    }else{
      last_time <- NA
    }

    if (input$period == 'minute'){
      period_pos <- 1*60*0.2
    }else if(input$period == 'hour'){
      period_pos <- 60*60
    }
    else if (input$period == 'day'){
      period_pos <- 24*60*60
    }else if(input$period == 'week'){
      period_pos <- 7*24*60*60
    }

    period_pos <- period_pos*input$period_count

    if (is.na(last_time) || (Sys.time() > (last_time + period_pos))){
      timer_init$period_pos <- period_pos
      timer_init$wait <- 0
    }else{
      timer_init$period_pos <- period_pos
      timer_init$wait <- (last_time + period_pos) - Sys.time()
    }
    
    message(timer_init$wait)

  }, ignoreInit = TRUE)


  # observeEvent(!is.null(timer_init$wait) && timer_init$wait > 5, {
  #   Sys.sleep(timer_init$wait)
  #   timer_init$wait <- 5
  # }, ignoreInit = TRUE)
  # 
  # observeEvent(!is.null(timer_init$wait) && timer_init$wait <= 5, {
  #   logged <- log_stats(file = input$file_dir$datapath, logfile = input$log_dir$datapath, period = input$period, period_count = input$period_count)
  #   if (logged){
  # 
  #     log_file <- read_csv(input$log_dir$datapath, col_names = FALSE)
  #     colnames(log_file) <- c('lines', 'chars', 'date')
  #     plot_log$log_plot <- ggplot(data = log_file, aes(x = date, y = as.numeric(chars))) +
  #       geom_line(colour = 'red')
  # 
  #     #
  #     plot_log$count <- plot_log$count + 1
  #     plot_log$plotted <- TRUE
  # 
  # 
  # 
  # 
  #     #Sys.sleep(10)
  # 
  #     # timer_init$wait <- timer_init$period_pos
  # 
  # 
  # 
  #   }
  # }, ignoreInit = TRUE)


  #progress_plot <- eventReactive(plot_log$count, {
  #  return(plot_log$log_plot)
  #}, ignoreInit = TRUE)

  # observeEvent(input$progress$data$x, {
  #   message("Yes")
  #   message(input$progress$outputId$x)
  #   timer_init$wait <- timer_init$period_pos
  #   plot_log$plotted <- FALSE
  # }, ignoreInit = TRUE)
  
  log_graph <- observe({
    invalidateLater(5000)
    if (!is.null(input$log_dir) && !is.null(input$file_dir) && input$log > 0){
      if (!is.null(isolate(timer_init$wait)) && isolate(timer_init$wait) == 0){
        message('hey')
        log_file <- read_csv(input$log_dir$datapath, col_names = FALSE)
        colnames(log_file) <- c('lines', 'chars', 'date')
        #message(input$progress$outputId)
        plot_log$log_plot <- ggplot(data = log_file, aes(x = date, y = as.numeric(chars))) +
          geom_line(colour = 'red')
      }else{
        message('sup')
        plot_log$log_plot <- ggplot()
      }
      
      
      # output$progress <- renderPlot({
      #   message('awe')  
      #   plot_log$log_plot
      #   message('awe2')
      #   })
      
      plot_log$plotted = TRUE
      timer_init$wait = timer_init$period_pos
      Sys.sleep(5)
      timer_init$wait = 0
      # Sys.sleep(timer_init$wait)
      plot_log$plotted = FALSE
      message(plot_log$plotted)
    }
  })
  
  # observeEvent(plot_log$plotted == FALSE, {
  #   message("check")
  #   timer_init$wait = 0
  # }, ignoreInit = TRUE)
  
  output$progress <- renderPlot({
    invalidateLater(10000)
    return(isolate(plot_log$log_plot))
  })


  # output$progress <- renderPlot({
  #   assign('timer_init$wait', timer_init$period_pos, envir = parent.frame())
  #   return(log_graph())
  #   
  # })
  
  # log_graph <- reactive({
  #   message('entered')
  #   if (plot_log$plotted){
  #     message('hey')
  #     log_file <- read_csv(input$log_dir$datapath, col_names = FALSE)
  #     colnames(log_file) <- c('lines', 'chars', 'date')
  #      #message(input$progress$outputId)
  #     return(ggplot(data = log_file, aes(x = date, y = as.numeric(chars))) +
  #       geom_line(colour = 'red'))
  #   }else{
  #     return(ggplot())
  #   }
  # })
  # 
  # 
  # output$progress <- renderPlot({
  #   assign('timer_init$wait', timer_init$period_pos, envir = parent.frame())
  #   return(log_graph())
  #   
  # })

}

shinyApp(ui = ui, server = server)

