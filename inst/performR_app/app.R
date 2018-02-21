library(shiny)
library(shinyFiles)
library(tidyverse)

log_stats <- function(file = getwd(), logfile = paste0(getwd(), '/logfile.csv'), period = 'hour', period_count = 1){

  if (file.exists(logfile)){
    last_entry <- unlist(strsplit(tail(readLines(logfile, warn = FALSE), n = 1), split = ','))
    last_time <- as.POSIXct(as.numeric(last_entry[length(last_entry)]), origin = "1970-01-01")
  }else{
    last_time <- NA
  }

  if (period == 'second'){
    period_pos <- 1
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
    
    message(last_entry[1])
    
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
        shinyFilesButton('log_dir', 
                         "Log file", 
                         h6("Select log file location"), 
                         multiple = FALSE), 
        shinyFilesButton('file_dir', 
                         "Track file", 
                         h6("Select a file to monitor"), 
                         multiple = FALSE), 
        # fileInput("log_dir",
        #              h6("Select log file location"),
        #             accept = c("text/csv",
        #                        "csv", ".csv")
        #   ),
           # fileInput("file_dir",
           #           h6("Select file to monitor")),

        selectizeInput('period', label = h6('Period'), choices = c('second', 'minute', 'hour', 'day', 'week'),
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
  files <- reactiveValues(file = NULL, log = NULL)
  periods <- reactiveValues(period = NULL, period_count = NULL)
  plot_log <- reactiveValues(plotted = FALSE, count = 0, log_plot = ggplot())
  
  sub <- gregexpr("/", dirname(getwd()), fixed = TRUE)[[1]]
  # sub <- unlist(list(0, sub))
  hier_count <- (length(sub)):1
  folders = lapply(1:(length(sub)-1), function(x) 
    paste0(rep(c('..'), times = hier_count[x]), collapse = '/'))
  
  folders_names <- sapply(1:(length(sub)-1), function(x) substr(dirname(getwd()), start = sub[1] + 1, stop = sub[x+1] - 1))
  
  names(folders) <- folders_names
  
  roots = c(folders, wd = '.')
  # roots = c(wd = '.')
  
  # roots = c(wd = '..')
  shinyFileChoose(input, 'log_dir', roots = roots)
  shinyFileChoose(input, 'file_dir', roots = roots)
  
  
  
  observeEvent(input$log > 0, {
    if (input$period == 'second'){
      period_pos <- 1
    }else if (input$period == 'minute'){
      period_pos <- 1*60
    }else if(input$period == 'hour'){
      period_pos <- 60*60
    }
    else if (input$period == 'day'){
      period_pos <- 24*60*60
    }else if(input$period == 'week'){
      period_pos <- 7*24*60*60
    }
    timer_init$period_pos <- period_pos*input$period_count
    timer_init$wait <- isolate(timer_init$period_pos)
    
    # files$file <- paste0(input$file_dir)
    # files$log <- paste0(input$log_dir)
    
    files$file <- paste0(substr(dirname(getwd()), 1, sub[1] - 1), '/', input$file_dir[[2]], '/', paste0(unlist(input$file_dir[[1]][[1]][2:3]), collapse = '/'))
    files$log <- paste0(substr(dirname(getwd()), 1, sub[1] - 1), '/', input$log_dir[[2]], '/', paste0(unlist(input$log_dir[[1]][[1]][2:3]), collapse = '/'))
    
    # files$file <- input$file_dir$datapath
    # files$log <- input$log_dir$datapath
    
    periods$period <- input$period
    periods$period_count <- input$period_count
    
  })



  
  log_graph <- observe({
    
    if (input$log > 0){
      invalidateLater(isolate(timer_init$wait)*1000)
      
      
      message("test")
      message(files$file)
      message(files$log)
      message(periods$period)
      message(periods$period_count)
      
      environment(log_stats) <- globalenv()
      
      update <- log_stats(file = files$file, logfile = files$log, 
                period = periods$period, period_count = periods$period_count)
      
      if (update){
        log_file <- read_csv(files$log, col_names = FALSE)
        colnames(log_file) <- c('lines', 'chars', 'date')
        plot_log$log_plot <- ggplot(data = log_file, aes(x = date, y = as.numeric(chars))) +
            geom_line(colour = 'red')
        plot_log$plotted <- TRUE
      }
    }
    

  })

  
  output$progress <- renderPlot({
    plot_log$log_plot
    #invalidateLater(5000)
    #return(isolate(plot_log$log_plot))
  })

}

shinyApp(ui = ui, server = server)

