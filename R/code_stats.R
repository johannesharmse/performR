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

    return(stats)

  }else{
    message(paste0(period_count, ' ', period, '(s) has not yet passed since last log (', as.character(last_time), ')'))
  }


}

# key_stats(file = 'code_stats.R', logfile = 'code_log.csv', period = 'minute', period_count = 1)


# file.mtime(file) # checks last modified/saved time - could be useful to check any activity
# file.info(file) # really useful for activity - can check last access time - whether any activity has taken place; last modified time (saved).

# Sys.getenv() # can see a lot about the script environment/profile. But maybe not useful. Should see this function as a type of commit/save button.

#FILES=PWD/*
#for f in $FILES
#do

# file_update = c("FILES=PWD/* && inotifywait -m /FILES -e create -e moved_to | && while read  action file; do
#         if [[ \"$file\" =~ .*R$ ]]; then # Does the file end with .xml?
#             echo \"xml file\" # If so, do your thing here!
#         fi
#     done")

# file_update = c("path=PWD && inotifywait -m /path -e create -e moved_to |
#     while read path action file; do
#         echo \"The file '$file' appeared in directory '$path' via '$action'\"
#         # do something with the file
#     done")




