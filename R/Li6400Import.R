#' Imports Licor 6400 photosynthesis system output files.
#' 
#' @param file Filename of the Li6400 text file (usually .csv or .tsv)
#' @param sep Character string to identify columns in the Li6400 file. Default is "`\t`" for tab-separated. "," for comma-separated.
#' @return Data.frame with the imported file.
#' @export

Li6400Import <- function(file, sep = "\t") {

  x <- readLines(file)
  
  # add the date to the HHMMSS vector - by default the file only has the time of day
  # grab date from second row in file
  the.date <- x[2]
  the.date <- gsub("\\\"", "",  the.date)

  # Licor uses non standard weekday names!
  the.date <- gsub("Thr", "Thu", the.date)
  
  the.date <- as.POSIXct(the.date,
               format = "%a %b %d %Y %H:%M:%S")

  # look for phrase "$STARTOFDATA$" to get row after which import should start
  start.data <- grep("STARTOFDATA", x)
  
  y <- utils::read.csv(file, 
                skip = start.data,
                #sep = "\t",
                sep = sep,
                na.strings = c("NA", ""))

  # assemble full date based on date in file header, as Li6400 only records time in the HHMMSS field
  the.day <- format(the.date, "%Y-%m-%d")
  y$HHMMSS <- as.character(y$HHMMSS)
  y$HHMMSS[!is.na(y$HHMMSS)] <- paste(the.day, y$HHMMSS[!is.na(y$HHMMSS)], sep = " ")
  
  y$HHMMSS <- as.POSIXct(y$HHMMSS)
  
  # move Remarks out of the way, they are provided separately
  remarks <- y[is.na(y$FTime), ]
  y <- y[!is.na(y$FTime), ]
  out <- list(data = y,
              remarks = remarks)
  return(out)
}


#the.folder <- "~/Swinburne/Licor/light_response_glasshouse"
#the.file <- "2019-02-18_Blue_lightresponse.csv"

#the.file <- paste(the.folder, the.file, sep = "/")

#x <- Li6400Import(the.file)


