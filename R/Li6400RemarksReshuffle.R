FillForward <- function(x) {
  #' Fill NA elements in a vector using the last non-NA entry without using \code{zoo:na.locf} 
  #'
  #' Fills NA elements in a vector using the last non-NA element preceeding each missing element. Follows \href{https://stat.ethz.ch/pipermail/r-help/2008-July/169195.html}{this suggestion}.
  #'
  #' @param x Vector with NA entries to be filled
  #' @return Returns gap filled vector
  #' @export
  for(i in seq_along(x)[-1]) if(is.na(x[i])) x[i] <- x[i-1]
  return(x)
}

Li6400RemarkReshuffle <- function(remark) {
  #' Re-organise remarks from a Licor 6400 IRGA measurement file previously processed via \code{\link{Li6400Import}}
  #'
  #' This is a rather simplistic approach to processing the between-observations remarks created by the Li6400 IRGA. Whether this approach makes sense is up to the specific use case. Assuming a block of samples (observations, Obs) is always preceeded by a remark, this approach works as the last remark is carried over to all following observations until a new remark is found. But as there can be many more remarks in file than samples and the last remark might not be the correct remark for a block of samples, the merge between the actual sample observations and remarks needs further processing afterwards. For convenience, the orginal remarks vector with gaps and the filled remarks are both provided in the output. Uses \code{\link{FillForward}}.
  #'
  #' @param remark Data.frame with the remarks as created by the function \code{\link{Li6400Import}}
  #' @return Data.frame with a new vector ForwardFilledRemarks in which gaps between remarks are filled with the last, previous remark.
  #' @seealso \code{\link{Li6400Import}}
  #' @export
  
  stopifnot(names(remark) %in% c("Remarks", "RemarkRow"))

  # create a continuous sequence from first to last remark row
  full.seq <- seq(from = min(remark$RemarkRow), to = max(remark$RemarkRow), by = 1)

  # merge full sequence with remarks
  r <- merge(as.data.frame(full.seq), remark,
           by.x = "full.seq",
           by.y = "RemarkRow",
           all.x = TRUE)
  names(r) <- gsub("full.seq", "Row", names(r))
  r$ForwardFilledRemarks <- FillForward(r$Remarks)
  return(r)
}
