#' Remove or pull attributes (e.g. labels)
#'
#' Often when Stata .dta files are imported into R with the haven package, they contain variable and value labels.  
#' While these metadata are useful, if they are incomplete, they often have annoying downstream repercussions.
#' For instance, ggplot can't facet if there are value labels.
#' 
#' These functions allow you to save all labels and levels from a data frame, and to remove them.
#' 
#' @name attr
NULL
# > NULL
#' @export
#' 
#' @param data imported data frame
#' @param type for pullAttributes, type of metadata to pull (label or labels)


#' @describeIn  attr Function to pull any attributes from a data frame.  Useful to cleanup imported Stata files.
#' @export
pullAttributes <- function(data) {
  
  metadata = lapply(data, function(x) attr(x, 'label'))
  metadata = data.frame(metadata)
  
  labels = lapply(data, function(x) attr(x, 'labels'))

  metadata = data.frame(var = colnames(metadata), varDescrip = t(metadata))

  df = mutate(metadata, varValues = labels)
  return(df)
}



#' @describeIn  attr Function to remove any attributes from a data frame.  Useful to cleanup imported Stata files.
#' @export
removeAttributes <- function(data) {
  data <- lapply(data, function(x) {
    attr(x, "labels") <- NULL
    x
  })
  data <- lapply(data, function(x) {
    attr(x, "label") <- NULL
    x
  })
  data <- lapply(data, function(x) {
    attr(x, "class") <- NULL
    x
  })
  data <- lapply(data, function(x) {
    attr(x, "levels") <- NULL
    x
  })
  data = data.frame(data)
}
