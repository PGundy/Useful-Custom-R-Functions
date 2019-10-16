
# Custom Functions --------------------------------------------------------

###### Ifelse Fix to Preserve Class
ifelseC <- function(cond, yes, no) {
  structure(ifelse(cond, yes, no), class = class(yes))
} # fix issue where ifelse usually strips classes
print("loaded: ifelseC -- maintains variable class through ifelse")


###### Fill NA function that respects date class
fillNA <- function(x) { # repeats the last non NA value. Keeps leading NA
  #NOTE -- tidyr has the fill command which is faster, but this keeps date class
  ind <- which(!is.na(x)) # get positions of nonmissing values
  if (is.na(x[1])) { # if it begins with a missing, add the
    ind <- c(1, ind) # first position to the indices
  }
  rep(x[ind], times = diff( # repeat the values at these indices
    c(ind, length(x) + 1) # diffing the indices + length yields how often
  ))
}
print("loaded: fillNA -- fills NA down maintaining variable class")


###### Combination of table(is.na(x))
tableNA <- function(x) {
  table(is.na(x)) #observe the number of NAs
}
print("loaded: tableNA -- wraper for table(is.na(x))")


###### A unique in unique list table summary of table(unique(x) %in% unique(y))
TableUniqueInUnique <- function(x, y) {
  table(unique(x) %in% unique(y))
}
print("loaded: TableUniqueInUnique -- checks unique values within a list of unique values ")


###### A percentage version of table(x)
tablePerc <- function(x){
  round(prop.table(table(x))*100, 2) #A % breakdown of a table
}
print("loaded: tablePerc - table() using percentages")


###### inverted str_subset - remove anything containing the pattern
str_subset_inv <- function(vec, pattern) {
  vec[!stringr::str_detect(vec, pattern)] #simple inverse of str_subset to remove things
}
print("loaded: str_subset_inv -- removes things with matching pattern")



###### fwrite.DF.to.csv.as.char - Ungroup object, everything as strings, and exports object to file
fwrite.DF.to.csv.as.char <- function(DF, file.path.and.file.name){
  DF %>% dplyr::ungroup() %>%
    dplyr::mutate_all(as.character) %>% 
    data.table::fwrite(., file.path.and.file.name)
  
  print(paste( paste0("[",deparse(quote(DF)), "]"),           #Data object
               "has been saved here:",                        # 
               paste0("[", file.path.and.file.name, "]") ))   #file.path.and.file.name
}
print("loaded: fwrite.DF.to.csv.as.char -- ungroup(), as.character(), and export as csv")





# ************** ----------------------------------------------------------------
# * Finished Command ----------------------------------------------------------------



Function.File <- (function() { attr(body(sys.function()), "srcfile")})()$filename
Function.File

print(paste("All functions loaded from:", Function.File))

