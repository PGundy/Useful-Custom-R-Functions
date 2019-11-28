
# Custom Functions --------------------------------------------------------


#org.df.names<-names(df)



####### colNamesCleaner
colNamesCleaner<- function(df) {
  
  ##The goal of this is to automate column name cleaning
  
  #Example<-airquality %>% mutate('Easy Example Test'="1", 'Medium..example   Test'="2", 'Hard exampleTEST'="3", 'Truly HARD TestCase  PST'="4", AnotherTESTof5FT2In="5", AnotherTESTof5FTT2In="6", ExampleOf1CommonProblem="7" )
  #colNamesCleaner(Example)
  
  
  
  if (("stringr" %in% installed.packages())==F)
    warning("'string' package not found", call. = FALSE)
  
  if(!is.data.frame(df)) warning("The object is not a data frame", call. = F)
  
  names(df)<-make.unique(names(df))
  names(df)<-stringr::str_replace_all(names(df), "[[:punct:]]", " ")
  names(df)<-stringr::str_replace_all(names(df),
            ## splits on the following cases... (unforunately must be 1 statement)
            ## (1) IF 1 (lower or digit) and next upper THEN insert space b/w
            ## (2) IF 2 or more upper until next lower THEN insert space b/w
            ## (3) IF digit then insert space
            ## (4) IF 1 or more lower until digit then insert space b/w
"(([[:lower:]]|[[:digit:]]){1}(?=[[:upper:]])|[[:upper:]]{2,}(?=([[:lower:]]|[[:digit:]]))|([[:lower:]]){1,}(?=[[:digit:]]))",
              "\\1 ")
  names(df)<-stringr::str_replace_all(names(df), "\\s{1,}", ".")
  names(df)
  
}







####### plotMissingness -- Automatically plot the percentage of NAs per column
plotMissingness<- function(df, Filter.NAs.Out=TRUE, Sort.By.Missingness=TRUE) {
  
  ##The Filter.NAs.Out==TRUE it removes variables with 0 NAs
  
  ## For an example of this function's output try using base R data:
  ##  airquality %>% mutate(New.Ex=NA, New.EX2=NA, Example.NA1=ifelse((Month%/%5)==1, NA, Month), Example.NA2=ifelse((Ozone%/%18)==1, NA, Month)) %>% plotMissingness(., FALSE)

  
  ## Let's verify packages exists
  if (("dplyr" %in% installed.packages())==F)
    warning("'dplyr' package not found", call. = FALSE)
  
  if (("tidyr" %in% installed.packages())==F)
    warning("'tidyr' package not found", call. = FALSE)
  
  if (("ggplot2" %in% installed.packages())==F) 
    warning("'ggplot2' package not found", call. = FALSE)
  
  if (("parallel" %in% installed.packages())==F) 
    warning("'parallel' package not found", call. = FALSE)
  
  ## Lets make sure that tidyr is up to date
  if ((exists('pivot_longer', where='package:tidyr', mode='function'))==FALSE
      ) warning("'tidyr' is out of date, please update to version 1.0+", call. = FALSE)
  
  
  if (missing(df)) warning("`df` is missing", call. = FALSE)
  if (!is.data.frame(df)) warning("`df` class is not data.frame", call. = FALSE)
  

    ##TODO: Allow for grouped variables to be based as facets
    
    df %>%
      parallel::mclapply(., function(y) sum(is.na(y))/length(y) ) %>% 
      parallel::mclapply(., round, 4) %>% 
      as.data.frame() %>% 
      tidyr::pivot_longer(., cols=names(.),
                          names_to = "Variable",
                          values_to = "Percent.of.NAs" ) %>% 
      dplyr::mutate(Org.Order=row_number()) %>%
      dplyr::arrange(., desc(Percent.of.NAs), desc(Variable)) %>% 
      dplyr::mutate(Missingness.Order=row_number()) %>% 
      
      ##TODO: Add piece on soreting tied NA percentages in alphabetic order
      
      #This statement is assuming TRUE which drops 0% NA vars or keeps them if FALSE
      dplyr::filter(if(Filter.NAs.Out){ Percent.of.NAs>0
                    }else{ !is.na(Percent.of.NAs) } ) %>%
      ggplot2::ggplot(data=.,
              ggplot2::aes(if(Sort.By.Missingness){ 
                                 x=reorder(Variable, Missingness.Order)
                          }else{ x=reorder(Variable, Org.Order) },
                          y=Percent.of.NAs,
                          fill=Percent.of.NAs)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::scale_fill_gradient2(low='blue', mid='yellow', high='red', 
                                    midpoint=0.5) +
      ggplot2::scale_y_continuous("Percent of NAs", 
                                  breaks=seq(0, 1, 0.2),
                                  limits = c(0,1.19)) +
      ggplot2::labs(title="Data Missingness, Percentage", x="Variables") +
      ggplot2::geom_text(ggplot2::aes(label=paste0(Percent.of.NAs*100, "%")),
                         hjust=-0.25) +
      ggplot2::coord_flip()
  } # plots % of missing information in each column
print("lodaded: plotMissingness")






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
tableRAW <- function(x) {
  table(paste0(x)) #observe the buckets of a vector including NAs
}
print("loaded: tableRAW -- wraper for table(paste0(x))")



###### Combination of table(is.na(x))
tableNA <- function(x) {
  table(is.na(x)) #observe the number of NAs
}
print("loaded: tableNA -- wraper for table(is.na(x))")



###### A unique in unique list table summary of table(unique(x) %in% unique(y))
TableUniqueInUnique <- function(x, y, Invert=FALSE) {
  if (Invert==FALSE) table(unique(x) %in% unique(y))
  else table(unique(y) %in% unique(x))
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
print("loaded: fwrite.DF.to.csv.as.char -- ungroup, as.character, and export as csv")





# ************** ----------------------------------------------------------------
# * Finished Command ----------------------------------------------------------------



Function.File <- (function() { attr(body(sys.function()), "srcfile")})()$filename
Function.File

print(paste("All functions loaded from:", Function.File))

