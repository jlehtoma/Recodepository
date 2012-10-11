# Author: Joona Lehtomäki <joona.lehtomaki@gmail.com>
# Updated: 23.09.2011
# Version: 0.1

if (!require("stringr")) {
  install.packages("stringr")
}

# Criteria
#
# Under Section V (the criteria for Critically Endangered, Endangered and 
# Vulnerable) there is a hierarchical alphanumeric numbering system of criteria 
# and subcriteria. These criteria and subcriteria (all three levels) form an 
# integral part of the Red List assessment and all those that result in the 
# assignment of a threatened category must be specified after the Category. 
# Under the criteria A to C and D under Vulnerable, the first level of the 
# hierarchy is indicated by the use of numbers (1-4) and if more than one is 
# met, they are separated by means of the '+' symbol. The second level is 
# indicated by the use of the lower-case alphabet characters (a-e). These are 
# listed without any punctuation. A third level of the hierarchy under Criteria 
# B and C involves the use of lower case roman numerals (i-v). These are placed 
# in parentheses (with no space between the preceding alphabet character and 
# start of the parenthesis) and separated by the use of commas if more than one 
# is listed. Where more than one criterion is met, they should be separated by 
# semicolons.

# Criteria string needs to be broken into individual components which are 
# compiled into expanded comma-separated vectors. For example 
# A1c+3c; B1ab(iii) would be 
# 
# c("A,1,c", "A,3,c", "B,1,a", "B,1,b,iii")

parse.criteria  <- function(str) {
  
  # Create regex templates used for different hierarchy levels
  template  <- list("level1"="[[:upper:]]{1}", "level2"="[[:digit:]]{1}", 
                    "level3"="[abcde]{1}", "level4"="[iv]+")
  # Create an empty vector to hold the actual values
  criteria  <- c()
  
  # Separate criteria if several are available
  criteria.strings <- unlist(strsplit(str, ";"))
  # Get rid of white space
  criteria.strings <- str_trim(criteria.strings)
  
  for (level1.string in criteria.strings) {
    current.criteria<- NULL
    
    # Separate level 2 tokens with '+'
    level2.strings  <- unlist(strsplit(level1.string, "\\+"))
    
    level1.token<- NULL
    
    for (string in level2.strings){
      level2.token <- NULL
      level3.token <- NULL
      level4.token <- NULL
      
      # Loop through all characters in the string  
      i <- 1
      while (i <= str_length(string)) {
        
        # Get the next charcter and see what it is
        token  <- str_sub(string, i, i)
        
        #browser()
        # TODO: checks for only 1 level 1 and 2 tokens
        if (grepl(template$level1, token)) {
          
          level1.token  <- token
        } else if (grepl(template$level2, token) && !is.null(level1.token)) {
          level2.token  <- token
        } else if (grepl(template$level3, token) && !is.null(level1.token) && !is.null(level2.token)) {
          # Check if previous character was a level2 variable -> if yes, 
          # start building the current criteria String
          #browser()
          prev.char <- str_sub(string, (i-1), (i-1))
          next.char <- ""
          if (i < length(string)) {
            next.char <- str_sub(string, (i+1), (i+1))
          }
          if (grepl(template$level2, prev.char)) {
            level3.token <- token
          } else if (grepl(template$level3, prev.char) || prev.char == ")") {
            # If the previous character was also a level3 variable it means
            # that there will be no more level4 variables 
            # -> current.criteria is complete, start building a new one
            #browser()
            if (prev.char != ")") {
              criteria <- c(criteria, paste(level1.token, level2.token, level3.token, 
                                        sep=""))
            }
            level3.token <- token
          }
        } else if (token == "("){
          
          # When we hit the "(" character we know we've hit the level4 
          # chracater. Get the whole content within the parenthesis
          level4.token <-  NULL
          while (token != ")") {
            prev.char <- str_sub(string, i, i)
            i = i + 1
            #browser()
            token <- str_sub(string, i, i)
            if (grepl(template$level4, token)) {
              if (grepl(template$level4, prev.char)) {
                level4.token <- paste(level4.token, token, sep="") 
              } else {
                level4.token <- token
              }
            } else if (token == ",") {
              current.criteria <- paste(level1.token, level2.token,
                                        level3.token, level4.token,
                                        sep="")
              if (!(current.criteria %in% criteria)) {
                criteria <- c(criteria, current.criteria)
              }
            }
          }
          #browser()
          current.criteria <- paste(level1.token, level2.token,
                                    level3.token, level4.token,
                                    sep="")
          
          if (!(current.criteria %in% criteria) ) {
            criteria <- c(criteria, current.criteria)
          }
        }
        i = i + 1
      }
      
      current.criteria <- paste(level1.token, level2.token,
                                level3.token, level4.token,
                                sep="")
      if (!(current.criteria %in% criteria)) {
        criteria <- c(criteria, current.criteria)
      }
    }
  }
  return(criteria)
}

criteria.count <- function(data.parsed) {
  # Unpack the criteria arrays to a new data frame
  df.parsed <- as.data.frame(count(unlist(data.parsed$parsed.criteria)))
  return(df.parsed)
}

parse.habitat <- function(x) {
  
  # Count the number of characters in the string to be parsed 
  n <- nchar(x)
  
  # Auxillary identifiers
  aux <- c("v", "h", "p")

  level1 <- NULL
  level2 <- NULL
  level3 <- NULL
  
  check.upper <- function(y) {
    if (!y == toupper(y)) {
      warning("There is only 1 charcter in the token, but it is not in upper case")  
    }
    return(y)
  }
  
  # If the length is 1, the string should be first level code
  if (n == 1) {
    # Check if the charcter is upper case
    check.upper(x)
    level1 <- x
  } else {
    chars <- unlist(str_split(x, ""))[1:nchar(x)+1]
    
    for (i in 1:length(chars)) {
      if (i == 1) {
        check.upper(x)
        level1 <- x
      } else {
        
      }
    }
        
  }
  
}

## TEST

str <- "B1ab(i,ii,v)c(iii,iv)+2b(i)c(ii,v)"
str2 <- "A2c+3c; B1ab(iii)"
str3 <- "D2"

cc  <- parse.criteria(str3)