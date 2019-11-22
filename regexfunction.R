###########################################################################
# Regex Builder
# Author: Nellie Ponarul
# Last Updated: 11/22/2019
# This file has functions necessary for building regex
# Dependencies: stringr, purrr
###########################################################################

# I. Program Setup --------------------------------------------------------
require(stringr)
require(purrr)

# II. Functions -----------------------------------------------------------

#This function determines whether or not a string is in a dictionary and if so, returns the definitions
#@param dict is the dictionary (in the form of a list)
#@param chr is the string that is looked up in the dictionary
#@return is the dictionary definition if found and if not is the @param chr
map_regex <- function(dict, chr) {
  ifelse(!(chr %in% names(dict)),chr,dict[[match(chr,names(dict))]])
}

#This function collapses a string vector into one string with elements separated by |
#@param str_vector is a vector of strings
#@return string with original strings separated by |
or <- function(str_vector) {
  return(paste0(str_vector, collapse="|"))
}

#This function escapes all special characters in a string
#@param s is the string
#@return string with escaped special characters
replace_lit <- function(s) {
  p <- str_replace_all(s, "\\.", "[.]")
  p <- str_replace_all(p, "\\*", "[*]")
  p <- str_replace_all(p, "\\+", "[+]")
  p <- str_replace_all(p, "\\|", "[|]")
  p <- str_replace_all(p, "\\[", "[[]")
  p <- str_replace_all(p, "\\]", "[]]")
  p <- str_replace_all(p, "\\^", "[^]")
  p <- str_replace_all(p, "\\(", "[(]")
  p <- str_replace_all(p, "\\)", "[)]")
  p <- str_replace_all(p, "\\$", "[$]")  
  return(p)
}

#This function takes a string description in the appropriate format (defined in documentation) and returns the matching regex pattern
#@param s is the formatted string description
#@return regex expression described in x 
regex_dict <- function(s) {
  #define dictionaries for types, quantities and lookarounds
  types_def <- c("number", "upper", "lower", "alpha", "alphanumeric", "word", "nonword", "space", "notspace", "punctuation","anything")
  types <- list("[0-9]","[A-Z]", "[a-z]", "[A-z]","[A-z0-9]", "\\w", "\\W", "\\s", "\\S", "[[:punct:]]", ".")
  names(types) <- types_def
  quantity_def <- c("1\\+", "0\\+","[0-9]+")
  quantity <- list("+", "*","literal")
  names(quantity) <- quantity_def
  lookarounds_def <-  c("followed by", "not followed by", "preceded by", "not preceded by")
  lookarounds <- list("?=", "?!", "?<=", "?<!")   
  names(lookarounds) <- lookarounds_def
  
  
  parts <- unlist(strsplit(s,';'))
  #map each piece to a translation
  results = c()
  #for each word
  for (p in parts){
    #if string matches (1+|0+|number), (one of the types)
    if(grepl(paste0("(",or(names(quantity)),")",",(", or(names(types)), ")"), p)){
      if(grepl("\\+", p)) {
        # print(map_regex(quantity, paste0(str_extract(p, "[01]"),"\\+")))
        results <- c(results,paste0(map_regex(types,str_extract(p, "(?<=,).+")),map_regex(quantity, paste0(str_extract(p, "[01]"),"\\+"))))
      } else {
        
        #convert to quantity plus type
        # print(p)
        results <- c(results,paste0(map_regex(types,str_extract(p, "[a-z]+")),"{",str_extract(p,"[0-9]+"),"}"))
      }
    # } else if(grepl(paste0("(",or(names(lookarounds)),")",",(", or(names(quantity)), "|.)"), p)){
    } else if(grepl(paste0(".+,(", or(names(lookarounds)), ")"), p)){      
      #add ifelse for lookbehind
      results <- c(results,paste0("(", map_regex(lookarounds,str_extract(p, or(names(lookarounds)))), replace_lit(str_extract(p, ".+(?=,)")), ")"))
    } else if(grepl(".+,literal", p)) {
      lit <- str_extract(p, ".+(?=,literal)")
      lit <- replace_lit(lit) #add escaping to special characters
      results <- c(results, lit)
    }
  }
  return(paste0(results, collapse=""))
}

