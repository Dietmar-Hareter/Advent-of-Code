setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data_input <- readLines('input_day07_Test.txt')
data_input <- readLines('input_day07.txt')


# Part 1

Is_valid <- function(Target, Total, Series_in) {
  if (length(Series_in) == 1) {
    if ((Total + Series_in == Target) | (Total * Series_in == Target)) {
      return(1)
    } else {
      return(0)
    }
  } else {
    return(Is_valid(Target, Total * Series_in[1], Series_in[-1]) | Is_valid(Target, Total + Series_in[1], Series_in[-1]))
  }
}

Valid <- 0
for (Lauf in 1:length(data_input)) {
  Temp <- unlist(stringr::str_split(data_input[Lauf], ":"))
  Target <- as.numeric(Temp[1])
  Series <- as.numeric(unlist(stringr::str_extract_all(Temp[2], "\\d+")))
  if (Is_valid(Target, Series[1], Series[-1]) == 1) {
    Valid <- Valid + Target
  }
}
options(scipen = 10)
print(as.character(Valid)) # Part 1: 3312271365652


# Part 2

Is_valid_Comp <- function(Target, Total, Series_in) {
  if (length(Series_in) == 1) {
    if ((Total + Series_in == Target) | (Total * Series_in == Target) | (as.numeric(paste0(Total, Series_in)) == Target)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(Is_valid_Comp(Target, Total * Series_in[1], Series_in[-1]) | 
             Is_valid_Comp(Target, Total + Series_in[1], Series_in[-1]) | 
             Is_valid_Comp(Target, as.numeric(paste0(Total, Series_in[1])), Series_in[-1]))
  }
}

Valid_Comp <- 0
for (Lauf in 1:length(data_input)) {
  if (Lauf %% 10 == 0) {
    print(paste0(Lauf, " von ", length(data_input)))
  }
  Temp <- unlist(stringr::str_split(data_input[Lauf], ":"))
  Target <- as.numeric(Temp[1])
  Series <- as.numeric(unlist(stringr::str_extract_all(Temp[2], "\\d+")))
  if (Is_valid_Comp(Target, Series[1], Series[-1]) == 1) {
    Valid_Comp <- Valid_Comp + Target
  }
}
print(as.character(Valid_Comp)) # Part 2: 509463489296712
