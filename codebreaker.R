library(cli)
library(beepr)

sprite_show <- function(txt)  {
  for (i in 1:nchar(txt)) {
    char <- substr(txt, i, i)
    if(char == "R") {cat(bg_red(" "))}
    if(char == "B") {cat(bg_blue(" "))}
    if(char == "C") {cat(bg_cyan(" "))}
    if(char == "G") {cat(bg_green(" "))}
    if(char == "M") {cat(bg_magenta(" "))}
    if(char == "Y") {cat(bg_yellow(" "))}
    if(char == "W") {cat(bg_white(" "))}
    if(char == "X") {cat(bg_black(" "))}
    if(char == ".") {cat(" ")}
    if(!char %in% c("R","B","C","G","M","Y","W","Y","W","X",".")) {cat(char)}
  }   
} # sprite_show() 


cb_intro <- function(name = NULL) {
  
  sprite1 <- paste0(
    ".......YYYY..YYYY..YYYYY..YYYYY.", "\n",
    "......YY....YY..YY.YY..YY.YY....", "\n",
    "......YY....YY..YY.YY  YY.YYYY..", "\n",
    "......YY... YY..YY.YY..YY.YY....", "\n",
    ".......YYYY..YYYY..YYYYY..YYYYY.")
  
  sprite2 <- paste0(
    "BBBB..CCCC.CCCC..CC..C..C.CCCC.RRRR..","\n",
    "B .BB.C..C.C....C..C.C.C..C....R..R..","\n",
    "BBBB..CCCC.CCC..CCCC.CC...CCC..RRRR..","\n",
    "B .BB.C.C..C....C..C.C.C..C....R.R...","\n",
    "BBBB..C..C.CCCC.C..C.C..C.CCCC.R..R..","\n")
  
  cat("\n")
  sprite_show(sprite1)
  cat("\n  Master Logic to Break the Code!\n")
  sprite_show(sprite2)
  cat("\n")
  cat("\n")
  
} # cb_intro()

cb_success <- function(name = NULL) {
  
  txt <- paste0(
    "..............YYYY.", "\n",
    ".............YY..YY", "\n",
    "...YYYYYYYYYYYY..YY", "\n",
    "...Y.Y.......YY..YY", "\n",
    "...Y.Y........YYYY.", "\n")
  
  cat("\n\n")
  sprite_show(txt)
  cat("   C o n g r a t s!")
  cat("\n")
  cat("\n")
  
} # cb_intro()

cb_show_color <- function(color)  {

  if (color == "R") { cat(bg_red(" R ")) }
  if (color == "B") { cat(bg_blue(" B ")) }
  if (color == "G") { cat(bg_green(" G ")) }
  if (color == "Y") { cat(bg_yellow(" Y ")) }
  if (color == "M") { cat(bg_magenta(" M ")) }
  if (color == "X") { cat(" X ") }
  
} # cb_show_color()

cb_select_colors <- function(name = NULL) {

  colors <- readline(prompt = "How many colors (2-5) ? ")
  colors <- suppressWarnings(as.integer(colors))
  
  if (is.na(colors) | colors < 2 | colors > 5) {
    cat("Selected default (2 colors)")
    colors <- 2
  }
  
  color_list <- NULL
  
  cat("\n")
  cat("Colors: ")
  if (colors >= 2) {
    cat(bg_blue(" B "))
    cat("lue ")
    color_list <- c(color_list, "B")
  }
  if (colors >= 1) {
    cat(bg_red(" R "))
    cat("ed ")
    color_list <- c(color_list, "R")
  }
  if (colors >= 3) {
    cat(bg_green(" G "))
    cat("reen ")
    color_list <- c(color_list, "G")
  }
  if (colors >= 4) {
    cat(bg_yellow(" Y "))
    cat("ellow ")
    color_list <- c(color_list, "Y")
  }
  if (colors >= 5) {
    cat(bg_magenta(" M "))
    cat("agenta ")
    color_list <- c(color_list, "M")
  }

  color_count <- colors
  color_chars <- paste(color_list, collapse = "")
  
  return(list(color_count = color_count, 
              color_chars = color_chars, 
              color_list = color_list))  
}

cb_code2vector <- function(code) {
  
  code_vector <- strsplit(code, split="",fixed = TRUE)[[1]]

  return(code_vector)
  
} # cb_code2vector()


cb_check_code <- function(code_check, code_secret)  {

  # code empty?
  if (nchar(code_check) == 0) {
    return(0)
  }
    
  # check for correct position + color
  pattern_check <- cb_code2vector(code_check)
  pattern_secret <- cb_code2vector(code_secret)
  
  check_correct <- 0
  for (i in 1:nchar(code_check))  {
    if (substr(code_check,i,i) == substr(code_secret,i,i)) {
      pattern_check[i] <- "."
      pattern_secret[i] <- "."
      check_correct <- check_correct + 1
    }
  }
  
  #pattern_check
  #pattern_secret
  
  # check for correct color
  
  check_color <- 0
  
  for (i in 1:length(pattern_check))  {
    
    char <- pattern_check[i]
    #cat(char, ": ")
    
    if (char != "." & char != "X") {
      
      color_match <- match(char, pattern_secret)
      #cat(pattern_check, " | ", pattern_secret, " | ")
      #cat(color_match)
      #cat("\n")
      
      if (!is.na(color_match[1])) {
        check_color <- check_color + 1
        pattern_check[i] <- "."
        pattern_secret[color_match[1]] <- "."
        
      } # if
      
    } # if
    
  } # for
      
  return(list(all = check_correct, color = check_color))
}

cb_clean_code <- function(code, code_length = 4)  {

  # convert into string (if necessary)
  if (is.vector(code)) {
    code <- paste(code, collapse = "")
  }

  # Upper Case
  code <- toupper(code)

  # check if EXIT
  if (code == "EXIT")  {
    return("EXIT")
  }
  
  # clean code
  code_clean <- ""
  for (i in 1:nchar(code)) {
    char <- substr(code, i, i)
    if (char %in% c("R","B","G","Y","M","X")) {
      code_clean <- paste0(code_clean, char) 
    }
  }  

  code_clean <- substr(code_clean, 1, code_length)
  
  return(code_clean)
}

cb_show_code <- function(code) {
  
  if (is.vector(code)) {
    code <- paste(code, collapse = "")
  }
  
  for (i in 1:nchar(code)) {
    char <- substr(code, i, i)
    if (char %in% c("R","B","G","Y","M","X")) {
      cb_show_color(char)
      cat(" ")
    }
  }  
  
} 

cb_input_code <- function(step = 1, code_length = 4, color_list = c("R", "B"))  {
  
  prompt <- paste0("Your code (you can use ",
                   paste(color_list, collapse=" "),
                    " X) : ")
  code <- readline(prompt = prompt)
  code <- cb_clean_code(code, code_length)
  
  if (code != "EXIT") {
  
    cat(col_silver("Try"), ifelse(step<10, paste0("0",step),step))
    cat(col_silver(": "))
    cb_show_code(code)

  } # if
  
  return(code)
  
}

codebreaker <- function(sound = TRUE, name = NULL)  {

  cb_intro()
  if (sound) {beep("fanfare")}
  setup <- cb_select_colors()

  cat("\n")
  cat("The code consists of 4 letters/colors (e.g. B R R B)\n")
  cat("Try to find it! (Enter exit to stop)\n")

  secret_code <- sample(setup$color_list, 4, replace = TRUE)
  secret_code <- paste(secret_code, collapse = "") 
  #cat("Secret code =",secret_code)

  try = 1
  game_over <- FALSE
    
  while(!game_over) {
    
    code <- cb_input_code(try, 4, setup$color_list)
    
    if (code == "EXIT") {
      cat("\n")
      cat("Game stopped!")
      break
    }
    
    correct <- cb_check_code(code, secret_code)
    cat(col_silver("correct:"), correct$all)
    cat(col_silver(" (color only:"),correct$color)
    cat(col_silver(")"))
    if (sound) {beep("ping")}
    
    if (correct$all >= 4)  {
      game_over <- TRUE
      cb_success()
      if (sound) {beep("mario")}
    } else {
      try <- try + 1
    }

  } # while
  
} # codebreaker()

codebreaker(sound = TRUE)
