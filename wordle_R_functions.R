# Some functions for playing Wordle





# wordle1 -----------------------------------------------------------------

# ARGUMENTS
#   letters_known:    A vector of letters known to appear in the word.
#   positions_known:  A list of vectors of known positions corresponding 
#                     to the known letters.
# OUTPUT
#   A vector of all possible words with known letters inserted at known
#   positions, with '-' inserted at unknown positions.
# 
# USAGE
#   letters_known <- c("P", "O", "E")
#   positions_known <- list(c(2, 5), c(3), c(1, 2))
#   wordle1(letters_known, positions_known)

wordle1 <- function(letters_known, positions_known) {
  # Create sequences of potential positions of known letters
  dd <- as.matrix(expand.grid(positions_known))
  
  # Remove sequences where positions collide
  repeated_positions <- apply(dd, MARGIN = 1, FUN = \(x) length(unique(x)) < length(x))
  dd <- dd[!repeated_positions, ]
  
  # Create words from the positions
  guess_word <- function(positions, letters) {
    guess <- rep("-", 5)
    for(i in seq_along(positions)) guess[positions[i]] <- letters[i]
    return(paste(guess, collapse = ""))
  }
  apply(dd, MARGIN = 1, FUN = guess_word, letters = letters_known)
}





wordle2 <- function(gray, yellow, green) {
  require(dplyr)
  
  ##### Green Hints
  
  # All possible permutations that conform to the green hints
  
  all_words <- green |> 
    lapply(FUN = \(x) if (x == "") LETTERS else x) |> 
    do.call(what = expand.grid)
  
  
  
  ##### Gray Hints
  
  # Remove words that contain any gray letters
  no_gray <- all_words |> 
    filter(!if_any(.fns = ~ .x %in% gray))
  
  
  
  ##### Yellow Hints
  
  # Remove words that have letters coinciding with the yellow letters
  remove_yellow <- no_gray |> 
    filter(
      !(L1 %in% yellow$`1`), 
      !(L2 %in% yellow$`2`), 
      !(L3 %in% yellow$`3`), 
      !(L4 %in% yellow$`4`), 
      !(L5 %in% yellow$`5`))
  
  # Require that words contain all yellow letters
  unique_yellow <- unlist(yellow) |> 
    unique() |> 
    setdiff(y = "")
  has_yellow <- remove_yellow |> 
    rowwise() |> 
    filter(all(unique_yellow %in% c(L1, L2, L3, L4, L5))) |> 
    ungroup()
  
  
  
  ##### Print valid words
  
  # Require that words agree with green hints
  valid_words <- has_yellow |> 
    mutate(word = paste0(L1, L2, L3, L4, L5)) |> 
    pull(word)
  
  return(valid_words)
}
