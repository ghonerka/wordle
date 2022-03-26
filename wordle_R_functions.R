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

