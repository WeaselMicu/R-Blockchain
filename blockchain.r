#Example of a first block
block_one <- list(index = 1,
             timestamp = "2018-02-25 16.36 EST",
             data = "my personal info",
             previous_hash = 0,
             proof = 9,
             new_hash = NULL)

#Function that creates a hashed "block"
hash_block <- function(block){
    block$new_hash <- digest(c(block$index,
                               block$timestamp,
                               block$data,
                               block$previous_hash), "sha256")
    return(block)
}

  ### Simple Proof of Work Alogrithm
    proof_of_work <- function(last_proof){
      proof <- last_proof + 1

      # Increment the proof number until a number is found that is divisable by 99 and by the proof of the previous block
      while (!(proof %% 99 == 0 & proof %% last_proof == 0 )){
        proof <- proof + 1
      }

      return(proof)
    }