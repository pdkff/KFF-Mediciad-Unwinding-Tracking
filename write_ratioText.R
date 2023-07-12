write_ratioText <- function(ratio){

  maximumNumberPeople = 10

  if(ratio < 0.1){
    maximumNumberPeople = 14
  }

  # Note:whenVerylow = < 0.1
  denom = 1:maximumNumberPeople
  numer = 1:maximumNumberPeople

  comparison <- lapply(numer, function(x) (x/denom - ratio)^2)
  #lapply(comparison, function(x) min(x))

  indices_min <- c()
  values_min <- c()

  for(i in 1:length(comparison)){
    index_min <- which(comparison[[i]] == min(comparison[[i]]))[1]
    values_min[i] <- comparison[[i]][index_min]
    indices_min[i] <- index_min
  }

  # Find the lowest difference with the requested ratio:
  numeratorForSentence <- which(values_min == min(values_min))[1]
  denominatorForSentence <- indices_min[numeratorForSentence]

  # Print results to console for validation
  # print(paste(numeratorForSentence,
  #             denominatorForSentence,
  #             round(numeratorForSentence/denominatorForSentence, 4), sep = "-"))

  # Write the sentence from our results, and return it:
  data.frame(numerator =numeratorForSentence,
             denominator =denominatorForSentence
  ) %>%
    return()
}
