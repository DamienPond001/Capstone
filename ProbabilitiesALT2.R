rm(list = ls())

PredictWord <- function(phrase, noPreds = 1)
{
  g1 <- 0.5
  g2 <- 0.5
  
  gram <- read.csv("sampleGram.csv")
  gram$probability <- 1
  gram$coefficient <- 1
  
  len <- length(strsplit(phrase, split = " ")[[1]])
  
  calculated <- calcProbabilities(phrase, gram, len, g1, g2)
  calculated <- calculated %>% mutate(probability = probability*coefficient)
  calculated <- calculated[order(calculated$probability, decreasing = TRUE),]
  return(calculated)
}

calcProbabilities <- function(phrase, gram, len, g1, g2)
{
  observedGram <- gram[gram$Gram == len+1 & grepl(paste("^", phrase, " ", sep = ""), gram$Var1),]
  unobservedGram <- gram[gram$Gram == len+1 & !grepl(paste("^", phrase, " ", sep = ""), gram$Var1),]
  observedGram <- calcObserved(phrase, observedGram, gram, len, g1, g2)
  unobservedGram <- calcUnobserved(phrase, unobservedGram, gram, len, g1, g2)
  rbind(observedGram, unobservedGram)
}

calcObserved <- function(phrase, observedGram, gram, len, g1, g2)
{
  denomCount <- gram$Freq[gram$Var1 == phrase]
  observedGram <- observedGram %>% mutate(probability = probability*((Freq-g2)/denomCount))
  observedGram <- observedGram[order(observedGram$probability, decreasing = TRUE),]
  observedGram
}


calcUnobserved <- function(phrase, unobservedGram, gram, len, g1, g2)
{

  phraseShortened <- gsub("^\\S+ {1,}", "", phrase)
  alpha <- ifelse(nrow(gram[gram$Var1 == phrase,]) > 0, gram[gram$Var1 == phrase, "alpha"], 1)
  
  if((len-1) > 0)
  {
    denom <- calcSummedB(phrase,phraseShortened, gram, len, g1, g2)
    probGram <- calcProbabilities(phraseShortened, gram, len-1, g1, g2)
    probGram[probGram$Gram == len,] <- probGram[probGram$Gram == len,] %>% mutate(probability = probability/denom)
    probGram <- probGram %>% mutate(coefficient = alpha*coefficient)
    
    return(probGram)
  }
  else
  {
    denom <- calcSummedB(phrase,phraseShortened, gram, len, g1, g2)
    unobservedGram <- gram[gram$Gram == 1,] %>% mutate(probability = probability*(Freq/denom))
    unobservedGram <- unobservedGram %>% mutate(coefficient = coefficient*alpha)
    return(unobservedGram)
  }
}

calcB <- function(phrase,phraseShortened, gram, len)
{
  lastOfPhrase <- as.character(gram[gram$Gram == len + 1 & grepl(paste("^", phrase, " ", sep = ""), gram$Var1), "Var1"])
  lastOfPhrase <- gsub("[a-z]{1,} {1,}", "",lastOfPhrase)
  
  if(len != 1)
  {
    lastOfShortPhrase <- as.character(gram[gram$Gram == len & grepl(paste("^", phraseShortened, " ", sep = ""), gram$Var1), "Var1"])
    lastOfShortPhrase <- gsub("[a-z]{1,} {1,}", "",lastOfShortPhrase)
  }
  else
  {
    lastOfShortPhrase <- as.character(gram[gram$Gram == len, "Var1"])
  }

  setdiff(lastOfShortPhrase,lastOfPhrase)
}

calcSummedB <- function(phrase, phraseShortened, gram, len, g1, g2)
{
  if(len == 1)
  {
    B <- calcB(phrase,phraseShortened, gram, len)
    denom <- sum(gram[gram$Gram == 1 & gram$Var1 %in% B, "Freq"])
    return(denom)
  }
  else
  {
    B <- calcB(phrase,phraseShortened, gram, len)
    phrases <- paste(phraseShortened, B, sep = " ")
    refreshedGram <- gram[gram$Gram <= len,]
    refreshedGram$probability <- 1
    refreshedGram$coefficient <- 1
    probabilities <- calcProbabilities(phraseShortened, refreshedGram, len-1, g1, g2)
    probabilities <- probabilities %>% mutate(probability = probability*coefficient)
    probabilities <- probabilities[probabilities$Var1 %in% phrases, "probability"]
    
    return(sum(probabilities))
  }
}

start <- Sys.time()
a <- PredictWord("to the")
end <- Sys.time()
print(end - start)