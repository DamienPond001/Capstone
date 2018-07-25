rm(list = ls())

PredictWord <- function(phrase, noPreds = 1)
{
  g1 <- 0.5
  g2 <- 0.5
  
  gram <- read.csv("sampleGram.csv")
  
  maxLen <- max(gram$Gram)
  len <- length(strsplit(phrase, split = " ")[[1]])
  
  calcProbabilities(phrase, gram, len, g1, g2)
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
  observedGram <- observedGram %>% mutate(probability = (Freq-g2)/denomCount)
  observedGram <- observedGram[order(observedGram$probability, decreasing = TRUE),]
  observedGram
}


calcUnobserved <- function(phrase, unobservedGram, gram, len, g1, g2)
{

  phraseShortened <- gsub("^\\S+ {1,}", "", phrase)
  alpha <- ifelse(nrow(gram[gram$Var1 == phrase,]) > 0, gram[gram$Var1 == phrase, "alpha"], 1)
  
  if((len-1) >= 0)
  {
    denom <- calcSummedB(phrase,phraseShortened, gram, len, g1, g2)
    probGram <- calcProbabilities(phraseShortened, gram, len-1, g1, g2)
    probGram$probability <- alpha*probGram$probability
    probGram
  }
  else
  {
    unobservedGram$probability <- 100#unobservedGram %>% mutate()
    unobservedGram
  }
}

calcB <- function(phrase,phraseShortened, gram, len)
{
  lastOfPhrase <- as.character(gram[gram$Gram == len + 1 & grepl(paste("^", phrase, " ", sep = ""), gram$Var1), "Var1"])
  lastOfPhrase <- gsub("[a-z]{1,} {1,}", "",lastOfPhrase)
  
  lastOfShortPhrase <- as.character(gram[gram$Gram == len & grepl(paste("^", phraseShortened, " ", sep = ""), gram$Var1), "Var1"])
  lastOfShortPhrase <- gsub("[a-z]{1,} {1,}", "",lastOfShortPhrase)

  setdiff(lastOfPhrase,lastOfShortPhrase)
}

calcSummedB <- function(phrase,phraseShortened, gram, len, g1, g2)
{
  B <- calcB(phrase,phraseShortened, gram, len)
  phrases <- paste(phraseShortened, B, sep = " ")

  # p <- sapply(phrases, calcProbabilities, gram = gram, len = len, g1 = g1, g2 = g2)
  # print(p)
}


start <- Sys.time()
a <- PredictWord("to the")
end <- Sys.time()
print(end - start)