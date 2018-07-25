rm(list = ls())

con1 <- file("./en_US/en_US.blogs.txt", "r")
con2 <- file("./en_US/en_US.news.txt", "r")
con3 <- file("./en_US/en_US.twitter.txt", "r")

library(stringr)
library(dplyr)
library(tidyr)


fromBlogs <- readLines(con1, n = 10000)
fromNews <- readLines(con2, n = 10000)
fromTwitter <- readLines(con3, n = 10000)
close(con1)
close(con2)
close(con3)

sample <- c(fromBlogs, fromNews, fromTwitter)
rm(list = c("fromBlogs", "fromNews", "fromTwitter"))


combineGrams <- function(String, noGrams)
{
    splitString <- strsplit(String, " ")[[1]]
    noSpaces <- noGrams - 1
    Grams <- c()
    if (length(splitString) > noGrams)
    {
        for (i in 1:(length(splitString) - noSpaces))
        {
            stringToAdd <- c()
            for(j in 0:noSpaces)
            {
                stringToAdd <- c(stringToAdd, splitString[i+j])
            }
            
            Grams <- c(Grams, paste(stringToAdd, collapse = " "))
        }
    }
    Grams
}

splitOnLastSpace <- function(string, returnPart)
{
    split <- strsplit(string, split = "")[[1]]
    spaceIndex <-  max(grep(" ", split))
    if(returnPart == 1)
    {
        paste(split[1:(spaceIndex-1)], collapse = "")
    }
    else
    {
        paste(split[((spaceIndex+1):length(split))], collapse = "")
    }
}

calcAlphas <- function(g1 = 0.5, g2 = 0.5,gram = masterGram)
{
  gram$alpha <- -1
  gram$ACount <- 0
  
  for(i in 1:(max(gram$Gram)-1))
  {
    subGram <- gram[gram$Gram == i,"Var1"]
    gram[gram$Gram == i,"ACount"] <- sapply(subGram, calcACount, len = i)
    
  }
  
  gram <- gram %>% mutate(alpha = 1 - (ACount/Freq))
  gram
  
}

calcACount <- function(word, len, g = 0.5, gram = masterGram)
{
  gram <- gram[gram$Gram == len+1 & grepl(paste("^", word, " ", sep = ""), gram$Var1),]
  count <- sum(gram$Freq) - g
  count
}

noPuncSample <- gsub(" {2,}", " ", gsub("[^a-zA-Z\\.' ]", "",sample))
noPuncSample <- gsub("^[ \\.'] ", "", noPuncSample)
noPuncSample <- gsub("[\\.]$", "", noPuncSample)
noPuncSample <- gsub("[ ]$", "", noPuncSample)
noPuncSample <- gsub(" +\\.+ ", " \\.", noPuncSample)
noPuncSample <- gsub(" +'\\.'+ ", "", noPuncSample)
noPuncSample <- gsub("\\.{2,}", "", noPuncSample)
noPuncSample <- gsub("[ ]{2,}", "", noPuncSample)

oneGram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = 1))))
oneGram <- oneGram[order(oneGram$Freq, decreasing = TRUE),]



noGrams <- 3
masterGram <- data.frame("Var1" = character(), "Freq"=  numeric(), "Total"=  numeric(), "Gram"=  numeric())
for(i in 1:noGrams)
{
    gram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = i))), stringsAsFactors = FALSE)
    gram <- gram[order(gram$Freq, decreasing = TRUE),]
    totalGrams <- sum(gram$Freq)
    gram$Total <- totalGrams
    gram$Gram <- i
    
    masterGram <- rbind(masterGram, gram)
}

#masterGram <- calcAlphas(gram = masterGram)


PredictWord <- function(phrase, noPreds = 1, gram = masterGram)
{
  g1 <- 0.5
  g2 <- 0.5
  
  maxLen <- max(gram$Gram)
  len <- length(strsplit(phrase, split = " ")[[1]])

  calcProbabilities(phrase, gram, len, g1, g2)
  

  
  #strsplit(subsetGram$Var1[1], split = " ")[[1]]
}

calcProbabilities <- function(phrase, gram, len, g1, g2)
{
  observedGram <- gram[gram$Gram == len+1 & grepl(paste("^", phrase, " ", sep = ""), gram$Var1),]
  unobservedGram <- gram[gram$Gram == len+1 & !grepl(paste("^", phrase, " ", sep = ""), gram$Var1),]
  
  observedGram <- calcObserved(phrase, observedGram, gram, len, g1, g2)
  unobservedGram <- calcUnobserved(phrase, unobservedGram, gram, len, g1, g2)
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
  phrase <- gsub("^\\S+ {1,}", "", phrase)
  
  if((len-1)>=0)
  {
    calcProbabilities(phrase, gram, len-1, g1, g2)
  }
  else
  {
    unobservedGram$probability <- 100
    unobservedGram
  }
}