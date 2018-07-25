rm(list = ls())

makeGram <- function()
{
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(parallel)
  
  con1 <- file("./en_US/en_US.blogs.txt", "r")
  con2 <- file("./en_US/en_US.news.txt", "r")
  con3 <- file("./en_US/en_US.twitter.txt", "r")
  
  sample <- sapply(list(con1, con2, con3), function(x){readLines(x, n = 10000)})
  
  close(con1)
  close(con2)
  close(con3)
  
  noPuncSample <- formatText(sample)
  rm(list = c("sample"))
  
  masterGram <- createGrams(noPuncSample)
  rm(noPuncSample)
  
  start <- Sys.time()
  masterGram <- calcAlphas(gram = masterGram)
  end <- Sys.time()
  print(end - start)
  write.csv(masterGram, file = "fullGram.csv")
  #return(masterGram)
}

formatText <- function(text)
{
  noPuncSample <- gsub(" {2,}", " ", gsub("[^a-zA-Z\\.' ]", "",text))
  noPuncSample <- gsub("^[ \\.'] ", "", noPuncSample)
  noPuncSample <- gsub("[\\.]$", "", noPuncSample)
  noPuncSample <- gsub("[\\.]", "", noPuncSample)
  noPuncSample <- gsub("[ ]$", "", noPuncSample)
  noPuncSample <- gsub(" +\\.+ ", " \\.", noPuncSample)
  noPuncSample <- gsub(" +'\\.'+ ", "", noPuncSample)
  noPuncSample <- gsub("\\.{2,}", "", noPuncSample)
  noPuncSample <- gsub("[ ]{2,}", "", noPuncSample)
  noPuncSample <- tolower(noPuncSample)
  
  return(noPuncSample)
}

createGrams <- function(noPuncSample)
{
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("noPuncSample"), envir = environment())
  
  oneGram <- as.data.frame(table(unlist(parLapply(cl, noPuncSample, combineGrams, noGrams = 1))))
  oneGram <- oneGram[order(oneGram$Freq, decreasing = TRUE),]
  
  noGrams <- 3
  masterGram <- data.frame("Var1" = character(), "Freq"=  numeric(), "Total"=  numeric(), "Gram"=  numeric())
  for(i in 1:noGrams)
  {
    gram <- as.data.frame(table(unlist(parLapply(cl, noPuncSample, combineGrams, noGrams = i))), stringsAsFactors = FALSE)
    gram <- gram[order(gram$Freq, decreasing = TRUE),]
    totalGrams <- sum(gram$Freq)
    gram$Total <- totalGrams
    gram$Gram <- i
    
    masterGram <- rbind(masterGram, gram)
  }
  stopCluster(cl)
  
  return(masterGram)
}

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


calcACount <- function(word, len, gram, g = 0.5)
{
  gram <- gram[grepl(word, gram$Var1),]
  count <- sum(gram$Freq - g)
  count
}

calcAlphas <- function(gram, g1 = 0.5, g2 = 0.5)
{
  gram$alpha <- -1
  gram$ACount <- 0
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  
  for(i in 1:(max(gram$Gram)-1))
  {
    subGram <- paste("^",gram[gram$Gram == i,"Var1"], " ", sep = "")
    subGramP1 <- gram[gram$Gram == i+1,]

    clusterExport(cl, c("subGram", "gram", "subGramP1"), envir = environment())

    gram[gram$Gram == i,"ACount"] <- parSapply(cl, subGram, calcACount, len = i, gram = subGramP1)
  }
  stopCluster(cl)
  gram <- gram %>% mutate(alpha = 1 - (ACount/Freq))
  gram
  
}


makeGram()