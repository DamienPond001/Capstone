rm(list = ls())
con1 <- file("./en_US/en_US.blogs.txt", "r")
con2 <- file("./en_US/en_US.news.txt", "r")
con3 <- file("./en_US/en_US.twitter.txt", "r")

# while ( TRUE ) {
#   line = readLines(con, n = 1)
#   if (grepl( "A computer once beat me at chess, but it was no match for me at kickboxing", line))
#   {
#     l <- l + 1
#   }
# 
#   if (length(line) == 0 ) 
#   {
#     break
#   }
# }

fromBlogs <- readLines(con1, n = 10000)
fromNews <- readLines(con2, n = 10000)
fromTwitter <- readLines(con3, n = 10000)
close(con1)
close(con2)
close(con3)

sample <- c(fromBlogs)#, fromNews, fromTwitter)
rm(list = c("fromBlogs", "fromNews", "fromTwitter"))

# listOfWords <- c()
# for (i in sample)
# {
#   listOfWords <- c(listOfWords,strsplit(gsub("[^a-zA-Z0-9 ]", "", i), " ")[[1]])
# }

#counts <- as.data.frame(table(listOfWords))

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

noPuncSample <- gsub(" {2,}", " ", gsub("[^a-zA-Z\\.' ]", "",sample))
noPuncSample <- gsub("^ ", "", noPuncSample)

oneGram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = 1))))
oneGram <- oneGram[order(oneGram$Freq, decreasing = TRUE),]
twoGram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = 2))))
twoGram <- twoGram[order(twoGram$Freq, decreasing = TRUE),]
threeGram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = 3))))
threeGram <- threeGram[order(threeGram$Freq, decreasing = TRUE),]
rm(list = c("sample", "noPuncSample"))

# library(ggplot2)
# ggplot(oneGram[1:10,], aes(x = Var1, y = Freq)) + geom_col()+
#   scale_x_discrete(limits=oneGram$Var1[1:10])
# ggplot(twoGram[1:10,], aes(x = Var1, y = Freq)) + geom_col()+
#   scale_x_discrete(limits=twoGram$Var1[1:10])
# ggplot(threeGram[1:10,], aes(x = Var1, y = Freq)) + geom_col()+
#   scale_x_discrete(limits=threeGram$Var1[1:10])

totalWordsByOrder <- numeric(1)
count = 1
for(i in oneGram$Freq)
{
  totalWordsByOrder <- c(totalWordsByOrder, i + totalWordsByOrder[count])
  count <- count + 1
}
totalWordsByOrder <- totalWordsByOrder[2:length(totalWordsByOrder)]
oneGram$count <- totalWordsByOrder
halfTotalWords <- oneGram[oneGram$count < sum(oneGram$Freq)/2,]
ninetyTotalWords <- oneGram[oneGram$count < (sum(oneGram$Freq)/10)*9,]

totalOneGrams <- sum(oneGram$Freq)
totalTwoGrams <- sum(twoGram$Freq)
totalThreeGrams <- sum(threeGram$Freq)

library(dplyr)
library(tidyr)
oneGram <- oneGram %>% mutate(probability_of_comb = Freq/totalOneGrams)
twoGram <- twoGram %>% mutate(probability_of_comb = Freq/totalTwoGrams) %>% separate(Var1, into = c("current", "next"), remove = FALSE)
threeGram <- threeGram %>% mutate(probability_of_comb = Freq/totalThreeGrams) %>% separate(Var1, into = c("previous", "current", "next")) %>% mutate(current = paste(previous, current))
threeGram <- subset(threeGram, select = -c(previous))

twoGramMerge <- merge(twoGram, oneGram, by.x = "current", by.y = "Var1")
twoGramMerge <- subset(twoGramMerge, select = -c(count, Var1))
colnames(twoGramMerge) <- c("current", "next", "Freq_of_current_next", "Prob_of_current_next", "Freq_of_current", "Prob_of_current") 
twoGramMerge <- twoGramMerge %>% mutate(Prob_of_next_given_current = Freq_of_current_next/Freq_of_current)

colnames(twoGram)[which(names(twoGram) == "current")] <- "current_1"
threeGramMerge <- merge(threeGram, twoGram, by.x = "current", by.y = "Var1")
threeGramMerge <- subset(threeGramMerge, select = -c(current_1, next.y))
colnames(threeGramMerge) <- c("current", "next", "Freq_of_current_next", "Prob_of_current_next", "Freq_of_current", "Prob_of_current")
threeGramMerge <- threeGramMerge %>% mutate(Prob_of_next_given_current = Freq_of_current_next/Freq_of_current)

oneGram <- subset(oneGram, select = -c(Freq, count))

PredictWord <- function(phrase, noPreds = 1, bigram = twoGramMerge, trigram = threeGramMerge, freq = oneGram)
{
  len <- length(strsplit(phrase, split= " ")[[1]])
  prediction <- as.character(freq[order(freq$probability_of_comb, decreasing = TRUE),"Var1"][1:noPreds])
  
  if(len == 1)
  {
    df <- bigram
    df <- df[df$current == phrase,]
    df <- as.character(df[order(df$Prob_of_next_given_current, decreasing = TRUE), "next"][1:noPreds])
    
    count = 1
    for(i in df)
    {
      if(!is.na(i))
      {
        prediction[count] <- i
      }
      count = count +1
    }
  }
  
  if(len == 2)
  {
    df <- trigram
    df <- df[df$current == phrase,]
    df <- as.character(df[order(df$Prob_of_next_given_current, decreasing = TRUE), "next"][1:noPreds])
    
    count = 1
    for(i in df)
    {
      if(!is.na(i))
      {
        prediction[count] <- i
      }
      count = count +1
    }
  }
  
  prediction
}

library(stringr)

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
