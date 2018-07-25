rm(list = ls())
con1 <- file("./en_US/en_US.blogs.txt", "r")
con2 <- file("./en_US/en_US.news.txt", "r")
con3 <- file("./en_US/en_US.twitter.txt", "r")

library(stringr)
library(dplyr)
library(tidyr)


fromBlogs <- readLines(con1, n = 20000)
fromNews <- readLines(con2, n = 20000)
fromTwitter <- readLines(con3, n = 20000)
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
totalWordsByOrder <- numeric(1)
count = 1
for(i in oneGram$Freq)
{
    totalWordsByOrder <- c(totalWordsByOrder, i + totalWordsByOrder[count])
    count <- count + 1
}
totalWordsByOrder <- totalWordsByOrder[2:length(totalWordsByOrder)]
oneGram$count <- totalWordsByOrder

totalOneGrams <- sum(oneGram$Freq)
oneGram <- oneGram %>% mutate(probability_of_comb = Freq/totalOneGrams)



# twoGram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = 2))))
# twoGram <- twoGram[order(twoGram$Freq, decreasing = TRUE),]
# threeGram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = 3))))
# threeGram <- threeGram[order(threeGram$Freq, decreasing = TRUE),]
#rm(list = c("sample", "noPuncSample"))


combined <- data.frame("current" = character(), "Freq_of_current_next"=  numeric(), "Prob_of_current_next"=  numeric(), "next" = character(), "Freq_of_current"=  numeric(), "Prob_of_current"=  numeric())
noGrams <- 4
gramToMerge <- oneGram
for(i in 2:noGrams)
{
    gram <- as.data.frame(table(unlist(lapply(noPuncSample, combineGrams, noGrams = i))), stringsAsFactors = FALSE)
    gram <- gram[order(gram$Freq, decreasing = TRUE),]
    totalGrams <- sum(gram$Freq)
    gram <- gram %>% mutate(probability_of_comb = Freq/totalGrams)
    gram$current <- sapply(gram$Var1, splitOnLastSpace, returnPart = 1)
    gram$Next <- sapply(gram$Var1, splitOnLastSpace, returnPart = 2)


    gramMerge <- merge(gram, gramToMerge, by.x = "current", by.y = "Var1")

    if(i == 2)
    {
      gramMerge <- subset(gramMerge, select = -c(count, Var1))
    }
    else
    {
      gramMerge <- subset(gramMerge, select = -c(count, Var1, current_1, Next.y))
    }
    
    colnames(gramMerge) <- c("current", "Freq_of_current_next", "Prob_of_current_next", "Next", "Freq_of_current", "Prob_of_current") 
    gramMerge <- gramMerge %>% mutate(Prob_of_next_given_current = Freq_of_current_next/Freq_of_current)
    colnames(gram)[which(names(gram) == "current")] <- "current_1"
    gramToMerge <- gram

    gramMerge <- gramMerge[gramMerge$Freq_of_current_next > 1, ]
    
    combined <- rbind(combined, gramMerge)
}
oneGram <- subset(oneGram, select = -c(Freq, count))
combined <- subset(combined, select = c(current, Next, Prob_of_next_given_current))
rm(list = c("gram", "gramMerge", "gramToMerge", "noPuncSample", "sample"))


#implement adjustment for phrases >= 2 words where last words are used instead 
PredictWord <- function(phrase, noPreds = 1, gram = combined, freq = oneGram)
{
    len <- length(strsplit(phrase, split= " ")[[1]])
    prediction <- as.character(freq[order(freq$probability_of_comb, decreasing = TRUE),"Var1"][1:noPreds])
    
    df <- gram
    df <- df[df$current == phrase,]
    df <- as.character(df[order(df$Prob_of_next_given_current, decreasing = TRUE), "Next"][1:noPreds])
        
    count = 1
    for(i in df)
    {
        if(!is.na(i))
        {
            prediction[count] <- i
        }
        count = count +1
    }
    
    prediction
}

