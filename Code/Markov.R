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

combineGrams <- function(string, noGrams)
{
    Chain <- list()
    string <- strsplit(string, split = " ")[[1]]
    k = noGrams - 2
      for(point in 1:(length(string) - (noGrams - 1)))
      {
        if(paste(string[point:(point+k)], collapse = " ") %in% names(Chain))
        {
          Chain[[paste(string[point:(point+k)], collapse = " ")]] <- c(Chain[[paste(string[point:(point+k)], collapse = " ")]], paste(string[(point+ k + 1)], sep = " "))
        }
        else
        {
          Chain[[paste(string[point:(point+k)], collapse = " ")]] <- c(paste(string[(point+ k + 1)], collapse = " "))
        }
      }
    Chain
}

noPuncSample <- gsub(" {2,}", " ", gsub("[^a-zA-Z\\.' ]", "",sample))
noPuncSample <- gsub("^ ", "", noPuncSample)
noPuncSample <- paste(noPuncSample, collapse = " ")

WordPredictions <- combineGrams(noPuncSample, noGrams = 2)



PredictWord <- function(phrase, WordPredictions, noPreds = 1)
{
  if(phrase %in% names(WordPredictions))
  {
    gsub("[^a-zA-Z ]", "",sample(WordPredictions[[phrase]],1))
  }
}
