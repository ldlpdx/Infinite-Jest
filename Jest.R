library(tm)
library(RColorBrewer)
library(broom)
library(formattable)
library(ggplot2)
library(ggthemes)
library(ggvis)
library(kernlab)
library(plotly)
library(plyr)
library(dplyr)
library(wordcloud)
library(rJava)
library(openNLP)
library(topicmodels)
library(quanteda)
library(tidyr)
library(gridExtra)
library(slam)
library(RTextTools)
library(Hmisc)
library(reshape)
library(gplots)
library(GGally)
library(Matrix)
library(qlcMatrix)
library(svs)
library(Rstem)
library(sentiment)
library(tidytext)
library(ggraph)
library(ggjoy)
theme_set(theme_minimal())

require(devtools)
install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install_url("http://cran.r-project.org/src/contrib/languageR_1.4.1..tar.gz")


##########################################################
# READ IN FILES; CREATE CORPUS; TOKENIZE
##########################################################

jest <- scan("David_Foster_Wallace_Infinite_Jest.txt", what = "character", sep = "\n")
jestLower <- tolower(jest)
jestCorpus <- corpus(texts(jestLower))
jest.words <- tokenize(jestCorpus, what = "word", removePunct = TRUE)
jest.sent <- tokenize(jestCorpus, what = "sentence")

library(readr)
jest.df <- data_frame(text= read_lines("David_Foster_Wallace_Infinite_Jest.txt")) %>%
  mutate(line = row_number())

jest.tokens <- jest.df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

jest.tokens %>%
  count(word, sort=TRUE)

jest.sentiment <- jest.tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index=line %/% 80, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment=positive - negative)

ggplot(jest.sentiment, aes(index, sentiment)) +
  geom_col() +
  theme_bw()

library(cowplot)
plotbase <- ggplot(jest.sentiment, aes(x=sentiment, y=index, height=sentiment)) 
plot_grid(plotbase + geom_ridgeline(),
          plotbase + geom_ridgeline(min_height=-400))



  
##########################################################
### --- Part-of-Speech tagging and syntactic parsing with R
##########################################################
### --- R script "Part-of-Speech tagging and syntactic parsing with R"
### --- Author: Martin Schweinberger
### --- This script aims at an automated approach to POS tagging
### --- a sample corpus.
##########################################################
### --- Prepare data
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages we need or which may be useful
# (to activate just delete the #)
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
#install.packages("openNLP")
#install.packages("NLP")
### additional packages
#install.packages("tm")
#install.packages("stringr")
install.packages("gsubfn")
#install.packages("plyr")
# load packages
library(NLP)
library(openNLP)
library(openNLPmodels.en)
### load additional packages
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
# to install openNLPmodels, please download an install the packages/models direktly from
# http://datacube.wu.ac.at/. To install these packages/models, simply enter
#install.packages("foo", repos = "http://datacube.wu.ac.at/", type = "source")
# into your R console. E.g. enter:
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
# to install the file "openNLPmodels.en_1.5-1.tar.gz"
##########################################################
# specify path of corpus
#pathname <- "./Clinton"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
#corpus.files = list.files(path = pathname, pattern = NULL, all.files = T,
#                          full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
#corpus.tmp <- lapply(clinton.sent, function(x) {
#  scan(x, what = "char", sep = "\t", quiet = T) }  )
# Paste all elements of the corpus together
#corpus.tmp <- lapply(clinton.sent, function(x){
#  x <- paste(x, collapse = " ")  }  )
# Clean corpus
#corpus.tmp <- lapply(clinton.sent, function(x) {
#  x <- enc2utf8(x)  }  )
#corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
#corpus.tmp <- str_trim(corpus.tmp, side = "both") # remove spaces at beginning and end of strings
# inspect result
#str(corpus.tmp)
#>chr [1:3] "This is the first sentence in the first file of the test corpus. This is a second sentence in the test corpus but I am too lazy"| __truncated__ ...


# convert corpus files into strings
#Corpus <- lapply(corpus.tmp, function(x){
#  x <- as.String(x)  }  )
###############################################################
# Start actual PoS-tagging
# apply annotators to Corpus
# Corpus.tagged <- lapply(Corpus, function(x){
#  sent_token_annotator <- Maxent_Sent_Token_Annotator()
#  word_token_annotator <- Maxent_Word_Token_Annotator()
#  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
#  y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
#  y2 <- NLP::annotate(x, pos_tag_annotator, y1)
#  y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
#  y2w <- subset(y2, type == "word")
#  tags <- sapply(y2w$features, '[[', "POS")
#  r1 <- sprintf("%s/%s", x[y2w], tags)
#  r2 <- paste(r1, collapse = " ")
#  return(r2)  }  )

# inspect results
#Corpus.tagged

###############################################################
### --- a function which POS tags corpus files
###############################################################
POStag <- function(file = file){
  require("NLP")
  require("openNLP")
  require("openNLPmodels.en")
  corpus.tmp <- lapply(file, function(x){
    x <- paste(x, collapse = " ")  }  )
  corpus.tmp <- lapply(corpus.tmp, function(x) {
    x <- enc2utf8(x)  }  )
  corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
  corpus.tmp <- str_trim(corpus.tmp, side = "both") 
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator() 
  lapply(Corpus, function(x){
    y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    y2<- NLP::annotate(x, pos_tag_annotator, y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
}
# TAG PARTS OF SPEECH
library(stringr)
jest.POS <- POStag(file = jest.words)

# CREATE DATAFRAME FROM LIST
jest.POS.un <- unlist(jest.POS)
jest.POS.df <- read.table(textConnection(gsub(" ", " \n ", jest.POS.un)), sep="/", quote = "", comment = '', encoding="latin1", fill = TRUE, stringsAsFactors=FALSE)

# EXTRACT BIGRAMS
bigram = function(word, type, patt1 = "JJ", patt2 = "JJ") {
  pairs = which(c(FALSE, str_detect(type, pattern = patt1)) &
                  c(str_detect(type, patt2), FALSE))
  return(paste(word[pairs - 1], word[pairs]))
}

jest.JJNN <- with(jest.POS.df, bigram(word = V1, type = V2))
jest.RBJJ <- with(jest.POS.df, bigram(word = V1, type = V2))
jest.RBVB <- with(jest.POS.df, bigram(word = V1, type = V2))
jest.JJJJ <- with(jest.POS.df, bigram(word = V1, type = V2))
