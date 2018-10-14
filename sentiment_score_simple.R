# Many thanks to Johannes Tynes, CFA for this excellent blog post
# https://www.linkedin.com/pulse/20141109035942-34768479-r-sentiment-scoring-hsbc-w-harvard-general-inquirer/


# Code to fetch news streams from 5 live sources, process the streams and text
# and apply a simple sentiment scoring algorigthm.
#
# A writeup of the analysis can be found here:
# https://www.linkedin.com/pulse/article/20141109035942-34768479-r-sentiment-scoring-hsbc-w-harvard-general-inquirer

# Define the packages we want to load:
packs = c(
  "tm",                         # Text mining
  "tm.plugin.webmining",        # Web-source plugin for text mining
  "SnowballC",                  # Stemmer
  "RColorBrewer",               # Colors for visualisation
  "ggplot2",                    # Plotting
  "wordcloud",                  # Draw wordclouds
  "openNLP"                     # Split text into sentences.
)

sapply(packs, require, character.only=TRUE)   # Load the packages.

# Download the corpora and insert them in a named list.
corpora = list(
  googlefinance = WebCorpus(GoogleFinanceSource("NYSE:HSBC")),
  googlenews    = WebCorpus(GoogleNewsSource("HSBC")),
  yahoofinance  = WebCorpus(YahooFinanceSource("HSBC")),
  yahooinplay   = WebCorpus(YahooInplaySource()),
  yahoonews     = WebCorpus(YahooNewsSource("HSBC"))
)

# Save the corpora list.
save(corpora, file="data/corpora.Rdat")


#load('data/corpora.Rdat')



# Break the corpora down into sentences and define some functions to do so.

ToSentences = function(text, language="en") {
  # Splits text into sentences using an Apache OpenNLP sentence detector.

  # Arguments:
  # "text" the text to be processed (character)
  # "lang" ISO-639 code of the language of the text (character)

  # Returns:
  # sentences of the text (character vector)
  if(length(text) ==0)      {return("")}
  if(nchar(text) == 0)   {return("")}   # Cover special case 0-character text.
 
  # Convert text to String object; allows for splitting by index.
  text = as.String(text)

  # Discover the sentence markers in the text (specify NLP as
  # source of annotate because there is also an annotate function in ggplot2)
  markers = NLP::annotate(
    text,
    Maxent_Sent_Token_Annotator(language=language)   # Annotator from OpenNLP
  )

  # Return sentences by splitting the text at the boundaries.
  text[markers]
}


CorpusToSentences = function(corpus) {
  # Split every document in the corpus into sentences and return a new corpus
  # with all the sentences as individual documents.

  # Extract the text from each document in the corpus.
  text = lapply(corpus, "[[", "content")

  # Basically convert the text
  docs = lapply(text, ToSentences)

  docs = as.vector(unlist(docs))

  # Return a corpus with sentences as documents.
  Corpus(VectorSource(docs))
}


# Create a new corpus which merges existing corpora after splitting them
# into sentences.
corpus = Reduce(c, lapply(corpora, CorpusToSentences))

# Process the corpora contents.
corpus = tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)

#toString = content_transformer(function(x, from, to) gsub(from, to, x))
#corpus = tm_map(corpus, toString, "hsbc", "hsbc")


# Stemming
# corpus = tm_map(corpus, stemDocument)


# Create a document term matrix from the corpus.
dtm = DocumentTermMatrix(corpus)

# Subset the DTM to include only documents including the term "hsbc".
dtm = dtm[rowSums(as.matrix(dtm[ , "hsbc"])) > 0, ]

# Remove terms which are not contained in any of the documents.
dtm = dtm[ , colSums(as.matrix(dtm)) > 0]


# ACQUIRING AND PROCESSING THE LEXICON.

# Load the sentiment lexicon (saved down in working directory as a
# comma separated value file).
lex = read.csv("inquirerbasic.csv", stringsAsFactors=FALSE)

# Collapse words with multiple entries into one entry. These are marked
# with a trailing #1, #2, ...

# Remove #1 tags. 
lex$Entry = gsub("#1", "", lex$Entry)

# Remove entries that are still numbered (i.e. two or higher)
lex = lex[!grepl("#", lex$Entry), ]

# Extract the positive and negative words from the lexicon.
neg.lex = tolower(lex$Entry[lex$Negativ != ""])
pos.lex = tolower(lex$Entry[lex$Positiv != ""])

terms = colnames(dtm)

# Find the positive and negative terms using the lexicons.
neg.terms = terms[terms %in% neg.lex]
pos.terms = terms[terms %in% pos.lex]

# Specify positive terms which may be quiestionable.
pos.terms.adj = setdiff(pos.terms, c("equity", "share", "consensus"))

# Calculate the negative and positive sentence scores ("document scores").
neg.scores = rowSums(as.matrix(dtm[ , neg.terms]))
pos.scores = rowSums(as.matrix(dtm[ , pos.terms]))

document.scores = pos.scores - neg.scores

# Calulate the document signs ("sentence signs").
document.signs = sign(document.scores)

# Calculate the sentiment score
sentiment.score = sum(document.signs == 1) / sum(document.signs !=0)


## Visualisation:


# Generate word clouds (positive and negative).
PosCloud = function() {
  wordcloud(
    pos.terms,
    colSums(as.matrix(dtm[ , pos.terms])),
    min.freq=1,
    scale=c(4,0.7),
    color=brewer.pal(n=9, "Blues")[6:9]
  )
}

NegCloud = function() {
  wordcloud(
    neg.terms,
    colSums(as.matrix(dtm[ , neg.terms])),
    min.freq=1,
    scale=c(4,0.7),
    color=brewer.pal(n=9, "Reds")[6:9]
  )
}
