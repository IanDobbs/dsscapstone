#load libraries
library(parallel)
library(tm)
library(stringi)
library(stopwords)
library(qdapRegex)
library(RWeka)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(stringr)

#fileurl <-  "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#download.file(fileurl, destfile = "Coursera-SwiftKey.zip", method = "curl")
#unzip("Coursera-SwiftKey.zip")

# Sampling
# set seed for reproducibility
set.seed(1962)
# List of file paths for the three separate text files
file_paths <- c("C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/final/en_US/en_US.blogs.txt",
                "C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/final/en_US/en_US.news.txt",
                "C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/final/en_US/en_US.twitter.txt")
# Function to calculate the total number of non-empty lines in a file
get_total_non_empty_lines <- function(file_path) {
        lines <- readLines(file_path, warn = FALSE)
        non_empty_lines <- lines[nchar(lines) > 0]
        return(length(non_empty_lines))
}
# Calculate the total number of non-empty lines in each file
total_non_empty_lines <- sapply(file_paths, get_total_non_empty_lines)
# Calculate the total non-empty lines across all files
total_non_empty_lines_all <- sum(total_non_empty_lines)
# Define the desired sample size as a proportion of the total non-empty lines
sample_proportion <- 0.1  # 10% of the total non-empty lines
# Calculate the sample size for each file
sample_size_per_file <- round(sample_proportion * total_non_empty_lines)
# Initialize an empty list to store sampled lines from each file
sampled_lines_list <- list()
# Loop through each file and sample lines while ignoring empty lines
for (i in 1:length(file_paths)) {
        file_path <- file_paths[i]
        num_non_empty_lines <- total_non_empty_lines[i]
        sample_size <- sample_size_per_file[i]
        
        # Read all lines while ignoring empty lines
        lines <- readLines(file_path, warn = FALSE)
        non_empty_lines <- lines[nchar(lines) > 0]
        
        # Sample lines proportionally to the non-empty lines
        sampled_indices <- sample(1:num_non_empty_lines, size = sample_size)
        
        # Select the sampled lines from non-empty lines
        sampled_lines <- non_empty_lines[sampled_indices]
        
        sampled_lines_list[[file_path]] <- sampled_lines
}
# Combine the sampled lines into a single dataset
combined_lines <- unlist(sampled_lines_list)
# Specify the output file
output_file <- "output_sample.txt"
# Write the combined lines to the output file
writeLines(combined_lines, con = output_file)
cat("Text sample data saved to", output_file, "\n")
# clean up
rm(get_total_non_empty_lines, sampled_lines_list, combined_lines, lines, non_empty_lines, sampled_lines)
gc()

# Define a function to count and replace profanity in a chunk of words
count_profanity_chunk <- function(chunk, profanity_list) {
        word_table <- table(chunk)
        profanity_count <- word_table[match(profanity_list, names(word_table))]
        profanity_count[is.na(profanity_count)] <- 0
        return(profanity_count)
}

replace_profanity_chunk <- function(chunk, profanity_list) {
        for (word in profanity_list) {
                chunk <- str_replace_all(chunk, regex(paste0("\\b", word, "\\b"), ignore_case = TRUE), "")
        }
        return(chunk)
}

# Read the profanity list
profanity_list <- readLines("C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/ofcomwords.txt",
                            encoding = "UTF-8", skipNul = TRUE)

# Specify the path to the input/output text file
input_file <- "C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/output_sample.txt"
output_file <- "output_filtered.txt"

# Read the text from the input file
text <- tolower(readLines(input_file, warn = FALSE))

# Tokenize the text into words
words <- unlist(strsplit(text, "\\s+"))

# Split the words into chunks for parallel processing
num_cores <- detectCores() - 1
word_chunks <- split(words, 1:length(words) %% num_cores)

# Initialize a cluster with the desired number of cores
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, c("word_chunks", "profanity_list", "count_profanity_chunk"))

# Parallelize the profanity counting
profanity_counts <- parLapply(cl, word_chunks, count_profanity_chunk, profanity_list)

# Combine the results from all chunks
result <- Reduce("+", profanity_counts)

# Create a data frame for analysis
profanity_data <- data.frame(Word = names(result), Count = result)

# Sort the data frame in descending order by count
profanity_data <- profanity_data[order(profanity_data$Count.Freq, decreasing = TRUE), ]

# Display the sorted data frame
print(profanity_data)

# Stop the cluster
stopCluster(cl)

# Check if there is profanity detected
if (sum(profanity_data$Count.Freq) > 0) {
        # Split the text into chunks for parallel processing (you can add error handling here)
        num_cores <- detectCores() - 1
        text_chunks <- split(text, 1:length(text) %% num_cores)
        
        # Initialize a cluster with the desired number of cores
        cl <- makeCluster(num_cores)
        
        # Load necessary packages in parallel workers
        clusterEvalQ(cl, library(stringr))
        
        # Export necessary variables and functions to the cluster
        clusterExport(cl, c("text_chunks", "profanity_list", "replace_profanity_chunk"))
        
        # Parallelize profanity removal
        filtered_chunks <- parLapply(cl, text_chunks, replace_profanity_chunk, profanity_list)
        
        # Combine the filtered text from chunks
        filtered_text <- do.call(paste, filtered_chunks)
        
        # Write the filtered text to the output file (add error handling)
        tryCatch({
                writeLines(filtered_text, con = output_file)
                cat("Filtered text saved to", output_file, "\n")
        }, error = function(e) {
                cat("Error writing filtered text to", output_file, "Error: ", e$message, "\n")
        })
        
        # Stop the cluster
        stopCluster(cl)
} else {
        # No profanity detected, proceed with the original text
        cat("No profanity detected. Continue with the original text:\n", text)
}
# clean up
rm(count_profanity_chunk, replace_profanity_chunk, filtered_chunks, text_chunks, word_chunks, filtered_text, text, words)
gc()

# clean the filtered text
input_file <- "C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/output_filtered.txt"
output_file <- "output_cleaned.rds"
output_file1 <- "output_cleaned.txt"

text <- tolower(readLines(input_file, warn = FALSE))
text <- rm_non_words(text) 

clean_text <- function(text) {
        corpus <- Corpus(VectorSource(text))
        
        for (i in seq_along(corpus)) {
                doc <- corpus[[i]]
                
                doc <- gsub("'", "", doc)
                doc <- gsub("don t", "dont", doc)
                doc <- gsub("can t", "cant", doc)
                doc <- gsub("doesn t", "doesnt", doc)
                doc <- gsub("didn t", "didnt", doc)
                doc <- gsub("wasn t", "wasnt", doc)
                doc <- gsub("isn t", "isnt", doc)
                
                doc <- removeNumbers(doc)
                doc <- removePunctuation(doc)
                doc <- removeWords(doc, stopwords("en"))
                doc <- stripWhitespace(doc)
                
                corpus[[i]] <- doc
        }
        
        cleaned_text <- sapply(corpus, as.character)
        
        return(cleaned_text)
}

cleaned_text <- clean_text(text)

saveRDS(cleaned_text, file = output_file)
writeLines(cleaned_text, con = output_file1)
cat("Filtered & cleaned text saved to", output_file1, "\n")
rm(clean_text, cleaned_text, text)
gc()

# the text data is in a file, use readLines() to read it into a character vector.
input_file <- "C:/Users/44772/Documents/Rstudio_projects/datasciencecoursera/dsscapstone/output_cleaned.txt"
cleaned_text <- tolower(readLines(input_file, warn = FALSE))
# Create a corpus from cleaned text
corpus <- VCorpus(VectorSource(cleaned_text))

# Create term document matrix for the corpus
unigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# Define a function to process a subset of the corpus
process_corpus_chunk <- function(chunk) {
        unigramtdm <- TermDocumentMatrix(chunk, control = list(tokenize = unigram_tokenizer))
        tdm_matrix <- as.matrix(unigramtdm)
        term_frequencies <- rowSums(tdm_matrix)
        uFreq <- data.frame(word = rownames(tdm_matrix), freq = term_frequencies)
        return(uFreq)
}

# Split the corpus into chunks
corpus_chunks <- split(corpus, 1:length(corpus))

# Process the corpus chunks in parallel using lapply
uFreq_list <- lapply(corpus_chunks, process_corpus_chunk)

# Combine the results
uFreq <- do.call(rbind, uFreq_list)

# Calculate total term frequencies
total_term_frequencies <- aggregate(freq ~ word, data = uFreq, FUN = sum)

# Merge the total term frequencies with the individual term frequencies
uFreq <- merge(uFreq, total_term_frequencies, by = "word", all.x = TRUE)

# remove freq.x column
uFreq <- uFreq[, c("word", "freq.y")]

# Rename columns
colnames(uFreq) <- c("word", "total_freq")

# Remove duplicates
uFreq <- unique(uFreq)

# Order by total frequency
uFreq <- uFreq[order(-uFreq$total_freq), ]

# Filter based on total frequency
uFreq <- subset(uFreq, total_freq >= 3)

# Extract the first word from the 'word' column
uFreq$Word1 <- sapply(strsplit(as.character(uFreq$word), split = ' '), `[`, 1)

# Clean up
rm(corpus_chunks, unigram_tokenizer, uFreq_list)

# Garbage collection
gc()

# Create term document matrix for the corpus
bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
process_corpus_chunk <- function(chunk) {
        bigramtdm <- TermDocumentMatrix(chunk, control = list(tokenize = bigram_tokenizer))
        tdm_matrix <- as.matrix(bigramtdm)
        term_frequencies <- rowSums(tdm_matrix)
        bFreq <- data.frame(word = rownames(tdm_matrix), freq = term_frequencies)
        return(bFreq)
}
corpus_chunks <- split(corpus, 1:length(corpus))
bFreq_list <- lapply(corpus_chunks, process_corpus_chunk)
bFreq <- do.call(rbind, bFreq_list)
total_term_frequencies <- aggregate(freq ~ word, data = bFreq, FUN = sum)
bFreq <- merge(bFreq, total_term_frequencies, by = "word", all.x = TRUE)
bFreq <- bFreq[, c("word", "freq.y")]
colnames(bFreq) <- c("word", "total_freq")
bFreq <- unique(bFreq)
bFreq <- bFreq[order(-bFreq$total_freq), ]
bFreq <- subset(bFreq, total_freq >= 3)
bFreq$Word1 <- sapply(strsplit(as.character(bFreq$word), split = ' '), `[`, 1)
bFreq$Word2 <- sapply(strsplit(as.character(bFreq$word), split = ' '), `[`, 2)
# Clean up
rm(corpus_chunks, bigram_tokenizer, bFreq_list)
# Garbage collection
gc()

# Create term document matrix for the corpus
trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
process_corpus_chunk <- function(chunk) {
        trigramtdm <- TermDocumentMatrix(chunk, control = list(tokenize = trigram_tokenizer))
        tdm_matrix <- as.matrix(trigramtdm)
        term_frequencies <- rowSums(tdm_matrix)
        tFreq <- data.frame(word = rownames(tdm_matrix), freq = term_frequencies)
        return(tFreq)
}
corpus_chunks <- split(corpus, 1:length(corpus))
tFreq_list <- lapply(corpus_chunks, process_corpus_chunk)
tFreq <- do.call(rbind, tFreq_list)
total_term_frequencies <- aggregate(freq ~ word, data = tFreq, FUN = sum)
tFreq <- merge(tFreq, total_term_frequencies, by = "word", all.x = TRUE)
tFreq <- tFreq[, c("word", "freq.y")]
colnames(tFreq) <- c("word", "total_freq")
tFreq <- unique(tFreq)
tFreq <- tFreq[order(-tFreq$total_freq), ]
tFreq <- subset(tFreq, total_freq >= 3)
tFreq$Word1 <- sapply(strsplit(as.character(tFreq$word), split = ' '), `[`, 1)
tFreq$Word2 <- sapply(strsplit(as.character(tFreq$word), split = ' '), `[`, 2)
tFreq$Word3 <- sapply(strsplit(as.character(tFreq$word), split = ' '), `[`, 3)
# Clean up
rm(corpus_chunks, trigram_tokenizer, tFreq_list)
# Garbage collection
gc()

# Create term document matrix for the corpus
quadgram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
process_corpus_chunk <- function(chunk) {
        quadgramtdm <- TermDocumentMatrix(chunk, control = list(tokenize = quadgram_tokenizer))
        tdm_matrix <- as.matrix(quadgramtdm)
        term_frequencies <- rowSums(tdm_matrix)
        qFreq <- data.frame(word = rownames(tdm_matrix), freq = term_frequencies)
        return(qFreq)
}
corpus_chunks <- split(corpus, 1:length(corpus))
qFreq_list <- lapply(corpus_chunks, process_corpus_chunk)
qFreq <- do.call(rbind, qFreq_list)
total_term_frequencies <- aggregate(freq ~ word, data = qFreq, FUN = sum)
qFreq <- merge(qFreq, total_term_frequencies, by = "word", all.x = TRUE)
qFreq <- qFreq[, c("word", "freq.y")]
colnames(qFreq) <- c("word", "total_freq")
qFreq <- unique(qFreq)
qFreq <- qFreq[order(-qFreq$total_freq), ]
qFreq <- subset(qFreq, total_freq >= 3)
qFreq$Word1 <- sapply(strsplit(as.character(qFreq$word), split = ' '), `[`, 1)
qFreq$Word2 <- sapply(strsplit(as.character(qFreq$word), split = ' '), `[`, 2)
qFreq$Word3 <- sapply(strsplit(as.character(qFreq$word), split = ' '), `[`, 3)
qFreq$Word4 <- sapply(strsplit(as.character(qFreq$word), split = ' '), `[`, 4)
# Clean up
rm(corpus_chunks, quadgram_tokenizer, qFreq_list)
# Garbage collection
gc()

# Create term document matrix for the corpus
quintgram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
process_corpus_chunk <- function(chunk) {
        quintgramtdm <- TermDocumentMatrix(chunk, control = list(tokenize = quintgram_tokenizer))
        tdm_matrix <- as.matrix(quintgramtdm)
        term_frequencies <- rowSums(tdm_matrix)
        qtFreq <- data.frame(word = rownames(tdm_matrix), freq = term_frequencies)
        return(qtFreq)
}
corpus_chunks <- split(corpus, 1:length(corpus))
qtFreq_list <- lapply(corpus_chunks, process_corpus_chunk)
qtFreq <- do.call(rbind, qtFreq_list)
total_term_frequencies <- aggregate(freq ~ word, data = qtFreq, FUN = sum)
qtFreq <- merge(qtFreq, total_term_frequencies, by = "word", all.x = TRUE)
qtFreq <- qtFreq[, c("word", "freq.y")]
colnames(qtFreq) <- c("word", "total_freq")
qtFreq <- unique(qtFreq)
qtFreq <- qtFreq[order(-qtFreq$total_freq), ]
qtFreq <- subset(qtFreq, total_freq >= 3)
qtFreq$Word1 <- sapply(strsplit(as.character(qtFreq$word), split = ' '), `[`, 1)
qtFreq$Word2 <- sapply(strsplit(as.character(qtFreq$word), split = ' '), `[`, 2)
qtFreq$Word3 <- sapply(strsplit(as.character(qtFreq$word), split = ' '), `[`, 3)
qtFreq$Word4 <- sapply(strsplit(as.character(qtFreq$word), split = ' '), `[`, 4)
qtFreq$Word5 <- sapply(strsplit(as.character(qtFreq$word), split = ' '), `[`, 5)
# Clean up
rm(corpus_chunks, quintgram_tokenizer, qtFreq_list)
# Garbage collection
gc()

uFreq <- subset(uFreq, select = -word)
bFreq <- subset(bFreq, select = -word)
tFreq <- subset(tFreq, select = -word)
qFreq <- subset(qFreq, select = -word)
qtFreq <- subset(qtFreq, select = -word)

colnames(uFreq)[1] <- 'Counts'
colnames(bFreq)[1] <- 'Counts'
colnames(tFreq)[1] <- 'Counts'
colnames(qFreq)[1] <- 'Counts'
colnames(qtFreq)[1] <- 'Counts'

word_counts_unigram <- uFreq
word_counts_bigram <- bFreq
word_counts_trigram <- tFreq
word_counts_quadgram <- qFreq
word_counts_quintgram <- qtFreq

#clean up
rm(uFreq, bFreq, tFreq, qFreq, qtFreq)

word_counts_unigram$Prob <- word_counts_unigram$Counts/sum(word_counts_unigram$Counts)

kn_trigram <- function(input){
        exp1 <- paste0('^', input[1], '$')
        exp2 <- paste0('^', input[2], '$')
        trigram_row <- word_counts_trigram[
                grepl(exp1, word_counts_trigram$Word1) &
                        grepl(exp2, word_counts_trigram$Word2) == T,]
        if (dim(trigram_row)[1] > 0){
                trigram_row <- trigram_row[1:min(as.integer(0.1*dim(trigram_row)[1]), 
                                                 3), ]        
                totalCounts <- sum(trigram_row$Counts)
                trigram_row$firstTerm <- (trigram_row$Counts - 3)/totalCounts
                
                discFactor <- 3/totalCounts * dim(trigram_row)[1]
                novel_trigram <- sapply(trigram_row$Word3, function(x) {
                        dim(word_counts_trigram[grepl(x,  
                                                      word_counts_trigram$Word3) == T,])[1]
                })
                trigram_row$secondTerm <- ((discFactor * novel_trigram)/
                                                   (dim(word_counts_trigram)[1]))
                trigram_row$Prob <- trigram_row$firstTerm + trigram_row$secondTerm
                trigram_row <- trigram_row[order(trigram_row$Prob, decreasing = T),]
                return(data.frame(Word = trigram_row$Word3, Prob = trigram_row$Prob))
        } else { return(NA) }
}

kn_quadgram <- function(input){
        exp1 <- paste0('^', input[1], '$')
        exp2 <- paste0('^', input[2], '$')
        exp3 <- paste0('^', input[3], '$')
        quadgram_row <- word_counts_quadgram[
                grepl(exp1, word_counts_quadgram$Word1) &
                        grepl(exp2, word_counts_quadgram$Word2) & 
                        grepl(exp3, word_counts_quadgram$Word3) == T,]
        if (dim(quadgram_row)[1] > 0){
                quadgram_row <- quadgram_row[1:min(as.integer(0.2*dim(quadgram_row)[1]), 
                                                   3), ]
                totalCounts <- sum(quadgram_row$Counts)
                quadgram_row$firstTerm <- (quadgram_row$Counts - 3)/totalCounts
                
                discFactor <- 3/totalCounts * dim(quadgram_row)[1]
                novel_quadgram <- sapply(quadgram_row$Word4, function(x) {
                        dim(word_counts_quadgram[grepl(x,  
                                                       word_counts_quadgram$Word4) == T,])[1]
                })
                quadgram_row$secondTerm <- ((discFactor * novel_quadgram)/
                                                    (dim(word_counts_quadgram)[1]))
                quadgram_row$Prob <- quadgram_row$firstTerm + quadgram_row$secondTerm
                quadgram_row <- quadgram_row[order(quadgram_row$Prob, decreasing = T),]
                return(data.frame(Word = quadgram_row$Word4, Prob = quadgram_row$Prob))
        } else { return(NA) }
}

kn_quintgram <- function(input){
        exp1 <- paste0('^', input[1], '$')
        exp2 <- paste0('^', input[2], '$')
        exp3 <- paste0('^', input[3], '$')
        exp4 <- paste0('^', input[4], '$')
        quintgram_row <- word_counts_quintgram[
                grepl(exp1, word_counts_quintgram$Word1) &
                        grepl(exp2, word_counts_quintgram$Word2) & 
                        grepl(exp3, word_counts_quintgram$Word3) & 
                        grepl(exp4, word_counts_quintgram$Word4) == T,]
        if (dim(quintgram_row)[1] > 0){
                quintgram_row <- quintgram_row[1:min(as.integer(0.2*dim(quintgram_row)[1]), 
                                                     3), ]
                totalCounts <- sum(quintgram_row$Counts)
                quintgram_row$firstTerm <- (quintgram_row$Counts - 3)/totalCounts
                
                discFactor <- 3/totalCounts * dim(quintgram_row)[1]
                novel_quintgram <- sapply(quintgram_row$Word5, function(x) {
                        dim(word_counts_quintgram[grepl(x,  
                                                        word_counts_quintgram$Word5) == T,])[1]
                })
                quintgram_row$secondTerm <- ((discFactor * novel_quintgram)/
                                                     (dim(word_counts_quintgram)[1]))
                quintgram_row$Prob <- quintgram_row$firstTerm + quintgram_row$secondTerm
                quintgram_row <- quintgram_row[order(quintgram_row$Prob, decreasing = T),]
                return(data.frame(Word = quintgram_row$Word5, Prob = quintgram_row$Prob))
        } else { return(NA) }
}

stupid_backoff_kn <- function(input){
        list_of_words <- tail(input, 1)
        #exp <- paste0('^', wordStem(list_of_words[1]), '$')
        exp <- paste0('^', list_of_words[1], '$')  # Use the original word, not wordStem
        bigram_row <- word_counts_bigram[grepl(exp, word_counts_bigram$Word1) == T,]
        bigram_row$Prob <- bigram_row$Counts/sum(bigram_row$Counts)
        if (dim(bigram_row)[1] == 0) {
                return(data.frame(Word = word_counts_unigram$Word1, 
                                  Prob = word_counts_unigram$Prob)[1:5,])
        } else {return(data.frame(Word = bigram_row$Word2,
                                  Prob = bigram_row$Prob)[1:5, ])}        
}

kneser_ney <- function(input = NULL){
        
        if (is.null(input)) {
                return(data.frame(Word = word_counts_unigram$Word1,
                                  Prob = word_counts_unigram$Prob)[1:5,]) 
        }
        
        cleaned_input <- gsub("[[:punct:][:blank:]]+", " ", input) # Remove Punctuations
        cleaned_input <- gsub("[']", " ", input) # Remove Interesting Punctuations
        cleaned_input <- gsub("[[:digit:]]", "", cleaned_input) # Remove Numbers
        cleaned_input <- cleaned_input %>% 
                tolower() %>% # Converting words to lower case
                trimws() # Remove Whitespaces
        list_of_words <- unlist(strsplit(cleaned_input, split = ' '))
        if (length(list_of_words) == 0) {
                return(data.frame(Word = word_counts_unigram$Word1,
                                  Prob = word_counts_unigram$Prob)[1:5,])
        } else if (length(list_of_words) == 1) {
                return(stupid_backoff_kn(cleaned_input))
        } else if (length(list_of_words) == 2) {
                if (anyNA(kn_trigram(list_of_words))) {
                        return(stupid_backoff_kn(cleaned_input))
                }  else { return(kn_trigram(list_of_words)) }
        } else if (length(list_of_words) == 3) {
                if (anyNA(kn_quadgram(list_of_words))) {
                        if (anyNA(kn_trigram(tail(list_of_words, 2)))) {
                                return(stupid_backoff_kn(cleaned_input))
                        } else { return(kn_trigram(tail(list_of_words, 2))) }
                } else { return(kn_quadgram(tail(list_of_words, 3))) }
        } else {
                if (anyNA(kn_quintgram(tail(list_of_words, 4)))) {
                        if (anyNA(kn_quadgram(tail(list_of_words, 3)))) {
                                if (anyNA(kn_trigram(tail(list_of_words, 2)))) {
                                        return(stupid_backoff_kn(cleaned_input))
                                } else { return(kn_trigram(tail(list_of_words, 2))) }
                        } else { return(kn_quadgram(tail(list_of_words, 3))) }
                } else { return(kn_quintgram(tail(list_of_words, 4))) }
        }
}