word_count_texcount <- function(file_path) {
  # Call texcount from TinyTeX to count words
  result <- system2("texcount", args = c("-total", "-1", file_path), stdout = TRUE)
  
  # Extract the main count (line containing "Words in text:")
  main_count <- grep("Words in text:", result, value = TRUE)
  
  # Extract the number from the line and convert to integer
  count <- as.integer(gsub("[^0-9]", "", main_count))
  
  return(count)
}

word_count_texcount <- function(file_path) {
  result <- system2("texcount", args = c("-total", "-1", file_path), stdout = TRUE)
  cat(result, sep = "\n")  # See what texcount is actually returning
}

getwd()
setwd("/home/ben/Documents/Theory of aggregate supply and shocks")
list.files()
file_path <- "A-theoretical-derivation-of-aggregate-supply-and-demand.tex"
word_count_texcount(file_path)


# word_count_tex <- function(file_path) {
#   # Read the .tex file
#   lines <- readLines(file_path, warn = FALSE)
#   
#   # Stop processing at the references section
#   ref_line <- grep("\\\\section\\*\\{References}\\s*\\\\label\\{references}", lines)
#   if (length(ref_line) > 0) {
#     lines <- lines[1:(ref_line[1] - 1)]
#   }
#   
#   # Collapse into one string
#   text <- paste(lines, collapse = " ")
#   
#   # Remove LaTeX equations (inline: $...$, display: \[...\], equation environments)
#   text <- gsub("\\$\\$.*?\\$\\$", "", text)
#   text <- gsub("\\$[^$]+\\$", "", text)
#   text <- gsub("\\\\[\\[].*?\\\\[]]", "", text)
#   text <- gsub("\\\\begin\\{equation\\*?}.*?\\\\end\\{equation\\*?}", "", text)
#   text <- gsub("\\\\begin\\{align\\*?}.*?\\\\end\\{align\\*?}", "", text)
#   
#   # Remove LaTeX commands like \textbf{...} or \section{...}
#   text <- gsub("\\\\[a-zA-Z]+\\*?(\\[[^\\]]*\\])?(\\{[^}]*\\})?", "", text)
#   
#   # Remove remaining braces and special characters
#   text <- gsub("[{}]", "", text)
#   
#   # Split and count words
#   words <- unlist(strsplit(text, "\\s+"))
#   words <- words[words != ""]
#   length(words)
# }
# 
# 
# file_path <- "/home/ben/Documents/Theory of aggregate supply and shocks/A-theoretical-derivation-of-aggregate-supply-and-demand.tex"
# word_count_tex(file_path)
# getwd()
# # list.files("/home/ben/Documents/Theory of aggregate supply and shocks")
# 
# word_count_rmd <- function(file_path) {
#   # Read the file
#   lines <- readLines(file_path, warn = FALSE)
#   
#   # Remove code chunks
#   in_chunk <- FALSE
#   lines_clean <- c()
#   for (line in lines) {
#     if (grepl("^```", line)) {
#       in_chunk <- !in_chunk
#     } else if (!in_chunk) {
#       lines_clean <- c(lines_clean, line)
#     }
#   }
#   
#   # Remove inline R code: `r ...`
#   lines_clean <- gsub("`r[^`]*`", "", lines_clean)
#   
#   # Remove everything after "# References {-}"
#   ref_line <- grep("^# References \\{-\\}", lines_clean)
#   if (length(ref_line) > 0) {
#     lines_clean <- lines_clean[1:(ref_line[1] - 1)]
#   }
#   
#   # Collapse into a single string
#   text <- paste(lines_clean, collapse = " ")
#   
#   # Remove LaTeX-style math: $...$, $$...$$, \[...\]
#   text <- gsub("\\$\\$[^\\$]+\\$\\$", "", text)
#   text <- gsub("\\$[^\\$]+\\$", "", text)
#   text <- gsub("\\\\[\\[].*?\\\\[]]", "", text)
#   
#   # Remove markdown formatting characters
#   text <- gsub("[*_#~`\\[\\]()<>|]|\\{|\\}", "", text)
#   
#   # Split and count words
#   words <- unlist(strsplit(text, "\\s+"))
#   words <- words[words != ""]
#   length(words)
# }
# 
# file_path <- "/home/ben/Documents/Theory of aggregate supply and shocks/A theoretical derivation of aggregate supply and demand.Rmd"
# word_count_rmd(file_path)
