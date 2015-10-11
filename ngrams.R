library(data.table)
library(slam)
library(tm)
library(RWeka)
ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 4))

tdm = messages[!is.na(Name), .(Name, Type, cleantext)]
tdm = split(tdm$cleantext, paste(tdm$Name, tdm$Type))
tdm = lapply(tdm, function(v) Corpus(VectorSource(v)))
tdm = lapply(tdm, function(x) TermDocumentMatrix(x, control = list(tokenize = ngramTokenizer)))
tdm = lapply(tdm, function(m) slam::row_sums(m))
tdm = lapply(tdm, function(av) data.table(phrase = names(av), N = av))
tdm = lapply(tdm, function(dt) dt[N > 1][order(-N)])

save(tdm, file = "ngrams.rda")
