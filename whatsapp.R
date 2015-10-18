# libraries
library(ggplot2)
library(RSQLite)
library(data.table)
library(stringr)
library(lubridate)
library(reshape2)
library(scales)
library(usefuncs)
library(tm)
library(magrittr)

# global params
kCommonContactsLimit = 15
me = TRUE

# set up db connection
dbdrv = dbDriver("SQLite")
db = dbConnect(dbdrv, "./ChatStorage.sqlite")
messages = setDT(dbReadTable(db, "ZWAMESSAGE"))

# find common contacts
contacts = messages[, .N, .(ZFROMJID, ZPUSHNAME)][!is.na(ZFROMJID) & N > kCommonContactsLimit]
contacts = contacts[!str_detect(ZFROMJID, "-"), .(ZFROMJID, ZPUSHNAME)]

# split group messages
messages = messages[, .(ZISFROMME, ZMESSAGEDATE, ZFROMJID, ZPUSHNAME, ZSTANZAID, ZTEXT, ZTOJID)]
messages[, ZSPLITSTANZAA := str_extract(ZSTANZAID, "[0-9]+")]
messages[, ZSPLITSTANZAB := gsub("-", "", str_extract(ZSTANZAID, "-[0-9]+"))]
messages[, ZID := ifelse(is.na(ZFROMJID), ZTOJID, ZFROMJID)]
messages[, ZISGROUPCHAT := str_detect(ZID, "-")]
gmessages = messages[ZISGROUPCHAT == TRUE]
messages = messages[ZISGROUPCHAT == FALSE]

# join name to whatsapp contact id
setkey(contacts, ZFROMJID)
setkey(messages, ZID)
messages = contacts[messages]

# parse date and type
messages[, DATETIME := as.character(as.POSIXct(ZMESSAGEDATE, origin = "2001-01-01"))]
messages[, DATE := str_extract(DATETIME, "[0-9]{4}-[0-9]{2}-[0-9]{2}")]
messages[, TIME := str_extract(DATETIME, "[0-9]{2}:[0-9]{2}:[0-9]{2}")]
messages[, ZISFROMME := as.character(ZISFROMME)]
messages[, ZISFROMME := ifelse(ZISFROMME == 1, "Sent", "Received")]

# readable format
messages = messages[, .(`Phone Number` = ZFROMJID, 
             Name = ZPUSHNAME, 
             RowID = NA_integer_,
             Date = DATE,
             Service = "Whatsapp", 
             Type = ZISFROMME,
             `Date Read/Sent` = DATETIME, 
             Text = ZTEXT,
             Time = TIME)]

# extract day and characters
messages[, day := weekdays(as.Date(Date))]
messages[, day := factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))]
messages[, nchar := nchar(Text)]

## my messages concatenated with SMS messages
if (me) { load("messages.rda") }

# find common contacts
common = messages[!is.na(Name), .N, Name][order(-N)][1:kCommonContactsLimit, Name]

# cast summary of texts and characters 
ntexts = dcast.data.table(messages, Name ~ Type)
nchars = dcast.data.table(messages, Name ~ Type, value.var = "nchars", fun.aggregate = sum)
setnames(ntexts, names(ntexts), c("Name", "Received Texts", "Sent Texts"))
setnames(nchars, names(nchars), c("Name2", "Received chars", "Sent chars"))
summary = cbind(ntexts, nchars)[, Name2 := NULL]
summary[, `Texts Ratio` := `Received Texts`/`Sent Texts`]
summary[, `Character Ratio` := `Received chars`/`Sent chars`]
summary[, Index := `Texts Ratio`*`Character Ratio`]
summary = summary[!is.na(Name)]

# preprocess text
messages[, cleantext := PrepareFreetext(Text)]
words = messages[Name %in% common, data.table(word = unlist(strsplit(cleantext, " "))), Type][, .N, .(Type, word)]

# process punctuation
messages[, punct := do.call(rbind, lapply(str_extract_all(Text, "[[:punct:]]+"), paste, collapse = " "))]
punct = unique(messages[Name != "", .(punct, Type, Name)][, 
                                                          data.table(punct = unlist(strsplit(punct, " ")))[, 
                                                                                                           .(pN = .N), punct], .(Name, Type)])
punct[, tN := sum(pN), .(Name, Type)]
sumpunct = punct[, .(N = sum(pN)), punct][order(-N)]

# PLOT which days do texts happen on
p_whentext = ggplot(messages[!is.na(Name), .N, .(Service, day)]) + 
  geom_bar(aes(x = day, y = N), stat = "identity", fill = "steelblue") + facet_grid(~Service) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                    legend.position = "none") +
  xlab("Day") + ylab("Count") + ggtitle("When do I Text?")

# PLOT numbers of texts facet
summary[, Name := reorder(Name, -(`Received Texts` + `Sent Texts`))]
p_ntexts = ggplot(melt(summary[Name %in% common, 
                               .(Name, Received = `Received Texts`, 
                                 Sent = `Sent Texts`)], id.vars = "Name")) +
  geom_bar(aes(x = Name, y = value, fill = variable), stat = "identity") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                     legend.position = "none") +
  facet_wrap(~variable) + ylab("Count") + ggtitle("Who do I Text?")

# PLOT neediness index
summary[, Name := reorder(Name, -Index)]
p_index = ggplot(melt(summary[Name %in% common, 
                    .(Name, `Texts Ratio`, `Character Ratio`, 
                      Index)], id.vars = "Name"), aes(x = Name, y = value)) + 
  geom_bar(fill = "steelblue", stat = "identity") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  facet_grid(~variable) +
  xlab("Name") + ylab("Received/Sent Ratio") +
  ggtitle("Ratio of texts recieved to sent")       

# PLOT inverse neediness index 
summary[, Name := reorder(Name, Index)]
p_invindex = ggplot(melt(summary[Name %in% common, 
                              .(Name, `Texts Ratio`, `Character Ratio`, 
                                Index)], id.vars = "Name"), aes(x = Name, y = 1/value)) + 
  geom_bar(fill = "steelblue", stat = "identity") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  facet_grid(~variable) +
  xlab("Name") + ylab("Received/Sent Ratio") +
  ggtitle("Ratio of texts recieved to sent")

# PLOT histogram by time, binwidth 15mins
p_whenhist = ggplot(messages) + 
  geom_histogram(aes(strptime(Time, format = "%H:%M:%S")), binwidth = 60 * 60 / 4, fill = "steelblue") +
  theme_bw() +  scale_x_datetime(labels = date_format("%H:%M")) +
  xlab("Time") + ylab("Number of Texts") + ggtitle("When Do I Get Text?") +
  facet_grid(Type ~ ., margins = TRUE)

# PLOT histogram by time, binwidth 30mins by person
p_whowhenhist = ggplot(messages[Name %in% common]) +
  geom_histogram(aes(strptime(Time, format = "%H:%M:%S"), fill = Type), binwidth = 3600/2) +
  theme_bw() + scale_x_datetime(labels = date_format("%H:%M")) + theme(legend.position = "none") +
  xlab("Time") + ylab("Number of Texts") + ggtitle("When Do I Get Text? (Part 2)") +
  facet_grid(Name ~ Type)

# PLOT total punctuation in messages
sumpunct[, punct := reorder(punct, -N)]
p_sumpunct = ggplot(sumpunct[N > 5]) +
  geom_bar(aes(x = punct, y = N), fill = "steelblue", stat = "identity") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                     legend.position = "none") +
  xlab("Punctuation") + ylab("Count") + ggtitle("What punctuation gets used?")

# PLOT heatmap of punctuation
p_punctheat = ggheatmap(punct[punct != "£" & Type == "Received"], 
          yaxis = "Name", 
          xaxis = "punct", 
          Intensity = "pN", 
          logFlag = FALSE, 
          rotateFlag = FALSE,
          minGroupBound = 10) + xlab("Punctuation") + ylab("Name") + ggtitle("Heatmap of Punctuation People Like")

# PLOT heatmap of punctuation logged
p_punctheatlog = ggheatmap(punct[punct != "£" & Type == "Received"], 
          yaxis = "Name", 
          xaxis = "punct", 
          Intensity = "pN", 
          logFlag = TRUE, 
          rotateFlag = FALSE,
          minGroupBound = 10) + xlab("Punctuation") + ylab("Name") + ggtitle("Heatmap of Punctuation People Like")

plots = list(
p_whentext,
p_ntexts,
p_index,
p_invindex,
p_whenhist,
p_whowhenhist,
p_sumpunct,
p_punctheat,
p_punctheatlog)

save(plots, file = "plots.rda")
