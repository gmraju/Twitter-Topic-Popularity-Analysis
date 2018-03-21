library(dplyr)
library(ggplot2)
library(stringr)
library(mapproj)
library(fiftystater)
library(RColorBrewer)
library(tm)

setwd("/sscc/home/o/oaz467/EECS510/Frappuccino_Data")

####### create dataset #######

#read in all files in directory
#these are the csv files with polarity added
filenames <- dir()
fraplist <- lapply(filenames,read.csv, stringsAsFactors = FALSE, colClasses = "character")
#append data frames to each other
frap <- bind_rows(fraplist)

####### format date and time #######
filtered_dt <- substr(frap$created_at, 5,19)
time        <- as.POSIXct(filtered_dt, tz = "UTC", format = "%b %d %H:%M:%S")
frap$time   <- time
frap$day    <- substr(frap$time, 6,10)
frap$hour   <- substr(frap$time, 12,13)

####### remove incomplete day #######
frap <- filter(frap, day != "05-31")
frap <- filter(frap, day != "05-04")

####### format polarity and subjectivity #######
frap$Polarity     <- as.numeric(frap$Polarity)
frap$Subjectivity <- as.numeric(frap$Subjectivity)

####### time series average polarity #######

polarity_by_hour <- frap %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE))

polarity_by_day <- frap %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE))

ggplot(polarity_by_hour, aes(hour, avg, group = 1)) + geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept = 0)+
  ggtitle("Average Polarity by Hour for All Frappuccino Tweets")+
  ylab("Average Polarity")

ggplot(polarity_by_day, aes(day, avg, group = 1)) + geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept = 0)+
  ggtitle("Average Polarity by Day for All Frappuccino Tweets")+
  ylab("Average Polarity")+
  geom_vline(xintercept = 15)

####### time series count #######

tweets_by_day <- frap %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n())

tweets_by_hour <- frap %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(tweets = n())

ggplot(tweets_by_day, aes(day, tweets, group = 1)) + geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Number of Tweets by Day for All Frappuccino Tweets")+
  ylab("Frequency")+
  geom_vline(xintercept = 15)

ggplot(tweets_by_hour, aes(hour, tweets, group = 1)) + geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Number of Tweets by Hour for All Frappuccino Tweets")+
  ylab("Frequency")

####### exploring location fields #######
unique(frap$place.full_name)
unique(frap$place.name)
unique(frap$place.country)
unique(frap[frap$place.country == "United States", "place.full_name"])

####### extracting tweets from US #######
USA_tweets <- filter(frap, place.country == "United States")

####### extracting state from US tweets #######
USA_tweets$loc_last_char <- str_sub(USA_tweets$place.full_name, start = -2)
USA_tweets$state_abb     <- ifelse(USA_tweets$loc_last_char %in% state.abb, USA_tweets$loc_last_char, NA)
USA_tweets$state         <- sapply(USA_tweets$state_abb,  function(x) ifelse(is.na(x), NA, state.name[grep(x, state.abb)]))
USA_tweets               <- filter(USA_tweets, !is.na(state))
USA_tweets$state         <- tolower(USA_tweets$state)

####### average state polarity #######
USA_polarity_avg <- USA_tweets %>%
  group_by(state) %>%
  summarise(Avg = mean(Polarity, na.rm = TRUE))

####### insert NA for missing states #######
missing_states <- tolower(state.name[!tolower(state.name) %in% USA_polarity_avg$state])
ms_df          <- data.frame(state = missing_states, Avg = NA)

USA_polarity_avg <- rbind(USA_polarity_avg, ms_df)

####### plot average state polarity #######
q <- ggplot(USA_polarity_avg, aes(map_id = state)) + 
  geom_map(aes(fill = Avg),
           map = fifty_states,
           color = "black",
           size = 0.2) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

q + fifty_states_inset_boxes() +
  scale_fill_gradient2(low = "red",
                      high = "blue",
                      mid = "white",
                      limits = c(-1,1),
                      na.value = "lightgrey") +
  ggtitle("Average Frappuccino Polarity by State")

####### plot normalized tweet intensity by state #######
# obtain state population data #
statePop <- read.csv("/sscc/home/o/oaz467/EECS510/state_populations.csv", header = FALSE, stringsAsFactors = FALSE)
statePop$V1 <- tolower(statePop$V1)
statePop$V3 <- as.numeric(gsub(",","", statePop$V3))

USA_tweet_freq <- USA_tweets %>%
  group_by(state) %>%
  summarise(Freq = n())

# add state population data and missing states #
USA_tweet_freq <- left_join(USA_tweet_freq, select(statePop, V1, V3), by = c("state" = "V1"))
USA_tweet_freq <- mutate(USA_tweet_freq, intensity = Freq*1000000/V3)
ms_df2         <- data.frame(state = missing_states, Freq = NA, V3 = NA, intensity = NA)
USA_tweet_freq <- rbind(USA_tweet_freq, ms_df2)

freqplot <- ggplot(USA_tweet_freq, aes(map_id = state)) + 
  geom_map(aes(fill = intensity),
           map = fifty_states,
           color = "black",
           size = 0.2) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

freqplot + fifty_states_inset_boxes() +
  scale_fill_gradient(low = "white",
                       high = "blue",
                       limits = c(0,10),
                       na.value = "lightgrey") +
  ggtitle("Normalized Frappuccino Tweet Intensity by State")

####### user stats #######
frap %>%
  arrange(desc(as.numeric(user.followers_count))) %>%
  select(user.screen_name, user.name) %>%
  unique() %>%
  head(50)

top_tweeters <- count(frap, user.screen_name, sort = TRUE)
#write.csv(top_tweeters, file = "/sscc/home/o/oaz467/EECS510/top_tweeters.csv")

most_retweeted <- count(frap, retweeted_status.user.screen_name, sort = TRUE)
#write.csv(most_retweeted, file = "/sscc/home/o/oaz467/EECS510/most_retweeted.csv")

####### create subsets for frap flavors #######
unicorn  <- frap[grepl("unicorn", frap$text, ignore.case = TRUE),]
smore    <- frap[grepl("smore", frap$text, ignore.case = TRUE),]
mint     <- frap[grepl("midnight mint|midnightmint", frap$text, ignore.case = TRUE),]
mocha    <- frap[grepl("mocha", frap$text, ignore.case = TRUE) & 
                  (!grepl("midnight mint|midnightmint", frap$text, ignore.case = TRUE)),]
greentea <- frap[grepl("green tea|greentea", frap$text, ignore.case = TRUE),]

flavor_list <- list(frap, unicorn, smore, mint, mocha, greentea)

flavors <- c("Frappuccino overall", "Unicorn", "Smore's", "Midnight mint mocha",
             "Mocha", "Green Tea")

####### flavor polarity comparison #######

fl_pol <- unlist(lapply(flavor_list, function(x) mean(x$Polarity, na.rm = TRUE)))
flavor_polarities <- data.frame(flavor = flavors, Avg_polarity = fl_pol)

ggplot(flavor_polarities, aes(flavor))+
  geom_bar(aes(weight = Avg_polarity), fill = c("chartreuse",
                                               "burlywood4", "darkolivegreen",
                                               "wheat4", "goldenrod4", "hotpink2"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Average Polarity")+
  ggtitle("Comparison of Frappuccino Flavor Sentiment")+
  xlab("Flavor")

polarity_by_day <- polarity_by_day %>%
  mutate(group = "Frappuccino overall")

unicorn_pol_day <- unicorn %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE)) %>%
  mutate(group = "Unicorn")

smore_pol_day <- smore %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE)) %>%
  mutate(group = "Smore's")

mint_pol_day <- mint %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE)) %>%
  mutate(group = "Midnight mint mocha")

mocha_pol_day <- mocha %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE)) %>%
  mutate(group = "Mocha")

greentea_pol_day <- greentea %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE)) %>%
  mutate(group = "Green Tea")


flavor_polarity_days <- rbind(unicorn_pol_day,
                         smore_pol_day,
                         mint_pol_day,
                         mocha_pol_day,
                         greentea_pol_day)

ggplot(flavor_polarity_days, aes(day, avg, group = group)) +
  geom_line(aes(color = group), size = 1.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(limits = c(-0.25,0.75)) +
  theme(legend.position="bottom")+
  geom_hline(yintercept = 0)+
  scale_color_manual(values=c("chartreuse",
                             "darkolivegreen",
                             "wheat4", "goldenrod4", "hotpink2"))+
  geom_vline(xintercept = 15)+
  ggtitle("Frappuccino Flavor Sentiment by Day")+
  ylab("Average Polarity")+
  xlab("Day")

####### most common languages #######
lang <- as.data.frame(sort(table(frap$lang),decreasing=T))
lang_key <- read.csv("/sscc/home/o/oaz467/EECS510/lang.csv", header = TRUE, stringsAsFactors = FALSE)
lang <- left_join(lang, lang_key, by = c("Var1" = "Language_.code"))

ggplot(lang, aes(Name))+
  geom_bar(aes(weight = Freq))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_log10()+
  ylab("Log number of Tweets")+
  xlab("Language")+
  ggtitle("Language Distribution for 'Frappuccino'")

####### flavor frequency comparison #######

unicorn_tweet_day <- unicorn %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n()) %>%
  mutate(group = "Unicorn")

smore_tweet_day <- smore %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n()) %>%
  mutate(group = "Smore's")

mint_tweet_day <- mint %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n()) %>%
  mutate(group = "Midnight mint mocha")

mocha_tweet_day <- mocha %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n()) %>%
  mutate(group = "Mocha")

greentea_tweet_day <- greentea %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n()) %>%
  mutate(group = "Green Tea")

flavor_count_days <- rbind(unicorn_tweet_day,
                              smore_tweet_day,
                              mint_tweet_day,
                              mocha_tweet_day,
                              greentea_tweet_day)

ggplot(flavor_count_days, aes(day, tweets, group = group)) +
  geom_line(aes(color = group), size = 1.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_y_continuous(limits = c(-0.25,0.75)) +
  theme(legend.position="bottom")+
  #geom_hline(yintercept = 0)+
  scale_color_manual(values=c("chartreuse",
                              "darkolivegreen",
                              "wheat4", "goldenrod4", "hotpink2"))+
  geom_vline(xintercept = 15)+
  ylab("Frequency")+
  xlab("Day")+
  ggtitle("Tweet Frequency by Day for Frappuccino Flavors")

####### happy hour frequency and polarity #######

happyhr  <- frap[grepl("happy hour|happyhour", frap$text, ignore.case = TRUE),]
happyhr_valid_days <- filter(happyhr, day %in% c("05-05","05-06","05-07","05-08",
                                                 "05-09","05-10","05-11","05-12","05-13","05-14"))

happyhr_by_day <- happyhr %>%
  filter(!is.na(day)) %>%
  group_by(day) %>%
  summarise(tweets = n())

happyhr_by_hour <- happyhr_valid_days %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(tweets = n())

ggplot(happyhr_by_day, aes(day, tweets, group = 1)) + geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Tweet Frequency by Day for Frappuccino Happy Hour")+
  ylab("Frequency")+
  xlab("Day")+
  geom_vline(xintercept = 15)
  

ggplot(happyhr_by_hour, aes(hour, tweets, group = 1)) + geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Tweet Frequency by Hour for Frappuccino Happy Hour",
          subtitle = "May 5-14")+
  ylab("Frequency")+
  xlab("Hour")

happyhr_polarity_by_hr <- happyhr %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(avg = mean(Polarity, na.rm = TRUE))

ggplot(happyhr_polarity_by_hr, aes(hour, avg, group = 1)) + geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Average Polarity by Hour for Frappuccino Happy Hour",
          subtitle = "May 5-14")+
  ylab("Average Polarity")+
  xlab("Hour")

####### total positive and negative #######

TopTweets <- frap %>%
  filter(Polarity == 1)

LowTweets <- frap %>%
  filter(Polarity == -1)

pols <- filter(frap, !is.na(Polarity)) %>%
  select(Polarity)
sign <- sapply(pols$Polarity, function(x) ifelse(x > 0, "Positive", ifelse(x<0, "Negative", "Neutral")))

ggplot(as.data.frame(sign), aes(sign))+geom_bar(fill = c("red", "grey50", "blue"))+
  ggtitle("Frequency of Polarity Signs")+
  ylab("Frequency")+
  xlab("Sign")

ggplot(frap, aes(Polarity)) + geom_histogram() +
  ggtitle("Histogram of Polarity for All Frappuccino Tweets")+
  ylab("Frequency")

nrow(TopTweets)
nrow(unique(TopTweets))
nrow(LowTweets)

tweetCorpus <- Corpus(VectorSource(frap$text))

tweetDTM <- DocumentTermMatrix(tweetCorpus)

m = as.matrix(tweetDTM)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

dm = data.frame(word=names(word_freqs), freq=word_freqs)
head(dm)

# hashtag analysis
hashtags_list <- str_extract_all(frap$text, "#\\S+")
hashtags <- data.frame(X = unlist(hashtags_list))

hashTable <- count(hashtags, X, sort = TRUE)
tweetTable <- count(frap, text, sort = TRUE)

#write.csv(hashTable, file = "/sscc/home/o/oaz467/EECS510/hashtags.csv")
#write.csv(tweetTable, file = "/sscc/home/o/oaz467/EECS510/tweet_freq.csv")


countryTable <- count(frap, place.country, sort = TRUE)
#write.csv(countryTable, file = "../CountryTable.csv", row.names = FALSE)

## investigating unicorn anomalies ##
count(filter(unicorn, day == "05-18"), text, sort = TRUE)
count(filter(unicorn, day == "05-28"), text, sort = TRUE)
count(filter(unicorn, day == "05-19"), text, sort = TRUE)
View(count(filter(unicorn, day == "05-05"), text, sort = TRUE))

unicorn %>%
  filter(day == "05-05") %>%
  count(text, sort = TRUE) %>%
  View()

#write.csv(frap, "/sscc/home/o/oaz467/EECS510/combined_data.csv", row.names = FALSE)
