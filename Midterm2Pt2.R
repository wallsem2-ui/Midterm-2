# Midterm Problem 2
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(ggwordcloud)
library(patchwork)

alllyrics <- read.csv("https://github.com/adashofdata/taylor_swift_data/raw/refs/heads/main/Taylor_Swift_Genius/taylor_swift_genius_data.csv")
red_words <- alllyrics %>%
  filter(Album == "Red") %>%
  unnest_tokens(word, Lyrics)
folklore_words <- alllyrics %>%
  filter(Album == "folklore") %>%
  unnest_tokens(word, Lyrics)

stopwords <- get_stopwords()
str(stopwords)
as.vector(stopwords$word)

red_words <- red_words %>%
  filter(!word %in% stopwords$word)
folklore_words <- folklore_words %>%
  filter(!word %in% stopwords$word)

#Word Clouds
red_pop_words <- red_words %>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarise(N= n()) %>%
  arrange(desc(N))%>%
  slice(1:100)
folklore_pop_words <- folklore_words %>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarise(N= n()) %>%
  arrange(desc(N))%>%
  slice(1:100)

wc1 <- ggplot(red_pop_words, aes(label=word, size=N))+
  geom_text_wordcloud()+
  scale_size_area(max_size=15)+
  theme_minimal()

wc2 <- ggplot(folklore_pop_words, aes(label=word, size=N))+
  geom_text_wordcloud()+
  scale_size_area(max_size=10)+
  theme_minimal()

spotify <- read.csv("https://github.com/adashofdata/taylor_swift_data/raw/refs/heads/main/Taylor_Swift_Spotify/taylor_swift_spotify_data.csv")

#Sent Analysis
word_sentiments <- get_sentiments("bing")
red_words_sent <- red_words %>%
  inner_join(word_sentiments, by="word")
folklore_words_sent <- folklore_words %>%
  inner_join(word_sentiments, by="word")

albums_sent <- bind_rows(red_words_sent, folklore_words_sent)

sentplot <- ggplot(albums_sent) +
  geom_bar(aes(x=Album, fill=sentiment), position="fill") +
  coord_cartesian(expand=F) +
  scale_fill_brewer(palette="Blues") +
  annotate("text", x=1, y=0.7, label="Negative\nWords") +
  annotate("text", x=2, y=0.2, label="Positive\nWords") +
  labs(title="Breakdown of Positive and negative words in two of Taylor Swift's albums") +
  theme_minimal() +
  theme(axis.title=element_blank(),
        legend.position="none")

#sent and attributes compared (at least 2plots)

spotify <- spotify %>%
  filter(Album %in% c("Red", "folklore"))

sent <- bind_rows(red_words_sent, folklore_words_sent)

dance <- spotify %>%
  select(Album, Song.Name, Danceability)%>%
  left_join(sent, by="Album")
s1 <- ggplot(dance) +
  geom_point(aes(x = Danceability, y = sentiment, color=Album)) +
  labs(title = "Distribution of Danceability by Sentiment", y="Sentiment") +
  theme_minimal()

tempo <- spotify %>%
  select(Album, Tempo)%>%
  left_join(sent, by="Album")
s2 <- ggplot(tempo) +
  geom_point(aes(x = Tempo, y = sentiment, color=Album)) +
  labs(title = "Distribution of Tempo by Sentiment", y="Sentiment") +
  theme_minimal()

energy <- spotify %>%
  select(Album, Song.Name, Energy)%>%
  left_join(sent, by="Album")
s3 <- ggplot(energy) +
  geom_point(aes(x = Energy, y = sentiment, color=Album)) +
  labs(title = "Distribution of Energy by Sentiment", y="Sentiment") +
  theme_minimal()

loudness <- spotify %>%
  select(Album, Song.Name, Loudness)%>%
  left_join(sent, by="Album")
s4 <- ggplot(loudness) +
  geom_point(aes(x = Loudness, y = sentiment, color=Album)) +
  labs(title = "Distribution of Loudness by Sentiment", y="Sentiment") +
  theme_minimal()

#DASHBOARDS

dash4 <- wc1 / wc2
dash5 <- s3 + s4
dash6 <- (s1 + s2) / sentplot

ggsave(filename="dash4.png", plot=dash4,
       dpi=600, width=15, height=10)
ggsave(filename="dash5.png", plot=dash5,
       dpi=600, width=15, height=10)
ggsave(filename="dash6.png", plot=dash6,
       dpi=600, width=15, height=10)

Part2Dash <- wc1 + wc2 / sentplot +s1 + s2 / s3 + s4
ggsave(filename="Part2Dash.png", plot=Part2Dash,
       dpi=600, width=15, height=10)
