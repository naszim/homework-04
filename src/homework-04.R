#######################
##Házi feladat 4.     #
##Programozás I.      #
##2016/17. II. félév  #
##Nagy Szimonetta     #
##2017.04.29.         #
#######################

##II.feladat
##1.)A data mappában találsz egy clinton_trump_tweets.csv nevű 
##fájlt. Ezt hívd be tweets néven.
tweets <- read.csv2("data/clinton_trump_tweets.csv", stringsAsFactors = F)
View(tweets)
##2.)Kérd le, hány tweet származik Hillary Clintontól és hány Donald Trumptól! 
##Ezt ábrázold is a fig/sample/tweet1.png alapján, majd mentsd ki a plotot
##a fig mappába tweet1.png néven!
hany_tweet <- summary(tweets$handle)
hany_tweet <- table(tweets$handle)
View(hany_tweet)
ggplot(tweets, aes(x=handle, fill=handle)) + 
geom_bar() +
  scale_fill_manual(values=c("blue", "red"), labels=c("Hillary Clinton", 
                                                      "Donald Trump")) +
  theme(axis.text.x = element_blank()) + 
  xlab(" ") + ylab("Tweet frequency") + labs(fill="Candidate")
rm(hany_tweet)
##3/1.)Kérd le, milyen nyelveken írta a tweeteket a két jelölt külön-külön! 
##Nézd meg, hogy a nem angol szövegek valóban nem angolok-e. Ha úgy gondolod, 
##hogy rosszul azonosították a nyelvet, akkor írd át az általad helyesnek 
##váltre. A spanyol nyelvű tweeteket nem kell végigböngészned, mert sok van, de 
##a többit mindenképp nézd meg.

#először subseteltem a df-et úgy, hogy amikor Viewerben megnyitom, lássam egymás
#mellett, hogy hanyadik sorban, milyen tweethez milyen nyelvet rendeltek. Ezután
#a lang oszlop fejlécére kattintva rendeztem nyelv szerint, és átírtam az
#eredeti df-ben, amit kellett. A végén eltávolítottam a felesleges df-eket.
hillary_lang <- subset.data.frame(tweets, tweets$handle == "HillaryClinton",
                                  select = c(handle, text, lang))
donald_lang <- subset.data.frame(tweets, tweets$handle == "realDonaldTrump",
                                 select = c(handle, text, lang))
View(hillary_lang)
View(donald_lang)
tweets[237, 11] <- "en"
tweets[6059, 11] <- "en"
tweets[60564, 11] <- "en"
tweets[6087, 11] <- "en"
rm(hillary_lang, donald_lang)

##3./2.)Ábrázold a két jelölt által használt nyelvek gyakoriságát a 
##fig/sample/tweet2.png szerint, majd mentsd ki a plotot a fig mappába 
##tweet2.png néven! Az oszlopszínek legyenek "darkgrey" és "cornflowerblue". 
##(A nyelvek gyakorisága eltérhet az előző lépés miatt.)

twlang <- cbind(tweets$handle, tweets$lang)
kt_twlang <- table(twlang[,1], twlang[,2])
kt_twlang <- as.data.frame(kt_twlang)
colnames(kt_twlang) <- c("handle", "lang", "freq")

ggplot(kt_twlang, aes(x=lang, y=freq, fill = handle)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkgrey", "cornflowerblue"),
                    labels=c("Hillary Clinton", "Donald Trump")) +
  xlab("Language") + ylab("Frequency") + labs(fill="Candidate")
rm(kt_twlang, twlang)

#4.) Írj egy függvényt, ami a "Hillary Clinton" bemenet esetében Hillary Clinton,
#"Donald Trump" bemenet esetében pedig Donald Trump tweetjeit adja ki a 
#retweet_count és a favorite_count összesített értéke alapján csökkenő 
#sorrendben. Legyen egy olyan argumentum is, amivel a kiadott tweetek számát 
#lehet meghatározni. Az argumentumoknak legyen default értéke. A függvény írjon 
#ki hibaüzenetet, ha rossz nevet kap. Ha a kiadott tweetek száma meghaladja a 
#lehetséges tweetek számát, írja ki a tweetek max számát. Mikor a függvény 
#kiadja a tweeteket, írja ki, hogy kinek a tweetjeit írja ki és mennyit. Kérd le
#a függvénnyel Hillary Clinton első 10, valamint Donald Trump első 15 legtöbbet 
#retweetelt és kedvelt tweetjét.
source("src/homework-04-functions.R")
twitter.campaign("Hillary Clinton", 10)
twitter.campaign("Donald Trump", 15)
##FELADAT VÉGE------------------------------------------------------------------

##IV. feladat
#1.) Hasonlítsd össze Hillary Clinton és Donald Trump tweetjeinek szentimentjeit 
#és emócióit. Vizsgáld meg oszlopdiagrammal és idősoros ábrával az eltéréseket. 
#Nézd meg, hogy statisztikailag szignifikáns-e a különbség a két jelölt 
#szentimentjeinek ill. emócióinak száma között.
kt_szentiment <- table(tweets$handle, tweets$text_sentiment)
kt_emotion <- table(tweets$handle, tweets$text_emotion)

#szignifikáns-e a különbség?
chisq.test(kt_szentiment)
chisq.test(kt_emotion)

#ábrákhoz előkészítem a kereszttáblát
kt_szentiment <- as.data.frame(kt_szentiment)
colnames(kt_szentiment) <- c("handle", "sent", "freq")
kt_emotion <- as.data.frame(kt_emotion)
colnames(kt_emotion) <- c("handle", "emotion", "freq")

#barchart
ggplot(kt_szentiment, aes(x=sent, y=freq, fill = handle)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red"), 
                    labels=c("Hillary Clinton", "Donald Trump")) +
  xlab("Sentiment") + ylab("Frequency") + labs(fill="Candidate")
rm(kt_emotion, kt_szentiment)

#idősoros ábra

#2.)Többen feltételezték, hogy Trump tweetjeit (legalább) két különböző ember 
#írja, az androidosokat feltételezhetően Trump, az iphone-osokat pedig valaki 
#más (a  mi adatainkban a source_url oszlop tartalmaz erre vonatkozó 
#információkat).
#Vizsgáld meg vizuálisan, hogy a mi adatunk szerint is különböznek-e a két 
#forrásból származó tweetek szentimentjei ill. emóciói. Nézd meg, hogy 
#statisztikailag szignifikáns-e a különbség.

##szentimentre
trump_szentiment <- subset.data.frame(tweets, tweets$handle == "realDonaldTrump",
                                      select = c(text, source_url, 
                                                 text_sentiment))

trump_szentiment["twsource"] <- NA
trump_szentiment <- within(
  trump_szentiment, twsource[source_url=="http://twitter.com/download/iphone"] 
  <- ("iphone"))
trump_szentiment <- within(
  trump_szentiment, twsource[source_url=="http://twitter.com/download/android"] 
  <- ("android"))
trump_szentiment <- subset.data.frame(
  trump_szentiment, trump_szentiment$twsource != "NA", 
  c(text, text_sentiment, twsource))

kt_trumpszentiment <- table(
  trump_szentiment$text_sentiment, trump_szentiment$twsource)

chisq.test(kt_trumpszentiment)


#ábra szentimentre
ggplot(trump_szentiment, aes(x=text_sentiment, fill=twsource)) +
  geom_bar(position = "dodge") + scale_fill_manual(values=c("yellow", "green")) + 
  xlab("szentiment") + ylab("gyakoriság") + labs(fill="készülék") +
  scale_x_discrete(limits=c("positive","negative", "neutral"))

rm(trump_szentiment)
##emócióra
trump_emotion <- subset.data.frame(tweets, tweets$handle == "realDonaldTrump",
                                      select = c(text, source_url, 
                                                 text_emotion))

trump_emotion["twsource"] <- NA
trump_emotion <- within(
  trump_emotion, twsource[source_url=="http://twitter.com/download/iphone"] 
  <- ("iphone"))
trump_emotion <- within(
  trump_emotion, twsource[source_url=="http://twitter.com/download/android"] 
  <- ("android"))
trump_emotion <- subset.data.frame(
  trump_emotion, trump_emotion$twsource != "NA", 
  c(text, text_emotion, twsource))

kt_trumpemotion <- table(
  trump_emotion$text_emotion, trump_emotion$twsource)

chisq.test(kt_trumpemotion)

#ábra emócióra
trump_emotion <- subset.data.frame(trump_emotion, trump_emotion$text_emotion != "unknown",
                  c("text_emotion", "twsource"))
ggplot(trump_emotion, aes(x=text_emotion, fill=twsource)) +
  geom_bar(position = "dodge") + scale_fill_manual(values=c("yellow", "green")) + 
  xlab("emóció") + ylab("gyakoriság") + labs(fill="készülék") + 
  scale_x_discrete(limits=c("joy","sadness", "surprise", "anger",
                            "fear", "disgust")) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))
rm(trump_emotion)

