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

##hány tweet származik a jelöltektől?
summary(tweets$handle)
##először csináltam egy táblát, amiben a handle szerepel gyakoriságokkal, majd
##ebből létrehoztam a barplotot.
hany_tweet <- table(tweets$handle)
View(hany_tweet)

##az x tengelyre a handle kerül, ez alapján történik az oszlopok színezése is. A
##geom_bar() üresen marad, mert gyakoriságokat ábrázolok. A színeket kézzel
##állítom be, a címkéket átírom a handle-ről a jelöltek nevére. Az x tengelyről
##eltávolítom a feliratot és a kategóriafeliratokat is (ezt csak úgy tudtam
##megoldani, hogy szóközt tettem oda), az y tengely feliratát az ábra szerint
##átírom. A legendnek a "Candidate" címet adom handle helyett.
ggplot(tweets, aes(x=handle, fill=handle)) + 
geom_bar() +
  scale_fill_manual(values=c("blue", "red"), labels=c("Hillary Clinton", 
                                                      "Donald Trump")) +
  theme(axis.text.x = element_blank()) + 
  xlab(" ") + ylab("Tweet frequency") + labs(fill="Candidate")

##eltávolítom a felesleges objektumot.
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
tweets[6064, 11] <- "en"
tweets[5439, 11] <- "en"
tweets[5378, 11] <- "en"
tweets[6206, 11] <- "en"
tweets[5448, 11] <- "en"
tweets[5296, 11] <- "en"
tweets[5642, 11] <- "en"
tweets[5517, 11] <- "en"
tweets[4434, 11] <- "en"
tweets[6087, 11] <- "en"

#felesleges objektumok eltávolítása.
rm(hillary_lang, donald_lang)

##3./2.)Ábrázold a két jelölt által használt nyelvek gyakoriságát a 
##fig/sample/tweet2.png szerint, majd mentsd ki a plotot a fig mappába 
##tweet2.png néven! Az oszlopszínek legyenek "darkgrey" és "cornflowerblue". 
##(A nyelvek gyakorisága eltérhet az előző lépés miatt.)

#először kimentettem objektumba a nekem kellő oszlopokat, majd ebből csináltam
#egy df-et, amit felhasználtam a plot létrehozásához.
twlang <- cbind(tweets$handle, tweets$lang)
kt_twlang <- table(twlang[,1], twlang[,2])
kt_twlang <- as.data.frame(kt_twlang)
colnames(kt_twlang) <- c("handle", "lang", "freq")

#az x tengelyre a twitter azonostó kerül, az y tengelyre a gyakoriság,
#az oszlopok színe a nyelv alapján lesz. A pozíció paraméter "dodge", így nem
#stacked barplotot csinál, hanem csoportosítottat. Kézzel töltöttem ki az
#oszlopok színét, utána hozzárendeltem a feliratokat a legendhez és a 
#tengelyekhez.
ggplot(kt_twlang, aes(x=handle, y=freq, fill = lang)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkgrey", "cornflowerblue"),
                    labels=c("English", "Spanish")) +
  xlab(" ") + ylab("Frequency") + labs(fill="Language") +
  scale_x_discrete(labels=c("Hillary Clinton", "Donald Trump"))

#felesleges objektum eltávolítása.
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

##III. feladat
#A fivethirtyeight package-ben van egy hiphop_cand_lyrics dataset. A 
#fivethirtyeight-en ezt az elemzést készítették az adatokhoz.

#Készítsd el ggplot2-vel az itt található első 2 ábrát úgy, hogy minél jobban 
#megegyezzen a plotok kinézete. Természetesen nem kell mindennek ugyanolyannak 
#lennie (pl. az oszlopoknak nem kell pontokból állnia), de törekedj a minél 
#nagyobb fokú egyezőségre. Mentsd ki az ábrákat a fig mappába hiphop1.png és 
#hiphop2.png néven!
#Készíts egy általad értelmesnek tartott ábrát a datasetben található változókat
#felhasználva! Mentsd ki az ábrát a fig mappába hiphop3.png néven!
hiphop <- fivethirtyeight::hiphop_cand_lyrics

#faktort csináltam a candidate oszlopból, hogy sorba tudjam rakni a leveleket,
#aztán manuálisan sorbaraktam őket.
hiphop$candidate <- as.factor(hiphop$candidate)
is.factor(hiphop$candidate)
desired_order <- c("Ted Cruz", "Ben Carson", "Bernie Sanders", "Mike Huckabee",
                   "Chris Christie", "Jeb Bush", "Hillary Clinton", "Donald Trump")
hiphop$candidate <- factor(as.character(hiphop$candidate), levels=desired_order)

#a fenti levelek szerint sorbarakva hívtam be az adatot a plotba, az x tengelyre
#az év került, az oszlopok kitöltése a jelöltek neve szerint történik, a gropuing
#is, beállítottam, hogy stacked barplot legyen, és a színeket beépített
#palettából vettem. Eltüntettem az x és y tengely címeit, a legendet a plot fölé
#tettem.
#Bár kapok ábrát, nem tökéletes, és az R mindig arra
#figyelmeztet, hogy az x tengelyen overlapping intervallumok vannak. Ezt nem
#sikerült megoldanom.
ggplot(hiphop[order(desc(hiphop$candidate)),], aes(x=album_release_date, 
                                             fill=candidate), group=candidate) + 
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Set3") + xlab(" ") + ylab(" ") + 
  theme(legend.position = "top")

#felesleges objektumok eltávolítása
rm(desired_order)

##FELADAT VÉGE------------------------------------------------------------------

##IV. feladat
#1.) Hasonlítsd össze Hillary Clinton és Donald Trump tweetjeinek szentimentjeit 
#és emócióit. Vizsgáld meg oszlopdiagrammal és idősoros ábrával az eltéréseket. 
#Nézd meg, hogy statisztikailag szignifikáns-e a különbség a két jelölt 
#szentimentjeinek ill. emócióinak száma között.

#a biztonság kedvéért újra beolvasom az eredeti fájlt.
tweets <- read.csv2("data/clinton_trump_tweets.csv", stringsAsFactors = F)

#létehozom a két kereszttáblát, ami alapján vizsgálódok.
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
kt_emotion <- subset.data.frame(kt_emotion, kt_emotion$emotion != "unknown",
                                c(handle, emotion, freq))
#barchart szentimentre
ggplot(kt_szentiment, aes(x=sent, y=freq, fill = handle)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red"), 
                    labels=c("Hillary Clinton", "Donald Trump")) +
  xlab("Sentiment") + ylab("Frequency") + labs(fill="Candidate")

#barchart emócióra
ggplot(kt_emotion, aes(x=emotion, y=freq, fill = handle)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red"), 
                    labels=c("Hillary Clinton", "Donald Trump")) +
  xlab("Emotion") + ylab("Frequency") + labs(fill="Candidate") + 
  scale_x_discrete(limits=c("joy","sadness", "surprise", "anger",
                            "fear", "disgust")) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))

#felesleges objektumok eltávolítása
rm(kt_emotion, kt_szentiment)

#idősoros ábra - nem sikerült, nem tudtam működőképeset csinálni.

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
  geom_bar(position = "dodge") + scale_fill_brewer(palette="Accent") + 
  xlab("szentiment") + ylab("gyakoriság") + labs(fill="készülék") +
  scale_x_discrete(limits=c("positive","negative", "neutral"))

#felesleges objektum eltávolítása
rm(trump_szentiment, kt_trumpszentiment)

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
  geom_bar(position = "dodge") + scale_fill_brewer(palette="Accent") + 
  xlab("emóció") + ylab("gyakoriság") + labs(fill="készülék") + 
  scale_x_discrete(limits=c("joy","sadness", "surprise", "anger",
                            "fear", "disgust")) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))

#felesleges objektum eltávolítása
rm(trump_emotion, kt_trumpemotion)
