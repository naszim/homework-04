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
mf_col <- c("blue", "red")
barplot(hany_tweet, col = mf_col, border = FALSE,
        main = "Candidate Tweets", ylab = "Tweet frequency")
par(xpd = T)
legend("right", 
       legend = c("Hillary Clinton", "Donald Trump"), 
       fill = mf_col)
par(mar=c(1, 1, 1, 8), xpd=TRUE)
par(xpd=T, mar=par()$mar+c(0,0,0,6))
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
                                  select = c(text, lang))
donald_lang <- subset.data.frame(tweets, tweets$handle == "realDonaldTrump",
                                 select = c(text, lang))
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
