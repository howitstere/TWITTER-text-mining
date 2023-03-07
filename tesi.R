library(rtweet)

#creating an object with 2000 tweets from 3 users
tlMCR <- get_timeline(c("GiorgiaMeloni", "CarloCalenda", "matteorenzi"), 2000) #TimeLine Meloni, Calenda, Renzi
by(tlMCR$created_at,tlMCR$screen_name,summary)

#see when the 2000 tweets have been created
#M has been posting 2000 tweets in 1 year 
#C posted 2000 tweets in just 3 months
#R posted 2000 tweets in almost 1 year and an half


tlMCRr <- tlMCR[as.Date(tlMCR$created_at) >= "2020-01-01",] #TimeLine Meloni, Calenda, Renzi Restricted
table(tlMCRr$screen_name)

#Grafici con numero di follower, produzione di tweet per i 3 politici italiani selezionati:
vsUsr <- c("GiorgiaMeloni", "CarloCalenda", "matteorenzi") #Vector with Selected Users 
sUsrs <- lookup_users(users = vsUsr)
sUsrs[,c("user_id","screen_name","location","account_created_at","followers_count","friends_count","statuses_count","favourites_count")]

par(mar=c(6,4,2,1))
par(mgp=c(1.5,0.5,0))
par(mfrow=c(1,2))
#istogramma a barre per numero di followers
barplot(sUsrs$followers_count, names.arg = sUsrs$screen_name, las=2, main="number of followers", 
        ylim = c(0,3500000), cex.axis = 0.7, cex.names = 0.8, cex.main=0.9) +
grid(NA,NULL)

#istogramma a barre per numero di tweet
barplot(sUsrs$statuses_count,names.arg = sUsrs$screen_name,las=2,main="number of tweets",
        cex.axis = 0.7,cex.names = 0.8, cex.main=0.9)+
grid(NA,NULL)

library(ggplot2)
ggplot(data=sUsrs, 
      aes(x=screen_name, 
          y=followers_count,
          fill=friends_count)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_light() +
  ggtitle("profiles ordered by number of followers")

ggplot(data=sUsrs, 
       aes(x=screen_name, 
           y=statuses_count)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ggtitle("profiles ordered by number of tweets") + 
  xlab("")

#Grafici con numero di follower, produzione di tweet per altri politici italiani selezionati
vUsr <- c("matteosalvinimi", "luigidimaio", "GiuseppeConteIT", "nzingaretti", "berlusconi") #Vector with other Users 
Usrs <- lookup_users(users = vUsr)
Usrs[,c("user_id","screen_name","location","account_created_at","followers_count","friends_count","statuses_count","favourites_count")]

par(mar=c(6,4,2,1))
par(mgp=c(1.5,0.5,0))
par(mfrow=c(1,2))
#istogramma a barre per numero di followers
barplot(Usrs$followers_count, names.arg = Usrs$screen_name, las=2, main="number of followers", 
        ylim = c(0,3500000), cex.axis = 0.7, cex.names = 0.8, cex.main=0.9) +
  grid(NA,NULL)

#istogramma a barre per numero di tweet
barplot(Usrs$statuses_count,names.arg = Usrs$screen_name,las=2,main="number of tweets",
        cex.axis = 0.7,cex.names = 0.8, cex.main=0.9)+
  grid(NA,NULL)

library(ggplot2)
ggplot(data=Usrs, 
       aes(x=screen_name, 
           y=followers_count,
           fill=friends_count)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_light() +
  ggtitle("profiles ordered by number of followers")

ggplot(data=Usrs, 
       aes(x=screen_name, 
           y=statuses_count)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ggtitle("profiles ordered by number of tweets") + 
  xlab("")


#Seleziono 10000 follower per ciascuno degli utenti selezionati M, C, R
#Verifico quanti di questi 10000 utenti sono unici per ciascuno dei tre account e quanti sono condivisi.
#Per ognuno dei tre personaggi abbiamo quindi un elenco di user_id che li seguono, 
#dobbiamo quindi 'abbinare' questi elenchi, verificando quanti user_id seguono uno, due o tutti e tre

fGM <- get_followers(user = sUsrs$user_id[1],n = 3000)
fCC <- get_followers(user = sUsrs$user_id[2],n = 3000)
fMR <- get_followers(user = sUsrs$user_id[5],n = 3000)
str(fGM)

# ad ogni elenco di follower aggiungo un campo 
# con un identificativo del personaggio seguito
fGM$user <- "GM"
fMR$user <- "MR"
fCC$user <- "CC"
# abbino gli elenchi di follower
# creo un data frame dall'abbinamento delle liste di Meloni e Renzi
fGMMR <- merge(fGM, fMR, by = "user_id", all = T)
# modifico i nomi delle ultime due colonne
colnames(fGMMR)[2:3] <- c("user.GM", "user.MR")
# abbino il nuovo data frame con la lista dei follower di Calenda
fGMMRCC <- merge(fGMMR, fCC, by = "user_id", all = T)
colnames(fGMMRCC)[4] <- "user.CC"

# sostituisco gli NA con uno spazio
fGMMRCC[is.na(fGMMRCC)] <- ""
# creo un campo con l'accorpamento delle sigle
fGMMRCC$who <- paste(fGMMRCC$user.GM,fGMMRCC$user.MR,fGMMRCC$user.CC,sep=" ")
# elimino gli spazi inutili dal campo who
fGMMRCC$who <- gsub(" "," ",fGMMRCC$who)
fGMMRCC$who <- gsub("^\\s+|\\s+$", "", fGMMRCC$who)

cbind(n=addmargins(table(fGMMRCC$who)),percent=addmargins(prop.table(table(fGMMRCC$who)))*100)

library(RColorBrewer) # libreria con tavolozza di colori

# tabella con i 10000 follower selezionati per i tre utenti
par(mfrow = c(1,3))
tbGM <- table(fGMMRCC[fGMMRCC$user.GM != "","who"])
pie(tbGM,clockwise = T,labels = paste(names(tbGM),tbGM/1000*100),cex=0.9,radius = 0.9,
    main="Meloni",col=brewer.pal(4,"Set1"))
tbMR <- table(fGMMRCC[fGMMRCC$user.MR != "","who"])
pie(tbMR,clockwise = T,labels = paste(names(tbMR),tbMR/1000*100),cex=0.9,radius = 0.9, 
    main="Renzi",col=brewer.pal(4,"Dark2"))
tbCC <- table(fGMMRCC[fGMMRCC$user.CC != "","who"])
pie(tbCC,clockwise = T,labels = paste(names(tbCC),tbCC/1000*100),cex=0.9,radius = 0.9, 
    main="Calenda",col = brewer.pal(4,"Accent"))

# scarica gli ultimi 100 tweet inviati da Meloni, Calenda e Renzi, escludendo i retweet
tl3 <- get_timeline(user = sUsrs$user_id[c(1,2,3)], n = 2000, include_rts = F)
table(tl3$screen_name)

#MELONI
tlGM <- get_timeline("GiorgiaMeloni", 2000) #Scarico 2000 tweet dalla timeline di Meloni
summary(tlGM$created_at) #Periodo in cui sono stati effettuati gli ultimi 2000 tweet da parte di Meloni
#Numero di retweet e di citazioni di tweet
table(tlGM$is_retweet)
table(tlGM$is_quote)
#Tabella con gli hashtag utilizzati;
#. il campo hashtags contiene una lista,
#. utilizzo la funzione unlist per trasformare il contenuto in un vettore,
#. creo una tabella di frequenza degli hashtag
#. ordino il risultato in senso decrescente (sort)
#.Visualizzo i 10 hashtag maggiormente utilizzati
sort(table(unlist(tlGM$hashtags)),decreasing = T)[1:10]
#tabella con le mentions utilizzate
sort(table(unlist(tlGM$mentions_screen_name)),decreasing = T)[1:10]

#CALENDA
tlCC <- get_timeline("CarloCalenda", 2000) #Scarico 2000 tweet dalla timeline di Calenda
summary(tlCC$created_at) #Periodo in cui sono stati effettuati gli ultimi 2000 tweet da parte di Calenda
#Numero di retweet e di citazioni di tweet
table(tlCC$is_retweet)
table(tlCC$is_quote)
#Tabella con gli hashtag utilizzati;
#. il campo hashtags contiene una lista,
#. utilizzo la funzione unlist per trasformare il contenuto in un vettore,
#. creo una tabella di frequenza degli hashtag
#. ordino il risultato in senso decrescente (sort)
#.Visualizzo i 10 hashtag maggiormente utilizzati
sort(table(unlist(tlCC$hashtags)),decreasing = T)[1:10]
#tabella con le mentions utilizzate
sort(table(unlist(tlCC$mentions_screen_name)),decreasing = T)[1:10]


#RENZI
tlMR <- get_timeline("matteorenzi", 2000) #Scarico 2000 tweet dalla timeline di Renzi
summary(tlMR$created_at) #Periodo in cui sono stati effettuati gli ultimi 2000 tweet da parte di Renzi
#Numero di retweet e di citazioni di tweet
table(tlMR$is_retweet)
table(tlMR$is_quote)
#Tabella con gli hashtag utilizzati;
#. il campo hashtags contiene una lista,
#. utilizzo la funzione unlist per trasformare il contenuto in un vettore,
#. creo una tabella di frequenza degli hashtag
#. ordino il risultato in senso decrescente (sort)
#.Visualizzo i 10 hashtag maggiormente utilizzati
sort(table(unlist(tlMR$hashtags)),decreasing = T)[1:10]
#tabella con le mentions utilizzate
sort(table(unlist(tlMR$mentions_screen_name)),decreasing = T)[1:10]

#i 3 insieme
#retweet postati
print(c("RETWEETS",table(c(tlMR$screen_name, tlMR$is_retweet)),
        table(c(tlGM$screen_name, tlGM$is_retweet )),
        table(c(tlCC$screen_name, tlCC$is_retweet ))))
#citazioni postate
print(c("QUOTES", table(c(tlMR$is_quote, tlMR$screen_name)),
        table(c(tlGM$is_quote, tlGM$screen_name)),
        table(c(tlCC$is_quote, tlCC$screen_name))))
#hashtags utilizzati dai tre
print(c("HASHTAGS", sort(table(unlist(tlMR$hashtags)),decreasing = T)[1:5],
        sort(table(unlist(tlGM$hashtags)),decreasing = T)[1:5],
      sort(table(unlist(tlCC$hashtags)),decreasing = T)[1:5]))
#mentions utilizzate dai tre 
print(c("MENTIONS",sort(table(unlist(tlMR$mentions_screen_name)),decreasing = T)[1:5],
        sort(table(unlist(tlGM$mentions_screen_name)),decreasing = T)[1:5],
        sort(table(unlist(tlCC$mentions_screen_name)),decreasing = T)[1:5]))

#hashtag comuni
vsUsr <- c("GiorgiaMeloni","CarloCalenda","matteorenzi")
lstHS <- list() # lista in cui inserire gli hashtag per user
nHS <- numeric() # vettore in cui inserire il numero di hashtag
uHS <- numeric() # vettore in cui inserire il numero di hashtag univoci
for(i in 1:3){
  tmp <- tlMCRr[tlMCRr$screen_name==vsUsr[i],"hashtags"] # lista degli hashtag user i
  tmp <- unlist(tmp,use.names = F) # trasformazione in vettore
  tmp <- na.omit(tmp) # escludo NA
  nHS[i] <- length(tmp) # numero di hashtag totali
  uHS[i] <- length(unique(tmp)) # numero di hashtag univoci
  lstHS[[i]] <- tmp
}
names(lstHS) <- vsUsr
names(nHS) <- vsUsr
names(uHS) <- vsUsr
cbind(Hashtag.Tot=nHS,Hashtag.Univoci=uHS)

library(purrr)
Reduce(intersect,list(lstHS[[1]],lstHS[[2]],lstHS[[3]])) #14

# se escludiamo Calenda:
Reduce(intersect,list(lstHS[[1]],lstHS[[3]])) #86

# se escludiamo Renzi:
Reduce(intersect,list(lstHS[[1]],lstHS[[2]])) #38

# se escludiamo Meloni:
Reduce(intersect,list(lstHS[[2]],lstHS[[3]])) #37





#creating an object with 2000 tweets from 3 users
tlMCR <- get_timeline(c("GiorgiaMeloni", "CarloCalenda", "matteorenzi"), 2000) #TimeLine Meloni, Calenda, Renzi
by(tlMCR$created_at,tlMCR$screen_name,summary)

#see when the 2000 tweets have been created
#M has been posting 2000 tweets in 1 year 
#C posted 2000 tweets in just 3 months
#R posted 2000 tweets in almost 1 year and an half

tlMCRr <- tlMCR[as.Date(tlMCR$created_at) >= "2020-01-01",] #TimeLine Meloni, Calenda, Renzi Restricted
table(tlMCRr$screen_name)

# close look to the favorites given to the users
# Quante volte questi tweet sono stati indicati come favoriti
library(dplyr)
tbFav <- tlMCRr %>%
  group_by(screen_name) %>% 
  summarise(n=n(),min=min(favorite_count),max=max(favorite_count),
            totFavoriti=sum(favorite_count),
            media=mean(favorite_count))
tbFav

#Distribuzione del numero di favoriti
ggplot(tlMCRr, aes(x=screen_name, y=favorite_count,fill = screen_name)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot() + 
  theme_bw()+xlab("") + ggtitle("Distribution of favorites per user") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")

#Numero medio di Favoriti
ggplot(tbFav, aes(x=reorder(screen_name,-media), y=media, 
                  fill=screen_name))+
  geom_bar(stat="identity")+
  theme_minimal()+xlab("")+ylab("Favorites per tweet")+
  ggtitle("Average number of favorites") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_brewer(palette = "Dark2") + theme(legend.position = "none")

#Numero totale di Favoriti
ggplot(tbFav, aes(x=reorder(screen_name,-totFavoriti), y=totFavoriti))+
  geom_bar(stat="identity")+
  theme_minimal()+xlab("")+ylab("Favorites count")+
  ggtitle("Total number of favorites") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position = "none")

#Quante volte i tweet scritti dai politici (non i retweet) sono stati ritwittati
tbRT <- tlMCRr[tlMCRr$is_retweet==F,] %>% # esclusione dei ReTweet
group_by(screen_name) %>% 
summarise(n=n(),min=min(retweet_count),max=max(retweet_count),
            totFavoriti=sum(retweet_count),
            media=mean(retweet_count))
tbRT

#Numero totale di ReTweet
ggplot(tbRT, aes(x=reorder(screen_name, -totFavoriti), y=totFavoriti, 
                 fill=screen_name)) +
  geom_bar(stat="identity")+
  theme_minimal()+xlab("")+ylab("N retweet")+
  ggtitle("Total number of Retweets") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_brewer(palette = "Dark2") + theme(legend.position = "none")

#Numero medio di ReTweet per tweet
ggplot(tbRT, aes(x=reorder(screen_name, -media), y=media, 
                 fill=screen_name)) +
  geom_bar(stat="identity")+
  theme_minimal()+xlab("")+ylab("N retweet per tweet")+
  ggtitle("Average number of Retweets") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_brewer(palette = "Dark2") + theme(legend.position = "none")

#Grafico dell'andamento del successo dei tweet (favoriti + retweet)
tab2 <- tlMCRr[,c("screen_name","created_at","favorite_count","retweet_count")] %>% 
  group_by(screen_name,giorno=as.Date(created_at)) %>% 
  summarise(favoriti=sum(favorite_count),retweet=sum(retweet_count),n=n())

# creazione variabile somma di favoriti e retweet
tab2$favret <- tab2$favoriti+tab2$retweet

ggplot(tab2, aes(x=giorno, y=favret, group=screen_name)) +
  geom_line(aes(color=screen_name),size= 1)+
  scale_color_manual(values=brewer.pal(9, "Set1")[c(1:5,9)])+
  theme_light()+ggtitle("Favorites & ReTweets per day") + xlab("") + ylab("Favorites & ReTweets")

p1 <- ggplot(tab2, aes(x=giorno, y=favret/n)) +
  geom_line(aes(color=screen_name),size= 1)+
  ylim(0,7000) +
  scale_color_manual(values=brewer.pal(9, "Set1")[c(1:5,9)])+
  theme_light() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p1 + facet_wrap( ~ screen_name, nrow = 2) + 
  theme(legend.position = "none") +
  ggtitle("Favorites & ReTweets for each tweet of the three users") +
  xlab("") + ylab("Favorites & ReTweets per tweet")



addmargins(table(tlMCRr$screen_name,tlMCRr$is_retweet))

# % di retweet per user
round(addmargins(prop.table(addmargins(table(tlMCRr$screen_name,tlMCRr$is_retweet),1),1),2)*100,1)

tb <- as.data.frame(table(tlMCRr$is_retweet,tlMCRr$screen_name))
ggplot(tb,aes(x=Var2,y=Freq, fill=Var1))+
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette="Dark2") +
  theme_minimal() + xlab("")+ylab("")+
  ggtitle("Distribution of retweets") +
  labs(fill = "is_retweet")

tlMCRr %>%
  dplyr::group_by(screen_name) %>% # raggruppo per screen_name
  ts_plot("weeks") + # creo un grafico con i tweet per settimana
  theme_light() + # imposto il tipo di grafico
  scale_color_manual(values=c("red", "darkgreen", "lightblue")) + # assegno i colori alle linee
  geom_line(size=0.8) + # spessore delle linee
  labs(title="Temporal distribution of the tweets") # titolo









