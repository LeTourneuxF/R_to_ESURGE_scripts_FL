
#Proportions pour recap avec et sans collier
#hcap.max avec uniquement recaptures et baguage
hcap.max


data<-hcap.max[,5:32]


#data<-data[1:1000,]





for(i in 1:nrow(data)){
  
  col<-which(data[i,]==2)
  data[i,29]<-ifelse(length(col)>0,1,0)
  
  
}



data.col<-data[which(data[,29]==1),c(1:28)]

n.data<-data.col

for(i in 1:nrow(n.data)){
  
  marking<-min(which(data.col[i,]==2|data.col[i,]==1))
  
  indline<-data.col[i,marking:ncol(data.col)]
  
  n.data[i,]<-c(indline,c(rep(0,times=(marking-1))))
  
}

head(n.data)

table(n.data[,1])

newdat<-n.data[which(n.data[,1]==2),]

table(newdat[,1])
table(newdat[,2])
table(newdat[,3])
table(newdat[,4])
table(newdat[,5])




table(newdat[,15])

prop1<-rep(NA, times=ncol(newdat))
prop2<-rep(NA, times=ncol(newdat))
col<-rep(NA, times=ncol(newdat))
lost<-rep(NA, times=ncol(newdat))

for(t in 1:ncol(newdat)){
  
  col[t]<-length(which(newdat[,t]==2))
  lost[t]<-length(which(newdat[,t]==1))
  prop1[t]<-col[t]/col[1]
  prop2[t]<-lost[t]/(col[t]+lost[t])
  
}

col[1]<-0
lost[1]<-NA

plot(col[2:25], main="Nb d'individus à la recapture \n noir: colliers recapturés; rouge colliers perdus", ylab='Nb.individus',xlab='year', pch=18)
points(lost[2:25],pch=18, col='red')

plot(prop1[1:25], type='l', col='black')
points(prop2[1:25], type='l', col='red')


setwd("C:\\Users\\Frederic Letourneaux\\Documents\\Doc\\Data\\2019 Hiver\\perte_col")

#Charger les données
col_loss<-read.table('collar_loss.txt')
#Gérer noms de colonnes
loss<-col_loss[,c(5:114)]
colnames(loss)<- seq(1,110,1)
c_loss<-cbind(col_loss[,1:4],loss)

#Créer nouvelles colonnes pour déterminer les occasions avec premier et dernier 2 et premier 1
c_loss$first2<-NA
c_loss$last2 <-NA
c_loss$first1<-NA

#Boucle pour extraire les occasions
for(i in 1:nrow(c_loss)){
  
  c_loss$first2[i]<-min(which(c_loss[i,5:114]==2))
  c_loss$last2[i]<-max(which(c_loss[i,5:114]==2))
  c_loss$first1[i]<-min(which(c_loss[i,5:114]==1))
  
}

#Vérifier qu'on a pas remis de collier sur une recap
which(c_loss$last2>c_loss$first1)

#Calculer le # d'occasions entre dernière obs collier et recap sans collier
c_loss$time<-c_loss$first1-c_loss$last2

#Colliers perdus et identifiés la même année
c_loss_1y<-c_loss[which(c_loss$time<5),]

c_loss_1y$t4loss<-c_loss_1y$first1-c_loss_1y$first2

table(c_loss_1y$t4loss)

#Colliers perdus et identifiés en 2 ans ou moins
c_loss_2y<-c_loss[which(c_loss$time<9),]

c_loss_2y<-c_loss_2y[-which(c_loss_2y$NIPO_B==111953),]

c_loss_2y$t4loss<-c_loss_2y$first1-c_loss_2y$first2

table(c_loss_2y$t4loss)

hist(c_loss_2y$t4loss/4, main="Years before collar loss (2 ans près)", ylab="# lost", xlab='years')

hist(c_loss_1y$t4loss/4, main="Years before collar loss (1 ans près)", ylab="# lost", xlab='years', breaks=10, xlim=c(0,10))

plot(table(c_loss_2y$t4loss/4), main="Age of collar at loss (2 ans près)", ylab="# lost", xlab='age')
plot(table(c_loss_1y$t4loss/4), main="Age of collar at loss (1 an près)", ylab="# lost", xlab='age')

#####*Recaptures colliers non-perdus####

col.recap<-read.table("C:\\Users\\Frederic Letourneaux\\Documents\\Doc\\Data\\2019 Hiver\\perte_col\\col.recap.txt", header=T)

ncol(col.recap[,5:117])
rec.hist<-col.recap[,5:114]



marking<-rep(NA, length=nrow(rec.hist))
for(i in 1:nrow(rec.hist)){
  marking[i]<-ifelse(2 %in% rec.hist[i,],0,i)
}

table(marking)
rec.hist<-rec.hist[-which(marking!=0),]

n.ind<-nrow(rec.hist)
n.occ<-ncol(rec.hist)
summer<-seq(1,n.occ,4)

marking<-rep(NA, length=n.ind)
for(i in 1:n.ind){
  marking[i]<-ifelse(2 %in% rec.hist[i,],0,i)
}
table(marking)

#nombre de resights
loss<-rep(NA, length=n.ind)
n.recaps<-rep(NA, length=n.ind)


collar.age<-matrix(nrow=n.ind, ncol=4)
max.known.age<-rep(NA, length=n.ind)
max.col<-rep(NA, length=n.ind)

for(i in 1:n.ind){
  
  loss[i]<-ifelse(1 %in% rec.hist[i,],1,0)
  n.recaps[i]<-length(which(rec.hist[i,]==5))  
  
  date.marked<-min(which(rec.hist[i,]==2))
  date.last.seen<-max(which(rec.hist[i,]!=0 & rec.hist[i,]!=3))
  date.last.seen.wcol<-max(which(rec.hist[i,] %in% c(2,5)))#3check this line
  
  if(!(date.marked %in% summer)) {
    collar.age[i,]<-NA
    
    
  }else{
    
  for(n in 1:n.recaps[i]) {
    
    date.cap<-which(rec.hist[i,]==5)[n]
    collar.age[i,n]<-date.cap-date.marked
    
  }}#fin else
    
  max.known.age[i]<-date.last.seen-date.marked
  max.col[i]<-date.last.seen.wcol-date.marked
}

max.known.age<-max.known.age/4
max.col<-max.col/4
hist(max.known.age, breaks=20)

hist(max.col, breaks=20)

table(collar.age[,1])
sum(table(collar.age[,1]))
collar.age<-collar.age[-which(is.na(collar.age[,1])),]
collar.age<-collar.age[-which(collar.age[,1]==-4),]
collar.age<-collar.age/4

table(collar.age[,1])
sum(table(collar.age[,2]))
sum(table(collar.age[,2]))
plot(table(collar.age[,1]), main='âge à la première recapture, n=754')
plot(table(collar.age[,2]), main='âge à la deuxième recapture, n=72')
plot(table(collar.age[,3]), main='âge à la troisième recapture, n=7')


plot(table(collar.age), main='âge à la recapture, n=754')


table(loss)

###Gérer les colliers recapturés physiquement
#####*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_  Recaptures *_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_####
setwd("C:\\Users\\Frederic Letourneaux\\Documents\\Doc\\Data\\2018 Hiver")

recap<-read.table("REC_90_17.txt", header=T, sep=";")

##### Créer occasion ####
recap$abs_occasion<-1
recap$occasion<-(4*recap$an_R+recap$abs_occasion)-4*1989

erreur<-recap[recap$erreur_R!='',]

recap_ok<-c(which(recap$erreur_R==''),which(recap$erreur_R=="OK"))
recap<-recap[c(recap_ok),]

##### Créer états ####
recap$state<-ifelse(recap$pres_collier_R==0,1,2)
recap.malecol<-recap[recap$sexe_B==4&recap$state==2,]
recap.nomalecol<-recap[!(recap$sexe_B==4&recap$state==2),]

#enlever mâles avec colliers, que ce soit erreurs ou autre.
recap.nomalecol2<-recap.nomalecol[!(recap.nomalecol$collier_B!=""&recap.nomalecol$sexe_B==4),]
occurences<-table(unlist(recap$pres_collier_R))
occurences
recap.nomalecol2<-recap.nomalecol2[!recap.nomalecol2$pres_collier_R==2,]



#Calcul pour jour julien
recap<-recap.nomalecol2[,c(3,8,10,11,19,18,17,25,26,27)]
recap$date<-formatC(as.numeric(recap$date_R),width=2,format='d',digits=0,flag='0')
recap$mois<-formatC(as.numeric(recap$mois_R),width=2,format='d',digits=0,flag='0')
recap$annee<-substr(recap$an_R,3,4)
recap$fulldate<-as.POSIXlt(paste(as.character(recap$date),as.character(recap$mois),as.character(recap$annee),sep=""), format = "%d%m%y")
recap$JD<-format(recap$fulldate, "%j")


nana<-is.na(recap$JD)
recap_na<-recap[which(nana==T),]

#recap<-recap[-which(nana==T),]
table(is.na(recap$JD))
recaps.clean<-recap[,c(1,2,15,3,4,8,9,10)]

length(which(recap$an_B==recap$an_R))
same<-rep(NA, length=nrow(recap))

for (i in 1:nrow(recap)){
  
  same[i]<-ifelse(recap$an_B[i]==recap$an_R[i],1,0)
  
}

recap2<-recap[-which(same==1),]
table(recaps.clean$state)
###################################

#####*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_  Baguage *_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_####

setwd("C:\\Users\\Frederic Letourneaux\\Documents\\Doc\\Data\\2018 Hiver")

band<-read.table("BAND_90_17.txt", header=T, sep=";")
##Vérifier les localités#
occurences<-table(unlist(band$loc_B))
occurences

band_young<-band[which(band$age_B==4),]
hist(band_young$an_B)

ny.banded<-table(band_young$an_B)

plot(ny.banded)
##### Créer occasion#
band$abs_occasion<-1
band$occasion<-(4*band$an_B+band$abs_occasion)-4*1989

##### Créer état#
band$state<-ifelse(band$collier_B=='',1,2)
occurences<-table(unlist(band$state))
occurences

#Identifier les mâles qui portent des colliers
which(band$sexe_B==4&band$state==2)
male.col<-band[band$sexe_B==4&band$state==2,]

#Enlever les colliers mis aux mâles
band.nocolmale<-band[!(band$sexe_B==4&band$state==2),]

#Enlever les oiseaux avec un changement de sexe
band.nocolmale<-band.nocolmale[-which(band.nocolmale$erreur_B!=""),]

table(band.nocolmale$erreur_B)



bands<-band.nocolmale[,c(3,8,7,6,10,11,16,17,18)]
bands$date<-formatC(as.numeric(bands$dat_B),width=2,format='d',digits=0,flag='0')
bands$mois<-formatC(as.numeric(bands$mo_B),width=2,format='d',digits=0,flag='0')
bands$annee<-substr(bands$an_B,3,4)
bands$fulldate<-as.POSIXlt(paste(as.character(bands$date),as.character(bands$mois),as.character(bands$annee),sep=""), format = "%d%m%y")
bands$JD<-format(bands$fulldate, "%j")


nana<-is.na(bands$JD)
#bands_na<-bands[which(nana==T),]

#bands<-bands[-which(nana==T),]
table(is.na(bands$JD))
band.clean<-bands[,c(1,2,14,5,6,7,8,9)]


#band.clean<-band.nocolmale[,c(3,8,10,11,17,18)]


#Vérifier que tous les NIPOs des tables de réobservations se trouvent dans la table band 
table(recaps.clean$NIPO_B %in% band.clean$NIPO_B)
table(band.clean$NIPO_B %in% recaps.clean$NIPO_B)


#Enlever les individus qui sont absents de la table band
recaps.clean<-recaps.clean[-which(recaps.clean$NIPO_B %in% band.clean$NIPO_B ==F),]

band.clean<-band.clean[-which(band.clean$NIPO_B %in% recaps.clean$NIPO_B ==F),]

data<-rbind(band.clean, recaps.clean)

data.fem<-data[which(data$sexe_B==5&data$age_B==1),]



library(reshape2)

hcap.max<-dcast(data.fem, NIPO_B+age_B+sexe_B+an_B~occasion, fill=0, fun.aggregate = min)

table(unlist(hcap.max[5:32]))
head(hcap.max)

###########################



data<-hcap.max

#Nombre de colonnes à la fin des données 
col_extra <- 0
vct<-matrix(nrow=length(data$NIPO_B), ncol=2)


#Identifier les colliers mis sur les recaps et oiseaux reobs apres mort

#data<-data[data$NIPO_B==54353,]

for(i in 1:length(data$NIPO_B)){
  one <- which(data[i,5:(ncol(data)-col_extra)]==1)+4               #Where are the ones
  
  two <- which(data[i,5:(ncol(data)-col_extra)]==2)+4  # Where are the 2s?
  
  five <- which(data[i,5:(ncol(data)-col_extra)]==5)+4
  
  first <- min(which(data[i,5:(ncol(data)-col_extra)]!=0)+4)
  
  vct[i,1]<-ifelse(length(two>0),1,
                   ifelse(length(five>0),1,0))
  
  
  if(length(one)>0 & length(two)>0) { #Indentify collars on recaps from state 1
    
    minone <- min(one)
    maxtwo <- max(two)
    
    vct[i,2] <- ifelse(minone<maxtwo, 1, #0)
                       ifelse((length(one)>0 & length(two)>0),8,0)) #ligne à ajouter pour connaître les oiseaux qui ont perdu un collier
  }  
  
  
  
  data[i,first]<-4*data[i,first]
  
}

table(vct[,1])
table(vct[,2])
head(data[which(vct[,1]==0),])


data<-data[-which(vct[,1]==0),]

ratio<-rep(NA, times=ncol(data[,c(5:32)]))
loss<-rep(NA, times=ncol(data[,c(5:32)]))
kept<-rep(NA, times=ncol(data[,c(5:32)]))


for(i in 1:length(ratio)){
  
  loss[i]<-length(which(data[,(i+4)]==1))
  kept[i]<-length(which(data[,(i+4)]==5))
  
  ratio[i]<-loss[i]/kept[i]
  
  
}



table(data[,7])



