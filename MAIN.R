##MAIN
##Evan Knox, Joe Mathews, and Alessandro Zito

#Unziping data files
#unzip("Harvard_CAS_1993.zip")
#unzip("Harvard_CAS_1997.zip")
#unzip("Harvard_CAS_1999.zip")
#unzip("Harvard_CAS_2001.zip")

#Reading in column names and widths from record layout files
datalabels93=read.table("Harvard_CAS_1993/DS0001/06577-0001-Record_layout.txt",skip=10)
datalabels93$width=(datalabels93$V3-datalabels93$V2)+1

#97 still isn't working for some reason -- mentions line 427, but
#no apparent problem on that line of the file.  Try nrows=400 or something
#and look for errors in formatting, I guess.
datalabels97=read.table("Harvard_CAS_1997/DS0001/03163-0001-Record_layout.txt",skip=10,nrows=426)
datalabels97$width=(datalabels97$V3-datalabels97$V2)+1
widths97=c(datalabels97$width,11,11,3,4)
labels97=c(as.character(datalabels97$V1),"DATERECD","DATESCAN","FORMCODE","IPEDS")


datalabels99=read.table("Harvard_CAS_1999/DS0001/03818-0001-Record_layout.txt",skip=10)
datalabels99$width=(datalabels99$V3-datalabels99$V2)+1

datalabels01=read.table("Harvard_CAS_2001/DS0001/04291-0001-Record_layout.txt",skip=9)
datalabels01$width=(datalabels01$V3-datalabels01$V2)+1

#Loading data (just one takes forever, apparently.  Consider making these into
#files and commiting/pushing them.)

#I think it works!  At least for this one.
data93=read.csv("Data/data93.csv")
data97=read.csv("Data/data97.csv")
data99=read.csv("Data/data99.csv")
data01=read.csv("Data/data01.csv")

#data cleaning for 97
#Need line in here to remove na's
gender = data97$A2
year=data97$A3
housemates=as.factor(data97$A8)
greek=data97$A9
binge5=data97$C1
binge4=data97$C2
currentalc=data97$C5
lastdrink=data97$C10
lastdrink = lastdrink[(lastdrink>2)]
drinkwithin30 = data97$C13
df97=data97[,103:114]
df97[is.na(df97)]=1
df97=as.matrix(df97)
drinkingprob=vector(length=length(df97[,1]),mode="numeric")
for(i in 1:length(df97[,1])){
  drinkingprob[i]=mean(df97[i,1:12])-1
}
AA=data97$E18_A
closefriends=data97$F2
GPA=data97$F4
maritalstatus=data97$G1
latinx=data97$G2
ethnicity=data97$G3
childhoodreligion=data97$G4
hsdrinktimes=data97$G9
hsdrinknumbers=data97$G10
hsbinges=data97$G11
daddrink=data97$G14
momdrink=data97$G15
familydrinking=data97$G16
momcollege=vector(length=length(data97$COLL_ID),mode="numeric")
dadcollege=vector(length=length(data97$COLL_ID),mode="numeric")
for(i in 1:length(data97$COLL_ID)){
  if(data97$G17[i]==3 | data97$G17[i] == 4){
    momcollege[i]=1
  }
  else{
    momcollege[i]=0
  }
  if(data97$G17[i]==2 | data97$G17[i]==4){
    dadcollege[i]=1
  }
  else{
    dadcollege[i]=0
  }
}

clean97=cbind(gender, year, housemates, greek, binge5, binge4, currentalc,lastdrink, drinkwithin30,drinkingprob,AA, closefriends,GPA, maritalstatus,childhoodreligion,hsdrinktimes,hsdrinknumbers,hsbinges,daddrink,momdrink,familydrinking,dadcollege,momcollege)
