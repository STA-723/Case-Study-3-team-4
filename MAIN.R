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
head(labels97)
data97=read.fwf("Harvard_CAS_1997/DS0001/03163-0001-Data.txt",widths=widths97,col.names=labels97)
write.csv(data97,file="Data/data97.csv")

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
gender = data97$A2
year=data97$A3
housemates=as.factor(data97$A8)
greek=data97$A9
binge5=data97$C1
binge4=data97$C2
currentalc=data97$C5
lastdrink=data97$C10
head(data97$)
lastdrink = lastdrink[(lastdrink>2)]
drunkwithin30 = data97$C13



