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
datalabels97=read.table("Harvard_CAS_1997/DS0001/03163-0001-Record_layout.txt",skip=10)
datalabels97$width=(datalabels97$V3-datalabels97$V2)+1

datalabels99=read.table("Harvard_CAS_1999/DS0001/03818-0001-Record_layout.txt",skip=10)
datalabels99$width=(datalabels99$V3-datalabels99$V2)+1

datalabels01=read.table("Harvard_CAS_2001/DS0001/04291-0001-Record_layout.txt",skip=9)
datalabels01$width=(datalabels01$V3-datalabels01$V2)+1

#Loading data (just one takes forever, apparently.  Consider making these into
#files and commiting/pushing them.)
data93=read.fwf("Harvard_CAS_1993/DS0001/06577-0001-Data.txt",widths=datalabels93$width,col.names = datalabels93$V1)
data97
data99
data01

#I think it works!  At least for this one.
head(data93$STUDY_ID)


