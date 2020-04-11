require(reshape2)
## Code for spectral analyses of barn swallow ventral color.
# make sure all the file names are the same length before running
fileloc<-dirname(file.choose()) # this is the location of the files, on the spec computer
options(width=100)
tmp1<-list.files(fileloc)

#Flexibly test for band # entry errors (since not all same length)
modelength<-which.max(tabulate(sapply(tmp1,nchar)))

###Test that all files are 'modelength' characters, counting .txt extension
(errors<-tmp1[nchar(tmp1)!=modelength])##Generate error matrix for invalid names
if(length(errors)>0){write.csv(errors,paste0(fileloc,"/Invalid names.csv"));print("Filename errors detected. Check 'Invalid names.csv' in fileloc directory",quote=F)}else{print("All filenames are same length")} ##Only generate error file if there are errors

tmp1<-tmp1[nchar(tmp1)==modelength]##Run subsequent code only on valid filenames


tbright<-NULL # these are the four descriptors this script will calculate
abright<-NULL
chrom<-NULL
hue<-NULL    

#set up progress bar
t=length(tmp1)
pb <- txtProgressBar(min=1, max=t-1,style=3) 

for(i in seq(along=tmp1)){
 tmp2<-read.table(paste0(fileloc,"/",tmp1[i]), skip=17, nrows=3648)
 tmp2a<-tmp2[tmp2$V1>=300 & tmp2$V1<=700,]
 tmpb<-sum(tmp2a$V2) # calculates total area under the curve = total brightness
 tbright<-c(tbright,tmpb) 
 tmpz<-(tmpb/length(tmp2a$V2)) # average brightness
 abright<-c(abright,tmpz)
 tmpc<- sum(tmp2a$V2[tmp2a$V1>=600 & tmp2a$V1<=700])/tmpb # proportion of brightness in red range
 chrom<-c(chrom,tmpc)
 slope<-NULL
 wl<-NULL
 xa<-550
 
 #update progress bar
 setTxtProgressBar(pb,i)
 
repeat { # sliding window to calculate where slope is steepest for hue
xb<-xa + 5
first<-tmp2a$V1[tmp2a$V1 > xa & tmp2a$V1 < xa + 1]
second<-tmp2a$V1[tmp2a$V1 > xb & tmp2a$V1 < xb + 1]
ref1<-tmp2a$V2[tmp2a$V1 == first[1]]
ref2<-tmp2a$V2[tmp2a$V1 == second[1]]
x<-ref2 - ref1
wl<-c(wl, first[1])
slope<-c(slope,x)
xa<-xa+1
if(xa == 696){break}
hues<-as.data.frame(cbind(wl,slope))
}
tmph<-hues$wl[which.max(hues$slope)]
hue<-c(hue, tmph)
 }#end for loop

# this is putting all the information into a data frame
tmpA<-data.frame(tmp1=tmp1,total.bright=tbright,avg.bright=abright,hue=hue,chrom=as.numeric(chrom)) 

## Ask how many digits are in the ID (i.e. how many digits to take from beginning of filename)
z0<-nchar(tmp1[1])-11 #try to guess ID length
z=6 #Length of ID string
#zz=z+5 #End of Date String
#zzz=z+6 #Character number for Patch
parsed<-cbind(ID=substr(tmp1[1],1,z))

#Test that strings are being interpreted correctly
# cat("\n****Verify this is parsing filenames right****\n");write.table(format(rbind(rep("",4),c("|FileName|","|Bird ID|","|Date|", "|Patch|"),c(tmp1[1],parsed)),justify="r"),quote=F,col.names=F,row.names=F)


tmpA$bird<-as.factor(substr(as.character(tmpA$tmp1),1,z)) # in the name of the files, characters 1 - z represent the band number for each individual
# tmpA$date<-as.factor(substr(as.character(tmpA$tmp1),z+1,zz)) # in the file name, characters __ - __ represent the date the sample was taken, if no date in file name
# tmpA$patch<-as.factor(substr(as.character(tmpA$tmp1),zzz,zzz)) # in the file name, character __ represents the patch the sample is from

head(tmpA)

# #Make sure all patches are capitalized to get only 4 levels
#     tmpA$patch<-toupper(tmpA$patch) 
#     
#     #test for patch mistakes
#     if(length(unique(tmpA$patch))!=4){cat("!!!! Yo, some patch names ain't right!");cat(unique(tmpA$patch))}

#Get input from user on how to order patches in resulting dataframe
## *** NOTE, user input only works if run line by line. Sourcing whole script defaults to TRBV order ***
    tmpB<-tmpA
    # cat("**Which patch order do you want: (1: B->R->T->V) or (2: T->R->B->V)? >>")
    # patchorder<-readLines(con=stdin(),1)
    #   if(patchorder!=1){tmpB$patch<-factor(tmpB$patch,levels=c("T","R","B","V"))}
    #   #Melt and recast the dataframe
      TMP<-melt(tmpB[,-1])
      sorted<-dcast(TMP,bird~variable,mean)#Sort, averaging duplicate values for patches (multiple sample dates are separated out)

#Add hyphens between 4th & 5th character of IDs
      # a<-substr(sorted$bird,1,4)
      # b<-substr(sorted$bird,5,10)
      # sorted$bird<-paste(a,b,sep="-")

      #All look good?
      head(sorted)      
      
write.csv(sorted,paste0(dirname(fileloc),"/mount_color_sorted.csv"))#Saves output one directory up from the raw file directory you selected in the beginning
      