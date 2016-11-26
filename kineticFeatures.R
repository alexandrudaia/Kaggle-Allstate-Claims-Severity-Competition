train=read.csv('/media/machine_learning/A80C461E0C45E7C01/all/train.csv',heade=T,sep=',')

cat=names(train)[2:117]
train[,cat]=sapply(train[,cat],as.numeric)

write.csv(train,'/media/machine_learning/A80C461E0C45E7C01/all/train.csv',row.names = F)


test=read.csv('/media/machine_learning/A80C461E0C45E7C01/all/test.csv',heade=T,sep=',')

cat=names(train)[2:117]
test[,cat]=sapply(test[,cat],as.numeric)

write.csv(test,'/media/machine_learning/A80C461E0C45E7C01/all/test.csv',row.names = F)

energy=c()
for( i in  1:nrow(train))
{
  row=table(as.numeric(train[i,2:117]))/116
  en=sum(row^2)
  energy=c(energy,en)
  
}
train[,"energy"]=energy

energy=c()
for( i in  1:nrow(test))
{
  row=table(as.numeric(test[i,2:117]))/116
  en=sum(row^2)
  energy=c(energy,en)
  
}
test[,"energy"]=energy

cont=names(train)[118:131]

tempTrain=train
tempTest=test
tempTrain[,cont]=trunc(tempTrain[,cont]*10)
tempTest[,cont]=trunc(tempTest[,cont]*10)

energyCont=c()

for( i in 1:nrow(tempTrain))
{
  en=table(as.numeric(tempTrain[i,cont]))/dim(tempTrain[,cont])[2]
  kinetic=sum(en^2)
  energyCont=c(energyCont,kinetic)
}

train["energyCont"]=energyCont
tempTrain["energyCOnt"]=energyCont

energyCont=c()

for( i in 1:nrow(tempTest))
{
  en=table(as.numeric(tempTest[i,cont]))/dim(tempTest[,cont])[2]
  kinetic=sum(en^2)
  energyCont=c(energyCont,kinetic)
}

test["energyCont"]=energyCont
tempTest["energyCOnt"]=energyCont

allEnergy=c()
all_feat=names(tempTrain)[2:131]
for( i in  1:nrow(train))
{
  en=table(as.numeric(tempTrain[i,all_feat]))/dim(tempTrain[,all_feat])[2]
  kin=sum(en^2)
  allEnergy=c(allEnergy,kin)
}
 
tempTrain['allEnergy']=allEnergy
train['allEnergy']=allEnergy

allEnergy=c()
all_feat=names(tempTrain)[2:131]
for( i in  1:nrow(test))
{
  en=table(as.numeric(tempTest[i,all_feat]))/dim(tempTest[,all_feat])[2]
  kin=sum(en^2)
  allEnergy=c(allEnergy,kin)
}

tempTest['allEnergy']=allEnergy
test['allEnergy']=allEnergy

  
write.csv(train,'/media/machine_learning/A80C461E0C45E7C01/all/train.csv',row.names = F)
write.csv(test,'/media/machine_learning/A80C461E0C45E7C01/all/test.csv',row.names = F)

write.csv(tempTrain,'/media/machine_learning/A80C461E0C45E7C01/all/tempTrain.csv',row.names = F)
write.csv(tempTest,'/media/machine_learning/A80C461E0C45E7C01/all/tempTest.csv',row.names = F)
