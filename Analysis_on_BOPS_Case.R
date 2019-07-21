setwd("/Users/sandhyasriraman/Downloads/")

#------------------------------------------------
#Analysis based on BOPS case study in USA and Canada

library("plyr")
library("readxl")
bops_bm=read_excel("home_and_kitchen_BOPS_data.xlsx",sheet=3)
bops_bm=na.omit(bops_bm)
sum_sales_usa=rep(0,84)
sum_sales_can=rep(0,84)
sum_sales_usa_aft=rep(0,84)
sum_sales_can_aft=rep(0,84)
n_can=rep(0,84)
n_usa=rep(0,84)
n_can_aft=rep(0,84)
n_usa_aft=rep(0,84)
ar=unique(bops_bm$`id (store)`)

for(j in 1:length(ar))
{
  for(i in 1:nrow(bops_bm))
  {
    if(bops_bm$`id (store)`[i]==ar[j])
      if(bops_bm$after[i]==TRUE)
      {
        if(bops_bm$usa[i]==TRUE)
        {
          sum_sales_usa_aft[j]= sum_sales_usa_aft[j]+bops_bm$sales[i]
          n_usa_aft[j]=n_usa_aft[j]+1
          
        }
        else if(bops_bm$usa[i]==FALSE)
        {
          sum_sales_can_aft[j]= sum_sales_can_aft[j]+bops_bm$sales[i]
          n_can_aft[j]=n_can_aft[j]+1
        }
        
      }
    else
      if(bops_bm$usa[i]==TRUE)
      {
        sum_sales_usa[j]= sum_sales_usa[j]+bops_bm$sales[i]
        n_usa[j]=n_usa[j]+1
        
      }
    else if(bops_bm$usa[i]==FALSE)
    {
      sum_sales_can[j]= sum_sales_can[j]+bops_bm$sales[i]
      n_can[j]=n_can[j]+1
    }
  }
}
usa_avg=rep(0,84)
can_avg=rep(0,84)
usa_avg_aft=rep(0,84)
can_avg_aft=rep(0,84)
for(j in 1:length(ar))
{
  if(sum_sales_usa[j]>0)
    usa_avg_aft[j]=sum_sales_usa_aft[j]/n_usa_aft[j]
  else
    usa_avg[j]=0
  if(sum_sales_can[j]>0)
    can_avg_aft[j]=sum_sales_can_aft[j]/n_can_aft[j]
  else
    can_avg[j]=0
  if(sum_sales_usa_aft[j]>0)
    usa_avg[j]=sum_sales_usa[j]/n_usa[j]
  else
    usa_avg_aft[j]=0
  if(sum_sales_can_aft[j]>0)
    can_avg[j]=sum_sales_can[j]/n_can[j]
  else
    can_avg[j]=0
}
pct_chg=rep(0,84)
for(j in 1:length(ar))
{
  pct_chg[j]=((usa_avg_aft[j]+can_avg_aft[j])-(usa_avg[j]+can_avg[j]))/((usa_avg[j]+can_avg[j]))*100
}

#Average percent change for Canada

avg_pct_can= (mean(sum_sales_can_aft)-mean(sum_sales_can))/mean(sum_sales_can)
avg_pct_can*100

#Average percent change for USA

avg_pct_usa= (mean(sum_sales_usa_aft)-mean(sum_sales_usa))/mean(sum_sales_usa)
avg_pct_usa*100

pct_chg #Percentage change per store
#Percentage change regression for USA

bops=rep(-1,84)
for(j in 1:84)#Checking if store was in a BOPS(USA) location
  for(i in 1:nrow(bops_bm))
    if(ar[j]==bops_bm$`id (store)`[i])
      if(bops[j]<0)
        bops[j]=bops_bm$usa[i]
regr_pct=lm(pct_chg~bops)
summary(regr_pct)

#Average pct change for DMAs

bops_dma=read_excel("home_and_kitchen_BOPS_data.xlsx",sheet=1)
bops_dma=bops_dma[,-8]#Removing NA columns
bops_dma=bops_dma[,-8]
bops_dma=bops_dma[,-8]
bops_dma=bops_dma[,-8]
bops_dma=bops_dma[,-8]
bops_dma=bops_dma[,-8]
bops_dma=na.omit(bops_dma)
unq=unique(bops_dma$`id (DMA)`)

sum_dma_after=rep(0,210)
sum_dma_before=rep(0,210)
n_aft=rep(0,210)
n_before=rep(0,210)
for(j in 1:length(unq))
{
  for(i in 1:nrow(bops_dma))
  {
    if(bops_dma$`id (DMA)`[i]==unq[j])
      if(bops_dma$after[i]==TRUE)
      {
        sum_dma_after[j]= sum_dma_after[j]+bops_dma$sales[i]
        n_aft[j]=n_aft[j]+1
      }
    else if(bops_dma$after[i]==FALSE)
    {
      sum_dma_before[j]= sum_dma_before[j]+bops_dma$sales[i]
      n_before[j]=n_before[j]+1
    }
  }
}

dma_avg_aft=rep(0,length(unq))
dma_avg_bef=rep(0,length(unq))
store_id=rep(0,length(unq))


for(j in 1:length(unq))
{
  dma_avg_aft[j]=mean(sum_dma_after[j])
  dma_avg_bef[j]=mean(sum_dma_before[j])
  store_id[j]=j
}

df=data.frame(meandma_avg_aft,dma_avg_bef,store_id)

pct_chg_dma=rep(0,length(unq))

for(j in 1:length(unq))
{
  pct_chg_dma[j]=(((dma_avg_aft[j])-(dma_avg_bef[j]))/(dma_avg_bef[j]))*100
}
cls=rep(-1,210)
for(j in 1:210)#Checking for stores close to BOPS
{
  for(i in 1:nrow(bops_dma))
  {
    if(unq[j]==bops_dma$`id (DMA)`[i])
      if(cls[j]<0)
        cls[j]=bops_dma$close[i]
  }
}

df=data.frame(dma_avg_aft,dma_avg_bef,store_id,cls)
n=length(which(df$cls==1))

cls_aft=rep(0,length(unq))
cls_bef=rep(0,length(unq))
for(j in 1:length(unq))
{
  if(cls[j]==1)
  {
    cls_aft[j]=dma_avg_aft[j]
    cls_bef[j]=dma_avg_bef[j]
  }
}
n=length(which(df$cls==1))
n
pct_dma_cls=(((sum(cls_aft)/n)-(sum(cls_bef)/n))/(sum(cls_bef)/n))*100
pct_dma_cls#Average Percentage change is sales 
#Regression for pct change in DMAs

regr_dma=lm(pct_chg_dma~cls)
summary(regr_dma)



#13 weeks Before BOPS similar analysis to above two

sum_sales_usa_13=rep(0,84)
sum_sales_can_13=rep(0,84)
sum_sales_usa_aft_13=rep(0,84)
sum_sales_can_aft_13=rep(0,84)
n_can_13=rep(0,84)
n_usa_13=rep(0,84)
n_can_aft_13=rep(0,84)
n_usa_aft_13=rep(0,84)
ar_13=unique(bops_bm$`id (store)`)
for(j in 1:length(ar_13))
{
  for(i in 1:nrow(bops_bm))
  {
    if(bops_bm$`id (store)`[i]==ar[j])
    {
      if(bops_bm$year[i]==2011 & bops_bm$week[i]>=43 & bops_bm$after[i]==TRUE | (bops_bm$year[i]==2012 & bops_bm$week[i]>=1 &bops_bm$week[i]<=2))
      {
        if(bops_bm$usa[i]==TRUE)
        {
          sum_sales_usa_aft_13[j]= sum_sales_usa_aft_13[j]+bops_bm$sales[i]
          n_usa_aft_13[j]=n_usa_aft_13[j]+1
        }
        else if(bops_bm$usa[i]==FALSE)
        {
          sum_sales_can_aft_13[j]= sum_sales_can_aft_13[j]+bops_bm$sales[i]
          n_can_aft_13[j]=n_can_aft_13[j]+1
        }
      }
      else if(bops_bm$year[i]==2011 & bops_bm$week[i]>=30 & bops_bm$week[i]<43 & bops_bm$after[i]==FALSE)
      {
        if(bops_bm$usa[i]==TRUE)
        {
          sum_sales_usa_13[j]= sum_sales_usa_13[j]+bops_bm$sales[i]
          n_usa_13[j]=n_usa_13[j]+1
        }
        else if(bops_bm$usa[i]==FALSE)
        {
          sum_sales_can_13[j]= sum_sales_can_13[j]+bops_bm$sales[i]
          n_can_13[j]=n_can_13[j]+1
        }
      }
    }
  }
}
usa_avg_13=rep(0,84)
can_avg_13=rep(0,84)
usa_avg_aft_13=rep(0,84)
can_avg_aft_13=rep(0,84)
for(j in 1:length(ar_13))
{
  if(sum_sales_usa_13[j]>0)
    usa_avg_aft_13[j]=sum_sales_usa_aft_13[j]/n_usa_aft_13[j]
  else
    usa_avg_13[j]=0
  if(sum_sales_can_13[j]>0)
    can_avg_aft_13[j]=sum_sales_can_aft_13[j]/n_can_aft_13[j]
  else
    can_avg_13[j]=0
  if(sum_sales_usa_aft_13[j]>0)
    usa_avg_13[j]=sum_sales_usa_13[j]/n_usa_13[j]
  else
    usa_avg_aft_13[j]=0
  if(sum_sales_can_aft_13[j]>0)
    can_avg_13[j]=sum_sales_can_13[j]/n_can_13[j]
  else
    can_avg_13[j]=0
}
pct_chg_13=rep(0,84)
chg=rep(0,84)
for(j in 1:length(ar_13))
{
  pct_chg_13[j]=((usa_avg_aft_13[j]+can_avg_aft_13[j])-(usa_avg_13[j]+can_avg_13[j]))/((usa_avg_13[j]+can_avg_13[j]))*100
}

avg_pct_can_13= (mean(sum_sales_can_aft_13)-mean(sum_sales_can_13))/mean(sum_sales_can_13)
avg_pct_can_13*100

avg_pct_usa_13= (mean(sum_sales_usa_aft_13)-mean(sum_sales_usa_13))/mean(sum_sales_usa_13)
avg_pct_usa_13*100

#Regression for PCt change in USA
bops_13=rep(-1,84)
for(j in 1:84)
  for(i in 1:nrow(bops_bm))
    if(ar[j]==bops_bm$`id (store)`[i])
      if(bops_13[j]<0)
        bops_13[j]=bops_bm$usa[i]

regr_pct_13=lm(pct_chg_13~bops_13)
summary(regr_pct_13)

#Regression for DMA
sum_dma_after_13=rep(0,210)
sum_dma_before_13=rep(0,210)
n_aft_13=rep(0,210)
n_before_13=rep(0,210)
for(j in 1:length(unq))
{
  for(i in 1:nrow(bops_dma))
  {
    if(bops_dma$`id (DMA)`[i]==unq[j])
      if(bops_dma$year[i]==2011 & bops_dma$week[i]>=43 & bops_dma$after[i]==TRUE | (bops_dma$year[i]==2012 & bops_dma$week[i]>=1 &bops_dma$week[i]<=2))
      {
        sum_dma_after_13[j]= sum_dma_after_13[j]+bops_dma$sales[i]
        n_aft_13[j]=n_aft_13[j]+1
      }
    else if(bops_dma$year[i]==2011 & bops_dma$week[i]>=30 & bops_dma$week[i]<43 & bops_dma$after[i]==FALSE)
    {
      sum_dma_before_13[j]= sum_dma_before_13[j]+bops_dma$sales[i]
      n_before_13[j]=n_before_13[j]+1
    }
  }
}

dma_avg_aft_13=rep(0,length(unq))
dma_avg_bef_13=rep(0,length(unq))
store_id_13=rep(0,length(unq))

#df=data.frame(dma_avg_aft,dma_avg_bef,store_id)

for(j in 1:length(unq))
{
  dma_avg_aft_13[j]=mean(sum_dma_after_13[j])
  dma_avg_bef_13[j]=mean(sum_dma_before_13[j])
  store_id_13[j]=j
}

df=data.frame(dma_avg_aft_13,dma_avg_bef_13,store_id_13)

pct_chg_dma_13=rep(0,length(unq))

for(j in 1:length(unq))
{
  
  pct_chg_dma_13[j]=(((dma_avg_aft_13[j])-(dma_avg_bef_13[j]))/(dma_avg_bef_13[j]))*100
  
}
cls_13=rep(-1,210)
for(j in 1:210)
  for(i in 1:nrow(bops_dma))
    if(unq[j]==bops_dma$`id (DMA)`[i])
      if(cls_13[j]<0)
        cls_13[j]=bops_dma$close[i]

df=data.frame(dma_avg_aft_13,dma_avg_bef_13,store_id_13,cls_13)
cls_aft_13=rep(0,210)
cls_bef_13=rep(0,210)
for(j in 1:210)
{
  if(cls_13[j]==1)
  {
    cls_aft_13[j]=dma_avg_aft_13[j]
    cls_bef_13[j]=dma_avg_bef_13[j]
  }
}
n_13=length(which(df$cls_13==1))
n_13
pct_dma_cls_13=(((sum(cls_aft_13)/n_13)-(sum(cls_bef_13)/n_13))/(sum(cls_bef_13)/n_13))*100
pct_dma_cls_13
regr_dma_13=lm(pct_chg_dma_13~cls_13)
summary(regr_dma_13)