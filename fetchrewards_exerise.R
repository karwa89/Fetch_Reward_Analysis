#Loading library
options(suppressMessages=TRUE)
library(jsonlite)
library(R.utils)
library(tibble)
library(tidyr)
library(sqldf)

#### unzip gz files using file browser #####
gunzip(file.choose(), remove=FALSE)
gunzip(file.choose(), remove=FALSE)
gunzip(file.choose(), remove=FALSE)

#### Reading json files using file browser####
#reading brand file
brand <- stream_in(file(file.choose()))

#reading receipt file
receipts <- stream_in(file(file.choose()))

#reading user file
users <- stream_in(file(file.choose()))



#### Fixing Users table  ####

#creating user id column from _id
users$user_id <- users[,c(1)][,c(1)]
#Removing _id column
users[,c(1)] <- NULL

#Readjusting column index
users <- users[,c(7,1:6)]

#Checking Removing duplicated users
users <- users[!duplicated(users),]


#### Fixing brand table ####
length(brand$cpg$`$id`$`$oid`)

#creating brand id column from _id
brand$brand_id <- brand[,c(1)][,c(1)]

#Splitting cpg value from data frame into columns  
brand$cpg_id <- brand$cpg$`$id`$`$oid`
brand$cpg_ref <- brand$cpg$`$ref`

#Removing _id column
brand[,c(1)] <- NULL

#Removing cpg table column
brand$cpg <- NULL

#Readjusting column index
brand <- brand[,c(7,1:6,8,9)]

#Checking and Removing duplicated users
brand <- brand[!duplicated(brand),]


#### receipt data fix ####

#Exploding rewards receipt itemlist key columns 
receipts_1 <- receipts |> 
  unnest_longer(rewardsReceiptItemList,indices_include = TRUE)

#Extracting data from rewards receipt item list in data frame 
rewards <- receipts_1$rewardsReceiptItemList

#Renaming column names 
name <- paste0("rewardsReceiptItemList_",colnames(rewards))
names(rewards) <- name

#### Adding column back to receipt rewards  
receipts_1 <- cbind(receipts_1,rewards)

## removing rewards from memory space
rm(rewards)

#creating receipt id column from _id
receipts_1$receipt_id <- receipts_1[,c(1)][,c(1)]
receipts_1[,c(1)] <- NULL
receipts_1$rewardsReceiptItemList <- NULL

#Readjusting column index
receipts_1 <- receipts_1[,c(48,1:47)]


#### When considering total number of items purchased from receipts with 'rewardsReceiptStatus’ of ‘Accepted’ or ‘Rejected’, which is greater?
greater_item_purchased <- sqldf(
"select rewardsReceiptStatus,sum(item_purchase) as item_accepted from 
(select receipt_id,rewardsReceiptStatus, avg(purchasedItemCount) as item_purchase from receipts_1 
      where rewardsReceiptStatus in ('FINISHED','REJECTED') group by receipt_id,rewardsReceiptStatus) a 
group by rewardsReceiptStatus order by item_accepted desc limit 1"
)
greater_item_purchased


#### Checking distinct brandcode from  receipt and brand 
length(unique(receipts_1$rewardsReceiptItemList_brandCode))
length(unique(brand$brandCode))

receipts_1$rewardsReceiptItemList_brandCode <- trim(receipts_1$rewardsReceiptItemList_brandCode)
brand$brandCode <- trim(brand$brandCode)

### brand code missing info from brand table that are present in receipts
missing_brandcode_list <- data.frame(missing_brand = sort(unique(receipts_1$rewardsReceiptItemList_brandCode[!receipts_1$rewardsReceiptItemList_brandCode %in% brand$brandCode])))
nrow(missing_brandcode_list)

