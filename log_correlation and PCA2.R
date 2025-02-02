#stat 222 
setwd("C:/Users/Danni/Desktop/Spring_2018/STAT 222")

library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(readr)

# Calculate log return
#log(s_{t}/s_{t-1}) = log(s_{t}) - log(s_{t-1})

#stock <- read.csv("stock_data.csv")

##################### For 5 years##############
stock_5yrs <- read.csv("stock_price_5yrs.csv")
head(stock_5yrs)

###Daily Return
daily_5yrs_return = data.frame(stock_5yrs$Date[-1])
stock_5yrs_list = as.vector(colnames(stock_5yrs)[-c(1)])
for (name in stock_5yrs_list){
  daily_5yrs_return[[name]] = diff(log(stock_5yrs[[name]]))
}
write_csv(daily_5yrs_return, "daily_5yrs_return.csv")


log_5yrs_price <- cbind(stock_5yrs[,1], log(stock_5yrs[,-1]))
dim(log_5yrs_price)


log_5yrs_price$days <-  weekdays(as.Date(log_5yrs_price$`stock_5yrs[, 1]`))
log_5yrs_price$weeks <- week(as.Date(log_5yrs_price$`stock_5yrs[, 1]`))
log_5yrs_price$years <- year(log_5yrs_price$`stock_5yrs[, 1]`)
log_5yrs_price$weekcycles <- paste(log_5yrs_price$years, log_5yrs_price$weeks, sep = "")
log_5yrs_price$months <- month(log_5yrs_price$`stock_5yrs[, 1]`)
log_5yrs_price$monthcycles <- paste(log_5yrs_price$years, log_5yrs_price$months, sep = "")

#### Weekly return code
#### Updated weekly return for 5 yrs after taking difference between different last days in each week
last_week_5yrs_df = log_5yrs_price[!duplicated(log_5yrs_price[,"weekcycles"], fromLast=TRUE),]
#first_week_df = log_5yrs_price[!duplicated(log_5yrs_price$weekcycles),]
#week_df = merge(last_week_5yrs_df, first_week_df, )
rm_col = c("days", "weeks","years","weekcycles","stock_5yrs[, 1]", "months", "monthcycles")
#week_5yrs_return = last_week_5yrs_df[,!(colnames(last_week_5yrs_df) %in% rm_col )] - first_week_df[,!(colnames(first_week_df) %in% rm_col )]
week_5yrs_return = data.frame(last_week_5yrs_df$`stock_5yrs[, 1]`[-1])
week_5yrs_list = as.vector(colnames(last_week_5yrs_df)[!colnames(last_week_5yrs_df) %in% rm_col])
for (name in week_5yrs_list){
  week_5yrs_return[[name]] = diff(last_week_5yrs_df[[name]])
}
#week_5yrs_return$date <- unique(log_5yrs_price$weekcycles)[-1]
write_csv(week_5yrs_return, "week_5yrs_return.csv")

#### Monthly return code
#### Updated monthly return for 5 yrs after taking difference between different last days in each month
last_month_5yrs_df = log_5yrs_price[!duplicated(log_5yrs_price[,"monthcycles"], fromLast=TRUE),]
#first_month_df = log_5yrs_price[!duplicated(log_5yrs_price$monthcycles),]
#month_5yrs_return = last_month_5yrs_df[,!(colnames(last_month_5yrs_df) %in% rm_col )] - first_month_df[,!(colnames(first_month_df) %in% rm_col )]
month_5yrs_return = data.frame(last_month_5yrs_df$`stock_5yrs[, 1]`[-1])
month_5yrs_list = as.vector(colnames(last_month_5yrs_df)[!colnames(last_month_5yrs_df) %in% rm_col])
for (name in month_5yrs_list){
  month_5yrs_return[[name]] = diff(last_month_5yrs_df[[name]])
}
write_csv(month_5yrs_return, "month_5yrs_return.csv")



"CVX" %in% colnames(month_5yrs_return)

month_5yrs_return$date = as.Date(month_5yrs_return$last_month_5yrs_df..stock_5yrs...1....1.)
month_5yrs_return = month_5yrs_return[rev(order(as.Date(month_5yrs_return$last_month_5yrs_df..stock_5yrs...1....1.))),]

ggplot(month_5yrs_return, aes(x=last_month_5yrs_df..stock_5yrs...1....1., group = 1)) + 
  geom_line(aes(y = CVX, color = "Chevron")) +
  geom_line(aes(y = AAL, color="American Airlines")) +
  xlab('Date') +
  ylab('Stock Price')+
  ggtitle("Stock Price of Facebook&McDonalds")



########################### For 15 Years ###########################################

stock_15yrs <- read.csv("stock_price_15yrs.csv")
head(stock_15yrs)

###Daily Return
daily_15yrs_return = data.frame(stock_15yrs$Date[-1])
stock_15yrs_list = as.vector(colnames(stock_15yrs)[-c(1)])
for (name in stock_15yrs_list){
  daily_15yrs_return[[name]] = diff(log(stock_15yrs[[name]]))
}
write_csv(daily_15yrs_return, "daily_15yrs_return.csv")

log_15yrs_price <- cbind(stock_15yrs[,1], log(stock_15yrs[,-1]))

log_15yrs_price$days <-  weekdays(as.Date(log_15yrs_price$`stock_15yrs[, 1]`))
log_15yrs_price$weeks <- week(as.Date(log_15yrs_price$`stock_15yrs[, 1]`))
log_15yrs_price$years <- year(log_15yrs_price$`stock_15yrs[, 1]`)
log_15yrs_price$weekcycles <- paste(log_15yrs_price$years, log_15yrs_price$weeks, sep = "")
log_15yrs_price$months <- month(log_15yrs_price$`stock_15yrs[, 1]`)
log_15yrs_price$monthcycles <- paste(log_15yrs_price$years, log_15yrs_price$months, sep = "")

#### Weekly return code
#### Updated weekly return for 5 yrs after taking difference between different last days in each week
last_week_15yrs_df = log_15yrs_price[!duplicated(log_15yrs_price[,"weekcycles"], fromLast=TRUE),]
rm_15yrs_col = c("days", "weeks","years","weekcycles","stock_15yrs[, 1]", "months", "monthcycles")
week_15yrs_return = data.frame(last_week_15yrs_df$`stock_15yrs[, 1]`[-1])
week_15yrs_list = as.vector(colnames(last_week_15yrs_df)[!colnames(last_week_15yrs_df) %in% rm_15yrs_col])
for (name in week_15yrs_list){
  week_15yrs_return[[name]] = diff(last_week_15yrs_df[[name]])
}
write_csv(week_15yrs_return, "week_15yrs_return.csv")

#### Monthly return code
#### Updated monthly return for 5 yrs after taking difference between different last days in each month
last_month_15yrs_df = log_15yrs_price[!duplicated(log_15yrs_price[,"monthcycles"], fromLast=TRUE),]
month_15yrs_return = data.frame(last_month_15yrs_df$`stock_15yrs[, 1]`[-1])
month_15yrs_list = as.vector(colnames(last_month_15yrs_df)[!colnames(last_month_15yrs_df) %in% rm_15yrs_col])
for (name in month_15yrs_list){
  month_15yrs_return[[name]] = diff(last_month_15yrs_df[[name]])
}
write_csv(month_15yrs_return, "month_15yrs_return.csv")


stock_15yrs[1,1]
stock_15yrs[3776,1]


######################### For 35 companies around 2007 - 2012
library(ggcorrplot)
### correlation matrix 
corr_matrix <- function(input_mat){
  corr <- data.frame(row.names = colnames(input_mat))
  for (i in 1:length(input_mat)){
    for (j in 1:length(input_mat)){
      corr[i,j] <- ccf(input_mat[,i], input_mat[,j], lag.max = 0, plot = FALSE, type = "correlation")
    }
  }
  colnames(corr) <- colnames(input_mat)
  return(corr)
}

rd_stocks_list <- sample(week_15yrs_list, 35)
# Weekly From 2007-01 to 2011-12
rd_stocks_df <- week_15yrs_return[201:465, colnames(week_15yrs_return) %in% rd_stocks_list] 
dim(rd_stocks_df)
corr_week <- corr_matrix(rd_stocks_df)
ggcorrplot(corr_week, hc.order = TRUE, type = "lower",outline.col = "white", 
           title = "Correlation Heat Map of Weekly Log Return During Financial Crisis (2007 Jan - 2011 Dec)")

rd_stocks_5yrs <- week_5yrs_return[, colnames(week_5yrs_return) %in% rd_stocks_list]
dim(rd_stocks_5yrs)
corr_week_5yrs <- corr_matrix(rd_stocks_5yrs)
ggcorrplot(corr_week_5yrs, hc.order = TRUE, type = "lower",outline.col = "white",
           title = "Correlation Heat Map of Weekly Log Return in Recent Five Years(2013 Mar - 2018 Mar)")


# Monthly From 2008-01 to 2012-12
rd_stocks_df_month <- month_15yrs_return[58:117, colnames(month_15yrs_return) %in% rd_stocks_list] 
dim(rd_stocks_df_month)
corr_month <- corr_matrix(rd_stocks_df_month)
ggcorrplot(corr_month, hc.order = TRUE, type = "lower",outline.col = "white", 
           title = "Correlation Heat Map of Monthly Log Return During Financial Crisis (2008 Jan - 2012 Dec)")

rd_stocks_5yrs_month <- month_5yrs_return[ , colnames(month_5yrs_return) %in% rd_stocks_list]
dim(rd_stocks_5yrs_month)
corr_month_5yrs <- corr_matrix(rd_stocks_5yrs_month)
ggcorrplot(corr_month_5yrs, hc.order = TRUE, type = "lower",outline.col = "white",
           title = "Correlation Heat Map of Monthly Log Return in Recent Five Years(2013 Mar - 2018 Mar)")


# For Rui's PCA
FCperiods_for_PCA <- month_15yrs_return[58:117,]
dim(FCperiods_for_PCA)
write_csv(FCperiods_for_PCA,"FCperiods_for_PCA.csv")

################# Pie Chart For Stock Industry Composition 

data_source <- read.csv("Russell_data.csv")
head(data_source)  

#### Pie Chart 
sectors <- data_source %>% group_by(Sector) %>% summarise(total = n())
sectors_prop <- sectors %>%
    arrange(desc(total)) %>%
    mutate(prop = percent(total / sum(total)),
           labels = paste0(prop,"(",total,")")) 
pie <- ggplot(sectors_prop, aes(x = "", y = total, fill = fct_inorder(Sector))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
#  geom_label_repel(aes(label = labels), size=1, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Sector"))
pie


###################### EDA for 5yrs and 15yrs / FEI

day_r15 <- read.csv("/Users/Starryfifi/Desktop/STAT222 Capstone/daily_15yrs_return.csv")
day_r15$date = as.Date(day_r15$stock_15yrs.Date..1.)

"ALK" %in% colnames(day_r15)

c1 = ccf(day_r15$CVX, day_r15$ALK, plot = FALSE, lag.max =0, type = "correlation")
#0.229

p1=ggplot() + 
  geom_line(data = day_r15, aes(x = date, y = CVX, color = "Chevron Corp.")) +
  geom_line(data = day_r15, aes(x = date, y = ALK, color = "Alaska Air")) +
  xlab('Date') +
  ylab('Stock Price')+
  ggtitle("Daily Return of Chevron&Alaska Air")

week_r15 <- read.csv("/Users/Starryfifi/Desktop/STAT222 Capstone/week_15yrs_return.csv")
week_r15$date = as.Date(week_r15$last_week_15yrs_df..stock_15yrs...1....1.)

c2 = ccf(week_r15$CVX, week_r15$ALK, plot = FALSE, lag.max =0, type = "correlation")
#0.185

p2=ggplot() + 
  geom_line(data = week_r15, aes(x = date, y = CVX, color = "Chevron Corp.")) +
  geom_line(data = week_r15, aes(x = date, y = ALK, color = "Alaska Air")) +
  xlab('Date') +
  ylab('Stock Price')+
  ggtitle("Weekly Return of Chevron&Alaska Air")


month_r15 <- read.csv("/Users/Starryfifi/Desktop/STAT222 Capstone/month_15yrs_return.csv")
month_r15$date = as.Date(month_r15$last_month_15yrs_df..stock_15yrs...1....1.)

c3 = ccf(month_r15$CVX, month_r15$ALK, plot = FALSE, lag.max =0, type = "correlation")
#-0.021

p3=ggplot() + 
  geom_line(data = month_r15, aes(x = date, y = CVX, color = "Chevron Corp.")) +
  geom_line(data = month_r15, aes(x = date, y = ALK, color = "Alaska Air")) +
  xlab('Date') +
  ylab('Stock Price')+
  ggtitle("Monthly Return of Chevron Corp&Alaska Air")

################################# 500 companies #################
head(data_source)
tmp15 <- colnames(stock_15yrs)
tmp5 <- colnames(stock_5yrs)
longer_comp <- data_source[data_source$Ticker %in% tmp5,]
tmpNo <- tmp5[!(tmp5 %in% tmp15)]
shorter_company <- longer_comp[longer_comp$Ticker %in% tmpNo,]
write_csv(shorter_company,"shorter_company.csv")

############################## Dow for 5 years Monthly
dim(stock_5yrs)
stock_5yrs$Date[1250:1258]

Dow <- read.csv("DJI_5yrs.csv")
Dow <- as.data.frame(Dow[ ,c("Date", "Adj.Close")])

Dow$months <- month(Dow$Date)
Dow$years <- year(Dow$Date)
Dow$monthcycles <- paste(Dow$years, Dow$months, sep = "")

#### Monthly return code
last_month_Dow = Dow[!duplicated(Dow[,"monthcycles"], fromLast=TRUE),]
Dow_log_return = data.frame(last_month_Dow$Date[-1])
Dow_log_return$log_return <- diff(log(last_month_Dow$Adj.Close))
dim(month_5yrs_return)
dim(Dow_log_return)

pca_5yrs <- prcomp(month_5yrs_return[,-1], scale.= TRUE, center = FALSE)
dim(month_5yrs_return)


pc1_dow = cbind(as.data.frame(month_5yrs_return$last_month_5yrs_df..stock_5yrs...1....1.), pca_5yrs$x[,1])
pc_dow_comb <- cbind(pc1_dow, Dow_log_return$log_return)
colnames(pc_dow_comb) <- c("Date", "PCA_X", "log_return")
pc_dow_comb$Date <- as.Date(pc_dow_comb$Date)

dim(pca_5yrs$rotation)
dim(month_5yrs_return)
x = diag(as.matrix(month_5yrs_return[,-1]) %*% as.matrix(pca_5yrs$rotation))

pc_dow_comb <- cbind(pc_dow_comb, as.data.frame(x))

library(latticeExtra)



colnames(pc_dow_comb)
obj1_dow <- xyplot(x ~ Date, pc_dow_comb, type = "l" , lwd=2, ylab = "PC1")
obj2_dow <- xyplot(log_return ~ Date, pc_dow_comb, type = "l", lwd=2, ylab = "Monthly log return of Dow")
doubleYScale(obj1_dow, obj2_dow, add.ylab2 = TRUE)

ccf(pc_dow_comb$x, pc_dow_comb$log_return, plot = FALSE, lag.max =0, type = "correlation")


############################## S&P500 for 15 years Monthly
SP500 <- read.csv("GSPC.csv")
SP500 <- as.data.frame(SP500[ ,c("Date", "Adj.Close")])

SP500$months <- month(SP500$Date)
SP500$years <- year(SP500$Date)
SP500$monthcycles <- paste(SP500$years, SP500$months, sep = "")

#### Monthly return code
last_month_SP500 = SP500[!duplicated(SP500[,"monthcycles"], fromLast=TRUE),]
SP500_log_return = data.frame(last_month_SP500$Date[-1])
SP500_log_return$log_return <- diff(log(last_month_SP500$Adj.Close))
dim(month_15yrs_return)
dim(SP500_log_return)

pca_15yrs <- prcomp(month_15yrs_return[,-1], scale.= TRUE, center = FALSE)

pc1_sp = cbind(as.data.frame(month_15yrs_return$last_month_15yrs_df..stock_15yrs...1....1.), pca_15yrs$x[,1])
pc_sp_comb <- cbind(pc1_sp, SP500_log_return$log_return)
colnames(pc_sp_comb) <- c("Date", "PCA_X", "log_return")
pc_sp_comb$Date <- as.Date(pc_sp_comb$Date)

dim(pca_15yrs$rotation)
dim(month_15yrs_return)
y = diag(as.matrix(month_15yrs_return[,-1]) %*% as.matrix(pca_15yrs$rotation))
pc_sp_comb <- cbind(pc_sp_comb, as.data.frame(y))

colnames(pc_sp_comb)

obj1_sp <- xyplot((-1)*PCA_X ~ Date, pc_sp_comb, type = "l" , lwd=2, ylab = "PC1")
obj2_sp <- xyplot(log_return ~ Date, pc_sp_comb, type = "l", lwd=2, ylab = "Monthly log return of SP500")
doubleYScale(obj1_sp, obj2_sp, add.ylab2 = TRUE)
ccf((pc_sp_comb$PCA_X)*(-1), pc_sp_comb$log_return, plot = FALSE, lag.max =0, type = "correlation")


#########################################################

# Calculate PCA after taking log
log_return_clean<-log_return[,-c(1,2)]
PCA <- prcomp(log_return_clean, scale.=TRUE)
PCA

plot(PCA$rotation[,1], PCA$rotation[,2])
abline(h = 0, v = 0, col = 'gray80')
text(PCA$rotation[,1], PCA$rotation[,2]+0.01, labels = colnames(stock_data_all))

####################################################
dis <- sqrt(2*(1-corr))
install.packages("vegan")
library(vegan)
tr<- spantree(dis)
plot(tr, cmdscale(dis), type = 'p')

install.packages("pajek")
install.packages("igraph")
library(igraph)
pt <- plot(tr, cmdscale(dis), type = 'p')
g <- erdos.renyi.game(10, 3/10)

head(corr)
corr <- as.numeric(corr)
corr <- as.matrix(corr)
corr = apply(corr, 2, function(x) as.numeric(x))
colnames(corr)


my_g <- graph.adjacency(corr, mode = "undirected", weighted = TRUE, diag = FALSE)
my_g1<-graph.adhesion(my_g)
mst <- minimum.spanning.tree(my_g)
plot.igraph(mst, 
            layout = layout.lgl,
            vertex.label = colnames(corr),
            vertex.label.font = 2,
            vertex.size = 12,
            vertex.color = "light blue",
            vertex.label.cex = 0.60,
            vertex.label.color = "black")
