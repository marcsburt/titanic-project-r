library('sampling')
library('UsingR')
library('scales')
library('dplyr')
library('magrittr')


train <- read.csv('/home/marcburt/Documents/School/BU DA/R/Final/train.csv')
test <- read.csv('/home/marcburt/Documents/School/BU DA/R/Final/test.csv')

save(train, file = '/home/marcburt/Documents/School/BU DA/R/Final/train.Rdata')
save(test, file = '/home/marcburt/Documents/School/BU DA/R/Final/test.Rdata')



#### Combining data
data <- bind_rows(train, test)

save(data, file = '/home/marcburt/Documents/School/BU DA/R/Final/titanic_combo.Rdata')
str(data)


#### Number of rows in the data
nrow(data)




#### Most obvious, did more males of females make it alive?

barplot(table(train$Survived, train$Sex), ylab="Passanger #", col = c("blue", "red"))
legend("topleft",legend = c("Died","Survived"),fill=c("blue","red"),inset = .05)


#### Does family size have an affect on whether the passenger survived

data$Fsize <- data$SibSp + data$Parch + 1

ggplot(data[1:nrow(train),], aes(x = Fsize, fill = factor(Survived)))+
	geom_bar(stat = 'count', position = 'dodge')

#### Looks like we can break the family down to three groups, single, small, and large to create a distinction
# Got this idea from one of the participants of the Kaggle competition
data$FsizeP[data$Fsize == 1] <- 'single'
data$FsizeP[data$Fsize == 2] <- 'couple'
data$FsizeP[data$Fsize >= 3 & data$Fsize <=4] <- 'small'
data$FsizeP[data$Fsize >= 5] <- 'large'

mosaicplot(table(data$FsizeP, data$Survived), main = 'Survival based on family size')


#### Maybe we can look at something similar for the passenger class.
# I'm almost sure this will make a difference
mosaicplot(table(train$Survived,train$Pclass), main="Survival by Class",ylab="Class",xlab="Survived")


#### basic analysis of the fare -> how much did a ticket cost
fare <- as.numeric(data$Fare, na.rm = TRUE)
summary(fare, na.rm = TRUE)

#### Based on summary we see that the distribution is negatively skewed.
#We can see that in the following chart
hist(fare, ylab="Count", xlab = "Price", breaks = 50, col = 'purple')
#### Boxplot to show the same.  Number of outliers, the most notable being the person who paid 512 for their ticket
boxplot(fare, horizontal = TRUE, xaxt = 'n')
axis(side = 1, at = round(fivenum(fare),0), labels = TRUE, las = 2)

#### to adjust the data, I want to get rid of the outliers.  I find because it's so significantely
#skewed I don't get a good read on the distribution of the data.


remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

rfare <- remove_outliers(fare)
rfare <- na.omit(rfare)
head(rfare)
summary(rfare, na.rm = TRUE)
#### These look much better
# Single graphs
hist(rfare, ylab="Count", xlab = "Price", breaks = 50, col = 'purple')

boxplot(rfare, horizontal = TRUE, xaxt = 'n')
axis(side = 1, at = round(fivenum(fare),0), labels = TRUE, las = 2)

par(mfrow = c(2,2))
	hist(fare, ylab="Count", xlab = "Price", breaks = 25, col = 'purple', main = "With Outliers")
	boxplot(fare, horizontal = TRUE, xaxt = 'n', main = 'With Outliers')
		axis(side = 1, at = round(fivenum(fare),0), labels = TRUE, las = 2)

	hist(rfare, ylab="Count", xlab = "Price", breaks = 25, col = 'purple', main = 'Without Outliers')
	boxplot(rfare, horizontal = TRUE, xaxt = 'n', main = 'Without Outliers')
		axis(side = 1, at = round(fivenum(rfare),0), labels = TRUE, las = 2)


#### The data seems to be skewed to the left.


### I want to introduce some various sampling techniques to get more of a sense on how the data is distributed

xbar <- na.omit(rfare)

cat(" Distribution Mean = ", mean(rfare, na.rm = TRUE)," SD = ", sd(rfare, na.rm = TRUE), "\n")

par(mfrow = c(2,2))

for (size in c(50, 75, 100, 125)) {
    for (i in 1:length(xbar)) {
	    xbar[i] <- mean(sample(rfare, size = size, 
            replace = TRUE))
        }
    hist(xbar, prob = TRUE, main = paste("Sample Size =" , size))

    cat("Sample Size = ", size, " Mean = ", mean(xbar, na.rm = TRUE),
    " SD = ", sd(xbar, na.rm = TRUE), "\n")
    } 

#### Sampling on the survival rate

#### basic probability of survival
train$Survived%>%
	table
train$Survived%>%
	table%>%
	prop.table


#### SIMPLE SAMPLING

s <- srswor(20, nrow(train))
sample <- data[s != 0, ]
sample$Survived%>%
    table
sample$Survived%>%
    table%>%
    prop.table

####Systematic Sampling

N <- nrow(train)
n <- 20
k <- ceiling(N/n)
r <- sample(k, 1)

s <- seq(r, by=k, length = n)

sample.2 <- train[s,]

sample.2$Survived%>%
    table
sample.2$Survived%>%
    table%>%
    prop.table



#### Confidence intervals of data given the amount of the data
sample.size <- 50
pop.sd <- sd(rfare)
sd.sample.means <- pop.sd/sqrt(sample.size)
samples <- 20

xbar <- numeric(samples)

for (i in 1:samples){
	sample.data.1 <- sample(as.numeric(rfare), size = sample.size)
	xbar[i] <- mean(sample.data.1)
	str <- sprintf("%2d: xbar = %.2f, CI = %.2f-%.2f", i, xbar[i], xbar[i] - 2*sd.sample.means,xbar[i] + 2*sd.sample.means)
	cat(str, '\n')


}







#### Confidence Intervals at 80 and 90.

conf <- c(75,80,85,90,95)
alpha <- 1 - conf/100
sample.data <- sample(rfare, size = sample.size)
xbar <- mean(sample.data)
sd.sample.means <- pop.sd/sqrt(sample.size)


for (i in alpha){
	str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f-%.2f", 100*(1-i),i, xbar - qnorm(1-i/2)*sd.sample.means,xbar + qnorm(1-i/2)*sd.sample.means)
	cat(str, '\n')
}



