#install.packages("ISLR")
#install.packages("MASS")
# including all libraries
library(ISLR)
library(MASS)

# viewing data set
names(Smarket)
dim(Smarket)
summary(Smarket)

# produces a matrix that contains all of the pairwise correlations among the predictors
pairs(Smarket)
#cor(Smarket)
cor(Smarket[,-9])

# plotting
attach(Smarket)
plot(Volume)

# fitting a logistic regression model using Lag1 through Lag5 and Volume
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial)
summary(glm.fits)

# access just the coefficients and their p-values for the fitted model
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

# used to predict the probability that the market will go up
glm.probs = predict(glm.fits,type = "response")
glm.probs[1:10]
contrasts(Direction)

# creates a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0.5
glm.pred = rep("Down",1250)
glm.pred[glm.probs > .5] = "Up"

# determines how many observations were correctly or incorrectly classified
# logistic regression correctly predicted 52.2% of the time
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred == Direction)

# fitting the model using part of the data and examine how well it predicts the held out data
# held out data will be observations from 2005 and examined data will be observations from 2001-2004
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial,subset = train)
glm.probs = predict(glm.fits,Smarket.2005,type = "response")

# comparing predictions to the actual movements of the market over that time period
# logistic regression using a subset correctly predicted 48% of the time which is worse
glm.pred = rep("Down",252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

# refitting the logistic regression model using just Lag1 and Lag2
# this predicts correctly 56% of the time
glm.fits = glm(Direction~Lag1+Lag2,data = Smarket,family = binomial,subset = train)
glm.probs = predict(glm.fits,Smarket.2005,type = "response")
glm.pred = rep("Down",252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)

# predicting the returns associated with particular values of Lag1 and Lag2
predict(glm.fits,newdata = data.frame(Lag1 = c(1.2,1.5),Lag2 = c(1.1,-0.8)),type = "response")

# fitting an lda model using only the observations before 2005 and observing data
lda.fit = lda(Direction~Lag1+Lag2,data = Smarket,subset = train)
lda.fit
plot(lda.fit)

# lda's predictions about the movement of the market
# predicted correctly 56% of the time and is almost identical to logistic regression
lda.pred = predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)

# testing probabily of the market decreasing by changing the threshold
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1] > .9)
