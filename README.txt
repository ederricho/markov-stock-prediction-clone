This program uses data from the closing price of a stock (xlsx file) and creates a 
Markov Chain to predict the price for the next day. The program does this by
predicting the likelyhood of the price moving from one state to the other
(observe the statespace below) and uses rnorm (normal distribution) with one value to create the noise/jump for the predicted value of the change in price.

Details on the Markov Chain (p stands for price):
The statespace for the change in price from day n to day n+1 is as follows:
1) p < -$2.00 (change in price is less than $2.00) 
2) -2 > p > -1 (change in price is between -$1.00 and -$2.00)
3) -1 > p > 0 (change in price is between -$1.00 and $0.00)
4) 0 > p > 1 (change in price is between $0.00 and $1.00)
5) 1 > p > 2 (change in price is between $1.00 and $2.00)
6) p > $2.00 (change in price is greater than $2.00)

This also uses ggplot2 to plot all of the graphs.