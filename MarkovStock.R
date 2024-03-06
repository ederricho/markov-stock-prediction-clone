# This code is the synthetic creation of stock data using markov chains

# Place all values of the closing price
values = Apple_5yr$Close[0:200]
x1 = c(1:length(values))

# Compare the differences
first_der = diff(values)/diff(x1)
x2 = c(1:length(first_der))

hist(first_der)

# Get the standard deviation:
std = sd(first_der)

# Get values that are two standard deviations away from the mean
sd.values = first_der[first_der>2*std | first_der< (-1*2)*std]

#hist(sd.values)

# Graph the "differences"
ggplot(data.frame(first_der), 
       aes(x=x2,y=first_der)) + geom_point()

#-------------------------------------------------------------------------------
# ----------------- Create Markov Chain for "first derivative" -----------------
#-------------------------------------------------------------------------------

# Retrieve Probabilities

# Between 0 and 1 ()
z.o = first_der[first_der>0 & first_der<1]
zero_one = length(z.o)/length(first_der)
mean1 = mean(z.o)
sd1 = sd(z.o)

# Between 1 and 2 ()
o.t = first_der[first_der>1 & first_der<2]
one_two = length(o.t)/length(first_der)
mean2 = mean(o.t)
sd2 = sd(o.t)

# Between 0 and -1 ()
z.n.o = first_der[first_der<0 & first_der>-1]
zero_n_one = length(z.n.o)/length(first_der)
mean3 = mean(z.n.o)
sd3 = sd(z.n.o)

# Between -1 and -2 ()
n.n.t = first_der[first_der<(-1) & first_der>(-2)]
none_n_two = length(n.n.t)/length(first_der)
mean4 = mean(n.n.t)
sd4 = sd(n.n.t)

# Greater than 2 ()
g.val = first_der[first_der>2]
greater = length(g.val)/length(first_der)
mean5 = mean(g.val)
sd5 = sd(g.val)

# Less than -2 ()
l.val = first_der[first_der<(-1*2)]
less = length(l.val)/length(first_der)
mean6 = mean(l.val)
sd6 = sd(l.val)

#----------------------------------------------------------------------------------
# Probabilities for T-Matrix:

#a: between 0 and 1
#b: between 1 and 2
#c: greater than 2
#d: between -1 and 0
#e: between -1 and -2
#f: less than -2

#----------------------------------------------------------------------------------
# Initialize counts for transition probabilities

aa.count = 0
ab.count = 0
ac.count = 0
ad.count = 0
ae.count = 0
af.count = 0
#------------
bb.count = 0
ba.count = 0
bc.count = 0
bd.count = 0
be.count = 0
bf.count = 0
#------------
cc.count = 0
ca.count = 0
cb.count = 0
cd.count = 0
ce.count = 0
cf.count = 0
#------------
dd.count = 0
da.count = 0
db.count = 0
dc.count = 0
de.count = 0
df.count = 0
#------------
ee.count = 0
ea.count = 0
eb.count = 0
ec.count = 0
ed.count = 0
ef.count = 0
#------------
ff.count = 0
fa.count = 0
fb.count = 0
fc.count = 0
fd.count = 0
fe.count = 0

#------------------------------------------------------------------------------
# Create Probability Count for T-Matrix
for (i in c(1:(length(first_der)-1))){
  # a to a
  if((first_der[i]>0 & first_der[i]<1) & (first_der[i+1]>0 & first_der[i+1]<1)){
    aa.count = aa.count + 1
    print(i)
    # a to b next
  }else if((first_der[i]>0 & first_der[i]<1) & (first_der[i+1]>1 & first_der[i+1]<2)){
    ab.count = ab.count + 1
    print(i)
    # a to c next
  }else if((first_der[i]>0 & first_der[i]<1) & (first_der[i+1]>2)){
    ac.count = ac.count + 1
    print(i)
    # a to d next
  }else if((first_der[i]>0 & first_der[i]<1) & (first_der[i+1]> (-1*1) & first_der[i+1]<0)){
    ad.count = ad.count + 1
    print(i)
    # a to e next
  }else if((first_der[i]>0 & first_der[i]<1) & (first_der[i+1]> (-2*1)) & first_der[i+1]<(-1*1)){
    ae.count = ae.count + 1
    print(i)
    # a to f next
  }else if((first_der[i]>0 & first_der[i]<1) & first_der[i+1]< (-2*1)){
    af.count = af.count + 1
    print(i)
#------------------------------------------------------------------------------------- 
    # B ProbBILITIES
  }else if((first_der[i]>1 & first_der[i]<2) & (first_der[i+1]>0 & first_der[i+1]<1)){
    ba.count = ba.count + 1
    print(i)
    # a to c next
  }else if((first_der[i]>1 & first_der[i]<2) & (first_der[i+1]>1 & first_der[i+1]<2)){
    bb.count = bb.count + 1
    print(i)
    # a to c next
  }else if((first_der[i]>1 & first_der[i]<2) & (first_der[i+1]>2)){
    bc.count = bc.count + 1
    print(i)
    # a to d next
  }else if((first_der[i]>1 & first_der[i]<2) & (first_der[i+1]> (-1*1)) & first_der[i+1]<0){
    bd.count = bd.count + 1
    print(i)
    # a to e next
  }else if((first_der[i]>1 & first_der[i]<2) & (first_der[i+1]> (-2*1) & first_der[i+1]< (-1*1))){
    be.count = be.count + 1
    print(i)
    # a to f next
  }else if((first_der[i]>1 & first_der[i]<2) & first_der[i+1]< (-2*1)){
    bf.count = bf.count + 1
    print(i)
#------------------------------------------------------------------------------------------
    # C Probabilities
  }else if(first_der[i]>2 & (first_der[i+1]>0 & first_der[i+1]<1)){
    ca.count = ca.count + 1
    print(i)
    # a to c next
  }else if(first_der[i]>2 & (first_der[i+1]>1 & first_der[i+1]<2)){
    cb.count = cb.count + 1
    print(i)
  # a to c next
  }else if(first_der[i]>2 & (first_der[i+1]>2)){
    cc.count = cc.count + 1
    print(i)
  # a to d next
  }else if(first_der[i]>2 & (first_der[i+1]>-1 & first_der[i+1]<0)){
    cd.count = cd.count + 1
    print(i)
  # a to e next
  }else if(first_der[i]>2 & (first_der[i+1]> (-2*1) & first_der[i+1]< (-1*1))){
    ce.count = ce.count + 1
    print(i)
  # a to f next
  }else if(first_der[i]>2 & first_der[i+1]< (-2*1)){
    cf.count = cf.count + 1
    print(i)
#--------------------------------------------------------------------------------
    # D Probabilities
  }else if((first_der[i]> (-1*1) & first_der[i]<0) & (first_der[i+1]>0 & first_der[i+1]<1)){
      da.count = da.count + 1
      print(i)
      # a to c next
  }else if((first_der[i]> (-1*1) & first_der[i]<0) & (first_der[i+1]>1 & first_der[i+1]<2)){
    db.count = db.count + 1
    print(i)
    # a to c next
  }else if((first_der[i]> (-1*1) & first_der[i]<0) & (first_der[i+1]>2)){
    dc.count = dc.count + 1
    print(i)
    # a to d next
  }else if((first_der[i]> (-1*1) & first_der[i]<0) & (first_der[i+1]> (-1*1) & first_der[i+1]<0)){
    dd.count = dd.count + 1
    print(i)
    # a to e next
  }else if((first_der[i]> (-1*1) & first_der[i]<0) & (first_der[i+1]> (-2*1) & first_der[i+1]< (-1*1))){
    de.count = de.count + 1
    print(i)
    # a to f next
  }else if((first_der[i]> (-1*1) & first_der[i]<0) & first_der[i+1]< (-2*1)){
    df.count = df.count + 1
    print(i)
#-------------------------------------------------------------------------------------------
    # E Probabilities
  }else if((first_der[i]> (-2*1) & first_der[i]< (-1*1)) & (first_der[i+1]>0 & first_der[i+1]<1)){
    ea.count = ea.count + 1
    print(i)
    # a to c next
  }else if((first_der[i]> (-2*1) & first_der[i]< (-1*1)) & (first_der[i+1]>1 & first_der[i+1]<2)){
    eb.count = eb.count + 1
    print(i)
  # a to c next
  }else if((first_der[i]> (-2*1)) & first_der[i]< (-1*1) & (first_der[i+1]>2)){
    ec.count = ec.count + 1
    print(i)
  # a to d next
  }else if((first_der[i]> (-2*1) & first_der[i]< (-1*1)) & (first_der[i+1]> (-1*1) & first_der[i+1]<0)){
    ed.count = ed.count + 1
    print(i)
  # a to e next
  }else if((first_der[i]> (-2*1) & first_der[i]< (-1*1)) & (first_der[i+1]> (-2*1) & first_der[i+1]< (-1*1))){
    ee.count = ee.count + 1
    print(i)
  # a to f next
  }else if((first_der[i]> (-2*1) & first_der[i]< (-1*1) & first_der[i+1]< (-2*1))){
    ef.count = ef.count + 1
    print(i)
#---------------------------------------------------------------------------------------------
    # F Probabilities
  }else if((first_der[i]< (-2*1)) & (first_der[i+1]>0 & first_der[i+1]<1)){
    fa.count = fa.count + 1
    print(i)
    # a to c next
  }else if((first_der[i]< (-2*1)) & (first_der[i+1]>1 & first_der[i+1]<2)){
    fb.count = fb.count + 1
    print(i)
  # a to c next
  }else if((first_der[i]< (-2*1)) & (first_der[i+1]>2)){
    fc.count = fc.count + 1
    print(i)
  # a to d next
  }else if((first_der[i]< (-2*1)) & (first_der[i+1]> (-1*1) & first_der[i+1]<0)){
    fd.count = fd.count + 1
    print(i)
  # a to e next
  }else if((first_der[i]< (-2*1)) & (first_der[i+1]> (-2*1) & first_der[i+1]< (-1*1))){
    fe.count = fe.count + 1
    print(i)
  # a to f next
  }else if((first_der[i]< (-2*1)) & first_der[i+1]< (-2*1)){
    ff.count = ff.count + 1
    print(i)
  }
  
}

# Probabilities for each vector:
# a to a
aa.vector = aa.count/length(first_der)
ab.vector = ab.count/length(first_der)
ac.vector = ac.count/length(first_der)
ad.vector = ad.count/length(first_der)
ae.vector = ae.count/length(first_der)
af.vector = af.count/length(first_der)

ba.vector = ba.count/length(first_der)
bb.vector = bb.count/length(first_der)
bc.vector = bc.count/length(first_der)
bd.vector = bd.count/length(first_der)
be.vector = be.count/length(first_der)
bf.vector = bf.count/length(first_der)

ca.vector = ca.count/length(first_der)
cb.vector = cb.count/length(first_der)
cc.vector = cc.count/length(first_der)
cd.vector = cd.count/length(first_der)
ce.vector = ce.count/length(first_der)
cf.vector = cf.count/length(first_der)

da.vector = da.count/length(first_der)
db.vector = db.count/length(first_der)
dc.vector = dc.count/length(first_der)
dd.vector = dd.count/length(first_der)
de.vector = de.count/length(first_der)
df.vector = df.count/length(first_der)

ea.vector = ea.count/length(first_der)
eb.vector = eb.count/length(first_der)
ec.vector = ec.count/length(first_der)
ed.vector = ed.count/length(first_der)
ee.vector = ee.count/length(first_der)
ef.vector = ef.count/length(first_der)

fa.vector = fa.count/length(first_der)
fb.vector = fb.count/length(first_der)
fc.vector = fc.count/length(first_der)
fd.vector = fd.count/length(first_der)
fe.vector = fe.count/length(first_der)
ff.vector = ff.count/length(first_der)

# Transition Matrix:

# initialize a 6x6 zero matrix
t.vec = c(aa.vector,ab.vector,ac.vector,ad.vector,ae.vector,af.vector,
          ba.vector,bb.vector,bc.vector,bd.vector,be.vector,bf.vector,
          ca.vector,cb.vector,cc.vector,cd.vector,ce.vector,cf.vector,
          da.vector,db.vector,dc.vector,dd.vector,de.vector,df.vector,
          ea.vector,eb.vector,ec.vector,ed.vector,ee.vector,ef.vector,
          fa.vector,fb.vector,fc.vector,fd.vector,fe.vector,ff.vector)
t.matrix = matrix(t.vec,nrow=6,ncol = 6)

print(t.matrix)

# Markov chain:
mc.matrix = state.matrix %*% t.matrix 

print(mc.matrix)

# Use the index to chose a number in the specific parameter and create a "clone" data set using MC

clone.data = c(values[1],values[2])
count = 0
sd = sd(first_der)
plot.list = list()
jump.v = c(0)

# Calculate the "Cloned" data
for(l in (2:length(first_der))){
  # Create State Matrix
  mat.vec = c(0,0,0,0,0,0)
  
  if(first_der[l]>=0 & first_der[l]<1){
    mat.vec[1] = 1
  }else if(first_der[l]>=1 & first_der[l]<2){
    mat.vec[2] = 1
  }else if(first_der[l]>=2){
    mat.vec[3] = 1
  }else if(first_der[l]>=(-1*1) & first_der[l]<0){
    mat.vec[4] = 1
  }else if(first_der[l]<(-1*1) & first_der[l]>(-2*1)){
    mat.vec[5] = 1
  }else if(first_der[l]<(-2*1)){
    mat.vec[6] = 1
  }
  state.matrix = matrix(mat.vec,nrow = 1,ncol = 6) 
  #print(state.matrix)
  
  mc.matrix = state.matrix %*% t.matrix
  #print(mc.matrix)
  
  # Take the greatest probability
  max = max(mc.matrix)
  # Find the Index
  prob.index = which(mc.matrix==max)
  
  # This is used if there are duplicate max probabilities in prob.index 
  if(length(prob.index)>1){
    prob.index = prob.index[1]
  }
  #print(mc.matrix)
  print(prob.index)
  #-------------------------------------------------------------------------
  # Assumption: Each partition of probabilities has >30 samples, this allows
  # us to assume normality
  if(prob.index==1){ # Between 0 and 1
    jump = rnorm(1,mean1,sd1)
    jump.v = append(jump.v,jump)
  }else if(prob.index==2){ # Between 1 and 2 
    jump = rnorm(1,mean2,sd2)
    jump.v = append(jump.v,jump)
  }else if(prob.index==3){ # Greater than 2
    jump = rnorm(1,mean3,sd3)
    jump.v = append(jump.v,jump)
  }else if(prob.index==4){ # Between -1 and 0
    jump = rnorm(1,mean4,sd4)
    jump.v = append(jump.v,jump)
  }else if(prob.index==5){ # Between -1 and -2
    jump = rnorm(1,mean5,sd5)
    jump.v = append(jump.v,jump)
  }else if(prob.index==6){ # Less than 2
    jump = rnorm(1,mean6,sd6)
    jump.v = append(jump.v,jump)
  }
  
  print(jump)
  
  clone.data = append(clone.data,clone.data[l-1] + (1/1)*jump)# * rnorm(1,mean(first_der),sd))
  #count = count + 1
  #print("Count")
  #print(l)
  
  #png(file = "C:/Users/EJ's/Desktop/R Pictures/pic.png")
  
  file.name = paste("plot",l,".png")
  png(file.name)
  plot(c(values[l],clone.data[l]),
       xlim=c(0,3),
       ylim=c(0,200),
       col=c('blue','red'),
       xlab="Index",
       ylab="Price"
       )
  legend("bottomright",c("Blue: Actual","Red: Markov Chain"))
  dev.off()
  #plot.list[[l]]= p.yes
  
}
# Remove Oscilations
for(p in c(1:length(clone.data))){
  if(p%%2==0){
    clone.data[p]=clone.data[p-1]
  }
}
print(clone.data)

# Plot both the difference between the values and the noise
x2 = c(1:length(first_der))
ggplot() +
  geom_point(data = data.frame(jump.v),aes(x=x2,y=jump.v), color="blue") +
  geom_line(data = data.frame(first_der),aes(x=x2,y=first_der), color = "red")

# Save the plots in your directory
for(d in 1:length(values)){
  file.name = paste("plot",d,".png")
  png(file.name)
  print(plot.list[[d]])
  dev.off()
}

# Graph Cloned Data
ggplot() +
  geom_line(data = data.frame(values),aes(x=x1,y=values), color="blue") +
  geom_point(data = data.frame(clone.data),aes(x=x1,y=clone.data), color = "red")

#ggplot(data.frame(clone.data),aes(x=x1,y=clone.data))+geom_point()

# Run Tests
t.test(jump.v,first_der)
hist(jump.v)
sd(jump.v)
ks.test(jump.v,'pnorm') # Is this normally distributed?
ks.test(jump.v,first_der) # Did they come from the same distribution

jump.error = jump.v - first_der
jump.error
summary(jump.error)


# Error Vector
error = abs(values - clone.data)
summary(error[3:length(error)])
hist(jump.error[3:length(jump.error)])
plot(error[3:length(error)])

# Regression on MC and Data
lm3 = lm(clone.data~values)
summary(lm3)

# Run nonlinear regression
x = values
y = clone.data
df = data.frame(x,y)
fit <- lm(y~poly(x,3),data=df)
summary(fit)

# Hypothesis: Regression Error follows a pattern
lm1 = lm(values~x1)
summary(lm1)
lm2 = lm(clone.data~x1)
summary(lm2)

# Make Vectors from Regression Lines
# Real Values regression vector:
real.regression.vec = c()
for(w in c(1:length(values))){
  new1 = lm1$coefficients[2]*w  + lm1$coefficients[1]
  real.regression.vec = append(real.regression.vec,new1)
  print(new1)
}

clone.regression.vec = c()
for(u in c(1:length(values))){
  new2 = lm2$coefficients[2]*u  + lm2$coefficients[1]
  clone.regression.vec = append(clone.regression.vec,new2)
  print(new2)
}

# Plot the difference in the regression lines
difference = abs(real.regression.vec - clone.regression.vec)
ggplot(data.frame(difference),aes(x=x1,y=difference))+geom_point()


# Extra
error.mc = abs(values-clone.data)
# Perhaps we can find a pattern in the error. If the error continues in a linear
# pattern, we can model/get a distribution of the error and find a "bound"
# for the price.
