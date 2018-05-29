#  Rug 7
## @knitr setup
require(lubridate)
require(xtable)

## @knitr fundef
temp_convert=function(degree,sys) {
  # function to convert between fahrenreit and celsius
  # Arguments:
  #	degree -- numeric value of temperature, 
  #       can be vector but not higher dimension
  #	sys -- system of delivered temperature, 
  #       must be F or C
  # Returns:
  #	a number or vector of the converted temperature, 
  #       with a name(ans) equal to either 
  #       "Fahrenheit" or "Celsius"
  # validate inputs
  if (!(sys=="F" | sys=="C")) stop("sys must be F or C")
  if (!is.numeric(degree)) stop("degree must be numeric")
  if (length(dim(degree))>1) 
    stop("dimension of degree must be no more than vector")
  # do the calculations
  if(sys=="F") {
    ans=(degree-32)/1.8
    names(ans)=rep("Celsius",length(ans))
  }
  if(sys=="C") {
    ans=32+1.8*degree
    names(ans)=rep("Fahrenheit",length(ans))
  }
  return(ans) # return the answer
}

## @knitr exuse
temp_convert(32,"F")
temp_convert(32,"D")
temp_convert(c(-10,0,10,20,30,40),"C")

## @knitr assign
k=3 #put a number in a variable
ages=c(4,6,7) #create a vector of numbers for ages of children
kid_names=c("Bill","Karen","Tom")  #create a vector of names
kids=data.frame(kid_names,ages) #create a data frame of the kids
kids
temperature=c(98.6,100.9,104)
location=c("School","Home Sick","Emergency Room")
kids=cbind(kids,temperature, location) #add temperature and location to the data frame
kids

## @knitr indexing
kid_names[2]  #the second child
kid_names[ages==max(ages)] #the oldest child
kids$ages #the ages of the children
kids[,2] #the ages of the children
kids[3,] #information about the third child

## @knitr indexing2
kids[2,3] #for the second child (row 2), what is the temperature (column 3)
kids[kids$kid_names=="Karen","temperature"] #Karen's temperature
subset(kids,kids$temperature>98.6) #which kids are sick

## @knitr nesting
pointers=c(3,2,4,6)
data=c(12,14,16,18,20,22,24,26)
data[pointers[2]] #value of the data pointed at by the second pointer
data[sum(1,2)]
cumsum(1:4)[3]
cumsum((1:4)[3])

## @knitr pipes1
floor((2+4)*(2+log(sqrt(3*(2+exp(4)))/12)))

## @knitr pipes2
require(magrittr)
exp(4) %>%
  add(2) %>%
  multiply_by(3) %>%
  sqrt() %>%
  divide_by(12) %>%
  log() %>%
  add(2) %>%
  multiply_by(2+4) %>%
  floor()

