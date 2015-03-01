
## Chapter 14: The R System -- Additional Topics 

## Sec 14.1: Graphical User Interfaces to R 
##                    Installing the R Commander  
## install.packages("Rcmdr", dependencies=TRUE) 

## ss 14.1.1: The R Commander's interface -- a guide to getting started 
## library(Rcmdr) 

## ss 14.1.2: The {rattle} GUI 
## library(rattle) 
## rattle() 

## ss 14.1.3: The Creation of Simple GUIs -- the {fgui} Package 
library(fgui) 
library(DAAG); library(lattice) 
## Create function that does the smoothing 
fitsmooth <- 
  function(form=mdbRain^(1/3) ~ Year, df=bomregions, myspan=0.75) 
    print(xyplot(form, data=df, type=c("p","smooth"), span=myspan)) 
## Create function that sets up a GUI interface to fitsmooth() 
callsmooth <- function()gui(func=fitsmooth,  
    argSlider=list(myspan=c(0.1,1.5,0.025)),  # start,stop,stepsize 
    output=NULL, callback=guiExec)   
## callsmooth() 
## Click on and/or move the slider to display the graph. 


## Sec 14.2: Working Directories, Workspaces and the Search List 
## ss 14.2.1: *The search path 
search() 

## ss 14.2.2: Workspace management 
xy <- matrix(rnorm(60000), nrow=600) 
xy.rowrange <- apply(xy, 1, range) 
save(xy, xy.rowrange, file="xy.RData") 
rm(xy, xy.rowrange) 

rm(list = ls(all=TRUE)) 

##          Changing the Working Directory and/or Workspace 
## ss 14.2.3: Utility functions 
# dir()                # List files in the working directory 
# file.choose()        # Choose a file interactively 
# sessionInfo()        # Print version numbers for R and for 
#                      # attached packages 
# system.file()        # Show path, by default to 'package="base"' 
#                      # Try, e.g.: system.file(package="DAAG") 
# R.home()             # Give the path to the R home directory 
# .Library             # Path to the default library. 
# Sys.getenv()         # Show settings of environment variables 
# object.size()        # Show size of R object 

dir() 

Sys.getenv("R_HOME") 
normalizePath(Sys.getenv("R_HOME")) 

system.file("misc/ViewTemps.RData", package="DAAG") 

## Sec 14.3: R System Configuration 
## ss 14.3.1: The R Windows installation directory tree 
## ss 14.3.2: The Library directories 
.libPaths("D:/R-2.8.1/library") 

.First <- function().libPaths("D:/R-2.8.1/library") 

## ss 14.3.3: The startup mechanism 

## Sec 14.4: Data Input and Output 
library(DAAG) 
datafile("oneBadRow") 
datafile("scan-demo") 

## ss 14.4.1: Input of data 
##       Issues for data input with {read.table()} and variants 
## read.table("quote.txt") 
## read.table("quote.txt", quote="'") 

##                   Tracking errors in input data 
nfields <- count.fields("oneBadRow.txt") 
nfields            # Number of fields, for each row 
 
## Now identify rows where the number of fields seems anomalous 
(1:length(nfields))[nfields == 4] 

##      One character string for each input row -- {readLines()} 
readLines("oneBadRow.txt", n=3)    # First 3 lines 
readLines("oneBadRow.txt", n=-1)   # All lines 

##                     Input of fixed format data 
##                  Input of large rectangular files 
scan("oneBadRow.txt", skip = 1, quiet= TRUE) 

data.frame(scan("scan-demo.txt", skip = 1, quiet= TRUE, 
           what=list(ab="", col2=1, col3=1))) 

##            Example -- input of the Boston housing data 
## readLines("bostonc.txt", n=11)[10:11] 

## strsplit(readLines("bostonc.txt", n=11)[10:11], split="\t") 

datafile('bostonc')
boston <- scan("bostonc.txt", n=-1, sep="\t", skip=10, 
               what=c(list(1,""), as.list(rep(1,19)))) 
colnams <- scan("bostonc.txt", skip=9, n=21, what="") 
boston <- data.frame(boston) 
names(boston) <- colnams 

boston <- read.table("bostonc.txt", sep="\t", skip=9,  
                     comment.char="", header=TRUE) 

## ss 14.4.2: Data output 
##                       Output of data frames 
write.table(fossilfuel, file="fuel.txt") 

##               Redirection of screen output to a file 
fossilfuel         # Below, this will be written to the file 
## sink("fuel2.txt") 
## fossilfuel         # NB: No output on screen 
## sink() 

##                   Output to a file using {cat()} 
## ss 14.4.3: Database connections 
library(DAAG); library(DAAG) 
library(RSQLite) 
driveLite <- dbDriver("SQLite") 
con <- dbConnect(driveLite, dbname="hillracesDB") 
dbWriteTable(con, "hills2000", hills2000, overwrite=TRUE) 
dbWriteTable(con, "nihills", nihills, overwrite=TRUE) 
dbListTables(con) 
## Obtain rows 11 to 20 from the newly created nihills table 
dbGetQuery(con, "select * from nihills limit 10 offset 10") 
dbDisconnect(con) 


## Sec 14.5: Functions and operators -- Some Further Details 
##            Issues for the writing and use of functions 
## ss 14.5.1: Function arguments 
##                       The {args()} function 
args(write.table)  # Version 2.10.0 of R 

##                         The {...} argument 
x <- c(1,5,7); z <- c(4,9,10,NA); u <- 1:10 
# Now do calculations that use x, z and u 
rm(x, z, u) 

## ss 14.5.2: Character string and vector functions 
substring("abracadabra",3, 8) # Extract characters 3 to 8 
nchar("abracadabra")          # Count the number of characters 
strsplit("abracadabra", "r")  # Split wherever "r" appears 
strsplit("abcd", split="")    # Split into separate characters 
paste("ab","c","d", sep="")   # Join together 
paste(c("a","b","c"), collapse="")  # Join vector elements 

## ss 14.5.3: Anonymous functions 
ssfun <- function(x)sum(x^2) 
sapply(elastic1, ssfun)   # elastic1 is from the DAAG package 

sapply(elastic1, function(x)sum(x^2)) 

growthfun <- function(x)(x[9] - x[1])/x[1] 
sapply(austpop[, -c(1,10)], growthfun) 

sapply(austpop[, -c(1,10)], function(x)(x[9] - x[1])/x[1]) 

## ss 14.5.4: Functions for working with dates (and times) 
# Electricity Billing Dates 
dd <- as.Date(c("2003-08-24","2003-11-23","2004-02-22","2004-05-03")) 
diff(dd) 

as.Date("1/1/1960", format="%d/%m/%Y") 
as.Date("1:12:1960",format="%d:%m:%Y") 
as.Date("1960-12-1") - as.Date("1960-1-1") 
as.Date("31/12/1960","%d/%m/%Y") 
julian(as.Date("1/1/1970","%d/%m/%Y")) 
julian(as.Date("1/1/2000","%d/%m/%Y")) 

dec1 <- as.Date("2004-12-1") 
format(dec1, format="%b %d %Y") 
format(dec1, format="%a %b %d %Y") 

## Labeling of graph: data frame jobs (DAAG) 
startofmonth <- seq(from=as.Date("1Jan1995", format="%d%b%Y"),  
                    by="1 month", length=24) 
atdates <- seq(from=as.Date("1Jan1995", format="%d%b%Y"),  
               by="6 month", length=4) 
datelabs <- format(atdates, "%b%y") 
xyplot(BC+Alberta ~ startofmonth, data=jobs, outer=TRUE, 
       scales=list(x=list(at=atdates, labels=datelabs)), 
       auto.key=list(columns=2, between=1)) 

dd <- as.Date(c("2003-08-24","2003-11-23","2004-02-22","2004-05-03")) 
weekdays(dd) 
months(dd) 
quarters(dd) 

## ss 14.5.5: Creating Groups 
library(MASS) 
catBP <- cut(Pima.tr2$bp, breaks=4) 
table(catBP, exclude=NULL) 

## ss 14.5.6: Logical operators 
c(TRUE, TRUE, FALSE) & c(FALSE, TRUE, FALSE) 
c(TRUE, TRUE, FALSE) && c(FALSE, TRUE, FALSE) 

##                  A vector form of {if} statement 
##                      Operators are functions 
"+"(2, 3) 


## Sec 14.6: Factors 
fac <- factor(c("c", "b", "a")) 
for (i in fac) print(i) 

##                          Ordered factors 
stress.level <- rep(c("low","medium","high"), 2) 
ordf.stress <- ordered(stress.level, levels=c("low", "medium", 
"high")) 
ordf.stress 
ordf.stress < "medium" 
ordf.stress >= "medium" 

##                          Factor contrasts 
contr.treatment(3) 
contr.sum(3) 
cities <- factor(c("Melbourne","Sydney","Adelaide")) 
contr.sum(cities) 

##      *Tests for main effects in the presence of interactions? 

## Sec 14.7: Missing Values 
x <- c(1, 6, 2, NA) 
is.na(x)           # TRUE for NAs, otherwise FALSE  
x == NA            # All elements are set to NA 
NA == NA 

x <- c(1, NA, 6, 2, 10) 
x > 4 # The second element will be NA 
x[x>4] # NB: This generates a vector of length 3 
## x[x > 4] <- c(101, 102) 

x[!is.na(x) & x > 4] <- c(101, 102) 
x 

##      Counting and identifying { NA}s -- the use of {table()} 
with(nswdemo, table(trt, re74>0, useNA="ifany")) 

fac_re74 <- with(nswdemo, factor(re74>0, exclude=NULL)) 
levels(fac_re74) 
levels(fac_re74) <- c("0", "gt0", "<NA>") 
with(nswdemo, table(trt, fac_re74)) 

##                         The removal of NAs 
##                    {NA}s in modeling functions 
options()$na.action # Version 2.10.0, following startup 

##             Sorting and ordering, where there are NAs 
x <- c(1, 20,  2, NA, 22) 
order(x)           # By default, na.last=TRUE 
x[order(x)] 
sort(x)            # By default na.last=NA 
sort(x, na.last=TRUE) 


## Sec 14.8: * Matrices and Arrays 
xx <- matrix(1:6, ncol=3)  # Equivalently, enter 
xx 

dim(xx) 

## Use as.vector() 
x <- as.vector(xx) 
## Alternatively, directly remove the dimension attribute 
x <- xx 
dim(x) <- NULL 

##                   The extraction of submatrices 
x34 <- matrix(1:12, ncol=4) 
x34 

x34[2:3, c(1,4)]     # Extract rows 2 & 3 and columns 1 & 4 
x34[-2, ]            # Extract all rows except the second 
x34[-2, -3]          # Omit row 2 and column 3 

x34[2, ]                # The dimension attribute is dropped 
x34[2, , drop=FALSE]    # Retain the dimension attribute 

##         Conversion of data frames and tables into matrices 
## ss 14.8.1: Matrix arithmetic 
## Set up example matrices G, H and B 
G <- matrix(1:12, nrow=4); H <- matrix(112:101, nrow=4); B <- 1:3 
G + H             # Element-wise addition (X & Y both n by m) 
G * H             # Element-wise multiplication 
G %*% B           # Matrix multiplication (X is n by k; B is k by m) 
## Set up example matrices X (square, full rank) and Y 
X <- matrix(c(1,1,1, -1,0,1, 2,4,2), nrow=3) 
Y <- matrix(1:6, nrow=3) 
solve(X, Y)       # Solve XB = Y (X must be square) 

##                      Computational Efficiency 
xy <- matrix(rnorm(1500000),ncol=50) 
dim(xy) 
system.time(xy+1)  # user and system are processor times 
xy.df <- data.frame(xy) 
system.time(xy.df+1) 

## ss 14.8.2: Outer products 
outer(1:4, 1:10) 

rbgshades <- outer(c("red","blue","green"), c("",paste(1:4)), 
                   function(x,y)paste(x,y, sep="")) 
rbgshades          # Display the matrix 
plot(rep(0:4, rep(3,5)), rep(1:3, 5), col=rbgshades, pch=15, cex=8) 

## ss 14.8.3: Arrays 
x <- 1:24 
dim(x) <- c(2, 12); print(x) 
 
dim(x) <- c(3, 4, 2); print(x) 


## Sec 14.9: Manipulations with Lists, Data Frames, Matrices and Time Series 
## ss 14.9.1: Lists -- an extension of the notion of ``vector'' 
zz <- list("Shireen", "Peter", c("Luke","Amelia","Ted"), c(8,4,0)) 
zz[c(3,1)] 
 zz[c(3,1)] 
## Return list whose only element is the vector c("Luke","Amelia") 
zz[3] 

## Return the vector c("Luke","Amelia","Ted") 
zz[[3]]  

duo <- list(family="Braun", names=c("Matthew","Phillip"),  
            ages=c(14,9)) 
duo[["names"]]      # Alternatively, specify duo$names 

##                  The dual identity of data frames 
xyz <- data.frame(x=1:4, y=11:14, z=I(letters[1:4])) 

xyz[1, ]            # Returns a data frame, i.e., a list) 
xyz[1, , drop=TRUE] 
unlist(xyz[1, ])    # Returns, here, a vector of atomic mode 
unlist(xyz[1, , drop=TRUE]) 

## ss 14.9.2: Changing the shape of data frames (or matrices) 
##                        Melting and casting 
library(reshape) 
Jobs <- melt(jobs, measure.vars=names(jobs)[1:6],  
             variable_name="Region", id.vars="Date") 
head(Jobs, 3) 

jobsBack <- cast(Jobs, Date ~ Region) 
head(jobsBack,3) 

## ss 14.9.3: * Merging data frames -- {merge()} 
new.Cars93 <- merge(x=Cars93, y=Cars93.summary[, "abbrev", drop=F], 
                    by.x="Type", by.y="row.names") 

## ss 14.9.4: Joining data frames, matrices and vectors -- {cbind()} 
##          Conversion of tables and arrays into data frames 
## ss 14.9.5: The {apply} family of functions 
##                      The {tapply()} function 
## Compare tapply() with aggregate(): data frame kiwishade (DAAG) 
with(kiwishade, tapply(yield, INDEX=list(block, shade), FUN=mean)) 
with(kiwishade, aggregate(yield, by=list(block, shade), FUN=mean)) 

##                       The {apply()} function 
##              The functions {lapply()} and {sapply()} 
## Uses data frame rainforest (DAAG) 
sapply(rainforest[, -7], range, na.rm=TRUE) 

## ss 14.9.6: Splitting vectors and data frames into lists -- {split()} 
with(Cars93.summary, split(No.of.cars, Max.passengers))  

## Split dataframe by Max.passengers (2nd column) 
split(Cars93.summary[, -2], Cars93.summary[, 2])  

## ss 14.9.7: Multivariate time series 
jobts <- ts(jobs[,1:6], start=1995, frequency=12) 
colnames(jobts) 

## Subseries through to the third month of 1995 
window(jobts, end=1995+2/12) 
 # Rows are 1995+0/12, 1990+1/12, 1990+2/12 

plot(jobts, plot.type="single")     # Use one panel for all 
plot(jobts, plot.type="multiple")   # Separate panels. 

## Alternative code for Figure 2.9A; plot as time series 
plot(jobts, plot.type="single", xlim=c(1995,1997.2), lty=1:6, log="y", 
     xaxt="n", xlab="", ylab="Number of Jobs") 
## Move label positions so that labels do not overlap 
ylast <- bounce(window(jobts, 1996+11/12), d=strheight("O"), log=TRUE) 
 # bounce() is from DAAG 
text(rep(1996+11/12,6), ylast, colnames(ylast), pos=4) 
datlab <- format(seq(from=as.Date("1Jan1995", format="%d%b%Y"), by="3 month",  
                 length=8), "%b%Y") 
axis(1, at=seq(from=1995, by=0.25, length=8), datlab)  


## Sec 14.10: Classes and Methods 
##                       S3 methods and classes 
print 

## ss 14.10.1: Printing and summarizing model objects 
elastic.lm <- lm(distance ~ stretch, data=elasticband) 
elastic.sum <- summary(elastic.lm) 

## ss 14.10.2: Extracting information from model objects 
names(elastic.lm) 

elastic.lm$coefficients 
elastic.lm[["coefficients"]] 
elastic.lm[[1]] 

coef(elastic.lm) 

x <- 1:5 
class(x) <- "lm"   # Inappropriate assignment of class 
## print(x) 

## ss 14.10.3: S4 classes and methods 
library(lme4) 
hp.lmList <- lmList(o2 ~ wattsPerKg | id, data=humanpower1) 
slotNames(hp.lmList) 
slot(hp.lmList, "call") 
hp.lmList@call 


## Sec 14.11: Manipulation of Language Constructs 
## ss 14.11.1: Model and graphics formulae 
plot.mtcars <- function(xvar="disp", yvar="hp"){ 
   mt.form <- paste(yvar, "~", xvar) 
   plot(formula(mt.form), data=mtcars) 
   } 
## Plot using data frame mtcars (datasets) 
names(mtcars) 
plot.mtcars() 
plot.mtcars(xvar="disp", yvar="mpg") 

##         Extraction of Variable Names from Formula Objects 
all.vars(mpg ~ disp) 

plot.mtcars <- function(form = mpg~disp){ 
   ## Include information that allows a meaningful label 
   mtcars.info <- c(mpg= "Miles/(US) gallon",  
                    cyl= "Number of cylinders",  
                    disp= "Displacement (cu.in.)",  
                    hp= "Gross horsepower",  
                    drat= "Rear axle ratio",  
                    wt= "Weight (lb/1000)",  
                    qsec= "1/4 mile time",  
                    vs= "V/S",  
                    am= "Transmission (0 = automatic, 1 = manual)",  
                    gear= "Number of forward gears",  
                    carb= "Number of carburettors") 
   xlab <- mtcars.info[all.vars(form)[1]] 
   ylab <- mtcars.info[all.vars(form)[2]] 
   plot(form, xlab=xlab, ylab=ylab, data=mtcars) 
} 

## ss 14.11.2: The use of a list to pass arguments 
mean(rnorm(20)) 
do.call("mean", args=list(x=rnorm(20))) 

simulate.distribution <- 
  function(distn="weibull", params=list(n=10)){  
    ## Simulates one of: weibull, gaussian, logistic 
    if(distn=="weibull" & !("shape" %in% names(params))) 
      params <- c(shape=1, params) 
    ## weibull requires a default shape argument.  
    ## =====================================================  
    ## Choose the function that will generate the random sample  
    rfun <- switch(distn,   
                   weibull = rweibull,  
                   normal = rnorm,  
                   logistic= rlogis)  
    ## Call rfun(). Use of do.call() makes it possible to give 
    ## the argument list as a list of named values.  
    ## Pass shape argument (or NULL), plus n = # of numbers 
    do.call("rfun", args=params)  
  } 

simulate.distribution() 
plot(density(simulate.distribution("normal", params=list(n=100)))) 
plot(density(simulate.distribution("weibull",  
             params=list(n=100, scale=0.5)))) 

mean.call <- call("mean", x=rnorm(5)) 
eval(mean.call) 
eval(mean.call) 

mean.call 

## ss 14.11.3: Expressions 
local(a+b*x+c*x^2, envir=list(x=1:4, a=3, b=5, c=1)) 

## ss 14.11.4: Environments 
test <- function()as.character(sys.call())  

test() 
newtest <- test    # Create a copy with a different name 
newtest() 

##       Automatic naming of a file that holds function output 
hcopy <- 
    function(width=2.25, height=2.25, pointsize=8){ 
        funtxt <- sys.call(1) 
        fnam <- paste(funtxt, ".pdf", sep="") 
        print(paste("Output is to the file '", fnam, "'", sep="")) 
        pdf(file=fnam, width=width, height=height, pointsize= 
            pointsize) 
    } 

fig1 <- function(){ 
    hcopy()             # Call with default arguments 
    curve(sin, -pi, 2*pi) 
    dev.off() 
} 

## ss 14.11.5: Function environments, and lazy evaluation 
##                          Lazy evaluation 
lazyfoo <- function(x=4, y = x^2)y 
lazyfoo() 
lazyfoo(x=3) 

# lazyfoo(y=x^3)     # We specify a value for y in the function call 
# x <- 9             # Now, in the parent environment, x=9 
# lazyfoo(y=x^3) 

##  Example -- a function that identifies objects added during asession 
dsetnames <- objects() 

additions <- function(objnames = dsetnames){ 
    newnames <- objects(envir=.GlobalEnv) 
    existing <- newnames %in% objnames 
    newnames[!existing] 
    } 

additions(dsetnames) 


## Sec 14.12: *Creation of R Packages 
system.file("misc/ViewTemps.RData", package="DAAG") 

## package.skeleton(name = "ViewTemps") 

##                             Namespaces 

## Sec 14.13: Document Preparation --- {Sweave()} and {xtable()} 
## Sweave("sec1-1.Rnw", keep.source=TRUE)    
  # actually, Sweave("sec1-1") is sufficient 
  # The argument keep.source=TRUE preserves the code layout 
  # and ensures that comments are retained. 

R.home(component="share/texmf/Sweave.sty") 


## Sec 14.14: Further Reading 
citation() 
citation("DAAG") 

##                             Vignettes 
vignette()           # All vignettes in all installed packages 

## vignette(package="grid") 

## vignette("viewports")    # Equivalent to vignette(topic="viewports") 

## Use of split() and sapply(): data frame science (DAAG) 
with(science, sapply(split(school, PrivPub), 
                     function(x)length(unique(x)))) 

### Thanks to Markus Hegland (ANU), who wrote the initial version 
##1 Generate the data 
  cat("generate data \n") 
  n <- 800           # length of noise vector 
  m <- 100           # length of signal vector 
  xsignal <- runif(m) 
  sig <- 0.01 
  enoise <- rnorm(m)*sig 
  ysignal <- xsignal**2+enoise 
  maxys <- max(ysignal) 
  minys <- min(ysignal) 
  x <- c(runif(n), xsignal) 
  y <- c(runif(n)*(maxys-minys)+minys, ysignal) 
  # random permutation of the data vectors 
  iperm <- sample(seq(x)) 
  x <- x[iperm] 
  y <- y[iperm] 
  # normalize the data, i.e., scale x & y values to 
  # lie between 0 & 1 
  xn <- (x - min(x))/(max(x) - min(x)) 
  yn <- (y - min(y))/(max(y) - min(y)) 
##1 End 
 
##2 determine number of neighbors within 
# a distance  <= h = 1/sqrt(length(xn)) 
  nx <- length(xn) 
  # determine distance matrix 
  d <- sqrt( (matrix(xn, nx, nx) - t(matrix(xn, nx, nx)) )**2 + 
             (matrix(yn, nx, nx) - t(matrix(yn, nx, nx)) )**2 ) 
  h <- 1/sqrt(nx) 
  nnear <- apply(d <= h, 1, sum) 
##2 End 
 
##3 Plot data, with reconstructed signal overlaid. 
  cat("produce plots \n") 
  # identify the points which have many such neighbors 
  ns <- 8 
  plot(x,y) 
  points(x[nnear > ns], y[nnear > ns], col="red", pch=16) 
##3 End 

n <- 10000; system.time(sd(rnorm(n))) 

library(plyr) 
aaply(.data=UCBAdmissions, .margins=1:2, .fun=sum) 
adply(.data=UCBAdmissions, .margins=1:3, .fun=identity) 
library(DAAG) 
daply(.data=tinting, .variables=c("sex","agegp"), .fun=length) 

##                             References 
##                   References for further reading 
##                             References 
##                   References for further reading 
