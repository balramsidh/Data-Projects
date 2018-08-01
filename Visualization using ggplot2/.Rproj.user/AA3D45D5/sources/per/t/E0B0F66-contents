# Data visualization using GGPLOT 2- part 2
# https://www.datacamp.com/courses/data-visualization-with-ggplot2-2

packages = c("ggplot2","tidyverse","RColorBrewer","Hmisc")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# ggplot2 is already loaded

# Explore the mtcars data frame with str()
str(mtcars)

# A scatter plot with LOESS smooth:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()+
  geom_smooth()


# A scatter plot with an ordinary Least Squares linear model:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()+
  geom_smooth(method="lm")


# The previous plot, without CI ribbon:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()+
  geom_smooth(method="lm",se=FALSE)



# The previous plot, without points:
ggplot(mtcars, aes(x = wt, y = mpg))+
  geom_smooth(method="lm",se=FALSE)


##### grouping : stat_smooth itself groups data based on the color coding, you can add a smooth line
#by specifying group=1 inside aes() of stat_smooth

# Define cyl as a factor variable
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

# Complete the following ggplot command as instructed
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  stat_smooth(method="lm",se=F,aes(group=1))

## modifying stat smooth

# Plot 1: change the LOESS span
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  # Add span below
  geom_smooth(se = F,span=0.7) # span defines window for which loess divides the data

# Plot 2: Set the overall model to LOESS and use a span of 0.7
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  # Change method and add span below
  stat_smooth( aes(group = 1),span=0.7,
               se = F, col = "black")

# the problem with above graph is that the overall smooth line is not coming in the legend, for this
# we need to add col in aes() inside stat_smooth
# Plot 3: Set col to "All", inside the aes layer of stat_smooth()
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  stat_smooth(method = "loess",
              # Add col inside aes()
              aes(group = 1,col = "ALL"),
              # Remove the col argument below
              se = F, span = 0.7)

#now the color of the overall line is not black anymore, using scale_Color_manual() for that. 
# Plot 4: Add scale_color_manual to change the colors
myColors <- c(brewer.pal(3, "Dark2"), "black")
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F, span = 0.75) +
  stat_smooth(method = "loess",
              aes(group = 1, col="All"),
              se = F, span = 0.7) +
  scale_color_manual("Cylinders",values=myColors) #legend title is cylinders

# Plot 1: Jittered scatter plot, add a linear model (lm) smooth:
ggplot(Vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(alpha = 0.2) +
  stat_smooth(method="lm",se=F)


# Plot 2: Only lm, colored by year
ggplot(Vocab, aes(x = education, y = vocabulary,col=factor(year))) +
  stat_smooth(method="lm",se=F)

# the above graph has too many years
#When mapping onto color you can sometimes treat a continuous scale, like year, 
#as an ordinal variable, but only if it is a regular series. 
#The better alternative is to leave it as a continuous variable and 
#use the group aesthetic as a factor to make sure your plot is drawn correctly.


# Plot 3: Set a color brewer palette
ggplot(Vocab, aes(x = education, y = vocabulary,col=factor(year))) +
  stat_smooth(method="lm",se=F) +
  scale_color_brewer()


# Plot 4: Add the group, specify alpha and size
ggplot(Vocab, aes(x = education, y = vocabulary,col=year,group=factor(year))) +
  stat_smooth(method = "lm", se = F,alpha=0.6,size=2) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))


# stat_sum

# Plot with linear and loess model
p <- ggplot(Vocab, aes(x = education, y = vocabulary)) +
  stat_smooth(method = "loess", aes(col = "x"), se = F) +
  stat_smooth(method = "lm", aes(col = "y"), se = F) +
  scale_color_discrete("Model", labels = c("x" = "LOESS", "y" = "lm"))

# Add stat_sum it counts the no of observations at each location on the plot 
p + stat_sum()

# Add stat_sum and set size range
p + stat_sum() + scale_size(range=c(1,10))


### stats outside geoms 

ggplot(iris, aes(y=Sepal.Length,x=Species)) +
  stat_summary(fun.y = mean,geom="point") +
  stat_summary(fun.data = mean_sdl,fun.args = list(mult=1),
               geom="errorbar",width=0.1)

# mammals dataset
library("MASS")

mam.new <- data.frame(body=log10(mammals$body))

ggplot(mam.new,aes(x=body)) +
  geom_histogram(aes(y=..density..)) +
  geom_rug()  +
  stat_function(fun = dnorm,color="red",
               arg = list(mean=mean(mam.new$body),
                        sd= sd(mam.new$body)))
#Exercise
# Display structure of mtcars
str(mtcars)

# Convert cyl and am to factors:
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Define positions:
posn.d <- position_dodge(width=0.1)
posn.jd <- position_jitterdodge(jitter.width=0.1,dodge.width=0.2)
posn.j <- position_jitter(width=0.2)

# base layers:
wt.cyl.am <- ggplot(mtcars,aes(x=cyl,y=wt,col=am,fill=am,group=am))


# wt.cyl.am, posn.d, posn.jd and posn.j are available

# Plot 1: Jittered, dodged scatter plot with transparent points
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)

# Plot 2: Mean and SD - the easy way
wt.cyl.am +
  stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),position=posn.d)


# Plot 3: Mean and 95% CI - the easy way
wt.cyl.am +
  stat_summary(fun.data=mean_cl_normal,position=posn.d)


# Plot 4: Mean and SD - with T-tipped error bars - fill in ___
wt.cyl.am +
  stat_summary(geom = "point", fun.y =mean,
               position = posn.d) +
  stat_summary(geom = "errorbar", fun.data =mean_sdl,
               position = posn.d, fun.args = list(mult = 1), width = 0.1)


?head
     