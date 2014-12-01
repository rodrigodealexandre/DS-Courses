---
title       : Different Sepal sizes
subtitle    : Species have different sepal sizes
author      : Rodrigo Bertollo de Alexandre
job         : Learner
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Course Project: Shiny Application and Reproducible Pitch

Different Species have different sepal sizes, and they seem to have a linear pattern between them.
if you load the iris dataset


```r
data(iris)
irisdataset <- iris
```

---
## Plot 1
### A plot of the Specie Virginica

```r
plant <- "virginica"
data <- subset(irisdataset, iris$Species == plant)   
 
plot(data$Sepal.Length, data$Sepal.Width, main = "A plot of the Sepal size", xlab = "Sepal Length", ylab = "Sepal Width",  xaxt="n")
```

![plot of chunk unnamed-chunk-2](assets/fig/unnamed-chunk-2.png) 

---
## Plot 2
### A plot of the Specie Versicolor

```r
plant <- "versicolor"
data <- subset(irisdataset, iris$Species == plant)   

plot(data$Sepal.Length, data$Sepal.Width, main = "A plot of the Sepal size", xlab = "Sepal Length", ylab = "Sepal Width",  xaxt="n")
```

![plot of chunk unnamed-chunk-3](assets/fig/unnamed-chunk-3.png) 


---
## Plot 3
### A plot of the Specie Setosa

```r
plant <- "setosa"
data <- subset(irisdataset, iris$Species == plant)   

plot(data$Sepal.Length, data$Sepal.Width, main = "A plot of the Sepal size", xlab = "Sepal Length", ylab = "Sepal Width",  xaxt="n")
```

![plot of chunk unnamed-chunk-4](assets/fig/unnamed-chunk-4.png) 



