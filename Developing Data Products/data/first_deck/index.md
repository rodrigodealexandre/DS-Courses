---
title       : Test title
subtitle    : subtest title
author      : me
job         : uneployed
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Read-And-Delete

1. Edit YAML front matter
2. Write using R Markdown
3. Use an empty line followed by three dashes to separate slides!

--- .class #id 

## Slide 2

```{r eval = FALSE
}
y <- rnorm(1:10, 2, sd=1)
x1 <- rnorm(1:10, 2, sd=1)
x2 <- rnorm(1:10, 2, sd=1)
x3 <- rnorm(1:10, 2, sd=1)

fit <- lm(y ~ x1 + x2 + x3)
summary(fit)

```

--- .class #id 

$x^2$
$$\frac{-b \pm \sqrt{b^2 - 4 a c}}{2a}$$


