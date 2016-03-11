# CompuStat holds the materials from my class of **Computacional Statistics part of the _ITAM_** Master of Data Science

##The class was held from August 2015 to December 2015

###Some of the subject covered so far are
1. Simulations
  * Random number generation
  * Monte Carlo Simulations
  

You will find here some examples in **R** to generate random numbers and other simulations. In the subfolders are some of the methods using *Shiny* applications.

At the same time I am learning
* R
* Shiny
* Bayesian Statistics
* R markdown
* Git and Github

I recently watched a TED talk about how you can learn any skill in 20 hours, it said:
> All you need is *20* hours of practice


Part of the work in the class included relearning many things forgotten:

Like `Recursion`

Here is some code:

```
fibo_recursivo <- function(n){
  if(n<2) return(n)
  return(fibo_recursivo(n-1)+fibo_recursivo(n-2))
}

fibo_recursivo(6)
```

[Visit my shiny apps site] (https://stukeil.shinyapps.io/FuncionInversa)


File Names | Description
------------ | -------------
FuncionInvers | I use the inverse function method to generate pseudo-random numbers from an exponential distribution
BoXMuller | I use the BoxMuller method to generate pseudo random numbers from a Normal distribution by using two uniform distributions
Beta | Just a shniy app to better understand the Beta Function very much used in Bayesian Statistics because of it´s conjugation properties

~~In case you haven't noticed I am just learning the basics of markdown~~
:poop:


Kind of a rothko ![A link to my first pi approximation](https://github.com/stu-keil/CompuStat/blob/master/Rplot.png)
