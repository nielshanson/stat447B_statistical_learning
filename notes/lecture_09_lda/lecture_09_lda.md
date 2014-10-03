# Lecture 9: Linear Discriminant Analysis (LDA)
Niels W. Hanson  

## Normal Model for $X|Y = c$

* models the conditional distribution of $X | Y$ instread of $Y | X$
* For example, we assume that
$$
X | Y c_j \sim MN( \mu_j, \sigma),
$$
where $MN$ is the multivariate normal with dimention of variables in $X$

## Fisher's LDA for NORMAL populations

* discision based on the posterior probability log odds
* it can be shown that this boundary is *linear*
* we can also existmate this boundary in *closed form*

### E.g. Vaso Constriction Data: LDA Fit

#### Q: Why are contours of constant $\hat{p}(x)$ *linear* in *volume* and *rate*?

* Because it is called *linear* discriminant analysis
* Because of the properties of the ratio of the multivariable normal distribution. The ratio will become a constant.

TODO: Calculate an example here.

### E.g. Vaso Constriction Data: LDA Fit

### Admissions Data: Three classes -- Admit, Borderline, Do Not Admit

* Based on the entire application these were hand-labeled, and someone wanted to automate the process
* see `T11-6.dat`
* Three classes and three multivariate normal distributions conditional on the class
* prior probabilities $p_j$


### LCA: Disadvantage *Normality*

* assume that each of th MVN distribution **have the same shape**, which from inspection may not be reasonable (very difficult to check in high dimention)
* variance-covariance matrix is **much more sensitive to outliers** than means, particularly in the low data situation
* outliers, because all densities ascribe near-zero answers, the ratio of these densities could do anything (strong responce even though very little data)

### Logistic Classification

* when we have $K>2$ classes we propose
  * for $k$
* multinomial maximum liklihood



```r
# TODO: Get working!
# library(nnet)
# admiss.logit <- multinom(y ~ GPA + GMAT, data=admiss, maxit=10000)
```

* turns out that logisitc regression is a special case of neral network
* made `y` an arbitrary factor variable
* fitting values is $\frac{\mbox{deviance}}{2}$


```r
# summary(admiss.logit)
```

* `predict` function does not always do the same thing... is it a predition or liklihoods or probabilitiy

#### Q. Which do you like the best? LDA? Logistic?

* boundaries look for
* Logistic regression because it does not use a normality assumption. 
* But, LCA is provably optimal if the data actually come from MVN distributions

## Nearest Neighbors

* if we knoew class probabilities $Pr(Y=y|x)$ the *optimal classification rule* is

* using very global models so far, as in they apply to all $X$
* perhaps we want to look *locally* for each $x$, let
    *$\hat{Pr}(Y=y)$
    
### k-nearest neightbours

#### Algorithm

* No parametric model for $Pr(Y|x)$, which makes this **extremely flexable**
* No lineararity

#### E.g., Admissions data in 1-NN

* probably optimal for large dataset even in 1-NN case
