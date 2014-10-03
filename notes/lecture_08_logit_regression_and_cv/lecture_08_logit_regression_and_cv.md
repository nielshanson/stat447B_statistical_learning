# STAT545A: Lecture 8 - Logit Regression and CV
Niels W. Hanson  

## Example: Digit Recognition

* notice that all the variables are significant
    * some of the vairalbe are co-linear
    * we can get away with a smaller model
    * some variables when removed will cause others to become significant

## Notes on Classification

* linear predictor, e.g., **logistic regression**
* predict based on a threshold
    * 1 if $\hat{p}(x) \gt 0.5$
    * 0 otherwise
* Is this optimal?
* Under Missclassification Error?
* Under the Expected Loss function
    * Define a function $Expected Loss$ $L(Y(x), \hat{y}(x))$

### Problems

* assumes that all types of errors have the same magnituide of loss? This is not the case in many practical cases?
    *E.g., Credit card company offers. Loss is only $1 if you don't take of up the offer.
* assumes that the model that is generating $\hat{p}$ is stable over time

## Flexabile Logistic Regression

* More flexible models: splines, penalized slplines, etc.
    * E.g. Generalized Additive Models (GAM)

* Uses the same folds when comparing models is to avoid an extra random error
   * Form of **blocking**, a difficult set of folds is difficult to model for both
   
## Linear Discriminant Analysis (LDA)

* Model distributions of the X conditional on the Class
* Multivariate Normal Model for $X|y = c$


* Plotting $\hat{p}(x)$
