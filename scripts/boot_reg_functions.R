# pgrs logistic regression function
# Input:
#   n.bootstrap     [integer] how many time should the analysis be bootstrapped
#   complete.data   [data.frame] data to be with dependent variable, covariates and score
#   dep.var         [character] column name of dependent variable
#   covars          [character] character vector with all covariates
#   my.range.list

pgrs.linreg.simple.nagelkerke <- function(mydata, index) {
    
    cov.formula <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"))
    cov.model <- lm(as.formula(cov.formula), data = mydata, subset = index)
    #cov.r2 <- nagelkerke(cov.model)$Pseudo.R.squared.for.model.vs.null[3]
    
    formula.1 <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"), "+", ind.var, sep = "")
    model.1 <- glm(as.formula(formula.1), data = mydata, subset = index)
    #r2 <- nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null[3]
    
    assign("mydata", mydata, .GlobalEnv)
    
    c(R2.diff = nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null[3] - nagelkerke(cov.model)$Pseudo.R.squared.for.model.vs.null[3], 
      P = coef(summary(model.1))[nrow(coef(summary(model.1))), ncol(coef(summary(model.1)))])
    
}

pgrs.linreg.simple <- function(mydata, index) {
    
    cov.formula <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"))
    cov.model <- lm(as.formula(cov.formula), data = mydata, subset = index)
    cov.r2 <- summary(cov.model)$adj.r.squared
    
    formula.1 <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"), "+", ind.var, sep = "")
    model.1 <- lm(as.formula(formula.1), data = mydata, subset = index)
    full.r2 <- summary(model.1)$adj.r.squared
    
    assign("mydata", mydata, .GlobalEnv)
    
    c(R2.diff = full.r2 - cov.r2, 
      P = coef(summary(model.1))[nrow(coef(summary(model.1))), ncol(coef(summary(model.1)))])
    
}

pgrs.linreg.simple.int <- function(mydata, index) {
  
  cov.formula <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"))
  cov.model <- lm(as.formula(cov.formula), data = mydata, subset = index)
  #cov.r2 <- nagelkerke(cov.model)$Pseudo.R.squared.for.model.vs.null[3]
  
  formula.1 <- paste(cov.formula, "+", int.var, "*", ind.var, sep = "")
  model.1 <- glm(as.formula(formula.1), data = mydata, subset = index)
  #r2 <- nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null[3]
  
  assign("mydata", mydata, .GlobalEnv)
  
  c(R2.diff = nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null[3] - nagelkerke(cov.model)$Pseudo.R.squared.for.model.vs.null[3], 
    P = coef(summary(model.1))[nrow(coef(summary(model.1))), ncol(coef(summary(model.1)))])
  
}

pgrs.logreg.simple <- function(mydata, index) {
    
    cov.formula <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"))
    cov.model <- glm(as.formula(cov.formula), data = mydata, family = "binomial", subset = index)
    #cov.r2 <- nagelkerke(cov.model)$Pseudo.R.squared.for.model.vs.null[3]
    
    formula.1 <- paste(dep.var, "~", paste(pcs, collapse = "+"), "+", paste(covars, collapse = "+"), "+", ind.var, sep = "")
    model.1 <- glm(as.formula(formula.1), data = mydata, family = "binomial", subset = index)
    #full.r2 <- nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null[3]
    
    assign("mydata", mydata, .GlobalEnv)
    
    c(R2.diff = nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null[3] - nagelkerke(cov.model)$Pseudo.R.squared.for.model.vs.null[3], 
      P = coef(summary(model.1))[nrow(coef(summary(model.1))), ncol(coef(summary(model.1)))])
    
}