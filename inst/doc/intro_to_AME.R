## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(24)

library(FLAME)

n <- 500 # Observations 
p <- 5 # Covariates

# Data to match
data <- gen_data(n, p) 

# Data to compute PE and determine which covariate sets to match on
holdout <- gen_data(n, p) 

## -----------------------------------------------------------------------------
head(data[, 1:p])

## -----------------------------------------------------------------------------
names(data)

## -----------------------------------------------------------------------------
FLAME_out <- FLAME(data = data, holdout = holdout)

## -----------------------------------------------------------------------------
names(FLAME_out)

## -----------------------------------------------------------------------------
matched_data <- FLAME_out$data[FLAME_out$data$matched, ]

## -----------------------------------------------------------------------------
FLAME_out$MGs[[1]]

## -----------------------------------------------------------------------------
FLAME_out$cov_sets

## -----------------------------------------------------------------------------
print(FLAME_out)

## -----------------------------------------------------------------------------
(FLAME_summ <- summary(FLAME_out))

## ---- fig.dim=c(8,6)----------------------------------------------------------
plot(FLAME_out, which_plots = 1)

## ---- fig.dim=c(8,6)----------------------------------------------------------
plot(FLAME_out, which_plots = 2)

## ---- fig.dim=c(8,6)----------------------------------------------------------
plot(FLAME_out, which_plots = 3)

## ---- fig.dim=c(8,6)----------------------------------------------------------
plot(FLAME_out, which_plots = 4)

## -----------------------------------------------------------------------------
high_quality <- FLAME_summ$MG$highest_quality
MG(high_quality, FLAME_out)
CATE(high_quality, FLAME_out)

## -----------------------------------------------------------------------------
FLAME_wo_replace <- FLAME_out # From previous run with replace = FALSE, as per the default
FLAME_w_replace <- FLAME(data, holdout, replace = TRUE)

## -----------------------------------------------------------------------------
print(FLAME_wo_replace$MGs[[32]])
print(FLAME_w_replace$MGs[[32]])

## -----------------------------------------------------------------------------
print(MG(32, FLAME_wo_replace)) # run with multiple = FALSE bc no replacement
print(MG(32, FLAME_w_replace, multiple = TRUE))

## -----------------------------------------------------------------------------
(unit32_MGs <- MG(32, FLAME_w_replace, multiple = TRUE, id_only = TRUE)[[1]])
(only_in_secondary <- setdiff(unit32_MGs[[2]], unit32_MGs[[3]]))

## -----------------------------------------------------------------------------
MG(only_in_secondary, FLAME_wo_replace)

## -----------------------------------------------------------------------------
FLAME_w_replace$data$MG

## -----------------------------------------------------------------------------
# early_stop arguments for illustration only

(FLAME_C_small <- 
   FLAME(data, holdout, C = 0.01, 
         early_stop_iterations = 4, early_stop_epsilon = Inf, verbose = 0))

(FLAME_C_big <- 
    FLAME(data, holdout, C = 10000, 
          early_stop_iterations = 4, early_stop_epsilon = Inf, verbose = 0))

## -----------------------------------------------------------------------------
my_PE_linear <- function(X, Y) {
  fit <- lm(Y ~ ., data = as.data.frame(cbind(X, Y = Y)))
  return(fit$fitted.values)
}
FLAME_out <- FLAME(data = data, holdout = holdout, PE_method = my_PE_linear)

## -----------------------------------------------------------------------------
my_PE_lasso <- function(X, Y) {
  df <- as.data.frame(cbind(X, Y = Y))
  X <- model.matrix(Y ~ ., data = df)
  fit <- glmnet::cv.glmnet(X, Y, alpha = 1, nfolds = 5)
  return(predict(fit, X))
}

FLAME_out <- FLAME(data = data, holdout = holdout, PE_method = my_PE_lasso)

## -----------------------------------------------------------------------------
# Recode the outcome to numeric 0 1
data$outcome <- ifelse(data$outcome > median(data$outcome), 1, 0)
holdout$outcome <- ifelse(holdout$outcome > median(holdout$outcome), 1, 0)

my_PE_lr <- function(X, Y) {
  df <- as.data.frame(cbind(X, Y = Y))
  X <- model.matrix(Y ~ ., data = df)
  fit <- glmnet::cv.glmnet(X, Y, alpha = 1, nfolds = 5, family = 'binomial')
  return(predict(fit, X, type = 'class'))
}

FLAME_out <- FLAME(data = data, holdout = holdout, PE_method = my_PE_lr)

## -----------------------------------------------------------------------------
(FLAME_exact <- FLAME(data, holdout, early_stop_iterations = 1))
FLAME_exact$cov_sets # No covariate sets are dropped

## -----------------------------------------------------------------------------
covs <- as.matrix(data[, 1:p])
inds <- sample(1:(n * p), size = round(0.05 * n * p), replace = FALSE) 
covs[inds] <- NA
data[, 1:p] <- covs

## -----------------------------------------------------------------------------
FLAME_drop <- FLAME(data, holdout, missing_data = 'drop')
units_with_missingness <- which(apply(covs, 1, function(x) any(is.na(x))))
all(!FLAME_drop$data$matched[units_with_missingness])

## -----------------------------------------------------------------------------
(57 %in% units_with_missingness) # an example unit 
MG(57, FLAME_out)

## -----------------------------------------------------------------------------
DAME_keep <- DAME(data, holdout, missing_data = 'keep', verbose = 0)
print(data[DAME_keep$MGs[[57]], ])
print(MG(57, DAME_keep))

## -----------------------------------------------------------------------------
DAME_impute <- DAME(data, holdout, missing_data = 'impute', verbose = 0)
print(DAME_impute$data[57, ])
print(MG(57, DAME_impute))

