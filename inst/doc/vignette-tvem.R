## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("dziakj1/tvem")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("tvem")

## -----------------------------------------------------------------------------
library(tvem)

## -----------------------------------------------------------------------------
set.seed(123)
the_data <- simulate_tvem_example()

## -----------------------------------------------------------------------------
print(head(the_data))
print(summary(the_data))

## -----------------------------------------------------------------------------
model1 <- tvem(data=the_data,
               formula=y~1,
               id=subject_id,
               time=time)

## -----------------------------------------------------------------------------
model1_2knots_unpenalized <- tvem(data=the_data,
               formula=y~1,
               id=subject_id,
               num_knots=2,
               penalize=FALSE,
               time=time)

## ----fig.width=4,fig.height=4-------------------------------------------------
print(model1_2knots_unpenalized)
plot(model1_2knots_unpenalized)

## ----fig.width=4,fig.height=4-------------------------------------------------
model2 <- select_tvem(data=the_data,
                    formula = x1~1,
                    id=subject_id,
                    time=time,
                    max_knots=5)
print(model2)
plot(model2)

## ----fig.width=4,fig.height=4-------------------------------------------------
model2_selected_knots <- select_tvem(data=the_data,
                    formula = x1~1,
                    id=subject_id,
                    time=time,
                    max_knots=10)
print(model2_selected_knots)
plot(model2_selected_knots)

## ----fig.width=4,fig.height=4-------------------------------------------------
model2_selected_knots_bic <- select_tvem(data=the_data,
                    formula = x1~1,
                    id=subject_id,
                    use_bic=TRUE,
                    time=time,
                    max_knots=10)
print(model2_selected_knots_bic)
plot(model2_selected_knots_bic)

## ----fig.width=4,fig.height=4-------------------------------------------------
model3 <- tvem(data=the_data,
               formula=y~x1+x2,
               id=subject_id,
               time=time)
print(model3)
plot(model3)

## ----fig.width=4,fig.height=4-------------------------------------------------
model4 <- tvem(data=the_data,
               formula=y~x1,
               invar_effect=~x2,
               id=subject_id,
               time=time)
print(model4)
plot(model4)

## -----------------------------------------------------------------------------
set.seed(123)
the_data <- simulate_tvem_example(simulate_binary=TRUE)

## -----------------------------------------------------------------------------
model_binary1 <- tvem(data=the_data,
               formula=y~1,
               family=binomial(),
               id=subject_id,
               time=time)

## -----------------------------------------------------------------------------
model_binary2 <- tvem(data=the_data,
               formula=y~x1,
               invar_effect=~x2,
               id=subject_id,
               family=binomial(),
               time=time)
print(model_binary2)
plot(model_binary2)

## -----------------------------------------------------------------------------
plot(model_binary2, exponentiate=TRUE)

## -----------------------------------------------------------------------------
set.seed(12345)
n <- 100
simulated_dataset <- NULL
for (this_id in 1:n) {
  age <- seq(10,16,by=.5)
  female <- rbinom(1,1,1/3)
  sample_weight <- ifelse(female==1,yes=2,no=1)
  if (female==1) {
    logistic_curve <- round(rnorm(1,0,1) + 135 + 
                              runif(1,.9,1.1)*(160-135)/(1+exp(-.75*(age-13))) + 
                              rnorm(length(age),0,.5),1)
  } else {
    logistic_curve <- round(rnorm(1,0,1) + 140 + 
                              runif(1,.9,1.1)*(175-140)/(1+exp(-(age-13))) +
                              rnorm(length(age),0,.5),1)
  }
  simulated_dataset <- rbind(simulated_dataset,
                             data.frame(id=this_id,
                                        sample_weight=sample_weight,
                                        female=female,
                                        age=age,
                                        height=logistic_curve))
}
summary(simulated_dataset)

## -----------------------------------------------------------------------------
tvem1_unweighted <- tvem(data=simulated_dataset,
                         formula=height~1,
                         id=id,
                         num_knots=5,
                         time=age)
tvem1_weighted <- tvem(data=simulated_dataset,
                         formula=height~1,
                         id=id,
                         num_knots=5,
                         time=age,
                         weights=sample_weight)
plot(tvem1_weighted)

## -----------------------------------------------------------------------------
print(summary((tvem1_unweighted$grid_fitted_coefficients$`(Intercept)`$estimate)))
print(summary((tvem1_weighted$grid_fitted_coefficients$`(Intercept)`$estimate)))

## -----------------------------------------------------------------------------
tvem2_unweighted <- tvem(data=simulated_dataset,
                         formula=height~female,
                         id=id,
                         num_knots=5,
                         time=age)
tvem2_weighted <- tvem(data=simulated_dataset,
                       formula=height~female,
                       id=id,
                       num_knots=5,
                       time=age,
                       weights=sample_weight)
plot(tvem2_weighted)
print(summary((tvem2_unweighted$grid_fitted_coefficients$`(Intercept)`$estimate)))
print(summary((tvem2_weighted$grid_fitted_coefficients$`(Intercept)`$estimate)))

## -----------------------------------------------------------------------------

cross_sectional_example <- simulate_cross_sectional_tvem_example(
  n_subjects = 500,  
  min_time = 20,
  max_time = 70,
  simulate_binary = TRUE)
  
print(head(cross_sectional_example))


model1_cross_sectional <- tvem(data=cross_sectional_example,
                               num_knots=2,
                               spline_order=1,
               formula=y~1,
               id=subject_id,
               time=time)
print(model1_cross_sectional)
plot(model1_cross_sectional)

model2_cross_sectional <- tvem(data=cross_sectional_example,
                               formula=y~x1+x2,
                               id=subject_id,
                               time=time)
print(model2_cross_sectional)


