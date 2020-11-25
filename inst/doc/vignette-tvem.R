## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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

