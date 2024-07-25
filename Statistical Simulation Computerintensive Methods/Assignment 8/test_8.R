alpha = 5
beta = 10

shape = nrow(df_auto)+alpha
scale = beta + sum(df_auto$mpg-(model.ls$coefficients[1]+model.ls$coefficients[2]*df_auto$horsepower))


dgamma(df_auto$horsepower, shape = shape, scale = scale)
