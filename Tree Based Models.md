 ### Grow the tree

    rpart(formula, data= , method= , control= )
    
    formula
    data = dataframe
    method = "class" for classification tree
              "anova" for regression tree
    control = controlling tree growth. E.g. control = rpart.control(minsplit = 30 , cp = 0.01) complexity factor  
