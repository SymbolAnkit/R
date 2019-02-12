 ### Grow the tree

    rpart(formula, data= , method= , control= )
    
    formula
    data = dataframe
    method = "class" for classification tree
              "anova" for regression tree
    control = controlling tree growth. E.g. control = rpart.control(minsplit = 30 , cp = 0.01) complexity factor  

### Examine the results
  
    printcp(fit)	- display cp table
    
    plotcp(fit) -	plot cross-validation results
    
    rsq.rpart(fit)-	plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the    "anova" method.
    
    print(fit)-	print results
    
    summary(fit)-	detailed results including surrogate splits
    
    plot(fit)-	plot decision tree
    
    text(fit)-	label the decision tree plot
