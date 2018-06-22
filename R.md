                                                  Random Numbers

---Genetare random numbers 

runif(1) - By default, its range is from 0 to 1.

0.5234033

runif(5)

0.609372596 0.915469242 0.434118286 0.848293641 0.009619427

 ---random numbers with defined range.

runif(5 , max = 100 , min = 50)

53.06248 91.00081 66.66493 87.40781 96.19702

sample(50:100,3,replace = T)

85 74 57

 ---Generate numbers from a normal distribution 

rnorm(5) - By default the mean is 0 and the standard deviation is 1.

-0.4315065 -0.8219912 -1.4829889 -1.1237049  0.7290666

 ---random numbers with defined mean and standard deviation.

rnorm(5 , mean = 5 , sd= 2)

4.509389 4.051663 8.033950 5.666774 6.602866

