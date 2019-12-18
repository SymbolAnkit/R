# Time Series
Time series analysis may be divided into two classes: 
        
    1.	frequency-domain methods 
    2.	time-domain methods

Time series analysis techniques may be divided into 

    1.	parametric and
    2.	non-parametric methods. 

   _The parametric approaches assume that the underlying stationary stochastic process has a certain structure which can be described using a small number of parameters (for example, using an autoregressive or moving average model). In these approaches, the task is to estimate the parameters of the model that describes the stochastic process._ 

   _By contrast, non-parametric approaches explicitly estimate the covariance or the spectrum of the process without assuming that the process has any particular structure._

Methods of time series analysis may also be divided into 
            
              linear and non-linear, and univariate and multivariate.


## Models for Time Series ##

Models for time series data can have many forms and represent different stochastic processes.
When modeling variations in the level of a process, three broad classes of practical importance are 
_the autoregressive (AR) models, the integrated (I) models, and the moving average (MA) models._

These three classes _depend linearly on previous data points_.
Combinations of these ideas produce autoregressive moving average (ARMA) and 
autoregressive integrated moving average (ARIMA) models. 
The autoregressive fractionally integrated moving average (ARFIMA) model generalizes the former three. 
Extensions of these classes to deal with vector-valued data are available under the heading of multivariate time-series models and sometimes the preceding acronyms are extended by including an initial "V" for "vector", as in VAR for vector autoregression. 

An additional set of extensions of these models is available for use where the observed time-series is driven by some "forcing" time-series (which may not have a causal effect on the observed series): the distinction from the multivariate case is that the forcing series may be deterministic or under the experimenter's control. For these models, the acronyms are extended with a final "X" for "exogenous".



