# 모델사후검증
This folder is for saving plots necessary for checking **if the applied LSDV(least-squares dummy variable) model satisfies major OLS(ordinary least squares) assumptions and has as little outliers as possible.**  
* **The major OLS assumptions** are as follows:
  * The residuals and the fitted values should not have a relationship with each other (in other words, they must be independent (at least linearly) from each other. This is because the fitted values themselves are linear transformations of independent variables, and independent variables are uncorrelated with residuals by the construction of the OLS)
  * The residuals, when standardized, must follow the standard normal distribution (mean = 0, sd = 1)
  * The standard deviation of residuals must be the same regardless of the values of independent variables (econometricians refer to this property as *homoskedasticity*)
  
* **The numbers to consider when determining outliers** are as follows:
  * **Leverage**: the measure of how the extreme value of the independent variable distorts the regression outcome
  * **Cook's Distance**: the measure of how much influence an observation would have on the regression outcome when eliminated from the sample
  
* **The plots** in this folder are:
  * **Residuals vs. Fitted** - A scatterplot showing if there is a relationship or pattern between residuals and fitted values of the OLS regression
  * **Normal Q-Q Plot** - A scatterplot showing how closely the residuals, when standardized, follow the standard normal distribution. If they show perfect standard normal distribution, all the points on the plot should be on a 45-degree line
  * **Scale-location** - A scatterplot showing how the spread of residuals change as the fitted values change. If there is a noticeable pattern in the plot, the homoskedasticity assumption of the OLS is broken
  * **Residuals vs. Leverage** - A scatterplot showing the leverage of each residual. Red lines indicating the borderline Cook's Distances (.5 and 1) are also drawn on the plot.
