# 허리둘레 결측 여부에 따른 분포
This folder is for saving figures of how age, gender, and BMI are distributed differently between those who measured their waist circumferences(WC) and those who didn't. This process is necessary for determining which model to use for filling in missing values of WC:
* **MCAR(Missing Completely At Random)** - the fact that *a certain variable is missing values* has ***nothing** to do with its hypothetical value and the values of other variables*.
* **MAR(Missing At Random)** - *the propensity for a data point to be missing* is ***not** related to the missing data*, but it is *related to **some** of the observed data.*
* **MNAR(Missing Not At Random)** - *a certain variable is missing values* because
  * the fact that it is missing values *has to do with its hypothetical values* or
  * the propensity for a data point to be missing is *related to **all** of the observed data*.
  
In case of **MCAR**, it is the best option to *drop observations with missing values*. In case of **MAR** and **MNAR**, it is a good idea to use **multiple imputation by chained equations(MICE)**, which repeatedly fills in missing values by running multiple Bayesian regressions on and on, taking turns emptying imputed values of each variable with missing values. Yet, *when there is only a single variable that is missing values, there is no meaning in using MICE.* In R, you can install package *mice* and fill in missing values with MICE method.  
