# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.

.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

#    ols_model<-lm(expenditure~income,data=consump)
#resid1<-resid(ols_model)
#sigma_hat_sq<-sum(resid1^2)/(n-2)
#error_dep<-resid1^2/sigma_hat_sq
#BP_ols<-lm(error_dep~consump$income)
#RSS=sum((fitted(BP_ols)-mean(error_dep))^2)
#Test statstic
#(chi_cal<-sum((fitted(BP_ols)-mean(error_dep))^2/2)

# Electricity consumption data
electricity_consumption<- read.csv(file.path(.get_course_path(), "Econometrics", "The_Simple_Linear_Regression_Model", "electricity consumption.csv"))

rownames(electricity_consumption) <- NULL

# To reduce spelling error
elcty<-electricity_consumption

# To increse freedom instead of data frame
attach(electricity_consumption)
y_dev<-Y-mean(Y)
x_dev<-X-mean(X)

#############
# For local rss
fit<-lm(Y~X,data=elcty)
TSS<-sum(elcty$Y-mean(elcty$Y))^2
RSS<-sum((fitted(fit)-mean(elcty$Y))^2)
ESS<-TSS-RSS

# F-tab and cal
F_stat<-(RSS/(2-1))/(ESS/(nrow(elcty)-2))

# ## For those not used a data frame in the first place but may latter need to use?
# elcty$y_dev<-elcty$Y-mean(elcty$Y)
# elcty$x_dev<-elcty$Y-mean(elcty$X)




# #fit<-a_hat+b_hat*elcty$X
# 
# # fit0 <-lm(expen ~ income, consmp)
# # # Sorted data based on income(x)
# # consmp_o<-consmp[order(consmp$income),] 
# # 
# # fit_gfq_n1<-lm(expen~income,consmp_o[1:8,])
# # fit_gfq_n2<-lm(expen~income,consmp_o[13:20,])
# # s1=sum(resid(fit_gfq_n1)^2)/(8-2)
# # s2=sum(resid(fit_gfq_n2)^2)/(8-2)
# # 
# # F_cal<-s2/s1
# # F_tab<-qf(0.05,8-2,8-2, lower.tail=FALSE)
# # BP-test
# res_bp<-resid(fit0)^2/(deviance(fit0)/(20-2))
# fit_bp<-lm(res_bp~consmp$income)
# rss=sum((fitted(fit_bp)-mean(res_bp))^2)
# 
# 
# 
# 
# 
# #=====================================
# #White test
# res_wt<-resid(fit0)
# 
# fit_wt<-lm(I(res_wt^2)~consmp$income+I(consmp$income^2))
# 
# Rw_2<-summary(fit_wt)$r.squared
# 
# #======================================
# #------------------ WLS
# # a sorted of residuals based on income explantory variable
# res_sorted<-resid(fit0)[order(consmp$income)]
# # values based on your slide note
# n1=7;n2=7;n3=6
# # Estimated variance for each three parts to estimate weight
# #sigma_hat1<-sum(res_sorted[1:7]^2)/n1
# #sigma_hat2<-sum(res_sorted[8:14]^2)/n2
# #sigma_hat3<-sum(res_sorted[15:20]^2)/n3
# # weight vector of size 20
# #weight<-rep(c(sqrt(sigma_hat1),sqrt(sigma_hat2),sqrt(sigma_hat3)),c(7,7,6))
# #fit_w1<-lm(I(expen/weight)~I(1/weight)+I(income/weight),consmp)
# 
# 
# #local({
# # fname <- file.path(.get_course_path(),
# #                    "Econometrics", "Heteroscedasticity", "goldfeld_quadnt.R")
# # file.copy(fname, "goldfeld_quadnt.R")
# # file.edit("goldfeld_quadnt.R")
# #})
# 
# #source("goldfeld_quadnt.R") # to run the elimination function must be sourced
# 
# #expenditure<-consump$expenditure
# #income<-consump$income

# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.
#BP_test_hetro<-function(x=as.data.frame(x)){
#n=nrow(x)
#3rd variable is expenditure, 2=income
#ols_model<-lm(x[,3]~x[,2],data=x)
#
#resid1<-resid(ols_model)





swirl_options(swirl_logging = TRUE)
