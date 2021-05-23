# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.

      .get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}


 auto<- read.csv(file.path(.get_course_path(), "Econometrics", "Autocorrelation", "auto.csv"))
 names(auto)=c("year","invest","vsh")
 
 fit0<-lm(invest~vsh,data=auto)
 ei_hat<-resid(fit0)
 
 fit0_ei<-lm(ei_hat[-1]~auto$vsh[-1]+ei_hat[-19])
T=19
p=1
roha<-0.8049

# Trasformation
y_transf<-auto$invest[-1]-0.8049*auto$invest[-length(auto$invest)]
x_transf<- auto$vsh[-1]-0.8049*auto$vsh[-length(auto$vsh)]

fit_no_ac<-lm(y_transf~x_transf)

#local({
 # fname <- file.path(.get_course_path(),
 #                    "Econometrics", "Heteroscedasticity", "goldfeld_quadnt.R")
 # file.copy(fname, "goldfeld_quadnt.R")
 # file.edit("goldfeld_quadnt.R")
#})

#source("goldfeld_quadnt.R") # to run the elimination function must be sourced

#expenditure<-consump$expenditure
#income<-consump$income

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
