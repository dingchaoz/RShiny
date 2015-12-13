diag <- c("a","b","c","d")
iupr <- c(0,1,0.33,1.2)
cpk <- c(0.30,1.4,1.0,2)
grade <- function(x){
        if(x < 1.33){
                RYG = "Red"
                grade_code = 1
        }
        else{
                RYG = "Green"
                grade_code = 3
        }
        list(RYG = RYG, code = grade_code)
}

IUR_Grade <- function(y){
        if(y<0.33){
                iurg <- "Red"
        }
        else {
                iurg <- "Green"
        }
        return(iurg)
}

GradeCode <- function(z){
        # browser()
        z$code
        
}
cap_RYG <- lapply(cpk,grade)
IUR_RYG <- tapply(diag,seq(1:length(diag)),IUR_Grade)
# RYG <- as.numeric(factor(cap_RYG))
# IRYG <- as.numeric(factor(IUR_RYG))