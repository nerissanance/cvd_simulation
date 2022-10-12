### synthesizeDD.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov  3 2021 (15:20)
## Version:
## Last-Updated: Nov  5 2021 (17:47)
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Synthesizing longitudinal diabetes dementia followup data
##'
##' A sequence of logistic regression models Danmark Statistics
##' @title Synthesizing longitudinal diabetes dementia followup data
##' @param coefficients Intercepts and regression coefficients (log-odds-ratios)
##' @return \code{lvm} object for simulation
##' @seealso \code{lvm}, \code{distribution}, \code{regression}, \code{sim}
##' @examples
##' library(lava)
##' library(data.table)
##' cc <- fread("data/coefficients.txt")
##' u <- synthesizeDD(cc)
##' d <- sim(u,1000)
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
synthesizeDD <- function(coefficients){
    requireNamespace("lava")
    coefficients <- data.table(coefficients)
    XNAMES <- names(coefficients)[-(1:3)]
    BETA <- coefficients[,-(1:3),with=0L]
    INTERCEPT <- coefficients[["(Intercept)"]]
    # empty lava model for simulation
    m <- lvm()
    distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
    distribution(m,"sex") <- binomial.lvm(p=0.4)
    m <- addvar(m,"ie.type")
    m <- addvar(m,"code5txt")
    m <- addvar(m,"quartile_income")
    # loop across time and variables
    for (j in 1:NROW(coefficients)){
        V <- coefficients$var[j]
        beta <- unlist(BETA[j,])
        X <- XNAMES[!is.na(beta)]
        beta <- beta[!is.na(beta)]
        # add V ~ Intercept + beta X
        distribution(m,V) <- binomial.lvm()
        intercept(m,V) <- INTERCEPT[j]
        regression(m,from=X,to=V) <- beta
    }
    class(m) <- c("synthesizeDD",class(m))
    m
}
sim.synthesizeDD <- function(x,...){
    class(x) <- class(x)[-1]
    d <- data.table(sim(x,...))
    v <- names(d)
    ttt <- sapply(strsplit(grep("death",v,value=TRUE),"event_death_"),"[",2)
    ## HERE
    ## for (t in ttt) set(x=d,j=paste0("fup_",t),value=d[[paste("event_death_",t)]]+d[[paste("event_dementia_",t)]]+d[[paste("censor_",t)]])
    #d <- apply(d,1,function(i){    })
    #fup <- data.table()
    #d[,fup:={browser();apply(.SD,1,function(x){browser()})},.SDcols=grep("event|censor|death",names(d))]
    d
}

#----------------------------------------------------------------------
### synthesizeDD.R ends here
