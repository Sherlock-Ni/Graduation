\name{ivreg2}
\alias{ivreg2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Instrumental Variable Regression and Diagnostics
}
\description{
This pacakge can help you to do IV Regression and Diagnostics.
The test includes an F test of the first stage regression for weak instuments, a Wu-Hausman Test
for endogeneity (F) and a Sargan test of overidentifing restrictions.
}
\usage{
ivreg2(form, endog, iv, data, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{
The formula that you want to regress
}
  \item{endog}{
The variable that is endogeneity
}
  \item{iv}{
The variable that you set as instument variable
}
  \item{data}{
The origin of the data which is a data frame, maybe should use as.data.frame
}
  \item{digits}{
The digit of result
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Stephen Ni
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
ivreg in AER
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (form, endog, iv, data, digits = 3)
{
    r1 <- lm(form, data)
    y <- r1$fitted.values + r1$resid
    x <- model.matrix(r1)
    aa <- rbind(endog == colnames(x), 1:dim(x)[2])
    z <- cbind(x[, aa[2, aa[1, ] == 0]], data[, iv])
    colnames(z)[(dim(z)[2] - length(iv) + 1):(dim(z)[2])] <- iv
    z <- as.matrix(z)
    pz <- z \%*\% (solve(crossprod(z))) \%*\% t(z)
    biv <- solve(crossprod(x, pz) \%*\% x) \%*\% (crossprod(x, pz) \%*\%
        y)
    sigiv <- crossprod((y - x \%*\% biv), (y - x \%*\% biv))/(length(y) -
        length(biv))
    vbiv <- as.numeric(sigiv) * solve(crossprod(x, pz) \%*\% x)
    res <- cbind(biv, sqrt(diag(vbiv)), biv/sqrt(diag(vbiv)),
        (1 - pnorm(biv/sqrt(diag(vbiv)))) * 2)
    res <- matrix(as.numeric(sprintf(paste("\%.", paste(digits,
        "f", sep = ""), sep = ""), res)), nrow = dim(res)[1])
    rownames(res) <- colnames(x)
    colnames(res) <- c("Coef", "S.E.", "t-stat", "p-val")
    y1 <- data[, endog]
    z1 <- x[, aa[2, aa[1, ] == 0]]
    bet1 <- solve(crossprod(z)) \%*\% crossprod(z, y1)
    bet2 <- solve(crossprod(z1)) \%*\% crossprod(z1, y1)
    rss1 <- sum((y1 - z \%*\% bet1)^2)
    rss2 <- sum((y1 - z1 \%*\% bet2)^2)
    p1 <- length(bet1)
    p2 <- length(bet2)
    n1 <- length(y)
    fs <- abs((rss2 - rss1)/(p2 - p1))/(rss1/(n1 - p1))
    firststage <- c(fs)
    firststage <- matrix(as.numeric(sprintf(paste("\%.", paste(digits,
        "f", sep = ""), sep = ""), firststage)), ncol = length(firststage))
    colnames(firststage) <- c("First Stage F-test")
    bols <- solve(crossprod(x)) \%*\% crossprod(x, y)
    sigols <- crossprod((y - x \%*\% bols), (y - x \%*\% bols))/(length(y) -
        length(bols))
    vbols <- as.numeric(sigols) * solve(crossprod(x))
    sigml <- crossprod((y - x \%*\% bols), (y - x \%*\% bols))/(length(y))
    x1 <- x[, !(colnames(x) \%in\% "(Intercept)")]
    z1 <- z[, !(colnames(z) \%in\% "(Intercept)")]
    pz1 <- z1 \%*\% (solve(crossprod(z1))) \%*\% t(z1)
    biv1 <- biv[!(rownames(biv) \%in\% "(Intercept)"), ]
    bols1 <- bols[!(rownames(bols) \%in\% "(Intercept)"), ]
    resids <- NULL
    resids <- cbind(resids, y1 - z \%*\% solve(crossprod(z)) \%*\%
        crossprod(z, y1))
    x2 <- cbind(x, resids)
    bet1 <- solve(crossprod(x2)) \%*\% crossprod(x2, y)
    bet2 <- solve(crossprod(x)) \%*\% crossprod(x, y)
    rss1 <- sum((y - x2 \%*\% bet1)^2)
    rss2 <- sum((y - x \%*\% bet2)^2)
    p1 <- length(bet1)
    p2 <- length(bet2)
    n1 <- length(y)
    fs <- abs((rss2 - rss1)/(p2 - p1))/(rss1/(n1 - p1))
    fpval <- 1 - pf(fs, p1 - p2, n1 - p1)
    hawu <- c(fs, fpval)
    hawu <- matrix(as.numeric(sprintf(paste("\%.", paste(digits,
        "f", sep = ""), sep = ""), hawu)), ncol = length(hawu))
    colnames(hawu) <- c("Wu-Hausman F-test", "p-val")
    ivres <- y - (x \%*\% biv)
    oid <- solve(crossprod(z)) \%*\% crossprod(z, ivres)
    sstot <- sum((ivres - mean(ivres))^2)
    sserr <- sum((ivres - (z \%*\% oid))^2)
    rsq <- 1 - (sserr/sstot)
    sargan <- length(ivres) * rsq
    spval <- 1 - pchisq(sargan, df = length(iv) - 1)
    overid <- c(sargan, spval)
    overid <- matrix(as.numeric(sprintf(paste("\%.", paste(digits,
        "f", sep = ""), sep = ""), overid)), ncol = length(overid))
    colnames(overid) <- c("Sargan test of over-identifying restrictions",
        "p-val")
    if (length(iv) - 1 == 0) {
        overid <- t(matrix(c("No test performed. Model is just identified")))
        colnames(overid) <- c("Sargan test of over-identifying restrictions")
    }
    full <- list(results = res, weakidtest = firststage, endogeneity = hawu,
        overid = overid)
    return(full)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
