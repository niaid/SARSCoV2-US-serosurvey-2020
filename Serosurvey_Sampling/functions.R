### CUSTOM FUNCTIONS 

# DECLARE CUSTOM FUNCTIONS 
# This function calculates age for us: 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  # Age calculation taken from
  # http://jenrichmond.rbind.io/portfolio/calculating-age/ 
  require(lubridate)
  period <- as.period(interval(birthDate, refDate), unit = "year")
  period$year
} 


# READ-IN OTHER CUSTOM FUNCTIONS: 
# THIS GIVES US A QUICK UNIVARIATE COMPARISON OF SHORTFALLS AND 
# POTENTIAL CHARACTERISTICS WE MAY WNAT TO OVERSAMPLE FROM OUR AVAILABLE POOL 
compare.prop <- function(varname = "sex", data = pooldat) {
  dat <- data
  var <- dat[, varname]
  temp <- data.frame(groups = unique(var)[order(unique(var))]
                     , overall = aggregate(
                       data$R10.000, by = list(var), sum)$x 
                     , accrued = aggregate(
                       data$n, by = list(var), sum)$x 
                     , target = aggregate(
                       data$newtarget, by = list(var), sum)$x 
# I tried including this, but it doens't give us much information
# since the shortfall is cross-classification specific but the 
# pool available is the marginal total avialable in the pool 
#                     , available = aggregate(
#                       data$pool, by = list(var), sum, na.rm = TRUE)$x
                     , shortfall = aggregate(
                       data$shortfall, by = list(var), sum)$x)
  temp$percentshort <- temp$shortfall / temp$target
  temp$percentaccrued <- temp$accrued/temp$overall 
  temp[ , c("groups", "overall", "accrued", "percentaccrued"
            , "target", "shortfall", "percentshort" )]
}



