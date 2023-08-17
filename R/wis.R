wis <- function(x, 
                # x is a set of samples of the model under investigation
                y,
                # y is the comparison model (reference model), same number of posterior samples
                # OR length one: the median point estimate to compare
                a = c(.02, .05, seq(.1,.9, by = .1)) 
                # a/2*100 is the nominal intervals considered
                ){
  
# Determine weights
  K = length(a)
  wk = a/2
  
# Get the quantiles from x
  xm = quantile(x, .5)
  xu = quantile(x, 1-wk)
  xl = quantile(x, wk)
  
  # Get comparison estimates
  if(length(y) == 1){
    ym = y
  }
  else{
  ym = quantile(y, .5)
    }
  
  # Calculate difference
  bias = abs(xm - ym)
  dev = xm - ym
  
  if(length(y) > 1) {
  stdy = sd(y)
  bias_std = bias/ym
  }  else { 
    stdy = NA
    bias_std = bias/ym
  }
  
  # Calculate coverage
  # coverage is a K vector, describing the coverage of each of x's intervals
  coverage = sapply(1:K, function(i){
    1-((sum(y < xl[i]) + sum(y > xu[i]))/length(y))
  })
  # coverage 95 is the 95% CI coverage
  coverage95 = 1-(sum(y < quantile(x, .025)) + sum( y > quantile(x, .975)))/length(y)
  coverage_mn = mean(abs(coverage-(1-a)))
  
  width = mean(wk*(xu - xl))
  widthstd = mean(wk*((xu-xl)/xm))
  width95 = (.05/2)*(quantile(x, .975) - quantile(x, .025))
  IS = sapply(1:K, function(i){
    (xu[i] - xl[i]) + mean((2/a[i]) * (xl[i] - ym) * (ym < xl[i])) + mean((2/a[i]) * (ym - xu[i]) * (ym > xu[i]))
    # (xu[i] - xl[i]) + mean((2/a[i]) * (xl[i] - y) * (y < xl[i])) + mean((2/a[i]) * (y - xu[i]) * (y > xu[i]))
  })
  spread = sum(wk * IS)
  WIS = (1/(K+0.5)) * (0.5*abs(ym-xm) + spread)  

  return(list(
    "df" = data.frame(bias, bias_std, stdy, dev, coverage95, coverage_mn,
                      width, widthstd, width95, WIS),
    "alpha" = 1-a,
    "coverage" = coverage,
    "coverage95" = coverage95,
    "coveragemn" = coverage_mn,
              "width" = width,
              "width95" = width95,
    "widthstd" = widthstd,
    "dev" = dev,
              "bias" = bias,
              "bias_std" = bias_std,
              "IS" = IS,
              "WIS" = WIS))
  }

# x <- rnorm(1000)
# y <- 0
# wis(x,y)
# y2 <- rnorm(1000, 1)
# wis(x, y2)
