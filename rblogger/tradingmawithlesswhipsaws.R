#http://www.quintuitive.com/2015/06/21/trading-moving-averages-with-less-whipsaws/
require(quantmod)
require(btutils) # For construct.indicator

# Download data
gspc = getSymbols("^GSPC",from="1900-01-01",auto.assign=F)

# Compute the returns
rets = ROC(Cl(gspc),type="discrete")

# Compute the adjusted returns
adj.rets = rets/sqrt(runSum(rets*rets,10)/9)

# The moving average
sma = runMean(adj.rets,n=200)

# The standard deviation
stddev = sqrt(runSum(adj.rets*adj.rets,200)/199)

# Long trades are entered when the average turns positive
upper.band = 0.0

# Long trades are closed when the average return goes below -0.05 standard deviations
lower.band = -0.05*stddev

# For the "uncushioned" version use
# lower.band = 0

uu = ifelse(sma>upper.band,1,0)
dd = ifelse(sma<lower.band,-1,0)
long.entries = (uu == 1 & lag(uu) == 0)
long.exits = (dd == -1 & lag(dd) == 0)
short.entries = long.entries
short.exits = long.entries
short.entries[] = FALSE
short.exits[] = FALSE

# Given entries and exits (both for long and short positions), this function builds
# the corresponding indicator. It's pure C++, so it's fast too.
ind = btutils::construct.indicator(long.entries, long.exits, short.entries, short.exits)