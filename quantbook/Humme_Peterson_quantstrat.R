require('FinancialInstrument')
currency(c('GBP', 'USD'))
getSymbols.FI(Symbols='GBPUSD', dir=system.file('extdata', package='quantstrat'), from='2002-10-21', to='2002-10-31')
GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(GBPUSD, 1800)
head(GBPUSD)

