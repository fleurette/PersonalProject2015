source("dataLoad.r")
source("dataPlot.r")
source("dataAnalyze.r")

# Load data
femaleData = loadFemaleData()
maleData = loadMaleData()

# Means
femaleData.means = getMeans(femaleData)
maleData.means = getMeans(maleData)
plotMeans(maleData.means, femaleData.means)

# Density
femaleData.density = getMeanDensity(femaleData.means)
maleData.density = getMeanDensity(maleData.means)
plotMeanDensities(maleData.density, femaleData.density)
