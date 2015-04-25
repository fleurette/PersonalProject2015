load('data.mat')

matlabPlot(strcat(path,'test_final.png'),testMales,testFemales)
matlabPlot(strcat(path,'pregnant_final.png'),pregnantMales,pregnantFemales)
matlabPlot(strcat(path,'test_adjusted_final.png'),testAdjustedMales,testAdjustedFemales)
matlabPlot(strcat(path,'pregnant_adjusted_final.png'),pregnantAdjustedMales,pregnantAdjustedFemales)

exit
