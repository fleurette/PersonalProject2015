function main(filePath,savePath)

load(filePath);

plot1=strcat('/../R/',savePath,'test_final');
plot2=strcat('/../R/',savePath,'pregnant_final');

plotFinal(strcat('/../R/',savePath,'test_final'),testAdjustedMales,testAdjustedFemales);
plotFinal(strcat('/../R/',savePath,'pregnant_final'),pregnantAdjustedMales,pregnantAdjustedFemales);

exit;
