function plotFinal(savePath,males,females)

% Extract means and sdes
x = transpose(males.taxis);
sdeMales = transpose(males.sd./sqrt(males.count));
meanMales = transpose(males.mean);
sdeFemales = transpose(females.sd./sqrt(females.count));
meanFemales = transpose(females.mean);
% Plot mean versus SDE
figure;
hold on;
handle = shadeplot(x,meanMales,sdeMales,'b',[0.2 0.9 0.9 ]);
legend('Area between mean and sde','Male mean tweet probability density','Male sde tweet probability density');
axis tight;
print([pwd,'/',savePath,'_sde_mean_males.png'],'-dpng');

figure;
hold on;
shadeplot(x,meanFemales,sdeFemales,'r',[0.6 0.2 0.2]);
legend('Area between mean and sde','Female mean tweet probability density','Female sde tweet probability density');
axis tight;
print([pwd,'/',savePath,'_sde_mean_females.png'],'-dpng');

% Plot Error plot
%errorbarxy(meanFemales,meanMales,sdeFemales,sdeMales)
%axis square
%axis equal
%xlabel('Male tweet probability density')
%ylabel('Female tweet probability density')
%print([pwd,'/',savePath,'errorbarxy.png'],'-dpng')
