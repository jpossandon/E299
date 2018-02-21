%%
figure,hold on
col = [1 0 0;0 0 1];
for qq = 1:length(q)
    rangeI = 0:.005:.2;
    indxData = q(qq).trialCount;
    [lC_unc,dev_unc,stat] = glmfit(10.^q(qq).intensity(1:indxData)',q(qq).response(1:indxData)','binomial','logit');
    [logitFit_unc logitFit_unc_lo logitFit_unc_hi] = glmval(lC_unc,rangeI ,'logit',stat);

    %TODO se
    
    % vline(0,'k--')
    %hline(.5,'k--')
    plot(10.^q(qq).intensity(1:indxData)',q(qq).response(1:indxData)','o','MarkerSize',6,'Color',col(qq,:))
    h(qq) = plot(rangeI ,logitFit_unc,'r','LineWidth',2,'Color',col(qq,:));
    xlim([0 rangeI(end)])
end

