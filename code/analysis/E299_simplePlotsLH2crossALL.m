
%%
% data
stat_avgdB = []
for subj = unique(allData.subjIndx)
    nndB = 1;
    for b = 1:2
        for cL = 0:1
            for cH = 0:1
                data = allData.trial_RT(allData.trial_correct==1 ...
                    & allData.trial_blockType==b ...
                    & allData.trial_crossed_legs==cL ...
                    & allData.trial_crossed_hand==cH ...
                    & allData.subjIndx==subj);
                    stat_avgdB(nndB,:,subj) = [median(data) mean(data) std(data)];
                    nndB = nndB+1;
            end
        end
    end
end

%%
% dB averaged
figure,
set(gcf,'Position',[-1 64 1134 650])
hold on
leglab  = {'Ext UL UH','Ext UL CH',...
    'Ext CL UH','Ext CL CH',...
    'Anat UL UH','Anat UL CH',...
    'Anat CL UH','Anat CL CH'};
xx=1;
lstl = {'-','-',':',':'};
mstl = {'s-','s-','o:','o:'};
cmap2 = [0.3 0.3 0.3;1 0 0;0.3 0.3 0.3;1 0 0];
for e = 1:4
  
%     auindx = e*2-1:e*2;
    errorbar(1:2,squeeze(mean(stat_avgdB([e,e+4],2,:),3)),...
         squeeze(std(stat_avgdB([e,e+4],2,:),1,3))./sqrt(size(stat_avgdB,3)),...
         'Color',cmap2(e,:),'LineStyle','none','LineWidth',2)     
    hhh(e) = plot(1:2,squeeze(mean(stat_avgdB([e,e+4],2,:),3)),mstl{e},'Color',cmap2(e,:),...
         'MarkerFaceColor',cmap2(e,:),'MarkerEdgeColor',[0 0 0],...
         'LineWidt',2,'MarkerSize',10);
end
title(sprintf('N = %d',size(stat_avgdB,3)))
axis([0.75 2.25 0 1])
set(gca,'XTick',1:2,'XTickLabel',{'External','Anatomical'},'FontSize',16)
legend(hhh,{' || legs  || hands',' || legs  X hands',' X legs  || hands',' X legs  X hands'},...
    'box','off','FontSize',18)
xlabel('Response mode','FontSize',18)
ylabel('RT (mean+SEM)','FontSize',18)
tightfig
figname = sprintf('%s%sallSubjects_%s_meansNoInt',pathFigures,filesep,task); 
print(gcf,'-dpng',figname)
close(gcf)
