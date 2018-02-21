%%
datapath    = '/Users/jossando/trabajo/E299/';
subj        = 1;
task        = 'handEye';
filename    = sprintf('%sdata/s%d_%s/s%d_%s_results',datapath,subj,task,subj,task);

load(filename)
edges       = 0:.05:1.5;
figure
cmap    = cbrewer('qual','Paired',9);
cmap(3:4,:) = [];
lnstl   = repmat({'-',':'},1,4);
leglab  = {'eyeOpenUnc','eyeOpenC','eyeClosedUnc','eyeClosedC'};
n=1;
for b = 1:2
   for c = 0:1
        
            data = result.trial_RT(result.trial_correct==1 ...
                & result.trial_blockType==b ...
                & result.trial_crossed==c )
            N = histc(data,edges);
            stat(n,:) = [median(data) mean(data) std(data)];
            h(n) = plot(edges,N./sum(N),'LineStyle',lnstl{n},'Color',cmap(n,:),'LineWidth',2); hold on;
            n=n+1;
    end
end
legend(h,leglab)
xlabel('Reaction Time (s)')
ylabel('Frequency')
box off
 
figname = sprintf('%sfigures/s%d_%s_hist',datapath,subj,task);
print(gcf,'-dpng',figname)
close gcf
%%
figure,
hold on


hh(1) = errorbar([1:2]-.1,stat(1:2,2),stat(1:2,3),...
    'Color',cmap(1,:),'LineWidth',2);
plot([1:2]-.1,stat(1:2,2),'o',...
     'MarkerFaceColor',cmap(1,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
 plot([1:2]-.1,stat(1:2,1),'s',...
        'MarkerFaceColor',cmap(1,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
  
hh(2) = errorbar([1:2]+.1,stat(3:4,2),stat(3:4,3),...
    'Color',cmap(3,:),'LineWidth',2);
plot([1:2]+.1,stat(3:4,2),'o',...
     'MarkerFaceColor',cmap(3,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
plot([1:2]+.1,stat(3:4,1),'s',...
        'MarkerFaceColor',cmap(3,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
   
   
legend(hh,{'Eyes Open','Eyes Closed'})
  
axis([0.5 2.5 0 .6])
set(gca,'XTick',1:2,'XTickLabel',{'Uncrossed','Crossed'})
ylabel('RT (mean+SD)','FontSize',14)
tightfig

figname = sprintf('%sfigures/s%d_%s_means',datapath,subj,task);
print(gcf,'-dpng',figname)