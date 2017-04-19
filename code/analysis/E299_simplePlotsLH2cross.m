%%
datapath    = '/Users/jossando/trabajo/E299/';
subj        = 1;
task        = 'LH2cross';
filename    = sprintf('%sdata%s%s%ss%d_%s%ss%d_%s_results',datapath,filesep,task,...
                        filesep,subj,task,filesep,subj,task);
load(filename)
%%
edges       = 0:.05:2;
figure
cmap    = cbrewer('qual','Paired',12);
% cmap(5:8,:) = [];
lnstl   = repmat({'-',':'},1,8);


 % ressult response coding
      % blockType 1 - answer external 2- answer anatomical
      % limbside (anatomical) 1 - left 2 - right
      % crossed_leg 0 - uncrossed 1 - crossed
      % crossed_hand 0 - uncrossed 1 - crossed
      % response (anatomical) 1- left 2- right
leglab  = {'Ext UL UH 3dB','Ext UL UH 15dB','Ext UL CH 3dB','Ext UL CH 15dB',...
    'Ext CL UH 3dB','Ext CL UH 15dB','Ext CL CH 3dB','Ext CL CH 15dB',...
    'Anat UL UH 3dB','Anat UL UH 15dB','Anat UL CH 3dB','Anat UL CH 15dB',...
    'Anat CL UH 3dB','Anat CL UH 15dB','Anat CL CH 3dB','Anat CL CH 15dB'};
n=1;
col = 1;
for b = 1:2
   for cL = 0:1
        for cH = 0:1
            for i = 1:2
            data = result.trial_RT(result.trial_correct==1 ...
                & result.trial_blockType==b ...
                & result.trial_crossed_legs==cL ...
                & result.trial_crossed_hand==cH ...
                & result.trial_int==i);
                N = histc(data,edges);
                stat(n,:) = [median(data) mean(data) std(data) sum(N)];
                h(n) = plot(edges,N./sum(N),'LineStyle',lnstl{n},'Color',cmap(col,:),'LineWidth',2); hold on;
                n=n+1;
            end
            col = col+1;
        end
    end
end
legend(h,leglab)
xlabel('Reaction Time (s)')
ylabel('Frequency')
box off
 
figname = sprintf('%sfigures%s%s%ss%d_%s_hist',datapath,filesep,task,filesep,subj,task);
print(gcf,'-dpng',figname)
close gcf
%%
figure,
set(gcf,'Position',[-1 64 1134 650])
hold on
leglab  = {'Ext UL UH','Ext UL CH',...
    'Ext CL UH','Ext CL CH',...
    'Anat UL UH','Anat UL CH',...
    'Anat CL UH','Anat CL CH'};
xx=1;
for e = [1,5,2,6,3,7,4,8]
  
    auindx = e*2-1:e*2;
    hh(e) = errorbar(xx:xx+1,stat(auindx,2),stat(auindx,3),...
        'Color',cmap(e,:),'LineWidth',2);
    plot(xx:xx+1,stat(auindx,2),'o',...
        'MarkerFaceColor',cmap(e,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
    plot(xx:xx+1,stat(auindx,1),'s',...
        'MarkerFaceColor',cmap(e,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
    text(xx,.95,leglab(e),'Color',cmap(e,:),'FontSize',12,'Fontweight','bold')
    xx = xx+2;
end
axis([0.5 16.5 0 1])
set(gca,'XTick',1:16,'XTickLabel',{'3dB','15dB'})
% legend(hh,{'extUnc','extC','anatUnc','anatC'})
xlabel('Intensity','FontSize',14)
ylabel('RT (mean+SD)','FontSize',14)
vline(2.5:2:15.5,':k')
tightfig
 
 figname = sprintf('%sfigures%s%s%ss%d_%s_means',datapath,filesep,task,filesep,subj,task);
 print(gcf,'-dpng',figname)