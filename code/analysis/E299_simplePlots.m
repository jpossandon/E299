%%
edges = 0:.05:1.5;
figure
cmap    = cbrewer('qual','Paired',18);
lnstl   = repmat({'-','-.',':'},1,4);
leglab  = {'ext1unc','ext2unc','ext3unc','ext1c','ext2c','ext3c',...
    'anat1unc','anat2unc','anat3unc','anat1c','anat2c','anat3c'};
n=1;
for b = 1:2
   for c = 0:1
        for i = 1:3
        
            data = result.trial_RT(result.trial_correct==1 ...
                & result.trial_blockType==b ...
                & result.trial_crossed==c ...
                & result.trial_int == i)
            N = histc(data,edges);
            stat(n,:) = [median(data) mean(data) std(data)];
            h(n) = plot(edges,N./sum(N),'LineStyle',lnstl{n},'Color',cmap(n,:),'LineWidth',2); hold on;
            n=n+1;
        end
    end
end
legend(h,leglab)
xlabel('Reaction Time (s)')
ylabel('Frequency')
box off
            
%%
figure,
hold on
leglab = {'ext Uncross','ext Cross','anat Uncross','anat Cross'};
for e = 1:4
    hh(e) = errorbar(e*3-2:e*3,stat(e*3-2:e*3,2),stat(e*3-2:e*3,3),...
        'Color',cmap(e*3-2,:),'LineWidth',2);
    plot(e*3-2:e*3,stat(e*3-2:e*3,2),'o',...
        'MarkerFaceColor',cmap(e*3-2,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
    plot(e*3-2:e*3,stat(e*3-2:e*3,1),'s',...
        'MarkerFaceColor',cmap(e*3-2,:),'MarkerEdgeColor',[0 0 0],'MarkerSize',8)
    text(e*3-2,.9,leglab(e),'Color',cmap(e*3-2,:),'FontSize',12,'Fontweight','bold')
end
axis([0.5 12.5 0 1])
set(gca,'XTick',1:12,'XTickLabel',{'3dB','6dB','9dB'})
% legend(hh,{'extUnc','extC','anatUnc','anatC'})
xlabel('Intensity','FontSize',14)
ylabel('RT (mean+SD)','FontSize',14)
vline(3.5:3:9.5,':k')
tightfig