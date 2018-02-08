function exp = E299_psych(exp,pahandle,DIO,wave)
%%
% first step get threshold
% add opening of specs, move the parameters of the curve to exp structure
% and the results of the theshold and sd
display(sprintf('\nSearching psych data for subject %s',exp.sNstr))
if exist(sprintf('%s%ss%s_psychCurve%s.mat',exp.Spath,filesep,exp.sNstr,datestr(now,'ddmmyy')))
    load(sprintf('%s%ss%s_psychCurve%s.mat',exp.Spath,filesep,exp.sNstr,datestr(now,'ddmmyy')),'q');
    display(sprintf('\nData exists, %d trials already done %s',q(1).trialCount+q(2).trialCount))
else
    display(sprintf('\nData does not exists, \ncreating new Quest structure'))
    q           = QuestCreate(exp.PC.tGuess,exp.PC.tGuessSd,exp.PC.pThreshold,exp.PC.beta,exp.PC.delta,exp.PC.gamma);
    q(2)        = q;
    save(sprintf('%s%ss%s_psychCurve%s.mat',exp.Spath,filesep,exp.sNstr,datestr(now,'ddmmyy')),'q');
end

%%
contPsych = 1;
while contPsych
    display(sprintf('\n%d trials already done %s',q(1).trialCount+q(2).trialCount))
    sCont = input('\nStart/Continue adquiring Psycometric curve data (y/n)','s');
    if strcmp(sCont,'y')
        nTrials = str2num(input('\nHow many stimuli/trials (e.g. 150)','s'));
    else
        display(sprintf('\nStopping then ...',q.trialCount))
        contPsych = 0;
        continue
    end
   randSOA     = .6+rand(nTrials,1);
   side        = randsample([1,2],nTrials,'true');
   response    = 0;          
   if ~isfield(q,'rt')
       q(1).rt = [];
       q(2).rt = [];
   end
    for t= 1:nTrials
         % we define next trial intensity according to Quest or from a
        % uniform distribution between -2sd and +2sd the threshold value
        if rand(1)<.8
            tIntensity  = QuestQuantile(q(side(t)));
        else
%             tIntensity  = 10.^(QuestMode(q)) + (10.^(QuestMode(q)+3.*QuestSd(q))-10.^(QuestMode(q))).*(2*rand(1)-1);
            tIntensity  =   -1.4 + (-.7-(-1.4)).*rand(1)
        end
        
        PsychPortAudio('FillBuffer', pahandle, wave.tact.*10.^tIntensity);             % this takes less than 1 ms
        display(sprintf('Stimulus %d Side %d Intensity %1.4f',t,side(t),10.^tIntensity))
    
        PsychPortAudio('Start', pahandle, 0,0,0);    % repeats infitnely, starts as soon as posible, and continues with code inmediatly (we are contrling the stimulation with the parallel port so it does not matter)
        WaitSecs(randSOA(t));
        putvalue(DIO.line(1:8),dec2binvec(side(t),8));     % 
        lastStim      = GetSecs;    
        WaitSecs(exp.sound.tactile_dur);
        putvalue(DIO.line(1:8),dec2binvec(0,8));     % stimulation channel is on during the complete trial
        PsychPortAudio('Stop', pahandle);
     
        while GetSecs-lastStim<1
            outVal = getvalue(DIO.Line(9:10));
            if sum(outVal(1:2))
                q(side(t)).rt = [q(side(t)).rt,GetSecs-lastStim];
                if outVal(1) == 0
                    response = 1;
                    break
                elseif outVal(2) == 0
                    response = 2;
                    break
                end
            end
        end
        if response == 0;
            q(side(t)).rt = [q(side(t)).rt,NaN];
        end
        if (response == 1 && side(t) ==1) || (response == 2 && side(t) ==2) 
            q(side(t))=QuestUpdate(q(side(t)),tIntensity,1); 
            display(sprintf('Button %d pressed %4.3f seconds, correct\n',response,q(side(t)).rt(end)))
            
        else
             display(sprintf('Button %d pressed %4.3f seconds, incorrect\n',response,q(side(t)).rt(end)))
             q(side(t))=QuestUpdate(q(side(t)),tIntensity,0); 
        end
        response = 0;
    end
    save(sprintf('%s%ss%s_psychCurve%s.mat',exp.Spath,filesep,exp.sNstr,datestr(now,'ddmmyy')),'q');
    display(sprintf('\nLeft Intensity threshold (mean): %1.4f\nIntensity sd: %1.4f',10.^QuestMean(q(1)),10.^(QuestMean(q(1))+QuestSd(q(1)))-10.^QuestMean(q(1))))
    display(sprintf('\nRight Intensity threshold (mean): %1.4f\nIntensity sd: %1.4f',10.^QuestMean(q(2)),10.^(QuestMean(q(2))+QuestSd(q(2)))-10.^QuestMean(q(2))))
end

%%
 display(sprintf('\nLeft Intensity threshold (mean): %1.4f\nIntensity sd: %1.4f',10.^QuestMean(q(1)),10.^(QuestMean(q(1))+QuestSd(q(1)))-10.^QuestMean(q(1))))
    display(sprintf('\nRight Intensity threshold (mean): %1.4f\nIntensity sd: %1.4f',10.^QuestMean(q(2)),10.^(QuestMean(q(2))+QuestSd(q(2)))-10.^QuestMean(q(2))))
E299_PsychCurve
%%
% intensities are threshold, and 3 and 6 dB of amplitude ratio (equal to
% sqrt(2) and 2 the intensity). With raw amplitude values, formula is 
% amplitued that gives xdB from reference amplitude is
% 10.^(x/20+log10(ref)) and with already log transformed intensities is 
% 10.^(x/20+ref)


if strcmp(exp.sTtyp,'singleLH') 
    exp.intensitites = [(3/20+QuestMean(q(1))) (6/20+QuestMean(q(1))) (9/20+QuestMean(q(1)));
                    (3/20+QuestMean(q(2))) (6/20+QuestMean(q(2))) (9/20+QuestMean(q(1)))];
elseif strcmp(exp.sTtyp,'LH2cross')  || strcmp(exp.sTtyp,'LH2crossHpos') || strcmp(exp.sTtyp,'LH2crossAnti')
    % 20.04.17, change to ~3.5 and ~14 dB (1.5 and 5 times intensity, log10(1.5)*20)
    % before it was 3 and 15 (to be used -+ .5 dB)
    % 03.05.17, change to ~6 and ~15.5 dB (2 and 6 times intensity, log10(2)*20)
    exp.intensitites = [(log10(2)+QuestMean(q(1))) (log10(6)+QuestMean(q(1)));
                    (log10(2)+QuestMean(q(2))) (log10(6)+QuestMean(q(2)))];
elseif strcmp(exp.sTtyp,'Mask')            % only one intensity 6dB from thresho;d
    exp.intensitites = [(log10(1)+QuestMean(q(1))) ;
                    (log10(1)+QuestMean(q(2)))];
end
exp.psych_curve = 1;