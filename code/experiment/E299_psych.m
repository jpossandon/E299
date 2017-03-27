%%
% first step get threshold
% add opening of specs, move the parameters of the curve to exp structure
% and the results of the theshold and sd
display(sprintf('\nSearching psych data for subject %s',sNstr))
if exist(sprintf('%s%ss%s_psychCurve.mat',Spath,filesep,sNstr))
    load(sprintf('%s%ss%s_psychCurve.mat',Spath,filesep,sNstr),'q');
    display(sprintf('\nData exists, %d trials already done %s',q.trialCount))
else
    display(sprintf('\nData does not exists, \ncreating new Quest structure'))
    exp.PC.tGuess      = -1.2;
    exp.PC.tGuessSd    = 1;
    exp.PC.pThreshold  = 0.82;
    exp.PC.beta        = 3.5;
    exp.PC.delta       = 0.02;
    exp.PC.gamma       = 0.5;
    q           = QuestCreate(tGuess,tGuessSd,pThreshold,beta,delta,gamma);
    save(sprintf('%s%ss%s_psychCurve.mat',Spath,filesep,sNstr),'q');
end

%%
contPsych = 1;
while contPsych
    display(sprintf('\n%d trials already done %s',q.trialCount))
    sCont = input('\nStart/Continue adquiring Psycometric curve data (y/n)','s');
    if strcmp(sCont,'y')
        nTrials = input('\nHow many stimuli/trials (e.g. 150)','d');
    else
        display(sprintf('\nStopping then ...',q.trialCount))
        contPsych = 1;
        continue
    end
    tIntensity  = QuestQuantile(q);
    randSOA     = .5+rand(nTrials,1);   
    response    = 0;          
    
    for t= 1:nTrials
        if t ==1
            lastStim      = GetSecs;                     
            side          = 1;
        end
        PsychPortAudio('FillBuffer', pahandle, wave.tact.*10.^tIntensity);             % this takes less than 1 ms
        display(sprintf('Stimulus %d Intensity %1.3f',t,10.^tIntensity))
    
        PsychPortAudio('Start', pahandle, 0,0,0);    % repeats infitnely, starts as soon as posible, and continues with code inmediatly (we are contrling the stimulation with the parallel port so it does not matter)
        WaitSecs(randSOA(t));
        putvalue(DIO.line(1:8),dec2binvec(side,8));     % 
        lastStim      = GetSecs;    
        WaitSecs(exp.sound.tactile_dur);
        putvalue(DIO.line(1:8),dec2binvec(0,8));     % stimulation channel is on during the complete trial
        PsychPortAudio('Stop', pahandle);
     
        while GetSecs-lastStim<1
            outVal = getvalue(DIO.Line(9:10));
            if outVal(1) == 0
                response = 1;
                display(sprintf('Button 1 pressed %4.3f seconds',GetSecs-lastStim))
                break
            end
        end
        if response == 1
            q=QuestUpdate(q,tIntensity,1); 
            response = 0;
        else
             q=QuestUpdate(q,tIntensity,0); 
        end
        save(sprintf('%s%ss%s_psychCurve.mat',Spath,filesep,sNstr),'q');
        
        % we define next trial intensity according to Quest or from a
        % uniform distribution between -2sd and +2sd the threshold value
        if rand(1)>.9
            tIntensity  = QuestQuantile(q);
        else
            tIntensity  = 10.^(QuestMode(q)-1.5*QuestSd(q)) + (10.^(QuestMode(q)+1.5.*QuestSd(q))-10.^(QuestMode(q)-1.5*QuestSd(q))).*rand(1);
        end
    end
end

%%
display(sprintf('\nIntensity threhsold: %1.3f\nIntensity sd: %1.3f',10.^QuestMode(q),10.^QuestSd(q)))