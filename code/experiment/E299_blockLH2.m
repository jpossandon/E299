%E299_block

% contPsych = 1;
%     display(sprintf('\n%d trials already done %s',q.trialCount))
%     sCont = input('\n Continue with next block (y/n)','s');
nTrials      = result.t_perBlock(next_block);
next_trial   = sum(result.t_perBlock(1:next_block-1))+1;
auxtint      = repmat([1 1 2 2],1,nTrials/4);
auxtlimb     = repmat([1 2],1,nTrials/2);
aunxrnd      = randperm(nTrials);
result.trial_int(next_trial:next_trial+nTrials-1)           = auxtint(aunxrnd);
result.trial_randSOA(next_trial:next_trial+nTrials-1)       = exp.soa_fix+rand(1,nTrials);   
result.trial_limbside(next_trial:next_trial+nTrials-1)      = auxtlimb(aunxrnd);
result.trial_blockType(next_trial:next_trial+nTrials-1)     = repmat(result.blockType(next_block),1,nTrials);                                         % 0 - uncrossed ; 1 - crossed
result.trial_crossed_legs(next_trial:next_trial+nTrials-1)  = repmat(result.block_crossed_legs(next_block),1,nTrials);                                         % 0 - uncrossed ; 1 - crossed
result.trial_crossed_hand(next_trial:next_trial+nTrials-1)  = repmat(result.block_crossed_hands(next_block),1,nTrials);  
  

for t= next_trial:next_trial+nTrials-1
    response    = 0; 
    side        = result.trial_limbside(t); %1 left 2 right (anatomical)
    result.trial_actualIntensity(t) = 10.^((rand(1)-.5)/20+exp.intensitites(side,result.trial_int(t)));

    % change to variable intensity so participants cannot associate a
    % specific intensity sensation with a response, the calculated
    % intensities of 1.5 and 5 times the threhsold are taken from an uniform
    % distribution around the level -+.5 dB
    
    PsychPortAudio('FillBuffer', pahandle, wave.tact.*result.trial_actualIntensity(t));             % this takes less than 1 ms
    display(sprintf('Stimulus %d Intensity %1.3f',t,result.trial_actualIntensity(t)))     
    
    PsychPortAudio('Start', pahandle, 0,0,0);    % repeats infitnely, starts as soon as posible, and continues with code inmediatly (we are contrling the stimulation with the parallel port so it does not matter)
    WaitSecs(result.trial_randSOA(t));
    putvalue(DIO.line(1:2),dec2binvec(side,2));     % 
    lastStim      = GetSecs;    
    WaitSecs(exp.sound.tactile_dur);
    putvalue(DIO.line(1:2),dec2binvec(0,2));     % stimulation channel is on during the complete trial
    PsychPortAudio('Stop', pahandle);
     
    while GetSecs-lastStim<exp.maxRT
        outVal = getvalue(DIO.Line(9:10));
        if outVal(1) == 0
            result.trial_RT(t) = GetSecs-lastStim;
            response = 1;  % in anatomical coordinates 1- left 2- right
             display(sprintf('Button Left pressed %4.3f seconds',GetSecs-lastStim))
            break
        elseif outVal(2) == 0
            result.trial_RT(t) = GetSecs-lastStim;
            response = 2;
             display(sprintf('Button Right pressed %4.3f seconds',GetSecs-lastStim))
            break
        end
    end
    
    result.trial_response(t) = response;
    if response==0
        result.trial_RT(t) = NaN;
        result.trial_correct(t) = NaN;
    else
      % ressult response coding
      % blockType 1 - answer external 2- answer anatomical
      % limbside (anatomical) 1 - left 2 - right
      % crossed_leg 0 - uncrossed 1 - crossed
      % crossed_hand 0 - uncrossed 1 - crossed
      % response (external) 1- left 2- right
      if (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 1 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 2 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 1 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 2 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 1 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 2 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 1 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 0 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 2 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 1 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 2 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 1 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 0 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 2 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 1 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 1 && result.trial_blockType(t) == 2 && result.trial_response(t) == 2) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 1 && result.trial_response(t) == 1) || ...
         (result.trial_crossed_legs(t) == 1 && result.trial_crossed_hand(t) == 1 && result.trial_limbside(t) == 2 && result.trial_blockType(t) == 2 && result.trial_response(t) == 1)
         
         result.trial_correct(t)      = 1;
         display(sprintf('Correct\n'))
      else 
         result.trial_correct(t)      = 0;
         display(sprintf('Incorrect\n'))
       end
    end
end
result.block_done(next_block) = 1;
result.block_session{next_block} = datestr(now,'ddmmyy');

save(sprintf('%s%ss%s_%s_results.mat',exp.Spath,filesep,exp.sNstr,exp.sTtyp),'result','-append')
 