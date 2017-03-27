%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E299_master
%
% This script controls the E282b experiment flexibly so it can be re-started
% at will, with different parameter, starting or continue with an optotrack
% session.
%
% JPO, Hamburg, 03.2017
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitStr                = regexp(fileparts(which('E282b_master')),...        % this tells us the path to the experiment in any computer
                            ['code'],'split');          
Ppath                   = splitStr{1}; clear splitStr

[exp,result,next_trial,sTtyp] = E299_initialize_subject(Ppath);                   % initialize ore recovers subject folder, settings and result structure

if ~psych_curveDone
    E299_psych
end
%%
E299_initHardware

% EXPERIMENTAL LOOP
while next_block
    if result.block_crossed(next_block)==0 && result.blockType(next_block)==0
    	display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',result.trial_block(next_trial)))
    elseif result.block_crossed(next_block)==0 && result.blockType(next_block)==1
        display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',result.trial_block(next_trial)))
    elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==0
    	display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',result.trial_block(next_trial)))
    elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==1
        display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',result.trial_block(next_trial)))
    end
    aux_inp = input(sprintf('\nContinue (c) or stop (s) the experiment: '),'s');
   
    if strcmp(aux_inp,'c')
        E299_block
    elseif strcmp(aux_inp,'s')
        display(sprintf('\n\nEXPERIMENT INTERRUPTED\n\n'))
        break
    end
    next_block = next_block+1;
end    

%%
PsychPortAudio('Close');
disp(sprintf('\nend of experiment'))

