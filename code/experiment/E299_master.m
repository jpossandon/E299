%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E299_master
%
% This script controls the E282b experiment flexibly so it can be re-started
% at will, with different parameter, starting or continue with an optotrack
% session.
%
% JPO, Hamburg, 03.2017
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitStr                = regexp(fileparts(which('E299_master')),...        % this tells us the path to the experiment in any computer
                            ['code'],'split');          
Ppath                   = splitStr{1}; clear splitStr

[exp,result,next_block] = E299_initialize_subject(Ppath);                   % initialize ore recovers subject folder, settings and result structure

E299_initHardware

% if ~exp.psych_curve
if strcmp(exp.sTtyp,'singleLH')
    exp = E299_psych(exp,pahandle,DIO,wave);
end
% end
%%


% EXPERIMENTAL LOOP
while next_block
    if strcmp(exp.sTtyp,'singleLH')
        if result.block_crossed(next_block)==0 && result.blockType(next_block)==1
            display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
        elseif result.block_crossed(next_block)==0 && result.blockType(next_block)==2
            display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
        elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==1
            display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
        elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==2
            display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
        end
    elseif strcmp(exp.sTtyp,'handEye')
        if result.block_crossed(next_block)==0 && result.blockType(next_block)==1
            display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES OPEN**\n',sum(result.block_done)+1))
        elseif result.block_crossed(next_block)==0 && result.blockType(next_block)==2
            display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES CLOSED**\n',sum(result.block_done)+1))
        elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==1
            display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES OPEN**\n',sum(result.block_done)+1))
        elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==2
            display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES CLOSED**\n',sum(result.block_done)+1))
        end
    end
    aux_inp = input(sprintf('\nContinue (c) or stop (s) the experiment: '),'s');
   
    if strcmp(aux_inp,'c')
        if strcmp(exp.sTtyp,'singleLH')
            E299_block
        elseif strcmp(exp.sTtyp,'handEye')
            E299_blockHE
        end
    elseif strcmp(aux_inp,'s')
        display(sprintf('\n\nEXPERIMENT INTERRUPTED\n\n'))
        break
    else
        error(sprintf('\nYou did not anser ''c'' or ''s''\nFinishing program\n'))
    end
    next_block = next_block+1;
end    

%%
PsychPortAudio('Close');
disp(sprintf('\nend of experiment'))

