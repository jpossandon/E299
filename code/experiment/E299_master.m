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
if strcmp(exp.sTtyp,'singleLH') || strcmp(exp.sTtyp,'LH2cross') || strcmp(exp.sTtyp,'LH2crossHpos')
    exp = E299_psych(exp,pahandle,DIO,wave);
end
% end
%%


% EXPERIMENTAL LOOP
while next_block
    E299_instructions
    
    aux_inp = input(sprintf('\nContinue (c) or stop (s) the experiment: '),'s');
   
    if strcmp(aux_inp,'c')
        if strcmp(exp.sTtyp,'singleLH')
            E299_block
        elseif strcmp(exp.sTtyp,'handEye')
            E299_blockHE
        elseif strcmp(exp.sTtyp,'LH2cross')
            E299_blockLH2
        elseif strcmp(exp.sTtyp,'LH2crossHpos')
            E299_blockLH2Hpos
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

