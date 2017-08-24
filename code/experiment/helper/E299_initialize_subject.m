function [exp,result,next_block] = E299_initialize_subject(Ppath)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function E299_initialize_subject(Ppath)
%
% JPO, Hamburg, 26.03.17
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sNstr           = input('\nSubject number: ','s');
sTtyp           = input('\nTrial type (LH2cross;LH2crossAnti;Mask;LH2crossHpos;singleLH;handEye): ','s');

% sTtyp           = 'singleLH';
Spath           = sprintf('%sdata%s%s%ss%s_%s',Ppath,filesep,sTtyp,filesep,sNstr,sTtyp);              % path to subject data folder
% TODO redo folder structure to exp_type_folder/subject_files
% check wether subject folder exists and what does the experimenter want to do
if isdir(Spath)                        
    aux_inp1 = input(sprintf(...
        '\n\nSubject %s already exists, do you want to continue (c) or restart (r) a session: ',sNstr),'s');
    
    if strcmp(aux_inp1,'r')
        aux_inp2 = input(sprintf('\nAre you sure you want to erase subject data s%s (y/n) : ',sNstr),'s');
        if strcmp(aux_inp2,'y')                                             % erasing previous subject folder, we actually copy it first to .../data/removed
            display(sprintf('\n\nCopyng old data s%s to ...\\data\\removed folder...',sNstr))
            if ~isdir(sprintf('%sdata%sremoved%s',Ppath,filesep,filesep))
                mkdir(sprintf('%sdata%sremoved%s',Ppath,filesep,filesep));
            end
            [SUCCESS,MESSAGE,MESSAGEID] = copyfile(Spath,...
                sprintf('%sdata%sremoved%ss%s_%s_%s%s',Ppath,filesep,filesep,sNstr,sTtyp,datestr(now,'ddmmyy'),filesep));
            if ~SUCCESS
                error(MESSAGEID,MESSAGE)
            end
            rmdir(Spath,'s')
            restart_flag = 1;
            cont_flag    = 0;
        elseif strcmp(aux_inp2,'n')
            error(sprintf('\n ... then you have to start again.\nFinishing program\n'))
        else 
            error(sprintf('\nYou did not anser ''y'' or ''n''\nFinishing program\n'))
        end
    elseif strcmp(aux_inp1,'c')
        restart_flag = 0;
        cont_flag    = 1;
    else 
        error(sprintf('\nYou did not anser ''r'' or ''c''\nFinishing program\n'))
    end
else 
   restart_flag = 1;
   cont_flag    = 0;
end
    
if restart_flag                                                             % create folder an subject specific setting structure
    display(sprintf('\n\nNew subject s%s task %s,\n creating subject settings, result files and folder structure ...\n',sNstr,sTtyp))
    mkdir(sprintf('%sdata%s%s%ss%s_%s',Ppath,filesep,sTtyp,filesep,sNstr,sTtyp));
    if strcmp(sTtyp,'singleLH') || strcmp(sTtyp,'LH2cross') || strcmp(sTtyp,'LH2crossAnti')...
            || strcmp(sTtyp,'LH2crossHpos') || strcmp(sTtyp,'Mask')
        exp.nBlocks             = 96;                                                   % total number of blocks
        exp.nTrials_perBlock    = 100; % trials per block can be flexible adjusted so no all blocks have the same amount of trials (e.g. shorter test block), it has to be a number divisible bz 4
        exp.maxRT               = 2;
        exp.soa_fix             = 1;
        exp.PC.tGuess           = -1;
        exp.PC.tGuessSd         = 1;
        exp.PC.pThreshold       = 0.82;
        exp.PC.beta             = 3.5;
        exp.PC.delta            = 0.02;
        exp.PC.gamma            = 0.5;
        if strcmp(sTtyp,'LH2crossHpos')
            exp.nTrials_perBlock    = 60; % trials per block can be flexible adjusted so no all blocks have the same amount of trials (e.g. shorter test block)
        elseif strcmp(sTtyp,'Mask')
            exp.nTrials_perBlock    = 140;
            exp.SOA                 = [-.06,-.03,0,.03,.06];
        end
    elseif strcmp(sTtyp,'handEye')
        exp.nBlocks             = 96;                                                   % total number of blocks
        exp.nTrials_perBlock    = 100; % trials per block can be flexible adjusted so no all blocks have the same amount of trials (e.g. shorter test block)
        exp.maxRT               = 2;
        exp.soa_fix             = 1;
    else
        error(sprintf('Task %s does not exist',sTyp))
    end
    exp.sNstr       = sNstr;
    exp.sTtyp       = sTtyp;
    exp.created     = datestr(now);
    exp.Spath       = Spath;

    save(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'exp')
%     save(sprintf('%s%ss%s_%s_settings.mat',Spath,filesep,sNstr,sTtyp),'exp');
    create_result   = 1;
    exp.psych_curve = 0;
end

if cont_flag
   display(sprintf('\nLoading setting for s%s, checking PsychCurve and result file,',sNstr))
 try
   load(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'exp');
   display(sprintf(' previous setting file was created on the %s',exp.created))
 catch
   exp.nBlocks             = 96;                                                   % total number of blocks
        exp.nTrials_perBlock    = 100; % trials per block can be flexible adjusted so no all blocks have the same amount of trials (e.g. shorter test block)
        exp.maxRT               = 2;
        exp.soa_fix             = 1; 
   exp.sNstr       = sNstr;
    exp.sTtyp       = sTtyp;
    exp.created     = datestr(now);
    exp.Spath       = Spath;
    exp.PC.tGuess      = -1;
    exp.PC.tGuessSd    = 1;
    exp.PC.pThreshold  = 0.82;
    exp.PC.beta        = 3.5;
    exp.PC.delta       = 0.02;
    exp.PC.gamma       = 0.5;
     save(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'exp','-append')
 end
   A = exist(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'file'); 
   if A == 0 
      display(sprintf('\nResult file for s%s does not exist,\n creating a new one',sNstr))
      create_result     = 1;
   elseif A==2
      display(sprintf('\nResult file for s%s exists,',sNstr))
      create_result     = 0;
      load(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'result')
      last_valid        =find(result.block_done,1,'last');
      if ~isempty(last_valid)
          display(sprintf('\n%d blocks already done,',last_valid))
          next_block = last_valid+1;
      else
        display(sprintf('\nZero blocs have been performed,\nwe start then from Block #1 ...\n'))
        next_block = 1;
      end
      save(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'result','-append')
   end
end

if create_result == 1                                                       % create new result file
    result.block_done         = zeros(1,exp.nBlocks);  
    result.t_perBlock         = repmat(exp.nTrials_perBlock,1,exp.nBlocks);
    result.block_session      = cell(1,exp.nBlocks); 
    %this are filled every block/trial
    result.trial_RT           = [];
    result.trial_blockType    = [];                                         % corresponding to block.blockType  
%     result.trial_crossed      = [];                                         % 0 - uncrossed ; 1 - crossed
    result.trial_response     = [];                                         % 0 - no response; 1 - left ; 2 - right (external)
    result.trial_limbside     = [];        
    result.trial_randSOA      = [];
    result.trial_correct      = [];                                         % NaN - no response, 0 - incorrect, 1 correct
    result.created            = datestr(now);
    if strcmp(sTtyp,'singleLH')
       result.block_crossed      = repmat(randsample([1 0],2),...           % 0 - uncrossed ; 1 - crossed 
                                    1,exp.nBlocks/2);
       result.blockType          = repmat([1 1 1 1 1 1 2 2 2 2 2 2],...    % 1 - answer external 2- answer anatomical
                                     1,exp.nBlocks/12);    
                                        % 0 - left ; 1 - right (anatomical
       result.trial_int          = [];                                     % trial intensity (1 - threshold; 2 - threhsold +sd; 3 -  threhsold +sd)
        
    elseif strcmp(sTtyp,'handEye')
        result.block_crossed      = repmat(randsample([1 0],2),...          % 0 - uncrossed ; 1 - crossed 
                                    1,exp.nBlocks/2);    
        result.blockType          = repmat([1 1 2 2],...                    % 1 - ezes open 2- eyes closed
                                     1,exp.nBlocks/4);    
    elseif strcmp(sTtyp,'LH2cross') || strcmp(sTtyp,'LH2crossAnti')
        % to get the balancing of hand and leg block crossing, with four
        % different conditions that can be arranged (permuted) in 24
        % different way. For complete balance with response mode we need
        % double subjects (this is all overkill, since we are not going to
        % have hundreds of subject ...)
        pb              = [perms([3 2 1 0]);perms([0 1 2 3])];             % get (double) all permutations 
        sind            = mod(str2num(sNstr)-1,48)+1;                      % which of the block ordering subject sNstr gets, this formulation works for any subject number
        auxbin          = dec2bin(pb(sind,:));                             % this two lines transform it to the coding of block_crossed below
        auxbin          = str2num(auxbin(:));
        result.block_crossed_legs       = repmat(auxbin(1:4)',...          % 0 - uncrossed ; 1 - crossed 
                                            1,exp.nBlocks/4);
        result.block_crossed_hands      = repmat(auxbin(5:8)',...          % 0 - uncrossed ; 1 - crossed 
                                            1,exp.nBlocks/4);                        
        if mod(str2num(sNstr),2)        % even subjects start with external blocks  
           result.blockType             = repmat(reshape(repmat([1 2],12,1),[1,24]),...    % 1 - answer external 2- answer anatomical
                                            1,exp.nBlocks/24); 
        else                            % odd subjects start with external blocks
           result.blockType             = repmat(reshape(repmat([2 1],12,1),[1,24]),...    % 1 - answer external 2- answer anatomical
                                            1,exp.nBlocks/24); 
        end
        result.trial_int                = []; 
        result.trial_actualIntensity    = [];
    elseif strcmp(sTtyp,'LH2crossHpos')
        % to get the balancing of hand and leg block crossing, with four
        % different conditions that can be arranged (permuted) in 24
        % different way. For complete balance with response mode and hand position
        % (if only center and right) we need 4 times the amount of subjects
        % subjects (this is all overkill, since we are not going to
        % have hundreds of subject ...)
        pb              = [perms([3 2 1 0]);perms([0 1 2 3]);...
                            perms([3 2 1 0]);perms([0 1 2 3])];             % get (double) all permutations, #1-4 correspond to the 4 conditions of hand and leg crossing, each number is a 2digit binary number coding whether hand and legs are crossed
        sind            = mod(str2num(sNstr)-1,96)+1;                      % which of the block ordering subject sNstr gets, this formulation works for any subject number
        auxbin          = dec2bin(pb(sind,:));                             % this two lines transform it to the coding of block_crossed below
        auxbin          = str2num(auxbin(:));
        result.block_crossed_legs       = repmat(auxbin(1:4)',...          % 0 - uncrossed ; 1 - crossed 
                                            1,exp.nBlocks/4);
        result.block_crossed_hands      = repmat(auxbin(5:8)',...          % 0 - uncrossed ; 1 - crossed 
                                            1,exp.nBlocks/4);                        
        if mod(str2num(sNstr),2)        % even subjects start with external blocks  
           result.blockType             = repmat(reshape(repmat([1 2],24,1),[1,48]),...    % 1 - answer external 2- answer anatomical
                                            1,exp.nBlocks/48); 
        else                            % odd subjects start with anatomical blocks
           result.blockType             = repmat(reshape(repmat([2 1],24,1),[1,48]),...    % 1 - answer external 2- answer anatomical
                                            1,exp.nBlocks/48); 
        end
        
        handrandaux       =  repmat([1 1 0 0],1, 200);      % every two subjects changes the hand ordering
        if handrandaux(str2num(sNstr))
            result.block_handpos              = repmat([2 2 2 2 3 3 3 3],1, exp.nBlocks/8); %1 - left ; 2 - central ; 3 - right 
        else
            result.block_handpos              = repmat([3 3 3 3 2 2 2 2],1, exp.nBlocks/8);
        end
        
        result.trial_int                = []; 
        result.trial_actualIntensity    = [];
    elseif strcmp(sTtyp,'Mask')
        result.block_crossed            = repmat(randsample([1 0],2),...          % 0 - uncrossed ; 1 - crossed 
                                                1,exp.nBlocks/2);
        for exprep = 1:exp.nBlocks/12                                           % ugly but per every p(right) we get two repetitions of crossed and uncrossed conditions
            auxpp = [repmat([.3,.5,.7],1,4);repmat([0 0 0 1 1 1],1,2)]                                    
            for bperexp = 1:12
                if bperexp<11
                    ei = randsample(find(auxpp(2,:)==result.block_crossed(12*exprep-12+bperexp)),1);
                else
                    ei = find(auxpp(2,:)==result.block_crossed(12*exprep-12+bperexp));
                end
                result.block_Pright(12*exprep-12+bperexp) = auxpp(1,ei) ;
                auxpp(:,ei) = []
            end
        end
        result.trial_int                = []; 
        result.trial_actualIntensity    = [];
        result = rmfield(result,'trial_blockType');
    end
    save(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'result','-append')
    next_block = 1;
end
