function [exp,result,next_trial,sTtyp] = E299_initialize_subject(Ppath)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function E299_initialize_subject(Ppath)
%
% JPO, Hamburg, 26.03.17
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sNstr           = input('\nSubject number: ','s');
sTtyp           = 'legStim_handResp';
Spath           = sprintf('%sdata%ss%s_%s',Ppath,filesep,sNstr,sTtyp);              % path to subject data folder

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
    display(sprintf('\n\nNew subject s%s,\n creating subject settings, result files and folder structure ...\n',sNstr,sTtyp))
    mkdir(sprintf('%sdata%ss%s_%s',Ppath,filesep,sNstr,sTtyp))
    if strcmp(sTtyp,'legStim_handResp')
        exp.nBlocks             = 96;                                                   % total number of blocks
        exp.nTrials_perBlock    = 100; % trials per block can be flexible adjusted so no all blocks have the same amount of trials (e.g. shorter test block)
        exp.maxRT               = 1.5;
        exp.soa_fix             = .5;
    else
        error(sprintf('Task %s does not exist',sTyp))
    end
    exp.sNstr       = sNstr;
    exp.sTtyp       = sTtyp;
    exp.created     = datestr(now);
    exp.Spath       = Spath;
    save(sprintf('%s%ss%s_%s_settings.mat',Spath,filesep,sNstr,sTtyp),'exp');
    create_result   = 1;
    psych_curveDone = 0;
end

if cont_flag
   display(sprintf('\nLoading setting for s%s, checking PsychCurve and result file,',sNstr))
   load(sprintf('%s%ss%s_%s_settings.mat',Spath,filesep,sNstr,sTtyp),'exp');
   display(sprintf(' previous setting file was created on the %s',exp.created))
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
      save(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'result')
   end
end

if create_result == 1                                                       % create new result file
    result.block_done         = zeros(1,exp.nBlocks);  
    result.block_crossed      = repmat(randsample([1 0],2),...              % 0 - uncrossed ; 1 - crossed 
                                1,exp.nBlocks/2);
    result.blockType          = repmat([1 1 1 1 1 1 2 2 2 2 2 2],...              % 1 - answer external 2- answer anatomical
                                 1,exp.nBlocks/12);    
    result.t_perBlock         = repmat(exp.nTrials_perBlock,1,exp.nBlocks);

    %this are filled every block/trial
    result.trial_RT           = [];
    result.trial_blockType    = [];                                         % corresponding to block.blockType  
    result.trial_crossed      = [];                                         % 0 - uncrossed ; 1 - crossed
    result.trial_response     = [];                                         % 0 - no response; 1 - left ; 2 - right (external)
    result.trial_limbside     = [];                                         % 0 - left ; 1 - right (anatomical
    result.trial_int          = [];                                         % trial intensity (1 - threshold; 2 - threhsold +sd; 3 -  threhsold +sd)
    result.trial_randSOA      = [];
    result.correct            = [];                                         % NaN - no response, 0 - incorrect, 1 correct
    result.created            = datestr(now);
    save(sprintf('%s%ss%s_%s_results.mat',Spath,filesep,sNstr,sTtyp),'result')
    next_block = 1;
end
