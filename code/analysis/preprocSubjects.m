%%
% data managment
% adds every subject data to a common structure, and creates a
% corresponding csv file
% creates single and all subjects figures
clear
forceNew        = 0;                        % to creates everything anew
forceaddSubject = 0;                        % replace data of addSubject in alldata 
addSubject      = [4];                  % subjects to process
exppath         = '/Users/jossando/trabajo/E299/';
datapath        = fullfile(exppath,'data','LH2cross');
alldataMatFile  = fullfile(datapath,'allSubject.mat');
alldataCSVFile  = fullfile(datapath,'allSubject.csv');
pathFigures     = fullfile(exppath,'figures','LH2cross');

if forceNew
    allData     = struct('subjIndx',[],'trial_RT',[],'trial_blockType',[],'trial_response',[],...
                'trial_limbside',[],'trial_randSOA',[],'trial_correct',[],...
                'trial_int',[],'trial_actualIntensity',[],'trial_crossed_legs',[],...
                'trial_crossed_hand',[]);
else
    load(alldataMatFile);
end

for subj = addSubject
    subjResultFile          = [datapath filesep 's' num2str(subj) '_LH2cross' filesep 's' num2str(subj) '_LH2cross_results'];
    load(subjResultFile)
    E299_simplePlotsLH2cross
    redux_result            = rmfield(result,{'block_done','t_perBlock','block_session',...
                            'trial_crossed','created','block_crossed_legs','block_crossed_hands','blockType'});
    redux_result.subjIndx   = subj*ones(1,length(redux_result.trial_RT)); 
    isSubjinallData         = any(allData.subjIndx==subj);
    if isSubjinallData && ~forceaddSubject
        error('Subject %d already added to allData structure and forceaddSubject = 0',subj)
    else
        if isSubjinallData && forceaddSubject
            sprintf('Subject %d data replaced in allData structure',subj)
            allData = struct_elim(allData,find(allData.subjIndx==subj),2,1);
        elseif ~isSubjinallData && forceaddSubject
            sprintf('Subject %d is not in allData structure and forceaddSubject = 0\n adding anyways',subj)
        elseif ~isSubjinallData && ~forceaddSubject
            sprintf('Adding subject %d to allData structure',subj)
        end
        allData                 = struct_cat(allData,redux_result,2);
    end
end
createdon = datestr(now);
save(alldataMatFile,'allData','createdon')
fallData = fields(allData);
for f = 1:length(fallData)
    allDatat.(fallData{f}) = allData.(fallData{f})';
end
struct2csv(allDatat,alldataCSVFile)