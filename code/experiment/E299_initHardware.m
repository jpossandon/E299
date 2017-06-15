%%
% Connect to the nidaq card, open lines for parallel output (control of
% tactile stimulation; port 0 - 8 bits) and digital input (button boxes;
% port 9 - last two bits

display(sprintf('\nSetting-up digital I/O card'))
if verLessThan('matlab', 'R2016a')
    DIO    = digitalio('nidaq','Dev1');                                        % control the DI/O nidaq card PCI 6509  
    lines  = addline(DIO,0:7,0,'Out');                                         % Output thorugh port 0 
    lines  = addline(DIO,6:7,9,'In');                                          %Input thorugh port 9, for button press 
    putvalue(DIO.line(1:8),0)                                                  % flush it   
else
    s = daq.createSession('ni');
    % read and write happens with s.inputSingleScan and s.OutputSingleScan 
    % for this to work without oppen 4 different sessions the output value
    % must include the information for the 3 ports in somthing like 
    % [tactors(2bit) high-byte(trigger,1-bit) low-byte(info,8-bit)]
    addDigitalChannel(s,'Dev2','Port9/Line6:7','InputOnly')                 % button port
    addDigitalChannel(s,'Dev2','Port0/Line0:1','OutputOnly')                % tactors port 
    addDigitalChannel(s,'Dev2','Port1/Line7','OutputOnly')                  % digital output to CED high-byte
    addDigitalChannel(s,'Dev2','Port2/Line0:7','OutputOnly')                % digital output to CED low-byte
    addDigitalChannel(s,'Dev2','Port3/Line4','OutputOnly')                  % data available input
    tactbits   = [0 0];
    trigbit    = [1];
    inputByte  = [0 0 0 0 0 0 0 0];
    dataAvbit  = [0];
    s.outputSingleScan([tactbits,trigbit,inputByte,dataAvbit])
end

display('\nDone.')

%%
% Open Audio ports and create the stimulus
display(sprintf('\nSetting-up audio ports and stimuli'))

exp.sound.fs                = 48000;                                        % sound sampling rate
exp.sound.nrchannels        = 2;                                            % stereo not really necessary here
InitializePsychSound;                                                       % Perform basic initialization of the sound driver:

try                                                                         % Try with the 'freq'uency we wanted:
    pahandle = PsychPortAudio('Open', [], [], 0, exp.sound.fs, exp.sound.nrchannels);
catch                                                                       % Failed. Retry with default frequency as suggested by device:
    fprintf('\nCould not open device at wanted playback frequency of %i Hz. Will retry with device default frequency.\n', exp.sound.fs);
    fprintf('Sound may sound a bit out of tune, ...\n\n');
    psychlasterror('reset');
    pahandle = PsychPortAudio('Open', [], [], 0, [], exp.sound.nrchannels);
end

exp.sound.tactile_dur = .025;  
exp.sound.tact_freq   = 200;
wave.tact             = sin(2.*pi.*exp.sound.tact_freq.*...                 % the tactile stimulus
                         [0:1/exp.sound.fs:exp.sound.tactile_dur-1/exp.sound.fs]);      
wave.tact(2,:)        = 0;                                                  % sound channel is stereo tactile stimulator is connected to 2nd channel, first channel can be used to give auditory feedback (not used here yet)
display(sprintf('\nDone.'))

input('\nPlease check that amplifier channels 1&2 are at intensity 7\n and that windows volume is at max\nPress Enter continue','s');
