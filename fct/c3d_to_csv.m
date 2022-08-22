%% Code to convert .c3d files into .csv
%
% Author: Mateus Souza Silva
% Date: 18/08/2022

folder = strcat(cd, filesep, 'data', filesep, 'raw', filesep, 'c3d', filesep);             % Folder with the database
files_list = dir(fullfile(folder, '**/*.c3d*'));                           % Lists all c3d files in the folder
number_files = length(files_list); 

btk_markers = ["RHMC2"; "RUSP"; "LHMC2"; "LUSP"];                          % Selected markers, if [] it will take all 

for file = 1 : number_files                                                % Loop for each file of the database
    file_path = [files_list(file).folder filesep files_list(file).name];
    [~, file_name, ~] = fileparts(file_path);

    btk_acq = btkReadAcquisition(file_path);                               % Functions of btk package to read .c3d data
    btk_data = btkGetMarkers(btk_acq);
    
    if isempty(btk_markers)                                                % Selecting all markers if you have not selected any
        btk_markers = fieldnames(btk_data);
    end
    
    btk_unit = btkGetPointsUnit(btk_acq, 'marker');                        % Verify the unit of space used in the data (m, cm, mm)
    

    data_len = size(btk_data.(string(btk_markers(1))), 1);                 % Length of the time series of this file
    
    output = table('size', [data_len*length(btk_markers) 5], ...           % Creating the output table to concatenate the data of each marker of this file
        'VariableTypes', {'string', 'double', 'double', 'double', 'double'}, ...
        'VariableNames', {'marker', 't', 'x', 'y', 'z'});
    
    
    sample_rate = btkGetPointFrequency(btk_acq);
    t = (1 : data_len) / sample_rate; t = t';
    output.t = repmat(t, length(btk_markers), 1);                          % Filling the time column in the output table
    
    for i_mrk = 1 : length(btk_markers)                                    % Loop for each marker used
        r = btk_data.(string(btk_markers(i_mrk)));                         % r = [x, y, z] time series
        if isequal(btk_unit, 'mm')                                         % Changing the unit to meters
            r = r / 1000;
        elseif isequal(btk_unit, 'cm')
            r = r / 100; 
        end
        
        output.marker(1 + data_len*(i_mrk - 1) : data_len*i_mrk) = ...     % Filling the output table of this file
            repmat(string(btk_markers(i_mrk)), data_len, 1);
        output.x(1 + data_len*(i_mrk - 1) : data_len*i_mrk) = r(:, 1);
        output.y(1 + data_len*(i_mrk - 1) : data_len*i_mrk) = r(:, 2);
        output.z(1 + data_len*(i_mrk - 1) : data_len*i_mrk) = r(:, 3);
    end
   

    output_folder = [cd filesep 'data' filesep 'raw' filesep 'csv' filesep file_name(1:5) filesep];
    
    if not(isfolder(output_folder))                                        % Creating the output folder if it does not exist
        mkdir(output_folder)
    end
    
    writetable(output, [output_folder file_name '.csv'], 'Delimiter', ',');% Writing the .csv file
end
