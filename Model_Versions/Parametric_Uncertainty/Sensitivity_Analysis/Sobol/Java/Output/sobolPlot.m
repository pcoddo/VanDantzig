function [] = sobolplot(input, output, format)
%SOBOLPLOT Plots first, total and second-order Sobol sensitivities
%
%   SOBOLPLOT(INPUT)
%   Produces a plot showing the first, total, and second-order sensitivities
%   read from the specified INPUT file.
%
%   SOBOLPLOT(INPUT, OUTPUT, FORMAT)
%   Similar to SOBOLPLOT(INPUT), but the resulting figure is saved to the
%   specified OUTPUT file.  The optional FORMAT option controls the format
%   of the resulting image file.  See 'HELP SAVEAS' for a list of valid
%   FORMAT options.
      
    nodeScaling = 100;
    lineScaling = 75;

    nparams = 0;
    fid = fopen(input);
    mode = 0;
    firstLines = {};
    totalLines = {};
    secondLines = {};

    % read the contents of the input file
    line = fgetl(fid);
    while ischar(line)
        if (strcmpi('First-Order Effects', line))
            mode = 1;
            nparams = 0;
        elseif (strcmpi('Total-Order Effects', line))
            mode = 2;
            nparams = 0;
        elseif (strcmpi('Second-Order Effects', line))
            mode = 3;
            nparams = 0;
        else
             nparams = nparams + 1;

             if (mode == 1)
                 firstLines{nparams} = line;
             elseif (mode == 2) 
                 totalLines{nparams} = line;
             elseif (mode == 3)
                 secondLines{nparams} = line;
             end
        end
        line = fgetl(fid);
    end
    
    fclose(fid);
    params = getParameters(firstLines);
    nparams = length(params);
    
    % define the locations of nodes
    nodes = zeros(nparams, 2);
    angle = 2*pi/nparams;

    for i=1:nparams
        nodes(i,:) = [cos((i-1)*angle), sin((i-1)*angle)];
    end

    % create the figure
    figure;
    hold on;
   
    % plot the second-order sensitivities as lines 
    for i=1:length(params)-1
        for j=i+1:length(params)
            value = max(0.01, getSecondOrder(params{i}, params{j}, secondLines));
            
            if (value > 0.01)
               plot([nodes(i,1), nodes(j,1)], [nodes(i,2), nodes(j,2)], '-', ...
                    'Color', [0 0 .5], 'LineWidth', lineScaling*value);
            end
        end
    end

    % plot the total-order sensitivities as rings
    for i=1:length(params)
        value = max(0.01, getFirstOrder(params{i}, totalLines));
        
        plot(nodes(i,1), nodes(i,2), 'Marker', 'o',...
            'MarkerEdgeColor', [.5 0 0], 'MarkerFaceColor', 'none',...
            'LineWidth', 2, 'MarkerSize', nodeScaling*value); 
    end
   
    % plot the first-order sensitivities as filled circles 
    for i=1:length(params)
        value = max(0.01, getFirstOrder(params{i}, firstLines));
        
        plot(nodes(i,1), nodes(i,2), 'Marker', 'o',...
            'MarkerEdgeColor', [.5 0 0], 'MarkerFaceColor', [.5 0 0],...
            'MarkerSize', nodeScaling*value); 
        
        text(nodes(i,1), nodes(i,2), params{i});
    end

    axis square;
    set(gca, 'visible', 'off');

    if (nargin == 2)
        saveas(gcf, output);
    elseif (nargin == 3)
        saveas(gcf, output, format);
    end
end

function [parameters] = getParameters(lines)
    for i=1:length(lines)
         [A,B,C,D,E,F] = regexpi(lines{i}, strcat('^\s*(.*)\s+-?[0-9]+\.[0-9]+(?:[eE]-?[0-9]+)?\s+\[[0-9]+\.[0-9]+(?:[eE]-?[0-9]+)?\]$'));
         
         if (length(E) == 1)
             parameters{i} = char(E{1});
         end
    end
end

function [value] = getFirstOrder(parameter, lines)
    for i=1:length(lines)
         [A,B,C,D,E,F] = regexpi(lines{i}, strcat('^\s*', parameter, '\s+(-?[0-9]+\.[0-9]+(?:[eE]-?[0-9]+)?)\s+\[[0-9]+\.[0-9]+(?:[eE]-?[0-9]+)?\]$'));
         
         if (length(E) == 1)
             value = str2double(E{1});
             return;
         end
    end
end

function [value] = getSecondOrder(parameter1, parameter2, lines)
    for i=1:length(lines)
         [A,B,C,D,E,F] = regexpi(lines{i}, strcat('^\s*', parameter1, '\s\*\s', parameter2, '\s+(-?[0-9]+\.[0-9]+(?:[eE]-?[0-9]+)?)\s+\[[0-9]+\.[0-9]+(?:[eE]-?[0-9]+)?\]$'));
         
         if (length(E) == 1)
             value = str2double(E{1});
             return;
         end
         
         [A,B,C,D,E,F] = regexpi(lines{i}, strcat('^\s*', parameter2, '\s\*\s', parameter1, '\s+(-?[0-9]+\.[0-9]+)\s+\[[0-9]+(?:[eE]-?[0-9]+)?\.[0-9]+(?:[eE]-?[0-9]+)?\]$'));
         
         if (length(E) == 1)
            value = str2double(E{1});
            return;
         end
    end
end
