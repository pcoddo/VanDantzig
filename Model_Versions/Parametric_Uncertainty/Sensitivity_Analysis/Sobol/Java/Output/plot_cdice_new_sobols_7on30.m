% plot_cdice_new_sobols_30on31.m  started on 23 August 2013
% revisions of many earlier attempts
%    This version uses updated read functions for the moeaframework 
%    sobol analysis output, assumes only 1 metric plot (which is
%    changed at the place marked), COULD read a parameters file
%    to determine the number of parameters (code commented out),

%    keeps only current plotting schemes for sizes of bubbles and
%    width & shading of lines. (would be improved if bubble sizes
%    were derived from areas not diameters....but this IS matlab

%    This version, in particular, plots the results from 30
%    parameter experiments on a 31 parameter plot (to eliminate
%    creating a new plot overlay).
%    No text is printed on this plot (see associated .eps overlay)

% 29 August 2013 plot_cdice_new_sobols_30on30.m
% in preparation for making a new overlay - this is 30 on 30 grid
%   (no empty gaps in the ring)

% 10 september 2013 plot_cdice_new_sobols_7on 30.m
%    

clear variables;
clear all;

% this has not been used recently; legend created in AI
%do_legend = 1;
do_legend = 0;

% change these for file to be read
% if changing number of parameters, see code for defining
% node positions below
fname = ('sobolIndices_reliability.txt');
nparams = 7;

% here we read the parameter ranges file to capture the names
% of the parameters  (really this is to set nparam)
% pfid = fopen('master_factor_ranges_30cs.txt');
% junk = textscan(pfid, '%s%f%f');
% param_names = junk{1};
% param_low = junk{2};
% param_high = junk{3};
% fclose(pfid);
% clear junk;
% nparams = length(param_names); %number of parameters

pfid = fopen(fname);
if (pfid == -1) disp('error opening file'); end

junk = fgetl(pfid); %skip "Parameter Sensitivity [Confidence]"
junk = fgetl(pfid); %skip "First-Order Effects"
first_order_cell = textscan(pfid, '%s%f%*c%*c%f%*c', nparams);
junk = textscan(pfid, '%s%s', 1); %skip "Total-Order Effects"
total_order_cell = textscan(pfid, '%s%f%*c%*c%f%*c', nparams);
junk = textscan(pfid, '%s%s', 1); %skip "Second-Order Effects"
second_order_cell = ...
        textscan(pfid, '%s%*s%s%f%*c%*c%f%*c', nchoosek(nparams,2));
fclose(pfid);


% key to Sobol data structure
% first and total order are similar, here is first order example
%  the char( ) is necessary to get the parameter name out of quotes
% for p = 1:nparams
%   char(first_order_cell{1}(p))  is the parameter name
%   first_order_cell{2}(p)    is the first order index
%   first_order_cell{3}(p)    is the confidence interval
% end

% second order:  (there are k=435 pairs of parameters for nparams=30)
%   to be clear (nparams*(nparams-1))/2 pairs
% k=1
% for i = 1:nparams-1
%   for j = i:nparams-1
%      char(second_order_cell{1}(k)) is the name of i parameter
%      char(second_order_cell{2}(k)) is the name of j parameter
%      second_order_cell{3}(k)  is the second order index
%      second_order_cell{4}(k)  is the confidence interval
%   end
%   k = k+1
% end


%locate nodes for spider plot
% establish 30 parameter grid
nparams_grid = 7;
nodes_grid = zeros(nparams_grid, 2);
angle = 2*pi/nparams_grid;
for i = 1:nparams_grid
    nodes_grid(i,:) = [cos((i-1)*angle), sin((i-1)*angle)];
end

% now choose the nodes active for 7 parameters
nodes = zeros(nparams, 2);
nodes(1,:) = nodes_grid(1,:); %popasym
nodes(2,:) = nodes_grid(2,:); %ga0
nodes(3,:) = nodes_grid(3,:); %gsigma
nodes(4,:) = nodes_grid(4,:); %b12
nodes(5,:) = nodes_grid(5,:); %t2xco2
nodes(6,:) = nodes_grid(6,:); %a2
nodes(7,:) = nodes_grid(7,:); %pback


disp('nodes defined');

%BEWARE... a whole pile of code removed as being obsolete
%   now relying on scaling of first and total order indices
%   break logic applies only to the second order lines
%   see older versions of code, if you must


% create a matrix of weights for the lines connecting
% the nodes; weight based on value of the second order index
% grayscale is a color table from very light gray to black
% first entry may be very light gray, but it is skipped in plotting
% to avoid criss-crossing lacework
grayscale = [0.85 0.85 0.85; 0.6 0.6 0.6; 0.4 0.4 0.4; 0.2 0.2 0.2;...
    0.1 0.1 0.1; 0.05 0.05 0.05; 0.025 0.025 0.025; 0 0 0; 0 0 0; 0 0 0];

% most #1 second orders are < 0.15 (and most <0.05)
% first entry will assign a 'white' line from above table
% negative second order index or otherwise too small to 
% notice at all (top 10 likely to be >=0.001)
line_weight = zeros(nparams-1,nparams-1);
line_color_index = zeros(nparams-1,nparams-1);
limax = 10; % number of entries in break_weight, grayscale assignments
%break_weight = [0.002,0.01,0.02,0.03,0.04,0.05,0.06,0.08,0.10,1.00];
%experiment with lower threshold
%break_weight = [0.0095,0.01,0.02,0.03,0.04,0.05,0.06,0.08,0.10,1.00];
% 10 July mod
% 11 August - change break_weight first entry from 0.00495 to 0.00995
% current low order is for suppressing all < 1%
break_weight = [0.00995,0.01495,0.02495,0.03495,0.04495,...
    0.05495,0.06495,0.07495,0.10,1.00];


%reworked version, plod through second_order_cell to fill
%line_weight(i,j) and line_color_index(i,j) for parameter pair p
% where i represents parameter1 and j parameter2 in the pair

p = 1;  %begin with first parameter pair
% add code to bail when p > (nparams*(nparams-1))/2. !!!
while p <= (nparams*(nparams-1))/2 
  for i = 1:nparams-1
    for j = i:nparams-1
        weight_assigned = 0;  % not done with this pair
        k = 1;    % start with lowest index
        while weight_assigned == 0
            if second_order_cell{3}(p) < break_weight(k)
                line_weight(i,j) = k;
                line_color_index(i,j) = k;
                weight_assigned = 1; % done, move to next pair
                p = p+1;
            else
                if k == limax 
                    line_weight(j,i) = k;
                    line_color_index(j,i) = k;
                    weight_assigned = 1; % done, move to next pair
                    p = p+1;
                else
                    k = k+1;  % move to next index
                end;
            end;
        end;
    end;
  end;
end;

disp('line weight and color indexes assigned');

% OK have sizes defined for the nodes and weights defined for the lines
% lets plot: lines, then rings, then filled circles 
figure(1)
axis square;
axis off;
hold on;
% the lines:
% make the line only if greater than some
% minimum display or line-weight, color
% note scaling here

for i = 1:nparams-1
    for j = i:nparams-1
        if (line_weight(i,j) > 1)
          line([nodes(i,1) nodes(j+1,1)],[nodes(i,2) nodes(j+1,2)],...
            'Color',grayscale(line_color_index(i,j),:),...
            'LineWidth',1.5*line_weight(i,j));
        end
    end;
end;

% after much experimentation here is the scheme for first and total nodes:
%    if total index > 1
%        make ring with face color white
%    end
%    if first index == 1
%        make the positional blot only
%    else
%       make the filled circle
%    end



% worth experimenting with nodeScaling...
% the 1% blots are hardly visible at all
%nodeScaling = 100;
%nodeScaling = 150;   %OK for 1%, but huge for dominant
nodeScaling = 125;

%total order
for p = 1:nparams
    if total_order_cell{2}(p) > 0.01  %threshold
        plot(nodes(p,1),nodes(p,2),'Marker','o',...
            'MarkerFaceColor',[1 1 1],...  %white face
            'MarkerEdgeColor','k',...      %black edge
            'MarkerSize',nodeScaling*total_order_cell{2}(p),...
            'LineWidth',2);
    end
end

% first order  - maybe no positional blot? (whole grid wacked out when
% omitting?????) - OK with markersize 1 (disappears)
for p = 1:nparams
    if first_order_cell{2}(p) < 0.01  %gray positional blot
        plot(nodes(p,1),nodes(p,2),'Marker','o',...
            'MarkerEdgeColor',grayscale(2,:),... %gray edge
            'MarkerFaceColor',grayscale(2,:),... %gray face
            'MarkerSize',1);
    else
        plot(nodes(p,1),nodes(p,2),'Marker','o',...
            'MarkerEdgeColor',grayscale(3,:),...  %grayer edge
            'MarkerFaceColor',grayscale(3,:),...  %grayer face
            'MarkerSize',nodeScaling*first_order_cell{2}(p));
     end
end;

hold off;


% following not used recently  - created legend in AI instead
% code would need to be reviewed for use

if do_legend == 1
    
    % and again, to show the low and high ends only in 1 Figure
    % horizontal row of First, Total nodes; then Second lines
    
    % define node positions here
    nodepos = zeros(4,2);
    nodepos(1,:) = [ 0.05, .5];  %first order <1%
    nodepos(2,:) = [ 0.20, .5];  % first order ~55%
    nodepos(3,:) = [ 0.35, .5];  % total order 1%
    nodepos(4,:) = [ 0.50, .5];  % total order ~75%
    
    figure(2)
    axis square;
    axis off;
    hold on;
    xlim([0 1]);
    ylim([0 1]);
    
   % plot first order limits
   plot(nodepos(1,1),nodepos(1,2),'Marker','o',...
         'MarkerEdgeColor',grayscale(2,:),...
         'MarkerFaceColor',grayscale(2,:),...
         'MarkerSize',node_size(1));
   plot(nodepos(2,1),nodepos(2,2),'Marker','o',...
         'MarkerEdgeColor',grayscale(3,:),...
         'MarkerFaceColor',grayscale(3,:),...
         'MarkerSize',node_size(9));
     
   % plot total order limits (white face color to avoid center dot)
   plot(nodepos(3,1),nodepos(3,2),'Marker','o',...
         'MarkerFaceColor',[1 1 1],...
         'MarkerEdgeColor','k',...
         'MarkerSize',node_size(2),'LineWidth',2);
   plot(nodepos(4,1),nodepos(4,2),'Marker','o',...
         'MarkerFaceColor',[1 1 1],...
         'MarkerEdgeColor','k',...
         'MarkerSize',node_size(11),'LineWidth',2);

   % then the second order limits
   line([.65,.78],[.5,.5],'Color',grayscale(2,:),'LineWidth',1.5*2);
   line([.85,.98],[.5,.5],'Color',grayscale(8,:),'LineWidth',1.5*8);
   hold off;
   
end   
   


disp('done');

