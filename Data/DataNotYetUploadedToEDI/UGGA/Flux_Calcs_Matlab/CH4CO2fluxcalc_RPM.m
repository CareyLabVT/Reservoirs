%% Picarro Import for GHG
%
% version: 2.0
% 1 September 2016
% RPM_adpated from D. Scott

% This program is intended to calculate water-air fluxes using
% recirculating chambers and the Picarro GHG. The user must first import 
% the raw Picarro data file, and save as "matlab.mat" in the same folder as
% this m-script.

% Source for converting from ppmv to moles/L: http://www.lenntech.com/calculators/ppm/converter-parts-per-million.htm
% Conversion for ppmv data to mg/m3 ==> conc in ppmv * MWgas / Vm where Vm
% = standard molar volume of ideal gas at 1 bar, 273K = 22.71108 L/mol


% Data inputs:
% matlab.mat: raw data file
% volCh = volume of chamber/hat for experiment
% volTub = volume of tubing between chamber/Picarro
% volTrap = volume of water trap
% volPic = internal volume of Picarro
% areaB = surface area of chamber, m2

clear all;


%% Parameter inputs
volCh = 0.020876028;                           % 2 gallon container * m3/gallon conversion
volTub = pi()*((0.125/2)/12).^2*13.12.*0.00378541;  % 1/8" tubing, 12' long
volTrap = .018;                                 % Trap volume in Liters
volPic = 7560/1e6;                                     % internal volume of Picarro, m3
volT2 = volCh+volTub+volTrap;                    % total volume of picarro+tubing+watertrap+chamber
volT = volPic+volTrap;                          % volPic includes everything minus trap
areaB = 0.1451465;                            % total surface area of chamber

%% read initial data and convert ppmv to mg/m3 gas concentration
data=csvread('ugga_ch4.csv');
date=data(:,1);
ch4=data(:,2);


% NEW
CH4_drymg = ch4.*12.01/22.71108/1000; % converts to mg/L assuming STP
%% Plot data for CH4, find peak locations using methane signal here * modify GHG choice for different systems
xx = date;
xxbegin=xx(1,1);                    % first sampling time in record
xx = xx-round(xxbegin,0);           % substract initial day(integer) from day vector
avgdt = (xx(end)-xx(1))/length(xx)*60*60*24;


yy = CH4_drymg;

[pks,locs,w,p] = findpeaks(ch4,'MinPeakProminence',.07);

figure('Units', 'centimeters', ...
    'Position', [70 20 18 12]); % left, bottom, width, height
plot(xx,yy,'-b',xx(locs),yy(locs),'.r')
xlabel('Time [days]'); ylabel('CH4_dry_PeakHeight')

%delPks = input('Select number of first peaks to delete from analysis')
delPks = 0;
pks(1:delPks,:)=[]; locs(1:delPks,:)=[]; w(1:delPks,:)=[]; p(1:delPks,:)=[]; 
plot(xx,yy,'-b',xx(locs),yy(locs),'.r')
xlabel('Time [days]'); ylabel('CH4_dry_PeakHeight')
clear delPks

%% Step through each peak and identify slope. Setup to calculate slope from -4.5 to -0.5 minutes before peak.

numPeaks = length(pks);             % total number of identified peaks
%numPeaks = 1;                       % remove after testing
tsBeforePeak = 5*60/avgdt;          % number of rows 5 minutes before peak
tsAfterPeak = 2*60/avgdt;           % number of rows 2 minutes after peak

for i = 1:numPeaks
    xT = (xx(locs(i)-tsBeforePeak:locs(i)+tsAfterPeak,:)-xx(locs(i)))*24*60;  % vector of time [min] around peak
    yyT = yy(locs(i)-tsBeforePeak:locs(i)+tsAfterPeak,:);       % vector of CH4 [mg/m3] around peak
    
    hold on
    figure('Units', 'centimeters', ...
    'Position', [70 20 36 12]); % left, bottom, width, height
    h = subplot(1,3,1);
        plot(xT,yyT)
        xlabel('Time [min]'); ylabel('CH4 [mg/m3]')
       
    %Fit slopes for methane
    tsBeforePeakfit = (4.5*60/avgdt);                               % number of rows 4.5 minutes before peak till 1 minute before peak
    tseBeforePeakfit = (0.5*60/avgdt);                               % number of rows 0.5 minutes before peak till 1 minute before peak
    xTf = (xx(locs(i)-tsBeforePeakfit:locs(i)-tseBeforePeakfit,:)-xx(locs(i)))*24*60;
    yyTf = yy(locs(i)-tsBeforePeakfit:locs(i)-tseBeforePeakfit,:);
    p1 = polyfit(xTf,yyTf,1);
    yfit = polyval(p1,xTf);
    yresid=yyTf-yfit;
    SSresid = sum(yresid.^2);
    SStotal=(length(yyTf)-1)*var(yyTf);
    rsq = 1-SSresid/SStotal;
   
    subplot(1,3,1)
    
    hold on
    plot(xTf,yfit,'-r')
    title(['RSQ = ' num2str(round(rsq,2))])

%     mtit(strovt, ...
%     	     'fontsize',14,'color',[1 0 0],...
% 	     'xoff',-.550,'yoff',.005);
    kTable(i,1) = locs(i);              % location of peak
    kTable(i,2) = xx(locs(i),1);        % time of peak, fraction of day 
    kTable(i,3) = p1(1);                % slope CH4 [mg/L/min]
    kTable(i,4) = rsq;                  % R2 for CH4 slope
    %pause
end

%%

% remove peaks from table that are duplicative/error
%kTable(18,:)=[]; kTable(9,:)=[]; kTable(2,:)=[]

% calculate fluxes
for i = 1:length(kTable)
    kTable(i,7) = volT/areaB *kTable(i,3).*1000.*60.*24; % CH4 flux [mgCH4-C/m2/d]
    kTable(i,9) = kTable(i,7);            % CH4 flux [mgCH4/m2/d]
end
csvwrite('UGGA_FCR_2017.csv',kTable);


