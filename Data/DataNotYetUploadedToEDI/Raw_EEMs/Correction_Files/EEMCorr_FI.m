function EEMCorr_FI
% code written by Rose M. Cory
% code adapted by D. Scott, 20101030
% Updated by A.G. Hounshell, 20190924
%   Calcuate Raman_Area from daily Raman Scan
%   Updated Instrument correction files
%   Use 'for loop' to calculate instrument Ex and Em corrections
% Updated by A.G. Hounshell, 20191114
%   Added to code to calculate fluorescence indices
%   Adapt code to select files for processing
%   Adapted to a function

% Purpose of this code is to correct and plot an EEM.
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%% STEP 1. CALCULATE RAMAN AREA USING RAMAN SCAN FROM THE SAME DAY
% INPUT DILUTION FACTOR (use if sample was diluted!)
dilution_factor = 2;

% Calculate Raman Area using the Raman Scan collected on the same day as
% analysis
% Select Raman File
[raman_name, directory_name] = uigetfile({'*.csv'},'Choose Raman Scan');
cd(directory_name);
data = importdata(raman_name);
raman.raw = data.data; % save the original file.
% Apply instrument corrections to Raman file
raman.corrfile = xlsread('mcorrect_raman.xls');
for i = 1:86
    raman.corr(i,1) = raman.corrfile(i,2)*raman.raw(i,4)/4.61251545;
end
% Calculate area under Raman peak
raman_area = trapz(smooth(raman.corr(6:64,1)));
%
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
% Folder names & standard files below; do not modify
samplecor = '_eemc.csv';
%% STEP 2. READ IN EEM FOR CORRECTION: SELECT FILE
%
[datafile_name, directory_name] = ua    z`igetfile({'*.csv'},'Choose EEM file for processing');
cd(directory_name);
A = importdata(datafile_name);
data = A.data(2:end,:);
MyData.raw = data; % save the original file.
sample = 'datafile_name';
ifile = strcat(sample,samplecor);    %whatever you want to name the output file
%
%% Step 3. Read in blank EEM
%
[blankfile_name, directory_name] = uigetfile({'*.csv'},'Choose Blank EEM');
cd(directory_name);
Ab = importdata(blankfile_name);
Abdata = Ab.data(2:end,:);
MyData.blank = Abdata; % save the original blank file.
%
%% Step 4. Read in Absorbance file for corrections
%
% Select sample absorbance scan
[absfile_name, directory_name] = uigetfile({'*.csv'},'Choose Absorbance Scan');
cd(directory_name);
abs = importdata(absfile_name);
% Choose blank absorbance scan
[absbfile_name, directory_name] = uigetfile({'*.csv'},'Choose Blank Absorbance Scan');
cd(directory_name);
absb = importdata(absbfile_name);
% Subract blank scan from sample scan
abs(:,2) = abs(:,2) - absb(:,2); % subtract blank absorbance
clear absb
abs_ex1 = abs(1:5:end, :); %takes abs values every 5nm
abs_ex1 = flipud(abs_ex1); % flips 
%
%% Step 5. Define excitation and emission wavelengths. Ensure correct!!
%
% Define excitation wavelengths
%
excitation = (240:5:450); % defines ex wavelengths
%
% Define emission wavelengths, cut labels from matrix A (uncorrected EEM)
% Cut emission wavelengths from A and Ab (blank matrix)
A.data = A.data(2:end,:); %cuts em wavelengths from 'A'
Ab.data = Ab.data(2:end,:);
Asize = size(A.data);
emissionLen = Asize(1);
%
emission =   300:2:600;  % May need to change based on wavelengths used
%
% Substract blank EEM from sample EEM
A.data = A.data-Ab.data;
clear Ab
MyData.Sub = A.data % Save blank subtracted EEM
%
%REDEFINES EX AND EM AS X AND Y
%
ylen = Asize(1);
xlen = Asize(2);
y = emission;
x = excitation;
xend = x(xlen);
yend = y(ylen);
%
%INTERPOLATING THE DATA
%
[xi, yi] = meshgrid(x(1):2.5:xend, y(1):1:yend); %defines wavelengths to interpolate to (ex by 2.5 and em by 1)
z = A.data(1:ylen, 1:xlen); %redefines 'A' as 'z'
zi = interp2(x, y, z, xi, yi, 'spline'); %interpolation
%
%READ IN THE EX AND EM CORRECTION FILES
%  
MC = xlsread('mcorrect_f4_300_600_1_corr.xls');   
XC = xlsread('xcorrect_f4_240_450_12_5_corr.xls');
%
%APPLYING EX AND EM CORRECTIONS: Using Matlab for loop
%
for i = 1:301
    for j = 1:85
       zi(i,j) = zi(i,j)/XC(j,2)*MC(i,2); 
    end
end
%
MyData.InsCorr = zi; % Save instrument corrected EEM
%
%NORMALIZES CORRECTED DATA TO RAMAN AREA
%
zir=zi/raman_area; 
%
MyData.Norm = zir; % Save raman normalized EEM
%
% ---------------------------------
%INNERFILTER CORRECTION
ao = 190:5:850; %defines wavelength range
%
ai = 190:2.5:850; %defines wavelength range to interpolate to
%
iabs_ex1(:,1) = flipud(ai');
iabs_ex1(:,2) = interp1(ao,abs_ex1(:,2),ai); %interpolates to 2.5 nm
%
ex_abs=iabs_ex1(161:245,:); %selects data from 240-450 (ex range)
ex_abs=flipud(ex_abs);
%
abs = flipud(abs);
em_abs=abs(251:551,:); %selects data from 300-600 (em range)
em_abs=flipud(em_abs);
%
for i=1:length(em_abs)
    for j=1:length(ex_abs)
        IFC(i,j)=(ex_abs(j,2)*dilution_factor)+(em_abs(i,2)*dilution_factor); %defines 'IFC' as the sum of the ex and em wavelengths for all ex/em pairs 
    end
end
czir=zir.*10.^(0.5*IFC); %applies inner filter correction
%
MyData.IFC = czir; % Save IFC corrected EEM
%
% CALCULATES FI AFTER ALL CORRECTIONS HAVE BEEN APPLIED
FInew=czir(171,53)/czir(221,53); %calculates FI ex=370 nm em470/em520
%
% CALCULATE OTHER FLUORESCENT INDICES ON CORRECTED DATA
[ex2, em2] = meshgrid(240:5:450,300:2:600);
icount = 0;
for i = 1:2:301
    icount = icount + 1;
    jcount = 0;
    for j = 1:2:85
        jcount = jcount + 1;        
        MyData.fin (icount,jcount) = czir(i,j)*dilution_factor;
    end
end
ex = ex2;
em = em2;  
%
maxFL = find(MyData.fin == max(max(MyData.fin))); % find maximum fluorescence
maxFLvalue=[ex(maxFL) em(maxFL) MyData.fin(maxFL)]; % find ex/em of maxFL; report intensity
    
Low = find(ex==255 & em>=300 & em<=345);
High = find(ex == 255 & em>=435 & em<=480);
HIX = sum(MyData.fin(High))/sum(MyData.fin(Low));

Bixlow = find(ex==310 & em==380);
Bixhigh = find(ex==310 & em==430);
BIX = MyData.fin(Bixlow)/MyData.fin(Bixhigh);

B = find(ex==275 & em==310);
Bpeak = MyData.fin(B);
    
T = find(ex==275 & em==340);
Tpeak = MyData.fin(T);
    
A = find(ex==260 & em>=380 & em<=460);
Apeak = sum(MyData.fin(A))/length(A);
    
C = find(ex>=320 & ex<=360 & em>=420 & em <=460);
Cpeak = sum(MyData.fin(C))/length(C);
    
M = find(ex>=290 & ex<=310 & em>=370 & em<=410);
Mpeak = sum(MyData.fin(M))/length(M);
    
N = find(ex==280 & em==370);
Npeak = MyData.fin(N);
    
peaks= {'Sample Name','Max Fl Ex ','Max Fl Em ','Max Fl ','B: ','T: ','A: ','C: ','M: ','N:','FI: ','BIX:','HIX:','T/B','T/M:','T/N:','T/C:','A/T:','A/C:','A/M:','M/C:','C/N:'; 
        datafile_name ex(maxFL) em(maxFL) MyData.fin(maxFL) Bpeak Tpeak Apeak Cpeak Mpeak Npeak FInew ...
        BIX HIX Tpeak/Bpeak Tpeak/Mpeak Tpeak/Npeak Tpeak/Cpeak Apeak/Tpeak Apeak/Cpeak Apeak/Mpeak Mpeak/Cpeak Cpeak/Npeak};
%
% Save peaks as .xls file
peak_results = ['results_',datafile_name(1:end-4)];
outputxls = xlswrite(peak_results,peaks,'EEM Results','B1');  
%
%SAVE THE RAMAN NORMALIZED AND EX, EM, AND INNER FILTERED CORRECTED EEM MATRIX
%
sample2 = ['p_',datafile_name(1:end-4)]; % save a processed EEM 
outfile = MyData.fin;
save(sample2,'outfile','-ascii');
save([datafile_name(1:end-4) '.mat'],'MyData');
%
%
%
%% PLOTS THE EEM 3D
eemfig = figure('InvertHardcopy','off','Color',[1 1 1]); % Creates a new figure box w/contour plot
colormap(jet)
axes('Parent',eemfig,'FontSize',18,'FontName','times');
box('on');
hold('all');contourf(ex,em,MyData.fin,18); 
axis tight 
colorbar('vert','FontName','Times','FontSize',18)
xlabel('Excitation wavelength (nm)')
ylabel('Emission wavelength (nm)')
title([datafile_name(1:end-4),' (RU)'])

% end EEMCorr_FI.m