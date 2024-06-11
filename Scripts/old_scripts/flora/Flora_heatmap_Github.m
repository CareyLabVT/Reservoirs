% Script to import Flora data and plot it in contour plot



% Load data and assign columns to variables
% Data is in csv file, with columns: Julian Day, depth (m), Flora variables
% (total, green, cyanos, diatoms, cryptos)
data=csvread('Flora_FCR50_2017_working.csv');
date=data(:,1);
elev=data(:,2);
Chl=data(:,3);
Gr=data(:,4);
Cy=data(:,5);
Di=data(:,6);
Cr=data(:,7);

% Set x and y limits NOTE: y-limits are specific to FCR
% xmin=min(date);
% xmax=max(date);
xmin=736816.43;
xmax=736868.49;
ymin=0.1; %min(elev);
ymax=9.3; %max(elev);


% Creates a function based on data for chl
F_Chl=scatteredInterpolant(date,elev,Chl);
F_Gr=scatteredInterpolant(date,elev,Gr);
F_Cy=scatteredInterpolant(date,elev,Cy);
F_Di=scatteredInterpolant(date,elev,Di);
F_Cr=scatteredInterpolant(date,elev,Cr);


% Define the plotting range
[x,y]=meshgrid(xmin:1:xmax,ymin:0.1:ymax);
Chlplot=F_Chl(x,y);
Grplot=F_Gr(x,y);
Cyplot=F_Cy(x,y);
Diplot=F_Di(x,y);
Crplot=F_Cr(x,y);

% Define contours and breaks (set the scale of the color bar with min/max
% for each column of phytoplankton spectral group)
e=ceil(max(Chl));
contchl=(0:0.1:17);
g=ceil(max(Gr));
contgr=(0:0.1:9.5);
h=ceil(max(Cy));
contcy=(0:0.1:1.5);
i=ceil(max(Di));
contdi=(0:0.1:9);
j=ceil(max(Cr));
contcr=(0:0.1:10);



% ASSIGN A LABEL TO DEFINE TRIANGLES FOR SAMPLE DATES
tss=unique(date);

% DEFINE IMPORTANT DATE LABELS 
CS = 736832.00;
SSS_on = 736803.00;
VEM1 = 736844.00;
% VEM2 = 736508.28;
% VEM3 = 736537.28;
% TURNOVER = 736612;
 
ynums = (ymin:0.1:ymax);
xnums_on(1:length(ynums)) = CS;
xnums_off(1:length(ynums)) = SSS_on;
xnums_off(1:length(ynums)) = VEM1;
% xnums_off(1:length(ynums)) = VEM2;
% xnums_off(1:length(ynums)) = VEM3;


% Plot Chl contours
figure
contourf(x,y,Chlplot,contchl, 'EdgeColor','none');
%line([736480 736480], [0.1 9.3], 'Linewidth',2, 'Color','k');
%line([736508 736508], [0.1 9.3], 'Linewidth',2, 'Color','k');
%set(gca,'XTick',[736480],'FontSize',12)
datetick('x','dd mmm','keeplimits','keepticks')
title('FCR50 2017 - Chl - epi (ug/L)');
ylabel('Depth (m)','FontSize',12);
caxis([0, 17]);
hchl = colorbar;
set(hchl,'fontsize',12);
ylabel(hchl, 'Chlorophyll \ita \rm(\mug/L)','FontSize',12)
xlabel('Date','FontSize',12);
colormap(jet)
set(gca,'YDir','reverse')



hold on

% plot triangles on top of figure showing sampling dates
lb_offset=1;
box off
ylim1=get(gca,'ylim')+0.04;
plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','black')
 
% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
text(CS-lb_offset,plot_loc,'Copper Sulfate','color','black','fontsize',10,'rotation',90);
text(SSS_on-lb_offset,plot_loc,'SSS Activated','color','black','fontsize',10,'rotation',90);
text(VEM1-lb_offset,plot_loc,'VEM1','color','black','fontsize',10,'rotation',90);
% text(VEM2-lb_offset,plot_loc,'VEM2','color','black','fontsize',10,'rotation',90);
% text(VEM3-lb_offset,plot_loc,'VEM3','color','black','fontsize',10,'rotation',90);
% text(TURNOVER-lb_offset,plot_loc,'TURNOVER','color','black','fontsize',10,'rotation',90);

 
% plot important date lines
plot([CS CS],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([VEM1 VEM1],[0 10],'-','linewidth',2,'color','black')
% plot([VEM2 VEM2],[0 10],'-','linewidth',2,'color','black')
% plot([VEM3 VEM3],[0 10],'-','linewidth',2,'color','black')
% plot([TURNOVER TURNOVER],[0 10],'-','linewidth',2,'color','black')

 
set(gca,'ylim',[ylim1(1) ylim1(2)])


saveas(gcf,'2017FCRchla', 'tif')

hold off

% Plot Green contours
figure
contourf(x,y,Grplot,contgr, 'EdgeColor','none');
%line([736480 736480], [0.1 9.3], 'Linewidth',2, 'Color','k');
%line([736508 736508], [0.1 9.3], 'Linewidth',2, 'Color','k');
%set(gca,'XTick',[736480],'FontSize',12)
datetick('x','dd mmm','keeplimits','keepticks')
title('FCR50 2017 - Green Algae - epi (ug/L)');
ylabel('Depth (m)','FontSize',12);
caxis([0, 9.5]);
hchl = colorbar;
set(hchl,'fontsize',13);
ylabel(hchl, 'Green Algae \rm(\mug/L)','FontSize',12)
xlabel('Date','FontSize',12);
colormap(jet)
set(gca,'YDir','reverse')


hold on

% plot triangles on top of figure showing sampling dates
lb_offset=1;
box off
ylim1=get(gca,'ylim')+0.04;
plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','black')
 
% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
text(CS-lb_offset,plot_loc,'Copper Sulfate','color','black','fontsize',10,'rotation',90);
text(SSS_on-lb_offset,plot_loc,'SSS Activated','color','black','fontsize',10,'rotation',90);
text(VEM1-lb_offset,plot_loc,'VEM1','color','black','fontsize',10,'rotation',90);
% text(VEM2-lb_offset,plot_loc,'VEM2','color','black','fontsize',10,'rotation',90);
% text(VEM3-lb_offset,plot_loc,'VEM3','color','black','fontsize',10,'rotation',90);
% text(TURNOVER-lb_offset,plot_loc,'TURNOVER','color','black','fontsize',10,'rotation',90);

 
% plot important date lines
plot([CS CS],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([VEM1 VEM1],[0 10],'-','linewidth',2,'color','black')
% plot([VEM2 VEM2],[0 10],'-','linewidth',2,'color','black')
% plot([VEM3 VEM3],[0 10],'-','linewidth',2,'color','black')
% plot([TURNOVER TURNOVER],[0 10],'-','linewidth',2,'color','black')

 
set(gca,'ylim',[ylim1(1) ylim1(2)])

saveas(gcf,'2017FCRgreens', 'tif')

hold off

% Plot cyanobacteria contours
figure
contourf(x,y,Cyplot,contcy, 'EdgeColor','none');
%line([736480 736480], [0.1 9.3], 'Linewidth',2, 'Color','k');
%line([736508 736508], [0.1 9.3], 'Linewidth',2, 'Color','k');
%set(gca,'XTick',[736480],'FontSize',12)
datetick('x','dd mmm','keeplimits','keepticks')
title('FCR50 2017 - Cyanobacteria - epi (ug/L)');
ylabel('Depth (m)','FontSize',12);
caxis([0, 1.5]);
hchl = colorbar;
set(hchl,'fontsize',12);
ylabel(hchl, 'Cyanobacteria \rm(\mug/L)','FontSize',12)
xlabel('Date','FontSize',12);
colormap(jet)
set(gca,'YDir','reverse')

hold on

% plot triangles on top of figure showing sampling dates
lb_offset=1;
box off
ylim1=get(gca,'ylim')+0.04;
plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','black')
 
% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
text(CS-lb_offset,plot_loc,'Copper Sulfate','color','black','fontsize',10,'rotation',90);
text(SSS_on-lb_offset,plot_loc,'SSS Activated','color','black','fontsize',10,'rotation',90);
text(VEM1-lb_offset,plot_loc,'VEM1','color','black','fontsize',10,'rotation',90);
% text(VEM2-lb_offset,plot_loc,'VEM2','color','black','fontsize',10,'rotation',90);
% text(VEM3-lb_offset,plot_loc,'VEM3','color','black','fontsize',10,'rotation',90);
% text(TURNOVER-lb_offset,plot_loc,'TURNOVER','color','black','fontsize',10,'rotation',90);

 
% plot important date lines
plot([CS CS],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([VEM1 VEM1],[0 10],'-','linewidth',2,'color','black')
% plot([VEM2 VEM2],[0 10],'-','linewidth',2,'color','black')
% plot([VEM3 VEM3],[0 10],'-','linewidth',2,'color','black')
% plot([TURNOVER TURNOVER],[0 10],'-','linewidth',2,'color','black')

 
set(gca,'ylim',[ylim1(1) ylim1(2)])


saveas(gcf,'2017FCRcyano', 'tif')

hold off

% Plot diatoms contours
figure
contourf(x,y,Diplot,contdi, 'EdgeColor','none');
%line([736480 736480], [0.1 9.3], 'Linewidth',2, 'Color','k');
%line([736508 736508], [0.1 9.3], 'Linewidth',2, 'Color','k');
%set(gca,'XTick',[736480],'FontSize',12)
datetick('x','dd mmm','keeplimits','keepticks')
title('FCR50 2017 - Diatoms - epi (ug/L)');
ylabel('Depth (m)','FontSize',12);
caxis([0, 9]);
hchl = colorbar;
set(hchl,'fontsize',13);
ylabel(hchl, 'Diatoms \rm(\mug/L)','FontSize',12)
xlabel('Date','FontSize',12);
colormap(jet)
set(gca,'YDir','reverse')

hold on


% plot triangles on top of figure showing sampling dates
lb_offset=1;
box off
ylim1=get(gca,'ylim')+0.04;
plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','black')
 
% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
text(CS-lb_offset,plot_loc,'Copper Sulfate','color','black','fontsize',10,'rotation',90);
text(SSS_on-lb_offset,plot_loc,'SSS Activated','color','black','fontsize',10,'rotation',90);
text(VEM1-lb_offset,plot_loc,'VEM1','color','black','fontsize',10,'rotation',90);
% text(VEM2-lb_offset,plot_loc,'VEM2','color','black','fontsize',10,'rotation',90);
% text(VEM3-lb_offset,plot_loc,'VEM3','color','black','fontsize',10,'rotation',90);
% text(TURNOVER-lb_offset,plot_loc,'TURNOVER','color','black','fontsize',10,'rotation',90);

 
% plot important date lines
plot([CS CS],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([VEM1 VEM1],[0 10],'-','linewidth',2,'color','black')
% plot([VEM2 VEM2],[0 10],'-','linewidth',2,'color','black')
% plot([VEM3 VEM3],[0 10],'-','linewidth',2,'color','black')
% plot([TURNOVER TURNOVER],[0 10],'-','linewidth',2,'color','black')

 
set(gca,'ylim',[ylim1(1) ylim1(2)])

saveas(gcf,'2017FCRdiatoms', 'tif')

hold off

% Plot cryp contours
figure
contourf(x,y,Crplot,contcr, 'EdgeColor','none');
%line([736480 736480], [0.1 9.3], 'Linewidth',2, 'Color','k');
%line([736508 736508], [0.1 9.3], 'Linewidth',2, 'Color','k');
%set(gca,'XTick',[736480],'FontSize',12)
datetick('x','dd mmm','keeplimits','keepticks')
title('FCR50 2017 - Cryptophytes (ug/L)');
ylabel('Depth (m)','FontSize',12);
caxis([0, 10]);
hchl = colorbar;
set(hchl,'fontsize',12);
ylabel(hchl, 'Cryptophytes \rm(\mug/L)','FontSize',12)
xlabel('Date','FontSize',12);
colormap(jet)
set(gca,'YDir','reverse')

hold on

% plot triangles on top of figure showing sampling dates
lb_offset=1;
box off
ylim1=get(gca,'ylim')+0.04;
plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','black')
 
% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
text(CS-lb_offset,plot_loc,'Copper Sulfate','color','black','fontsize',10,'rotation',90);
text(SSS_on-lb_offset,plot_loc,'SSS Activated','color','black','fontsize',10,'rotation',90);
text(VEM1-lb_offset,plot_loc,'VEM1','color','black','fontsize',10,'rotation',90);
% text(VEM2-lb_offset,plot_loc,'VEM2','color','black','fontsize',10,'rotation',90);
% text(VEM3-lb_offset,plot_loc,'VEM3','color','black','fontsize',10,'rotation',90);
% text(TURNOVER-lb_offset,plot_loc,'TURNOVER','color','black','fontsize',10,'rotation',90);
 
% plot important date lines
plot([CS CS],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([VEM1 VEM1],[0 10],'-','linewidth',2,'color','black')
% plot([VEM2 VEM2],[0 10],'-','linewidth',2,'color','black')
% plot([VEM3 VEM3],[0 10],'-','linewidth',2,'color','black')
% plot([TURNOVER TURNOVER],[0 10],'-','linewidth',2,'color','black')
 
set(gca,'ylim',[ylim1(1) ylim1(2)])

saveas(gcf,'2017FCRcrypto', 'tif')

hold off