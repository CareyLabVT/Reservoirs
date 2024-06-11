%Flora Contour Map in FCR Polygon
%Author: Mary Lofton & Chris Chen
%Date: 14JUL17

%Data Format: csv
%Column 1: elevation (a *negative* number connotating depth)
%Column 2-...: measured variables
%Last Column: dist (50=820, 45=725, 30=510, 20=320, 10=153)

%Import data

clear, clc;
data=csvread('071317_FCR_all.csv');

%Assign column names
elev=data(:,1);
Chla=data(:,2);
Greens=data(:,3);
Cyanos=data(:,4);
Diatoms=data(:,5);
Cryptos=data(:,6);
dist=data(:,7);

%Draw FCR polygon and meshgrid
polygon_numl=[0,-0.6;150,-1;295,-3.6;464,-5.2;568,-5.6;726,-7.4;820,-9.25;905,-9;905,0;0,0];

xmin=min(dist);
xmax=max(dist);
ymin=min(elev); 
ymax=max(elev);

[x,y]=meshgrid(xmin:0.1:xmax,ymin:0.1:ymax);

% Creates a function based on data for dist, elev, and phyto data
F_Chla=scatteredInterpolant(dist,elev,Chla);
F_Greens=scatteredInterpolant(dist,elev,Greens);
F_Cyanos=scatteredInterpolant(dist,elev,Cyanos);
F_Diatoms=scatteredInterpolant(dist,elev,Diatoms);
F_Cryptos=scatteredInterpolant(dist,elev,Cryptos);

%Returns matrix of values within polygon and applies matrix to phyto data
%function
in_plg=inpolygon(x,y,polygon_numl(:,1),polygon_numl(:,2));
Chlaplot=F_Chla(x,y);
Chlaplot(~in_plg)=nan;
Greensplot=F_Greens(x,y);
Greensplot(~in_plg)=nan;
Cyanosplot=F_Cyanos(x,y);
Cyanosplot(~in_plg)=nan;
Diatomsplot=F_Diatoms(x,y);
Diatomsplot(~in_plg)=nan;
Cryptosplot=F_Cryptos(x,y);
Cryptosplot(~in_plg)=nan;

% Define number of contour lines in plot
a=ceil(max(Chla));
b=ceil(max(Greens));
c=ceil(max(Cyanos));
d=ceil(max(Diatoms));
e=ceil(max(Cryptos));

contChla=(0:0.1:16);
contGreens=(0:0.1:6);
contCyanos=(0:0.1:0.5); 
contDiatoms=(0:0.1:0);
contCryptos=(0:0.1:14);

% Define transect sites
fifty = 820;
fortyfive = 725;
thirty = 510;
twenty = 320;
ten = 153; 
 
ynums = (ymin:0.1:ymax);
xnums_on(1:length(ynums)) = fifty;
xnums_off(1:length(ynums)) = fortyfive;
xnums_off(1:length(ynums)) = thirty;
xnums_off(1:length(ynums)) = twenty;
xnums_off(1:length(ynums)) = ten;


% Plot Chla contours
figure
contourf(x,y,Chlaplot,contChla, 'EdgeColor','none');
title('Total phytos 071317','FontSize',14);
set(gca, 'Xtick',[153,320,510,725,820], 'xticklabel', {'FCR10','FCR20','FCR30','FCR45','FCR50'})
%xlabel('FCR labels','FontSize',14);
ylabel('Depth (m)','FontSize',14);
caxis([0,16]);
hChla = colorbar;
ylabel(hChla, '\mug/L','FontSize',11);
colormap(jet)

hold on

% Plot transect labels
%lb_offset=-8;
%box off
%ylim1=get(gca,'ylim')+0.04;
%plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
%text(fifty-lb_offset,plot_loc,'50','color','black','fontsize',10,'rotation',90);
%text(fortyfive-lb_offset,plot_loc,'45','color','black','fontsize',10,'rotation',90);
%text(thirty-lb_offset,plot_loc,'30','color','black','fontsize',10,'rotation',90);
%text(twenty-lb_offset,plot_loc,'20','color','black','fontsize',10,'rotation',90);
%text(ten-lb_offset,plot_loc,'10','color','black','fontsize',10,'rotation',90);
 
% Plot transect lines
%plot([fifty fifty],[0 -9.3],'-.','linewidth',1,'color','black')
%plot([fortyfive fortyfive],[0 -7.5],'-.','linewidth',1,'color','black')
%plot([thirty thirty],[0 -5.5],'-.','linewidth',1,'color','black')
%plot([twenty twenty],[0 -3.75],'-.','linewidth',1,'color','black')
%plot([ten ten],[0 -1],'-.','linewidth',1,'color','black')
 
% set(gca,'ylim',[ylim1(1) ylim1(2)])
saveas(gcf,'FCR_071317_all', 'tif')
hold off

%Plot Greens contours
figure
contourf(x,y,Greensplot,contGreens, 'EdgeColor','none');%'EdgeColor','none'
title('Green algae 071317','FontSize',15)
set(gca, 'Xtick',[153,320,510,725,820], 'xticklabel', {'FCR10','FCR20','FCR30','FCR45','FCR50'})
%xlabel('Distance (m)','FontSize',25);
ylabel('Depth (m)','FontSize',25);
caxis([0,6]);
hChla = colorbar;
ylabel(hChla, '\mug/L','FontSize',11)
colormap(jet)

hold on

% Plot transect labels
%lb_offset=-8;
%box off
%ylim1=get(gca,'ylim')+0.04;
%plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
%text(fifty-lb_offset,plot_loc,'50','color','black','fontsize',10,'rotation',90);
%text(fortyfive-lb_offset,plot_loc,'45','color','black','fontsize',10,'rotation',90);
%text(thirty-lb_offset,plot_loc,'30','color','black','fontsize',10,'rotation',90);
%text(twenty-lb_offset,plot_loc,'20','color','black','fontsize',10,'rotation',90);
%text(ten-lb_offset,plot_loc,'10','color','black','fontsize',10,'rotation',90);
 
% Plot transect lines
%plot([fifty fifty],[0 -9.3],'-.','linewidth',1,'color','black')
%plot([fortyfive fortyfive],[0 -7.5],'-.','linewidth',1,'color','black')
%plot([thirty thirty],[0 -5.5],'-.','linewidth',1,'color','black')
%plot([twenty twenty],[0 -3.75],'-.','linewidth',1,'color','black')
%plot([ten ten],[0 -1],'-.','linewidth',1,'color','black')
 
%set(gca,'ylim',[ylim1(1) ylim1(2)])
saveas(gcf,'FCR_071317_gr', 'tif')
hold off

%Plots Cyanos contours
figure
contourf(x,y,Cyanosplot,contCyanos, 'EdgeColor','none');%'EdgeColor','none');
title('Cyanobacteria 071317','FontSize',15)
set(gca, 'Xtick',[153,320,510,725,820], 'xticklabel', {'FCR10','FCR20','FCR30','FCR45','FCR50'})
%xlabel('Distance (m)','FontSize',25);
ylabel('Depth (m)','FontSize',25);
caxis([0,0.5]);
hChla = colorbar;
ylabel(hChla, '\mug/L','FontSize',11)
colormap(jet)

hold on

% Plot transect labels
%lb_offset=-8;
%box off
%ylim1=get(gca,'ylim')+0.04;
%plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
%text(fifty-lb_offset,plot_loc,'50','color','black','fontsize',10,'rotation',90);
%text(fortyfive-lb_offset,plot_loc,'45','color','black','fontsize',10,'rotation',90);
%text(thirty-lb_offset,plot_loc,'30','color','black','fontsize',10,'rotation',90);
%text(twenty-lb_offset,plot_loc,'20','color','black','fontsize',10,'rotation',90);
%text(ten-lb_offset,plot_loc,'10','color','black','fontsize',10,'rotation',90);
 
% Plot transect lines
%plot([fifty fifty],[0 -9.3],'-.','linewidth',1,'color','black')
%plot([fortyfive fortyfive],[0 -7.5],'-.','linewidth',1,'color','black')
%plot([thirty thirty],[0 -5.5],'-.','linewidth',1,'color','black')
%plot([twenty twenty],[0 -3.75],'-.','linewidth',1,'color','black')
%plot([ten ten],[0 -1],'-.','linewidth',1,'color','black')
 
% set(gca,'ylim',[ylim1(1) ylim1(2)])
saveas(gcf,'FCR_071317_cy', 'tif')
hold off

%Plot Diatoms contours
figure
contourf(x,y,Diatomsplot,contDiatoms, 'EdgeColor','none');%'EdgeColor','none');
title('Diatoms 071317','FontSize',15)
set(gca, 'Xtick',[153,320,510,725,820], 'xticklabel', {'FCR10','FCR20','FCR30','FCR45','FCR50'})
%xlabel('Distance (m)','FontSize',25);
%ylabel('Depth (m)','FontSize',25);
caxis([0,0]);
hChla = colorbar;
ylabel(hChla, '\mug/L','FontSize',11)
colormap(jet)

hold on

% Plot transect labels
%lb_offset=-8;
%box off
%ylim1=get(gca,'ylim')+0.04;
%plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
%text(fifty-lb_offset,plot_loc,'50','color','black','fontsize',10,'rotation',90);
%text(fortyfive-lb_offset,plot_loc,'45','color','black','fontsize',10,'rotation',90);
%text(thirty-lb_offset,plot_loc,'30','color','black','fontsize',10,'rotation',90);
%text(twenty-lb_offset,plot_loc,'20','color','black','fontsize',10,'rotation',90);
%text(ten-lb_offset,plot_loc,'10','color','black','fontsize',10,'rotation',90);
 
% Plot transect lines
%plot([fifty fifty],[0 -9.3],'-.','linewidth',1,'color','black')
%plot([fortyfive fortyfive],[0 -7.5],'-.','linewidth',1,'color','black')
%plot([thirty thirty],[0 -5.5],'-.','linewidth',1,'color','black')
%plot([twenty twenty],[0 -3.75],'-.','linewidth',1,'color','black')
%plot([ten ten],[0 -1],'-.','linewidth',1,'color','black')
 
% set(gca,'ylim',[ylim1(1) ylim1(2)])
saveas(gcf,'FCR_071317_di', 'tif')
hold off

%Plot Cryptos contours
figure
contourf(x,y,Cryptosplot,contCryptos, 'EdgeColor','none');%'EdgeColor','none');
title('Cryptophytes 071317','FontSize',15)
set(gca, 'Xtick',[153,320,510,725,820], 'xticklabel', {'FCR10','FCR20','FCR30','FCR45','FCR50'})
%xlabel('Distance (m)','FontSize',25);
ylabel('Depth (m)','FontSize',25);
caxis([0,14]);
hChla = colorbar;
ylabel(hChla, '\mug/L','FontSize',11)
colormap(jet)

hold on

% Plot transect labels
%lb_offset=-8;
%box off
%ylim1=get(gca,'ylim')+0.04;
%plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/4;
%text(fifty-lb_offset,plot_loc,'50','color','black','fontsize',10,'rotation',90);
%text(fortyfive-lb_offset,plot_loc,'45','color','black','fontsize',10,'rotation',90);
%text(thirty-lb_offset,plot_loc,'30','color','black','fontsize',10,'rotation',90);
%text(twenty-lb_offset,plot_loc,'20','color','black','fontsize',10,'rotation',90);
%text(ten-lb_offset,plot_loc,'10','color','black','fontsize',10,'rotation',90);
 
% Plot transect lines
%plot([fifty fifty],[0 -9.3],'-.','linewidth',1,'color','black')
%plot([fortyfive fortyfive],[0 -7.5],'-.','linewidth',1,'color','black')
%plot([thirty thirty],[0 -5.5],'-.','linewidth',1,'color','black')
%plot([twenty twenty],[0 -3.75],'-.','linewidth',1,'color','black')
%plot([ten ten],[0 -1],'-.','linewidth',1,'color','black')
 
% set(gca,'ylim',[ylim1(1) ylim1(2)])
saveas(gcf,'FCR_071317_cr', 'tif')
hold off
